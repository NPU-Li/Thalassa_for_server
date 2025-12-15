#!/usr/bin/env python3
"""
LAUNCH BATCH PROPAGATIONS OVER A GRID OF INITIAL CONDITIONS
Launch propagations with THALASSA for the initial conditions specified by the
grid.dat file, and using the settings specified by the griddef.json file. These
are both assigned by the user, and grid.dat is first created by coegrid.py.

Author:
  Davide Amato
  The University of Arizona
  davideamato@email.arizona.edu

Revisions:
  180529: Script creation.
  180605: Various bugs fixed. Added output of current SID.
  180610: Add resume flag.
  2023: Added minSID and maxSID parameters for range control.

"""

import argparse
import sys
import os
import json
import multiprocessing as mp
import psutil
import time
import datetime
import subprocess
import platform
from coegrid import chunkSize

thalassaPath = os.path.abspath('../thalassa.x')


def sizeof_fmt(num, suffix='B'):
    """
    Copied from https://web.archive.org/web/20111010015624/http://blogmag.net/
    blog/read/38/Print_human_readable_file_size .

    Author:
      Fred Cirera
    """
    for unit in ['','Ki','Mi','Gi','Ti','Pi','Ei','Zi']:
        if abs(num) < 1024.0:
            return "%3.1f%s%s" % (num, unit, suffix)
        num /= 1024.0
    
    return "%.1f%s%s" % (num, 'Yi', suffix)

def sizeOfPropagation(Ntot, dt, duration):
    """
    Estimates the size of the output from one THALASSA batch propagation.

    Author:
    Davide Amato
    The University of Arizona
    davideamato@email.arizona.edu

    Revisions:
    180529: Script creation.

    """

    NPoints = duration/dt * 365.25 + 1
    bytesEachProp = 2 * (602 + NPoints * 161)
    bytesTot = Ntot * bytesEachProp

    sizeTot = sizeof_fmt(bytesTot, suffix='B')

    return sizeTot

def runThalassa(outDir, resume, SID):
    """
    Launch THALASSA for the propagation identified by 'SID'.

    Author:
    Davide Amato
    The University of Arizona
    davideamato@email.arizona.edu

    Revisions:
    180529: Script creation.
    180605: Add output of SID.
    180610: Add skipping of propagations if resume flag is true.
    180618: Overwrite output directory in the input file with resume flag.

    """
    
    # Reconstruct directory from SID
    iChunk = (SID - 1) // chunkSize + 1
    subDir = 'C{:03d}'.format(iChunk)
    subSubDir = 'S{:010d}'.format(int(SID))
    outfile = os.path.join(outDir, subDir, subSubDir, 'orbels.dat')

    if (resume and os.path.isfile(outfile)):
        print('Simulation SID {0} already exists. Skipping...'.format(SID), flush=True)

    else:
        if (resume):
            with open(os.path.join(outDir, subDir, subSubDir, 'input.txt'), 'r') as f:
                inpFile = f.readlines()
            inpFile[44] = "out:   " + os.path.abspath(os.path.join(outDir, subDir, subSubDir, ' '))
            with open(os.path.join(outDir, subDir, subSubDir, 'input.txt'), 'w') as f:
                f.writelines(inpFile)
        
        print('Launching simulation SID = {0}'.format(SID), flush=True)
        # Launch THALASSA
        subprocess.call([thalassaPath,
        os.path.abspath(os.path.join(outDir, subDir, subSubDir, 'input.txt')),
        os.path.abspath(os.path.join(outDir, subDir, subSubDir, 'object.txt'))])

def main():
    
    # Parse arguments
    parser = argparse.ArgumentParser(description='Launch THALASSA propagations '
    'over a grid of initial conditions.')
    
    parser.add_argument('outDir', nargs='?',
    help='path to the output directory for the batch propagations')
    
    parser.add_argument('--nproc',
    help='number of parallel processes to be launched. If absent, it defaults with '
    'the number of available CPUs.', type=int)
    
    parser.add_argument('--minSID',
    help='minimum SID to start processing from (default: 1)', type=int, default=1)
    
    parser.add_argument('--maxSID',
    help='maximum SID to process up to (default: all)', type=int, default=None)
    
    parser.add_argument('--force',
    help='don''t ask for user confirmation before starting the simulation.',
    action='store_true')
    
    parser.add_argument('--resume',
    help='resume previous simulation. This skips directories where the file "propagation.log" is present.',
    action='store_true')
    
    if len(sys.argv) == 1:
        parser.print_help(sys.stderr)
        sys.exit(1)
    
    args = parser.parse_args()
    
    gridDefFile = os.path.join(args.outDir, 'griddef.json')

    print('THALASSA GRID PROPAGATION SCRIPT')
    print('Reading grid definition from ' + os.path.abspath(gridDefFile) + '...', end=" ")
    
    # Get number of simulations and trajectory structure
    with open(gridDefFile, 'r') as f:
        gridDefDict = json.load(f)
    print('Done.\n')

    # Read number of simulations, output to user
    sDirs = []
    for root, dirs, files in os.walk(args.outDir):
        sDirs.extend([d for d in dirs if d.startswith('S')])
    nTot = len(sDirs)
    
    # Set default maxSID if not provided
    if args.maxSID is None:
        args.maxSID = nTot
    else:
        # Ensure maxSID doesn't exceed total number of simulations
        args.maxSID = min(args.maxSID, nTot)
    
    # Validate minSID and maxSID
    if args.minSID < 1:
        print(f"Warning: minSID {args.minSID} is less than 1, setting to 1")
        args.minSID = 1
    
    if args.minSID > args.maxSID:
        print(f"Error: minSID ({args.minSID}) is greater than maxSID ({args.maxSID})")
        sys.exit(1)
    
    # Calculate the number of simulations in the specified range
    nRange = args.maxSID - args.minSID + 1
    
    dt = gridDefDict["Integration"]["Step"]
    duration = gridDefDict["Integration"]["Duration"]
    
    # Calculate total size for all simulations
    sizeAll = sizeOfPropagation(nTot, dt, duration)
    # Calculate size for the specified range
    sizeRange = sizeOfPropagation(nRange, dt, duration)

    proceedMsg = """\n========================================================================
GRID PROPAGATION SUMMARY:
  Total simulations available: {0}
  Simulations in specified range (SID {1} to {2}): {3}
  Each simulation: {4:g} years long, {5:g} days time step
  
  Total disk space for all {0} simulations: {6}
  Disk space for current range ({3} simulations): {7}
  
  Do you want to continue? (Y/N)
""".format(nTot, args.minSID, args.maxSID, nRange, duration, dt, sizeAll, sizeRange)

    if not args.force:
        proceed = input(proceedMsg)
        if proceed.lower() != 'y':
            sys.exit(1)
    
    # Launch propagations in parallel
    if args.nproc:
        nproc = args.nproc
    else:
        nproc = psutil.cpu_count(logical=False)
    
    startTime = datetime.datetime.now()
    
    # Configure logging
    if args.resume:
        log = open(os.path.join(args.outDir, 'grid.log'), 'a', 1)
        log.writelines('\nResuming the simulation... ')
    else:
        log = open(os.path.join(args.outDir, 'grid.log'), 'w', 1)
        log.writelines('THALASSA GRID PROPAGATION - LOG FILE\n')

    log.writelines('Start of logging on {0}.\n'.format(startTime.isoformat()))
    log.writelines('OS: {0}.\n'.format(platform.platform()))
    log.writelines('Number of parallel processes: {0:d}.\n'.format(nproc))
    log.writelines('Total simulations available: {0:d}.\n'.format(nTot))
    log.writelines('Processing SID range: {0:d} to {1:d} ({2:d} simulations).\n'.format(
        args.minSID, args.maxSID, nRange))
    log.writelines('Duration: {0:g} years.\n'.format(duration))
    log.writelines('Step size: {0:g} days.\n'.format(dt))

    # Create argument list for multiprocessing
    runArgs = [(args.outDir, args.resume, SID) for SID in range(args.minSID, args.maxSID + 1)]

    os.chdir('../')
    with mp.Pool(processes=(nproc)) as pool:
        pool.starmap(runThalassa, runArgs)

    endTime = datetime.datetime.now()
    log.writelines('End of grid propagation on {0}.\n'.format(endTime.isoformat()))
    gridDur = endTime - startTime
    log.writelines('Wall time elapsed: {0}.\n'.format(str(gridDur)))
    log.writelines('Processed SIDs: {0} to {1}\n'.format(args.minSID, args.maxSID))

    log.close()
    
    print(f"\n========================================")
    print(f"Grid propagation completed!")
    print(f"Processed SIDs: {args.minSID} to {args.maxSID}")
    print(f"Total simulations processed: {nRange}")
    print(f"Wall time: {gridDur}")
    print(f"Start time: {startTime.strftime('%Y-%m-%d %H:%M:%S')}")
    print(f"End time: {endTime.strftime('%Y-%m-%d %H:%M:%S')}")
    print(f"Log file: {os.path.join(args.outDir, 'grid.log')}")
    print(f"========================================\n")

if __name__ == '__main__':
    main()