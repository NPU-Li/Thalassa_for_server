#!/bin/bash
#SBATCH --job-name=ThalassaChdir
#SBATCH --nodes=3
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=50
#SBATCH --mem=0
#SBATCH --partition=cpu_56c
#SBATCH --time=12:00:00
#SBATCH --output=/public/home/guojg/thalassa_dir/output_data/logfile/job_%j.log
#SBATCH --error=/public/home/guojg/thalassa_dir/output_data/logfile/job_%j.err

echo "========================================"
echo "Job ID: $SLURM_JOB_ID"
echo "Start time: $(date)"
echo "========================================"

srun --nodes=1 --ntasks=1 --cpus-per-task=50 --exclusive \
    bash -c '
    cd /public/home/guojg/thalassa_dir/batch && \
    python launchgrid.py /public/home/guojg/thalassa_dir/output_data/test1 \
        --nproc 1 \
        --minSID 1 \
        --maxSID 2 \
        --force
    ' &
PID1=$!


srun --nodes=1 --ntasks=1 --cpus-per-task=40 --exclusive \
    bash -c '
    cd /public/home/guojg/thalassa_dir/batch && \
    python launchgrid.py /public/home/guojg/thalassa_dir/output_data/test1 \
        --nproc 1 \
        --minSID 3 \
        --maxSID 4 \
        --force
    ' &
PID2=$!

srun --nodes=1 --ntasks=1 --cpus-per-task=40 --exclusive \
    bash -c '
    cd /public/home/guojg/thalassa_dir/batch && \
    python launchgrid.py /public/home/guojg/thalassa_dir/output_data/test1 \
        --nproc 1 \
        --minSID 5 \
        --maxSID 6 \
        --force
    ' &
PID3=$!


wait $PID1 $PID2 $PID3

echo "========================================"
echo "Job completed at: $(date)"