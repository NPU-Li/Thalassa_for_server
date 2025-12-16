#!/bin/bash
#SBATCH --job-name=ThalassaChdir
#SBATCH --nodes=2
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

srun --nodes=1 --ntasks=1 --cpus-per-task=40 --exclusive \
    bash -c '
    cd /public/home/guojg/thalassa_dir/batch && \
    python launchgrid.py /public/home/guojg/thalassa_dir/output_data/test1 \
        --nproc 2 \
        --minSID 1 \
        --maxSID 3 \
        --force
    ' &
PID1=$!


srun --nodes=1 --ntasks=1 --cpus-per-task=40 --exclusive \
    bash -c '
    cd /public/home/guojg/thalassa_dir/batch && \
    python launchgrid.py /public/home/guojg/thalassa_dir/output_data/test1 \
        --nproc 2 \
        --minSID 4 \
        --maxSID 6 \
        --force
    ' &
PID2=$!

wait $PID1 $PID2

echo "========================================"
echo "Job completed at: $(date)"