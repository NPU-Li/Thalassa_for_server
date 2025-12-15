#!/bin/bash
#SBATCH --job-name=Thalassa_Task2
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=40
#SBATCH --mem=0
#SBATCH --partition=cpu_56c
#SBATCH --nodelist=compute02     
#SBATCH --exclusive
#SBATCH --time=12:00:00
#SBATCH --output=/public/home/guojg/thalassa_dir/batch/task2_%j.log
#SBATCH --error=/public/home/guojg/thalassa_dir/batch/task2_%j.err

echo "========================================="
echo "Task 2 started at: $(date)"
echo "Running on node: $(hostname)"
echo "========================================="

SAT_NUM="0000000002"
INPUT_FILE="/public/home/guojg/thalassa_dir/output_batch1/C001/S${SAT_NUM}/input.txt"
OBJECT_FILE="/public/home/guojg/thalassa_dir/output_batch1/C001/S${SAT_NUM}/object.txt"

cd /public/home/guojg/thalassa_dir
time ./thalassa.x "$INPUT_FILE" "$OBJECT_FILE"

echo "Task 2 ended at: $(date)"
