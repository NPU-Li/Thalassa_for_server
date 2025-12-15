#!/bin/bash
#SBATCH --job-name=ThalassaArray
#SBATCH --array=1-2
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=40
#SBATCH --mem=0
#SBATCH --partition=cpu_56c
#SBATCH --time=12:00:00
#SBATCH --output=/public/home/guojg/thalassa_dir/batch/array_%A_%a.log
#SBATCH --error=/public/home/guojg/thalassa_dir/batch/array_%A_%a.err
#SBATCH --chdir=/tmp

case $SLURM_ARRAY_TASK_ID in
    1)
        SAT_NUM="0000000001"
        ;;
    2)
        SAT_NUM="0000000002"
        ;;
    *)
        echo "wrong ID"
        exit 1
        ;;
esac

INPUT_FILE="/public/home/guojg/thalassa_dir/output_batch1/C001/S${SAT_NUM}/input.txt"
OBJECT_FILE="/public/home/guojg/thalassa_dir/output_batch1/C001/S${SAT_NUM}/object.txt"

cd /public/home/guojg/thalassa_dir

time /public/home/guojg/thalassa_dir/thalassa.x "$INPUT_FILE" "$OBJECT_FILE"

exit $EXIT_CODE