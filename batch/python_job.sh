#!/bin/bash
#SBATCH --job-name=PythonTest
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --partition=cpu_56c

# load the environment
module purge
module load apps/python/3.6.1

# run python
python --version

