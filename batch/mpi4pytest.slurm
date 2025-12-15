#!/bin/bash

#BATCH --job-name=mpi4pytest      # 作业名
#SBATCH --partition=cpu_56c      # 64c512g队列
#SBATCH --nodes=1                       # 申请一个节点
#SBATCH --ntasks-per-node=10              # 节点上只运行一个任务
#SBATCH -n 10                     # 作业核心数
#SBATCH --output=%j.out
#SBATCH --error=%j.err
CONDA_PATH="/public/home/guojg/PAI/anaconda3"

ulimit -s unlimited
ulimit -l unlimited

echo "Activating conda environment..."
source "$CONDA_PATH/etc/profile.d/conda.sh"
conda activate thalassa_env

mpirun -np 10  python3 test.py