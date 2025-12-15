#!/bin/bash
#SBATCH --job-name=EnvTest
#SBATCH --nodes=2
#SBATCH --ntasks-per-node=1
#SBATCH --time=00:05:00
#SBATCH --output=env_test_%j.out
#SBATCH --error=env_test_%j.err
#SBATCH --partition=cpu_56c

CONDA_PATH="/public/home/guojg/PAI/anaconda3"

echo "=== Testing Conda Environment Propagation ==="

# 方法1：直接 srun
echo "Method 1: Direct srun"
source "$CONDA_PATH/etc/profile.d/conda.sh"
conda activate thalassa_env
srun --export=ALL bash -c 'echo "$(hostname): Python=$(which python), Version=$(python --version 2>&1)"'

echo ""

# 方法2：在每个节点激活
echo "Method 2: Activate on each node"
srun bash -c "source $CONDA_PATH/etc/profile.d/conda.sh; conda activate thalassa_env; echo \"\$(hostname): Python=\$(which python), Version=\$(python --version 2>&1)\""

echo ""

# 方法3：测试路径访问
echo "Method 3: Test path access"
srun bash -c 'echo "$(hostname): $(ls -ld /public/home/guojg/thalassa_dir/batch 2>&1 | head -1)"'
