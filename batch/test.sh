#!/bin/bash
#SBATCH --job-name=ThalassaSingleSim
#SBATCH --nodes=2
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=40
#SBATCH --mem=0
#SBATCH --partition=cpu_56c
#SBATCH --time=12:00:00
#SBATCH --output=/public/home/guojg/thalassa_dir/batch/job_%j.log  # 绝对路径
#SBATCH --error=/public/home/guojg/thalassa_dir/batch/job_%j.err    # 绝对路径

echo "========================================"
echo "Job ID: $SLURM_JOB_ID"
echo "Start time: $(date)"
echo "日志将保存到: /public/home/guojg/thalassa_dir/batch/"
echo "========================================"
module purge
module load compiler/intel-compiler/2021.3.0
module load mpi/intelmpi/2021.3.0

# 节点1：运行第一个卫星 (S0000000001)
srun --nodes=1 --ntasks=1 --cpus-per-task=40 --exclusive \
    --output=/public/home/guojg/thalassa_dir/batch/node1_sat1_%j.log \
    --error=/public/home/guojg/thalassa_dir/batch/node1_sat1_%j.err \
    bash -c '
    cd /public/home/guojg/thalassa_dir && \
    /public/home/guojg/thalassa_dir/thalassa.x \
        /public/home/guojg/thalassa_dir/output_batch1/C001/S0000000001/input.txt \
        /public/home/guojg/thalassa_dir/output_batch1/C001/S0000000001/object.txt
    ' &
PID1=$!


#srun  --mpi=pmi2  $ARGS
# 节点2：运行第二个卫星 (S0000000002)  
srun --nodes=1 --ntasks=1 --cpus-per-task=40 --exclusive \
    --output=/public/home/guojg/thalassa_dir/batch/node2_sat2_%j.log \
    --error=/public/home/guojg/thalassa_dir/batch/node2_sat2_%j.err \
    bash -c '
    cd /public/home/guojg/thalassa_dir && \
    /public/home/guojg/thalassa_dir/thalassa.x \
        /public/home/guojg/thalassa_dir/output_batch1/C001/S0000000002/input.txt \
        /public/home/guojg/thalassa_dir/output_batch1/C001/S0000000002/object.txt
    ' &
PID2=$!

wait $PID1 $PID2

echo "========================================"
echo "Job completed at: $(date)"
echo "主日志: /public/home/guojg/thalassa_dir/batch/job_${SLURM_JOB_ID}.log"
echo "节点1(卫星1)日志: /public/home/guojg/thalassa_dir/batch/node1_sat1_${SLURM_JOB_ID}.log"
echo "节点2(卫星2)日志: /public/home/guojg/thalassa_dir/batch/node2_sat2_${SLURM_JOB_ID}.log"
echo "卫星1输出目录: /public/home/guojg/thalassa_dir/output_batch1/C001/S0000000001/"
echo "卫星2输出目录: /public/home/guojg/thalassa_dir/output_batch1/C001/S0000000002/"