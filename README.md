# Introduction to THALASSA_for_server
原项目来自https://gitlab.com/souvlaki/thalassa  原作者Davide Amato，在此仅做了微小的服务器并行化改动。

The original project is from https://gitlab.com/souvlaki/thalassa, originally created by Davide Amato. Here, only minor server parallelization modifications have been made.

原项目通过launchgrid.py生成批量计算文件，现在通过/thalassa_dir/batch下的matlab脚本gen_thalassa_data生成json文件和文件阵列。

The original project generated batch computation files through launchgrid.py. Now, JSON files and file arrays are generated via the MATLAB script gen_thalassa_data located in /thalassa_dir/batch.

由于原项目json文件仅起到估计存储空间的作用及提供SID数量的作用，并且只能生成连续间隔网格文件，在这里修改代码使得launchgrid.py从SID文件夹数直接读取数量，并可以读取随机间隔网格。

Since the JSON files in the original project only served to estimate storage space and provide the number of SIDs, and could only generate grid files with continuous intervals, the code has been modified here to allow launchgrid.py to directly read the number from the SID folder count and also read grid files with random intervals.

在launchgrid.py中增加了SID计算数量控制，在内存瓶颈的时候缩减并行数量，并可以将任务提交到其他节点进行计算。所有任务在shell脚本里以并行时间步提交。提交脚本为run_thalassa.sh，通过以下命令提交：

In launchgrid.py, SID computation quantity control has been added to reduce the number of parallel processes when memory bottlenecks occur, and tasks can also be submitted to other nodes for computation. All tasks are submitted with parallel time steps in the shell script. The submission script is run_thalassa.sh, which can be submitted using the following command:

`sbatch run_thalassa.sh`

脚本内容为：

The shell is：

```shell
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
```
