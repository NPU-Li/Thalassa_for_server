from mpi4py import MPI
import numpy as np

comm = MPI.COMM_WORLD  # 获取通信子
rank = comm.Get_rank()  # 获取当前进程的rank
size = comm.Get_size()  # 获取进程的总数

sendbuf = None  # 定义发送缓冲区

if rank == 0:
    # 在根进程中初始化数组
    sendbuf = np.arange(size * 2, dtype='i')
    print("Sendbuf:", sendbuf)

# 定义接收缓冲区
recvbuf = np.empty(2, dtype='i')

# 将数据从根进程分发到其他进程中
comm.Scatter(sendbuf, recvbuf, root=0)
print("Process %d received: %s" % (rank, recvbuf))