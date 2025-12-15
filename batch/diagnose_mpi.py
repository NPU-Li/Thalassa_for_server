#!/usr/bin/env python3
import sys
import os
import subprocess

def run_command(cmd):
    """运行命令并返回输出"""
    try:
        result = subprocess.run(cmd, shell=True, capture_output=True, text=True)
        return result.stdout.strip()
    except:
        return ""

print("="*70)
print("MPI4py详细诊断")
print("="*70)

# 1. 检查mpi4py基本信息
try:
    import mpi4py
    from mpi4py import MPI
    
    print("✅ mpi4py导入成功")
    print(f"  版本: {mpi4py.__version__}")
    print(f"  路径: {mpi4py.__file__}")
    
    # 获取MPI信息
    vendor = MPI.get_vendor()
    print(f"  MPI供应商代码: {vendor}")
    
    # 供应商代码含义
    vendor_dict = {
        (1, 'MPICH'): 'MPICH或衍生版本',
        (2, 'Intel MPI'): 'Intel MPI',
        (3, 'Open MPI'): 'Open MPI',
        (4, 'Microsoft MPI'): 'Microsoft MPI',
        (0, 'Unknown'): '未知MPI'
    }
    
    vendor_name = vendor[1] if len(vendor) > 1 else 'Unknown'
    print(f"  MPI供应商: {vendor_name}")
    
    # 获取详细版本信息
    version_info = MPI.Get_library_version()
    print("\n  MPI库详细信息:")
    for i, line in enumerate(version_info.split('\n')[:5]):
        if line.strip():
            print(f"    {line}")
    
except ImportError as e:
    print(f"❌ mpi4py导入失败: {e}")
    sys.exit(1)
except Exception as e:
    print(f"❌ 获取MPI信息失败: {e}")

print("\n" + "-"*70)

# 2. 检查系统MPI环境
print("系统MPI环境检查:")

# 检查常见的mpirun
mpirun_paths = [
    "/usr/bin/mpirun",
    "/usr/local/bin/mpirun",
    os.path.expanduser("~/.local/bin/mpirun"),
    "/public/software/mpi/intelmpi/2021.3.0/bin/mpirun",
    "/public/software/mpi/openmpi/gnu/4.0.3/bin/mpirun",
    "/public/home/guojg/PAI/anaconda3/envs/thalassa_env/bin/mpirun"
]

for path in mpirun_paths:
    if os.path.exists(path):
        print(f"  ✅ 找到mpirun: {path}")
        try:
            version = run_command(f"{path} --version | head -1")
            if version:
                print(f"     版本: {version[:50]}...")
        except:
            pass

# 3. 检查环境变量
print("\nMPI相关环境变量:")
mpi_vars = [
    'MPI_HOME', 'I_MPI_ROOT', 'OMPI_HOME', 'MPICH_HOME',
    'PATH', 'LD_LIBRARY_PATH', 'PYTHONPATH'
]

for var in mpi_vars:
    value = os.environ.get(var, '未设置')
    if 'PATH' in var:
        # 显示PATH中包含MPI的部分
        paths = value.split(':')
        mpi_paths = [p for p in paths if 'mpi' in p.lower()]
        if mpi_paths:
            print(f"  {var}:")
            for p in mpi_paths[:3]:  # 只显示前3个
                print(f"    - {p}")
            if len(mpi_paths) > 3:
                print(f"    - ... 还有{len(mpi_paths)-3}个")
    elif value != '未设置':
        print(f"  {var}: {value}")

print("\n" + "="*70)

# 4. 运行简单MPI测试
print("运行简单MPI功能测试...")
test_code = """
from mpi4py import MPI
import sys

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
size = comm.Get_size()

if rank == 0:
    print(f'MPI通信测试: 总进程数={size}')
    
    # 发送数据测试
    data = {'test': 'MPI communication', 'rank': rank}
    for i in range(1, size):
        comm.send(data, dest=i, tag=42)
    
    # 接收回复
    for i in range(1, size):
        reply = comm.recv(source=i, tag=43)
        print(f'  收到进程{i}的回复: {reply}')
else:
    # 接收数据
    data = comm.recv(source=0, tag=42)
    data['received'] = True
    comm.send(data, dest=0, tag=43)

comm.Barrier()
if rank == 0:
    print('✅ MPI通信测试通过')
"""

# 保存测试代码
with open('/tmp/mpi_test.py', 'w') as f:
    f.write(test_code)

# 尝试运行测试
print("\n尝试运行MPI测试...")
test_result = run_command("mpirun -n 2 python /tmp/mpi_test.py 2>&1")
if "MPI通信测试通过" in test_result:
    print("✅ MPI功能测试成功")
else:
    print("⚠️  MPI功能测试可能有问题")
    print(f"   输出: {test_result[:100]}...")

print("\n" + "="*70)
print("诊断完成")
print("="*70
