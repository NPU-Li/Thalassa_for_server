#!/bin/bash
# check_slurm_config.sh - 检查SLURM配置

echo "========== 检查SLURM配置 =========="
echo "当前时间: $(date)"
echo ""

# 检查slurm.conf文件
SLURM_CONF="/etc/slurm/slurm.conf"
if [ -f "$SLURM_CONF" ]; then
    echo "1. 检查slurm.conf配置:"
    grep -E "SelectType|SelectTypeParameters" "$SLURM_CONF"
    
    echo ""
    echo "2. 检查分区配置:"
    grep -A 10 "PartitionName=cpu_56c" "$SLURM_CONF" || echo "未找到cpu_56c分区配置"
    
    echo ""
    echo "3. 检查节点配置:"
    grep -E "NodeName=comput" "$SLURM_CONF" | head -5
else
    echo "找不到slurm.conf文件，尝试其他位置..."
    find /etc -name "slurm*.conf" 2>/dev/null | head -5
fi

echo ""
echo "4. 查看当前生效的配置:"
scontrol show config | grep -A 5 -B 5 "SelectType"

echo ""
echo "5. 查看节点资源分配策略:"
sinfo -o "%15N %10c %10m %10C %10O"