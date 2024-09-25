#!/bin/bash

DRIVER_IP="130.60.194.131"

# Workers
WORKERS=(
    130.60.194.131
    # 130.60.194.132
    # 130.60.194.133
    130.60.194.134
    130.60.194.135
    # 130.60.194.136
    # 130.60.194.137
    # 130.60.194.138
    # 130.60.194.139
    # 130.60.194.140
)

TOTAL_WORKERS=${#WORKERS[@]}

EXPERIMENTS=(
    # "gameOfLifeScaleOutTest"  
    # "stockMarketScaleOutTest"
    "SBMScaleOutTest"
    "ERMScaleOutTest"
)

stopWorkers(){
    for worker in "${WORKERS[@]}"; do
        echo "Connecting to zilu@$worker and running cleanJava.sh"
        ssh "zilu@$worker" "bash /home/user/zilu/cleanJava.sh" > /dev/null 2>&1 &
    done
}

# Config
LOG="/local/scratch/zilu/scaleOutLog"
mkdir -p "$LOG"