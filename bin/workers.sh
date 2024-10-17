#!/bin/bash

DRIVER_IP="130.60.194.135"
DRIVER_PORT="8000"
WORKER_PORT="8001"

# Workers
WORKERS=(
    130.60.194.131
    130.60.194.132
    130.60.194.133
    130.60.194.134
    130.60.194.135
    130.60.194.136
    130.60.194.137
    130.60.194.138
    130.60.194.139
    130.60.194.140
)

TOTAL_WORKERS=(10 8 6 4 2)

WORKLOADS=(
    "gameOfLife"  
    "stockMarket"
    "SBM"
    "ERM"
)

CATEGORIES=(
    "Unopt"
    "FusionNoLocalMessage"
    "FusionOnly"
    "FusionWithRemoteMessage"
    "Fused"
)

# EXPERIMENTS=(
#     "gameOfLifeFusionOnlyScaleOutTest"
#     "stockMarketAggregateFusionOnlyScaleOutTest"
#     "stockMarketFusionNoLocalMessageScaleOutTest"
#     "stockMarketFusionWithRemoteMessageScaleOutTest"
#     # "stockMarketFusedAggregateScaleOutTest"
#     # "stockMarketGraphScaleOutTest"
#     # "gameOfLifeScaleOutTest"  
#     # "gameOfLifeFusedScaleOutTest"  
#     # "gameOfLifeGraphScaleOutTest"
# #     "SBMScaleOutTest"
# #     "SBMFusedScaleOutTest"
# #     "SBMGraphScaleOutTest"
# #     "ERMScaleOutTest"
# #     "ERMFusedScaleOutTest"
# #     "ERMGraphScaleOutTest"
# )

stopWorkers(){
    for worker in "${WORKERS[@]}"; do
        echo "Connecting to zilu@$worker and running cleanJava.sh"
        ssh "zilu@$worker" "bash /home/user/zilu/cleanJava.sh" > /dev/null 2>&1 &
        sleep 5
    done
}

# Config
LOG="/local/scratch/zilu/scaleOutLog"
mkdir -p "$LOG"