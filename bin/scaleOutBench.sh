#!/bin/bash

# Read user input
read -p "Enter the role: [0 for driver, others for pre-configured workers] " choice

tests=(
    "gameOfLifeScaleOutTest"  
    "stockMarketScaleOutTest"
    "SBMScaleOutTest"
    "ERMScaleOutTest"
)

# Config
folder_name="scaleOut50000"

# Driver
driver_ip="130.60.194.131"
driver_port="25000"

# Workers
worker_ips=(
    "130.60.194.134"
    “130.60.194.131”
)
worker_port="25300"
total_machines=${#worker_ips[@]}

# Repeat
repeat=3

case $choice in
    0)
    for i in $(seq 1 $repeat); do
        echo "Executing test: $cmd"
        if [ -d "$folder_name" ]; then
            # Get the current timestamp
            timestamp=$(date +"%Y%m%d_%H%M%S")
            
            # Rename the folder by appending the timestamp
            mv "$folder_name" "${folder_name}_$timestamp"
            echo "Renamed the folder to ${folder_name}_$timestamp"
        fi
        # Create a new folder with the original name
        mkdir "$folder_name"
        echo "Created a new folder: $folder_name"

        for cmd in "${tests[@]}"; do
            sbt -mem 100000 "project akka; test:runMain simulation.akka.test.$cmd driver $driver_ip $driver_port $total_machines"
            driver_port=$((driver_port + 1))
        done
    done
    ;;
    *)
    machineId=$((choice - 1))
    for i in $(seq 1 $repeat); do
        for cmd in "${tests[@]}"; do
            echo "Executing test: $cmd"
            sbt -mem 100000 "project akka; test:runMain simulation.akka.test.$cmd worker ${worker_ips[$machineId]} $worker_port $total_machines $machineId $driver_ip:$driver_port"
            driver_port=$((driver_port + 1))
            worker_port=$((worker_port + 1))
        done
    done
    ;;
esac

# # Restart the script if the user didn't choose to exit
# exec "$0"