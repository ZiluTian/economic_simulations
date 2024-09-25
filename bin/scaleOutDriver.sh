#!/bin/bash

source /home/user/zilu/economic_simulations/bin/workers.sh

# Driver
JAVA_HOME="/usr/lib/jvm/java-11-openjdk-amd64/bin"
WORKING_DIR="/home/user/zilu/economic_simulations"

# Clean up any remaining Bench processes
stopWorkers

for cmd in "${EXPERIMENTS[@]}"; do
    # Start the driver
    DRIVER_COMMAND="export PATH=$JAVA_HOME:$PATH && cd $WORKING_DIR && bash bin/scaleOutBench.sh 0 $cmd"
    ssh "$DRIVER_IP" "$DRIVER_COMMAND" &
    # Wait for the lock on sbt to be availble
    sleep 3

    # Execute workers
    for i in $(seq 1 $TOTAL_WORKERS); do
        machineId=$((i - 1))
        WORKER_COMMAND="export PATH=$JAVA_HOME:$PATH && cd $WORKING_DIR && bash bin/scaleOutBench.sh $i $cmd"

        echo "Connecting to machine $machineId choice $i ip ${WORKERS[$machineId]} $cmd"    
        sleep 5
        ssh "zilu@${WORKERS[$machineId]}" "$WORKER_COMMAND" &
        
        # Check if the ssh command was successful
        if [ $? -ne 0 ]; then
            echo "Failed to execute command on ${WORKERS[$machineId]}"
        else
            echo "Command executed successfully on ${WORKERS[$machineId]}"
        fi
    done

    sleep 10
    # driver
    tail -n 0 -f "$LOG/$cmd.log" | while read line; do
        echo "Check if simulation has ended!"
        if echo "$line" | grep -q "Average"; then
            echo "Keyword 'Average' found."
            # Execute the command
            echo "$line" >> "$LOG/${cmd}_m${TOTAL_WORKERS}"
            stopWorkers
            break
        fi
        # Wait for 2 seconds before checking the next batch of lines
        sleep 2
    done
done 