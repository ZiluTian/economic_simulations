#!/bin/bash

source /home/user/zilu/economic_simulations/bin/workers.sh

# Driver
JAVA_HOME="/usr/lib/jvm/java-11-openjdk-amd64/bin"
WORKING_DIR="/home/user/zilu/economic_simulations"

# Clean up any remaining Bench processes
stopWorkers

START_CLUSTER_TIMEOUT=6000
MAX_RETRY=5

# Repeat
startRepeat=1
endRepeat=3

startExperiment(){
    cmd=$1
    totalWorkers=$2
    echo "Start experiment $cmd $totalWorkers"

    file_path="$LOG/$cmd.log"
    if [ -f "$file_path" ]; then
        timestamp=$(date +"%Y-%m-%d_%H-%M-%S")
        mv "$file_path" "${file_path}_$timestamp"
        echo "$file_path renamed to: ${file_path}_$timestamp"
    else
        echo "File does not exist: $file_path"
    fi

    DRIVER_COMMAND="export PATH=$JAVA_HOME:$PATH && cd $WORKING_DIR && bash bin/scaleOutBench.sh 0 $cmd $totalWorkers"
    ssh "$DRIVER_IP" "$DRIVER_COMMAND" > $file_path 2>&1 &
    # Wait for the lock on sbt to be availble
    sleep 3

    # Execute workers
    for i in $(seq 1 $totalWorkers); do
        machineId=$((i - 1))
        WORKER_COMMAND="export PATH=$JAVA_HOME:$PATH && cd $WORKING_DIR && bash bin/scaleOutBench.sh $i $cmd $totalWorkers"

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
}

for totalWorkers in "${TOTAL_WORKERS[@]}"; do
    for rpt in $(seq $startRepeat $endRepeat); do
        for workload in "${WORKLOADS[@]}"; do
            for category in "${CATEGORIES[@]}"; do
                cmd="${workload}${category}ScaleOutTest"
                sleep 10
                # Start the driver
                startExperiment "$cmd" "$totalWorkers" 

                # Restart the cluster if it takes more than 10minutes to start a cluster
                SECONDS=0
                failure_count=0

                while true; do
                    echo "Wait for the simulation to start"

                    if grep -q "Connection refused" "$LOG/$cmd.log"; then 
                        echo "Connection refused. Retry the experiment"
                        stopWorkers
                        startExperiment "$cmd" "$totalWorkers"
                    fi

                    # Check the file for the keyword
                    if grep -q "Round 1 takes" "$LOG/$cmd.log"; then
                        echo "Keyword 'Round 1' found! Continue"
                        break
                    fi

                    if [ "$SECONDS" -ge "$START_CLUSTER_TIMEOUT" ]; then
                        echo "Timeout reached. Keyword not found. Restarting the experiment"
                        # Run the failure command
                        stopWorkers
                        startExperiment "$cmd"
                        failure_count=$((failure_count + 1))
                    fi

                    if [ "$failure_count" -ge "$MAX_RETRY" ]; then
                        echo "Max error retry has reached. Exit"
                        exit 1
                    fi

                    # Wait 2 seconds before checking again
                    sleep 5
                done

                sleep 10
                # driver
                while true; do
                    echo "Wait for the simulation to stop"
                    # Check the file for the keyword
                    if grep -q "Average" "$LOG/$cmd.log"; then
                        echo "Keyword 'Average' found."
                        # Execute the command
                        mv "$LOG/$cmd.log" "$LOG/${cmd}_m${totalWorkers}_r${rpt}"
                        sleep 20
                        stopWorkers
                        break
                    fi
                    # Wait 1 second before checking again
                    sleep 5
                done
                sleep 10
            done 
            sleep 5
        done
        sleep 5
    done
done