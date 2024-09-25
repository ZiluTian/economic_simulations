#!/bin/bash

source /home/user/zilu/economic_simulations/bin/workers.sh

choice=$1
cmd=$2

driver_port="25001"
worker_port="25500"

# Repeat
repeat=1

case $choice in
    0)
        # for i in $(seq 1 $repeat); do
            echo "Driver executing test: $cmd"
            # Create a new folder with the original name
            touch "$LOG/$cmd.log" 
            # echo "Created a new folder: $LOG"
            sbt -mem 100000 "project akka; test:runMain simulation.akka.test.$cmd driver $DRIVER_IP $driver_port $TOTAL_WORKERS"            
            # driver_port=$((driver_port + 1))
        # done
        ;;
    *)
        machineId=$((choice - 1))
        worker_ip=${WORKERS[$machineId]}
        # for i in $(seq 1 $repeat); do
            echo "Worker executing test: $cmd"
            sbt -mem 100000 "project akka; test:runMain simulation.akka.test.$cmd worker $worker_ip $worker_port $TOTAL_WORKERS $machineId $DRIVER_IP:$driver_port"
            # worker_port=$((worker_port + 1))
            sleep 10
            # ps aux | grep java | ps -v 'grep' | awk '{print $2}' | xargs kill  > /dev/null 2>&1 &
            # driver_port=$((driver_port + 1))
        # done
        ;;
esac

# # Restart the script if the user didn't choose to exit
# exec "$0"
