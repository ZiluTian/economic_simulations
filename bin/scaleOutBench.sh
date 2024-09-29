#!/bin/bash

source /home/user/zilu/economic_simulations/bin/workers.sh

choice=$1
cmd=$2

case $choice in
    0)
        # for i in $(seq 1 $repeat); do
            echo "Driver executing test: $cmd"
            # Create a new folder with the original name
            touch "$LOG/$cmd.log" 
            # echo "Created a new folder: $LOG"
            sbt -mem 100000 "project akka; test:runMain simulation.akka.test.$cmd driver $DRIVER_IP $DRIVER_PORT $TOTAL_WORKERS"            
        # done
        ;;
    *)
        MACHINE_ID=$((choice - 1))
        WORKER_IP=${WORKERS[$MACHINE_ID]}
        # for i in $(seq 1 $repeat); do
            echo "Worker executing test: $cmd"
            sbt -mem 100000 "project akka; test:runMain simulation.akka.test.$cmd worker $WORKER_IP $WORKER_PORT $TOTAL_WORKERS $MACHINE_ID $DRIVER_IP:$DRIVER_PORT"
        # done
        ;;
esac

# # Restart the script if the user didn't choose to exit
# exec "$0"
