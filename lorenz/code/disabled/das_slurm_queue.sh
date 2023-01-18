#!/usr/bin/env bash

# script to put a process into a slurm system queue
# must be executed as follows:
# sbatch queueing_script.sh
# memory values are given im MBytes

#SBATCH --job-name="DAS_simulation"
#SBATCH --mail-user=acevedo@zedat.fu-berlin.de
#SBATCH --mail-type=end
#SBATCH --ntasks=8
#SBATCH --mem-per-cpu=512
#SBATCH --time=08:00:00

./das.sh --task=launch --model=L96_2s --sim_id="2013_11_30"
# { time -p bash $SIM_LIST $sim_id; } &> "$STORE/${sim_id}.log" &
# sim_pid=$!
# echo " Simulation List $SIM_LIST"
# echo " Simulation ID $sim_id (PID $sim_pid)"
# echo " To check the progress execute"
# echo " tail -f $STORE/${sim_id}.log"
