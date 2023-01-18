#!/usr/bin/env bash
#=======================================================================
#> @brief Main calculation launcher
#> @param sim_id Simulation identifier (from simulation_list)
#> @param verity Calculation significance level
#>        - 0: Very fast config for testing and comp. time estimation
#>        - 1: Real config to get statistically significant results.
#> @param debug  Debugging flag
#>        - "on" : Compile with debugging symbols and run with ddd
#>        - "off": Compile with optimization and run directly
#=======================================================================
das_compute(){
    funct_opening 4

    local  sim_id=$1
    export verity=$2
    echo "==============================================="
    echo " Running simulation called ${sim_id}"
    echo "==============================================="

    case $verity in
        0)  export STORE=$TMP_DIR;
	    #rm -rf $STORE
            (   /usr/bin/time -v simulation_list.sh $sim_id
	        echo $SECONDS > fast_run_length.dat         )
            read fast_run_length < fast_run_length.dat
            real_run_length=$(echo ${fast_run_length}*100/3600|bc -l)
            fast_run_length=$(printf '%f3' $fast_run_length)
            real_run_length=$(printf '%f3' $real_run_length)
            rm fast_run_length.dat
            echo " Fast run duration: $fast_run_length seconds"
            echo " Estimated real run duration: $real_run_length hours"
            ;;
        1)  export STORE=$ARCH_DIR
            logfile="$ARCH_DIR/${sim_id}.log"
            { /usr/bin/time -v simulation_list.sh $sim_id; } &> $logfile  &
            sim_pid=$!
            echo " Simulation PID = $sim_pid"
            echo " To check the progress execute"
            echo " tail -f $logfile"
            ;;
        *) error "Unsupported verity level $verity"
    esac

    echo " Storing dir: $STORE"
    funct_closing 4
}

trap "" SIGHUP # Simulations do not terminate when this script closes
set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source das_initialize.sh
source ${COM_DAS_DIR}/common_das_tools.sh

das_compute $@
exit $?
