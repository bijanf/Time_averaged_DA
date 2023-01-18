#!/usr/bin/env bash
#=======================================================================
#> @brief launch system routines with verity=0 ( \see run"()" )
#> Arguments:
#> - none        : test the sanity of the whole system
#> - "-s sim_id" : test the simulation identified with sim_id
#>
#> @attention This routine should be run without argument after every
#>            potentially harmfull code change.
#=======================================================================
das_test(){
    funct_opening 4

    # Default parameters
    test_simulation="no"
    export verity=0

    # Customizing options
    while getopts "s:v:g" opt; do
        case $opt in
            s)  test_simulation="yes"
                local  sim_id="$OPTARG" ;;
            v)  export verity="$OPTARG" ;;
            g)  export    DBG="on"      ;;
        esac
    done
    shift $((OPTIND-1))
    
    sim_list=$model/simulation_list.sh
    export STORE=$ARCH_DIR/test/$model

    if [ "$test_simulation" == "yes" ]; then
        echo "=========================================================="
        echo " Testing simulation ${sim_id} for model $model"
        echo "=========================================================="
#        (   time -p bash $SIM_LIST $sim_id
        (   /usr/bin/time -v bash $sim_list $sim_id
            echo $SECONDS > fast_run_length.dat         )
        read fast_run_length < fast_run_length.dat
        real_run_length=$(echo ${fast_run_length}*100/3600|bc -l)
        fast_run_length=$(printf '%f3' $fast_run_length)
        real_run_length=$(printf '%f3' $real_run_length)
        rm fast_run_length.dat
        echo " Fast run duration: $fast_run_length seconds"
        echo " Estimated real run duration: $real_run_length hours"
    else
#        for model in ${model_list[@]}; do
            ./test_list.sh $model
#        done
    fi

    echo " Storing dir: $STORE"
    funct_closing 4
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source ./das_initialize.sh
source ${COM_DAS_DIR}/common_das_tools.sh

das_test $@
exit $?
