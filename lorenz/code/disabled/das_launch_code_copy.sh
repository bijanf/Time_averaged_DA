#!/usr/bin/env bash
#=======================================================================
#> @brief This script permits to run several threads of the
#> experiment launcher by creating temporary copies of the code.
#> This allows continuing developing while code is running.
#> @param sim_id Simulation identifier (from simulation_list)
#=======================================================================
das_launch_code_copy(){
    funct_opening 3
    local  sim_id=$1

    prefix_tmp="das_code_tmp"
    tmp_code(){ echo "$TMP_DIR/${prefix_tmp}${count}"; }

    # Finding free temporary folder
    count=1 # Code copy number
    while [[ -d $(tmp_code) ]]; do
        (( count++ ))
    done

    echo " Copying code"
    HERE=`pwd`; cd ..
    cp -ra code $(tmp_code) # copy preserving attributes

    echo " Starting calculation"
    cd $(tmp_code)
    ./das_compute.sh $sim_id 1 | tee $ARCH_DIR/code_copy${count}.log &

    #Removing temporary code copy
    #wait; rm -fr $(tmp_code)
    
    funct_closing 3
}
set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source das_initialize.sh
source ${COM_DAS_DIR}/common_das_tools.sh
das_launch_code_copy $@
exit $?
