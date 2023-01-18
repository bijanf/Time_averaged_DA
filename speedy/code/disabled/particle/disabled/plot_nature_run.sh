#!/usr/bin/env bash
#=======================================================================
#> @brief Add info
#> #plot_nature_run nature_0006hr_0002ts_spin1
#=======================================================================
plot_nature_run(){
 	[[ $# -eq 1 ]] || error "Usage: plot_nature_run nature_run_name"
	nature_run_name=$1

    echo "==============================================="
    echo " Plotting $nature_run_name"
    echo "==============================================="
    
    storing_dir=$ARCHIVE_DIR
    nature_run_dir=${storing_dir}/${nature_run_name}
    
    echo " - Parsing Arguments"

    [ -d   $nature_run_dir ] || error "There is no free run dataset '$nature_run_name' in $storing_dir"
    
    cd $nature_run_dir
    [ -f dataset_complete.dat ] || error "Nature Run dataset is not complete"
   
    speedy2nc.sh "states/YYYYMMDDHH_grid_sigma_Insta.ctl" &> /dev/null
    speedy2nc.sh "states/YYYYMMDDHH_grid_sigma_Taver.ctl" &> /dev/null
    
    echo " climatology checking plots are needed here !!!"

    return 0
}
set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

plot_nature_run $@
exit $?
