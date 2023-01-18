#!/usr/bin/env bash
#=======================================================================
#> @brief Do statistical analysis of a run set
#=======================================================================
run_set_stats(){
    funct_opening 2

    stats="on"; plots="on"; symmetry_flag=""
    while getopts "spd" opt; do
        case $opt in
            s) plots="off";; # only stats
            p) stats="off";; # only plots
            d) stats="off"   # config for a run_set_diff
                symmetry_flag="-s" ;;
        esac
    done
    shift $((OPTIND-1))

    dataset_dir=$1; dataset_dir="$(absolute_path "$dataset_dir")"

    cd $dataset_dir
    read          run_mode < config/run_mode.cfg
    readarray -t run_names < config/run_names.cfg
    read            n_comp < config/n_comp.cfg
    read       dataset_dim < config/dataset_dim.cfg

    echo " Finding out parameter span"
    case "$dataset_dim" in
        1)  read par1_name < config/par1_name.cfg;
            par_name[0]=$par1_name
            ;;
        2)  read par1_name < config/par1_name.cfg
            read par2_name < config/par2_name.cfg
            par_name[0]=$par1_name
            par_name[1]=$par2_name
            ;;
    esac

    if [ $stats == "on" ];then
        analize_individual_runs
        gather_stats_into_lists
        create_netcdfs
    fi

    if [ $plots == "on" ];then
        create_plots
        create_report
    fi


    funct_closing 2
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source ${COM_DAS_DIR}/common_das_tools.sh
source ${COM_DAS_DIR}/run_set_tools.sh
run_set_stats $@
exit $?
