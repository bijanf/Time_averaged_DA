#!/usr/bin/env bash

free_run_plot(){
    funct_opening 2
    dataset_dir=$1
    cd $dataset_dir

    read spinup_cycles < $dataset_dir/config/nspinup.cfg
    read n_comp        < $dataset_dir/config/n_comp.cfg
    free_run_dir=$dataset_dir

    trajectory_stats "nature_Insta" comp1 c
    forecast_stats "nature_Insta" "free_prior_Emean_Insta" comp1 c
    if [[ $n_comp -eq 2 ]]; then
        trajectory_stats "nature_Insta" comp2 w
        forecast_stats "nature_Insta" "free_prior_Emean_Insta" comp2 w
    fi

#    export_4byte_binaries_as_netcdf

    # ensemble_stats "prior" "Insta"
    # ensemble_stats "prior" "Taver"

    print_statistics
    mkdir stats; mv *.dat stats
    mkdir ncl_scripts; mv *.ncl ncl_scripts

    funct_closing 2
}
set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source ${COM_DAS_DIR}/common_das_tools.sh
source ${COM_DAS_DIR}/common_postprocess.sh
free_run_plot $@
exit $?
