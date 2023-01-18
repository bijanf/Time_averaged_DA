#!/usr/bin/env bash
run_assi_plot(){
    funct_opening 2
    dataset_dir=$1
    cd $dataset_dir

    read free_run_dir  < $dataset_dir/config/free_run_dir.cfg
    read spinup_cycles < $dataset_dir/config/nspinup.cfg
    read run_mode      < $dataset_dir/config/run_mode.cfg
    read n_comp        < $dataset_dir/config/n_comp.cfg
    # read model         < $dataset_dir/config/model_name.cfg
    # source $CODE_DIR/$model/model_plots.sh

    export_4byte_binaries_as_netcdf

    for ens_phase in prior postr; do
        for Tkind in Insta Taver; do
            ensemble_stats $ens_phase $Tkind
            ens_assi_skill $ens_phase $Tkind "$free_run_dir"
	    # trajectory_stats $ens_phase $Tkind
	    # plot_rms_stats   $ens_phase $Tkind
        done
    done

    print_statistics
    mkdir stats; mv *.dat stats

    funct_closing 2
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source ${COM_DAS_DIR}/common_das_tools.sh
source ${COM_DAS_DIR}/common_postprocess.sh
run_assi_plot $@
exit $?
