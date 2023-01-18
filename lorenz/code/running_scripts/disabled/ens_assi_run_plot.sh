#!/usr/bin/env bash
#set -x
ens_assi_run_plot(){
    funct_opening 2
    dataset_dir=$1
    cd $dataset_dir

    trajectory_stats "prior_Emean"
    trajectory_stats "postr_Emean"

    read model < $dataset_dir/config/model_name.cfg
#    local model=$(trim $(cat config/model_name.cfg))
    source $CODE_DIR/$model/model_plots.sh

    export_4byte_binaries_as_netcdf
    plot_ensemble_run
#    plot_rms_stats
#    trajectory_stats "ens_assi"

    funct_closing 2
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source ${COM_DAS_DIR}/common_das_tools.sh
ens_assi_run_plot $@
exit $?
