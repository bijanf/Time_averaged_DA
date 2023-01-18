#!/usr/bin/env bash
#set -x
ens_assi_vs_cycle_length_plot(){
    funct_opening 2
    dataset_dir=$1

    cd $dataset_dir

    # Plot individual ens_assi runs
    cat ens_assi_runs.dat | xargs -I % -P $cpus \
	ens_assi_run_plot.sh $dataset_dir/% > /dev/null

    scalars=( {prior,postr}_{Insta,Taver}_error_{Tstd_Xmean,Xstd_Tmean} )
#    scalars=( {prior,postr}_{Insta,Taver}_RMSSS_Xmean )
    for scalar in ${scalars[@]-} cycle_length; do
        rm -f lists/${scalar}_list.dat
        touch lists/${scalar}_list.dat
    done

    # Gather results into lists
    while read ens_assi_name; do
        cd $dataset_dir/$ens_assi_name
        for scalar in ${scalars[@]-}; do
            cat overall_stats/${scalar}.dat>>../lists/${scalar}_list.dat
        done
        cat config/cycle_length.cfg >> ../lists/cycle_length_list.dat
    done < $dataset_dir/ens_assi_runs.dat

    # Build datasets in netcdf format
    cd $dataset_dir/lists
    for scalar in ${scalars[@]-} cycle_length; do
        create_netcdf_scalar_vs_par "cycle_length" $scalar
    done
    ls *.nc | xargs -I % mv % ../

    funct_closing 2
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source ${COM_DAS_DIR}/common_das_tools.sh
ens_assi_vs_cycle_length_plot $@
exit $?
