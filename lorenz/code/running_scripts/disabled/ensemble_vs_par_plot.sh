#!/usr/bin/env bash
ensemble_run_vs_cycle_length_plot(){
    funct_opening 2
    dataset_dir=$1
    cd $dataset_dir

    read run_mode   < config/run_mode.cfg

    # Plot individual ensemble runs
    cat config/ensemble_runs.cfg | xargs -I % -P $cpus \
        ensemble_run_plot.sh $dataset_dir/% > /dev/null

    case "$run_mode" in
        #scalars=()
        "free")
	    scalars=(       prior_{Insta,Taver}_error_{Tstd_Xmean,Xstd_Tmean})
	    ;;
        "assi")
	    scalars=({prior,postr}_{Insta,Taver}_error_{Tstd_Xmean,Xstd_Tmean} \
            {prior,postr}_{Insta,Taver}_RMSSS_Xmean)
	    ;;
        *) error "Unknown run_mode $run_mode";;
    esac

    mkdir lists
    for scalar in ${scalars[@]-} cycle_length; do
        rm -f lists/${scalar}_list.dat
        touch lists/${scalar}_list.dat
    done

    # Gather results into lists
    while read ensemble_name; do
        cd $dataset_dir/$ensemble_name
        for scalar in ${scalars[@]-}; do
            cat overall_stats/${scalar}.dat>>../lists/${scalar}_list.dat
        done
        cat config/cycle_length.cfg >> ../lists/cycle_length_list.dat
    done < $dataset_dir/config/ensemble_runs.cfg

    # Build datasets in netcdf format
    cd $dataset_dir/lists
    for scalar in ${scalars[@]-}; do
        create_netcdf_scalar_vs_par "cycle_length" $scalar
    done
    ls *.nc | xargs -I % mv % ../

    funct_closing 2
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source ${COM_DAS_DIR}/common_das_tools.sh
ensemble_run_vs_cycle_length_plot $@
exit $?
