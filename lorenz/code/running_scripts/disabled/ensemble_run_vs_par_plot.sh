#!/usr/bin/env bash
ensemble_run_vs_par_plot(){
    funct_opening 2
    dataset_dir=$1
    cd $dataset_dir

    read      par_name      < config/par_name.cfg
    readarray ensemble_runs < config/ensemble_runs.cfg
    
    # read run_mode   < config/run_mode.cfg
    # case "$run_mode" in
    #     "free")
    # 	    scalars=(       prior_{Insta,Taver}_error_{Tstd_Xmean,Xstd_Tmean})
    # 	    ;;
    #     "assi")
    # 	    scalars=({prior,postr}_{Insta,Taver}_error_{Tstd_Xmean,Xstd_Tmean} \
    #         {prior,postr}_{Insta,Taver}_RMSSS_Xmean)
    # 	    ;;
    #     *) error "Unknown run_mode $run_mode";;
    # esac

    # Plot individual ensemble runs
    cat config/ensemble_runs.cfg | xargs -I % -P $cpus \
        ensemble_run_plot.sh $dataset_dir/% > /dev/null
    readarray scalars < <(cd ${ensemble_runs[0]}/overall_stats; ls *.dat | cut -d'.' -f1)
printf -- '%s\n' "${scalars[@]}"
    # Gather results into lists
    rm -fr lists; mkdir lists
    while read ensemble_name; do
        cd $dataset_dir/$ensemble_name/overall_stats
	 ls
	# pwd
	# jnjn
        ls *.dat | xargs -I % bash -c "cat % >> ../../lists/list_%"
	cat ../config/${par_name}.cfg >> ../../lists/list_${par_name}.dat
    done < $dataset_dir/config/ensemble_runs.cfg

    # Build datasets in netcdf format
    cd $dataset_dir/lists
    for scalar in ${scalars[@]-}; do
        create_netcdf_scalar_vs_par $par_name $scalar
    done
    ls *.nc | xargs -I % mv % ../

    funct_closing 2
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source ${COM_DAS_DIR}/common_das_tools.sh
ensemble_run_vs_par_plot $@
exit $?
