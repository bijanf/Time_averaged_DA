#!/usr/bin/env bash
nature_vs_par_plot(){
    funct_opening 2
    dataset_dir=$1
    cd $dataset_dir

    read         par_name    < config/par_name.cfg
    readarray -t nature_runs < config/nature_runs.cfg

    echo " - Plotting individual nature runs"
    printf '%s\n' "${nature_runs[@]}"| xargs -I % -P $cpus \
        nature_run_plot.sh $dataset_dir/% > /dev/null
    readarray scalars < <(cd "${nature_runs[0]}"/stats; ls *.dat | cut -d'.' -f1)
#    scalars=( nature_{Insta,Taver}_Tstdd_Xmean )

    # Plot individual nature runs (loop parallelization)
    # readarray nature_runs < nature_runs.dat
    # nprocess=0
    # for nature_name in ${nature_runs[@]}; do
    #     (( nprocess+=1 ))
    #     echo "plotting $nature_name"
    #     nature_run_plot.sh $dataset_dir/$nature_name > /dev/null &
    #     (( nprocess+=1 )); [ $(( nprocess%cpus )) == 0 ] && wait
    # done


    # Gather results into lists
    rm -fr lists; mkdir lists
    while read nature_name; do
        cd $dataset_dir/$nature_name/stats
        ls *.dat | xargs -I % bash -c "cat % >> ../../lists/list_%"
        cat ../config/${par_name}.cfg >> ../../lists/list_${par_name}.dat
    done < $dataset_dir/config/nature_runs.cfg

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
nature_vs_par_plot $@
exit $?
