#!/usr/bin/env bash
run_vs_par1_par2_plot(){
    funct_opening 2
    dataset_dir=$1;  cd $dataset_dir

    read             par1_name < config/par1_name.cfg
    read             par2_name < config/par2_name.cfg
    readarray -t run_set_names < config/run_set_names.cfg
    # read      dataset_kind < ${run_names[0]}/config/dataset_kind.cfg
    
    # Plot individual run line sets
    printf '%s\n' "${run_set_names[@]}" | xargs -I % -P 1 \
        run_vs_par_plot.sh $dataset_dir/% > /dev/null

    # # Gather results into lists
    # rm -fr lists; mkdir lists
    # for run_set_name in ${run_set_names[@]}; do 
    #     cd $dataset_dir/$run_set_name/stats
    #     ls *.dat | xargs -I % bash -c "cat % >> ../../lists/list_%"
    # 	cat ../config/${par_name}.cfg >> ../../lists/list_${par_name}.dat
    # done
    
    # # Build datasets in netcdf format
    # readarray -t scalars < <(cd "${run_names[0]}"/stats; ls *.dat|cut -d'.' -f1)
    # cd $dataset_dir/lists
    # for scalar in ${scalars[@]-}; do
    #     create_netcdf_scalar_vs_par $par_name $scalar
    # done
    # ls *.nc | xargs -I % mv % ../

    funct_closing 2
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source ${COM_DAS_DIR}/common_das_tools.sh
run_vs_par1_par2_plot $@
exit $?
