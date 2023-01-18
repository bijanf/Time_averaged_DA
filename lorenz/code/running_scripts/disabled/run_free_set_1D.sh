#!/usr/bin/env bash
run_free_set(){
    funct_opening 3
    [[ $# -eq 3 ]] || error "Usage: Incorrect # of arguments"

    local  dataset_dir=$1
    local     par_name=$2
    local  par_span_st=$3 # ( in numpy syntax )

    # Create dataset dir
    create_archive_dir $dataset_dir || return 0
    cd $dataset_dir; mkdir runs

    # Configure run
    dataset_kind="run_set";
    dataset_dim=1
    run_free_dir(){ echo $dataset_dir/runs/$run_free_name; }
    source $CODE_DIR/$model/default_run_free_pars.sh
    eval "par_span=\"$par_span_st\""

    # report and save metainfo
    echo " Creating a run_free set"
    print_line 0
    set_par_file "dataset_kind"
    set_par_file "dataset_dim"
    set_par_file "par_name"
    set_par_file "par_span_st"
    set_par_file "dataset_dir"
#    set_par_file "sampling_dir"
    print_line 0

    # create par_values file
    case "$par_name" in
        "cycle_length") # Real valued parameters
            echo_real_par_values $par_span > ${par_name}_values.cfg 
            ;;
        *) error "Unsupported parameter $par_name";;
    esac

    # Send free runs
    nprocess=0;
    readarray -t par_values < ${par_name}_values.cfg
    for par_value in ${par_values[@]}; do

	source $CODE_DIR/$model/default_run_free_pars.sh
        eval "export ${par_name}=\"$par_value\""

        run_free_name=${par_name}${par_value}
        echo " free run $run_free_name"
        echo $run_free_name >> run_names.cfg
	echo $par_value     >> par_value_list.cfg

        { time -p run_free.sh $(run_free_dir); } &> ${run_free_name}.log &

        (( nprocess+=1 )); [ $(( nprocess%cpus )) == 0 ] && wait
    done
    wait

    # Tidy up dataset
    cat run_names.cfg | xargs -I% mv %.log $dataset_dir/runs/%
    mkdir config; mv *.cfg config
    funct_closing 3
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source ${COM_DAS_DIR}/common_das_tools.sh
run_free_set $@
exit $?
