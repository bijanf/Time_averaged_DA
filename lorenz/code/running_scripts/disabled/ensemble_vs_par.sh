#!/usr/bin/env bash
ensemble_vs_cycle_length(){
    funct_opening 3
    local   dataset_dir=$1
    local      run_mode=$2
    local reference_set=$3 # nature of ensemble set dir

    echo " Creating ensemble_run set"
    
    # Prepare output dataset
    create_archive_dir $dataset_dir || return 0
    cd $dataset_dir
    print_line 0
    set_par_file "run_mode"
    dataset_kind="ensemble_vs_par"; set_par_file "dataset_kind"
    par_name="cycle_length"       ; set_par_file "par_name"
#    par_span=$cycle_length_span   ; set_par_file "par_span"
    set_par_file "dataset_dir"
    set_par_file "reference_set"
    print_line 0

    case "$run_mode" in
        "free") cp $reference_set/config/nature_runs.cfg .
            readarray reference_names < nature_runs.cfg
            ;;
        "assi") cp $reference_set/config/ensemble_runs.cfg ens_free_runs.cfg
            readarray reference_names < ens_free_runs.cfg
            ;;
        *     ) error "Unknown run_mode flag $run_mode";;
    esac

    reference_run(){ echo $reference_set/$reference_name; }
    ensemble_dir (){ echo $dataset_dir/$ensemble_name; }

    # Send ensemble runs
    nprocess=0; touch ensemble_runs.cfg
    for reference_name in ${reference_names[@]}; do
        export cycle_length=$(cat $(reference_run)/config/cycle_length.cfg)

        ensemble_name=$reference_name
        echo " Ensemble run $ensemble_name"
        echo $ensemble_name >> ensemble_runs.cfg

        { time -p ensemble_run.sh $(ensemble_dir) $run_mode $(reference_run); } \
            &> ${ensemble_name}_report.txt &
	
        (( nprocess+=1 )); [ $(( nprocess%cpus )) == 0 ] && wait
    done
    wait
    cat ensemble_runs.cfg | xargs -I% mv %_report.txt $dataset_dir/%

    mkdir config; mv *.cfg config
    funct_closing 3
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source ${COM_DAS_DIR}/common_das_tools.sh
ensemble_vs_cycle_length $@
exit $?
