#!/usr/bin/env bash
#set -x
#set -v
ens_assi_vs_cycle_length(){
    funct_opening 3
    dataset_dir=$1
    ens_free_vs_cycle_length_dir=$2

    echo " Creating ens_assi_runs for a set of ens_free_runs"

    ens_assi_dir(){ echo $dataset_dir/$ens_assi_name; }
    ens_free_dir(){ echo $ens_free_vs_cycle_length_dir/$ens_free_name; }

    # Prepare output dataset
    create_archive_dir $dataset_dir || return 0
    read nature_vs_cycle_length_dir < \
        $ens_free_vs_cycle_length_dir/config/nature_vs_cycle_length_dir.cfg
    cd $dataset_dir; mkdir lists
    touch ensemble_runs.dat
    cp $ens_free_vs_cycle_length_dir/ensemble_runs.dat .

    # Send ens_assi runs
    readarray ens_free_runs < ensemble_runs.dat
    nprocess=0
    for ens_free_name in ${ens_free_runs[@]}; do
        export cycle_length=$(cat $(ens_free_dir)/config/cycle_length.cfg)

        ens_assi_name=$ens_free_name
        echo " Ens assi run $ens_assi_name"
        echo $ens_assi_name >> ensemble_runs.dat
        { time -p ens_assi_run.sh $(ens_assi_dir) $(ens_free_dir); } \
            &> ${ens_assi_name}_report.txt &
        (( nprocess+=1 )); [ $(( nprocess%cpus )) == 0 ] && wait
    done
    wait

    cat ensemble_runs.dat | xargs -I% mv %_report.txt $dataset_dir/%

    # Add metainfo
    cd $dataset_dir
    echo "cycle_length" > par_name.cfg
    cp $nature_vs_cycle_length_dir/config/cycle_length_span.cfg par_span.cfg
    echo "$ens_free_vs_cycle_length_dir"> ens_free_vs_cycle_length_dir.cfg
    echo "$FUNCNAME"    > dataset_kind.cfg
    echo "assi"         > run_mode.cfg
    mkdir config; mv *.cfg config

    funct_closing 3
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source ${COM_DAS_DIR}/common_das_tools.sh
ens_assi_vs_cycle_length $@
exit $?
