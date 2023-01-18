#!/usr/bin/env bash
#set -x
#set -v
ens_free_vs_cycle_length(){
    funct_opening 3
    dataset_dir=$1
    nature_vs_cycle_length_dir=$2

    echo " Creating ens_free_runs for a set of nature runs"

    ens_free_dir(){ echo $dataset_dir/$ens_free_name; }
    nature_dir()  { echo $nature_vs_cycle_length_dir/$nature_name; }

    # Prepare output dataset
    create_archive_dir $dataset_dir || return 0
    cd $dataset_dir; mkdir lists
    touch ensemble_runs.dat
    cp $nature_vs_cycle_length_dir/nature_runs.dat .

    # Send ens_free runs
    readarray nature_runs < nature_runs.dat
    nprocess=0
    for nature_name in ${nature_runs[@]}; do
        export cycle_length=$(cat $(nature_dir)/config/cycle_length.cfg)

        ens_free_name=$nature_name
        echo " Ens free run $ens_free_name"
        echo $ens_free_name >> ensemble_runs.dat
        { time -p ens_free_run.sh $(ens_free_dir) $(nature_dir); } \
            &> ${ens_free_name}_report.txt &
        (( nprocess+=1 )); [ $(( nprocess%cpus )) == 0 ] && wait
    done
    wait
    cat ensemble_runs.dat | xargs -I% mv %_report.txt $dataset_dir/%

    # Add metainfo
    cd $dataset_dir
    echo "cycle_length" > par_name.cfg
    cp $nature_vs_cycle_length_dir/config/cycle_length_span.cfg .
    echo $nature_vs_cycle_length_dir > nature_vs_cycle_length_dir.cfg
    echo "$FUNCNAME"    > dataset_kind.cfg
    echo "free"         > run_mode.cfg
    mkdir config; mv *.cfg config

    funct_closing 3
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source ${COM_DAS_DIR}/common_das_tools.sh
ens_free_vs_cycle_length $@
exit $?
