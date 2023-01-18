#!/usr/bin/env bash
#------------------------------------------------------
#> @brief Create a pair of run sets (very preliminar)
#------------------------------------------------------
run_assi_set_pair(){
    funct_opening 3

    calc="on"; stat="on"; #plot="on"
#    while getopts "m:r:csp" opt; do
    while getopts "r:cs" opt; do
        case $opt in
#            m) local      run_mode="$OPTARG" ;;
            r) local run_free_set_dir="$OPTARG" ;;
            c) stat="off"; plot="off" ;; # calc only
            s) calc="off"; plot="off" ;; # stat only
#            p) calc="off"; stat="off" ;; # plot only
        esac
    done
    shift $((OPTIND-1))

    dataset_dir=$1; shift 1

    [ "$calc" == "on" ] && calc_run_set_pair $@
                #[ "$stat" == "on" ] && stat_run_set_pair

    funct_closing 3
}

calc_run_set_pair(){
    create_archive_dir $dataset_dir
    cd $dataset_dir

    dataset_kind="run_set_pair"
    run_mode="assi"
    run_assi_set_dir(){ echo "$dataset_dir/update_mode_${update_mode}"; }
    run_assi_set_diff_dir="$dataset_dir/update_mode_Hakim-Augm1"
    update_modes=(Hakim Augm1)
#               update_modes=(Augm1)


    # report and save metainfo
    echo " Creating a pair of run sets"
    print_line 0
    set_par_file "dataset_kind"
    set_par_file "run_mode"
    set_par_file "dataset_dir"
#    set_par_file "ext_pars"
    set_par_file "verity"
    set_par_file run_assi_set_diff_dir
    env > env.cfg
    print_line 0


    for update_mode in ${update_modes[@]}; do
        echo " - update_mode = $update_mode"
        export update_mode
        { time -p run_set.sh -m assi -r $run_free_set_dir $(run_assi_set_dir) $@;} &> $(run_assi_set_dir).log
        mv *.log $(run_assi_set_dir)
        echo $(run_assi_set_dir) >> run_set_dirs.cfg
    done


    mkdir config; mv *.cfg config
}

stat_run_set_pair(){
    cd $dataset_dir
    read run_assi_set_diff_dir < config/run_assi_set_diff_dir.cfg
    readarray -t run_set_dirs  < config/run_set_dirs.cfg

    run_set_diff.sh "$run_assi_set_1_dir" "${run_set_dirs[@]}"
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source ${COM_DAS_DIR}/common_das_tools.sh
run_assi_set_pair $@
exit $?
