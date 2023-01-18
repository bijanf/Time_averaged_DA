#!/usr/bin/env bash
set -eu # strict config (non-zero returns and unset vars halt script)
source $DAS_DIR/common_tools.sh
source $DAS_DIR/common_metainfo_tools.sh
source $DAS_DIR/run_set_tools.sh

usage(){ cat <<EOF
Compare to 2 conformable run sets with the same run_mode.
Usage: $(basename $0) [--set_A=*] [--set_B=*] [--stat=*] [--plot=*]
                      [-h|--help] run_set_diff_dir
*/^: Required/optional argument
EOF
exit 1
}
long_opts="help,set_A:,set_B:,stat:,plot:,only:"
PARSED_ARGS=$(getopt -n "$0" -o h --long "$long_opts" -- "$@")
eval set -- "$PARSED_ARGS"
while true; do
    case "$1" in
        -h|--help)         usage            ;;
        --set_A  ) run_set_A_dir=$2; shift 2;;
        --set_B  ) run_set_B_dir=$2; shift 2;;
        --stat   )     stat_flag=$2; shift 2;;
        --plot   )     plot_flag=$2; shift 2;;
        --only   )     only_task=$2; shift 2;;
        --       )         shift 1 ; break  ;;
    esac
done

dataset_dir=$1

run_set_diff(){
    funct_opening 3

    dataset_dir="$(absolute_path "$dataset_dir")"
    expected_dataset_kind="run_set_diff"

    perform_simulation_tasks

    funct_closing 3
}

calc_function(){
    funct_opening 2

    run_set_A_dir="$(absolute_path "$run_set_A_dir")"
    run_set_B_dir="$(absolute_path "$run_set_B_dir")"
    read run_set_A_type < $run_set_A_dir/config/dataset_kind.cfg
    read run_set_B_type < $run_set_B_dir/config/dataset_kind.cfg
    [[ $run_set_A_type == "run_set" ]] || error "dataset_kind mismatch"
    [[ $run_set_B_type == "run_set" ]] || error "dataset_kind mismatch"

    cp -r $run_set_A_dir/config .
    get_config $dataset_dir
    dataset_kind="run_set_diff"
    set_par_file "dataset_kind"; mv *.cfg config

    i_run=1;
    for run_name in "${run_names[@]}"; do
        echo $run_name >> stat_consumption.log
	
        my_time -o stat_consumption.log -a \
        run_diff.sh \
            --run_A="$run_set_A_dir/runs/$run_name" \
            --run_B="$run_set_B_dir/runs/$run_name" \
            "$dataset_dir/runs/$run_name" > /dev/null &
        pids[$i_run]=$!
        echo " run $run_name [PID ${pids[$i_run]}]"

        (( i_run%cpus == 0 )) && wait_for_child_processes "strict"
        (( i_run+=1 ))
    done
    wait_for_child_processes "strict" # of an incomplete batch

    scalars=($(find $dataset_dir/runs/${run_names[0]}/stats -name "*.dat"))
    scalars=(${scalars[@]##*/}); scalars=(${scalars[@]%.*})
    printf "%s\n" "${scalars[@]}" > $dataset_dir/config/scalars.cfg

    rm -fr stats; mkdir stats; cd stats

    print_line 1

    create_scalar_netcdfs

    
    funct_closing 2
}

stat_function(){
    funct_opening 2

    get_config; parse_dataset_kind

    # !! duplicated above !!!!!!!!!!
    scalars=($(find $dataset_dir/runs/${run_names[0]}/stats -name "*.dat"))
    scalars=(${scalars[@]##*/}); scalars=(${scalars[@]%.*})
    printf "%s\n" "${scalars[@]}" > $dataset_dir/config/scalars.cfg

    rm -fr stats; mkdir stats; cd stats

    print_line 1

    create_scalar_netcdfs

    funct_closing 2
}

plot_function(){
    funct_opening 2

    get_config; parse_dataset_kind

    rm -fr plots; mkdir plots; cd plots

    echo " Plotting stats"
    case "$dataset_dim" in
        1)
            cp $DAS_DIR/line_plot.ncl .

            prefixes=( \
                ${run_mode}_error_Tstdd_reduc_Fmean \
                ${run_mode}_Esprd_Tmean_reduc_Fmean \
                ${run_mode}_Emean_Tvar_reduc_Fmean \
                )

            for prefix in ${prefixes[@]}; do
                echo " - $prefix"
                #-------------------------------------------------------------
                #  Plot definition
                plot_name  (){ echo $prefix;}
                plot_title (){ echo "$(Quantity_l $prefix)";}
                line_files=($(find ../stats -name "${prefix}_*.nc"))
                line_label (){
                    case $run_mode in
                        "free") echo "$(Tkind_l "$file")";;
                        "assi") echo "$(Tkind_l "$file") $(Ephase_l "$file")";;
                    esac; }
                declare -a opts
                case "$prefix" in
                    *reduc*)
                        opts+=(min_level=0.0); opts+=(max_level=100.0)
                        ;;
                esac
                create_line_plot &
                #-------------------------------------------------------------
                pids[$i_run]=$! ; (( i_run+=1 ))
                if (( i_run%cpus == 0 )); then
                    wait_for_child_processes "strict"
                fi
            done
            wait_for_child_processes "strict" # of an incomplete batch
            ;;
        2)
            cp $DAS_DIR/surface_plot.ncl .
            for scalar in ${scalars[@]}; do
                echo " - $scalar"
								plot_name=$scalar
								create_surface_plot "$plot_name"
            done
            ;;
    esac

#    report_run_set

    funct_closing 2
}

docu_function(){
    funct_opening 2

    get_config; parse_dataset_kind

    rm -fr docu; mcd docu

    universal_report
    
    funct_closing 2
}

run_set_diff $@
exit $?
