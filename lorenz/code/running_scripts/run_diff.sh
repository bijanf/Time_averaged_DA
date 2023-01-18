#!/usr/bin/env bash
set -eu # strict config (non-zero returns and unset vars halt script)
source $DAS_DIR/common_tools.sh
source $DAS_DIR/common_metainfo_tools.sh
source $DAS_DIR/run_tools.sh

usage(){ cat <<EOF
Compare to 2 conformable runs with the same run_mode.
Usage: $(basename $0) [--run_A=*] [--run_B=*] [--stat=*] [--plot=*]
                      [-h|--help] run_diff_dir
*/^: Required/optional argument
EOF
exit 1
}

PARSED_ARGS=$(getopt -n "$0" -o h --long "help,run_A:,run_B:,stat:,plot:" -- "$@")
eval set -- "$PARSED_ARGS"
while true; do
    case "$1" in
        -h|--help)         usage            ;;
        --run_A  ) run_A_dir=$2; shift 2;;
        --run_B  ) run_B_dir=$2; shift 2;;
        --stat   )     stat_flag=$2; shift 2;;
        --plot   )     plot_flag=$2; shift 2;;
        --       )         shift 1 ; break  ;;
    esac
done

dataset_dir=$1

run_diff(){
    funct_opening 3

    dataset_dir="$(absolute_path "$dataset_dir")"

    perform_simulation_tasks

    funct_closing 3
}

calc_function(){
    funct_opening 2

    run_A_dir="$(absolute_path "$run_A_dir")"
    run_B_dir="$(absolute_path "$run_B_dir")"
    read run_A_type < $run_A_dir/config/dataset_kind.cfg
    read run_A_mode < $run_A_dir/config/run_mode.cfg
    read run_B_type < $run_B_dir/config/dataset_kind.cfg
    read run_B_mode < $run_B_dir/config/run_mode.cfg
    [[ $run_A_type == "run" ]] || error "dataset_kind mismatch"
    [[ $run_B_type == "run" ]] || error "dataset_kind mismatch"
    [[ $run_A_mode == $run_B_mode ]] || error "run_mode mismatch"


    cp -r $run_A_dir/config .
    get_config $dataset_dir
    dataset_kind="run_diff"
    set_par_file "dataset_kind"; mv *.cfg config

    rm -fr stats; mkdir stats; cd stats

    find_reduction_stats(){

        # error reduction
        Reduc \
            "$run_A_dir/stats/${run_mode}_error_Tstdd_${Ephase}_${Tkind}" \
            "$run_B_dir/stats/${run_mode}_error_Tstdd_${Ephase}_${Tkind}" \
            "${run_mode}_error_Tstdd_reduc_${Ephase}_${Tkind}"
        Fmean \
            "${run_mode}_error_Tstdd_reduc_${Ephase}_${Tkind}" \
            "${run_mode}_error_Tstdd_reduc_Fmean_${Ephase}_${Tkind}"

        # Spread reduction
        Reduc \
            "$run_A_dir/stats/${run_mode}_Esprd_Tmean_${Ephase}_${Tkind}" \
            "$run_B_dir/stats/${run_mode}_Esprd_Tmean_${Ephase}_${Tkind}" \
            "${run_mode}_Esprd_Tmean_reduc_${Ephase}_${Tkind}"
        Fmean \
            "${run_mode}_Esprd_Tmean_reduc_${Ephase}_${Tkind}" \
            "${run_mode}_Esprd_Tmean_reduc_Fmean_${Ephase}_${Tkind}"

        # variance reduction
        Reduc \
            "$run_A_dir/stats/${run_mode}_Emean_Tvar_${Ephase}_${Tkind}" \
            "$run_B_dir/stats/${run_mode}_Emean_Tvar_${Ephase}_${Tkind}" \
            "${run_mode}_Emean_Tvar_reduc_${Ephase}_${Tkind}"
        Fmean \
            "${run_mode}_Emean_Tvar_reduc_${Ephase}_${Tkind}" \
            "${run_mode}_Emean_Tvar_reduc_Fmean_${Ephase}_${Tkind}"
    }

    set_reduction_stats_to_NaN(){
	echo "9.96921e+36f" > \
	    "${run_mode}_error_Tstdd_reduc_Fmean_${Ephase}_${Tkind}.dat"
        echo "9.96921e+36f" > \
	    "${run_mode}_Esprd_Tmean_reduc_Fmean_${Ephase}_${Tkind}.dat"
        echo "9.96921e+36f" > \
	    "${run_mode}_Emean_Tvar_reduc_Fmean_${Ephase}_${Tkind}.dat"
    }

    case $run_mode in
        "free") Ephases=(prior);;
        "assi") Ephases=(prior postr);;
    esac

    # if any of the runs crashed all stats become NaN
    if [[ -e "$run_A_dir/config/run_crashed.cfg" ]] || \
        [[ -e "$run_B_dir/config/run_crashed.cfg" ]]; then
	for Ephase in ${Ephases[@]}; do
            for Tkind in Insta Taver Tanom; do
		set_reduction_stats_to_NaN
            done
	done
    else
	for Ephase in ${Ephases[@]}; do
            for Tkind in Insta Taver Tanom; do
		find_reduction_stats
            done
	done
    fi

    print_statistics

    funct_closing 2
}

stat_function(){
    funct_opening 2
    echo " No statistic tasks for this script"
    funct_closing 2
}

plot_function(){
    funct_opening 2
    echo " No plotting tasks for this script"
    funct_closing 2
}

docu_function(){
    funct_opening 2
    echo " No documentation tasks implemented for this script"
    funct_closing 2
}

run_diff $@
exit $?
