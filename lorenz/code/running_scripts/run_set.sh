#!/usr/bin/env bash
set -eu # strict config (non-zero returns and unset vars halt script)
source $DAS_DIR/common_tools.sh
source $DAS_DIR/run_set__1.sh
# source $CODE_DIR/das_model_config.sh

declare -A cfg flag

usage(){ cat <<EOF
Create 1D- or 2D-set of runs.
Usage: $(basename $0) [--run_mode=*] [--ref_dir=*] [--par1=^] [--span1=^]
    [--par2=^] [--span2=^] [--calc=*] [--stat=*] [--task=*] [--no_single]
[-h|--help] dataset_dir
*/^: Required/optional argument
EOF
exit 1
}

run_set(){
    # echo " Creating a set of runs"

    long_opts="help,run_mode:,ref_dir:,par1::,span1::,par2::,span2::,calc:,stat:,plot:,task:,calc_runs:,stat_runs:,plot_runs:,replot:,keep_raw_data::,detailed_stats::,verbose:,run_verbose:"
    PARSED_ARGS=$(getopt -n "$0"  -o h --long "$long_opts" -- "$@")
    eval set -- "$PARSED_ARGS"

    while true; do
        case "$1" in
            -h|--help       )                    usage  ;;
            --ref_dir       )  reference_dir=$2; shift 2;;
            --run_mode      )       run_mode=$2; shift 2;;
            --par1          )  ext_par1_name=$2; shift 2;;
            --span1         )  ext_par1_span=$2; shift 2;;
            --par2          )  ext_par2_name=$2; shift 2;;
            --span2         )  ext_par2_span=$2; shift 2;;
            --calc          )     flag[calc]=$2; shift 2;;
            --stat          )     flag[stat]=$2; shift 2;;
            --plot          )     flag[plot]=$2; shift 2;;
            --docu          )     flag[docu]=$2; shift 2;;
            --view          )     flag[view]=$2; shift 2;;
            --task          )      only_task=$2; shift 2;;
            --calc_runs     ) calc_runs_flag=$2; shift 2;;
            --stat_runs     ) stat_runs_flag=$2; shift 2;;
            --plot_runs     ) plot_runs_flag=$2; shift 2;;
            --replot        )         replot=$2; shift 2;;
            --keep_raw_data )  keep_raw_data=$2; shift 2;;
            --detailed_stats) detailed_stats=$2; shift 2;;
            --verbose       )        verbose=$2; shift 2;;
            --run_verbose   )    run_verbose=$2; shift 2;;
            --              )           shift 1; break  ;;
        esac
    done
    dataset_dir=$1

    dataset_dir="$(absolute_path "$dataset_dir")"

    calc_runs_flag=${calc_runs_flag:-yes}
    stat_runs_flag=${stat_runs_flag:-yes}
    plot_runs_flag=${plot_runs_flag:-no}
    run_verbose=${run_verbose:-0}
    verbose=${verbose:-0}

    dataset_kind=$(barename $0)

    [[ -n ${replot:-} ]] && \
        rm -f "$dataset_dir/logs/plot.success" "$dataset_dir/logs/docu.success"

    perform_simulation_tasks
}

config_function(){
    funct_opening 2

    add_to_cfg "dataset_kind" "run_mode" "model"

    if [[ -n ${ext_par1_name:-} ]] && [[ -n ${ext_par2_name:-} ]]; then
        ext_pars=2
    elif [[ -n ${ext_par1_name:-} ]]; then
        ext_pars=1
    else
        ext_pars=0
    fi

    keep_raw_data=${keep_raw_data:-no}
    detailed_stats=${detailed_stats:-no}

    case ${cfg[run_mode]} in
        "free") config_run_free_set ;;
        "assi") config_run_assi_set ;;
        *     ) error "unsupported run_mode ${cfg[run_mode]}";;
    esac

    add_to_cfg "dataset_dir" "keep_raw_data" "detailed_stats"
    add_to_cfg "ext_pars" "verity" "dataset_dim" "diag"

    if [[ ${cfg[run_mode]} == "assi" ]]; then
        rel_reference_dir=$(relpath $reference_dir $dataset_dir )
        add_to_cfg "run_free_set_dim" "reference_dir" "rel_reference_dir"
    fi

    par1_label="$(long_name ${par1_name})"
    add_to_cfg "par1_name" "par1_span" "par1_label"
    set_par_values_file "par1" "$par1_span"
    if [[ $dataset_dim -eq 2 ]]; then
        par2_label="$(long_name ${par2_name})"
        add_to_cfg "par2_name" "par2_span" "par2_label"
        set_par_values_file "par2" "$par2_span"
    fi

    store_cfg;  print_cfg
    env > logs/env.log
    find $CODE_DIR/models/$model -maxdepth 1 ! -type d -not -name ".*" \
        | xargs -I{} cp {} ./logs

    funct_closing 2
}

calc_function(){
    funct_opening 2

    mkdir runs

    [[ $calc_runs_flag == yes ]] && calc_runs

    funct_closing 2
}

stat_function(){
    funct_opening 2

    [[ -d "$dataset_dir/runs" ]] || error "runs_data not present"

    # [[ -f logs/calc.success ]] || error "calc phase was unseccessful"

    # case "$(hostname)" in
    #     # apparently calc03/04 need some  time to settle down after calc phase
    #     calc03 |calc04 ) sleep 60s ;;
    # esac

    find_successful_runs

    if [[ -n ${cfg[successful_runs]:-} ]];then
        local successful_runs=(${cfg[successful_runs]})

        [[ $stat_runs_flag == yes ]] && stat_runs

        gather_run_set_stats
    else
        echo " No successful runs to stat" 1>&2
    fi

    funct_closing 2
}

plot_function(){
    funct_opening 2

    # [[ -f logs/stat.success ]] || run_set.sh --task=stat "$dataset_dir"
    [[ -f logs/stat.success ]] || error "stat phase was unseccessful"


    if [[ ${cfg[par1_name]} == "obs_operator" ]] || \
        [[ ${cfg[par1_name]} == "update_mode" ]] || \
        [[ ${cfg[par1_name]} == "SNR" ]]; then
        cd $dataset_dir; rm -fr plots; mcd plots
        matlab -nosplash -nodesktop -nodisplay < \
            $CODE_DIR/plotting/plot_dataset.m #> /dev/null
        return 0
    fi

    if [[ -n ${cfg[successful_runs]:-} ]];then
        local successful_runs=(${cfg[successful_runs]})

        [[ $plot_runs_flag == yes ]] && plot_runs

        plot_run_set

        #[[ ${cfg[detailed_stats]} == "yes" ]] && plot_run_set_detailed

        [[ ${cfg[run_mode]} == "assi" ]] &&     link_free_run_set_plots

    else
        echo " All runs crashed. No plots to create"
    fi


    funct_closing 2
}

docu_function(){
    funct_opening 2

    [[ -f logs/plot.success ]] || \
        run_set.sh --task=plot "$dataset_dir" && sleep 10s

    [[ -n ${cfg[report_name]:-} ]] && texfile="${cfg[report_name]}.tex"

    # currently only assi run sets can be reported

    if [[ ${cfg[run_mode]} == "assi" ]]; then

        if [[ ${cfg[par1_name]} == "obs_operator" ]] || \
            [[ ${cfg[par1_name]} == "update_mode" ]]; then
            universal_report
            return 0
        fi

        #link_free_run_set_plots
        case ${cfg[dataset_dim]} in
            1) report_run_set1D ;;
            2) report_run_set2D ;;
        esac
    fi

    funct_closing 2
}

clean_function(){
    funct_opening 2

    cd $dataset_dir
    local run_names=(${cfg[run_names]})
    for runname in ${run_names[@]}; do
        run.sh --task=clean --verbose=1 \
            "$dataset_dir/runs/$runname"
    done

    # run_set.sh --task=clean "${cfg[rel_run_free_set_dir]}"

    # find .  -path "*/runs*" -delete
    # find .  -path "*/code*" -delete
    # find .. -path "*/bin*"  -delete
    # find .  -path "*nature_Insta_all.*" -delete # only needed for calc phase and VERY heavy

    funct_closing 2
}

view_function(){
    funct_opening 2

    [[ -f logs/docu.success ]] || ( docu_function )
    report_file=$(find . -maxdepth 1 -name "*.pdf")
    [[ -n ${report_file:-} ]] && okular $report_file

    funct_closing 2
}

run_set $@
exit $?

