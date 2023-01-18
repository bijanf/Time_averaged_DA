#!/usr/bin/env bash
set -eu # strict config (non-zero returns and unset vars halt script)
source $CODE_DIR/running_scripts/common_tools.sh
source $CODE_DIR/running_scripts/run_set__tools_Level1.sh
source $CODE_DIR/running_scripts/run_set__tools_Level2.sh
declare -A cfg flag

usage(){ cat << EOF
Create 1D- or 2D-set of runs (nature,free and assi ensemble runs).
Usage: $(basename $0) [--run_mode=*] [--ref_dir=*] [--par1=^] [--span1=^]
    [--par2=^] [--span2=^] [--calc=*] [--stat=*] [--task=*] [--no_single]
[-h|--help] dataset_dir
*/^: Required/optional argument
EOF
exit 1
}

#=======================================================================
#> @brief Runs create_nature_run for different time_steps using
#>        appropiate nature names
#>
#> Usage example:
#=======================================================================

run_set(){
    long_opts="help,run_type:,ref_set_dir:,spinup_dir:,sampling_dir:,par1_name::,par1_span::,par1_unit::,par2_name::,par2_span::,par2_unit::,calc:,stat:,plot:,task:,calc_runs:,stat_runs:,plot_runs:,replot::,keep_raw_data::,detailed_stats::,verbose:,run_verbose:"
    PARSED_ARGS=$(getopt -n "$0"  -o h --long "$long_opts" -- "$@")
    eval set -- "$PARSED_ARGS"

    while true; do
        case "$1" in
            -h|--help       )                      usage  ;;
            --run_type      )         run_type=$2; shift 2;;
            --ref_set_dir   )      ref_set_dir=$2; shift 2;;
            --spinup_dir    )       spinup_dir=$2; shift 2;;
            --sampling_dir  )     sampling_dir=$2; shift 2;;
            --par1_name     )    ext_par1_name=$2; shift 2;;
            --par1_span     )    ext_par1_span=$2; shift 2;;
            --par1_unit     )    ext_par1_unit=$2; shift 2;;
            --par2_name     )    ext_par2_name=$2; shift 2;;
            --par2_span     )    ext_par2_span=$2; shift 2;;
            --par2_unit     )    ext_par2_unit=$2; shift 2;;
            --calc          )        calc_flag=$2; shift 2;;
            --stat          )        stat_flag=$2; shift 2;;
            --plot          )        plot_flag=$2; shift 2;;
            --calc_runs     )   calc_runs_flag=$2; shift 2;;
            --stat_runs     )   stat_runs_flag=$2; shift 2;;
            --plot_runs     )   plot_runs_flag=$2; shift 2;;
            --replot        )           replot=$2; shift 2;;
            --task          )        only_task=$2; shift 2;;
            --keep_raw_data )    keep_raw_data=$2; shift 2;;
            --detailed_stats)   detailed_stats=$2; shift 2;;
            --verbose       )          verbose=$2; shift 2;;
            --run_verbose   )      run_verbose=$2; shift 2;;
            --              )             shift 1; break  ;;
        esac
    done
    dataset_dir=$1
    # dataset_dir="$(absolute_path "$dataset_dir")"

    calc_runs_flag=${calc_runs_flag:-yes}
    stat_runs_flag=${stat_runs_flag:-yes}
    plot_runs_flag=${plot_runs_flag:-yes}
    run_verbose=${run_verbose:-0}
    dataset_kind=$(barename $0)
    keep_raw_data=${keep_raw_data:-no}
    detailed_stats=${detailed_stats:-no}
    verbose=${verbose:-1}

    case "$verbose" in
        2) exec 3>&1         ;;
        1) exec 3>/dev/null  ;;
        0) exec 3>/dev/null  ;;
    esac

    [[ -n ${replot:-} ]] && \
        rm -f "$dataset_dir/logs/plot.success" "$dataset_dir/logs/docu.success"

    perform_simulation_tasks
}

config_function(){
    funct_opening 2

    add_to_cfg "dataset_kind" "run_type" "dataset_dir" "model"

    keep_raw_data=${keep_raw_data:-no}
    detailed_stats=${detailed_stats:-no}
    add_to_cfg "keep_raw_data" "detailed_stats"

    case ${run_type} in
        nature) add_to_cfg "run_length_unit" "run_length" "spinup_dir"
            ref_set_dim=0
            ;;
        free  ) add_to_cfg "sampling_dir" "ref_set_dir"
            [[ -d "$ref_set_dir" ]] || error "Non existent ref_set_dir $ref_set_dir"
            read ref_set_dim < $ref_set_dir/config/dataset_dim.cfg
            ;;
        assi  ) add_to_cfg "ref_set_dir"
            [[ -d "$ref_set_dir" ]] || error "Non existent ref_set_dir $ref_set_dir"
            read ref_set_dim < $ref_set_dir/config/dataset_dim.cfg
            ;;
#           |free|assi) config_${run_type}_run_set ;;
        *     ) error "unsupported run_mode ${cfg[run_type]}";;
    esac

    config_run_set_parameters
    add_to_cfg "ext_pars" "verity" "dataset_dim" #"diag"

    par1_label="$(long_name ${par1_name})"
    add_to_cfg "par1_name" "par1_span" "par1_unit" "par1_label"
    set_par_values_file "par1" "$par1_span"
    if [[ $dataset_dim -eq 2 ]]; then
        par2_label="$(long_name ${par2_name})"
        add_to_cfg "par2_name" "par2_span" "par2_unit" "par2_label"
        set_par_values_file "par2" "$par2_span"
    fi

    store_cfg;  print_cfg

    scp "$MODEL_DIR/model_config.sh" ./logs

    funct_closing 2
}

calc_function(){
    funct_opening 2


    case ${cfg[run_type]} in
        nature   ) order="parallel" ;;
        free|assi) order="serial" ;;
        *        ) error "unsupported run_type ${run_type}";;
    esac
    perform_over_runs "calc" $order
#     case ${cfg[run_type]} in
#         nature|free|assi) calc_${cfg[run_type]}_run_set ;;
#         *     ) error "unsupported run_mode ${run_type}";;
#     esac

    funct_closing 2
}

stat_function(){
    funct_opening 2

    [[ -f logs/calc.success ]] || error "stat phase was unseccessful"

    # find_successful_runs
    rm -f config/successful_runs.cfg config/crashed_runs.cfg
    loop_core(){
        if [[ -e "./runs/$(run_name)/config/run_crashed.cfg" ]]; then
            echo $(run_name) >> config/crashed_runs.cfg
        else
            echo $(run_name) >> config/successful_runs.cfg
        fi
    }
    loop_over_set
    get_config_file "config/successful_runs.cfg"

    if [[ -n ${cfg[successful_runs]:-} ]];then
        local successful_runs=(${cfg[successful_runs]})

#        [[ $stat_runs_flag == yes ]] && perform_task_on_runs "stat"
        [[ $stat_runs_flag == yes ]] && perform_over_runs "stat" "parallel"

        # wait
        # sleep 5s # Apparently CDO output needs some time to settle down

        # unify !!!!!!!!
        # gather_run_set_stats
        success_run_dir="$dataset_dir/runs/${successful_runs[0]}/stats"
        scalars=($(find "$success_run_dir" -name "*.dat"))
        if [[ -z ${scalars[@]:-} ]];then
            echo "no scalars to gather"
            return 0
        fi
        barename ${scalars[@]} > "$dataset_dir/config/scalars.cfg"
        get_config_file "$dataset_dir/config/scalars.cfg"

        rm -fr stats; mcd stats

        i_run=1
        for scalar_name in ${cfg[scalars]}; do
            echo " - ${scalar_name}.nc"
            declare -a opts
            opts+=($(printf '%s=\"%s\"' scalar_name $scalar_name))
            opts+=($(printf '%s=\"%s\"' cfg_dir     $dataset_dir/config))
            my_ncl --logfile="${scalar_name}.ncl_log" \
                ${opts[@]} $CODE_DIR/running_scripts/create_netcdf_scalar.ncl &
            pids[$i_run]=$!
            unset opts
            (( i_run%cpus == 0 )) && wait_for_child_processes "strict"
            (( i_run+=1 ))
        done
        wait_for_child_processes "strict" # of an incomplete batch

    else
        echo " No successful runs to stat" 1>&2
    fi

    funct_closing 2
}

plot_function(){
    funct_opening 2

    rm -fr plots; mcd plots

    [[ $plot_runs_flag == yes ]] && perform_over_runs "plot" "parallel"

    #matlab -r "$CODE_DIR/plotting/plot_dataset.m" #> /dev/null
    #matlab -nosplash -nodesktop -nodisplay < \
    #    "$CODE_DIR/plotting/plot_dataset.m" #> /dev/null

    touch "${dataset_dir}/logs/plot.success"

    funct_closing 2
}

docu_function(){
    funct_opening 1

    [[ -f logs/plot.success ]] || ( plot_function )

    #quality=draft
    orientation=${orientation:-"portrait"}
#     orientation=${orientation:-"landscape"}
    margins="[left=10mm,right=10mm,top=5mm,bottom=20mm]"
    # array_width="0.3\textwidth"
    speedy_vars=("t" "u" "v" "q")

    open_report
    report_title "$(label ${cfg[run_type]} run) Run Set"

#     report_metainfo
    cfg_dir="./config"
    report_config "Configuration"

    case ${cfg[run_type]} in
        "nature")
            docu_run_set_state 'nature' 'timstd1'
            ;;
        "free")
            docu_run_set_error_spread_vs_time
            #docu_run_set_stats_vs_time 'free_prior_error'
            docu_run_set_state_vertical_zonal_mean 'free_prior_error' 'timstd1'
            docu_run_set_state_vertical_zonal_mean 'free_prior_Esprd' 'timmean'
            #docu_run_set_state_vertical_zonal_mean 'free_prior_Emean' 'timstd1'
            ;;
        "assi")
            docu_run_set_error_spread_vs_time
            #docu_run_set_stats_vs_time 'assi_prior_error'

            docu_run_set_state_vertical_zonal_mean 'assi_postr_error' 'timstd1_reduc'
            docu_run_set_state_vertical_zonal_mean 'assi_prior_error' 'timstd1_reduc'

            docu_run_set_state_vertical_zonal_mean 'assi_postr_error' 'timstd1_incre'
            docu_run_set_state_vertical_zonal_mean 'assi_prior_error' 'timstd1_incre'

            docu_run_set_state_vertical_zonal_mean 'assi_postr_error' 'timstd1'
            docu_run_set_state_vertical_zonal_mean 'assi_prior_error' 'timstd1'

            docu_run_set_state_vertical_zonal_mean 'assi_postr_Esprd' 'timmean'
            docu_run_set_state_vertical_zonal_mean 'assi_prior_Esprd' 'timmean'
            #docu_run_set_state_vertical_zonal_mean 'assi_prior_Emean' 'timstd1'
            ;;
    esac

    include_file "logs/model_config.sh"

    close_report
    my_pdflatex "$texfile"

    funct_closing 1
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
