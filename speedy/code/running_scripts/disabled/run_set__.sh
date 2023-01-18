#!/usr/bin/env bash

config_run_set_parameters(){

    if [[ -n ${ext_par1_name:-} ]] && [[ -n ${ext_par2_name:-} ]]; then
        ext_pars=2
    elif [[ -n ${ext_par1_name:-} ]]; then
        ext_pars=1
    else
        ext_pars=0
    fi

    dataset_dim=$(($ref_set_dim + $ext_pars))
    [[ $dataset_dim -gt 2 ]] &&  error "Unsupported run set dimension $dataset_dim"

    case $ref_set_dim in
        0)
            case $ext_pars in
                1)
                    par1_name=$ext_par1_name
                    par1_span=$ext_par1_span
                    par1_unit=$ext_par1_unit
                    ref_run_dir(){ echo "$ref_set_dir"; }
                    ;;
                2)
                    par1_name=$ext_par1_name
                    par1_span=$ext_par1_span
                    par1_unit=$ext_par1_unit
                    par2_name=$ext_par2_name
                    par2_span=$ext_par2_span
                    par2_unit=$ext_par2_unit
                    ref_run_dir(){ echo "$ref_set_dir"; }
                    ;;
                *)  error "Unsupported ref_set_dim-ext_pars combination ${ref_set_dim}-${ext_pars}";;
            esac
            ;;
        1)
            case $ext_pars in
                1)
                    par1_name=$ext_par1_name
                    par1_span=$ext_par1_span
                    par1_unit=$ext_par1_unit
                    read par2_name < $ref_set_dir/config/par1_name.cfg
                    read par2_span < $ref_set_dir/config/par1_span.cfg
                    read par2_unit < $ref_set_dir/config/par1_unit.cfg
                    ref_run_dir (){ echo "$ref_set_dir/runs/${par2_name}-${par2_value}"; }
                    ;;
                0)
                    read par1_name < $ref_set_dir/config/par1_name.cfg
                    read par1_span < $ref_set_dir/config/par1_span.cfg
                    read par1_unit < $ref_set_dir/config/par1_unit.cfg
                    ref_run_dir (){ echo "$ref_set_dir/runs/${par1_name}-${par1_value}"; }
                    ;;
                *) error "Unsupported ref_set_dim-ext_pars combination ${ref_set_dim}-${ext_pars}";;
            esac
            ;;
        2)
            read par1_name < $ref_set_dir/config/par1_name.cfg
            read par1_span < $ref_set_dir/config/par1_span.cfg
            read par1_unit < $ref_set_dir/config/par1_unit.cfg
            read par2_name < $ref_set_dir/config/par2_name.cfg
            read par2_span < $ref_set_dir/config/par2_span.cfg
            read par2_unit < $ref_set_dir/config/par2_unit.cfg
            ref_run_dir (){ echo "$ref_set_dir/runs/$(run_name)"; }
            ;;
        *)
            error "Unsupported run_free_set_dim $run_free_set_dim";;
    esac
}

calc_nature_run_set(){
    funct_opening 2

    echo " Creating a set of nature runs"
    cd "$dataset_dir"

    loop_core(){
        echo $(run_name) >> config/run_names.cfg
        # echo $(run_name) >> ../logs/calc_consumption.log

        echo $i1 >> config/par1_value_pos.cfg
        [[ $dataset_dim -eq 2 ]] && echo $i2 >> config/par2_value_pos.cfg

        single_calc_run_command >/dev/null &
        run_pid=$!

        pids[$i_run]=$run_pid
        echo " run $(run_name) [PID ${pids[$i_run]}]"

        if (( i_run%cpus == 0 )); then
            wait_for_child_processes #"strict"
        fi
    }
    loop_over_set
    wait_for_child_processes #"strict" # of an incomplete batch

    # Distrubute run calc_consumption.logs
    echo "Run ${task} consumption" > logs/${task}_consumption.log
    loop_core(){
        cat "$(run_name).log" >> logs/${task}_consumption.log
        cat "$(run_name).log" > \
            "$(run_dir)/logs/${task}_consumption.log"
        rm  "$(run_name).log"
  #------------------------------------------------------------------
  # next line is simpler, but in some systems mv shows permission problems
  #  mv "$(run_name).log" "$(run_dir)/logs/calc_consumption.log";
  #------------------------------------------------------------------
    }
    loop_over_set

    funct_closing 2
}

calc_free_run_set(){
    funct_opening 2

    echo " Creating a set of free ensemble runs"
    calc_ens_run_set

    funct_closing 2
}

calc_assi_run_set(){
    funct_opening 2

    echo " Creating a set of ensemble runs with data assimilation"
    calc_ens_run_set

    funct_closing 2
}

calc_ens_run_set(){
    #funct_opening 2

    loop_core(){
        echo " - $(run_name)"
        echo $(run_name) >> config/run_names.cfg
        # echo $(run_name) >> ../logs/calc_consumption.log

        echo $i1 >> config/par1_value_pos.cfg
        [[ $dataset_dim -eq 2 ]] && echo $i2 >> config/par2_value_pos.cfg

        single_calc_run_command > /dev/null

    }
    loop_over_set

    # Distrubute run calc_consumption.logs
    loop_core(){
        mv "$(run_name).log" "$(run_dir)/logs/calc_consumption.log"; }
    loop_over_set

    #funct_closing 2
}

docu_run_set_state(){
    state_name=$1
    time_stat=$2

    state_vars=("u" "v" "t" "q")
    diags=('vertmean' 'zonmean')
    plot_prefix="${state_name}_grid_sigma"

    par1_values=(${cfg[par1_values]})
    par1_span_length=${cfg[par1_span_length]}
    par1_name=${cfg[par1_name]}

    for var in ${state_vars[@]}; do
        for Tkind in Taver Insta Tanom;do
#            section_title "$(Tkind_l $Tkind) $(Quantity_l "$plot_prefix")"
#            echo "\centerline{\LARGE\bf $title}"     >> $texfile
#            echo "\clearpage"   >> $texfile
            echo "\begin{minipage}[c]{\textwidth}"   >> $texfile
            section_title "\url{$state_name} $(Tkind_l $Tkind) $var \url{$time_stat}"
            for diag in ${diags[@]}; do
                scalar=${plot_prefix}_${Tkind}_${time_stat}_${diag}
                plot_path(){ echo "./runs/$(run_name)/plots/${scalar}_${var}.pdf"; }

                case "$diag" in
                    vertmean) array_width="0.50\textwidth" ;;
                    zonmean ) array_width="0.24\textwidth" ;;
                esac

                case "${cfg[dataset_dim]}" in
                    1) plot_array_vertical "" ;;
                    2) plot_array_bunch2D  "$(Ephase_l "$scalar")" ;;
                    *) error "Unsupported bunch_dim ${cfg[bunch_dim]}";;
                esac
            done
#             echo "\hfill"   >> $texfile
            echo "\end{minipage}"   >> $texfile
            #newpage
        done

    done
}

docu_run_set_stats_vs_time(){
    state_name=$1
#    time_stat=$2

    state_vars=("u" "v" "t" "q")
#    for diag in ${diags[@]}; do
    diag="fldstd1_vertmean"
    plot_prefix="${state_name}_grid_sigma"
    array_width="0.80\textwidth"

    par1_values=(${cfg[par1_values]})
    par1_span_length=${cfg[par1_span_length]}
    par1_name=${cfg[par1_name]}

    for var in ${state_vars[@]}; do
        echo "\begin{minipage}[c]{\textwidth}"   >> $texfile
        section_title "\url{$state_name} $var \url{$time_stat}"
        scalar=${plot_prefix}_${diag}_${var}
        plot_path(){ echo "./runs/$(run_name)/plots/${scalar}.pdf"; }
        plot_array_vertical ""
        echo "\end{minipage}"   >> $texfile
    done
}

loop_over_set(){
    i_run=1;
    case ${cfg[dataset_dim]} in
        1)
            i1=0
            for par1_value in ${cfg[par1_values]};do
                eval "export ${cfg[par1_name]}=\"$par1_value\""
                eval "export ${cfg[par1_name]}_unit=\"${cfg[par1_unit]}\""
                loop_core
                (( i1+=1 )); (( i_run+=1 ))
            done
            unset i1
            ;;
        2)
            i2=0
            for par2_value in ${cfg[par2_values]};do
                eval "export ${cfg[par2_name]}=\"$par2_value\""
                eval "export ${cfg[par2_name]}_unit=\"${cfg[par2_unit]}\""
                i1=0
                for par1_value in ${cfg[par1_values]};do
                    eval "export ${cfg[par1_name]}=\"$par1_value\""
                    eval "export ${cfg[par1_name]}_unit=\"${cfg[par1_unit]}\""
                    loop_core
                    (( i_run+=1 )); (( i1+=1 ))
                done
                unset i1
                (( i2+=1 ))
            done
            unset i2
            ;;
        *) error "Unsupported run_set_dim ${cfg[dataset_dim]}" ;;
    esac
    unset -f loop_core
    unset i_run
}

run_name(){
    case ${cfg[dataset_dim]} in
        1) echo "${cfg[par1_name]}-${par1_value}";;
        2) echo "${cfg[par1_name]}-${par1_value}_${cfg[par2_name]}-${par2_value}";;
    esac
}

run_dir(){
    echo "runs/$(run_name)"
}

ref_run_dir (){
    echo "$ref_set_dir/runs/${par1_name}-${par1_value}";
}

single_run_command(){
    case $task in
        calc)
            single_run_calc_command ;;
        stat|plot)
            my_time -o "$(run_name)_${task}.log" \
                run.sh --task=$task --verbose=$run_verbose \
                "$(run_dir)"
            #--verbose=$set_verbose \
            ;;
        *    ) error "unsupported task $task";;
    esac
}

single_run_calc_command(){
  
    echo $(run_name) >> config/run_names.cfg
    # echo $(run_name) >> ../logs/calc_consumption.log
    echo     $i1 >> config/par1_value_pos.cfg
    if [[ $dataset_dim -eq 2 ]];then
        echo $i2 >> config/par2_value_pos.cfg
    fi

    case ${cfg[run_type]} in
        "nature")
            my_time  -o "$(run_name)_${task}.log" \
                run.sh --run_type=nature --task=calc \
                --spinup_dir="${cfg[spinup_dir]}" "$(run_dir)"
            ;;
        "free")
            my_time  -o "$(run_name)_${task}.log" \
                run.sh --run_type=free --task=calc \
                --sampling_dir="${cfg[sampling_dir]}" \
                --nature_dir="$(ref_run_dir)" "$(run_dir)"
            ;;
        "assi")
            my_time  -o "$(run_name)_${task}.log" \
                run.sh --run_type=assi --task=calc \
                --free_run_dir="$(ref_run_dir)" \
                "$(run_dir)"
            ;;
        *     ) error "unsupported run_mode ${cfg[run_mode]}";;
    esac
}

perform_over_runs(){
    funct_opening 1

    local task=$1
    local order=$2

    cd $dataset_dir
    echo " task = $task"
    echo "Run ${task} consumption" > logs/${task}_consumption.log

    case $order in
        "serial")
            loop_core(){
                echo " - $(run_name)" 1>&2
                single_run_command > "$(run_name).log" || echo "$(run_name) crashed" 1>&2
#                single_run_command >/dev/null
            }
            loop_over_set
            ;;
        "parallel")
            loop_core(){
                single_run_command >/dev/null &
                run_pid=$!

                pids[$i_run]=$run_pid
                echo " - run $(run_name) [PID ${pids[$i_run]}]"

                if (( i_run%cpus == 0 )); then
                    wait_for_child_processes #"strict"
                fi
            }
            loop_over_set
            wait_for_child_processes #"strict" # of an incomplete batch
            ;;
        *   ) error "unsupported order $order";;
    esac

    # Distrubute run consumption logs
    echo "Run ${task} consumption" > logs/${task}_consumption.log
    loop_core(){
        cat "$(run_name)_${task}.log" >> logs/${task}_consumption.log
        cat "$(run_name)_${task}.log" > \
            "$(run_dir)/logs/${task}_consumption.log"
        rm  "$(run_name)_${task}.log"
  #------------------------------------------------------------------
  # next line is simpler, but in some systems mv shows permission problems
  #  mv "$(run_name).log" "$(run_dir)/logs/calc_consumption.log";
  #------------------------------------------------------------------
    }
    loop_over_set

    funct_closing 1
}
