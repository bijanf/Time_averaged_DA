#!/usr/bin/env bash

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
    opts(){ echo "-o $(run_name)_${task}.log";}

    case $task in
        calc)
            single_run_calc_command ;;
        stat)
            my_time $(opts) run.sh --task=$task --verbose=$run_verbose \
                "$(run_dir)" 
            #--verbose=$set_verbose \
            ;;
        plot)
            run_verbose=1
            my_time $(opts) run.sh --task=$task --verbose=$run_verbose \
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
            my_time $(opts) run.sh --run_type=nature --task=calc \
                --spinup_dir="${cfg[spinup_dir]}" "$(run_dir)"
            ;;
        "free")
            my_time $(opts) run.sh --run_type=free --task=calc \
                --sampling_dir="${cfg[sampling_dir]}" \
                --nature_dir="$(ref_run_dir)" "$(run_dir)"
            ;;
        "assi")
            my_time $(opts) run.sh --run_type=assi --task=calc \
                --free_run_dir="$(ref_run_dir)" "$(run_dir)"
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
                single_run_command > "$(run_name)_${task}.log" || echo "$(run_name) crashed" 1>&2
#                single_run_command >/dev/null
            }
            loop_over_set
            ;;
        "parallel")
            loop_core(){
                single_run_command > "$(run_name)_${task}.log" &
                # single_run_command & #>/dev/null &
                run_pid=$!

                pids[$i_run]=$run_pid
                echo " - run $(run_name) [PID ${pids[$i_run]}]"

                if (( i_run%cpus == 0 )); then
                    wait_for_child_processes "strict"
                fi
            }
            loop_over_set
            wait_for_child_processes "strict" # of an incomplete batch
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



plot_array_vertical_par1(){
    scalar=$1; array_title="${2:-}"

    par1_values=(${cfg[par1_values]})
    par1_span_length=${cfg[par1_span_length]}
    par1_name=${cfg[par1_name]}

    plot_path(){ echo "./runs/$(run_name)/plots/${scalar}.pdf"; }
    array_width=${array_width:-"0.80\textwidth"}
    plot_array_vertical "$array_title"
}

docu_run_set_state_vertical_zonal_mean(){
    state_name=$1; time_stat=$2

    plot_prefix="${state_name}_grid_sigma"
#     Tkinds=(Taver Insta Tanom)
    Tkinds=(Taver Insta)

    for Tkind in ${Tkinds[@]}; do
        for var in ${speedy_vars[@]}; do
            echo "\begin{minipage}[c]{\textwidth}"   >> $texfile
            section_title "Overall $(Tkind_l $Tkind) $(label $state_name Ephase) $(label $var speedy_var) $(label ${state_name}*${time_stat} Quantity)"
            echo "\centering"   >> $texfile
            #echo "\begin{center}"   >> $texfile
            scalar=${plot_prefix}_${Tkind}_${time_stat}_${var}
            array_width="0.7\textwidth"
            plot_array_vertical_par1 $scalar ""
            #echo "\end{center}"   >> $texfile
            echo "\end{minipage}"   >> $texfile
        done
    done
}

#docu_run_set_state_vertical_zonal_mean(){
    #state_name=$1; time_stat=$2

    #diags=('vertmean' 'zonmean')
    #plot_prefix="${state_name}_grid_sigma"
##     Tkinds=(Taver Insta Tanom)
    #Tkinds=(Taver Insta)
##     Tkinds=()
##     Tkinds=(Taver)

    #for var in ${speedy_vars[@]}; do
        #for Tkind in ${Tkinds[@]}; do
            #echo "\begin{minipage}[c]{\textwidth}"   >> $texfile
            #section_title "Overall $(Tkind_l $Tkind) $(label $state_name Ephase) $(label $var speedy_var) $(label ${state_name}*${time_stat} Quantity)"
            #echo "\centering"   >> $texfile
            ##echo "\begin{center}"   >> $texfile
            #for diag in ${diags[@]}; do
                #scalar=${plot_prefix}_${Tkind}_${time_stat}_${diag}_${var}
                #case "$diag" in
                    #vertmean)
                        #array_width="0.46\textwidth"
                        #plot_array_vertical_par1 $scalar "Vertical mean"
                        #;;
                    #zonmean )
                        #array_width="0.22\textwidth"
                        #plot_array_vertical_par1 $scalar "Zonal mean"
                        #;;
                #esac
            #done
            ##echo "\end{center}"   >> $texfile
            #echo "\end{minipage}"   >> $texfile
        #done
    #done

##     Tkinds=(Insta)
##
##     for var in ${speedy_vars[@]}; do
##         for Tkind in ${Tkinds[@]}; do
##             echo "\begin{minipage}[c]{\textwidth}"   >> $texfile
##             section_title "Overall $(Tkind_l $Tkind) $(label $state_name Ephase) $(label $var speedy_var) $(label ${state_name}*${time_stat} Quantity)"
##             echo "\centering"   >> $texfile
##             #echo "\begin{center}"   >> $texfile
##             for diag in ${diags[@]}; do
##                 scalar=${plot_prefix}_${Tkind}_${time_stat}_${diag}_${var}
##                 case "$diag" in
##                     vertmean)
##                         array_width="0.46\textwidth"
##                         plot_array_vertical_par1 $scalar "Vertical mean"
##                         ;;
##                     zonmean )
##                         array_width="0.22\textwidth"
##                         plot_array_vertical_par1 $scalar "Zonal mean"
##                         ;;
##                 esac
##             done
##             #echo "\end{center}"   >> $texfile
##             echo "\end{minipage}"   >> $texfile
##         done
##     done

#}

docu_run_set_stats_vs_time(){
    state_name=$1
#    time_stat=$2

#    for diag in ${diags[@]}; do
    diag="fldstd1_vertmean"
    plot_prefix="${state_name}_grid_sigma"
    array_width="0.50\textwidth"

    par1_values=(${cfg[par1_values]})
    par1_span_length=${cfg[par1_span_length]}
    par1_name=${cfg[par1_name]}

    for var in ${speedy_vars[@]}; do
        echo "\begin{minipage}[c]{\textwidth}"   >> $texfile
        section_title "\url{$state_name} $var \url{$time_stat}"
        scalar=${plot_prefix}_${diag}_${var}
        plot_path(){ echo "./runs/$(run_name)/plots/${scalar}.pdf"; }
        plot_array_vertical ""
        echo "\end{minipage}"   >> $texfile
    done
}

docu_run_set_error_spread_vs_time(){

    plot_prefix="${cfg[run_type]}_prior_grid_sigma_error-spread"
    array_width="0.70\textwidth"

    for var in ${speedy_vars[@]}; do
        plot_array_vertical_par1 ${plot_prefix}_${var} "Global $(label $var speedy_var) Error-Spread"
    done
}

