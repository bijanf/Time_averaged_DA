#!/usr/bin/env bash
set -eu # strict config (non-zero returns and unset vars halt script)
source "$DAS_DIR/common_tools.sh"
source "$DAS_DIR/run_set_bunch__1.sh"
source "$MODEL_DIR/model_tools.sh"

declare -A cfg run_sets flag

usage(){ cat << EOF
Create a bunch of run sets.
Usage: $(basename $0) [--ref_dir=*] [--set_par1=*] [--set_span1=*]
                   [--set_par2=*] [--set_span2=*] [--bunch_par1=*]
              [--bunch_span1=*] [--bunch_par2=*] [--bunch_span2=*]
         [--calc=*] [--stat=*] [--task=*] [-h|--help] dataset_dir
*/^: Required/optional argument
EOF
exit 1
}

#------------------------------------------------------
#> @brief Create a bunch of run sets
#> (currently only assimilation run_sets)
#------------------------------------------------------
run_set_bunch(){
    long_opts="help,run_mode:,ref_dir:,set_par1:,set_span1:,set_par2:,set_span2:,bunch_par1:,bunch_span1:,bunch_par2:,bunch_span2:,calc:,stat:,plot:,docu:,calc_sets:,stat_sets:,plot_sets:,docu_sets:,task:,detailed_stats::,replot:,verbose:,set_verbose:,report_name:,orientation:"
    PARSED_ARGS=$(getopt -n "$0"  -o h --long "$long_opts" -- "$@")
    eval set -- "$PARSED_ARGS"

    while true; do
        case "$1" in
            -h|--help       )                      usage  ;;
            --run_mode      )         run_mode=$2; shift 2;;
            --ref_dir       ) run_free_set_dir=$2; shift 2;;
            --set_par1      )    set_par1_name=$2; shift 2;;
            --set_span1     )    set_par1_span=$2; shift 2;;
            --set_par2      )    set_par2_name=$2; shift 2;;
            --set_span2     )    set_par2_span=$2; shift 2;;
            --bunch_par1    )  bunch_par1_name=$2; shift 2;;
            --bunch_span1   )  bunch_par1_span=$2; shift 2;;
            --bunch_par2    )  bunch_par2_name=$2; shift 2;;
            --bunch_span2   )  bunch_par2_span=$2; shift 2;;
            --calc          )       flag[calc]=$2; shift 2;;
            --stat          )       flag[stat]=$2; shift 2;;
            --plot          )       flag[plot]=$2; shift 2;;
            --docu          )       flag[docu]=$2; shift 2;;
            --view          )       flag[view]=$2; shift 2;;
            --task          )        only_task=$2; shift 2;;
            --calc_sets     )   calc_sets_flag=$2; shift 2;;
            --stat_sets     )   stat_sets_flag=$2; shift 2;;
            --plot_sets     )   plot_sets_flag=$2; shift 2;;
            --docu_sets     )   docu_sets_flag=$2; shift 2;;
            --detailed_stats)   detailed_stats=$2; shift 2;;
            --replot        )           replot=$2; shift 2;;
            --verbose       )          verbose=$2; shift 2;;
            --set_verbose   )      set_verbose=$2; shift 2;;
            --report_name   )      report_name=$2; shift 2;;
            --orientation   )      orientation=$2; shift 2;;
            --              )             shift 1; break  ;;
        esac
    done
    dataset_dir=$1

    # echo " Creating a bunch of run sets"

    dataset_dir="$(absolute_path "$dataset_dir")"

    verbose=${verbose:-1}
    set_verbose=${set_verbose:-0}
    # single=${single:-yes}
    calc_sets_flag=${calc_sets_flag:-yes}
    stat_sets_flag=${stat_sets_flag:-yes}
    # plot_sets_flag=${plot_sets_flag:-yes}
    docu_sets_flag=${docu_sets_flag:-no}

    dataset_kind=$(barename $0)

    [[ -n ${replot:-} ]] && \
        rm -f "$dataset_dir/logs/plot.success" "$dataset_dir/logs/docu.success"

    perform_simulation_tasks
}

config_function(){
    funct_opening 2

    add_to_cfg "dataset_kind" "run_mode" "model"
    [[ ${cfg[run_mode]} == free ]] && \
        error "Run_mode free not allowed in run_set_bunch"

    if [[ -n ${bunch_par1_name:-} ]]; then
        if [[ -n ${bunch_par2_name:-} ]]; then
            bunch_dim=2
        else
            bunch_dim=1
        fi
    else    bunch_dim=0
    fi

    detailed_stats=${detailed_stats:-no}

    add_to_cfg "dataset_dir"
    add_to_cfg "bunch_dim" "verity" "run_free_set_dir" "diag"

    rel_run_free_set_dir="$(relpath $run_free_set_dir $dataset_dir)"
    add_to_cfg "rel_run_free_set_dir"

    if [[ -n ${set_par1_name:-} ]]; then
        set_par1_label="$(long_name $set_par1_name)"
        add_to_cfg "set_par1_name" "set_par1_label" "set_par1_span"
    fi

    if [[ -n ${set_par2_name:-} ]]; then
        set_par2_label="$(long_name $set_par2_name)"
        add_to_cfg "set_par2_name" "set_par2_label" "set_par2_span"
    fi

    bunch_par1_label="$(long_name $bunch_par1_name)"
    add_to_cfg "bunch_par1_name" "bunch_par1_span" "bunch_par1_label"
    set_par_values_file "bunch_par1" "$bunch_par1_span"

    if [[ ${cfg[bunch_dim]} -eq 2 ]]; then
        bunch_par2_label="$(long_name $bunch_par2_name)"
        add_to_cfg "bunch_par2_name" "bunch_par2_span" "bunch_par2_label"
        set_par_values_file "bunch_par2" "$bunch_par2_span"
    fi
    [[ -n ${report_name:-} ]] && add_to_cfg "report_name"

    store_cfg;  print_cfg

    env > logs/env.log
    find $CODE_DIR/models/$model -maxdepth 1 ! -type d  -not -name ".*" \
        | xargs -I{} cp {} ./logs

    funct_closing 2
}

calc_function(){
    funct_opening 2

    mkdir run_sets

    [[ $calc_sets_flag == yes ]] && calc_run_sets

    funct_closing 2
}

stat_function(){
    funct_opening 2

    [[ -f logs/calc.success ]] || error "calc phase was unseccessful"

    [[ $stat_sets_flag == yes ]] && perform_task_on_sets "stat" #stat_run_sets

    #find_successful_run_sets
    #local run_set_names=(${cfg[run_set_names]})

    #initialize_run_sets_array
    # create_run_set_diffs

    funct_closing 2
}

plot_function(){
    funct_opening 2
    # set -x

    # [[ -f logs/stat.success ]] || run_set_bunch.sh --task=stat "$dataset_dir"
    # touch logs/stat.success
    # ls logs
    [[ -f logs/stat.success ]] || error "stat phase was unseccessful"
    find_successful_run_sets

    initialize_run_sets_array

    # plot reference free dataset
    if [[ ! -f "${cfg[rel_run_free_set_dir]}/logs/plot.success" ]]; then
        read free_dataset_dim < "${cfg[rel_run_free_set_dir]}/config/dataset_dim.cfg"
        case $free_dataset_dim in
            0  ) run.sh     --task=plot "${cfg[rel_run_free_set_dir]}" ;;
            #0  );;
            1|2) run_set.sh --task=plot "${cfg[rel_run_free_set_dir]}" ;;
            *  ) error "Unsupported free_dataset_dim $free_dataset_dim" ;;
        esac
    fi

    case "${cfg[set_dim]}" in
        1) plot_sets_flag=${plot_sets_flag:-no};;
        2) plot_sets_flag=${plot_sets_flag:-yes};;
    esac

    [[ $plot_sets_flag == yes ]] && perform_task_on_sets "plot"

    if [[ -n ${cfg[successful_sets]:-} ]];then
        local successful_sets=(${cfg[successful_sets]})
        #echo ${cfg[successful_sets]}
        scalars=($(find "$dataset_dir/run_sets/${successful_sets[0]}/stats" -name "*.nc"))
        scalars=($(barename ${scalars[@]}))
        printf "%s \n" "${scalars[@]}" > "$dataset_dir/config/scalars.cfg"
        get_config_file "$dataset_dir/config/scalars.cfg"

    # #------------------------------------------------------
    # # get scalar list from successful run_set
    # local run_set_names=(${cfg[run_set_names]})
    # unset scalars
    # for run_set_name_s in ${cfg[run_set_names]}; do
    #     if [[ -d "$dataset_dir/run_sets/$run_set_name_s/plots" ]]; then
    #         scalars=($(find "$dataset_dir/run_sets/$run_set_name_s/plots" -name "*.pdf"))
    #         break
    #     fi
    # done
    # if [[ -z ${scalars:-} ]];then
    #     echo "All run sets crashed completely. No plots to be done."
    #     return 0
    # fi
    # scalars=($(barename ${scalars[@]}))
    # printf "%s\n" "${scalars[@]}" > "$dataset_dir/config/scalars.cfg"
    # #------------------------------------------------------

        #case "${cfg[set_dim]}" in
            #1)
                echo " Plotting stats"
                rm -fr plots; mcd plots
                # create_line_plots
                # echo " Selected Plots"

                matlab -nosplash -nodesktop -nodisplay < \
                   $CODE_DIR/plotting/plot_dataset.m #> /dev/null
                #matlab < "$CODE_DIR/plotting/plot_dataset.m"
                # cp $DAS_DIR/run_set1D_bunch_plots.m .
                #;;
        #esac
        touch "$dataset_dir/logs/plot.success"
    else
        echo "All run sets crashed completely. No plots to be done."
    fi

    funct_closing 2
}

docu_function(){
    funct_opening 2

    [[ -f logs/plot.success ]] || ( plot_function )
    #[[ -f logs/plot.success ]] || \
    #    run_set_bunch.sh --task=plot "$dataset_dir" #&& sleep 100s

    [[ $docu_sets_flag == yes ]] && perform_task_on_sets "docu"

    # Link reference free run_set plots here to produce a complete report
    mkdir -p plots; cd plots
    find "../${cfg[rel_run_free_set_dir]}/plots" -name "*.pdf" \
        -print0 | xargs -0 -I{} ln -fs {} .
    cd ..

    # # patch for incorrect ncl calculated bunch_par_span_lengths (batch2)
    # cfg[bunch_par1_span_length]=$(echo ${cfg[bunch_par1_values]}|wc -w)
    # if [[ -n ${cfg[bunch_par2_span_length]:-} ]]; then
    #     cfg[bunch_par2_span_length]=$(echo ${cfg[bunch_par2_values]}|wc -w)
    # fi

    initialize_run_sets_array

    [[ -n ${cfg[report_name]:-} ]] && texfile="${cfg[report_name]}.tex"

    # recursive_docu=yes
    recursive_docu=${recursive_docu:-no}
    [[ $recursive_docu == yes ]] && perform_task_on_sets "docu"

    case ${cfg[set_dim]:-1} in
        1) report_run_set1D_bunch;;
        2) report_run_set2D_bunch;;
        *) error "Unsupported set_dim ${cfg[set_dim]}"
    esac
    
    sleep 10

    # texfile="report_selected_plots.tex"
    # universal_report

    funct_closing 2
}

view_function(){
    funct_opening 2

    [[ -f logs/docu.success ]] || ( docu_function )
    report_file=($(find . -maxdepth 1 -name "*.pdf"))
    [[ -n ${report_file[0]:-} ]] && okular "${report_file[0]}"
    
    funct_closing 2
}

clean_function(){
    funct_opening 2

    run_set.sh --task=clean --run_verbose=1 "${cfg[rel_run_free_set_dir]}"
    
    find .  -path "*/runs*" -delete
    find .  -path "*/code*" -delete
    find .. -path "*/bin*"  -delete
    # find .  -path "*nature_Insta_all.*" -delete # only needed for calc phase and VERY heavy

    funct_closing 2
}


#assi_linear__error_increase(){

    ##find ../${cfg[rel_run_free_dir]}/$stats_dir/ -name "*.nc" \
    ##    -print0 | xargs -0 -I{} ln -fs {} .
    #for run_set_name in ${cfg[run_set_names]}; do
        
    #done
    
    #${cfg[bunch_par1_span_length]}
    #for Ephase in ${Ephases[@]}; do
        #for Tkind in Insta Taver Tanom; do
                                                ## error reduction
            #Reduc \
                #assi_error_Tstdd_${Ephase}_${Tkind} \
                #free_error_Tstdd_prior_${Tkind} \
                #assi_error_Tstdd_reduc_${Ephase}_${Tkind}
      #${cfg[diag]} \
                #assi_error_Tstdd_reduc_${Ephase}_${Tkind} \
                #assi_error_Tstdd_reduc_${cfg[diag]}_${Ephase}_${Tkind}
            ## Fmean \
            ##     assi_error_Tstdd_reduc_${Ephase}_${Tkind} \
            ##     assi_error_Tstdd_reduc_Fmean_${Ephase}_${Tkind}

                                                ## Spread reduction
            #Reduc \
                #assi_Esprd_Tmean_${Ephase}_${Tkind} \
                #free_Esprd_Tmean_prior_${Tkind} \
                #assi_Esprd_Tmean_reduc_${Ephase}_${Tkind}
      #${cfg[diag]} \
                #assi_Esprd_Tmean_reduc_${Ephase}_${Tkind} \
                #assi_Esprd_Tmean_reduc_${cfg[diag]}_${Ephase}_${Tkind}
            ## Fmean \
            ##     assi_Esprd_Tmean_reduc_${Ephase}_${Tkind} \
            ##     assi_Esprd_Tmean_reduc_Fmean_${Ephase}_${Tkind}

                                                ## variance reduction
            #Reduc \
                #assi_Emean_Tstdd_${Ephase}_${Tkind} nature_Tstdd_${Tkind} \
                #assi_Emean_Tstdd_reduc_${Ephase}_${Tkind}
      #${cfg[diag]} \
                #assi_Emean_Tstdd_reduc_${Ephase}_${Tkind} \
                #assi_Emean_Tstdd_reduc_${cfg[diag]}_${Ephase}_${Tkind}
            ## Reduc \
            ##     assi_Emean_Tstdd_${Ephase}_${Tkind} \
            ##     free_Emean_Tstdd_prior_${Tkind} \
            ##     assi_Emean_Tstdd_reduc_${Ephase}_${Tkind}
      ## ${cfg[diag]} \
            ##     assi_Emean_Tstdd_reduc_${Ephase}_${Tkind} \
            ##     assi_Emean_Tstdd_reduc_${cfg[diag]}_${Ephase}_${Tkind}
            ## Fmean \
            ##     assi_Emean_Tstdd_reduc_${Ephase}_${Tkind} \
            ##     assi_Emean_Tstdd_reduc_Fmean_${Ephase}_${Tkind}

            ## Reduc \
            ##     assi_Emean_Tvar_${Ephase}_${Tkind} \
            ##     free_Emean_Tvar_prior_${Tkind} \
            ##     assi_Emean_Tvar_reduc_${Ephase}_${Tkind}
            ## Fmean \
            ##     assi_Emean_Tvar_reduc_${Ephase}_${Tkind} \
            ##     assi_Emean_Tvar_reduc_Fmean_${Ephase}_${Tkind}
        #done
    #done
#}


run_set_bunch $@
exit $?
