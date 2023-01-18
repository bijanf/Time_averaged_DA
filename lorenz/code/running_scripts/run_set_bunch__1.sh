#!/usr/bin/env bash

loop_over_bunch(){
    case ${cfg[bunch_dim]} in
        1)
            i1=0; iset=0
            for bunch_par1_value in ${cfg[bunch_par1_values]};do
                eval "export ${cfg[bunch_par1_name]}=\"$bunch_par1_value\""
                loop_core
                (( i1+=1 )); (( iset+=1 ))
            done
            unset i1; unset iset
            ;;
        2)
            i2=0; iset=0
            for bunch_par2_value in ${cfg[bunch_par2_values]};do
                eval "export ${cfg[bunch_par2_name]}=\"$bunch_par2_value\""
                i1=0
                for bunch_par1_value in ${cfg[bunch_par1_values]};do
                    eval "export ${cfg[bunch_par1_name]}=\"$bunch_par1_value\""
                    loop_core
                    (( i1+=1 )); (( iset+=1 ))
                done
                unset i1
                (( i2+=1 ))
            done
            unset i2; unset iset
            ;;
        *) error "Unsupported bunch_dim ${cfg[bunch_dim]}" ;;
    esac
    unset -f loop_core
}


loop_over_bunch_par2(){
    eval "bunch_par2_name=\"${cfg[bunch_par2_name]}\""
    i2=0;# iset=0
    for bunch_par2_value in ${cfg[bunch_par2_values]};do
	eval "bunch_par2_index=\"$i2\""
# 	echo "export $bunch_par2_name=\$bunch_par2_value"
	eval "export $bunch_par2_name=\$bunch_par2_value"
	loop_over_bunch_par2_core
	(( i2+=1 )); #(( iset+=1 ))
    done
    unset i2;# unset iset
    unset -f bunch_par2_value
    unset -f bunch_par2_index    
    unset -f loop_over_bunch_par2_core
}

loop_over_bunch_par1(){
    eval "bunch_par1_name=\"${cfg[bunch_par1_name]}\""
    i1=0;# iset=0
    for bunch_par1_value in ${cfg[bunch_par1_values]};do
	eval "bunch_par1_index=\"$i1\""
# 	echo "export $bunch_par1_name=\$bunch_par1_value"
	eval "export $bunch_par1_name=\$bunch_par1_value"
	loop_over_bunch_par1_core
	(( i1+=1 )); #(( iset+=1 ))
    done
    unset i1;# unset iset
    unset -f bunch_par1_value
    unset -f bunch_par1_index    
    unset -f loop_over_bunch_par1_core
}

run_set_name(){
    case ${cfg[bunch_dim]} in
        1) echo "$(par_id ${cfg[bunch_par1_name]})";;
        2) echo "$(par_id ${cfg[bunch_par1_name]} ${cfg[bunch_par2_name]})";;
        # 1) echo "${cfg[bunch_par1_name]}_${bunch_par1_value}"     ;;
        # 2) echo "${cfg[bunch_par1_name]}_${bunch_par1_value}__${cfg[bunch_par2_name]}_${bunch_par2_va1)";;
    esac
}

run_set_label(){
    case ${cfg[bunch_dim]} in
        1) echo "$(long_name ${cfg[bunch_par1_name]}) = $bunch_par1_value"     ;;
        2) echo "$(long_name ${cfg[bunch_par1_name]}) = $bunch_par1_value, $(long_name ${cfg[bunch_par2_name]}) = $bunch_par2_value" ;;
    esac
}

run_set_dir(){
    echo "run_sets/$(run_set_name)"
}

initialize_run_sets_array(){
    case ${cfg[bunch_dim]} in
        1) loop_core(){ run_sets[${i1}      ]=$(run_set_name); } ;;
        2) loop_core(){ run_sets[${i1},${i2}]=$(run_set_name); } ;;
    esac
    loop_over_bunch
    declare -r run_sets
}

calc_run_sets(){
    funct_opening 1

    echo "Run set calc consumption" > logs/calc_consumption.log

    loop_core(){
        echo " - $(run_set_name)"| tee -a logs/calc_consumption.log
        echo "$(run_set_name)" >> config/run_set_names.cfg

        echo $i1 >> config/bunch_par1_value_pos.cfg
        if [[ ${cfg[bunch_dim]} -eq 2 ]]; then
            echo $i2 >> config/bunch_par2_value_pos.cfg
        fi

    # Optimal Inflation value should be set only if the
    # run_set_bunch does not study inflation dependence
        case ${cfg[bunch_dim]} in
            1)  [[ ! ${cfg[bunch_par1_name]} == "infl_enkf" ]] && set_inflation
                ;;
            2)  [[ ! ${cfg[bunch_par1_name]} == "infl_enkf" ]] &&
                [[ ! ${cfg[bunch_par2_name]} == "infl_enkf" ]] && set_inflation
                ;;
        esac


        my_time -o calc_consumption.log \
            run_set.sh --run_mode=assi --task=calc \
            --par1=${set_par1_name:-} --span1=${set_par1_span:-} \
            --par2=${set_par2_name:-} --span2=${set_par2_span:-} \
            --detailed_stats=${detailed_stats:-} \
            --ref_dir="$run_free_set_dir" \
            --verbose=$set_verbose \
            "$(run_set_dir)"
        wait; sleep 1s
        cat calc_consumption.log  >> logs/calc_consumption.log
        cat calc_consumption.log > \
            "$(run_set_dir)/logs/calc_consumption.log"
        rm calc_consumption.log
        # mv calc_consumption.log "$(run_set_dir)/logs/"
    }
    loop_over_bunch

    funct_closing 1
}

find_successful_run_sets(){
    funct_opening 1

    # Complement configuration
    local run_set_names=(${cfg[run_set_names]})
    read set_dim < "./run_sets/${run_set_names[0]}/config/dataset_dim.cfg"
    set_cfg_file "set_dim"
    get_config_file config/set_dim.cfg
    read n_comp <  "./run_sets/${run_set_names[0]}/config/n_comp.cfg"
    set_cfg_file "n_comp"

    rm -f config/successful_sets.cfg config/crashed_sets.cfg
    loop_core(){
        successful_runs_file="./run_sets/$(run_set_name)/config/successful_runs.cfg"
        if [[ -s "$successful_runs_file" ]];then
            echo $(run_set_name) >> config/successful_sets.cfg
        else
            echo $(run_set_name) >> config/crashed_sets.cfg
        fi
    }
    loop_over_bunch
    get_config_file config/successful_sets.cfg

    funct_closing 1
}

perform_task_on_sets(){
    funct_opening 1

    local task=$1
    echo " task = $task"
    [[ $task != calc ]] || error "calc task not allowed, use calc_run_sets function."

    echo "Run set ${task} consumption" > logs/${task}_consumption.log

    loop_core(){
        echo " - $(run_set_name)"| tee -a logs/${task}_consumption.log
        my_time -o ${task}_consumption.log \
            run_set.sh --task=${task} --verbose=$set_verbose \
            "$(run_set_dir)"
        cat ${task}_consumption.log >> logs/${task}_consumption.log
        cat ${task}_consumption.log > \
            "$(run_set_dir)/logs/${task}_consumption.log"
        rm ${task}_consumption.log
        # mv ${task}_consumption.log "$(run_set_dir)/logs/"
    }
    loop_over_bunch

    funct_closing 1
}

create_run_set_diffs(){
    funct_opening 1

    cd $dataset_dir
    rm -fr run_set_diffs; rm -f config/run_set_diff_names.cfg
    mkdir run_set_diffs #; cd run_set_diffs

    case ${cfg[bunch_dim]} in
        1)
            loop_core(){
                if (( i1 != 0 )); then
                    run_set_diff_name="${run_sets[0]}-${run_sets[$i1]}"
                    run_set_diff_dir="$dataset_dir/run_set_diffs/$run_set_diff_name"
                    echo " - $run_set_diff_name" | tee -a diff_consumption.log
                    echo "$run_set_diff_name" >> config/run_set_diff_names.cfg
                    my_time -o diff_consumption.log -a \
                        run_set_diff.sh --task=calc \
                        --set_A="$dataset_dir/run_sets/${run_sets[0]}" \
                        --set_B="$dataset_dir/run_sets/${run_sets[$i1]}" \
                        "$run_set_diff_dir" >/dev/null
                fi
            }
            loop_over_bunch
            ;;
        2)
            loop_core(){
                if (( i1 != 0 )) && (( i2 != 0 )); then
                    run_set_diff_name="${run_sets[0,0]}-${run_sets[$i1,$i2]}"
                    run_set_diff_dir="$dataset_dir/run_set_diffs/$run_set_diff_name"
                    echo " - $run_set_diff_name" | tee -a diff_consumption.log
                    echo "$run_set_diff_name" >> config/run_set_diff_names.cfg
                    my_time -o diff_consumption.log -a \
                        run_set_diff.sh --task=calc \
                        --set_A="$dataset_dir/run_sets/${run_sets[0,0]}" \
                        --set_B="$dataset_dir/run_sets/${run_sets[$i1,$i2]}" \
                        "$run_set_diff_dir" >/dev/null
                fi
            }
            loop_over_bunch
            ;;

            # for ((i2 = 0 ; i2 < cfg[bunch_par2_span_length]; i2++)); do
            #     for ((i1 = 1 ; i1 < cfg[bunch_par1_span_length]; i1++)); do

            #         run_set_diff_name="${run_sets[0,$i2]}-${run_sets[$i1,$i2]}"
            #         run_set_diff_dir="$dataset_dir/diffs/$run_set_diff_name"
            #         echo " - $run_set_diff_name"
            #         echo "$run_set_diff_name" >> ../config/run_set_diff_names.cfg

            #         run_set_diff.sh --plot=no \
            #             --set_A="$dataset_dir/run_sets/${run_sets[0,$i2]}" \
            #             --set_B="$dataset_dir/run_sets/${run_sets[$i1,$i2]}" \
            #             "$run_set_diff_dir" > run_set_diff.log || error
            #         mv run_set_diff.log "$run_set_diff_dir"
            #     done
            # done

            # bunch_length=${#run_set_names[@]}
            # for ((i1 = 1 ; i1 < bunch_length; i1++)); do
            #     run_set_diff_name="${run_set_names[0]}-${run_set_names[$i1]}"
            #     run_set_diff_dir="$dataset_dir/diffs/$run_set_diff_name"

            #     echo " - $run_set_diff_name"
            #     echo "$run_set_diff_name" >> ../config/run_set_diff_names.cfg

            #     run_set_diff.sh --no_plot \
            #         --set_A="$ref_run_set_dir" \
            #         --set_B="$dataset_dir/run_sets/$run_set_name" \
            #         "$run_set_diff_dir" > run_set_diff.log || error
            #     mv run_set_diff.log "$run_set_diff_dir"
            # done
         #   ;;

   #          for ((i2 = 1 ; i2 < cfg[bunch_par2_span_length]; i2++)); do
   #              for ((i1 = 0 ; i1 < cfg[bunch_par1_span_length]; i1++)); do

   #                  run_set_diff_name="${run_sets[$i1,0]}-${run_sets[$i1,$i2]}"
   #                  run_set_diff_dir="$dataset_dir/diffs/$run_set_diff_name"
   #                  echo " - $run_set_diff_name"
   #                  echo "$run_set_diff_name" >> ../config/run_set_diff_names.cfg

   #                  run_set_diff.sh --plot=no \
   #                      --set_A="$dataset_dir/run_sets/${run_sets[$i1,0]}" \
   #                      --set_B="$dataset_dir/run_sets/${run_sets[$i1,$i2]}" \
   #                      "$run_set_diff_dir" > run_set_diff.log || error
   #                  mv run_set_diff.log "$run_set_diff_dir"
   #              done
   #          done

        *)    error "Unsupported bunch_dim ${cfg[bunch_dim]}" ;;
    esac

    get_config_file $dataset_dir/config/run_set_diff_names.cfg

    funct_closing 1
}

create_line_plots(){
    funct_opening 1

    cp $CODE_DIR/plotting/line_plot.ncl .

    # case "${cfg[bunch_dim]}" in
    #     1)# prefixes=(error_Tstdd*prior error_Tstdd*postr Tvar*prior Tvar*postr)
    #         # prefixes=(error_Tstdd*{prior,postr} Tvar*{prior,postr})    ;;
    #         prefixes=( \
    #           error_Tstdd_${cfg[diag]}_*_{Insta,Taver,Tanom} \
    #           Esprd_Tmean_${cfg[diag]}_*_{Insta,Taver,Tanom} \
    #           Emean_Tstdd_${cfg[diag]}_*_{Insta,Taver,Tanom})    ;;
    #     2) #prefixes=(error_Tstdd*prior error_Tstdd*postr Tvar*prior Tvar*postr)
    #         prefixes=( \
    #             error_Tstdd*{prior,postr}*{Insta,Taver,Tanom} \
    #             Tvar*{prior,postr}*{Insta,Taver,Tanom} \
    #             ) ;;
    # esac

    prefixes=(${cfg[scalars]})

    i_run=0
    for prefix in ${prefixes[@]}; do
        echo " - $prefix"

         #-------------------------------------------------------------
        #  Plot definition
        plot_name  (){ echo $prefix;}
        plot_title (){ echo "$(Quantity_l $prefix)";}
        declare -a opts
        case "$prefix" in
            *reduc*)
                opts+=(min_level=-20.0); opts+=(max_level=100.0) ;;
            *)
                opts+=(min_level=0.0); opts+=(max_level=10.0) ;;
            # *error_Tstdd_${cfg[diag]}*)
            #     opts+=(min_level=0.0);;
        esac

        opts+=($(printf '%s=\"%s\"' plot_name "$(plot_name)"))
        opts+=(n_comp=${cfg[n_comp]});
        echo ${opts[@]} > $(plot_name).opts
        unset opts

        echo "$(plot_title)" > $(plot_name)_title.txt

        loop_core(){
            file="../$(run_set_dir)/stats/${prefix}.nc"
            echo "$file"            >> $(plot_name)_files.txt
            echo "$(run_set_label)"  >> $(plot_name)_labels.txt
        }
        loop_over_bunch

        my_ncl --logfile="$(plot_name).ncl_log" \
            $(cat $(plot_name).opts) line_plot.ncl &
        unset -f plot_title
        unset -f plot_name

        #-------------------------------------------------------------
        pids[$i_run]=$! ; (( i_run+=1 ))
        if (( i_run%cpus == 0 )); then
            wait_for_child_processes "strict"
        fi
    done
    wait_for_child_processes "strict" # of an incomplete batch

    funct_closing 1
}

report_metainfo(){

    newpage
    echo "\begin{landscape}" >> $texfile
    echo "\begin{multicols}{2}" >> $texfile

    if [[ -n ${cfg[report_name]:-} ]];then
        section_title "\bf EXPERIMENT"
        section_title "\bf\url{${cfg[report_name]}}"
    fi
    cfg_dir="./config"
    report_config "D.A. Run set bunch configuration"

    cfg_dir="./${cfg[rel_run_free_set_dir]}/config"
    report_config "Free Run Configuration"

    if [[ -f "logs/default_config.sh" ]];then
        include_file "logs/default_config.sh"
        # include_file "logs/calc.log"
    fi
    echo "\end{multicols}" >> $texfile
    echo "\end{landscape}" >> $texfile

}

report_run_set1D_bunch(){
    funct_opening 2

    #orientation=landscape
    margins="[left=10mm,right=10mm,top=15mm,bottom=20mm]"

    open_report
    report_metainfo

    plots_dir="./plots"

    plot_frame "$plots_dir/nature_Tstdd__plot.pdf" \
        --title="Nature Standard Deviation" \
        --subtitle_on=no --width="0.75\textwidth"

    [[ -f "$plots_dir/corr_nature__obs_clean_Taver__plot.pdf" ]] && \
        plot_frame "$plots_dir/corr_nature__obs_clean_Taver__plot.pdf" \
        --title="Correlation Taver. Obs - Nature state" \
        --subtitle_on=no --width="0.75\textwidth"

    newpage

    plot_title(){ echo "$(Ephase_l "$plot_file")"; }

    echo "\begin{minipage}[c]{\textwidth}" >> $texfile
    section_title "Error"
    for Tkind in Insta Taver Tanom;do
        row_files=( ${plots_dir}/error-plot_{prior,postr}_${Tkind}.pdf)
        plot_row "$(Tkind_l ${Tkind})" no
    done
    echo "\end{minipage}" >> $texfile

    if [[ ${cfg[bunch_par1_name]} == 'obs_operator' ]]; then
        echo "\begin{minipage}[c]{\textwidth}" >> $texfile
        section_title "Error Increase regarding linear obs"
        for Tkind in Insta Taver Tanom;do
            row_files=( ${plots_dir}/error-increase_plot_{prior,postr}_${Tkind}.pdf)
            plot_row "$(Tkind_l ${Tkind})" no
        done
        echo "\end{minipage}" >> $texfile
    fi

    #echo "\begin{minipage}[c]{\textwidth}" >> $texfile
    #section_title "Error by bunch par1"
    #column_files=( ${plots_dir}/error_Tstdd__*.pdf)
    #plot_column
    #echo "\end{minipage}" >> $texfile

    plot_path(){ echo "${plots_dir}/error_Tstdd__$(run_set_name).pdf"; }
    case "${cfg[bunch_dim]}" in
        1)
            array_width="0.55\textwidth"
            plot_array_bunch1D_vertical "Error by $(label ${cfg[bunch_par1_name]} par)" ;;
#                    1) plot_array_bunch1D_horizontal "$(Ephase_l "$scalar")" ;;
        2)
            array_width="1.0\textheight"
	    echo "\begin{landscape}" >> $texfile
            plot_array_bunch2D "Error by $(label ${cfg[bunch_par1_name]} par) and $(label ${cfg[bunch_par2_name]} par)"
	    echo "\end{landscape}" >> $texfile
	    ;;
            # plot_array_bunch1D_vertical "Error by $(label ${cfg[bunch_par1_name]} par)" ;;
      # plot_array_bunch2D          "$(Ephase_l "$scalar")" ;;
        *) error "Unsupported bunch_dim ${cfg[bunch_dim]}";;
    esac


    echo "\begin{minipage}[c]{\textwidth}" >> $texfile
    section_title "Error-Spread"
    for Tkind in Insta Taver Tanom;do
        row_files=( ${plots_dir}/error-spread_plot_{prior,postr}_${Tkind}.pdf)
        plot_row "$(Tkind_l ${Tkind})" no
    done
    echo "\end{minipage}" >> $texfile

    echo "\begin{minipage}[c]{\textwidth}" >> $texfile
    section_title "State Standard Deviation"
    for Tkind in Insta Taver Tanom;do
        row_files=( ${plots_dir}/state_std_dev_plot_{prior,postr}_${Tkind}.pdf)
        plot_row "$(Tkind_l ${Tkind})" no
    done
    echo "\end{minipage}" >> $texfile

    # cstat="Xsel"
    cstat=${cfg[diag]}

    #prefixes=("error_Tstdd_reduc" "Esprd_Tmean_reduc" "Emean_Tstdd_reduc")
    prefixes=("error_Tstdd_reduc")
    plot_title(){ echo "$(Ephase_l "$plot_file")"; }
    row_width="\textwidth"

    newpage
    for prefix in "${prefixes[@]}";do
        echo "\begin{minipage}[c]{\textwidth}" >> $texfile
        section_title "$(Quantity_l $prefix) regarding free run"
        for Tkind in Insta Taver Tanom;do
            row_files=(${plots_dir}/assi_${prefix}_${cstat}_{prior,postr}_${Tkind}.pdf)
            plot_row "$(Tkind_l ${Tkind})" no
        done
        echo "\end{minipage}" >> $texfile
        echo "\newpage"                        >> $texfile
    done

    echo "\newpage"                        >> $texfile

    close_report
    my_pdflatex "$texfile"

    funct_closing 2
}

report_run_set2D_bunch(){
    funct_opening 1

    #quality=draft
    #case "${cfg[n_comp]}" in
        #1)
            #orientation=landscape
            #column_width="0.5\textwidth"
            #columns="twocolumn"
            #;;
        #2)
    orientation="portrait"

    case "${cfg[bunch_dim]}" in
        1)  orientation=${orientation:-"portrait"};;
        2)  orientation=${orientation:-"landscape"};;
    esac
    # orientation=${orientation:-"portrait"}
    # orientation=${orientation:-"landscape"}

    margins="[left=5mm,right=5mm,top=5mm,bottom=20mm]"
    # array_width="0.3\textwidth"
            #;;
    #esac
    open_report
    report_metainfo

    for plot_prefix in \
        "assi_error_Tstdd" \
        "assi_error_Tstdd_reduc" \
        "assi_Esprd_Tmean" \
        "assi_Esprd_Tmean_reduc" \
        "assi_Emean_Tstdd" \
        "assi_Emean_Tstdd_reduc"
    do
                                #---------------------
                                # scalar-wise arrays
                                #---------------------
        for Tkind in Insta Taver Tanom;do
            # echo "\begin{minipage}[c]{\textwidth}"   >> $texfile
            echo "\clearpage"   >> $texfile
            title="$(Tkind_l $Tkind) $(Quantity_l "$plot_prefix")"
            echo "\centerline{\LARGE\bf $title}"     >> $texfile
            # section_title "$(Tkind_l $Tkind) $(Quantity_l "$plot_prefix")"
            for Ephase in postr prior;do
                scalar=${plot_prefix}_${cfg[diag]}_${Ephase}_${Tkind}

                plot_path(){ echo "./run_sets/$(run_set_name)/plots/${scalar}.pdf"; }
                                                         # # Reference differences along bunch_par1
                                                         # plot_path(){
                                                         #     if (( i1 == 0 )); then
                                                         #         echo "../run_sets/${run_sets[$i1]}/plots/${scalar}.pdf"
                                                         #     else
                                                         #         run_set_diff_name="${run_sets[0]}-${run_sets[$i1]}"
                                                         #         echo "../run_set_diffs/$run_set_diff_name/plots/${scalar}.pdf"
                                                         #     fi; }
                case "${cfg[bunch_dim]}" in
                    1) plot_array_bunch1D_vertical "$(Ephase_l "$scalar")" ;;
#                    1) plot_array_bunch1D_horizontal "$(Ephase_l "$scalar")" ;;
                    2) plot_array_bunch2D          "$(Ephase_l "$scalar")" ;;
                    *) error "Unsupported bunch_dim ${cfg[bunch_dim]}";;
                esac
            done
#             echo "\hfill"   >> $texfile
            # echo "\end{minipage}"   >> $texfile
            #newpage
        done

    done

    # free_run_set2D_plots_page "./${cfg[rel_run_free_set_dir]}/plots"
    free_run_set1D_plots_page "./${cfg[rel_run_free_set_dir]}/plots"

    run_set_name0=${cfg[run_set_names]%% *}
    stat1=obs_error_stdd_${cfg[diag]}
    plot_frame --title="Observations $(Quantity_l "$stat1")" \
        --subtitle_on=no --width="0.5\textwidth" \
        "./run_sets/$run_set_name0/plots/$stat1"
    echo "\newpage"                    >> $texfile

    #plot_Tkind_row_column1(){
        #title="$1"
        #plot_title(){ echo "$(Tkind_l "$plot_file")"; }

        #echo "\begin{minipage}[c]{$row_width}" >> $texfile
        #section_title "$title"
        #for row_prefix in ${row_prefixes[@]};do
            #row_files=(${row_prefix}_{Insta,Taver,Tanom}.pdf)
            #if [[ $row_prefix == ${row_prefixes[0]} ]];then
                #plot_title(){ echo "$(Tkind_l "$plot_file")"; }
            #else
                #plot_title(){ echo ""; }
            #fi
            #plot_row "$(row_title)" no
        #done
        #echo "\end{minipage}"                    >> $texfile
    #}

    ## plot_width="$(echo "0.8/(${cfg[bunch_par1_span_length]}))"|bc)\textwidth"
    ## plot_width="$(echo "print (1.0/(${cfg[bunch_par1_span_length]}))"|python)\textwidth"
    ## echo "print (1.0/(${cfg[bunch_par1_span_length]}))"| python > plot_width.tmp

    #for plot_prefix in \
        #"assi_error_Tstdd_${cfg[diag]}_postr" \
        #"assi_error_Tstdd_${cfg[diag]}_prior" \
        #"assi_error_Tstdd_reduc_${cfg[diag]}_postr" \
        #"assi_error_Tstdd_reduc_${cfg[diag]}_prior" \
        #"assi_Esprd_Tmean_${cfg[diag]}_postr" \
        #"assi_Esprd_Tmean_${cfg[diag]}_prior" \
        #"assi_Esprd_Tmean_reduc_${cfg[diag]}_postr" \
        #"assi_Esprd_Tmean_reduc_${cfg[diag]}_prior" \
        #"assi_Emean_Tstdd_${cfg[diag]}_postr" \
        #"assi_Emean_Tstdd_${cfg[diag]}_prior" \
        #"assi_Emean_Tstdd_reduc_${cfg[diag]}_postr" \
        #"assi_Emean_Tstdd_reduc_${cfg[diag]}_prior"; do
        #echo " - $plot_prefix"

                                ##---------------------
                                ## Tkind-wise rows
                                ##---------------------
        #declare -a row_prefixes
        #loop_core(){
            #row_prefixes[$iset]="./run_sets/$(run_set_name)/plots/${plot_prefix}"; }
        #loop_over_bunch
        #row_title(){
            #case "${cfg[bunch_dim]}" in
                #1)
                    #label1="$(label "$row_prefix" ${cfg[bunch_par1_name]})"
                    #echo "$label1";;
                #2)
                    #label1="$(label "$row_prefix" ${cfg[bunch_par1_name]})"
                    #label2="$(label "$row_prefix" ${cfg[bunch_par2_name]})"
                    #echo "$label1, $label2";;
            #esac
        #}
        ## row_title(){ echo "\url{$row_prefix}"; }

        ##plot_Tkind_row_column "$(run_l $plot_prefix) $(Quantity_l $plot_prefix)"
    #done

    close_report
    my_pdflatex "$texfile"

    funct_closing 1
}

# plot_array_bunch2D__(){
#     funct_opening 1
#
#     bunch_par1_values=(${cfg[bunch_par1_values]})
#     bunch_par2_values=(${cfg[bunch_par2_values]})
#
#     plot_width="$(echo "print (0.95/(${cfg[bunch_par1_span_length]}))"|python)"
#     plot_width="${plot_width}\textwidth"
#     # echo "print (1.0/(${bunch_par1_span_length}))"| python > plot_width.tmp
#
#     if (( cfg[bunch_par1_span_length] > cfg[bunch_par2_span_length] ));then
#         orientation="landscape"
#     else
#         orientation="portrait"
#     fi
#
#     open_report
#
#     plot_array(){
#         array_title=$1
#         delimiter(){
#             if (( i1 < (cfg[bunch_par1_span_length] - 1) )); then
#                 echo "&"
#             else
#                 echo "\\\\"
#             fi
#         }
#         tab_format(){
#             case "${cfg[bunch_par1_span_length]}" in
#                 1) echo "|c|c|c|";;
#                 2) echo "|c|c|c|c|";;
#                 3) echo "|c|c|c|c|c|";;
#                 *) error "Unsupported bunch_par1_span_length ${cfg[bunch_par1_span_length]}";;
#             esac
#         }
#
#         echo "\begin{minipage}[c]{\textwidth}"   >> $texfile
#         echo "\vspace*{3mm}"                     >> $texfile
#         echo "\centering"                        >> $texfile
#         echo "\centerline{\LARGE \bf $array_title}" >> $texfile
#         echo "\vspace*{3mm}"                     >> $texfile
#
#         echo "\begin{tabular}{$(tab_format)}"        >> $texfile
#         echo "\hline"                            >> $texfile
#         echo "&&\multicolumn{${cfg[bunch_par1_span_length]}}{|c|}{\url{${cfg[bunch_par1_name]}}} \\\\" >> $texfile
#         echo "\hline"                            >> $texfile
#
#         echo "&&"                                >> $texfile
#
#         for ((i1 = 0 ; i1 < cfg[bunch_par1_span_length]; i1++)); do
#             bunch_par1_value=${bunch_par1_values[$i1]}
#             echo "\url{$bunch_par1_value}"       >> $texfile
#             delimiter  >> $texfile
#         done
#         echo "\hline"                            >> $texfile
#
#         for ((i2 = 0 ; i2 < cfg[bunch_par2_span_length]; i2++)); do
#             bunch_par2_value=${bunch_par2_values[$i2]}
#
#             if (( i2 == 0 )); then
#                 echo "\multirow{${cfg[bunch_par2_span_length]}}{*}{\rotatebox[origin=c]{90}{\url{${cfg[bunch_par2_name]}}}}" >> $texfile
#             fi
#             echo "&\rotatebox[origin=c]{90}{\url{$bunch_par2_value}}&" >> $texfile
#
#             for ((i1 = 0 ; i1 < cfg[bunch_par1_span_length]; i1++)); do
#                 bunch_par1_value=${bunch_par1_values[$i1]}
#
#                 echo "\raisebox{0mm}{\includegraphics[width=$plot_width]{$(plot_path)}}" >> $texfile
#
#                 if (( i1 < (cfg[bunch_par1_span_length] - 1) )); then
#                     echo "&"                     >> $texfile
#                 else
#                     echo "\\\\\cline{2-4}"       >> $texfile
#                 fi
#                 # delimiter  >> $texfile
#             done
#         done
#         echo "\hline"                            >> $texfile
#         echo "\end{tabular}"                     >> $texfile
#         echo "\end{minipage}"                    >> $texfile
#     }
#
#     for scalar in ${cfg[scalars]}; do
#         echo " - $scalar"
#
#         # Absolute values
#         plot_path(){ echo "./run_sets/$(run_set_name)/plots/${scalar}.pdf"; }
#         plot_array "$(Tkind_l "$scalar") $(Ephase_l "$scalar") $(Quantity_l "$scalar")"
#
# #         # Reference differences along bunch_par1
# #         plot_path(){
# #             if (( i1 == 0 )); then
# #                 echo "../run_sets/${run_sets[$i1,$i2]}/stats/${scalar}.pdf"
# #             else
# #                 run_set_diff_name="${run_sets[0,$i2]}-${run_sets[$i1,$i2]}"
# #                 echo "../diffs/$run_set_diff_name/stats/${scalar}.pdf"
# #             fi; }
# #         plot_array
#
# #         # Reference differences along bunch_par2
# #         plot_path(){
# #             if (( i2 == 0 )); then
# #                 echo "../run_sets/${run_sets[$i1,$i2]}/stats/${scalar}.pdf"
# #             else
# #                 run_set_diff_name="${run_sets[$i1,0]}-${run_sets[$i1,$i2]}"
# #                 echo "../diffs/$run_set_diff_name/stats/${scalar}.pdf"
# #             fi; }
# #         plot_array
#
#     done
#
#     report_metainfo
#     close_report
#     my_pdflatex "$texfile"
#
#     funct_closing 1
# }
#

