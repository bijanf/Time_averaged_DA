#!/usr/bin/env bash

config_run_free_set(){

    dataset_dim=$ext_pars
    case $dataset_dim in
        1)  par1_name=$ext_par1_name; par1_span=$ext_par1_span;;
        2)  par1_name=$ext_par1_name; par1_span=$ext_par1_span
            par2_name=$ext_par2_name; par2_span=$ext_par2_span;;
        *) error "Unsupported set dimension $dataset_dim";;
    esac
}

config_run_assi_set(){

    [[ -d "$reference_dir" ]] || error "Non existent reference_dir $reference_dir"
    read run_free_set_dim < $reference_dir/config/dataset_dim.cfg
    dataset_dim=$(($run_free_set_dim + $ext_pars))
    [[ $dataset_dim -gt 2 ]] &&  error "Unsupported run set dimension $dataset_dim"

    case $run_free_set_dim in
        0)
            case $ext_pars in
                1)
                    par1_name=$ext_par1_name; par1_span=$ext_par1_span
                    run_free_dir(){ echo "$reference_dir"; }
                    ;;
                2)
                    par1_name=$ext_par1_name; par1_span=$ext_par1_span
                    par2_name=$ext_par2_name; par2_span=$ext_par2_span
                    run_free_dir(){ echo "$reference_dir"; }
                    ;;
                *)
                    error "run_free_set_dim-ext_pars combination $run_free_set_dim - $ext_pars Not ready yet";;
            esac
            ;;
        1)
            case $ext_pars in
                1)
                    par1_name=$ext_par1_name; par1_span=$ext_par1_span
                    read par2_name < $reference_dir/config/par1_name.cfg
                    read par2_span < $reference_dir/config/par1_span.cfg
                    run_free_dir (){
                        echo "$reference_dir/runs/$(par_id ${par2_name})"
                    }
                    # run_free_dir (){
                    #     echo "$reference_dir/runs/${par2_name}_${par2_value}"; }
                    ;;
                0)
                    read par1_name < $reference_dir/config/par1_name.cfg
                    read par1_span < $reference_dir/config/par1_span.cfg
                    run_free_dir (){
                        echo "$reference_dir/runs/$(par_id ${par1_name})"
                    }
                    # run_free_dir (){
                    #     echo "$reference_dir/runs/${par1_name}-${par1_value}"; }
                    ;;
                *) error "run_free_set_dim-ext_pars combination $run_free_set_dim - $ext_pars Not ready yet"
                    ;;
            esac
            ;;
        2)
            read par1_name < $reference_dir/config/par1_name.cfg
            read par1_span < $reference_dir/config/par1_span.cfg
            read par2_name < $reference_dir/config/par2_name.cfg
            read par2_span < $reference_dir/config/par2_span.cfg
            run_free_dir (){ echo "$reference_dir/runs/$(run_name)"; }
            ;;
        *)
            error "Unsupported run_free_set_dim $run_free_set_dim";;
    esac

}

loop_over_set(){
    i_run=1;
    case ${cfg[dataset_dim]} in
        1)
            i1=0
            for par1_value in ${cfg[par1_values]}; do
                eval "export ${cfg[par1_name]}=\"$par1_value\""
                loop_core
                (( i1+=1 )); (( i_run+=1 ))
            done
            unset i1
            ;;
        2)
            i2=0
            for par2_value in ${cfg[par2_values]};do
                eval "export ${cfg[par2_name]}=\"$par2_value\""
                i1=0
                for par1_value in ${cfg[par1_values]};do
                    eval "export ${cfg[par1_name]}=\"$par1_value\""
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
        # 1) echo "${cfg[par1_name]}_${par1_value}";;
        # 2) echo "${cfg[par1_name]}_${par1_value}__${cfg[par2_name]}_${par2_value}";;
        1) echo "$(par_id ${cfg[par1_name]})";;
        2) echo "$(par_id ${cfg[par1_name]} ${cfg[par2_name]})";;
    esac
}

run_dir(){
    echo "runs/$(run_name)"
}

single_run_command(){
    case ${cfg[run_mode]} in
        "free")
            my_time -o "$(run_name).log" \
                run.sh --run_mode=free --task=calc \
                --detailed_stats=${cfg[detailed_stats]} \
                --keep_raw_data=yes \
                --stop_at_nan=no \
                --verbose=$run_verbose \
                "$(run_dir)" &
            ;;
        "assi")
            my_time -o "$(run_name).log" \
                run.sh --run_mode=assi --task=calc \
                --ref_dir=$(run_free_dir) \
                --detailed_stats=${cfg[detailed_stats]} \
                --keep_raw_data=yes \
                --stop_at_nan=no \
                --verbose=$run_verbose \
                "$(run_dir)" &
            ;;
        *     ) error "unsupported run_mode ${cfg[run_mode]}";;
    esac
}

calc_runs(){
    funct_opening 1

    cd "$dataset_dir"
    loop_core(){
        echo $(run_name) >> config/run_names.cfg
        # echo $(run_name) >> ../logs/calc_consumption.log

        echo $i1 >> config/par1_value_pos.cfg
        [[ $dataset_dim -eq 2 ]] && echo $i2 >> config/par2_value_pos.cfg

  # (
  #     source "$MODEL_DIR/model_config.sh"
  #     das_make_all $model
  # )

        single_run_command
        run_pid=$!

        pids[$i_run]=$run_pid
        echo " run $(run_name) [PID ${pids[$i_run]}]"

        if (( i_run%cpus == 0 )); then
            wait_for_child_processes
        fi
    }
    loop_over_set 2> logs/runs_error.log
    wait_for_child_processes # of an incomplete batch

    # Distrubute run calc_consumption.logs
    loop_core(){
        mv "$(run_name).log" "$(run_dir)/logs/calc_consumption.log"; }
    loop_over_set

    funct_closing 1
}

find_successful_runs(){
    funct_opening 1

    # Complement configuration
    local run_names=(${cfg[run_names]})
    read n_comp < "./runs/${run_names[0]}/config/n_comp.cfg"
    set_cfg_file n_comp

    rm -f config/successful_runs.cfg config/crashed_runs.cfg
    touch config/successful_runs.cfg

    loop_core(){
        if [[ -e "./runs/$(run_name)/config/run_crashed.cfg" ]]; then
            echo $(run_name) >> config/crashed_runs.cfg
        else
            echo $(run_name) >> config/successful_runs.cfg
        fi
    }
    loop_over_set
    get_config_file "config/successful_runs.cfg"

    funct_closing 1
}

stat_runs(){
    funct_opening 1

    i_run=1
    for run_name_s in ${cfg[successful_runs]}; do
        run_dir_s="runs/$run_name_s"

        my_time -o "$run_dir_s/logs/stat_consumption.log" \
            run.sh --task=stat \
            --detailed_stats=${cfg[detailed_stats]} \
            --keep_raw_data=no --verbose=$run_verbose \
            "$run_dir_s" &
        pids[$i_run]=$!
        echo " run $run_name_s [PID ${pids[$i_run]}]"

        (( i_run%cpus == 0 )) && wait_for_child_processes "strict"
        (( i_run+=1 ))
    done
    wait_for_child_processes "strict" # of an incomplete batch

    funct_closing 1
}

plot_runs(){
    funct_opening 1

    cd $dataset_dir
    loop_core(){
        run.sh --task=plot \
            --verbose=$run_verbose \
            "$dataset_dir/runs/$(run_name)" &
        pids[$i_run]=$!
        echo " - $(run_name) [PID ${pids[$i_run]}]"
        if (( i_run%cpus == 0 )); then
            wait_for_child_processes "strict"
        fi
    }
    loop_over_set
    wait_for_child_processes "strict" # of an incomplete batch

    funct_closing 1
}

gather_run_set_stats(){
    funct_opening 1

    success_run_dir="$dataset_dir/runs/${successful_runs[0]}/stats"
    scalars=($(find "$success_run_dir" -name "*.dat"))
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
            ${opts[@]} $DAS_DIR/create_netcdf_scalar.ncl &
        pids[$i_run]=$!
        unset opts
        (( i_run%cpus == 0 )) && wait_for_child_processes "strict"
        (( i_run+=1 ))
    done
    wait_for_child_processes "strict" # of an incomplete batch

    funct_closing 1
}

plot_run_set(){
    funct_opening 1

    cd $dataset_dir; rm -fr plots; mcd plots
    # cstat="Xsel"
    cstat=${cfg[diag]} #"Fmean"

    case ${cfg[dataset_dim]} in
        1)
            cp $CODE_DIR/plotting/line_plot.ncl .

            echo " Grouped stats"
            case ${cfg[run_mode]} in
                "free")
                    prefixes=( \
                        nature_Tstdd_${cstat} \
                        free_Emean_Tstdd_${cstat} \
                        free_Esprd_Tmean_${cstat} \
                        free_error_Tstdd_${cstat} )
                    ;;
                "assi")
                    prefixes=( \
                        assi_Emean_Tstdd_${cstat} \
                        assi_Esprd_Tmean_${cstat} \
                        assi_error_Tstdd_${cstat} \
                        assi_Emean_Tstdd_reduc_${cstat} \
                        assi_Esprd_Tmean_reduc_${cstat} \
                        assi_error_Tstdd_reduc_${cstat} \
                        )
                    ;;
            esac

            i_run=0
            for prefix in ${prefixes[@]}; do
                echo " - $prefix"

                #-------------------------------------------------------------
                #  Plot definition
                plot_name  (){ echo $prefix;}
                plot_title (){ echo "$(Quantity_l $prefix)";}
                line_files=( \
                    ../stats/${prefix}*Insta.nc \
                    ../stats/${prefix}*Taver.nc \
                    ../stats/${prefix}*Tanom.nc \
                    )
                # line_files=($(find "../stats" -name "${prefix}*.nc"|sort))
                line_label (){
                    case ${cfg[run_mode]} in
                        "free") echo "$(Tkind_l "$file")";;
                        "assi") echo "$(Tkind_l "$file") $(Ephase_l "$file")";;
                    esac;
                }
                declare -a opts
                case "$prefix" in
                    *error_Tstdd_${cstat}*) opts+=(min_level=0.0);;
    *reduc*)   opts+=(min_level=-20.0); opts+=(max_level=100.0) ;;
        esac
        create_line_plot &
                #-------------------------------------------------------------
        pids[$i_run]=$! ; (( i_run+=1 ))
        if (( i_run%cpus == 0 )); then
            wait_for_child_processes "strict"
        fi
        done
        wait_for_child_processes "strict" # of an incomplete batch

        if [[ ${cfg[run_mode]} == "free" ]]; then
            echo " Individual stats"
            i_run=0
            for scalar in ${cfg[scalars]}; do
                echo " - $scalar"

                    #----------------------------------------------------------
                    #  Plot definition
                plot_name  (){ echo $scalar;}
                plot_title (){ echo "$(Quantity_l $scalar)";}
                line_files=("../stats/${scalar}.nc")
                line_label (){ echo "$(basename $file)"; }
                declare -a opts
                opts+=($(printf '%s=\"%s\"' legend_panel false))
                create_line_plot &
                    #----------------------------------------------------------
                pids[$i_run]=$! ; (( i_run+=1 ))
                if (( i_run%cpus == 0 )); then
                    wait_for_child_processes "strict"
                fi
            done
            wait_for_child_processes "strict" # of an incomplete batch
        fi
        ;;
        2)
        cp $CODE_DIR/plotting/surface_plot.ncl .
        i_run=0
        for scalar in ${cfg[scalars]}; do
            echo " - $scalar"
            create_surface_plot $scalar &
            pids[$i_run]=$! ;  (( i_run+=1 ))
            if (( i_run%cpus == 0 )); then
                wait_for_child_processes "strict"
            fi
        done
        wait_for_child_processes "strict" # of an incomplete batch
        ;;
        esac

        funct_closing 1
        }

        plot_run_set_detailed(){
            funct_opening 1

            if (( ${cfg[dataset_dim]} != 1 )); then
                echo "detailed stat plots only allowed for 1D run sets"
                return 0
            fi

            cd $dataset_dir;
            rm -fr plots_convergence; mcd plots_convergence
            cp $CODE_DIR/plotting/line_plot.ncl .

            run_det_stat_dir="../runs/${successful_runs[0]}/stats_detailed"
            det_stats=($(find "$run_det_stat_dir" -name "*fluct*.nc"))
            det_stats=($(barename ${det_stats[@]}))

            i_plot=0
            for det_stat in ${det_stats[@]}; do
                echo " - $det_stat"
        #-------------------------------------------------------------
        #  Plot definition
                plot_name=${det_stat}
        # plot_name=${det_stat}_convergence
                plot_title="$(Quantity_l $plot_name) fluct. (%)"
                declare -a opts
                opts+=($(printf '%s=\"%s\"' plot_name "$plot_name"))
                opts+=(n_comp=${cfg[n_comp]});
                opts+=(min_level=0.0); opts+=(max_level=50.0)
                echo ${opts[@]} > ${plot_name}.opts
                unset opts
                echo "$plot_title" > ${plot_name}_title.txt
                loop_core(){
                    [[ -e "../$(run_dir)/config/run_crashed.cfg" ]] && return 0
                    file="../$(run_dir)/stats_detailed/${det_stat}.nc"
                    line_label="$(long_name ${cfg[par1_name]}) = $par1_value"
                    echo "$file"       >> ${plot_name}_files.txt
                    echo "$line_label" >> ${plot_name}_labels.txt
                }
                loop_over_set
                my_ncl --logfile="${plot_name}.ncl_log" \
                    $(cat ${plot_name}.opts) line_plot.ncl &
        #-------------------------------------------------------------
                pids[$i_plot]=$! ; (( i_plot+=1 ))
                if (( i_plot%cpus == 0 )); then
                    wait_for_child_processes "strict"
                fi
            done
            wait_for_child_processes "strict" # of an incomplete batch

            funct_closing 1
        }

        report_metainfo(){

            newpage
            if [[ -n ${cfg[report_name]:-} ]];then
                section_title "\bf EXPERIMENT \url{${cfg[report_name]}}"
            fi
            cfg_dir="./config"
            report_config "D.A. Run set configuration"

    # newpage
            cfg_dir="./${cfg[rel_reference_dir]}/config"
            report_config "Free Run Configuration"

            if [[ -f "config/default_config.sh" ]];then
        # newpage;
                include_file "config/default_config.sh"
        # include_file "logs/calc.log"
            fi
        }

#=======================================================================
#> @brief Gather run set stat plots into a pdf document
#=======================================================================
        report_run_set1D(){
            funct_opening 2

            open_report

            plots_dir="./plots"
            plot_title(){ echo ""; }
            cstat=${cfg[diag]}

            column_files=( \
                $plots_dir/free_error_Tstdd_${cstat}.pdf \
                $plots_dir/free_Esprd_Tmean_${cstat}.pdf \
                $plots_dir/free_Emean_Tstdd_${cstat}.pdf)
            plot_column "Free ensemble overall stats"

            if [[ ${cfg[run_mode]} == assi ]]; then
                column_files=( \
                    $plots_dir/assi_error_Tstdd_${cstat}.pdf \
                    $plots_dir/assi_Esprd_Tmean_${cstat}.pdf \
                    $plots_dir/assi_Emean_Tstdd_${cstat}.pdf)
                plot_column "D.A. ensemble overall stats"

                column_files=( \
                    $plots_dir/assi_error_Tstdd_reduc_${cstat}.pdf \
                    $plots_dir/assi_Esprd_Tmean_reduc_${cstat}.pdf \
                    $plots_dir/assi_Emean_Tstdd_reduc_${cstat}.pdf)
                plot_column "D.A. ensemble overall stats reduction"
            fi

            if [[ ${cfg[detailed_stats]} == yes ]]; then

                plots_dir="./plots_convergence"
                plot_title(){ echo "$(Tkind_l "$plot_file")"; }

                file_patterns=( \
                    "free_error_Tstdd_${cstat}_postr*fluct*.pdf" \
                    "free_error_Tstdd_${cstat}_prior*fluct*.pdf" \
                    "free_Esprd_Tmean_${cstat}*postr*fluct*.pdf" \
                    "free_Esprd_Tmean_${cstat}*prior*fluct*.pdf" \
                    "free_Emean_Tstdd_${cstat}*postr*fluct*.pdf" \
                    "free_Emean_Tstdd_${cstat}*prior*fluct*.pdf" \
                    )
                for file_pattern in ${file_patterns[@]};do
                    column_files=($(find "$plots_dir" -name $file_pattern))
                    column_title="$(run_l $file_pattern) $(Quantity_l $file_pattern) Fluctuations"
                    plot_column "$column_title"
                done

                if [[ ${cfg[run_mode]} == assi ]]; then
                    file_patterns=( \
                        "assi_error_Tstdd_${cstat}_postr*fluct*.pdf" \
                        "assi_error_Tstdd_${cstat}_prior*fluct*.pdf" \
                        "assi_error_Tstdd*reduc*postr*fluct*.pdf" \
                        "assi_error_Tstdd*reduc*prior*fluct*.pdf" \
                        "assi_Esprd_Tmean_${cstat}*postr*fluct*.pdf" \
                        "assi_Esprd_Tmean_${cstat}*prior*fluct*.pdf" \
                        "assi_Esprd_Tmean*reduc*postr*fluct*.pdf" \
                        "assi_Esprd_Tmean*reduc*prior*fluct*.pdf" \
                        "assi_Emean_Tstdd_${cstat}*postr*fluct*.pdf" \
                        "assi_Emean_Tstdd_${cstat}*prior*fluct*.pdf" \
                        "assi_Emean_Tstdd*reduc*postr*fluct*.pdf" \
                        "assi_Emean_Tstdd*reduc*prior*fluct*.pdf" \
                        )
                    for file_pattern in ${file_patterns[@]};do
                        column_files=($(find "$plots_dir" -name $file_pattern))
                        column_title="$(run_l $file_pattern) $(Quantity_l $file_pattern) Fluctuations"
                        plot_column "$column_title"
                    done

                fi
            fi

    # Just dump every plot inside
    # plot_files=(../plots/*${cstat}.pdf ../plots_convergence/*.pdf)
    # frame_title(){ echo "$(Tkind_l "$plot_file") $(Ephase_l "$plot_file")";}
    # for plot_file in "${plot_files[@]:-}"; do
    #     plot_frame "$(frame_title)" $plot_file
    # done

            report_metainfo
            close_report
            my_pdflatex "$texfile"

            funct_closing 2
        }

        report_run_set2D(){
            funct_opening 1

            texfile="run_set_report.tex"
            case "${cfg[n_comp]}" in
                1) orientation=portrait;;
                2) orientation=landscape;;
            esac
            open_report

            cfg_dir="./${cfg[rel_reference_dir]}/config"
            report_config "Free Run Configuration"
            cfg_dir="./config"
            report_config "D.A. Run Configuration"
            newpage

            plots_dir="./plots"
            cstat=${cfg[diag]}

            row_title(){ echo "$(run_l $row_prefix)"; }

            row_prefixes=( \
                "$plots_dir/free_error_Tstdd_${cstat}_prior" \
                "$plots_dir/assi_error_Tstdd_${cstat}_prior" \
                "$plots_dir/assi_error_Tstdd_${cstat}_postr" )
            plot_Tkind_row_column "RMSE"

            row_prefixes=( \
                "assi_error_Tstdd_reduc_${cstat}_prior" \
                "assi_error_Tstdd_reduc_${cstat}_postr" )
            plot_Tkind_row_column "RMSE Reduction"

            row_prefixes=( \
                "free_Esprd_Tmean_${cstat}_prior" \
                "assi_Esprd_Tmean_${cstat}_prior" \
                "assi_Esprd_Tmean_${cstat}_postr" )
            plot_Tkind_row_column "SPREAD"

            row_prefixes=( \
                "assi_Esprd_Tmean_reduc_${cstat}_prior" \
                "assi_Esprd_Tmean_reduc_${cstat}_postr" )
            plot_Tkind_row_column "SPREAD Reduction"

            row_prefixes=( \
                "free_Emean_Tstdd_${cstat}_prior" \
                "assi_Emean_Tstdd_${cstat}_prior" \
                "assi_Emean_Tstdd_${cstat}_postr" )
            plot_Tkind_row_column "STATE STANDARD DEVIATION"

            row_prefixes=( \
                "assi_Emean_Tstdd_reduc_${cstat}_prior" \
                "assi_Emean_Tstdd_reduc_${cstat}_postr" )
            plot_Tkind_row_column "STATE STANDARD DEVIATION Reduction"

    # echo "\newpage"                    >> $texfile

    # row_files=($plots_dir/nature_Tstdd_${cstat}_{Insta,Taver,Tanom}.pdf)
    # plot_row "Nature state Std. Deviation"

    # if [[ -e $plots_dir/nature_Tstdd_${cstat}.pdf ]]; then
    #     plot_width="0.6\textwidth"
    #     plot_frame " " $plots_dir/nature_Tstdd_${cstat}.pdf
    # fi

    #plot_files=($plots_dir/*.pdf)
    #frame_title(){ echo $(basename ${plot_file%.*});}
    #plot_row

    #if [[ ${cfg[run_mode]} == "assi" ]]; then
        #echo "\begin{minipage}[c]{\textwidth}" >> $texfile
                                #section_title "RMSE"
        #row_files=($plots_dir/assi*error_Tstdd_${cstat}*postr*.pdf)
        #plot_row "- D.A. Analysis" no
        #row_files=($plots_dir/assi*error_Tstdd_${cstat}*prior*.pdf)
        #plot_row "- D.A. Forecast" no
        #row_files=($plots_dir/free*error_Tstdd_${cstat}*prior*.pdf)
        #plot_row "- Free Forecast" no
        #echo "\end{minipage}"                    >> $texfile
    #fi

    #echo "\begin{minipage}[c]{\textwidth}" >> $texfile
    #if [[ ${cfg[run_mode]} == "assi" ]]; then
        #row_files=($plots_dir/assi*error*reduc*postr*.pdf)
        #plot_row "D.A. Analysis Error Reduction" no
        #row_files=($plots_dir/assi*error*reduc*prior*.pdf)
        #plot_row "D.A. Forecast Error Reduction" no
    #fi
    #row_files=($plots_dir/free*error_Tstdd_${cstat}*prior*.pdf)
    #plot_row "Free Forecast RMSE" no
    #echo "\end{minipage}"                    >> $texfile

    #echo "\begin{minipage}[c]{\textwidth}" >> $texfile
    #if [[ ${cfg[run_mode]} == "assi" ]]; then
        #row_files=($plots_dir/assi*Esprd*reduc*prior*.pdf)
        #plot_row "D.A. Forecast Spread Reduction"
        #row_files=($plots_dir/assi*Esprd*reduc*postr*.pdf)
        #plot_row "D.A. Analysis Spread Reduction"
    #fi
    #row_files=($plots_dir/free*Esprd*prior*.pdf)
    #plot_row "Free Forecast Spread" no
    #echo "\end{minipage}"                    >> $texfile

    #echo "\begin{minipage}[c]{\textwidth}" >> $texfile
    #if [[ ${cfg[run_mode]} == "assi" ]]; then
        #row_files=($plots_dir/assi*Tstdd*reduc*prior*.pdf)
        #plot_row "D.A. Forecast Variance Reduction" no
        #row_files=($plots_dir/assi*Tstdd*reduc*postr*.pdf)
        #plot_row "D.A. Analysis Variance Reduction" no
    #fi
    #row_files=($plots_dir/free*Tstdd*prior*.pdf)
    #plot_row "Free Forecast Std. Deviation" no
    #echo "\end{minipage}"                    >> $texfile

            report_metainfo
            close_report
            my_pdflatex "$texfile"

            funct_closing 1
        }


#    save_hard_drive #  needs to be more robust
# save_hard_drive(){
#     funct_opening 1

#     cd $dataset_dir
#     storage_saving=1
#     if [[ $storage_saving -ge 1 ]]; then
#         # if [ ${cfg[run_mode]} == "free" ];then
#         #     find . -wholename *nature_Insta_all.nc | xargs rm
#         # fi
#         find . -name *nature_obs_clean_Insta_4b.grd | xargs rm
#         find . -name *.list | xargs rm
#     fi
#     if [[ $storage_saving -ge 2 ]]; then
#         find . -wholename */runs/*.nc | xargs rm
#     fi

#     funct_closing 1
# }


