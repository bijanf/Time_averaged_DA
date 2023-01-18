#----------------------------
#> latex reporting functions
#----------------------------
open_report(){
    texfile=${texfile:-"report.tex"}
    orientation=${orientation:-portrait}
    columns=${columns:-"onecolumn"}
    margins=${margins:-"[left=5mm,right=5mm,top=15mm,bottom=20mm]"}
    quality=${quality:-"final"} # draft

    echo " Creating $texfile"

#    echo "\documentclass[draft,landscape]{article}" > $texfile
    echo "\documentclass[$orientation,$columns]{article}"> $texfile
    echo "\usepackage[$quality]{graphicx}"        >> $texfile
    echo "\usepackage{pdflscape}"                 >> $texfile
    echo "\usepackage{url}"                       >> $texfile
    echo "\usepackage{pbox}"                      >> $texfile
    echo "\usepackage${margins}{geometry}"        >> $texfile
    echo "\usepackage{multirow}"                  >> $texfile
    echo "\usepackage{multicol}"                  >> $texfile
    echo "\usepackage{spverbatim}"                >> $texfile
    echo "\begin{document}"                       >> $texfile
    echo "\centering"                             >> $texfile
}

close_report(){
    echo "\end{document}"                        >> $texfile
}

report_title(){
    fontsize=${fontsize:-"\Large"}
    echo "\centerline{\raisebox{3mm}{\fbox{\pbox{\textwidth}{\LARGE\bf $1}}}}">> $texfile
    #echo "\centerline{\raisebox{3mm}{${fontsize}\bf $1}}">> $texfile
    echo "\vspace*{1mm}\\">> $texfile
}

section_title(){
    fontsize=${fontsize:-"\Large"}
    #echo "\centerline{\raisebox{3mm}{\fbox{\pbox{\textwidth}{\LARGE\bf $1}}}}">> $texfile
    echo "\centerline{\raisebox{3mm}{${fontsize}\bf $1}}">> $texfile
    echo "\vspace*{1mm}\\">> $texfile
}

newpage(){ 
echo "\newpage"  >> $texfile; }

report_stats(){
    title=${1:-"Overall Statistics"}
    fontsize=${fontsize:-"\normalsize"} #large
    echo "\leftline{\bf $fontsize $title}"    >> $texfile
    echo "\begin{verbatim}"                   >> $texfile
    stats_dir=${stats_dir:-"./stats"}
    print_statistics                          >> $texfile
    echo "\end{verbatim}"                     >> $texfile
}

report_config(){
    title=${1:-"Configuration"}
    cfg_dir=${cfg_dir:-"./config"}
    fontsize=${fontsize:-"\normalsize"} #large
    echo "\leftline{\bf $fontsize $title}">> $texfile
    echo "\begin{verbatim}"               >> $texfile
    print_config "$cfg_dir"               >> $texfile
    echo "\end{verbatim}"                 >> $texfile
}

include_file(){
    local file_path="$1"
    [[ -f "$file_path" ]] || return 0
    
    local file_name=$(basename $file_path)
    local title=${2:-"\url{$file_name}"}
    fontsize=${fontsize:-"\normalsize"} #large
    echo "\leftline{\bf $fontsize $title}">> $texfile
    echo "$fontsize"                      >> $texfile
    echo "\begin{spverbatim}"             >> $texfile
    cat "$file_path"                      >> $texfile
    echo "\end{spverbatim}"               >> $texfile
}

plot_frame(){
    local plot_title; local plot_width; local subtitle_on
    PARSED_ARGS=$(getopt -n "plot_frame" -o h --long "title:,width:,subtitle_on:" -- "$@")
    eval set -- "$PARSED_ARGS"
    while true; do
        case "$1" in
            --h          )   echo "no help"; shift 1;;
            --title      )  plot_title="$2"; shift 2;;
            --width      )  plot_width="$2"; shift 2;;
            --subtitle_on) subtitle_on="$2"; shift 2;;
            --           )         shift 1 ; break  ;;
        esac
    done
    local plot_file=$1;
    plot_title=${plot_title:-}
    plot_width=${plot_width:-"0.48\textwidth"}
    subtitle_on=${subtitle_on:-yes}

    echo "\begin{minipage}[c]{$plot_width}" >> $texfile
    if [[ -n $plot_title ]];then
        echo "\centerline{\large\bf\raisebox{-5mm}{$plot_title}}" >> $texfile
    fi
    if [[ $subtitle_on == yes ]];then
        plot_name="$(basename ${plot_file%.*})"
        echo "\centerline{\scriptsize\raisebox{2mm}{\url{($plot_name})}}" >> $texfile
        #\url{} is an easy workaround to latex-underscore issue
    fi
        # echo "\vspace*{-2mm}" >> $texfile
    echo "\noindent\includegraphics[width=1.0\textwidth]{$plot_file}" >> $texfile
    echo "\end{minipage}"                    >> $texfile
}

plot_frame_h(){
    local plot_file=$1;
    local plot_title=$2
    local plot_name_on=${3:-yes}
    frame_height=${frame_height:-"\textheight"}

    echo "\begin{minipage}[t][$frame_height][t]{\textwidth}" >> $texfile
    # echo "\begin{minipage}[c]{$plot_width}" >> $texfile
    echo "\centerline{\large\bf\raisebox{-5mm}{$plot_title}}" >> $texfile
    if [[ $plot_name_on == yes ]];then
        plot_name="$(basename ${plot_file%.*})"
        echo "\centerline{\scriptsize\raisebox{2mm}{\url{($plot_name})}}" >> $texfile
        #\url{} is an easy workaround to latex-underscore issue
    fi
        # echo "\vspace*{-2mm}" >> $texfile
    echo "\noindent\includegraphics[height=\textheight]{$plot_file}" >> $texfile
    echo "\end{minipage}"                    >> $texfile
}

plot_row(){
          #PARSED_ARGS=$(getopt -n "plot_row" -o --long "title:" -- "$@")
    #eval set -- "$PARSED_ARGS"
    #while true; do
        #case "$1" in
            #--title ) row_title="$2"; shift 2;;
                                                #--      )       shift 1 ; break  ;;
                                #esac
                #done
    local row_title=${1:-}
    local plot_name_on=${2:-yes}
    row_width=${row_width:-"\textwidth"}
    file_number=${#row_files[@]}
    (( $file_number == 0 )) && return 0
    plot_width="$(echo "print (0.96/(${file_number}))"|python)\textwidth"

    echo "\begin{minipage}[c]{$row_width}" >> $texfile
    if [[ -n ${row_title:-} ]];then
        echo "\centerline{\fbox{\pbox{\textwidth}{\Large\bf \raisebox{0mm}{$row_title}}}}"  >> $texfile
    fi
    for plot_file in "${row_files[@]}"; do
        [[ -e "$plot_file" ]] || continue
        plot_frame --title="$(plot_title)" \
      --subtitle_on=$plot_name_on \
            --width=$plot_width "$plot_file"
        echo "\vspace*{3mm}" >> $texfile
        echo "\hfill" >> $texfile
    done
    echo "\end{minipage}"                  >> $texfile
}

plot_column(){
    column_title=${1:-""}
    column_width=${column_width:-"\textwidth"}
    (( ${#column_files[@]} == 0 )) && return 0
    file_number=${#column_files[@]}
    column_height=${column_height:-"0.95"}
    plot_height="$(echo "print (${column_height}*0.90/(${file_number}))"|python)\textheight"

    echo "\begin{minipage}[t][${column_height}\textheight][c]{\textwidth}" >> $texfile
    section_title "$column_title"
    echo "\centering"                            >> $texfile
    for plot_file in "${column_files[@]}"; do
        [[ -e "$plot_file" ]] || continue
        plot_name="$(basename ${plot_file%.*})"
        echo "\centerline{\Large\bf $(plot_title) \scriptsize\url{($plot_name})}" >> $texfile
        echo "\noindent\includegraphics[height=$plot_height]{$plot_file}" >> $texfile
    done
    echo "\end{minipage}"                    >> $texfile
}

plot_array_bunch1D_horizontal(){
    array_title=${1:-""}
    array_subtitle=${2:-""}
    bunch_par1_span_length=${cfg[bunch_par1_span_length]}
    plot_width="$(echo 0.85/${cfg[bunch_par1_span_length]}|bc -l)"
    plot_width="${plot_width}\textwidth"

    bunch_par1_values=(${cfg[bunch_par1_values]})

    delimiter(){
        if (( i1 < (bunch_par1_span_length - 1) )); then
            echo "&"
        else
            echo "\\\\"
        fi
    }
    tab_format(){
        case "$bunch_par1_span_length" in
            1) echo "|c|";;
            2) echo "|c|c|";;
            3) echo "|c|c|c|";;
            4) echo "|c|c|c|c|";;
            5) echo "|c|c|c|c|c|";;
            6) echo "|c|c|c|c|c|c|";;
            *) error "Unsupported bunch_par1_span_length $bunch_par1_span_length";;
        esac
    }

    echo "\begin{minipage}[c]{\textwidth}"   >> $texfile
    echo "\vspace*{3mm}"                     >> $texfile
    echo "\centering"                        >> $texfile
    echo "\centerline{\Large \bf $array_title \footnotesize\url{$array_subtitle}}" >> $texfile
    # echo "\centerline{\Large \bf $array_title}" >> $texfile
    echo "\vspace*{3mm}"                     >> $texfile

    echo "\begin{tabular}{$(tab_format)}"    >> $texfile
    echo "\hline"                            >> $texfile

    label1="$(long_name ${cfg[bunch_par1_name]})"
                #label1="\url{${cfg[bunch_par1_name]}}"
    echo "\multicolumn{$bunch_par1_span_length}{|c|}{$label1} \\\\" >> $texfile
    echo "\hline"                            >> $texfile

    for ((i1 = 0 ; i1 < bunch_par1_span_length; i1++)); do
        bunch_par1_value=${bunch_par1_values[$i1]}
                                #label2="\url{$bunch_par1_value}"
        label2="$(label $bunch_par1_value ${cfg[bunch_par1_name]})"
        echo "$label2"                          >> $texfile
        delimiter  >> $texfile
    done
    echo "\hline"                            >> $texfile
    for ((i1 = 0 ; i1 < bunch_par1_span_length; i1++)); do
        bunch_par1_value=${bunch_par1_values[$i1]}
        echo "\parbox[s]{$plot_width}{\includegraphics[width=$plot_width]{$(plot_path)}}" >> $texfile
        delimiter  >> $texfile
    done
    echo "\hline"                            >> $texfile
    echo "\end{tabular}"                     >> $texfile
    echo "\end{minipage}"                    >> $texfile
}

plot_array_vertical(){
    array_title=${1:-""}
    array_subtitle=${2:-""}
    array_width=${array_width:-"0.45\textwidth"}
    # plot_width="$(echo 0.15/${cfg[bunch_par1_span_length]}|bc -l)"
#    plot_width="$(echo 0.85/${cfg[bunch_par1_span_length]}|bc -l)"
#    plot_width="${plot_width}\textwidth"
#    plot_height="$(echo 0.85/${cfg[bunch_par1_span_length]}|bc -l)"
#    plot_height="${plot_width}\textheight"
    plot_width="0.8\textwidth"

    #par1_values=(${cfg[par1_values]})
    #par1_span_length=${cfg[par1_span_length]}
    #par1_name=${cfg[par1_name]}

    delimiter (){ echo "\\\\"; }
    tab_format(){ echo "|c|c|c|"; }

    echo "\begin{minipage}[c]{$array_width}"      >> $texfile
    echo "\vspace*{3mm}"                        >> $texfile
    echo "\centering"                           >> $texfile
    if [[ -n $array_title ]] && [[ -z $array_subtitle ]];then
        echo "\centerline{\Large \bf $array_title}" >> $texfile
        echo "\vspace*{3mm}"                        >> $texfile
    fi
    if [[ -n $array_title ]] && [[ -n $array_subtitle ]];then
        echo "\centerline{\Large \bf $array_title \footnotesize\url{$array_subtitle}}" >> $texfile
        echo "\vspace*{3mm}"                        >> $texfile
    fi
    echo "\begin{tabular}{$(tab_format)}"       >> $texfile
    echo "\hline"                               >> $texfile

    label3="$(long_name $par1_name)"
    echo "\multirow{$par1_span_length}{*}{\rotatebox[origin=c]{90}{$label3}}" >> $texfile

    for ((i1 = 0 ; i1 < par1_span_length; i1++)); do
        par1_value=${par1_values[$i1]}
        label4="$(label $par1_value $par1_name)"
        echo "&\rotatebox[origin=b]{90}{$label4} &" >> $texfile
        # echo "\parbox[s]{$plot_width}{\includegraphics[width=$plot_width]{$(plot_path)}}" >> $texfile
        echo "\parbox[s]{$plot_width}{\includegraphics[width=$plot_width]{$(plot_path)}}" >> $texfile
        echo "\\\\\cline{2-$((1 + 2))}" >> $texfile
    done
    echo "\hline"                               >> $texfile
    echo "\end{tabular}"                        >> $texfile
    echo "\end{minipage}"                       >> $texfile
}


plot_array_bunch1D_vertical(){
    array_title=${1:-""}
    array_subtitle=${2:-""}
    array_width=${array_width:-"0.45\textwidth"}
    # plot_width="$(echo 0.15/${cfg[bunch_par1_span_length]}|bc -l)"
#    plot_width="$(echo 0.85/${cfg[bunch_par1_span_length]}|bc -l)"
#    plot_width="${plot_width}\textwidth"
#    plot_height="$(echo 0.85/${cfg[bunch_par1_span_length]}|bc -l)"
#    plot_height="${plot_width}\textheight"
    plot_width="0.8\textwidth"

    bunch_par1_values=(${cfg[bunch_par1_values]})

    delimiter (){ echo "\\\\"; }
    
    tab_format(){ echo "|c|c|c|"; }

    echo "\begin{minipage}[c]{$array_width}"      >> $texfile
    echo "\vspace*{3mm}"                        >> $texfile
    echo "\centering"                           >> $texfile
    echo "\centerline{\Large \bf $array_title \footnotesize\url{$array_subtitle}}" >> $texfile
                #echo "\centerline{\LARGE \bf $array_title}" >> $texfile
    echo "\vspace*{3mm}"                        >> $texfile

    echo "\begin{tabular}{$(tab_format)}"       >> $texfile
    echo "\hline"                               >> $texfile

    label3="$(long_name ${cfg[bunch_par1_name]})"
    echo "\multirow{${cfg[bunch_par1_span_length]}}{*}{\rotatebox[origin=c]{90}{$label3}}" >> $texfile

#     for ((i1 = 0 ; i1 < cfg[bunch_par1_span_length]; i1++)); do
#   bunch_par1_value=${bunch_par1_values[$i1]}
    loop_over_bunch_par1_core(){
	label4="$(label $bunch_par1_value ${cfg[bunch_par1_name]})"
	echo "&\rotatebox[origin=b]{90}{$label4} &" >> $texfile
      # echo "\parbox[s]{$plot_width}{\includegraphics[width=$plot_width]{$(plot_path)}}" >> $texfile
	echo "\parbox[s]{$plot_width}{\includegraphics[width=$plot_width]{$(plot_path)}}" >> $texfile
	echo "\\\\\cline{2-$((1 + 2))}" >> $texfile
    }
    loop_over_bunch_par1
#    done
    echo "\hline"                               >> $texfile
    echo "\end{tabular}"                        >> $texfile
    echo "\end{minipage}"                       >> $texfile
}

plot_array_bunch2D(){
    array_title=${1:-""}
    array_subtitle=${2:-""}
    array_width=${array_width:-"\textwidth"}
    # plot_width="$(echo 0.15/${cfg[bunch_par1_span_length]}|bc -l)"
    plot_width="$(echo 0.85/${cfg[bunch_par1_span_length]}|bc -l)"
    plot_width="${plot_width}\textwidth"

    bunch_par1_values=(${cfg[bunch_par1_values]})
    bunch_par2_values=(${cfg[bunch_par2_values]})

    delimiter(){
        if (( i1 < (cfg[bunch_par1_span_length] - 1) )); then
            echo "&"
        else
            echo "\\\\"
        fi
    }
    tab_format(){
        case "${cfg[bunch_par1_span_length]}" in
            1) echo "|c|c|c|";;
            2) echo "|c|c|c|c|";;
            3) echo "|c|c|c|c|c|";;
            4) echo "|c|c|c|c|c|c|";;
            5) echo "|c|c|c|c|c|c|c|";;
            6) echo "|c|c|c|c|c|c|c|c|";;
            7) echo "|c|c|c|c|c|c|c|c|c|";;
            *) error "Unsupported bunch_par1_span_length ${cfg[bunch_par1_span_length]}";;
        esac
    }

    echo "\begin{minipage}[c]{$array_width}"    >> $texfile
    echo "\vspace*{3mm}"                        >> $texfile
    echo "\centering"                           >> $texfile
    echo "\centerline{\Large \bf $array_title \footnotesize\url{$array_subtitle}}" >> $texfile
                #echo "\centerline{\LARGE \bf $array_title}" >> $texfile
    echo "\vspace*{3mm}"                        >> $texfile

    echo "\begin{tabular}{$(tab_format)}"       >> $texfile
    echo "\hline"                               >> $texfile
    label1="$(long_name ${cfg[bunch_par1_name]})"
                #label1="\url{${cfg[bunch_par1_name]}}"
    echo "&&\multicolumn{${cfg[bunch_par1_span_length]}}{|c|}{$label1} \\\\" >> $texfile
    echo "\hline"                               >> $texfile

    echo "&&"                                   >> $texfile

    loop_over_bunch_par1_core(){
        label2="$(label $bunch_par1_value ${cfg[bunch_par1_name]})"
        echo "$label2"                          >> $texfile
        delimiter                               >> $texfile
    }
    loop_over_bunch_par1
    echo "\hline"                               >> $texfile

    loop_over_bunch_par2_core(){
        if (( i2 == 0 )); then
            label3="$(long_name ${cfg[bunch_par2_name]})"
            echo "\multirow{${cfg[bunch_par2_span_length]}}{*}{\rotatebox[origin=c]{90}{$label3}}" >> $texfile
        fi
                                #label4="\url{$bunch_par2_value}"
        label4="$(label $bunch_par2_value ${cfg[bunch_par2_name]})"
        echo "&\rotatebox[origin=b]{90}{$label4} &" >> $texfile

	loop_over_bunch_par1_core(){
            echo "\parbox[s]{$plot_width}{\includegraphics[width=$plot_width]{$(plot_path)}}" >> $texfile

            if (( i1 < (cfg[bunch_par1_span_length] - 1) )); then
                echo "&"                        >> $texfile
            else
                echo "\\\\\cline{2-$((cfg[bunch_par1_span_length] + 2))}" >> $texfile
            fi
                                                # delimiter  >> $texfile
	}
	loop_over_bunch_par1
    }
    loop_over_bunch_par2

    echo "\hline"                               >> $texfile
    echo "\end{tabular}"                        >> $texfile
    echo "\end{minipage}"                       >> $texfile
}

# echo ""
# echo $(plot_path)
# echo run_set_name $(run_set_name)
# echo "par_id \${cfg[bunch_par1_name]} =   $(par_id ${cfg[bunch_par1_name]})"
# echo "par_id \${cfg[bunch_par1_name]} \${cfg[bunch_par2_name]} =  $(par_id ${cfg[bunch_par1_name]} ${cfg[bunch_par2_name]})"
# echo "bunch_par2_name = $bunch_par2_name"
# echo "bunch_par2_value = $bunch_par2_value"
# echo "bunch_par1_name $bunch_par1_name"
# echo "bunch_par1_value = $bunch_par1_value"
# echo "cfg[bunch_par2_name] = ${cfg[bunch_par2_name]}"
# #echo "cfg[bunch_par2_value] ${cfg[bunch_par2_value]}"
# echo "comp_localization = $comp_localization"
# echo "update_mode = $update_mode"

#     for ((i2 = 0 ; i2 < cfg[bunch_par2_span_length]; i2++)); do
#         bunch_par2_value=${bunch_par2_values[$i2]}
# 
#         if (( i2 == 0 )); then
#                                                 #label3="\url{${cfg[bunch_par2_name]}}"
#             label3="$(long_name ${cfg[bunch_par2_name]})"
#             echo "\multirow{${cfg[bunch_par2_span_length]}}{*}{\rotatebox[origin=c]{90}{$label3}}" >> $texfile
#         fi
#                                 #label4="\url{$bunch_par2_value}"
#         label4="$(label $bunch_par2_value ${cfg[bunch_par2_name]})"
#         echo "&\rotatebox[origin=b]{90}{$label4} &" >> $texfile
# 
#         for ((i1 = 0 ; i1 < cfg[bunch_par1_span_length]; i1++)); do
# 
#             bunch_par1_value=${bunch_par1_values[$i1]}
# echo $bunch_par1_value
# echo $bunch_par2_value
# echo $(plot_path)
# echo run_set_name $(run_set_name)
# echo "par_id stuff 1    $(par_id ${cfg[bunch_par1_name]})"
# echo "par_id stuff 2    $(par_id ${cfg[bunch_par1_name]} ${cfg[bunch_par2_name]})"
# #echo "bunch_par2_name $bunch_par2_name"
# echo "bunch_par2_value $bunch_par2_value"
# echo "cfg[bunch_par2_name] ${cfg[bunch_par2_name]}"
# #echo "cfg[bunch_par2_value] ${cfg[bunch_par2_value]}"
# 
#             echo "\parbox[s]{$plot_width}{\includegraphics[width=$plot_width]{$(plot_path)}}" >> $texfile
# 
#             if (( i1 < (cfg[bunch_par1_span_length] - 1) )); then
#                 echo "&"                        >> $texfile
#             else
#                 echo "\\\\\cline{2-$((cfg[bunch_par1_span_length] + 2))}" >> $texfile
#             fi
#                                                 # delimiter  >> $texfile
#         done
#     done

plot_Tkind_row_column(){
    title="$1"
    column_width=${column_width:-"\textwidth"}
    plot_title(){ echo "$(Tkind_l "$plot_file")"; }

    row_width=$column_width
    # echo "\begin{minipage}[c]{$column_width}" >> $texfile
    echo "\newpage" >> $texfile
    section_title "$title"
    for row_prefix in ${row_prefixes[@]};do
        row_files=(${row_prefix}_{Insta,Taver,Tanom}.pdf)
        if [[ $row_prefix == ${row_prefixes[0]} ]];then
            plot_title(){ echo "$(Tkind_l "$plot_file")"; }
        else
            plot_title(){ echo ""; }
        fi
        plot_row "$(row_title)" no
    done
    # echo "\end{minipage}"                    >> $texfile
}

free_run_set1D_plots_page(){
    plots_dir=${1:-"./plots"}

    plot_title(){ echo ""; }
    column_files=( \
        $plots_dir/free_error_Tstdd_${cfg[diag]}.pdf \
        $plots_dir/free_Esprd_Tmean_${cfg[diag]}.pdf \
        $plots_dir/free_Emean_Tstdd_${cfg[diag]}.pdf)
    plot_column "Free ensemble overall stats"

    row_files=($plots_dir/nature_Tstdd_${cfg[diag]}.pdf)
    plot_row "Nature Standard Deviation" no

    # row_files=($plots_dir/nature_Tstdd_${cfg[diag]}*{Insta,Taver,Tanom}.pdf)
    # plot_row "Nature Standard Deviation" no
}

free_run_set2D_plots_page(){
    plots_dir=${1:-"./plots"}

    row_prefixes=( \
        $plots_dir/free_error_Tstdd_${cfg[diag]}_prior \
        $plots_dir/free_Esprd_Tmean_${cfg[diag]}_prior \
        $plots_dir/free_Emean_Tstdd_${cfg[diag]}_prior )
    row_title(){ echo "$(run_l $row_prefix) $(Quantity_l $row_prefix)"; }
    plot_Tkind_row_column "FREE RUN OVERALL STATS"

    row_files=($plots_dir/nature_Tstdd_${cfg[diag]}*{Insta,Taver,Tanom}.pdf)
    plot_row "Nature Standard Deviation" no
}

universal_report(){
    funct_opening 2

    plot_width=${plot_width:-"0.48\textwidth"}
    plots_dir=${plots_dir:-'plots'}

    open_report
   
    run_set_plots=($(find $plots_dir -path "*.pdf"))

    frame_title(){ echo "";}
    for run_set_plot in "${run_set_plots[@]}"; do
        plot_frame $run_set_plot
    done
    close_report

    my_pdflatex "$texfile"

    funct_closing 2
}


link_free_run_set_plots(){
    funct_opening 1

    # Link reference free run_set plots here to produce a complete report
    cd $dataset_dir/plots
    find "../${cfg[rel_reference_dir]}/plots" -name "*.pdf" \
        -print0 | xargs -0 -I{} ln -fs {} .
    if [[ ${cfg[detailed_stats]} == "yes" ]]; then
        cd $dataset_dir/plots_convergence
        find "../${cfg[rel_reference_dir]}/plots_convergence" -name "*.pdf" \
            -print0 | xargs -0 -I{} ln -fs {} .
    fi
    cd $dataset_dir

    funct_closing 1
}
