#!/usr/bin/env bash
set -eu # strict config (non-zero returns and unset vars halt script)
source "$DAS_DIR/common_tools.sh"
source "$DAS_DIR/run__1.sh"
source "$MODEL_DIR/model_tools.sh"

declare -A cfg flag

usage(){ cat <<EOF
Perform individual run.
Usage: $(basename $0) [--task=*] [--run_mode=*] [--ref_dir=*] [--calc=*]
        [--stat=*] [--only=*] [--detailed_stats=*] [-h|--help] dataset_dir
*/^: Required/optional argument
EOF
exit 1
}

#================================================================
#> @brief Perform individual run
#> Needed environment variable
#> - verity Calculation significance level
#>   - 0: Very fast config for testing and comp. time estimation
#>   - 1: Real config to get statistically significant results.
#================================================================
run(){
    long_opts="help,task:,run_mode:,ref_dir:,calc:,stat:,plot:,task:,detailed_stats::,stop_at_nan:,keep_raw_data::,verbose:,replot:,report_name:"
    PARSED_ARGS=$(getopt -n "$0" -o h --long "$long_opts" -- "$@")
    eval set -- "$PARSED_ARGS"

    while true; do
        case "$1" in
            -h|--help       )                    usage  ;;
            --run_mode      )       run_mode=$2; shift 2;;
            --ref_dir       )  reference_dir=$2; shift 2;;
            --calc          )     flag[calc]=$2; shift 2;;
            --stat          )     flag[stat]=$2; shift 2;;
            --plot          )     flag[plot]=$2; shift 2;;
            --docu          )     flag[docu]=$2; shift 2;;
            --view          )     flag[view]=$2; shift 2;;
            --task          )      only_task=$2; shift 2;;
            --detailed_stats) detailed_stats=$2; shift 2;;
            --stop_at_nan   )    stop_at_nan=$2; shift 2;;
            --keep_raw_data )  keep_raw_data=$2; shift 2;;
            --replot        )         replot=$2; shift 2;;
            --verbose       )        verbose=$2; shift 2;;
            --report_name   )    report_name=$2; shift 2;;
            --              )           shift 1; break  ;;
        esac
    done
    dataset_dir=$1

    dataset_dir="$(absolute_path "$dataset_dir")"

    # set_task_flags

    detailed_stats=${detailed_stats:-no}
    keep_raw_data=${keep_raw_data:-no}
    dataset_kind=$(barename $0)
    dataset_dim=0
    verbose=${verbose:-1}

    [[ -n ${replot:-} ]] && \
        rm -f "$dataset_dir/logs/plot.success" "$dataset_dir/logs/docu.success"

    perform_simulation_tasks
}

config_function(){
    funct_opening 2

    add_to_cfg "dataset_kind" "dataset_dim" "run_mode" "model"
    add_to_cfg "verity" "detailed_stats" #"stop_at_nan"
    add_to_cfg "keep_raw_data" "diag"

    if [[ ${cfg[detailed_stats]} == yes ]]; then
        add_to_cfg "cycles_span"
        set_par_values_file "stat_cycles" "$cycles_span"
    fi

    if [[ ${cfg[run_mode]} == assi ]];then
        run_free_dir=$reference_dir
        [[ -d "$run_free_dir" ]] || \
            error "Non existent run_free_dir $run_free_dir"
        rel_run_free_dir=$(relpath $run_free_dir $dataset_dir )
        add_to_cfg "run_free_dir" "rel_run_free_dir"
    fi
    [[ -n ${report_name:-} ]] && add_to_cfg "report_name"

    store_cfg;  print_cfg

    # cp $CODE_DIR/models/$model/* ./logs

    funct_closing 2
}

calc_function(){
    funct_opening 2

    # To make sure executables are updated
    # fortran code is compiled for every run
    rm -fr code
    rm -fr bin
    cd $CODE_DIR
    create_code_copy "$dataset_dir"
    cd "$dataset_dir/code"
    source ./initialize_das.sh
    
    model_update_code
    $CODE_DIR/das_make_all.sh $model || error "Compilation crashed"


    cd "$dataset_dir"

    mcd raw_data
    (calc_run) && (postprocess_run) || echo yes > "run_crashed.cfg"

    mv *.cfg ../config  # gather config files
    find . -maxdepth 1 -type l -delete # delete soft links
    # find . -maxdepth 1 -type l | xargs rm -f # delete soft links
    #rm -fr ../code
    find ../code -delete
    find ../bin  -delete

    funct_closing 2
}

#=======================================================================
#> @brief Do statistical analysis of a run
#=======================================================================
stat_function(){
    funct_opening 2

    if [[ -e "$dataset_dir/config/run_crashed.cfg" ]] ; then
        echo "Run crashed during calc phase. Stat phase aborted"
        return 0
    fi

    # stat reference free dataset
    if [[ ${cfg[run_mode]} == "assi" ]]; then
        [[ -f "${cfg[rel_run_free_dir]}/logs/stat.success" ]] || \
            run.sh --task=stat "${cfg[rel_run_free_dir]}"
    fi

    if [[ ! -d "$dataset_dir/raw_data" ]] ; then
        echo "Raw output has been deleted. Stat phase aborted"
        return 0
    fi

    case ${cfg[run_mode]} in
        "free") Ephases=(prior);;
        "assi") Ephases=(prior postr);;
        *     ) error "unsupported run_mode ${cfg[run_mode]}";;
    esac

    # rm -fr stats; mcd stats
    # rm -fr stats; mkdir -p stats; cd stats
    [[ -d stats ]] && find stats -delete; mkdir -p stats; cd stats

    stat_run

    #if [[ $detailed_stats == "yes" ]]; then
        #cd "$dataset_dir"
        #rm -fr stats_detailed stat_cycles-*
        #run_detailed_stats
    #fi

    if [[ ${cfg[keep_raw_data]} == "no" ]] && [[ ${cfg[run_mode]} == "assi" ]]; then
        rm -rf "$dataset_dir/raw_data"
        find -L "$dataset_dir/stats" -type l -delete # remove broken links
    fi

    funct_closing 2
}

plot_function(){
    funct_opening 2

    # [[ -f "${cfg[rel_run_free_dir]}/config/n_comp.cfg" ]] && \
    # 	cp "${cfg[rel_run_free_dir]}/config/n_comp.cfg" config/

    if [[ -e $dataset_dir/config/run_crashed.cfg ]] ; then
        echo "Run crashed during calc phase. Plot phase aborted"
        return 0
    fi

    rm -fr plots; mkdir plots; cd plots
    
    if [[ ${cfg[keep_raw_data]} == yes ]];then
        matlab -nosplash -nodesktop -nodisplay < \
            $CODE_DIR/plotting/plot_dataset.m #> /dev/null
        # matlab < "$CODE_DIR/plotting/plot_dataset.m"
        # matlab -r $CODE_DIR/plotting/plot_dataset.m
        #cp $CODE_DIR/plotting/plot_dataset.m .
        #matlab -r plot_dataset.m
    fi

    echo ' Statistics'
    cp $CODE_DIR/plotting/line_plot.ncl .
    plot_run

    #[[ $detailed_stats == "yes" ]] && plot_detailed_run_stats

    funct_closing 2
}

docu_function(){
    funct_opening 2

    if [[ -e "$dataset_dir/config/run_crashed.cfg" ]] ; then
        echo "Run crashed during calc phase. Docu phase aborted"
        return 0
    fi

    [[ -f logs/plot.success ]] || \
        run.sh --task=plot "$dataset_dir"

    [[ -n ${cfg[report_name]:-} ]] && texfile="${cfg[report_name]}.tex"

    open_report

    plots_dir="./plots"

    nature_plots=($plots_dir/nature_*.pdf)
    for nature_plot in ${nature_plots[@]}; do
        plot_frame --width="0.6\textwidth" --title=" " --subtitle_on=no \
            "$nature_plot"
    done

    plot_title(){ echo "$(Tkind_l $plot_file)"; }
    column_files=($plots_dir/{Insta,Taver,Tanom}_Fstdd_stats.pdf)
    plot_column "Field Statistics"

    column_height=0.66
    plot_title(){ echo "$(Ephase_l $plot_file)"; }
    column_files=($plots_dir/{postr,prior}_Fstdd_stats.pdf)
    plot_column "Field Statistics"

    newpage
    echo "\begin{landscape}" >> $texfile
    echo "\begin{multicols}{2}" >> $texfile
#\columnbreak
    if [[ -n ${cfg[report_name]:-} ]];then
        section_title "\bf EXPERIMENT"
        section_title "\bf\url{${cfg[report_name]}}"
    fi

    if [[ ${cfg[run_mode]} == assi ]]; then
        cfg_dir="./config"
        report_config "D.A. Run Configuration"
        cfg_dir="./${cfg[rel_run_free_dir]}/config"
        report_config "Free Run Configuration"
    fi

    if [[ ${cfg[run_mode]} == free ]]; then
        cfg_dir="./config"
        report_config "Free Run Configuration"
    fi

    if [[ -f "logs/default_config.sh" ]];then
        newpage
        include_file "logs/default_config.sh"
        # include_file "logs/calc.log"
    fi

    newpage
    report_stats

    echo "\end{multicols}" >> $texfile
    echo "\end{landscape}" >> $texfile

    close_report

    my_pdflatex "$texfile"

    funct_closing 2
}

clean_function(){
    funct_opening 2

    find .  -path "*/code*" -delete
    find .. -path "*/bin*"  -delete
    find .  -path "*nature_Insta_all.*" -delete # only needed for calc phase and VERY heavy

    funct_closing 2
}

view_function(){
    funct_opening 2

    [[ -f logs/docu.success ]] || ( docu_function )
    report_file=$(find . -maxdepth 1 -name "*.pdf")
    [[ -n ${report_file:-} ]] && okular $report_file

    funct_closing 2
}


run $@
exit $?
