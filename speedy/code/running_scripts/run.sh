#!/usr/bin/env bash
set -eu # strict config (non-zero returns and unset vars halt script)
source $CODE_DIR/running_scripts/common_tools.sh
source $CODE_DIR/running_scripts/run__tools_Level2.sh
source $CODE_DIR/speedy_tools/model_tools.sh
# source tools/report_tools.sh

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
#> spinup   = gridded spinned up state
#> free_run = ensemble run without D.A. 
#> assi_run = ensemble run with D.A. 
#> Needed environment variable
#> - verity Calculation significance level
#>   - 0: Very fast config for testing and comp. time estimation
#>   - 1: Real config to get statistically significant results.
#================================================================
run(){
    long_opts="help,task:,run_type:,time_start:,spinup_dir:,sampling_dir:,nature_dir:,free_run_dir:,assi_run_normT_obs_dir:,assi_run_normAdd_obs_dir:,assi_run_normMin_obs_dir:,assi_run_normProd_obs_dir:,assi_run_normThalf_obs_dir:,cycle_length:,run_length:,time_unit:,cycles:,ref_dir:,ensemble_size:,calc:,stat:,plot:,task:,detailed_stats::,stop_at_nan:,keep_raw_data::,keep_members::,verbose:,report_name:"
    PARSED_ARGS=$(getopt -n "$0" -o h --long "$long_opts" -- "$@")
    eval set -- "$PARSED_ARGS"

    while true; do
        case "$1" in
            -h|--help          )                       usage  ;;
            --run_type         )          run_type=$2; shift 2;;
            --ref_dir          )     reference_dir=$2; shift 2;;
            --spinup_dir       )        spinup_dir=$2; shift 2;;
            --sampling_dir     )      sampling_dir=$2; shift 2;;
            --nature_dir       )        nature_dir=$2; shift 2;;
            --free_run_dir     )      free_run_dir=$2; shift 2;;
	    --assi_run_normT_obs_dir   ) assi_run_normT_obs_dir=$2; shift 2;;
	    --assi_run_normAdd_obs_dir ) assi_run_normAdd_obs_dir=$2; shift 2;;
	    --assi_run_normMin_obs_dir ) assi_run_normMin_obs_dir=$2; shift 2;;
	    --assi_run_normThalf_obs_dir ) assi_run_normThalf_obs_dir=$2; shift 2;;
	    --assi_run_normProd_obs_dir ) assi_run_normProd_obs_dir=$2; shift 2;;
            --time_start       )        time_start=$2; shift 2;;
            --run_length       )        run_length=$2; shift 2;;
            --run_length_unit  )   run_length_unit=$2; shift 2;;
            --cycle_length     )      cycle_length=$2; shift 2;;
            --cycle_length_unit) cycle_length_unit=$2; shift 2;;
            --cycles           )            cycles=$2; shift 2;;
            # --ensemble_size )   ensemble_size=$2; shift 2;;
            --calc             )         calc_flag=$2; shift 2;;
            --stat             )         stat_flag=$2; shift 2;;
            --plot             )         plot_flag=$2; shift 2;;
            # --replot           )            replot=$2; shift 2;;
            --task             )         only_task=$2; shift 2;;
            --detailed_stats   )    detailed_stats=$2; shift 2;;
            --stop_at_nan      )       stop_at_nan=$2; shift 2;;
            --keep_raw_data    )     keep_raw_data=$2; shift 2;;
            --keep_members     )      keep_members=$2; shift 2;;
            --verbose          )           verbose=$2; shift 2;;
            --report_name      )       report_name=$2; shift 2;;
            --                 )              shift 1; break  ;;
        esac
    done
    dataset_dir=$1

    dataset_dir="$(absolute_path "$dataset_dir")"
    detailed_stats=${detailed_stats:-no}
    keep_raw_data=${keep_raw_data:-no}
    dataset_kind=$(barename $0)
    dataset_dim=0
    verbose=${verbose:-2}
   
    exec 3>&1 

# case "$verbose" in
#         2) exec 3>&1         ;;
#         1) exec 3>/dev/null  ;;
# 	0) exec 3>/dev/null  ;;
#     esac

    declare -r Tkinds=(Insta Taver Tanom)
    declare -r speedy_vars=("u" "v" "t" "q")

    #[[ -n ${replot:-} ]] && \
        rm -f "$dataset_dir/logs/plot.success" "$dataset_dir/logs/docu.success"

    perform_simulation_tasks

    exec 3>&-
}

config_function(){
    funct_opening 2

    add_to_cfg "dataset_kind" "dataset_dim" "run_type" "model"

    config_${cfg[run_type]}_run 

    [[ -n ${report_name:-} ]] && add_to_cfg "report_name"
    store_cfg;  print_cfg

    scp "$MODEL_DIR/model_config.sh" ./logs

    funct_closing 2
}

calc_function(){
    funct_opening 2

    mcd raw_data

    calc_${cfg[run_type]}_run 

    funct_closing 2
}

stat_function(){
    funct_opening 2

    if [[ -e "$dataset_dir/config/run_crashed.cfg" ]] ; then
        echo "Run crashed during calc phase. Stat phase aborted"
        return 0
    fi

    if [[ ! -d "$dataset_dir/raw_data" ]] ; then
        echo "Raw output has been deleted. Stat phase aborted"
        return 0
    fi
    
    [[ -d "$dataset_dir/stats" ]] && find $dataset_dir/stats -delete
    [[ -d "$dataset_dir/plots" ]] && mv "$dataset_dir/plots" "$dataset_dir/plots_outdated"
    [[ -d "$dataset_dir/plots" ]] && rm "$dataset_dir/logs/plot.success"
    
    #rm -fr stats
    mcd stats
    
    stat_${cfg[run_type]}_run 

    # rm -fr stats; mcd stats
    # stat_run

    # if [[ $detailed_stats == "yes" ]]; then
    #     cd "$dataset_dir"
    #     rm -fr stats_detailed stat_cycles-*
    #     run_detailed_stats
    # fi

    # if [[ ${cfg[keep_raw_data]} == "no" ]] && [[ ${cfg[run_type]} == "assi" ]]; then
    #     rm -rf "$dataset_dir/raw_data"
    #     find -L "$dataset_dir/stats" -type l -delete # remove broken links
    # fi

    funct_closing 2
}

plot_function(){
    funct_opening 2

    if [[ -f "$dataset_dir/config/run_crashed.cfg" ]] ; then
        echo "Run crashed during calc phase. Plot phase aborted"
        return 0
    fi

    if [[ ! -f "$dataset_dir/logs/stat.success" ]] ; then
        echo "Stat phase was not succesfull. Plot phase aborted"
        return 0
    fi


    # rm -fr plots; mcd plots
    mkdir -p plots; cd plots

    # postStat_${cfg[run_type]}_run 
    
    sleep 10
    echo " Plotting with matlab"
    matlab -nosplash -nodesktop -nodisplay < "$CODE_DIR/plotting/plot_dataset.m"
#    matlab -r $CODE_DIR/plotting/plot_dataset.m #> /dev/null
    
    # plot_run
    # [[ $detailed_stats == "yes" ]] && plot_detailed_run_stats

    touch "${dataset_dir}/logs/plot.success"

    funct_closing 2
}



docu_function(){
    funct_opening 1

    [[ -f logs/plot.success ]] || ( plot_function )
    [[ -n ${cfg[report_name]:-} ]] && texfile="${cfg[report_name]}.tex"

    #quality=draft
    orientation=${orientation:-"portrait"}
#     orientation=${orientation:-"landscape"}
    margins="[left=15mm,right=15mm,top=5mm,bottom=20mm]"

    open_report
    report_title "$(label ${cfg[run_type]} run) Run"

    cfg_dir="./config"
    report_config "Configuration"

   
#     report_metainfo

    array_width="0.8\textwidth"

    docu_${cfg[run_type]}_run 

#     case ${cfg[run_type]} in
#          "nature") 
# #        docu_run_state 'nature' 'timstd1'
#          ;;
#          "free") 
#             array_width="0.8\textwidth"
#             plot_array_vertical_speedyvars "free_prior_grid_sigma_error-spread" "Global forecast Error-spread"

#             for Tkind in Taver Insta Tanom; do
#                 docu_speedy_scalar_vertical_zonal_mean 'free_prior_error' $Tkind 'timstd1'
#                 docu_speedy_scalar_vertical_zonal_mean 'free_prior_Esprd' $Tkind 'timmean'
#                 #docu_speedy_scalar_vertical_zonal_mean 'free_prior_Emean' $Tkind 'timstd1'
#             done
#          ;;
#          "assi")
#              array_width="0.8\textwidth"
#              plot_array_vertical_speedyvars "assi_postr_grid_sigma_error-spread" "Global DA analysis Error-spread"
#              plot_array_vertical_speedyvars "assi_prior_grid_sigma_error-spread" "Global DA forecast Error-spread"
#              plots_dir="$dataset_dir/${cfg[rel_free_run_dir]}/plots"
#              plot_array_vertical_speedyvars "free_prior_grid_sigma_error-spread" "Global Free forecast Error-spread"

#              for Tkind in Taver Insta Tanom; do
#                 plots_dir="$dataset_dir/plots"
#                  docu_speedy_scalar_vertical_zonal_mean 'assi_postr_error' $Tkind 'reduc'
#                  docu_speedy_scalar_vertical_zonal_mean 'assi_prior_error' $Tkind 'reduc'

#                 docu_speedy_scalar_vertical_zonal_mean 'assi_postr_error' $Tkind 'timstd1'
#                 docu_speedy_scalar_vertical_zonal_mean 'assi_prior_error' $Tkind 'timstd1'
#                 plots_dir="$dataset_dir/${cfg[rel_free_run_dir]}/plots"
#                 docu_speedy_scalar_vertical_zonal_mean 'free_prior_error' $Tkind 'timstd1'

#                 plots_dir="$dataset_dir/plots"
#                 docu_speedy_scalar_vertical_zonal_mean 'assi_postr_Esprd' $Tkind 'timmean'
#                 docu_speedy_scalar_vertical_zonal_mean 'assi_prior_Esprd' $Tkind 'timmean'
#                 plots_dir="$dataset_dir/${cfg[rel_free_run_dir]}/plots"
#                 docu_speedy_scalar_vertical_zonal_mean 'free_prior_Esprd' $Tkind 'timmean'
#                 #docu_speedy_scalar_vertical_zonal_mean 'assi_postr_Emean' $Tkind 'timstd1'
#                 #docu_speedy_scalar_vertical_zonal_mean 'assi_prior_Emean' $Tkind 'timstd1'
#              done
#          ;;
#     esac

    include_file "logs/model_config.sh"

    close_report
    my_pdflatex "$texfile" 
    #pdflatex "$texfile" #editted by Bijan 
    funct_closing 1
}

view_function(){
    funct_opening 2

    [[ -f logs/docu.success ]] || ( docu_function )
    report_file=$(find . -maxdepth 1 -name "*.pdf")
    [[ -n ${report_file:-} ]] && okular $report_file
    
    funct_closing 2
}




# docu_function(){
#     funct_opening 2

#     if [[ -e "$dataset_dir/config/run_crashed.cfg" ]] ; then
#         echo "Run crashed during calc phase. Docu phase aborted"
#         return 0
#     fi

#     [[ -n ${cfg[report_name]:-} ]] && texfile="${cfg[report_name]}.tex"

#     open_report

#     plots_dir="./plots"

#     plot_title(){ echo "$(Tkind_l $plot_file)"; }
#     column_files=($plots_dir/{Insta,Taver,Tanom}_Fstdd_stats.pdf)
#     plot_column "Field Statistics"

#     column_height=0.66
#     plot_title(){ echo "$(Ephase_l $plot_file)"; }
#     column_files=($plots_dir/{postr,prior}_Fstdd_stats.pdf)
#     plot_column "Field Statistics"

#     newpage
#     echo "\begin{landscape}" >> $texfile
#     echo "\begin{multicols}{2}" >> $texfile
# #\columnbreak
#     if [[ -n ${cfg[report_name]:-} ]];then
#         section_title "\bf EXPERIMENT"
#         section_title "\bf\url{${cfg[report_name]}}"
#     fi

#     cfg_dir="./config"
#     report_config "D.A. Run Configuration"

#     cfg_dir="./${cfg[rel_run_free_dir]}/config"
#     report_config "Free Run Configuration"

#     if [[ -f "logs/default_config.sh" ]];then
#         newpage
#         include_file "logs/default_config.sh"
#         # include_file "logs/calc.log"
#     fi

#     newpage
#     report_stats

#     echo "\end{multicols}" >> $texfile
#     echo "\end{landscape}" >> $texfile

#     close_report

#     my_pdflatex "$texfile"

#     funct_closing 2
# }



run $@
exit $?
