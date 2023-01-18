#!/usr/bin/env bash
source $CODE_DIR/running_scripts/run__tools_Level1.sh

#-------------------------------------------------
#> Spinup routines
#-------------------------------------------------
config_spinup_run(){
    funct_opening 3

    spinup_final=$(time_increment.exe $spinup_start $spinup_length $spinup_length_unit)
    add_to_cfg "spinup_start" "spinup_final" "spinup_length_unit"
    add_to_cfg "spinup_length" "spinup_length_unit"

    funct_closing 3
}

calc_spinup_run(){
    funct_opening 3

    speedy_link_forcings "t30" .
    speedy_run 0 2 ${cfg[spinup_start]} ${cfg[spinup_final]} "$(pwd)"
    rm fort.* #time_forecast.dat # Cleaning up

    funct_closing 3
}

stat_spinup_run(){
    funct_opening 3

    Tkind=Insta
    create_control_file "spinup" $Tkind ${cfg[spinup_final]} \
        ${cfg[spinup_length]} ${cfg[spinup_length_unit]} "../raw_data" >&3
    CDO "../raw_data" "../raw_data" "spinup_grid_sigma_${Tkind}.ctl" import_binary # BIJAN
    #CDO_LAND "../raw_data" "../raw_data" "spinup_grid_sigma_${Tkind}.ctl" import_binary
    # for vert_coord in press sigma; do
    for vert_coord in sigma; do
	reduceDimensionality "../raw_data" . "spinup_grid_${vert_coord}_${Tkind}.nc" >&3
    done

    funct_closing 3
}

docu_spinup_run(){
    funct_opening 3
    echo " Nothing yet"
    funct_closing 3
}

#-------------------------------------------------
# sampling run routines
#-------------------------------------------------
config_sampling_run(){
    funct_opening 3

    cycle_length=$sampling_step
    cycle_length_unit=$sampling_step_unit
    cycles=$sampling_size
    config_nature_run

    funct_closing 3
}

calc_sampling_run(){
    funct_opening 3

    calc_nature_run

    funct_closing 3
}

stat_sampling_run(){
    funct_opening 3

    for Tkind in ${Tkinds[@]}; do
        create_control_file "nature" $Tkind ${cfg[time_one]} \
            ${cfg[cycle_length]} ${cfg[cycle_length_unit]} "../raw_data/nature" >&3

        CDO "../raw_data/nature" "../raw_data" "nature_grid_sigma_${Tkind}.ctl" import_binary
	CDO "../raw_data" . nature_grid_sigma_${Tkind} timstd1
	reduceDimensionality . . nature_grid_sigma_${Tkind}_timstd1 >&3
	
	
    done

    funct_closing 3
}

docu_sampling_run(){
    funct_opening 3
    echo " Nothing yet"
    funct_closing 3
}

#-------------------------------------------------
# Nature run routines
#-------------------------------------------------
config_nature_run(){
    funct_opening 3

    # - convert months to days
    if [[ $cycle_length_unit == 'mo' ]];then
        if [[ $cycle_length == "00.033" ]]; then
            cycle_length_unit="dy"; cycle_length=1
        fi
        if [[ $cycle_length == "00.250" ]]; then
            cycle_length_unit="dy"; cycle_length=7
        fi
        if [[ $cycle_length == "00.500" ]]; then
            cycle_length_unit="dy"; cycle_length=15
        fi
    fi

    # add_to_cfg "verity" #"detailed_stats" #"stop_at_nan"
    add_to_cfg "keep_raw_data"
    rel_spinup_dir=$(relpath $spinup_dir $dataset_dir )
    add_to_cfg "rel_spinup_dir" #"spinup_dir"

    # Setting Time interval" # (Starts from the end of spinup)"
    read time_start < "$spinup_dir/config/spinup_final.cfg"

    if [[ -n ${cycles:-} ]]; then
        run_length=$(echo "$cycle_length * $cycles"|bc)
        run_length_unit=$cycle_length_unit
        add_to_cfg "cycles"
    fi

    time_final=$(time_increment.exe $time_start   $run_length   $run_length_unit)
    time_one=$(  time_increment.exe $time_start $cycle_length $cycle_length_unit)
    add_to_cfg "cycle_length" "cycle_length_unit"
    add_to_cfg   "run_length"   "run_length_unit"
    add_to_cfg "time_start"  "time_final" "time_one"

    funct_closing 3
}

calc_nature_run(){
    funct_opening 3

    create_speedy_folder "nature" >&3
    cd nature

    # link initial state
    ln -s "$spinup_dir/raw_data/${time_start}_grid_sigma_Insta.rst" .

    # Run generation
    time_loop_core(){
        speedy_run 2 4 $time_present $time_forecast "$(pwd)" >&3
#         speedy_run 2 4 $time_present $time_forecast "$(pwd)" > speedy_run.log
    }
    time_loop

    clean_speedy_folder .
#
#     # Clean up
#     rm time_forecast.dat fort.*
#     # Unlinking initial state
#     rm ${cfg[time_start]}*.rst

    funct_closing 3
}

stat_nature_run(){
    funct_opening 3

    ( gather_monthly_means >&3 )

    for Tkind in ${Tkinds[@]}; do
        create_control_file "nature" $Tkind ${cfg[time_one]} \
            ${cfg[cycle_length]} ${cfg[cycle_length_unit]} "../raw_data/nature" >&3

        CDO "../raw_data/nature" "../raw_data" "nature_grid_sigma_${Tkind}.ctl" import_binary

	# mean
	CDO "../raw_data" . "nature_grid_sigma_${Tkind}.nc"      timstd1
	reduceDimensionality .    . "nature_grid_sigma_${Tkind}_timstd1" >&3

	# standard deviation
	CDO "../raw_data" . "nature_grid_sigma_${Tkind}.nc"      timmean
	reduceDimensionality .     . "nature_grid_sigma_${Tkind}_timmean" >&3
	
    done

    funct_closing 3
}

plot_nature_run(){
    funct_opening 3
    echo "No plots so far"
    funct_closing 3
}

docu_nature_run(){
    funct_opening 3

    for Tkind in Taver Insta; do
        docu_speedy_scalar_vertical_zonal_mean 'nature' $Tkind 'timstd1'
        docu_speedy_scalar_vertical_zonal_mean 'nature' $Tkind 'timmean'
    done

    funct_closing 3
}

#-------------------------------------------------
# free run routines
#-------------------------------------------------
config_free_run(){
    funct_opening 3

    [[ -d   $nature_dir ]] || error "No nature dataset $nature_dir"
    [[ -d $sampling_dir ]] || error "No sampling dataset $sampling_dir"
    # [[ -f $nature_dir/dataset_complete.dat ]] || error "Nature Run dataset is not complete"
    
    keep_members=${keep_members:-no}
    rel_nature_dir=$(  relpath   $nature_dir $dataset_dir)
    rel_sampling_dir=$(relpath $sampling_dir $dataset_dir)
    add_to_cfg "rel_nature_dir" "rel_sampling_dir"
    add_to_cfg "ensemble_size" "keep_members"
    get_config_file "$nature_dir/config/cycle_length.cfg"
    get_config_file "$nature_dir/config/cycle_length_unit.cfg"
    get_config_file "$nature_dir/config/run_length.cfg"
    get_config_file "$nature_dir/config/run_length_unit.cfg"
    get_config_file "$nature_dir/config/time_start.cfg"
    get_config_file "$nature_dir/config/time_final.cfg"
    get_config_file "$nature_dir/config/time_one.cfg"
#     get_config_file "$nature_dir/config/time_unit.cfg"
#     get_config_file "$nature_dir/config/cycles.cfg"
#     add_to_cfg "verity" "detailed_stats" "stop_at_nan" "keep_raw_data"

    funct_closing 3
}

calc_free_run(){
    funct_opening 3

    create_member_ids >&3
    create_ensemble_folders "prior" >&3
    set_initial_ensemble    "prior" >&3

    # Free Prediction cycle
    time_loop_core(){
#         echo " time_present = $time_present"

        propagate_ensemble 2 4 "prior" "prior" > $dataset_dir/logs/propagate_ensemble.log

        for Tkind in ${Tkinds[@]}; do
            calculate_ensemble_stats "prior" $Tkind $time_forecast >&3
        done

        if [[ ${cfg[keep_members]} == no ]];then
            remove_present_time_members "prior" >&3
        fi
    }
    time_loop
    [[ ${cfg[keep_members]} == no ]] && remove_member_folders "prior"

    funct_closing 3
}

stat_free_run(){
    funct_opening 3

    create_member_ids >&3

    export_ensemble_run_output_as_netcdf "free" "prior"

    if [[ ${cfg[keep_members]} == yes ]];then
        postprocess_members "prior" >&3
    fi

    ( ensemble_spatial_stats "prior" ) >&3
    ( ensemble_time_stats    "prior" ) >&3

    funct_closing 3
}

plot_free_run(){
    funct_opening 3

    state_stats_vs_time "../raw_data" "${cfg[run_type]}_prior_error" >&3

    funct_closing 3
}

docu_free_run(){
    funct_opening 3

    plot_array_vertical_speedyvars \
	"free_prior_grid_sigma_error-spread" "Global forecast Error-spread"

    # for Tkind in Taver Insta Tanom; do
    for Tkind in Taver Insta; do
        docu_speedy_scalar_vertical_zonal_mean 'free_prior_Emean' $Tkind 'timmean'
        docu_speedy_scalar_vertical_zonal_mean 'free_prior_error' $Tkind 'timstd1'
        docu_speedy_scalar_vertical_zonal_mean 'free_prior_Esprd' $Tkind 'timmean'
        docu_speedy_scalar_vertical_zonal_mean 'free_prior_Emean' $Tkind 'timstd1'
    done

    funct_closing 3
}

#-------------------------------------------------
# assi_run routines
#-------------------------------------------------
config_assi_run(){
    funct_opening 3

    [[ -d "$free_run_dir" ]] || error "No free run dataset $free_run_dir"

    rel_free_run_dir=$(relpath $free_run_dir $dataset_dir)
    add_to_cfg "rel_free_run_dir"

    get_config_file "$free_run_dir/config/rel_nature_dir.cfg"
    get_config_file "$free_run_dir/config/rel_sampling_dir.cfg"
    nature_dir="$free_run_dir/${cfg[rel_nature_dir]}"
    sampling_dir="$free_run_dir/${cfg[rel_sampling_dir]}"

    config_free_run

    add_to_cfg "obs_operator"

    if [[ -n ${assi_run_normT_obs_dir:-} ]]; then
	rel_assi_run_normT_obs_dir=$(relpath $assi_run_normT_obs_dir $dataset_dir)
	add_to_cfg "rel_assi_run_normT_obs_dir"
    fi
    if [[ -n ${assi_run_normAdd_obs_dir:-} ]]; then
	rel_assi_run_normAdd_obs_dir=$(relpath $assi_run_normAdd_obs_dir $dataset_dir)
	add_to_cfg "rel_assi_run_normAdd_obs_dir"
    fi
    if [[ -n ${assi_run_normMin_obs_dir:-} ]]; then
	rel_assi_run_normMin_obs_dir=$(relpath $assi_run_normMin_obs_dir $dataset_dir)
	add_to_cfg "rel_assi_run_normMin_obs_dir"
    fi
    if [[ -n ${assi_run_normThalf_obs_dir:-} ]]; then
	rel_assi_run_normThalf_obs_dir=$(relpath $assi_run_normThalf_obs_dir $dataset_dir)
	add_to_cfg "rel_assi_run_normMin_obs_dir"
    fi
    if [[ -n ${assi_run_normProd_obs_dir:-} ]]; then
	rel_assi_run_normThalf_obs_dir=$(relpath $assi_run_normProd_obs_dir $dataset_dir)
	add_to_cfg "rel_assi_run_normProd_obs_dir"
    fi
    funct_closing 3
}

calc_assi_run(){
    funct_opening 3

    if [[ ${cfg[cycle_length_unit]} == "mo" ]];then
        export nmonths=${cfg[cycle_length]}
    else
        export nmonths=1
    fi

    ( observe_nature ) >&3

    create_member_ids >&3
    create_ensemble_folders "prior" >&3
    create_ensemble_folders "postr" >&3

    # in assi mode speedy starts from posterior
    set_initial_ensemble "postr" >&3
    link_surface_geopotential_height .
    link_nature_stats_to_ensemble

    time_loop_core(){

        propagate_ensemble 2 4 "postr" "prior" >&3

        calculate_ensemble_obs >&3

        update_Taver_ensemble  >&3

        time_averaged_recomposition >&3

        for Ephase in prior postr; do
            for Tkind in Insta Tanom; do
                calculate_ensemble_stats $Ephase $Tkind $time_forecast >&3
            done
        done

        # - Inflation
        if [ -f infl_mul.grd ]; then
            mv infl_mul.grd infl/${time_present}.grd
            ln -fs infl/${time_present}.grd .
#            cp infl_mul.grd infl/${time_present}.grd
        fi
        #- Logs
        #mv NOUT-000 log/${time_present}_rank-000.log

        if [[ ${cfg[keep_members]} == no ]];then
            remove_present_time_members "prior" >&3
            remove_present_time_members "postr" >&3
        fi

    }
    time_loop

    if [[ ${cfg[keep_members]} == no ]];then
        remove_member_folders "prior" >&3
        remove_member_folders "postr" >&3
    fi

    cd $dataset_dir/raw_data/prior; rm fort.21 gs?????.grd
    cd $dataset_dir/raw_data/postr; rm fort.21

    funct_closing 3
}

stat_assi_run(){
    funct_opening 3

    ( create_member_ids ) >&3

    ( export_ensemble_run_output_as_netcdf "assi" "prior" "postr" ) >&3

    if [[ ${cfg[keep_members]} == yes ]]; then
        ( postprocess_members "prior" ) >&3
        ( postprocess_members "postr" ) >&3
    fi

    ( ensemble_spatial_stats "prior" ) >&3
    ( ensemble_spatial_stats "postr" ) >&3
    ( ensemble_time_stats    "prior" ) >&3
    ( ensemble_time_stats    "postr" ) >&3
    ( calculate_error_reduction      ) >&3 
    
    # document the commands     reference from free ----    assi--  
   ( error_comparison_regarding "free" "prior" "../${cfg[rel_free_run_dir]}" "prior") >&3
   ( error_comparison_regarding "free" "prior" "../${cfg[rel_free_run_dir]}" "postr") >&3
    
    #if  [[ ! ${cfg[obs_operator]} == "norm_T" ]] ||	\
    #[[ ! ${cfg[obs_operator]} == "norm_T_half" ]]    ;then
    #   ( error_comparison_regarding "NormT" "prior" \
	#    "../${cfg[rel_assi_run_normT_obs_dir]}" "prior" ) >&3
    #   ( error_comparison_regarding "NormT" "postr" \
	#   "../${cfg[rel_assi_run_normT_obs_dir]}" "postr" ) >&3
    #fi
	#if [[ ! ${cfg[obs_operator]} == "norm_add" ]];then
    #    ( error_increase_regarding_linear_obs ) >&3
    #fi
    # 
	#if [[ ! ${cfg[obs_operator]} == "norm_add" ]] &&
    #    [[ ! ${cfg[obs_operator]} == "norm_min" ]] ;then
	#( error_decrease_regarding_norm_min ) >&3
    #fi

   # if  [[ ! ${cfg[obs_operator]} == "norm_T"   ]] && \
#	[[ ! ${cfg[obs_operator]} == "norm_add" ]] ;then
 #       ( error_comparison_regarding "NormAdd" "prior" \
	#    "../${cfg[rel_assi_run_normAdd_obs_dir]}"  "prior" ) >&3
     #   ( error_comparison_regarding "NormAdd" "postr" \
	  #  "../${cfg[rel_assi_run_normAdd_obs_dir]}"  "postr" ) >&3
    #fi
    #if  [[ ! ${cfg[obs_operator]} == "norm_T_half"   ]] && \
	#[[ ! ${cfg[obs_operator]} == "norm_add" ]] ;then
     #   ( error_comparison_regarding "NormAdd" "prior" \
	 #   "../${cfg[rel_assi_run_normAdd_obs_dir]}"  "prior" ) >&3
      #  ( error_comparison_regarding "NormAdd" "postr" \
	  #  "../${cfg[rel_assi_run_normAdd_obs_dir]}"  "postr" ) >&3
    #fi
    

    funct_closing 3
}

# postStat_assi_run(){
#     funct_opening 3

#     if [[ ! ${cfg[obs_operator]} == "norm_add" ]];then
#         ( error_increase_regarding_linear_obs ) >&3
#     fi
#     if [[ ! ${cfg[obs_operator]} == "norm_add" ]] &&
# 	[[ ! ${cfg[obs_operator]} == "norm_min" ]] ;then
#         ( error_decrease_regarding_norm_min ) >&3
#     fi

#     funct_closing 3
# }

docu_assi_run(){
    funct_opening 3

    #plot_array_vertical_speedyvars \
	#"assi_postr_grid_sigma_error-spread" "Global DA analysis Error-spread"
    #plot_array_vertical_speedyvars \
	#"assi_prior_grid_sigma_error-spread" "Global DA forecast Error-spread"
	
	# NEW LINES IN DOCU FROM BIJAN
    plot_array_vertical_speedyvars	\
    "assi_prior_Error_Insta"	"assi prior Error Insta"   #    "Global DA analysis Error-spread BIJAN"
    plot_array_vertical_speedyvars \
	"assi_postr_Error_Insta" 	"assi postr Error Insta"
	plot_array_vertical_speedyvars \
	"assi_prior_Spread_Insta"             "assi prior Spread Insta"
	plot_array_vertical_speedyvars \
	"assi_postr_Spread_Insta"            "assi postr Spread Insta"
    plot_array_vertical_speedyvars \
	"assi_prior_Error_Taver" 			 "assi prior Error Taver"
		plot_array_vertical_speedyvars \
	"assi_postr_Error_Taver"            "assi postr Error Taver"
    plot_array_vertical_speedyvars \
	"assi_prior_Spread_Taver"             "assi prior Spread Taver"
	plot_array_vertical_speedyvars \
	"assi_postr_Spread_Taver"            "assi postr Spread Taver"
    
	#if  [[ ${cfg[obs_operator]} == "norm_prod" ]]; then
	
	#plot_array_vertical_speedyvars	\
   # "assi_prior_Error_Insta-Prod-vs-Min"	"assi prior Error Insta Prod-vs-Min"   #    "Global DA analysis Error-spread BIJAN"
    #plot_array_vertical_speedyvars \
	#"assi_postr_Error_Insta-Prod-vs-Min" 	"assi postr Error Insta Prod-vs-Min"
	#plot_array_vertical_speedyvars \
	#"assi_prior_Spread_Insta-Prod-vs-Min"             "assi prior Spread Insta Prod-vs-Min"
	#plot_array_vertical_speedyvars \
	#"assi_postr_Spread_Insta-Prod-vs-Min"            "assi postr Spread Insta Prod-vs-Min"
   # plot_array_vertical_speedyvars \
	#"assi_prior_Error_Taver-Prod-vs-Min" 			 "assi prior Error Taver Prod-vs-Min"
	#	plot_array_vertical_speedyvars \
	#"assi_postr_Error_Taver-Prod-vs-Min"            "assi postr Error Taver Prod-vs-Min"
    #plot_array_vertical_speedyvars \
	#"assi_prior_Spread_Taver-Prod-vs-Min"             "assi prior Spread Taver Prod-vs-Min"
	#plot_array_vertical_speedyvars \
	#"assi_postr_Spread_Taver-Prod-vs-Min"            "assi postr Spread Taver Prod-vs-Min"
	
	#fi
	
	
    # plots_dir="$dataset_dir/${cfg[rel_free_run_dir]}/plots"
    # plot_array_vertical_speedyvars \
    # 	"free_prior_grid_sigma_error-spread" "Global Free forecast Error-spread"

#    for Tkind in Taver Insta Tanom; do
    for Tkind in Taver Insta; do

	plots_dir="$dataset_dir/plots"
        docu_speedy_scalar_vertical_zonal_mean 'assi_postr_error' \
	    $Tkind 'timstd1_DecreaseReffree_prior'
        docu_speedy_scalar_vertical_zonal_mean 'assi_prior_error' \
	    $Tkind 'timstd1_DecreaseReffree_prior'

	#if [[ ! ${cfg[obs_operator]} == "norm_T" ]];then
	    #docu_speedy_scalar_vertical_zonal_mean 'assi_postr_error' \
		#$Tkind 'timstd1_IncreaseRefNormT_postr'
	    #docu_speedy_scalar_vertical_zonal_mean 'assi_prior_error' \
		#$Tkind 'timstd1_IncreaseRefNormT_prior'
	#fi

	#if  [[ ! ${cfg[obs_operator]} == "norm_T"   ]] &&
	    #[[ ! ${cfg[obs_operator]} == "norm_add" ]]; then
	    #docu_speedy_scalar_vertical_zonal_mean 'assi_postr_error' \
		#$Tkind 'timstd1_IncreaseRefNormAdd_postr'
	    #docu_speedy_scalar_vertical_zonal_mean 'assi_prior_error' \
		#$Tkind 'timstd1_IncreaseRefNormAdd_prior'
	#fi
	# if [[ ! ${cfg[obs_operator]} == "norm_T" ]] &&
	#     [[ ! ${cfg[obs_operator]} == "norm_min" ]] ;then
	#       docu_speedy_scalar_vertical_zonal_mean \
	# 	  'assi_postr_error' $Tkind 'timstd1_reducRefMin'
	#       docu_speedy_scalar_vertical_zonal_mean \
	# 	  'assi_prior_error' $Tkind 'timstd1_reducRefMin'
	# fi

	for Ephase in prior postr; do
            docu_speedy_scalar_vertical_zonal_mean "assi_${Ephase}_Emean" $Tkind 'timmean'
            docu_speedy_scalar_vertical_zonal_mean "assi_${Ephase}_error" $Tkind 'timstd1'
            docu_speedy_scalar_vertical_zonal_mean "assi_${Ephase}_Esprd" $Tkind 'timmean'
            docu_speedy_scalar_vertical_zonal_mean "assi_${Ephase}_Emean" $Tkind 'timstd1'
	done
	
#         docu_speedy_scalar_vertical_zonal_mean 'assi_postr_error' $Tkind 'timstd1'
#         docu_speedy_scalar_vertical_zonal_mean 'assi_prior_error' $Tkind 'timstd1'

# #         plots_dir="$dataset_dir/${cfg[rel_free_run_dir]}/plots"
# #         docu_speedy_scalar_vertical_zonal_mean 'free_prior_error' $Tkind 'timstd1'

#         plots_dir="$dataset_dir/plots"
#         docu_speedy_scalar_vertical_zonal_mean 'assi_postr_Esprd' $Tkind 'timmean'
#         docu_speedy_scalar_vertical_zonal_mean 'assi_prior_Esprd' $Tkind 'timmean'

	# plots_dir="$dataset_dir/${cfg[rel_free_run_dir]}/plots"
        # docu_speedy_scalar_vertical_zonal_mean 'free_prior_Esprd' $Tkind 'timmean'
        #docu_speedy_scalar_vertical_zonal_mean 'assi_postr_Emean' $Tkind 'timstd1'
        #docu_speedy_scalar_vertical_zonal_mean 'assi_prior_Emean' $Tkind 'timstd1'
    done

    funct_closing 3
}
