#!/usr/bin/env bash

#-------------------------------------------------
#> Spinup routines
#-------------------------------------------------
config_spinup_run(){
    funct_opening 2

    time_length=$cycle_length
    time_final=$(time_increment.exe $time_start $cycle_length $cycle_length_unit)
    add_to_cfg "time_start" "cycle_length" "cycle_length_unit" "time_final"

    funct_closing 2
}

calc_spinup_run(){
    funct_opening 2

    speedy_link_forcings "t30" .
    speedy_run 0 2 ${cfg[time_start]} ${cfg[time_final]} "$(pwd)"
    rm fort.* #time_forecast.dat # Cleaning up

    funct_closing 2
}

stat_spinup_run(){
    funct_opening 2

    for Tkind in Insta; do
        create_control_file "spinup" $Tkind ${cfg[time_final]} \
            ${cfg[cycle_length]} ${cfg[cycle_length_unit]} "../raw_data"
        my_cdo -f nc import_binary \
            "../raw_data/spinup_grid_sigma_${Tkind}.ctl" \
            "../raw_data/spinup_grid_sigma_${Tkind}.nc"
    done

    # for vert_coord in press sigma; do
    for vert_coord in sigma; do
        spatial_stats "../raw_data/spinup_grid_${vert_coord}_Insta.nc"
    done

    funct_closing 2
}

#-------------------------------------------------
# Nature run routines
#-------------------------------------------------
config_nature_run(){
    funct_opening 2

    if [[ $cycle_length_unit == 'mo' ]];then
       if [[ $cycle_length == "00.033" ]]; then
          cycle_length_unit="dy"; cycle_length=1
      fi
    fi

    # add_to_cfg "verity" #"detailed_stats" #"stop_at_nan"
    add_to_cfg "keep_raw_data"
    rel_spinup_dir=$(relpath $spinup_dir $dataset_dir )
    add_to_cfg "rel_spinup_dir" #"spinup_dir"

    # Setting Time interval" # (Starts from the end of spinup)"
    read time_start < "$spinup_dir/config/time_final.cfg"
#     run_length=$(echo "$cycle_length * $cycles"|bc)
    time_final=$(time_increment.exe $time_start   $run_length $cycle_length_unit)
    time_one=$(  time_increment.exe $time_start $cycle_length $cycle_length_unit)
    add_to_cfg "run_length" "cycle_length" "time_unit"
    add_to_cfg "time_start" "time_final"   "time_one" 

    funct_closing 2
}

calc_nature_run(){
    funct_opening 2

    create_speedy_folder "nature";   cd nature

    # link initial state
    ln -s "$spinup_dir/raw_data/${time_start}_grid_sigma_Insta.rst" .

    # Run generation
    time_loop_core(){
        echo " time_present = $time_present"
        speedy_run 2 4 $time_present $time_forecast "$(pwd)" # > /dev/null
    }
    time_loop

    clean_speedy_folder .
#
#     # Clean up
#     rm time_forecast.dat fort.*
#     # Unlinking initial state
#     rm ${cfg[time_start]}*.rst

    funct_closing 2
}

stat_nature_run(){
    funct_opening 2

    for Tkind in ${Tkinds[@]}; do
        create_control_file "nature" $Tkind ${cfg[time_one]} \
            ${cfg[cycle_length]} ${cfg[cycle_length_unit]} "../raw_data/nature"
        my_cdo -f nc import_binary \
            "../raw_data/nature/nature_grid_sigma_${Tkind}.ctl" \
            "../raw_data/nature_grid_sigma_${Tkind}.nc"
    done

    stat_state "../raw_data" nature Tstdd

    funct_closing 2
}

#-------------------------------------------------
# free run routines
#-------------------------------------------------
config_free_run(){
    funct_opening 2

    [[ -d   $nature_dir ]] || error "No nature dataset $nature_dir"
    [[ -d $sampling_dir ]] || error "No sampling dataset $sampling_dir"
    # [[ -f $nature_dir/dataset_complete.dat ]] || error "Nature Run dataset is not complete"
    keep_members=${keep_members:-no}
    rel_nature_dir=$(relpath $nature_dir $dataset_dir)
    rel_sampling_dir=$(relpath $sampling_dir $dataset_dir)

    #add_to_cfg     "nature_dir"     "sampling_dir"
    add_to_cfg "rel_nature_dir" "rel_sampling_dir"
    get_config_file "$nature_dir/config/cycle_length.cfg"
    get_config_file "$nature_dir/config/cycle_length_unit.cfg"
    get_config_file "$nature_dir/config/run_length.cfg"
    get_config_file "$nature_dir/config/cycles.cfg"
    get_config_file "$nature_dir/config/time_start.cfg"
    get_config_file "$nature_dir/config/time_final.cfg"
    get_config_file "$nature_dir/config/time_one.cfg"
    add_to_cfg      "ensemble_size" "keep_members"
    # add_to_cfg "verity" #"detailed_stats" #"stop_at_nan"
    # add_to_cfg "keep_raw_data"

    funct_closing 2
}

calc_free_run(){
    funct_opening 2

    create_member_ids
    create_ensemble_folders "prior"
    set_initial_ensemble "prior"

    # Free Prediction cycle
    time_loop_core(){
        echo " time_present = $time_present"

        propagate_ensemble 2 4 "prior" "prior" > /dev/null

        for Tkind in ${Tkinds[@]}; do
            calculate_ensemble_mean_spread "prior" $Tkind $time_forecast > /dev/null
        done

        if [[ ${cfg[keep_members]} == no ]];then
            remove_present_time_members "prior"
        fi
    }
    time_loop
    [[ ${cfg[keep_members]} == no ]] && remove_member_folders "prior"

    funct_closing 2
}

stat_free_run(){
    funct_opening 2

    create_member_ids

    for Estat in Emean Esprd; do
        for Tkind in ${Tkinds[@]}; do
            create_control_file "free_prior_${Estat}" $Tkind ${cfg[time_one]} \
                ${cfg[cycle_length]} ${cfg[cycle_length_unit]} \
                "../raw_data/prior/$Estat"
            my_cdo -f nc import_binary \
                "../raw_data/prior/$Estat/free_prior_${Estat}_grid_sigma_${Tkind}.ctl" \
                "../raw_data/free_prior_${Estat}_grid_sigma_${Tkind}.nc"
        done
    done

    if [[ ${cfg[keep_members]} == yes ]];then
        postprocess_members "prior"
    fi

    calculate_ensemble_error "prior"

    funct_closing 2
}

#-------------------------------------------------
# assi_run routines
#-------------------------------------------------
config_assi_run(){
    funct_opening 2

    [[ -d $free_run_dir ]] || error "No free run dataset $free_run_dir"
    read   rel_nature_dir <   "$free_run_dir/config/rel_nature_dir.cfg"
    read rel_sampling_dir < "$free_run_dir/config/rel_sampling_dir.cfg"
    nature_dir="$free_run_dir/$rel_nature_dir"
    sampling_dir="$free_run_dir/$rel_sampling_dir"

    config_free_run

    funct_closing 2
}

calc_assi_run(){
    funct_opening 2

    ( observe_nature )

    create_member_ids
    create_ensemble_folders "prior"
    create_ensemble_folders "postr"

    set_initial_ensemble "postr" # in assi mode speedy starts from posterior
    link_surface_geopotential_height .

    time_loop_core(){
        echo "time_present = $time_present"

        propagate_ensemble 2 4 "postr" "prior" > /dev/null

        link_dirty_observations $time_forecast .

        update_Taver_ensemble

        time_averaged_recomposition

        for Ephase in prior postr; do
            for Tkind in Insta Tanom; do
                calculate_ensemble_mean_spread $Ephase $Tkind $time_forecast > /dev/null
            done
        done

            ## - Inflation
            #if [ -f infl_mul.grd ]; then
                #cp infl_mul.grd infl_mul/${time_present}.grd
            #fi
            #  - Logs
            #mv NOUT-000 log/${time_present}_rank-000.log

        if [[ ${cfg[keep_members]} == no ]];then
            remove_present_time_members "prior"
            remove_present_time_members "postr"
        fi

    }
    time_loop

    if [[ ${cfg[keep_members]} == no ]];then
        remove_member_folders "prior"
        remove_member_folders "postr"
    fi

    cd $dataset_dir/raw_data/prior; rm fort.21 gs?????.grd
    cd $dataset_dir/raw_data/postr; rm fort.21

    funct_closing 2
}

stat_assi_run(){
    funct_opening 2

    create_member_ids

    for Ephase in prior postr; do
        for Estat in Emean Esprd; do
            for Tkind in ${Tkinds[@]}; do
                create_control_file "assi_${Ephase}_${Estat}" $Tkind ${cfg[time_one]} \
                    ${cfg[cycle_length]} ${cfg[cycle_length_unit]} \
                    "../raw_data/$Ephase/$Estat"
                my_cdo -f nc import_binary \
                    "../raw_data/$Ephase/$Estat/assi_${Ephase}_${Estat}_grid_sigma_${Tkind}.ctl" \
                    "../raw_data/assi_${Ephase}_${Estat}_grid_sigma_${Tkind}.nc"
            done
        done
    done

    if [[ ${cfg[keep_members]} == yes ]];then
        postprocess_members "prior"
        postprocess_members "postr"
    fi

    calculate_ensemble_error "prior"
    calculate_ensemble_error "postr"

    funct_closing 2
}


