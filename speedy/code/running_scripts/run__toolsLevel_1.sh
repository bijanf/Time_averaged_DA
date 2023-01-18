#!/usr/bin/env bash
#===========================================================
# Time cycles
#===========================================================
time_loop(){
    time_present=${cfg[time_start]};
    while (( time_present < cfg[time_final] )); do

        time_forecast=$(time_increment.exe \
	    $time_present ${cfg[cycle_length]} ${cfg[cycle_length_unit]})

        echo " time_present = $time_present"

        ( time_loop_core )

        time_present=$time_forecast

    done
    unset -f time_loop_core
}

time_loop_parallel(){
    policy=${1:-strict}
#     policy=${1:-relaxed}
    time_present=${cfg[time_start]};
    im=1
    while (( time_present < cfg[time_final] )); do
        time_forecast=$(time_increment.exe \
            $time_present ${cfg[cycle_length]} ${cfg[cycle_length_unit]})

        time_loop_core &

        pids[$im]=$!
        echo " time_present = $time_present [PID ${pids[$im]}]"

        (( im%cpus == 0 )) && wait_for_child_processes $policy
        (( im+=1 ))

        time_present=$time_forecast
#         echo $time_present
    done
    wait_for_child_processes $policy # of an incomplete batch
    unset -f time_loop_core
}

#===========================================================
# Ensemble cycles
#===========================================================
ensemble_loop_serial(){
    im=1
    for member_id in ${member_ids[@]}; do

        ensemble_loop_core

        (( im+=1 ))
    done
    unset -f ensemble_loop_core
}

ensemble_loop_parallel(){
    policy=${1:-strict}
#     policy=${1:-relaxed}
    im=1
    for member_id in ${member_ids[@]}; do

        ensemble_loop_core &

        pids[$im]=$!
        echo " Member $member_id [PID ${pids[$im]}]"

        (( im%cpus == 0 )) && wait_for_child_processes $policy
        (( im+=1 ))
    done
    wait_for_child_processes $policy # of an incomplete batch
    unset -f ensemble_loop_core
}

#===========================================================
# Speedy related routines
#===========================================================
# create folder and link forcings in it
create_speedy_folder(){
    speedy_folder=$1
    mkdir -p $speedy_folder
    speedy_link_forcings t30 "$speedy_folder"
}

clean_speedy_folder(){
    speedy_folder=$1
    rm -f $speedy_folder/fort.*
    rm -f $speedy_folder/time_forecast.dat
    rm -f $speedy_folder/${cfg[time_start]}_grid_sigma_*.rst
}

link_surface_geopotential_height(){
    target_dir=$1
    ln -fs $CODE_DIR/speedy_tools/orography_t30.dat $target_dir/fort.21
}

#===========================================================
# Ensemble related routines
#===========================================================
create_member_ids(){
    funct_opening 2

    # array of members labels with 3 digits
    for (( member_n=1; member_n <= cfg[ensemble_size]; member_n++)); do
        member_ids[member_n]="$(printf '%03d' ${member_n})"
    done
    declare -r member_ids

    funct_closing 2
}

remove_present_time_members(){
    funct_opening 2

    Ephase=$1
    ensemble_loop_core(){
        rm -f $dataset_dir/raw_data/$Ephase/$member_id/${time_present}*; }
    ensemble_loop_parallel
#     for member_id in ${member_ids[@]}; do
#         rm -f $dataset_dir/raw_data/$Ephase/$member_id/${time_present}*
#     done

    funct_closing 2
}

remove_member_folders(){
    funct_opening 2

    Ephase=$1
    ensemble_loop_core(){ rm -fr $dataset_dir/raw_data/$Ephase/$member_id; }
    ensemble_loop_parallel
#     for member_id in ${member_ids[@]}; do
#         rm -fr $dataset_dir/raw_data/$Ephase/$member_id
#     done

    funct_closing 2
}

create_ensemble_folders(){
    funct_opening 2

    Ephase=$1

    for member_id in ${member_ids[@]}; do
        create_speedy_folder "./$Ephase/$member_id"
    done

    for Estat in Emean Esprd Eskew; do
        mkdir -p "$dataset_dir/raw_data/$Ephase/$Estat"
    done

    mkdir -p "$dataset_dir/raw_data/infl"

    link_surface_geopotential_height "$dataset_dir/raw_data/$Ephase"

    funct_closing 2
}

propagate_ensemble(){
    funct_opening 2

    in_flag=$1; out_flag=$2; Ephase_in=$3; Ephase_out=$4
    ensemble_loop_core(){
        in_dir=$dataset_dir/raw_data/$Ephase_in/${member_id}
        out_dir=$dataset_dir/raw_data/$Ephase_out/${member_id}
        (
	    cd $in_dir
            speedy_run $in_flag $out_flag $time_present $time_forecast \
                $out_dir
        )
    }
    ensemble_loop_parallel

    funct_closing 2
}

calculate_ensemble_obs(){
    funct_opening 2

    ensemble_loop_core(){
        in_dir=$dataset_dir/raw_data/prior/${member_id}
        out_dir=$dataset_dir/raw_data/prior/${member_id}
        (   cd $in_dir
            link_surface_geopotential_height .

            clean_obs.exe \
                ${time_forecast}_grid_sigma_Taver.rst \
                $dataset_dir/raw_data/obs/station.tbl \
                ${time_forecast}_Taver_clean.obs \
                ${time_present}_monthly_means.rst \
                > $out_dir/${time_present}_clean_obs.log
        )
    }
    ensemble_loop_parallel

    funct_closing 2
}

gather_monthly_means(){
    funct_opening 2

    if [[ ${cfg[cycle_length_unit]} == "mo" ]];then
        export run_length=${cfg[run_length]}
        export cycle_length=${cfg[cycle_length]}
    else
        error "only monthly step allowed currently"
    fi

    cd "../raw_data/nature"
    rm -f nature_monthly_means.rst
    ls ??????????_monthly_means.rst > monthly_means_list.dat
    gather_monthly_means.exe #> $dataset_dir/logs/dirty_obs.log

    create_control_file_monthly_means nature_monthly_means \
        ${cfg[time_one]} ${cfg[run_length]} .
    create_control_file_monthly_means nature_monthly_means__mean \
        ${cfg[time_final]} 1 .
    create_control_file_monthly_means nature_monthly_means__stdd \
        ${cfg[time_final]} 1 .
    create_control_file_monthly_means nature_monthly_means__min \
        ${cfg[time_final]} 1 .
    create_control_file_monthly_means nature_monthly_means__max \
        ${cfg[time_final]} 1 .

    export_fortran_binaries_as_netcdf

    mv *.nc ../

    funct_closing 2
}

#---------------------------------------------------------
#> @brief Gather initial states from a sampling dataset
#---------------------------------------------------------
set_initial_ensemble(){
    funct_opening 2

    Ephase=$1

    sampling_dir="$dataset_dir/${cfg[rel_sampling_dir]}/raw_data/nature"
    sample_name=($(find $sampling_dir -path "*_grid_sigma_Insta.rst"))
    [[ ${#sample_name[@]} -ge $ensemble_size ]] || error "Too few samples"

    n=1
    # echo ${!sample_name[@]}
    while (( n <= cfg[ensemble_size] )); do
        ln -s "${sample_name[$((n-1))]}" \
            $dataset_dir/raw_data/$Ephase/${member_ids[$n]}/${cfg[time_start]}_grid_sigma_Insta.rst
        (( n+=1 ))
    done

    funct_closing 2
}

clean_initial_ensemble(){
    funct_opening 2

    Ephase=$1
    ensemble_loop_core(){
        rm $dataset_dir/raw_data/$Ephase/$member_id/${cfg[time_zero]}_grid_sigma_Insta.rst
    }
    ensemble_loop_parallel

    funct_closing 2
}

link_member_states(){
    funct_opening 2

    Ephase=$1; Tkind=$2; time=$3; target_dir=$4
    ensemble_loop_core(){
        ln -fs \
            $dataset_dir/raw_data/$Ephase/$member_id/${time}_grid_sigma_${Tkind}.rst \
            $target_dir/gs01${member_id}.grd
    }
    ensemble_loop_serial

    funct_closing 2
}

link_nature_stats_to_ensemble(){
    funct_opening 2

#     nature_dir=../../${cfg[rel_nature_dir]}/raw_data/nature
    nature_dir=$dataset_dir/${cfg[rel_nature_dir]}/raw_data/nature
    ensemble_loop_core(){
        ln -fs \
            $nature_dir/nature_*.rst \
            $dataset_dir/raw_data/prior/$member_id/
    }
    ensemble_loop_serial

    funct_closing 2
}

link_member_monthly_means(){
    funct_opening 2

    time=$1; target_dir=$2
    ensemble_loop_core(){
        ln -fs \
            $dataset_dir/raw_data/prior/$member_id/${time}_monthly_means.rst \
            $target_dir/monthly_means_01${member_id}.grd
    }
    ensemble_loop_serial

    funct_closing 2
}

link_ensemble_TA_obs(){
    funct_opening 2

    time=$1;
    target_dir=$2
    ensemble_loop_core(){
        ln -fs \
            $dataset_dir/raw_data/prior/$member_id/${time}_Taver_clean.obs \
            $target_dir/ta_obs_01${member_id}.grd
    }
    ensemble_loop_serial

    funct_closing 2
}

copy_nature_control_files(){
    funct_opening 2

    target_dir=$1; state_name=$2
    nature_dir="$dataset_dir/${cfg[rel_nature_dir]}"
    for Tkind in ${Tkinds[@]};do
        cp  $nature_dir/raw_data/nature/nature_grid_sigma_${Tkind}.ctl \
            $target_dir/${state_name}_grid_sigma_${Tkind}.ctl
    done

    funct_closing 2
}

update_Taver_ensemble(){
    funct_opening 2

    link_dirty_observations $time_forecast .
    link_ensemble_TA_obs    $time_forecast .
    link_member_states "prior" "Taver" $time_forecast .

#     mpiexec -n $cpus letkf.exe < /dev/null #> /dev/null
    mpiexec -n $cpus letkf.exe < /dev/null > /dev/null

    print_line;
    echo 'tail -n 18 NOUT-000 :'
    tail -n 18 NOUT-000 # show observational departure
    #cat NOUT-000 # show observational departure
    print_line;

    mv gues_me.grd "prior/Emean/${time_forecast}_grid_sigma_Taver.rst"
    mv gues_sp.grd "prior/Esprd/${time_forecast}_grid_sigma_Taver.rst"
    mv anal_me.grd "postr/Emean/${time_forecast}_grid_sigma_Taver.rst"
    mv anal_sp.grd "postr/Esprd/${time_forecast}_grid_sigma_Taver.rst"

    ensemble_loop_core(){
        mv    anal${member_id}.grd \
            postr/${member_id}/${time_forecast}_grid_sigma_Taver.rst
    }
    ensemble_loop_serial

    funct_closing 2
}

time_averaged_recomposition(){
    funct_opening 2

    ensemble_loop_core(){
        (
            cd postr/${member_id}
            ln -fs ../../prior/${member_id}/${time_forecast}_grid_sigma_Tanom.rst .
        )
#       cp  prior/${member_id}/${time_forecast}_grid_sigma_Tanom.rst \
#           postr/${member_id}/${time_forecast}_grid_sigma_Tanom.rst
        add_states.exe \
            postr/${member_id}/${time_forecast}_grid_sigma_Taver.rst \
            postr/${member_id}/${time_forecast}_grid_sigma_Tanom.rst \
            postr/${member_id}/${time_forecast}_grid_sigma_Insta.rst > /dev/null
    }
    ensemble_loop_parallel

    funct_closing 2
}

#=======================================================================
# Observation related routines
#=======================================================================
#----------------------------------------------------
#> @brief Harvests observations from a nature run
#----------------------------------------------------
observe_nature(){
    funct_opening 2

    mcd "obs"

    create_station_set.sh $station_set_number $(pwd)/station.tbl
    link_surface_geopotential_height .

    nature_dir=../../${cfg[rel_nature_dir]}/raw_data/nature
    ln -sf $nature_dir/nature_*.rst .

    echo " Generating clean obs"

    time_loop_core(){
        clean_obs.exe \
            $nature_dir/${time_forecast}_grid_sigma_Taver.rst \
            station.tbl ${time_forecast}_grid_sigma_Taver_clean.obs \
            $nature_dir/${time_present}_monthly_means.rst \
            > clean_obs_${time_forecast}.log
    }
    time_loop_parallel

    time_loop_core(){ cat clean_obs_${time_forecast}.log; }
    time_loop

    echo " Generating dirty obs"
    ls *clean.obs > clean_obs_list.dat
    obs_number=$(cat clean_obs_list.dat | wc -l)
    (( $obs_number > 1 )) || error "A least 2 obs are needed to calculate Tstdd"

    sully_obs.exe > $dataset_dir/logs/dirty_obs.log

    # monitor obs
    #time_loop_core(){
        #echo $time_present
        #dump_obs.exe ${time_forecast}_grid_sigma_Taver_clean.obs
        #dump_obs.exe ${time_forecast}_grid_sigma_Taver_clean.obs.dirty
    #}
    #time_loop

    funct_closing 2
}

link_dirty_observations(){
    funct_opening 2

    time=$1
    target_dir=$2

    ln -fs $dataset_dir/raw_data/obs/${time}_grid_sigma_Taver_clean.obs.dirty \
        $target_dir/obs01.dat

    funct_closing 2
}

#===========================================================
# Postprocessing routines
#===========================================================
#-----------------------------------------------------------
#> @brief Look for Grads control files and try to write
#> the associated fortran binary files in netcdf using CDO
#-----------------------------------------------------------
export_grads_binaries_as_netcdf(){
    funct_opening 2

    folder=$1
    option=${2:-none}

    grads_files=($(find $folder -name "*.ctl"))

    if [[ -n ${grads_files[@]:-} ]]; then
        for grads_file in ${grads_files[@]}; do
            export_grads_binary_as_netcdf "$grads_file" "$option"
        done
    else
        echo "No fortran binaries to export"
    fi

    funct_closing 2
}

export_grads_binary_as_netcdf(){
    funct_opening 2

    binary_path=$1
    option=${2:-none}
    #cd $dir_path

    binary_name=$(barename "$binary_path")
    binary_dir=$(dirname "$binary_path")
    ( cd $binary_dir
        my_cdo -f nc import_binary \
            ${binary_name}.ctl ${binary_name}.nc || return_val=$?

        if [[ $option == erase ]] && [[ ${return_val:-} == 0 ]];then
            rm ${binary_name}.{grd,ctl}
        fi
    )

    funct_closing 2
}

export_ensemble_run_output_as_netcdf(){
    funct_opening 2

    run_type=$1; shift 1
    Ephases=$@
    Estats=(Emean Esprd Eskew)
    # case "$run_type" in
    #     free) Estats=(Emean Esprd Eskew);;
    #     assi) Estats=(Emean Esprd);;
    # esac

    for Ephase in ${Ephases[@]}; do
        for Estat in ${Estats[@]}; do
            for Tkind in ${Tkinds[@]}; do
                create_control_file "${run_type}_${Ephase}_${Estat}" $Tkind ${cfg[time_one]} \
                    ${cfg[cycle_length]} ${cfg[cycle_length_unit]} \
                    "../raw_data/$Ephase/$Estat" >&3
                my_cdo -f nc import_binary \
                    "../raw_data/$Ephase/$Estat/${run_type}_${Ephase}_${Estat}_grid_sigma_${Tkind}.ctl" \
                    "../raw_data/${run_type}_${Ephase}_${Estat}_grid_sigma_${Tkind}.nc"
            done
        done
    done

    funct_closing 2
}

create_common_descriptor_files(){
    funct_opening 2

    state_dir=$1
    state_name=$2
    ( cd "$state_dir"
        for Tkind in ${Tkinds[@]}; do
            mv ${cfg[time_one]}_grid_sigma_${Tkind}.ctl \
                ${state_name}_grid_sigma_${Tkind}.ctl
        done
        rm -f [0-9]*.ctl
    )

    funct_closing 2
}

#===========================================================
# Statistical functions
#===========================================================
calculate_ensemble_stats(){
    funct_opening 2

    Ephase=$1 # prior or postr
    Tkind=$2 # Insta,Taver,Tanom
    time=$3

    (   cd $dataset_dir/raw_data/$Ephase
        link_member_states $Ephase $Tkind $time .

        mpiexec -n $cpus Emean_spread.exe < /dev/null

        mv gues_me.grd "Emean/${time}_grid_sigma_${Tkind}.rst"
        mv gues_sp.grd "Esprd/${time}_grid_sigma_${Tkind}.rst"
    )

    (   cd $dataset_dir/raw_data/$Ephase
        touch member_list.dat
        ensemble_loop_core(){
            member_name="member_${member_id}.rst"
            # ln -fs ${member_id}/${time_present}_grid_sigma_${Tkind}.rst \
            #     $member_name
            # echo "$member_name" >> member_list.dat
            echo ${member_id}/${time}_grid_sigma_${Tkind}.rst \
                >> member_list.dat
        }
        ensemble_loop_serial

        Eskewness.exe "member_list.dat" "Eskew.rst" #> /dev/null
        mv "Eskew.rst" Eskew/${time_present}_grid_sigma_${Tkind}.rst
        rm member_list.dat #member_???.rst
    )

    funct_closing 2
}

calculate_ensemble_mean(){
    funct_opening 2

    Ephase=$1 # prior or postr
    Tkind=$2  # Insta,Taver,Tanom
    (
        cd $dataset_dir/raw_data/$Ephase
        touch member_list.dat
        ensemble_loop_core(){
            member_name="member_${member_id}.rst"
            # ln -fs ${member_id}/${time_present}_grid_sigma_${Tkind}.rst \
            #     $member_name
            # echo "$member_name" >> member_list.dat
            echo ${member_id}/${time_present}_grid_sigma_${Tkind}.rst \
                >> member_list.dat
        }
        ensemble_loop_serial

        Emean.exe "member_list.dat" "Emean.rst" > /dev/null
        mv "Emean.rst" Emean/${time_present}_grid_sigma_${Tkind}.rst
        rm member_list.dat #member_???.rst
    )

    funct_closing 2
}

ensemble_skewness(){
    funct_opening 2

    Ephase=$1 # prior or postr
    Tkind=$2  # Insta,Taver,Tanom
    time=$3
    (
        cd $dataset_dir/raw_data/$Ephase
        touch member_list.dat
        ensemble_loop_core(){
            member_name="member_${member_id}.rst"
            # ln -fs ${member_id}/${time_present}_grid_sigma_${Tkind}.rst \
            #     $member_name
            # echo "$member_name" >> member_list.dat
            echo ${member_id}/${time}_grid_sigma_${Tkind}.rst \
                >> member_list.dat
        }
        ensemble_loop_serial

        Eskewness.exe "member_list.dat" "Eskew.rst" #> /dev/null
        mv "Eskew.rst" Eskew/${time_present}_grid_sigma_${Tkind}.rst
        rm member_list.dat #member_???.rst
    )

    funct_closing 2
}

#----------------------------
# Absolute comparisons
#----------------------------
Decrease(){
    local stat_A=$1
    local stat_B=$2 #reference
    local sufix=${3:-'reduc'}
    my_cdo sub ${stat_B}.nc     ${stat_A}.nc     ${stat_A}_${sufix}.nc
}

Increase(){
    local stat_A=$1
    local stat_B=$2 #reference
    local sufix=${3:-'incre'}
    my_cdo sub ${stat_A}.nc     ${stat_B}.nc     ${stat_A}_${sufix}.nc
}

#------------------------------------------------------
# Relative comparisons (very noisy in low error areas)
#------------------------------------------------------
# Decrease(){
#     local stat_A=$1
#     local stat_B=$2 #reference
#     local prefix=${3:-'reduc'}
#     my_cdo sub ${stat_B}.nc     ${stat_A}.nc     ${stat_A}_sub.nc
#     my_cdo div ${stat_A}_sub.nc ${stat_B}.nc     ${stat_A}_div.nc
#     my_cdo mulc,100.0           ${stat_A}_div.nc ${stat_A}_${prefix}.nc
#     rm                          ${stat_A}_sub.nc ${stat_A}_div.nc
# }

# Increase(){
#     local stat_A=$1
#     local stat_B=$2 #reference
#     my_cdo sub ${stat_A}.nc     ${stat_B}.nc     ${stat_A}_sub.nc
#     my_cdo div ${stat_A}_sub.nc ${stat_B}.nc     ${stat_A}_div.nc
#     my_cdo mulc,100.0           ${stat_A}_div.nc ${stat_A}_incre.nc
#     rm                          ${stat_A}_sub.nc ${stat_A}_div.nc
# }
#------------------------------------------------------

ensemble_time_stats(){
    funct_opening 2

    Ephase=$1
    echo " $Ephase"

    cd "$dataset_dir/stats"
    for Tkind in ${Tkinds[@]}; do
        CDO "../raw_data" . \
            "${cfg[run_type]}_${Ephase}_error_grid_sigma_${Tkind}" fldstd1
        select_levels . . "${cfg[run_type]}_${Ephase}_error_grid_sigma_${Tkind}_fldstd1"
        CDO "../raw_data" . \
            "${cfg[run_type]}_${Ephase}_Esprd_grid_sigma_${Tkind}" fldmean
        select_levels . . "${cfg[run_type]}_${Ephase}_Esprd_grid_sigma_${Tkind}_fldmean"
    done

    funct_closing 2
}

select_levels(){

    state_dir=$1; outDir="$2"; state_name="$3";
    CDO "$state_dir" "$outDir" "$state_name" "selname,t" "sellevidx,1"
    CDO "$state_dir" "$outDir" "$state_name" "selname,q" "sellevidx,2"
    CDO "$state_dir" "$outDir" "$state_name" "selname,u" "sellevidx,5"
    CDO "$state_dir" "$outDir" "$state_name" "selname,v" "sellevidx,5"
    CDO "$state_dir" "$outDir" "$state_name" "selname,ps"
}

reduceDimensionality(){
    funct_opening 2

    stat_dir=$1; outDir="$2"; stat_name="$3";# time_oper="$4"
    echo " $stat_name"

    CDO           "$stat_dir" "$outDir" "${stat_name}" zonmean
    select_levels "$stat_dir" "$outDir" "${stat_name}" >&3

    funct_closing 2
}

ensemble_spatial_stats(){
    funct_opening 2

    Ephase=$1
    echo " $Ephase"

    cd "$dataset_dir/stats"
    for Tkind in Insta Tanom; do
        # Skewness
        CDO ../raw_data . "${cfg[run_type]}_${Ephase}_Eskew_grid_sigma_${Tkind}" timmean
        reduceDimensionality   . . "${cfg[run_type]}_${Ephase}_Eskew_grid_sigma_${Tkind}_timmean"
    done

    for Tkind in ${Tkinds[@]}; do
        my_cdo sub \
            ../${cfg[rel_nature_dir]}/raw_data/nature_grid_sigma_${Tkind}.nc \
            ../raw_data/${cfg[run_type]}_${Ephase}_Emean_grid_sigma_${Tkind}.nc \
            ../raw_data/${cfg[run_type]}_${Ephase}_error_grid_sigma_${Tkind}.nc

        # Average Mean
        CDO        ../raw_data . "${cfg[run_type]}_${Ephase}_Emean_grid_sigma_${Tkind}" timmean
        reduceDimensionality . . "${cfg[run_type]}_${Ephase}_Emean_grid_sigma_${Tkind}_timmean"

        # Mean Standard Deviation
        CDO        ../raw_data . "${cfg[run_type]}_${Ephase}_Emean_grid_sigma_${Tkind}" timstd1
        reduceDimensionality . . "${cfg[run_type]}_${Ephase}_Emean_grid_sigma_${Tkind}_timstd1"

        # Spread
        CDO        ../raw_data . "${cfg[run_type]}_${Ephase}_Esprd_grid_sigma_${Tkind}" timmean
        reduceDimensionality . . "${cfg[run_type]}_${Ephase}_Esprd_grid_sigma_${Tkind}_timmean"

        # RMSE
        CDO        ../raw_data . "${cfg[run_type]}_${Ephase}_error_grid_sigma_${Tkind}" timstd1
        reduceDimensionality . . "${cfg[run_type]}_${Ephase}_error_grid_sigma_${Tkind}_timstd1"
    done

    funct_closing 2
}

calculate_error_reduction(){
    funct_opening 2

    cd "$dataset_dir/stats"
    Ephases=('prior' 'postr')
    freeStatsDir="../${cfg[rel_free_run_dir]}/stats"
    for Tkind in ${Tkinds[@]}; do
        for Ephase in ${Ephases[@]}; do
            stat_sufix="error_grid_sigma_${Tkind}_timstd1"
            stat_name="assi_${Ephase}_${stat_sufix}"
            Decrease       "assi_${Ephase}_${stat_sufix}" \
                "${freeStatsDir}/free_prior_${stat_sufix}"
            reduceDimensionality . . "${stat_name}_reduc"
            # CDO . . "${stat_name}_reduc" zonmean
            # CDO . . "${stat_name}_reduc" vertmean
            # CDO . . "${stat_name}_reduc" sellevidx,1
            # CDO . . "${stat_name}_reduc_vertmean"    fldmean
            # CDO . . "${stat_name}_reduc_sellevidx,1" fldmean
        done
    done

    funct_closing 2
}

export_nc_scalar_to_ascii(){
    scalar_name=$1

    for var in ${speedy_vars[@]}; do
        my_cdo select,name=$var \
            ${scalar_name}.nc ${scalar_name}_${var}.nc
        my_cdo output \
            ${scalar_name}_${var}.nc > ${scalar_name}_${var}.dat
    done
}

error_increase_regarding_linear_obs(){
    funct_opening 2

    assi_run_norm_addObs_dir="../${cfg[rel_assi_run_norm_addObs_dir]}/stats"

#     assi_run_name="assi_run_set__m24__stationSet8__obs_op_norm_add"
#     linearObsStatsDir="../../../../${assi_run_name}/runs/cycle_length-${cfg[cycle_length]}/stats"

    cd "$dataset_dir/stats"
    Ephases=('prior' 'postr')
    for Tkind in ${Tkinds[@]}; do
        for Ephase in ${Ephases[@]}; do
            stat_name="assi_${Ephase}_error_grid_sigma_${Tkind}_timstd1"
            Increase ${stat_name} "${assi_run_norm_addObs_dir}/${stat_name}"
            reduceDimensionality . . "${stat_name}_incre"
            # CDO . . "${stat_name}_incre" zonmean
            # CDO . . "${stat_name}_incre" vertmean
            # CDO . . "${stat_name}_incre" sellevidx,1
            # CDO . . "${stat_name}_incre_vertmean"    fldmean
            # CDO . . "${stat_name}_incre_sellevidx,1" fldmean
            # export_nc_scalar_to_ascii "${stat_name}_incre_vertmean"
            # export_nc_scalar_to_ascii "${stat_name}_incre_sellevidx,1_fldmean"
        done
    done


    funct_closing 2
}

error_decrease_regarding_norm_min(){
        funct_opening 2


        normMinObsStatsDir="../../assi_run__m24__hLoc500km__stationSet8__obs_op_norm_min/stats"
#     assi_run_name="assi_run_set__m24__stationSet8__obs_op_norm_min"
#     normMinObsStatsDir="../../../../${assi_run_name}/runs/cycle_length-${cfg[cycle_length]}/stats"

        cd "$dataset_dir/stats"
        Ephases=('prior' 'postr')
        stat_id="reducRefMin"

        for Tkind in ${Tkinds[@]}; do
            for Ephase in ${Ephases[@]}; do
                stat_name="assi_${Ephase}_error_grid_sigma_${Tkind}_timstd1"
                Decrease ${stat_name} "${normMinObsStatsDir}/${stat_name}" "$stat_id"
                reduceDimensionality . . "${stat_name}_${stat_id}"
            # CDO . . "${stat_name}_${stat_id}" zonmean

# CDO . . "${stat_name}_${stat_id}" vertmean
            # CDO . . "${stat_name}_${stat_id}" sellevidx,1
            # CDO . . "${stat_name}_${stat_id}_vertmean"    fldmean
            # CDO . . "${stat_name}_${stat_id}_sellevidx,1" fldmean
            # export_nc_scalar_to_ascii "${stat_name}_${stat_id}_vertmean"
            # export_nc_scalar_to_ascii "${stat_name}_${stat_id}_sellevidx,1_fldmean"
            done
        done

        funct_closing 2
    }

    error_reduction_regarding(){
        funct_opening 2

	refRunId="$1"
	refRunDir="$2"
#        normMinObsStatsDir="../../assi_run__m24__hLoc500km__stationSet8__obs_op_norm_min/stats"
#     assi_run_name="assi_run_set__m24__stationSet8__obs_op_norm_min"
#     normMinObsStatsDir="../../../../${assi_run_name}/runs/cycle_length-${cfg[cycle_length]}/stats"

        cd "$dataset_dir/stats"
        Ephases=('prior' 'postr')
        stat_id="ErrorReduc_ref${refRunId}"

        for Tkind in ${Tkinds[@]}; do
            for Ephase in ${Ephases[@]}; do
                stat_name="assi_${Ephase}_error_grid_sigma_${Tkind}_timstd1"
                Decrease ${stat_name} "${refRunDir}/stats/${stat_name}" "$stat_id"
                reduceDimensionality . . "${stat_name}_${stat_id}"
            done
        done

        funct_closing 2
    }

# ensemble_spatial_stats(){
#     funct_opening 2

#     cd "$dataset_dir/stats"
#     for Tkind in ${Tkinds[@]}; do
#         CDO "../raw_data" "${cfg[run_type]}_prior_error_grid_sigma_${Tkind}" fldstd1 vertmean
#         CDO "../raw_data" "${cfg[run_type]}_postr_error_grid_sigma_${Tkind}" fldstd1 vertmean
#         CDO "../raw_data" "${cfg[run_type]}_prior_Esprd_grid_sigma_${Tkind}" fldmean vertmean
#         CDO "../raw_data" "${cfg[run_type]}_postr_Esprd_grid_sigma_${Tkind}" fldmean vertmean
#     done

#     funct_closing 2
# }




    state_time_spatial_stats(){
        funct_opening 2

        state_dir=$1; state=$2; Tstat=$3
        diags=("zonmean" "vertmean" "sellevidx,1")
    ## for vert_coord in press sigma; do

        for Tkind in ${Tkinds[@]}; do
            CDO "${state_dir}" "${state}_grid_sigma_${Tkind}" $Tstat
            for diag in ${diags[@]}; do
                CDO . "${state}_grid_sigma_${Tkind}_${Tstat}" $diag
            done
        done

        funct_closing 2
    }




#===========================================================
# Reporting functions
#===========================================================

    plot_array_vertical_speedyvars(){
        funct_opening 2

        scalar=$1; array_title="${2:-}"
        echo " scalar = $scalar"

    # speedy_vars=("u" "v" "t" "q")
        par1_values=(${speedy_vars[@]})
        par1_span_length=${#speedy_vars[@]}
            par1_name="speedy_var"

            plots_dir=${plots_dir:-"$dataset_dir/plots"}

            plot_path(){ echo "$plots_dir/${scalar}_${par1_value}.pdf"; }
#    plot_path(){ echo "./plots/${scalar}_${par1_value}.pdf"; }
            array_width=${array_width:-"0.80\textwidth"}
            plot_array_vertical "$array_title"

            funct_closing 2
        }

        docu_speedy_scalar_vertical_zonal_mean(){
            funct_opening 2

            state_name=$1; Tkind=$2; time_stat=$3

            array_width="0.80\textwidth"
            echo "\begin{minipage}[c]{\textwidth}"   >> $texfile
            section_title "Overall $(Tkind_l $Tkind) $(label $state_name run) $(label ${state_name}*${time_stat} Quantity)"
            scalar=${state_name}_grid_sigma_${Tkind}_${time_stat}

            plot_array_vertical_speedyvars $scalar
            echo "\end{minipage}"   >> $texfile

            funct_closing 2
        }

#docu_speedy_scalar_vertical_zonal_mean(){
    #state_name=$1; Tkind=$2; time_stat=$3

    #diags=('vertmean' 'zonmean')
    #echo "\begin{minipage}[c]{\textwidth}"   >> $texfile
    #section_title "Overall $(Tkind_l $Tkind) $(label $state_name run) $(label ${state_name}*${time_stat} Quantity)"
    #for diag in ${diags[@]}; do
        #scalar=${state_name}_grid_sigma_${Tkind}_${time_stat}_${diag}

        #case "$diag" in
            #vertmean)
                #array_width="0.60\textwidth"
                #plot_array_vertical_speedyvars $scalar "Vertical mean"
                #;;
            #zonmean )
                #array_width="0.29\textwidth"
                #plot_array_vertical_speedyvars $scalar "Zonal mean"
                #;;
        #esac
    #done
    #echo "\end{minipage}"   >> $texfile
#}

#===========================================================
# not tested
#===========================================================

        report_metainfo(){
            funct_opening 2

    #newpage
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


            funct_closing 2
        }

        postprocess_ensemble(){
            funct_opening 2

            Ephase=$1
            if [[ ${cfg[keep_members]} == yes ]]; then
                ensemble_loop_core(){
                    cd $dataset_dir/raw_data/$Ephase/$member_id

                    create_common_descriptor_files "member_${member_id}"
                    export_fortran_binaries_as_netcdf

                    rm ${cfg[time_start]}_grid_sigma_*.rst
                    rm -f time_forecast.dat fort.*
                }
                ensemble_loop_parallel
            else
                remove_member_folders $Ephase
            fi
            find $dataset_dir/raw_data/$Ephase -maxdepth 1 -type l | xargs rm -f

            funct_closing 2
        }

        postprocess_members(){
            funct_opening 2

            Ephase=$1
            clean_ensemble_folders "$Ephase"
#     ensemble_loop_core(){
#         cd $dataset_dir/raw_data/$Ephase/$member_id
#         #create_common_descriptor_files "member_${member_id}"
#         #export_fortran_binaries_as_netcdf
#     }
#     ensemble_loop_serial

#     for member_id in ${member_ids[@]}; do
#         cd $dataset_dir/raw_data/$Ephase/$member_id
#         #create_common_descriptor_files "member_${member_id}"
#         #export_fortran_binaries_as_netcdf
#     done

            funct_closing 2
        }

        clean_ensemble_folders(){
            funct_opening 2

            Ephase=$1
            ensemble_loop_core(){
                cd $dataset_dir/raw_data/$Ephase/$member_id
                rm -f fort.*
                rm -f time_forecast.dat
                rm -f ${cfg[time_start]}_grid_sigma_*.rst
            }
            ensemble_loop_parallel

    # rm gs?????.grd fort.21 obs01.dat
    # for Estat in Emean Esprd; do
    #     folder="$Ephase/$Estat"
    #     mkdir -p "$folder"
    #     copy_nature_control_files "$folder" \
    #         "${cfg[run_type]}_${Ephase}_${Estat}"
    # done

            funct_closing 2
        }

#-------------------------------
# Outdated stuff
#-------------------------------

# state_stats_vs_time(){
#     funct_opening 2


#     state_dir=$1; state=$2; #Tstat=$3
#  #   diags=("zonmean" "vertmean")
#     ## for vert_coord in press sigma; do

#     cd "$dataset_dir/stats"
#     for Tkind in ${Tkinds[@]}; do
#         CDO "$state_dir" "${state}_grid_sigma_${Tkind}" fldstd1 vertmean
#     done

#     funct_closing 2
# }


#     local input_nc="$1"
#     input_name=$(barename $input_nc)
#     #diag=("Zmean" "Lev-1" "Lev-1_Hmean")
#     diag=("zonmean" "vertmean")

#     my_cdo  zonmean $input_nc ${input_name}_zonmean.nc
#     my_cdo vertmean $input_nc ${input_name}_vertmean.nc
# #     my_cdo sellevidx,1 $input_nc ${input_name}_Lev-1.nc
# #     my_cdo  fldmean ${input_name}_Lev-1.nc ${input_name}_Lev-1_Hmean.nc

#     #my_cdo vertmean $input_nc ${input_name}_Lmean.nc
#     #my_cdo  fldmean $input_nc ${input_name}_Hmean.nc
#     #my_cdo  sellevidx,1 \
#         #${input_name}_Hmean.nc ${input_name}_Hmean_surf.nc
#     # my_cdo  sellevel,0.95 \
#     #     ${input_name}_Hmean.nc ${input_name}_Hmean_surf.nc
#     #my_cdo vertmean ${input_name}_Hmean.nc ${input_name}_Gmean.nc

#     funct_closing 2
# }


# Decrease(){
#     local stat_A=$1
#     local stat_B=$2 #reference
#     local stat_reduc=$3
#     my_cdo sub ${stat_B}.nc ${stat_A}.nc ${stat_A}_sub.nc
#     my_cdo div ${stat_A}_sub.nc ${stat_B}.nc ${stat_A}_div.nc
#     my_cdo mulc,100.0 ${stat_A}_div.nc ${stat_reduc}.nc
#     rm ${stat_A}_sub.nc ${stat_A}_div.nc
# }

# Increase(){
#     local stat_A=$1
#     local stat_B=$2 #reference
#     local stat_reduc=$3
#     my_cdo sub ${stat_A}.nc ${stat_B}.nc ${stat_A}_sub.nc
#     my_cdo div ${stat_A}_sub.nc ${stat_B}.nc ${stat_A}_div.nc
#     my_cdo mulc,100.0 ${stat_A}_div.nc ${stat_reduc}.nc
#     rm ${stat_A}_sub.nc ${stat_A}_div.nc
# }



# cdo_state(){
#     state_dir=$1; state=$2; oper1=$3; oper2=${4:-}

#     if (( $# == 3 )); then
#         for Tkind in ${Tkinds[@]}; do
#             my_cdo ${oper1} \
#                 "${state_dir}/${state}_grid_sigma_${Tkind}.nc" \
#                 "${state}_grid_sigma_${Tkind}_${oper1}.nc"
#         done
#     else
#         for Tkind in ${Tkinds[@]}; do
#             my_cdo ${oper1} \
#                 "${state_dir}/${state}_grid_sigma_${Tkind}.nc" \
#                 "${state}_grid_sigma_${Tkind}_${oper1}.nc"
#             my_cdo ${oper2} \
#                 "${state}_grid_sigma_${Tkind}_${oper1}.nc" \
#                 "${state}_grid_sigma_${Tkind}_${oper1}_${oper2}.nc"
#         done
#     fi
# }



#stat_state(){
    #state_dir=$1; state=$2; Tstat=$3

    #for Tkind in ${Tkinds[@]}; do
        #spatial_stats $state_dir/${state}_grid_sigma_${Tkind}.nc
        #for suffix in ${diag[@]}; do
            ## my_cdo timmean "$state_dir/${state}_grid_sigma_${Tkind}_${suffix}"
            #case $Tstat in
                #Tstdd)
                    #my_cdo timstd1 \
                        #"${state}_grid_sigma_${Tkind}_${suffix}.nc" \
                        #"${state}_grid_sigma_${Tkind}_${suffix}_${Tstat}.nc";;
                #Tmean)
                    #my_cdo timmean \
                        #"${state}_grid_sigma_${Tkind}_${suffix}.nc" \
                        #"${state}_grid_sigma_${Tkind}_${suffix}_${Tstat}.nc";;
                #*     ) error "unsupported Tstat $Tstat";;
            #esac
        #done

##  my_cdo select,name=t \
##      "${state}_grid_sigma_${Tkind}_${suffix}_${Tstat}.nc" \
##      "${state}_${Tstat}_grid_sigma_${Tkind}_${suffix}_temp.nc"

##         for suffix in "Lev-1_Hmean"; do
##             my_cdo select,name=t \
##                 "${state}_${Tstat}_grid_sigma_${Tkind}_${suffix}.nc" \
##                 "${state}_${Tstat}_grid_sigma_${Tkind}_${suffix}_temp.nc"
##             my_cdo output \
##                 "${state}_${Tstat}_grid_sigma_${Tkind}_${suffix}_temp.nc" > \
##                 "${state}_${Tstat}_grid_sigma_${Tkind}_${suffix}_temp.dat"
##             rm "${state}_${Tstat}_grid_sigma_${Tkind}_${suffix}_temp.nc"
##         done
    #done
        ### for vert_coord in press sigma; do

#}
