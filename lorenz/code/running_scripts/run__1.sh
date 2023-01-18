#!/usr/bin/env bash

calc_run(){
    funct_opening 1

    # ulimit -s ${stack_limit:-unlimited} # not allowed by slurm (many warnings)
    ulimit -c ${coredump_Maxsize:-0}

    case ${cfg[run_mode]} in
        "free") # Create nature and free ensemble runs
            sampling_run.exe && \
                nature_run.exe && \
                ensemble_free_run.exe  || error "run crashed"
            ;;
        "assi") # Create ensemble run with data assimilation
            # Bring necessary files # Initial conditions and nature_run
            ln -s ../$rel_run_free_dir/raw_data/sampling.grd .
            ln -s ../$rel_run_free_dir/raw_data/nature_*.grd .
            nature_obs.exe && ensemble_assi_run.exe || error "run crashed"
#            nature_obs.exe && ensemble_assi_run.exe &>/dev/null || error "run crashed"
            ;;
        *     ) error "unsupported run_mode ${cfg[run_mode]}";;
    esac

    # find . -maxdepth 1 -type l | xargs rm -f # delete soft links

    funct_closing 1
}

postprocess_run(){
    funct_opening 1

        # if [[ ! -f run_crashed.cfg ]]; then  # delete soft links
    # if [[ -f run_crashed.cfg ]]; then
    #     echo " - Run crashed probably due to filter divergence"
    # else
        # Complement raw_data
    case ${cfg[run_mode]} in
        "free")
            export_fortran_binaries_as_netcdf
            Subtr nature_Insta nature_Taver nature_Tanom
            ;;
        "assi")
            export_fortran_binaries_as_netcdf "erase"
            find "../$rel_run_free_dir/raw_data/" -name "*.nc" \
                -print0 | xargs -0 -I{} ln -fs {} .
            ;;
    esac

    case ${cfg[run_mode]} in
        "free") Ephases=(prior);;
        "assi") Ephases=(prior postr);;
        *     ) error "unsupported run_mode ${cfg[run_mode]}";;
    esac

    for Ephase in ${Ephases[@]}; do
        for Tkind in Insta Taver Tanom; do
                # 2. Error standard deviation
            Subtr                    nature_${Tkind} \
                ${run_mode}_Emean_${Ephase}_${Tkind} \
                ${run_mode}_error_${Ephase}_${Tkind}
        done
    done
    # fi

    funct_closing 1
}

#===========================================================
#> @brief Overall run stats
#===========================================================
stat_run(){
    funct_opening 1

    stats_dir="."
    stat_cycles=${cfg[cycles]}
    echo " - stat_cycles = $stat_cycles"

    find ../raw_data/ -name "*.nc" -print0 | xargs -0 -I{} ln -fs {} .

    case ${cfg[run_mode]} in
        "free")
            nature_time_stats
            ensemble_field_stats
            ensemble_time_stats
            ;;
        "assi")
            find "../${cfg[rel_run_free_dir]}/stats" -name "*.nc" \
                -print0 | xargs -0 -I{} ln -fs {} .
            ensemble_field_stats
            ensemble_time_stats
            assi_free_time_stats_reduction
            ${cfg[diag]} "obs_error_stdd" "obs_error_stdd_${cfg[diag]}"
            print_statistic "obs_error_stdd_${cfg[diag]}.dat"
            
            covariance__obs_Taver_vs_state_Tanom
            # correlation__obs_Taver_vs_state_Tanom
            #cat ../config/threshold_up.cfg > threshold_up.cfg 
            ;;
    esac

    print_statistics

    funct_closing 1
}

run_detailed_stats(){
    funct_opening 1

    # Perform stats for different stat_cycles
    for stat_cycles in ${cfg[stat_cycles_values]}; do
        echo " - stat_cycles = $stat_cycles"

        stats_dir="stat_cycles-$stat_cycles"
        echo "$stats_dir" >> config/stats_dirs.cfg
        mcd "$stats_dir"
        find ../raw_data/ -name "*.nc" -print0 | xargs -0 -I{} ln -fs {} .

        case ${cfg[run_mode]} in
            "free")
                nature_time_stats
                ensemble_time_stats
                ;;
            "assi")
                ensemble_time_stats
                assi_free_time_stats_reduction
                ;;
        esac
        cd ..
    done

    # Gather results into netcdfs
    scalars=($(find "$stats_dir" -name "*.dat"))
    barename ${scalars[@]} > "config/scalars.cfg"
    get_config_file "config/scalars.cfg"

    mcd "stats_detailed"
    i_run=1
    for scalar in ${cfg[scalars]}; do
        declare -a opts
        opts+=($(printf '%s=\"%s\"' scalar_name $scalar))
        opts+=($(printf '%s=\"%s\"' cfg_dir     $dataset_dir/config))
        my_ncl --logfile="${scalar}.ncl_log" \
            ${opts[@]} $DAS_DIR/create_detailed_run_stats.ncl
    done

    if [[ ${cfg[run_mode]} == assi ]];then
        cd .. ; rm -fr stat_cycles-*
    fi

    funct_closing 1
}

nature_time_stats(){

    for Tkind in Insta Taver Tanom; do
        # Tvar nature_${Tkind} nature_Tvar_${Tkind}
        # Fmean nature_Tvar_${Tkind} nature_Tvar_Fmean_${Tkind}
        Tstdd nature_${Tkind} nature_Tstdd_${Tkind}
        ${cfg[diag]} nature_Tstdd_${Tkind} nature_Tstdd_${cfg[diag]}_${Tkind}
        # Fmean nature_Tstdd_${Tkind} nature_Tstdd_Fmean_${Tkind}
    done
}

correlation__obs_Taver_vs_state_Tanom(){
    
    my_cdo selindexbox,1,1,1,1 \
      obs_clean_Taver.nc \
      obs_clean_Taver_index1.nc

    for Tkind in Tanom Taver Insta; do
        my_cdo selindexbox,2,2,1,1 \
          nature_${Tkind}.nc \
          nature_${Tkind}_index2.nc
        
        touch corr_nature_${Tkind}__obs_clean_Taver.dat
        for var in comp1 comp2; do
            my_cdo select,name=$var \
              nature_${Tkind}_index2.nc \
              nature_${Tkind}_index2_${var}.nc
            my_cdo timcor \
              nature_${Tkind}_index2_${var}.nc \
              obs_clean_Taver_index1.nc \
              corr_nature_${Tkind}__obs_clean_Taver__${var}.nc
            my_cdo output \
              corr_nature_${Tkind}__obs_clean_Taver__${var}.nc >> \
              corr_nature_${Tkind}__obs_clean_Taver.dat
            
            rm -f corr_nature_${Tkind}__obs_clean_Taver__${var}.nc
            rm -f nature_${Tkind}_index2_${var}.nc
        done
        rm -f nature_${Tkind}_index2.nc
    done
    rm -f obs_clean_Taver_index1.nc
}

covariance__obs_Taver_vs_state_Tanom(){
    
    my_cdo selindexbox,1,1,1,1 \
      obs_clean_Taver.nc \
      obs_clean_Taver_index1.nc

    for Tkind in Tanom Taver Insta; do
        my_cdo selindexbox,2,2,1,1 \
          nature_${Tkind}.nc \
          nature_${Tkind}_index2.nc
        
        touch covar_nature_${Tkind}__obs_clean_Taver.dat
        for var in comp1 comp2; do
            my_cdo select,name=$var \
              nature_${Tkind}_index2.nc \
              nature_${Tkind}_index2_${var}.nc
            my_cdo timcovar \
              nature_${Tkind}_index2_${var}.nc \
              obs_clean_Taver_index1.nc \
              covar_nature_${Tkind}__obs_clean_Taver__${var}.nc
            my_cdo output \
              covar_nature_${Tkind}__obs_clean_Taver__${var}.nc >> \
              covar_nature_${Tkind}__obs_clean_Taver.dat
            
            rm -f covar_nature_${Tkind}__obs_clean_Taver__${var}.nc
            rm -f nature_${Tkind}_index2_${var}.nc
        done
        rm -f nature_${Tkind}_index2.nc
    done
    rm -f obs_clean_Taver_index1.nc
}

ensemble_field_stats(){

    for Ephase in ${Ephases[@]}; do
        for Tkind in Insta Taver Tanom; do

            # 2. Error standard deviation
            Subtr                    nature_${Tkind} \
                ${cfg[run_mode]}_Emean_${Ephase}_${Tkind} \
                ${cfg[run_mode]}_error_${Ephase}_${Tkind}

            Fstdd \
                ${cfg[run_mode]}_error_${Ephase}_${Tkind} \
                ${cfg[run_mode]}_error_Fstdd_${Ephase}_${Tkind}
            # my_cdo output -timselmean,$steps,${cfg[spinup_cycles]} \
            #     "${cfg[run_mode]}_error_Fstdd_${Ephase}_${Tkind}.nc" > \
            #     "${cfg[run_mode]}_error_Fstdd_Tmean_${Ephase}_${Tkind}.dat"

            # 3. Spread stats (Ensemble anomaly field standard deviation)
            Fmean_nc \
                ${cfg[run_mode]}_Esprd_${Ephase}_${Tkind} \
                ${cfg[run_mode]}_Esprd_Fmean_${Ephase}_${Tkind}
            # Fstdd \
            #     ${cfg[run_mode]}_Esprd_${Ephase}_${Tkind} \
            #     ${cfg[run_mode]}_Esprd_Fstdd_${Ephase}_${Tkind}
            # my_cdo output -timselmean,$steps,${cfg[spinup_cycles]} \
            #     "${cfg[run_mode]}_Esprd_Fstdd_${Ephase}_${Tkind}.nc" > \
            #     "${cfg[run_mode]}_Esprd_Fstdd_Tmean_${Ephase}_${Tkind}.dat"
        done
    done
}

ensemble_time_stats(){

    for Ephase in ${Ephases[@]}; do
        for Tkind in Insta Taver Tanom; do
            # 1. State variance
            Tstdd \
                ${cfg[run_mode]}_Emean_${Ephase}_${Tkind} \
                ${cfg[run_mode]}_Emean_Tstdd_${Ephase}_${Tkind}
      ${cfg[diag]} \
          ${cfg[run_mode]}_Emean_Tstdd_${Ephase}_${Tkind} \
    ${cfg[run_mode]}_Emean_Tstdd_${cfg[diag]}_${Ephase}_${Tkind}
      
            # Fmean \
            #     ${cfg[run_mode]}_Emean_Tstdd_${Ephase}_${Tkind} \
            #     ${cfg[run_mode]}_Emean_Tstdd_Fmean_${Ephase}_${Tkind}

      
            # Tvar \
            #     ${cfg[run_mode]}_Emean_${Ephase}_${Tkind} \
            #     ${cfg[run_mode]}_Emean_Tvar_${Ephase}_${Tkind}
            # Fmean \
            #     ${cfg[run_mode]}_Emean_Tvar_${Ephase}_${Tkind} \
            #     ${cfg[run_mode]}_Emean_Tvar_Fmean_${Ephase}_${Tkind}
            # my_cdo timselstd1,$steps,${cfg[spinup_cycles]} \
            #     "${cfg[run_mode]}_Emean_${Ephase}_${Tkind}.nc" \
            #     "${cfg[run_mode]}_Emean_Tstdd_${Ephase}_${Tkind}.nc"
            # my_cdo output -fldmean \
            #   "${cfg[run_mode]}_Emean_Tstdd_${Ephase}_${Tkind}.nc" > \
            #   "${cfg[run_mode]}_Emean_Tstdd_Fmean_${Ephase}_${Tkind}.dat"

            # 2. Error standard deviation
            Tstdd \
                ${cfg[run_mode]}_error_${Ephase}_${Tkind} \
                ${cfg[run_mode]}_error_Tstdd_${Ephase}_${Tkind}
      ${cfg[diag]} \
          ${cfg[run_mode]}_error_Tstdd_${Ephase}_${Tkind} \
    ${cfg[run_mode]}_error_Tstdd_${cfg[diag]}_${Ephase}_${Tkind}
            # Fmean \
            #     ${cfg[run_mode]}_error_Tstdd_${Ephase}_${Tkind} \
            #     ${cfg[run_mode]}_error_Tstdd_Fmean_${Ephase}_${Tkind}

            # 3. Spread stats (Ensemble anomaly field standard deviation)
            Tmean \
                ${cfg[run_mode]}_Esprd_${Ephase}_${Tkind} \
                ${cfg[run_mode]}_Esprd_Tmean_${Ephase}_${Tkind}
      ${cfg[diag]} \
    ${cfg[run_mode]}_Esprd_Tmean_${Ephase}_${Tkind} \
    ${cfg[run_mode]}_Esprd_Tmean_${cfg[diag]}_${Ephase}_${Tkind}
            # Fmean \
            #     ${cfg[run_mode]}_Esprd_Tmean_${Ephase}_${Tkind} \
            #     ${cfg[run_mode]}_Esprd_Tmean_Fmean_${Ephase}_${Tkind}
        done
    done
}

assi_free_time_stats_reduction(){

    find ../${cfg[rel_run_free_dir]}/$stats_dir/ -name "*.nc" \
        -print0 | xargs -0 -I{} ln -fs {} .
    for Ephase in ${Ephases[@]}; do
        for Tkind in Insta Taver Tanom; do
                                                # error reduction
            Reduc \
                assi_error_Tstdd_${Ephase}_${Tkind} \
                free_error_Tstdd_prior_${Tkind} \
                assi_error_Tstdd_reduc_${Ephase}_${Tkind}
      ${cfg[diag]} \
                assi_error_Tstdd_reduc_${Ephase}_${Tkind} \
                assi_error_Tstdd_reduc_${cfg[diag]}_${Ephase}_${Tkind}
            # Fmean \
            #     assi_error_Tstdd_reduc_${Ephase}_${Tkind} \
            #     assi_error_Tstdd_reduc_Fmean_${Ephase}_${Tkind}

                                                # Spread reduction
            Reduc \
                assi_Esprd_Tmean_${Ephase}_${Tkind} \
                free_Esprd_Tmean_prior_${Tkind} \
                assi_Esprd_Tmean_reduc_${Ephase}_${Tkind}
      ${cfg[diag]} \
                assi_Esprd_Tmean_reduc_${Ephase}_${Tkind} \
                assi_Esprd_Tmean_reduc_${cfg[diag]}_${Ephase}_${Tkind}
            # Fmean \
            #     assi_Esprd_Tmean_reduc_${Ephase}_${Tkind} \
            #     assi_Esprd_Tmean_reduc_Fmean_${Ephase}_${Tkind}

                                                # variance reduction
            Reduc \
                assi_Emean_Tstdd_${Ephase}_${Tkind} nature_Tstdd_${Tkind} \
                assi_Emean_Tstdd_reduc_${Ephase}_${Tkind}
      ${cfg[diag]} \
                assi_Emean_Tstdd_reduc_${Ephase}_${Tkind} \
                assi_Emean_Tstdd_reduc_${cfg[diag]}_${Ephase}_${Tkind}
            # Reduc \
            #     assi_Emean_Tstdd_${Ephase}_${Tkind} \
            #     free_Emean_Tstdd_prior_${Tkind} \
            #     assi_Emean_Tstdd_reduc_${Ephase}_${Tkind}
      # ${cfg[diag]} \
            #     assi_Emean_Tstdd_reduc_${Ephase}_${Tkind} \
            #     assi_Emean_Tstdd_reduc_${cfg[diag]}_${Ephase}_${Tkind}
            # Fmean \
            #     assi_Emean_Tstdd_reduc_${Ephase}_${Tkind} \
            #     assi_Emean_Tstdd_reduc_Fmean_${Ephase}_${Tkind}

            # Reduc \
            #     assi_Emean_Tvar_${Ephase}_${Tkind} \
            #     free_Emean_Tvar_prior_${Tkind} \
            #     assi_Emean_Tvar_reduc_${Ephase}_${Tkind}
            # Fmean \
            #     assi_Emean_Tvar_reduc_${Ephase}_${Tkind} \
            #     assi_Emean_Tvar_reduc_Fmean_${Ephase}_${Tkind}
        done
    done
}


plot_run(){
    funct_opening 1

    line_dir="../stats"

    echo " - Tkind-wise"

    for Tkind in Insta Taver Tanom; do
        echo "   - $Tkind"
        #-------------------------------------------------------------
        #  Plot definition
        plot_name  (){ echo "${Tkind}_Fstdd_stats"; }
        plot_title (){ echo ""; }
        line_label (){ echo "$(run_l $file) $(Quantity_l $file)"; }
        # line_label (){ echo "$file"; }
        case ${cfg[run_mode]} in
            "free")
                line_files=( \
                    $line_dir/free_error_Fstdd_prior_${Tkind}.nc \
                    $line_dir/free_Esprd_Fmean_prior_${Tkind}.nc );;
            "assi")
                line_files=( \
                    $line_dir/free_error_Fstdd_prior_${Tkind}.nc \
                    $line_dir/assi_error_Fstdd_prior_${Tkind}.nc \
                    $line_dir/assi_error_Fstdd_postr_${Tkind}.nc \
                    $line_dir/free_Esprd_Fmean_prior_${Tkind}.nc \
                    $line_dir/assi_Esprd_Fmean_prior_${Tkind}.nc \
                    $line_dir/assi_Esprd_Fmean_postr_${Tkind}.nc );;
        esac
        declare -a opts
        opts+=(min_level=0.0) #; opts+=(max_level=10.0) ;;
        opts+=($(printf '%s=\"%s\"' cdo_output yes))
        create_line_plot
        #-------------------------------------------------------------
    done

    echo " - Ephase-wise"

    case ${cfg[run_mode]} in
        "free") Ephases=(prior);;
        "assi") Ephases=(prior postr);;
    esac

    for Ephase in ${Ephases[@]}; do
        echo "   - $Ephase"

        #-------------------------------------------------------------
        #  Plot definition
        plot_name  (){ echo "${Ephase}_Fstdd_stats"; }
        plot_title (){ echo ""; }
        line_label (){ echo "$(Tkind_l "$file") $(Quantity_l "$file")"; }
        file_pattern="*${cfg[run_mode]}_*Fstdd*${Ephase}*.nc"
        line_files=($(find "$line_dir" -name "$file_pattern"|sort))
        declare -a opts
        opts+=($(printf '%s=\"%s\"' cdo_output yes))
        create_line_plot
        #-------------------------------------------------------------
    done

    funct_closing 1
}

plot_detailed_run_stats(){
    funct_opening 1

    line_dir="../stats_detailed"

    echo " - Tkind-wise"

    for Tkind in Insta Taver Tanom; do
        echo "   - $Tkind"

        #-------------------------------------------------------------
        #  Plot definition
        plot_name   (){ echo "${Tkind}_Fstdd_fluctuations"; }
        plot_title  (){ echo "Statistical fluctuations (%)"; }
        line_label  (){ echo "$(Ephase_l "$file") $(Quantity_l "$file")"; }

        case ${cfg[run_mode]} in
            free) file_pattern="*${cfg[run_mode]}_*Fmean*${Tkind}*fluct.nc" ;;
            assi) file_pattern="*${cfg[run_mode]}_*reduc*Fmean*${Tkind}*fluct.nc";;
        esac
        line_files=($(find "$line_dir" -name "$file_pattern"|sort))
        declare -a opts
        opts+=(min_level=0.0); opts+=(max_level=50.0)
        create_line_plot
        #-------------------------------------------------------------
    done

    echo " - Ephase-wise"

    case ${cfg[run_mode]} in
        "free") Ephases=(prior);;
        "assi") Ephases=(prior postr);;
    esac

    for Ephase in ${Ephases[@]}; do
        echo "   - $Ephase"

        #-------------------------------------------------------------
        #  Plot definition
        plot_name   (){ echo "${Ephase}_${cfg[diag]}_fluctuations"; }
        plot_title  (){ echo "Statistical fluctuations (%)"; }
        line_label  (){ echo "$(Tkind_l "$file") $(Quantity_l "$file")"; }
        case ${cfg[run_mode]} in
            free) file_pattern="*${cfg[run_mode]}_*${cfg[diag]}*${Ephase}*fluct.nc";;
            assi) file_pattern="*${cfg[run_mode]}_*reduc*${cfg[diag]}*${Ephase}*fluct.nc";;
        esac
        line_files=($(find "$line_dir" -name "$file_pattern"|sort))
        declare -a opts
        opts+=(min_level=0.0); opts+=(max_level=50.0)
        create_line_plot
        #-------------------------------------------------------------
    done

    funct_closing 1
}

#=======================================================
#> Postprocessing Operators
#=======================================================
#> Subtr  : Subtraction
#> Tmean  : Temporal mean value (without spinup period)
#> Tstdd  : Temporal std. dev.  (without spinup period)
#> Fmean  : Field mean value
#> Fstdd  : Field standard deviation
#> Reduc  : Stat reduction of A with reference to B
#=======================================================

Subtr(){
    local input_A=$1
    local input_B=$2
    local  output=$3
    my_cdo sub ${input_A}.nc ${input_B}.nc ${output}.nc
}

Tmean(){
    local  input=$1
    local output=$2
    my_cdo seltimestep,${cfg[spinup_cycles]}/$stat_cycles \
        ${input}.nc ${input}_seltimestep.nc
    my_cdo timmean ${input}_seltimestep.nc ${output}.nc
    rm ${input}_seltimestep.nc
}

Tstdd(){
    local  input=$1
    local output=$2
    my_cdo seltimestep,${cfg[spinup_cycles]}/$stat_cycles \
        ${input}.nc ${input}_seltimestep.nc
    my_cdo timstd1 ${input}_seltimestep.nc ${output}.nc
    rm ${input}_seltimestep.nc
}

Tvar(){
    local  input=$1
    local output=$2
    my_cdo seltimestep,${cfg[spinup_cycles]}/$stat_cycles \
        ${input}.nc ${input}_seltimestep.nc
    my_cdo timvar1 ${input}_seltimestep.nc ${output}.nc
    rm ${input}_seltimestep.nc
}

Fmean(){
    local  input=$1
    local output=$2
    my_cdo fldmean ${input}.nc ${input}_fldmean.nc
    my_cdo output ${input}_fldmean.nc > ${output}.dat
    rm ${input}_fldmean.nc
}

Fmean_nc(){
    local  input=$1
    local output=$2
    my_cdo fldmean ${input}.nc ${output}.nc
}

Fstdd(){
    local  input=$1
    local output=$2
    my_cdo fldstd1 ${input}.nc ${output}.nc
}

Reduc(){
    local stat_A=$1
    local stat_B=$2 #reference
    local stat_reduc=$3
    my_cdo sub ${stat_B}.nc ${stat_A}.nc ${stat_A}_sub.nc
    my_cdo div ${stat_A}_sub.nc ${stat_B}.nc ${stat_A}_div.nc
    my_cdo mulc,100.0 ${stat_A}_div.nc ${stat_reduc}.nc
    rm ${stat_A}_sub.nc ${stat_A}_div.nc
}

Xsel(){
    local  input=$1
    local output=$2
    my_cdo selindexbox,1,1,1,1 \
  ${input}.nc ${input}_Xsel.nc
    my_cdo output ${input}_Xsel.nc > ${output}.dat 
    rm ${input}_Xsel.nc
}


    # convergence_stats(){
    #     funct_opening 0
    #     local traject=$1
    #     cycles_res=5

    #     rm -f ${traject}_Tstdd_vs_run_length.nc
    #     rm -f ${traject}_Tstdd_Fmean_vs_run_length.nc
    #     cat > ${traject}_convergence_stats.ncl <<_EOF_
    #     load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
    #     load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
    #     load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
    #     begin

    #     ; Open input files
    #     traject_nc     = addfile ("${traject}.nc","r")
    #     Tstdd_nc       = addfile ("${traject}_Tstdd_vs_run_length.nc","c")
    #     Tstdd_Fmean_nc = addfile ("${traject}_Tstdd_Fmean_vs_run_length.nc","c")

    #     cycles_res        = $cycles_res
    #     n_comp            = ${cfg[n_comp]}
    #     run_length_array  = tolong(floor(ispan(1,cycles_res,1)*${cycles}/cycles_res))

    #     ; Calculate statistics

    #     do icomp = 1, n_comp
    #     var_name = "comp" + sprinti("%1.1i",icomp)
    #     var = traject_nc->\$var_name\$

    #     var_Tstdd                      = new((/cycles_res,$nc/),float)
    #     var_Tstdd!0                    = "run_length"
    #     var_Tstdd&run_length           =  tofloat(run_length_array)
    #     var_Tstdd&run_length@long_name = "run_length"

    #     do i_cycles = 0, cycles_res-1
    #     ; stdd along time direction
    #     var_Tstdd(i_cycles,:) = dim_stddev_n_Wrap(var(${spinup_cycles}-1:run_length_array(i_cycles)-1,:),0)
    #     end do

    #     ; - Overall
    #     var_Tstdd_Fmean= dim_avg_n_Wrap(var_Tstdd,1)
    #     print(var_Tstdd)
    #     print(var_Tstdd_Fmean)

    #     Tstdd_nc->\$var_name\$ = var_Tstdd
    #     Tstdd_Fmean_nc->\$var_name\$ = var_Tstdd_Fmean

    #     end do

    #     end
    #     exit
    #     _EOF_

    #     call_ncl ${traject}_convergence_stats.ncl
    #     ncl ${traject}_convergence_stats.ncl
    #     ddddd
    #     funct_closing 0
    # }

# #------------------------------------------------------
# #> @brief Create nature and free ensemble runs
# #------------------------------------------------------
# calc_run_free(){
#     funct_opening 1

#     # if [[ "$stop_at_nan" == "yes" ]]; then
#     #     launch_fortran_exe      sampling_run.exe && \
#     #         launch_fortran_exe        nature_run.exe && \
#     #         launch_fortran_exe ensemble_free_run.exe || error "run_crashed.cfg"
#     # else
#         # launch_fortran_exe          sampling_run.exe && \
#         #     launch_fortran_exe        nature_run.exe && \
#         #     launch_fortran_exe ensemble_free_run.exe || echo 1 > "run_crashed.cfg"
#     # fi
#     sampling_run.exe && nature_run.exe && ensemble_free_run.exe || echo 1 > "run_crashed.cfg"

#     funct_closing 1
# }

# #------------------------------------------------------
# #> @brief Create ensemble with data assimilation
# #------------------------------------------------------
# calc_run_assi(){
#     funct_opening 1

#     # run_free_dir=$reference_dir
#     # [[ -d "$run_free_dir" ]] || error "Non existent run_free_dir $run_free_dir"

#     # rel_run_free_dir=$(relpath $run_free_dir $dataset_dir )
#     # # set_par_file "run_free_dir"
#     # # set_par_file "rel_run_free_dir"
#     # add_to_cfg "run_free_dir"
#     # add_to_cfg "rel_run_free_dir"

#     # Bring necessary files
#     ln -s ../$rel_run_free_dir/raw_data/sampling.grd . # Initial conditions
#     ln -s ../$rel_run_free_dir/raw_data/nature_*.grd . # nature_run

#     # Run fortran code
#     # if [[ "$stop_at_nan" == "yes" ]]; then
#     #     launch_fortran_exe        nature_obs.exe && \
#     #         launch_fortran_exe ensemble_assi_run.exe || error "run_crashed.cfg"
#     # else
#     # launch_fortran_exe        nature_obs.exe && \
#     #     launch_fortran_exe ensemble_assi_run.exe || echo 1 > "run_crashed.cfg"
#     # fi
#     nature_obs.exe && ensemble_assi_run.exe || echo 1 > "run_crashed.cfg"

#     funct_closing 1
# }

# launch_fortran_exe(){

#     fortran_exe=$1
#     # env > ${fortran_exe}_env.log

#     coredump_Maxsize=${coredump_Maxsize:-0}
#     # coredump_Maxsize=${coredump_Maxsize:-"unlimited"}

#     echo ""
#     print_line 1
#     echo " - Running fortran bin $fortran_exe"
#     print_line 1

#     ulimit -s unlimited         # stack limit
#     ulimit -c $coredump_Maxsize # coredump size limit

# #    ./$fortran_exe || ret_code=$?
#     $fortran_exe 2>&1 || ret_code=$?

#     if [ ${ret_code:-0} != 0 ]; then
#         echo " $fortran_exe crashed with return code $ret_code" 1>&2
#         return $ret_code
#     fi
# }


