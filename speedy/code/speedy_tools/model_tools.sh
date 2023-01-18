#!/usr/bin/env bash
#=======================================================================
#> @brief This function creates symbolic links to forcing fields
#> in work_dir folder (Adapted from inpfiles.s)
#=======================================================================
speedy_link_forcings(){
    [[ $# -eq 2 ]] || error "Usage: speedy_link_forcings res work_dir"
    res=$1 # resolution (t21, t30, ..)
    work_dir="$2"

    [[ $res == t30 ]] || error "Unsuported resolution $res"

    SPEEDY="$CODE_DIR/speedy_ver32"

    # Climatological fields
    SB="$SPEEDY/data/bc/${res}/clim"
    ln -fs        "$SB/orog_lsm_alb.${res}.grd" "$work_dir/fort.20"
    ln -fs    "$SB/sst_8190clim.${res}.sea.grd" "$work_dir/fort.21"
    ln -fs "$SB/seaice_8190clim.${res}.sea.grd" "$work_dir/fort.22"
    ln -fs   "$SB/skt_8190clim.${res}.land.grd" "$work_dir/fort.23"
    ln -fs "$SB/sndep_8190clim.${res}.land.grd" "$work_dir/fort.24"
    ln -fs          "$SB/veget.${res}.land.grd" "$work_dir/fort.25"
    ln -fs "$SB/soilw_8190clim.${res}.land.grd" "$work_dir/fort.26"

    # Anomaly fields
    SC="$SPEEDY/data/bc/${res}/anom"
    case "$SST" in
         "clim_81_90") ;;
         "79_90") 
             ln -fs  "$SC/sst_anom_7990.${res}.grd" \
                "$work_dir/fort.30" ;;
         "50_99") 
             ln -fs "$SC/sst_anom_1950_1999_mean8190.${res}.grd" \
                "$work_dir/fort.30" ;;
          "1854_2010")
             ln  -fs "$SC/noaa_anom_1854_2010_mean1979_2008.${1}.grd" \
                 "$work_dir/fort.30" ;;
                     
         ##"el_nino") 
             #ln -fs "$SC/elnino_anom_p1.${res}.grd" \
                #"$work_dir/fort.30" ;;
         *) error "Unknown SST_mode $SST_mode";;
    esac
    # ln -fs $SC/sst_anom_8190.${res}.sea.grd $work_dir/fort.30

    # Heatflux climatology for slab models
    if [[ "$surf_mode" == "slab_model" ]]; then
        ln -fs "${SPEEDY}/hflux/hflux_573_1981_1990_clim.grd" \
            "$work_dir/fort.31"
    fi
}

#=======================================================================
#> @brief Wrapper script to run SPEEDY executable at_gcm.exe
#> at_gcm.exe, restart and forcing files must be in the same folder.
#> Restart and forcing files can be also just soft links.
#> @note See @ref da_files for details
# ! da_files is at the bottom of this file
#=======================================================================
speedy_run(){
    [[ $# -eq 5 ]] || error "Usage: speedy_run istart output_flag time_start time_leap output_dir"

    istart=$1      # Input  restart files flag (info in com_tsteps.h)
    output_flag=$2 # Output restart files flag (info in com_tsteps.h)
    time_present=$3
    time_forecast=$4
    output_dir="$5"

    #time_forecast=$(time_increment.exe $time_present $time_leap)
    #time_leap=6
    #echo "$time_forecast" > time_forecast.dat

    # Setting restart mode and initial time
    echo $istart               > fort.2
    echo $output_flag         >> fort.2

    # Set initial time
    echo ${time_present:0:4}   > fort.7 # year
    echo ${time_present:4:2}  >> fort.7 # month
    echo ${time_present:6:2}  >> fort.7 # day
    echo ${time_present:8:2}  >> fort.7 # hour

    # Set final time
    echo ${time_forecast:0:4}  > fort.8 # year
    echo ${time_forecast:4:2} >> fort.8 # month
    echo ${time_forecast:6:2} >> fort.8 # day
    echo ${time_forecast:8:2} >> fort.8 # hour

    # Time leap info needed for ctl file creation
    #echo $time_leap > fort.9

    # Set initial conditions (restart files)
#     case "$istart" in
#         0|1|2) # Reference atmosphere|spectral Instantaneous|gridded Instantaneous
#             ;;
#         3)     # Time average decomposed
# #             ln -fs ${time_present}_grid_sigma_Taver.rst fort.92
# #             ln -fs ${time_present}_grid_sigma_Tanom.rst fort.93
#             ;;
#         *) error "Invalid istart value $istart";;
#     esac

    # Running speedy
    at_gcm.exe #> /dev/null

    [[ -f fort.11 ]] && mv fort.11 ${time_present}_monthly_means.rst
#     [[ -f fort.13 ]] && mv fort.13 ${time_forecast}_monthly_var_cov.rst    
#     [[ -f fort.15 ]] && mv fort.15 ${time_forecast}_monthly_diabaticheating.rst    
    # ./at_gcm.exe # > speedy.log

    # Storing final state
    [[ "$output_dir" != $(pwd) ]] && mv ${time_forecast}* $output_dir
    [[ "$output_dir" != $(pwd) ]] && mv ${time_present}_monthly_means.rst $output_dir
    #[[ "$output_dir" != "." ]] && mv ${time_forecast}* $output_dir

    # Renaming spectral restart file
    #[ $output_flag == 1 ] && mv fort.10 ${time_forecast}_spect_sigma.rst
    # if [ ! "$(pwd)" == "$output_dir" ]; then
##          if [ "$output_flag" == 2 ]; then
  ## Reference atmosphere|spectral Instantaneous|gridded Instantaneous
#mv ${time_forecast}_grid_sigma_Insta.rst ${output_dir}
 #mv ${time_forecast}_grid_press_Insta.rst ${output_dir}
##              mv ${time_forecast}_grid_sigma_Taver.rst ${output_dir}
##               mv ${time_forecast}_grid_sigma_Insta.ctl ${output_dir}
##          fi

#if [ "$output_flag" == 3 ] || [ "$output_flag" == 4 ]; then
  ## equal to 2 plus Gridded Sigma Time-averaged state| and Gridded Sigma Time-averaged anomaly
##              mv ${time_forecast}_grid_sigma_Insta.rst ${output_dir}
##                  mv ${time_forecast}_grid_press_Insta.rst ${output_dir}
 #mv ${time_forecast}_grid_sigma_Taver.rst ${output_dir}
 ## Anomaly is not moved to comply with the analysis cycle design
 ## Both input restarts must be in postr folder
 #fi
 ## ;;
##              *) error "Invalid output_flag value $output_flag";;
##          esac
#           mv ${time_forecast}*.ctl ${output_dir}
# mv ${time_forecast}* ${output_dir}

 # fi
 # Storing Spectraly filtered analysis fields (currently disabled)
 #mv ${time_present}_TA.grd ${archive_dir}/anal_f/${member}
 #mv ${time_present}_AN.grd ${archive_dir}/anal_f/${member}

    return 0 # Success
}

create_control_file(){
    funct_opening 1

    name_prefix=$1; Tkind=$2; time_start=$3;
    time_leap_value=$4; time_leap_unit=$5; target_dir=$6

    file_name=${name_prefix}_grid_sigma_${Tkind}

    echo " $file_name"
    
    description="$(Tkind_l $Tkind) Speedy Model Prognostic Variables"
    year=${time_start:0:4};  month=${time_start:4:2}
    day=${time_start:6:2};    hour=${time_start:8:2}
    months=( \
        'JAN' 'FEB' 'MAR' 'APR' 'MAY' 'JUN' \
        'JUL' 'AUG' 'SEP' 'OCT' 'NOV' 'DEC')

    time_leap_value=$(strip_leading_zeros $time_leap_value)
    time_leap_value=$(echo "print int($time_leap_value)"|python)
    time_leap_value_char=$(printf "%i" $time_leap_value)

    cat > $target_dir/${file_name}.ctl <<EOF
DSET ^%y4%m2%d2%h2_grid_sigma_${Tkind}.rst                             
TITLE $description
UNDEF -9.99E33
OPTIONS template big_endian
XDEF      96  LINEAR     0.000     3.750                                        
YDEF      48  LEVELS   -87.159   -83.479   -79.777   -76.070   -72.362   -68.652
   -64.942   -61.232   -57.521   -53.810   -50.099   -46.389   -42.678   -38.967
   -35.256   -31.545   -27.833   -24.122   -20.411   -16.700   -12.989    -9.278
    -5.567    -1.856     1.856     5.567     9.278    12.989    16.700    20.411
    24.122    27.833    31.545    35.256    38.967    42.678    46.389    50.099
    53.810    57.521    61.232    64.942    68.652    72.362    76.070    79.777
    83.479    87.159
ZDEF 7 LEVELS  0.950 0.835 0.685 0.510 0.340 0.200 0.080        
TDEF 9999 LINEAR ${hour}Z${day}${months[$((month-1))]}${year} ${time_leap_value_char}${time_leap_unit}
VARS 6
U    7 99 U-wind [m/s]
V    7 99 V-wind [m/s]
T    7 99 Temperature [K]
Q    7 99 Specific Humidity [kg/kg]
PS   0 99 Surface Pressure [Pa]
RAIN 0 99 Precipitation [mm/6hr]
ENDVARS
@ lev String long_name Sigma
EOF

    funct_closing 1
}

create_control_file_monthly_means(){
    funct_opening 1

    file_name=$1; time_start=$2; n_months=$3; target_dir=$4

    echo " $file_name"

    description="Monthly means of Speedy Model diagnostics"
    year=${time_start:0:4};  month=${time_start:4:2}
    day=${time_start:6:2};    hour=${time_start:8:2}
    months=( \
        'JAN' 'FEB' 'MAR' 'APR' 'MAY' 'JUN' \
        'JUL' 'AUG' 'SEP' 'OCT' 'NOV' 'DEC')

#     time_leap_value=$(strip_leading_zeros $time_leap_value)
#     time_leap_value=$(echo "print int($time_leap_value)"|python)
#     time_leap_value_char=$(printf "%i" $time_leap_value)

    cat > $target_dir/${file_name}.ctl <<EOF
DSET ${file_name}.rst                                            
TITLE $description
UNDEF   9.999E+19                                                               
OPTIONS SEQUENTIAL BIG_ENDIAN                                          
XDEF      96  LINEAR     0.000     3.750                                        
YDEF      48  LEVELS   -87.159   -83.479   -79.777   -76.070   -72.362   -68.652
   -64.942   -61.232   -57.521   -53.810   -50.099   -46.389   -42.678   -38.967
   -35.256   -31.545   -27.833   -24.122   -20.411   -16.700   -12.989    -9.278
    -5.567    -1.856     1.856     5.567     9.278    12.989    16.700    20.411
    24.122    27.833    31.545    35.256    38.967    42.678    46.389    50.099
    53.810    57.521    61.232    64.942    68.652    72.362    76.070    79.777
    83.479    87.159
ZDEF       7  LEVELS   925   850   700   500   300   200   100
TDEF ${n_months} LINEAR ${hour}Z${day}${months[$((month-1))]}${year} 1mo
VARS      17                                                                    
SP         0  99  surface pressure                [hPa]                         
MSLP       0  99  mean-sea-level pressure         [hPa]                         
ST         0  99  surface temperature            [degK]                         
SKINT      0  99  skin temperature               [degK]                         
SWAV       0  99  soil wetness availability         [%]                         
U0         0  99  near-surface u-wind             [m/s]                         
V0         0  99  near-surface v-wind             [m/s]                         
TEMP0      0  99  near-surface air temperature   [degK]                         
RH0        0  99  near-surface relative humidity    [%]                         
CLC        0  99  cloud cover (total)               [%]                         
CLTOP      0  99  pressure at cloud top           [hPa]                         
SHF        0  99  sensible heat flux      (uw.) [W/m^2]                         
TSR        0  99  top shortwave rad.      (dw.) [W/m^2]                         
SSR        0  99  surface shortwave rad.  (dw.) [W/m^2]                         
SLR        0  99  surface longwave rad.   (uw.) [W/m^2]                         
LSTA       0  99  land-surface temp. anomaly     [degK]                         
SSTA       0  99   sea-surface temp. anomaly     [degK]                         
ENDVARS                
EOF

    funct_closing 1
}

# create_control_file_monthly_means(){
# 
#     time_start=$1
#     time_final=$2
#     time_leap_value=$3
#     time_leap_unit=$4
#     target_dir=$5
# 
#     file_name=${time_start}_monthly_means
#     description="Monthly means of Speedy Model diagnostics"
#     year=${time_start:0:4};  month=${time_start:4:2}
#     day=${time_start:6:2};    hour=${time_start:8:2}
#     months=( \
#         'JAN' 'FEB' 'MAR' 'APR' 'MAY' 'JUN' \
#         'JUL' 'AUG' 'SEP' 'OCT' 'NOV' 'DEC')
# 
#     time_leap_value=$(strip_leading_zeros $time_leap_value)
#     time_leap_value=$(echo "print int($time_leap_value)"|python)
#     time_leap_value_char=$(printf "%i" $time_leap_value)
# 
#     cat > $target_dir/${file_name}.ctl <<EOF
# DSET ${file_name}.rst                                            
# TITLE $description
# UNDEF   9.999E+19                                                               
# OPTIONS SEQUENTIAL BIG_ENDIAN                                          
# XDEF      96  LINEAR     0.000     3.750                                        
# YDEF      48  LEVELS   -87.159   -83.479   -79.777   -76.070   -72.362   -68.652
#    -64.942   -61.232   -57.521   -53.810   -50.099   -46.389   -42.678   -38.967
#    -35.256   -31.545   -27.833   -24.122   -20.411   -16.700   -12.989    -9.278
#     -5.567    -1.856     1.856     5.567     9.278    12.989    16.700    20.411
#     24.122    27.833    31.545    35.256    38.967    42.678    46.389    50.099
#     53.810    57.521    61.232    64.942    68.652    72.362    76.070    79.777
#     83.479    87.159
# ZDEF       7  LEVELS   925   850   700   500   300   200   100
# TDEF ${run_length} LINEAR ${hour}Z${day}${months[$((month-1))]}${year} 1mo
# VARS      33                                                                    
# GH         7  99  geopotential height               [m]                         
# TEMP       7  99  abs. temperature               [degK]                         
# U          7  99  zonal (u) wind                  [m/s]                         
# V          7  99  meridional (v) wind             [m/s]                         
# Q          7  99  specific humidity              [g/Kg]                         
# RH         7  99  relative humidity                 [%]                         
# OMEGA      7  99  pressure vertical velocity     [Pa/s]                         
# PSI        7  99  streamfunction           [10^6 m^2/s]                         
# SP         0  99  surface pressure                [hPa]                         
# MSLP       0  99  mean-sea-level pressure         [hPa]                         
# ST         0  99  surface temperature            [degK]                         
# SKINT      0  99  skin temperature               [degK]                         
# SWAV       0  99  soil wetness availability         [%]                         
# U0         0  99  near-surface u-wind             [m/s]                         
# V0         0  99  near-surface v-wind             [m/s]                         
# TEMP0      0  99  near-surface air temperature   [degK]                         
# RH0        0  99  near-surface relative humidity    [%]                         
# CLC        0  99  cloud cover (total)               [%]                         
# CLTOP      0  99  pressure at cloud top           [hPa]                         
# SHF        0  99  sensible heat flux      (uw.) [W/m^2]                         
# TSR        0  99  top shortwave rad.      (dw.) [W/m^2]                         
# SSR        0  99  surface shortwave rad.  (dw.) [W/m^2]                         
# SLR        0  99  surface longwave rad.   (uw.) [W/m^2]                         
# LSTA       0  99  land-surface temp. anomaly     [degK]                         
# SSTA       0  99   sea-surface temp. anomaly     [degK]                         
# PRECLS     0  99  large-scale precipitation    [mm/day]                         
# PRECNV     0  99  convective precipitation     [mm/day]                         
# EVAP       0  99  evaporation                  [mm/day]                         
# OLR        0  99  outgoing longwave rad.  (uw.) [W/m^2]                         
# USTR       0  99  u-stress                (uw.) [N/m^2]                         
# VSTR       0  99  v-stress                (uw.) [N/m^2]                         
# LSHF       0  99  heat flux into land sfc (dw.) [W/m^2]                         
# SSHF       0  99  heat flux into  sea sfc (dw.) [W/m^2]                         
# ENDVARS                
# EOF
# }

speedy_build(){
    RES=t30          # Spectral resolution
    F77=gfortran          # Compiler to be used

    FFLAGS="-fdefault-real-8 -fno-align-commons -O3 -ffree-line-length-none -fconvert=big-endian"
    #FFLAGS="-fdefault-real-8 -O3 -ffree-line-length-none -fconvert=big-endian"
    DBG_FLAGS=""
#COMPILE=gfortran-4.4
#COMOTT1=-fdefault-real-8 -fno-align-commons -O3 -ffree-line-length-none
##COMOTT1=-fdefault-real-8 -fno-align-commons -O3 -march=native -ffast-math -funroll-loops
#COMCONV=-fconvert=big-endian
#COMLIB1=

    configure

    echo " Gathering code in bin folder"
    rm -rf $BIN_DIR; mkdir -p $BIN_DIR; cd $BIN_DIR

    echo " - Original model source files"
    find "$CODE_DIR/speedy_ver32/source" -maxdepth 1 -type f | xargs -I{} cp {} .
    mv par_horres_${RES}.h atparam.h
    mv par_verres.h        atparam1.h

    echo " - Parameter and namelist files"
    cp $CODE_DIR/speedy_ver32/ver32.input/cls_*.h .
    #cp $SPEEDY/model/ver32.input/inpfiles.s  $CA/
    #cp $SPEEDY/model/ver32.input/cls_*.h     $SPEEDY/model/input/exp_$2
    #cp $SPEEDY/model/ver32.input/inpfiles.s  $SPEEDY/model/input/exp_$2

    echo " - Modified model files"
    cp $CODE_DIR/speedy_ver32/update/* .

    echo " Compiling model"

    FILES=( \
        at_gcm.f \
        dyn_geop.f \
        dyn_grtend.f \
        dyn_implic.f \
        dyn_sptend.f \
        dyn_step.f \
        dyn_stloop.f \
        ini_impint.f \
        ini_indyns.f \
        ini_inforc.f \
        ini_iniall.f \
        ini_inphys.f \
        ini_inirdf.f \
        ini_invars.f \
        ini_stepone.f \
        phy_convmf.f \
        phy_fordate.f \
        phy_lscond.f \
        phy_phypar.f \
        phy_radiat.f \
        phy_shtorh.f \
        phy_suflux.f \
        phy_vdifsc.f \
        ppo_diagns.f \
        ppo_restart.f \
        ppo_setctl.f \
        ppo_setctl_daily.f \
        ppo_setgrd.f \
        ppo_tminc.f \
        ppo_tminc_daily.f \
        ppo_tmout.f \
        ppo_tmout_daily.f \
        ppo_iogrid.f \
        sfc_anomod.f \
        spe_matinv.f \
        spe_spectral.f \
        spe_subfft_fftpack.f \
        ta_vars.f \
        timeinc_6hr.f \
        ta_tools.f \
        ppo_set_ctl_grid.f)

    for FILE in ${FILES[@]}; do
        echo " - $FILE"
        $F77 $FFLAGS $DBG_FLAGS -c $FILE
    done

    names=(${FILES[@]%.*})
    for name in ${names[@]}; do
        objs=(${objs[@]:-} ${name}.o)
    done

    #echo " Linking programs"
    $F77 $FFLAGS $DBG_FLAGS -o at_gcm.exe ${objs[@]}

}

configure(){
    case "$(hostname)" in
        tux05|tux21|calc02|tux35)
            tux04_F90="gfortran"
            F90=$tux04_F90
            F77=gfortran
            # F77=gfortran-4.4
            case "$F90" in
                "gfortran")
                    F90_OPT="-O3 -fconvert=big-endian";;
                "ifort")
                    F90_OPT="-fast -O3 -convert big_endian";;
                *)
                    error "Unknown compiler $F90"
                    exit 1;;
            esac
            ;;
        calc01|calc03|calc04)
            tux04_F90="gfortran"
            F90=$tux04_F90
            F77=gfortran
            case "$F90" in
                "gfortran")
                    F90_OPT="-O3 -fconvert=big-endian";;
                "ifort")
                    F90_OPT="-fast -O3 -convert big_endian";;
                *)
                    error "Unknown compiler $F90"
                    exit 1;;
            esac
            ;;
        soroban|node???)
            module load blas/gcc/64/1
            F90=gfortran
            F77=gfortran
            case "$F90" in
                "gfortran")
                    F90_OPT="-O3 -fconvert=big-endian";;
                "ifort")
                    F90_OPT="-fast -O3 -convert big_endian";;
                *)
                    error "Unknown compiler $F90"
                    exit 1;;
            esac
            ;;
        *)
            error "unknown machine";;
    esac
}

# speedy_build(){
#     RES=t30          # Spectral resolution
#     F77=gfortran          # Compiler to be used

#     FFLAGS="-fdefault-real-8 -fno-align-commons -O3 -ffree-line-length-none -fconvert=big-endian"
#     #FFLAGS="-fdefault-real-8 -O3 -ffree-line-length-none -fconvert=big-endian"
#     DBG_FLAGS=""
# #COMPILE=gfortran-4.4
# #COMOTT1=-fdefault-real-8 -fno-align-commons -O3 -ffree-line-length-none
# ##COMOTT1=-fdefault-real-8 -fno-align-commons -O3 -march=native -ffast-math -funroll-loops
# #COMCONV=-fconvert=big-endian
# #COMLIB1=

#     configure

#     echo " Gathering code in bin folder"
#     rm -rf $BIN_DIR; mkdir -p $BIN_DIR; cd $BIN_DIR

#     echo " - Original model source files"
#     find "$SPEEDY/model/source" -maxdepth 1 -type f | xargs -I{} cp {} .
#     mv par_horres_${RES}.h atparam.h
#     mv par_verres.h        atparam1.h

#     echo " - Parameter and namelist files"
#     cp $SPEEDY/model/ver32.input/cls_*.h .
#     #cp $SPEEDY/model/ver32.input/inpfiles.s  $CA/
#     #cp $SPEEDY/model/ver32.input/cls_*.h     $SPEEDY/model/input/exp_$2
#     #cp $SPEEDY/model/ver32.input/inpfiles.s  $SPEEDY/model/input/exp_$2

#     echo " - Modified model files"
#     cp $SPEEDY/model/update/* .

#     echo " Compiling model"

#     FILES=( \
#         at_gcm.f \
#         dyn_geop.f \
#         dyn_grtend.f \
#         dyn_implic.f \
#         dyn_sptend.f \
#         dyn_step.f \
#         dyn_stloop.f \
#         ini_impint.f \
#         ini_indyns.f \
#         ini_inforc.f \
#         ini_iniall.f \
#         ini_inphys.f \
#         ini_inirdf.f \
#         ini_invars.f \
#         ini_stepone.f \
#         phy_convmf.f \
#         phy_fordate.f \
#         phy_lscond.f \
#         phy_phypar.f \
#         phy_radiat.f \
#         phy_shtorh.f \
#         phy_suflux.f \
#         phy_vdifsc.f \
#         ppo_diagns.f \
#         ppo_restart.f \
#         ppo_setctl.f \
#         ppo_setctl_daily.f \
#         ppo_setgrd.f \
#         ppo_tminc.f \
#         ppo_tminc_daily.f \
#         ppo_tmout.f \
#         ppo_tmout_daily.f \
#         ppo_iogrid.f \
#         sfc_anomod.f \
#         spe_matinv.f \
#         spe_spectral.f \
#         spe_subfft_fftpack.f \
#         ta_vars.f \
#         timeinc_6hr.f \
#         ta_tools.f \
#         ppo_set_ctl_grid.f)

#     for FILE in ${FILES[@]}; do
#         echo " - $FILE"
#         $F77 $FFLAGS $DBG_FLAGS -c $FILE
#     done

#     names=(${FILES[@]%.*})
#     for name in ${names[@]}; do
#         objs=(${objs[@]:-} ${name}.o)
#     done

#     #echo " Linking programs"
#     $F77 $FFLAGS $DBG_FLAGS -o at_gcm.exe ${objs[@]}

# }

# configure(){
#     case "$(hostname)" in
#         tux04|calc02)
#             tux04_F90="gfortran"
#             F90=$tux04_F90
#             F77=gfortran-4.4
#             case "$F90" in
#                 "gfortran")
#                     F90_OPT="-O3 -fconvert=big-endian";;
#                 "ifort")
#                     F90_OPT="-fast -O3 -convert big_endian";;
#                 *)
#                     error "Unknown compiler $F90"
#                     exit 1;;
#             esac
#             ;;
#         soroban|node???)
#             module load blas/gcc/64/1
#             F90=gfortran
#             F77=gfortran
#             case "$F90" in
#                 "gfortran")
#                     F90_OPT="-O3 -fconvert=big-endian";;
#                 "ifort")
#                     F90_OPT="-fast -O3 -convert big_endian";;
#                 *)
#                     error "Unknown compiler $F90"
#                     exit 1;;
#             esac
#             ;;
#         *)
#             error "unknown machine";;
#     esac
# }
