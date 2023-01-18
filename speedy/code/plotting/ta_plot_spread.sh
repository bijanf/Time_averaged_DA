#!/usr/bin/env bash
#=======================================================================
#> @brief Add info
#=======================================================================
ta_plot_spread()
{
    funct_opening

    local ensemble_run_dir=$1  # $FREE_RUN_DIR or $ASSIM_RUN_DIR
    local            field=$2  # anal or gues
    
    [ "$field" == "anal" ] || [ "$field" == "guess" ] || error_exit "Field '$field' is not valid!"

    mkdir -p ${TMP_DIR}/${field}

    speedy2nc ${ensemble_run_dir}/${field}/sprd/yyyymmddhh_TA.ctl
    mv ${ensemble_run_dir}/${field}/sprd/yyyymmddhh_TA.nc ${TMP_DIR}/${field}/spread.nc

    cd ${TMP_DIR}/${field}
    ncview spread.nc > /dev/null 2>&1 &

    # temperature
    cdo chname,t,t_spread -fldmean -sellevel,0.95 -selname,t spread.nc spread_t_sig095.nc > /dev/null 2>&1
    ncatted -a long_name,t_spread,o,c,"Temperature spread at sigma=0.95" spread_t_sig095.nc
    ncatted -O -a units,t_spread,c,c,"K" spread_t_sig095.nc
    ncview spread_t_sig095.nc > /dev/null 2>&1 &
    # zonal wind
    cdo chname,u,u_spread -fldmean -sellevel,0.51 -selname,u spread.nc spread_u_sig051.nc > /dev/null 2>&1
    ncatted -a long_name,u_spread,o,c,"U-wind spread at sigma=0.51" spread_u_sig051.nc
    ncatted -O -a units,u_spread,c,c,"m/s" spread_u_sig051.nc
    ncview spread_u_sig051.nc > /dev/null 2>&1 &
    # meridional wind
    cdo chname,v,v_spread -fldmean -sellevel,0.51 -selname,v spread.nc spread_v_sig051.nc > /dev/null 2>&1
    ncatted -a long_name,v_spread,o,c,"V-wind spread at sigma=0.51" spread_v_sig051.nc
    ncatted -O -a units,v_spread,c,c,"m/s" spread_v_sig051.nc
    ncview spread_v_sig051.nc > /dev/null 2>&1 &
    # specific humidity
    cdo chname,q,q_spread -fldmean -sellevel,0.51 -selname,q spread.nc spread_q_sig051.nc > /dev/null 2>&1
    ncatted -a long_name,q_spread,o,c,"Specific humidity spread at sigma=0.51" spread_q_sig051.nc
    ncatted -O -a units,q_spread,c,c,"kg/kg" spread_q_sig051.nc
    ncview spread_q_sig051.nc > /dev/null 2>&1 &
    # Surface pressure
    cdo chname,ps,ps_spread -fldmean -selname,ps   spread.nc spread_ps.nc > /dev/null 2>&1
    ncatted -a long_name,ps_spread,o,c,"Surface pressure spread" spread_ps.nc
    ncatted -O -a units,ps_spread,c,c,"Pa" spread_ps.nc
    ncview spread_ps.nc > /dev/null 2>&1  &
    # Precipitation
	#    cdo chname,rain,rain_spread -fldmean -selname,rain spread.nc spread_rain.nc > /dev/null 2>&1
	#    ncatted -a long_name,rain_spread,o,c,"Precipitation spread" spread_rain.nc
	#    ncatted -O -a units,rain_spread,c,c,"mm/6hr" spread_rain.nc
	#    ncview spread_rain.nc > /dev/null 2>&1  &

    funct_closing
}
set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

ta_plot_spread $@

exit $?
