#!/usr/bin/env bash
#=======================================================================
#> @brief Add info
#=======================================================================
ta_plot_error(){
#    funct_opening

    local ensemble_run_dir=$1  # $FREE_RUN_DIR or $ASSIM_RUN_DIR
    local            field=$2  # anal or gues
    
    [ "$field" == "anal" ] || [ "$field" == "guess" ] || error_exit "Field '$field' is not valid!"

    mkdir -p ${TMP_DIR}/${field}

    speedy2nc ${NATURE_DIR}/yyyymmddhh_TA.ctl
    mv ${NATURE_DIR}/yyyymmddhh_TA.nc ${TMP_DIR}/${field}/nature.nc

    speedy2nc ${ensemble_run_dir}/${field}/mean/yyyymmddhh_TA.ctl
    mv ${ensemble_run_dir}/${field}/mean/yyyymmddhh_TA.nc ${TMP_DIR}/${field}/field.nc

    cd ${TMP_DIR}/${field}
    cdo sub nature.nc field.nc misfit.nc > /dev/null
    #ncview misfit.nc > /dev/null 2>&1 &

    # temperature
    cdo chname,t,t_rmse -sqrt -fldmean -sqr -sellevel,0.95 -selname,t misfit.nc rmse_t_sig095.nc > /dev/null 2>&1
    ncatted -a long_name,t_rmse,o,c,"Temperature RMSE at sigma=0.95" rmse_t_sig095.nc
    ncatted -O -a units,t_rmse,c,c,"K" rmse_t_sig095.nc
    ncview rmse_t_sig095.nc > /dev/null 2>&1 &
    # zonal wind
    cdo chname,u,u_rmse -sqrt -fldmean -sqr -sellevel,0.51 -selname,u misfit.nc rmse_u_sig051.nc > /dev/null 2>&1
    ncatted -a long_name,u_rmse,o,c,"U-wind RMSE at sigma=0.51" rmse_u_sig051.nc
    ncatted -O -a units,u_rmse,c,c,"m/s" rmse_u_sig051.nc
    ncview rmse_u_sig051.nc > /dev/null 2>&1 &
    # meridional wind
    cdo chname,v,v_rmse -sqrt -fldmean -sqr -sellevel,0.51 -selname,v misfit.nc rmse_v_sig051.nc > /dev/null 2>&1
    ncatted -a long_name,v_rmse,o,c,"V-wind RMSE at sigma=0.51" rmse_v_sig051.nc
    ncatted -O -a units,v_rmse,c,c,"m/s" rmse_v_sig051.nc
    ncview rmse_v_sig051.nc > /dev/null 2>&1 &
    # specific humidity
    cdo chname,q,q_rmse -sqrt -fldmean -sqr -sellevel,0.51 -selname,q misfit.nc rmse_q_sig051.nc > /dev/null 2>&1
    ncatted -a long_name,q_rmse,o,c,"Specific humidity RMSE at sigma=0.51" rmse_q_sig051.nc
    ncatted -O -a units,q_rmse,c,c,"kg/kg" rmse_q_sig051.nc
    ncview rmse_q_sig051.nc > /dev/null 2>&1 &
    # Surface pressure
    cdo chname,ps,ps_rmse -sqrt -fldmean -sqr -selname,ps   misfit.nc rmse_ps.nc > /dev/null 2>&1
    ncatted -a long_name,ps_rmse,o,c,"Surface pressure RMSE" rmse_ps.nc
    ncatted -O -a units,ps_rmse,c,c,"Pa" rmse_ps.nc
    ncview rmse_ps.nc > /dev/null 2>&1  &
    # Precipitation
#    cdo chname,rain,rain_rmse -sqrt -fldmean -sqr -selname,rain misfit.nc rmse_rain.nc > /dev/null 2>&1
#    ncatted -a long_name,rain_rmse,o,c,"Precipitation RMSE" rmse_rain.nc
#    ncatted -O -a units,rain_rmse,c,c,"mm/6hr" rmse_rain.nc
#    ncview rmse_rain.nc > /dev/null 2>&1  &

#    funct_closing
}
set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

ta_plot_error $@

exit $?
