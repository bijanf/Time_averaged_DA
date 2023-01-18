#!/bin/bash

speedy2nc(){
        echo "******************************************"
        echo " ${FUNCNAME} START"
        echo "******************************************"

	#echo "=================================================="
	#echo "   Converter from Speedy .grd output to NetCDF"
	#echo "=================================================="
	#echo "                 ATTENTION:" 
	#echo "Do not use this script with leap year daily data!!"
	#echo "--------------------------------------------------"
	
	# Info: cdo import_binary command doesn't support the ctl option
	# 365_day_calendar. The simplest solution is to remove it.
	# !! This botched fix fails with leap year daily data
	# !! as CDO assumes gregorian calendar
	
	local fullname=$1
	 dirname=$(dirname $fullname); filename=$(basename $fullname .ctl)

	echo "Converting to NetCDF format the speedy binary files"
	echo "refered to by descriptor file ${filename}.ctl "
	
	cd $dirname
    calendar365days=$(grep '365_day_calendar' $fullname | wc -l)
	if [ $calendar365days -eq 1 ]; then
	   echo "Option 365_day_calendar present in the descriptor file"
	   cp ${filename}.ctl ${filename}_copy.ctl                                           || error_exit "Error copying descriptor file!"
	   sed -i 's/365_day_calendar//g' ${filename}_copy.ctl                               || error_exit "Error editing descriptor file!"
	   cdo -f nc import_binary ${filename}_copy.ctl ${filename}_copy.nc > /dev/null 2>&1 || error_exit "Error importing bynary file!"
	   cdo setcalendar,365days ${filename}_copy.nc ${filename}.nc > /dev/null 2>&1       || error_exit "Error setting new calendar"
	   rm ${filename}_copy.nc && rm ${filename}_copy.ctl                                 || error_exit "Error cleaning interim files"
	else
	   cdo -f nc import_binary ${filename}.ctl ${filename}.nc  > /dev/null           || error_exit "Error importing bynary file!"
	fi
	echo ' '
    echo "******************************************"
    echo " ${FUNCNAME} NORMAL END"
    echo "******************************************"

}

ncview_speedy_file(){
        echo "******************************************"
        echo " ${FUNCNAME} START"
        echo "******************************************"

	local descriptor_fullname=$1
	descriptor_dirname=$(dirname $descriptor_fullname)
	descriptor_filename=$(basename $descriptor_fullname .ctl)
	
	speedy2nc $descriptor_fullname #> /dev/null 2>&1 || error_exit "Error in speedyGrd2netcdf.sh!"
	#./speedyGrd2netcdf.sh $descriptor_file || error_exit "Error in speedyGrd2netcdf.sh!"
	
	cd ${descriptor_dirname}
	echo "Launching ncview..."
	ncview ${descriptor_filename}.nc > /dev/null 2>&1  || echo "ncview not gently closed"
	
	echo "Cleaning interim .nc files."
	rm ${descriptor_filename}.nc	
    echo "******************************************"
    echo " ${FUNCNAME} NORMAL END"
    echo "******************************************"
	
}

ncview_speedy(){
    echo "******************************************"
    echo " ${FUNCNAME} START"
    echo "******************************************"

    local fullname=$1
         shortname=$(basename $fullname)
	PROGNAME=$FUNCNAME
	if [[ -f $fullname ]]; then
	    echo "Plotting binary files described by $shortname"
	    ncview_speedy_file $fullname
	elif [[ -d $fullname ]]; then
	    echo "Plotting binary files of the folder $shortname"
	    binary_sets=$(find $fullname -name '*.ctl')
	    echo "Number of binary sets found: $(echo $binary_sets | wc -w)"
	    for binary_set in $binary_sets
	    do	      
#	      ncview_speedy_file $binary_set >/dev/null 2>&1 &
	      ncview_speedy_file $binary_set >/dev/null &
	    done
	    wait
	else
	    error_exit "$shortname is neither a file nor a folder"
	fi
    echo "******************************************"
    echo " ${FUNCNAME} NORMAL END"
    echo "******************************************"

}

plot_error(){
    echo "******************************************"
    echo " ${FUNCNAME} START"
    echo "******************************************"

    local field=$1 #anal or gues
    [ "$field" == "anal" ] || [ "$field" == "gues" ] || error_exit "Field '$field' is not valid!"

    mkdir -p ${TMP_DIR}/${field}
    speedy2nc ${NATURE_DIR}/yyyymmddhh.ctl
    mv ${NATURE_DIR}/yyyymmddhh.nc ${TMP_DIR}/${field}/nature.nc

    speedy2nc ${EXP_ARCH_DIR}/${field}/mean/yyyymmddhh.ctl
    mv ${EXP_ARCH_DIR}/${field}/mean/yyyymmddhh.nc ${TMP_DIR}/${field}/field.nc
    
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
    cdo chname,rain,rain_rmse -sqrt -fldmean -sqr -selname,rain misfit.nc rmse_rain.nc > /dev/null 2>&1
    ncatted -a long_name,rain_rmse,o,c,"Precipitation RMSE" rmse_rain.nc
    ncatted -O -a units,rain_rmse,c,c,"mm/6hr" rmse_rain.nc
    ncview rmse_rain.nc > /dev/null 2>&1  &
    echo "******************************************"
    echo " ${FUNCNAME} NORMAL END"
    echo "******************************************"

}
