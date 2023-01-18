#!/usr/bin/env bash
#=======================================================================
#> @brief Convert Speedy model binary restart file to NetCDF format"
#=======================================================================
speedy2nc(){
	[[ $# -eq 1 ]] || error "Usage: speedy2nc ctl_path"
	ctl_path=$1 # ctl file (relative or absolute)

    # if ctl_path is a relative path make it absolute
    if ! [ "$ctl_path" == "/*" ]; then
		there=$(pwd)
		ctl_path="${there}/${ctl_path}"
    fi

	 ctl_dir=$(dirname $ctl_path); 
	ctl_name=$(basename $ctl_path .ctl)

	echo "Converting to NetCDF format the speedy binary files"
	echo "refered to by descriptor file ${ctl_name}.ctl "
	
	cd $ctl_dir
	
	# Making sure that templating is on
    replace "OPTIONS" "OPTIONS template big_endian" ${ctl_name}.ctl

    echo " - Importing bynary file"
	cdo -f nc import_binary ${ctl_name}.ctl ${ctl_name}.nc  # > /dev/null

	#-------------------------------------------------------------------
	# Old setting for Speedy v41 binary restart files
	# In this version a 365 day calendar is used, which is not supported
	# by cdo import_binary command. The simplest solution was removing 
	# the option 365_day_calendar in the ctl file, however this patch
	# FAILS with leap year daily data as CDO assumes gregorian calendar
	# if no calendar option is present in the ctl file.
	#-------------------------------------------------------------------
    #calendar365days=$(grep '365_day_calendar' $ctl_path | wc -l)
	#if [ $calendar365days -eq 1 ]; then
	   #echo "Option 365_day_calendar present in the descriptor file"
	   #cp ${ctl_name}.ctl ${ctl_name}_copy.ctl                                           || error "Error copying descriptor file!"
##	   sed -i 's/365_day_calendar//g' ${ctl_name}_copy.ctl                               || error "Error editing descriptor file!"
	   ##cdo -f nc import_binary ${ctl_name}_copy.ctl ${ctl_name}_copy.nc > /dev/null 2>&1 || error "Error importing bynary file!"
	   #cdo -f nc import_binary ${ctl_name}_copy.ctl ${ctl_name}_copy.nc > /dev/null || error "Error importing bynary file!"
	   #cdo setcalendar,360days ${ctl_name}_copy.nc ${ctl_name}.nc > /dev/null 2>&1       || error "Error setting new calendar"
	   #rm ${ctl_name}_copy.nc && rm ${ctl_name}_copy.ctl                                 || error "Error cleaning interim files"
	#else
	   #cdo -f nc import_binary ${ctl_name}.ctl ${ctl_name}.nc  > /dev/null               || error "Error importing bynary file!"
	#fi
	#-------------------------------------------------------------------
#    funct_closing
}

#------------------------------------------------------
#> @brief Find the line starting with string1 in file
#>       and replace it (the whole line) with string2
#------------------------------------------------------
function replace(){
    [[ $# -eq 3 ]] || error "Usage: replace string1 string2 file"
    sed -i "s/$(echo $1 | sed -e 's/\([[\/.*]\|\]\)/\\&/g').*/$(echo $2 | sed -e 's/[\/&]/\\&/g')/g" "$3"
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

speedy2nc $@
exit $?
