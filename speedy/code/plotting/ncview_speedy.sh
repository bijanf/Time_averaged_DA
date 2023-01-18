#!/usr/bin/env bash
#=======================================================================
#> @brief Script to Plot SPEEDY binary files with ncview
#=======================================================================
ncview_speedy(){
    [[ $# -eq 1 ]] || error "Usage: ncview_speedy ctl_path"
    ctl_path=$1 # ctl file or folder with ctl files
    
    # if ctl_path is a relative path
    if ! [ "$ctl_path" == "/*" ]; then
	there=$(pwd)
	ctl_path="${there}/${ctl_path}"
    fi

    ctl_name=$(basename $ctl_path)

    if [[ -f $ctl_path ]]; then
        echo "Plotting binary files described by $ctl_name"
	ncview_speedy_file ${ctl_path}
    elif [[ -d $ctl_path ]]; then
        echo "Plotting binary files of the folder $ctl_name"
	binary_sets=$(find $ctl_path -name '*.ctl')
	echo "Number of binary sets found: $(echo $binary_sets | wc -w)"
	for binary_set in $binary_sets
	do
	    ncview_speedy_file $binary_set >/dev/null &
	done
	wait
    else
	error "$ctl_name is neither a file nor a folder"
    fi
}

ncview_speedy_file(){
    [[ $# -eq 1 ]] || error "Usage: ncview_speedy_file ncview_speedy ctl_path"
    local ctl_path=$1 # Grads control file absolute path

           ctl_dir=$(dirname $ctl_path)
    local ctl_name=$(basename $ctl_path .ctl)
    
    speedy2nc.sh $ctl_path > /dev/null || error "Unsuccesful NetCDF conversion."
    
    cd ${ctl_dir}
    echo "Launching ncview..."
    ncview ${ctl_name}.nc > /dev/null || echo "ncview not gently closed"
    
    echo "Cleaning interim .nc files."
    rm ${ctl_name}.nc	
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

ncview_speedy $@
exit $?
