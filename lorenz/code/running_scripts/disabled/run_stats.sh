#!/usr/bin/env bash
#=======================================================================
#> @brief Do statistical analysis of a run
#=======================================================================
run_stats(){
    funct_opening 2
    dataset_dir=$1; dataset_dir="$(absolute_path "$dataset_dir")"

    cd $dataset_dir

    # read model < $dataset_dir/config/model_name.cfg
    # source $CODE_DIR/$model/model_plots.sh
    #export_4byte_binaries_as_netcdf

    read run_mode      < $dataset_dir/config/run_mode.cfg
    read n_comp        < $dataset_dir/config/n_comp.cfg
    read spinup_cycles < $dataset_dir/config/nspinup.cfg
    
    rm -rf stats; mkdir stats; cd stats
    find ../raw_data/ -name "*.nc" -print0 | xargs -0 -I{} ln -s {} .

    case $run_mode in
        "free") run_free_stats $@ ;;
        "assi") run_assi_stats $@ ;;
        *     ) error "unsupported run_mode $run_mode";;
    esac

    print_statistics

#    find . -maxdepth 1 -type l | xargs rm -f # delete soft links

    funct_closing 2
}

run_free_stats(){
    funct_opening 1

    ncdiff nature_Insta.nc nature_Taver.nc nature_Tanom.nc
    trajectory_stats "nature_Tanom"

#    overall_min_max nature_Insta_all
    #ncra -y min nature_Insta_all.nc nature_Insta_min.nc
    #ncra -y max nature_Insta_all.nc nature_Insta_max.nc

    ncdiff free_prior_Emean_Insta.nc free_prior_Emean_Taver.nc free_prior_Emean_Tanom.nc
    trajectory_stats "free_prior_Emean_Tanom"

    for Tkind in Insta Taver; do
        trajectory_stats "nature_${Tkind}"
        trajectory_stats "free_prior_Emean_${Tkind}"
        ncdiff "nature_${Tkind}.nc" "free_prior_Emean_${Tkind}.nc" "free_prior_error_${Tkind}.nc"
        trajectory_stats "free_prior_error_${Tkind}"
    done
    funct_closing 1
}

run_assi_stats(){
    funct_opening 1

    read rel_run_free_dir < $dataset_dir/config/rel_run_free_dir.cfg

    find ../$rel_run_free_dir/stats/ -name "*.nc" \
	-print0 | xargs -0 -I{} ln -s {} .
    # ln -s $rel_run_free_dir/raw_data/nature*.nc .
    # ln -s $rel_run_free_dir/*error_Tstdd.nc .

    for Ephase in prior postr; do
        ncdiff assi_${Ephase}_Emean_Insta.nc assi_${Ephase}_Emean_Taver.nc assi_${Ephase}_Emean_Tanom.nc
        trajectory_stats "assi_${Ephase}_Emean_Tanom"

        for Tkind in Insta Taver; do
            trajectory_stats "assi_${Ephase}_Emean_${Tkind}"
            ncdiff "nature_${Tkind}.nc" "assi_${Ephase}_Emean_${Tkind}.nc" "assi_${Ephase}_error_${Tkind}.nc"
            trajectory_stats "assi_${Ephase}_error_${Tkind}"
            ens_assi_skill "$rel_run_free_dir" $Ephase $Tkind
        done
    done

    funct_closing 1
}

#======================================================
#> Operators along x dimension
#> Xmean : mean value
#> Xstd  : standard deviation
#> Operators along x dimension (without spinup period)
#> Tmean : mean value
#> Tstd  : standard deviation
#======================================================
trajectory_stats(){
    funct_opening 0
    local traject=$1
    cat > ${traject}_stats.ncl <<_EOF_
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
begin

; Open input files
traject_nc = addfile ("${traject}.nc","r")
  Xmean_nc = addfile ("${traject}_Xmean.nc","c")
  Xstdd_nc = addfile ("${traject}_Xstdd.nc","c")
  Tmean_nc = addfile ("${traject}_Tmean.nc","c")
  Tstdd_nc = addfile ("${traject}_Tstdd.nc","c")

; Calculate statistics
n_comp = $n_comp
var_Tmean_Xmean = new((/$n_comp/),float)
var_Tstdd_Xmean = new((/$n_comp/),float)

do icomp = 1, n_comp
    var_name = "comp" + sprinti("%1.1i",icomp)
    var = traject_nc->\$var_name\$

    ; - along x direction
    var_Xmean = dim_avg_n_Wrap   (var,1)
    var_Xstdd = dim_stddev_n_Wrap(var,1)
    Xmean_nc->\$var_name\$ = var_Xmean
    Xstdd_nc->\$var_name\$ = var_Xstdd

    ; - along time direction
    var_Tmean = dim_avg_n_Wrap   (var(${spinup_cycles}:,:),0)
    var_Tstdd = dim_stddev_n_Wrap(var(${spinup_cycles}:,:),0)
    Tmean_nc->\$var_name\$ = var_Tmean
    Tstdd_nc->\$var_name\$ = var_Tstdd

    ; - Overall
    var_Tstdd_Xmean(icomp-1) = dim_avg_n_Wrap(var_Tstdd,0)
    var_Tmean_Xmean(icomp-1) = dim_avg_n_Wrap(var_Tmean,0)
end do

asciiwrite("${traject}_Tmean_Xmean.dat",var_Tmean_Xmean)
asciiwrite("${traject}_Tstdd_Xmean.dat",var_Tstdd_Xmean)

end
exit
_EOF_
    call_ncl ${traject}_stats.ncl
    funct_closing 0
}

overall_min_max(){
    funct_opening 0
    local traject=$1
    cat > ${traject}_min_max.ncl <<_EOF_
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
begin

; Open input files
traject_nc = addfile ("${traject}.nc","r")

; Calculate statistics
n_comp = $n_comp
var_min = new((/$n_comp/),float)
var_max = new((/$n_comp/),float)

do icomp = 1, n_comp
    var_name = "comp" + sprinti("%1.1i",icomp)
    var = traject_nc->\$var_name\$

    var_min(icomp-1) = min(var)
    var_max(icomp-1) = max(var)
end do

asciiwrite("${traject}_min.dat",var_min)
asciiwrite("${traject}_max.dat",var_max)

end
exit
_EOF_
    call_ncl ${traject}_min_max.ncl
    funct_closing 0
}


ens_assi_skill(){
    funct_opening 0
    local ens_free_dir=$1
    local       Ephase=$2
    local        Tkind=$3
    cat > ens_assi_${Ephase}_${Tkind}_skill.ncl <<_EOF_
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
begin

; Open netcdf files
;ens_free_error_Tstdd_nc = addfile("$ens_free_dir/stats/free_prior_error_${Tkind}_Tstdd.nc","r")
ens_free_error_Tstdd_nc = addfile("free_prior_error_${Tkind}_Tstdd.nc","r")
ens_assi_error_Tstdd_nc = addfile("assi_${Ephase}_error_${Tkind}_Tstdd.nc","r")
               RMSSS_nc = addfile("assi_${Ephase}_${Tkind}_RMSSS.nc"      ,"c")

; Calculate skill
n_comp = $n_comp
RMSSS_Xmean = new((/$n_comp/),float)

do icomp = 1, n_comp
   var_name = "comp" + sprinti("%1.1i",icomp)

   ens_free_error_Tstdd = ens_free_error_Tstdd_nc->\$var_name\$
   ens_assi_error_Tstdd = ens_assi_error_Tstdd_nc->\$var_name\$

   ones = new((/dimsizes(ens_assi_error_Tstdd)/),float)
   ones = 1

   RMSSS = ens_assi_error_Tstdd  ; just to get metainfo!!
   RMSSS = ones - (ens_assi_error_Tstdd/ens_free_error_Tstdd)
   RMSSS_nc->\$var_name\$ = RMSSS

   RMSSS_Xmean(icomp-1) = dim_avg_n_Wrap(RMSSS,0)
end do

asciiwrite("assi_${Ephase}_${Tkind}_RMSSS_Xmean.dat",RMSSS_Xmean)

end
exit
_EOF_
    call_ncl ens_assi_${Ephase}_${Tkind}_skill.ncl
    funct_closing 0
}

#========================================================
#> @brief Display all Overall statistics files (*.dat)
#> present in the current folder
#========================================================
print_statistics(){
    echo "----------------------------------------------------"
    echo " STATISTIC                          | Comp1 | Comp2"
    echo "----------------------------------------------------"
    ls *.dat | while read stat_name; do
        readarray -t stat < $stat_name
#        echo " ${stat_name%.*}"
        printf ' %34s %7.3f %7.3f\n' "${stat_name%.*}" "${stat[@]}"
    done
    echo "----------------------------------------------------"
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source ${COM_DAS_DIR}/common_das_tools.sh

run_stats $@
exit $?
