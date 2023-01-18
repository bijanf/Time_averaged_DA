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
    #     n_comp            = $n_comp
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
    #     var_Tstdd(i_cycles,:) = dim_stddev_n_Wrap(var(${nspinup}-1:run_length_array(i_cycles)-1,:),0)
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


    # ens_assi_skill(){
    #     funct_opening 0
    #     local ens_free_dir=$1
    #     local       Ephase=$2
    #     local        Tkind=$3
    #     cat > ens_assi_${Ephase}_${Tkind}_skill.ncl <<_EOF_
    #     load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
    #     load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
    #     load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
    #     begin

    #     ; Open netcdf files
    #     ;ens_free_error_Tstdd_nc = addfile("$ens_free_dir/stats/free_prior_error_${Tkind}_Tstdd.nc","r")
    #     ens_free_error_Tstdd_nc = addfile("free_prior_error_${Tkind}_Tstdd.nc","r")
    #     ens_assi_error_Tstdd_nc = addfile("assi_${Ephase}_error_${Tkind}_Tstdd.nc","r")
    #     RMSSS_nc = addfile("assi_${Ephase}_${Tkind}_RMSSS.nc"      ,"c")

    #     ; Calculate skill
    #     n_comp = $n_comp
    #     RMSSS_Fmean = new((/$n_comp/),float)

    #     do icomp = 1, n_comp
    #     var_name = "comp" + sprinti("%1.1i",icomp)

    #     ens_free_error_Tstdd = ens_free_error_Tstdd_nc->\$var_name\$
    #     ens_assi_error_Tstdd = ens_assi_error_Tstdd_nc->\$var_name\$

    #     ones = new((/dimsizes(ens_assi_error_Tstdd)/),float)
    #     ones = 1

    #     RMSSS = ens_assi_error_Tstdd  ; just to get metainfo!!
    #     RMSSS = ones - (ens_assi_error_Tstdd/ens_free_error_Tstdd)
    #     RMSSS_nc->\$var_name\$ = RMSSS

    #     RMSSS_Fmean(icomp-1) = dim_avg_n_Wrap(RMSSS,0)
    #     end do

    #     asciiwrite("assi_${Ephase}_${Tkind}_RMSSS_Fmean.dat",RMSSS_Fmean)

    #     end
    #     exit
    #     _EOF_
    #     call_ncl ens_assi_${Ephase}_${Tkind}_skill.ncl
    #     funct_closing 0
    # }

# trajectory_stats(){
#     funct_opening 0
#     local traject=$1

#     if [[ "$detailed_stats" == "yes" ]]; then
#         cat > ${traject}_stats.ncl <<_EOF_
#         load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
#         load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
#         load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
#         begin

#         ; Open input files
#         traject_nc = addfile ("${traject}.nc","r")
#         Fmean_nc = addfile ("${traject}_Fmean.nc","c")
#         Xstdd_nc = addfile ("${traject}_Xstdd.nc","c")
#         Tmean_nc = addfile ("${traject}_Tmean.nc","c")
#         Tstdd_nc = addfile ("${traject}_Tstdd.nc","c")

#         ; Calculate statistics
#         n_comp = $n_comp
#         var_Tmean_Fmean = new((/$n_comp/),float)
#         var_Tstdd_Fmean = new((/$n_comp/),float)

#         do icomp = 1, n_comp
#         var_name = "comp" + sprinti("%1.1i",icomp)
#         var = traject_nc->\$var_name\$

#         ; - along x direction
#         var_Fmean = dim_avg_n_Wrap   (var,1)
#         var_Xstdd = dim_stddev_n_Wrap(var,1)
#         Fmean_nc->\$var_name\$ = var_Fmean
#         Xstdd_nc->\$var_name\$ = var_Xstdd

#         ; - along time direction
#         var_Tmean = dim_avg_n_Wrap   (var(${nspinup}:,:),0)
#         var_Tstdd = dim_stddev_n_Wrap(var(${nspinup}:,:),0)
#         Tmean_nc->\$var_name\$ = var_Tmean
#         Tstdd_nc->\$var_name\$ = var_Tstdd

#         ; - Overall
#         var_Tstdd_Fmean(icomp-1) = dim_avg_n_Wrap(var_Tstdd,0)
#         var_Tmean_Fmean(icomp-1) = dim_avg_n_Wrap(var_Tmean,0)
#         end do

#         asciiwrite("${traject}_Tmean_Fmean.dat",var_Tmean_Fmean)
#         asciiwrite("${traject}_Tstdd_Fmean.dat",var_Tstdd_Fmean)

#         end
#         exit
#         _EOF_
#     else
#         cat > ${traject}_stats.ncl <<_EOF_
#         load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
#         load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
#         load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
#         begin

#         ; Open input files
#         traject_nc = addfile ("${traject}.nc","r")
#         Tstdd_nc = addfile ("${traject}_Tstdd.nc","c")

#         ; Calculate statistics
#         n_comp = $n_comp
#         var_Tmean_Fmean = new((/$n_comp/),float)
#         var_Tstdd_Fmean = new((/$n_comp/),float)

#         do icomp = 1, n_comp
#         var_name = "comp" + sprinti("%1.1i",icomp)
#         var = traject_nc->\$var_name\$

#         ; - along time direction
#         var_Tstdd = dim_stddev_n_Wrap(var(${nspinup}:,:),0)
#         Tstdd_nc->\$var_name\$ = var_Tstdd

#         ; - Overall
#         var_Tstdd_Fmean(icomp-1) = dim_avg_n_Wrap(var_Tstdd,0)
#         end do

#         asciiwrite("${traject}_Tstdd_Fmean.dat",var_Tstdd_Fmean)

#         end
#         exit
#         _EOF_
#     fi

#     call_ncl ${traject}_stats.ncl
#     funct_closing 0
# }
