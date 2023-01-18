#!/usr/bin/env bash


create_netcdf_from_lists(){
    case "$#" in
        2) create_netcdf_scalar_vs_par $@ ;;
        3) create_netcdf_scalar_vs_par $@ ;;
        *) error "Unsupported number of parameters $(( $# - 1))";;
    esac
}

# #=======================================================================
# #> @brief Create netcdf file from lists
# #=======================================================================
# function create_netcdf_scalar_vs_par(){
#     # funct_opening 1
#     local scalar_name=$1
#     local    par_name=$2


#     figure_name="${scalar_name}_vs_${par_name}"
#     rm -f ${figure_name}.nc
#     cat > ${figure_name}.ncl <<_EOF_
# load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
# load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
# load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
# begin
# ; - Read in data
# scalar = readAsciiTable("${scalar_name}.list",1,"float",0)
# par    = readAsciiTable(   "${par_name}.list",1,"float",0)
# ; - Write as netcdf
# f_out  = addfile("${figure_name}.nc","c")
# scalar!0              = "${par_name}"
# scalar&${par_name}    =    par(:,0)
# f_out->${scalar_name} = scalar
# end
# _EOF_
#     call_ncl ${figure_name}.ncl
#     echo "${figure_name}.nc created"
#     # funct_closing 1
# }

# #=======================================================================
# #> @brief Create netcdf file from lists
# #=======================================================================
# function create_netcdf_scalar_vs_par1_par2(){
#     # funct_opening 1
#     local scalar_name=$1
#     local   par1_name=$2
#     local   par2_name=$2

#     figure_name="${scalar_name}_vs_${par1_name}_${par2_name}"
#     rm -f ${figure_name}.nc
#     cat > ${figure_name}.ncl <<_EOF_
# load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
# load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
# load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
# begin
# ; - Read in data
# scalar = readAsciiTable("list_${scalar_name}.dat",1,"float",0)
# ;par1   = readAsciiTable(  "list_${par1_name}.dat",1,"float",0)
# ;par2   = readAsciiTable(  "list_${par2_name}.dat",1,"float",0)
# par1   = readAsciiTable(  "list_par1_value_n.dat",1,"float",0)
# par2   = readAsciiTable(  "list_par2_value_n.dat",1,"float",0)

# par1_value_n_list.cfg
# ; - Write as netcdf
# f_out  = addfile("${figure_name}.nc","c")
# scalar!0              = "${par_name}"
# scalar&${par_name}    =    par(:,0)
# f_out->${scalar_name} = scalar
# end
# _EOF_
#     call_ncl ${figure_name}.ncl
#     echo "${figure_name}.nc created"
#     # funct_closing 1
# }


# trajectory_stats(){
# #    funct_opening 1
#     local traject=$1
#     cat > ${traject}_stats.ncl <<_EOF_
# load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
# load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
# load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
# begin
# ; read in data
# file1       = addfile ("${traject}.nc","r")
# Insta_state = file1->Insta_state
# Taver_state = file1->Taver_state
# ; Calculate statistics
# ; - along x direction
# Insta_state_Xstdd = dim_stddev_n_Wrap(Insta_state,1)
# Insta_state_Xmean = dim_avg_n_Wrap   (Insta_state,1)
# Taver_state_Xstdd = dim_stddev_n_Wrap(Taver_state,1)
# Taver_state_Xmean = dim_avg_n_Wrap   (Taver_state,1)
# file2 = addfile ("${traject}_Xstats.nc","c")
# file2->Insta_state_Xstdd = Insta_state_Xstdd
# file2->Insta_state_Xmean = Insta_state_Xmean
# file2->Taver_state_Xstdd = Taver_state_Xstdd
# file2->Taver_state_Xmean = Taver_state_Xmean
# ; - along time direction
# Insta_state_Tstdd = dim_stddev_n_Wrap(Insta_state,0)
# Insta_state_Tmean = dim_avg_n_Wrap   (Insta_state,0)
# Taver_state_Tstdd = dim_stddev_n_Wrap(Taver_state,0)
# Taver_state_Tmean = dim_avg_n_Wrap   (Taver_state,0)
# file3 = addfile ("${traject}_Tstats.nc","c")
# file3->Insta_state_Tstdd = Insta_state_Tstdd
# file3->Insta_state_Tmean = Insta_state_Tmean
# file3->Taver_state_Tstdd = Taver_state_Tstdd
# file3->Taver_state_Tmean = Taver_state_Tmean
# ; - Overall
# Insta_state_Tstdd_Xmean = dim_avg_n_Wrap(Insta_state_Tstdd,0)
# asciiwrite("${traject}_Insta_Tstdd_Xmean.dat",Insta_state_Tstdd_Xmean)
# Taver_state_Tstdd_Xmean = dim_avg_n_Wrap(Taver_state_Tstdd,0)
# asciiwrite("${traject}_Taver_Tstdd_Xmean.dat",Taver_state_Tstdd_Xmean)
# end
# _EOF_
#     call_ncl ${traject}_stats.ncl
# #    funct_closing 1
# }
