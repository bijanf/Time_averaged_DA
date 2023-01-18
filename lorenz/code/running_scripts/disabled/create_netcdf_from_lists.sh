#!/usr/bin/env bash
#=======================================================================
#> @brief Create netcdf file from lists
#=======================================================================
create_netcdf_from_lists(){
    case "$#" in
        2) create_netcdf_scalar_vs_par1 $@ ;;
        3) create_netcdf_scalar_vs_par1_par2 $@ ;;
        *) error "Unsupported number of parameters $(( $# - 1))";;
    esac
}

create_netcdf_scalar_vs_par1(){
    # funct_opening 1
    local scalar_name=$1
    local   par1_name=$2

    figure_name="${scalar_name}_vs_${par1_name}"
    rm -f ${figure_name}.nc
    cat > ${figure_name}.ncl <<_EOF_
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
begin
; - Read in data
scalar_list = readAsciiTable("${scalar_name}.list",1,"float",0)
par1_pos    = readAsciiTable("${par1_name}_value_pos.list",1,"integer",0)
par1_values = readAsciiTable(   "${par1_name}_values.list",1,"float",0)

     list_length = max(dimsizes(scalar_list))
par1_span_length = max(dimsizes(par1_values))

scalar = new((/par1_span_length,1/),float)

do i = 0, list_length-1
   scalar(par1_pos(i,0),0) = scalar_list(i,0)
end do

; - Write as netcdf
f_out  = addfile("${figure_name}.nc","c")
scalar!0              = "${par1_name}"
scalar&${par1_name}   =    par1_values(:,0)
f_out->${scalar_name} = scalar
end
_EOF_
    call_ncl ${figure_name}.ncl
    echo "${figure_name}.nc created"
    # funct_closing 1
}


#=======================================================================
#> @brief Create netcdf file from lists
#=======================================================================
create_netcdf_scalar_vs_par1_par2(){
    # funct_opening 1
    local scalar_name=$1
    local   par1_name=$2
    local   par2_name=$3

    figure_name="${scalar_name}_vs_${par1_name}_${par2_name}"
    rm -f ${figure_name}.nc
    cat > ${figure_name}.ncl <<_EOF_
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
begin
; - Read in data
scalar_list = readAsciiTable("${scalar_name}.list",1,"float",0)
par1_pos    = readAsciiTable("${par1_name}_value_pos.list",1,"integer",0)
par2_pos    = readAsciiTable("${par2_name}_value_pos.list",1,"integer",0)
par1_values = readAsciiTable(   "${par1_name}_values.list",1,"float",0)
par2_values = readAsciiTable(   "${par2_name}_values.list",1,"float",0)

     list_length = max(dimsizes(scalar_list))
par1_span_length = max(dimsizes(par1_values))
par2_span_length = max(dimsizes(par2_values))

scalar = new((/par1_span_length,par2_span_length/),float)

do i = 0, list_length-1
   scalar(par1_pos(i,0),par2_pos(i,0)) = scalar_list(i,0)
end do

; - Write as netcdf
f_out  = addfile("${figure_name}.nc","c")
scalar!0              = "${par1_name}"
scalar&${par1_name}   =    par1_values(:,0)
scalar!1              = "${par2_name}"
scalar&${par2_name}   =    par2_values(:,0)
f_out->${scalar_name} = scalar
end
_EOF_
    call_ncl ${figure_name}.ncl
    echo "${figure_name}.nc created"
    # funct_closing 1
}
set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source ${COM_DAS_DIR}/common_das_tools.sh
#source ${COM_DAS_DIR}/common_postprocess.sh
create_netcdf_from_lists $@
exit $?
