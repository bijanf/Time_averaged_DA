#!/usr/bin/env bash
#set -x
#=======================================================================
#> @brief Calculates the variance of the state variables over
#>        a set of state binary files 
#> Usage example
#> file_list="nature_run"; ls *_grid_sigma.rst >$file_list
#> calculate_variance $file_list
#=======================================================================
plot_skill()
{
 	#[[ $# -eq 1 ]] || error "Usage: calculate_variance binary_file_list"
	#binary_file_list=$1
	
	#binary_files="${binary_file_list%.*}" # get rid of the extention .dat
	#variance_file="${binary_files}_var.rst"
	
	#echo "================================================"
    #echo " Calculating variable variance of states listed"
    #echo " in  $binary_file_list"
	#echo "================================================"

	#echo " - Creating binary file"
	#THERE=$(pwd); cd $THERE
	#calculate_variance.exe $binary_file_list
	#mv variance_file.rst $variance_file

	echo " - Creating descriptor file"
	(
cat<<_EOF_
;*************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
;************************************************
begin
;************************************************
; read in netCDF file
;************************************************
  a = addfile("MSSS_prior_Taver.nc","r")
;************************************************
; read in surface temperature
;************************************************
  t = a->t(0,0,:,:)       ; read July zonal winds
;************************************************
; create plot
;************************************************
  wtype                = "eps"
  wtype@wkPaperWidthF  = 11
  wtype@wkPaperHeightF = 22
  wtype@wkOrientation  = "landscape"          

  wks = gsn_open_wks(wtype,"MSSS_temp")
;  wks = gsn_open_wks("ps","MSSS_temp")
  
  gsn_define_colormap(wks,"BlueYellowRed")
  gsn_reverse_colormap(wks)

  res                   = True
  res@mpProjection      = "CylindricalEqualArea"       ; choose projection
  res@mpGridAndLimbOn   = True              ; turn on lat/lon lines
  res@mpPerimOn         = False             ; turn off box around plot
  res@mpGridLatSpacingF = 30.               ; spacing for lat lines
  res@mpGridLonSpacingF = 30.               ; spacing for lon lines
  res@mpFillOn          = False
 
  res@cnFillOn          = True              ; color plot desired
  res@cnLinesOn          = False              ; color plot desired
  res@cnLineLabelsOn    = False             ; turn off contour lines
  res@txFontHeightF     = 0.015 


  res@cnLevelSelectionMode="ManualLevels"
  res@cnMinLevelValF=-1
  res@cnMaxLevelValF=1
  res@cnLevelSpacingF=0.05
  
  res@gsnPaperOrientation="auto"
  res@gsnMaximize=True

  res@lbLabelFontHeightF  = 0.015           ; label bar font height

  contour = gsn_csm_contour_map(wks,t,res)  ; create the plot
end
_EOF_
	)>"plot_skill.ncl"
	
	ncl plot.ncl > /dev/null
	
	echo " - Successful end"
	echo "================================================"
	
	return 0 # Success
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

plot_skill $@
exit $?


