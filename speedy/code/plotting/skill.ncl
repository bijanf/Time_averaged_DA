;*************************************************
; skill.ncl
;************************************************
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
  t = a->t(0,0,:,:)                                ; read July zonal winds
;************************************************
; create plot
;************************************************
  wtype                = "pdf"
  wtype@wkPaperWidthF  = 10
  wtype@wkPaperHeightF = 16
  wtype@wkOrientation  = "landscape"          

  wks = gsn_open_wks(wtype,"MSSS_temp")
  
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
