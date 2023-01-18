;==============================================================
; Create Line plot for 1D run_set stats for 2-component models
;==============================================================
begin
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

title       = asciiread(plot_name +  "_title.txt",-1, "string")
nc_path     = asciiread(plot_name +  "_files.txt",-1, "string")
label       = asciiread(plot_name + "_labels.txt",-1, "string")
n_scalar    = dimsizes (nc_path)
color_map   = "cyclic"
var_name    = (/"comp1","comp2"/)
color_array = ispan(1,n_scalar,1)
font_height  = .04
ticks_height = 0.035

; open  workstation
wtype                = "pdf"
wtype@wkPaperWidthF  =  10*n_comp
wtype@wkPaperHeightF =  6
;wtype@wkOrientation  = "landscape"

wks   = gsn_open_wks(wtype,plot_name)

plot = new(n_comp + 1,graphic)  ; create a plot array

do icomp = 0, n_comp-1
  do i_scalar = 0, n_scalar-1
                                ; read in data
    scalar_nc = addfile(nc_path(i_scalar),"r")
    dSizes    = getfiledimsizes(scalar_nc)
    rank      = dimsizes(dSizes)

    ; print(dSizes)
    if (i_scalar.eq.0)
      var_array = new((/n_scalar,dSizes(0)/),float)
    end if

    var_array(i_scalar,:) = scalar_nc->$var_name(icomp)$
    delete(scalar_nc)
  end do

  par_name  = var_array!1
  par_label = "Assimilation cycles"

                                ; Define plot config structure res
  res                      = True
  res@gsnDraw              = False ; don't draw
  res@gsnFrame             = False ; don't advance frame
  res@cnInfoLabelOn        = False ; turn off cn info label
  res@gsnPaperOrientation  = "portrait"

  res@xyLineColors         = color_array
  res@xyMonoLineThickness  = True
  res@xyLineThicknessF     = 2.0
  res@xyDashPattern        = 0     ; Make curves all solid

  if(n_comp.gt.1)then
    res@tiMainString         = var_array@long_name
                                ; res@tiMainString  = var_name(icomp)
    res@tiMainFontHeightF    = font_height
  end if
  
  res@tiXAxisString        = par_label
  res@tiXAxisFontHeightF   = font_height
  res@tiYAxisString        = title    ; y-axis label
  res@tiYAxisFontHeightF   = font_height

  res@tmXBLabelFontHeightF = ticks_height
  res@tmXBPrecision        = 2
  res@tmXBAutoPrecision    = True
  res@tmYLLabelFontHeightF = ticks_height
;   res@tmYLLabelsOn         = True

  if (icomp.eq.1) then
    res@tiYAxisOn          = False
  end if

  ; res@trXMaxF              = 200
  res@gsnYRefLine          = 0.0        ; create a reference line

  if(isvar("min_level").and.isvar("max_level")) then
    res@trYMinF = min_level
    res@trYMaxF = max_level
  end if

  gsn_define_colormap(wks,color_map)

  plot(icomp)  = gsn_csm_xy(wks,var_array&$par_name$,var_array,res)
  delete(var_array)
end do

; legend

lgres                     = True
lgres@lgBoxLinesOn        = False
; lgres@lgTitleOn           = True
; lgres@lgTitleString       = title
; lgres@lgTitleFontHeightF  = .08
lgres@lgLineColors        = res@xyLineColors
lgres@lgMonoLineThickness = res@xyMonoLineThickness
lgres@lgLineThicknessF    = res@xyLineThicknessF
lgres@lgBoxMinorExtentF   = 0.2       ; Shorten the legend lines
lgres@lgItemType          = "Lines"   ; show lines only (default)
lgres@lgLabelFontHeightF  = .03       ; label font height
lgres@lgDashIndexes       = ispan(1,n_scalar,1)*0
;lgres@vpWidthF            = 0.3      ; width of legend (NDC)
lgres@vpHeightF           = 0.09 * n_scalar   ; height of legend (NDC)
lgres@lgAutoManage        = True      ; or let ncl to do that

plot(n_comp) = gsn_create_legend(wks,n_scalar,label,lgres) ; create legend

; plot panel

resP                            = True
;resP@txString                  = title
;resP@txStringFontHeightF        = .03
resP@gsnMaximize                = True
resP@gsnPaperOrientation        = "portrait"
resP@gsnPanelYWhiteSpacePercent = 0
gsn_panel(wks,plot,(/1,n_comp+1/),resP)

end
exit
