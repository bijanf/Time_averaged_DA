;==============================================================
; Create Line plot for 1D run_set stats for 2-component models
;==============================================================
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

plot_name   = asciiread("plot_name.txt",-1,"string")
title       = asciiread(plot_name + "_title.txt",-1,"string")
nc_path     = asciiread(plot_name + "_files.txt",-1,"string")
label       = asciiread(plot_name + "_labels.txt" ,-1,"string")
n_scalar    = dimsizes (nc_path)
color_map   = "cyclic"
n_comp      = 2
var_name    = (/"comp1","comp2"/)
color_array = ispan(1,n_scalar,1)

; open  workstation
wtype                = "pdf"
wtype@wkPaperWidthF  =  16
wtype@wkPaperHeightF =  7
;wtype@wkOrientation  = "landscape"

wks   = gsn_open_wks(wtype,plot_name)

plot = new(3,graphic)  ; create a plot array

do icomp = 0, n_comp-1

  do is = 0, n_scalar-1
                                ; read in data
    scalar_nc = addfile(nc_path(is),"r")
    dSizes    = getfiledimsizes(scalar_nc)
    rank      = dimsizes(dSizes)

    if (is.eq.0)
      var_array = new((/n_scalar,dSizes(0)/),float)
    end if

    var_array(is,:) = scalar_nc->$var_name(icomp)$
    delete(scalar_nc)
  end do

  par_name = var_array!1
                                ; Define plot config structure res
  res                      = True
  res@gsnDraw              = False ; don't draw
  res@gsnFrame             = False ; don't advance frame
  res@cnInfoLabelOn        = False ; turn off cn info label
  res@gsnPaperOrientation  = "portrait"

  res@xyLineColors         = color_array
  res@xyMonoLineThickness  = True
  res@xyLineThicknessF     = 5.0
  res@xyDashPattern        = 0     ; Make curves all solid

  res@tiMainString         = var_name(icomp)
  res@tiMainFontHeightF    = .04
  res@tiXAxisString        = par_name
  res@tiXAxisFontHeightF   = .04
  res@tiYAxisString        = title    ; y-axis label
  res@tiYAxisFontHeightF   = .04

  res@tmXBLabelFontHeightF = .03
  res@tmXBPrecision        = 2
  res@tmXBAutoPrecision    = True
  res@tmYLLabelFontHeightF = .03
;   res@tmYLLabelsOn         = True

  if (icomp.eq.1) then
    res@tiYAxisOn          = False
  end if

  res@gsnYRefLine          = 0.0        ; create a reference line

  gsn_define_colormap(wks,color_map)

  plot(icomp)  = gsn_csm_xy(wks,var_array&$par_name$,var_array,res)
  delete(var_array)
end do

; legend

lgres                     = True
lgres@lgBoxLinesOn        = False
lgres@lgTitleOn           = True
lgres@lgTitleString       = title
lgres@lgTitleFontHeightF  = .08
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

plot(2) = gsn_create_legend(wks,n_scalar,label,lgres) ; create legend

; plot panel

resP                            = True
;resP@txString                  = title
;resP@txStringFontHeightF        = .03
resP@gsnMaximize                = True
resP@gsnPaperOrientation        = "portrait"
resP@gsnPanelYWhiteSpacePercent = 0
gsn_panel(wks,plot,(/1,3/),resP)

exit
