;==============================================================
; Create Line plot for 1D run_set stats for 2-component models
;==============================================================
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

title       = asciiread(plot_name +  "_title.txt",-1, "string")
nc_path     = asciiread(plot_name +  "_files.txt",-1, "string")
label       = asciiread(plot_name + "_labels.txt",-1, "string")
n_scalar    = dimsizes (nc_path)
var_name    = (/"comp1","comp2"/)
var_label   = (/"Component 1","Component 2"/)
float_nan   = -9.99e+33    ; CDO default
color_array = ispan(1,n_scalar,1)
font_height  = .04
ticks_height = 0.035

if     (n_scalar.le.6)then
  color_map   = "cyclic"
else if(n_scalar.le.14)then
  color_map   = "grads_default"
else if(n_scalar.le.30)then
  color_map   = "default"
end if
end if
end if

; if(.not.isvar("legend_panel")) then
;   legend_panel = True
;   panels = n_comp + 1
; else
;   panels = n_comp
; end if
panels = n_comp + 1

; open  workstation
wtype                = "pdf"
wtype@wkPaperHeightF =  6

; if(legend_panel.eq.True) then
  if(n_comp.eq.1)then
    wtype@wkPaperWidthF  =  6*(1.6)
  else
    wtype@wkPaperWidthF  =  6*(2.4)
  end if
; else  
;   if(n_comp.eq.1)then
;     wtype@wkPaperWidthF  =  6*(0.8)
;   else
;     wtype@wkPaperWidthF  =  6*(1.6)
;   end if
; end if

;wtype@wkPaperWidthF  =  6*(1.8)   ; 2 comps and legend
;wtype@wkPaperWidthF  =  6*panels  ; 1 comp  and legend
;wtype@wkOrientation  = "landscape"

wks   = gsn_open_wks(wtype,plot_name)

plot = new(panels,graphic)  ; create a plot array

do icomp = 0, n_comp-1
  do i_scalar = 0, n_scalar-1
                                ; read in data
    scalar_nc = addfile(nc_path(i_scalar),"r")
    dSizes    = getfiledimsizes(scalar_nc)
    rank      = dimsizes(dSizes)

    if(isvar("cdo_output")) then ; cdo output is always 4-dimensional
      if (i_scalar.eq.0)
        var_array = new((/n_scalar,dSizes(3)/),float)
      end if
      var_array(i_scalar,:) = scalar_nc->$var_name(icomp)$(:,0,0,0)
    else
      if (i_scalar.eq.0)
        var_array = new((/n_scalar,dSizes(0)/),float)
      end if
      var_array(i_scalar,:) = scalar_nc->$var_name(icomp)$
    end if

    delete(scalar_nc)
  end do

  par_name  = var_array!1

  if(isvar("cdo_output")) then ; cdo output is always 4-dimensional
    par_label = "analysis cycles"
  else
    par_label = var_array&$par_name$@long_name
  end if

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

                                ; INDIVIDUAL PLOT TITLE
                                ; if(n_comp.eq.1)then
                                ;   res@tiMainString       = var_array@long_name
                                ; else
  if(n_comp.gt.1)then
    res@tiMainString       = var_label(icomp)
  end if
  res@tiMainFontHeightF    = font_height

                                ; LABELS
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

  res@gsnYRefLine          = 0.0        ; create a reference line

;  if(isvar("min_level").and.isvar("max_level")) then
;    res@trYMinF = min_level
;    res@trYMaxF = max_level
;  end if

  if(isvar("min_x")) then
    res@trXMinF = min_x
  end if

  if(isvar("max_x")) then
    res@trXMaxF = max_x
  end if
  
  if(isvar("min_level")) then
    res@trYMinF = min_level
  end if

  if(isvar("max_level")) then
    res@trYMaxF = max_level
  end if

  gsn_define_colormap(wks,color_map)

  var_array@_FillValue = float_nan

  plot(icomp)  = gsn_csm_xy(wks,var_array&$par_name$,var_array,res)
  delete(var_array)
end do

; legend
; if(legend_panel.eq.True)then

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
  lgres@vpWidthF            = 0.85      ; width of legend (NDC)
                                ; lgres@vpHeightF           = 0.09 * n_scalar   ; height of legend (NDC)
  lgres@lgAutoManage        = True      ; or let ncl to do that

  plot(n_comp) = gsn_create_legend(wks,n_scalar,label,lgres) ; create legend
; end if

; plot panel

resP                            = True
;resP@txString                  = title
;resP@txStringFontHeightF        = .03
resP@gsnMaximize                = True
resP@gsnPaperOrientation        = "portrait"
resP@gsnPanelYWhiteSpacePercent = 0
resP@gsnPanelXWhiteSpacePercent = 0
gsn_panel(wks,plot,(/1,panels/),resP)

exit
