;                 --color_map  )   color_map=$2  ; shift 2;;
;                 --reverse_map) reverse_map=$2  ; shift 2;;
;                 --raster     ) raster_mode=True; shift 1;; # No contour interpolation
;                 --symMinMax  )   symMinMax=True; shift 1;;
;                 --min_level  )   min_level=$2  ; shift 2;;
;                 --max_level  )   max_level=$2  ; shift 2;;
;                 --spacing    )     spacing=$2  ; shift 2;;
;                 --lower_threshold) lower_threshold=$2; shift 2;;
;                 --upper_threshold) upper_threshold=$2; shift 2;;

;         local run_set_2D_stat_path=$1
;         local      scalar=$(basename "$run_set_2D_stat_path"); scalar=${scalar%.*}
;         local   color_map=${color_map:-"BlueYellowRed"}
;     #"BlAqGrYeOrReVi200" #"ncl_default"
;         local reverse_map=${reverse_map:-False}
;         local raster_mode=${raster_mode:-False}
;         local   symMinMax=${symMinMax:-False}
;         local   min_level=${min_level:-}
;         local   max_level=${max_level:-}
;         local     spacing=${spacing:-}
;         upper_threshold=${upper_threshold:-"30.0"}
;         lower_threshold=${lower_threshold:-"-30.0"}

load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

; n_comp      = 2
var_name    = (/"comp1","comp2"/)
var_label   = (/"Component 1","Component 2"/)
font_height  = .04
ticks_height = 0.035
; read in data
run_set_2D_stat_nc = addfile(nc_path,"r")
dSizes             = getfiledimsizes(run_set_2D_stat_nc)
; label_array        = new((/n_comp/),string)
float_nan   = -9.99e+33    ; CDO default

if (.not.isvar("color_map")) then
  color_map = "BlueYellowRed"
end if
;dimNames = getvardims(var_array)

plot = new(n_comp,graphic)  ; create a plot array

; open  workstation
wtype                = "pdf"
;wtype                = "eps"
wtype@wkPaperHeightF =  7
; if (n_comp.eq.2) then
wtype@wkPaperWidthF  =  8*n_comp

; else
; wtype@wkPaperWidthF  =  16
; end if

;wtype@wkOrientation  = "landscape"
wks   = gsn_open_wks(wtype,plot_name)

do icomp = 0, n_comp-1
  var = run_set_2D_stat_nc->$var_name(icomp)$
  ; label_array(icomp) = var_name(icomp)

; Define plot config structure res
  res                      = True
  res@gsnDraw              = False ; don't draw
  res@gsnFrame             = False ; don't advance frame
  res@cnInfoLabelOn        = False ; turn off cn info label
  res@gsnPaperOrientation  = "portrait"

; - Contour plot
  ; if (isvar("color_fill"))
    res@cnFillOn         = True         ; color Fill
  ; end if
  ; res@cnLinesOn        = False       ; Turn off contour lines
    
  contour_lines = False
  if (isvar("contour_lines"))
    res@cnLinesOn    = contour_lines ; Raster Mode
  end if
  res@cnLineLabelsOn       = False ; contour lines switch
    
  if (isvar("raster_mode"))
    res@cnFillMode    = "RasterFill" ; Raster Mode
  end if

  ; if(symMinMax == True)
  ;   symMinMaxPlt (var,20,False,res)
  ;   upper_threshold = $upper_threshold
  ;   lower_threshold = $lower_threshold
  ;   min_val = min(var)
  ;   max_val = max(var)

  ;   if ((max_val.gt. upper_threshold).or.(min_val .lt. lower_threshold))
  ;     res@cnLevelSelectionMode = "ManualLevels"
  ;     res@cnMaxLevelValF   = upper_threshold
  ;     res@cnMinLevelValF   = lower_threshold
  ;     res@cnLevelSpacingF  = (res@cnMaxLevelValF - res@cnMinLevelValF)/20.0
  ;   end if
  ; end if

  if(isvar("min_level").and.isvar("max_level")) then
    res@cnLevelSelectionMode = "ManualLevels"
    res@cnMinLevelValF       = min_level
    res@cnMaxLevelValF       = max_level
    res@cnLevelSpacingF      = (max_level -min_level)/levels
  end if

  ; upper_threshold = $upper_threshold
  ; lower_threshold = $lower_threshold
  ; min_val = min(var)
  ; max_val = max(var)

  ; if ((max_val.gt. upper_threshold).or.(min_val .lt. lower_threshold))
  ;   res@cnLevelSelectionMode = "ManualLevels"
  ;   res@cnMaxLevelValF   = min((/max_val,upper_threshold/))
  ;   res@cnMinLevelValF   = max((/min_val,lower_threshold/))
  ;   res@cnLevelSpacingF  = (res@cnMaxLevelValF - res@cnMinLevelValF)/20.0
  ; end if
  
  ; res@lbOrientation        = "vertical"

; - Color palette
  gsn_define_colormap(wks,color_map)

  if(isvar("reverse_colormap")) then
    gsn_reverse_colormap(wks)
  end if
  ; res@gsnSpreadColors      = True ; span full colormap
  res@lbOrientation =   "vertical" 

  if(n_comp.gt.1)then
    res@tiMainString         = var_label(icomp)
    res@tiMainFontHeightF    = font_height
  end if

  ; par1_name  = var!0
  ; par1_label = var&$par1_name$@long_name
  ; par2_name  = var!1
  ; par2_label = var&$par2_name$@long_name

  ; res@tiXAxisString        = par2_label
  res@tiXAxisFontHeightF   = font_height
  ; res@tiYAxisString        = par1_label    ; y-axis label
  res@tiYAxisFontHeightF   = font_height


  res@tmXBLabelFontHeightF = ticks_height
  res@tmYLLabelFontHeightF = ticks_height
  res@tmXBFormat = "@.2^f"
;  res@tmYLFormat = "@.3^f"
  res@tmYLAutoPrecision    = True

  if (icomp.gt.0) then
    res@tiYAxisOn    = False
    res@tmYLLabelsOn = False
  end if

  var@_FillValue            = float_nan

  plot(icomp) = gsn_csm_contour(wks,var,res)
end do

resP                      = True ; modify the panel plot
;resP@txString             = "${scalar}"
resP@gsnMaximize          = True
resP@gsnPaperOrientation  = "portrait"
resP@gsnPanelYWhiteSpacePercent = 0

gsn_panel(wks,plot,(/1,n_comp/),resP)

;newplot = gsn_attach_plots(plot(0),(/plot(1)/),res,res)
;draw(plot(0))
;frame(wks)


exit
