#!/usr/bin/env bash
#set -x

model_plot_nature_run(){
    funct_opening 1
    dataset_name="nature_state_Insta_4b"

    #echo " - Plotting with NCL"
    figure_name=$dataset_name
    (cat<<_EOF_
;************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
;************************************************
begin
;************************************************
; read in data
;************************************************
f     = addfile ("${dataset_name}.nc","r")
C1    = f->c1
;************************************************
; plotting parameters
;************************************************
wtype = "pdf"
wtype@wkPaperWidthF = 10
wtype@wkPaperHeightF = 16
wtype@wkOrientation = "landscape"

wks   = gsn_open_wks (wtype,"${figure_name}")                ; open workstation

res                  = True                     ; plot mods desired
; res@tiMainString     = "nature plot"          ; add title
res@vpHeightF= 0.4                    ; change aspect ratio of plot
res@vpWidthF = 0.8
res@gsnMaximize=True
res@gsnPaperOrientation="landscape"

res@xyLineColors      = (/"blue","red"/)         ; change line color
res@xyLineThicknesses = (/2.0,2.0/)              ; make 2nd lines thicker
res@xyDashPattern     = 0                        ; Make curves all solid
res@tiYAxisOn = False

res@pmLegendDisplayMode    = "Always"         ; Turn on a legend
res@pmLegendOrthogonalPosF = -1.1            ; Move legend inside plot
res@pmLegendParallelPosF   =  0.8             ; Move legend to right
res@pmLegendWidthF         =  0.1             ; Change width and height
res@pmLegendHeightF        =  0.08

res@lgPerimFill            = "SolidFill"      ; Fill legend box w/white
res@lgPerimFillColor       = "white"          ; so it masks XY curves
res@lgLabelFontHeightF     = .03                 ; label font height

res@xyExplicitLegendLabels = (/"C1(1)","C1(21)"/) ; create explicit labels

time = C1&time * 0.05
C1anom = C1(:,0,0,0)
C1anom = dim_rmvmean(C1anom)
C2anom = C1(:,0,0,20)
C2anom = dim_rmvmean(C2anom)

data      = new((/2,dimsizes(time)/),float)
data(0,:) = C1anom
data(1,:) = C2anom

plot  = gsn_csm_xy (wks,time,data,res) ; create plot
end
_EOF_
    ) > ${figure_name}.ncl
    ncl ${figure_name}.ncl > /dev/null;
    pdf90 "${figure_name}.pdf" &> /dev/null
    mv "${figure_name}-rotated90.pdf" "${figure_name}.pdf" > /dev/null

    funct_closing 1
}

plot_state_var_vs_x(){
    funct_opening 0
    binary_file=$1
    varname=$2

    echo " - Exporting data as netcdf using CDO"
    cdo -f nc import_binary ${binary_file}.ctl ${binary_file}.nc &> /dev/null

    echo " - Plotting with NCL"
    figure_name="${binary_file}"
    (cat<<_EOF_
    ;************************************************
    load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
    load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
;************************************************
begin
;************************************************
; read in data
;************************************************
f     = addfile ("${binary_file}.nc","r")
$varname  = f->$varname
;************************************************
; plotting parameters
;************************************************
wtype = "pdf"
wtype@wkPaperWidthF  = 10
wtype@wkPaperHeightF = 16
wtype@wkOrientation  = "landscape"

wks   = gsn_open_wks (wtype,"${binary_file}")                ; open workstation

res                  = True                     ; plot mods desired
res@vpHeightF  = 0.4                    ; change aspect ratio of plot
res@vpWidthF   = 0.8
res@gsnMaximize=True
res@gsnPaperOrientation="landscape"

plot  = gsn_csm_xy (wks,${varname}&lon,${varname}(0,0,0,:),res) ; create plot
end
_EOF_
    ) > ${figure_name}.ncl
    ncl ${figure_name}.ncl > /dev/null;

    pdf90 "${figure_name}.pdf" &> /dev/null
    mv "${figure_name}-rotated90.pdf" "${figure_name}.pdf" > /dev/null

    funct_closing 0
}
