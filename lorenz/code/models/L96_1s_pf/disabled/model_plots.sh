#!/usr/bin/env bash

model_plot_nature_run(){
    trajectory_stats "nature"
}

trajectory_stats(){
    funct_opening 1

    local traject=$1
#    mnksnkvsn
#    exit
    (cat<<_EOF_
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
begin
; read in data
file1       = addfile ("${traject}.nc","r")
Insta_state = file1->Insta_state
Taver_state = file1->Taver_state
; Calculate statistics
; - along x direction 
Insta_state_Xstdd = dim_stddev_n_Wrap(Insta_state,1)
Insta_state_Xmean = dim_avg_n_Wrap   (Insta_state,1)
Taver_state_Xstdd = dim_stddev_n_Wrap(Taver_state,1)
Taver_state_Xmean = dim_avg_n_Wrap   (Taver_state,1)
file2 = addfile ("${traject}_Xstats.nc","c")
file2->Insta_state_Xstdd = Insta_state_Xstdd
file2->Insta_state_Xmean = Insta_state_Xmean
file2->Taver_state_Xstdd = Taver_state_Xstdd
file2->Taver_state_Xmean = Taver_state_Xmean
; - along time direction 
Insta_state_Tstdd = dim_stddev_n_Wrap(Insta_state,0)
Insta_state_Tmean = dim_avg_n_Wrap   (Insta_state,0)
Taver_state_Tstdd = dim_stddev_n_Wrap(Taver_state,0)
Taver_state_Tmean = dim_avg_n_Wrap   (Taver_state,0)
file3 = addfile ("${traject}_Tstats.nc","c")
file3->Insta_state_Tstdd = Insta_state_Tstdd
file3->Insta_state_Tmean = Insta_state_Tmean
file3->Taver_state_Tstdd = Taver_state_Tstdd
file3->Taver_state_Tmean = Taver_state_Tmean
; - Overall
Insta_state_Tstdd_Xmean = dim_avg_n_Wrap(Insta_state_Tstdd,0)
asciiwrite("${traject}_Insta_Tstdd_Xmean.dat",Insta_state_Tstdd_Xmean)
Taver_state_Tstdd_Xmean = dim_avg_n_Wrap(Taver_state_Tstdd,0)
asciiwrite("${traject}_Taver_Tstdd_Xmean.dat",Taver_state_Tstdd_Xmean)

end
exit
_EOF_
   )>${traject}_stats.ncl
   ncl ${traject}_stats.ncl > /dev/null;
    funct_closing 1
}


# model_plot_nature_run(){
#     funct_opening 1
#     dataset_name="nature_state_Insta_4b"

#     #echo " - Plotting with NCL"
#     figure_name=$dataset_name
#     (cat<<_EOF_
# ;************************************************
# load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
# load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
# ;************************************************
# begin
# ;************************************************
# ; read in data
# ;************************************************
# f     = addfile ("${dataset_name}.nc","r")
# C1    = f->c1
# ;************************************************
# ; plotting parameters
# ;************************************************
# wtype = "pdf"
# wtype@wkPaperWidthF = 10
# wtype@wkPaperHeightF = 16
# wtype@wkOrientation = "landscape"

# wks   = gsn_open_wks (wtype,"${figure_name}")                ; open workstation

# res                  = True                     ; plot mods desired
# ; res@tiMainString     = "nature plot"          ; add title
# res@vpHeightF= 0.4                    ; change aspect ratio of plot
# res@vpWidthF = 0.8
# res@gsnMaximize=True
# res@gsnPaperOrientation="landscape"

# res@xyLineColors      = (/"blue","red"/)         ; change line color
# res@xyLineThicknesses = (/2.0,2.0/)              ; make 2nd lines thicker
# res@xyDashPattern     = 0                        ; Make curves all solid
# res@tiYAxisOn = False

# res@pmLegendDisplayMode    = "Always"         ; Turn on a legend
# res@pmLegendOrthogonalPosF = -1.1            ; Move legend inside plot
# res@pmLegendParallelPosF   =  0.8             ; Move legend to right
# res@pmLegendWidthF         =  0.1             ; Change width and height
# res@pmLegendHeightF        =  0.08

# res@lgPerimFill            = "SolidFill"      ; Fill legend box w/white
# res@lgPerimFillColor       = "white"          ; so it masks XY curves
# res@lgLabelFontHeightF     = .03                 ; label font height

# res@xyExplicitLegendLabels = (/"C1(1)","C1(21)"/) ; create explicit labels

# time = C1&time * 0.05
# C1anom = C1(:,0,0,0)
# C1anom = dim_rmvmean(C1anom)
# C2anom = C1(:,0,0,20)
# C2anom = dim_rmvmean(C2anom)

# data      = new((/2,dimsizes(time)/),float)
# data(0,:) = C1anom
# data(1,:) = C2anom

# plot  = gsn_csm_xy (wks,time,data,res) ; create plot
# end
# _EOF_
#     ) > ${figure_name}.ncl
#     ncl ${figure_name}.ncl #> /dev/null;
#     pdf90 "${figure_name}.pdf" &> /dev/null
#     mv "${figure_name}-rotated90.pdf" "${figure_name}.pdf" > /dev/null

#     funct_closing 1
# }

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
