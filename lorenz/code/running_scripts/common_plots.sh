







contour_plot(){
    nc_file_name=$1
    var=$2
    levels_mode="Automatic"
    MinLevelVal="0"; MaxLevelVal="1"
    if [[ $# -eq 4 ]];then
        levels_mode="Manual"
        MinLevelVal=$3; MaxLevelVal=$4
    fi
    cat > "${nc_file_name}.ncl" <<_EOF_
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
begin

; read in data
  a = addfile("${nc_file_name}.nc","r")
  ${var} = a->${var}(:,:)

; open  workstation
  wtype = "pdf"
  wks   = gsn_open_wks(wtype,"${var}")

; Define plot config structure res
  res                      = True
  res@gsnPaperOrientation  = "auto"
  res@gsnMaximize          = True

; - Contour plot
  res@cnFillOn         = True               ; color Fill 
  res@cnFillMode       = "RasterFill"       ; Raster Mode
  res@cnLinesOn        =  False             ; Turn off contour lines
;  res@cnLineLabelsOn       = False ; contour lines switch
;  res@cnLevelSelectionMode = "${levels_mode}Levels"
;  res@cnMinLevelValF       = $MinLevelVal
;  res@cnMaxLevelValF       = $MaxLevelVal
;  res@cnLevelSpacingF      = 0.05
  res@lbOrientation        = "vertical"


; - Color palette
  gsn_define_colormap(wks,"BlueYellowRed")
;  gsn_reverse_colormap(wks)
  res@gsnSpreadColors      = True               ; span full colormap

  symMinMaxPlt (${var},20,False,res)
  contour = gsn_csm_contour(wks,${var},res)  ; create the plot
end
exit
_EOF_
    pdf_ncl $nc_file_name
}


plot_rms_stats(){
    ens_phase=$1
    ens_Tkind=$2

    rmse_file="${ens_phase}_${ens_Tkind}_Ermse"
    spread_file="${ens_phase}_${ens_Tkind}_Esprd"
    figure_name="${ens_phase}_${ens_Tkind}_rms_stats"

    cat > "${figure_name}.ncl" <<_EOF_
;************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
;************************************************
begin
;************************************************
; read in data
;************************************************
f     = addfile ("${rmse_file}.nc","r")
rmse   = f->rmse
g     = addfile ("${spread_file}.nc","r")
spread = g->spread
;************************************************
; spread parameters
;************************************************
wtype = "pdf"
wtype@wkPaperWidthF = 10
wtype@wkPaperHeightF = 16
wtype@wkOrientation = "landscape"

wks   = gsn_open_wks (wtype,"${figure_name}") ; open workstation

res                    = True         ; plot mods desired
; res@tiMainString       = "RMSE"       ; title
res@vpHeightF          = 0.4           ; change aspect ratio of plot
res@vpWidthF           = 0.8
res@gsnMaximize        = True
res@gsnPaperOrientation= "landscape"

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

res@xyExplicitLegendLabels = (/"RMSE","Spread"/) ; create explicit labels

time = rmse&time
data      = new((/2,dimsizes(time)/),float)
data(0,:) =   rmse(:,0,0,0)
data(1,:) = spread(:,0,0,0)

plot  = gsn_csm_xy (wks,time,data,res) ; create plot

end
exit
_EOF_
    call_ncl ${figure_name}.ncl
    pdf90 "${figure_name}.pdf" &> /dev/null
    mv "${figure_name}-rotated90.pdf" "${figure_name}.pdf" > /dev/null
}

nature_obs(){
    funct_opening 1

    dataset_kind="obs"

    cdo -f nc import_binary ${dataset_kind}.ctl ${dataset_kind}.nc #&> /dev/null

    figure_name="${dataset_kind}"
    echo "- Calling NCL"
    cat > plot_${dataset_kind}.ncl <<_EOF_
;************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
;************************************************
begin
;************************************************
; read in data
;************************************************
f     = addfile ("${dataset_kind}.nc","r")
x     = f->y
;************************************************
; plotting parameters
;************************************************
wtype = "pdf"
wtype@wkPaperWidthF  = 10
wtype@wkPaperHeightF = 16
wtype@wkOrientation  = "landscape"

wks   = gsn_open_wks (wtype,"${figure_name}")                ; open workstation

res                    = True                     ; plot mods desired
res@vpHeightF          = 0.4                    ; change aspect ratio of plot
res@vpWidthF           = 0.8
res@gsnMaximize        = True
res@gsnPaperOrientation= "landscape"

data2      = new((/1,dimsizes(x&time)/),float)
; data2      = new((/3,dimsizes(x&time)/),float)

data2(0,:) = x(:,0,0,1)
; data2(1,:) = x(:,0,0,2)
; data2(2,:) = x(:,0,0,3)

plot  = gsn_csm_xy (wks,x&time,data2,res) ; create plot
end
exit
_EOF_
    call_ncl plot_${dataset_kind}.ncl > /dev/null;

    pdf90 "${figure_name}.pdf" &> /dev/null
    mv "${figure_name}-rotated90.pdf" "${figure_name}.pdf" > /dev/null

    funct_closing 1
}


plot_rms_stats_Tmean_vs_par(){
    local par_name=$1
    local   prefix=$2
    sprd_file="${prefix}_Esprd_Tmean_list.dat"
    rmse_file="${prefix}_Ermse_Tmean_list.dat"
    par_file="${par_name}_list.dat"
    figure_name="${prefix}_RMS_stats_Tmean_vs_${par_name}"
    cat > ${figure_name}.ncl <<_EOF_
;************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
;************************************************
begin
;************************************************
; read in data
;************************************************
spread = readAsciiTable("$sprd_file",1,"float",0)
rmse   = readAsciiTable("$rmse_file",1,"float",0)
par    = readAsciiTable("$par_file" ,1,"float",0)
;************************************************
; plotting parameters
;************************************************
wtype = "pdf"
wtype@wkPaperWidthF = 10
wtype@wkPaperHeightF = 16
wtype@wkOrientation = "landscape"

wks   = gsn_open_wks (wtype,"$figure_name")                ; open workstation

res                  = True                     ; plot mods desired
res@tiXAxisString         = "$par_name"           ; x-axis label
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
res@xyExplicitLegendLabels = (/"SPREAD","RMSE"/) ; create explicit labels

data      = new((/2,dimsizes(par(:,0))/),float)
data(0,:) = spread(:,0)
data(1,:) = rmse(:,0)

plot  = gsn_csm_xy (wks,par(:,0),data,res) ; create plot
end
exit
_EOF_
    call_ncl ${figure_name}.ncl > /dev/null;
    pdf90 "${figure_name}.pdf" &> /dev/null
    mv "${figure_name}-rotated90.pdf" "${figure_name}.pdf" > /dev/null
}

plot_particle_trajectory(){
    funct_opening 0
    dataset_kind=$1

    cdo -f nc import_binary ${dataset_kind}.ctl ${dataset_kind}.nc #&> /dev/null

    figure_name="${dataset_kind}"
    cat > ${figure_name}.ncl <<_EOF_
;************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
;************************************************
begin
;************************************************
; read in data
;************************************************
f     = addfile ("${dataset_kind}.nc","r")
x     = f->x                                    ; get u data
;************************************************
; plotting parameters
;************************************************
wtype = "pdf"
wtype@wkPaperWidthF  = 10
wtype@wkPaperHeightF = 16
wtype@wkOrientation  = "landscape"

wks   = gsn_open_wks (wtype,"${figure_name}")                ; open workstation

res                  = True                     ; plot mods desired
; res@tiMainString     = "nature plot"          ; add title
res@vpHeightF  = 0.4                    ; change aspect ratio of plot
res@vpWidthF   = 0.8
res@gsnMaximize=True
res@gsnPaperOrientation="landscape"

data2      = new((/1,dimsizes(x&time)/),float)
; data2      = new((/3,dimsizes(x&time)/),float)

data2(0,:) = x(:,0,0,1)
; data2(1,:) = x(:,0,0,2)
; data2(2,:) = x(:,0,0,3)

plot  = gsn_csm_xy (wks,x&time,data2,res) ; create plot
end
exit
_EOF_
    call_ncl ${figure_name}.ncl > /dev/null;
    pdf90 "${figure_name}.pdf" &> /dev/null
    mv "${figure_name}-rotated90.pdf" "${figure_name}.pdf" > /dev/null

    funct_closing 0
}

plot_particle_trajectory_RMSE(){
    funct_opening 0
    dataset_kind=$1

    echo " - Exporting data as netcdf using CDO"
    cdo -f nc import_binary ${dataset_kind}.ctl ${dataset_kind}.nc #&> /dev/null

    figure_name="${dataset_kind}"
    cat > ${dataset_kind}.ncl <<_EOF_
;************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
;************************************************
begin
;************************************************
; read in data
;************************************************
f     = addfile ("${dataset_kind}.nc","r")
rmse   = f->rmse
;************************************************
; plotting parameters
;************************************************
wtype = "pdf"
wtype@wkPaperWidthF  = 10
wtype@wkPaperHeightF = 16
wtype@wkOrientation  = "landscape"

wks   = gsn_open_wks (wtype,"${figure_name}"); open workstation

res                  = True                  ; plot mods desired
res@vpHeightF  = 0.4                    ; change aspect ratio of plot
res@vpWidthF   = 0.8
res@gsnMaximize=True
res@gsnPaperOrientation="landscape"

plot  = gsn_csm_xy (wks,rmse&time,rmse(:,0,0,0),res) ; create plot
end
exit
_EOF_
    call_ncl ${dataset_kind}.ncl > /dev/null;
    pdf90 "${figure_name}.pdf" &> /dev/null
    mv "${figure_name}-rotated90.pdf" "${figure_name}.pdf" > /dev/null

    funct_closing 0
}

plot_skill_vs_par(){
    local par_name=$1
    local state_Tkind=$2
    prior_skill_file="prior_${state_Tkind}_RMSSS_Xmean_list.dat"
    postr_skill_file="postr_${state_Tkind}_RMSSS_Xmean_list.dat"
    par_file="${par_name}_list.dat"
    figure_name="${state_Tkind}_RMSSS_Xmean_vs_${par_name}"
    cat > ${figure_name}.ncl <<_EOF_
;************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
;************************************************
begin
;************************************************
; read in data
;************************************************
prior_skill = readAsciiTable("$prior_skill_file",1,"float",0)
postr_skill = readAsciiTable("$postr_skill_file",1,"float",0)
par   = readAsciiTable(  "$par_file" ,1,"float",0)
;************************************************
; plotting parameters
;************************************************
wtype = "pdf"
wtype@wkPaperWidthF = 10
wtype@wkPaperHeightF = 16
wtype@wkOrientation = "landscape"

wks   = gsn_open_wks (wtype,"$figure_name")   ; open workstation

res                   = True                   ; plot mods desired
res@tiXAxisString     = "$par_name"       ; x-axis label
res@tiYAxisString     = "RMSSS"
res@vpHeightF= 0.4                    ; change aspect ratio of plot
res@vpWidthF = 0.8
res@gsnMaximize=True
res@gsnPaperOrientation="landscape"

res@xyLineColors      = (/"blue","red"/)         ; change line color
res@xyLineThicknesses = (/2.0,2.0/)              ; make 2nd lines thicker
res@xyDashPattern     = 0                        ; Make curves all solid

res@pmLegendDisplayMode    = "Always"         ; Turn on a legend
res@pmLegendOrthogonalPosF = -1.1            ; Move legend inside plot
res@pmLegendParallelPosF   =  0.8             ; Move legend to right
res@pmLegendWidthF         =  0.1             ; Change width and height
res@pmLegendHeightF        =  0.08
res@lgPerimFill            = "SolidFill"      ; Fill legend box w/white
res@lgPerimFillColor       = "white"          ; so it masks XY curves
res@lgLabelFontHeightF     = .03                 ; label font height
res@xyExplicitLegendLabels = (/"PRIOR","POSTERIOR"/) ; create explicit labels
res@trYMinF                = -0.1   ; Limits for Y axis.  The limits
res@trYMaxF                =  1.1  ; for the X axis will be different
; fo each plot.

data      = new((/2,dimsizes(par(:,0))/),float)
data(0,:) = prior_skill(:,0)
data(1,:) = postr_skill(:,0)

plot  = gsn_csm_xy (wks,par(:,0),data,res) ; create plot
end
exit
_EOF_
    call_ncl ${figure_name}.ncl > /dev/null;
    pdf90 "${figure_name}.pdf" &> /dev/null
    mv "${figure_name}-rotated90.pdf" "${figure_name}.pdf" > /dev/null
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
# exit
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
exit
_EOF_
    ) > ${figure_name}.ncl
    call_ncl ${figure_name}.ncl > /dev/null;

    pdf90 "${figure_name}.pdf" &> /dev/null
    mv "${figure_name}-rotated90.pdf" "${figure_name}.pdf" > /dev/null

    funct_closing 0
}
