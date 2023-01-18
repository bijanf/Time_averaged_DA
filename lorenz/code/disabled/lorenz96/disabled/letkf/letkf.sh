#!/bin/sh
set -e
ORO=
OBS=regular13
EXP=M10L30I05
F90=gfortran
CDIR=`pwd`
cd ..
L96DIR=`pwd`
cd ..
ENKFDIR=`pwd`
COMDIR=$ENKFDIR/common
OUTDIR=$L96DIR/DATA
WKDIR=$L96DIR/tmp
rm -rf $WKDIR
mkdir -p $WKDIR
cd $WKDIR
cp $COMDIR/SFMT.f90 .
cp $COMDIR/common.f90 .
cp $COMDIR/netlib.f .
cat $COMDIR/netlibblas.f >> ./netlib.f
cp $COMDIR/common_mtx.f90 .
cp $COMDIR/common_letkf.f90 .
cp $L96DIR/model/lorenz96$ORO.f90 .
cp $L96DIR/obs/h_ope.f90 .
cp $CDIR/letkf.f90 .
$F90 -o letkf SFMT.f90 common.f90 netlib.f common_mtx.f90 common_letkf.f90 lorenz96$ORO.f90 h_ope.f90 letkf.f90
rm *.mod
#rm *.o
ln -s $OUTDIR/$OBS/obs.dat .
ln -s $OUTDIR/spinup/init*.dat .
ln -s $OUTDIR/nature.dat .
time ./letkf
rm -rf $OUTDIR/$OBS/$EXP
mkdir -p $OUTDIR/$OBS/$EXP
for FILE in guesmean analmean gues anal infl rmse_t rmse_x
do
if test -f $FILE.dat
then
mv $FILE.dat $OUTDIR/$OBS/$EXP
fi
done
cp $CDIR/*.ctl $OUTDIR/$OBS/$EXP
cp *.ctl $OUTDIR/$OBS/$EXP
cp *.grd $OUTDIR/$OBS/$EXP

cd $OUTDIR/$OBS/$EXP

plot_rms_stats(){
    
    ens_phase="prior"
      rmse_file="rmse_4byte"
    spread_file="spread_4byte"
    
    echo " - Exporting data as netcdf using CDO"
    cdo -f nc import_binary   ${rmse_file}.ctl   ${rmse_file}.nc #&> /dev/null
    cdo -f nc import_binary ${spread_file}.ctl ${spread_file}.nc #&> /dev/null

	echo " - Plotting with NCL"
    figure_name="rms_stats" 
	(cat<<_EOF_
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
; plotting parameters
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
_EOF_
	) >"${figure_name}.ncl"
	ncl ${figure_name}.ncl > /dev/null;
    pdf90 "${figure_name}.pdf" &> /dev/null
    mv "${figure_name}-rotated90.pdf" "${figure_name}.pdf" > /dev/null
    #convert "${figure_name}.pdf" "${figure_name}.jpg"
	
	
}

plot_rms_stats
echo "NORMAL END"
