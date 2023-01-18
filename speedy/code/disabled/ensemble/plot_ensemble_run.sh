#!/usr/bin/env bash
#set -x
#=======================================================================
#> @brief Add info
#plot_free_ensemble_run free-run_0006hr_0002ts_m20_samp2
#=======================================================================
plot_ensemble_run(){
 	[[ $# -eq 1 ]] || error "Usage: plot_ensemble_run ens_run_name"
	ens_run_name=$1
    echo "==============================================="
    echo " Plotting ensemble run"
    echo " $ens_run_name"
    echo "==============================================="
#    visual="yes"
    visual="no"
    
    storing_dir=$ARCHIVE_DIR
    ens_run_dir=${storing_dir}/${ens_run_name}
    
    echo " - Parsing Arguments"
    [ -d   $ens_run_dir ] || error "There is no ens run dataset '$ens_run_name' in $storing_dir"
    cd ${ens_run_dir}
    [ -f dataset_complete.dat ] || error "Ensemble Run dataset is not complete"
    
    echo " - Reading ensemble run metainfo"
      nature_name=$(cat nature_name.dat)
     dataset_type=$(cat dataset_type.dat)
           A_mode=$(cat A_mode.dat)
        time_step=$(cat time_step.dat)
    
    echo "   dataset_type      = $dataset_type"
    echo "   Augmentation_mode = $A_mode"

    if [ $dataset_type == "assim-run" ]; then
        free_run_name=$(cat free_run_name.dat)
        free_run_dir=${storing_dir}/${free_run_name}
    fi
    
      nature_dir=${storing_dir}/${nature_name}

#            plot_error "prior" "Insta"
            plot_error "prior" "Taver"
    
    if [ $dataset_type == "assim-run" ]; then
    
#            plot_error "postr" "Insta"
#            calculate_MeanSquareSkillScore "postr" "Insta"
            
        if [ $A_mode == "Taver" ]; then
        
            plot_error "postr" "Taver"
#            calculate_MeanSquareSkillScore "postr" "Taver"
        fi
    fi

    echo "==============================================="
    echo " Sucessful Plotting"
    echo "==============================================="

    return 0 # Success
}
    
plot_error(){
    [[ $# -eq 2 ]] || error "Usage: plot_error ens_phase (prior|postr) state_kind (Insta|Taver)"
     local ens_phase=$1 # prior or postr
    local state_kind=$2 # Insta or Taver

    cd $ens_run_dir/$ens_phase

#    calculate_ensemble_mean_error

#    calculate_RootMeanSquareErrors

    error_var_file="ens_mean_error_var_${state_kind}.nc"
    calculate_error_variance $error_var_file
    plot_error_variance $error_var_file "t"  "0" "0" "15"  "0.5"
#    plot_error_variance $error_var_file "ps" "0" "0" "300" "3"
#    plot_error_variance $error_var_file "q"  "0" "0" "0.5" "0.01"

    
#    if [ $dataset_type == "assim-run" ]; then
#        calculate_MeanSquareSkillScore $ens_phase $state_kind
#    fi

    return 0
}

calculate_ensemble_mean_error(){
    
    echo " - Calculating $ens_phase $state_kind ensemble mean error"
    speedy2nc.sh mean/YYYYMMDDHH_grid_sigma_${state_kind}.ctl &> /dev/null

    rm -f nature_run.nc free_run.nc
    ln -s $nature_dir/states/YYYYMMDDHH_grid_sigma_${state_kind}.nc nature_run.nc
    ln -s $ens_run_dir/$ens_phase/mean/YYYYMMDDHH_grid_sigma_${state_kind}.nc free_run.nc
    cdo sub nature_run.nc free_run.nc ens_mean_error_${state_kind}.nc &> /dev/null
    rm nature_run.nc free_run.nc
    [ $visual == "yes" ] && ncview ens_mean_error_${state_kind}.nc &> /dev/null &

    return 0
}

calculate_RootMeanSquareErrors(){
    echo " - Calculating ensemble mean RMSE"
    
    calculate_RMSE    "t" "Temperature"    "0.95" "K"
    calculate_RMSE    "u" "u-wind"         "0.51" "m/s"
    calculate_RMSE    "v" "v-wind"         "0.51" "m/s"
    calculate_RMSE    "q" "Spec. humidity" "0.51" "kg/kg"
    calculate_RMSE   "ps" "Surf. pressure" "none" "Pa"
    #calculate_RMSE "rain" "Precipitation." "none" "mm/??hr"
    #           var_name | long_var_name | sigma_level | new_unit
    return 0
}

calculate_RMSE(){
    [[ $# -eq 4 ]] || error "Usage: calculate_RMSE var_name long_var_name sigma_level new_unit"
         var_name=$1
    long_var_name=$2
      sigma_level=$3
         new_unit=$4
        
     new_var_name="${var_name}_rmse"
    new_var_label="$ens_phase $state_kind $long_var_name RMSE at sigma = $sigma_level"
        rmse_file="ens_mean_RMSE_${var_name}_sig${sigma_level}.nc"
    
    case $var_name in
#    "ps"|"rain")     # Surface pressure or Precipitation (2D fields)
    "ps")            # Surface pressure
        new_var_label="$ens_phase $state_kind $long_var_name RMSE"
            rmse_file="ens_mean_RMSE_${var_name}.nc"
        cdo chname,$var_name,$new_var_name -sqrt -fldmean -sqr -selname,$var_name ens_mean_error.nc $rmse_file &> /dev/null;;
    "t"|"u"|"v"|"q") # temperature, zonal wind, meridional wind or specific humidity (3D fields)
        new_var_label="$ens_phase $state_kind $long_var_name RMSE at sigma = $sigma_level"
            rmse_file="ens_mean_RMSE_${var_name}_sig${sigma_level}.nc"
        cdo chname,$var_name,$new_var_name -sqrt -fldmean -sqr -sellevel,$sigma_level -selname,$var_name ens_mean_error.nc $rmse_file &> /dev/null;;
    *)
        error "Unsupported variable $var_name";;
    esac
        
    ncatted -a long_name,"$new_var_name",o,c,"$new_var_label" $rmse_file
    ncatted -O -a units,"$new_var_name",c,c,$new_unit $rmse_file
    [ $visual == "yes" ] && ncview $rmse_file &> /dev/null &
    
    return 0
}

calculate_error_variance(){
    local error_var_file=$1
    echo "calculating error variance"    
#        cdo timvar1 ens_mean_error_${state_kind}.nc $error_var_file #&> /dev/null &
#        cdo timselvar1,224,32 ens_mean_error_${state_kind}.nc $error_var_file #&> /dev/null &
#        cdo timselvar1,112,16 ens_mean_error_${state_kind}.nc $error_var_file #&> /dev/null &
    cdo timselvar1,90,10 ens_mean_error_${state_kind}.nc $error_var_file #&> /dev/null &
    ncatted -a long_name,"t",o,c,"Temperature Error Variance" "$error_var_file"
    ncatted -a long_name,"u",o,c,"Zonal wind Error Variance" "$error_var_file"
    ncatted -a long_name,"v",o,c,"Meridional wind Error Variance" "$error_var_file"
    ncatted -a long_name,"ps",o,c,"Surface pressure Error Variance" "$error_var_file"
    ncatted -a long_name,"q",o,c,"specific humidity Error Variance" "$error_var_file"

       
    [ $visual == "yes" ] && ncview $error_var_file &> /dev/null &
    return 0
}

calculate_MeanSquareSkillScore(){
    #[[ $# -eq 2 ]] || error "Usage: calculate_MeanSquareSkillScore ens_phase (prior|postr) state_kind (Insta|Taver)"
    #local  ens_phase=$1 # prior or postr
    #local state_kind=$2 # Insta or Taver
    
    echo "calculating $ens_phase $state_kind MSSS"   
    
    #cd $ens_run_dir/$ens_phase

    MSSS_file="MSSS_${ens_phase}_${state_kind}.nc"
    ln -fs $free_run_dir/prior/ens_mean_error_var_${state_kind}.nc free_run_variance_${ens_phase}_${state_kind}.nc
    cdo addc,1 -mulc,-1 -div $error_var_file free_run_variance_${ens_phase}_${state_kind}.nc $MSSS_file &> /dev/null
    ncatted -a long_name,"t",o,c,"Temperature MSSS" "$MSSS_file"
    ncatted -a long_name,"u",o,c,"Zonal wind MSSS" "$MSSS_file"
    ncatted -a long_name,"v",o,c,"Meridional wind MSSS" "$MSSS_file"
    ncatted -a long_name,"ps",o,c,"Surface pressure MSSS" "$MSSS_file"
    ncatted -a long_name,"q",o,c,"specific humidity MSSS" "$MSSS_file"
    
    plot_skill $MSSS_file "t" 0
    plot_skill $MSSS_file "t" 5
    plot_skill $MSSS_file "ps" 0
    
#    ncview $MSSS_file &> /dev/null &
    return 0
}

plot_skill(){
    [[ $# -eq 3 ]] || error "Usage:"
    local MSSS_file=$1
	 local var_name=$2
        local level=$3
	
	echo "================================================"
    echo " Creating pdf and jpg MSSS plots"
	echo "================================================"

    title="Time averaging period = $((time_step/24)) days"
        case $var_name in
#    "ps"|"rain")     # Surface pressure or Precipitation (2D fields)
    "ps")            # Surface pressure
        import_line="var = a->${var_name}(0,:,:)"
        figure_name="MSSS_${ens_phase}_${state_kind}_${var_name}"
        ;;        
    "t"|"u"|"v"|"q") # temperature, zonal wind, meridional wind or specific humidity (3D fields)
        import_line="var = a->${var_name}(0,${level},:,:)"
        figure_name="MSSS_${ens_phase}_${state_kind}_${var_name}_level${level}"
        ;;
    *)
        error "Unsupported variable $var_name";;
    esac
  
	echo " - Creating ncl script"
	(cat<<_EOF_
;*************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
;************************************************
begin
;************************************************
; read in netCDF file
;************************************************
  a = addfile("${MSSS_file}","r")
;************************************************
; read in surface temperature
;************************************************
  $import_line
;************************************************
; create plot
;************************************************
  wtype                = "pdf"
  wtype@wkPaperWidthF  = 10
  wtype@wkPaperHeightF = 16
  wtype@wkOrientation  = "landscape"          

  wks = gsn_open_wks(wtype,"${figure_name}")
  
  gsn_define_colormap(wks,"BlueYellowRed")
  gsn_reverse_colormap(wks)

  res                   = True
  res@mpProjection      = "CylindricalEqualArea"; choose projection
  res@mpGridAndLimbOn   = True                  ; turn on lat/lon lines
  res@mpPerimOn         = False                 ; turn off box around plot
  res@mpGridLatSpacingF = 30.                   ; spacing for lat lines
  res@mpGridLonSpacingF = 30.                   ; spacing for lon lines
  res@mpFillOn          = False
 
  res@cnFillOn          = True                  ; color plot desired
  res@cnLinesOn          = False                ; color plot desired
  res@cnLineLabelsOn    = False                 ; turn off contour lines
  res@txFontHeightF     = 0.015 


  res@cnLevelSelectionMode="ManualLevels"
  res@cnMinLevelValF=-1
  res@cnMaxLevelValF=1
  res@cnLevelSpacingF=0.05
  
  res@gsnPaperOrientation="auto"
  res@gsnMaximize=True

  res@lbLabelFontHeightF  = 0.015           ; label bar font height

  res@tiMainString       = "$title"  ; add a title
  res@tiMainFontHeightF  = .018                               ; font height
  contour = gsn_csm_contour_map(wks,var,res)  ; create the plot
end
_EOF_
	)>"plot_skill.ncl"

  	echo " - Calling ncl"
	ncl plot_skill.ncl > /dev/null;
     rm plot_skill.ncl
    
    pdf90 "${figure_name}.pdf" &> /dev/null
    mv "${figure_name}-rotated90.pdf" "${figure_name}.pdf" > /dev/null
    convert "${figure_name}.pdf" "${figure_name}.jpg"
	
	echo " - Successful end"
	echo "================================================"
	
	return 0 # Success
}

plot_error_variance(){
    [[ $# -eq 6 ]] || error "Usage:"
    local ens_error_var_file=$1
              local var_name=$2
                 local level=$3
                  local min_value=$4
                local max_value=$5
               local value_spacing=$6
	
	echo "================================================"
    echo " Creating pdf and jpg Error Variance plots"
	echo "================================================"

    title="Time averaging period = $((time_step/24)) days"
        case $var_name in
#    "ps"|"rain")     # Surface pressure or Precipitation (2D fields)
    "ps")            # Surface pressure
        import_line="var = a->${var_name}(0,:,:)"
        figure_name="Error_var_${ens_phase}_${state_kind}_${var_name}"
        ;;        
    "t"|"u"|"v"|"q") # temperature, zonal wind, meridional wind or specific humidity (3D fields)
        import_line="var = a->${var_name}(0,${level},:,:)"
        figure_name="Error_var_${ens_phase}_${state_kind}_${var_name}_level${level}"
        ;;
    *)
        error "Unsupported variable $var_name";;
    esac
  
	echo " - Creating ncl script"
	(cat<<_EOF_
;*************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
;************************************************
begin
;************************************************
; read in netCDF file
;************************************************
  a = addfile("${ens_error_var_file}","r")
;************************************************
; read in surface temperature
;************************************************
  $import_line
;************************************************
; create plot
;************************************************
  wtype                = "pdf"
  wtype@wkPaperWidthF  = 10
  wtype@wkPaperHeightF = 16
  wtype@wkOrientation  = "landscape"          

  wks = gsn_open_wks(wtype,"${figure_name}")
  
  gsn_define_colormap(wks,"BlGrYeOrReVi200")

  res                   = True
  res@mpProjection      = "CylindricalEqualArea"; choose projection
  res@mpGridAndLimbOn   = True                  ; turn on lat/lon lines
  res@mpPerimOn         = False                 ; turn off box around plot
  res@mpGridLatSpacingF = 30.                   ; spacing for lat lines
  res@mpGridLonSpacingF = 30.                   ; spacing for lon lines
  res@mpFillOn          = False
 
  res@cnFillOn          = True                  ; color plot desired
  res@cnLinesOn          = False                ; color plot desired
  res@cnLineLabelsOn    = False                 ; turn off contour lines
  res@txFontHeightF     = 0.015 


  res@cnLevelSelectionMode="ManualLevels"
  res@cnMinLevelValF=$min_value
  res@cnMaxLevelValF=$max_value
  res@cnLevelSpacingF=$value_spacing

  
  res@gsnPaperOrientation="auto"
  res@gsnMaximize=True

  res@lbLabelFontHeightF  = 0.015           ; label bar font height

  res@tiMainString       = "$title"  ; add a title
  res@tiMainFontHeightF  = .018                               ; font height
  contour = gsn_csm_contour_map(wks,var,res)  ; create the plot
end
_EOF_
	)>"plot_error_var.ncl"
  
  	echo " - Calling ncl"
	ncl plot_error_var.ncl > /dev/null;
     rm plot_error_var.ncl
    
    pdf90 "${figure_name}.pdf" &> /dev/null
    mv "${figure_name}-rotated90.pdf" "${figure_name}.pdf" > /dev/null
    convert "${figure_name}.pdf" "${figure_name}.jpg"
	
	echo " - Successful end"
	echo "================================================"
	
	return 0 # Success
}


set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

plot_ensemble_run $@
exit $?


# old stuff
#calculate_RMSE(){
    ## temperature
    #cdo chname,t,t_rmse -sqrt -fldmean -sqr -sellevel,0.95 -selname,t misfit.nc rmse_t_sig095.nc > /dev/null 2>&1
    #ncatted -a long_name,t_rmse,o,c,"Temperature RMSE at sigma=0.95" rmse_t_sig095.nc
    #ncatted -O -a units,t_rmse,c,c,"K" rmse_t_sig095.nc
    #ncview rmse_t_sig095.nc > /dev/null 2>&1 &
    ## zonal wind
    #cdo chname,u,u_rmse -sqrt -fldmean -sqr -sellevel,0.51 -selname,u misfit.nc rmse_u_sig051.nc > /dev/null 2>&1
    #ncatted -a long_name,u_rmse,o,c,"U-wind RMSE at sigma=0.51" rmse_u_sig051.nc
    #ncatted -O -a units,u_rmse,c,c,"m/s" rmse_u_sig051.nc
    #ncview rmse_u_sig051.nc > /dev/null 2>&1 &

    ## meridional wind
    #cdo chname,v,v_rmse -sqrt -fldmean -sqr -sellevel,0.51 -selname,v misfit.nc rmse_v_sig051.nc > /dev/null 2>&1
    #ncatted -a long_name,v_rmse,o,c,"V-wind RMSE at sigma=0.51" rmse_v_sig051.nc
    #ncatted -O -a units,v_rmse,c,c,"m/s" rmse_v_sig051.nc
    #ncview rmse_v_sig051.nc > /dev/null 2>&1 &

    ## specific humidity
    #cdo chname,q,q_rmse -sqrt -fldmean -sqr -sellevel,0.51 -selname,q misfit.nc rmse_q_sig051.nc > /dev/null 2>&1
    #ncatted -a long_name,q_rmse,o,c,"Specific humidity RMSE at sigma=0.51" rmse_q_sig051.nc
    #ncatted -O -a units,q_rmse,c,c,"kg/kg" rmse_q_sig051.nc
    #ncview rmse_q_sig051.nc > /dev/null 2>&1 &

    ## Surface pressure
    #cdo chname,ps,ps_rmse -sqrt -fldmean -sqr -selname,ps   misfit.nc rmse_ps.nc > /dev/null 2>&1
    #ncatted -a long_name,ps_rmse,o,c,"Surface pressure RMSE" rmse_ps.nc
    #ncatted -O -a units,ps_rmse,c,c,"Pa" rmse_ps.nc
    #ncview rmse_ps.nc > /dev/null 2>&1  &
    ## Precipitation
##    cdo chname,rain,rain_rmse -sqrt -fldmean -sqr -selname,rain misfit.nc rmse_rain.nc > /dev/null 2>&1
##    ncatted -a long_name,rain_rmse,o,c,"Precipitation RMSE" rmse_rain.nc
##    ncatted -O -a units,rain_rmse,c,c,"mm/6hr" rmse_rain.nc
##    ncview rmse_rain.nc > /dev/null 2>&1  &
#}
