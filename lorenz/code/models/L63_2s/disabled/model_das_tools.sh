#!/usr/bin/env bash
#set -x

make_all(){
	funct_opening 2

	#echo " Finding out right configuration" # (machine dependent)
  #configure
	rm -rf $WKDIR; mkdir -p $WKDIR; cd $WKDIR
	
	local i=1
	programs[$i]="$COM_DAS_DIR/spinup_run.f90";   (( i++ ))
	programs[$i]="$COM_DAS_DIR/nature_run.f90";   (( i++ ))
#	programs[$i]="$COM_DAS_DIR/nature_obs.f90";   (( i++ ))
#	programs[$i]="$COM_DAS_DIR/ensemble_run.f90"; (( i++ ))

	for program_path in ${programs[@]}
	do
		build_program "$program_path"
	done
	funct_closing 2
}

##=========================================================================
##> @brief Build programs which use Miyoshi's common F90 modules
##>
##> Usage example:
##> $ build_program.sh generate_observations
##=========================================================================
#build_program(){  
  #[[ $# -eq 1 ]] || error "Usage: build_program program_code_fullpath"
  #program_code_fullpath=$1
  #program_name=$(basename "$program_code_fullpath")
	#program_name="${program_name%.*}"
	##	extension="${filename##*.}"
  #program_dir=$(dirname ${program_code_fullpath})

	#echo " - Building program $program_name"
	
	#cp $program_dir/${program_name}.f90 .
  #cp $COM_DIR/SFMT.f90 .
  #cp $COM_DIR/common.f90 .
  #cp $COM_DIR/netlib.f .
  #cat $COM_DIR/netlibblas.f >> ./netlib.f
  #cp $COM_DIR/common_mtx.f90 .
  #cp $COM_DIR/common_letkf.f90 .
  #cp $COM_DIR/common_das_tools.f90 .
  #cp $MODEL_DIR/model_core.f90 .
##  cp $MODEL_DIR/model_obs_operator.f90 .
  #cp $COM_DAS_DIR/common_particle.f90 .
  #cp $COM_DIR/common_ensemble.f90 .
##    cp $MODEL_DIR/model_tools.f90 .
##    cp $MODEL_DIR/da_experiment.f90 .
  
  #$F90 $F90_opt -o ${program_name}.exe common.f90 SFMT.f90 netlib.f \
  #common_mtx.f90 common_letkf.f90 common_das_tools.f90 model_core.f90 \
  #common_particle.f90 common_ensemble.f90 ${program_name}.f90
	
	 ## model_obs_operator.f90 
	 
##	mv ${program_name}.exe $program_dir/
##	cd ..
#}

set_model_nml(){
    funct_opening 1
    echo " - Creating das namelist file"
    dt="0.0050d0"; force="8.0d0" #    namelist      /model_nml/ dt,force

    (cat<<_EOF_
&model_nml
    dt=${dt},
    force=${force},
/
_EOF_
    )>"das_configuration.nml"
    #(cat<<_EOF_
#&model_nml
    #dt=${dt},
    #force=${force},
#/
#&trajectory_nml
 #sixhr_steps_per_leap=$sixhr_steps_per_leap,
 #leaps=${leaps},
 #rand_start=$rand_start,
#/
#_EOF_
    #)>"das_configuration.nml"

#    cat das_configuration.nml

    funct_closing 1
}

#initialize_run(){
	    #dataset_dir=$1
	#fortran_program=$2
	
	#create_archive_dir $dataset_dir || return 0
    #cd $dataset_dir

    #cp $DAS_MODEL_DIR/${fortran_program}.f90 .
    #build_program "$fortran_program"
#}

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
