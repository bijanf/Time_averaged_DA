#!/usr/bin/env bash
#set -x

model_fast_config(){
    funct_opening 2
    export        model
    export      nspinup=100
    export       cycles=240 #00
    export cycle_length=10
    export  Taver_steps=2
#export        ndays=100
#export      dspinup=30
    funct_closing 2
}


model_make_all(){
    funct_opening 2
    echo " Compiling fortran programs for model $model"

    #echo " Finding out right configuration" # (machine dependent)
    #configure
    rm -rf $WKDIR; mkdir -p $WKDIR; cd $WKDIR
    #cd $MODEL_DIR
    local i=1
    programs[$i]="$COM_DAS_DIR/spinup_run.f90";   (( i++ ))
    programs[$i]="$COM_DAS_DIR/nature_run.f90";   (( i++ ))
 #   programs[$i]="$COM_DAS_DIR/nature_obs.f90";   (( i++ ))
    programs[$i]="$COM_DAS_DIR/ensemble_run.f90"; (( i++ ))

    for program_path in ${programs[@]}
    do
        build_program "$program_path"
    done
    funct_closing 2
}


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

