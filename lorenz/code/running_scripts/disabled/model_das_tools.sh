#!/usr/bin/env bash
#set -x
#=======================================================================
# Lorenz96 model with time periodic forcing
#=======================================================================
model_fast_config(){
    funct_opening 1
#    export        model
    export  F_mean="8.0d0"
    export  F_ampl="1.0d0"
    export   F_tau="0.5d0"

    export      nspinup=100
    export       cycles=240 #00
    export cycle_length=10
    export  Taver_steps=2

    export  update_mode="Augm1" #  'Hakim' or 'Augm1'
    export          SNR="10"
    export       detail="1"

#export        ndays=100
#export      dspinup=30
    funct_closing 1
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

