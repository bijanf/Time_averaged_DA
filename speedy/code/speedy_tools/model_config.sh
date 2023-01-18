#!/usr/bin/env bash

#=======================================================================
# speedy-TA EnKF configuration
#=======================================================================
model_config(){
    funct_opening 3

    default_config

    #addi_info="__m${ensemble_size}"
    #exp_dir(){ echo "$STORE/$model/$par_set/${exp_name}${addi_info}";}

    [[ $clone == yes ]] && clone_code 
    
    #if [[ $clone == yes ]];then
        #create_code_copy "$(exp_dir)"
        #cd "$(exp_dir)/code"
        #source ./initialize_das.sh
    #fi
    source "$MODEL_DIR/model_tools.sh"

    if [[ $build == yes ]];then
        ( das_make_all $model )
    fi

    funct_closing 3
}

default_config(){
    echo " setting Observation operator parameters"

    station_set_number=${station_set_number:-8}
#     1) empty_station_set;;
#     2) homogeneous_station_set 0 ;;
#     3) homogeneous_station_set 1 ;;
#     4) homogeneous_station_set 2 ;;
#     5) homogeneous_station_set 3 ;;
#     6) homogeneous_station_set 4 ;;
#     7) max_tree_network          ;;
#     8) Breitenmosser_network     ;;
    export station_set_number

    obs_operators[0]='T'
    obs_operators[1]='norm_T'
    obs_operators[2]='norm_M'
    obs_operators[3]='norm_add'
    obs_operators[4]='norm_min'
    obs_operators[5]='norm_prod'
    obs_operators[6]='ramp_T'
    obs_operators[7]='ramp_M'
    obs_operators[8]='ramp_add'
    obs_operators[9]='ramp_prod'
    #obs_operators[4]='Resp_product'
    #obs_operators[5]='Resp_lukasiewicz'
    #obs_operators[6]='Resp_yager'

    export obs_operator=${obs_operator:-${obs_operators[3]}}
    #export obs_operator=${obs_operators[$3]}

    #export          eta="0.5" # slow-fast mix parameter (0:slow-1:fast) 
    #export  range_upper="1.5" # in standard deviations
    #export  range_lower="1.5" # in standard deviations
    # export  range_upper="3.0" # in standard deviations
    # export  range_lower="3.0" # in standard deviations

    export          SNR="10.0"

    #------------------------------
    # Filter config
    #------------------------------
    case "$verity" in
        0)  export ensemble_size=4  ;;
        *)  export ensemble_size=24 ;;
        #*)  export ensemble_size=4 ;;
    esac

    #------------------------------
    # Diagnostic Statistics
    #------------------------------
    #export diag=("Zmean" "Lev-1" "Lev-1_Hmean")

    #-----------------------#
    # Trajectory parameters #
    #-----------------------#
    # The spinup_start should be defined here !!!
     export spinup_start=1978010100;
   #  export spinup_start=1860010100; # [modified by Bijan] 

    case "$verity" in
        0)  sampling_step=6; sampling_step_unit="hr" ;;
		*)  sampling_step=2; sampling_step_unit="dy" ;; # we should find the proper one
	esac
	export sampling_step
	export sampling_step_unit


    case "$verity" in
        0)
            spinup_start=1978120100;
            spinup_length=1; spinup_length_unit="mo"
            SST="clim_81_90"
            surf_mode="prescribed" 
            sampling_step=6; sampling_step_unit="hr"
#             spinup_length=24; spinup_length_unit="hr"
#             run_length=24;       run_length_unit="hr"
#             cycle_length=6;    cycle_length_unit="hr"
#             span[cycle_length]="(/6,12/)"; unit[cycle_length]="hr"
            run_length=3;       run_length_unit="mo"
            cycle_length=1;    cycle_length_unit="mo"
            span[cycle_length]="(/1/)"; unit[cycle_length]="mo"
            ;;
        1)
            spinup_length=12; spinup_length_unit="mo"
            run_length=60;       run_length_unit="mo"
            cycle_length=12;    cycle_length_unit="mo"

            unit[cycle_length]="mo"
#            span[cycle_length]="(/0.033,0.25,0.5,1,2,3/)" #modified by Bijan 
            span[cycle_length]="(/1,2,3,4,6,12/)";#modified by Bijan
#           span[cycle_length]="(/0.033,0.250,1,2,3,4,6,12/)"
            ;;
        2)
            spinup_length=12; spinup_length_unit="mo"
            run_length=120;      run_length_unit="mo"
            cycle_length=1;    cycle_length_unit="mo"
            unit[cycle_length]="mo"
            SST="50_99"
#            span[cycle_length]="(/0.033,0.25,0.5,1,2,3/)"
#            span[cycle_length]="(/1,2,3,4,6,12/)";
            span[cycle_length]="(/1,3,6,12/)";
#           span[cycle_length]="(/0.033,0.250,1,2,3,4,6,12/)"
            ;;
        3)
            spinup_start=1950010100; SST="50_99"
	        spinup_length=12; spinup_length_unit="mo"
            run_length=360;      run_length_unit="mo"
            cycle_length=12;    cycle_length_unit="mo"
            unit[cycle_length]="mo"; span[cycle_length]="(/1,3,6,12/)";
            ;;
        4)
            spinup_length=12; spinup_length_unit="mo"
            run_length=1200;     run_length_unit="mo"
            cycle_length=1;    cycle_length_unit="mo"

            span[cycle_length]="(/1,2,3,4,6,12/)"; unit[cycle_length]="mo"
#           span[cycle_length]="(/0.033,0.250,1,2,3,4,6,12/)"
            ;;
        5)  spinup_start=1860010100; SST="1854_2010"
	        spinup_length=12; spinup_length_unit="mo"
            run_length=1800;      run_length_unit="mo"
            cycle_length=12;    cycle_length_unit="mo"
            unit[cycle_length]="mo"; span[cycle_length]="(/1,3,6,12/)";
            surf_mode="slab_model";
            #surf_mode="slab_plus_pres";
            ;;
        6)	spinup_start=1860010100; SST="1854_2010"
	        spinup_length=12; spinup_length_unit="mo"
            run_length=1800;      run_length_unit="mo"
            cycle_length=12;    cycle_length_unit="mo"
            unit[cycle_length]="mo"; span[cycle_length]="(/1,3,6,12/)";
            surf_mode="slab_model";
            ;;    
        
    esac

    export sampling_size=$ensemble_size;
    export sampling_step;   export sampling_step_unit
    export spinup_length;   export spinup_length_unit
    export run_length;         export run_length_unit
    export cycle_length;     export cycle_length_unit
	export surf_mode;		export SST # BIJAN
    echo " Setting ensemble size"

    cat > $CODE_DIR/common/ensemble_size.f90 << EOF
MODULE ensemble_size
IMPLICIT NONE
INTEGER,PARAMETER :: nbv=$ensemble_size   ! ensemble size
END MODULE ensemble_size
EOF

    echo " Setting SPEEDY Parameters"
    
    #SSTs=("1854_2010" "clim_81_90" "79_90" "50_99" "el_nino") 
    #export SST=${SST:-"clim_81_90"} 
    #export SST=${SST:-"1854_2010"} # BIJAN
    case "$SST" in
         "clim_81_90")                          IASST=0;;
         "1854_2010"|"79_90"|"50_99"|"el_nino") IASST=1;;
         *) error "Unknown SST $SST";;
    esac

    #surf_modes=("prescribed" "slab_model")
    #export surf_mode=${surf_mode:-"prescribed"}
    case "$surf_mode" in
         "prescribed"    ) IALST=0; IAICE=0  ;;
         "slab_model"    ) IALST=1; IAICE=1; IASST=2;;
         "slab_plus_pres") IALST=1; IAICE=1; IASST=3;;
         *) error "Unknown surf_mode $surf_mode";;
    esac

    cat > $CODE_DIR/speedy_ver32/update/cls_instep.h << EOF
      NSTEPS = 36     ! Number of time steps in one day

C      NSTDIA = 36*30 ! Diagnostic print-out period
      NSTPPR = 6     ! Post-processing period
      NSTOUT = 36*30
C      IDOUT  = 1
C      IPOUT  = 1     ! pressure level gridded output flag
C      NMONRS = 3

      ISEASC = 1      ! Seasonal cycle flag (0:no, 1:yes)

      NSTRAD = 3      ! Short wave radiation period
      NSTRDF = 0      ! Random diabatic forcing duration
C    (0: none, >= 0: # of initial steps, < 0: whole integration  )      
      INDRDF = 0      ! Random diabatic forcing initialization index

C     surface temperature anomaly flags

      IALST  = $IALST ! for land-surface temp  (0:no, 1:slab-model)
      IASST  = $IASST ! for  sea surface temp  (0:no, 1:prescribed, 2:slab-model, 3:prescribed + slab-model)
      IAICE  = $IAICE ! for sea-ice            (0:no, 1:slab-model)

      ISST0  = 25     ! Record in SST anomaly file corr. to initial month
C--
C--   Logical flags (common LFLAG1)
C      LPPRES = .true.
EOF


#    export diag=("Zmean" "Lev-1" "Lev-1_Hmean")

    #----------------------
    # Sampling parameters
    #----------------------
#     export sampling_period=50
    # cycles=100

#     cycle_steps=5
#     product $dt $cycle_steps cycle_length
#     export cycle_length
#
#     # export   Taver_mode="tied"
#
#     export   Taver_mode="loose";
#     Taver_steps=4; product $dt $Taver_steps Taver_length
#     export Taver_length
#
#     case "$verity" in
#         0)  cycles_step=10       ;;
#         1)  cycles_step=100      ;;
#         2)  cycles_step=500      ;;
#         3)  cycles_step=20000     ;;
#         *) error "Unknown verity level $verity";;
#     esac
#     export   spinup_cycles=$cycles_step
#     export          cycles=$((cycles_step * 10))
#     export     cycles_span="ispan($spinup_cycles + $cycles_step,$cycles,$cycles_step)"
#
#     #----------
#     # Filter
#     #----------
#     export        filter="enkf"
#     export     infl_mode="cycle" # inflation mode ("step_","cycle","post_")
#     export       infl_enkf="1.1"
#     export infl_cycle_length_scaling="none"
#
#
#     #-------------------------------
#     # Time-averaged updating mode
#     export     update_mode="Hakim"
#     #-------------------------------

}

model_config
