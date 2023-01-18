#!/usr/bin/env bash

model_update_code(){

    # Station network module
    if [[ $obs_operator == 'identity' ]];then
        cat > $CODE_DIR/common/station_data.f90 << EOF
MODULE station_data
  IMPLICIT NONE
  INTEGER,PARAMETER :: ny=4
  INTEGER,PARAMETER :: station_ind (ny)=(/2,3,5,6/)  ! in state_var indices
  INTEGER,PARAMETER :: station_pos (ny)=(/0,0,0,0/)
  INTEGER,PARAMETER :: station_comp(ny)=(/1,1,2,2/)
END MODULE station_data
EOF
    else
        cat > $CODE_DIR/common/station_data.f90 << EOF
MODULE station_data
  IMPLICIT NONE
  INTEGER,PARAMETER :: ny=2
  INTEGER,PARAMETER :: station_ind(ny)=(/2,3/) ! in state_var indices
  INTEGER,PARAMETER :: station_pos(ny)=(/1,1/)
END MODULE station_data
EOF
    fi

    # ensemble_size module
    cat > $CODE_DIR/common/ensemble_size.f90 << EOF
MODULE ensemble_size
  IMPLICIT NONE
  INTEGER,PARAMETER :: nbv=$ensemble_size   ! ensemble size
END MODULE ensemble_size
EOF
}

set_inflation(){

    # if [[ $obs_operator == 'ident_uncoupled' ]];then
    #     coupled_analysis="no"
    # else
    #     coupled_analysis="yes"
    # fi
    # export coupled_analysis
    #export  coupled_analysis="yes"
    # export  coupled_analysis="no"

    # Set inflation according to configuration coupled_analysis flag

    case "$par_set" in
        "uncoupled")
            error "set infl_enkf!!";;
        # Original Pena & Kalnay coupling configurations
        "weather_convection")
            error "set infl_enkf!!";;
        "extratropical_Ocen_atm_strong_coupling")
            error "set infl_enkf!!";;
        "extratropical_Ocen_atm_weak_coupling")
            error "set infl_enkf!!";;
        "enso")
            infl_enkf="1.005";;

        # Tödler et al coupling configurations for shallow-deep soil layers
        "shallow-deep_soil_weak_coupling")
            infl_enkf="1.04" ;;
        "shallow-deep_soil_weak_coupling_minus")
            infl_enkf="1.04" ;;
        "shallow-deep_soil_mid_coupling")
            infl_enkf="1.005" ;;
        "shallow-deep_soil_strong_coupling")
            error "set infl_enkf!!";;

        # My new configurations
        "enso-c0.00")
            infl_enkf="1.02" ;;
	    # case "$comp_localization" in
	    # 	"no" ) infl_enkf="1.06";;
	    # 	"yes") infl_enkf="1.02";;
            #     *    ) error "Unknown comp_localization $comp_localization";;
	    # esac
	    # ;;
        "enso-c0.10")
            infl_enkf="1.02" ;;
        "enso-c0.20")
            echo "set infl_enkf!!" 1>&2;;
        "enso-c0.22")
            echo "set infl_enkf!!" 1>&2;;
        "enso-c0.23")
            echo "set infl_enkf!!" 1>&2;;
        "enso-c0.24")
            echo "set infl_enkf!!" 1>&2;;
        "enso-c0.25")
            echo "set infl_enkf!!" 1>&2;;
        "enso-c0.30")
            echo "set infl_enkf!!" 1>&2;;
        "enso-c0.40")
            echo "set infl_enkf!!" 1>&2;;
        "enso-c0.50")
            infl_enkf="1.02" ;;
        "enso-c0.55")
            echo "set infl_enkf!!" 1>&2;;
        "enso-c0.60")
            echo "set infl_enkf!!" 1>&2;;
        "enso-c0.70")
            echo "set infl_enkf!!" 1>&2;;
        "enso-c0.90")
            echo "set infl_enkf!!" 1>&2;;
        "enso-c1.00")
            infl_enkf="1.04" ;;
        *) error "Unknown par_set $par_set";;
    esac
    export infl_enkf


}
