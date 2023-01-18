#!/usr/bin/env bash

model_update_code(){

    # Station network module
    case "$n_obs" in
        40) cat > $CODE_DIR/common/station_data.f90 << EOF
MODULE station_data
  IMPLICIT NONE
  INTEGER,PROTECTED :: j_pos
  INTEGER,PARAMETER :: ny = 40
  INTEGER,PARAMETER :: station_ind (ny)=(/(j_pos, j_pos=1,ny)/)
  INTEGER,PARAMETER :: station_pos (ny)=(/(j_pos, j_pos=1,ny)/)
  INTEGER,PARAMETER :: station_comp(ny)=(/(      0, j_pos=1,ny)/)
END MODULE station_data
EOF
	    ;;
        20) cat > $CODE_DIR/common/station_data.f90 << EOF
MODULE station_data
  IMPLICIT NONE
  INTEGER,PROTECTED :: j_pos
  INTEGER,PARAMETER :: ny = 40/2
  INTEGER,PARAMETER :: station_ind (ny)=(/(j_pos*2, j_pos=1,ny)/)
  INTEGER,PARAMETER :: station_pos (ny)=(/(j_pos*2, j_pos=1,ny)/)
  INTEGER,PARAMETER :: station_comp(ny)=(/(      0, j_pos=1,ny)/)
END MODULE station_data
EOF
	    ;;
        *) error "Unknown n_obs $n_obs";;
    esac

    # ensemble_size module
    cat > $CODE_DIR/common/ensemble_size.f90 << EOF
MODULE ensemble_size
  IMPLICIT NONE
  INTEGER,PARAMETER :: nbv=$ensemble_size   ! ensemble size
END MODULE ensemble_size
EOF


#     if [[ $obs_operator == 'ident' ]];then
#     else
#         cat > $CODE_DIR/common/station_data.f90 << EOF
# MODULE station_data
#   IMPLICIT NONE
#   INTEGER,PARAMETER :: ny=2
#   INTEGER,PARAMETER :: station_ind(ny)=(/2,3/) ! in state_var indices
#   INTEGER,PARAMETER :: station_pos(ny)=(/1,1/)
# END MODULE station_data
# EOF
#     fi

}

set_inflation(){
    # echo "Already set in set pars";
    true
}
