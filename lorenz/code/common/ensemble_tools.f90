MODULE ensemble_tools
  USE common
  USE common_tools
  USE dynamical_system, ONLY: nx,ny,time,dt,set_model_time,rk4_step,nbv
  USE observation_operator, ONLY: clean_obs
  IMPLICIT NONE

  REAL(r_dble),PROTECTED :: ensemble_state  (nx,nbv)
  INTEGER     ,PRIVATE   :: im,icomp
  TYPE :: ensemble
     REAL(r_dble)   :: state(nx,nbv)
     REAL(r_dble)   :: Emean(nx)
     REAL(r_dble)   :: Esprd(nx)
  END TYPE ensemble
  
  TYPE :: Taugm_ensemble
     REAL(r_dble)   :: state(nx,nbv)
     REAL(r_dble)   :: Emean(nx)
     REAL(r_dble)   :: Esprd(nx)
  END TYPE Taugm_ensemble


CONTAINS

  SUBROUTINE E_stats(E)
    TYPE(ensemble),INTENT(INOUT) :: E
    REAL(r_dble)                 :: E_Eanom(nx,nbv)
    E%Emean =              SUM(E%state   ,2)/ nbv
    E_Eanom = E%state - SPREAD(E%Emean   ,2,  nbv)
    E%Esprd =       SQRT(  SUM(E_Eanom**2,2)/(nbv -1))
  END SUBROUTINE E_stats

  SUBROUTINE inflate(E,infl_factor)
    REAL(r_dble),INTENT(INOUT) :: E(:,:)
    INTEGER                    :: E_size
    REAL(r_dble)               :: E_mean(SIZE(E,1))
    REAL(r_dble)               :: E_anom(SIZE(E,1),SIZE(E,2))
    REAL(r_dble)               :: infl_factor

    E_size = SIZE(E,2)

    E_mean = SUM (E,2)/ E_size
    E_anom = E      - SPREAD(E_mean ,2,  E_size)

    E_anom = E_anom * infl_factor
    E      = E_anom + SPREAD(E_mean ,2,  E_size)
  END SUBROUTINE inflate

  SUBROUTINE inflate_array(E,infl_factor)
    REAL(r_dble),INTENT(INOUT) :: E(:,:,:)
    REAL(r_dble)               :: infl_factor
    INTEGER                    :: ia
    DO ia=1,SIZE(E,3)
       CALL inflate(E(:,:,ia),infl_factor)
    END DO
  END SUBROUTINE inflate_array

  ! SUBROUTINE ensemble_initialize(t0,E0)
  !   REAL(r_dble) :: t0
  !   REAL(r_dble) :: E0(nx,nbv)
  !   CALL set_ensemble_state(E0)
  !   CALL set_model_time(t0)
  ! END SUBROUTINE ensemble_initialize

  SUBROUTINE set_ensemble_time(t0)
    REAL(r_dble) :: t0
    CALL set_model_time(t0)
  END SUBROUTINE set_ensemble_time

  SUBROUTINE set_ensemble_state(Ein)
    REAL(r_dble) :: Ein(nx,nbv)
    ensemble_state(:,:) = Ein(:,:)
  END SUBROUTINE set_ensemble_state

  SUBROUTINE ensemble_step
    DO im=1,nbv
       CALL rk4_step(ensemble_state(:,im))
       ! CALL rk4_step(ensemble_state(:,im),ensemble_state(:,im))
    END DO
    CALL set_model_time(time + dt)
  END SUBROUTINE ensemble_step

  FUNCTION ensemble_clean_obs(E)
    REAL(r_dble),INTENT(IN) :: E(:,:)
    REAL(r_dble)            :: ensemble_clean_obs(ny,SIZE(E,2))
    INTEGER                 :: states_number
    states_number = SIZE(E,2)
    DO im=1,states_number
       ensemble_clean_obs(:,im) = clean_obs(E(:,im))
    END DO
  END FUNCTION ensemble_clean_obs

  FUNCTION ensemble_array_clean_obs(E)
    REAL(r_dble),INTENT(IN) :: E(:,:,:)
    REAL(r_dble)            :: ensemble_array_clean_obs(ny,SIZE(E,2),SIZE(E,3))
    INTEGER                 :: ia !states_number
    ! states_number = SIZE(E,2)
    DO ia=1,SIZE(E,3)
       DO im=1,SIZE(E,2)
          ensemble_array_clean_obs(:,im,ia) = clean_obs(E(:,im,ia))
       END DO
    END DO
  END FUNCTION ensemble_array_clean_obs

  ! FUNCTION ensemble_clean_obs(E)
  !   REAL(r_dble),INTENT(IN) :: E(nx,nbv)
  !   REAL(r_dble)            :: ensemble_clean_obs(ny,nbv)
  !   DO im=1,nbv
  !      ensemble_clean_obs(:,im) = clean_obs(E(:,im))
  !   END DO
  ! END FUNCTION ensemble_clean_obs

END MODULE ensemble_tools

!   SUBROUTINE E_stats(E,E_mean,E_sprd)
!   REAL(r_dble),INTENT(IN)  :: E(nx,nbv)
!   REAL(r_dble),INTENT(OUT) :: E_mean(nx)
!   REAL(r_dble)             :: E_anom(nx,nbv)
!   REAL(r_dble),INTENT(OUT) :: E_sprd(nx)
!   E_mean =        SUM(E,2)/ nbv
!   E_anom = E - SPREAD(E_mean   ,2,  nbv)
!   E_sprd = SQRT(  SUM(E_anom**2,2)/(nbv))
! END SUBROUTINE E_stats
! FUNCTION Emean(E)
!   REAL(r_dble),INTENT(IN) :: E(nx,nbv)
!   REAL(r_dble)            :: Emean(nx)
!   Emean =        SUM(E,2)/ nbv
! END FUNCTION Emean

! FUNCTION Esprd(E,E_mean)
!   REAL(r_dble),INTENT(IN) :: E     (nx,nbv)
!   REAL(r_dble),INTENT(IN) :: E_mean(nx)
!   REAL(r_dble)            :: E_anom(nx,nbv)
!   REAL(r_dble)            :: Esprd (nx)
!   E_anom = E - SPREAD(E_mean   ,2,  nbv)
!   Esprd  = SQRT(  SUM(E_anom**2,2)/(nbv))
! END FUNCTION Esprd
