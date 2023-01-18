!=======================================================================
!> @brief TADA (Time-average data assimilation) experiment module
!=======================================================================
MODULE common_ta_filter
  USE common
  USE common_das_tools
  USE common_letkf
  USE common_dyn_system
  USE common_obs_operator
  USE common_trajectory
  USE common_filter

  IMPLICIT NONE
  REAL(r_dble),PROTECTED :: ensemble_state  (nx,nbv)
  INTEGER     ,PRIVATE   :: im,icomp
  INTEGER                :: detail      ! Output storage level
  INTEGER                :: Taver_steps ! in time steps (1: instantaneous obs)
  REAL(r_dble)           :: infl_factor_ini
  CHARACTER(5)           :: loc_mode    ! localization mode ('fixed','adapt','both')
  !  INTEGER               :: detail    ! Output storage level
  !                                       >= O : only overall diagnostics
  !                                       >= 1 : ensemble statistics
  !                                       >= 2 : ensemble members
  REAL(r_dble) :: nature_state_Insta    (nx,  1)
  REAL(r_dble) :: nature_state_Taver    (nx,  1)
  REAL(r_dble) ::  prior_state_Insta    (nx,nbv)
  REAL(r_dble) ::  prior_state_Taver    (nx,nbv)
  REAL(r_dble) ::  postr_state_Insta    (nx,nbv)
  REAL(r_dble) ::  postr_state_Taver    (nx,nbv)
  REAL(r_dble) :: nature_obs_clean_Insta(ny,  1)
  REAL(r_dble) :: nature_obs_clean_Taver(ny,  1)
  REAL(r_dble) :: nature_obs_dirty_Taver(ny,  1)
  REAL(r_dble) ::  prior_obs_clean_Taver(ny,nbv)

  REAL(r_dble) :: prior_Emean_Insta(nx)
  REAL(r_dble) :: prior_Emean_Taver(nx)
  REAL(r_dble) :: prior_Insta_Esprd
  REAL(r_dble) :: prior_Taver_Esprd

  REAL(r_dble) :: postr_Emean_Insta(nx)
  REAL(r_dble) :: postr_Emean_Taver(nx)
  REAL(r_dble) :: postr_Insta_Esprd
  REAL(r_dble) :: postr_Taver_Esprd


  REAL(r_dble),ALLOCATABLE :: &
       &       nature_state_Insta_array(:,:,:), &
       &        prior_state_Insta_array(:,:,:), &
       &        postr_state_Insta_array(:,:,:), &
       &   nature_obs_clean_Insta_array(:,:,:), &
       &    prior_obs_clean_Insta_array(:,:,:)
  INTEGER,PRIVATE     :: ita,ix

CONTAINS

  !=======================================================================
  !> @brief Configure TADA experiment
  !=======================================================================
  SUBROUTINE ta_filter_initialize
    CALL print_line
    CALL set_int_par_file('nbv',nbv)
    CALL print_line

    CALL print_line
    PRINT*,' Time-average DA exp. config'
    CALL print_line

    ! current hardwiring
    Taver_steps = cycle_steps
    CALL set_int_par_file('Taver_steps',Taver_steps)
    !CALL get_int_par_env('Taver_steps',Taver_steps)

    CALL print_line
    !---------------------------------------------------------------------
    ! Parameter parsing
    !---------------------------------------------------------------------
    IF(Taver_steps.GT.cycle_steps) &
         & CALL error('Time-average period larger than cycle length')
    ! IF(.NOT.((detail.EQ.1).OR.(detail.EQ.2)))THEN
    !    PRINT*,' Detail = ',detail
    !    CALL error('Unsupported detail level ')
    ! END IF

  END SUBROUTINE ta_filter_initialize

  !=======================================================================
  !> @brief  Hakim's Time-averaged filtering approach
  !=======================================================================
  SUBROUTINE update_Hakim
    REAL(r_dble)::  prior_state_Tanom    (nx,nbv)

    ! Ensemble Time average decomposition
    prior_state_Tanom = &
         prior_state_Insta_array(:,:,Taver_steps) - prior_state_Taver

    CALL filter_find_analysis(prior_state_Taver, prior_obs_clean_Taver, &
         &               nature_obs_dirty_Taver,     postr_state_Taver)

    ! Ensemble Time average Recomposition
    postr_state_Insta = postr_state_Taver + prior_state_Tanom

    WRITE(470) REAL(SUM(infl_factor)/nx,r_sngl)

  END SUBROUTINE update_Hakim

  !=======================================================================
  !> @brief Time Augmented ensemble (~4D-EnKF)
  !=======================================================================
  SUBROUTINE update_Augm1
    REAL(r_dble),ALLOCATABLE::  prior_state_Augm1 (:,:)
    REAL(r_dble),ALLOCATABLE::  postr_state_Augm1 (:,:)
    INTEGER iaugm,im

    ALLOCATE(prior_state_Augm1 (nx*Taver_steps,nbv))
    ALLOCATE(postr_state_Augm1 (nx*Taver_steps,nbv))

    ! Ensemble time augmentation
    DO im=1,nbv
       iaugm=0
       DO ita=1,Taver_steps
          DO ix=1,nx
             iaugm=iaugm+1
             prior_state_Augm1(iaugm,im) = prior_state_Insta_array(ix,im,ita)
          END DO
       END DO
    END DO

    CALL filter_find_analysis(prior_state_Augm1,prior_obs_clean_Taver, &
         &               nature_obs_dirty_Taver,    postr_state_Augm1)

    ! Ensemble time deaugmentation
    DO im=1,nbv
       DO iaugm=1,Taver_steps*nx
          ix  = MODULO(iaugm-1,nx)+1; ita = CEILING(iaugm/REAL(nx))
          postr_state_Insta_array(ix,im,ita) = postr_state_Augm1(iaugm,im)
       END DO
    END DO

    postr_state_Taver = SUM(postr_state_Insta_array,3)/Taver_steps
    postr_state_Insta = postr_state_Insta_array(:,:,Taver_steps)

    WRITE(470) REAL((SUM(infl_factor)/(nx*Taver_steps)),r_sngl)

  END SUBROUTINE update_Augm1

  FUNCTION Emean(E)
    REAL(r_dble),INTENT(IN) :: E(nx,nbv)
    REAL(r_dble)            :: Emean(nx)
    Emean =        SUM(E,2)/ nbv
  END FUNCTION Emean

  FUNCTION Esprd(E,E_mean)
    REAL(r_dble),INTENT(IN) :: E     (nx,nbv)
    REAL(r_dble),INTENT(IN) :: E_mean(nx)
    REAL(r_dble)            :: E_anom(nx,nbv)
    REAL(r_dble)            :: Esprd (nx)
    E_anom = E - SPREAD(E_mean ,2,  nbv)
    Esprd  = SQRT(  SUM(E_anom**2)/(nbv*nx))
  END FUNCTION Esprd

  SUBROUTINE E_stats(E,E_mean,E_sprd)
    REAL(r_dble),INTENT(IN)  :: E(nx,nbv)
    REAL(r_dble),INTENT(OUT) :: E_mean(nx)
    REAL(r_dble)             :: E_anom(nx,nbv)
    REAL(r_dble),INTENT(OUT) :: E_sprd
    E_mean =        SUM(E,2)/ nbv
    E_anom = E - SPREAD(E_mean ,2,  nbv)
    E_sprd = SQRT(  SUM(E_anom**2)/(nbv*nx))
  END SUBROUTINE E_stats

  SUBROUTINE ensemble_initialize(t0,E0)
    REAL(r_dble) :: t0
    REAL(r_dble) :: E0(nx,nbv)
    CALL ensemble_set_state(E0)
    CALL model_set_time(t0)
  END SUBROUTINE ensemble_initialize

  SUBROUTINE ensemble_set_state(Ein)
    REAL(r_dble) :: Ein(nx,nbv)
    ensemble_state(:,:) = Ein(:,:)
  END SUBROUTINE ensemble_set_state

  SUBROUTINE ensemble_step
    DO im=1,nbv
       CALL rk4_step(ensemble_state(:,im))
       ! CALL rk4_step(ensemble_state(:,im),ensemble_state(:,im))
    END DO
    CALL model_set_time(time + dt)
  END SUBROUTINE ensemble_step

  FUNCTION ensemble_clean_obs()
    !FUNCTION ensemble_clean_obs(E)
    !   REAL(r_dble),INTENT(IN) :: E(nx,nbv)
    REAL(r_dble)            :: ensemble_clean_obs(ny,nbv)
    INTEGER                 :: im

    DO im=1,nbv
       ensemble_clean_obs(:,im) = clean_obs(ensemble_state(:,im))
       !   ensemble_clean_obs(:,im) = clean_obs(E(:,im))
    END DO
  END FUNCTION ensemble_clean_obs


END MODULE common_ta_filter
