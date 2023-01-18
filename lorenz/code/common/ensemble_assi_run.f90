!<---------------------------------------------------------------
!> Propagate an ensemble with time-averaged data assimilation
!<---------------------------------------------------------------
PROGRAM ensemble_assi_run
  USE common_tools
  USE     io_tools
  USE dynamical_system    , ONLY: &
       nx,ny,model_initialize
  USE trajectory          , ONLY: &
       t0,step,cycles,ic,cycle_length,cycle_steps, &
       Taver_mode, Taver_length,Taver_steps,ta_ind,&
       trajectory_initialize
  USE ensemble_tools      , ONLY: &
       nbv,ensemble,ensemble_state, ensemble_step, E_stats,&
       set_ensemble_time,set_ensemble_state, ensemble_clean_obs, &
       inflate,ensemble_array_clean_obs,inflate_array
  USE observation_operator, ONLY: &
       obs_operator_initialize, obs_operator_initialize_noise
  USE ta_filter           , ONLY: &
       update_mode,filter_initialize,filter_find_analysis,infl_enkf, infl_step,infl_mode,infl_cycle_length_scaling
  IMPLICIT NONE

  REAL(r_dble)   :: ensemble_ini(nx,nbv)

  TYPE(file_hdl) :: nature_obs_dirty_Taver_hdl
  REAL(r_dble)   :: nature_obs_dirty_Taver(ny)

  REAL(r_dble),ALLOCATABLE :: prior_state_Insta_array(:,:,:)
  TYPE(ensemble) :: prior_Insta, prior_Taver, prior_Tanom
  TYPE(file_hdl) :: prior_Emean_Insta_hdl,prior_Esprd_Insta_hdl
  TYPE(file_hdl) :: prior_Emean_Taver_hdl,prior_Esprd_Taver_hdl
  TYPE(file_hdl) :: prior_Emean_Tanom_hdl,prior_Esprd_Tanom_hdl

  REAL(r_dble),ALLOCATABLE :: postr_state_Insta_array(:,:,:)
  TYPE(ensemble) :: postr_Insta, postr_Taver, postr_Tanom
  TYPE(file_hdl) :: postr_Emean_Insta_hdl,postr_Esprd_Insta_hdl
  TYPE(file_hdl) :: postr_Emean_Taver_hdl,postr_Esprd_Taver_hdl
  TYPE(file_hdl) :: postr_Emean_Tanom_hdl,postr_Esprd_Tanom_hdl

  REAL(r_dble),ALLOCATABLE :: prior_obs_clean_Insta_array(:,:,:)
  REAL(r_dble)   :: prior_obs_clean_Taver(ny,nbv)

  REAL(r_dble)   :: ens_state_tmp(nx,nbv)
  INTEGER        :: i,ix

  CALL print_line
  PRINT *,'PROGRAM ensemble_assi_run'
  CALL print_line

  CALL      model_initialize
  CALL trajectory_initialize
  CALL set_ensemble_time(t0)

  CALL draw_initial_conditions((/(i,i=2,nbv+1)/),ensemble_ini)
  CALL set_ensemble_state(ensemble_ini)

  CALL obs_operator_initialize
  CALL obs_operator_initialize_noise
  CALL filter_initialize

  IF(infl_mode .EQ. 'step_')THEN
     SELECT CASE (TRIM(infl_cycle_length_scaling))
     CASE ('none')
        infl_step = infl_enkf
     CASE ('const')
        infl_step = infl_enkf**(1.0d0/cycle_steps)
     CASE ('sqrt')
        infl_enkf = (infl_enkf*SQRT(cycle_steps-1.0d0)) + 1.0d0
        infl_step = infl_enkf**(1.0d0/cycle_steps)
     CASE DEFAULT
        CALL error(' Unknown infl_cycle_length_scaling '//TRIM(infl_cycle_length_scaling))
     END SELECT
  END IF
  CALL set_real_par_file  ('infl_step',infl_step)


  PRINT*,'- Allocating time-averaging arrays'

  ALLOCATE(    prior_state_Insta_array(nx,nbv,Taver_steps))
  ALLOCATE(prior_obs_clean_Insta_array(ny,nbv,Taver_steps))
  ALLOCATE(    postr_state_Insta_array(nx,nbv,Taver_steps))

  PRINT*,'- Opening I/O files'

  CALL open_file(nature_obs_dirty_Taver_hdl,'obs_dirty_Taver','obs'    ,171,'read')

  CALL open_file(prior_Emean_Insta_hdl,'assi_Emean_prior_Insta','state',172,'write')
  CALL open_file(prior_Emean_Taver_hdl,'assi_Emean_prior_Taver','state',173,'write')
  CALL open_file(prior_Emean_Tanom_hdl,'assi_Emean_prior_Tanom','state',174,'write')

  CALL open_file(prior_Esprd_Insta_hdl,'assi_Esprd_prior_Insta','state',175,'write')
  CALL open_file(prior_Esprd_Taver_hdl,'assi_Esprd_prior_Taver','state',176,'write')
  CALL open_file(prior_Esprd_Tanom_hdl,'assi_Esprd_prior_Tanom','state',177,'write')

  CALL open_file(postr_Emean_Insta_hdl,'assi_Emean_postr_Insta','state',178,'write')
  CALL open_file(postr_Emean_Taver_hdl,'assi_Emean_postr_Taver','state',179,'write')
  CALL open_file(postr_Emean_Tanom_hdl,'assi_Emean_postr_Tanom','state',180,'write')

  CALL open_file(postr_Esprd_Insta_hdl,'assi_Esprd_postr_Insta','state',181,'write')
  CALL open_file(postr_Esprd_Taver_hdl,'assi_Esprd_postr_Taver','state',182,'write')
  CALL open_file(postr_Esprd_Tanom_hdl,'assi_Esprd_postr_Tanom','state',183,'write')

  !   OPEN(470,FILE='infl_factor_Xmean.grd',FORM='unformatted')

  PRINT*,' Initializing time-averaging arrays'
  step = 0
  DO
     ! forecast step
     CALL ensemble_step; step = step + 1
     ! prior_Insta%state = ensemble_state

     IF(infl_mode .EQ. 'step_')THEN
        ! pre-Inflation of the instantaneous ensemble every time step
        ens_state_tmp=ensemble_state
        CALL inflate(ens_state_tmp,infl_step)
        CALL set_ensemble_state(ens_state_tmp)
     END IF

     ! Filling up time averaging array
     prior_state_Insta_array    (:,:,ta_ind()) = ensemble_state


     ! prior_state_Insta_array    (:,:,ta_ind()) = prior_Insta%state
     ! prior_obs_clean_Insta_array(:,:,ta_ind()) = ensemble_clean_obs(prior_Insta%state)

     IF(step.EQ.Taver_steps) EXIT
  END DO

  PRINT*,'- Starting ensemble run'
  ic = 0
  DO WHILE(ic.LT.cycles)

     ! forecast step
     CALL ensemble_step; step = step + 1

     IF(infl_mode .EQ. 'step_')THEN
        ! pre-Inflation of the instantaneous ensemble every time step
        ens_state_tmp=ensemble_state
        CALL inflate(ens_state_tmp,infl_step)
        CALL set_ensemble_state(ens_state_tmp)
     END IF

     ! Filling up time averaging array
     prior_state_Insta_array    (:,:,ta_ind()) = ensemble_state
     prior_Insta%state = ensemble_state
     ! prior_state_Insta_array    (:,:,ta_ind()) = prior_Insta%state
     ! prior_obs_clean_Insta_array(:,:,ta_ind()) = ensemble_clean_obs(prior_Insta%state)

     IF(MODULO(step,cycle_steps).EQ.0) then
        ic = ic + 1
        IF(MODULO(ic,cycles/10).EQ.0) PRINT  '(A,I6)','  cycle ',ic

        IF(infl_mode .EQ. 'cycle')THEN
           ! pre-Inflation of the time extended ensemble every analysis cycle
           CALL inflate_array(prior_state_Insta_array,infl_enkf)
           ! CALL set_ensemble_state(prior_Insta%state)
        END IF


        ! Generate Extended ensemble observations
        prior_obs_clean_Insta_array = ensemble_array_clean_obs(prior_state_Insta_array)
        prior_obs_clean_Taver = SUM( prior_obs_clean_Insta_array,3)/Taver_steps


        !------------------------
        ! Ensemble Time averaging
        prior_Insta%state =     prior_state_Insta_array(:,:,ta_ind())
        prior_Taver%state = SUM(prior_state_Insta_array,3)/Taver_steps
        prior_Tanom%state = prior_Insta%state - prior_Taver%state

        ! Background Ensemble statistics
        CALL E_stats(prior_Insta)
        CALL E_stats(prior_Taver)
        CALL E_stats(prior_Tanom)

        ! Store Background
        CALL write_state (prior_Emean_Insta_hdl,prior_Insta%Emean)
        CALL write_state (prior_Emean_Taver_hdl,prior_Taver%Emean)
        CALL write_state (prior_Emean_Tanom_hdl,prior_Tanom%Emean)

        CALL write_state (prior_Esprd_Insta_hdl,prior_Insta%Esprd)
        CALL write_state (prior_Esprd_Taver_hdl,prior_Taver%Esprd)
        CALL write_state (prior_Esprd_Tanom_hdl,prior_Tanom%Esprd)


        !---------------
        ! Analysis step
        !---------------
        CALL read_obs(nature_obs_dirty_Taver_hdl,nature_obs_dirty_Taver)

        ! Ensemble Update
        SELECT CASE (update_mode)
        CASE ('Hakim') ! Time averaged ensemble
           CALL update_Hakim
        CASE ('Augm0') ! Time Augmented ensemble (Cheap way)
           CALL update_Augm0
        CASE ('Augm1') ! Time Augmented ensemble (Brute force)
           CALL update_Augm1
        CASE ('Augm2')
           CALL update_Augm2
        CASE ('Augm3')
           CALL update_Augm3
        CASE ('Augm4')
           CALL update_Augm4
        CASE ('Insta')
           CALL update_Insta
        END SELECT

        ! Analysis Ensemble statistics
        CALL E_stats(postr_Insta)
        CALL E_stats(postr_Taver)
        CALL E_stats(postr_Tanom)

        ! Store analysis
        CALL write_state (postr_Emean_Insta_hdl,postr_Insta%Emean)
        CALL write_state (postr_Emean_Taver_hdl,postr_Taver%Emean)
        CALL write_state (postr_Emean_Tanom_hdl,postr_Tanom%Emean)

        CALL write_state (postr_Esprd_Insta_hdl,postr_Insta%Esprd)
        CALL write_state (postr_Esprd_Taver_hdl,postr_Taver%Esprd)
        CALL write_state (postr_Esprd_Tanom_hdl,postr_Tanom%Esprd)

        ! Closing analysis cycle

        ! IF(infl_mode .EQ. 'post_')THEN
        ! post-Inflation of the instantaneous ensemble every time step
        ! CALL inflate(postr_Insta%state,infl_enkf)
        ! END IF

        CALL set_ensemble_state(postr_Insta%state)

     END IF

  END DO

  PRINT*,'- Closing I/O files'

  CALL close_file(nature_obs_dirty_Taver_hdl)

  CALL close_file(prior_Emean_Insta_hdl)
  CALL close_file(prior_Emean_Taver_hdl)
  CALL close_file(prior_Emean_Tanom_hdl)
  CALL close_file(prior_Esprd_Insta_hdl)
  CALL close_file(prior_Esprd_Taver_hdl)
  CALL close_file(prior_Esprd_Tanom_hdl)

  CALL close_file(postr_Emean_Insta_hdl)
  CALL close_file(postr_Emean_Taver_hdl)
  CALL close_file(postr_Emean_Tanom_hdl)
  CALL close_file(postr_Esprd_Insta_hdl)
  CALL close_file(postr_Esprd_Taver_hdl)
  CALL close_file(postr_Esprd_Tanom_hdl)

  !   CLOSE(470)

  STOP

CONTAINS

  !=======================================================================
  !> @brief  Traditional update of the last Instantaneous state
  !=======================================================================
  SUBROUTINE update_Insta

    CALL filter_find_analysis(prior_Insta%state, prior_obs_clean_Taver, &
         &         nature_obs_dirty_Taver,           postr_Insta%state)

    postr_Taver%state = prior_Taver%state
    postr_Tanom%state = postr_Insta%state - postr_Taver%state

    ! WRITE(470) REAL(SUM(infl_factor)/nx,r_sngl)

  END SUBROUTINE update_Insta

  !=======================================================================
  !> @brief  Hakim's Time-averaged filtering approach
  !=======================================================================
  SUBROUTINE update_Hakim

    ! Ensemble Time average decomposition
    prior_Tanom%state = &
         & prior_state_Insta_array(:,:,ta_ind()) - prior_Taver%state

    CALL filter_find_analysis(prior_Taver%state, prior_obs_clean_Taver, &
         &         nature_obs_dirty_Taver,           postr_Taver%state)

    ! Ensemble Time average Recomposition
    postr_Tanom%state = prior_Tanom%state
    postr_Insta%state = postr_Tanom%state + postr_Taver%state

    ! WRITE(470) REAL(SUM(infl_factor)/nx,r_sngl)

  END SUBROUTINE update_Hakim

  !=======================================================================
  !> @brief Time Augmented ensemble (~4D-EnKF) (Cheap way)
  !=======================================================================
  SUBROUTINE update_Augm0
    INTEGER i_ta
    DO i_ta = 1,Taver_steps
        CALL filter_find_analysis( &
         &  prior_state_Insta_array(:,:,i_ta),&
         &  prior_obs_clean_Taver, nature_obs_dirty_Taver, &
         &  postr_state_Insta_array(:,:,i_ta))
    END DO

    postr_Insta%state =     postr_state_Insta_array(:,:,ta_ind())
    postr_Taver%state = SUM(postr_state_Insta_array,3)/Taver_steps
    postr_Tanom%state = postr_Insta%state - postr_Taver%state

    ! WRITE(470) REAL((SUM(infl_factor)/(nx*Taver_steps)),r_sngl)

  END SUBROUTINE update_Augm0

  !=======================================================================
  !> @brief Time Augmented ensemble (~4D-EnKF) (Brute force way)
  !=======================================================================
  SUBROUTINE update_Augm1
    REAL(r_dble),ALLOCATABLE::  prior_state_Augm (:,:)
    REAL(r_dble),ALLOCATABLE::  postr_state_Augm (:,:)
    INTEGER iaugm,im,i_ta

    ALLOCATE(prior_state_Augm (nx*Taver_steps,nbv))
    ALLOCATE(postr_state_Augm (nx*Taver_steps,nbv))

    ! Ensemble time augmentation
    DO im = 1,nbv
       iaugm = 0
       DO i_ta = 1,Taver_steps
          DO ix = 1,nx
             iaugm = iaugm + 1
             prior_state_Augm(iaugm,im) = prior_state_Insta_array(ix,im,i_ta)
          END DO
       END DO
    END DO

    CALL filter_find_analysis(prior_state_Augm, prior_obs_clean_Taver, &
         &               nature_obs_dirty_Taver,     postr_state_Augm)

    ! Ensemble time deaugmentation
    DO im = 1,nbv
       DO iaugm = 1,Taver_steps*nx
          ix  = MODULO(iaugm-1,nx) + 1;  i_ta = CEILING(iaugm/REAL(nx))
          postr_state_Insta_array(ix,im,i_ta) = postr_state_Augm(iaugm,im)
       END DO
    END DO

    postr_Insta%state =     postr_state_Insta_array(:,:,ta_ind())
    postr_Taver%state = SUM(postr_state_Insta_array,3)/Taver_steps
    postr_Tanom%state = postr_Insta%state - postr_Taver%state

    ! WRITE(470) REAL((SUM(infl_factor)/(nx*Taver_steps)),r_sngl)

  END SUBROUTINE update_Augm1

  !=======================================================================
  !> @brief Time-averaged + anomalies Augmentation
  !=======================================================================
  SUBROUTINE update_Augm2
    REAL(r_dble),ALLOCATABLE::  prior_state_Augm (:,:)
    REAL(r_dble),ALLOCATABLE::  postr_state_Augm (:,:)
    REAL(r_dble),ALLOCATABLE :: prior_state_Tanom_array(:,:,:)
    REAL(r_dble),ALLOCATABLE :: postr_state_Tanom_array(:,:,:)
    INTEGER iaugm,im,i_ta

    ALLOCATE(prior_state_Tanom_array(nx,nbv,Taver_steps))
    ALLOCATE(postr_state_Tanom_array(nx,nbv,Taver_steps))
    ALLOCATE(prior_state_Augm (nx*(Taver_steps+1),nbv))
    ALLOCATE(postr_state_Augm (nx*(Taver_steps+1),nbv))

    ! Ensemble Time average decomposition
    prior_state_Tanom_array = prior_state_Insta_array - &
         &             SPREAD(prior_Taver%state,3, Taver_steps)

    ! Ensemble time augmentation
    DO im = 1,nbv
       iaugm = 0
       DO i_ta = 1,Taver_steps
          DO ix = 1,nx
             iaugm = iaugm + 1
             prior_state_Augm(iaugm,im) = prior_state_Tanom_array(ix,im,i_ta)
          END DO
       END DO
       DO ix = 1,nx
          iaugm = iaugm + 1
          prior_state_Augm(iaugm,im) = prior_Taver%state(ix,im)
       END DO
    END DO

    CALL filter_find_analysis(prior_state_Augm, prior_obs_clean_Taver, &
         &               nature_obs_dirty_Taver,     postr_state_Augm)

    ! Ensemble time deaugmentation
    DO im = 1,nbv
       DO iaugm = 1,(Taver_steps+1)*nx
          ix  = MODULO(iaugm-1,nx) + 1;  i_ta = CEILING(iaugm/REAL(nx))
          IF(i_ta.LE.Taver_steps)THEN
             postr_state_Tanom_array(ix,im,i_ta) = postr_state_Augm(iaugm,im)
          ELSE
             postr_Taver%state(ix,im) = postr_state_Augm(iaugm,im)
          END IF
       END DO
    END DO

    ! Ensemble Time average Recomposition
    postr_state_Insta_array = postr_state_Tanom_array + &
         &             SPREAD(postr_Taver%state,3, Taver_steps)
    postr_Taver%state = SUM(postr_state_Insta_array,3)/Taver_steps
    postr_Tanom%state = postr_state_Tanom_array(:,:,ta_ind())
    postr_Insta%state = postr_state_Insta_array(:,:,ta_ind())

  END SUBROUTINE update_Augm2

  !=======================================================================
  !> @brief Time-averaged + last anomaly Augmentation
  !=======================================================================
  SUBROUTINE update_Augm3
    REAL(r_dble),ALLOCATABLE::  prior_state_Augm (:,:)
    REAL(r_dble),ALLOCATABLE::  postr_state_Augm (:,:)
!    REAL(r_dble),ALLOCATABLE :: prior_state_Tanom_array(:,:,:)
!    REAL(r_dble),ALLOCATABLE :: postr_state_Tanom_array(:,:,:)
    INTEGER iaugm,im,i_ta

    ALLOCATE(prior_state_Augm (nx*2,nbv))
    ALLOCATE(postr_state_Augm (nx*2,nbv))
    
    ! Ensemble time augmentation
    DO im = 1,nbv
       iaugm = 0
       DO ix = 1,nx
          iaugm = iaugm + 1
          prior_state_Augm(iaugm,im) = prior_Tanom%state(ix,im)
       END DO
       DO ix = 1,nx
          iaugm = iaugm + 1
          prior_state_Augm(iaugm,im) = prior_Taver%state(ix,im)
       END DO
    END DO

    CALL filter_find_analysis(prior_state_Augm, prior_obs_clean_Taver, &
         &               nature_obs_dirty_Taver,     postr_state_Augm)

    ! Ensemble time deaugmentation
    DO im = 1,nbv
       DO iaugm = 1,nx*2
          ix  = MODULO(iaugm-1,nx) + 1;  i_ta = CEILING(iaugm/REAL(nx))
          IF(i_ta.EQ.1)THEN
             postr_Tanom%state(ix,im) = postr_state_Augm(iaugm,im)
          ELSE
             postr_Taver%state(ix,im) = postr_state_Augm(iaugm,im)
          END IF
       END DO
    END DO

    ! Ensemble Time average Recomposition
!    postr_state_Insta_array = postr_state_Tanom_array + &
!         &             SPREAD(postr_Taver%state,3, Taver_steps)
!    postr_Taver%state = SUM(postr_state_Insta_array,3)/Taver_steps
!    postr_Tanom%state = postr_state_Tanom_array(:,:,ta_ind())
    postr_Insta%state = postr_Taver%state + postr_Tanom%state

  END SUBROUTINE update_Augm3

  !=======================================================================
  !> @brief Time-averaged + last Instantaneous Augmentation
  !=======================================================================
  SUBROUTINE update_Augm4
    REAL(r_dble),ALLOCATABLE::  prior_state_Augm (:,:)
    REAL(r_dble),ALLOCATABLE::  postr_state_Augm (:,:)
!    REAL(r_dble),ALLOCATABLE :: prior_state_Tanom_array(:,:,:)
!    REAL(r_dble),ALLOCATABLE :: postr_state_Tanom_array(:,:,:)
    INTEGER iaugm,im,i_ta

    ALLOCATE(prior_state_Augm (nx*2,nbv))
    ALLOCATE(postr_state_Augm (nx*2,nbv))

    ! Ensemble augmentation
    DO im = 1,nbv
       iaugm = 0
       DO ix = 1,nx
          iaugm = iaugm + 1
          prior_state_Augm(iaugm,im) = prior_Insta%state(ix,im)
       END DO
       DO ix = 1,nx
          iaugm = iaugm + 1
          prior_state_Augm(iaugm,im) = prior_Taver%state(ix,im)
       END DO
    END DO

    CALL filter_find_analysis(prior_state_Augm, prior_obs_clean_Taver, &
         &               nature_obs_dirty_Taver,     postr_state_Augm)

    ! Ensemble deaugmentation
    DO im = 1,nbv
       DO iaugm = 1,nx*2
          ix  = MODULO(iaugm-1,nx) + 1;  i_ta = CEILING(iaugm/REAL(nx))
          IF(i_ta.EQ.1)THEN
             postr_Insta%state(ix,im) = postr_state_Augm(iaugm,im)
          ELSE
             postr_Taver%state(ix,im) = postr_state_Augm(iaugm,im)
          END IF
       END DO
    END DO

    postr_Tanom%state = postr_Insta%state - postr_Taver%state

  END SUBROUTINE update_Augm4

END PROGRAM ensemble_assi_run

