!<----------------------------------------------------------------------------
!> Propagate an ensemble with or without data assimilation
!<----------------------------------------------------------------------------
PROGRAM ensemble_run
  USE common_dyn_system
  USE common_ta_filter
  USE common_obs_operator
  IMPLICIT NONE
  CHARACTER(4)         :: run_mode        ! 'free' 'assi'
  CHARACTER(5)         :: update_mode     ! 'Hakim','Augm1'
  TYPE(state_nc_handle):: nature_Insta_all_hdl
  TYPE(state_nc_handle):: prior_Emean_Insta_hdl
  TYPE(state_nc_handle):: prior_Emean_Taver_hdl
  TYPE(state_nc_handle):: postr_Emean_Insta_hdl
  TYPE(state_nc_handle):: postr_Emean_Taver_hdl
  TYPE  (obs_nc_handle):: nature_obs_clean_Insta_hdl
  TYPE  (obs_nc_handle):: nature_obs_clean_Taver_hdl
  TYPE  (obs_nc_handle):: nature_obs_clean_Taver_Tvar_hdl
  INTEGER              :: ita, total_steps

  CALL ensemble_run_initialize

  CALL ensemble_run_perform

  STOP

CONTAINS

  SUBROUTINE ensemble_run_initialize
    REAL(r_size)         :: ensemble_ini  (nx,nbv)
    INTEGER              :: im
    TYPE(state_nc_handle):: sampling_hdl

    CALL print_line
    CALL set_int_par_file('nbv',nbv)
    CALL set_int_par_file('ny',ny)
    CALL print_line

    PRINT*,' - Reading initial conditions'
    CALL open_state_nc_file('sampling',sampling_hdl)
    DO im=1,nbv
       CALL read_state_in_nc_file(sampling_hdl,im+1,ensemble_ini(:,im))
    END DO
    CALL close_readonly_state_nc_file(sampling_hdl)

    CALL ensemble_initialize(0.0d0,REAL(ensemble_ini,r_size))
    !---------------------------------------------------------------------
    ! Initializing modules
    !---------------------------------------------------------------------
    CALL trajectory_initialize
    CALL get_string_par_env('run_mode'   ,run_mode)

    IF (run_mode.EQ.'assi') THEN
			   
			 CALL obs_operator_initialize
			 CALL create_nature_obs
       CALL obs_operator_initialize_noise
			 CALL  ta_filter_initialize

       CALL get_string_par_env('update_mode',update_mode)
       SELECT CASE (update_mode)
       CASE ('Hakim') ! Time averaged ensemble
          CALL filter_initialize(nx ,var_pos)
       CASE ('Augm1') ! Time Augmented ensemble (~4D-EnKF)
          CALL filter_initialize(nx*Taver_steps, &
               & RESHAPE(SPREAD(var_pos,2,Taver_steps),(/nx*Taver_steps/)))
       END SELECT
    END IF
  END SUBROUTINE ensemble_run_initialize


  !=======================================================================
  !> @brief Read nature run, produce obs and calculate obs variance
  !=======================================================================
  SUBROUTINE create_nature_obs
    REAL(r_sngl) :: nature_obs_clean_Taver_Tmean(ny)
    REAL(r_sngl) :: nature_obs_clean_Taver1_acum(ny)
    REAL(r_sngl) :: nature_obs_clean_Taver2_acum(ny)
    REAL(r_sngl) :: nature_obs_clean_Taver_Tvar (ny)

    PRINT*,'- Creating clean nature observations'
    CALL print_line

    ALLOCATE(nature_obs_clean_Insta_array(ny,1,Taver_steps))

    CALL open_state_nc_file('nature_Insta_all',nature_Insta_all_hdl)
    CALL create_obs_nc_file('nature_obs_clean_Insta',nature_obs_clean_Insta_hdl)
    CALL create_obs_nc_file('nature_obs_clean_Taver',nature_obs_clean_Taver_hdl)
    ! OPEN(92,FILE='nature_obs_clean_Insta_4b.grd',FORM='unformatted')
    ! OPEN(93,FILE='nature_obs_clean_Taver_4b.grd',FORM='unformatted')

    nature_obs_clean_Taver1_acum=0
    nature_obs_clean_Taver2_acum=0

    total_steps=0
    DO ic = 1,cycles
       IF(MODULO(ic,100).EQ.0) PRINT  '(A,I6)','  cycle ',ic
       ita = 0
       DO icl=1,cycle_steps
          total_steps = total_steps + 1
          CALL read_state_in_nc_file &
               & (nature_Insta_all_hdl,total_steps,nature_state_Insta(:,1))
          nature_obs_clean_Insta(:,1) = clean_obs(REAL(nature_state_Insta,r_size))

          CALL write_obs_in_nc_file &
               & (nature_obs_clean_Insta_hdl,total_steps,nature_obs_clean_Insta)
          ! WRITE(92) nature_obs_clean_Insta
          
          ! Filling up time averaging array
          IF (icl.GT.(cycle_steps-Taver_steps)) THEN
             ita = ita + 1
             nature_obs_clean_Insta_array(:,:,ita) = nature_obs_clean_Insta
          END IF
       END DO

       nature_obs_clean_Taver = SUM(nature_obs_clean_Insta_array,3)/Taver_steps
       CALL write_obs_in_nc_file &
            & (nature_obs_clean_Taver_hdl,ic,nature_obs_clean_Taver(:,1))
       ! WRITE(93) nature_obs_clean_Taver(:,1)

       nature_obs_clean_Taver1_acum = nature_obs_clean_Taver1_acum + &
            &                         nature_obs_clean_Taver(:,1)
       nature_obs_clean_Taver2_acum = nature_obs_clean_Taver2_acum + &
            &                         nature_obs_clean_Taver(:,1)**2

    END DO
    CALL close_readonly_state_nc_file(nature_Insta_all_hdl)
    CALL close_obs_nc_file(nature_obs_clean_Insta_hdl,(/(ic*dt,ic=1,total_steps)/))
    CALL close_obs_nc_file(nature_obs_clean_Taver_hdl,(/(ic*cycle_steps*dt,ic=1,cycles)/))
    ! CLOSE(92); CLOSE(93)

    DEALLOCATE(nature_obs_clean_Insta_array)

    ! - Calculate Time variance of clean Time averaged observations
    nature_obs_clean_Taver_Tmean =  nature_obs_clean_Taver1_acum / cycles
    nature_obs_clean_Taver_Tvar  = (nature_obs_clean_Taver2_acum - &
         &             cycles*nature_obs_clean_Taver_Tmean**2 )/(cycles-1)

    CALL create_obs_nc_file('nature_obs_clean_Taver_Tvar',nature_obs_clean_Taver_Tvar_hdl)
    CALL write_obs_in_nc_file &
            & (nature_obs_clean_Taver_Tvar_hdl,1,REAL(nature_obs_clean_Taver_Tvar,r_size))
    CALL close_obs_nc_file(nature_obs_clean_Taver_Tvar_hdl,(/0.0d0/))

    ! CALL com_write_1Dfield_4b('nature_obs_clean_Taver_Tvar_4b',ny,&
    !      &                               'var',nature_obs_clean_Taver_Tvar)

  END SUBROUTINE create_nature_obs



  SUBROUTINE ensemble_run_perform

    PRINT*,'- Running TA-DA experiment'
    !---------------------------------------------------------------------
    ! Time-average arrays allocation
    !---------------------------------------------------------------------
    ALLOCATE(    nature_state_Insta_array(nx,  1,Taver_steps))
    ALLOCATE(nature_obs_clean_Insta_array(ny,  1,Taver_steps))
    ALLOCATE(     prior_state_Insta_array(nx,nbv,Taver_steps))
    ALLOCATE( prior_obs_clean_Insta_array(ny,nbv,Taver_steps))
    ALLOCATE(     postr_state_Insta_array(nx,nbv,Taver_steps))
    !---------------------------------------------------------------------
    ! Openning input files
    !---------------------------------------------------------------------
    CALL open_state_nc_file('nature_Insta_all',nature_Insta_all_hdl)
    CALL   open_obs_nc_file('nature_obs_clean_Taver',nature_obs_clean_Taver_hdl)    
    ! OPEN(411,FILE='nature_obs_clean_Taver_4b.grd',FORM='unformatted')
    !---------------------------------------------------------------------
    ! Opennig output files
    !---------------------------------------------------------------------
    CALL create_state_nc_file(run_mode//'_prior_Emean_Insta',prior_Emean_Insta_hdl)
    CALL create_state_nc_file(run_mode//'_prior_Emean_Taver',prior_Emean_Taver_hdl)
    IF (run_mode.EQ.'assi') then
       OPEN(470,FILE='infl_factor_Xmean.grd',FORM='unformatted')
       CALL create_state_nc_file(run_mode//'_postr_Emean_Insta',postr_Emean_Insta_hdl)
       CALL create_state_nc_file(run_mode//'_postr_Emean_Taver',postr_Emean_Taver_hdl)
    END IF

    !---------------------------------------------------------------------
    PRINT*,' Starting Analysis Cycle'
    !---------------------------------------------------------------------
    total_steps=0
    main_loop: DO ic = 1,cycles,1
       IF(MODULO(ic,100).EQ.0) PRINT  '(A,I6)','  cycle ',ic
       !---------------
       ! forecast step
       !---------------
       ita = 0
       DO icl=1,cycle_steps
          CALL ensemble_step
          prior_state_Insta = ensemble_state
          total_steps = total_steps + 1
          CALL read_state_in_nc_file &
               & (nature_Insta_all_hdl,total_steps,nature_state_Insta(:,1))

          ! Filling up time averaging arrays
          IF (icl.GT.(cycle_steps-Taver_steps)) THEN
             ita = ita + 1
             prior_state_Insta_array     (:,:,ita) = prior_state_Insta
             prior_obs_clean_Insta_array (:,:,ita) = ensemble_clean_obs()
             nature_state_Insta_array    (:,:,ita) = nature_state_Insta
             nature_obs_clean_Insta_array(:,:,ita) = nature_obs_clean_Insta
          END IF
       END DO

       !------------------
       ! Time averaging
       !------------------
       nature_state_Taver    = SUM(    nature_state_Insta_array,3)/Taver_steps
       prior_obs_clean_Taver = SUM( prior_obs_clean_Insta_array,3)/Taver_steps
       prior_state_Taver     = SUM(     prior_state_Insta_array,3)/Taver_steps
       !---------------------------
       ! Contaminate nature TA obs
       !---------------------------
       CALL read_obs_in_nc_file &
               & (nature_obs_clean_Taver_hdl,ic,nature_obs_clean_Taver)
       ! READ(411) nature_obs_clean_Taver
       nature_obs_dirty_Taver(:,1)= sully_obs(nature_obs_clean_Taver)

       !---------------------------
       ! Store forecast
       !---------------------------
       CALL write_state_in_nc_file &
            & (prior_Emean_Insta_hdl,ic,Emean(prior_state_Insta))
       CALL write_state_in_nc_file &
            & (prior_Emean_Taver_hdl,ic,Emean(prior_state_Taver))

       !---------------
       ! Analysis step
       !---------------
       IF (run_mode.EQ.'assi') then
          !-------------------
          ! Ensemble Update
          !-------------------
          SELECT CASE (update_mode)
          CASE ('Hakim') ! Time averaged ensemble
             CALL update_Hakim
          CASE ('Augm1') ! Time Augmented ensemble (~4D-EnKF)
             CALL update_Augm1
          END SELECT

          !---------------------------
          ! Store analysis
          !---------------------------
          CALL write_state_in_nc_file &
               & (postr_Emean_Insta_hdl,ic,Emean(postr_state_Insta))
          CALL write_state_in_nc_file &
               & (postr_Emean_Taver_hdl,ic,Emean(postr_state_Taver))

          !------------------------
          ! Closing analysis cycle
          !------------------------
          CALL ensemble_set_state(postr_state_Insta)
       END IF
    END DO main_loop
    !---------------------------------------------------------------------
    ! Closing input files
    !---------------------------------------------------------------------
    CALL close_readonly_state_nc_file(nature_Insta_all_hdl)
    CALL close_readonly_obs_nc_file(nature_obs_clean_Taver_hdl)
    ! CLOSE(411)
    !---------------------------------------------------------------------
    ! Closing output files
    !---------------------------------------------------------------------
    CALL close_state_nc_file(prior_Emean_Insta_hdl,(/(ic*cycle_steps*dt,ic=1,cycles)/))
    CALL close_state_nc_file(prior_Emean_Taver_hdl,(/(ic*cycle_steps*dt,ic=1,cycles)/))
    IF (run_mode.EQ.'assi') then
       CALL close_state_nc_file(postr_Emean_Insta_hdl,(/(ic*cycle_steps*dt,ic=1,cycles)/))
       CALL close_state_nc_file(postr_Emean_Taver_hdl,(/(ic*cycle_steps*dt,ic=1,cycles)/))
       CLOSE(470)
    END IF
  END SUBROUTINE ensemble_run_perform

END PROGRAM ensemble_run

