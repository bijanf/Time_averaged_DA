PROGRAM nature_obs
  USE common_tools
  USE     io_tools
  USE dynamical_system    , ONLY: &
       model_initialize,nx,ny
  USE trajectory          , ONLY: &
       t0,step,cycles,ic,cycle_length,cycle_steps, Taver_mode,&
       Taver_length,Taver_steps,ta_ind, trajectory_initialize
  USE observation_operator, ONLY:&
       obs_operator_initialize, obs_operator_initialize_noise,&
       write_obs_error_stdd, clean_obs,sully_obs
  IMPLICIT NONE

  REAL(r_dble)  :: nature_Insta(nx)
  TYPE(file_hdl):: nature_Insta_all_hdl
  REAL(r_dble),ALLOCATABLE :: obs_clean_Insta_array(:,:)
  REAL(r_dble)  :: obs_clean_Insta(ny), obs_clean_Taver(ny), obs_dirty_Taver(ny)
  TYPE(file_hdl):: obs_clean_Insta_hdl, obs_clean_Taver_hdl, obs_dirty_Taver_hdl

  REAL(r_dble)  :: obs_clean_Taver_Tmean(ny), obs_clean_Taver_Tvar (ny)
  REAL(r_dble)  :: obs_clean_Taver1_acum(ny), obs_clean_Taver2_acum(ny)
  TYPE(file_hdl):: obs_clean_Taver_Tvar_hdl

  CALL print_line
  PRINT *,'PROGRAM nature_obs'
  CALL print_line

  CALL      model_initialize
  CALL trajectory_initialize

  CALL obs_operator_initialize
  CALL create_clean_obs

  CALL obs_operator_initialize_noise
  CALL write_obs_error_stdd

  CALL create_dirty_obs

  STOP

CONTAINS

  !<----------------------------------------------------------------------------
  !> @brief Read nature run, produce obs and calculate obs variance
  !<----------------------------------------------------------------------------
  SUBROUTINE create_clean_obs
    PRINT*,'- Creating clean nature observations'
    CALL print_line

    ALLOCATE(obs_clean_Insta_array(ny,Taver_steps))

    CALL open_file(nature_Insta_all_hdl,'nature_Insta_all','state',171,'read')
    CALL open_file( obs_clean_Insta_hdl,'obs_clean_Insta' ,'obs'  ,172,'write')
    CALL open_file( obs_clean_Taver_hdl,'obs_clean_Taver' ,'obs'  ,173,'write')

    obs_clean_Taver1_acum=0; obs_clean_Taver2_acum=0
    ! CALL get_int_par_file('total_steps',total_steps)

    PRINT*,' Initializing time-averaging arrays'
    step = 0
    DO
       CALL read_state(nature_Insta_all_hdl,nature_Insta)
       step = step + 1

       obs_clean_Insta = clean_obs(nature_Insta)
       ! print*, nature_Insta, obs_clean_Insta
       CALL write_obs(obs_clean_Insta_hdl,obs_clean_Insta)

       ! Filling up time averaging array
       obs_clean_Insta_array(:,ta_ind()) = obs_clean_Insta

       IF(step.EQ.Taver_steps) EXIT
    END DO

    PRINT*,' Generating nature observations'
    ic = 0
    DO WHILE(ic.LT.cycles)

       CALL read_state(nature_Insta_all_hdl,nature_Insta)
       step = step + 1

       obs_clean_Insta = clean_obs(nature_Insta)
       ! print*, nature_Insta, obs_clean_Insta
       CALL write_obs(obs_clean_Insta_hdl,obs_clean_Insta)

       ! Filling up time averaging array
       obs_clean_Insta_array(:,ta_ind()) = obs_clean_Insta

       IF(MODULO(step,cycle_steps).EQ.0) then
          ic = ic + 1
          IF(MODULO(ic,cycles/10).EQ.0) PRINT  '(A,I6)','  cycle ',ic

          ! Time averaging
          obs_clean_Taver = SUM(obs_clean_Insta_array,2)/Taver_steps
          CALL write_obs(obs_clean_Taver_hdl,obs_clean_Taver)

          ! Obs accumulation
          obs_clean_Taver1_acum = &
               & obs_clean_Taver1_acum + obs_clean_Taver
          obs_clean_Taver2_acum = &
               & obs_clean_Taver2_acum + obs_clean_Taver**2
       END IF
    END DO

    CALL close_file(nature_Insta_all_hdl)
    CALL close_file(obs_clean_Insta_hdl)
    CALL close_file(obs_clean_Taver_hdl)

    DEALLOCATE(obs_clean_Insta_array)

    ! - Calculate Time variance of clean Time averaged observations
    obs_clean_Taver_Tmean =  obs_clean_Taver1_acum / cycles
    obs_clean_Taver_Tvar  = (obs_clean_Taver2_acum - &
         &             cycles*obs_clean_Taver_Tmean**2 )/(cycles-1)

    CALL open_file (obs_clean_Taver_Tvar_hdl,'obs_clean_Taver_Tvar','obs',174,'write')
    CALL write_obs (obs_clean_Taver_Tvar_hdl,obs_clean_Taver_Tvar)
    CALL close_file(obs_clean_Taver_Tvar_hdl)

  END SUBROUTINE create_clean_obs

  !<----------------------------------------------------------------------------
  !> @brief Pollute clean obs
  !<----------------------------------------------------------------------------
  SUBROUTINE create_dirty_obs

    PRINT*,'- Creating dirty nature observations'
    CALL print_line

    CALL open_file( obs_clean_Taver_hdl,'obs_clean_Taver','obs',175,'read')
    CALL open_file( obs_dirty_Taver_hdl,'obs_dirty_Taver','obs',176,'write')

    DO ic = 1,cycles
       IF(MODULO(ic,cycles/10).EQ.0) PRINT  '(A,I6)','  cycle ',ic

       CALL read_obs(obs_clean_Taver_hdl,obs_clean_Taver)
       obs_dirty_Taver = sully_obs(obs_clean_Taver)
       CALL write_obs(obs_dirty_Taver_hdl,obs_dirty_Taver)

    END DO

    CALL close_file(obs_clean_Taver_hdl)
    CALL close_file(obs_dirty_Taver_hdl)

  END SUBROUTINE create_dirty_obs

END PROGRAM nature_obs
