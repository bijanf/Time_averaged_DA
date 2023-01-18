!<---------------------------------------------------
!> Propagate a free ensemble
!<---------------------------------------------------
PROGRAM ensemble_free_run
  USE common_tools
  USE dynamical_system, ONLY: &
       nx,ny,model_initialize
  USE trajectory      , ONLY: &
       t0,step,cycles,ic,cycle_length,cycle_steps, &
       Taver_mode, Taver_length,Taver_steps,ta_ind,&
       trajectory_initialize
  USE ensemble_tools  , ONLY: &
       nbv,ensemble,ensemble_state, ensemble_step, E_stats,&
       set_ensemble_time,set_ensemble_state
  USE io_tools        , ONLY: &
       file_hdl,open_file,close_file,write_state,&
       read_state, draw_initial_conditions
  IMPLICIT NONE

  REAL(r_dble)   :: ensemble_ini(nx,nbv)

  REAL(r_dble),ALLOCATABLE :: prior_state_Insta_array(:,:,:)
  TYPE(ensemble) :: prior_Insta, prior_Taver, prior_Tanom
  TYPE(file_hdl) :: prior_Emean_Insta_hdl,prior_Esprd_Insta_hdl
  TYPE(file_hdl) :: prior_Emean_Taver_hdl,prior_Esprd_Taver_hdl
  TYPE(file_hdl) :: prior_Emean_Tanom_hdl,prior_Esprd_Tanom_hdl

  INTEGER        :: i

  CALL print_line
  PRINT *,'PROGRAM ensemble_free_run'
  CALL print_line

  CALL      model_initialize
  CALL trajectory_initialize
  CALL set_ensemble_time(t0)

  CALL draw_initial_conditions((/(i,i=2,nbv+1)/),ensemble_ini)
  CALL set_ensemble_state(ensemble_ini)

  PRINT*,'- Allocating time-averaging arrays'

  ALLOCATE(prior_state_Insta_array(nx,nbv,Taver_steps))

  PRINT*,'- Opening I/O files'

  CALL open_file(prior_Emean_Insta_hdl,'free_Emean_prior_Insta','state',172,'write')
  CALL open_file(prior_Emean_Taver_hdl,'free_Emean_prior_Taver','state',173,'write')
  CALL open_file(prior_Emean_Tanom_hdl,'free_Emean_prior_Tanom','state',174,'write')

  CALL open_file(prior_Esprd_Insta_hdl,'free_Esprd_prior_Insta','state',175,'write')
  CALL open_file(prior_Esprd_Taver_hdl,'free_Esprd_prior_Taver','state',176,'write')
  CALL open_file(prior_Esprd_Tanom_hdl,'free_Esprd_prior_Tanom','state',177,'write')

  PRINT*,' Initializing time-averaging arrays'
  step = 0
  DO
     ! forecast step
     CALL ensemble_step; step = step + 1
     prior_Insta%state = ensemble_state

     ! Filling up time averaging array
     prior_state_Insta_array(:,:,ta_ind()) = prior_Insta%state

     IF(step.EQ.Taver_steps) EXIT
  END DO

  PRINT*,' Starting ensemble run'
  ic = 0
  DO WHILE(ic.LT.cycles)

     ! forecast step
     CALL ensemble_step; step = step + 1
     prior_Insta%state = ensemble_state

     ! Filling up time averaging array
     prior_state_Insta_array(:,:,ta_ind()) = prior_Insta%state

     IF(MODULO(step,cycle_steps).EQ.0) then
        ic = ic + 1
        IF(MODULO(ic,cycles/10).EQ.0) PRINT  '(A,I6)','  cycle ',ic

        ! Time averaging
        prior_Taver%state = SUM(prior_state_Insta_array,3)/Taver_steps
        prior_Tanom%state =     prior_Insta%state - prior_Taver%state

        ! Ensemble statistics
        CALL E_stats(prior_Insta)
        CALL E_stats(prior_Taver)
        CALL E_stats(prior_Tanom)

        ! Store forecast
        CALL write_state (prior_Emean_Insta_hdl,prior_Insta%Emean)
        CALL write_state (prior_Emean_Taver_hdl,prior_Taver%Emean)
        CALL write_state (prior_Emean_Tanom_hdl,prior_Tanom%Emean)

        CALL write_state (prior_Esprd_Insta_hdl,prior_Insta%Esprd)
        CALL write_state (prior_Esprd_Taver_hdl,prior_Taver%Esprd)
        CALL write_state (prior_Esprd_Tanom_hdl,prior_Tanom%Esprd)
     END IF

  END DO

  PRINT*,'- Closing I/O files'

  CALL close_file(prior_Emean_Insta_hdl)
  CALL close_file(prior_Emean_Taver_hdl)
  CALL close_file(prior_Emean_Tanom_hdl)
  CALL close_file(prior_Esprd_Insta_hdl)
  CALL close_file(prior_Esprd_Taver_hdl)
  CALL close_file(prior_Esprd_Tanom_hdl)

  STOP

END PROGRAM ensemble_free_run
