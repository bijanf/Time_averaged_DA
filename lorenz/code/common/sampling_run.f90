PROGRAM sampling_run
  USE common_tools
  USE     io_tools
  USE dynamical_system, ONLY:nx,ny,model_state,model_step,&
       & set_model_time, set_model_state, model_initialize, initial_state
  IMPLICIT NONE

  REAL(r_dble)   :: t0,x0(nx)
  INTEGER        :: i,j,sampling_period,sampling_size
  TYPE(file_hdl) :: sampling_hdl

  CALL print_line
  PRINT *,'PROGRAM sampling_run'
  CALL print_line

  CALL model_initialize

  PRINT*,' Generating a model climatology sampling'
  CALL print_line
  CALL get_int_par_env ('sampling_period',sampling_period)
  CALL get_int_par_env ('sampling_size'  ,sampling_size)

  CALL get_real_par_env('t0'             ,t0)
  CALL set_model_time(t0)

  CALL com_randn(nx,x0)  ! x0(1)=10; x0(2:)=0
  !  CALL set_model_state(x0)
  CALL set_model_state(initial_state)

  CALL open_file(sampling_hdl,'sampling','state',151,'write')

  DO j=0,sampling_size
     DO i=1,sampling_period
        CALL model_step
     END DO

     CALL write_state(sampling_hdl,model_state)
  END DO

  CALL close_file(sampling_hdl)

  STOP
END PROGRAM sampling_run
