PROGRAM nature_run
  USE common_tools
  USE dynamical_system, ONLY:model_initialize,&
       nx,ny,model_state,model_step,&
       set_model_time, set_model_state 
  USE trajectory, ONLY: trajectory_initialize,&
       t0,step,cycles,ic,cycle_length,cycle_steps,&
       Taver_mode,Taver_length,Taver_steps,ta_ind
  USE io_tools, ONLY: &
       file_hdl,open_file,close_file,write_state,&
       read_state,draw_initial_conditions
  IMPLICIT NONE

  REAL(r_dble)   :: x0(nx)

  REAL(r_dble),ALLOCATABLE :: nature_Insta_array(:,:)
  REAL(r_dble)   :: nature_Insta(nx), nature_Taver(nx)
  TYPE(file_hdl) :: nature_Insta_hdl, nature_Taver_hdl
  TYPE(file_hdl) :: nature_Insta_all_hdl
  TYPE(file_hdl) :: nature_Insta_all_min_hdl, nature_Insta_all_max_hdl
  REAL(r_dble)   :: nature_Insta_all_min(nx), nature_Insta_all_max(nx)
  REAL(r_dble)   :: nature_Insta_all_sum1(nx), nature_Insta_all_sum2(nx)
  REAL(r_dble)   :: nature_Insta_all_mean(nx), nature_Insta_all_stdd(nx)
  TYPE(file_hdl) :: nature_Insta_all_mean_hdl, nature_Insta_all_stdd_hdl

  CALL print_line
  PRINT *,'PROGRAM nature_run'
  CALL print_line

  ! INTEGER        :: ita
  ! logical        :: random_start
  ! CHARACTER(30)  :: state_filename

  CALL      model_initialize
  CALL trajectory_initialize
  CALL set_model_time(t0)

  CALL draw_initial_conditions((/1/),x0)
  CALL set_model_state(x0)

  ! Time-average array allocation
  ALLOCATE(nature_Insta_array(nx,Taver_steps))

  CALL open_file(nature_Insta_all_hdl,'nature_Insta_all','state',152,'write')
  CALL open_file(nature_Insta_hdl    ,'nature_Insta'    ,'state',153,'write')
  CALL open_file(nature_Taver_hdl    ,'nature_Taver'    ,'state',154,'write')

  nature_Insta_all_min = x0
  nature_Insta_all_max = x0
  nature_Insta_all_sum1 = 0.0d0
  nature_Insta_all_sum2 = 0.0d0
  

  PRINT*,' Initializing time-averaging arrays'
  step = 0
  DO
     ! Forecast step
     CALL model_step; step = step + 1
     CALL write_state(nature_Insta_all_hdl,model_state)

     nature_Insta_all_sum1 = nature_Insta_all_sum1 + model_state
     nature_Insta_all_sum2 = nature_Insta_all_sum2 + model_state ** 2.0d0

     ! Filling up time averaging array
     nature_Insta_array(:,ta_ind()) = model_state

     IF(step.EQ.Taver_steps) EXIT

  END DO

  PRINT*,' Starting nature run'
  ic = 0
  DO WHILE(ic.LT.cycles)

     ! Forecast step
     CALL model_step; step = step + 1
     CALL write_state(nature_Insta_all_hdl,model_state)

     nature_Insta_all_sum1 = nature_Insta_all_sum1 + model_state
     nature_Insta_all_sum2 = nature_Insta_all_sum2 + model_state ** 2.0d0

      ! Filling up time averaging array
     nature_Insta_array(:,ta_ind()) = model_state

     IF(MODULO(step,cycle_steps).EQ.0) then
        ic = ic + 1
        IF(MODULO(ic,cycles/10).EQ.0) PRINT  '(A,I6)','  cycle ',ic

        ! Time averaging
        nature_Taver = SUM(nature_Insta_array,2)/Taver_steps
        nature_Insta = nature_Insta_array(:,ta_ind())

        ! Storage
        CALL write_state(nature_Insta_hdl,nature_Insta)
        CALL write_state(nature_Taver_hdl,nature_Taver)
     END IF

     nature_Insta_all_min = MIN(nature_Insta_all_min,model_state)
     nature_Insta_all_max = MAX(nature_Insta_all_max,model_state)

  END DO

  CALL close_file(nature_Insta_all_hdl)
  CALL close_file(nature_Insta_hdl)
  CALL close_file(nature_Taver_hdl)

  
  CALL set_int_par_file('total_steps',step)

  CALL open_file  (nature_Insta_all_min_hdl,'nature_Insta_all_min','state',155,'write')
  CALL write_state(nature_Insta_all_min_hdl, nature_Insta_all_min)
  CALL close_file (nature_Insta_all_min_hdl)

  CALL open_file  (nature_Insta_all_max_hdl,'nature_Insta_all_max','state',155,'write')
  CALL write_state(nature_Insta_all_max_hdl, nature_Insta_all_max)
  CALL close_file (nature_Insta_all_max_hdl)

  
  nature_Insta_all_mean = nature_Insta_all_sum1 / step
  nature_Insta_all_stdd = SQRT(&
      & (nature_Insta_all_sum2 - step * nature_Insta_all_mean ** 2.0d0) &
      & / (step - 1))

  CALL open_file  (nature_Insta_all_mean_hdl,'nature_Insta_all_mean','state',155,'write')
  CALL write_state(nature_Insta_all_mean_hdl, nature_Insta_all_mean)
  CALL close_file (nature_Insta_all_mean_hdl)

  CALL open_file  (nature_Insta_all_stdd_hdl,'nature_Insta_all_stdd','state',155,'write')
  CALL write_state(nature_Insta_all_stdd_hdl, nature_Insta_all_stdd)
  CALL close_file (nature_Insta_all_stdd_hdl)

  STOP
END PROGRAM nature_run
