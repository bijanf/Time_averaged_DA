PROGRAM particle_run
  USE common
  USE common_das_tools
  USE model_tools
  USE common_dyn_system
  USE common_trajectory
  IMPLICIT NONE

  REAL(r_size) :: x0(nx)
  logical      :: random_start
  CHARACTER(30):: state_filename

  CALL      model_initialize
  CALL trajectory_initialize

  ! CALL get_string_par_env('state_filename',state_filename)

  ! INQUIRE(FILE='random_start.cfg',EXIST=random_start)
  ! IF(random_start) THEN
  !    CALL com_randn(nx,x0)
  ! ELSE
  !    OPEN (210,FILE=  'init.dat',FORM='unformatted')
  !    READ (210) x0;  CALL set_model_state(x0)
  !    CLOSE(210)
  ! END IF
  !CALL com_randn(nx,x0)
  x0(1)=5; x0(2:)=0
  CALL model_initialize(0,x0)

  'nature_state_Insta_4b'
  CALL model_set_state_4byte_ctl(,cycles*cycle_length)
  CALL com_set_binary_ctl(state_filename,'state',nx,cynt)

  OPEN(291,FILE=trim(state_filename)//.dat,FORM='unformatted')
  OPEN(292,FILE='nature_state_Insta_4b.dat',FORM='unformatted')

  DO ic = 1,cycles
     IF(MODULO(ic,100).EQ.0) PRINT  '(A,I6)','  cycle ',ic
     DO icl=1,cycle_length
        CALL model_step
        CALL model_write_state_8byte(291)
        CALL model_write_state_4byte(292)
     END DO
  END DO

  CLOSE(291); CLOSE(292)

  STOP
END PROGRAM particle_run

