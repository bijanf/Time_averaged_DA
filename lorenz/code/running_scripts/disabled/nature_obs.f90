PROGRAM nature_obs
  USE common
  USE common_das_tools
  USE model_core
  USE model_obs_operator
  USE common_particle
  IMPLICIT NONE

  REAL(r_size) ::               x(nx)
  REAL(r_size) ::       obs_Taver(ny,1) !,state_Tanom
  REAL(r_size) ::               y(ny)
  REAL(r_size) ::         y1_acum(ny)
  REAL(r_size) ::         y2_acum(ny)
  REAL(r_size) :: clean_obs_Tmean(ny)
  REAL(r_size) ::  clean_obs_Tvar(ny)
  REAL(r_size),ALLOCATABLE :: state_Insta_array(:,:,:)
  REAL(r_size),ALLOCATABLE ::   obs_Insta_array(:,:,:)
  INTEGER :: Taver_steps! in time steps (1: instantaneous obs)
  CALL get_int_par_env ('Taver_steps',Taver_steps)

  CALL   trajectory_initialize
  ALLOCATE(state_Insta_array(nx,1,cycle_length))
  ALLOCATE(  obs_Insta_array(ny,1,cycle_length))


  ! - Calculate variance of clean observations

  !CALL obs_operator_initialize

  OPEN(91,FILE=    'nature_run_8byte.dat',FORM='unformatted')
  OPEN(92,FILE='nature_obs_clean_Insta.dat',FORM='unformatted')

  y1_acum=0; y2_acum=0

  IF(Taver_steps.GT.cycle_length)THEN
     PRINT*,'Error: Unsupported time averaging lenght'; STOP 1
  END IF

  DO ic = 1,cycles
     IF(MODULO(ic,100).EQ.0) PRINT  '(A,I6)','  cycle ',ic

     DO icl=1,cycle_length
        READ(91) x
        state_Insta_array(:,1,icl) = x
        obs_Insta_array(:,1,icl) = clean_obs(x)
        WRITE(92) clean_obs(x)
     END DO

     obs_Taver = SUM(obs_Insta_array &
          &           (:,:,cycle_length-Taver_steps+1:),3)/Taver_steps
     y = obs_Taver(:,1)

     y1_acum = y1_acum + y
     y2_acum = y2_acum + y**2

  END DO

  clean_obs_Tmean =  y1_acum / cycles
  clean_obs_Tvar  = (y2_acum - cycles * clean_obs_Tmean**2 ) / (cycles - 1)

  CALL com_write_1Dfield_4byte('nature_clean_obs_Tvar',ny,'var',clean_obs_Tvar)
  CLOSE(91); CLOSE(92)

  !  ! - Create noisy observations

  !     CALL obs_operator_initialize_noise

  !     OPEN(91,FILE='nature_state_Insta.dat',FORM='unformatted')
  !     OPEN(92,FILE=  'nature_obs_dirty.dat',FORM='unformatted')

  !  DO ic = 1,cycles
  !             IF(MODULO(ic,50).EQ.0) PRINT  '(A,I6)','  cycle ',ic

  !             DO icl=1,cycle_length
  !                     READ(91) x
  !             END DO

  !             WRITE(92) dirty_obs(x)
  !!            y = dirty_obs(x); WRITE(92) REAL(y,r_sngl)

  !  END DO

  !  CLOSE(91); CLOSE(92)

  STOP
END PROGRAM nature_obs
