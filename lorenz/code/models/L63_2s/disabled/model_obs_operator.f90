!=======================================================================
! observation operator
!=======================================================================
MODULE model_obs_operator
  USE common
  USE common_das_tools
  USE model_core
  IMPLICIT NONE

  INTEGER,PARAMETER        :: ny=20
  INTEGER :: station_pos(ny)=(/2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40/)
!  INTEGER,PARAMETER        :: ny=13
!  INTEGER :: station_pos(ny)=(/1,4,7,10,13,16,19,22,25,28,31,34,37/)
!  REAL(r_size),PROTECTED   :: obserr=1.0d0
  REAL(r_size)             :: SNR !   = 10.0d0
!  INTEGER     ,ALLOCATABLE :: station_pos  (:)
  REAL(r_size),ALLOCATABLE :: obs_error_std(:)
  LOGICAL                  :: ready=.FALSE.

CONTAINS

SUBROUTINE obs_operator_initialize_noise
!  REAL(r_size) :: clean_obs_Tvar(ny)
  REAL(r_sngl) :: clean_obs_Tvar(ny)
!  ALLOCATE(  station_pos(ny))
  ALLOCATE(obs_error_std(ny))
  
!  ny=13
!  station_pos = (/1,4,7,10,13,16,19,22,25,28,31,34,37/)

	OPEN (111,FILE='nature_clean_obs_Tvar_4byte.dat',FORM='unformatted',STATUS='OLD')
!	OPEN (111,FILE='clean_obs_Tvar.dat',FORM='unformatted',STATUS='OLD')
	READ (111) clean_obs_Tvar
	CLOSE(111)
  CALL get_real_par_env ('SNR',SNR)
	obs_error_std = SQRT(clean_obs_Tvar/SNR)
	ready         = .TRUE.
END SUBROUTINE

SUBROUTINE initialize_error
  PRINT*,'Error: Observation operator has been initialized'
	STOP 1
END SUBROUTINE

FUNCTION clean_obs(x)
	REAL(r_size) :: x(nx)
	REAL(r_size) :: clean_obs(ny)
	
!  IF (.NOT. ready) CALL initialize_error
		
	clean_obs = x(station_pos)
	
END FUNCTION clean_obs

FUNCTION dirty_obs(x)
	REAL(r_size) :: x(nx)
	REAL(r_size) :: dirty_obs(ny)
  REAL(r_size) :: gaussian_noise(ny)
  
  IF (.NOT. ready) CALL initialize_error
	
	CALL com_randn(ny,gaussian_noise)
  dirty_obs  = clean_obs(x) + gaussian_noise * obs_error_std

END FUNCTION dirty_obs

END MODULE model_obs_operator

!FUNCTION dirty_obs(x)
!	REAL(r_size) :: x(nx)
!	REAL(r_size) :: dirty_obs(ny)
!  REAL(r_size) :: gaussian_noise(ny)
	
!	CALL com_randn(ny,gaussian_noise)
!  dirty_obs  = clean_obs(x) + gaussian_noise * obserr

!END FUNCTION dirty_obs
