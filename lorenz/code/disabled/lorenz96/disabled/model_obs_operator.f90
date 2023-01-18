!=======================================================================
! observation operator
!=======================================================================
MODULE model_obs_operator
  USE common
  USE common_das_tools
  USE model_core
  IMPLICIT NONE

 !  INTEGER,PARAMETER :: ny=nx
!   INTEGER           :: station_pos(ny)=(/(i_pos, i_pos=1,ny)/)
!   ! INTEGER,PARAMETER :: ny=nx/2
!   ! INTEGER           :: station_pos(ny)=(/(i_pos*2, i_pos=1,ny)/)
!   ! INTEGER,PARAMETER :: ny=13
!   ! INTEGER           :: station_pos(ny)=(/1,4,7,10,13,16,19,22,25,28,31,34,37/)
!   ! INTEGER,ALLOCATABLE :: station_pos  (:)

!   REAL(r_size)      :: SNR !   = 10.0d0
!   REAL(r_size)      :: obs_error_std(ny)
!   LOGICAL           :: ready=.FALSE.

! CONTAINS

!   SUBROUTINE obs_operator_initialize_noise(obs_var_filename)
!     CHARACTER(*) :: obs_var_filename
!     REAL(r_sngl) :: obs_clean_Taver_Tvar(ny)

!     CALL print_line
!     PRINT*,' Obs. noise config'
!     CALL print_line
!     CALL get_real_par_env ('SNR',SNR)
!     CALL print_line

!     OPEN (111,FILE=obs_var_filename//'.dat',FORM='unformatted',STATUS='OLD')
!     READ (111) obs_clean_Taver_Tvar
!     CLOSE(111)
!     obs_error_std = SQRT(obs_clean_Taver_Tvar/SNR)
!     ready         = .TRUE.
!   END SUBROUTINE obs_operator_initialize_noise

!   SUBROUTINE initialize_error
!     PRINT*,'Error: Observation operator has been initialized'
!     STOP 1
!   END SUBROUTINE initialize_error

!   FUNCTION clean_obs(x)
!     REAL(r_size) :: x(nx)
!     REAL(r_size) :: clean_obs(ny)

!     !  IF (.NOT. ready) CALL initialize_error

!     clean_obs = x(station_pos)

!   END FUNCTION clean_obs

!   FUNCTION sully_obs(clean_y)
!     REAL(r_size) :: clean_y(ny)
!     REAL(r_size) :: sully_obs(ny)
!     REAL(r_size) :: gaussian_noise(ny)

!     IF (.NOT. ready) CALL initialize_error

!     CALL com_randn(ny,gaussian_noise)
!     sully_obs  = clean_y + gaussian_noise * obs_error_std

!   END FUNCTION sully_obs

END MODULE model_obs_operator
