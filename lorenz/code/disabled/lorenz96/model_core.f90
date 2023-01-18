!=======================================================================
! Lorenz96 model dynamics definition
!=======================================================================
MODULE model_core
  USE common
  USE common_das_tools
  IMPLICIT NONE

  INTEGER,PARAMETER,PUBLIC ::     nx = 40      ! number of grid points
  INTEGER,PARAMETER,PUBLIC :: n_comp  = 1      ! Number of components
  INTEGER,PARAMETER,PUBLIC :: comp_size(n_comp) = 40      ! Number of components

  REAL(r_size),PROTECTED   ::   force =  8.0d0   ! F term
  ! REAL(r_size),PARAMETER   ::      dt = 0.005d0 ! time of one time step
  REAL(r_size),PARAMETER   ::      dt = 0.01d0 ! time of one time step
  REAL(r_size),PARAMETER   ::  oneday =  0.2d0 ! time for one day
  REAL(r_size),PARAMETER   :: alpha1  =  0.5d0 ! Component 1 Time scaling constant
  INTEGER     ,PARAMETER   :: dts_per_day = INT(oneday/dt)
  INTEGER :: i_pos
  INTEGER :: var_pos(nx)=(/(i_pos, i_pos=1,nx)/)
  INTEGER,PARAMETER,PUBLIC :: comp_var_first(n_comp) =(/1/)! First component var.
  INTEGER,PARAMETER,PUBLIC :: comp_var_final(n_comp) =(/nx/)! Final component var.

  INTEGER,PARAMETER :: ny=nx
  INTEGER           :: station_pos(ny)=(/(i_pos, i_pos=1,ny)/)
  ! INTEGER,PARAMETER :: ny=nx/2
  ! INTEGER           :: station_pos(ny)=(/(i_pos*2, i_pos=1,ny)/)
  ! INTEGER,PARAMETER :: ny=13
  ! INTEGER           :: station_pos(ny)=(/1,4,7,10,13,16,19,22,25,28,31,34,37/)
  ! INTEGER,ALLOCATABLE :: station_pos  (:)

CONTAINS

  SUBROUTINE model_tendencies(x1,t,dx1_dt)
    REAL(r_size),INTENT(IN)  :: x1(1:nx)
    REAL(r_size),INTENT(IN)  :: t
    REAL(r_size),INTENT(OUT) :: dx1_dt(1:nx)
    INTEGER                  :: i

    dx1_dt(1)    = ( x1(  nx) * ( x1(  2) - x1(nx-1) ) - x1( 1) + force )/alpha1
    dx1_dt(2)    = ( x1(   1) * ( x1(  3) - x1(nx  ) ) - x1( 2) + force )/alpha1
    DO i=3,nx-1
       dx1_dt(i) = ( x1( i-1) * ( x1(i+1) - x1( i-2) ) - x1( i) + force )/alpha1
    END DO
    dx1_dt (nx)  = ( x1(nx-1) * ( x1(  1) - x1(nx-2) ) - x1(nx) + force )/alpha1

    dx1_dt (:)   = dt * dx1_dt(:)
  END SUBROUTINE model_tendencies


  FUNCTION clean_obs(x)
    REAL(r_size) :: x(nx)
    REAL(r_size) :: clean_obs(ny)

    !  IF (.NOT. ready) CALL initialize_error

    clean_obs = x(station_pos)

  END FUNCTION clean_obs

END MODULE model_core
