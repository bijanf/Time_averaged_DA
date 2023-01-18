!=======================================================================
! Lorenz96 model with time periodic forcing
!=======================================================================
MODULE model_core
  USE common
  USE common_tools
  IMPLICIT NONE
  SAVE
  !====================================================================
  ! Dynamical system
  !====================================================================
  CHARACTER(30),PARAMETER :: model_name        = 'L96_1s_pf'
  INTEGER      ,PARAMETER :: nc                = 40   ! Component size
  INTEGER      ,PARAMETER :: n_comp            = 1    ! Number of components
  CHARACTER(5) ,PARAMETER :: comp_name(n_comp) = (/'comp1'/)
  INTEGER      ,PARAMETER :: comp_size(n_comp) = (/nc/)
  INTEGER      ,PARAMETER :: nx                = nc*n_comp ! Total model state size
  ! Component vars distribution in the model state vector
  INTEGER      ,PARAMETER :: comp_var_first(n_comp) =(/1/) ! First component var.
  INTEGER      ,PARAMETER :: comp_var_final(n_comp) =(/nc/)! Final component var.
  LOGICAL      ,PARAMETER :: spatially_extended     =.TRUE.   
  INTEGER                 :: i_pos
  INTEGER      ,PARAMETER :: var_pos(nc)=(/(i_pos, i_pos=1,nc)/)
  REAL(r_dble) ,PARAMETER :: initial_state(nx) = (/(i_pos/nc, i_pos=1,nc)/)

  ! Parameters
  REAL(r_size) ,PROTECTED :: force               ! Forcing term
  REAL(r_size) ,PROTECTED :: F_mean     =  8.0d0
  REAL(r_size) ,PROTECTED :: F_ampl     =  1.0d0
  REAL(r_size) ,PROTECTED :: F_tau      =  0.5d0
  REAL(r_size) ,PARAMETER :: alpha1     =  0.5d0 ! Comp. 1 Time scaling constant
  ! REAL(r_size),PARAMETER  :: dt          = 0.005d0
  ! REAL(r_size)            :: dt         = 0.01d0 ! model time step
  ! REAL(r_size) ,PARAMETER :: oneday     =  0.2d0 ! time for one day
  ! INTEGER      ,PARAMETER :: dts_per_day = INT(oneday/dt)

  !====================================================================
  ! Observation operator
  !====================================================================
  ! observe every grid point
  ! INTEGER      ,PARAMETER :: ny=nc
  ! INTEGER      ,PARAMETER :: station_pos(ny)=(/(i_pos, i_pos=1,ny)/)
  ! observe every second grid point
  INTEGER      ,PARAMETER :: ny=nc/2
  INTEGER      ,PARAMETER :: station_pos(ny)=(/(i_pos*2, i_pos=1,ny)/)
  ! observe every third grid point
  ! INTEGER,PARAMETER       :: ny=13
  ! INTEGER                 :: station_pos(ny)=(/1,4,7,10,13,16,19,22,25,28,31,34,37/)
  ! INTEGER,ALLOCATABLE     :: station_pos  (:)

CONTAINS

  SUBROUTINE model_get_par_values
    CALL print_line
    PRINT*,' Model config'
    CALL print_line
    CALL set_string_par_file('model_name',model_name)
    CALL get_real_par_env   ('F_mean',F_mean)
    CALL get_real_par_env   ('F_ampl',F_ampl)
    CALL get_real_par_env   ('F_tau' ,F_tau )
    CALL print_line
  END SUBROUTINE model_get_par_values

  FUNCTION F(t,x)
    REAL(r_size),INTENT(IN)  :: x(nx)
    REAL(r_size),INTENT(IN)  :: t
    REAL(r_size)             :: F(nx)
    INTEGER                  :: i

    force = F_mean + F_ampl * SIN( 2.0d0*pi*t / F_tau )

    F(1)    = ( x(  nx) * ( x(  2) - x(nx-1) ) - x( 1) + force )/alpha1
    F(2)    = ( x(   1) * ( x(  3) - x(nx  ) ) - x( 2) + force )/alpha1
    DO i=3,nx-1
       F(i) = ( x( i-1) * ( x(i+1) - x( i-2) ) - x( i) + force )/alpha1
    END DO
    F (nx)  = ( x(nx-1) * ( x(  1) - x(nx-2) ) - x(nx) + force )/alpha1

  END FUNCTION F

END MODULE model_core
