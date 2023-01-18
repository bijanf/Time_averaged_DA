!=======================================================================
!> [Lorenz 63 2 scale model, Pena & Kalnay 2004](http://www.nonlin-processes-geophys.net/11/319/2004/npg-11-319-2004.html)
!=======================================================================
MODULE model_core
  USE common_tools
  IMPLICIT NONE
  SAVE
  !====================================================================
  ! Dynamical system
  !====================================================================
  CHARACTER(30),PARAMETER:: model_name = 'L63_2s'
  INTEGER      ,PARAMETER:: nc         = 3        ! Component size
  INTEGER      ,PARAMETER:: n_comp     = 2         ! Number of components
  CHARACTER(5) ,PARAMETER:: comp_name(n_comp)=(/'comp1','comp2'/)
  INTEGER      ,PARAMETER:: comp_size(n_comp)=(/nc,nc/)! Comp state size
  INTEGER      ,PARAMETER:: nx         = nc*n_comp ! Total model state size
  ! Component vars distribution in the model state vector
  INTEGER      ,PARAMETER:: comp_var_first(n_comp)=(/1 ,nc+1/)! First component var.
  INTEGER      ,PARAMETER:: comp_var_final(n_comp)=(/nc,  nx/)! Final component var.
  ! Spatial distribution of model state vector (Relevant for obs)
  LOGICAL      ,PARAMETER:: spatially_extended =.FALSE. 
  INTEGER                :: i_pos,j_pos
  INTEGER      ,PARAMETER:: var_pos(nx)=(/((i_pos, i_pos=1,nc),j_pos=1,2)/)
  !  INTEGER ,PARAMETER :: var_pos(nx)=(/((i_pos+nc*(j_pos), i_pos=1,nc),j_pos=0,2,2)/)
  REAL(r_dble),PARAMETER:: initial_state(nx) = (/1,0,0,1,0,0/)

  ! Model parameters
  ! Standard Lorenz 63 paramaters
  REAL(r_dble),PARAMETER:: sigma = 10.0, b = 8.0/3.0, r = 28.0
  ! Coupling parameters 
  REAL(r_dble),PARAMETER:: tao = 0.1 ! Temporal scale factor
  REAL(r_dble),PROTECTED:: c, cz     ! x-y, z Coupling strengths
  REAL(r_dble),PROTECTED:: S         ! Amplitude scale factor
  REAL(r_dble),PROTECTED:: k1        ! Offset (uncentering parameter)
  ! Original Pena & Kalnay coupling configurations
  !  %c = 0.0; cz = 0.0; S = 1.0; k1 = 10; % Uncoupled
  !  %c = 0.15; cz = 0.0; S = 0.1; k1 = 10; % Weather/convection
  !   c = 0.15, cz = 0.0, S = 1.0, k1 = 10 ! Extratropical Ocen/atm. 1
  !  %c = 0.08; cz = 0.0; S = 1.0; k1 = 10; % Extratropical Ocen/atm. 2
  !  %c = 1.0; cz = 1.0; S = 1.0; k1 = -11; % ENSO
  ! Tödler et al coupling configuration for shallow-deep soil layers
  ! %c  = 0.1; cz = 0.1; S = 8.0; k1 = 10.0; % Weak coupling
  ! %c  = 0.5; cz = 0.5; S = 8.0; k1 = 10.0; % Intermediate coupling
  ! %c  = 0.9; cz = 0.9; S = 8.0; k1 = 10.0; % Strong coupling


  !====================================================================
  ! Parameter Configuration
  !====================================================================

  !====================================================================
  ! Observation operator
  !====================================================================
  INTEGER      ,PARAMETER :: ny=nc
  INTEGER      ,PARAMETER :: station_pos(ny)=(/(i_pos, i_pos=1,ny)/)
  ! INTEGER,PARAMETER       :: ny=13
  ! INTEGER                 :: station_pos(ny)=(/1,4,7,10,13,16,19,22,25,28,31,34,37/)
  ! INTEGER,ALLOCATABLE     :: station_pos  (:)

CONTAINS

  SUBROUTINE model_get_par_values
    CALL print_line
    PRINT*,' Model config'
    CALL print_line
    CALL set_string_par_file('model_name',model_name)
    CALL get_real_par_env   ('c' ,c )
    CALL get_real_par_env   ('cz',cz)
    CALL get_real_par_env   ('S' ,S )
    CALL get_real_par_env   ('k1',k1)
    CALL print_line
  END SUBROUTINE model_get_par_values

  FUNCTION F(t,x)
    REAL(r_dble),INTENT(IN)  :: x(nx)
    REAL(r_dble),INTENT(IN)  :: t
    REAL(r_dble)             :: F(nx)

    ! Fast subsystem
    F(1) =        sigma * ( x(2) - x(1) )        - c  * ( S*x(4) + k1 );
    F(2) =        r* x(1) - x(2) - x(1) * x(3)   + c  * ( S*x(5) + k1 );
    F(3) =           x(1) * x(2) -    b * x(3)   + cz *     x(6);
    
    ! Slow subsystem
    F(4) = tao *  sigma * ( x(5) - x(4) )        - c  * (   x(1) + k1 );
    F(5) = tao *( r* x(4) - x(5) - S*x(4)*x(6) ) + c  * (   x(2) + k1 );
    F(6) = tao *( S* x(4) * x(5) - b*x(6)      ) - cz *     x(3);

  END FUNCTION F
  
END MODULE model_core
