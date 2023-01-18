!=======================================================================
! Lorenz96 2-component model with periodic forcing
!=======================================================================
MODULE model_core
  ! USE common
  USE common_tools
  USE ensemble_size
  USE station_data
  IMPLICIT NONE
  SAVE
  !--------------------------------------------------
  ! Dynamical system
  !--------------------------------------------------
  CHARACTER(30),PARAMETER :: model_name = 'L96_2s_pf'
  INTEGER      ,PARAMETER :: nc         = 40        ! Component size
  INTEGER      ,PARAMETER :: n_comp     = 2         ! Number of components
  CHARACTER(5) ,PARAMETER :: comp_name(n_comp)=(/'comp1','comp2'/)
  INTEGER      ,PARAMETER :: comp_size(n_comp)=(/nc,nc/)! Comp state size
  INTEGER      ,PARAMETER :: nx         = nc*n_comp ! Total model state size
  ! Component vars distribution in the model state vector
  INTEGER      ,PARAMETER :: comp_var_first(n_comp)=(/1 ,nc+1/)! First component var.
  INTEGER      ,PARAMETER :: comp_var_final(n_comp)=(/nc,  nx/)! Final component var.
  ! Spatial distribution of model state vector (Relevant for obs)
  LOGICAL      ,PARAMETER:: spatially_extended =.TRUE. 
  INTEGER                 :: i_pos !,j_pos
  INTEGER      ,PARAMETER :: var_pos(nx)=(/((i_pos, i_pos=1,nc),j_pos=1,2)/)
  !  INTEGER      ,PARAMETER :: var_pos(nx)=(/((i_pos+nc*(j_pos), i_pos=1,nc),j_pos=0,2,2)/)

  REAL(r_dble) ,PARAMETER:: initial_state(nx)=(/((i_pos/nc, i_pos=1,nc),j_pos=1,2)/)

  !--------------------------------
  ! Dynamical system parameters
  !--------------------------------
  REAL(r_dble) ,PROTECTED :: force           ! Comp. 1 forcing term
  REAL(r_dble) ,PROTECTED :: F_mean = 9.0d0
  REAL(r_dble) ,PROTECTED :: F_ampl = 1.0d0
  REAL(r_dble) ,PROTECTED :: F_tau  = 0.5d0
  REAL(r_dble) ,PROTECTED :: h      = 1.0d0  ! Coupling constant
  REAL(r_dble) ,PROTECTED :: c      = 0.1d0  ! Comp. 2 time scale factor
  REAL(r_dble) ,PROTECTED :: b      = 1.0d0  ! Comp. 2 inverse amplitude factor
  REAL(r_dble) ,PROTECTED :: k1,k2
  
  !-----------------------
  ! Observation operator
  !-----------------------
  ! INTEGER      ,PARAMETER :: ny=nx
  ! INTEGER      ,PARAMETER :: station_pos(ny)=(/(i_pos, i_pos=1,ny)/)
!   INTEGER      ,PARAMETER :: ny = nc/2
!   INTEGER      ,PARAMETER :: station_pos(ny)=(/(i_pos*2, i_pos=1,ny)/)
  ! INTEGER,PARAMETER       :: ny=13
  ! INTEGER                 :: station_pos(ny)=(/1,4,7,10,13,16,19,22,25,28,31,34,37/)
  ! INTEGER,ALLOCATABLE     :: station_pos  (:)

CONTAINS

  SUBROUTINE model_get_par_values
    CALL print_line
    PRINT*,' Model config'
    CALL print_line
    CALL set_string_par_file('model_name',model_name)
    CALL get_real_par_env   ('F_mean'    ,F_mean    )
    CALL get_real_par_env   ('F_ampl'    ,F_ampl    )
    CALL get_real_par_env   ('F_tau'     ,F_tau     )
    CALL get_real_par_env   ('h'         ,h         )
    CALL get_real_par_env   ('c'         ,c         )
    CALL get_real_par_env   ('b'         ,b         )
    CALL print_line
    
    k1 = h*c/b; k2 = c*b
    
  END SUBROUTINE model_get_par_values

  FUNCTION F(t,x)
    REAL(r_dble),INTENT(IN)  :: x(nx)
    REAL(r_dble),INTENT(IN)  :: t
    REAL(r_dble)             :: F(nx)
    INTEGER                  :: i

    force = F_mean + F_ampl * SIN (2.0d0 * pi * t / F_tau )

    ! Component 1 (state variables 1 to nc)
    F  (1) = force + x(     nc) * (x(       2) - x(  nc-1)) -    x(       1) - k1 * x( 1+nc)
    F  (2) = force + x(      1) * (x(       3) - x(  nc  )) -    x(       2) - k1 * x( 2+nc)
    DO i=3,nc-1
       F(i)= force + x(    i-1) * (x(     i+1) - x(   i-2)) -    x(       i) - k1 * x( i+nc)
    END DO 
    F (nc) = force + x(   nc-1) * (x(       1) - x(  nc-2)) -    x(      nc) - k1 * x(nc+nc)

    ! Component 2 (state variables nc+1 to nx)
    F   (1+nc)= k2 * x(  2 +nc) * (x(  nc +nc) - x(  3+nc)) - c* x(   1 +nc) + k1 * x(    1)
    DO i=2,nc-2
       F(i+nc)= k2 * x(i+1 +nc) * (x( i-1 +nc) - x(i+2+nc)) - c* x(   i +nc) + k1 * x(    i)
    END DO
    F(nc-1+nc)= k2 * x( nc +nc) * (x(nc-2 +nc) - x(  1+nc)) - c* x(nc-1 +nc) + k1 * x( nc-1)
    F(nc  +nc)= k2 * x(  1 +nc) * (x(nc-1 +nc) - x(  2+nc)) - c* x(nc   +nc) + k1 * x( nc  )

  END FUNCTION F
  
END MODULE model_core
