!=======================================================================
! Slow-fast mixture Lorenz96 model (Devised by S. Reich)
!=======================================================================
MODULE model_core
  USE common_tools
  USE ensemble_size
  IMPLICIT NONE
  SAVE
  !--------------------------------------------------
  ! Dynamical system
  !--------------------------------------------------
  CHARACTER(30),PARAMETER :: model_name       = 'L96_2s_mix'
  INTEGER      ,PARAMETER :: nc               = 40       ! Component size
  INTEGER      ,PARAMETER :: n_comp           = 2        ! Number of components
  CHARACTER(5) ,PARAMETER :: comp_name(n_comp)=(/'comp1','comp2'/)
  INTEGER      ,PARAMETER :: comp_size(n_comp)=(/nc,nc/) ! Comp state size
  INTEGER      ,PARAMETER :: nx               = nc*n_comp! Total model state size
  ! Component vars distribution in the model state vector
  INTEGER      ,PARAMETER :: comp_var_first(n_comp)=(/1 ,nc+1/)! First component var.
  INTEGER      ,PARAMETER :: comp_var_final(n_comp)=(/nc,  nx/)! Final component var.
  LOGICAL      ,PARAMETER :: spatially_extended    =.TRUE.   
  INTEGER                 :: i_pos ,j_pos
  INTEGER      ,PARAMETER :: var_pos(nx)=(/((i_pos, i_pos=1,nc),j_pos=1,2)/)
!  INTEGER      ,PARAMETER :: var_pos(nx)=(/((i_pos+nc*(j_pos), i_pos=1,nc),j_pos=0,2,2)/)
  
  REAL(r_dble) ,PARAMETER :: initial_state(nx) = (/((i_pos/nc, i_pos=1,nc),j_pos=1,2)/)

  !--------------------------------
  ! Dynamical system parameters
  !--------------------------------
  REAL(r_dble) ,PROTECTED :: force   ! Forcing
  REAL(r_dble) ,PROTECTED :: alpha1  ! Component 1 Time scaling constant
  REAL(r_dble) ,PROTECTED :: alpha2  ! Component 2 Time scaling constant
  REAL(r_dble) ,PROTECTED :: link    ! Coupling constant [0,0.35)
  !                                    link=0.4 makes the model crash
  ! REAL(r_dble) ,PROTECTED :: dt      ! time of one time step
  ! REAL(r_dble),PARAMETER   :: oneday  = 0.2d0    ! time for one day
  ! INTEGER     ,PARAMETER   :: dts_per_day = INT(oneday/dt)

  !-----------------------
  ! Observation operator
  !-----------------------
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
    CALL set_string_par_file('model_name'  ,model_name)    
    CALL get_real_par_env   ('force'       ,force)
    CALL get_real_par_env   ('alpha1'      ,alpha1)
    CALL get_real_par_env   ('alpha2'      ,alpha2)
    CALL get_real_par_env   ('link'        ,link)
    CALL print_line
  END SUBROUTINE model_get_par_values

  FUNCTION F(t,x)
    REAL(r_dble),INTENT(IN)  :: x(nx)
    REAL(r_dble),INTENT(IN)  :: t
    REAL(r_dble)             :: F(nx)
    INTEGER                  :: i

    ! Component 1 (state variables 1 to nc)
    F   (1  )  = ( ((1-link)* x(  nc   ) + link* x(  nc+nc)) * (x(  2   ) - x(nc-1   )) - x( 1   ) + force )/alpha1
    F   (2  )  = ( ((1-link)* x(   1   ) + link* x(   1+nc)) * (x(  3   ) - x(nc     )) - x( 2   ) + force )/alpha1
    DO i=3,nc-1
       F(i  )  = ( ((1-link)* x( i-1   ) + link* x( i-1+nc)) * (x(i+1   ) - x( i-2   )) - x( i   ) + force )/alpha1
    END DO
    F   (nc )  = ( ((1-link)* x(nc-1   ) + link* x(nc-1+nc)) * (x(  1   ) - x(nc-2   )) - x(nc   ) + force )/alpha1

    ! Component 2 (state variables nc+1 to nx)
    F   (1+nc) = ( ((1-link)* x(  nc+nc) + link* x(  nc   )) * (x(  2+nc) - x(nc-1+nc)) - x( 1+nc) + force )/alpha2
    F   (2+nc) = ( ((1-link)* x(   1+nc) + link* x(   1   )) * (x(  3+nc) - x(nc  +nc)) - x( 2+nc) + force )/alpha2
    DO i=3,nc-1
       F(i+nc) = ( ((1-link)* x( i-1+nc) + link* x( i-1   )) * (x(i+1+nc) - x( i-2+nc)) - x( i+nc) + force )/alpha2
    END DO
    F  (nc+nc) = ( ((1-link)* x(nc-1+nc) + link* x(nc-1   )) * (x(  1+nc) - x(nc-2+nc)) - x(nc+nc) + force )/alpha2
    !                                      ----------------
    !                                       Coupling term
  END FUNCTION F

END MODULE model_core
