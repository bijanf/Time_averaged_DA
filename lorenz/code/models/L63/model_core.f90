!=======================================================================
!> [ Lorenz 63 model ]()
!=======================================================================
MODULE model_core
  USE common_tools
  IMPLICIT NONE
  SAVE
  !====================================================================
  ! Dynamical system
  !====================================================================
  CHARACTER(30),PARAMETER:: model_name       = 'L63'
  INTEGER      ,PARAMETER:: nc               = 3         ! Component size
  INTEGER      ,PARAMETER:: n_comp           = 1         ! Number of components
  CHARACTER(5) ,PARAMETER:: comp_name(n_comp)=(/'comp1'/)
  INTEGER      ,PARAMETER:: comp_size(n_comp)=(/nc/)     ! Comp state size
  INTEGER      ,PARAMETER:: nx               = nc*n_comp ! Total model state size
  ! Component vars distribution in the model state vector
  INTEGER      ,PARAMETER:: comp_var_first(n_comp)=(/1 /)! First component var.
  INTEGER      ,PARAMETER:: comp_var_final(n_comp)=(/nc/)! Final component var.
  ! Spatial distribution of model state vector (Relevant for localization)
  LOGICAL      ,PARAMETER:: spatially_extended =.FALSE. 
  INTEGER      ,PRIVATE  :: i_pos
  INTEGER      ,PARAMETER:: var_pos(nc)       = (/(1, i_pos=1,nc)/)
  REAL(r_dble) ,PARAMETER:: initial_state(nx) = (/1,0,0/)
  
  ! Standard Lorenz 63 paramaters
  REAL(r_dble),PARAMETER :: sigma = 10.0, b = 8.0/3.0, r = 28.0

  !====================================================================
  ! Observation operator
  !====================================================================
  ! Full observability
  ! INTEGER      ,PARAMETER :: ny=nc
  ! INTEGER      ,PRIVATE   :: j_pos
  ! INTEGER      ,PARAMETER :: station_pos(ny)=(/(j_pos, j_pos=1,ny)/)
  ! variables 1 and 2 observed
  INTEGER      ,PARAMETER :: ny=2
  INTEGER      ,PARAMETER :: station_pos(ny)=(/1,2/)

CONTAINS

  SUBROUTINE model_get_par_values
    CALL print_line
    PRINT*,' Model config'
    CALL print_line
    PRINT*,' No free parameters'
    CALL print_line
  END SUBROUTINE model_get_par_values

  FUNCTION F(t,x)
    REAL(r_dble),INTENT(IN)  :: x(nx)
    REAL(r_dble),INTENT(IN)  :: t
    REAL(r_dble)             :: F(nx)

    F(1) = sigma * ( x(2) - x(1) )     
    F(2) = r* x(1) - x(2) - x(1) * x(3)
    F(3) =    x(1) * x(2) -    b * x(3)
    
  END FUNCTION F
  
END MODULE model_core
