!===================================================================
! [PURPOSE:] 
!> @brief Creates an Speedy restart file with all variables set to zero
!  which is used in the initial step of time-averaged DA ensemble runs
!> @author Walter Acevedo (21.08.2013)
!===================================================================
PROGRAM create_null_speedy_state
    USE common
    USE common_speedy
    USE common_obs_speedy

    IMPLICIT NONE
    REAL(r_size),ALLOCATABLE ::        v3d(:,:,:,:) ! 3D fields
    REAL(r_size),ALLOCATABLE ::        v2d(:,:,:)   ! 2D fields
    character(50)            :: bynary_file
    LOGICAL,PARAMETER        :: verbose   =.TRUE.
  
    ALLOCATE(       v3d(nlon,nlat,nlev,nv3d)) ! 3d state variables
    ALLOCATE(       v2d(nlon,nlat,nv2d))      ! 2d state variables
    
    v3d = 0.d0; v2d = 0.d0

    ! Saving output
    call write_grd('null_speedy_state_grid_sigma.rst', v3d, v2d)

    DEALLOCATE(v3d,v2d)
    
    write(*,*)'null state Speedy restart file was created' 
    
    STOP
END PROGRAM create_null_speedy_state
