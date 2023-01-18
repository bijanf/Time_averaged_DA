!===================================================================
! [PURPOSE:] 
!> @brief Calculates the difference between 2 gridded sigma speedy
!> restart files
!> @author Walter Acevedo (21.08.2013)
!===================================================================
PROGRAM calculate_difference
    USE common
    USE common_speedy
    USE common_obs_speedy

    IMPLICIT NONE
    REAL(r_size),ALLOCATABLE :: v3d_1   (:,:,:,:) ! 3D fields
    REAL(r_size),ALLOCATABLE :: v2d_1   (:,:,:)   ! 2D fields
    REAL(r_size),ALLOCATABLE :: v3d_2   (:,:,:,:) ! 3D fields
    REAL(r_size),ALLOCATABLE :: v2d_2   (:,:,:)   ! 2D fields
    REAL(r_size),ALLOCATABLE :: v3d_diff(:,:,:,:) ! 3D fields
    REAL(r_size),ALLOCATABLE :: v2d_diff(:,:,:)   ! 2D fields
    CHARACTER(50)            :: file_1,file_2,file_diff
    integer                  :: n_args, ios
    LOGICAL,PARAMETER        :: verbose   =.TRUE.
    
    n_args = IARGC()
    IF (n_args /= 3) then
        WRITE(0,*) 'Error. Incorrect arguments number'
        WRITE(0,*) 'Usage: $ calculate_difference.exe file1.rst file2.rst diff_file.rst'
        CALL EXIT (1)
    END IF 
     
    CALL GETARG(1,file_1)
    CALL GETARG(2,file_2)
    CALL GETARG(3,file_diff)
    
    write(*,*) 'Calculating difference between '
    write(*,*) trim(file_1), ' and ', trim(file_2)

    ALLOCATE(    v3d_1(nlon,nlat,nlev,nv3d)) ! 3d state variables
    ALLOCATE(    v2d_1(nlon,nlat,nv2d))      ! 2d state variables
    ALLOCATE(    v3d_2(nlon,nlat,nlev,nv3d)) ! 3d state variables
    ALLOCATE(    v2d_2(nlon,nlat,nv2d))      ! 2d state variables
    ALLOCATE( v3d_diff(nlon,nlat,nlev,nv3d)) ! 3d state variables
    ALLOCATE( v2d_diff(nlon,nlat,nv2d))      ! 2d state variables
  
    ! Reading files
    CALL read_grd(file_1,v3d_1,v2d_1)
    CALL read_grd(file_2,v3d_2,v2d_2)

    v3d_diff = v3d_1 - v3d_2
    v2d_diff = v2d_1 - v2d_2
    
    CALL write_grd(file_diff, v3d_diff, v2d_diff)

    DEALLOCATE(v3d_1,v3d_2,v2d_1,v2d_2,v3d_diff,v2d_diff)
    
    write(*,*)'difference file ', trim(file_diff), ' was created' 
    
    STOP
END PROGRAM calculate_difference
