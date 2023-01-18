!===================================================================
!> @brief Calculates the mean state over a set of state binary files.
!> @author Walter Acevedo (03.08.2013)
!===================================================================
PROGRAM mean
    USE common
    USE common_speedy
    USE common_obs_speedy

    IMPLICIT NONE
    REAL(r_size),ALLOCATABLE :: v3d     (:,:,:,:) ! 3D fields
    REAL(r_size),ALLOCATABLE :: v2d     (:,:,:)   ! 2D fields
    REAL(r_size),ALLOCATABLE :: v3d_mean(:,:,:,:) ! 3D fields
    REAL(r_size),ALLOCATABLE :: v2d_mean(:,:,:)   ! 2D fields
    CHARACTER(50)            :: state_file_list, mean_file, bynary_file
    integer                  :: n_args, ios, n_state
    integer                  :: in_unit_1 = 134
    LOGICAL,PARAMETER        :: verbose   =.FALSE.
    
    n_args = IARGC()
    IF (n_args /= 2) then
        WRITE(0,*) 'Error. Incorrect arguments number'
        WRITE(0,*) 'Usage: $ mean state_file_list mean_file'
        CALL EXIT (1)
    END IF 
     
    CALL GETARG(1,state_file_list)
    CALL GETARG(2,mean_file)
  
    ALLOCATE(v3d     (nlon,nlat,nlev,nv3d)) ! 3d state variables
    ALLOCATE(v2d     (nlon,nlat,nv2d))      ! 2d state variables
    ALLOCATE(v3d_mean(nlon,nlat,nlev,nv3d)) ! 3d state variables
    ALLOCATE(v2d_mean(nlon,nlat,nv2d))      ! 2d state variables
    
    v3d_mean = 0.d0
    v2d_mean = 0.d0
  
    ! Reading files
    open(in_unit_1, FILE=state_file_list, FORM='FORMATTED')
    n_state = 0
    do        
        read(in_unit_1,'(A)',IOSTAT=ios) bynary_file
        IF(ios /= 0) EXIT
        n_state = n_state + 1

        if (verbose) print*, bynary_file
        CALL read_grd(bynary_file,v3d,v2d)
        
        ! Acummulating
        v3d_mean = v3d_mean + v3d
        v2d_mean = v2d_mean + v2d
    enddo
    close(in_unit_1)
    write(*,*) n_state, ' files were read'
    
    v3d_mean = v3d_mean / n_state
    v2d_mean = v2d_mean / n_state

    ! Saving output
    call write_grd(mean_file, v3d_mean, v2d_mean)

    DEALLOCATE(v3d,v3d_mean,v2d,v2d_mean)
    write(*,*) ' Mean state was stored in ', trim(mean_file)
    
    STOP
END PROGRAM mean
