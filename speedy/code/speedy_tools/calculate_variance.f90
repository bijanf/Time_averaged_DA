!===================================================================
!> @brief Calculates the Variance of the state variables over
!>        a set of state binary files.
!> Formula used:
!> \f$ var=\frac{1}{n-1}\sum_{i=1}^n (x_i - \overline{x})^2
!>        =\frac{1}{n-1}(\sum x_i^2 - n\overline{x}^2)\f$
!>
!> @author Walter Acevedo (03.08.2013)
!===================================================================
PROGRAM calculate_variance
    USE common
    USE common_speedy
    USE common_obs_speedy

    IMPLICIT NONE
    REAL(r_size),ALLOCATABLE ::        v3d(:,:,:,:) ! 3D fields
    REAL(r_size),ALLOCATABLE ::   v3d_acum(:,:,:,:) ! 3D fields
    REAL(r_size),ALLOCATABLE ::   v3d_mean(:,:,:,:) ! 3D fields
    REAL(r_size),ALLOCATABLE :: v3d_2_acum(:,:,:,:) ! 3D fields
    REAL(r_size),ALLOCATABLE ::    v3d_var(:,:,:,:) ! 3D fields
    REAL(r_size),ALLOCATABLE ::        v2d(:,:,:)   ! 2D fields
    REAL(r_size),ALLOCATABLE ::   v2d_acum(:,:,:)   ! 2D fields
    REAL(r_size),ALLOCATABLE ::   v2d_mean(:,:,:)   ! 2D fields
    REAL(r_size),ALLOCATABLE :: v2d_2_acum(:,:,:)   ! 2D fields
    REAL(r_size),ALLOCATABLE ::    v2d_var(:,:,:)   ! 2D fields
    CHARACTER(50)            :: state_file_list != 'state_file_list.txt'
    CHARACTER(50)            :: variance_file  ! = 'variance.grd'
    character(50)            :: bynary_file
    integer                  :: n_state !, n_states
    integer                  :: in_unit_1 = 134
    integer                  :: n_args, ios
    LOGICAL,PARAMETER        :: verbose   =.TRUE.
    
    n_args = IARGC()
    IF (n_args /= 2) then
        WRITE(0,*) 'Error. Incorrect arguments number'
        WRITE(0,*) 'Usage: $ calculate_variance state_file_list variance_file'
        CALL EXIT (1)
    END IF 
     
    CALL GETARG(1,state_file_list)
    CALL GETARG(2,variance_file)
  
    ALLOCATE(       v3d(nlon,nlat,nlev,nv3d)) ! 3d state variables
    ALLOCATE(  v3d_acum(nlon,nlat,nlev,nv3d)) ! 3d state variables
    ALLOCATE(  v3d_mean(nlon,nlat,nlev,nv3d)) ! 3d state variables
    ALLOCATE(v3d_2_acum(nlon,nlat,nlev,nv3d)) ! 3d state variables
    ALLOCATE(   v3d_var(nlon,nlat,nlev,nv3d)) ! 3d state variables
    ALLOCATE(       v2d(nlon,nlat,nv2d))      ! 2d state variables
    ALLOCATE(  v2d_acum(nlon,nlat,nv2d))      ! 2d state variables
    ALLOCATE(  v2d_mean(nlon,nlat,nv2d))      ! 2d state variables
    ALLOCATE(v2d_2_acum(nlon,nlat,nv2d))      ! 2d state variables
    ALLOCATE(   v2d_var(nlon,nlat,nv2d))      ! 2d state variables
    
      v3d_acum = 0.d0
    v3d_2_acum = 0.d0
      v2d_acum = 0.d0
    v2d_2_acum = 0.d0
  
    ! Reading files
    open(in_unit_1, FILE=state_file_list, FORM='FORMATTED')
    n_state = 0
    do        
        read(in_unit_1,'(A)',IOSTAT=ios) bynary_file
        IF(ios /= 0) EXIT
        n_state = n_state + 1

        !write(6,*) bynary_file
        CALL read_grd(bynary_file,v3d,v2d)
        
        ! Acummulating
          v3d_acum =   v3d_acum + v3d
          v2d_acum =   v2d_acum + v2d
        v3d_2_acum = v3d_2_acum + (v3d * v3d)
        v2d_2_acum = v2d_2_acum + (v2d * v2d)
    enddo
    close(in_unit_1)
    write(*,*) n_state, ' files were read'
    
      v3d_mean =   v3d_acum / n_state
      v2d_mean =   v2d_acum / n_state

    v3d_var = (v3d_2_acum - n_state*(v3d_mean * v3d_mean)) / (n_state - 1)
    v2d_var = (v2d_2_acum - n_state*(v2d_mean * v2d_mean)) / (n_state - 1)

    ! Saving output
    call write_grd(variance_file, v3d_var, v2d_var)

    DEALLOCATE(v3d,v3d_acum,v3d_2_acum,v3d_var)
    DEALLOCATE(v2d,v2d_acum,v2d_2_acum,v2d_var)
    
    STOP
END PROGRAM calculate_variance
