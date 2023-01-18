!===================================================================
!> @brief Calculates the skewness of an ensemble of state binary files.
!> @author Walter Acevedo (03.12.2014)
!===================================================================
PROGRAM skewness
  USE common
  USE common_speedy
  USE common_obs_speedy

  IMPLICIT NONE

  REAL(r_size),ALLOCATABLE :: v2d     (:,:,:)   ! 2D fields
  REAL(r_size),ALLOCATABLE :: v2d_sum2(:,:,:)   
  REAL(r_size),ALLOCATABLE :: v2d_sum3(:,:,:)   
  REAL(r_size),ALLOCATABLE :: v2d_sum1(:,:,:)   
  REAL(r_size),ALLOCATABLE :: v2d_mean(:,:,:)   
  REAL(r_size),ALLOCATABLE :: v2d_stdd(:,:,:)   
  REAL(r_size),ALLOCATABLE :: v2d_skew(:,:,:)   
  REAL(r_size),ALLOCATABLE :: v3d     (:,:,:,:) ! 3D fields
  REAL(r_size),ALLOCATABLE :: v3d_sum1(:,:,:,:) 
  REAL(r_size),ALLOCATABLE :: v3d_sum2(:,:,:,:) 
  REAL(r_size),ALLOCATABLE :: v3d_sum3(:,:,:,:) 
  REAL(r_size),ALLOCATABLE :: v3d_mean(:,:,:,:)   
  REAL(r_size),ALLOCATABLE :: v3d_stdd(:,:,:,:)   
  REAL(r_size),ALLOCATABLE :: v3d_skew(:,:,:,:)
  
  CHARACTER(50)            :: state_file_list, skew_file, bynary_file
  integer                  :: n_args, ios, n_state
  integer                  :: in_unit_1 = 134
  LOGICAL,PARAMETER        :: verbose   =.FALSE.

  n_args = IARGC()
  IF (n_args /= 2) then
     WRITE(0,*) 'Error. Incorrect arguments number'
     WRITE(0,*) 'Usage: $ mean state_file_list skew_file'
     CALL EXIT (1)
  END IF

  CALL GETARG(1,state_file_list)
  CALL GETARG(2,skew_file)

  ALLOCATE(v2d     (nlon,nlat,nv2d))      ! 2d state variables
  ALLOCATE(v2d_sum1(nlon,nlat,nv2d))
  ALLOCATE(v2d_sum2(nlon,nlat,nv2d))
  ALLOCATE(v2d_sum3(nlon,nlat,nv2d))
  ALLOCATE(v2d_mean(nlon,nlat,nv2d))
  ALLOCATE(v2d_stdd(nlon,nlat,nv2d))
  ALLOCATE(v2d_skew(nlon,nlat,nv2d))
  ALLOCATE(v3d     (nlon,nlat,nlev,nv3d)) ! 3d state variables
  ALLOCATE(v3d_sum1(nlon,nlat,nlev,nv3d))
  ALLOCATE(v3d_sum2(nlon,nlat,nlev,nv3d))
  ALLOCATE(v3d_sum3(nlon,nlat,nlev,nv3d))
  ALLOCATE(v3d_mean(nlon,nlat,nlev,nv3d))
  ALLOCATE(v3d_stdd(nlon,nlat,nlev,nv3d))
  ALLOCATE(v3d_skew(nlon,nlat,nlev,nv3d))

  v3d_sum1 = 0.d0; v2d_sum1 = 0.d0
  v3d_sum2 = 0.d0; v2d_sum2 = 0.d0
  v3d_sum3 = 0.d0; v2d_sum3 = 0.d0

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
     v2d_sum1 = v2d_sum1 + v2d;    v3d_sum1 = v3d_sum1 + v3d;
     v2d_sum2 = v2d_sum2 + v2d**2; v3d_sum2 = v3d_sum2 + v3d**2;    
     v2d_sum3 = v2d_sum3 + v2d**3; v3d_sum3 = v3d_sum3 + v3d**3;    
  enddo
  close(in_unit_1)
  write(*,*) n_state, ' files were read'

  v3d_mean = v3d_sum1 / n_state
  v2d_mean = v2d_sum1 / n_state
!   CALL monitor_real_4D(v3d_mean,'v3d_mean')
!   CALL monitor_real_3D(v2d_mean,'v2d_mean')

  v3d_stdd = SQRT((v3d_sum2 - n_state*v3d_mean**2)/(n_state-1))
  v2d_stdd = SQRT((v2d_sum2 - n_state*v2d_mean**2)/(n_state-1))
!   CALL monitor_real_4D(v3d_stdd,'v3d_stdd')
!   CALL monitor_real_3D(v2d_stdd,'v2d_stdd')

  v3d_skew = -9.99E33;
  WHERE(v3d_stdd /= 0.0) v3d_skew = &
       & ((v3d_sum3/n_state) - 3.0* v3d_mean*(v3d_stdd**2) - v3d_mean**3)/(v3d_stdd**3)

  v2d_skew = -9.99E33;
  WHERE(v2d_stdd /= 0.0) v2d_skew = &
       & ((v2d_sum3/n_state) - 3.0* v2d_mean*(v2d_stdd**2) - v2d_mean**3)/(v2d_stdd**3)

  if (verbose) CALL monitor_real_4D(v3d_skew,'v3d_skew')
  if (verbose) CALL monitor_real_3D(v2d_skew,'v2d_skew')
  
  ! Saving output
  call write_grd(skew_file, v3d_skew, v2d_skew)

  DEALLOCATE(v3d,v3d_mean,v2d,v2d_mean)
  write(*,*) ' Ensemble skewness was stored in ', trim(skew_file)

  STOP
END PROGRAM skewness
