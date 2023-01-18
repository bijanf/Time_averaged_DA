!=======================================================================
!> @brief Generates tree-ring like observations.
!> Initially only time averaged surface temperature measurements.
!> Based on makeobs.f90
!> @author Walter Acevedo (03.08.2013)
!=======================================================================
PROGRAM generate_obs
  !$USE OMP_LIB
  USE common
  USE common_speedy
  USE common_obs_speedy
  USE common_tools

  IMPLICIT NONE
!   LOGICAL,PARAMETER        :: verbose=.TRUE.
  !INTEGER,PARAMETER        :: obkind = 5   ! # of obs. kinds
  ! REAL(r_size),ALLOCATABLE ::     v3d(:,:,:,:) ! 3D fields
  ! REAL(r_size),ALLOCATABLE :: v3d_var(:,:,:,:) ! 3D fields
  ! REAL(r_size),ALLOCATABLE ::     v2d(:,:,:)   ! 2D fields
  ! REAL(r_size),ALLOCATABLE :: v2d_var(:,:,:)   ! 2D fields
  !REAL(r_size)             :: oberr(obkind)! Obs. kind error array
  !INTEGER                  :: obelm(obkind)! Obs. kind. id   array
!   LOGICAL                  :: ex
!   REAL(r_size)             :: a
  INTEGER                  :: ios
!   INTEGER                  :: i, j, k, n,nn
!   INTEGER                  :: nobkind !, nstation
  INTEGER                  :: nobs
  REAL(r_size)             :: SNR           ! Signal to noise ratio
!   CHARACTER(100)           :: cdummy
!   CHARACTER(200)           :: nature_file !, station_file !, obs_file
  CHARACTER(200)           :: clean_obs_file
  CHARACTER(200)           :: dirty_obs_file
  integer                  :: in_unit_1 = 134
  integer                  :: n_state !, n_states

!   REAL(r_sngl)             :: wk(6)         ! Observation file row
  !                                      wk(1)-> observation class id
  !                                      wk(2)-> longitude
  !                                      wk(3)-> latitude
  !                                      wk(4)-> pressure
  !                                      wk(5)-> Observation value
  !                                      wk(6)-> Observation error
!   CHARACTER(50)            :: tmp_string
!   integer                  :: n_args

  REAL(r_size),ALLOCATABLE :: elem(:) ! element number
  REAL(r_size),ALLOCATABLE :: rlon(:)
  REAL(r_size),ALLOCATABLE :: rlat(:)
  REAL(r_size),ALLOCATABLE :: rlev(:)
  REAL(r_size),ALLOCATABLE :: clean_obs(:)
  REAL(r_size),ALLOCATABLE :: dirty_obs(:)
  REAL(r_size),ALLOCATABLE :: oerr(:)
  REAL(r_size),ALLOCATABLE :: obs1_sum(:)
  REAL(r_size),ALLOCATABLE :: obs2_sum(:)
  REAL(r_size),ALLOCATABLE :: obs_mean(:)
  REAL(r_size),ALLOCATABLE :: obs_var(:)
  REAL(r_size),ALLOCATABLE :: obs_error_std(:)
  REAL(r_size),ALLOCATABLE :: randn_series(:)
  REAL(r_size),ALLOCATABLE :: obs_error(:)

  CALL get_real_par_env ('SNR',SNR)


  PRINT*,' - find number of nature states'

  open(in_unit_1, FILE='clean_obs_list.dat', FORM='FORMATTED')
  n_state = 0
  do
     read(in_unit_1,'(A)',IOSTAT=ios) clean_obs_file
     IF(ios /= 0) EXIT
     n_state = n_state + 1
  enddo
  close(in_unit_1)

  ! find number of observation per state

  CALL get_nobs(clean_obs_file,nobs)



  ALLOCATE(elem         (nobs)) ! element number
  ALLOCATE(rlon         (nobs))
  ALLOCATE(rlat         (nobs))
  ALLOCATE(rlev         (nobs))
  ALLOCATE(clean_obs    (nobs))
  ALLOCATE(dirty_obs    (nobs))
  ALLOCATE(oerr         (nobs))
  ALLOCATE(obs1_sum     (nobs))
  ALLOCATE(obs2_sum     (nobs))
  ALLOCATE(obs_mean     (nobs))
  ALLOCATE(obs_var      (nobs))
  ALLOCATE(obs_error_std(nobs))
  ALLOCATE(randn_series (nobs))
  ALLOCATE(obs_error    (nobs))


  PRINT*,' - Calculate observation variance'

  obs1_sum = 0.0d0
  obs2_sum = 0.0d0

  open(in_unit_1, FILE='clean_obs_list.dat', FORM='FORMATTED')
  do
     read(in_unit_1,'(A)',IOSTAT=ios) clean_obs_file
     IF(ios /= 0) EXIT

     CALL read_obs(clean_obs_file,nobs,elem,rlon,rlat,rlev,clean_obs,oerr)

     obs1_sum = obs1_sum +  clean_obs
     obs2_sum = obs2_sum + (clean_obs * clean_obs)

  enddo
  close(in_unit_1)



  CALL com_randn(nobs,randn_series)

  obs_mean  =  obs1_sum / n_state
  obs_var   = (obs2_sum - n_state*(obs_mean * obs_mean)) / (n_state - 1)
  obs_error_std = SQRT(obs_var/SNR)
  obs_error = randn_series  * obs_error_std


  PRINT*,' - Create polluted observations'

  open(in_unit_1, FILE='clean_obs_list.dat', FORM='FORMATTED')
  do
     read(in_unit_1,'(A)',IOSTAT=ios) clean_obs_file
     IF(ios /= 0) EXIT

      PRINT*,' - '//clean_obs_file

     CALL read_obs(clean_obs_file,nobs,elem,rlon,rlat,rlev,clean_obs,oerr)
     dirty_obs = clean_obs + obs_error
     
     dirty_obs_file = TRIM(clean_obs_file)//'.dirty'
     CALL write_obs(dirty_obs_file,nobs,elem,rlon,rlat,rlev,dirty_obs,obs_error_std)
!     CALL write_obs(dirty_obs_file,nobs,elem,rlon,rlat,rlev,dirty_obs,obs_error)

  enddo
  close(in_unit_1)


  DEALLOCATE(elem) ! element number
  DEALLOCATE(rlon)
  DEALLOCATE(rlat)
  DEALLOCATE(rlev)
  DEALLOCATE(clean_obs)
  DEALLOCATE(dirty_obs)
  DEALLOCATE(oerr)
  DEALLOCATE(obs1_sum)
  DEALLOCATE(obs2_sum)
  DEALLOCATE(obs_mean)
  DEALLOCATE(obs_var)
  DEALLOCATE(obs_error_std)


  STOP
END PROGRAM generate_obs


SUBROUTINE write_obs(cfile,nn,elem,rlon,rlat,rlev,odat,oerr)
  ! USE common
  USE common_obs_speedy
  IMPLICIT NONE
  
  CHARACTER(*),INTENT(IN) :: cfile
  INTEGER     ,INTENT(IN) :: nn
  REAL(r_size),INTENT(IN) :: elem(nn) ! element number
  REAL(r_size),INTENT(IN) :: rlon(nn)
  REAL(r_size),INTENT(IN) :: rlat(nn)
  REAL(r_size),INTENT(IN) :: rlev(nn)
  REAL(r_size),INTENT(IN) :: odat(nn)
  REAL(r_size),INTENT(IN) :: oerr(nn)
  REAL(r_sngl)            :: wk(6)
  INTEGER                 :: n,iunit = 91

  OPEN(iunit,FILE=cfile,FORM='unformatted',ACCESS='sequential')
  DO n=1,nn
     wk(1) = REAL(elem(n),r_sngl)
     wk(2) = REAL(rlon(n),r_sngl)
     wk(3) = REAL(rlat(n),r_sngl)
     wk(4) = REAL(rlev(n),r_sngl)
     wk(5) = REAL(odat(n),r_sngl)
     wk(6) = REAL(oerr(n),r_sngl)
     SELECT CASE(NINT(wk(1)))
     CASE(id_u_obs)
        wk(4) = wk(4) / 100.0 ! hPa -> Pa
     CASE(id_v_obs)
        wk(4) = wk(4) / 100.0 ! hPa -> Pa
     CASE(id_t_obs)
        wk(4) = wk(4) / 100.0 ! hPa -> Pa
     CASE(id_q_obs)
        wk(4) = wk(4) / 100.0 ! hPa -> Pa
     CASE(id_ps_obs)
        wk(5) = wk(5) / 100.0 ! hPa -> Pa
        wk(6) = wk(6) / 100.0 ! hPa -> Pa
     CASE(id_rh_obs)
        wk(4) = wk(4) / 100.0 ! hPa -> Pa
        wk(5) = wk(5) / 0.01 ! percent input
        wk(6) = wk(6) / 0.01 ! percent input
     END SELECT
     WRITE(iunit) wk
  END DO
  CLOSE(iunit)

  RETURN
END SUBROUTINE write_obs

