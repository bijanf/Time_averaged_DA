!=======================================================================
!> @brief Generates tree-ring like observations.
!> Initially only time averaged surface temperature measurements.
!> Based on makeobs.f90
!> @author Walter Acevedo (03.08.2013)
!=======================================================================
PROGRAM clean_obs
  !$USE OMP_LIB
!  USE common
!  USE common_tools
!  USE common_speedy
  USE common_obs_speedy
  
!  CALL clean_obs_standard
  CALL clean_obs_trw

  STOP
CONTAINS
 
  SUBROUTINE clean_obs_trw
!  !$USE OMP_LIB
    INTEGER                  :: i, j, k, nn, ios
    INTEGER                  :: nstation
    INTEGER                  :: nobs
    CHARACTER(100)           :: cdummy
    CHARACTER(200)           :: nature_file, station_file, obs_file
    REAL(r_sngl)             :: wk(6)         ! Observation file row
    !                                      wk(1)-> observation class id
    !                                      wk(2)-> longitude
    !                                      wk(3)-> latitude
    !                                      wk(4)-> pressure
    !                                      wk(5)-> Observation value
    !                                      wk(6)-> Observation error
    integer                  :: n_args
    INTEGER                  :: nmonths
    REAL(r_size)             :: TA_OBS(NLON,NLAT)
    CHARACTER(200)           :: monthly_means_file
    REAL(r_size),ALLOCATABLE :: v3d(:,:,:,:) ! 3D fields
    REAL(r_size),ALLOCATABLE :: v2d(:,:,:)   ! 2D fields
    CHARACTER(20)            :: obs_operator
    LOGICAL,PARAMETER        :: verbose=.TRUE.
    

  !  INTEGER,PARAMETER        :: obkind = 1   ! # of obs. kinds
  !  INTEGER                  :: obelm(obkind)! Obs. kind. id   array
  !  INTEGER                  :: nobkind
  !  LOGICAL                  :: ex
  !  REAL(r_size)             :: a
  !   CHARACTER(50)            :: tmp_string

    n_args = IARGC()
    IF (n_args /= 4) then
       WRITE(0,*) 'Error. Incorrect arguments number'
       WRITE(0,*) 'Usage: $ clean_obs.exe nature_file station_file obs_file monthly_means_file'
       CALL EXIT (1)
    END IF

    CALL GETARG(1,nature_file)
    CALL GETARG(2,station_file)
    CALL GETARG(3,obs_file)
    CALL GETARG(4,monthly_means_file)
    
    CALL get_int_from_env('nmonths',nmonths)
    CALL get_string_from_env ('obs_operator',obs_operator)

    ! Initial settings

    CALL set_common_speedy
    
    ! Read nature run

    IF(verbose) PRINT*, 'Reading nature file ', trim(nature_file)
    ALLOCATE(v3d(nlon,nlat,nlev,nv3d)) ! 3d state variables
    ALLOCATE(v2d(nlon,nlat,nv2d))      ! 2d state variables
    CALL read_grd(nature_file,v3d,v2d)
    
    ! Read nature monthly means
    
    TA_OBS = time_averaged_obs(monthly_means_file,nmonths,trim(obs_operator))
! call monitor_real4_2D(wk(4),'wk(4)')       
    !  TA_OBS = time_averaged_obs(monthly_means_file,nmonths,'T')
    !  CALL time_averaged_obs(monthly_means_file,nmonths,'temp',TA_OBS)
    !
    ! Read Locations of stations from station.tbl (unit 10)
    !
    OPEN(10,FILE=station_file)
    READ(10,'(A)') cdummy
    READ(10,'(A)') cdummy
    nstation = 0
    DO
       READ(10,'(2I3)',IOSTAT=ios) i,j
       IF(ios /= 0) EXIT
       nstation = nstation + 1
       ! print*, i,j
    END DO
    CLOSE(10)
    IF(verbose) PRINT*, 'Number of stations = ', nstation
    !
    ! Count number of obs
    !
    nobs = nstation
    IF(verbose) call monitor_intg_0D(nobs,'nobs =')
    !
    ! Write Observations in obs_file = 'obs.dat' (unit 90)
    !
    nn = 0
    OPEN(90,FILE=obs_file,FORM='unformatted',ACCESS='sequential')
    OPEN(10,FILE=station_file)
    READ(10,'(A)') cdummy
    READ(10,'(A)') cdummy
    
    DO
       READ(10,'(2I3)',IOSTAT=ios) i,j
       IF(ios /= 0) EXIT
       nn = nn+1 ! Obs counter
       k  = 1
       ! IF(verbose) call monitor_intg_0D(nn,'nn =')

       wk(1) = REAL(id_t_obs,r_sngl)     ! obs. kind id
  !      wk(1) = REAL(id_trw_linear_obs,r_sngl)     ! obs. kind id
       wk(2) = REAL(  lon(i),r_sngl)     ! longitute
       wk(3) = REAL(  lat(j),r_sngl)     ! latitude
  !      wk(4) = REAL(phi0(i,j),r_sngl)    ! surface geopotential (from unit 21)
! call monitor_real4_0D(wk(4),'wk(4)')       
       wk(4) = REAL(v2d(i,j,iv2d_ps)*sig(1),r_sngl)/100.0 ! hPa
! call monitor_real4_0D(wk(4),'wk(4)')       
! call monitor_real4_0D(wk(4),'wk(5)')
! call monitor_real_0D(TA_OBS(i,j),'TA_OBS(i,j) = ')       

       wk(5) = REAL(TA_OBS(i,j))         ! Obs.value
! call monitor_real4_0D(wk(4),'wk(5)')       
       wk(6) = 0.0d0                     ! null Obs. Error (clean_obs)

       WRITE(90) wk

    END DO
    CLOSE(90)
    !
    ! Count number of obs
    !
    IF(verbose) THEN
       WRITE(6,*) '# of potential observations (without filtering): ',nn
       WRITE(6,*) 'obsmake is reading file ',obs_file
       CALL get_nobs(obs_file,nn)
    END IF

    RETURN
  END SUBROUTINE clean_obs_trw
                
  SUBROUTINE clean_obs_standard
  !  !$USE OMP_LIB
    LOGICAL,PARAMETER        :: verbose=.TRUE.
    REAL(r_size),ALLOCATABLE ::     v3d(:,:,:,:) ! 3D fields
    REAL(r_size),ALLOCATABLE ::     v2d(:,:,:)   ! 2D fields
    INTEGER                  :: i, j, k, nn, ios
    INTEGER                  :: nstation
    INTEGER                  :: nobs
    CHARACTER(100)           :: cdummy
    CHARACTER(200)           :: nature_file, station_file, obs_file
    REAL(r_sngl)             :: wk(6)         ! Observation file row
    !                                      wk(1)-> observation class id
    !                                      wk(2)-> longitude
    !                                      wk(3)-> latitude
    !                                      wk(4)-> pressure
    !                                      wk(5)-> Observation value
    !                                      wk(6)-> Observation error
    integer                  :: n_args
  CHARACTER(200)              :: monthly_means_file
    !INTEGER,PARAMETER        :: obkind = 5   ! # of obs. kinds
    !INTEGER                  :: obelm(obkind)! Obs. kind. id   array
  !   INTEGER                  :: nobkind
  !   LOGICAL                  :: ex
  !   REAL(r_size)             :: a
  !   CHARACTER(50)            :: tmp_string

    n_args = IARGC()
    IF (n_args /= 4) then
       WRITE(0,*) 'Error. Incorrect arguments number'
       WRITE(0,*) 'Usage: $ clean_obs.exe nature_file station_file obs_file monthly_means_file'
       CALL EXIT (1)
    END IF

    CALL GETARG(1,nature_file)
    CALL GETARG(2,station_file)
    CALL GETARG(3,obs_file)
    CALL GETARG(4,monthly_means_file)

    ! Initial settings

    CALL set_common_speedy

    ! Read nature run

    IF(verbose) PRINT*, 'Reading nature file ', trim(nature_file)
    ALLOCATE(v3d(nlon,nlat,nlev,nv3d)) ! 3d state variables
    ALLOCATE(v2d(nlon,nlat,nv2d))      ! 2d state variables
    CALL read_grd(nature_file,v3d,v2d)


    ! Read state variance
    !
    ! ALLOCATE(v3d_var(nlon,nlat,nlev,nv3d)) ! 3d state variables
    ! ALLOCATE(v2d_var(nlon,nlat,nv2d))      ! 2d state variables
    ! CALL read_grd(var_file,v3d_var,v2d_var)
    !
    !  ! Read observation errors from obserr.tbl (unit 11)
    !  !
    !  OPEN(11,FILE='obserr.tbl')
    !  READ(11,'(A)') cdummy
    !  READ(11,'(A)') cdummy
    !  nobkind = 0
    !  DO
    !    READ(11,'(L1,X,I5,X,ES9.2)',IOSTAT=ios) ex,i,a
    !    IF(ios /= 0) EXIT
    !    IF(ex) THEN
    !      nobkind = nobkind+1
    !      obelm(nobkind) = i
    !      oberr(nobkind) = a
    !    END IF
    !  END DO
    !  CLOSE(11)
    !
    ! Read Locations of stations from station.tbl (unit 10)
    !
    OPEN(10,FILE=station_file)
    READ(10,'(A)') cdummy
    READ(10,'(A)') cdummy
    nstation = 0
    DO
       READ(10,'(2I3)',IOSTAT=ios) i,j
       IF(ios /= 0) EXIT
       nstation = nstation + 1
       ! print*, i,j
    END DO
    CLOSE(10)
    IF(verbose) PRINT*, 'Number of stations = ', nstation
    !
    ! Count number of obs
    !
    nobs = nstation
    !  nobs = 0
    !  DO n=1,nobkind
    !    IF(obelm(n) == id_ps_obs) THEN ! Surface pressure has only 1 level
    !      nobs = nobs + nstation
    !    ELSE                           ! while the other variables have nlev levels
    !      nobs = nobs + nstation*nlev
    !    END IF
    !  END DO
    !  IF(verbose) PRINT*, '(A,I)','nobs = ',nobs
    !
    ! Random number
    !
    ! ALLOCATE(error(nobs))
    ! CALL com_randn(nobs,error)
    !
    ! Write Observations in obs_file = 'obs.dat' (unit 90)
    !
    nn = 0
    OPEN(90,FILE=obs_file,FORM='unformatted',ACCESS='sequential')
    OPEN(10,FILE=station_file)
    READ(10,'(A)') cdummy
    READ(10,'(A)') cdummy
    DO
       READ(10,'(2I3)',IOSTAT=ios) i,j
       IF(ios /= 0) EXIT
       nn = nn+1 ! Obs counter
       k  = 1

       ! obs_error_std = SQRT(v3d_var(i,j,k,iv3d_t)/SNR)
       !        print*,i
       !        print*,j
       !        print*,k
       !        print*,iv3d_t
       !        print*,SNR
       !        print*,v3d_var(i,j,k,iv3d_t)
       !        print*,obs_error_std
       !obs_error_std = 1.00E+00

       wk(1) = REAL(id_t_obs,r_sngl)     ! obs. kind id
       wk(2) = REAL(  lon(i),r_sngl)     ! longitute
       wk(3) = REAL(  lat(j),r_sngl)     ! latitude
       !wk(4) = REAL(phi0(i,j),r_sngl)    ! surface geopotential (from unit 21)
       wk(4) = REAL(v2d(i,j,iv2d_ps)*sig(1),r_sngl)/100.0 ! hPa
  !     wk(5) = REAL(TA_OBS(i,j)) ! + error(nn) * obs_error_std,r_sngl)
       wk(5) = REAL(v3d(i,j,k,iv3d_t)) ! + error(nn) * obs_error_std,r_sngl)
       ! Obs.value
       wk(6) = 0.0d0 !REAL(obs_error_std,r_sngl)! Obs. Error

       WRITE(90) wk

       !    DO n=1,nobkind
       !      IF(obelm(n) == id_ps_obs) THEN  ! Surface pressure observations
       !        wk(1) = REAL( obelm(n),r_sngl)
       !        wk(2) = REAL(   lon(i),r_sngl)
       !        wk(3) = REAL(   lat(j),r_sngl)
       !        wk(4) = REAL(phi0(i,j),r_sngl)! surface geopotential (from unit 21)
       !        nn = nn+1
       !        !! Pressure observation divided by 100 !!
       !        wk(5) = REAL(v2d(i,j,iv2d_ps) + error(nn)*oberr(n),r_sngl)/100.0 ! hPa
       !        wk(6) = REAL( oberr(n),r_sngl)/100.0 ! hPa
       !        WRITE(90) wk
       !      ELSE                            ! 3D variables observations
       !        wk(1) = REAL(obelm(n),r_sngl)
       !        wk(2) = REAL(  lon(i),r_sngl)
       !        wk(3) = REAL(  lat(j),r_sngl)
       !        wk(6) = REAL(oberr(n),r_sngl)
       !        DO k=1,nlev
       !          wk(4) = REAL(v2d(i,j,iv2d_ps)*sig(k),r_sngl)/100.0 ! hPa
       !          nn = nn+1
       !          SELECT CASE(obelm(n))
       !          CASE(id_u_obs)
       !            wk(5) = REAL(v3d(i,j,k,iv3d_u) + error(nn)*oberr(n),r_sngl)
       !          CASE(id_v_obs)
       !            wk(5) = REAL(v3d(i,j,k,iv3d_v) + error(nn)*oberr(n),r_sngl)
       !          CASE(id_t_obs)
       !            wk(5) = REAL(v3d(i,j,k,iv3d_t) + error(nn)*oberr(n),r_sngl)
       !          CASE(id_q_obs)
       !            wk(5) = REAL(v3d(i,j,k,iv3d_q) + error(nn)*oberr(n),r_sngl)
       !          END SELECT
       !          IF(obelm(n) == id_q_obs .AND. k >= 5) CYCLE !No upper-level Q obs
       !          WRITE(90) wk
       !        END DO
       !      END IF
       !    END DO
    END DO
    CLOSE(90)
    !
    ! Count number of obs
    !
    IF(verbose) THEN
       WRITE(6,*) '# of potential observations (without filtering): ',nn
       WRITE(6,*) 'obsmake is reading file ',obs_file
       CALL get_nobs(obs_file,nn)
    END IF

    DEALLOCATE(v3d,v2d)
    ! DEALLOCATE(v3d,v3d_var,v2d,v2d_var,error)

    RETURN
  END SUBROUTINE clean_obs_standard



END PROGRAM clean_obs


