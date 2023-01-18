MODULE letkf_obs
!=======================================================================
!
! [PURPOSE:] Observational procedures
!
! [HISTORY:]
!   01/23/2009 Takemasa MIYOSHI  created
!   29/07/2014 Walter Acevedo    modified
!
!=======================================================================
!$USE OMP_LIB
USE common
USE common_mpi
USE common_speedy
USE common_obs_speedy
USE common_mpi_speedy
USE common_letkf

IMPLICIT NONE
PUBLIC

INTEGER     ,PARAMETER        :: nslots = 1           ! number of time slots for 4D-LETKF
INTEGER     ,PARAMETER        :: nbslot = 1           ! basetime slot
                                                      ! description             | unit
! REAL(r_size),PARAMETER        :: sigma_obs = 12000.0d3! horizontal localization | meters  
REAL(r_size),PARAMETER        :: sigma_obs  = 500.0d3 ! horizontal localization | meters  
REAL(r_size),PARAMETER        :: sigma_obsv =   0.4d0 ! vertical localization   | ln(p)
!REAL(r_size),PARAMETER        :: sigma_obsv = 0.1d0  ! vertical localization   | ln(p)
REAL(r_size),PARAMETER        :: sigma_obst =   3.0d0 ! temporal localization   | time slots 
!REAL(r_size),PARAMETER        :: threshold_dz = 1000.0d0
REAL(r_size),PARAMETER        ::  gross_error = 10.0d0

INTEGER                 ,SAVE :: nobs
REAL(r_size)            ,SAVE :: dist_zero
REAL(r_size)            ,SAVE :: dist_zerov
REAL(r_size),ALLOCATABLE,SAVE :: dlon_zero(:)
REAL(r_size)            ,SAVE :: dlat_zero
REAL(r_size),ALLOCATABLE,SAVE :: obselm (:)
REAL(r_size),ALLOCATABLE,SAVE :: obslon (:)
REAL(r_size),ALLOCATABLE,SAVE :: obslat (:)
REAL(r_size),ALLOCATABLE,SAVE :: obslev (:)
REAL(r_size),ALLOCATABLE,SAVE :: obsdat (:)
REAL(r_size),ALLOCATABLE,SAVE :: obserr (:)
!   REAL(r_size),ALLOCATABLE,SAVE :: obsk(:)
REAL(r_size),ALLOCATABLE,SAVE :: obsdep (:)
REAL(r_size),ALLOCATABLE,SAVE :: obshdxf(:,:)
INTEGER                 ,SAVE :: nobsgrd(nlon,nlat)

CONTAINS
!-----------------------------------------------------------------------
! Initialize
!-----------------------------------------------------------------------
SUBROUTINE set_letkf_obs
    IMPLICIT NONE
!     REAL(r_size)             ::    v3d(nlon,nlat,nlev,nv3d)
!     REAL(r_size)             ::    v2d(nlon,nlat,nv2d)
!     REAL(r_size)             :: p_full(nlon,nlat,nlev)
!     REAL(r_size)             :: qg,dz,tg
!     REAL(r_size)             :: ri,rj
!     REAL(r_size)             :: dlon1,dlon2,dlon,dlat
    REAL(r_size),ALLOCATABLE ::  wk2d   (:,:)
    INTEGER     ,ALLOCATABLE :: iwk2d   (:,:)
    REAL(r_size),ALLOCATABLE :: tmpelm  (:)
    REAL(r_size),ALLOCATABLE :: tmplon  (:)
    REAL(r_size),ALLOCATABLE :: tmplat  (:)
    REAL(r_size),ALLOCATABLE :: tmplev  (:)
    REAL(r_size),ALLOCATABLE :: tmpdat  (:)
    REAL(r_size),ALLOCATABLE :: tmperr  (:)
    REAL(r_size),ALLOCATABLE :: tmpk    (:)
    REAL(r_size),ALLOCATABLE :: tmpdep  (:)
    REAL(r_size),ALLOCATABLE :: tmphdxf (:,:)
    INTEGER     ,ALLOCATABLE :: tmpqc0  (:,:)
    INTEGER     ,ALLOCATABLE :: tmpqc   (:)
    REAL(r_size),ALLOCATABLE :: tmp2elm (:)
    REAL(r_size),ALLOCATABLE :: tmp2lon (:)
    REAL(r_size),ALLOCATABLE :: tmp2lat (:)
    REAL(r_size),ALLOCATABLE :: tmp2lev (:)
    REAL(r_size),ALLOCATABLE :: tmp2dat (:)
    REAL(r_size),ALLOCATABLE :: tmp2err (:)

    REAL(r_size),ALLOCATABLE :: tmp1elm (:)
    REAL(r_size),ALLOCATABLE :: tmp1lon (:)
    REAL(r_size),ALLOCATABLE :: tmp1lat (:)
    REAL(r_size),ALLOCATABLE :: tmp1lev (:)
    REAL(r_size),ALLOCATABLE :: tmp1dat (:)
    REAL(r_size),ALLOCATABLE :: tmp1err (:)

    !REAL(r_size),ALLOCATABLE :: tmp2k(:)
    REAL(r_size),ALLOCATABLE :: tmp2dep (:)
    REAL(r_size),ALLOCATABLE :: tmp2hdxf(:,:)
    INTEGER                  :: nobslots(nslots)
    INTEGER                  :: n,i,j,ierr,islot,nn,l,im
    INTEGER                  :: nj (0:nlat-1)
    INTEGER                  :: njs(1:nlat-1)
    CHARACTER(9)             ::  obsfile =   'obsTT.dat'
!     CHARACTER(11)            :: guesfile = 'gsTTNNN.grd'
    CHARACTER(23)            :: guess_obs_file = 'ta_obs_TTNNN.grd'
!     real*8,dimension(NLON,NLAT) :: TA_OBS!,INST_OBS
!     INTEGER                  :: nmonths = 1!, nn
    REAL(r_size),ALLOCATABLE :: err_dummy(:)
    REAL(r_size),ALLOCATABLE :: elm_dummy(:)
    REAL(r_size),ALLOCATABLE :: hdxf_dummy(:)
    LOGICAL                  :: verb = .true.

    !-------------------------------------------------------
    if(verb) call print_msg('set_letkf_obs START')
    !-------------------------------------------------------
    !     WRITE(6,'(A)') 'Hello from set_common_obs_speedy'
    
    dist_zero  = sigma_obs  * SQRT(10.0d0/3.0d0) * 2.0d0
    dist_zerov = sigma_obsv * SQRT(10.0d0/3.0d0) * 2.0d0
    dlat_zero  = dist_zero / pi / re * 180.0d0
    ALLOCATE(dlon_zero(nij1))
    DO i=1,nij1
        dlon_zero(i) = dlat_zero / COS(pi*lat1(i)/180.0d0)
    END DO
    
    DO islot=1,nslots
        WRITE(obsfile(4:5),'(I2.2)') islot
        WRITE(6,'(A,I3.3,2A)') 'MYRANK ',myrank,' is reading a file ',obsfile
        CALL get_nobs(obsfile,nobslots(islot))
    END DO
    nobs = SUM(nobslots)
    WRITE(6,'(I10,A)') nobs,' INPUT OBSERVATIONS IN TOTAL'
    
    IF(nobs == 0) THEN
        WRITE(6,'(A)') 'No observation assimilated'
        RETURN
    END IF

    !-------------------------------------------------------
    if(verb) call print_msg('INITIALIZE GLOBAL VARIABLES')
    !-------------------------------------------------------
    ALLOCATE( tmpelm (nobs)    )
    ALLOCATE( tmplon (nobs)    )
    ALLOCATE( tmplat (nobs)    )
    ALLOCATE( tmplev (nobs)    )
    ALLOCATE( tmpdat (nobs)    )
    ALLOCATE( tmperr (nobs)    )
    ALLOCATE( tmpk   (nobs)    )
    ALLOCATE( tmpdep (nobs)    )
    ALLOCATE( tmphdxf(nobs,nbv))
    ALLOCATE( tmpqc0 (nobs,nbv))
    ALLOCATE( tmpqc  (nobs)    )
    tmpqc0  = 1
    tmphdxf = 0.0d0

    ALLOCATE( err_dummy (nobs))
    ALLOCATE( elm_dummy (nobs))
    ALLOCATE( hdxf_dummy(nobs))

    ALLOCATE( tmp1elm (nobs)    )
    ALLOCATE( tmp1lon (nobs)    )
    ALLOCATE( tmp1lat (nobs)    )
    ALLOCATE( tmp1lev (nobs)    )
    ALLOCATE( tmp1dat (nobs)    )
    ALLOCATE( tmp1err (nobs)    )
    
    !-------------------------------------------------------
    if(verb) call print_msg('Reading ensemble observations')
    !-------------------------------------------------------
    islot = 1
    l = 0
    DO 
        im = myrank+1 + nprocs * l
        IF(im > nbv) EXIT

        WRITE(guess_obs_file(8:12),'(I2.2,I3.3)') islot,im
        WRITE(6,'(A,I3.3,2A)') 'MYRANK ',myrank,' is reading file ',guess_obs_file

        CALL read_obs(guess_obs_file,nobs,&
        & tmp1elm,tmp1lon,tmp1lat, tmp1lev,tmphdxf(:,im),tmperr)
        
       
!         tmpqc0 (:,im) = 1

        l = l+1
    END DO
    
    if(verb) call monitor_real_2D(tmphdxf,'tmphdxf')

    !-------------------------------------------------------
    if(verb) call print_msg('QUALITY CONTROL 0')
    ! verify station position validity
    !-------------------------------------------------------
    nn=0
    timeslots: DO islot=1,nslots
        IF(nobslots(islot) == 0) CYCLE
        WRITE(         obsfile(4:5),'(I2.2)') islot
        WRITE(6,'(A,I3.3,2A)') 'MYRANK ',myrank,' is reading file ',obsfile
        
!        tmp_span = nn+1:nn+nobslots(islot)
!        CALL read_obs(obsfile,nobslots(islot),tmpelm(tmp_span),tmplon(tmp_span),&
!        &   tmplat(tmp_span),tmplev(tmp_span),tmpdat(tmp_span),tmperr(tmp_span) )
        
        CALL read_obs(obsfile,nobslots(islot),&
        & tmpelm(nn+1:nn+nobslots(islot)),&
        & tmplon(nn+1:nn+nobslots(islot)),&
        & tmplat(nn+1:nn+nobslots(islot)),&
        & tmplev(nn+1:nn+nobslots(islot)),&
        & tmpdat(nn+1:nn+nobslots(islot)),&
        & tmperr(nn+1:nn+nobslots(islot)) )

        if(verb) call monitor_real_1D(tmperr, 'tmperr')
!         print*,'tmperr',tmperr
!     PRINT*,'tmplev(nn+1:nn+nobslots(islot))',tmplev(nn+1:nn+nobslots(islot))        
!     READ*
        l=0
        DO
            im = myrank+1 + nprocs * l
            IF(im > nbv) EXIT

! 	    WRITE(        guesfile(3:7),'(I2.2,I3.3)') islot,im
!             WRITE(6,'(A,I3.3,2A)') 'MYRANK ',myrank,' is reading a file',guesfile
!             CALL read_grd(guesfile,v3d,v2d)
!             CALL calc_pfull(nlon,nlat,v2d(:,:,iv2d_ps),p_full)
      

!  	    WRITE(guess_obs_file(8:12),'(I2.2,I3.3)') islot,im
! ! !         get_nobs(guess_obs_file,nn)
!  	    WRITE(6,'(A,I3.3,2A)') 'MYRANK ',myrank,' is reading file ',guess_obs_file
!  	    CALL read_obs(guess_obs_file,nobs,&
!  	    & elm_dummy,tmplon,tmplat, tmplev,hdxf_dummy,err_dummy)


! 	CALL read_obs(guess_obs_file,nobs,&
!         & tmpelm,tmplon,tmplat, tmplev,tmphdxf(:,im),tmperr)


!$OMP PARALLEL DO SCHEDULE(DYNAMIC) PRIVATE(n,ri,rj,dz,tg,qg)
!      print *,'nobslots(islot)',nobslots(islot)
            DO n=1,nobslots(islot)
!                 CALL phys2ijk(p_full,tmpelm(nn+n),&
!                 & tmplon(nn+n),tmplat(nn+n),tmplev(nn+n),ri,rj,tmpk(nn+n))
!                 
!                 IF(CEILING(ri) < 2 .OR. nlon+1 < CEILING(ri)) THEN
!                 !IF(NINT(ri) < 2 .OR. nlon+1 < NINT(ri)) THEN
!                     !$OMP CRITICAL
!                     WRITE(6,'(A)') '* X-coordinate out of range'
!                     WRITE(6,'(A,F6.2,A,F6.2)') '*   ri=',ri,', rlon=',tmplon(nn+n)
!                     !$OMP END CRITICAL
!                     CYCLE
!                 END IF
!                 
!                 IF(CEILING(rj) < 2 .OR. nlat < CEILING(rj)) THEN
!                 !IF(NINT(rj) < 2 .OR. nlat < NINT(rj)) THEN
!                     !$OMP CRITICAL
!                     WRITE(6,'(A)') '* Y-coordinate out of range'
!                     WRITE(6,'(A,F6.2,A,F6.2)') '*   rj=',rj,', rlat=',tmplat(nn+n)
!                     !$OMP END CRITICAL
!                     CYCLE
!                 END IF
!                 
!                 IF(CEILING(tmpk(n+nn)) > nlev) THEN
!                 !IF(NINT(tmpk(n+nn)) > nlev) THEN
!                     CALL itpl_2d(phi0,ri,rj,dz)
!                     !$OMP CRITICAL
!                     WRITE(6,'(A)') '* Z-coordinate out of range'
!                     WRITE(6,'(A,F6.2,A,F10.2,A,F6.2,A,F6.2,A,F10.2)') &
!                     & '*   rk=',tmpk(nn+n),', rlev=',tmplev(nn+n),&
!                     & ', (lon,lat)=(',tmplon(nn+n),',',tmplat(nn+n),'), phi0=',dz
!                     !$OMP END CRITICAL
!                     CYCLE
!                 END IF
                
!                WRITE(6,*) 'NINT(tmpk(nn+n))=', NINT(tmpk(nn+n)), ' tmpelm(nn+n)=',tmpelm(nn+n),'nn',nn,'n',n
                
!                 !IF(CEILING(tmpk(nn+n)) < 2 .AND. NINT(tmpelm(nn+n)) /= id_ps_obs) THEN
!                 ! my version 
!                 IF((NINT(tmpk(nn+n)) < 2) .AND. (NINT(tmpelm(nn+n)) /= id_ps_obs)) THEN
!                     
!                     !IF(NINT(tmpelm(nn+n)) == id_u_obs .OR. NINT(tmpelm(nn+n)) == id_v_obs) THEN
!                     ! my version
!                     IF(NINT(tmpelm(nn+n)) == id_u_obs .OR. NINT(tmpelm(nn+n)) == id_v_obs .OR. NINT(tmpelm(nn+n)) == id_t_obs) THEN
!                         tmpk(nn+n) = 1.00001d0
!                     ELSE
!                         CALL itpl_2d(phi0,ri,rj,dz)
!                         !$OMP CRITICAL
!                         WRITE(6,'(A)') '* Z-coordinate out of range'
!                         WRITE(6,'(A,F6.2,A,F10.2,A,F6.2,A,F6.2,A,F10.2)') &
!                         & '*   rk=',tmpk(nn+n),', rlev=',tmplev(nn+n),&
!                         & ', (lon,lat)=(',tmplon(nn+n),',',tmplat(nn+n),'), phi0=',dz
!                         !$OMP END CRITICAL
!                         CYCLE
!                     END IF
!                 END IF
!                 
!                 IF(NINT(tmpelm(nn+n)) == id_ps_obs .AND. tmpdat(nn+n) < -100.0d0) THEN
!                     CYCLE
!                 END IF
                !       IF(NINT(tmpelm(nn+n)) == id_ps_obs) THEN
                !         CALL itpl_2d(phi0,ri,rj,dz)
                !         dz = dz - tmplev(nn+n)
                !         IF(ABS(dz) < threshold_dz) THEN ! pressure adjustment threshold
                !           CALL itpl_2d(t(:,:,1),ri,rj,tg)
                !           CALL itpl_2d(q(:,:,1),ri,rj,qg)
                !           CALL prsadj(tmpdat(nn+n),dz,tg,qg)
                !         ELSE
                !!OMP CRITICAL
                !           PRINT '(A)','PS obs vertical adjustment beyond threshold'
                !           PRINT '(A,F10.2,A,F6.2,A,F6.2,A)',&
                !             & '  dz=',dz,', (lon,lat)=(',tmplon(nn+n),',',tmplat(nn+n),')'
                !!OMP END CRITICAL
                !           CYCLE
                !         END IF
                !       END IF
                !
                ! observational operator

!                CALL Trans_XtoY(tmpelm(nn+n),ri,rj,tmpk(nn+n),v3d,v2d,&
!                &                               p_full,tmphdxf(nn+n,im))
!  		tmphdxf(nn+n,im) = hdxf_dummy(nn+n)
                 tmpqc0 (nn+n,im) = 1
            END DO
            !$OMP END PARALLEL DO
            l = l+1
        END DO
        nn = nn + nobslots(islot)
    END DO timeslots
    
   
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    ALLOCATE(wk2d(nobs,nbv))
    wk2d = tmphdxf
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    CALL MPI_ALLREDUCE(wk2d,tmphdxf,nobs*nbv,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
    DEALLOCATE(wk2d)
    ALLOCATE (iwk2d(nobs,nbv))
    iwk2d = tmpqc0
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    CALL MPI_ALLREDUCE(iwk2d,tmpqc0,nobs*nbv,MPI_INTEGER,MPI_MAX,MPI_COMM_WORLD,ierr)
    DEALLOCATE(iwk2d)
    
    !-------------------------------------------------------
    if(verb) call print_msg('QUALITY CONTROL 1')
    ! Filtering observations with excessive gross error
    !-------------------------------------------------------    
    !$OMP PARALLEL DO SCHEDULE(DYNAMIC) PRIVATE(n,i)
    DO n=1,nobs
        tmpqc(n) = MINVAL(tmpqc0(n,:))
        IF(tmpqc(n) /= 1) CYCLE
        
        ! ensemble obs mean -> tmpdep
        
        tmpdep(n) = tmphdxf(n,1)
! 	print*,'tmpdep(n)',tmpdep(n)
        DO i=2,nbv
            tmpdep(n) = tmpdep(n) + tmphdxf(n,i)
! 	    print*,'tmpdep(n)',tmpdep(n)
        END DO
        tmpdep(n) = tmpdep(n) / REAL(nbv,r_size)

	! ensemble obs anomaly -> tmphdxf
        
        DO i=1,nbv
            tmphdxf(n,i) = tmphdxf(n,i) - tmpdep(n) ! Hdx
        END DO
        
        ! obs departure -> tmpdep
        
        tmpdep(n) = tmpdat(n) - tmpdep(n) ! y-Hx

        ! If the obs. departure is larger than gross_error times
        ! the obs. error, then the obs. is blacklisted
        
        IF( ABS(tmpdep(n)) > gross_error * tmperr(n) ) THEN !gross error
!             write(6,*) 'Observation ',n,' shows gross error (',tmpdep(n),'). Thus it gets blacklisted.'
!             tmpqc(n) = 1
             tmpqc(n) = 0
        END IF
    END DO
    !$OMP END PARALLEL DO

!     print*,'tmpdep',tmpdep
!     print*,'tmpdep',MAXVAL(tmpdep)
!     print*,'SUM(tmpqc)',SUM(tmpqc)
!     print*,'SUM(tmpqc0)',SUM(tmpqc0)
    DEALLOCATE(tmpqc0)
   
    WRITE(6,'(I10,A)') SUM(tmpqc),' OBSERVATIONS TO BE ASSIMILATED'
    
!     CALL monit_dep(nobs,tmpelm,tmpdep,tmpqc)
    !
    ! temporal observation localization
    !
    !  nn = 0
    !  DO islot=1,nslots
    !    tmperr(nn+1:nn+nobslots(islot)) = tmperr(nn+1:nn+nobslots(islot)) &
    !      & * exp(0.25d0 * (REAL(islot-nbslot,r_size) / sigma_obst)**2)
    !    nn = nn + nobslots(islot)
    !  END DO
    !
    ! SELECT OBS IN THE NODE
    !
    nn = 0
    DO n=1,nobs
        IF(tmpqc(n) /= 1) CYCLE
       
       !    IF(tmplat(n) < MINVAL(lat1) .OR. MAXVAL(lat1) < tmplat(n)) THEN
        !      dlat = MIN( ABS(MINVAL(lat1)-tmplat(n)),ABS(MAXVAL(lat1)-tmplat(n)) )
        !      IF(dlat > dlat_zero) CYCLE
        !    END IF
        !    IF(tmplon(n) < MINVAL(lon1) .OR. MAXVAL(lon1) < tmplon(n)) THEN
        !      dlon1 = ABS(MINVAL(lon1) - tmplon(n))
        !      dlon1 = MIN(dlon1,360.0d0-dlon1)
        !      dlon2 = ABS(MAXVAL(lon1) - tmplon(n))
        !      dlon2 = MIN(dlon2,360.0d0-dlon2)
        !      dlon =  MIN(dlon1,dlon2) &
        !         & * pi*re*COS(tmplat(n)*pi/180.d0)/180.0d0
        !      IF(dlon > dist_zero) CYCLE
        !    END IF
        nn = nn+1
        tmpelm (nn)   = tmpelm (n)
        tmplon (nn)   = tmplon (n)
        tmplat (nn)   = tmplat (n)
        tmplev (nn)   = tmplev (n)
        tmpdat (nn)   = tmpdat (n)
        tmperr (nn)   = tmperr (n)
        tmpk   (nn)   = tmpk   (n)
        tmpdep (nn)   = tmpdep (n)
        tmphdxf(nn,:) = tmphdxf(n,:)
        tmpqc  (nn)   = tmpqc  (n)
    END DO
    nobs = nn
    WRITE(6,'(I10,A,I3.3)') nobs,' OBSERVATIONS TO BE ASSIMILATED IN MYRANK ',myrank

    !-------------------------------------------------------
    if(verb) call print_msg('SORTING')
    !-------------------------------------------------------    
    ALLOCATE( tmp2elm (nobs)    )
    ALLOCATE( tmp2lon (nobs)    )
    ALLOCATE( tmp2lat (nobs)    )
    ALLOCATE( tmp2lev (nobs)    )
    ALLOCATE( tmp2dat (nobs)    )
    ALLOCATE( tmp2err (nobs)    )
    !ALLOCATE( tmp2k   (nobs)    )
    ALLOCATE( tmp2dep (nobs)    )
    ALLOCATE( tmp2hdxf(nobs,nbv))
    ALLOCATE( obselm  (nobs)    )
    ALLOCATE( obslon  (nobs)    )
    ALLOCATE( obslat  (nobs)    )
    ALLOCATE( obslev  (nobs)    )
    ALLOCATE( obsdat  (nobs)    )
    ALLOCATE( obserr  (nobs)    )
    !ALLOCATE( obsk    (nobs)    )
    ALLOCATE( obsdep  (nobs)    )
    ALLOCATE( obshdxf (nobs,nbv))
    nobsgrd = 0
    nj = 0
    !$OMP PARALLEL PRIVATE(i,j,n,nn)
    !$OMP DO SCHEDULE(DYNAMIC)
    DO j=1,nlat-1
        DO n=1,nobs
            IF(tmplat(n) < lat(j) .OR. lat(j+1) <= tmplat(n)) CYCLE
            nj(j) = nj(j) + 1
        END DO
    END DO
    !$OMP END DO
    !$OMP DO SCHEDULE(DYNAMIC)
    DO j=1,nlat-1
        njs(j) = SUM(nj(0:j-1))
    END DO
    !$OMP END DO
    !$OMP DO SCHEDULE(DYNAMIC)
    DO j=1,nlat-1
        nn = 0
        DO n=1,nobs
            IF(tmplat(n) < lat(j) .OR. lat(j+1) <= tmplat(n)) CYCLE
            nn = nn + 1
            tmp2elm (njs(j)+nn)   = tmpelm (n)
            tmp2lon (njs(j)+nn)   = tmplon (n)
            tmp2lat (njs(j)+nn)   = tmplat (n)
            tmp2lev (njs(j)+nn)   = tmplev (n)
            tmp2dat (njs(j)+nn)   = tmpdat (n)
            tmp2err (njs(j)+nn)   = tmperr (n)
            !tmp2k   (njs(j)+nn)   = tmpk   (n)
            tmp2dep (njs(j)+nn)   = tmpdep (n)
            tmp2hdxf(njs(j)+nn,:) = tmphdxf(n,:)
        END DO
    END DO
    !$OMP END DO
    !$OMP DO SCHEDULE(DYNAMIC)
    DO j=1,nlat-1
        IF(nj(j) == 0) THEN
            nobsgrd(:,j) = njs(j)
            CYCLE
        END IF
        nn = 0
        DO i=1,nlon
            DO n=njs(j)+1,njs(j)+nj(j)
                IF(i < nlon) THEN
                    IF(tmp2lon(n) < lon(i) .OR. lon(i+1) <= tmp2lon(n)) CYCLE
                ELSE
                    IF(tmp2lon(n) < lon(nlon) .OR. 360.0d0 <= tmp2lon(n)) CYCLE
                END IF
                nn = nn + 1
                obselm (njs(j)+nn)   = tmp2elm (n)
                obslon (njs(j)+nn)   = tmp2lon (n)
                obslat (njs(j)+nn)   = tmp2lat (n)
                obslev (njs(j)+nn)   = tmp2lev (n)
                obsdat (njs(j)+nn)   = tmp2dat (n)
                obserr (njs(j)+nn)   = tmp2err (n)
                !obsk   (njs(j)+nn)   = tmp2k   (n)
                obsdep (njs(j)+nn)   = tmp2dep (n)
                obshdxf(njs(j)+nn,:) = tmp2hdxf(n,:)
            END DO
            nobsgrd(i,j) = njs(j) + nn
        END DO
        IF(nn /= nj(j)) THEN
            !$OMP CRITICAL
            !      WRITE(6,'(A,2I)') 'OBS DATA SORT ERROR: ',nn,nj(j)
            !      WRITE(6,'(F6.2,A,F6.2)') lat(j),'< LAT <',lat(j+1)
            !      WRITE(6,'(F6.2,A,F6.2)') MINVAL(tmp2lat(njs(j)+1:njs(j)+nj(j))),'< OBSLAT <',MAXVAL(tmp2lat(njs(j)+1:njs(j)+nj(j)))
            WRITE(6,*) 'OBS DATA SORT ERROR: ',nn,nj(j)
            WRITE(6,*) lat(j),'< LAT <',lat(j+1)
            WRITE(6,*) MINVAL(tmp2lat(njs(j)+1:njs(j)+nj(j))),'< OBSLAT <',MAXVAL(tmp2lat(njs(j)+1:njs(j)+nj(j)))
            !$OMP END CRITICAL
        END IF
    END DO
    !$OMP END DO
    !$OMP END PARALLEL
    
    if(verb) call monitor_real_1D(tmperr, 'tmperr')
!     print*,'tmperr',tmperr
    
    DEALLOCATE( tmp2elm  )
    DEALLOCATE( tmp2lon  )
    DEALLOCATE( tmp2lat  )
    DEALLOCATE( tmp2lev  )
    DEALLOCATE( tmp2dat  )
    DEALLOCATE( tmp2err  )
    !DEALLOCATE( tmp2k    )
    DEALLOCATE( tmp2dep  )
    DEALLOCATE( tmp2hdxf )
    DEALLOCATE( tmpelm   )
    DEALLOCATE( tmplon   )
    DEALLOCATE( tmplat   )
    DEALLOCATE( tmplev   )
    DEALLOCATE( tmpdat   )
    DEALLOCATE( tmperr   )
    DEALLOCATE( tmpk     )
    DEALLOCATE( tmpdep   )
    DEALLOCATE( tmphdxf  )
    DEALLOCATE( tmpqc    )
    
    RETURN
END SUBROUTINE set_letkf_obs

SUBROUTINE monit_mean(file)
    !-----------------------------------------------------------------------
    ! Monitor departure from gues/anal mean
    !-----------------------------------------------------------------------
    IMPLICIT NONE
    CHARACTER(4),INTENT(IN) :: file
    REAL(r_size) :: v3d(nlon,nlat,nlev,nv3d)
    REAL(r_size) :: v2d(nlon,nlat,nv2d)
    REAL(r_size) :: p_full(nlon,nlat,nlev)
!     REAL(r_size) :: elem
    REAL(r_size) :: bias_u,bias_v,bias_t,bias_ps,bias_q,bias_rh
    REAL(r_size) :: rmse_u,rmse_v,rmse_t,rmse_ps,rmse_q,rmse_rh
    REAL(r_size) :: hdxf,dep,ri,rj,rk
    INTEGER      :: n,iu,iv,it,iq,ips,irh
    CHARACTER(11):: filename='filexxx.grd'
    
    rmse_u  = 0.0d0
    rmse_v  = 0.0d0
    rmse_t  = 0.0d0
    rmse_q  = 0.0d0
    rmse_ps = 0.0d0
    rmse_rh = 0.0d0
    bias_u  = 0.0d0
    bias_v  = 0.0d0
    bias_t  = 0.0d0
    bias_q  = 0.0d0
    bias_ps = 0.0d0
    bias_rh = 0.0d0
    iu  = 0
    iv  = 0
    it  = 0
    iq  = 0
    ips = 0
    irh = 0
    
    WRITE(filename(1:7),'(A4,A3)') file,'_me'
    CALL read_grd(filename,v3d,v2d)
    CALL calc_pfull(nlon,nlat,v2d(:,:,iv2d_ps),p_full)
    
    DO n=1,nobs
        CALL phys2ijk(p_full,obselm(n),obslon(n),obslat(n),obslev(n),ri,rj,rk)
        IF(CEILING(rk) > nlev) CYCLE
        IF(CEILING(rk) < 2 .AND. NINT(obselm(n)) /= id_ps_obs) THEN
            IF(NINT(obselm(n)) == id_u_obs .OR. NINT(obselm(n)) == id_v_obs) THEN
                rk = 1.00001d0
            ELSE
                CYCLE
            END IF
        END IF
        CALL Trans_XtoY(obselm(n),ri,rj,rk,v3d,v2d,p_full,hdxf)
        dep = obsdat(n) - hdxf
        SELECT CASE(NINT(obselm(n)))
        CASE(id_u_obs)
            rmse_u = rmse_u + dep**2
            bias_u = bias_u + dep
            iu = iu + 1
        CASE(id_v_obs)
            rmse_v = rmse_v + dep**2
            bias_v = bias_v + dep
            iv = iv + 1
        CASE(id_t_obs)
            rmse_t = rmse_t + dep**2
            bias_t = bias_t + dep
            it = it + 1
        CASE(id_q_obs)
            rmse_q = rmse_q + dep**2
            bias_q = bias_q + dep
            iq = iq + 1
        CASE(id_ps_obs)
            rmse_ps = rmse_ps + dep**2
            bias_ps = bias_ps + dep
            ips = ips + 1
        CASE(id_rh_obs)
            rmse_rh = rmse_rh + dep**2
            bias_rh = bias_rh + dep
            irh = irh + 1
        END SELECT
    END DO
    
    IF(iu == 0) THEN
        rmse_u = undef
        bias_u = undef
    ELSE
        rmse_u = SQRT(rmse_u / REAL(iu,r_size))
        bias_u = bias_u / REAL(iu,r_size)
    END IF

    IF(iv == 0) THEN
        rmse_v = undef
        bias_v = undef
    ELSE
        rmse_v = SQRT(rmse_v / REAL(iv,r_size))
        bias_v = bias_v / REAL(iv,r_size)
    END IF

    IF(it == 0) THEN
        rmse_t = undef
        bias_t = undef
    ELSE
        rmse_t = SQRT(rmse_t / REAL(it,r_size))
        bias_t = bias_t / REAL(it,r_size)
    END IF

    IF(iq == 0) THEN
        rmse_q = undef
        bias_q = undef
    ELSE
        rmse_q = SQRT(rmse_q / REAL(iq,r_size))
        bias_q = bias_q / REAL(iq,r_size)
    END IF

    IF(ips == 0) THEN
        rmse_ps = undef
        bias_ps = undef
    ELSE
        rmse_ps = SQRT(rmse_ps / REAL(ips,r_size))
        bias_ps = bias_ps / REAL(ips,r_size)
    END IF

    IF(irh == 0) THEN
        rmse_rh = undef
        bias_rh = undef
    ELSE
        rmse_rh = SQRT(rmse_rh / REAL(irh,r_size))
        bias_rh = bias_rh / REAL(irh,r_size)
    END IF
    
    WRITE(6,'(3A)') '== PARTIAL OBSERVATIONAL DEPARTURE (',file,') =============================='
    WRITE(6,'(A,6A11)'   ) 'Var.: ',    'U',   'V',   'T',   'Q',   'PS',   'RH'
    WRITE(6,'(A,6I11)'   ) 'Nobs: ',    iu,    iv,    it,    iq,    ips,    irh
    WRITE(6,'(A,6ES11.3)') 'RMSE: ',rmse_u,rmse_v,rmse_t,rmse_q,rmse_ps,rmse_rh
    WRITE(6,'(A,6ES11.3)') 'BIAS: ',bias_u,bias_v,bias_t,bias_q,bias_ps,bias_rh
    WRITE(6,'(A)') '========================================================================'
!    WRITE(6,'(3A)') '== PARTIAL OBSERVATIONAL DEPARTURE (',file,') =============================='
!    WRITE(6,'(6A12)') 'U','V','T','Q','PS','RH'
!    WRITE(6,'(6ES12.3)') bias_u,bias_v,bias_t,bias_q,bias_ps,bias_rh
!    WRITE(6,'(6ES12.3)') rmse_u,rmse_v,rmse_t,rmse_q,rmse_ps,rmse_rh
!    WRITE(6,'(A)') '== NUMBER OF OBSERVATIONS =============================================='
!    WRITE(6,'(6A12)') 'U','V','T','Q','PS','RH'
!    WRITE(6,'(6I12)') iu,iv,it,iq,ips,irh
!    WRITE(6,'(A)') '========================================================================'
    
    RETURN
END SUBROUTINE monit_mean

END MODULE letkf_obs
