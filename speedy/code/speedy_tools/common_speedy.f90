MODULE common_speedy
    !=======================================================================
    !
    ! [PURPOSE:] Common Information for SPEEDY
    !
    ! [HISTORY:]
    !   10/15/2004 Takemasa Miyoshi  created
    !   01/23/2009 Takemasa Miyoshi  modified
    !
    !=======================================================================
    !$USE OMP_LIB
    USE common
    IMPLICIT NONE
    PUBLIC
    !-----------------------------------------------------------------------
    ! General parameters
    !-----------------------------------------------------------------------
    INTEGER,PARAMETER :: nlon = 96             !             longitudes' #      
    INTEGER,PARAMETER :: nlat = nlon/2         !              latitudes' # 
    INTEGER,PARAMETER :: nlev = 7              !        vertical levels' #   
    INTEGER,PARAMETER :: nv3d = 4 ! u,v,t,q    !           3d variables' #
    INTEGER,PARAMETER :: nv2d = 2 ! ps,rain    !           2d variables' #
    INTEGER,PARAMETER :: iv3d_u    = 1         !                      u id
    INTEGER,PARAMETER :: iv3d_v    = 2         !                      v id
    INTEGER,PARAMETER :: iv3d_t    = 3         !                      t id
    INTEGER,PARAMETER :: iv3d_q    = 4         !                      q id
    INTEGER,PARAMETER :: iv2d_ps   = 1         !                     ps id
    INTEGER,PARAMETER :: iv2d_rain = 2         !                   rain id
    INTEGER,PARAMETER :: nij0 = nlon * nlat    !  horizontal grid points # 
    INTEGER,PARAMETER :: nlevall=nlev*nv3d+nv2d!       horizontal arrays' # 
    INTEGER,PARAMETER :: ngpv = nij0*nlevall   !   total state variables' #
    REAL(r_size),SAVE ::       lon(nlon)       !           Longitude vector
    REAL(r_size),SAVE ::       lat(nlat)       !            Latitude vector
    REAL(r_size),SAVE ::       sig(nlev)       !               Sigma vector
    REAL(r_size),SAVE ::        dx(nlat)       ! grid cell lon width vector
    REAL(r_size),SAVE ::        dy(nlat)       ! grid cell lat widht vector
    REAL(r_size),SAVE ::       dy2(nlat)       !  
    REAL(r_size),SAVE ::     fcori(nlat)       !  Coriolis parameter vector   
    REAL(r_size),SAVE :: phi0(nlon,nlat)       !        Geopotential height
    CHARACTER(4),SAVE :: element(nv3d+nv2d)    !            Variable labels

CONTAINS
!-----------------------------------------------------------------------
! Set parameters and create basic data structures  
!-----------------------------------------------------------------------
SUBROUTINE set_common_speedy
    
    IMPLICIT NONE
    INTEGER :: i !,j
    
    WRITE(6,'(A)') 'Hello from set_common_speedy'
    !
    ! Elements
    !
    element(iv3d_u)           = 'U   '
    element(iv3d_v)           = 'V   '
    element(iv3d_t)           = 'T   '
    element(iv3d_q)           = 'Q   '
    element(nv3d + iv2d_ps)   = 'PS  '
    element(nv3d + iv2d_rain) = 'RAIN'
    !
    ! Lon, Lat, Sigma
    !
    !$OMP PARALLEL DO PRIVATE(i)
    DO i = 1,nlon
        lon(i) = 360.d0 / nlon * (i-1)
    END DO
    !$OMP END PARALLEL DO
    lat(1) = -87.159d0
    lat(2) = -83.479d0
    lat(3) = -79.777d0
    lat(4) = -76.070d0
    lat(5) = -72.362d0
    lat(6) = -68.652d0
    lat(7) = -64.942d0
    lat(8) = -61.232d0
    lat(9) = -57.521d0
    lat(10)= -53.810d0
    lat(11)= -50.099d0
    lat(12)= -46.389d0
    lat(13)= -42.678d0
    lat(14)= -38.967d0
    lat(15)= -35.256d0
    lat(16)= -31.545d0
    lat(17)= -27.833d0
    lat(18)= -24.122d0
    lat(19)= -20.411d0
    lat(20)= -16.700d0
    lat(21)= -12.989d0
    lat(22)=  -9.278d0
    lat(23)=  -5.567d0
    lat(24)=  -1.856d0
    lat(25)=   1.856d0
    lat(26)=   5.567d0
    lat(27)=   9.278d0
    lat(28)=  12.989d0
    lat(29)=  16.700d0
    lat(30)=  20.411d0
    lat(31)=  24.122d0
    lat(32)=  27.833d0
    lat(33)=  31.545d0
    lat(34)=  35.256d0
    lat(35)=  38.967d0
    lat(36)=  42.678d0
    lat(37)=  46.389d0
    lat(38)=  50.099d0
    lat(39)=  53.810d0
    lat(40)=  57.521d0
    lat(41)=  61.232d0
    lat(42)=  64.942d0
    lat(43)=  68.652d0
    lat(44)=  72.362d0
    lat(45)=  76.070d0
    lat(46)=  79.777d0
    lat(47)=  83.479d0
    lat(48)=  87.159d0
    
    sig(1) =    .950d0
    sig(2) =    .835d0
    sig(3) =    .685d0
    sig(4) =    .510d0
    sig(5) =    .340d0
    sig(6) =    .200d0
    sig(7) =    .080d0
    !
    ! dx and dy
    !
    !$OMP PARALLEL
    !$OMP WORKSHARE
    dx(:) = 2.0d0 * pi * re * cos(lat(:) * pi / 180.0d0) / REAL(nlon,r_size)
    !$OMP END WORKSHARE
    
    !$OMP DO
    DO i=1,nlat-1
        dy(i) = 2.0d0 * pi * re * (lat(i+1) - lat(i)) / 360.0d0
    END DO
    !$OMP END DO
    !$OMP END PARALLEL
    dy(nlat) = 2.0d0 * pi * re * (90.0d0 - lat(nlat)) / 180.0d0
    
    !$OMP PARALLEL DO
    DO i=2,nlat
        dy2(i) = (dy(i-1) + dy(i)) * 0.5d0
    END DO
    !$OMP END PARALLEL DO
    dy2(1) = (dy(nlat) + dy(1)) * 0.5d0
    
    !
    ! Corioris parameter
    !
    !$OMP PARALLEL WORKSHARE
    fcori(:) = 2.0d0 * r_omega * sin(lat(:) * pi / 180.0d0)
    !$OMP END PARALLEL WORKSHARE
    !
    ! Surface geopotential (Read Orography file)
    !
    READ(21) phi0
    
    RETURN
END SUBROUTINE set_common_speedy

!-----------------------------------------------------------------------
! File I/O
!-----------------------------------------------------------------------
SUBROUTINE read_grd(filename,v3d,v2d)
    !
    ! single precision grid file -> double precision state vector 
    !
    IMPLICIT NONE
    CHARACTER(*),INTENT(IN) :: filename
    REAL(r_size),INTENT(OUT) :: v3d(nlon,nlat,nlev,nv3d)! 3d state variables
    REAL(r_size),INTENT(OUT) :: v2d(nlon,nlat,nv2d)     ! 2d state variables
    REAL(r_sngl) :: buf4(nlon,nlat)
    INTEGER :: iunit,iolen
    INTEGER :: k,n,irec
    
    iunit=11
    INQUIRE(IOLENGTH=iolen) iolen
    OPEN(iunit,FILE=filename,FORM='unformatted',ACCESS='direct',RECL=nij0*iolen)
    
    irec=1

    ! 3d variables first 
    DO n=1,nv3d
        DO k=1,nlev
            READ(iunit,REC=irec) buf4
            irec = irec + 1
            v3d(:,:,k,n) = REAL(buf4,r_size)
        END DO
    END DO

    ! 2d variables at the end
    DO n=1,nv2d
        READ(iunit,REC=irec) buf4
        irec = irec + 1
        v2d(:,:,n) = REAL(buf4,r_size)
    END DO
    
    CLOSE(iunit)
    
    RETURN
END SUBROUTINE read_grd

SUBROUTINE read_grd4(filename,v3d,v2d)
    !
    ! single precision grid file -> single precision state vector 
    !
    IMPLICIT NONE
    CHARACTER(*),INTENT(IN) :: filename
    REAL(r_sngl),INTENT(OUT) :: v3d(nlon,nlat,nlev,nv3d)! 3d state variables
    REAL(r_sngl),INTENT(OUT) :: v2d(nlon,nlat,nv2d)     ! 2d state variables
    INTEGER :: iunit,iolen
    INTEGER :: i,j,k,n,irec
    
    iunit=11
    INQUIRE(IOLENGTH=iolen) iolen
    OPEN(iunit,FILE=filename,FORM='unformatted',ACCESS='direct',RECL=nij0*iolen)
    
    irec=1
    DO n=1,nv3d
        DO k=1,nlev
            READ(iunit,REC=irec) ((v3d(i,j,k,n),i=1,nlon),j=1,nlat)
            irec = irec + 1
        END DO
    END DO
    
    DO n=1,nv2d
        READ(iunit,REC=irec) ((v2d(i,j,n),i=1,nlon),j=1,nlat)
        irec = irec + 1
    END DO
    
    CLOSE(iunit)
    
    RETURN
END SUBROUTINE read_grd4

SUBROUTINE write_grd(filename,v3d,v2d)
    !
    ! Double precision state vector -> single precision grid file
    !
    IMPLICIT NONE
    CHARACTER(*),INTENT(IN) :: filename
    REAL(r_size),INTENT(IN) :: v3d(nlon,nlat,nlev,nv3d)! 3d state variables
    REAL(r_size),INTENT(IN) :: v2d(nlon,nlat,nv2d)     ! 2d state variables
    REAL(r_sngl) :: buf4(nlon,nlat)
    INTEGER :: iunit,iolen
    INTEGER :: k,n,irec
    
    iunit=55
    INQUIRE(IOLENGTH=iolen) iolen
    OPEN(iunit,FILE=filename,FORM='unformatted',ACCESS='direct',RECL=nij0*iolen)
    
    irec=1
    DO n=1,nv3d
        DO k=1,nlev
            buf4 = REAL(v3d(:,:,k,n),r_sngl)
            WRITE(iunit,REC=irec) buf4
            irec = irec + 1
        END DO
    END DO
    
    DO n=1,nv2d
        buf4 = REAL(v2d(:,:,n),r_sngl)
        WRITE(iunit,REC=irec) buf4
        irec = irec + 1
    END DO
    
    CLOSE(iunit)
    
    RETURN
END SUBROUTINE write_grd

SUBROUTINE write_grd4(filename,v3d,v2d)
    !
    ! single precision state vector -> single precision grid file
    !
    IMPLICIT NONE
    CHARACTER(*),INTENT(IN) :: filename
    REAL(r_sngl),INTENT(IN) :: v3d(nlon,nlat,nlev,nv3d)! 3d state variables
    REAL(r_sngl),INTENT(IN) :: v2d(nlon,nlat,nv2d)     ! 2d state variables
    INTEGER :: iunit,iolen
    INTEGER :: i,j,k,n,irec
    
    iunit=55
    INQUIRE(IOLENGTH=iolen) iolen
    OPEN(iunit,FILE=filename,FORM='unformatted',ACCESS='direct',RECL=nij0*iolen)
    
    irec=1
    DO n=1,nv3d
        DO k=1,nlev
            WRITE(iunit,REC=irec) ((v3d(i,j,k,n),i=1,nlon),j=1,nlat)
            irec = irec + 1
        END DO
    END DO

    DO n=1,nv2d
        WRITE(iunit,REC=irec) ((v2d(i,j,n),i=1,nlon),j=1,nlat)
        irec = irec + 1
    END DO
    
    CLOSE(iunit)
    
    RETURN
END SUBROUTINE write_grd4

!-----------------------------------------------------------------------
! p_full
!-----------------------------------------------------------------------
SUBROUTINE calc_pfull(ix,jy,ps,p_full)
    !
    ! Create 3d pressure array from 2d surface pressure array
    !
    IMPLICIT NONE
    INTEGER,INTENT(IN)       ::        ix,jy      !         grid size
    REAL(r_size),INTENT(IN)  ::     ps(ix,jy)     !  surface pressure
    REAL(r_size),INTENT(OUT) :: p_full(ix,jy,nlev)! 3d pressure field
    INTEGER                  ::        i, j, k
    
    !$OMP PARALLEL DO PRIVATE(i,j,k)
    DO k=1,nlev
        DO j=1,jy
            DO i=1,ix
                p_full(i,j,k) = ps(i,j) * sig(k)
            END DO
        END DO
    END DO
    !$OMP END PARALLEL DO
    
    RETURN
END SUBROUTINE calc_pfull

!-----------------------------------------------------------------------
! Monitor
!-----------------------------------------------------------------------
SUBROUTINE monit_grd(v3d,v2d)
    !
    ! Show max and min values per variable and level
    !
    IMPLICIT NONE
    REAL(r_size),INTENT(IN) :: v3d(nlon,nlat,nlev,nv3d)! 3d state variables
    REAL(r_size),INTENT(IN) :: v2d(nlon,nlat,nv2d)     ! 2d state variables
    INTEGER :: k,n
    
    DO k=1,nlev
        WRITE(6,'(I2,A)') k,'th level'
        DO n=1,nv3d
            WRITE(6,'(A,2ES10.2)') element(n),MAXVAL(v3d(:,:,k,n)),MINVAL(v3d(:,:,k,n))
        END DO
    END DO
    
    DO n=1,nv2d
        WRITE(6,'(A,2ES10.2)') element(nv3d+n),MAXVAL(v2d(:,:,n)),MINVAL(v2d(:,:,n))
    END DO
    
    RETURN
END SUBROUTINE monit_grd

!-----------------------------------------------------------------------
! Ensemble manipulations
!-----------------------------------------------------------------------
SUBROUTINE ensmean_grd(member,nij,v3d,v2d,v3dm,v2dm)
    !
    ! Calculate 3d ensemble mean from 3d ensemble
    !
    IMPLICIT NONE
    INTEGER,INTENT(IN) :: member                          !   ensemble members' #
    INTEGER,INTENT(IN) :: nij                             !        grid points' #
    REAL(r_size),INTENT(IN)  :: v3d (nij,nlev,member,nv3d)!      3d ensemble vars
    REAL(r_size),INTENT(IN)  :: v2d (nij,member,nv2d)     !      2d ensemble vars
    REAL(r_size),INTENT(OUT) :: v3dm(nij,nlev,nv3d)       ! 3d ensemble mean vars    
    REAL(r_size),INTENT(OUT) :: v2dm(nij,nv2d)            ! 2d ensemble mean vars
    INTEGER :: i,k,m,n
    
    DO n=1,nv3d
        !$OMP PARALLEL DO PRIVATE(i,k,m)
        DO k=1,nlev
            DO i=1,nij
                v3dm(i,k,n) = v3d(i,k,1,n)
                DO m=2,member
                    v3dm(i,k,n) = v3dm(i,k,n) + v3d(i,k,m,n)
                END DO
                v3dm(i,k,n) = v3dm(i,k,n) / REAL(member,r_size)
            END DO
        END DO
        !$OMP END PARALLEL DO
    END DO
    
    DO n=1,nv2d
        DO i=1,nij
            v2dm(i,n) = v2d(i,1,n)
            DO m=2,member
                v2dm(i,n) = v2dm(i,n) + v2d(i,m,n)
            END DO
            v2dm(i,n) = v2dm(i,n) / REAL(member,r_size)
        END DO
    END DO
    
    RETURN
END SUBROUTINE ensmean_grd

END MODULE common_speedy
