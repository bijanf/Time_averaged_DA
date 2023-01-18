MODULE common_obs_speedy
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
  USE common_speedy
  USE common_tools

  IMPLICIT NONE
  PUBLIC

  INTEGER,PARAMETER ::  nid_obs    =     7 ! # of observation types
  INTEGER,PARAMETER :: id_u_obs    =  2819
  INTEGER,PARAMETER :: id_v_obs    =  2820
  INTEGER,PARAMETER :: id_t_obs    =  3073
  INTEGER,PARAMETER :: id_q_obs    =  3330
  INTEGER,PARAMETER :: id_rh_obs   =  3331
  INTEGER,PARAMETER :: id_ps_obs   = 14593
  INTEGER,PARAMETER :: id_rain_obs =  9999
  INTEGER,PARAMETER :: id_trw_linear_obs =  7835
  ! Monthly means parameters
  INTEGER,PARAMETER :: NGP=NLON*NLAT
  INTEGER,PARAMETER :: NS2D =17, NS2D_D=10, NS2D2= 2, NS3D1=8 !,NS3D_D= 6
  INTEGER,PARAMETER :: NS3D2= 6, NS3D3 = 5, NS3D = NS3D1+NS3D2+NS3D3

CONTAINS

  SUBROUTINE Trans_XtoY(elm,ri,rj,rk,v3d,v2d,p_full,yobs)
    !-----------------------------------------------------------------------
    ! Transformation from model variables to an observation
    !-----------------------------------------------------------------------
    IMPLICIT NONE
    REAL(r_size),INTENT(IN)  ::    elm
    REAL(r_size),INTENT(IN)  ::    ri,rj,rk
    REAL(r_size),INTENT(IN)  ::    v3d(nlon,nlat,nlev,nv3d)
    REAL(r_size),INTENT(IN)  ::    v2d(nlon,nlat,nv2d)
    REAL(r_size),INTENT(IN)  :: p_full(nlon,nlat,nlev)
    REAL(r_size),INTENT(OUT) ::    yobs
    REAL(r_size)             ::     rh(nlon,nlat,nlev)
    INTEGER                  ::     j,k !,i
    INTEGER                  ::     is,ie,js,je,ks,ke

    ie = CEILING( ri );     is = ie-1
    je = CEILING( rj );     js = je-1
    ke = CEILING( rk );     ks = ke-1

    SELECT CASE (NINT(elm))
    CASE(id_u_obs)  ! U
       CALL itpl_3d(v3d(:,:,:,iv3d_u),ri,rj,rk,yobs)
    CASE(id_v_obs)  ! V
       CALL itpl_3d(v3d(:,:,:,iv3d_v),ri,rj,rk,yobs)
    CASE(id_t_obs)  ! T
       CALL itpl_3d(v3d(:,:,:,iv3d_t),ri,rj,rk,yobs)
    CASE(id_q_obs)  ! Q
       CALL itpl_3d(v3d(:,:,:,iv3d_q),ri,rj,rk,yobs)
    CASE(id_ps_obs) ! PS
       CALL itpl_2d(v2d(:,:,iv2d_ps),ri,rj,yobs)
    CASE(id_rain_obs) ! RAIN
       CALL itpl_2d(v2d(:,:,iv2d_rain),ri,rj,yobs)
    CASE(id_rh_obs) ! RH
       DO k=ks,ke
          DO j=js,je
             IF(ie <= nlon ) THEN
                CALL calc_rh(v3d(is,j,k,iv3d_t),v3d(is,j,k,iv3d_q),&
                     & p_full(is,j,k),rh(is,j,k))
                CALL calc_rh(v3d(ie,j,k,iv3d_t),v3d(ie,j,k,iv3d_q),&
                     & p_full(ie,j,k),rh(ie,j,k))
             ELSE
                CALL calc_rh(v3d(is,j,k,iv3d_t),v3d(is,j,k,iv3d_q),&
                     & p_full(is,j,k),rh(is,j,k))
                CALL calc_rh(v3d( 1,j,k,iv3d_t),v3d( 1,j,k,iv3d_q),&
                     & p_full( 1,j,k),rh( 1,j,k))
             END IF
          END DO
       END DO
       CALL itpl_3d(rh,ri,rj,rk,yobs)
    END SELECT

    RETURN
  END SUBROUTINE Trans_XtoY

  SUBROUTINE calc_rh(t,q,p,rh)
    !-----------------------------------------------------------------------
    ! Compute relative humidity (RH)
    !-----------------------------------------------------------------------
    IMPLICIT NONE
    REAL(r_size),PARAMETER   ::  t0 = 273.15d0
    REAL(r_size),PARAMETER   :: e0c =   6.11d0
    REAL(r_size),PARAMETER   ::  al =   17.3d0
    REAL(r_size),PARAMETER   ::  bl =  237.3d0
    REAL(r_size),PARAMETER   :: e0i = 6.1121d0
    REAL(r_size),PARAMETER   ::  ai = 22.587d0
    REAL(r_size),PARAMETER   ::  bi = 273.86d0
    REAL(r_size),INTENT(IN)  :: t,q,p
    REAL(r_size),INTENT(OUT) :: rh
    REAL(r_size) :: e,es,tc

    e = q * p * 0.01d0 / (0.378d0 * q + 0.622d0)

    tc = t-t0
    IF(tc >= 0.0d0) THEN
       es = e0c * exp(al*tc/(bl+tc))
    ELSE IF(tc <= -15.d0) THEN
       es = e0i * exp(ai*tc/(bi+tc))
    ELSE
       es = e0c * exp(al*tc/(bl+tc)) * (15.0d0+tc)/15.0d0 &
            + e0i * exp(ai*tc/(bi+tc)) * (-tc) / 15.0d0
    END IF

    rh = e/es

    RETURN
  END SUBROUTINE calc_rh

  !-----------------------------------------------------------------------
  ! Pressure adjustment for a different height level
  !-----------------------------------------------------------------------
  SUBROUTINE prsadj(p,dz,t,q)
    IMPLICIT NONE
    REAL(r_size),INTENT(INOUT) ::     p
    REAL(r_size),INTENT(IN)    ::    dz ! height difference (target - original) [m]
    REAL(r_size),INTENT(IN)    ::     t ! temperature [K] at target level
    REAL(r_size),INTENT(IN)    ::     q ! humidity [kg/kg] at target level
    REAL(r_size),PARAMETER     :: gamma = 5.0d-3 ! lapse rate [K/m]
    REAL(r_size)               :: tv

    tv = t * (1.0d0 + 0.608d0 * q)
    IF(dz /= 0) THEN
       !    p = p * ((-gamma*dz+tv)/tv)**(gg/(gamma*rd)) !tv is at original level
       p = p * (tv/(tv+gamma*dz))**(gg/(gamma*rd)) !tv is at target level
    END IF

    RETURN
  END SUBROUTINE prsadj

  !-----------------------------------------------------------------------
  ! Coordinate conversion
  !-----------------------------------------------------------------------
  SUBROUTINE phys2ijk(p_full,elem,rlon,rlat,rlev,ri,rj,rk)
    !
    ! (rlon,rlat,rlev)  ->  (ri,rj,rk)
    !
    IMPLICIT NONE
    REAL(r_size),INTENT(IN)  :: p_full(nlon,nlat,nlev)
    REAL(r_size),INTENT(IN)  :: elem
    REAL(r_size),INTENT(IN)  :: rlon
    REAL(r_size),INTENT(IN)  :: rlat
    REAL(r_size),INTENT(IN)  :: rlev ! pressure level
    REAL(r_size),INTENT(OUT) :: ri
    REAL(r_size),INTENT(OUT) :: rj
    REAL(r_size),INTENT(OUT) :: rk
    REAL(r_size)             :: aj,ak
    REAL(r_size)             :: rlnp
    REAL(r_size)             :: lnps(nlon,nlat)
    REAL(r_size)             :: lnp_profile(nlev)
    INTEGER                  :: i,j,k
    !
    ! rlon [0°,360°] -> ri (1,nlon+1]
    !
    IF(rlon == 0.0 .OR. rlon == 360.0) THEN
       ri = REAL(nlon+1,r_size)
    ELSE
       ri = rlon / 360.0d0 * REAL(nlon,r_size) + 1.0d0
    END IF
    ! Error catching mechanism. Never visited if rlon in [0°,360°]
    IF(CEILING(ri) < 2 .OR. nlon+1 < CEILING(ri)) RETURN
    !
    !  rlat [-90°,90°] -> rj  [0,nlat)
    !
    DO j = 1,nlat
       IF(rlat < lat(j)) EXIT
    END DO
    IF(j == 1) THEN
       rj = (rlat + 90.0d0) / (lat(1) + 90.0d0)
    ELSE IF(j == nlat+1) THEN
       ! this condition is never fullfiled since j in {1,nlat}  :/
       aj = (rlat - lat(nlat)) / (90.0d0 - lat(nlat))
       rj = REAL(nlat,r_size) + aj
    ELSE
       aj = (rlat - lat(j-1)) / (lat(j) - lat(j-1))
       rj = REAL(j-1,r_size) + aj
    END IF
    ! error catching mechanism visited if rlat < lat(1)
    IF(CEILING(rj) < 2 .OR. nlat < CEILING(rj)) RETURN
    !
    ! rlev -> rk (1,nlev-1]
    !
    IF(NINT(elem) == id_ps_obs) THEN ! surface pressure observation
       rk = 0.0d0
    ELSE IF(NINT(elem) == id_trw_linear_obs) THEN ! trw linear
       rk = 0.0d0
    ELSE
       ! pressure logarithm horizontal interpolation
       i = CEILING(ri) ! ri: (1, nlon+1] -> i: {2,...,nlon + 1}
       ! p_full(nlon +1,*,*) doesn't exist !!!
       j = CEILING(rj) ! rj: [0, nlat  ) -> j: {0,...,nlat}  !
       ! j=0 occurs only for rj=0 !! singular point
       DO k = 1,nlev
          IF(i <= nlon) THEN
             lnps(i-1:i,j-1:j) = LOG ( p_full(i-1:i,j-1:j,k) )
          ELSE ! for i = nlon + 1  (longitudinal Periodicity)
             lnps(i-1  ,j-1:j) = LOG ( p_full(i-1  ,j-1:j,k) )
             lnps(  1  ,j-1:j) = LOG ( p_full(  1  ,j-1:j,k) )
          END IF
          CALL itpl_2d( lnps, ri, rj, lnp_profile(k) )
          ! lnp_profile(k): pressure logaritm profile at point (rlon,rlat)
       END DO
       !
       rlnp = LOG(rlev) ! Log pressure
       !
       ! find rk
       !
       DO k = 2,nlev-1
          IF( lnp_profile(k) < rlnp ) EXIT ! assuming descending order of lnp_profile
       END DO
       ak = (rlnp - lnp_profile(k-1)) / (lnp_profile(k) - lnp_profile(k-1))
       rk = REAL(k-1,r_size) + ak  ! rk : (1,nlev-1]

       !       WRITE(6,'(A,F6.2)') '-- ak = ', ak
       !       WRITE(6,'(A,I3)'  ) '--  k = ',  k
       !       WRITE(6,'(A,F6.2)') '-- rk = ', rk
    END IF

    RETURN
  END SUBROUTINE phys2ijk

  !-----------------------------------------------------------------------
  ! Interpolation routines
  !-----------------------------------------------------------------------
  SUBROUTINE itpl_2d(var,ri,rj,var5)
    !
    !  Interpolation of 2d horizontal field var in point (ri,rj)
    !
    IMPLICIT NONE
    REAL(r_size),INTENT(IN)  :: var(nlon,nlat)
    REAL(r_size),INTENT(IN)  :: ri
    REAL(r_size),INTENT(IN)  ::    rj
    REAL(r_size),INTENT(OUT) :: var5
    REAL(r_size)             :: ai,aj
    INTEGER                  ::  i, j

    i = CEILING(ri) ! in {2,...,nlon+1} !! var(nlon+1,*) doesn't exist !!!
    ai = ri - REAL(i-1,r_size)
    j = CEILING(rj) ! in {0,...,nlat}   !! j=0 only for rj=0 !! singular point
    aj = rj - REAL(j-1,r_size)

    IF(i <= nlon) THEN
       var5 = var(i-1,j-1) * (1-ai) * (1-aj) &
            & + var(i  ,j-1) *    ai  * (1-aj) &
            & + var(i-1,j  ) * (1-ai) *    aj  &
            & + var(i  ,j  ) *    ai  *    aj
    ELSE
       var5 = var(i-1,j-1) * (1-ai) * (1-aj) &
            & + var(1  ,j-1) *    ai  * (1-aj) &
            & + var(i-1,j  ) * (1-ai) *    aj  &
            & + var(1  ,j  ) *    ai  *    aj
    END IF

    RETURN
  END SUBROUTINE itpl_2d

  SUBROUTINE itpl_3d(var,ri,rj,rk,var5)
    !
    !  Interpolation of 3d field var in point (ri,rj,rk)
    !
    IMPLICIT NONE
    REAL(r_size),INTENT(IN) :: var(nlon,nlat,nlev)
    REAL(r_size),INTENT(IN) :: ri
    REAL(r_size),INTENT(IN) :: rj
    REAL(r_size),INTENT(IN) :: rk
    REAL(r_size),INTENT(OUT) :: var5
    REAL(r_size) :: ai,aj,ak
    INTEGER :: i,j,k

    i = CEILING(ri)
    ai = ri - REAL(i-1,r_size)
    j = CEILING(rj)
    aj = rj - REAL(j-1,r_size)
    k = CEILING(rk)
    ak = rk - REAL(k-1,r_size)

    IF(i <= nlon) THEN
       var5 =   var(i-1,j-1,k-1) * (1-ai) * (1-aj) * (1-ak) &
            & + var(i  ,j-1,k-1) *    ai  * (1-aj) * (1-ak) &
            & + var(i-1,j  ,k-1) * (1-ai) *    aj  * (1-ak) &
            & + var(i  ,j  ,k-1) *    ai  *    aj  * (1-ak) &
            & + var(i-1,j-1,k  ) * (1-ai) * (1-aj) *    ak  &
            & + var(i  ,j-1,k  ) *    ai  * (1-aj) *    ak  &
            & + var(i-1,j  ,k  ) * (1-ai) *    aj  *    ak  &
            & + var(i  ,j  ,k  ) *    ai  *    aj  *    ak
    ELSE
       var5 =   var(i-1,j-1,k-1) * (1-ai) * (1-aj) * (1-ak) &
            & + var(1  ,j-1,k-1) *    ai  * (1-aj) * (1-ak) &
            & + var(i-1,j  ,k-1) * (1-ai) *    aj  * (1-ak) &
            & + var(1  ,j  ,k-1) *    ai  *    aj  * (1-ak) &
            & + var(i-1,j-1,k  ) * (1-ai) * (1-aj) *    ak  &
            & + var(1  ,j-1,k  ) *    ai  * (1-aj) *    ak  &
            & + var(i-1,j  ,k  ) * (1-ai) *    aj  *    ak  &
            & + var(1  ,j  ,k  ) *    ai  *    aj  *    ak
    END IF

    RETURN
  END SUBROUTINE itpl_3d

  !-----------------------------------------------------------------------
  ! Monitor departure
  !-----------------------------------------------------------------------
  SUBROUTINE monit_dep(nn,elm,dep,qc)
    IMPLICIT NONE
    INTEGER,     INTENT(IN) :: nn      ! Number of obs.
    REAL(r_size),INTENT(IN) :: elm(nn)
    REAL(r_size),INTENT(IN) :: dep(nn)
    INTEGER,INTENT(IN)      ::  qc(nn) ! Obs. White list
    ! 1: Useful obs.
    ! 0: Obs. with departure larger gross_error*obs. error
    REAL(r_size) :: rmse_u, rmse_v, rmse_t, rmse_q, rmse_ps, rmse_rh
    REAL(r_size) :: bias_u, bias_v, bias_t, bias_q, bias_ps, bias_rh
    INTEGER      :: n,  iu,     iv,     it,     iq,     ips,     irh

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

    !write(6,*)'nn = ', nn

    DO n=1,nn
       !IF(qc(n) /= 1) write(6,*)'obs ', n ,' is skipped!!! (qc(n))',qc(n)
       IF(qc(n) /= 1) CYCLE
       SELECT CASE(NINT(elm(n)))
       CASE(id_u_obs)
          rmse_u = rmse_u + dep(n)**2
          bias_u = bias_u + dep(n)
          iu = iu + 1
       CASE(id_v_obs)
          rmse_v = rmse_v + dep(n)**2
          bias_v = bias_v + dep(n)
          iv = iv + 1
       CASE(id_t_obs)
          rmse_t = rmse_t + dep(n)**2
          bias_t = bias_t + dep(n)
          it = it + 1
       CASE(id_q_obs)
          rmse_q = rmse_q + dep(n)**2
          bias_q = bias_q + dep(n)
          iq = iq + 1
       CASE(id_ps_obs)
          rmse_ps = rmse_ps + dep(n)**2
          bias_ps = bias_ps + dep(n)
          ips = ips + 1
       CASE(id_rh_obs)
          rmse_rh = rmse_rh + dep(n)**2
          bias_rh = bias_rh + dep(n)
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

    WRITE(6,'(A)') '== NUMBER OF OBSERVATIONS TO BE ASSIMILATED ============================'
    WRITE(6,'(6A12)') 'U','V','T','Q','PS','RH'
    WRITE(6,'(6I12)') iu,iv,it,iq,ips,irh
    WRITE(6,'(A)') '== OBSERVATIONAL DEPARTURE ============================================='
    WRITE(6,'(6A12)') 'U','V','T','Q','PS','RH'
    WRITE(6,'(6ES12.3)') bias_u,bias_v,bias_t,bias_q,bias_ps,bias_rh
    WRITE(6,'(6ES12.3)') rmse_u,rmse_v,rmse_t,rmse_q,rmse_ps,rmse_rh
    WRITE(6,'(A)') '========================================================================'

    RETURN
  END SUBROUTINE monit_dep

  !-----------------------------------------------------------------------
  ! Basic modules for observation input
  !-----------------------------------------------------------------------
  SUBROUTINE get_nobs(cfile,nn)
    IMPLICIT NONE
    CHARACTER(*),INTENT(IN) :: cfile
    INTEGER,INTENT(OUT) :: nn
    REAL(r_sngl) :: wk(6)
    INTEGER :: ios
    INTEGER :: iu,iv,it,iq,irh,ips,itrw
    INTEGER :: iunit
    LOGICAL :: ex

    nn = 0
    iu = 0
    iv = 0
    it = 0
    iq = 0
    irh = 0
    ips = 0
    itrw = 0
    iunit=91
    INQUIRE(FILE=cfile,EXIST=ex)
    IF(ex) THEN
       OPEN(iunit,FILE=cfile,FORM='unformatted',ACCESS='sequential')
       DO
          READ(iunit,IOSTAT=ios) wk
          IF(ios /= 0) EXIT

          !      WRITE(6,'(A12,F10.4)') ' HEY! wk(1):',wk(1)


          SELECT CASE(NINT(wk(1)))
          CASE(id_u_obs)
             iu = iu + 1
          CASE(id_v_obs)
             iv = iv + 1
          CASE(id_t_obs)
             it = it + 1
          CASE(id_q_obs)
             iq = iq + 1
          CASE(id_rh_obs)
             irh = irh + 1
          CASE(id_ps_obs)
             ips = ips + 1
          CASE(id_trw_linear_obs)
             itrw = itrw + 1
          END SELECT
          nn = nn + 1
       END DO
       WRITE(6,'(I10,A)') nn,' OBSERVATIONS INPUT'
       WRITE(6,'(A12,I10)') '          U:',iu
       WRITE(6,'(A12,I10)') '          V:',iv
       WRITE(6,'(A12,I10)') '          T:',it
       WRITE(6,'(A12,I10)') '          Q:',iq
       WRITE(6,'(A12,I10)') '         RH:',irh
       WRITE(6,'(A12,I10)') '         Ps:',ips
       WRITE(6,'(A12,I10)') '        trw:',itrw
       CLOSE(iunit)
    ELSE
       !       WRITE(6,'(2A)') cfile,' does not exist -- skipped'
       WRITE(0,'(2A)') cfile,' does not exist' ! added by WA
       stop 1 ! added by WA
    END IF

    RETURN
  END SUBROUTINE get_nobs

  SUBROUTINE read_obs(cfile,nn,elem,rlon,rlat,rlev,odat,oerr)
    IMPLICIT NONE
    CHARACTER(*),INTENT(IN)  :: cfile
    INTEGER     ,INTENT(IN)  ::      nn
    REAL(r_size),INTENT(OUT) :: elem(nn) ! element number
    REAL(r_size),INTENT(OUT) :: rlon(nn)
    REAL(r_size),INTENT(OUT) :: rlat(nn)
    REAL(r_size),INTENT(OUT) :: rlev(nn)
    REAL(r_size),INTENT(OUT) :: odat(nn)
    REAL(r_size),INTENT(OUT) :: oerr(nn)
    REAL(r_sngl)             ::   wk(6)
    INTEGER                  :: n,iunit=91

    OPEN(iunit,FILE=cfile,FORM='unformatted',ACCESS='sequential')
    DO n=1,nn
       READ(iunit) wk
       SELECT CASE(NINT(wk(1)))
       CASE(id_u_obs)
          wk(4) = wk(4) * 100.0 ! hPa -> Pa
       CASE(id_v_obs)
          wk(4) = wk(4) * 100.0 ! hPa -> Pa
       CASE(id_t_obs)
          wk(4) = wk(4) * 100.0 ! hPa -> Pa
       CASE(id_q_obs)
          wk(4) = wk(4) * 100.0 ! hPa -> Pa
       CASE(id_ps_obs)
          wk(5) = wk(5) * 100.0 ! hPa -> Pa
          wk(6) = wk(6) * 100.0 ! hPa -> Pa
       CASE(id_rh_obs)
          wk(4) = wk(4) * 100.0 ! hPa -> Pa
          wk(5) = wk(5) * 0.01 ! percent input
          wk(6) = wk(6) * 0.01 ! percent input
          !        CASE(id_trw_linear_obs)
          !           wk(5) = wk(5) * 100.0 ! hPa -> Pa
          !           wk(6) = wk(6) * 100.0 ! hPa -> Pa
       END SELECT
       elem(n) = REAL(wk(1),r_size)
       rlon(n) = REAL(wk(2),r_size)
       rlat(n) = REAL(wk(3),r_size)
       rlev(n) = REAL(wk(4),r_size)
       odat(n) = REAL(wk(5),r_size)
       oerr(n) = REAL(wk(6),r_size)
    END DO
    CLOSE(iunit)

    RETURN
  END SUBROUTINE read_obs

  !-----------------------------------------------------------------------
  ! Basic modules for monthly means I-O
  !-----------------------------------------------------------------------
  SUBROUTINE get_vars_from_array(array2D,&
       &  PSG,FPQ,TS,TSKIN,SOILW1,U0,V0,T0,CLOUDC,CLTOP,TSR,SSR,SLR,SSTAN)
    REAL*4,INTENT(IN) :: array2D(NGP,NS2D)
    real*8,INTENT(OUT),dimension(NLON,NLAT):: &
         &  PSG,FPQ,TS,TSKIN,SOILW1,U0,V0,T0,CLOUDC,CLTOP,TSR,SSR,SLR,SSTAN
    real*8            :: dummy(NLON,NLAT)
    INTEGER           :: n0

    n0=1
    PSG    = RESHAPE(array2D(:,n0),(/NLON,NLAT/)); n0 = n0 + 1
    FPQ    = RESHAPE(array2D(:,n0),(/NLON,NLAT/)); n0 = n0 + 1
    TS     = RESHAPE(array2D(:,n0),(/NLON,NLAT/)); n0 = n0 + 1
    TSKIN  = RESHAPE(array2D(:,n0),(/NLON,NLAT/)); n0 = n0 + 1
    SOILW1 = RESHAPE(array2D(:,n0),(/NLON,NLAT/)); n0 = n0 + 1
    U0     = RESHAPE(array2D(:,n0),(/NLON,NLAT/)); n0 = n0 + 1
    V0     = RESHAPE(array2D(:,n0),(/NLON,NLAT/)); n0 = n0 + 1
    T0     = RESHAPE(array2D(:,n0),(/NLON,NLAT/)); n0 = n0 + 1
    dummy  = RESHAPE(array2D(:,n0),(/NLON,NLAT/)); n0 = n0 + 1
    CLOUDC = RESHAPE(array2D(:,n0),(/NLON,NLAT/)); n0 = n0 + 1
    CLTOP  = RESHAPE(array2D(:,n0),(/NLON,NLAT/)); n0 = n0 + 1
    dummy  = RESHAPE(array2D(:,n0),(/NLON,NLAT/)); n0 = n0 + 1
    TSR    = RESHAPE(array2D(:,n0),(/NLON,NLAT/)); n0 = n0 + 1
    SSR    = RESHAPE(array2D(:,n0),(/NLON,NLAT/)); n0 = n0 + 1
    SLR    = RESHAPE(array2D(:,n0),(/NLON,NLAT/)); n0 = n0 + 1
    dummy  = RESHAPE(array2D(:,n0),(/NLON,NLAT/)); n0 = n0 + 1
    SSTAN  = RESHAPE(array2D(:,n0),(/NLON,NLAT/)); n0 = n0 + 1

    RETURN
  END subroutine get_vars_from_array

  SUBROUTINE put_vars_into_array(array2D,&
       &  PSG,FPQ,TS,TSKIN,SOILW1,U0,V0,T0,CLOUDC,CLTOP,TSR,SSR,SLR,SSTAN)
    REAL*4,INTENT(out) :: array2D(NGP,NS2D)
    real*8,INTENT(in),dimension(NLON,NLAT):: &
         &  PSG,FPQ,TS,TSKIN,SOILW1,U0,V0,T0,CLOUDC,CLTOP,TSR,SSR,SLR,SSTAN
    real*8      :: dummy(NLON,NLAT)
    INTEGER     :: n0

    n0=1
    array2D(:,n0) = RESHAPE(PSG   ,(/NLON*NLAT/)); n0 = n0 + 1
    array2D(:,n0) = RESHAPE(FPQ   ,(/NLON*NLAT/)); n0 = n0 + 1
    array2D(:,n0) = RESHAPE(TS    ,(/NLON*NLAT/)); n0 = n0 + 1
    array2D(:,n0) = RESHAPE(TSKIN ,(/NLON*NLAT/)); n0 = n0 + 1
    array2D(:,n0) = RESHAPE(SOILW1,(/NLON*NLAT/)); n0 = n0 + 1
    array2D(:,n0) = RESHAPE(U0    ,(/NLON*NLAT/)); n0 = n0 + 1
    array2D(:,n0) = RESHAPE(V0    ,(/NLON*NLAT/)); n0 = n0 + 1
    array2D(:,n0) = RESHAPE(T0    ,(/NLON*NLAT/)); n0 = n0 + 1
    array2D(:,n0) = RESHAPE(dummy ,(/NLON*NLAT/)); n0 = n0 + 1
    array2D(:,n0) = RESHAPE(CLOUDC,(/NLON*NLAT/)); n0 = n0 + 1
    array2D(:,n0) = RESHAPE(CLTOP ,(/NLON*NLAT/)); n0 = n0 + 1
    array2D(:,n0) = RESHAPE(dummy ,(/NLON*NLAT/)); n0 = n0 + 1
    array2D(:,n0) = RESHAPE(TSR   ,(/NLON*NLAT/)); n0 = n0 + 1
    array2D(:,n0) = RESHAPE(SSR   ,(/NLON*NLAT/)); n0 = n0 + 1
    array2D(:,n0) = RESHAPE(SLR   ,(/NLON*NLAT/)); n0 = n0 + 1
    array2D(:,n0) = RESHAPE(dummy ,(/NLON*NLAT/)); n0 = n0 + 1
    array2D(:,n0) = RESHAPE(SSTAN ,(/NLON*NLAT/)); n0 = n0 + 1

    RETURN
  END subroutine put_vars_into_array

  SUBROUTINE get_monthly_array(iunit,SAVE2D)
    INTEGER,INTENT(IN)  :: iunit
    REAL*4 ,INTENT(OUT) :: SAVE2D  (NGP,NS2D)!, SAVE3D(NGP,NLEV,NS3D)
    !     REAL*4                  :: SAVE2D_L(NGP,NS2D_D-NS2D2)!, FACT2D(NS2D)
    real*4             :: r4out(ngp)
    INTEGER            :: j,n!,n0!,nm!,k

    ! Import time-mean file including 3-d and 2-d fields

    !       do n=1,ns3d1
    !         do k=nlev,1,-1
    !           read(iunit) R4OUT
    !           do j=1,ngp
    !             SAVE3D(j,k,n) = R4OUT(j)
    !           enddo
    !         enddo
    !       enddo
    !
    do n=1,ns2d
       read (iunit) R4OUT
       do j=1,ngp
          SAVE2D(j,n) = R4OUT(j)
       enddo
    enddo
    !
    !       do n=1,ns2d_d-ns2d2
    !         read (iunit) R4OUT
    !         do j=1,ngp
    !           SAVE2D_L(j,n) = R4OUT(j)
    !         enddo
    !       enddo

    RETURN
  END SUBROUTINE get_monthly_array

  SUBROUTINE put_monthly_array(iunit,SAVE2D)
    INTEGER ,INTENT(IN):: iunit
    REAL*4  ,INTENT(IN):: SAVE2D  (NGP,NS2D)!, SAVE3D(NGP,NLEV,NS3D)
    !     REAL*4                  :: SAVE2D_L(NGP,NS2D_D-NS2D2)!, FACT2D(NS2D)
    real*4             :: r4out(ngp)
    INTEGER            :: j,n

    ! Dump individual monthly mean into monthly means file
    ! including 3-d and 2-d fields

    !     do n=1,ns3d1
    !       do k=nlev,1,-1
    !   do j=1,ngp
    !     R4OUT(j) = SAVE3D(j,k,n)
    !   enddo
    !   write(iunit) R4OUT
    !       enddo
    !     enddo
    !
    do n=1,ns2d
       do j=1,ngp
          R4OUT(j) = SAVE2D(j,n)
       enddo
       write (iunit) R4OUT
    enddo
    !
    !     do n=1,ns2d_d-ns2d2
    !       do j=1,ngp
    !   R4OUT(j) = SAVE2D_L(j,n)
    !       enddo
    !       write (iunit) R4OUT
    !     enddo

    RETURN
  END SUBROUTINE put_monthly_array

  SUBROUTINE get_monthly_vars(iunit,&
       &  PSG,FPQ,TS,TSKIN,SOILW1,U0,V0,T0,CLOUDC,CLTOP,TSR,SSR,SLR,SSTAN)
    INTEGER,INTENT(IN) :: iunit
    real*8 ,INTENT(OUT),dimension(NLON,NLAT)::&
         & PSG,FPQ ,TS,TSKIN,SOILW1,U0,V0,T0,CLOUDC,CLTOP,TSR,SSR,SLR,SSTAN
    REAL*4             :: SAVE2D  (NGP,NS2D)

    CALL get_monthly_array(iunit,SAVE2D)

    CALL get_vars_from_array(SAVE2D,&
         &  PSG,FPQ,TS,TSKIN,SOILW1,U0,V0,T0,CLOUDC,CLTOP,TSR,SSR,SLR,SSTAN)

    RETURN
  END SUBROUTINE get_monthly_vars

  subroutine save_monthly_means_array_8b(iunit,filename,SAVE2D_8b)
    INTEGER     ,INTENT(IN):: iunit
    CHARACTER(*),INTENT(IN):: filename
    REAL*8      ,INTENT(IN):: SAVE2D_8b(NGP,NS2D)
    REAL*4                 :: SAVE2D_4b(NGP,NS2D)

    SAVE2D_4b = SAVE2D_8b

    OPEN(iunit,FILE=filename,FORM='unformatted',ACCESS='sequential')
    CALL put_monthly_array(iunit,SAVE2D_4b)
    CLOSE(iunit)
  end subroutine save_monthly_means_array_8b

  subroutine save_monthly_means_array(iunit,filename,SAVE2D)
    INTEGER     ,INTENT(IN):: iunit
    CHARACTER(*),INTENT(IN):: filename
    REAL*4      ,INTENT(IN):: SAVE2D(NGP,NS2D)

    OPEN(iunit,FILE=filename,FORM='unformatted',ACCESS='sequential')
    CALL put_monthly_array(iunit,SAVE2D)
    CLOSE(iunit)
  end subroutine save_monthly_means_array

  subroutine load_monthly_means_array(iunit,filename,SAVE2D)
    INTEGER     ,INTENT(IN) :: iunit
    CHARACTER(*),INTENT(IN) :: filename
    REAL*4      ,INTENT(OUT):: SAVE2D(NGP,NS2D)

    OPEN(iunit,FILE=filename,FORM='unformatted',ACCESS='sequential')
    CALL get_monthly_array(iunit,SAVE2D)
    CLOSE(iunit)
  end subroutine load_monthly_means_array

  subroutine load_monthly_means_vars(iunit,filename,&
       &  PSG,FPQ,TS,TSKIN,SOILW1,U0,V0,T0,CLOUDC,CLTOP,TSR,SSR,SLR,SSTAN)
    INTEGER     ,INTENT(IN) :: iunit
    CHARACTER(*),INTENT(IN) :: filename
    real*8,INTENT(OUT),dimension(NLON,NLAT):: &
         &  PSG,FPQ,TS,TSKIN,SOILW1,U0,V0,T0,CLOUDC,CLTOP,TSR,SSR,SLR,SSTAN
    REAL*4                  :: SAVE2D  (NGP,NS2D)!, SAVE3D(NGP,NLEV,NS3D)

    CALL load_monthly_means_array(iunit,filename,SAVE2D)
    CALL get_vars_from_array(SAVE2D,&
         &  PSG,FPQ,TS,TSKIN,SOILW1,U0,V0,T0,CLOUDC,CLTOP,TSR,SSR,SLR,SSTAN)
  end subroutine load_monthly_means_vars

  !   subroutine save_monthly_means(iunit,filename,nmonths,SAVE2D)
  !     INTEGER     ,INTENT(IN):: iunit
  !     CHARACTER(*),INTENT(IN):: filename
  !     INTEGER     ,INTENT(IN):: nmonths
  !     REAL*4      ,INTENT(IN):: SAVE2D(NGP,NS2D,nmonths)!, SAVE3D(NGP,NLEV,NS3D)
  !     INTEGER                :: j
  !
  !     OPEN(iunit,FILE=filename,FORM='unformatted',ACCESS='sequential')
  !     do j=1,nmonths
  !       CALL put_monthly_array(iunit,SAVE2D(:,:,j))
  !     enddo
  !     CLOSE(iunit)
  !   end subroutine

  SUBROUTINE write_monthly_mean(iunit,PSG,FPQ,TS,&
       &               TSKIN,SOILW1,U0,V0,T0,CLOUDC,CLTOP,TSR,SSR,SLR,SSTAN)
    INTEGER ,INTENT(IN)     :: iunit
    real*8  ,INTENT(IN)       &
         & ,dimension(NLON,NLAT) :: PSG,FPQ ,TS,TSKIN,SOILW1,U0,V0,T0, &
         &                          CLOUDC,CLTOP,TSR,SSR,SLR,SSTAN
    REAL*4                  :: SAVE2D  (NGP,NS2D)!, SAVE3D(NGP,NLEV,NS3D)

    CALL put_vars_into_array(SAVE2D,&
         &  PSG,FPQ,TS,TSKIN,SOILW1,U0,V0,T0,CLOUDC,CLTOP,TSR,SSR,SLR,SSTAN)

    CALL put_monthly_array(iunit,SAVE2D)

    RETURN
  END SUBROUTINE write_monthly_mean


  FUNCTION time_averaged_obs(monthly_means_file,cycle_length,obs_id)
    CHARACTER(*)               ,INTENT(IN)  :: monthly_means_file,obs_id
    INTEGER                    ,INTENT(IN)  :: cycle_length
    real*8,dimension(NLON,NLAT)             :: time_averaged_obs
    real*8,dimension(NLON,NLAT,cycle_length):: MONTHLY_OBS
    real*8,dimension(NLON,NLAT,cycle_length):: PSG,FPQ ,TS,TSKIN,SOILW1,U0,V0,T0
    real*8,dimension(NLON,NLAT,cycle_length):: CLOUDC,CLTOP,TSR,SSR,SLR,SSTAN

    real*8,dimension(NLON,NLAT):: PSGmin,FPQmin,TSmin,TSKINmin,SOILW1min,U0min,&
         &        V0min,T0min,CLOUDCmin,CLTOPmin,TSRmin,SSRmin,SLRmin,SSTANmin
    real*8,dimension(NLON,NLAT):: PSGmax,FPQmax,TSmax,TSKINmax,SOILW1max,U0max,&
         &        V0max,T0max,CLOUDCmax,CLTOPmax,TSRmax,SSRmax,SLRmax,SSTANmax
    real*8,dimension(NLON,NLAT):: PSGmean,FPQmean,TSmean,TSKINmean,SOILW1mean,U0mean,&
         &        V0mean,T0mean,CLOUDCmean,CLTOPmean,TSRmean,SSRmean,SLRmean,SSTANmean
    real*8,dimension(NLON,NLAT):: PSGstdd,FPQstdd,TSstdd,TSKINstdd,SOILW1stdd,U0stdd,&
         &        V0stdd,T0stdd,CLOUDCstdd,CLTOPstdd,TSRstdd,SSRstdd,SLRstdd,SSTANstdd

    ! real*8,dimension(NLON,NLAT,cycle_length):: threshold_up_T,threshold_down_T
    ! real*8,dimension(NLON,NLAT,cycle_length):: threshold_up_M,threshold_down_M
    integer                                 :: unit_in_1 = 1789, j

    OPEN(unit_in_1,FILE=monthly_means_file,FORM='unformatted',ACCESS='sequential')
    do j=1,cycle_length
       CALL get_monthly_vars(unit_in_1,PSG(:,:,j),FPQ(:,:,j),TS(:,:,j),&
            &     TSKIN(:,:,j),SOILW1(:,:,j),U0(:,:,j),V0(:,:,j),T0(:,:,j),CLOUDC(:,:,j),&
            &     CLTOP(:,:,j),TSR(:,:,j),SSR(:,:,j),SLR(:,:,j),SSTAN(:,:,j))
    end do
    CLOSE(unit_in_1)

    CALL load_monthly_means_vars(unit_in_1,'nature_monthly_means__min.rst', &
         & PSGmin,FPQmin,TSmin,TSKINmin,SOILW1min,U0min,V0min,T0min,&
         & CLOUDCmin,CLTOPmin,TSRmin,SSRmin,SLRmin,SSTANmin)
    CALL load_monthly_means_vars(unit_in_1,'nature_monthly_means__max.rst', &
         & PSGmax,FPQmax,TSmax,TSKINmax,SOILW1max,U0max,V0max,T0max,&
         & CLOUDCmax,CLTOPmax,TSRmax,SSRmax,SLRmax,SSTANmax)
    CALL load_monthly_means_vars(unit_in_1,'nature_monthly_means__mean.rst', &
         & PSGmean,FPQmean,TSmean,TSKINmean,SOILW1mean,U0mean,V0mean,T0mean,&
         & CLOUDCmean,CLTOPmean,TSRmean,SSRmean,SLRmean,SSTANmean)
    CALL load_monthly_means_vars(unit_in_1,'nature_monthly_means__stdd.rst', &
         & PSGstdd,FPQstdd,TSstdd,TSKINstdd,SOILW1stdd,U0stdd,V0stdd,T0stdd,&
         & CLOUDCstdd,CLTOPstdd,TSRstdd,SSRstdd,SLRstdd,SSTANstdd)

    ! threshold_down_T = SPREAD(T0min,3,cycle_length)
    ! threshold_up_T   = SPREAD(T0max,3,cycle_length)
    ! ! threshold_down_T = T0min;     threshold_up_T   = T0max;
    ! ! threshold_down_T = 250.0; threshold_up_T   = 330.0;

    ! threshold_down_M = SPREAD(SOILW1min,3,cycle_length)
    ! threshold_up_M   = SPREAD(SOILW1max,3,cycle_length)
    ! ! threshold_down_M = SOILW1min; threshold_up_M   = SOILW1max;
    ! ! threshold_down_M =   0.1; threshold_up_M   =   0.9;

    SELECT CASE (obs_id)
    CASE('norm_T')
       MONTHLY_OBS = norm_function(T0    ,T0max,T0min)
       ! MONTHLY_OBS = norm_function(T0    ,threshold_up_T,threshold_down_T)
    CASE('norm_M')
       MONTHLY_OBS = norm_function(SOILW1,SOILW1max,SOILW1min)
    CASE('norm_add')
       MONTHLY_OBS = norm_function(T0    ,    T0max,    T0min) &
            &      + norm_function(SOILW1,SOILW1max,SOILW1min)
    CASE('norm_prod')
       MONTHLY_OBS = norm_function(T0    ,    T0max,    T0min) &
            &      * norm_function(SOILW1,SOILW1max,SOILW1min)
    CASE('norm_min')
       MONTHLY_OBS = MIN(norm_function(T0    ,    T0max,    T0min),&
            &            norm_function(SOILW1,SOILW1max,SOILW1min))
    
    CASE('norm_T_half')
		MONTHLY_OBS = norm_function(T0    ,T0max,T0min)

!     CASE('T')
!        MONTHLY_OBS = TS
!     CASE('norm_T')
!        MONTHLY_OBS = norm_function(TS    ,TSmax,TSmin)
!        ! MONTHLY_OBS = norm_function(TS    ,threshold_up_T,threshold_down_T)
!     CASE('norm_M')
!        MONTHLY_OBS = norm_function(SOILW1,SOILW1max,SOILW1min)
!     CASE('norm_add')
!        MONTHLY_OBS = norm_function(TS    ,    TSmax,    TSmin) &
!             &      + norm_function(SOILW1,SOILW1max,SOILW1min)
!     CASE('norm_prod')
!        MONTHLY_OBS = norm_function(TS    ,    TSmax,    TSmin) &
!             &      * norm_function(SOILW1,SOILW1max,SOILW1min)
!     CASE('norm_min')
!        MONTHLY_OBS = MIN(norm_function(TS    ,    TSmax,    TSmin),&
!             &            norm_function(SOILW1,SOILW1max,SOILW1min))

       ! CASE('ramp_T')
       !    MONTHLY_OBS = ramp_function(T0    ,threshold_up_T,threshold_down_T)
       ! CASE('ramp_M')
       !    MONTHLY_OBS = ramp_function(SOILW1,threshold_up_M,threshold_down_M)
       ! CASE('ramp_add')
       !    MONTHLY_OBS = ramp_function(T0    ,threshold_up_T,threshold_down_T) &
       !         &           + ramp_function(SOILW1,threshold_up_M,threshold_down_M)
       ! CASE('ramp_prod')
       !    MONTHLY_OBS = ramp_function(T0    ,threshold_up_T,threshold_down_T) &
       !         &           * ramp_function(SOILW1,threshold_up_M,threshold_down_M)
    CASE DEFAULT
       CALL ERROR('Unsupported obs_id '//obs_id)
    END SELECT
    !    call monitor_real_3D(MONTHLY_OBS,'MONTHLY_OBS')
    !    read*

    time_averaged_obs = SUM(MONTHLY_OBS,3)/cycle_length

  END FUNCTION time_averaged_obs

  ! FUNCTION write_detailed_station_time_series(monthly_means_file,cycle_length,obs_id)
  !   CHARACTER(*)               ,INTENT(IN)  :: monthly_means_file,obs_id
  !   INTEGER                    ,INTENT(IN)  :: cycle_length
  !   real*8,dimension(NLON,NLAT)             :: time_averaged_obs
  !   real*8,dimension(NLON,NLAT,cycle_length):: MONTHLY_OBS
  !   real*8,dimension(NLON,NLAT,cycle_length):: PSG,FPQ ,TS,TSKIN,SOILW1,U0,V0,T0
  !   real*8,dimension(NLON,NLAT,cycle_length):: CLOUDC,CLTOP,TSR,SSR,SLR,SSTAN

  !   real*8,dimension(NLON,NLAT):: PSGmin,FPQmin,TSmin,TSKINmin,SOILW1min,U0min,&
  !        &        V0min,T0min,CLOUDCmin,CLTOPmin,TSRmin,SSRmin,SLRmin,SSTANmin
  !   real*8,dimension(NLON,NLAT):: PSGmax,FPQmax,TSmax,TSKINmax,SOILW1max,U0max,&
  !        &        V0max,T0max,CLOUDCmax,CLTOPmax,TSRmax,SSRmax,SLRmax,SSTANmax
  !   real*8,dimension(NLON,NLAT):: PSGmean,FPQmean,TSmean,TSKINmean,SOILW1mean,U0mean,&
  !        &        V0mean,T0mean,CLOUDCmean,CLTOPmean,TSRmean,SSRmean,SLRmean,SSTANmean
  !   real*8,dimension(NLON,NLAT):: PSGstdd,FPQstdd,TSstdd,TSKINstdd,SOILW1stdd,U0stdd,&
  !        &        V0stdd,T0stdd,CLOUDCstdd,CLTOPstdd,TSRstdd,SSRstdd,SLRstdd,SSTANstdd

  !   ! real*8,dimension(NLON,NLAT,cycle_length):: threshold_up_T,threshold_down_T
  !   ! real*8,dimension(NLON,NLAT,cycle_length):: threshold_up_M,threshold_down_M
  !   integer                                 :: unit_in_1 = 1789, j

  !   OPEN(unit_in_1,FILE=monthly_means_file,FORM='unformatted',ACCESS='sequential')
  !   do j=1,cycle_length
  !      CALL get_monthly_vars(unit_in_1,PSG(:,:,j),FPQ(:,:,j),TS(:,:,j),&
  !           &     TSKIN(:,:,j),SOILW1(:,:,j),U0(:,:,j),V0(:,:,j),T0(:,:,j),CLOUDC(:,:,j),&
  !           &     CLTOP(:,:,j),TSR(:,:,j),SSR(:,:,j),SLR(:,:,j),SSTAN(:,:,j))
  !   end do
  !   CLOSE(unit_in_1)

  !   CALL load_monthly_means_vars(unit_in_1,'nature_monthly_means__min.rst', &
  !        & PSGmin,FPQmin,TSmin,TSKINmin,SOILW1min,U0min,V0min,T0min,&
  !        & CLOUDCmin,CLTOPmin,TSRmin,SSRmin,SLRmin,SSTANmin)
  !   CALL load_monthly_means_vars(unit_in_1,'nature_monthly_means__max.rst', &
  !        & PSGmax,FPQmax,TSmax,TSKINmax,SOILW1max,U0max,V0max,T0max,&
  !        & CLOUDCmax,CLTOPmax,TSRmax,SSRmax,SLRmax,SSTANmax)
  !   CALL load_monthly_means_vars(unit_in_1,'nature_monthly_means__mean.rst', &
  !        & PSGmean,FPQmean,TSmean,TSKINmean,SOILW1mean,U0mean,V0mean,T0mean,&
  !        & CLOUDCmean,CLTOPmean,TSRmean,SSRmean,SLRmean,SSTANmean)
  !   CALL load_monthly_means_vars(unit_in_1,'nature_monthly_means__stdd.rst', &
  !        & PSGstdd,FPQstdd,TSstdd,TSKINstdd,SOILW1stdd,U0stdd,V0stdd,T0stdd,&
  !        & CLOUDCstdd,CLTOPstdd,TSRstdd,SSRstdd,SLRstdd,SSTANstdd)

  !   ! threshold_down_T = SPREAD(T0min,3,cycle_length)
  !   ! threshold_up_T   = SPREAD(T0max,3,cycle_length)
  !   ! ! threshold_down_T = T0min;     threshold_up_T   = T0max;
  !   ! ! threshold_down_T = 250.0; threshold_up_T   = 330.0;

  !   ! threshold_down_M = SPREAD(SOILW1min,3,cycle_length)
  !   ! threshold_up_M   = SPREAD(SOILW1max,3,cycle_length)
  !   ! ! threshold_down_M = SOILW1min; threshold_up_M   = SOILW1max;
  !   ! ! threshold_down_M =   0.1; threshold_up_M   =   0.9;

  !   SELECT CASE (obs_id)
  !   CASE('T')
  !      MONTHLY_OBS = T0
  !   CASE('norm_T')
  !      MONTHLY_OBS = norm_function(T0    ,T0max,T0min)
  !      ! MONTHLY_OBS = norm_function(T0    ,threshold_up_T,threshold_down_T)
  !   CASE('norm_M')
  !      MONTHLY_OBS = norm_function(SOILW1,SOILW1max,SOILW1min)
  !   CASE('norm_add')
  !      MONTHLY_OBS = norm_function(T0    ,    T0max,    T0min) &
  !           &      + norm_function(SOILW1,SOILW1max,SOILW1min)
  !   CASE('norm_prod')
  !      MONTHLY_OBS = norm_function(T0    ,    T0max,    T0min) &
  !           &      * norm_function(SOILW1,SOILW1max,SOILW1min)
  !   CASE('norm_min')
  !      MONTHLY_OBS = MIN(norm_function(T0    ,    T0max,    T0min),&
  !           &            norm_function(SOILW1,SOILW1max,SOILW1min))
  !      ! CASE('ramp_T')
  !      !    MONTHLY_OBS = ramp_function(T0    ,threshold_up_T,threshold_down_T)
  !      ! CASE('ramp_M')
  !      !    MONTHLY_OBS = ramp_function(SOILW1,threshold_up_M,threshold_down_M)
  !      ! CASE('ramp_add')
  !      !    MONTHLY_OBS = ramp_function(T0    ,threshold_up_T,threshold_down_T) &
  !      !         &           + ramp_function(SOILW1,threshold_up_M,threshold_down_M)
  !      ! CASE('ramp_prod')
  !      !    MONTHLY_OBS = ramp_function(T0    ,threshold_up_T,threshold_down_T) &
  !      !         &           * ramp_function(SOILW1,threshold_up_M,threshold_down_M)
  !   CASE DEFAULT
  !      CALL ERROR('Unsupported obs_id '//obs_id)
  !   END SELECT
  !   !    call monitor_real_3D(MONTHLY_OBS,'MONTHLY_OBS')
  !   !    read*

  !   time_averaged_obs = SUM(MONTHLY_OBS,3)/cycle_length

  ! END FUNCTION write_detailed_station_time_series

  FUNCTION norm_function(x,threshold_up,threshold_down)
    REAL(r_dble),INTENT(IN)::              x(:,:,:)
    REAL(r_dble)           ::  norm_function(SIZE(X,1),SIZE(X,2),SIZE(X,3))
    REAL(r_dble),INTENT(IN)::   threshold_up(SIZE(X,1),SIZE(X,2))
    REAL(r_dble),INTENT(IN):: threshold_down(SIZE(X,1),SIZE(X,2))
    REAL(r_dble)           :: range(SIZE(X,1),SIZE(X,2))
    INTEGER                :: i,j,k

    range = threshold_up - threshold_down

    DO i=1,SIZE(X,1)
       DO j=1,SIZE(X,2)
          IF(range(i,j).EQ.0.0) THEN
             DO k=1,SIZE(X,3)
                norm_function(i,j,k)= 1.0D99
             END DO
          ELSE
             DO k=1,SIZE(X,3)
                norm_function(i,j,k)= (x(i,j,k)-threshold_down(i,j))/range(i,j)
             END DO
          END IF
       END DO
    END DO

    ! CALL monitor_real_2D(range,'range')
    ! norm_function    = (x - SPREAD(threshold_down,3,SIZE(X,3))) / &
    !      &   SPREAD(threshold_up - threshold_down,3,SIZE(X,3))

  END FUNCTION norm_function

  ! FUNCTION norm_function(x,threshold_up,threshold_down)
  !   REAL(r_dble),INTENT(IN)::              x(:,:,:)
  !   REAL(r_dble)           ::  norm_function(SIZE(X,1),SIZE(X,2),SIZE(X,3))
  !   REAL(r_dble),INTENT(IN)::   threshold_up(SIZE(X,1),SIZE(X,2),SIZE(X,3))
  !   REAL(r_dble),INTENT(IN):: threshold_down(SIZE(X,1),SIZE(X,2),SIZE(X,3))
  !   REAL(r_dble)           :: range(SIZE(X,1),SIZE(X,2),SIZE(X,3))

  !   range = threshold_up - threshold_down
  !   ! CALL monitor_real_3D(range,'range')
  !   norm_function    = (x - threshold_down) / (threshold_up - threshold_down)

  ! END FUNCTION norm_function

  FUNCTION ramp_function(x,threshold_up,threshold_down)
    REAL(r_dble),INTENT(IN)::              x(:,:,:)
    REAL(r_dble)           ::  ramp_function(SIZE(X,1),SIZE(X,2),SIZE(X,3))
    REAL(r_dble),INTENT(IN)::   threshold_up(SIZE(X,1),SIZE(X,2),SIZE(X,3))
    REAL(r_dble),INTENT(IN):: threshold_down(SIZE(X,1),SIZE(X,2),SIZE(X,3))
    REAL(r_dble)           ::         x_norm(SIZE(X,1),SIZE(X,2),SIZE(X,3))
    REAL(r_dble)           ::          zeros(SIZE(X,1),SIZE(X,2),SIZE(X,3))
    REAL(r_dble)           ::           ones(SIZE(X,1),SIZE(X,2),SIZE(X,3))

    zeros = 0.0; ones = 1.0
    x_norm    = norm_function(x,threshold_up,threshold_down)
    !     x_anomaly    = (x - threshold_down) / (threshold_up - threshold_down)
    ramp_function = MIN( MAX(x_norm, zeros) ,ones)
    !    ramp_function = x_anomaly

  END FUNCTION ramp_function

  !  SUBROUTINE time_averaged_obs(monthly_means_file,nmonths,obs_id,TA_OBS)
  !!  SUBROUTINE time_averaged_obs(monthly_means_file,nmonths,obs_id,TA_OBS)
  !    CHARACTER(*)               ,INTENT(IN) :: monthly_means_file,obs_id
  !    INTEGER                    ,INTENT(IN) :: nmonths
  !    real*8,dimension(NLON,NLAT),INTENT(OUT):: TA_OBS
  !    real*8,dimension(NLON,NLAT,nmonths)    :: MONTHLY_OBS
  !    real*8,dimension(NLON,NLAT,nmonths)    :: PSG,FPQ ,TS,TSKIN,SOILW1,U0,V0,T0
  !    real*8,dimension(NLON,NLAT,nmonths)    :: CLOUDC,CLTOP,TSR,SSR,SLR,SSTAN
  !    real*8,dimension(NLON,NLAT,nmonths)    :: threshold_up_T,threshold_down_T
  !    real*8,dimension(NLON,NLAT,nmonths)    :: threshold_up_M,threshold_down_M

  !    CALL read_monthly_means(monthly_means_file,nmonths,PSG,FPQ ,TS,&
  !    &              TSKIN,SOILW1,U0,V0,T0,CLOUDC,CLTOP,TSR,SSR,SLR,SSTAN)

  !    threshold_down_T = 273.0; threshold_up_T   = 300.0;
  !    threshold_down_M =   0.1; threshold_up_M   =   0.9;

  !    SELECT CASE (obs_id)
  !    CASE('temp')
  !      MONTHLY_OBS = T0
  !    CASE('T_RESP')
  !      MONTHLY_OBS = ramp_function(T0    ,threshold_up_T,threshold_down_T)
  !    CASE('M_RESP')
  !      MONTHLY_OBS = ramp_function(SOILW1,threshold_up_M,threshold_down_M)
  !    CASE('RESP_ADD')
  !      MONTHLY_OBS = ramp_function(T0    ,threshold_up_T,threshold_down_T) &
  !      &           + ramp_function(SOILW1,threshold_up_M,threshold_down_M)
  !    CASE DEFAULT
  !      CALL ERROR('Unsupported obs_id')
  !    END SELECT

  !    TA_OBS = SUM(MONTHLY_OBS,3)/nmonths

  !    RETURN
  !  END SUBROUTINE time_averaged_obs

  !  SUBROUTINE time_averaged_obs(monthly_means_file,nmonths,obs_id,TA_OBS)

  !    CHARACTER(*), INTENT(IN)   :: monthly_means_file,obs_id
  !    INTEGER     , INTENT(IN)   :: nmonths
  !    real*8,dimension(NLON,NLAT):: TA_OBS,INST_OBS
  !    INTEGER     ,PARAMETER     :: NGP=NLON*NLAT
  !    INTEGER     ,PARAMETER     :: NS2D=17, NS2D_D=10, NS2D2=2, NS3D1=8 !,NS3D_D= 6
  !    INTEGER ,PARAMETER         :: NS3D2=6, NS3D3=5
  !    INTEGER ,PARAMETER         :: NS3D=NS3D1+NS3D2+NS3D3
  !    REAL*4                     :: SAVE2D  (NGP,NS2D), SAVE3D(NGP,NLEV,NS3D)
  !    REAL*4                     :: SAVE2D_L(NGP,NS2D_D-NS2D2)!, FACT2D(NS2D)
  !    real*4                     :: r4out(ngp)
  !    real*8,dimension(NLON,NLAT):: PSG,FPQ ,TS,TSKIN,SOILW1,U0,V0,T0
  !    real*8,dimension(NLON,NLAT):: CLOUDC,CLTOP,TSR,SSR,SLR,SSTAN,dummy
  !    INTEGER                    :: j,k,n,n0,nm, iunit

  !    real*8,dimension(NLON,NLAT):: threshold_up,threshold_down

  !    WRITE(*,*)monthly_means_file

  !    iunit=11
  !    OPEN(iunit,FILE=monthly_means_file,FORM='unformatted',ACCESS='sequential')

  !    TA_OBS = 0.0
  !    DO nm=1,nmonths

  !      ! Import time-mean file including 3-d and 2-d fields

  !      do n=1,ns3d1
  !        do k=nlev,1,-1
  !          read(iunit) R4OUT
  !          do j=1,ngp
  !            SAVE3D(j,k,n) = R4OUT(j)
  !          enddo
  !        enddo
  !      enddo
  !    !
  !      do n=1,ns2d
  !        read (iunit) R4OUT
  !        do j=1,ngp
  !          SAVE2D(j,n) = R4OUT(j)
  !        enddo
  !      enddo
  !    !
  !      do n=1,ns2d_d-ns2d2
  !        read (iunit) R4OUT
  !        do j=1,ngp
  !          SAVE2D_L(j,n) = R4OUT(j)
  !        enddo
  !      enddo

  !      n0=1
  !      PSG    = RESHAPE(SAVE2D(:,n0), (/NLON,NLAT/)); n0 = n0 + 1
  !      FPQ    = RESHAPE(SAVE2D(:,n0), (/NLON,NLAT/)); n0 = n0 + 1
  !      TS     = RESHAPE(SAVE2D(:,n0), (/NLON,NLAT/)); n0 = n0 + 1
  !      TSKIN  = RESHAPE(SAVE2D(:,n0), (/NLON,NLAT/)); n0 = n0 + 1
  !      SOILW1 = RESHAPE(SAVE2D(:,n0), (/NLON,NLAT/)); n0 = n0 + 1
  !      U0     = RESHAPE(SAVE2D(:,n0), (/NLON,NLAT/)); n0 = n0 + 1
  !      V0     = RESHAPE(SAVE2D(:,n0), (/NLON,NLAT/)); n0 = n0 + 1
  !      T0     = RESHAPE(SAVE2D(:,n0), (/NLON,NLAT/)); n0 = n0 + 1
  !      dummy  = RESHAPE(SAVE2D(:,n0), (/NLON,NLAT/)); n0 = n0 + 1
  !      CLOUDC = RESHAPE(SAVE2D(:,n0), (/NLON,NLAT/)); n0 = n0 + 1
  !      CLTOP  = RESHAPE(SAVE2D(:,n0), (/NLON,NLAT/)); n0 = n0 + 1
  !      dummy  = RESHAPE(SAVE2D(:,n0), (/NLON,NLAT/)); n0 = n0 + 1
  !      TSR    = RESHAPE(SAVE2D(:,n0), (/NLON,NLAT/)); n0 = n0 + 1
  !      SSR    = RESHAPE(SAVE2D(:,n0), (/NLON,NLAT/)); n0 = n0 + 1
  !      SLR    = RESHAPE(SAVE2D(:,n0), (/NLON,NLAT/)); n0 = n0 + 1
  !      dummy  = RESHAPE(SAVE2D(:,n0), (/NLON,NLAT/)); n0 = n0 + 1
  !      SSTAN  = RESHAPE(SAVE2D(:,n0), (/NLON,NLAT/)); n0 = n0 + 1

  !      SELECT CASE (obs_id)
  !      CASE('temp')
  !        INST_OBS = T0
  !      CASE('trw_linear')
  !        INST_OBS = T0 + SOILW1
  !      CASE('T_ramp')
  !        threshold_up   = 300.0
  !        threshold_down = 273.0
  !        INST_OBS = ramp_function(T0,threshold_up,threshold_down)
  !      CASE DEFAULT
  !        CALL ERROR('Unsupported obs_id')
  !      END SELECT

  !      TA_OBS = TA_OBS + INST_OBS

  !!    CALL MONITOR_ARRAY (T0,'T0')
  !!    CALL MONITOR_ARRAY (v3d(:,:,1,iv3d_t),'v3d(i,j,1,iv3d_t)')
  !!    CALL MONITOR_ARRAY (SOILW1,'SOILW1')
  !!    CALL MONITOR_ARRAY (TS,'TS')
  !!    T0 = T0 - v3d(:,:,1,iv3d_t)
  !!    CALL MONITOR_ARRAY (T0,'T0 - v3d(:,:,1,iv3d_t)')
  !!    call extract1F (SAVE2D_D,FPQ,    ngp,n0)
  !!    call extract1F (SAVE2D_D,T0

  !    enddo
  !    TA_OBS = TA_OBS / nmonths

  !    CLOSE(iunit)

  !    RETURN
  !  END SUBROUTINE time_averaged_obs


  !  FUNCTION ramp_function(x,threshold_up,threshold_down)
  !    REAL(r_dble),INTENT(IN) ::              x(NLON,NLAT)
  !    REAL(r_dble)            ::  ramp_function(NLON,NLAT)
  !    REAL(r_dble)            ::   threshold_up(NLON,NLAT)
  !    REAL(r_dble)            :: threshold_down(NLON,NLAT)
  !    REAL(r_dble)            ::      x_anomaly(NLON,NLAT)
  !    REAL(r_dble)            ::          zeros(NLON,NLAT)
  !    REAL(r_dble)            ::           ones(NLON,NLAT)

  !    zeros = 0.0; ones = 1.0
  !    x_anomaly    = (x - threshold_down) / (threshold_up - threshold_down)
  !   ramp_function = MIN( MAX(x_anomaly, zeros) ,ones)
  !  END FUNCTION ramp_function

  !  FUNCTION vsl_response(nx,x,threshold_up,threshold_down)
  !    INTEGER     ,INTENT(IN) :: nx
  !    REAL(r_dble),INTENT(IN) :: x(nx)
  !    REAL(r_dble)            :: vsl_response(nx)
  !    REAL(r_dble)            :: threshold_up(nx)
  !    REAL(r_dble)            :: threshold_down(nx)
  !    REAL(r_dble)            :: x_anomaly(nx)
  !    REAL(r_dble)            :: zeros(nx), ones(nx)

  !    zeros = 0.0; ones = 1.0
  !    x_anomaly = (x - threshold_down) / (threshold_up - threshold_down)
  !!     IF(sat_level.EQ.0.0) THEN
  !!        vsl_response = x_anomaly
  !!     ELSE
  !       vsl_response = MIN( MAX(x_anomaly, zeros) ,ones)
  !!     END IF
  !  END FUNCTION vsl_response

  !        SUBROUTINE extract1F (FSAVE,Fext,NGP,NF)

  !! C *** Take one field to storage array
  !      IMPLICIT NONE
  !      INTEGER,INTENT(INOUT) :: NF
  !      INTEGER,INTENT(IN)    :: NGP
  !      REAL   ,INTENT(IN)    :: FSAVE(NGP,*)
  !      REAL   ,INTENT(OUT)   :: Fext(NGP)
  !      INTEGER               :: J

  !      NF=NF+1

  !      DO J=1,NGP
  !        Fext(J) = FSAVE(J,NF)
  !      ENDDO

  !      RETURN
  !      END SUBROUTINE extract1F


END MODULE common_obs_speedy
