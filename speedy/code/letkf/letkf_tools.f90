MODULE letkf_tools
  !=======================================================================
  !
  ! [PURPOSE:] Module for LETKF with SPEEDY
  !
  ! [HISTORY:]
  !   01/26/2009 Takemasa Miyoshi  created
  !   29/07/2014 Walter Acevedo    modified
  !	  15/03/2016 Bijan Fallah      modified
  !=======================================================================
  USE common
  USE common_tools
  USE common_mpi       , ONLY: myrank
  USE common_speedy
  USE common_mpi_speedy, ONLY: scatter_grd_mpi,gather_grd_mpi,nij1
  USE common_letkf     , ONLY: letkf_core
  USE letkf_obs        , ONLY: nid_obs,nbv,nobs,nobsgrd,&
       & id_u_obs,id_v_obs,id_t_obs,id_q_obs,id_rh_obs,id_ps_obs,id_rain_obs,&
       & lon1,lat1,obslon,obslat,dist_zero,dist_zerov,dlon_zero,dlat_zero,&
       & sigma_obsv,sigma_obs, obsdep,obslev,obsdat,obshdxf,obserr,obselm

  IMPLICIT NONE

  PRIVATE
  PUBLIC ::  das_letkf,cov_infl_mul,sp_infl_add

  !                         multiplicative inflation
  REAL(r_size),PROTECTED :: cov_infl_mul = 1.00d0 
  !                           > 0: globally constant covariance inflation
  !                           < 0: 3D inflation values input from a GPV
  !                              file "infl_mul.grd"
  !                         additive inflation
  REAL(r_size),PARAMETER :: sp_infl_add = 0.d0

  INTEGER     ,SAVE      :: nobstotal
  !TVS  LOGICAL,PARAMETER :: msw_vbc = .FALSE.
  INTEGER     ,SAVE      :: var_local_n2n(nv3d+nv2d)
  REAL(r_size),PARAMETER :: var_local    (nv3d+nv2d,nid_obs) = &
       !               U      V      T      Q     PS   RAIN
       & RESHAPE((/ 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0,  & ! U
       &            1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0,  & ! V
       &            1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0,  & ! T
       &            1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0,  & ! Q
       &            1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0,  & ! RH
       &            1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0,  & ! PS
       &            1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0 /)& ! RAIN
       & ,(/nv3d+nv2d,nid_obs/))

CONTAINS
  !-----------------------------------------------------------------------
  ! Data Assimilation
  !-----------------------------------------------------------------------
  SUBROUTINE das_letkf(gues3d,gues2d,anal3d,anal2d)
    IMPLICIT NONE
    CHARACTER(12)             :: inflfile = 'infl_mul.grd'
    REAL(r_size),INTENT(INOUT):: gues3d (nij1,nlev, nbv,nv3d) ! background ensemble
    REAL(r_size),INTENT(INOUT):: gues2d (nij1, nbv,nv2d)      !  output: destroyed
    REAL(r_size),INTENT(OUT)  :: anal3d (nij1,nlev, nbv,nv3d) ! analysis ensemble
    REAL(r_size),INTENT(OUT)  :: anal2d (nij1, nbv,nv2d)
    REAL(r_size),ALLOCATABLE  :: mean3d (:,:,:)
    REAL(r_size),ALLOCATABLE  :: mean2d (:,:)
    REAL(r_size),ALLOCATABLE  :: hdxf   (:,:)
    REAL(r_size),ALLOCATABLE  :: rdiag  (:)
    REAL(r_size),ALLOCATABLE  :: rloc   (:)
    REAL(r_size),ALLOCATABLE  :: dep    (:)
    REAL(r_size),ALLOCATABLE  :: work3d (:,:,:)
    REAL(r_size),ALLOCATABLE  :: work2d (:,:)
    REAL(r_sngl),ALLOCATABLE  :: work3dg(:,:,:,:)
    REAL(r_sngl),ALLOCATABLE  :: work2dg(:,:,:)
    REAL(r_size),ALLOCATABLE  :: logpfm (:,:)
    REAL(r_size)              :: parm
    REAL(r_size)              :: trans  (nbv,nbv,nv3d+nv2d)
    LOGICAL                   :: ex
    INTEGER                   :: ij,ilev,n,m,i,k,nobsl!,ierr,j
    LOGICAL                   :: verb = .true.

    if(verb) call print_msg('Hello from das_letkf')

    nobstotal = nobs !+ ntvs
    WRITE(6,'(A,I8)') 'Target observation numbers : NOBS=',nobs!,', NTVS=',ntvs
    !
    ! In case of no obs
    !
    IF(nobstotal == 0) THEN
       WRITE(6,'(A)') 'No observation assimilated'
       anal3d = gues3d
       anal2d = gues2d
       RETURN
    END IF

    !-------------------------------------------------------
    if(verb) call print_msg(' Variable localization')
    !-------------------------------------------------------
    var_local_n2n(1) = 1
    DO n=2,nv3d+nv2d
       DO i=1,n
          var_local_n2n(n) = i
          IF(MAXVAL(ABS(var_local(i,:)-var_local(n,:))) < TINY(var_local)) EXIT
       END DO
    END DO

    !-------------------------------------------------------
    if(verb) call print_msg(' FCST PERTURBATIONS')
    !-------------------------------------------------------
    ALLOCATE(mean3d(nij1,nlev,nv3d))
    ALLOCATE(mean2d(nij1,nv2d))
    CALL ensmean_grd(nbv,nij1,gues3d,gues2d,mean3d,mean2d)
    DO n=1,nv3d
       DO m=1,nbv
          DO k=1,nlev
             DO i=1,nij1
                gues3d(i,k,m,n) = gues3d(i,k,m,n) - mean3d(i,k,n)
             END DO
          END DO
       END DO
    END DO
    DO n=1,nv2d
       DO m=1,nbv
          DO i=1,nij1
             gues2d(i,m,n) = gues2d(i,m,n) - mean2d(i,n)
          END DO
       END DO
    END DO

    !-------------------------------------------------------
    if(verb) call print_msg(' multiplicative inflation')
    !-------------------------------------------------------
    IF(cov_infl_mul > 0.0d0) THEN ! fixed multiplicative inflation parameter
       ALLOCATE( work3d(nij1,nlev,nv3d) )
       ALLOCATE( work2d(nij1,nv2d) )
       work3d = cov_infl_mul
       work2d = cov_infl_mul
       work3d(:,nlev,:) = 1.01d0
    END IF

    IF(cov_infl_mul <= 0.0d0) THEN ! 3D parameter values are read-in
       ALLOCATE( work3dg(nlon,nlat,nlev,nv3d) )
       ALLOCATE( work2dg(nlon,nlat,nv2d) )
       ALLOCATE( work3d(nij1,nlev,nv3d) )
       ALLOCATE( work2d(nij1,nv2d) )
       INQUIRE(FILE=inflfile,EXIST=ex)
       IF(ex) THEN
          IF(myrank == 0) THEN
             WRITE(6,'(A,I3.3,2A)') 'MYRANK ',myrank,' is reading.. ',inflfile
             CALL read_grd4(inflfile,work3dg,work2dg)
          END IF
          CALL scatter_grd_mpi(0,work3dg,work2dg,work3d,work2d)
       ELSE
          WRITE(6,'(2A)') '!!WARNING: no such file exist: ',inflfile
          work3d = -1.0d0 * cov_infl_mul
          work2d = -1.0d0 * cov_infl_mul
       END IF
    END IF

    !-------------------------------------------------------
    if(verb) call print_msg('p_full for background ensemble mean')
    !-------------------------------------------------------
    ALLOCATE(logpfm(nij1,nlev))
    CALL calc_pfull(nij1,1,mean2d(:,iv2d_ps),logpfm)
    logpfm = DLOG(logpfm)

    !-------------------------------------------------------
    if(verb) call print_msg(' MAIN ASSIMILATION LOOP')
    !-------------------------------------------------------
    ALLOCATE( hdxf(1:nobstotal,1:nbv),rdiag(1:nobstotal),rloc(1:nobstotal),dep(1:nobstotal) )
    DO ilev=1,nlev
       !     WRITE(6,'(A,I3)') 'ilev = ',ilev
       DO ij=1,nij1
          !-------------------------------------------------------
          if(verb) call print_msg('update 3d variables')
          !-------------------------------------------------------
          DO n=1,nv3d
             IF(var_local_n2n(n) < n) THEN
                trans(:,:,n) = trans(:,:,var_local_n2n(n))
                work3d(ij,ilev,n) = work3d(ij,ilev,var_local_n2n(n))
             ELSE
                !-------------------------------------------------------
                if(verb) call print_msg(' obs_local')
                !-------------------------------------------------------
                CALL obs_local(ij,ilev,n,hdxf,rdiag,rloc,dep,nobsl,logpfm)
                parm = work3d(ij,ilev,n)

                !          if(verb) call monitor_intg_0D(nobstotal,'nobstotal')
                !          if(verb) call monitor_intg_0D(nobsl,'nobsl')
                !          if(verb) call monitor_real_2D( hdxf, 'hdxf')
                !          if(verb) call monitor_real_1D(rdiag,'rdiag')
                !          if(verb) call monitor_real_1D(rdiag, 'rloc')
                !          if(verb) call monitor_real_1D(  dep,  'dep')
                !          if(verb) call monitor_real_0D( parm, 'parm')

                !-------------------------------------------------------
                if(verb) call print_msg(' letkf_core')
                !-------------------------------------------------------
                CALL letkf_core(nobstotal,nobsl,hdxf,rdiag,rloc,dep,parm,trans(:,:,n))
                work3d(ij,ilev,n) = parm
             END IF
             DO m=1,nbv
                anal3d(ij,ilev,m,n) = mean3d(ij,ilev,n)
                DO k=1,nbv
                   anal3d(ij,ilev,m,n) = anal3d(ij,ilev,m,n) &
                        & + gues3d(ij,ilev,k,n) * trans(k,m,n)
                END DO
             END DO
          END DO

          IF(ilev >= 5) THEN !no analysis for upper-level Q
             DO m=1,nbv
                anal3d(ij,ilev,m,iv3d_q) = mean3d(ij,ilev,iv3d_q) &
                     & + gues3d(ij,ilev,m,iv3d_q)
             END DO
          END IF

          !-------------------------------------------------------
          if(verb) call print_msg('update 2d variables at ilev=1')
          !-------------------------------------------------------
          IF(ilev == 1) THEN

             DO n=1,nv2d

                IF(var_local_n2n(nv3d+n) <= nv3d) THEN
                   trans(:,:,nv3d+n) = trans(:,:,var_local_n2n(nv3d+n))
                   work2d(ij,n) = work2d(ij,var_local_n2n(nv3d+n))
                ELSE IF(var_local_n2n(nv3d+n) < nv3d+n) THEN
                   trans(:,:,nv3d+n) = trans(:,:,var_local_n2n(nv3d+n))
                   work2d(ij,n) = work2d(ij,var_local_n2n(nv3d+n)-nv3d)
                ELSE
                   CALL obs_local(ij,ilev,nv3d+n,hdxf,rdiag,rloc,dep,nobsl,logpfm)
                   parm = work2d(ij,n)
                   if(verb) call print_msg(' letkf_core')
                   CALL letkf_core(nobstotal,nobsl,hdxf,rdiag,rloc,dep,parm,trans(:,:,nv3d+n))
                   work2d(ij,n) = parm
                END IF

                DO m=1,nbv
                   anal2d(ij,m,n)  = mean2d(ij,n)
                   DO k=1,nbv
                      anal2d(ij,m,n) = anal2d(ij,m,n) + gues2d(ij,k,n) * trans(k,m,nv3d+n)
                   END DO
                END DO

             END DO

          END IF

       END DO
    END DO

    DEALLOCATE(hdxf,rdiag,rloc,dep)

    IF(cov_infl_mul < 0.0d0) THEN
       CALL gather_grd_mpi(0,work3d,work2d,work3dg,work2dg)
       IF(myrank == 0) THEN
          WRITE(6,'(A,I3.3,2A)') 'MYRANK ',myrank,' is writing.. ',inflfile
          CALL write_grd4(inflfile,work3dg,work2dg)
       END IF
       DEALLOCATE(work3dg,work2dg,work3d,work2d)
    END IF

    !-------------------------------------------------------
    if(verb) call print_msg('Additive inflation')
    !-------------------------------------------------------
    IF(sp_infl_add > 0.0d0) THEN
       CALL read_ens_mpi('addi',nbv,gues3d,gues2d)
       ALLOCATE( work3d(nij1,nlev,nv3d) )
       ALLOCATE( work2d(nij1,nv2d) )
       CALL ensmean_grd(nbv,nij1,gues3d,gues2d,work3d,work2d)
       DO n=1,nv3d
          DO m=1,nbv
             DO k=1,nlev
                DO i=1,nij1
                   gues3d(i,k,m,n) = gues3d(i,k,m,n) - work3d(i,k,n)
                END DO
             END DO
          END DO
       END DO
       DO n=1,nv2d
          DO m=1,nbv
             DO i=1,nij1
                gues2d(i,m,n) = gues2d(i,m,n) - work2d(i,n)
             END DO
          END DO
       END DO

       DEALLOCATE(work3d,work2d)
       WRITE(6,'(A)') '===== Additive covariance inflation ====='
       WRITE(6,'(A,F10.4)') '  parameter:',sp_infl_add
       WRITE(6,'(A)') '========================================='
       !    parm = 0.7d0
       !    DO ilev=1,nlev
       !      parm_infl_damp(ilev) = 1.0d0 + parm &
       !        & + parm * REAL(1-ilev,r_size)/REAL(nlev_dampinfl,r_size)
       !      parm_infl_damp(ilev) = MAX(parm_infl_damp(ilev),1.0d0)
       !    END DO
       DO n=1,nv3d
          DO m=1,nbv
             DO ilev=1,nlev
                DO ij=1,nij1
                   anal3d(ij,ilev,m,n) = anal3d(ij,ilev,m,n) &
                        & + gues3d(ij,ilev,m,n) * sp_infl_add
                END DO
             END DO
          END DO
       END DO
       DO n=1,nv2d
          DO m=1,nbv
             DO ij=1,nij1
                anal2d(ij,m,n) = anal2d(ij,m,n) + gues2d(ij,m,n) * sp_infl_add
             END DO
          END DO
       END DO
    END IF

    DEALLOCATE(logpfm,mean3d,mean2d)
    RETURN
  END SUBROUTINE das_letkf
  !-----------------------------------------------------------------------
  ! Project global observations to local
  !     (hdxf_g,dep_g,rdiag_g) -> (hdxf,dep,rdiag)
  !-----------------------------------------------------------------------
  SUBROUTINE obs_local(ij,ilev,nvar,hdxf,rdiag,rloc,dep,nobsl,logpfm)
    IMPLICIT NONE
    INTEGER     ,INTENT(IN)  :: ij,ilev,nvar
    REAL(r_size),INTENT(IN)  :: logpfm(nij1,nlev)
    REAL(r_size),INTENT(OUT) :: hdxf (nobstotal,nbv)
    REAL(r_size),INTENT(OUT) :: rdiag(nobstotal)
    REAL(r_size),INTENT(OUT) :: rloc (nobstotal)
    REAL(r_size),INTENT(OUT) :: dep  (nobstotal)
    INTEGER     ,INTENT(OUT) :: nobsl
    REAL(r_size)             :: minlon,maxlon,minlat,maxlat,dist,dlev
    REAL(r_size)             :: tmplon,tmplat,tmperr!,tmpwgt(nlev)
    !   INTEGER                  :: tmpqc
    INTEGER,ALLOCATABLE      :: nobs_use(:)
    INTEGER                  :: imin,imax,jmin,jmax!,im,ichan
    INTEGER                  :: n,nn,tvnn,iobs
    LOGICAL                  :: verb = .false.

    if(verb) call print_msg(' INITIALIZE obs_local')
    if(verb) call monitor_intg_0D(nobs,'nobs')

    IF( nobs > 0 ) THEN
       ALLOCATE(nobs_use(nobs))
    END IF

    if(verb) call print_msg('data search')

    minlon = lon1(ij) - dlon_zero(ij)
    maxlon = lon1(ij) + dlon_zero(ij)
    minlat = lat1(ij) - dlat_zero
    maxlat = lat1(ij) + dlat_zero

    IF(maxlon - minlon >= 360.0d0) THEN
       minlon = 0.0d0; maxlon = 360.0d0
    END IF

    !   if(verb) call monitor_real_0D(minlon,'minlon')
    !   if(verb) call monitor_real_0D(maxlon,'maxlon')
    !   if(verb) call monitor_real_0D(minlat,'minlat')
    !   if(verb) call monitor_real_0D(maxlat,'maxlat')

    DO jmin=1,nlat-2
       IF(minlat < lat(jmin+1)) EXIT
    END DO
    DO jmax=1,nlat-2
       IF(maxlat < lat(jmax+1)) EXIT
    END DO

    nn = 1
    IF(minlon >= 0 .AND. maxlon <= 360.0) THEN

       DO imin=1,nlon-1
          IF(minlon < lon(imin+1)) EXIT
       END DO
       DO imax=1,nlon-1
          IF(maxlon < lon(imax+1)) EXIT
       END DO
       IF( nobs > 0 ) &
            & CALL obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)

       if(verb) call monitor_intg_0D(nn,'nn 1')

    ELSE IF(minlon >= 0 .AND. maxlon > 360.0) THEN

       DO imin=1,nlon-1
          IF(minlon < lon(imin+1)) EXIT
       END DO

       maxlon = maxlon - 360.0d0
       IF(maxlon > 360.0d0) THEN

          imin = 1; imax = nlon
          IF( nobs > 0 ) &
               & CALL obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)
          if(verb) call monitor_intg_0D(nn,'nn 2')

       ELSE

          DO imax=1,nlon-1
             IF(maxlon < lon(imax+1)) EXIT
          END DO

          IF(imax > imin) THEN

             imin = 1; imax = nlon
             IF( nobs > 0 ) &
                  & CALL obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)
             if(verb) call monitor_intg_0D(nn,'nn 3')

          ELSE

             imin = 1
             IF( nobs > 0 ) &
                  & CALL obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)
             DO imin=1,nlon-1
                IF(minlon < lon(imin+1)) EXIT
             END DO
             imax = nlon
             IF( nobs > 0 ) &
                  & CALL obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)

          END IF
       END IF

    ELSE IF(minlon < 0 .AND. maxlon <= 360.0d0) THEN

       DO imax=1,nlon-1
          IF(maxlon < lon(imax+1)) EXIT
       END DO
       minlon = minlon + 360.0d0
       IF(minlon < 0) THEN
          imin = 1
          imax = nlon
          IF( nobs > 0 ) &
               & CALL obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)

       ELSE
          DO imin=1,nlon-1
             IF(minlon < lon(imin+1)) EXIT
          END DO

          IF(imin < imax) THEN
             imin = 1; imax = nlon
             IF( nobs > 0 ) &
                  & CALL obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)

          ELSE
             imin = 1
             IF( nobs > 0 ) &
                  & CALL obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)

             DO imin=1,nlon-1
                IF(minlon < lon(imin+1)) EXIT
             END DO
             imax = nlon
             IF( nobs > 0 ) &
                  & CALL obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)

          END IF
       END IF
    ELSE

       maxlon = maxlon - 360.0d0
       minlon = minlon + 360.0d0

       IF(maxlon > 360.0 .OR. minlon < 0) THEN

          imin = 1; imax = nlon
          IF( nobs > 0 ) &
               & CALL obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)

       ELSE

          DO imin=1,nlon-1
             IF(minlon < lon(imin+1)) EXIT
          END DO
          DO imax=1,nlon-1
             IF(maxlon < lon(imax+1)) EXIT
          END DO

          IF(imin > imax) THEN

             imin = 1; imax = nlon
             IF( nobs > 0 ) &
                  & CALL obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)

          ELSE

             IF( nobs > 0 ) &
                  & CALL obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)

          END IF
       END IF
    END IF
    nn = nn-1

    IF(nn < 1) THEN
       if(verb) call print_msg(' wayout')
       nobsl = 0
       RETURN
    END IF

    if(verb) call print_msg(' CONVENTIONAL obs_local')

    !          if(verb) call monitor_intg_0D(nobstotal,'nobstotal')
    if(verb) call monitor_intg_0D(nn,'nn')
    !          if(verb) call monitor_real_2D( hdxf, 'hdxf')
    !          if(verb) call monitor_real_1D(rdiag,'rdiag')
    !          if(verb) call monitor_real_1D(rdiag, 'rloc')
    !          if(verb) call monitor_real_1D(  dep,  'dep')
    !          if(verb) call monitor_real_0D( parm, 'parm')


    nobsl = 0
    IF(nn > 0) THEN
       DO n=1,nn

          if(verb) call print_msg(' vertical localization')

          IF(NINT(obselm(nobs_use(n))) == id_ps_obs .AND. ilev > 1) THEN
             !       print*,'1n'
             dlev = ABS(LOG(obsdat(nobs_use(n))) - logpfm(ij,ilev))
          ELSE IF(NINT(obselm(nobs_use(n))) /= id_ps_obs) THEN
             !       print*,'2n'
             !       print*,'obslev',obslev(nobs_use(n))
             dlev = ABS(LOG(obslev(nobs_use(n))) - logpfm(ij,ilev))
          ELSE
             !       print*,'3n'
             dlev = 0.0d0
          END IF
          IF(dlev > dist_zerov) CYCLE

          if(verb) call print_msg('horizontal localization')

          tmplon = obslon(nobs_use(n))
          tmplat = obslat(nobs_use(n))
          CALL com_distll_1( tmplon, tmplat,lon1(ij), lat1(ij), dist)

          IF(dist > dist_zero ) CYCLE


          if(verb) call print_msg('variable localization')

          SELECT CASE(NINT(obselm(nobs_use(n))))
          CASE(id_u_obs)
             iobs=1
          CASE(id_v_obs)
             iobs=2
          CASE(id_t_obs)
             iobs=3
          CASE(id_q_obs)
             iobs=4
          CASE(id_rh_obs)
             iobs=5
          CASE(id_ps_obs)
             iobs=6
          CASE(id_rain_obs)
             iobs=7
          CASE DEFAULT
             call error('Unknown obs_id')
          END SELECT

          IF(var_local(nvar,iobs) < TINY(var_local)) CYCLE

          nobsl = nobsl + 1
          hdxf(nobsl,:) = obshdxf(nobs_use(n),:)
          dep(nobsl)    = obsdep(nobs_use(n))

          if(verb) call print_msg('Observational localization')
          tmperr = obserr(nobs_use(n))
          rdiag(nobsl) = tmperr * tmperr
          rloc (nobsl) = EXP(-0.5d0 * ((dist/sigma_obs)**2 + &
               & (dlev/sigma_obsv)**2))  * var_local(nvar,iobs)

       END DO
    END IF

    ! DEBUG
    ! IF( ILEV == 1 .AND. ILON == 1 ) &
    ! & WRITE(6,*) 'ILEV,ILON,ILAT,NN,TVNN,NOBSL=',ilev,ij,nn,tvnn,nobsl
    !
    IF( nobsl > nobstotal ) THEN
       WRITE(6,'(A,I5,A,I5)') 'FATAL ERROR, NOBSL=',nobsl,' > NOBSTOTAL=',nobstotal
       WRITE(6,*) 'IJ,NN,TVNN=', ij, nn, tvnn
       STOP 99
    END IF

    IF( nobs > 0 ) THEN
       DEALLOCATE(nobs_use)
    END IF

    RETURN
  END SUBROUTINE obs_local

  SUBROUTINE obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)
    INTEGER,INTENT(IN)    :: imin, imax,jmin,jmax
    INTEGER,INTENT(INOUT) ::   nn, nobs_use(nobs)
    INTEGER               ::    j,n,ib,ie,ip
    LOGICAL               :: verb = .true.

    if(verb) call monitor_intg_0D(nn,'nn before')
    if(verb) call monitor_intg_1D(nobs_use,'nobs_use before')

    DO j=jmin,jmax

       IF(imin > 1) THEN
          ib = nobsgrd(imin-1,j)+1
       ELSE
          IF(j > 1) THEN
             ib = nobsgrd(nlon,j-1)+1
          ELSE
             ib = 1
          END IF
       END IF

       ie = nobsgrd(imax,j)
       n  = ie - ib + 1

       IF(n == 0) CYCLE

       DO ip=ib,ie
          IF(nn > nobs) WRITE(6,*) 'FATALERROR, NN > NOBS', NN, NOBS
          nobs_use(nn) = ip
          nn = nn + 1
       END DO
    END DO

    if(verb) call monitor_intg_0D(nn,'nn after')
    if(verb) call monitor_intg_1D(nobs_use,'nobs_use after')

    RETURN
  END SUBROUTINE obs_local_sub

END MODULE letkf_tools
