MODULE ta_filter
  USE common_tools
  !  USE common_letkf        , ONLY: letkf_core
  USE model_core          , ONLY: nx,ny,nc,station_pos,station_comp,&
       &                          var_pos,var_comp,spatially_extended
  USE trajectory          , ONLY: Taver_steps
  USE observation_operator, ONLY: obs_error_stdd
  USE ensemble_tools      , ONLY: nbv,inflate
  IMPLICIT NONE

  CHARACTER(5),PROTECTED  :: update_mode       ! 'Hakim','Augm1'
  CHARACTER(5),PROTECTED  :: filter            ! 'enkf'| (letkf currently disabled)
  CHARACTER(5),PROTECTED  :: comp_localization ! 'yes' | 'no'
  INTEGER                 :: naugm
  REAL(r_dble),ALLOCATABLE:: infl_factor(:)
  REAL(r_dble)            :: infl_enkf, infl_step
  CHARACTER(5)            :: infl_cycle_length_scaling
  REAL(r_dble)            :: loc_radius
  REAL(r_dble)            :: loc_yy(ny,ny),    loc_yy_comp(ny,ny) 
  REAL(r_dble),ALLOCATABLE:: loc_xy(: , :),    loc_xy_comp(:,:)
  INTEGER     ,ALLOCATABLE:: augm_var_pos(:),augm_var_comp(:)
  CHARACTER(5)            :: infl_mode         ! inflation mode ('fixed' or 'adapt')

  ! unused letkf vars
  ! REAL(r_dble) :: xlocal   = 3.0d0  ! localization scale
  ! CHARACTER(5) :: local_mode        ! localization mode ('fixed','adapt','both')
  ! REAL(r_dble) :: sa       =  1.0d0 ! adaptive localization parameter a
  ! REAL(r_dble) :: sb       =  1.0d0 ! adaptive localization parameter b
  ! CHARACTER(5) :: infl_mode         ! inflation mode ('fixed' or 'adapt')
  ! REAL(r_dble) :: parm
  ! REAL(r_dble) :: xmaxloc
  REAL(r_dble) :: dep      (ny)
  REAL(r_dble) :: Innov    (ny,nbv)
  REAL(r_dble) :: hxfm     (ny)
  REAL(r_dble) :: hdxf     (ny,nbv)
  REAL(r_dble) :: dist
  INTEGER      :: i,j
  ! REAL(r_dble) :: rdiag_loc(ny)
  ! REAL(r_dble) ::  rloc_loc(ny)
  ! REAL(r_dble) ::   dep_loc(ny)
  ! REAL(r_dble) ::  hdxf_loc(ny,nbv)
  ! REAL(r_dble) :: trans    (nbv,nbv)
  ! INTEGER      :: ny_loc
  ! INTEGER      :: nbv2 ! Adaptive localization parameter
  ! REAL(r_dble) :: dist
  ! REAL(r_dble) :: obsloc(3),wa,wb
  ! INTEGER      :: i,j,n,nn,ios,ix,iy,ind_tmp


CONTAINS

  SUBROUTINE filter_initialize
    ! REAL(r_dble) :: infl_factor_ini ! Initial inflation factor
    CHARACTER(40):: func_name = 'filter_initialize'
    CALL funct_opening(func_name)

    CALL get_string_par_env ('filter' ,filter)
    CALL get_string_par_env ('update_mode',update_mode)
    CALL get_string_par_env ('comp_localization',comp_localization)
    ! print*,shape(comp_localization)
    ! read*
    ! CALL get_string_par_env ('coupled_analysis',coupled_analysis)

    SELECT CASE (update_mode)
    CASE ('Hakim') ! Time averaged ensemble
       naugm        = nx
       ! augm_var_pos = var_pos
    CASE ('Augm0') ! Time Augmented ensemble (Cheap way)
       naugm        = nx
    CASE ('Augm1') ! Time Augmented ensemble (Brute force way)
       naugm        = nx*Taver_steps
       ! augm_var_pos = RESHAPE(SPREAD(var_pos,2,Taver_steps),(/nc*Taver_steps/))
    CASE ('Augm2')
       naugm        = nx*(Taver_steps+1)
    CASE ('Augm3')
       naugm        = nx*2
    CASE ('Augm4')
       naugm        = nx*2
    CASE ('Insta')
       naugm        = nx
    END SELECT
    CALL set_int_par_file   ('naugm'  ,naugm ) ! Size of the augmented vector

    SELECT CASE (filter)
       ! CASE ('letkf')
       !    CALL get_string_par_env ('infl_mode'      ,infl_mode)
       !    CALL get_real_par_env   ('infl_factor_ini',infl_factor_ini)
       !    CALL get_string_par_env ('local_mode'     ,local_mode)
       !    CALL get_real_par_env   ('xlocal'         ,xlocal)
       !    CALL set_real_par_file  ('sa'             ,sa)
       !    CALL set_real_par_file  ('sb'             ,sb)
       !    ALLOCATE(infl_factor(naugm))
       !    infl_factor  = infl_factor_ini
       !    xmaxloc = xlocal * 2.0d0 * SQRT(10.0d0/3.0d0)
       !    nbv2 = CEILING(REAL(nbv)/2.0)
    CASE ('enkf')
       CALL get_real_par_env   ('infl_enkf'      ,infl_enkf)
       CALL get_string_par_env ('infl_mode'      ,infl_mode)
       CALL get_string_par_env ('infl_cycle_length_scaling',infl_cycle_length_scaling)
    CASE DEFAULT
       CALL error('Unknown filter '//trim(filter))
    END SELECT
    CALL print_line

    IF (spatially_extended) THEN
       call print_msg('Creating spatial localization matrices')

       ALLOCATE(augm_var_pos(naugm),loc_xy(naugm,ny))

       SELECT CASE (update_mode)
       CASE ('Hakim','Augm0','Insta')
          augm_var_pos = var_pos
       CASE ('Augm1')
          augm_var_pos = RESHAPE(SPREAD(var_pos,2,Taver_steps),(/nc*Taver_steps/))
       CASE ('Augm2')
          augm_var_pos = RESHAPE(SPREAD(var_pos,2,Taver_steps+1),(/nc*(Taver_steps+1)/))
       CASE ('Augm3','Augm4')
          augm_var_pos = RESHAPE(SPREAD(var_pos,2,2),(/nc*2/))
       CASE DEFAULT
          CALL error('Unknown update_mode '//trim(update_mode))
       END SELECT

       CALL get_real_par_env   ('loc_radius',loc_radius)
       IF(loc_radius.LE.0.0) CALL error('Localization radius should be larger than zero')

       ! Initialize localization matrices
       DO i = 1,naugm
          DO j = 1,ny
             dist = abs(augm_var_pos(i) - station_pos(j));
             loc_xy(i,j) = gaspari_cohn(loc_radius,MIN(dist,nc-dist));
          END DO
       END DO
       DO i = 1,ny
          DO j = 1,ny
             dist = abs(station_pos(i) - station_pos(j));
             loc_yy(i,j) = gaspari_cohn(loc_radius,MIN(dist,nc-dist));
          END DO
       END DO
       
    END IF

    IF (comp_localization.EQ.'yes') THEN
       call print_msg('Creating component localization matrices')

       ALLOCATE(augm_var_comp(naugm),loc_xy_comp(naugm,ny))

       SELECT CASE (update_mode)
       CASE ('Hakim','Augm0','Insta')
          augm_var_comp = var_comp
       CASE ('Augm1')
          augm_var_comp = RESHAPE(SPREAD(var_comp,2,Taver_steps),(/nc*Taver_steps/))
       CASE ('Augm2')
          augm_var_comp = RESHAPE(SPREAD(var_comp,2,Taver_steps+1),(/nc*(Taver_steps+1)/))
       CASE ('Augm3','Augm4')
          augm_var_comp = RESHAPE(SPREAD(var_comp,2,2),(/nc*2/))
       CASE DEFAULT
          CALL error('Unknown update_mode '//trim(update_mode))
       END SELECT

       !       call monitor_real_2D(var_loc_xy,'var_loc_xy')
       !       call monitor_intg_1D(augm_var_comp,'augm_var_comp')
       !       call monitor_intg_1D(station_pos,'station_pos')
       DO i = 1,ny
          DO j = 1,ny
             IF (station_comp(i).EQ. station_comp(j)) THEN
                loc_yy_comp(i,j) = 1.0
             ELSE
                loc_yy_comp(i,j) = 0.0
             END IF
          END DO
       END DO
       DO i = 1,naugm
          DO j = 1,ny
             IF (augm_var_comp(i) .EQ. station_comp(j)) THEN
                loc_xy_comp(i,j) = 1.0
             ELSE
                loc_xy_comp(i,j) = 0.0
             END IF
          END DO
       END DO
    END IF

    CALL funct_closing(func_name)
  END SUBROUTINE filter_initialize

  SUBROUTINE filter_find_analysis (xf, hxf, y, xa)
    REAL(r_dble),INTENT(IN)  ::   xf(:,:)
    REAL(r_dble),INTENT(IN)  ::    y(ny)
    REAL(r_dble),INTENT(IN)  ::  hxf(:,:)
    REAL(r_dble),INTENT(OUT) ::   xa(:,:)
    REAL(r_dble)             ::  dxf (lbound(xf,1):ubound(xf,1),nbv)
    REAL(r_dble)             ::   xfm(lbound(xf,1):ubound(xf,1))
    ! INTEGER                  ::   ia

    xfm   =          SUM( xf, 2)/nbv  ! ensemble mean
    dxf   =  xf - SPREAD( xfm,2, nbv) ! ensemble anomalies
    hxfm  =          SUM(hxf ,2)/nbv  ! ensemble obs mean
    hdxf  = hxf - SPREAD(hxfm,2, nbv) ! Ensemble obs anomalies
    ! dep   =   y - hxfm                ! Observational departure
    Innov = SPREAD(y,2,nbv) - hxf

    !---------------
    ! analysis step
    !---------------
    SELECT CASE (filter)
       ! CASE ('letkf')
       !    gridpoint_loop: DO ia=1,naugm
       !       !---------------
       !       ! Localization
       !       !---------------
       !       ny_loc = 0
       !       parm   = infl_factor(ia)

       !       DO iy=1,ny
       !          !     dist = REAL( MIN( ABS(station_pos(iy) - ia),&
       !          !&                 nx - ABS(station_pos(iy) - ia)),r_dble)
       !          dist = REAL( MIN( ABS(station_pos(iy) - augm_var_pos(ia)) , &
       !               &       nx - ABS(station_pos(iy) - augm_var_pos(ia))),r_dble)

       !          ! Spatial localization
       !          IF(dist < xmaxloc) THEN
       !             ny_loc  = ny_loc + 1
       !             dep_loc(ny_loc) = dep(iy)

       !             rdiag_loc(ny_loc) = obs_error_stdd(iy)**2

       !             IF(local_mode == 'fixed') THEN
       !                ! fixed localization
       !                rloc_loc(ny_loc) = EXP(-0.5 * (dist/xlocal)**2)
       !             ELSE
       !                ! adaptive localization
       !                CALL com_correl(    nbv2, hdxf(iy,     1:nbv2), &
       !                     &                     dxf(ia,     1:nbv2), obsloc(1))
       !                CALL com_correl(nbv-nbv2, hdxf(iy,nbv2+1:nbv ), &
       !                     &                     dxf(ia,nbv2+1:nbv ), obsloc(2))
       !                CALL com_correl(nbv     , hdxf(iy,      :    ), &
       !                     &                     dxf(ia,      :    ), obsloc(3))

       !                wa = 1.0d0 - (0.5d0*ABS(obsloc(1)-obsloc(2)))
       !                wb = ABS(obsloc(3));
       !                rloc_loc(ny_loc) = wa**sa * wb**sb

       !                IF(local_mode == 'both_') THEN
       !                   rloc_loc(ny_loc) = rloc_loc(ny_loc) * EXP(-0.5 * (dist/xlocal)**2)
       !                END IF
       !             END IF

       !             hdxf_loc(ny_loc,:) = hdxf(iy,:)
       !             IF(rloc_loc(ny_loc) < 0.0001d0) ny_loc = ny_loc-1
       !          END IF
       !       END DO
       !       !--------------
       !       ! LETKF
       !       !-------------
       !       CALL letkf_core(ny,ny_loc,hdxf_loc,rdiag_loc,rloc_loc,dep_loc,parm,trans)

       !       ! Building analysis ensemble at gridpoint ia
       !       DO j=1,nbv
       !          xa(ia,j) = xfm(ia)
       !          DO i=1,nbv
       !             xa(ia,j) = xa(ia,j) + dxf(ia,i) * trans(i,j)
       !          END DO
       !       END DO

       !       IF(infl_mode .EQ. 'adapt') infl_factor(ia) = parm

       !    END DO gridpoint_loop
    CASE ('enkf')
       ! CALL enkf_update(naugm,ny,nbv,hdxf,dep,obs_error_stdd,dxf,xf,xa)
       CALL enkf_update(naugm,ny,nbv,hdxf,Innov,obs_error_stdd,dxf,xf,xa)
    END SELECT
  END SUBROUTINE filter_find_analysis


  SUBROUTINE enkf_update(naugm,ny,m,hdxf,Innov,obs_error_stdd,dxf,xf,xa)
    INTEGER     ,INTENT(IN) :: naugm         ! dimension of analysis space
    INTEGER     ,INTENT(IN) :: ny            ! number of observations
    INTEGER     ,INTENT(IN) :: m             ! ensemble size
    REAL(r_dble),INTENT(IN) :: hdxf(ny,m)    ! HE
    ! REAL(r_dble),INTENT(IN) ::  dep(ny)    ! observational departure (y-Hx)
    REAL(r_dble),INTENT(IN) :: Innov(ny,m)   ! Innovations
    REAL(r_dble),INTENT(IN) :: obs_error_stdd(ny)
    REAL(r_dble),INTENT(IN) ::  dxf(naugm,m) ! background ensemble ptb
    REAL(r_dble),INTENT(IN) ::   xf(naugm,m) ! background ensemble
    REAL(r_dble),INTENT(OUT)::   xa(naugm,m) ! analysis ensemble
    REAL(r_dble)            :: pert(ny,m)    ! Perturbations
    REAL(r_dble)            ::   K(naugm,ny) ! Kalman gain
    REAL(r_dble)            ::   R(ny,ny)    ! observational error covariance
    REAL(r_dble)            :: wk2d1(ny,ny),wk2d2(ny,ny),Pxy(naugm,ny),Pyy(ny,ny)
    REAL(r_dble)            :: randn_vector(ny*m)
    INTEGER                 :: iy

    R = 0.0; FORALL (iy=1:ny) R(iy,iy) = obs_error_stdd(iy)**2

    CALL com_randn(ny*m,randn_vector)
    pert = MATMUL(SQRT(R),RESHAPE(randn_vector,(/ny,m/)))
    pert = anomaly(pert,2) ! Unbiassing perturbations

    Pxy = MATMUL( dxf,TRANSPOSE(hdxf)) / REAL(m-1,r_dble)
    Pyy = MATMUL(hdxf,TRANSPOSE(hdxf)) / REAL(m-1,r_dble)

    IF (spatially_extended)THEN
       Pxy = Pxy * loc_xy
       Pyy = Pyy * loc_yy
    END IF

    IF (comp_localization.EQ.'yes') THEN
       Pxy = Pxy * loc_xy_comp
       Pyy = Pyy * loc_yy_comp
    END IF

    wk2d1 = Pyy + R

    ! CALL mtx_inv(ny,wk2d1,wk2d2)
    CALL inverse(wk2d1,wk2d2,ny)

    K  = MATMUL(Pxy, wk2d2)
    ! xa = xf + MATMUL(K, SPREAD(dep,2,m) + pert)
    xa = xf + MATMUL(K, Innov + pert)

    ! CALL inflate(xa,infl_enkf)

  END SUBROUTINE enkf_update

  subroutine inverse(a,c,n)
    !============================================================
    ! Inverse matrix
    ! Method: Based on Doolittle LU factorization for Ax=b
    ! Alex G. December 2009
    !-----------------------------------------------------------
    ! input ...
    ! a(n,n) - array of coefficients for matrix A
    ! n      - dimension
    ! output ...
    ! c(n,n) - inverse matrix of A
    ! comments ...
    ! the original matrix a(n,n) will be destroyed
    ! during the calculation
    !===========================================================
    implicit none
    integer n
    double precision a(n,n), c(n,n)
    double precision L(n,n), U(n,n), b(n), d(n), x(n)
    double precision coeff
    integer i, j, k

    ! step 0: initialization for matrices L and U and b
    ! Fortran 90/95 aloows such operations on matrices
    L=0.0
    U=0.0
    b=0.0

    ! step 1: forward elimination
    do k=1, n-1
       do i=k+1,n
          coeff=a(i,k)/a(k,k)
          L(i,k) = coeff
          do j=k+1,n
             a(i,j) = a(i,j)-coeff*a(k,j)
          end do
       end do
    end do

    ! Step 2: prepare L and U matrices
    ! L matrix is a matrix of the elimination coefficient
    ! + the diagonal elements are 1.0
    do i=1,n
       L(i,i) = 1.0
    end do
    ! U matrix is the upper triangular part of A
    do j=1,n
       do i=1,j
          U(i,j) = a(i,j)
       end do
    end do

    ! Step 3: compute columns of the inverse matrix C
    do k=1,n
       b(k)=1.0
       d(1) = b(1)
       ! Step 3a: Solve Ld=b using the forward substitution
       do i=2,n
          d(i)=b(i)
          do j=1,i-1
             d(i) = d(i) - L(i,j)*d(j)
          end do
       end do
       ! Step 3b: Solve Ux=d using the back substitution
       x(n)=d(n)/U(n,n)
       do i = n-1,1,-1
          x(i) = d(i)
          do j=n,i+1,-1
             x(i)=x(i)-U(i,j)*x(j)
          end do
          x(i) = x(i)/u(i,i)
       end do
       ! Step 3c: fill the solutions x(n) into column k of C
       do i=1,n
          c(i,k) = x(i)
       end do
       b(k)=0.0
    end do
  end subroutine inverse

  FUNCTION gaspari_cohn(loc_radius,distance)
    IMPLICIT NONE
    REAL(r_dble),INTENT(IN) :: loc_radius
    REAL(r_dble),INTENT(IN) :: distance
    REAL(r_dble)            :: gaspari_cohn
    REAL(r_dble)            :: a,b

    IF (distance.LT.0.0d0) CALL error('Distance cannot be negative')
    a = loc_radius * 0.5d0
    b = distance / a

    IF     ( distance <=   a ) THEN
       gaspari_cohn = b**5 *     (- 0.25d0) + b**4  *          0.5d0  + &
            &         b**3 * (5.0d0/ 8.0d0) + b**2  * (- 5.0d0/3.0d0) + 1.0d0
    ELSE IF( distance <= 2*a ) THEN
       gaspari_cohn = b**5 * (1.0d0/12.0d0) + b**4  *        (-0.5d0) + &
            &         b**3 * (5.0d0/ 8.0d0) + b**2  * (  5.0d0/3.0d0) + 4.0d0 + &
            &         b    *       (-5.0d0) + b**(-1)*(- 2.0d0/3.0d0)
    ELSE
       gaspari_cohn = 0.0d0
    END IF

    RETURN
  END FUNCTION gaspari_cohn

END MODULE ta_filter



