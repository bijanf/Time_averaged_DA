MODULE common_particle
  USE common_das_tools
  USE model_core
  IMPLICIT NONE

  ! Trajectory variables
  INTEGER,PROTECTED     :: cycles
  INTEGER               :: ic
  INTEGER,PROTECTED     :: cycle_length    ! in model time steps
  INTEGER               :: icl
  INTEGER,PROTECTED     :: nspinup ! spin-up length in time steps
  !  INTEGER,PROTECTED     :: dspinup ! spin-up length in days
  REAL(r_size),PROTECTED:: model_state(nx) ! Model state
  REAL(r_size),PROTECTED:: time       
!  REAL(r_size),PROTECTED:: time_step       ! time of one time step
  INTEGER               :: ix,icomp
  CHARACTER(50)         :: model

CONTAINS

  SUBROUTINE trajectory_initialize
    CALL print_line
    PRINT*,' Trajectory config'
    CALL print_line
    CALL get_string_par_env('model'       ,model)
    CALL get_int_par_env   ('cycle_length',cycle_length)
    CALL get_int_par_env   ('cycles'      ,cycles)
    CALL get_int_par_env   ('nspinup'     ,nspinup)
    time      = 0.0d0
!    time_step = dt
    CALL print_line
    !---------------------------------------------------------------------
    ! Parameter parsing
    !---------------------------------------------------------------------
    IF(nspinup.GE.cycles) CALL error('Spinup period longer than run')

    ! Time stepping parameter setting
    !  INTEGER      :: ndays
    !  CALL get_int_par_file ('ndays'      ,ndays)
    !   cycles       =       ndays * 4
    !  cycle_length = dts_per_day / 4
    !  PRINT '(A,I8)'  ,' cycle_length : ',cycle_length
    !  PRINT '(A,I8)'  ,' cycles       : ',cycles
    !CALL get_int_par_env('dspinup'      ,dspinup)
    !nspinup = dspinup * 4 ! time steps for spin-up
  END SUBROUTINE trajectory_initialize

    SUBROUTINE set_model_state(x0)
    REAL(r_size) :: x0(nx)
    model_state(:) = x0(:)
  END SUBROUTINE set_model_state

  SUBROUTINE model_step
    CALL rk4_step(model_state,model_state)
    time = time + dt
  END SUBROUTINE model_step


  SUBROUTINE rk4_step(x1,x2)
    REAL(r_size),INTENT(IN)  :: x1(1:nx)
    REAL(r_size),INTENT(OUT) :: x2(1:nx)
    REAL(r_size),ALLOCATABLE :: xtmp(:),q1(:),q2(:),q3(:),q4(:)

    !--[1.1.1] allocation --------------------------------------------------
    ALLOCATE(xtmp(1:nx))
    ALLOCATE(  q1(1:nx))
    ALLOCATE(  q2(1:nx))
    ALLOCATE(  q3(1:nx))
    ALLOCATE(  q4(1:nx))

    !--[1.1.2] time integration --------------------------------------------
    xtmp(:) = x1(:);                  CALL model_tendencies(xtmp,time,q1)
    xtmp(:) = x1(:) + 0.5d0 * q1(:);  CALL model_tendencies(xtmp,time,q2)
    xtmp(:) = x1(:) + 0.5d0 * q2(:);  CALL model_tendencies(xtmp,time,q3)
    xtmp(:) = x1(:) +         q3(:);  CALL model_tendencies(xtmp,time,q4)
    x2  (:) = x1(:) + (q1(:) + 2.0d0 * q2(:) + 2.0d0 * q3(:) + q4(:)) / 6.0d0

    !--[1.1.3] tidy up -----------------------------------------------------
    DEALLOCATE( xtmp,q1,q2,q3,q4 )
  END SUBROUTINE rk4_step

  SUBROUTINE var_vs_t_Tmean(var_name)
    CHARACTER(*),INTENT(IN)  :: var_name
    REAL(r_sngl),ALLOCATABLE :: var_ts(:)
    REAL(r_sngl)             :: var_ts_Tmean
    REAL(r_sngl)             :: var_ts_Tstd

    ALLOCATE(var_ts(cycles))
    CALL com_read_var_vs_t_4byte(var_name//'.dat',cycles,var_ts)

    CALL trajectory_scalar_Tstats(var_ts,var_ts_Tmean,var_ts_Tstd)
    !    var_ts_Tmean = trajectory_Tmean(var_ts)
    PRINT  '(A33,F8.3)',' '//var_name//'_Tmean = ',var_ts_Tmean

    OPEN (550,FILE=var_name//'_Tmean.dat')
    WRITE(550,*) var_ts_Tmean
    CLOSE(550)

    OPEN (550,FILE=var_name//'_Tstd.dat')
    WRITE(550,*) var_ts_Tstd
    CLOSE(550)

  END SUBROUTINE var_vs_t_Tmean

  FUNCTION trajectory_Tmean(var_t)
    REAL(r_sngl) :: var_t(:)
    REAL(r_sngl) :: trajectory_Tmean

    trajectory_Tmean = SUM(var_t(nspinup+1:cycles)) / (cycles-nspinup)
  END FUNCTION trajectory_Tmean

  SUBROUTINE trajectory_scalar_Tstats(var_ts,var_ts_Tmean,var_ts_Tstd)
    REAL(r_sngl),INTENT(IN)  :: var_ts(:)   ! Scalar time series
    REAL(r_sngl),INTENT(OUT) :: var_ts_Tmean
    REAL(r_sngl),INTENT(OUT) :: var_ts_Tstd
    REAL(r_sngl),ALLOCATABLE :: var_ts_cut(:)
    REAL(r_sngl),ALLOCATABLE :: var_ts_Tanom(:)

    ALLOCATE(var_ts_cut  (cycles-nspinup))
    ALLOCATE(var_ts_Tanom(cycles-nspinup))

    var_ts_cut   = var_ts(nspinup+1:cycles)
    var_ts_Tmean =      SUM(var_ts_cut    ) / (cycles-nspinup)
    var_ts_Tanom = var_ts_cut - SPREAD(var_ts_Tmean,1,cycles-nspinup)
    var_ts_Tstd  = SQRT(SUM(var_ts_Tanom**2)/(cycles-nspinup-1))

    DEALLOCATE(var_ts_cut)
    DEALLOCATE(var_ts_Tanom)

  END SUBROUTINE trajectory_scalar_Tstats

END MODULE common_particle
