MODULE trajectory
  USE common_tools
  USE dynamical_system,ONLY:dt
  IMPLICIT NONE; PRIVATE; SAVE
  PUBLIC t0,step,cycles,ic,cycle_length,cycle_steps,Taver_mode,&
       Taver_length,Taver_steps,spinup_cycles, ta_ind,&
       & trajectory_initialize

  REAL(r_dble)           :: t0           ! Initial time
  INTEGER                :: step         ! Model step counter
  INTEGER     ,PROTECTED :: cycles       ! Assim. cycle number
  INTEGER                :: ic           ! Assim. cycle counter
  REAL(r_dble),PROTECTED :: cycle_length ! Assim. cycle length
  INTEGER     ,PROTECTED :: cycle_steps  ! Assim. cycle length in model steps > 0
  CHARACTER(5),PROTECTED :: Taver_mode   ! tied | loose
  REAL(r_dble),PROTECTED :: Taver_length ! Time Average length
  INTEGER                :: Taver_steps  ! Time Average length in time steps > 0
  !                                        (1: instantaneous obs)
  INTEGER     ,PROTECTED :: spinup_cycles! spin-up length in cycles
  !  INTEGER,PROTECTED     :: dspinup ! spin-up length in days
  !  REAL(r_dble),PROTECTED:: time_step       ! time of one time step

CONTAINS

  SUBROUTINE trajectory_initialize
    CALL print_line
    PRINT*,' Trajectory config'
    CALL print_line
    CALL get_real_par_env  ('dt'          ,dt)
    CALL get_real_par_env  ('t0'          ,t0)
    CALL get_real_par_env  ('Taver_length',Taver_length)
    CALL get_string_par_env('Taver_mode'  ,Taver_mode)
    SELECT CASE (TRIM(Taver_mode))
    CASE ('tied')
       cycle_length = Taver_length
       CALL set_real_par_file ('cycle_length',cycle_length)
    CASE ('loose')
       CALL get_real_par_env  ('cycle_length',cycle_length)
    CASE DEFAULT
        CALL error('Unsupported Taver_mode '//Taver_mode)
    END SELECT
    CALL get_int_par_env   (       'cycles'       ,cycles)
    CALL get_int_par_env   ('spinup_cycles',spinup_cycles)

    Taver_steps = NINT(Taver_length/dt)
    cycle_steps = NINT(cycle_length/dt)
    CALL set_int_par_file('Taver_steps',Taver_steps)
    CALL set_int_par_file('cycle_steps',cycle_steps)

    CALL print_line
    !---------------------------------------------------------------------
    ! Parameter parsing
    !---------------------------------------------------------------------
    IF(spinup_cycles.GE.cycles) CALL error('Spinup period longer than run')
    ! IF(Taver_steps.GT.buffer_steps) CALL error('Taver_steps > buffer_steps')

  END SUBROUTINE trajectory_initialize

  FUNCTION ta_ind()
    INTEGER :: ta_ind
    ta_ind =  MODULO(step-1,Taver_steps) + 1
  END FUNCTION ta_ind

  ! SUBROUTINE var_vs_t_Tmean(var_name)
  !   CHARACTER(*),INTENT(IN)  :: var_name
  !   REAL(r_sngl),ALLOCATABLE :: var_ts(:)
  !   REAL(r_sngl)             :: var_ts_Tmean
  !   REAL(r_sngl)             :: var_ts_Tstd

  !   ALLOCATE(var_ts(cycles))
  !   CALL com_read_var_vs_t_4byte(var_name//'.dat',cycles,var_ts)

  !   CALL trajectory_scalar_Tstats(var_ts,var_ts_Tmean,var_ts_Tstd)
  !   !    var_ts_Tmean = trajectory_Tmean(var_ts)
  !   PRINT  '(A33,F8.3)',' '//var_name//'_Tmean = ',var_ts_Tmean

  !   OPEN (550,FILE=var_name//'_Tmean.dat')
  !   WRITE(550,*) var_ts_Tmean
  !   CLOSE(550)

  !   OPEN (550,FILE=var_name//'_Tstd.dat')
  !   WRITE(550,*) var_ts_Tstd
  !   CLOSE(550)

  ! END SUBROUTINE var_vs_t_Tmean

  ! FUNCTION trajectory_Tmean(var_t)
  !   REAL(r_sngl) :: var_t(:)
  !   REAL(r_sngl) :: trajectory_Tmean

  !   trajectory_Tmean = SUM(var_t(spinup_cycles+1:cycles)) / (cycles-spinup_cycles)
  ! END FUNCTION trajectory_Tmean

  ! SUBROUTINE trajectory_scalar_Tstats(var_ts,var_ts_Tmean,var_ts_Tstd)
  !   REAL(r_sngl),INTENT(IN)  :: var_ts(:)   ! Scalar time series
  !   REAL(r_sngl),INTENT(OUT) :: var_ts_Tmean
  !   REAL(r_sngl),INTENT(OUT) :: var_ts_Tstd
  !   REAL(r_sngl),ALLOCATABLE :: var_ts_cut(:)
  !   REAL(r_sngl),ALLOCATABLE :: var_ts_Tanom(:)

  !   ALLOCATE(var_ts_cut  (cycles-spinup_cycles))
  !   ALLOCATE(var_ts_Tanom(cycles-spinup_cycles))

  !   var_ts_cut   = var_ts(spinup_cycles+1:cycles)
  !   var_ts_Tmean =      SUM(var_ts_cut    ) / (cycles-spinup_cycles)
  !   var_ts_Tanom = var_ts_cut - SPREAD(var_ts_Tmean,1,cycles-spinup_cycles)
  !   var_ts_Tstd  = SQRT(SUM(var_ts_Tanom**2)/(cycles-spinup_cycles-1))

  !   DEALLOCATE(var_ts_cut)
  !   DEALLOCATE(var_ts_Tanom)

  ! END SUBROUTINE trajectory_scalar_Tstats

END MODULE trajectory
