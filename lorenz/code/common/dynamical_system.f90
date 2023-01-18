MODULE dynamical_system
  USE common_tools
  USE model_core
  IMPLICIT NONE

  REAL(r_dble),PROTECTED :: model_state(nx) ! Model state
  REAL(r_dble),PROTECTED :: time
  REAL(r_dble)           ::  dt = 0.01d0 ! time of one time step
  INTEGER     ,PRIVATE   :: ix,icomp,iy

CONTAINS

  SUBROUTINE model_initialize
    CALL set_string_par_file('model_name',model_name)
    CALL set_int_par_file   ('n_comp'    ,n_comp)
    CALL set_int_par_file   ('nc'        ,nc)
    CALL set_int_par_file   ('nx'        ,nx)
    CALL model_get_par_values
  END SUBROUTINE model_initialize

  SUBROUTINE set_model_time(time_in)
    REAL(r_dble) :: time_in
    time = time_in
  END SUBROUTINE set_model_time

  SUBROUTINE set_model_state(x0)
    REAL(r_dble) :: x0(nx)
    model_state = x0
  END SUBROUTINE set_model_state

  SUBROUTINE model_step
    CALL rk4_step(model_state)
    ! CALL rk4_step(model_state,model_state)
    time = time + dt
  END SUBROUTINE model_step

  !> Runge-Kutta integrator for time-dependent vector fields
  SUBROUTINE rk4_step(x1)
    ! SUBROUTINE rk4_step(x1,x2)
    REAL(r_dble),INTENT(INOUT)  :: x1(nx)
    ! REAL(r_dble),INTENT(OUT) :: x2(nx)
    REAL(r_dble) :: q1(nx),q2(nx),q3(nx),q4(nx),x_tmp(nx)

    q1 = F(time       , x1   ); x_tmp = x1 + q1 * dt/2
    q2 = F(time + dt/2, x_tmp); x_tmp = x1 + q2 * dt/2
    q3 = F(time + dt/2, x_tmp); x_tmp = x1 + q3 * dt
    q4 = F(time + dt  , x_tmp)
    x1 = x1 + (q1 + q2*2 + q3*2 + q4) * dt/6
    ! x2 = x1 + (q1 + q2*2 + q3*2 + q4) * dt/6
  END SUBROUTINE rk4_step

  ! SUBROUTINE rk4_step(x1)
  !   ! SUBROUTINE rk4_step(x1,x2)
  !   REAL(r_dble),INTENT(INOUT)  :: x1(nx)
  !   ! REAL(r_dble),INTENT(OUT) :: x2(nx)
  !   REAL(r_dble) :: q1(nx),q2(nx),q3(nx),q4(nx)

  !   q1 = F(time       , x1            )
  !   q2 = F(time + dt/2, x1 + q1 * dt/2)
  !   q3 = F(time + dt/2, x1 + q2 * dt/2)
  !   q4 = F(time + dt  , x1 + q3 * dt  )
  !   x1 = x1 + (q1 + q2*2 + q3*2 + q4) * dt/6
  !   ! x2 = x1 + (q1 + q2*2 + q3*2 + q4) * dt/6
  ! END SUBROUTINE rk4_step
END MODULE dynamical_system
