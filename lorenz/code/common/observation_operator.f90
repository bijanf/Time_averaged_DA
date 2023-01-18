MODULE observation_operator
  USE common_tools
  USE     io_tools
  USE dynamical_system,ONLY: nx,ny,nc,station_ind
  IMPLICIT NONE

  CHARACTER(20),PROTECTED :: obs_operator
  LOGICAL      ,PROTECTED :: obs_op_ready=.FALSE.
  REAL(r_dble) ,PROTECTED :: threshold_down(nx), threshold_up(nx) ! Response function Thresholds
  REAL(r_dble) ,PROTECTED :: x_min (nx), x_max (nx),x_range(nx)
  REAL(r_dble) ,PROTECTED :: x_mean(nx), x_stdd(nx)
  REAL(r_dble) ,PROTECTED :: SNR !   = 10.0d0
  REAL(r_dble) ,PROTECTED :: obs_error_stdd(ny)
  LOGICAL      ,PROTECTED :: noise_ready=.FALSE.
  REAL(r_dble) ,PROTECTED :: eta   ! slow-fast mix parameter (0:slow-1:fast) 
  REAL(r_dble) ,PROTECTED :: range_upper, range_lower ! response window extrems
!  REAL(r_dble) ,PROTECTED :: sat_level, range_upper, range_lower 

CONTAINS

  SUBROUTINE obs_operator_initialize
    TYPE(file_hdl) :: nature_Insta_all_min_hdl , nature_Insta_all_max_hdl
    TYPE(file_hdl) :: nature_Insta_all_mean_hdl, nature_Insta_all_stdd_hdl
    CALL print_line
    CALL set_int_par_file('ny',ny)
    CALL get_string_par_env ('obs_operator',obs_operator)

    SELECT CASE (TRIM(obs_operator))
    CASE ('Norm_add')
        CALL get_real_par_env ('eta',eta)
    CASE ('Resp_add')
        CALL get_real_par_env ('eta',eta)
        CALL get_real_par_env ('range_upper',range_upper)
        CALL get_real_par_env ('range_lower',range_lower)
    CASE ('Resp_min','Resp_product','Resp_lukasiewicz','Resp_yager')
        CALL get_real_par_env ('range_upper',range_upper)
        CALL get_real_par_env ('range_lower',range_lower)
    END SELECT

!     IF ((sat_level.GE.1.0d0).OR.(sat_level.LT.0.0d0)) THEN
!        CALL error('Sat_level out of range [0,1)')
!     END IF

    CALL print_line

    PRINT*,'- Reading mean and standard deviation values'

    CALL open_file (nature_Insta_all_mean_hdl,'nature_Insta_all_mean','state',958,'read')
    CALL read_state(nature_Insta_all_mean_hdl,x_mean)
    CALL close_file(nature_Insta_all_mean_hdl)

    CALL open_file (nature_Insta_all_stdd_hdl,'nature_Insta_all_stdd','state',959,'read')
    CALL read_state(nature_Insta_all_stdd_hdl,x_stdd)
    CALL close_file(nature_Insta_all_stdd_hdl)

    PRINT*,'- Reading extreme nature values'

    CALL open_file (nature_Insta_all_min_hdl,'nature_Insta_all_min','state',958,'read')
    CALL read_state(nature_Insta_all_min_hdl,x_min)
    CALL close_file(nature_Insta_all_min_hdl)

    CALL open_file (nature_Insta_all_max_hdl,'nature_Insta_all_max','state',959,'read')
    CALL read_state(nature_Insta_all_max_hdl,x_max)
    CALL close_file(nature_Insta_all_max_hdl)


!    range_upper    = 3.0d0 ! in standard deviations
!    range_lower    = 3.0d0 ! in standard deviations
    
    threshold_up   = x_mean + range_upper *  x_stdd
    threshold_down = x_mean - range_lower *  x_stdd

!     x_range        = 6.0d0  *  x_stdd
!     threshold_up   = x_mean + (x_range / 2.0d0) * (1.0d0 - sat_level)
!     threshold_down = x_mean - (x_range / 2.0d0) * (1.0d0 - sat_level)

!     x_range        = x_max -  x_min
!     threshold_up   = x_max - (x_range/2.0d0)*sat_level
!     threshold_down = x_min + (x_range/2.0d0)*sat_level


    obs_op_ready=.TRUE.
  END SUBROUTINE obs_operator_initialize


  FUNCTION clean_obs(x)
    REAL(r_dble),INTENT(IN) :: x(nx)
    REAL(r_dble)            :: clean_obs(ny)
    REAL(r_dble)            :: signal(nx)
    REAL(r_dble),PARAMETER  :: zero(ny) = 0.0, one(ny) = 1.0

    IF (.NOT. obs_op_ready)&
         & CALL error('Observation operator has not been initialized')


    SELECT CASE (TRIM(obs_operator))
    CASE ('identity')
       clean_obs =     x     (station_ind)
    CASE ('comp1')
       clean_obs =     x     (station_ind)
    CASE ('comp2')
       clean_obs =                           x     (station_ind + nc)
    CASE ('comp_add')
       clean_obs =     x     (station_ind) + x     (station_ind + nc)
    CASE ('Norm_add')
       signal = normalize(x)
       clean_obs =  eta * signal(station_ind) + (1.0d0-eta) * signal(station_ind + nc)
    CASE ('Resp_comp1')
        signal = vsl_response(x)
       clean_obs =     signal(station_ind)
    CASE ('Resp_comp2')
        signal = vsl_response(x)
       clean_obs =                           signal(station_ind + nc)
    CASE ('Resp_add')
        signal = vsl_response(x)
       clean_obs =  eta * signal(station_ind) + (1.0d0-eta) * signal(station_ind + nc)
    CASE ('Resp_min')
        signal = vsl_response(x)
       clean_obs = MIN(signal(station_ind) , signal(station_ind + nc))
    CASE ('Resp_product')
        signal = vsl_response(x)
       clean_obs =     signal(station_ind) * signal(station_ind + nc)
    CASE ('Resp_lukasiewicz')
        signal = vsl_response(x)
       clean_obs = MAX(zero, signal(station_ind) + signal(station_ind + nc) - one(:))
    CASE ('Resp_yager')
        signal = vsl_response(x)
        clean_obs = MAX(zero, one(:)-((one(:)-signal(station_ind))**2 + &
          &                  (one(:)-signal(station_ind + nc))**2)**0.5)
    CASE DEFAULT
       CALL error('Unknown observation operator '//TRIM(obs_operator))
    END SELECT
  END FUNCTION clean_obs


  FUNCTION vsl_response(x)
    REAL(r_dble),INTENT(IN) :: x(nx)
    REAL(r_dble)            :: vsl_response(nx)
    REAL(r_dble)            :: x_anomaly(nx)
    REAL(r_dble),PARAMETER  :: zeros(nx) = 0.0, ones(nx) = 1.0

    x_anomaly = (x - threshold_down) / (threshold_up - threshold_down)
!     IF(sat_level.EQ.0.0) THEN
!        vsl_response = x_anomaly
!     ELSE
       vsl_response = MIN( MAX(x_anomaly, zeros) ,ones)
!     END IF
  END FUNCTION vsl_response

  FUNCTION normalize(x)
    REAL(r_dble),INTENT(IN) :: x(nx)
    REAL(r_dble)            :: normalize(nx)
    normalize = (x - x_mean) / x_stdd
  END FUNCTION normalize


  !   FUNCTION vsl_anomaly(x)
  !     REAL(r_dble),INTENT(IN) :: x(nx)
  !     REAL(r_dble)            :: vsl_anomaly(nx)
  !
  !     vsl_anomaly = (x - threshold_down) / (threshold_up - threshold_down)
  !   END FUNCTION vsl_anomaly

  !  VS-Lite growth rate function (only for scalars)
  !  FUNCTION vsl_response(x,threshold_down, threshold_up)
  !    REAL(r_dble),INTENT(IN) :: x, threshold_down, threshold_up
  !    REAL(r_dble)            :: vsl_response
  !    IF (threshold_down >= threshold_up) CALL error ('threshold_down should be < threshold_up')
  !    IF      (x < threshold_down) THEN
  !       vsl_response = 0.0
  !    ELSE IF (x > threshold_up) THEN
  !       vsl_response = 1.0
  !    ELSE
  !       vsl_response = (x - threshold_down)/(threshold_up - threshold_down)
  !    END IF
  !  END FUNCTION vsl_response


  SUBROUTINE obs_operator_initialize_noise
    REAL(r_dble)  :: obs_clean_Taver_Tvar(ny)
    TYPE(file_hdl):: obs_clean_Taver_Tvar_hdl
    CALL print_line
    PRINT*,' Obs. noise config'
    CALL print_line
    CALL get_real_par_env ('SNR',SNR)
    CALL print_line

    PRINT*,'- Reading clean observation variance'

    CALL open_file (obs_clean_Taver_Tvar_hdl,'obs_clean_Taver_Tvar','obs',960,'read')
    CALL read_obs  (obs_clean_Taver_Tvar_hdl,obs_clean_Taver_Tvar)
    CALL close_file(obs_clean_Taver_Tvar_hdl)

    obs_error_stdd = SQRT(obs_clean_Taver_Tvar/SNR)
    noise_ready   = .TRUE.
  END SUBROUTINE obs_operator_initialize_noise


  SUBROUTINE write_obs_error_stdd
    TYPE(file_hdl):: obs_error_stdd_hdl
    TYPE(file_hdl) :: threshold_up_hdl,threshold_down_hdl
    IF (.NOT. noise_ready)&
         & CALL error('Observation noise has not been initialized')
    CALL open_file (obs_error_stdd_hdl,'obs_error_stdd','obs',961,'write')
    CALL write_obs (obs_error_stdd_hdl,obs_error_stdd)
    CALL close_file(obs_error_stdd_hdl)

    CALL open_file  (threshold_up_hdl,'threshold_up','state',155,'write')
    CALL write_state(threshold_up_hdl, threshold_up)
    CALL close_file (threshold_up_hdl)

    CALL open_file  (threshold_down_hdl,'threshold_down','state',155,'write')
    CALL write_state(threshold_down_hdl, threshold_down)
    CALL close_file (threshold_down_hdl)

  END SUBROUTINE write_obs_error_stdd

  FUNCTION sully_obs(clean_y)
    REAL(r_dble) :: clean_y(ny)
    REAL(r_dble) :: sully_obs(ny)
    REAL(r_dble) :: gaussian_noise(ny)

    IF (.NOT. noise_ready)&
         & CALL error('Observation noise has not been initialized')

    CALL com_randn(ny,gaussian_noise)
    sully_obs  = clean_y + gaussian_noise * obs_error_stdd
  END FUNCTION sully_obs


END MODULE observation_operator
