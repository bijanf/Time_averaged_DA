!=======================================================================
!> @brief TADA (Time-average data assimilation) experiment module
!=======================================================================
MODULE time_averaging
  USE common
  USE common_das_tools
  USE common_letkf
  USE common_dyn_system
  USE common_obs_operator
  USE common_trajectory
  USE common_filter

  IMPLICIT NONE
  ! INTEGER     ,PRIVATE   :: im,icomp
  ! INTEGER                :: detail      ! Output storage level
  ! INTEGER                :: Taver_steps ! in time steps (1: instantaneous obs)
  ! REAL(r_dble)           :: infl_factor_ini
  ! CHARACTER(5)           :: loc_mode    ! localization mode ('fixed','adapt','both')
  !  INTEGER               :: detail    ! Output storage level
  !                                       >= O : only overall diagnostics
  !                                       >= 1 : ensemble statistics
  !                                       >= 2 : ensemble members
  REAL(r_dble) :: nature_state_Insta    (nx,  1)
  REAL(r_dble) :: nature_state_Taver    (nx,  1)
  REAL(r_dble) ::  prior_state_Insta    (nx,nbv)
  REAL(r_dble) ::  prior_state_Taver    (nx,nbv)
  REAL(r_dble) ::  postr_state_Insta    (nx,nbv)
  REAL(r_dble) ::  postr_state_Taver    (nx,nbv)
  REAL(r_dble) :: nature_obs_clean_Insta(ny,  1)
  REAL(r_dble) :: nature_obs_clean_Taver(ny,  1)
  REAL(r_dble) :: nature_obs_dirty_Taver(ny,  1)
  REAL(r_dble) ::  prior_obs_clean_Taver(ny,nbv)

  REAL(r_dble) :: prior_Emean_Insta(nx)
  REAL(r_dble) :: prior_Emean_Taver(nx)
  REAL(r_dble) :: prior_Insta_Esprd
  REAL(r_dble) :: prior_Taver_Esprd

  REAL(r_dble) :: postr_Emean_Insta(nx)
  REAL(r_dble) :: postr_Emean_Taver(nx)
  REAL(r_dble) :: postr_Insta_Esprd
  REAL(r_dble) :: postr_Taver_Esprd


  REAL(r_dble),ALLOCATABLE :: &
       &       nature_state_Insta_array(:,:,:), &
       &        prior_state_Insta_array(:,:,:), &
       &        postr_state_Insta_array(:,:,:), &
       &   nature_obs_clean_Insta_array(:,:,:), &
       &    prior_obs_clean_Insta_array(:,:,:)
  INTEGER,PRIVATE     :: ita,ix

CONTAINS

  !=======================================================================
  !> @brief Configure TADA experiment
  !=======================================================================
  SUBROUTINE ta_filter_initialize
    CALL print_line
    CALL set_int_par_file('nbv',nbv)
    CALL print_line

    CALL print_line
    PRINT*,' Time-average DA exp. config'
    CALL print_line


    CALL print_line
    !---------------------------------------------------------------------
    ! Parameter parsing
    !---------------------------------------------------------------------
    IF(Taver_steps.GT.cycle_steps) &
         & CALL error('Time-average period larger than cycle length')
    ! IF(.NOT.((detail.EQ.1).OR.(detail.EQ.2)))THEN
    !    PRINT*,' Detail = ',detail
    !    CALL error('Unsupported detail level ')
    ! END IF

  END SUBROUTINE ta_filter_initialize




END MODULE time_averaging
