!=======================================================================
!> @brief Ensemble related variables and procedures
!=======================================================================
MODULE common_ensemble
  USE common
  USE common_letkf
  USE common_dyn_system
  USE common_trajectory
  USE model_tools
  IMPLICIT NONE
  INTEGER,PRIVATE::icomp,detail=2 


  TYPE :: E_stat
     REAL(r_size):: Emean(nx)
     REAL(r_size):: Eanom(nx,nbv)
     REAL(r_sngl):: Esprd
     REAL(r_sngl):: error(nx)
     REAL(r_sngl):: Ermse
     REAL(r_sngl):: Ermse_comp(n_comp)
     REAL(r_sngl):: Esprd_comp(n_comp)
  END TYPE E_stat

CONTAINS

  !=======================================================================
  !> @brief TADA experiment results' analysis
  !=======================================================================
  SUBROUTINE ensemble_run_diagnose(run_mode)
    CHARACTER(4),INTENT(IN)  ::run_mode
    REAL(r_sngl),ALLOCATABLE :: infl_t(:)
    REAL(r_sngl)             :: infl_Tmean
    PRINT*,'========================='
    PRINT*,' Experiment Diagnostics'
    PRINT*,'========================='

    CALL ensemble_write_overall_rms_stats('prior_Insta')
    CALL ensemble_write_overall_rms_stats('prior_Taver')

    IF (run_mode.EQ.'assi') then
       CALL ensemble_write_overall_rms_stats('postr_Insta')
       CALL ensemble_write_overall_rms_stats('postr_Taver')
       CALL calculate_skill('prior','Insta')
       CALL calculate_skill('prior','Taver')
       CALL calculate_skill('postr','Insta')
       CALL calculate_skill('postr','Taver')

       ALLOCATE(infl_t(cycles))
       CALL com_read_var_vs_t_4byte('infl_factor_Xmean.dat',cycles,infl_t)
       infl_Tmean = trajectory_Tmean(infl_t)
       CALL print_line
       PRINT  '(A,F12.5)','  INFL = ',infl_Tmean
       CALL print_line
    END IF

  END SUBROUTINE ensemble_run_diagnose

  SUBROUTINE ensemble_get_stats(E,nature,E_stats)
    REAL(r_size),INTENT(IN) :: E       (nx,nbv)
    REAL(r_size),INTENT(IN) :: nature  (nx)
    TYPE(E_stat),INTENT(OUT):: E_stats
    INTEGER                 :: cli, cui

    !---------------------------!
    ! Model ensemble statistics !
    !---------------------------!
    E_stats%Emean =        SUM(E             ,2)/ nbv
    E_stats%Eanom = E - SPREAD(E_stats%Emean ,2,  nbv)
    E_stats%Esprd = SQRT(  SUM(E_stats%Eanom**2)/(nbv*nx) )
    E_stats%error = nature -   E_stats%Emean
    E_stats%Ermse = SQRT(  SUM(E_stats%error**2)/ nx)

    !----------------------------------!
    ! Ensemble statistics by component !
    !----------------------------------!
    IF(n_comp.GT.1)THEN
       DO icomp=1,n_comp
          cli = comp_var_first(icomp); cui = comp_var_final(icomp)
          E_stats%Esprd_comp(icomp) = SQRT(SUM(&
               E_stats%Eanom(cli:cui,:)**2)/(comp_size(icomp)*nbv))
          E_stats%Ermse_comp(icomp) = SQRT(SUM(&
               E_stats%error(cli:cui  )**2)/ comp_size(icomp)     )
       END DO
    END IF

  END SUBROUTINE ensemble_get_stats



  SUBROUTINE open_ensemble_files(uini,prefix)
    INTEGER     ,INTENT(IN) :: uini
    CHARACTER(*),INTENT(IN) :: prefix
    CHARACTER               :: C
    CHARACTER(60)           :: rmse_name, sprd_name

    ! IF(detail.GE.2)OPEN(uini+0,FILE=prefix//'.dat'      ,FORM='unformatted')
    IF(detail.GE.1)OPEN(uini+0,FILE=prefix//'_Emean.dat',FORM='unformatted')
    !    CALL model_set_state_4byte_ctl (prefix//'_Emean',cycles)

    IF(detail.GE.1)OPEN(uini+1,FILE=prefix//'_error.dat',FORM='unformatted')
    !    CALL model_set_state_4byte_ctl (prefix//'_error',cycles)

    sprd_name = prefix//'_Esprd'
    rmse_name = prefix//'_Ermse'
    CALL com_set_var_vs_t_ctl(trim(sprd_name),cycles,'SPREAD')
    CALL com_set_var_vs_t_ctl(trim(rmse_name),cycles,'RMSE')
    OPEN(uini+2,FILE=trim(sprd_name)//'.dat',FORM='unformatted')
    OPEN(uini+3,FILE=trim(rmse_name)//'.dat',FORM='unformatted')

    IF(n_comp.GT.1)THEN
       DO icomp=1,n_comp
          WRITE(C,'(I1)') icomp
          rmse_name = prefix//'_Ermse_comp'//C
          sprd_name = prefix//'_Esprd_comp'//C
          CALL com_set_var_vs_t_ctl(trim(sprd_name),cycles,'SPREAD')
          CALL com_set_var_vs_t_ctl(trim(rmse_name),cycles,'RMSE')
          OPEN(uini+2+icomp*2,FILE=trim(sprd_name)//'.dat',FORM='unformatted')
          OPEN(uini+3+icomp*2,FILE=trim(rmse_name)//'.dat',FORM='unformatted')
       END DO
    END IF
  END SUBROUTINE open_ensemble_files


  SUBROUTINE feed_ensemble_files(uini,Ens_state,stats)
    INTEGER     ,INTENT(IN) :: uini
    REAL(r_size),INTENT(IN) :: Ens_state(:,:)
    TYPE(E_stat),INTENT(IN) :: stats

    ! IF(detail.GE.2) WRITE(uini+0) REAL(Ens_state  ,r_sngl)
    IF(detail.GE.1) WRITE(uini+0) REAL(stats%Emean,r_sngl)
    IF(detail.GE.1) WRITE(uini+1) REAL(stats%error,r_sngl)
    IF(detail.GE.0) WRITE(uini+2) REAL(stats%Esprd,r_sngl)
    IF(detail.GE.0) WRITE(uini+3) REAL(stats%Ermse,r_sngl)

    IF(n_comp.GT.1)THEN
       DO icomp=1,n_comp
          WRITE(uini+2+icomp*2) REAL(stats%Esprd_comp(icomp),r_sngl)
          WRITE(uini+3+icomp*2) REAL(stats%Ermse_comp(icomp),r_sngl)
       END DO
    END IF
  END SUBROUTINE feed_ensemble_files


  SUBROUTINE close_ensemble_files(uini)
    INTEGER,INTENT(IN) :: uini

    ! IF(detail.GE.2) CLOSE(uini+0)
    IF(detail.GE.1) CLOSE(uini+0)
    IF(detail.GE.1) CLOSE(uini+1)
    CLOSE(uini+2); CLOSE(uini+3)
    IF(n_comp.GT.1)THEN
       DO icomp=1,n_comp
          CLOSE(uini+2+icomp*2); CLOSE(uini+3+icomp*2)
       END DO
    END IF
  END SUBROUTINE close_ensemble_files

  !=======================================================================
  !> @brief Ensemble Root mean square statistics
  !=======================================================================
  SUBROUTINE ensemble_write_overall_rms_stats(prefix)
    CHARACTER(*),INTENT(IN)  :: prefix
    CHARACTER                :: C
    CHARACTER(60)            :: var_name
    REAL(r_sngl),ALLOCATABLE :: var_t(:)
    REAL(r_size)             :: var_Tmean
    ALLOCATE(var_t(cycles))

    CALL print_line
    PRINT*,' '//prefix//' overall diagnostics'
    CALL print_line
    var_name = prefix//'_Esprd';  CALL var_vs_t_Tmean(trim(var_name))
    var_name = prefix//'_Ermse';  CALL var_vs_t_Tmean(trim(var_name))
    CALL print_line

    IF(n_comp.GT.1)THEN
       DO icomp=1,n_comp
          WRITE(C,'(I1)') icomp;
          var_name = prefix//'_Esprd_comp'//C; CALL var_vs_t_Tmean(trim(var_name))
          var_name = prefix//'_Ermse_comp'//C; CALL var_vs_t_Tmean(trim(var_name))
       END DO
       CALL print_line
    END IF
  END SUBROUTINE ensemble_write_overall_rms_stats

  SUBROUTINE calculate_skill(ens_phase,updated_state)
    CHARACTER(*),INTENT(IN)  :: ens_phase,updated_state
    REAL(r_sngl),ALLOCATABLE :: ens_free_error(:,:)
    REAL(r_sngl),ALLOCATABLE :: ens_assi_error(:,:)
    REAL(r_sngl) :: ens_free_error_Tmean(nx)
    REAL(r_sngl) :: ens_free_error_Tstd (nx)
    REAL(r_sngl) :: ens_assi_error_Tstd (nx)
    REAL(r_sngl) :: ens_assi_error_Tmean(nx)
    REAL(r_sngl) :: ens_assi_RMS_skill_score(nx)
    REAL(r_sngl) :: ens_assi_RMSE(nx)
    INTEGER::i
    ! PRINT*,'========================='
    ! PRINT*,' Experiment Diagnostics'
    ! PRINT*,'========================='

    ALLOCATE(ens_free_error(nx,cycles))
    ALLOCATE(ens_assi_error(nx,cycles))
    CALL com_read_1Dfield_vs_t_4byte &
         ('ens_free_prior_'//updated_state//'_error.dat',cycles,ens_free_error)
    CALL com_read_1Dfield_vs_t_4byte &
         (   ens_phase//'_'//updated_state//'_error.dat',cycles,ens_assi_error)
    DO i=1,nx
       CALL trajectory_scalar_Tstats &
            (ens_free_error(i,:),ens_free_error_Tmean(i),ens_free_error_Tstd(i))
       CALL trajectory_scalar_Tstats &
            (ens_assi_error(i,:),ens_assi_error_Tmean(i),ens_assi_error_Tstd(i))
       ens_assi_RMS_skill_score(i)=1-(ens_assi_error_Tstd(i)/ens_free_error_Tstd(i))
    END DO

    OPEN (850,FILE=ens_phase//'_'//updated_state//'_RMSSS_Xmean.dat')
    WRITE(850,*) SUM(ens_assi_RMS_skill_score)/nx
    CLOSE(850)

    DEALLOCATE(ens_free_error)
    DEALLOCATE(ens_assi_error)
  END SUBROUTINE calculate_skill


  !SUBROUTINE get_nature_trajectory
  !    INTEGER :: i,model_steps
  !    CALL get_int_par_file('model_steps' ,model_steps)

  !    ALLOCATE(nature(nx,model_steps))
  !    OPEN(10,FILE='nature_trajectory_4byte.grd',FORM='unformatted')
  !    DO i=1,model_steps
  !        READ(10) nature(:,i)
  !    END DO
  !    CLOSE(10)

  !!    CALL read_particle_trajectory_4byte('nature_trajectory_4byte.grd',model_steps,nature)

  !END SUBROUTINE get_nature_trajectory


  !SUBROUTINE ensemble_obs_stats(Eobs, Eobs_mean, Eobs_anom)
  !     INTEGER                  :: i
  !     REAL(r_size),INTENT(in)  :: Eobs     (:,:)
  !     REAL(r_size),INTENT(out) :: Eobs_mean(:)
  !     REAL(r_size),INTENT(out) :: Eobs_anom(:,:)
  !     DO i=1,stations
  !             CALL com_mean(nbv,Eobs(i,:),Eobs_mean(i))
  !     END DO
  !     Eobs_anom   = Eobs - SPREAD(Eobs_mean(:),2,nbv)
  !END SUBROUTINE ensemble_obs_stats

  !FUNCTION get_ensemble_mean_4byte()
  !    REAL(r_sngl) :: get_ensemble_mean_4byte(nx)
  !    get_ensemble_mean_4byte(:) = E_mean(:)
  !END FUNCTION get_ensemble_mean_4byte

END MODULE common_ensemble
