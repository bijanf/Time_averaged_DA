!=======================================================================
! Fortran binary I/O routines
!=======================================================================
MODULE io_tools
  USE common_tools
  USE dynamical_system,ONLY:nx,ny,nc,model_name,n_comp,comp_name, &
       &                    comp_var_first,comp_var_final
  IMPLICIT NONE
  PRIVATE
  PUBLIC file_hdl,open_file,close_file,write_state,&
       & read_state, write_obs,read_obs,draw_initial_conditions

  TYPE :: file_hdl
     CHARACTER(50):: name
     CHARACTER(10):: type
     INTEGER      :: unit
     CHARACTER(5) :: status
     INTEGER      :: records
     ! CHARACTER(50):: opened
  END TYPE file_hdl

  INTEGER,PRIVATE :: icomp

  ! TYPE :: state_nc_handle
  !    INTEGER ::    ncfile_id
  !    integer :: dim_ids(NDIMS)
  !    INTEGER :: t_dimid, t_varid
  !    INTEGER :: x_dimid, x_varid
  !    INTEGER ::       comp_varid(n_comp)
  !    integer :: start(NDIMS), count(NDIMS)
  ! END TYPE state_nc_handle

  ! TYPE :: obs_nc_handle
  !    INTEGER ::    ncfile_id
  !    integer :: dim_ids(NDIMS)
  !    INTEGER :: t_dimid, t_varid
  !    INTEGER :: y_dimid, y_varid
  !    INTEGER ::        obs_varid
  !    integer :: start(NDIMS), count(NDIMS)
  ! END TYPE obs_nc_handle

  ! TYPE :: scalar_nc_handle
  !    INTEGER ::        ncfile_id
  !    INTEGER :: t_dimid, t_varid
  !    INTEGER ::     scalar_varid
  ! END TYPE scalar_nc_handle

CONTAINS
  SUBROUTINE open_file(hdl,name,type,unit,status)
    TYPE(file_hdl),INTENT(INOUT):: hdl
    CHARACTER(*)  ,INTENT(IN)   :: name
    CHARACTER(*)  ,INTENT(IN)   :: type
    INTEGER       ,INTENT(IN)   :: unit
    CHARACTER(*)  ,INTENT(IN)   :: status
    INTEGER                     :: ios

    hdl%name   = name
    hdl%type   = type
    hdl%unit   = unit
    hdl%status = status

    SELECT CASE( TRIM(hdl%status) )
    CASE ('write')
       OPEN(hdl%unit,FILE=TRIM(hdl%name)//'.grd',&
            & FORM='unformatted',STATUS='NEW',IOSTAT=ios)
    CASE ('read')
       OPEN(hdl%unit,FILE=TRIM(hdl%name)//'.grd',&
            & FORM='unformatted',STATUS='OLD',IOSTAT=ios)
    CASE DEFAULT
       CALL error('Unknown status '//TRIM(hdl%status))
    END SELECT
    IF(ios /= 0)THEN
       PRINT*,'IOSTAT = ',ios
       CALL error('Trouble opening file '//TRIM(hdl%name)//'.grd.')
    END IF

    hdl%records = 0
  END SUBROUTINE open_file
  !
  !  For model state
  !
  SUBROUTINE open_state_file(hdl,unit,name,status)
    TYPE(file_hdl),INTENT(INOUT):: hdl
    INTEGER       ,INTENT(IN)   :: unit
    CHARACTER(*)  ,INTENT(IN)   :: name
    CHARACTER(*)  ,INTENT(IN)   :: status
    INTEGER                     :: ios

    hdl%unit   = unit
    hdl%name   = name
    hdl%status = status

    SELECT CASE( TRIM(hdl%status) )
    CASE ('write')
       OPEN(hdl%unit,FILE=TRIM(hdl%name)//'.grd',&
            & FORM='unformatted',STATUS='NEW',IOSTAT=ios)
    CASE ('read')
       OPEN(hdl%unit,FILE=TRIM(hdl%name)//'.grd',&
            & FORM='unformatted',STATUS='OLD',IOSTAT=ios)
    END SELECT
    IF(ios /= 0) CALL error('Trouble opening state file')

    hdl%records = 0
  END SUBROUTINE open_state_file

  SUBROUTINE write_state(hdl,state)
    TYPE(file_hdl),INTENT(INOUT) :: hdl
    REAL(r_dble)  ,INTENT(IN)    :: state(nx)

    IF (.NOT. (TRIM(hdl%status) == 'write'))&
         & CALL error('Read access file! No writing allowed')

    DO icomp = 1,n_comp
       WRITE(hdl%unit) &
            & REAL(state(comp_var_first(icomp):comp_var_final(icomp)),r_sngl)
    END DO

    hdl%records = hdl%records + 1
  END SUBROUTINE write_state

  SUBROUTINE read_state(hdl,state)
    TYPE(file_hdl),INTENT(INOUT):: hdl
    REAL(r_dble)  ,INTENT(OUT)  :: state(nx)
    REAL(r_sngl)                :: state_tmp(nc)

    IF (.NOT. (TRIM(hdl%status) == 'read'))&
         & CALL error('Write access file! No reading allowed')

    DO icomp = 1,n_comp
       READ(hdl%unit) state_tmp
       state(comp_var_first(icomp):comp_var_final(icomp)) = &
            & REAL(state_tmp,r_dble)
    END DO
    hdl%records = hdl%records + 1
  END SUBROUTINE read_state

  SUBROUTINE close_file(hdl)
    TYPE(file_hdl),INTENT(IN) :: hdl

    CLOSE(hdl%unit)

    IF (TRIM(hdl%status) == 'write') THEN

       SELECT CASE( TRIM(hdl%type) )
       CASE ('state')
          CALL set_state_ctl(hdl)
       CASE ('obs')
          CALL set_obs_ctl(hdl)
       CASE ('spread')
          CALL set_spread_ctl(hdl)
       CASE DEFAULT
          CALL error('Unknown file type '//TRIM(hdl%type))
       END SELECT

    END IF
  END SUBROUTINE close_file

  SUBROUTINE set_state_ctl(hdl)
    TYPE(file_hdl),INTENT(IN) :: hdl
    OPEN (911,FILE=TRIM(hdl%name)//'.ctl',FORM='FORMATTED')
    WRITE(911,'(A)'     )'DSET ^'//TRIM(hdl%name)  //'.grd'
    WRITE(911,'(A)'     )'TITLE '//TRIM(model_name)//' state field'
    WRITE(911,'(A)'     )'UNDEF -9.99E33'
    WRITE(911,'(A)'     )'OPTIONS sequential'
    WRITE(911,'(A,I3,A)')'XDEF ',nc,' LINEAR 1 1'
    WRITE(911,'(A)'     )'YDEF  1 LINEAR 1 1'
    WRITE(911,'(2A)'    )'ZDEF  1 LINEAR 1 1'
    WRITE(911,'(A,I9,A)')'TDEF ',hdl%records, ' LINEAR 00Z01JAN2001 1HR'
    WRITE(911,'(A,I1.1)')'VARS ',n_comp
    DO icomp = 1,n_comp
       WRITE(911,'(A)'  ) comp_name(icomp)//' 1 99 '//comp_name(icomp)
    END DO
    WRITE(911,'(A)'     )'ENDVARS'
    CLOSE(911)
  END SUBROUTINE set_state_ctl

  SUBROUTINE set_obs_ctl(hdl)
    TYPE(file_hdl),INTENT(IN) :: hdl
    OPEN (911,FILE=TRIM(hdl%name)//'.ctl',FORM='FORMATTED')
    WRITE(911,'(A)'     )'DSET ^'//TRIM(hdl%name)//'.grd'
    WRITE(911,'(A)'     )'TITLE '//TRIM(model_name)//' observations'
    WRITE(911,'(A)'     )'UNDEF -9.99E33'
    WRITE(911,'(A)'     )'OPTIONS sequential'
    WRITE(911,'(A,I3,A)')'XDEF ',ny,' LINEAR 1 1'
    WRITE(911,'(A)'     )'YDEF  1 LINEAR 1 1'
    WRITE(911,'(2A)'    )'ZDEF  1 LINEAR 1 1'
    WRITE(911,'(A,I9,A)')'TDEF ',hdl%records, ' LINEAR 00Z01JAN2001 1HR'
    WRITE(911,'(A)'     )'VARS 1'
    WRITE(911,'(A)'     )'obs 1 99 observations' ! ('//TRIM(obs_operator)//')'
    WRITE(911,'(A)'     )'ENDVARS'
    CLOSE(911)
  END SUBROUTINE set_obs_ctl

  SUBROUTINE set_spread_ctl(hdl)
    TYPE(file_hdl),INTENT(IN) :: hdl
    OPEN (911,FILE=TRIM(hdl%name)//'.ctl',FORM='FORMATTED')
    WRITE(911,'(A)'     )'DSET ^'//TRIM(hdl%name)//'.grd'
    WRITE(911,'(A)'     )'TITLE '//TRIM(model_name)//' ensemble spread'
    WRITE(911,'(A)'     )'UNDEF -9.99E33'
    WRITE(911,'(A)'     )'OPTIONS sequential'
    WRITE(911,'(A)'     )'XDEF  1 LINEAR 1 1'
    WRITE(911,'(A)'     )'YDEF  1 LINEAR 1 1'
    WRITE(911,'(2A)'    )'ZDEF  1 LINEAR 1 1'
    WRITE(911,'(A,I9,A)')'TDEF ',hdl%records, ' LINEAR 00Z01JAN2001 1HR'
    WRITE(911,'(A)'     )'VARS 1'
    WRITE(911,'(A)'     )'spread 1 99 spread'
    WRITE(911,'(A)'     )'ENDVARS'
    CLOSE(911)
  END SUBROUTINE set_spread_ctl
  !
  !  For observations
  !
  SUBROUTINE write_obs(hdl,obs_vector)
    TYPE(file_hdl),INTENT(INOUT) :: hdl
    REAL(r_dble)  ,INTENT(IN)    :: obs_vector(ny)

    IF (.NOT. (TRIM(hdl%status) == 'write'))&
         & CALL error('Read access file! No writing allowed')

    WRITE(hdl%unit) REAL(obs_vector,r_sngl)

    hdl%records = hdl%records + 1
  END SUBROUTINE write_obs

  SUBROUTINE read_obs(hdl,obs_vector)
    TYPE(file_hdl),INTENT(INOUT) :: hdl
    REAL(r_dble)  ,INTENT(OUT)   :: obs_vector(ny)
    REAL(r_sngl)                 :: obs_vector_tmp(ny)

    IF (.NOT. (TRIM(hdl%status) == 'read'))&
         & CALL error('Write access file! No reading allowed')

    READ(hdl%unit) obs_vector_tmp
    obs_vector = REAL(obs_vector_tmp,r_dble)

    hdl%records = hdl%records + 1
  END SUBROUTINE read_obs
  !
  !  For spread
  !
  SUBROUTINE write_spread(hdl,spread)
    TYPE(file_hdl),INTENT(INOUT) :: hdl
    REAL(r_dble)  ,INTENT(IN)    :: spread

    IF (.NOT. (TRIM(hdl%status) == 'write'))&
         & CALL error('Read access file! No writing allowed')

    WRITE(hdl%unit) REAL(spread,r_sngl)

    hdl%records = hdl%records + 1
  END SUBROUTINE write_spread

  SUBROUTINE read_spread(hdl,spread)
    TYPE(file_hdl),INTENT(INOUT) :: hdl
    REAL(r_dble)  ,INTENT(OUT)   :: spread
    REAL(r_sngl)                 :: spread_tmp

    IF (.NOT. (TRIM(hdl%status) == 'read'))&
         & CALL error('Write access file! No reading allowed')

    READ(hdl%unit) spread_tmp
    spread = REAL(spread_tmp,r_dble)

    hdl%records = hdl%records + 1
  END SUBROUTINE read_spread


  SUBROUTINE draw_initial_conditions(positions,ini_states)
    INTEGER     ,INTENT(IN) :: positions(:)
    REAL(r_dble),INTENT(OUT):: ini_states(nx,size(positions))
    INTEGER                 :: im, sampling_count, n_ini_cond
    TYPE(file_hdl)          :: sampling_hdl
    REAL(r_dble)            :: tmp_state(nx)

    n_ini_cond = size(positions)

    PRINT '(A,I3,A)',' - Getting ',n_ini_cond,' initial conditions from sampling'

    CALL open_file (sampling_hdl,'sampling','state',551,'read')

    im = 1; sampling_count = 0
    DO WHILE ( im .LE. n_ini_cond )
       sampling_count = sampling_count + 1
       CALL read_state (sampling_hdl,tmp_state)
       IF (sampling_count == positions(im)) then
          ini_states(:,im) = tmp_state
          im = im + 1
       END IF
    END DO

    CALL close_file(sampling_hdl)

  END SUBROUTINE draw_initial_conditions


  ! !=======================================================================
  ! !> @brief Define new state netcdf file where to write an state
  ! !=======================================================================
  ! subroutine create_state_nc_file(state_name,hdl)
  !   CHARACTER(*)         ,INTENT(IN) :: state_name
  !   TYPE(state_nc_handle),INTENT(OUT):: hdl

  !   call check(nf90_create(state_name//'.nc', NF90_CLOBBER, hdl%ncfile_id) )
  !   ! State file definition
  !   call check(nf90_def_dim(hdl%ncfile_id, "x",             nc, hdl%x_dimid))
  !   call check(nf90_def_dim(hdl%ncfile_id, "t", NF90_UNLIMITED, hdl%t_dimid))

  !   call check(nf90_def_var(hdl%ncfile_id, "x", NF90_REAL,hdl%x_dimid,hdl%x_varid))
  !   call check(nf90_put_att(hdl%ncfile_id, hdl%x_varid,"long_name","x"))
  !   ! call check(nf90_put_att(hdl%ncfile_id, hdl%x_varid,"units"    ,"none"))
  !   call check(nf90_def_var(hdl%ncfile_id, "t", NF90_REAL,hdl%t_dimid,hdl%t_varid))
  !   call check(nf90_put_att(hdl%ncfile_id, hdl%t_varid,"long_name","time"))
  !   ! call check(nf90_put_att(hdl%ncfile_id, hdl%t_varid,"units"    ,"none"))

  !   hdl%dim_ids = (/hdl%x_dimid, hdl%t_dimid/)
  !   DO icomp = 1,n_comp
  !      call check(nf90_def_var(hdl%ncfile_id,comp_name(icomp),NF90_REAL,&
  !           &                            hdl%dim_ids,hdl%comp_varid(icomp)))
  !      call check(nf90_put_att(hdl%ncfile_id,hdl%comp_varid(icomp),&
  !           &                         "long_name",comp_name(icomp)))
  !      ! call check(nf90_put_att(hdl%ncfile_id,hdl%comp_varid(icomp),"units","none"))
  !   END DO
  !   call check(nf90_enddef (hdl%ncfile_id))

  !   hdl%count = (/nc,1/); hdl%start = (/1,1/)
  ! end subroutine create_state_nc_file

  ! !=======================================================================
  ! !> @brief Write on an opened state netcdf file
  ! !=======================================================================
  ! subroutine write_state_in_nc_file(hdl,ic,state)
  !   TYPE(state_nc_handle),INTENT(INOUT):: hdl
  !   INTEGER              ,INTENT(IN)   :: ic
  !   REAL(r_size)         ,INTENT(IN)   :: state(nx)

  !   hdl%start(2) = ic
  !   DO icomp = 1,n_comp
  !      call check(nf90_put_var(hdl%ncfile_id, hdl%comp_varid(icomp), &
  !           & REAL(state(comp_var_first(icomp):comp_var_final(icomp)),r_sngl), &
  !           & start = hdl%start, count = hdl%count) )
  !   END DO
  ! end subroutine write_state_in_nc_file

  ! !=======================================================================
  ! !> @brief Close state netcdf file when finished writing on it
  ! !=======================================================================
  ! subroutine close_state_nc_file(hdl,time_array)
  !   TYPE(state_nc_handle),INTENT(IN):: hdl
  !   REAL(r_size)         ,INTENT(IN):: time_array(:)
  !   call check(nf90_put_var(hdl%ncfile_id, hdl%x_varid,(/(ix,ix=1,nc)/)))
  !   call check(nf90_put_var(hdl%ncfile_id, hdl%t_varid, time_array))
  !   call check(nf90_close(hdl%ncfile_id))
  ! end subroutine close_state_nc_file

  ! !=======================================================================
  ! !> @brief Open an already existing state netcdf file to read from
  ! !=======================================================================
  ! subroutine open_state_nc_file(state_name,hdl)
  !   CHARACTER(*)         ,INTENT(IN) :: state_name
  !   TYPE(state_nc_handle),INTENT(OUT):: hdl

  !   call check(nf90_open(state_name//'.nc', nf90_nowrite, hdl%ncfile_id) )

  !   call check(nf90_inq_varid(hdl%ncfile_id, "x", hdl%x_varid))
  !   call check(nf90_inq_varid(hdl%ncfile_id, "t", hdl%t_varid))

  !   hdl%dim_ids = (/hdl%x_dimid, hdl%t_dimid/)
  !   DO icomp = 1,n_comp
  !      call check(nf90_inq_varid(hdl%ncfile_id,comp_name(icomp),hdl%comp_varid(icomp)))
  !   END DO

  !   hdl%count = (/nc,1/); hdl%start = (/1,1 /)
  ! end subroutine open_state_nc_file

  ! !=======================================================================
  ! !> @brief Read from an open state netcdf file
  ! !=======================================================================
  ! subroutine read_state_in_nc_file(hdl,ic,state)
  !   TYPE(state_nc_handle),INTENT(INOUT):: hdl
  !   INTEGER              ,INTENT(IN)   :: ic
  !   REAL(r_size)         ,INTENT(OUT)  :: state(nx)
  !   REAL(r_sngl)                       :: state_tmp(nx)

  !   hdl%start(2) = ic
  !   DO icomp = 1,n_comp
  !      call check(nf90_get_var(hdl%ncfile_id, hdl%comp_varid(icomp), &
  !           & state_tmp(comp_var_first(icomp):comp_var_final(icomp)), &
  !           & start = hdl%start, count = hdl%count) )
  !   END DO
  !   state = state_tmp
  ! end subroutine read_state_in_nc_file

  ! !=======================================================================
  ! !> @brief Close state netcdf file when finished writing on it
  ! !=======================================================================
  ! subroutine close_readonly_state_nc_file(hdl)
  !   TYPE(state_nc_handle),INTENT(IN):: hdl
  !   call check(nf90_close(hdl%ncfile_id))
  ! end subroutine close_readonly_state_nc_file



  ! !=======================================================================
  ! !> @brief Define new netcdf file where to write an obs
  ! !=======================================================================
  ! subroutine create_obs_nc_file(obs_name,hdl)
  !   CHARACTER(*)       ,INTENT(IN) :: obs_name
  !   TYPE(obs_nc_handle),INTENT(OUT):: hdl

  !   call check(nf90_create(obs_name//'.nc', NF90_CLOBBER, hdl%ncfile_id) )
  !   ! Obs file definition
  !   call check(nf90_def_dim(hdl%ncfile_id, "y",             ny, hdl%y_dimid))
  !   call check(nf90_def_dim(hdl%ncfile_id, "t", NF90_UNLIMITED, hdl%t_dimid))

  !   call check(nf90_def_var(hdl%ncfile_id, "y", NF90_REAL,hdl%y_dimid,hdl%y_varid))
  !   call check(nf90_put_att(hdl%ncfile_id, hdl%y_varid,"long_name","y"))
  !   call check(nf90_def_var(hdl%ncfile_id, "t", NF90_REAL,hdl%t_dimid,hdl%t_varid))
  !   call check(nf90_put_att(hdl%ncfile_id, hdl%t_varid,"long_name","time"))

  !   hdl%dim_ids = (/hdl%y_dimid, hdl%t_dimid/)
  !   call check(nf90_def_var(hdl%ncfile_id,"obs",NF90_REAL,hdl%dim_ids,hdl%obs_varid))
  !   call check(nf90_put_att(hdl%ncfile_id,hdl%obs_varid,"long_name","observations"))
  !   call check(nf90_enddef (hdl%ncfile_id))

  !   hdl%count = (/ny,1/); hdl%start = (/1,1/)
  ! end subroutine create_obs_nc_file

  ! !=======================================================================
  ! !> @brief Write on an opened obs netcdf file
  ! !=======================================================================
  ! subroutine write_obs_in_nc_file(hdl,ic,obs)
  !   TYPE(obs_nc_handle),INTENT(INOUT):: hdl
  !   INTEGER            ,INTENT(IN)   :: ic
  !   REAL(r_size)       ,INTENT(IN)   :: obs(ny)

  !   hdl%start(2) = ic
  !   call check(nf90_put_var(hdl%ncfile_id, hdl%obs_varid, REAL(obs(:),r_sngl), &
  !        & start = hdl%start, count = hdl%count) )
  ! end subroutine write_obs_in_nc_file

  ! !=======================================================================
  ! !> @brief Close obs netcdf file when finished writing on it
  ! !=======================================================================
  ! subroutine close_obs_nc_file(hdl,time_array)
  !   TYPE(obs_nc_handle),INTENT(IN):: hdl
  !   REAL(r_size)       ,INTENT(IN):: time_array(:)
  !   INTEGER:: iy
  !   call check(nf90_put_var(hdl%ncfile_id, hdl%y_varid,(/(iy,iy=1,ny)/)))
  !   call check(nf90_put_var(hdl%ncfile_id, hdl%t_varid, time_array))
  !   call check(nf90_close(hdl%ncfile_id))
  ! end subroutine close_obs_nc_file

  ! !=======================================================================
  ! !> @brief Open an already existing obs netcdf file to read from
  ! !=======================================================================
  ! subroutine open_obs_nc_file(obs_name,hdl)
  !   CHARACTER(*)       ,INTENT(IN) :: obs_name
  !   TYPE(obs_nc_handle),INTENT(OUT):: hdl

  !   call check(nf90_open(obs_name//'.nc', nf90_nowrite, hdl%ncfile_id) )

  !   call check(nf90_inq_varid(hdl%ncfile_id, "y", hdl%y_varid))
  !   call check(nf90_inq_varid(hdl%ncfile_id, "t", hdl%t_varid))

  !   hdl%dim_ids = (/hdl%y_dimid, hdl%t_dimid/)
  !   call check(nf90_inq_varid(hdl%ncfile_id,"obs",hdl%obs_varid))

  !   hdl%count = (/ny,1/); hdl%start = (/1,1 /)
  ! end subroutine open_obs_nc_file

  ! !=======================================================================
  ! !> @brief Read from an open obs netcdf file
  ! !=======================================================================
  ! subroutine read_obs_in_nc_file(hdl,ic,obs)
  !   TYPE(obs_nc_handle),INTENT(INOUT):: hdl
  !   INTEGER              ,INTENT(IN) :: ic
  !   REAL(r_size)         ,INTENT(OUT):: obs(ny)
  !   REAL(r_sngl)                     :: obs_tmp(ny)

  !   hdl%start(2) = ic
  !   call check(nf90_get_var(hdl%ncfile_id, hdl%obs_varid, obs_tmp(:), &
  !        & start = hdl%start, count = hdl%count) )
  !   obs = obs_tmp
  ! end subroutine read_obs_in_nc_file

  ! !=======================================================================
  ! !> @brief Close obs netcdf file when finished writing on it
  ! !=======================================================================
  ! subroutine close_readonly_obs_nc_file(hdl)
  !   TYPE(obs_nc_handle),INTENT(IN):: hdl
  !   call check(nf90_close(hdl%ncfile_id))
  ! end subroutine close_readonly_obs_nc_file

  ! !=======================================================================
  ! ! For an scalar
  ! !=======================================================================
  ! subroutine create_scalar_nc_file(scalar_name,hdl)
  !   CHARACTER(*)          ,INTENT(IN)  :: scalar_name
  !   TYPE(scalar_nc_handle),INTENT(OUT) :: hdl

  !   call check(nf90_create(scalar_name//'.nc', NF90_CLOBBER, hdl%ncfile_id) )

  !   ! scalar file definition
  !   call check(nf90_def_dim(hdl%ncfile_id, "t", NF90_UNLIMITED, hdl%t_dimid))
  !   call check(nf90_def_var(hdl%ncfile_id, "t", NF90_FLOAT,hdl%t_dimid,hdl%t_varid))
  !   call check(nf90_put_att(hdl%ncfile_id, hdl%t_varid,"long_name","time"))

  !   call check(nf90_def_var(hdl%ncfile_id,"scalar",NF90_FLOAT,hdl%t_dimid,hdl%scalar_varid))
  !   call check(nf90_put_att(hdl%ncfile_id,hdl%scalar_varid,"long_name","scalar"))
  !   call check(nf90_enddef (hdl%ncfile_id))

  ! end subroutine create_scalar_nc_file

  !  !=======================================================================
  !  !> @brief Write on an opened scalar netcdf file
  !  !=======================================================================
  !  subroutine write_scalar_in_nc_file(hdl,ic,scalar)
  !    TYPE(scalar_nc_handle),INTENT(IN):: hdl
  !    INTEGER               ,INTENT(IN)   :: ic
  !    REAL(r_size)          ,INTENT(IN)   :: scalar
  !    !!!! there is very subtle bug here !!!!
  !    call nf90_put_var(hdl%ncfile_id, hdl%scalar_varid,&
  !    & REAL(scalar,r_sngl), start = 1,count = 1)
  !!    call check(nf90_put_var(hdl%ncfile_id, hdl%scalar_varid,&
  !!    & REAL(scalar,r_sngl), start = ic) )
  !  end subroutine write_scalar_in_nc_file

  !  !=======================================================================
  !  !> @brief Close scalar netcdf file when finished writing on it
  !  !=======================================================================
  !  subroutine close_scalar_nc_file(hdl,time_array)
  !    TYPE(scalar_nc_handle),INTENT(IN):: hdl
  !    REAL(r_size)          ,INTENT(IN):: time_array(:)
  !    call check(nf90_put_var(hdl%ncfile_id, hdl%t_varid, time_array))
  !    call check(nf90_close(hdl%ncfile_id))
  !  end subroutine close_scalar_nc_file


  ! !=======================================================================
  ! ! @brief Standard caller of netcdf interface functions
  ! !=======================================================================
  ! !> @cond
  ! subroutine check(status)
  !   integer, intent ( in) :: status

  !   if(status /= nf90_noerr) then
  !      print *, trim(nf90_strerror(status))
  !      stop 1
  !   end if
  ! end subroutine check
  ! !> @endcond






  ! SUBROUTINE model_write_state_8byte(out_unit)
  !   INTEGER,INTENT(IN)  :: out_unit
  !   WRITE(out_unit) model_state(1:nx)
  ! END SUBROUTINE model_write_state_8byte

  ! SUBROUTINE model_write_state_4byte(out_unit)
  !   INTEGER,INTENT(IN)  :: out_unit
  !   WRITE(out_unit) REAL(model_state(   1:nc),r_sngl)
  !   WRITE(out_unit) REAL(model_state(nc+1:nx),r_sngl)
  ! END SUBROUTINE model_write_state_4byte


  ! SUBROUTINE model_set_state_4byte_ctl(filename,snapshots)
  !   CHARACTER(*),INTENT(IN):: filename !, DESCRIP
  !   INTEGER ,INTENT(IN)  :: snapshots
  ! !  REAL    ,INTENT(IN)  :: dt

  !   OPEN (11,FILE=filename//'_4byte.ctl',FORM='FORMATTED')
  !   WRITE(11,'(A)'     )'DSET ^'//filename//'_4byte.dat'
  ! !  WRITE(11,'(A)'     )'TITLE LORENZ96 MODEL OUTPUT'
  !   WRITE(11,'(A)'     )'UNDEF -9.99E33'
  !   WRITE(11,'(A)'     )'OPTIONS sequential'
  !   WRITE(11,'(A)'     )'XDEF 40 LINEAR 1 1'
  !   WRITE(11,'(A)'     )'YDEF  1 LINEAR 1 1'
  !   WRITE(11,'(2A)'    )'ZDEF  1 LINEAR 1 1'
  !   WRITE(11,'(A,I6,A)')'TDEF ', snapshots, ' LINEAR 00Z01JAN2001 1HR'
  !   WRITE(11,'(A,I1.1)')'VARS 1'
  !   WRITE(11,'(A)'     )'C1 1 99 C1'
  !   WRITE(11,'(A)'     )'ENDVARS'
  !   CLOSE(11)
  !   RETURN
  ! END SUBROUTINE
END MODULE io_tools
