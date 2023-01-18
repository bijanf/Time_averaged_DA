!=======================================================================
! Lorenz96 model dynamics definition
!=======================================================================
MODULE model_tools
  USE common
  USE common_dyn_system
  USE netcdf
  IMPLICIT NONE

CONTAINS


  ! !=======================================================================
  ! !> @brief Read nature run, produce obs and calculate obs variance
  ! !=======================================================================
  ! subroutine open_state_nc_file(state_name,hdl)
  !   CHARACTER(*)         ,INTENT(IN) :: state_name
  !   TYPE(state_nc_handle),INTENT(OUT):: hdl

  !   call check(nf90_create(state_name//'.nc', NF90_CLOBBER, hdl%ncfile_id) )
  !   ! State file definition
  !   call check(nf90_def_dim(hdl%ncfile_id, "x",             nx, hdl%x_dimid))
  !   call check(nf90_def_dim(hdl%ncfile_id, "t", NF90_UNLIMITED, hdl%t_dimid))

  !   call check(nf90_def_var(hdl%ncfile_id, "x", NF90_REAL,hdl%x_dimid,hdl%x_varid))
  !   call check(nf90_def_var(hdl%ncfile_id, "t", NF90_REAL,hdl%t_dimid,hdl%t_varid))
  !   ! call check(nf90_put_att(hdl%ncfile_id, t_varid, "units", "second") )
  !   ! call check( nf90_put_att(ncid, temp_varid, UNITS, TEMP_UNITS) )

  !   hdl%dim_ids = (/hdl%x_dimid, hdl%t_dimid/)
  !   call check(nf90_def_var(hdl%ncfile_id,"comp1",NF90_REAL,&
  !        &                            hdl%dim_ids,hdl%comp1_varid))
  !   call check(nf90_enddef (hdl%ncfile_id))

  !   hdl%count = (/ nx, 1 /); hdl%start = (/ 1, 1 /)
  ! end subroutine open_state_nc_file

  ! subroutine write_state_in_nc_file(hdl,ic,state_Insta,state_Taver)
  !   TYPE(state_nc_handle),INTENT(INOUT):: hdl
  !   INTEGER              ,INTENT(IN)   :: ic
  !   REAL(r_size)         ,INTENT(IN)   :: state_Insta(:)
  !   REAL(r_size)         ,INTENT(IN)   :: state_Taver(:)

  !   hdl%start(2) = ic
  !   call check(nf90_put_var(hdl%ncfile_id, hdl%Insta_varid, state_Insta, &
  !        & start = hdl%start, count = hdl%count) )
  !   call check(nf90_put_var(hdl%ncfile_id, hdl%Taver_varid, state_Taver, &
  !        & start = hdl%start, count = hdl%count) )
  ! end subroutine write_state_in_nc_file


  ! subroutine close_state_nc_file(hdl,time_array)
  !   TYPE(state_nc_handle),INTENT(IN):: hdl
  !   REAL(r_size)         ,INTENT(IN):: time_array(:)
  !   ! call check(nf90_put_var(hdl%ncfile_id, hdl%t_varid, (/(ic*dt,ic=1,cycles)/)))
  !   call check(nf90_put_var(hdl%ncfile_id, hdl%t_varid, time_array))
  !   call check(nf90_close(hdl%ncfile_id))
  ! end subroutine close_state_nc_file


  SUBROUTINE model_write_state_8byte(out_unit)
    INTEGER,INTENT(IN)  :: out_unit

    ! WRITE(*,*) model_state(1:nx)
    ! read*
    WRITE(out_unit) model_state(1:nx)
  END SUBROUTINE model_write_state_8byte


  SUBROUTINE model_write_state_4byte(out_unit)
    INTEGER,INTENT(IN)  :: out_unit
    WRITE(out_unit) REAL(model_state(1:nx),r_sngl)
  END SUBROUTINE model_write_state_4byte


  SUBROUTINE model_set_state_4byte_ctl(filename,snapshots)
    CHARACTER(*),INTENT(IN):: filename !, DESCRIP
    INTEGER ,INTENT(IN)  :: snapshots
    ! REAL    ,INTENT(IN)  :: dt

    CALL com_set_binary_ctl(filename,'C1',nx,snapshots)

    ! OPEN (911,FILE=filename//'.ctl',FORM='FORMATTED')
    ! WRITE(911,'(A)'     )'DSET ^'//filename//'.dat'
    ! !  WRITE(11,'(A)'     )'TITLE LORENZ96 MODEL OUTPUT'
    ! WRITE(911,'(A)'     )'UNDEF -9.99E33'
    ! WRITE(911,'(A)'     )'OPTIONS sequential'
    ! WRITE(911,'(A)'     )'XDEF 40 LINEAR 1 1'
    ! WRITE(911,'(A)'     )'YDEF  1 LINEAR 1 1'
    ! WRITE(911,'(2A)'    )'ZDEF  1 LINEAR 1 1'
    ! WRITE(911,'(A,I6,A)')'TDEF ', snapshots, ' LINEAR 00Z01JAN2001 1HR'
    ! WRITE(911,'(A,I1.1)')'VARS 1'
    ! WRITE(911,'(A)'     )'C1 1 99 C1'
    ! WRITE(911,'(A)'     )'ENDVARS'
    ! CLOSE(911)
    RETURN
  END SUBROUTINE model_set_state_4byte_ctl

  !SUBROUTINE model_write_var_vs_x_4byte(filename,varname,var)
  !     CHARACTER(*),INTENT(IN):: filename !, DESCRIP
  !     CHARACTER(*),INTENT(IN):: varname
  !     REAL(r_size),INTENT(IN):: var(nx)

  !     OPEN (151,FILE=filename//'_4byte.dat',FORM='unformatted')
  !  WRITE(151) REAL(var,r_sngl)
  !  CLOSE(151)
  !     CALL model_set_var_vs_x_ctl(filename//'_4byte',varname)
  !END SUBROUTINE

  !SUBROUTINE model_set_var_vs_x_ctl(filename,varname)
  !     CHARACTER(*),INTENT(IN):: filename !, DESCRIP
  !     CHARACTER(*),INTENT(IN):: varname

  !     OPEN (11,FILE=filename//'.ctl',FORM='FORMATTED')
  !     WRITE(11,'(3A)'    )'DSET ^',trim(filename),'.dat'
  !     WRITE(11,'(A)'     )'TITLE LORENZ96 MODEL OUTPUT'
  !     WRITE(11,'(A)'     )'UNDEF -9.99E33'
  !     WRITE(11,'(A)'     )'OPTIONS sequential'
  !     WRITE(11,'(A)'     )'XDEF 40 LINEAR 1 1'
  !     WRITE(11,'(A)'     )'YDEF  1 LINEAR 1 1'
  !     WRITE(11,'(A)'     )'ZDEF  1 LINEAR 1 1'
  !     WRITE(11,'(A)' )'TDEF  1 LINEAR 00Z01JAN2001 1HR'
  !     WRITE(11,'(A)'     )'VARS 1'
  !     WRITE(11,'(3A)'    ) varname, ' 1 99 ', varname
  !     WRITE(11,'(A)'     )'ENDVARS'
  !     CLOSE(11)
  !END SUBROUTINE


END MODULE model_tools
