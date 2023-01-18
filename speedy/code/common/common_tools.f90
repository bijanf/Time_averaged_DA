MODULE common_tools
  USE common,ONLY:r_dble,r_sngl,com_randn,com_correl,pi
  IMPLICIT NONE
  PRIVATE
  PUBLIC error,print_line,print_msg, funct_opening,funct_closing,&
       & monitor_intg_0D, monitor_intg_1D, monitor_intg_2D,&
       & monitor_real_0D, monitor_real_1D, monitor_real_2D,&
       & monitor_real_3D, monitor_real_4D,&
       & monitor_real4_0D, monitor_real4_2D, monitor_real4_3D,&
       & get_string_par_env, set_string_par_file, get_string_from_env,&
       &    get_int_par_env,    set_int_par_file,    get_int_from_env,&
       &   get_real_par_env,   set_real_par_file,   get_real_from_env,&
       &  r_dble,r_sngl,com_randn,com_correl,pi,anomaly!,gaspari_cohn

CONTAINS

  FUNCTION anomaly(array,dim)
    IMPLICIT NONE

    REAL(r_dble) ,INTENT(IN) :: array (:,:)
    INTEGER      ,INTENT(IN) :: dim
    REAL(r_dble)             :: anomaly(SIZE(array,1),SIZE(array,2))

    anomaly = array - SPREAD(SUM(array,dim)/SIZE(array,dim), dim, SIZE(array,dim))

  END FUNCTION anomaly

  !=======================================================================
  !> @brief SCREENING subroutines
  !=======================================================================
  SUBROUTINE error(message)
    CHARACTER(*),INTENT(IN):: message
    WRITE(0,*) 'Error: '//message
    STOP 1
  END SUBROUTINE error

  SUBROUTINE print_line
    WRITE(*,'(A)')'----------------------------------------------------------'
  END SUBROUTINE print_line

  SUBROUTINE print_msg(message)
    CHARACTER(*),INTENT(IN):: message
    CALL print_line
    WRITE(*,'(2A)') ' ', message
    CALL print_line
  END SUBROUTINE print_msg

  SUBROUTINE funct_opening(func_name)
    CHARACTER(*),INTENT(IN):: func_name
    CALL print_msg(trim(func_name)//' START')
  END SUBROUTINE funct_opening

  SUBROUTINE funct_closing(func_name)
    CHARACTER(*),INTENT(IN):: func_name
    CALL print_msg(trim(func_name)//' NORMAL END')
  END SUBROUTINE funct_closing

  !=======================================================================
  !> @brief MONITORING subroutines
  !=======================================================================
  SUBROUTINE monitor_real_0D(var,var_name)
    IMPLICIT NONE
    REAL*8      ,INTENT(IN) :: var
    CHARACTER(*),INTENT(IN) :: var_name
    call print_line
    write(*,*) ' ', var_name, ' = ', var
    call print_line
    RETURN
  END SUBROUTINE monitor_real_0D

  SUBROUTINE monitor_real4_0D(var,var_name)
    IMPLICIT NONE
    REAL*4      ,INTENT(IN) :: var
    CHARACTER(*),INTENT(IN) :: var_name
    call print_line
    write(*,*) ' ', var_name, ' = ', var
    call print_line
    RETURN
  END SUBROUTINE monitor_real4_0D

  SUBROUTINE monitor_real_1D(var,var_name)
    IMPLICIT NONE
    REAL*8      ,INTENT(IN) :: var(:)
    CHARACTER(*),INTENT(IN) :: var_name
    call print_line
    write(*,*)' MONITORING ', var_name
    call print_line
    write(*,*)' SHAPE =', SHAPE(var)
    write(*,*)' MINVAL=', MINVAL(var)
    write(*,*)' MINLOC=', MINLOC(var)
    write(*,*)' MAXVAL=', MAXVAL(var)
    write(*,*)' MAXLOC=', MAXLOC(var)
    write(*,*)' MEAN  =', SUM(var)/SIZE(var,1)
    call print_line
    RETURN
  END SUBROUTINE monitor_real_1D

  SUBROUTINE monitor_real_2D(var,var_name)
    IMPLICIT NONE
    REAL*8      ,INTENT(IN) :: var(:,:)
    CHARACTER(*),INTENT(IN) :: var_name
    call print_line
    write(*,*)' MONITORING ', var_name
    call print_line
    write(*,*)' SHAPE =', SHAPE(var)
    write(*,*)' MINVAL=', MINVAL(var)
    write(*,*)' MINLOC=', MINLOC(var)
    write(*,*)' MAXVAL=', MAXVAL(var)
    write(*,*)' MAXLOC=', MAXLOC(var)
    write(*,*)' MEAN  =', SUM(var)/(SIZE(var,1)*SIZE(var,2))
    call print_line
    RETURN
  END SUBROUTINE monitor_real_2D

  SUBROUTINE monitor_real4_2D(var,var_name)
    IMPLICIT NONE
    REAL*4      ,INTENT(IN) :: var(:,:)
    CHARACTER(*),INTENT(IN) :: var_name
    call print_line
    write(*,*)' MONITORING ', var_name
    call print_line
    write(*,*)' SHAPE =', SHAPE(var)
    write(*,*)' MINVAL=', MINVAL(var)
    write(*,*)' MINLOC=', MINLOC(var)
    write(*,*)' MAXVAL=', MAXVAL(var)
    write(*,*)' MAXLOC=', MAXLOC(var)
    write(*,*)' MEAN  =', SUM(var)/(SIZE(var,1)*SIZE(var,2))
    call print_line
    RETURN
  END SUBROUTINE monitor_real4_2D

  SUBROUTINE monitor_real4_3D(var,var_name)
    IMPLICIT NONE
    REAL*4      ,INTENT(IN) :: var(:,:,:)
    CHARACTER(*),INTENT(IN) :: var_name
    call print_line
    write(*,*)' MONITORING ', var_name
    call print_line
    write(*,*)' SHAPE =', SHAPE(var)
    write(*,*)' MINVAL=', MINVAL(var)
    write(*,*)' MINLOC=', MINLOC(var)
    write(*,*)' MAXVAL=', MAXVAL(var)
    write(*,*)' MAXLOC=', MAXLOC(var)
    write(*,*)' MEAN  =', SUM(var)/(SIZE(var,1)*SIZE(var,2))
    call print_line
    RETURN
  END SUBROUTINE monitor_real4_3D

  SUBROUTINE monitor_real_3D(var,var_name)
    IMPLICIT NONE
    REAL*8      ,INTENT(IN) :: var(:,:,:)
    CHARACTER(*),INTENT(IN) :: var_name
    call print_line
    write(*,*)' MONITORING ', var_name
    call print_line
    write(*,*)' SHAPE =', SHAPE(var)
    write(*,*)' MINVAL=', MINVAL(var)
    write(*,*)' MINLOC=', MINLOC(var)
    write(*,*)' MAXVAL=', MAXVAL(var)
    write(*,*)' MAXLOC=', MAXLOC(var)
    write(*,*)' MEAN  =', SUM(var)/(SIZE(var,1)*SIZE(var,2)*SIZE(var,3))
    call print_line
    RETURN
  END SUBROUTINE monitor_real_3D

  SUBROUTINE monitor_real_4D(var,var_name)
    IMPLICIT NONE
    REAL*8      ,INTENT(IN) :: var(:,:,:,:)
    CHARACTER(*),INTENT(IN) :: var_name
    call print_line
    write(*,*)' MONITORING ', var_name
    call print_line
    write(*,*)' SHAPE =', SHAPE(var)
    write(*,*)' MINVAL=', MINVAL(var)
    write(*,*)' MINLOC=', MINLOC(var)
    write(*,*)' MAXVAL=', MAXVAL(var)
    write(*,*)' MAXLOC=', MAXLOC(var)
    write(*,*)' MEAN  =', SUM(var)/(SIZE(var,1)*SIZE(var,2)*SIZE(var,3))
    call print_line
    RETURN
  END SUBROUTINE monitor_real_4D

  SUBROUTINE monitor_intg_0D(var,var_name)
    IMPLICIT NONE
    INTEGER     ,INTENT(IN) :: var
    CHARACTER(*),INTENT(IN) :: var_name
    call print_line
    write(*,*) ' ', var_name, ' = ', var
    call print_line
    RETURN
  END SUBROUTINE monitor_intg_0D

  SUBROUTINE monitor_intg_1D(var,var_name)
    IMPLICIT NONE
    INTEGER     ,INTENT(IN) :: var(:)
    CHARACTER(*),INTENT(IN) :: var_name
    call print_line
    write(*,*)' MONITORING ', var_name
    call print_line
    write(*,*)' SHAPE =', SHAPE(var)
    write(*,*)' MINVAL=', MINVAL(var)
    write(*,*)' MINLOC=', MINLOC(var)
    write(*,*)' MAXVAL=', MAXVAL(var)
    write(*,*)' MAXLOC=', MAXLOC(var)
    write(*,*)' MEAN  =', REAL(SUM(var))/SIZE(var,1)
    call print_line
    RETURN
  END SUBROUTINE monitor_intg_1D

  SUBROUTINE monitor_intg_2D(var,var_name)
    IMPLICIT NONE
    INTEGER     ,INTENT(IN) :: var(:,:)
    CHARACTER(*),INTENT(IN) :: var_name
    call print_line
    write(*,*)' MONITORING ', var_name
    call print_line
    write(*,*)' SHAPE =', SHAPE(var)
    write(*,*)' MINVAL=', MINVAL(var)
    write(*,*)' MINLOC=', MINLOC(var)
    write(*,*)' MAXVAL=', MAXVAL(var)
    write(*,*)' MAXLOC=', MAXLOC(var)
    write(*,*)' MEAN  =', REAL(SUM(var))/SIZE(var,1)
    call print_line
    RETURN
  END SUBROUTINE monitor_intg_2D

  !=======================================================================
  !> @brief STRING IO subroutines
  !=======================================================================
  SUBROUTINE get_string_par_env(par_name,par_value)
    CHARACTER(*),INTENT(IN) :: par_name
    CHARACTER(*),INTENT(OUT):: par_value
    ! CHARACTER(50)           :: par_value_char
    INTEGER                 :: len, status

    CALL GET_ENVIRONMENT_VARIABLE(par_name, par_value, len, status)
    IF (status.EQ.1) CALL error ("Unset enviroment variable "//par_name)
    PRINT '(A20,A,A20)',par_name,' = ',trim(par_value)

    CALL set_string_par_file(par_name,trim(par_value))
  END SUBROUTINE get_string_par_env

  SUBROUTINE get_string_from_env(par_name,par_value)
    CHARACTER(*),INTENT(IN) :: par_name
    CHARACTER(*),INTENT(OUT):: par_value
    ! CHARACTER(50)           :: par_value_char
    INTEGER                 :: len, status

    CALL GET_ENVIRONMENT_VARIABLE(par_name, par_value, len, status)
    IF (status.EQ.1) CALL error ("Unset enviroment variable "//par_name)

  END SUBROUTINE get_string_from_env

  SUBROUTINE set_string_par_file(par_name,par_value)
    CHARACTER(*) :: par_name
    CHARACTER(*) :: par_value
    OPEN (190,FILE='par.cfg',FORM='formatted')
    write(190,*) trim(par_value)
    CLOSE(190)
    CALL system('mv par.cfg '//par_name//'.cfg')
  END SUBROUTINE set_string_par_file

  !=======================================================================
  !> @brief INTEGER IO subroutines
  !=======================================================================
  SUBROUTINE get_int_par_env(par_name,par_value)
    CHARACTER(*),INTENT(IN) :: par_name
    INTEGER     ,INTENT(OUT):: par_value
    CHARACTER(50)           :: par_value_char
    INTEGER                 :: len, status

    CALL GET_ENVIRONMENT_VARIABLE(par_name, par_value_char, len, status)
    IF (status.EQ.1) CALL error ("Unset enviroment variable "//par_name)

    READ(par_value_char,*) par_value
    PRINT '(A20,A,I20)',par_name,' = ',par_value

    CALL set_int_par_file(par_name,par_value)
  END SUBROUTINE get_int_par_env

  SUBROUTINE get_int_from_env(par_name,par_value)
    CHARACTER(*),INTENT(IN) :: par_name
    INTEGER     ,INTENT(OUT):: par_value
    CHARACTER(50)           :: par_value_char
    INTEGER                 :: len, status

    CALL GET_ENVIRONMENT_VARIABLE(par_name, par_value_char, len, status)
    IF (status.EQ.1) CALL error ("Unset enviroment variable "//par_name)

    READ(par_value_char,*) par_value
  END SUBROUTINE get_int_from_env

  SUBROUTINE set_int_par_file(par_name,par_value)
    CHARACTER(*):: par_name
    INTEGER     :: par_value
    OPEN (190,FILE='int_par.dat',FORM='formatted')
    write(190,*) par_value
    CLOSE(190)
    CALL system('mv int_par.dat '//par_name//'.cfg')
  END SUBROUTINE set_int_par_file

  !=======================================================================
  !> @brief REAL IO subroutines
  !=======================================================================
  SUBROUTINE get_real_from_env(par_name,par_value)
    CHARACTER(*),INTENT(IN) :: par_name
    REAL(r_dble),INTENT(OUT):: par_value
    CHARACTER(50)           :: par_value_char
    INTEGER                 :: len, status

    CALL GET_ENVIRONMENT_VARIABLE(par_name,par_value_char, len, status)
    IF (status.EQ.1) CALL error ("Unset enviroment variable "//par_name)

    READ(par_value_char,*) par_value
  END SUBROUTINE get_real_from_env

  SUBROUTINE get_real_par_env(par_name,par_value)
    CHARACTER(*),INTENT(IN) :: par_name
    REAL(r_dble),INTENT(OUT):: par_value
    CHARACTER(50)           :: par_value_char
    INTEGER                 :: len, status

    CALL GET_ENVIRONMENT_VARIABLE(par_name,par_value_char, len, status)
    IF (status.EQ.1) CALL error ("Unset enviroment variable "//par_name)
    READ(par_value_char,*) par_value

    PRINT '(A20,A,F20.3)', par_name,' = ',par_value

    CALL set_real_par_file(par_name,par_value)
  END SUBROUTINE get_real_par_env

  SUBROUTINE set_real_par_file(par_name,par_value)
    CHARACTER(*),INTENT(IN) :: par_name
    REAL(r_dble),INTENT(IN) :: par_value
    OPEN (190,FILE='real_par.dat',FORM='formatted')
    write(190,*) par_value
    CLOSE(190)
    CALL system('mv real_par.dat '//par_name//'.cfg')
  END SUBROUTINE set_real_par_file

  SUBROUTINE get_int_par_file(par_name,par_value)
    CHARACTER(*)       :: par_name
    INTEGER,INTENT(OUT):: par_value
    OPEN (190,FILE=par_name//'.cfg',FORM='formatted')
    READ (190,*) par_value
    CLOSE(190)
    PRINT '(A15,A,I20)',par_name,' = ',par_value
  END SUBROUTINE get_int_par_file





  ! dEPRECATED STUFF


  ! SUBROUTINE get_real_par_file(par_name,par_value)
  !   CHARACTER(*)            :: par_name
  !   REAL(r_dble),INTENT(OUT):: par_value
  !   OPEN (190,FILE=par_name//'.dat',FORM='formatted')
  !   READ (190,*) par_value
  !   CLOSE(190)
  !   PRINT '(A15,A,F8.3)',par_name,' = ',par_value
  ! END SUBROUTINE get_real_par_file

  ! SUBROUTINE com_write_1Dfield_4b(filename,points,varname,var)
  !   CHARACTER(*),INTENT(IN):: filename !, DESCRIP
  !   INTEGER     ,INTENT(IN):: points
  !   CHARACTER(*),INTENT(IN):: varname
  !   REAL(r_sngl),INTENT(IN):: var(:)

  !   OPEN (210,FILE=filename//'.grd',FORM='unformatted')
  !   WRITE(210) REAL(var,r_sngl)
  !   CLOSE(210)

  !   OPEN (210,FILE=filename//'.ctl',FORM='FORMATTED')
  !   WRITE(210,'(3A)'    )'DSET ^',filename,'.grd'
  !   !   WRITE(210,'(A)'     )'TITLE LORENZ96 MODEL OUTPUT'
  !   WRITE(210,'(A)'     )'UNDEF -9.99E33'
  !   WRITE(210,'(A)'     )'OPTIONS sequential'
  !   WRITE(210,'(A,I6,A)')'XDEF ',points,' LINEAR 1 1'
  !   WRITE(210,'(A)'     )'YDEF 1 LINEAR 1 1'
  !   WRITE(210,'(A)'     )'ZDEF 1 LINEAR 1 1'
  !   WRITE(210,'(A,I6,A)')'TDEF 1 LINEAR 00Z01JAN2001 1HR'
  !   WRITE(210,'(A)'     )'VARS 1'
  !   WRITE(210,'(3A)'    ) varname, ' 1 99 ', varname
  !   WRITE(210,'(A)'     )'ENDVARS'
  !   CLOSE(210)
  ! END SUBROUTINE com_write_1Dfield_4b


  ! SUBROUTINE com_write_1Dfield_vs_t_4byte(var,varname,points,snapshots)
  !   INTEGER     ,INTENT(IN):: points
  !   INTEGER     ,INTENT(IN):: snapshots
  !   CHARACTER(*),INTENT(IN):: varname
  !   REAL(r_dble),INTENT(IN):: var(:,:)
  !   CHARACTER(60)          :: filename !, DESCRIP
  !   INTEGER                :: i

  !   filename = varname//'vs_t_4byte'

  !   OPEN (210,FILE=TRIM(filename)//'.dat',FORM='unformatted')
  !   DO i=1,snapshots
  !      !    WRITE(410) REAL(parm_infl(:,i),r_sngl)
  !      WRITE(210) REAL(var(:,i),r_sngl)
  !   END DO
  !   CLOSE(210)

  !   OPEN (210,FILE=TRIM(filename)//'.ctl',FORM='FORMATTED')
  !   WRITE(210,'(2A)'    )'DSET ^',TRIM(filename)//'.dat'
  !   !   WRITE(210,'(A)'     )'TITLE LORENZ96 MODEL OUTPUT'
  !   WRITE(210,'(A)'     )'UNDEF -9.99E33'
  !   WRITE(210,'(A)'     )'OPTIONS sequential'
  !   WRITE(210,'(A,I6,A)')'XDEF ',points,' LINEAR 1 1'
  !   WRITE(210,'(A)'     )'YDEF 1 LINEAR 1 1'
  !   WRITE(210,'(A)'     )'ZDEF 1 LINEAR 1 1'
  !   WRITE(210,'(A,I6,A)')'TDEF ',snapshots,' LINEAR 00Z01JAN2001 1HR'
  !   WRITE(210,'(A)'     )'VARS 1'
  !   WRITE(210,'(3A)'    ) varname, ' 1 99 ', varname
  !   WRITE(210,'(A)'     )'ENDVARS'
  !   CLOSE(210)
  ! END SUBROUTINE com_write_1Dfield_vs_t_4byte


  ! SUBROUTINE com_write_var_vs_t_4byte(filename,snapshots,varname,var)
  !   CHARACTER(*),INTENT(IN):: filename !, DESCRIP
  !   INTEGER     ,INTENT(IN):: snapshots
  !   CHARACTER(*),INTENT(IN):: varname
  !   REAL(r_dble),INTENT(IN):: var(:)
  !   INTEGER                :: i

  !   OPEN(210,FILE=filename//'_4byte.dat',FORM='unformatted')
  !   DO i=1,snapshots
  !      WRITE(210) REAL(var(i),r_sngl)
  !   END DO
  !   CLOSE(210)
  !   CALL com_set_var_vs_t_ctl(filename//'_4byte',snapshots,varname)
  ! END SUBROUTINE com_write_var_vs_t_4byte


  ! SUBROUTINE com_read_var_vs_t_4byte(filename,snapshots,var)
  !   CHARACTER(*),INTENT(IN) :: filename !, DESCRIP
  !   INTEGER     ,INTENT(IN) :: snapshots
  !   ! REAL(r_sngl):: var_4byte_tmp
  !   REAL(r_sngl),INTENT(OUT):: var(:)
  !   INTEGER                 :: i

  !   OPEN(910,FILE=filename,FORM='unformatted')
  !   DO i=1,snapshots
  !      READ(910) var(i)
  !   END DO
  !   CLOSE(910)
  ! END SUBROUTINE com_read_var_vs_t_4byte


  ! SUBROUTINE com_read_1Dfield_vs_t_4byte(filename,snapshots,var)
  !   CHARACTER(*),INTENT(IN) :: filename !, DESCRIP
  !   INTEGER     ,INTENT(IN) :: snapshots
  !   ! REAL(r_sngl):: var_4byte_tmp
  !   REAL(r_sngl),INTENT(OUT):: var(:,:)
  !   INTEGER                 :: i
  !   OPEN(910,FILE=filename,FORM='unformatted')
  !   DO i=1,snapshots
  !      READ(910) var(:,i)
  !   END DO
  !   CLOSE(910)
  ! END SUBROUTINE com_read_1Dfield_vs_t_4byte


  ! SUBROUTINE com_set_var_vs_t_ctl(filename,snapshots,varname)
  !   CHARACTER(*),INTENT(IN):: filename !, DESCRIP
  !   INTEGER     ,INTENT(IN):: snapshots
  !   CHARACTER(*),INTENT(IN):: varname

  !   OPEN (11,FILE=filename//'.ctl',FORM='FORMATTED')
  !   WRITE(11,'(3A)'    )'DSET ^',trim(filename),'.grd'
  !   !   WRITE(11,'(A)'     )'TITLE LORENZ96 MODEL OUTPUT'
  !   WRITE(11,'(A)'     )'UNDEF -9.99E33'
  !   WRITE(11,'(A)'     )'OPTIONS sequential'
  !   WRITE(11,'(A)'     )'XDEF 1 LINEAR 1 1'
  !   WRITE(11,'(A)'     )'YDEF 1 LINEAR 1 1'
  !   WRITE(11,'(A)'     )'ZDEF 1 LINEAR 1 1'
  !   WRITE(11,'(A,I6,A)')'TDEF ',snapshots,' LINEAR 00Z01JAN2001 1HR'
  !   WRITE(11,'(A)'     )'VARS 1'
  !   WRITE(11,'(3A)'    ) varname, ' 1 99 ', varname
  !   WRITE(11,'(A)'     )'ENDVARS'
  !   CLOSE(11)
  ! END SUBROUTINE com_set_var_vs_t_ctl

  ! SUBROUTINE com_set_binary_ctl(binaryname,varname,nx,nt)
  !   CHARACTER(*),INTENT(IN):: binaryname
  !   INTEGER     ,INTENT(IN):: nx,nt
  !   CHARACTER(*),INTENT(IN):: varname

  !   OPEN (11,FILE=binaryname//'.ctl',FORM='FORMATTED')
  !   WRITE(11,'(3A)'    )'DSET ^',trim(binaryname),'.grd'
  !   !   WRITE(11,'(A)'     )'TITLE LORENZ96 MODEL OUTPUT'
  !   WRITE(11,'(A)'     )'UNDEF -9.99E33'
  !   WRITE(11,'(A)'     )'OPTIONS sequential'
  !   WRITE(11,'(A,I4,A)')'XDEF ',nx,' LINEAR 1 1'
  !   WRITE(11,'(A)'     )'YDEF 1 LINEAR 1 1'
  !   WRITE(11,'(A)'     )'ZDEF 1 LINEAR 1 1'
  !   WRITE(11,'(A,I6,A)')'TDEF ',nt,' LINEAR 00Z01JAN2001 1HR'
  !   WRITE(11,'(A)'     )'VARS 1'
  !   WRITE(11,'(3A)'    ) varname, ' 1 99 ', varname
  !   WRITE(11,'(A)'     )'ENDVARS'
  !   CLOSE(11)
  ! END SUBROUTINE com_set_binary_ctl

END MODULE common_tools
