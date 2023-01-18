!=======================================================================
!> @brief
!> @author Walter Acevedo (15.08.2013)
!=======================================================================
PROGRAM gather_monthly_means
  !$USE OMP_LIB
  USE common
  USE common_speedy
  USE common_obs_speedy
  USE common_tools

  IMPLICIT NONE
  CHARACTER(200):: file_1 = 'monthly_means_list.dat'
  INTEGER       :: unit_1 = 341
  CHARACTER(200):: file_2
  INTEGER       :: unit_2 = 342
  CHARACTER(200):: file_3 = 'nature_monthly_means.rst'
  INTEGER       :: unit_3 = 343
  CHARACTER(200):: file_4 = 'nature_monthly_means__min.rst'
  INTEGER       :: unit_4 = 344
  CHARACTER(200):: file_5 = 'nature_monthly_means__max.rst'
  INTEGER       :: unit_5 = 345
  CHARACTER(200):: file_6 = 'nature_monthly_means__mean.rst'
  INTEGER       :: unit_6 = 346
  CHARACTER(200):: file_7 = 'nature_monthly_means__stdd.rst'
  INTEGER       :: unit_7 = 347
  INTEGER       :: run_length,cycle_length
  INTEGER       :: ios
  LOGICAL,PARAMETER:: verb = .false.

  CALL get_int_from_env('run_length',run_length)
  CALL get_int_from_env('cycle_length',cycle_length)
  if(verb) call monitor_intg_0D(run_length,'run_length')
  if(verb) call monitor_intg_0D(cycle_length,'cycle_length')

  CALL create_common_file

  CALL create_min_max_file

  STOP

CONTAINS

  SUBROUTINE create_common_file
    REAL*4         :: array2D(NGP,NS2D)
    integer        :: j
    character(200) :: func_name = 'create_common_file'

    if(verb) call funct_opening(func_name)

    OPEN(unit_1,FILE=file_1,FORM=  'formatted',status='old')
    OPEN(unit_3,FILE=file_3,FORM='unformatted',ACCESS='sequential', status='new')

    do
       read(unit_1,'(A)',IOSTAT=ios) file_2
       IF(ios /= 0) EXIT

       OPEN(unit_2,FILE=file_2,FORM='unformatted',ACCESS='sequential', status='old')

       do j=1,cycle_length

          CALL get_monthly_array(unit_2,array2D)

          CALL put_monthly_array(unit_3,array2D)

       enddo

       CLOSE(unit_2)

    enddo

    close(unit_1)
    close(unit_3)

    if(verb) call funct_closing(func_name)
  END SUBROUTINE create_common_file

  SUBROUTINE create_min_max_file
    REAL*4,dimension(NGP,NS2D) :: array2D_4b
    REAL*8,dimension(NGP,NS2D) &
         &         :: array2D    ,array2D_mean,array2D_stdd, array2D_var, &
         &            array2D_min,array2D_max ,array2D_sum1, array2D_sum2
    integer        :: j
    character(200) :: func_name = 'create_min_max_file'

    if(verb) call funct_opening(func_name)

    array2D_sum1 = 0.0
    array2D_sum2 = 0.0

    OPEN(unit_3,FILE=file_3,FORM='unformatted',ACCESS='sequential', status='old')


    if(verb) call print_msg('Accumulating stats')
    !     if(verb) call system('ls -lh')

    do j=1,run_length
       if(verb) call monitor_intg_0D(j,'j')

       call get_monthly_array(unit_3,array2D_4b)
       array2D = array2D_4b

       array2D_sum1 = array2D_sum1 + array2D
       array2D_sum2 = array2D_sum2 + array2D*array2D

       if(j.EQ.1) then
          array2D_min = array2D
          array2D_max = array2D
       else
          array2D_min = min(array2D_min,array2D)
          array2D_max = max(array2D_max,array2D)
       endif

    enddo

    close(unit_3)

    if(verb) call monitor_real_2D(array2D_sum1,'array2D_sum1')
    if(verb) call monitor_real_2D(array2D_sum2,'array2D_sum2')

    array2D_mean =   array2D_sum1 / real(run_length)
    if(verb) call monitor_real_2D(array2D_mean,'array2D_mean')

    array2D_var  = ( array2D_sum2 - run_length * array2D_mean ** 2.0d0) / (run_length - 1)
    if(verb) call monitor_real_2D(array2D_var,'array2D_var')

    ! ABS is needed for rounding errors can yield small negative variance values
    array2D_stdd = SQRT(ABS(array2D_var))
    if(verb) call monitor_real_2D(array2D_stdd,'array2D_stdd')

    if(verb) call print_msg('Creating stat files')

    CALL save_monthly_means_array_8b(unit_4,file_4,array2D_min)
    CALL save_monthly_means_array_8b(unit_5,file_5,array2D_max)
    CALL save_monthly_means_array_8b(unit_6,file_6,array2D_mean)
    CALL save_monthly_means_array_8b(unit_7,file_7,array2D_stdd)

    IF(verb) call funct_closing(func_name)

  END SUBROUTINE create_min_max_file

END PROGRAM gather_monthly_means
