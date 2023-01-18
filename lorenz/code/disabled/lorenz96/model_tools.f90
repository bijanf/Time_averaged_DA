!=======================================================================
! Lorenz96 model dynamics definition
!=======================================================================
MODULE model_tools
  USE common
  USE common_particle
  USE model_core
  IMPLICIT NONE

CONTAINS

  SUBROUTINE model_write_state_8byte(out_unit)
    INTEGER,INTENT(IN)  :: out_unit
    WRITE(out_unit) model_state(1:nx)
  END SUBROUTINE model_write_state_8byte

  SUBROUTINE model_write_state_4byte(out_unit)
    INTEGER,INTENT(IN)  :: out_unit
    WRITE(out_unit) REAL(model_state(1:nx),r_sngl)
  END SUBROUTINE model_write_state_4byte

  SUBROUTINE model_set_state_4byte_ctl(filename,snapshots)
    CHARACTER(*),INTENT(IN):: filename !, DESCRIP
    INTEGER ,INTENT(IN)  :: snapshots
    !  REAL    ,INTENT(IN)  :: dt

    OPEN (11,FILE=filename//'.ctl',FORM='FORMATTED')
    WRITE(11,'(A)'     )'DSET ^'//filename//'.dat'
    !  WRITE(11,'(A)'     )'TITLE LORENZ96 MODEL OUTPUT'
    WRITE(11,'(A)'     )'UNDEF -9.99E33'
    WRITE(11,'(A)'     )'OPTIONS sequential'
    WRITE(11,'(A)'     )'XDEF 40 LINEAR 1 1'
    WRITE(11,'(A)'     )'YDEF  1 LINEAR 1 1'
    WRITE(11,'(2A)'    )'ZDEF  1 LINEAR 1 1'
    WRITE(11,'(A,I6,A)')'TDEF ', snapshots, ' LINEAR 00Z01JAN2001 1HR'
    WRITE(11,'(A,I1.1)')'VARS 1'
    WRITE(11,'(A)'     )'C1 1 99 C1'
    WRITE(11,'(A)'     )'ENDVARS'
    CLOSE(11)
    RETURN
  END SUBROUTINE model_set_state_4byte_ctl

END MODULE model_tools
