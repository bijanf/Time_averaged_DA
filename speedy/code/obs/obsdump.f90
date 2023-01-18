PROGRAM dump_obs
    IMPLICIT NONE
    REAL(4) :: wk(6)
    INTEGER :: ios
    CHARACTER(1) :: S
    CHARACTER(50):: obs_file
    INTEGER      :: n_args
    
    n_args = IARGC()
    IF (n_args /= 1) then
        WRITE(0,*) 'Error. Incorrect arguments number'
        WRITE(0,*) 'Usage: $ dump_obs obs_file'
        CALL EXIT (1)
    END IF 
     
    CALL GETARG(1,obs_file)
  
    OPEN(3,FILE=obs_file,FORM='unformatted')
    DO
        READ(3,IOSTAT=ios) wk
        IF(ios /= 0) THEN
            PRINT '(A)','END OF FILE'
            EXIT
        END IF
        PRINT '(I6,2F7.2,F10.2,2ES12.2)',NINT(wk(1)),wk(2),wk(3),wk(4),wk(5),wk(6)
        PRINT '(A)','PRESS "S" TO STOP'
        READ(5,'(A1)') S
        IF(S == 'S') EXIT
    END DO
    CLOSE(3)
    STOP
END PROGRAM dump_obs
