!=======================================================================
!> @brief Allows to visualize binary observation files in the terminal
!=======================================================================
PROGRAM dump_obs
    IMPLICIT NONE
    REAL(4)      :: wk(6)
    INTEGER      :: ios,n_obs
!     CHARACTER(1) :: S
    CHARACTER(50):: obs_file
    INTEGER      :: n_args
    
    n_args = IARGC()
    IF (n_args /= 1) then
        WRITE(0,*) 'Error. Incorrect arguments number'
        WRITE(0,*) 'Usage: $ dump_obs obs_file'
        CALL EXIT (1)
    END IF 
     
    CALL GETARG(1,obs_file)

    PRINT *, trim(obs_file), ' observations:'
    PRINT '(A)', '---------------------------------------------------------------------'
    PRINT '(A)', '| Kind id |    lon |    lat |  pressure |       value |       error |'
    PRINT '(A)', '|---------|--------|--------|-----------|-------------|-------------|'
  
    n_obs = 0
    OPEN(3,FILE=obs_file,FORM='unformatted')
    DO
        READ(3,IOSTAT=ios) wk
        IF(ios /= 0) THEN
            EXIT
        END IF
        
        PRINT '(A,        I6,     A,   F7.2, A,   F7.2, A,  F10.2, A,  ES12.2, A,   ES12.2, A)',&
               '|  ',NINT(wk(1)),' |',wk(2),' |',wk(3),' |',wk(4),' |', wk(5),' |',  wk(6),' |'
                  
        n_obs = n_obs + 1
!        PRINT '(A)','PRESS "S" TO STOP'
!        READ(5,'(A1)') S
!        IF(S == 'S') EXIT
    END DO
    PRINT *, '--------------------------------------------------------------------'
    PRINT *, 'Observation number: ',n_obs
    PRINT *, '--------------------------------------------------------------------'
    CLOSE(3)
    STOP
END PROGRAM dump_obs
