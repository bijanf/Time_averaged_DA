      SUBROUTINE STEPONE
C--
C--   SUBROUTINE STEPONE
C--
C--   Purpose : call initialization of semi-implicit scheme 
C--             and perform initial time step
C--   Initialized common block : LFLAG2
C--

      include "com_tsteps.h"
      include "com_lflags.h"
  
      include "com_iitest.h"   ! Verbosity
      
      IF (ISTART.EQ.0 .OR. ISTART.EQ.2 .OR. ISTART.EQ.3) THEN   
      
      ! Gridded restart datasets seem to have only one time step  !WA

        DELTH = 0.5*DELT
        LRADSW = .TRUE.
        LRANDF = .FALSE.

        if (iitest.eq.1) print*, 'STEPONE: semi-impl. initialization'
        CALL IMPINT (DELTH,ALPH)

        if (iitest.eq.1) print*, 'STEPONE: forward half-step'
        CALL STEP (1,1,DELTH,ALPH,ROB)

        if (iitest.eq.1) print*, 'STEPONE: semi-impl. initialization'
        CALL IMPINT (DELT,ALPH)

        if (iitest.eq.1) print*, 'STEPONE: leapfrog half-step'
        CALL STEP (1,2,DELT,ALPH,ROB)

      ENDIF

      if (iitest.eq.1) print*, 'STEPONE: semi-impl. initialization'
      CALL IMPINT (DELT2,ALPH)

C-- 
      RETURN
      END
  

