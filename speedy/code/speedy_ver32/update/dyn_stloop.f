      SUBROUTINE STLOOP (ISTEP)
C--
C--   SUBROUTINE STLOOP (ISTEP)
C--
C--   Purpose: Perform a series of time steps calling 
C--            post-processing/output routines at selected steps
C--   Input/output : ISTEP = time step index
C--   Updated common block : LFLAG2
C-- 

      include "com_tsteps.h"
      include "com_lflags.h"
c      include "com_date.h"
 
      include "com_iitest.h"   ! Verbosity

c     One 6 hour cycle   

      DO J=1,NSTEPS/4
    
         if (iitest.eq.1) print*, 'ISTEP = ', ISTEP
    
C        Set logical flags
    
         LRADSW = (MOD(ISTEP,NSTRAD).EQ.1)
         LRANDF = ((ISTEP.LE.NSTRDF).OR.(NSTRDF.LT.0))
    
C        Perform one leapfrog time step
    
         if (iitest.eq.1) print*, 'STLOOP: calling STEP'
         CALL STEP (2,2,DELT2,ALPH,ROB)   
    
C        Do diagnostic, post-processing and I/O tasks 
     
         if (iitest.eq.1) print*, 'STLOOP: calling DIAGNS'
         CALL DIAGNS (2,ISTEP)

C        Increment time-averaged variables

         if (iitest.eq.1) print*, 'STLOOP: calling TA_VARS'
         CALL TA_VARS (1,ISTEP)

C         post-processing and I/O tasks 

	  IF (MOD(ISTEP,NSTPPR).EQ.0) THEN
            if (iitest.eq.1) PRINT *, ' ISTEP = ',ISTEP
	    CALL TMINC
	  END IF

c         if (iitest.eq.1) print*, 'STLOOP: calling TMINC_DAILY'
c         CALL TMINC_DAILY
            
c         IF (IHOUT.NE.1) THEN !For 6hr runs time-mean are not calculated nor written
c            IF (MOD(ISTEP,NSTPPR).EQ.0) CALL TMINC
c            IF (MOD(ISTEP,NSTOUT).EQ.0) CALL TMOUT (1)
c            IF (MOD(ISTEP,NSTEPS).EQ.0) CALL TMOUT_DAILY (1)
c         END IF                                                      !TM
    
         ISTEP = ISTEP + 1
    
      END DO
              
      RETURN
      END
      
