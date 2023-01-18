      SUBROUTINE INIALL
C--
C--   SUBROUTINE INIALL
C--
C--   Purpose : Call initializion routines for all model common blocks 
C--
      include "atparam.h"
      include "atparam1.h"
      include "par_tmean.h"

      include "com_tsteps.h"
      include "com_dyncon1.h"

      include "com_outfiles.h"
      include "com_date.h"

      include "com_iitest.h"   ! Verbosity

      REAL PPL(KX)

C--   1. Initialize ffts

      if (iitest.eq.1) print*, 'INIALL: calling INIFFT'
      CALL INIFFT

C     2. Set Restart mode (All restart files in sigma levels)

      READ (2,*) ISTART ! see com_tsteps.h for info
      if (iitest.eq.1) print*, 'ISTART = ', ISTART

      READ (2,*) JSTART  ! see com_tsteps.h for info
      if (iitest.eq.1) print*, 'JSTART = ', JSTART


C--   3. Initialize constants for time-stepping and dynamics

C     3.1 Initialize constants and operators
 
      if (iitest.eq.1) print*, 'INIALL: calling INDYNS'
      CALL INDYNS

C     3.2 Set post-processing levels

      DO K=1,KX
        PPL(K)=PRLEV(FSG(K))
      ENDDO

C     4. Set Initial time

      READ (7,*,END=200) IYEAR
      READ (7,*,END=200) IMONTH
      READ (7,*,END=200) IDATE
      READ (7,*,END=200) IHOUR
      
      ITIME = IHOUR + IDATE*100 + IMONTH*10000 + IYEAR*1000000
      PRINT *, 'Initial time = ', ITIME

C     5. Set Final time

      READ (8,*,END=204) KYEAR
      READ (8,*,END=204) KMONTH
      READ (8,*,END=204) KDATE
      READ (8,*,END=204) KHOUR

      MAX_TIME = KHOUR + KDATE*100 + KMONTH*10000 + KYEAR*1000000
      PRINT *, 'Final time   = ', MAX_TIME
      
      
C--   6. Initialize constants for physical parametrization

      if (iitest.eq.1) print*, 'INIALL: calling INPHYS'
      CALL INPHYS (HSG,PPL,RADANG)

C--   7. Initialize forcing fields (boundary cond. + random forcing)

      if (iitest.eq.1) print*, 'INIALL: calling INFORC'
      CALL INFORC (GRAV)

      if (iitest.eq.1) print*, 'INIALL: calling INIRDF'
      CALL INIRDF (INDRDF)

C--   8. Initialize model variables (atmosphere + surface)

      if (iitest.eq.1) print*, 'INIALL: calling INVARS'
      CALL INVARS

      if (iitest.eq.1) print*, 'INIALL: calling SFC_IN'
      CALL SFC_IN

C--   9. Set time-averaged variables /DYNSP3/ to zero

      if (iitest.eq.1) print*, 'INIALL: calling TA_VARS'
      CALL TA_VARS (0,ISTEP)

C--   6. Initialize time-mean arrays
       
      if (iitest.eq.1) print*, 'calling TMOUT'
      CALL TMOUT (0)
      
c      IF (IDOUT .GT. 0)  CALL TMOUT_DAILY (0)
 
C--   7. Set up the time-mean and daily-mean output (grads format)

C     READ(2,'(A3)') NORUN
      NORUN = '000'

c      NTM = ((NMONTS-1)*30+NDAYSL)*NSTEPS/NSTOUT
c      NDM = ((NMONTS-1)*30+NDAYSL)
c      NDAYTM = NSTOUT/NSTEPS
c      NDAYDM = 1

      if (iitest.eq.1) print*, 'INIALL: calling SETCTL'

c      !IS3D=1
c      !CALL SETCTL(12,IX,IL,KX,NTM,NDAYTM,IS3D,NS3D1,NS2D,NS2D_D-NS2D2,
c     !*             RADANG,PPL,'attm',NORUN,IYEAR0,IMONT0)

C--   Daily-mean

c      IF(IDOUT .gt. 0 .and. IDOUT .le. 3 ) then
c        CALL SETCTL_DAILY (18,IX,IL,KX,NDM,NDAYDM,IS3D,NS3D_D,NS2D_D,
c     *                      RADANG,PPL,'daytm',NORUN,IYEAR0,IMONT0)
c      ENDIF
C--
c      !IS3D=IS3D+NS3D1
c      !CALL SETCTL (14,IX,IL,KX,NTM,NDAYTM,IS3D,NS3D2,0,0,
c     !*               RADANG,PPL,'atva',NORUN,IYEAR0,IMONT0)

c     !IS3D=IS3D+NS3D2
c      !CALL SETCTL (16,IX,IL,KX,NTM,NDAYTM,IS3D,NS3D3,0,0,
c     !*               RADANG,PPL,'atdf',NORUN,IYEAR0,IMONT0)
     
      IF (JSTART.EQ.2) THEN ! Gridded output restart
c      
c        CALL IOGRID (5) ! write a GrADS control file (sigma levels)
c        
        IF (IPOUT.EQ.1) THEN                           !TM
c          CALL IOGRID (3) ! write a GrADS control file (p levels)
          CALL GEOP (1)
        END IF
        
      END IF                                                            !TM
C--
      RETURN

 200  PRINT *, ' Problem with the initial time file (fort.2)'
      STOP 1
 204  PRINT *, ' Problem with the final time file (fort.4)'
      STOP 1

      
      END
  

      FUNCTION PRLEV (SIGLEV)
C--									
C--   FUNCTION PRLEV (SIGLEV)
C--   Purpose : select the closest standard pressure level for post-proc.
C--   Input :   SIGLEV = sigma level

c      REAL PLEV(16)

c      DATA PLEV/ 0.925, 0.85, 0.775, 0.7, 0.6, 0.5, 0.4, 0.3,  
c     *           0.25, 0.2, 0.15, 0.10, 0.07, 0.05, 0.03, 0.01/

      REAL PLEV(12)

      DATA PLEV/ 0.925, 0.850, 0.775, 0.700, 0.600, 0.500, 0.400,  
     *           0.300, 0.250, 0.200, 0.150, 0.100/

      PRLEV = 1.
      DIF = 1.-SIGLEV

      DO K=1,16
        ADIF = ABS(PLEV(K)-SIGLEV)
        IF (ADIF.LE.DIF) THEN
          DIF = ADIF
          PRLEV = PLEV(K)
        ENDIF
      ENDDO

      RETURN
      END



      

