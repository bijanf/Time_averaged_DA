!===================================================================
!> @brief SPEEDY Main File
!>
!> ISTART = Defines where to start from
!> - 0: Reference atmosphere (No restart needed)
!> - 1: Spectral Sigma Instantaneous state (fort.3)       2      3
!> - 2: Gridded  Sigma Instantaneous state (fort.90)
!> - 3: Gridded  Sigma Time-averaged and anomoly states (fort.92 & fort.93)
!>
!> JSTART (Output_flag) = Defines state bynary files to be produced
!> - 1: Spectral Sigma Instantaneous state (fort.10)
!> - 2: Gridded  Sigma and pressure Instantaneous state
!> - 3: equal to 2 plus Gridded Sigma Time-averaged state
!> - 4: equal to 3 plus Gridded Sigma Time-averaged anomaly
!>
!> @note See @ref da_files for details
!===================================================================
      PROGRAM AT_GCM
C--
C--   Main program : AT_GCM
C--
C--   Modified common blocks: DATE1
C--
      include "atparam.h"
      include "atparam1.h"

      include "com_tsteps.h"
      include "com_date.h"
      
      include "com_iitest.h"
      
      iitest = 0 ! Verbosity level 
      
      PRINT *, 'AT_GCM program STARTING'
      if (iitest.eq.1) PRINT *, 'AT_GCM program STARTING'

C-----------------------------------------------------------      

C--   1. Initialization of all model constants and variables 

      if (iitest.eq.1) print*, 'AT_GCM: calling INIALL'
      CALL INIALL

C     set up the forcing fields for the first time step

      if (iitest.eq.1) print*, 'AT_GCM: calling FORDATE'
!      IDAY = IDATE                                                  !WA
!      IF (IDAY.GT.30) IDAY=30                                       !WA
      IDAY = 0                                                     !WA
      CALL FORDATE 

C     do the initial (2nd-order) time step, initialize the semi-impl. scheme

      if (iitest.eq.1) print*, 'AT_GCM: calling STEPONE'
      CALL STEPONE

C     Re-initialize the surface-flux anomalies  

      if (iitest.eq.1) print*, 'AT_GCM: calling SFC_AN'
      CALL SFC_AN (0,0,0,NSTEPS)

C-----------------------------------------------------------      
! 
! C--   2. Storing spectrally filtered initial state
!       
!       IF      (JSTART.EQ.1) THEN
! 
! C--   2.1  Write spectral restart file in sigma levels
! 
!         if (iitest.eq.1) print *, 'AT_GCM: calling RESTART'
!         CALL RESTART (2)
!         
!       ELSE IF (JSTART.EQ.2) THEN
! 
! C--   2.2  Write gridded restart file in sigma levels
! 
!         if (iitest.eq.1) print *, 'AT_GCM: calling IOGRID'
!         CALL IOGRID (4)
!         
! C--   2.3  Write gridded restart file in pressure levels
! 
!         if (iitest.eq.1) print *, 'AT_GCM: calling IOGRID'
!         IF (IPOUT.EQ.1) CALL IOGRID (2)
! 
!       ELSE
! 
!         PRINT *,'Invalid JSTART value : ', JSTART
!         STOP
!       
!       END IF
      
C------------------------------------------------------------ 

C--   3. Main Time loop

      ISTEP = 1          

      DO WHILE ( ITIME .LT. MAX_TIME )
      
C       Modify the forcing fields according to the date

!        IDAY = JDAY                                                   !TM
        IDAY = IDATE                                                  !TM
        IF (IDAY.GT.30) IDAY=30                                       !TM
        IF (IHOUR.EQ.0) CALL FORDATE                                 !WA 
        
C       Integrate the atmospheric model for 6 hours

        if (iitest.eq.1) print *, 'AT_GCM: calling STLOOP'
        CALL STLOOP (ISTEP)

C       advance time 

        CALL TIMEINC_6HR

! 	NSTOUT = 30*36
        IF ((IDATE.EQ.1).AND.(IHOUR.EQ.0)) CALL TMOUT (1)

        
!         if (iitest.eq.1) PRINT *, ' ISTEP = ',ISTEP
        if (iitest.eq.1) PRINT *, ' Present time = ', ITIME
                
C       Integrate the surface anomaly model daily (if requested)

        IF (IHOUR.EQ.0) THEN
          if (iitest.eq.1) print *, 'AT_GCM: calling SFC_AN'
          CALL SFC_AN (IALST,IASST,IAICE,NSTEPS)
        END IF       
            
      ENDDO

C------------------------------------------------------------ 

C--   4. Final Restart storing
      
      IF (JSTART.EQ.1) THEN
C--     Write spectral restart file in sigma levels
        if (iitest.eq.1) print *, 'AT_GCM: calling RESTART'
        CALL RESTART (2)
      END IF

!       READ (9,*) ILEAP
        
      IF (JSTART.GE.2) THEN
C--     Write gridded restart file in sigma levels
        if (iitest.eq.1) print *, 'AT_GCM: calling IOGRID'
        CALL IOGRID (4)
c        CALL IOGRID (5)
c        if (iitest.eq.1) print *, 'AT_GCM: calling SET_CTL_GRID'
c        CALL SET_CTL_GRID ('sigma','Insta',ILEAP,
c     &                              'Speedy Model Prognostic Variables')

        
C--     Write gridded restart file in pressure levels
c         if (iitest.eq.1) print *, 'AT_GCM: calling IOGRID'
c        IF (IPOUT.EQ.1) CALL IOGRID (2)
c        IF (IPOUT.EQ.1) CALL IOGRID (3)
c        CALL IOGRID (2)
c        CALL IOGRID (3)
c        if (iitest.eq.1) print *, 'AT_GCM: calling SET_CTL_GRID'
c        CALL SET_CTL_GRID ('press','Insta',ILEAP,
c     &                              'Speedy Model Prognostic Variables')

      END IF

       
      IF (JSTART.GE.3) THEN
C       Write gridded sigma time-average state to binary file
        if (iitest.eq.1) print*, 'AT_GCM: calling TA_VARS'
        CALL TA_VARS (2,ISTEP)
c        CALL SET_CTL_GRID ('sigma','Taver',ILEAP,
c     &                'Time-averaged Speedy Model Prognostic Variables')
      END IF
        
      IF (JSTART.EQ.4) THEN
C       Write gridded sigma time-average anomaly state to binary file
        if (iitest.eq.1) print*, 'AT_GCM: calling TA_VARS'
        CALL TA_VARS (3,ISTEP)
c        CALL SET_CTL_GRID ('sigma','Tanom',ILEAP,
c     &              'Time-averaged anomaly Speedy prognostic variables')
         

      END IF

      IF ( JSTART.LT.1 .OR. JSTART.GT.4 ) THEN
        PRINT *,'Invalid JSTART value : ', JSTART
        STOP
      END IF

C------------------------------------------------------------ 

!       PRINT '(A,I4.4,A,I2.2,A,I2.2,A,I2.2)',
!      &      ' End of year/month/date/hour = ',
!      &      IYEAR,'/',IMONTH,'/',IDATE,'/',IHOUR

C------------------------------------------------------------ 
      
C      WRITE(87,'(I4.4,I2.2,I2.2,I2.2)') IYEAR,IMONTH,IDATE,IHOUR        !WA

      if (iitest.eq.1) PRINT *, 'AT_GCM program NORMAL END'
      STOP

      END    
      
