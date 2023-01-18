
ISTART = Restart files mode 
VALUE  INPUT     LEVELS  TS  FILE     OUTPUT   LEVELS       TS  FILE 
   0:  REST      sigma   1   None     SPECTRAL sigma        2   fort.10
   1:  SPECTRAL  sigma   2   fort.3   SPECTRAL sigma        2   fort.10
1110:  SPECTRAL  sigma   2   fort.3   GRIDDED  sigma and p  1?
1111:  GRIDDED   sigma   1   fort.90  GRIDDED  sigma and p  1?
1112:  GRIDDED   sigma   1   fort.90  GRIDDED  sigma        1?

TS: Number of time steps present in the restart file


C      LENGTH = FTIME - ITIME
      
c      SIXHRRUN=0
c      IF( ( FTIME - ITIME ) .EQ. 6) THEN
c        PRINT *, 'sIX HOUR RUN'
c        NDAYSL=1
c        SIXHRRUN=1
c        IHOUT=1
c      END IF
    
!     Sanity check 
c      IF (IHOUT.EQ.1.AND.NMONTS.GE.3) THEN                              !TM
c        PRINT *,'You are about to produce 6-hourly output for more',    !TM
c     &          'than 3 months! Check the NMONTS parameter! (AT_GCM)'   !TM
c        STOP                                                            !TM
c      END IF                                                            !TM

cC     Open output files
c      CALL SETGRD (0)                                                   !TM
c      IF (IHOUT.NE.1) CALL SETGRD (0)                                   !TM

c!     Write initial data                                                !TM
c      IF (IHOUT.EQ.1.AND.IPOUT.EQ.1) CALL IOGRID (2)                    !TM
c      IF (IHOUT.EQ.1) CALL IOGRID (4)                                   !TM




c     ISTART  INPUT RESTART  FILE   OUTPUT RESTART  FILE 
c         0:   Rest state   None     SPECTRAL      fort.10
c         1:   Rest state   None     GRIDDED  
c         2:   SPECTRAL     fort.3   SPECTRAL      fort.10
c         3:   SPECTRAL     fort.3   GRIDDED  
c         4:   GRIDDED      fort.90  SPECTRAL      fort.10
c         5:   GRIDDED      fort.90  GRIDDED  
