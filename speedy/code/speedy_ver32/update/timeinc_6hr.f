C     Time Increment function for 360 day Calendar

c      SUBROUTINE TIMEINC_6HR
      
c      include "com_date.h"
      
c      IHOUR = IHOUR + 6
      
c      ! every midnight date variables are updated
c      IF (IHOUR.GE.24) THEN
      
c        IHOUR = IHOUR - 24
c        IDATE = IDATE + 1

c        IF (IDATE .GT. 30) THEN
c            IDATE  = 1
c            IMONTH = IMONTH + 1
c        END IF
        
c        IF (IMONTH .GT. 12) THEN
c            IMONTH = 1
c            IYEAR  = IYEAR + 1
c        END IF
        
c      END IF 
      
c      ITIME = IHOUR + IDATE*100 + IMONTH*10000 + IYEAR*1000000
       
c      END


cC     Time Increment function for Gregorian Calendar

      SUBROUTINE TIMEINC_6HR
      
      include "com_date.h"
      
      IHOUR = IHOUR + 6
      
      IF (IHOUR.GT.23) THEN
      
        ! every midnight date variables are updated
        IHOUR = IHOUR - 24
        IDATE = IDATE + 1
        !
        !  Calendar system
        !
        IF (IMONTH == 2) THEN            ! February

          IF (MOD(IYEAR,4) == 0) THEN    ! Leap year                   
            IF(IDATE == 30) THEN                                  !TM
              IDATE  = 1                                          !TM
              IMONTH = IMONTH + 1                                 !TM
            END IF                                                !TM
          ELSE                           ! Normal year
            IF(IDATE == 29) THEN                                  !TM
              IDATE  = 1                                          !TM
              IMONTH = IMONTH + 1                                 !TM
            END IF                                                !TM
          END IF  

        ELSE IF(IMONTH==4.OR.IMONTH==6.OR.IMONTH==9.OR.IMONTH==11) THEN 
          
          IF (IDATE == 31) THEN                                   !TM
            IDATE  = 1                                            !TM
            IMONTH = IMONTH + 1                                   !TM
          END IF                                                  !TM
        
        ELSE                                                      !TM
        
          IF (IDATE == 32) THEN                                   !TM
            IDATE  = 1                                            !TM
            IMONTH = IMONTH + 1                                   !TM
          END IF                                                  !TM
        
        END IF                                                    !TM
        
        IF (IMONTH == 13) THEN                                    !TM
          IMONTH = 1                                              !TM
          IYEAR  = IYEAR + 1                                      !TM
        END IF                                                    !TM
        
      END IF 
      
      ITIME = IHOUR + IDATE*100 + IMONTH*10000 + IYEAR*1000000
       
      END
