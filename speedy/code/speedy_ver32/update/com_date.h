C
C--:  /DATE1/: date and time variables (updated in FORDATE)
C--:  IYEAR IMONTH IDAY IDATE, IHOUR = Present time date
C--:  SIXHRRUN  = Six hour run  flag, (set to 1 when NDAYSL = 0)
C--:  FDAY , TYEAR,

!     common /DATE1/ IYEAR, IMONTH, IDAY, FDAY, TYEAR                    !TM
      common /DATE1/ IYEAR, IMONTH, IDAY, FDAY, TYEAR, IDATE, IHOUR,     !TM
     &             SIXHRRUN, ITIME, MIN_TIME, MAX_TIME
C     FYEAR, FMONTH, FDATE, FHOUR
