      PROGRAM TIME_INCREMENT

      include "com_date.h"
      
      character*(10) time_one, increment_string, increment_unit 
      integer        new_imonth
      real           increment_value
     
      Iarguments = IARGC()
      IF (Iarguments /= 3) then
        WRITE(0,*) 'TIME_INCREMENT Error. 3 arguments needed.'
        CALL EXIT (1)
      END IF 
     
      CALL GETARG(1,time_one)
      CALL GETARG(2,increment_string)
      CALL GETARG(3,increment_unit)
      
      READ( time_one, '(I4,I2,I2,I2)' ) IYEAR, IMONTH, IDATE, IHOUR 
c      READ( increment_string, '(I10)' ) increment_value
      READ(increment_string,*) increment_value
      increment_value = int(increment_value)

      SELECT CASE (TRIM(increment_unit))
      CASE ('hr')
        IF(modulo(increment_value,6.0) .NE. 0) THEN
          WRITE(0,*) 'TIME_INCREMENT Error.'
          WRITE(0,*) 'Increment must be multiple of 6'
          CALL EXIT (1)
        END IF
        n_increments = increment_value / 6
        DO n_inc = 1, n_increments
          CALL TIMEINC_6HR
        END DO

      CASE ('dy')
        n_increments = increment_value * 4
        DO n_inc = 1, n_increments
          CALL TIMEINC_6HR
        END DO      

      CASE ('mo')
        IF (IDATE .GT. 28) THEN
          WRITE(0,*) 'TIME_INCREMENT Error.'
          WRITE(0,*) 'Due to heteregeneous month lenght, ONLY days'
          WRITE(0,*) 'below 28 are allowed when increasing month.'
          CALL EXIT (1)
        END IF
        new_imonth = imonth + increment_value
        imonth = MODULO(new_imonth-1, 12) + 1
        iyear = iyear + floor((new_imonth-1)/12.0)*1
          
      CASE ('yr')
        iyear = iyear + increment_value
          
      CASE DEFAULT
        WRITE(0,*) ' Unknown increment_unit '//TRIM(increment_unit)
        CALL EXIT (1)
      END SELECT
      
      ITIME = IHOUR + IDATE*100 + IMONTH*10000 + IYEAR*1000000
      
      WRITE(*,'(I10)') ITIME
        
      STOP

      END     
