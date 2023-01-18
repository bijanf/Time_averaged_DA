C>======================================================================
C> @brief Writes a GrADS control file for a gridded state.
C>  
C> @param[in]  LEVELS    = 'sigma' | 'press'
C> @param[in]  TIME_KIND = 'Insta' | 'Taver' | 'Tanom'
C> @return     0 if successful or 1 if failed
C>
C> @note Template option was disabled due to 360 day calendar Grads 
C>       support lack. 
C=======================================================================
      SUBROUTINE SET_CTL_GRID (LEVELS,TIME_KIND,ILEAP,DESCRIP)

      include "atparam.h"
      include "atparam1.h"
      PARAMETER (NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT)
      include "com_date.h"
      include "com_tsteps.h"
      include "com_dyncon1.h"
      include "com_physcon.h"  ! P0
      include "com_iitest.h"   ! Verbosity
      
      CHARACTER(5),INTENT(IN)  :: LEVELS,TIME_KIND
      CHARACTER(*),INTENT(IN)  :: DESCRIP
      INTEGER     ,INTENT(IN)  :: ILEAP
      CHARACTER(50)             :: Z_LEVELS
      CHARACTER(50)             :: ctl_name, rst_name_end
      CHARACTER(10)             :: itime_str
      INTEGER                   :: INCR, N_VARS
      CHARACTER(2)              :: INC_UNIT
      character(3),dimension(12):: SMONTH = (/'JAN','FEB','MAR','APR',
     &                                        'MAY','JUN','JUL','AUG',
     &                                        'SEP','OCT','NOV','DEC'/)

C     Setting present time string
      WRITE (itime_str(1:10),'(I10)') ITIME
      
C     Setting filenames
      ctl_name = itime_str//'_grid_'//LEVELS//'_'//TIME_KIND//'.ctl'
      rst_name_end =        '_grid_'//LEVELS//'_'//TIME_KIND//'.rst'
      
C     Setting file description

c      SELECT CASE (TIME_KIND)
c         CASE ('Insta')
c            descrip = 'Speedy Model Prognostic Variables'
c         CASE ('Taver')
c            descrip = 'Time-averaged Speedy Model Prognostic Variables'
c         CASE ('Tanom')
c            descrip ='Time-averaged anomaly Speedy prognostic variables'
c         CASE DEFAULT
c            WRITE(0,*) 'Error: Unsupported TIME_KIND ', TIME_KIND
c            CALL EXIT (1)
c      END SELECT

C     Setting variables number and vertical levels

      SELECT CASE (LEVELS)
         CASE ('press')
            N_VARS = 7
            WRITE(Z_LEVELS,'(A)')    '925 850 700 500 300 200 100'
         CASE ('sigma')
            N_VARS = 6
            WRITE(Z_LEVELS,'(7F6.3)')(SIG(K),K=7,1,-1)
         CASE DEFAULT
            WRITE(0,*) 'Error: Unsupported LEVELS ', LEVELS
            CALL EXIT (1)
      END SELECT 

C     Finding appropiate GRADS time increment

      IF (ILEAP.LT.96) THEN
        INCR = ILEAP
        INC_UNIT='hr'
      ELSE IF(ILEAP.LT.(24*99)) THEN
        INCR = ILEAP/24
        INC_UNIT='dy'
        IF (mod(ILEAP,24) .NE. 0) THEN
            WRITE(0,*)
     &      'Error. If larger than 96 hr, ILEAP must be multiple of 24'
            CALL EXIT (1)
        END IF
      ELSE
        INCR = 6
        INC_UNIT='hr'
        write(*,*) 'Warning: ILEAP longer than 99 days!!'
        write(*,*) 'Ctl file time increment just set to 6hr'
      END IF  
        
C     Previous config for 360 day calendar
c      IF (ILEAP.LT.96) THEN
c        INCR = ILEAP
c        INC_UNIT='hr'
c      ELSE IF(ILEAP.LT.(24*99)) THEN
c        INCR = ILEAP/24
c        INC_UNIT='dy'
c        IF (mod(ILEAP,24) .NE. 0) THEN
c            WRITE(0,*)
c     &      'Error. If larger than 96 hr, ILEAP must be multiple of 24'
c            CALL EXIT (1)
c        END IF
c      ELSE IF(ILEAP.LT.(24*30*99)) THEN
c        INCR = ILEAP/(24*30)
c        INC_UNIT='mn'
c        IF (mod(ILEAP,(24*30)) .NE. 0) THEN
c            WRITE(0,*)
c     &  'Error. If larger than 99 days, ILEAP must be multiple of 24*30'
c            CALL EXIT (1)
c        END IF
c      ELSE
c        WRITE(0,*) 'Error. Unsuported ILEAP value ', ILEAP 
c        CALL EXIT (1)
c      END IF

c     Writing control file

      OPEN (11,FILE=ctl_name,FORM='FORMATTED')
      WRITE(11,'(A)'       )'DSET ^%y4%m2%d2%h2'//rst_name_end
      WRITE(11,'(A)'       )'TITLE '//DESCRIP
      WRITE(11,'(A)'       )'UNDEF -9.99E33'
      WRITE(11,'(A)'       )'OPTIONS template big_endian'
      WRITE(11,'(A)'       )'XDEF 96 LINEAR 0.0 3.75'
      WRITE(11,'(A,48F8.3)')'YDEF 48 LEVELS ',
     &                           (RADANG(J)*90.0d0/ASIN(1.0d0),J=1,48)
      WRITE(11,'(2A)'      )'ZDEF 7 LEVELS ',Z_LEVELS
      WRITE(11,'(A,I4,A,I2.2,A,I2.2,A,I4.4,A,I2.2,A)'
     &                      )'TDEF ', 9999, ' LINEAR ', IHOUR, 'Z',IDATE
     &                       ,SMONTH(IMONTH), IYEAR,' ', INCR,  INC_UNIT
      WRITE(11,'(A,I1.1)'  )'VARS ', N_VARS
      WRITE(11,'(A)'       )'U 7 99 U-wind [m/s]'
      WRITE(11,'(A)'       )'V 7 99 V-wind [m/s]'
      WRITE(11,'(A)'       )'T 7 99 Temperature [K]'
      WRITE(11,'(A)'       )'Q 7 99 Specific Humidity [kg/kg]'
      IF (LEVELS .EQ.'press') THEN
        WRITE(11,'(A)'     )'Z 7 99 Geopotential Height [m]'
      END IF
      WRITE(11,'(A)'       )'PS 0 99 Surface Pressure [Pa]'
      WRITE(11,'(A)'       )'RAIN 0 99 Precipitation [mm/6hr]'
      WRITE(11,'(A)'       )'ENDVARS'
      CLOSE(11)

      RETURN
      END
      
