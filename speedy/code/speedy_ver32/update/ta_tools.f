C     ==================================================================
C>    @brief Write sigma gridded 4 byte state variables as binary file 
C     ==================================================================
      SUBROUTINE WRITE_GR4(fname,UGR4,VGR4,TGR4,QGR4,PHIGR4,PSGR4,RRGR4)

      include "atparam.h"
      include "atparam1.h"

      PARAMETER (NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT)

      CHARACTER(*),INTENT(IN) :: fname
      REAL(4),DIMENSION(NGP,KX)        :: UGR4, VGR4,TGR4,QGR4,PHIGR4
      REAL(4),DIMENSION(NGP)           :: PSGR4,RRGR4

      PRINT *, 'Writing Gridded Binary ', fname
      OPEN (96,FILE=fname,FORM='UNFORMATTED',
     &                                    ACCESS='DIRECT', RECL=4*IX*IL)
              
      irec=1
      DO k=KX,1,-1
        WRITE (96,REC=irec) (UGR4(j,k),j=1,NGP)
        irec=irec+1
      END DO
      DO k=KX,1,-1
        WRITE (96,REC=irec) (VGR4(j,k),j=1,NGP)
        irec=irec+1
      END DO
      DO k=KX,1,-1
        WRITE (96,REC=irec) (TGR4(j,k),j=1,NGP)
        irec=irec+1
      END DO
      DO k=KX,1,-1
        WRITE (96,REC=irec) (QGR4(j,k),j=1,NGP)
        irec=irec+1
      END DO
      WRITE   (96,REC=irec)(PSGR4(j)  ,j=1,NGP)
      irec=irec+1
      WRITE   (96,REC=irec)(RRGR4(j)  ,j=1,NGP)
      CLOSE   (96)
      
      RETURN
      END

C     ==================================================================
C>    @brief Monitoring 4 byte state variables
C     ==================================================================
      SUBROUTINE MONITOR_GR4 (UGR4, VGR4,TGR4,QGR4,PHIGR4,PSGR4,RRGR4)
      include "atparam.h"
      include "atparam1.h"
      PARAMETER (NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT)

      REAL(4),DIMENSION(NGP,KX)        :: UGR4, VGR4,TGR4,QGR4,PHIGR4
      REAL(4),DIMENSION(NGP)           :: PSGR4,RRGR4
C--       Monitoring         
      PRINT *, 'Gridded state variables:'
      PRINT *,' UGR4  :',MINVAL( UGR4) ,MAXVAL (UGR4)
      PRINT *,' VGR4  :',MINVAL( VGR4) ,MAXVAL (VGR4)
      PRINT *,' TGR4  :',MINVAL( TGR4) ,MAXVAL (TGR4)
      PRINT *,' QGR4  :',MINVAL( QGR4) ,MAXVAL (QGR4)
      PRINT *,' PSGR4 :',MINVAL(PSGR4) ,MAXVAL(PSGR4)
      
      RETURN
      END


c      SUBROUTINE MONITOR_SPECTRAL_TIME_AVERAGE
c      include "atparam.h"
c      include "atparam1.h"
c      PARAMETER (NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT)
c      include "com_tavars.h"
c      PRINT *, 'Spectral Output Time average state:'
c      PRINT *,' VOR_TA:',MINVAL(ABS(VOR_TA)),MAXVAL(ABS(VOR_TA))
c      PRINT *,' DIV_TA:',MINVAL(ABS(DIV_TA)),MAXVAL(ABS(DIV_TA))
c      PRINT *,'   T_TA:',MINVAL(ABS(  T_TA)),MAXVAL(ABS(  T_TA))
c      PRINT *,'  PS_TA:',MINVAL(ABS( PS_TA)),MAXVAL(ABS( PS_TA))
c      PRINT *,'  TR_TA:',MINVAL(ABS( TR_TA)),MAXVAL(ABS( TR_TA))
c       
c      RETURN
c      END      

C     ==================================================================
C>    @brief Conversion from spectral model variable to gridded variable
C     ==================================================================
      SUBROUTINE SPECT_TO_GRID 
     &    (VOR_D, DIV_D, T_D, TR_D, PS_D, UGR4, VGR4, TGR4, QGR4, PSGR4)
      
      include "atparam.h"
      include "atparam1.h"
      PARAMETER (NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT)
      include "com_dynvar.h"   ! PHI
      include "com_physcon.h"  ! P0
      include "com_iitest.h"   ! Verbosity

      COMPLEX,DIMENSION(MX,NX,KX,2)    :: VOR_D, DIV_D, T_D
      COMPLEX,DIMENSION(MX,NX,2)       :: PS_D
      COMPLEX,DIMENSION(MX,NX,KX,2,NTR):: TR_D
      REAL   ,DIMENSION(NGP,KX)        :: UGR , VGR ,TGR ,QGR ,PHIGR
      REAL   ,DIMENSION(NGP,KX)        :: UGR1, VGR1,TGR1,QGR1,PHIGR1
      REAL(4),DIMENSION(NGP,KX)        :: UGR4, VGR4,TGR4,QGR4,PHIGR4
      REAL   ,DIMENSION(NGP)           :: PSGR 
      REAL(4),DIMENSION(NGP)           :: PSGR4,RRGR4
      REAL   ,DIMENSION(NGP)           ::       RRGR1 
      COMPLEX,DIMENSION(MX,NX)         :: UCOSTMP,VCOSTMP

      DO K=1,KX
         CALL UVSPEC(VOR_D(1,1,K,1),DIV_D(1,1,K,1),UCOSTMP,VCOSTMP)
         CALL GRID(UCOSTMP,UGR(1,K),2)
         CALL GRID(VCOSTMP,VGR(1,K),2)
      END DO

      DO K=1,KX
         CALL GRID( T_D(1,1,K,1)  ,TGR(1,K),1)
         CALL GRID(TR_D(1,1,K,1,1),QGR(1,K),1)
         CALL GRID(PHI(1,1,K),PHIGR(1,K),1)
      END DO

      CALL GRID(PS_D(1,1,1),PSGR(1),1)

C--     2.4 Select first time step only     
         
        UGR1 =   UGR
        VGR1 =   VGR
        TGR1 =   TGR
        QGR1 =   QGR
      PHIGR1 = PHIGR

C--     2.4 Conversion from double prec. gridded vars to single prec. ones 

        UGR4 =   UGR1
        VGR4 =   VGR1
        TGR4 =   TGR1
        QGR4 =   QGR1*1.0d-3 ! kg/kg
       PSGR4 = P0*exp(PSGR)! Pa
      PHIGR4 = PHIGR1/GG   ! m
       RRGR4 =  RRGR1
C--
      RETURN
      END

C     ==================================================================
C>    @brief Write a GrADS control file for state in sigma levels
C     ==================================================================
      SUBROUTINE WRITE_SIGMA_CTL (ctlname, rst_end, description)
       
      include "atparam.h"
      include "atparam1.h"
      PARAMETER (NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT)
      include "com_physcon.h"      
      include "com_date.h"
      include "com_dyncon1.h"
      
      CHARACTER(*),INTENT(IN) :: ctlname, rst_end, description
      CHARACTER(3)            :: CMON3

      IF (IMONTH.EQ.1) THEN
         CMON3='JAN'
      ELSE IF (IMONTH.EQ.2) THEN
         CMON3='FEB'
      ELSE IF (IMONTH.EQ.3) THEN
         CMON3='MAR'
      ELSE IF (IMONTH.EQ.4) THEN
         CMON3='APR'
      ELSE IF (IMONTH.EQ.5) THEN
         CMON3='MAY'
      ELSE IF (IMONTH.EQ.6) THEN
         CMON3='JUN'
      ELSE IF (IMONTH.EQ.7) THEN
         CMON3='JUL'
      ELSE IF (IMONTH.EQ.8) THEN
         CMON3='AUG'
      ELSE IF (IMONTH.EQ.9) THEN
         CMON3='SEP'
      ELSE IF (IMONTH.EQ.10) THEN
         CMON3='OCT'
      ELSE IF (IMONTH.EQ.11) THEN
         CMON3='NOV'
      ELSE IF (IMONTH.EQ.12) THEN
         CMON3='DEC'
      ELSE
         STOP 'Month number out of bounds'
      END IF

      OPEN (11,FILE=ctlname,FORM='FORMATTED')
      WRITE (11,'(A)'       ) 'DSET ^%y4%m2%d2%h2'//rst_end
      WRITE (11,'(A)'       ) 'TITLE'//description
      WRITE (11,'(A)'       ) 'UNDEF -9.99E33'
      WRITE (11,'(A)'       ) 'OPTIONS template big_endian'
      WRITE (11,'(A)'       ) 'XDEF 96 LINEAR 0.0 3.75'
      WRITE (11,'(A,48F8.3)') 'YDEF 48 LEVELS ',
     &                           (RADANG(J)*90.0d0/ASIN(1.0d0),J=1,48)
      WRITE (11,'(A,7F6.3)' ) 'ZDEF 7 LEVELS ',(SIG(K),K=7,1,-1)
      WRITE (11,'(A,I4,A,I2.2,A,I2.2,A,I4.4,A)'
     &                         ) 'TDEF ',9999,' LINEAR ',IHOUR,'Z',
     &                            IDATE,CMON3,IYEAR,' 6HR'
      WRITE (11,'(A)'       ) 'VARS 6'
      WRITE (11,'(A)'       ) 'U 7 99 U-wind [m/s]'
      WRITE (11,'(A)'       ) 'V 7 99 V-wind [m/s]'
      WRITE (11,'(A)'       ) 'T 7 99 Temperature [K]'
      WRITE (11,'(A)'       ) 'Q 7 99 Specific Humidity [kg/kg]'
      WRITE (11,'(A)'       ) 'PS 0 99 Surface Pressure [Pa]'
      WRITE (11,'(A)'       ) 'RAIN 0 99 Precipitation [mm/6hr]'
      WRITE (11,'(A)'       ) 'ENDVARS'
      CLOSE (11)
         
      RETURN
      END
      
c      SUBROUTINE WRITE_GR4_TA_VARS

c      include "atparam.h"
c      include "atparam1.h"

c      PARAMETER (NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT)

c      include "com_tavars.h"
      
cC--       Monitoring         
c      PRINT *, 'Writing Gridded Time average state variables:'
c      OPEN (96,FILE='state_ta_average.grd',FORM='UNFORMATTED',
c     &         ACCESS='DIRECT', RECL=4*IX*IL)
              
c      irec=1
c      DO k=KX,1,-1
c        WRITE (96,REC=irec) (UGR4_TA(j,k),j=1,NGP)
c        irec=irec+1
c      END DO
c      DO k=KX,1,-1
c        WRITE (96,REC=irec) (VGR4_TA(j,k),j=1,NGP)
c        irec=irec+1
c      END DO
c      DO k=KX,1,-1
c        WRITE (96,REC=irec) (TGR4_TA(j,k),j=1,NGP)
c        irec=irec+1
c      END DO
c      DO k=KX,1,-1
c        WRITE (96,REC=irec) (QGR4_TA(j,k),j=1,NGP)
c        irec=irec+1
c      END DO
c      WRITE   (96,REC=irec)(PSGR4_TA(j)  ,j=1,NGP)
c      irec=irec+1
c      WRITE   (96,REC=irec)(RRGR4_TA(j)  ,j=1,NGP)
c      CLOSE   (96)
      
c      RETURN
c      END
      
c      SUBROUTINE WRITE_GR4_AN_VARS

c      include "atparam.h"
c      include "atparam1.h"
c      PARAMETER (NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT)
c      include "com_tavars.h"
      
cC--       Monitoring         
c      PRINT *, 'Writing Gridded Time average anomaly variables'
c      OPEN (97,FILE='state_ta_anomaly.grd',FORM='UNFORMATTED',
c     &         ACCESS='DIRECT', RECL=4*IX*IL)
              
c      irec=1
c      DO k=KX,1,-1
c        WRITE (97,REC=irec) (UGR4_AN(j,k),j=1,NGP)
c        irec=irec+1
c      END DO
c      DO k=KX,1,-1
c        WRITE (97,REC=irec) (VGR4_AN(j,k),j=1,NGP)
c        irec=irec+1
c      END DO
c      DO k=KX,1,-1
c        WRITE (97,REC=irec) (TGR4_AN(j,k),j=1,NGP)
c        irec=irec+1
c      END DO
c      DO k=KX,1,-1
c        WRITE (97,REC=irec) (QGR4_AN(j,k),j=1,NGP)
c        irec=irec+1
c      END DO
c      WRITE   (97,REC=irec) (PSGR4_AN(j) ,j=1,NGP)
c      irec=irec+1
c      WRITE (97,REC=irec) (RRGR4_AN(j),j=1,NGP)
c      CLOSE (97)
      
c      RETURN
c      END

c      SUBROUTINE MONITOR_GR4_AN_VARS
c      include "atparam.h"
c      include "atparam1.h"
c      PARAMETER (NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT)
c      include "com_tavars.h"
cC--       Monitoring         
c      PRINT *, 'Gridded Time average anomaly variables:'
c      PRINT *,' UGR4_AN  :',MINVAL( UGR4_AN) ,MAXVAL (UGR4_AN)
c      PRINT *,' VGR4_AN  :',MINVAL( VGR4_AN) ,MAXVAL (VGR4_AN)
c      PRINT *,' TGR4_AN  :',MINVAL( TGR4_AN) ,MAXVAL (TGR4_AN)
c      PRINT *,' QGR4_AN  :',MINVAL( QGR4_AN) ,MAXVAL (QGR4_AN)
c      PRINT *,' PSGR4_AN :',MINVAL(PSGR4_AN) ,MAXVAL(PSGR4_AN)
      
c      RETURN
c      END
c      SUBROUTINE MONITOR_GR4_VARS
c      include "atparam.h"
c      include "atparam1.h"
c      PARAMETER (NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT)
c      include "com_grvars.h"
cC--       Monitoring         
c      PRINT *, 'Gridded state variables:'
c      PRINT *,' UGR4  :',MINVAL( UGR4) ,MAXVAL (UGR4)
c      PRINT *,' VGR4  :',MINVAL( VGR4) ,MAXVAL (VGR4)
c      PRINT *,' TGR4  :',MINVAL( TGR4) ,MAXVAL (TGR4)
c      PRINT *,' QGR4  :',MINVAL( QGR4) ,MAXVAL (QGR4)
c      PRINT *,' PSGR4 :',MINVAL(PSGR4) ,MAXVAL(PSGR4)
      
c      RETURN
c      END

c      SUBROUTINE MONITOR_GR4_TA_VARS
c      include "atparam.h"
c      include "atparam1.h"
c      PARAMETER (NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT)
c      include "com_tavars.h"
cC--       Monitoring         
c      PRINT *, 'Gridded Time average state variables:'
c      PRINT *,' UGR4_TA  :',MINVAL( UGR4_TA) ,MAXVAL (UGR4_TA)
c      PRINT *,' VGR4_TA  :',MINVAL( VGR4_TA) ,MAXVAL (VGR4_TA)
c      PRINT *,' TGR4_TA  :',MINVAL( TGR4_TA) ,MAXVAL (TGR4_TA)
c      PRINT *,' QGR4_TA  :',MINVAL( QGR4_TA) ,MAXVAL (QGR4_TA)
c      PRINT *,' PSGR4_TA :',MINVAL(PSGR4_TA) ,MAXVAL(PSGR4_TA)
      
c      RETURN
c      END

c      SUBROUTINE WRITE_GR4_VARS
c      include "atparam.h"
c      include "atparam1.h"

c      PARAMETER (NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT)

c      include "com_grvars.h"
cC--       Monitoring         
c      PRINT *, 'Writing Gridded state variables:'
c      OPEN (95,FILE='state_instantaneous.grd',FORM='UNFORMATTED',
c     &         ACCESS='DIRECT', RECL=4*IX*IL)
              
c      irec=1
c      DO k=KX,1,-1
c        WRITE (95,REC=irec) (UGR4(j,k),j=1,NGP)
c        irec=irec+1
c      END DO
c      DO k=KX,1,-1
c        WRITE (95,REC=irec) (VGR4(j,k),j=1,NGP)
c        irec=irec+1
c      END DO
c      DO k=KX,1,-1
c        WRITE (95,REC=irec) (TGR4(j,k),j=1,NGP)
c        irec=irec+1
c      END DO
c      DO k=KX,1,-1
c        WRITE (95,REC=irec) (QGR4(j,k),j=1,NGP)
c        irec=irec+1
c      END DO
c      WRITE   (95,REC=irec) (PSGR4(j) ,j=1,NGP)
cc        irec=irec+1
cc        WRITE (95,REC=irec) (RRGR4(j),j=1,NGP)
c      CLOSE (95)
      
c      RETURN
c      END
