C     ==================================================================
C>    @brief Operates on time-average state variables (common /TAVARS/).
C>    IMODE values:
C>    - 0 : Set time-averaged variables to zero
C>    - 1 : Increment time-averaged variables
C>    - 2 : Calculate and Write time-average state
C>    - 3 : Calculate and Write time-average anomaly state
C     ==================================================================
      SUBROUTINE TA_VARS (IMODE,ISTEP)
      include "atparam.h"
      include "atparam1.h"

      PARAMETER (NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT)

      include "com_date.h"
      include "com_tsteps.h"
      include "com_dynvar.h"
      include "com_grvars.h"
      include "com_tavars.h"
      include "com_physcon.h"  ! P0
      
      include "com_iitest.h"   ! Verbosity
      
      CHARACTER(10) ::  itime_str
      CHARACTER(40) ::  sigma_Taver_end = '_grid_sigma_Taver'
      CHARACTER(40) ::  sigma_Taver_rst, sigma_Taver_ctl 
      CHARACTER(40) ::  sigma_Tanom_end = '_grid_sigma_Tanom'
      CHARACTER(40) ::  sigma_Tanom_rst, sigma_Tanom_ctl 
      
C     Setting present time string
      WRITE (itime_str(1:10),'(I10)') ITIME

C     Setting filenames
      sigma_Taver_rst = trim(sigma_Taver_end)//'.rst'
      sigma_Taver_ctl = trim(sigma_Taver_end)//'.ctl'
      sigma_Tanom_rst = trim(sigma_Tanom_end)//'.rst'
      sigma_Tanom_ctl = trim(sigma_Tanom_end)//'.ctl'


      IF (IMODE.EQ.0) THEN

         if(iitest==1) PRINT *,
     &                 'Set time-averaged variables (/TAVARS/) to zero'
         DO K=KX,1,-1
           DO j=1,NGP
             UGR4_Taver(j,k) = 0.0
             VGR4_Taver(j,k) = 0.0
             TGR4_Taver(j,k) = 0.0
             QGR4_Taver(j,k) = 0.0
           END DO
         END DO
         DO j=1,NGP
           PSGR4_Taver(j) = 0.0
         END DO
         
        if(iitest==1)CALL MONITOR_GR4(UGR4_Taver,VGR4_Taver, TGR4_Taver
     &               , QGR4_Taver, PHIGR4_Taver,PSGR4_Taver,RRGR4_Taver)

      ELSE IF (IMODE.EQ.1) THEN
      
        if(iitest==1) PRINT *, 'Increment time-averaged variables'

C        2.3 Conversion from spectral model variable to gridded variable

        CALL SPECT_TO_GRID (VOR,DIV,T,TR,PS,UGR4,VGR4,TGR4,QGR4,PSGR4)
     
        DO K=KX,1,-1
          DO j=1,NGP
            UGR4_Taver(j,k) = UGR4_Taver(j,k) + UGR4(j,k)
            VGR4_Taver(j,k) = VGR4_Taver(j,k) + VGR4(j,k)
            TGR4_Taver(j,k) = TGR4_Taver(j,k) + TGR4(j,k)
            QGR4_Taver(j,k) = QGR4_Taver(j,k) + QGR4(j,k)
          END DO
        END DO
        DO j=1,NGP
           PSGR4_Taver(j) = PSGR4_Taver(j) + PSGR4(j)
        END DO

        if(iitest==1)CALL MONITOR_GR4(UGR4_Taver,VGR4_Taver, TGR4_Taver
     &               , QGR4_Taver, PHIGR4_Taver,PSGR4_Taver,RRGR4_Taver)

      ELSE IF (IMODE.EQ.2) THEN
      
        if(iitest==1) PRINT *,'Calculate-Write time-averaged variables'
      
c       2.1 Calculate time-average values

        write(*,*) 'ISTEP',ISTEP
        fmean=1./real(ISTEP-1)
        
        DO K=KX,1,-1
          DO j=1,NGP
            UGR4_Taver(j,k) = UGR4_Taver(j,k) * fmean
            VGR4_Taver(j,k) = VGR4_Taver(j,k) * fmean
            TGR4_Taver(j,k) = TGR4_Taver(j,k) * fmean
            QGR4_Taver(j,k) = QGR4_Taver(j,k) * fmean
          END DO
        END DO
        DO j=1,NGP
           PSGR4_Taver(j) = PSGR4_Taver(j) * fmean
        END DO

        if(iitest==1) WRITE (*,*) 'Time-averaged variables'
        if(iitest==1)CALL MONITOR_GR4(UGR4_Taver,VGR4_Taver, TGR4_Taver
     &               , QGR4_Taver, PHIGR4_Taver,PSGR4_Taver,RRGR4_Taver)

C       Store Time-Averaged state in binary
        CALL WRITE_GR4(itime_str//sigma_Taver_rst,UGR4_Taver,VGR4_Taver,
     &       TGR4_Taver,QGR4_Taver,PHIGR4_Taver,PSGR4_Taver,RRGR4_Taver)
     
C       Create descriptor file
c        CALL WRITE_SIGMA_CTL(itime_str//sigma_Taver_ctl,sigma_Taver_rst, 
c     &       'Time-averaged SPEEDY Model Prognostic Variables') 

C       Store binary name
        OPEN (196,FILE = 'grid_sigma_Taver_state_name.dat',
     &            FORM = 'FORMATTED')
        WRITE(196,'(A)') trim(itime_str//sigma_Taver_rst)
        CLOSE(196)


      ELSE IF (IMODE.EQ.3) THEN
      
        if(iitest==1) PRINT *,'Calculate-Write time-anomaly variables'
        
c       Calculate present gridded state

        CALL SPECT_TO_GRID (VOR,DIV,T,TR,PS,UGR4,VGR4,TGR4,QGR4,PSGR4)
        if(iitest==1) 
     &         CALL MONITOR_GR4(UGR4,VGR4,TGR4,QGR4,PHIGR4,PSGR4,RRGR4)

c       2.2 Calculate time-average anomaly values

        DO K=KX,1,-1
          DO j=1,NGP
            UGR4_Tanom(j,k) = -UGR4_Taver(j,k) + UGR4(j,k)
            VGR4_Tanom(j,k) = -VGR4_Taver(j,k) + VGR4(j,k)
            TGR4_Tanom(j,k) = -TGR4_Taver(j,k) + TGR4(j,k)
            QGR4_Tanom(j,k) = -QGR4_Taver(j,k) + QGR4(j,k)
          END DO
        END DO
        DO j=1,NGP
           PSGR4_Tanom(j) = -PSGR4_Taver(j) + PSGR4(j)
        END DO
        
        if(iitest==1) WRITE (*,*) 'Time-averaged anomaly variables'
        if(iitest==1)CALL MONITOR_GR4(UGR4_Tanom,VGR4_Tanom, TGR4_Tanom
     &               , QGR4_Tanom, PHIGR4_Tanom,PSGR4_Tanom,RRGR4_Tanom)
        
C       Store Time-Averaged state in binary
        CALL WRITE_GR4(itime_str//sigma_Tanom_rst,UGR4_Tanom,VGR4_Tanom,
     &       TGR4_Tanom,QGR4_Tanom,PHIGR4_Tanom,PSGR4_Tanom,RRGR4_Tanom)
        
c        CALL WRITE_SIGMA_CTL(itime_str//sigma_Tanom_ctl,sigma_Tanom_rst, 
c     &     'Time-averaged anomaly of SPEEDY Model Prognostic Variables') 

      ELSE

        PRINT *,'TA_VARS: Invalid IMODE value : ', IMODE
        STOP

      ENDIF

C--
      RETURN
      END

cC     ==================================================================
cC>    @brief Write sigma gridded 4 byte state variables as binary file 
cC     ==================================================================
c      SUBROUTINE WRITE_GR4(fname,UGR4,VGR4,TGR4,QGR4,PHIGR4,PSGR4,RRGR4)

c      include "atparam.h"
c      include "atparam1.h"

c      PARAMETER (NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT)

c      CHARACTER(*),INTENT(IN) :: fname
c      REAL(4),DIMENSION(NGP,KX)        :: UGR4, VGR4,TGR4,QGR4,PHIGR4
c      REAL(4),DIMENSION(NGP)           :: PSGR4,RRGR4

c      PRINT *, 'Writing Gridded Binary ', fname
c      OPEN (96,FILE=fname,FORM='UNFORMATTED',
c     &                                    ACCESS='DIRECT', RECL=4*IX*IL)
              
c      irec=1
c      DO k=KX,1,-1
c        WRITE (96,REC=irec) (UGR4(j,k),j=1,NGP)
c        irec=irec+1
c      END DO
c      DO k=KX,1,-1
c        WRITE (96,REC=irec) (VGR4(j,k),j=1,NGP)
c        irec=irec+1
c      END DO
c      DO k=KX,1,-1
c        WRITE (96,REC=irec) (TGR4(j,k),j=1,NGP)
c        irec=irec+1
c      END DO
c      DO k=KX,1,-1
c        WRITE (96,REC=irec) (QGR4(j,k),j=1,NGP)
c        irec=irec+1
c      END DO
c      WRITE   (96,REC=irec)(PSGR4(j)  ,j=1,NGP)
c      irec=irec+1
c      WRITE   (96,REC=irec)(RRGR4(j)  ,j=1,NGP)
c      CLOSE   (96)
      
c      RETURN
c      END

cC     ==================================================================
cC>    @brief Monitoring 4 byte state variables
cC     ==================================================================
c      SUBROUTINE MONITOR_GR4 (UGR4, VGR4,TGR4,QGR4,PHIGR4,PSGR4,RRGR4)
c      include "atparam.h"
c      include "atparam1.h"
c      PARAMETER (NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT)

c      REAL(4),DIMENSION(NGP,KX)        :: UGR4, VGR4,TGR4,QGR4,PHIGR4
c      REAL(4),DIMENSION(NGP)           :: PSGR4,RRGR4
cC--       Monitoring         
c      PRINT *, 'Gridded state variables:'
c      PRINT *,' UGR4  :',MINVAL( UGR4) ,MAXVAL (UGR4)
c      PRINT *,' VGR4  :',MINVAL( VGR4) ,MAXVAL (VGR4)
c      PRINT *,' TGR4  :',MINVAL( TGR4) ,MAXVAL (TGR4)
c      PRINT *,' QGR4  :',MINVAL( QGR4) ,MAXVAL (QGR4)
c      PRINT *,' PSGR4 :',MINVAL(PSGR4) ,MAXVAL(PSGR4)
      
c      RETURN
c      END


cc      SUBROUTINE MONITOR_SPECTRAL_TIME_AVERAGE
cc      include "atparam.h"
cc      include "atparam1.h"
cc      PARAMETER (NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT)
cc      include "com_tavars.h"
cc      PRINT *, 'Spectral Output Time average state:'
cc      PRINT *,' VOR_Taver:',MINVAL(ABS(VOR_Taver)),MAXVAL(ABS(VOR_Taver))
cc      PRINT *,' DIV_Taver:',MINVAL(ABS(DIV_Taver)),MAXVAL(ABS(DIV_Taver))
cc      PRINT *,'   T_Taver:',MINVAL(ABS(  T_Taver)),MAXVAL(ABS(  T_Taver))
cc      PRINT *,'  PS_Taver:',MINVAL(ABS( PS_Taver)),MAXVAL(ABS( PS_Taver))
cc      PRINT *,'  TR_Taver:',MINVAL(ABS( TR_Taver)),MAXVAL(ABS( TR_Taver))
cc       
cc      RETURN
cc      END      

cC     ==================================================================
cC>    @brief Conversion from spectral model variable to gridded variable
cC     ==================================================================
c      SUBROUTINE SPECT_TO_GRID 
c     &    (VOR_D, DIV_D, T_D, TR_D, PS_D, UGR4, VGR4, TGR4, QGR4, PSGR4)
      
c      include "atparam.h"
c      include "atparam1.h"
c      PARAMETER (NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT)
c      include "com_dynvar.h"   ! PHI
c      include "com_physcon.h"  ! P0
c      include "com_iitest.h"   ! Verbosity

c      COMPLEX,DIMENSION(MX,NX,KX,2)    :: VOR_D, DIV_D, T_D
c      COMPLEX,DIMENSION(MX,NX,2)       :: PS_D
c      COMPLEX,DIMENSION(MX,NX,KX,2,NTR):: TR_D
c      REAL   ,DIMENSION(NGP,KX)        :: UGR , VGR ,TGR ,QGR ,PHIGR
c      REAL   ,DIMENSION(NGP,KX)        :: UGR1, VGR1,TGR1,QGR1,PHIGR1
c      REAL(4),DIMENSION(NGP,KX)        :: UGR4, VGR4,TGR4,QGR4,PHIGR4
c      REAL   ,DIMENSION(NGP)           :: PSGR 
c      REAL(4),DIMENSION(NGP)           :: PSGR4,RRGR4
c      REAL   ,DIMENSION(NGP)           ::       RRGR1 
c      COMPLEX,DIMENSION(MX,NX)         :: UCOSTMP,VCOSTMP

c      DO K=1,KX
c         CALL UVSPEC(VOR_D(1,1,K,1),DIV_D(1,1,K,1),UCOSTMP,VCOSTMP)
c         CALL GRID(UCOSTMP,UGR(1,K),2)
c         CALL GRID(VCOSTMP,VGR(1,K),2)
c      END DO

c      DO K=1,KX
c         CALL GRID( T_D(1,1,K,1)  ,TGR(1,K),1)
c         CALL GRID(TR_D(1,1,K,1,1),QGR(1,K),1)
c         CALL GRID(PHI(1,1,K),PHIGR(1,K),1)
c      END DO

c      CALL GRID(PS_D(1,1,1),PSGR(1),1)

cC--     2.4 Select first time step only     
         
c        UGR1 =   UGR
c        VGR1 =   VGR
c        TGR1 =   TGR
c        QGR1 =   QGR
c      PHIGR1 = PHIGR

cC--     2.4 Conversion from double prec. gridded vars to single prec. ones 

c        UGR4 =   UGR1
c        VGR4 =   VGR1
c        TGR4 =   TGR1
c        QGR4 =   QGR1*1.0d-3 ! kg/kg
c       PSGR4 = P0*exp(PSGR)! Pa
c      PHIGR4 = PHIGR1/GG   ! m
c       RRGR4 =  RRGR1
cC--
c      RETURN
c      END
      
cC     ==================================================================
cC>    @brief Write a GrADS control file for state in sigma levels
cC     ==================================================================
c      SUBROUTINE WRITE_SIGMA_CTL (ctlname, rst_end, description)
       
c      include "atparam.h"
c      include "atparam1.h"
c      PARAMETER (NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT)
c      include "com_physcon.h"      
c      include "com_date.h"
c      include "com_dyncon1.h"
      
c      CHARACTER(*),INTENT(IN) :: ctlname, rst_end, description
c      CHARACTER(3)            :: CMON3

c      IF (IMONTH.EQ.1) THEN
c         CMON3='JAN'
c      ELSE IF (IMONTH.EQ.2) THEN
c         CMON3='FEB'
c      ELSE IF (IMONTH.EQ.3) THEN
c         CMON3='MAR'
c      ELSE IF (IMONTH.EQ.4) THEN
c         CMON3='APR'
c      ELSE IF (IMONTH.EQ.5) THEN
c         CMON3='MAY'
c      ELSE IF (IMONTH.EQ.6) THEN
c         CMON3='JUN'
c      ELSE IF (IMONTH.EQ.7) THEN
c         CMON3='JUL'
c      ELSE IF (IMONTH.EQ.8) THEN
c         CMON3='AUG'
c      ELSE IF (IMONTH.EQ.9) THEN
c         CMON3='SEP'
c      ELSE IF (IMONTH.EQ.10) THEN
c         CMON3='OCT'
c      ELSE IF (IMONTH.EQ.11) THEN
c         CMON3='NOV'
c      ELSE IF (IMONTH.EQ.12) THEN
c         CMON3='DEC'
c      ELSE
c         STOP 'Month number out of bounds'
c      END IF

c      OPEN (11,FILE=ctlname,FORM='FORMATTED')
c      WRITE (11,'(A)'       ) 'DSET ^%y4%m2%d2%h2'//rst_end
c      WRITE (11,'(A)'       ) 'TITLE'//description
c      WRITE (11,'(A)'       ) 'UNDEF -9.99E33'
c      WRITE (11,'(A)'       ) 'OPTIONS template big_endian'
c      WRITE (11,'(A)'       ) 'XDEF 96 LINEAR 0.0 3.75'
c      WRITE (11,'(A,48F8.3)') 'YDEF 48 LEVELS ',
c     &                           (RADANG(J)*90.0d0/ASIN(1.0d0),J=1,48)
c      WRITE (11,'(A,7F6.3)' ) 'ZDEF 7 LEVELS ',(SIG(K),K=7,1,-1)
c      WRITE (11,'(A,I4,A,I2.2,A,I2.2,A,I4.4,A)'
c     &                         ) 'TDEF ',9999,' LINEAR ',IHOUR,'Z',
c     &                            IDATE,CMON3,IYEAR,' 6HR'
c      WRITE (11,'(A)'       ) 'VARS 6'
c      WRITE (11,'(A)'       ) 'U 7 99 U-wind [m/s]'
c      WRITE (11,'(A)'       ) 'V 7 99 V-wind [m/s]'
c      WRITE (11,'(A)'       ) 'T 7 99 Temperature [K]'
c      WRITE (11,'(A)'       ) 'Q 7 99 Specific Humidity [kg/kg]'
c      WRITE (11,'(A)'       ) 'PS 0 99 Surface Pressure [Pa]'
c      WRITE (11,'(A)'       ) 'RAIN 0 99 Precipitation [mm/6hr]'
c      WRITE (11,'(A)'       ) 'ENDVARS'
c      CLOSE (11)
         
c      RETURN
c      END
      
cc      SUBROUTINE WRITE_GR4_Taver_VARS

cc      include "atparam.h"
cc      include "atparam1.h"

cc      PARAMETER (NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT)

cc      include "com_tavars.h"
      
ccC--       Monitoring         
cc      PRINT *, 'Writing Gridded Time average state variables:'
cc      OPEN (96,FILE='state_ta_average.grd',FORM='UNFORMATTED',
cc     &         ACCESS='DIRECT', RECL=4*IX*IL)
              
cc      irec=1
cc      DO k=KX,1,-1
cc        WRITE (96,REC=irec) (UGR4_Taver(j,k),j=1,NGP)
cc        irec=irec+1
cc      END DO
cc      DO k=KX,1,-1
cc        WRITE (96,REC=irec) (VGR4_Taver(j,k),j=1,NGP)
cc        irec=irec+1
cc      END DO
cc      DO k=KX,1,-1
cc        WRITE (96,REC=irec) (TGR4_Taver(j,k),j=1,NGP)
cc        irec=irec+1
cc      END DO
cc      DO k=KX,1,-1
cc        WRITE (96,REC=irec) (QGR4_Taver(j,k),j=1,NGP)
cc        irec=irec+1
cc      END DO
cc      WRITE   (96,REC=irec)(PSGR4_Taver(j)  ,j=1,NGP)
cc      irec=irec+1
cc      WRITE   (96,REC=irec)(RRGR4_Taver(j)  ,j=1,NGP)
cc      CLOSE   (96)
      
cc      RETURN
cc      END
      
cc      SUBROUTINE WRITE_GR4_Tanom_VARS

cc      include "atparam.h"
cc      include "atparam1.h"
cc      PARAMETER (NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT)
cc      include "com_tavars.h"
      
ccC--       Monitoring         
cc      PRINT *, 'Writing Gridded Time average anomaly variables'
cc      OPEN (97,FILE='state_ta_anomaly.grd',FORM='UNFORMATTED',
cc     &         ACCESS='DIRECT', RECL=4*IX*IL)
              
cc      irec=1
cc      DO k=KX,1,-1
cc        WRITE (97,REC=irec) (UGR4_Tanom(j,k),j=1,NGP)
cc        irec=irec+1
cc      END DO
cc      DO k=KX,1,-1
cc        WRITE (97,REC=irec) (VGR4_Tanom(j,k),j=1,NGP)
cc        irec=irec+1
cc      END DO
cc      DO k=KX,1,-1
cc        WRITE (97,REC=irec) (TGR4_Tanom(j,k),j=1,NGP)
cc        irec=irec+1
cc      END DO
cc      DO k=KX,1,-1
cc        WRITE (97,REC=irec) (QGR4_Tanom(j,k),j=1,NGP)
cc        irec=irec+1
cc      END DO
cc      WRITE   (97,REC=irec) (PSGR4_Tanom(j) ,j=1,NGP)
cc      irec=irec+1
cc      WRITE (97,REC=irec) (RRGR4_Tanom(j),j=1,NGP)
cc      CLOSE (97)
      
cc      RETURN
cc      END

cc      SUBROUTINE MONITOR_GR4_Tanom_VARS
cc      include "atparam.h"
cc      include "atparam1.h"
cc      PARAMETER (NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT)
cc      include "com_tavars.h"
ccC--       Monitoring         
cc      PRINT *, 'Gridded Time average anomaly variables:'
cc      PRINT *,' UGR4_Tanom  :',MINVAL( UGR4_Tanom) ,MAXVAL (UGR4_Tanom)
cc      PRINT *,' VGR4_Tanom  :',MINVAL( VGR4_Tanom) ,MAXVAL (VGR4_Tanom)
cc      PRINT *,' TGR4_Tanom  :',MINVAL( TGR4_Tanom) ,MAXVAL (TGR4_Tanom)
cc      PRINT *,' QGR4_Tanom  :',MINVAL( QGR4_Tanom) ,MAXVAL (QGR4_Tanom)
cc      PRINT *,' PSGR4_Tanom :',MINVAL(PSGR4_Tanom) ,MAXVAL(PSGR4_Tanom)
      
cc      RETURN
cc      END
cc      SUBROUTINE MONITOR_GR4_VARS
cc      include "atparam.h"
cc      include "atparam1.h"
cc      PARAMETER (NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT)
cc      include "com_grvars.h"
ccC--       Monitoring         
cc      PRINT *, 'Gridded state variables:'
cc      PRINT *,' UGR4  :',MINVAL( UGR4) ,MAXVAL (UGR4)
cc      PRINT *,' VGR4  :',MINVAL( VGR4) ,MAXVAL (VGR4)
cc      PRINT *,' TGR4  :',MINVAL( TGR4) ,MAXVAL (TGR4)
cc      PRINT *,' QGR4  :',MINVAL( QGR4) ,MAXVAL (QGR4)
cc      PRINT *,' PSGR4 :',MINVAL(PSGR4) ,MAXVAL(PSGR4)
      
cc      RETURN
cc      END

cc      SUBROUTINE MONITOR_GR4_Taver_VARS
cc      include "atparam.h"
cc      include "atparam1.h"
cc      PARAMETER (NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT)
cc      include "com_tavars.h"
ccC--       Monitoring         
cc      PRINT *, 'Gridded Time average state variables:'
cc      PRINT *,' UGR4_Taver  :',MINVAL( UGR4_Taver) ,MAXVAL (UGR4_Taver)
cc      PRINT *,' VGR4_Taver  :',MINVAL( VGR4_Taver) ,MAXVAL (VGR4_Taver)
cc      PRINT *,' TGR4_Taver  :',MINVAL( TGR4_Taver) ,MAXVAL (TGR4_Taver)
cc      PRINT *,' QGR4_Taver  :',MINVAL( QGR4_Taver) ,MAXVAL (QGR4_Taver)
cc      PRINT *,' PSGR4_Taver :',MINVAL(PSGR4_Taver) ,MAXVAL(PSGR4_Taver)
      
cc      RETURN
cc      END

cc      SUBROUTINE WRITE_GR4_VARS
cc      include "atparam.h"
cc      include "atparam1.h"

cc      PARAMETER (NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT)

cc      include "com_grvars.h"
ccC--       Monitoring         
cc      PRINT *, 'Writing Gridded state variables:'
cc      OPEN (95,FILE='state_instantaneous.grd',FORM='UNFORMATTED',
cc     &         ACCESS='DIRECT', RECL=4*IX*IL)
              
cc      irec=1
cc      DO k=KX,1,-1
cc        WRITE (95,REC=irec) (UGR4(j,k),j=1,NGP)
cc        irec=irec+1
cc      END DO
cc      DO k=KX,1,-1
cc        WRITE (95,REC=irec) (VGR4(j,k),j=1,NGP)
cc        irec=irec+1
cc      END DO
cc      DO k=KX,1,-1
cc        WRITE (95,REC=irec) (TGR4(j,k),j=1,NGP)
cc        irec=irec+1
cc      END DO
cc      DO k=KX,1,-1
cc        WRITE (95,REC=irec) (QGR4(j,k),j=1,NGP)
cc        irec=irec+1
cc      END DO
cc      WRITE   (95,REC=irec) (PSGR4(j) ,j=1,NGP)
ccc        irec=irec+1
ccc        WRITE (95,REC=irec) (RRGR4(j),j=1,NGP)
cc      CLOSE (95)
      
cc      RETURN
cc      END
