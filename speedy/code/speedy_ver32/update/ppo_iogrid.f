      SUBROUTINE IOGRID (IMODE)
C--
C--   SUBROUTINE IOGRID (IMODE)
C--   Created by TM
C--
C--   Purpose : read or write a gridded file in sigma coordinate
C--   Input :   IMODE = 1 : read model variables from a gridded file (sigma)
C--                   = 2 : write model variables  to a gridded file (p)
C--                   = 3 : write a GrADS control file (for p)
C--                   = 4 : write model variables  to a gridded file (sigma)
C--                   = 5 : write a GrADS control file (for sigma)
C--   Initialized common blocks (if IMODE = 1) : DYNSP1, SFCANOM 
C--

      include "atparam.h"
      include "atparam1.h"

      PARAMETER (NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT)

      include "com_physcon.h"
      include "com_physvar.h"
      include "com_dynvar.h"
      include "com_dyncon1.h"
      include "com_anomvar.h"

      include "com_date.h"
      include "com_tsteps.h"
      include "par_tmean.h"
      include "com_tmean_daily.h"

      include "com_grvars.h"
      include "com_iitest.h"   ! Verbosity

      
c      COMPLEX UCOSTMP(MX,NX),VCOSTMP(MX,NX)
c      REAL    UGR (NGP,KX),   VGR (NGP,KX),  TGR (NGP,KX)
c      REAL    UGR1(NGP,KX),   VGR1(NGP,KX),  TGR1(NGP,KX)
c      REAL(4) UGR4(NGP,KX),   VGR4(NGP,KX),  TGR4(NGP,KX)
c      REAL    QGR (NGP,KX), PHIGR (NGP,KX), PSGR (NGP)
c      REAL    QGR1(NGP,KX), PHIGR1(NGP,KX), RRGR1(NGP)
c      REAL(4) QGR4(NGP,KX), PHIGR4(NGP,KX), PSGR4(NGP)
c      REAL(4) RRGR4(NGP)

C--   For vertical interpolation !adapted from ppo_tminc.f
      INTEGER K0(ngp)
      REAL    W0(ngp), ZOUT(ngp), ZINP(nlev), RDZINP(nlev)
!
      INTEGER       ::      irec
      CHARACTER(10) ::  itime_str
      CHARACTER(40) ::  press_end = '_grid_press_Insta'
      CHARACTER(40) ::  sigma_end = '_grid_sigma_Insta'
      CHARACTER(40) ::  press_rst, press_ctl
      CHARACTER(40) ::  sigma_rst, sigma_ctl 
      CHARACTER(3)  ::  CMON3 = 'JAN'
      
!       REAL(4) UGR4_1(NGP,KX),  VGR4_1(NGP,KX) 
!       REAL(4) TGR4_1(NGP,KX),  QGR4_1(NGP,KX), PSGR4_1(NGP)

      
C     Setting present time string
      WRITE (itime_str(1:10),'(I10)') ITIME

C     Setting filenames
      sigma_rst = trim(sigma_end)//'.rst'
      sigma_ctl = trim(sigma_end)//'.ctl'
      press_rst = trim(press_end)//'.rst'
      press_ctl = trim(press_end)//'.ctl'
      
      

      IF (IMODE.EQ.1) THEN

C--      1.1 Read the gridded dataset (Sigma-level)

         PRINT '(A,I4.4,A,I2.2,A,I2.2,A,I2.2)',
     &     ' Read gridded dataset for ',
     &     IYEAR,'/',IMONTH,'/',IDATE,'/',IHOUR

         OPEN (90,FILE = itime_str//sigma_rst,
     &            FORM ='UNFORMATTED',ACCESS='DIRECT',RECL=4*NGP)    
         irec=1
         DO K=KX,1,-1
            READ (90,REC=irec) ( UGR4(j,k),j=1,NGP)
            irec=irec+1
         END DO
         DO K=KX,1,-1
            READ (90,REC=irec) ( VGR4(j,k),j=1,NGP)
            irec=irec+1
         END DO
         DO K=KX,1,-1
            READ (90,REC=irec) ( TGR4(j,k),j=1,NGP)
            irec=irec+1
         END DO
         DO K=KX,1,-1
            READ (90,REC=irec) ( QGR4(j,k),j=1,NGP)
            irec=irec+1
         END DO
         READ    (90,REC=irec) (PSGR4(j)  ,j=1,NGP)
         CLOSE   (90)

C--      1.2 Conversion from single prec. gridded vars to double prec. ones 

          UGR =  UGR4
          VGR =  VGR4
          TGR =  TGR4
          QGR =  QGR4 *1.0d3
         PSGR = PSGR4
         PSGR = log(PSGR/P0)

C--      Monitoring         
         if(iitest==1) PRINT *,' UGR  :',MINVAL( UGR) ,MAXVAL (UGR)
         if(iitest==1) PRINT *,' VGR  :',MINVAL( VGR) ,MAXVAL (VGR)
         if(iitest==1) PRINT *,' TGR  :',MINVAL( TGR) ,MAXVAL (TGR)
         if(iitest==1) PRINT *,' QGR  :',MINVAL( QGR) ,MAXVAL (QGR)
         if(iitest==1) PRINT *,' PSGR :',MINVAL(PSGR) ,MAXVAL(PSGR)

C--      1.3 Conversion from gridded variable to spectral variable

         DO K=1,KX
             CALL VDSPEC(UGR(1,K),VGR(1,K),VOR(1,1,K,1),DIV(1,1,K,1),2)
             
             IF(IX.EQ.IY*4) THEN
                 CALL TRUNCT(VOR(1,1,K,1))
                 CALL TRUNCT(DIV(1,1,K,1))
             END IF
             
             CALL SPEC(TGR(1,K),T(1,1,K,1))
             CALL SPEC(QGR(1,K),TR(1,1,K,1,1))
             
         END DO
         CALL SPEC(PSGR(1),PS(1,1,1))



      ELSE IF (IMODE.EQ.2.OR.IMODE.EQ.4) THEN
      
C--     2. Write date and model variables to the gridded file (2:P,4:sigma)
!
!       2.1 Conversion from spectral model variable to gridded variable
!
        DO K=1,KX
          CALL UVSPEC(VOR(1,1,K,1),DIV(1,1,K,1),UCOSTMP,VCOSTMP)
          CALL GRID(UCOSTMP,UGR(1,K),2)
          CALL GRID(VCOSTMP,VGR(1,K),2)
        END DO

        DO K=1,KX
          CALL GRID(T(1,1,K,1),TGR(1,K),1)
          CALL GRID(TR(1,1,K,1,1),QGR(1,K),1)
          CALL GRID(PHI(1,1,K),PHIGR(1,K),1)
        END DO

        CALL GRID(PS(1,1,1),PSGR(1),1)

        IF (IMODE.EQ.4) THEN  ! sigma-level output

C--     2.2.1 Select first time step only     
         
            UGR1 =   UGR
            VGR1 =   VGR
            TGR1 =   TGR
            QGR1 =   QGR
          PHIGR1 = PHIGR

        ELSE                  ! p-level output

C--     2.2.2 Vertical interpolation from sigma level to pressure level (ppo_tminc.f)

          ZINP(1) = -SIGL(1)
          
          DO k=2,nlev
            ZINP(k) = -SIGL(k)
            RDZINP(k) = 1.0d0/(ZINP(k-1)-ZINP(k))
          END DO
    
          DO k = 1,KX
            DO j = 1,ngp
              ZOUT(j) = PSGR(j) - log(POUT(k))
            END DO
    
            CALL SETVIN(ZINP,RDZINP,ZOUT,ngp,kx,K0,W0)
    
            CALL VERINT(TGR1(1,k),TGR,ngp,kx,K0,W0)
    
            DO j=1,ngp
             
              IF(ZOUT(j).LT.ZINP(nlev)) THEN
                textr = MAX(TGR1(j,k),TGR(j,nlev))
                 aref = RD * 0.006d0/GG * (ZINP(nlev)-ZOUT(j))
                 tref = TGR(j,nlev) * (1.0d0 + aref + 0.5*aref*aref)
                TGR1(j,k) = textr + 0.7d0 * (tref - textr)
              END IF
              
            END DO
    
            DO j=1,ngp
              W0(j) = MAX(W0(j),REAL(0.0))
            END DO
    
            CALL VERINT(UGR1(1,k),UGR,ngp,kx,K0,W0)
            CALL VERINT(VGR1(1,k),VGR,ngp,kx,K0,W0)
            CALL VERINT(QGR1(1,k),QGR,ngp,kx,K0,W0)
    
            DO j=1,ngp
              phi1 = PHIGR(j,K0(j)) +
     &            0.5*RD*(TGR1(j,k)+TGR(j,K0(j)))*(ZOUT(j)-ZINP(K0(j)))
              phi2 = PHIGR(j,K0(j)-1) +
     &         0.5*RD*(TGR1(j,k)+TGR(j,K0(j)-1))*(ZOUT(j)-ZINP(K0(j)-1))
              PHIGR1(j,k) = phi1 + W0(j)*(phi2-phi1)
            END DO
    
            DO j=1,ngp
              RRGR1(j) = (SAVE2D_L(j,1) + SAVE2D_L(j,2)) ! g/m^2/s
     &                          *3.6*4.0/REAL(NSTEPS)*6.0 ! mm/6hr
            END DO
            
          END DO
             
        END IF
         
C----   Output

	if (iitest.eq.1) PRINT *,' Writing gridded dataset'
!         PRINT '(A,I4.4,A,I2.2,A,I2.2,A,I2.2)',
!      &        ' Write gridded dataset for ',
!      &         IYEAR,'/',IMONTH,'/',IDATE,'/',IHOUR

C--     2.3 Conversion from double prec. gridded vars to single prec. ones 

          UGR4 =   UGR1
          VGR4 =   VGR1
          TGR4 =   TGR1
          QGR4 =   QGR1*1.0d-3 ! kg/kg
        PHIGR4 = PHIGR1/GG   ! m
         RRGR4 =  RRGR1
         PSGR4 = P0*exp(PSGR)! Pa

C--     Monitoring         
        if(iitest==1) PRINT *,' UGR4  :',MINVAL( UGR4) ,MAXVAL (UGR4)
        if(iitest==1) PRINT *,' VGR4  :',MINVAL( VGR4) ,MAXVAL (VGR4)
        if(iitest==1) PRINT *,' TGR4  :',MINVAL( TGR4) ,MAXVAL (TGR4)
        if(iitest==1) PRINT *,' QGR4  :',MINVAL( QGR4) ,MAXVAL (QGR4)
        if(iitest==1) PRINT *,' PSGR4 :',MINVAL(PSGR4) ,MAXVAL(PSGR4)

        
        IF (IMODE.EQ.2) THEN
! C         Store binary name
!           OPEN (196,FILE = 'grid_press_Insta_state_name.dat',
!      &              FORM = 'FORMATTED')
!           WRITE(196,'(A)') trim(itime_str//press_rst)
!           CLOSE(196)

          OPEN (99,FILE   = itime_str//press_rst, FORM='UNFORMATTED'
     &            ,ACCESS ='DIRECT'  , RECL=4*IX*IL)

        ELSE
! C         Store binary name
!           OPEN (196,FILE = 'grid_sigma_Insta_state_name.dat',
!      &              FORM = 'FORMATTED')
!           WRITE(196,'(A)') trim(itime_str//sigma_rst)
!           CLOSE(196)

          OPEN (99,FILE   = itime_str//sigma_rst, FORM = 'UNFORMATTED',
     &             ACCESS ='DIRECT'  , RECL = 4*IX*IL)
     
        END IF
         
        irec=1
        DO k=KX,1,-1
          WRITE (99,REC=irec) (UGR4(j,k),j=1,NGP)
          irec=irec+1
        END DO
        DO k=KX,1,-1
          WRITE (99,REC=irec) (VGR4(j,k),j=1,NGP)
          irec=irec+1
        END DO
        DO k=KX,1,-1
          WRITE (99,REC=irec) (TGR4(j,k),j=1,NGP)
          irec=irec+1
        END DO
        DO k=KX,1,-1
          WRITE (99,REC=irec) (QGR4(j,k),j=1,NGP)
          irec=irec+1
        END DO
         
        IF (IMODE.EQ.2) THEN !Z output is only for p-level
          DO k=KX,1,-1
            WRITE (99,REC=irec) (PHIGR4(j,k),j=1,NGP)
            irec=irec+1
          END DO
        END IF
         
        WRITE (99,REC=irec) (PSGR4(j),j=1,NGP)
        irec=irec+1
        WRITE (99,REC=irec) (RRGR4(j),j=1,NGP)

        CLOSE (99)

        if(iitest==1) PRINT *,' UGR  :',MINVAL(UGR4),MAXVAL(UGR4)
        if(iitest==1) PRINT *,' VGR  :',MINVAL(VGR4),MAXVAL(VGR4)
        if(iitest==1) PRINT *,' TGR  :',MINVAL(TGR4),MAXVAL(TGR4)
        if(iitest==1) PRINT *,' QGR  :',MINVAL(QGR4),MAXVAL(QGR4)
        if(iitest==1) PRINT *,' PHIGR:',MINVAL(PHIGR4),MAXVAL(PHIGR4)
        if(iitest==1) PRINT *,' PSGR :',MINVAL(PSGR4),MAXVAL(PSGR4)
        if(iitest==1) PRINT *,' RRGR :',MINVAL(RRGR4),MAXVAL(RRGR4)
        
        

c      ELSE IF (IMODE.EQ.3) THEN

c        CALL SET_CTL_GRID ('press','Insta',
c     &                              'Speedy Model Prognostic Variables')

c      ELSE IF (IMODE.EQ.5) THEN

c        CALL SET_CTL_GRID ('sigma','Insta',
c     &                              'Speedy Model Prognostic Variables')




c      ELSE IF (IMODE.EQ.3.OR.IMODE.EQ.5) THEN

cC--   3. Write a GrADS control file (3:p,5:sigma)

c         IF (IMONTH.EQ.1) THEN
c           CMON3='JAN'
c         ELSE IF (IMONTH.EQ.2) THEN
c           CMON3='FEB'
c         ELSE IF (IMONTH.EQ.3) THEN
c           CMON3='MAR'
c         ELSE IF (IMONTH.EQ.4) THEN
c           CMON3='APR'
c         ELSE IF (IMONTH.EQ.5) THEN
c           CMON3='MAY'
c         ELSE IF (IMONTH.EQ.6) THEN
c           CMON3='JUN'
c         ELSE IF (IMONTH.EQ.7) THEN
c           CMON3='JUL'
c         ELSE IF (IMONTH.EQ.8) THEN
c           CMON3='AUG'
c         ELSE IF (IMONTH.EQ.9) THEN
c           CMON3='SEP'
c         ELSE IF (IMONTH.EQ.10) THEN
c           CMON3='OCT'
c         ELSE IF (IMONTH.EQ.11) THEN
c           CMON3='NOV'
c         ELSE IF (IMONTH.EQ.12) THEN
c           CMON3='DEC'
c         END IF

c         IF (IMODE.EQ.3) THEN ! p-level
c           OPEN (11, FILE = itime_str//press_ctl, FORM ='FORMATTED')    
c           WRITE(11,'(A)') 'DSET ^%y4%m2%d2%h2'//press_rst
           
c         ELSE                 ! sigma-level
c           OPEN (11, FILE = itime_str//sigma_ctl, FORM ='FORMATTED')    
c           WRITE(11,'(A)') 'DSET ^%y4%m2%d2%h2'//sigma_rst
           
c         END IF
         
c         WRITE (11,'(A)') 'TITLE SPEEDY MODEL OUTPUT'
c         WRITE (11,'(A)') 'UNDEF -9.99E33'
c         WRITE (11,'(A)') 'OPTIONS template big_endian'
c         WRITE (11,'(A)') 'XDEF 96 LINEAR 0.0 3.75'
c         WRITE (11,'(A,48F8.3)') 'YDEF 48 LEVELS ',
c     &     (RADANG(J)*90.0d0/ASIN(1.0d0),J=1,48)
         
c         IF (IMODE.EQ.3) THEN
c           WRITE (11,'(A)') 'ZDEF 7 LEVELS 925 850 700 500 300 200 100'
c         ELSE
c           WRITE (11,'(A,7F6.3)') 'ZDEF 7 LEVELS ',(SIG(K),K=7,1,-1)
c         END IF

c           WRITE (11,'(A,I4,A,I2.2,A,I2.2,A,I4.4,A)') 'TDEF ',
c     &         9999,' LINEAR ',IHOUR,'Z',IDATE,CMON3,IYEAR,' 6HR'
         
cc         IF (NDAYSL.NE.0) THEN
cc           WRITE (11,'(A,I4,A,I2.2,A,I2.2,A,I4.4,A)') 'TDEF ',
cc     &         NDAYSL*4+1,' LINEAR ',IHOUR,'Z',IDATE,CMON3,IYEAR,' 6HR'
cc         ELSE
cc           WRITE (11,'(A,I4,A,I2.2,A,I2.2,A,I4.4,A)') 'TDEF ',
cc     &                  2,' LINEAR ',IHOUR,'Z',IDATE,CMON3,IYEAR,' 6HR'
cc         END IF
         
c         IF (IMODE.EQ.3) THEN          !     p-level
c           WRITE (11,'(A)') 'VARS 7'
c         ELSE                          ! sigma-level
c           WRITE (11,'(A)') 'VARS 6'
c         END IF
         
c         WRITE  (11,'(A)') 'U 7 99 U-wind [m/s]'
c         WRITE  (11,'(A)') 'V 7 99 V-wind [m/s]'
c         WRITE  (11,'(A)') 'T 7 99 Temperature [K]'
c         WRITE  (11,'(A)') 'Q 7 99 Specific Humidity [kg/kg]'
c         IF (IMODE.EQ.3) THEN
c           WRITE(11,'(A)') 'Z 7 99 Geopotential Height [m]'
c         END IF
c         WRITE  (11,'(A)') 'PS 0 99 Surface Pressure [Pa]'
c         WRITE  (11,'(A)') 'RAIN 0 99 Precipitation [mm/6hr]'
c         WRITE  (11,'(A)') 'ENDVARS'
c         CLOSE  (11)


      ELSE IF (IMODE.EQ.6) THEN

C--      Start from time-average state and anomaly gridded states
c--      in sigma levels with 1 time step

	 if (iitest.eq.1) PRINT *,'Reading time-averaged gridded dataset'

         OPEN (92,FILE = itime_str//'_grid_sigma_Taver.rst',
     &            FORM ='UNFORMATTED',ACCESS='DIRECT',RECL=4*NGP)    
         irec=1
         DO K=KX,1,-1
            READ (92,REC=irec) ( UGR4(j,k),j=1,NGP)
            irec=irec+1
         END DO
         DO K=KX,1,-1
            READ (92,REC=irec) ( VGR4(j,k),j=1,NGP)
            irec=irec+1
         END DO
         DO K=KX,1,-1
            READ (92,REC=irec) ( TGR4(j,k),j=1,NGP)
            irec=irec+1
         END DO
         DO K=KX,1,-1
            READ (92,REC=irec) ( QGR4(j,k),j=1,NGP)
            irec=irec+1
         END DO
         READ    (92,REC=irec) (PSGR4(j)  ,j=1,NGP)
         CLOSE   (92)

C--       Monitoring         

          if(iitest==1) PRINT *,' UGR4  :',MINVAL( UGR4) ,MAXVAL (UGR4)
          if(iitest==1) PRINT *,' VGR4  :',MINVAL( VGR4) ,MAXVAL (VGR4)
          if(iitest==1) PRINT *,' TGR4  :',MINVAL( TGR4) ,MAXVAL (TGR4)
          if(iitest==1) PRINT *,' QGR4  :',MINVAL( QGR4) ,MAXVAL (QGR4)
          if(iitest==1) PRINT *,' PSGR4 :',MINVAL(PSGR4) ,MAXVAL(PSGR4)

          if (iitest.eq.1) PRINT *,
     &                    'Reading time average anomaly gridded dataset'

         OPEN (93,FILE = itime_str//'_grid_sigma_Tanom.rst',
     &            FORM ='UNFORMATTED',ACCESS='DIRECT',RECL=4*NGP)    
         irec=1
         DO K=KX,1,-1
            READ (93,REC=irec) ( UGR4_1(j,k),j=1,NGP)
            irec=irec+1
         END DO
         DO K=KX,1,-1
            READ (93,REC=irec) ( VGR4_1(j,k),j=1,NGP)
            irec=irec+1
         END DO
         DO K=KX,1,-1
            READ (93,REC=irec) ( TGR4_1(j,k),j=1,NGP)
            irec=irec+1
         END DO
         DO K=KX,1,-1
            READ (93,REC=irec) ( QGR4_1(j,k),j=1,NGP)
            irec=irec+1
         END DO
         READ    (93,REC=irec) (PSGR4_1(j)  ,j=1,NGP)
         CLOSE   (93)
         
C--     Monitoring         

        if(iitest==1)PRINT *,' UGR4_1 :',MINVAL( UGR4_1),MAXVAL (UGR4_1)
        if(iitest==1)PRINT *,' VGR4_1 :',MINVAL( VGR4_1),MAXVAL (VGR4_1)
        if(iitest==1)PRINT *,' TGR4_1 :',MINVAL( TGR4_1),MAXVAL (TGR4_1)
        if(iitest==1)PRINT *,' QGR4_1 :',MINVAL( QGR4_1),MAXVAL (QGR4_1)
        if(iitest==1)PRINT *,' PSGR4_1:',MINVAL(PSGR4_1),MAXVAL(PSGR4_1)


      ELSE
         PRINT *,'Invalid IMODE value',IMODE
         STOP
      ENDIF
C--
      RETURN

C--   4. Stop integration if gridded file is not found

c  200 CONTINUE
c
c      print*, ' Hey, what are you doing?',
c     &  ' fort.2 should contain time setting'
c
c      STOP 'invalid gridded data input'

C--
      END

