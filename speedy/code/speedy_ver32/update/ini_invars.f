
      SUBROUTINE INVARS 
C--
C--   SUBROUTINE INVARS (ISTART)
C--
C--   Purpose : initialize all spectral variables starting from
C--             different types of restart datasets in sigma levels
C--   Input :   ISTART -> Input restart file mode (see com_tsteps.h for info)
C--
C--   Initialized common blocks : DATE1, DYNSP1, DYNSP2 (PHIS only),
C--                               SFCANOM, SFCFLUX
C--
      include "atparam.h"
      include "atparam1.h"

      PARAMETER (NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT)

      include "com_date.h"
      include "com_tsteps.h"

      include "com_dyncon0.h"
      include "com_dyncon1.h"

      include "com_dynvar.h"
      include "com_forcing.h"
      include "com_anomvar.h"

      include "com_grvars.h"
      include "com_physcon.h"  ! P0
      
      include "com_iitest.h"   ! Verbosity

      COMPLEX ZERO, CCON, SURFS(MX,NX)
      REAL  SURFG(IX,IL)
      
!       REAL(4) UGR4_1(NGP,KX),  VGR4_1(NGP,KX) 
!       REAL(4) TGR4_1(NGP,KX),  QGR4_1(NGP,KX), PSGR4_1(NGP)
      

      GAM1 = GAMMA/(1000.*GRAV)
      ZERO = (0.,0.)
      CCON = (1.,0.)*SQRT(2.)


C--   1. Compute spectral surface geopotential

      CALL SPEC (PHI0,PHIS)
      IF (IX.EQ.IY*4) CALL TRUNCT (PHIS)

      CALL GRID (PHIS,PHIS0,1)


      IF (ISTART.EQ.0) THEN

C--   2. Start from reference atmosphere (at rest) 

        if (iitest.eq.1) print*, 'Starting from rest'

        !IYEAR  = IYEAR0
        !IMONTH = IMONT0
        !IDATE = 1                                                        !TM
        !IHOUR = 0                                                        !TM

C       2.1 Set vorticity, divergence and tracers to zero

        DO K=1,KX
          DO N=1,NX
            DO M=1,MX
              VOR(M,N,K,1)=ZERO
              DIV(M,N,K,1)=ZERO
            ENDDO
          ENDDO
        ENDDO

        DO ITR=1,NTR
          DO K=1,KX
            DO N=1,NX
              DO M=1,MX
                TR(M,N,K,1,ITR)=ZERO
              ENDDO
            ENDDO
          ENDDO
        ENDDO

C       2.2 Set reference temperature :
C           tropos:  T = 288 degK at z = 0, constant lapse rate
C           stratos: T = 216 degK, lapse rate = 0

        TREF  = 288.
        TTOP  = 216.
        GAM2  = GAM1/TREF
        RGAM  = RGAS*GAM1
        RGAMR = 1./RGAM

C       Surface and stratospheric air temperature

        DO N=1,NX
          DO M=1,MX
            T(M,N,1,1)=ZERO
            SURFS(M,N)=-GAM1*PHIS(M,N)
          ENDDO
        ENDDO

        T(1,1,1,1)=CCON*TTOP
        SURFS(1,1)=CCON*TREF-GAM1*PHIS(1,1)

C       Temperature at tropospheric levels
        DO K=2,KX
          FACTK=FSG(K)**RGAM
          DO N=1,NX
            DO M=1,MX
              T(M,N,K,1)=SURFS(M,N)*FACTK
            ENDDO
          ENDDO
        ENDDO

C       2.3 Set log(ps) consistent with temperature profile
C           p_ref = 1013 hPa at z = 0   

        RLOG0=LOG(1.013)
        DO J=1,IL
          DO I=1,IX
            SURFG(I,J)=RLOG0+RGAMR*LOG(1.-GAM2*PHIS0(I,J))
          ENDDO
        ENDDO

        CALL SPEC (SURFG,PS)
        IF (IX.EQ.IY*4) CALL TRUNCT (PS)

C       2.4 Set tropospheric spec. humidity in g/kg
C           Qref = RHref * Qsat(288K, 1013hPa)

        ESREF=17.
        QREF=REFRH1*0.622*ESREF
        QEXP=HSCALE/HSHUM
        
C       Saturation spec. humidity at the surface 

        DO J=1,IL
          DO I=1,IX
            SURFG(I,J)=QREF*EXP(QEXP*SURFG(I,J))
          ENDDO
        ENDDO

        CALL SPEC (SURFG,SURFS)
        IF (IX.EQ.IY*4) CALL TRUNCT (SURFS)

C       Spec. humidity at tropospheric levels      

        DO K=2,KX
          FACTK=FSG(K)**QEXP
          DO N=1,NX
            DO M=1,MX
              TR(M,N,K,1,1)=SURFS(M,N)*FACTK
            ENDDO
          ENDDO
        ENDDO

C       Print diagnostics from initial conditions
        
c        CALL DIAGNS (1,0)
        

      ELSE IF (ISTART.EQ.1) THEN

C--   3. Start from a spectral restart file in sigma levels
C--                                       with 2 time steps
c         CALL RESTART (1)

        READ (3,END=200) IYEAR, IMONTH, IDATE, IHOUR

        if (iitest.eq.1) PRINT '(A,I4.4,A,I2.2,A,I2.2,A,I2.2)',
     &       'Read spectral restart dataset for ',
     &       IYEAR,'/',IMONTH,'/',IDATE,'/',IHOUR
     
        READ (3) VOR
        READ (3) DIV
        READ (3) T
        READ (3) PS
        READ (3) TR

        READ (3) STANOM
         
C       Print diagnostics from initial conditions
  
c        CALL DIAGNS (1,0)
c        CALL DIAGNS (2,0)


      ELSE IF (ISTART.EQ.2) THEN

C--   4. Start from a gridded restart file in sigma levels
C--                                       with 1 time step
	  CALL IOGRID (1)
c        if (iitest.eq.1) PRINT '(A,I4.4,A,I2.2,A,I2.2,A,I2.2)',
c     &                         ' Read gridded dataset for ',
c     &                           IYEAR,'/',IMONTH,'/',IDATE,'/',IHOUR
     
cc       4.1 Read binary dataset     

c        OPEN (90,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NGP)
c        irec=1
c        DO K=KX,1,-1
c          READ (90,REC=irec) ( UGR4(j,k),j=1,NGP)
c          irec=irec+1
c        END DO
c        DO K=KX,1,-1
c          READ (90,REC=irec) ( VGR4(j,k),j=1,NGP)
c          irec=irec+1
c        END DO
c        DO K=KX,1,-1
c          READ (90,REC=irec) ( TGR4(j,k),j=1,NGP)
c          irec=irec+1
c        END DO
c        DO K=KX,1,-1
c          READ (90,REC=irec) ( QGR4(j,k),j=1,NGP)
c          irec=irec+1
c        END DO
c        READ    (90,REC=irec) (PSGR4(j)  ,j=1,NGP)
c        CLOSE   (90)

cC--     1.2 Conversion from single prec. gridded vars to double prec. ones 

c        UGR =  UGR4
c        VGR =  VGR4
c        TGR =  TGR4
c        QGR =  QGR4 *1.0d3
c        PSGR = PSGR4
c        PSGR = log(PSGR/P0)

cC--     Monitoring         

c        if(iitest==1) PRINT *,' UGR  :',MINVAL( UGR) ,MAXVAL (UGR)
c        if(iitest==1) PRINT *,' VGR  :',MINVAL( VGR) ,MAXVAL (VGR)
c        if(iitest==1) PRINT *,' TGR  :',MINVAL( TGR) ,MAXVAL (TGR)
c        if(iitest==1) PRINT *,' QGR  :',MINVAL( QGR) ,MAXVAL (QGR)
c        if(iitest==1) PRINT *,' PSGR :',MINVAL(PSGR) ,MAXVAL(PSGR)

cC--     1.3 Conversion from gridded variable to spectral variable

c        DO K=1,KX
c          CALL VDSPEC(UGR(1,K),VGR(1,K),VOR(1,1,K,1),DIV(1,1,K,1),2)
             
c          IF(IX.EQ.IY*4) THEN
c            CALL TRUNCT(VOR(1,1,K,1))
c            CALL TRUNCT(DIV(1,1,K,1))
c          END IF
             
c          CALL SPEC(TGR(1,K),T(1,1,K,1))
c          CALL SPEC(QGR(1,K),TR(1,1,K,1,1))
             
c        END DO
c        CALL SPEC(PSGR(1),PS(1,1,1))

C       Print diagnostics from initial conditions

c        CALL DIAGNS (1,0)
        

      ELSE IF (ISTART.EQ.3) THEN
      
C--   5. Start from time-average state and anomaly gridded states
c--      in sigma levels with 1 time step
      
c        CALL TA_VARS (3)
C        3.1 Read binary datasets
                                                   !TM
           CALL IOGRID (6)                             
                                                   
!         if (iitest.eq.1) PRINT *,'Reading time-averaged gridded dataset'
! 
!           OPEN (92,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NGP)
!           irec=1
!           DO K=KX,1,-1
!             READ (92,REC=irec) ( UGR4(j,k),j=1,NGP)
!              irec=irec+1
!           END DO
!           DO K=KX,1,-1
!             READ (92,REC=irec) ( VGR4(j,k),j=1,NGP)
!             irec=irec+1
!           END DO
!           DO K=KX,1,-1
!             READ (92,REC=irec) ( TGR4(j,k),j=1,NGP)
!             irec=irec+1
!           END DO
!           DO K=KX,1,-1
!             READ (92,REC=irec) ( QGR4(j,k),j=1,NGP)
!             irec=irec+1
!           END DO
!           READ    (92,REC=irec) (PSGR4(j)  ,j=1,NGP)
!           CLOSE   (92)
! 
! C--       Monitoring         
! 
!           if(iitest==1) PRINT *,' UGR4  :',MINVAL( UGR4) ,MAXVAL (UGR4)
!           if(iitest==1) PRINT *,' VGR4  :',MINVAL( VGR4) ,MAXVAL (VGR4)
!           if(iitest==1) PRINT *,' TGR4  :',MINVAL( TGR4) ,MAXVAL (TGR4)
!           if(iitest==1) PRINT *,' QGR4  :',MINVAL( QGR4) ,MAXVAL (QGR4)
!           if(iitest==1) PRINT *,' PSGR4 :',MINVAL(PSGR4) ,MAXVAL(PSGR4)
! 
!           if (iitest.eq.1) PRINT *,
!      &                    'Reading time average anomaly gridded dataset'
! 
!           OPEN (93,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4*NGP)
!           irec=1
!           DO K=KX,1,-1
!             READ (93,REC=irec) ( UGR4_1(j,k),j=1,NGP)
!             irec=irec+1
!           END DO
!           DO K=KX,1,-1
!             READ (93,REC=irec) ( VGR4_1(j,k),j=1,NGP)
!             irec=irec+1
!           END DO
!           DO K=KX,1,-1
!             READ (93,REC=irec) ( TGR4_1(j,k),j=1,NGP)
!             irec=irec+1
!           END DO
!           DO K=KX,1,-1
!             READ (93,REC=irec) ( QGR4_1(j,k),j=1,NGP)
!             irec=irec+1
!           END DO
!           READ    (93,REC=irec) (PSGR4_1(j)  ,j=1,NGP)
!           CLOSE   (93)
! 
! C--     Monitoring         
! 
!         if(iitest==1)PRINT *,' UGR4_1 :',MINVAL( UGR4_1),MAXVAL (UGR4_1)
!         if(iitest==1)PRINT *,' VGR4_1 :',MINVAL( VGR4_1),MAXVAL (VGR4_1)
!         if(iitest==1)PRINT *,' TGR4_1 :',MINVAL( TGR4_1),MAXVAL (TGR4_1)
!         if(iitest==1)PRINT *,' QGR4_1 :',MINVAL( QGR4_1),MAXVAL (QGR4_1)
!         if(iitest==1)PRINT *,' PSGR4_1:',MINVAL(PSGR4_1),MAXVAL(PSGR4_1)

C--      Recompose instantatenous initial state

         DO K=KX,1,-1
           DO j=1,NGP
             UGR4(j,k) = UGR4(j,k) + UGR4_1(j,k)
           END DO
         END DO
         DO K=KX,1,-1
           DO j=1,NGP
             VGR4(j,k) = VGR4(j,k) + VGR4_1(j,k)
           END DO
         END DO
         DO K=KX,1,-1
           DO j=1,NGP
             TGR4(j,k) = TGR4(j,k) + TGR4_1(j,k)
           END DO
         END DO
         DO K=KX,1,-1
           DO j=1,NGP
             QGR4(j,k) = QGR4(j,k) + QGR4_1(j,k)
           END DO
         END DO
         DO j=1,NGP
           PSGR4(j) = PSGR4(j) + PSGR4_1(j)
         END DO
      
C--       Monitoring         
        if (iitest.eq.1) PRINT *,
     &                    'Recomposed instantatenous initial state:'

          if(iitest==1) PRINT *,' UGR4  :',MINVAL( UGR4) ,MAXVAL (UGR4)
          if(iitest==1) PRINT *,' VGR4  :',MINVAL( VGR4) ,MAXVAL (VGR4)
          if(iitest==1) PRINT *,' TGR4  :',MINVAL( TGR4) ,MAXVAL (TGR4)
          if(iitest==1) PRINT *,' QGR4  :',MINVAL( QGR4) ,MAXVAL (QGR4)
          if(iitest==1) PRINT *,' PSGR4 :',MINVAL(PSGR4) ,MAXVAL(PSGR4)

C--     1.2 Conversion from single prec. gridded vars to double prec. ones 

        UGR =  UGR4
        VGR =  VGR4
        TGR =  TGR4
        QGR =  QGR4 *1.0d3
        PSGR = PSGR4
        PSGR = log(PSGR/P0)


        if (iitest.eq.1) PRINT *,
     &                'Scaled Recomposed instantatenous initial state:'

C--     Monitoring         

        if(iitest==1) PRINT *,' UGR  :',MINVAL( UGR) ,MAXVAL (UGR)
        if(iitest==1) PRINT *,' VGR  :',MINVAL( VGR) ,MAXVAL (VGR)
        if(iitest==1) PRINT *,' TGR  :',MINVAL( TGR) ,MAXVAL (TGR)
        if(iitest==1) PRINT *,' QGR  :',MINVAL( QGR) ,MAXVAL (QGR)
        if(iitest==1) PRINT *,' PSGR :',MINVAL(PSGR) ,MAXVAL(PSGR)

C--     1.3 Conversion from gridded variable to spectral variable

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

      ELSE

        PRINT *,'Invalid ISTART value : ', ISTART
        STOP

      ENDIF


C     Print diagnostics from initial conditions

      CALL DIAGNS (1,0)
      
      RETURN
      
C--   4. Stop integration if restart file is not found

  200 CONTINUE

      print*, ' No restart dataset for the specified initial date'

      STOP 'invalid restart'
      
      END

