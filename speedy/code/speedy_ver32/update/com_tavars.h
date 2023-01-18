C--    Created by WA
C--   /TAVARS/ : diagnostic spectral variables for time averaged state 
C--              (initial. in INVARS, updated in STEP)
C--    VOR_Taver     = vorticity
C--    DIV_Taver     = divergence
C--    T_Taver       = abs. temperature
C--    PS_Taver      = log of (norm.) sfc pressure (p_s/p0)
C--    TR_Taver      = tracers (tr.1: spec. humidity in g/kg)

      COMMON  /TAVARS/ UGR4_Taver(NGP,KX),  UGR4_Tanom(NGP,KX),
     *                 VGR4_Taver(NGP,KX),  VGR4_Tanom(NGP,KX), 
     *                 TGR4_Taver(NGP,KX),  TGR4_Tanom(NGP,KX),
     *                 QGR4_Taver(NGP,KX),  QGR4_Tanom(NGP,KX),
     *                PSGR4_Taver(NGP)   , PSGR4_Tanom(NGP)   ,
     *                RRGR4_Taver(NGP)   , RRGR4_Tanom(NGP)
       
       REAL(4) UGR4_Taver,  UGR4_Tanom,  VGR4_Taver,  VGR4_Tanom, 
     *         TGR4_Taver,  TGR4_Tanom,  QGR4_Taver,  QGR4_Tanom,
     *        PSGR4_Taver, PSGR4_Tanom, RRGR4_Taver, RRGR4_Tanom

         
c         PHIGR4(NGP,KX), 
c      COMMON  /TAVARS/  VOR_Taver(MX,NX,KX,2), DIV_Taver(MX,NX,KX,2), 
c     *                  VOR_Tanom(MX,NX,KX,2), DIV_Tanom(MX,NX,KX,2),
c     *    T_Taver(MX,NX,KX,2),  PS_Taver(MX,NX,2), TR_Taver(MX,NX,KX,2,NTR),
c     *    T_Tanom(MX,NX,KX,2),  PS_Tanom(MX,NX,2), TR_Tanom(MX,NX,KX,2,NTR)
c
c      COMPLEX         VOR_Taver, DIV_Taver, T_Taver, PS_Taver, TR_Taver
c      COMPLEX         VOR_Tanom, DIV_Tanom, T_Tanom, PS_Tanom, TR_Tanom


C--              (as opposed to DYNSP1, DYNSP3 has only one time step)
c      COMMON /DYNSP3/ VOR_Taver(MX,NX,KX), DIV_Taver(MX,NX,KX), 
c     *                  T_Taver(MX,NX,KX),  PS_Taver(MX,NX),
c     *                 TR_Taver(MX,NX,KX,NTR)
