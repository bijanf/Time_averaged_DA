C--
C--   /GRVARS/ : Gridded variables for postprocessing 
C--              (used in ppo_iogrid.f)
C--    VOR     = vorticity
C--    DIV     = divergence
C--    T       = abs. temperature
C--    PS      = log of (norm.) sfc pressure (p_s/p0)
C--    TR      = tracers (tr.1: spec. humidity in g/kg)

      COMMON /GRVARS/ UCOSTMP(MX,NX),VCOSTMP(MX,NX),
     *             UGR (NGP,KX),   VGR (NGP,KX),  TGR (NGP,KX),
     *             UGR1(NGP,KX),   VGR1(NGP,KX),  TGR1(NGP,KX),
     *             UGR4(NGP,KX),   VGR4(NGP,KX),  TGR4(NGP,KX),
     *             QGR (NGP,KX), PHIGR (NGP,KX), PSGR (NGP),
     *             QGR1(NGP,KX), PHIGR1(NGP,KX),             RRGR1(NGP),
     *             QGR4(NGP,KX), PHIGR4(NGP,KX), PSGR4(NGP), RRGR4(NGP),
     *             UGR4_1(NGP,KX),  VGR4_1(NGP,KX), TGR4_1(NGP,KX),
     *             QGR4_1(NGP,KX), PSGR4_1(NGP)

      COMPLEX UCOSTMP,VCOSTMP
      REAL    UGR ,   VGR,    TGR,    QGR,    PHIGR,  PSGR
      REAL    UGR1,   VGR1,   TGR1,   QGR1,   PHIGR1,        RRGR1
      REAL(4) UGR4,   VGR4,   TGR4,   QGR4,   PHIGR4, PSGR4, RRGR4
      REAL(4) UGR4_1, VGR4_1, TGR4_1, QGR4_1, PSGR4_1
