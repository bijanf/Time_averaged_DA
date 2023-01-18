      NSTEPS = 36     ! Number of time steps in one day

C      NSTDIA = 36*30 ! Diagnostic print-out period
      NSTPPR = 6     ! Post-processing period
      NSTOUT = 36*30
C      IDOUT  = 1
C      IPOUT  = 1     ! pressure level gridded output flag
C      NMONRS = 3

      ISEASC = 1      ! Seasonal cycle flag (0:no, 1:yes)

      NSTRAD = 3      ! Short wave radiation period
      NSTRDF = 0      ! Random diabatic forcing duration
C    (0: none, >= 0: # of initial steps, < 0: whole integration  )      
      INDRDF = 0      ! Random diabatic forcing initialization index

C     surface temperature anomaly flags
      IASST  = 0 ! for land    (0:no, 1:slab-model)
      IAICE  = 0 ! for sea-ice (0:no, 1:slab-model)
      IALST  = 0 ! for sea     (0:no, 1:prescribed,
C               2:slab-model, 3:prescribed + slab-model)

      ISST0  = 25     ! Record in SST anomaly file corr. to initial month
C--
C--   Logical flags (common LFLAG1)
C      LPPRES = .true.
