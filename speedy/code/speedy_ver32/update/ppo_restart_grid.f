C>======================================================================
C> @brief Reads or writes a gridded restart file.
C> @param[in]  TASK  = 'read' | 'save'
C> @param[in]  SPACE = 'spec' | 'grid'
C> @return     0 if successful or 1 if failed
C>
C> @note As opposed to Spectral restart files which have 2 time steps,
C> gridded ones have only 1 and therefore to start from a gridded state
C> it is always necessary to perform a Euler Forward step (STEPONE)
C> before doing leapfrogstep
C=======================================================================

      SUBROUTINE RESTART_GRID (TASK, LEVELS)

      include "atparam.h"
      include "atparam1.h"
      PARAMETER (NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT)
      include "com_date.h"
c      include "com_tsteps.h"
c      include "com_dyncon1.h"
c      include "com_physcon.h"  ! P0
      include "com_grvars.h"
      include "com_tavars.h"

      include "com_iitest.h"   ! Verbosity

cc iogrid includes
c      include "com_physcon.h"
c      include "com_physvar.h"
c      include "com_dynvar.h"
c      include "com_dyncon1.h"
c      include "com_anomvar.h"

c      include "com_date.h"
c      include "com_tsteps.h"
c      include "par_tmean.h"
c      include "com_tmean_daily.h"

c      include "com_grvars.h"
c      include "com_iitest.h"   ! Verbosity

      
      CHARACTER(4),INTENT(IN):: TASK
      CHARACTER(5),INTENT(IN):: LEVELS
      CHARACTER(10)          :: itime_str
      CHARACTER(50)          :: rst_name

C     Setting present time string
      WRITE (itime_str(1:10),'(I10)') ITIME

C     Setting filename
      rst_name = itime_str//'_grid_'//LEVELS//'_Insta.rst'
      
      IF ((TASK.EQ.'read').AND.(LEVELS.EQ.'sigma')) THEN

        PRINT*,'Reading gridded sigma restart file '
        PRINT*, rst_name
        
        OPEN (90,FILE   = rst_name, FORM = 'UNFORMATTED',
     &           ACCESS = 'DIRECT', RECL = 4*NGP)    
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

      ELSE IF ((TASK.EQ.'save').AND.(LEVELS.EQ.'sigma')) THEN

        PRINT*,'Saving gridded sigma restart file '
        PRINT*, rst_name

        CALL WRITE_GR4(rst_name, UGR4, VGR4, TGR4, QGR4, PSGR4, RRGR4)
        
C       Store binary name
        OPEN (196,FILE = 'grid_sigma_Insta_state_name.dat',
     &            FORM = 'FORMATTED')
        WRITE(196,'(A)') trim(rst_name)
        CLOSE(196)


      ELSE IF ((TASK.EQ.'read').AND.(LEVELS.EQ.'press')) THEN

        WRITE(0,*) 'Error: Currently it is not possible to start'
        WRITE(0,*) 'from pressure level restart files'
        CALL EXIT (1)

      ELSE IF ((TASK.EQ.'save').AND.(LEVELS.EQ.'press')) THEN

        PRINT*,'Saving gridded pressure-level restart file '
        PRINT*, rst_name

        CALL WRITE_GR4_PRESSURE(rst_name,   UGR4,  VGR4,  TGR4,
     &                              QGR4, PHIGR4, PSGR4, RRGR4)

C       Store binary name
        OPEN (196,FILE = 'grid_press_Insta_state_name.dat',
     &            FORM = 'FORMATTED')
        WRITE(196,'(A)') trim(rst_name)
        CLOSE(196)

      ELSE
      
        WRITE(0,*) 'Error: Unsupported TASK-LEVELS combination:  '
        WRITE(0,*) TASK, '-', LEVELS
        CALL EXIT (1)

      END IF

      RETURN
      END

