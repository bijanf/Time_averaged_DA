PROGRAM Emean_spread
!$USE OMP_LIB
USE common
USE common_mpi
USE common_speedy
USE common_mpi_speedy
USE common_letkf
USE letkf_obs
USE letkf_tools

IMPLICIT NONE
REAL(r_size),ALLOCATABLE :: gues3d(:,:,:,:)
REAL(r_size),ALLOCATABLE :: gues2d(:,:,:)
REAL(r_size) :: rtimer00,rtimer
INTEGER      :: ierr
CHARACTER(8) :: stdoutf='NOUT-000'
CHARACTER(4) ::   guesf='gs00'

!-----------------------------------------------------------------------
! Initial settings
!-----------------------------------------------------------------------
CALL CPU_TIME(rtimer00)
CALL initialize_mpi
!
WRITE(stdoutf(6:8), '(I3.3)') myrank
WRITE(6,'(3A,I3.3)') 'STDOUT goes to ',stdoutf,' for MYRANK ', myrank
OPEN(6,FILE=stdoutf)
WRITE(6,'(A,I3.3,2A)') 'MYRANK=',myrank,', STDOUTF=',stdoutf
!
CALL set_common_speedy
CALL set_common_mpi_speedy
ALLOCATE(gues3d(nij1,nlev,nbv,nv3d))
ALLOCATE(gues2d(nij1,     nbv,nv2d))

! Start timer

CALL CPU_TIME(rtimer)
WRITE(6,'(A,2F10.2)') '### TIMER(INITIALIZE):',rtimer,rtimer-rtimer00
rtimer00=rtimer

! Read prior ensemble

CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
WRITE(guesf(3:4),'(I2.2)') nbslot
CALL read_ens_mpi(guesf,nbv,gues3d,gues2d)

! Write prior ensemble mean and spread

CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
CALL write_ensmspr_mpi('gues',nbv,gues3d,gues2d)
!
CALL CPU_TIME(rtimer)
WRITE(6,'(A,2F10.2)') '### TIMER(READ_GUES):',rtimer,rtimer-rtimer00
rtimer00=rtimer

! Finalize

CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
CALL finalize_mpi

STOP
END PROGRAM Emean_spread
