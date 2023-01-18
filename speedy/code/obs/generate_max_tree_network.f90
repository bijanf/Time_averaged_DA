PROGRAM generate_max_tree_network
!=======================================================================
! [PURPOSE:] 
!> @brief For generating a station table with all gridboxes potentially
!> having trees. 
!> Criterion used: land-sea ratio >= 0.7 and albedo < 0.4
!>
!> @author Walter Acevedo (09.08.2013)
!=======================================================================
!$USE OMP_LIB
    USE common
    USE common_speedy
    USE common_obs_speedy

    IMPLICIT NONE
    real(4)                  :: r4inp(nlon,nlat)
    real(4)                  :: fmask1(nlon,nlat)
    real(4)                  :: alb0(nlon,nlat)
    integer                  :: i,j,out_unit = 456
    integer                  :: verbose = 1
    character(30)            :: station_file = 'max_tree_network.tbl'   
  
    !---------------------------------------------------------------------------
    ! Code fragment taken from speedy/source/ini_inforc.f
    !-   1. Read time-invariant fields (orography, land-sea mask, sfc albedo)

    !if (verbose.ge.1) print*,' read orography' 
    read (20) ((r4inp(i,j),i=1,nlon),j=nlat,1,-1)
    !do j = 1,nlat
    !   do i = 1,nlon
    !       phi0(i,j) = grav*r4inp(i,j)
    !   enddo
    !enddo
    !call truncg (ntrun,phi0,phis0)
 
    if (verbose.ge.1) print*,'Reading fractional land-sea mask'  
    read (20) ((r4inp(i,j),i=1,nlon),j=nlat,1,-1)
    do j = 1,nlat
        do i = 1,nlon
            fmask1(i,j) = r4inp(i,j)
        enddo
    enddo

    if (verbose.ge.1) print*,' Reading surface albedo'  
    read (20) ((r4inp(i,j),i=1,nlon),j=nlat,1,-1)
    do j = 1,nlat
        do i = 1,nlon
            alb0(i,j) = 0.01*r4inp(i,j)
        enddo
    enddo
!---------------------------------------------------------------------------

    CALL set_common_speedy

    if (verbose.ge.1) print*,'Selecting all land gridpoints potentially having trees'  
    
    open(UNIT=out_unit, FILE=station_file, FORM='FORMATTED')
    write(out_unit,'(A)') '  I  J'
    write(out_unit,'(A)') '------'

    do j = 1,nlat
        do i = 1,nlon
        
            ! getting rid of the sea
            if (fmask1(i,j) .lt. 0.7) cycle
            
            ! getting rid of Ice-covered areas
            if (alb0(i,j) .gt. 0.4) cycle
            
            write(out_unit,'(2I3)'),i,j
        enddo
    enddo
    close(out_unit)
    
    if (verbose.ge.1) print*, 'File ', station_file, ' was generated'

  STOP
END PROGRAM generate_max_tree_network
