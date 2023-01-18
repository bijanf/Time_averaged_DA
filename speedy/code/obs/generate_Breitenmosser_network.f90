PROGRAM generate_Breitenmosser_network
  !=======================================================================
  ! [PURPOSE:]
  !> @brief  Creates a station set resembling Breitenmosser TRW network
  !> @author Walter Acevedo (24.08.2014)
  !=======================================================================
  USE common
  USE common_tools
  USE common_speedy
  USE common_obs_speedy
  IMPLICIT NONE
  integer           :: trw_mask(nlon,nlat) = 0
  ! integer           :: trw_mask_new(nlon,nlat) = 0
  integer           :: max_tree_mask(nlon,nlat) = 0
  real(4)           :: chrono_lon, chrono_lat
  integer           :: i,j,n_chronos,n_stations,ios, nstation
  integer           :: inp_unit = 455
  integer           :: out_unit = 456
  character(30)     :: station_coord_file = '1901_1970__effective_coord.txt'
  character(30)     :: station_file       =      'Breitenmosser_network.tbl'
  integer,parameter :: verbose  = 0
  character(30)     :: program_name       = 'generate_Breitenmosser_network'
  CHARACTER(100)           :: cdummy

  CALL funct_opening(program_name)

  CALL set_common_speedy

  CALL print_msg('Finding chronology grid box index')

  open(inp_unit, FILE=station_coord_file, FORM='FORMATTED')

  n_chronos = 0
  DO
     READ(inp_unit,'(2E16.7)',IOSTAT=ios) chrono_lon, chrono_lat

     IF(ios /= 0) EXIT

     IF(verbose.ge.1) CALL monitor_real4_0D(chrono_lon,'chrono_lon')
     IF(verbose.ge.1) CALL monitor_real4_0D(chrono_lat,'chrono_lat')

     !        i = CEILING(chrono_lon / 360.0d0 * REAL(nlon,r_size))
     ! DO i = 1,nlon-1
     !   IF(chrono_lon < (lon(i)+lon(i+1))/2.0) EXIT
     ! END DO
     ! DO i = 1,nlon
     !    if(i.eq.1)then
     !       IF(chrono_lon < lon(i) - (lon(i+1)-lon(i))/2.0) EXIT
     !    else
     !       IF(chrono_lon < (lon(i-1)+lon(i))/2.0) EXIT
     !    end if
     ! END DO
     DO i = 1,nlon
       IF(chrono_lon < lon(i)) EXIT
     END DO

     DO j = 1,nlat
        IF(chrono_lat < lat(j)) EXIT
     END DO

     if(i.EQ.97) i=1

     trw_mask(i,j) = 1

     IF(verbose.ge.1) CALL monitor_intg_0D(i,'i')
     IF(verbose.ge.1) CALL monitor_intg_0D(j,'j')

     n_chronos = n_chronos + 1
  END DO
  close(inp_unit)

  CALL monitor_intg_0D(n_chronos,'Number of chronologies')

  CALL print_msg('Finding intersection with land sea mask')

  OPEN(10,FILE='station_tree_coverage_max.tbl')
  READ(10,'(A)') cdummy
  READ(10,'(A)') cdummy
  nstation = 0
  DO
     READ(10,'(2I3)',IOSTAT=ios) i,j
     IF(ios /= 0) EXIT
     max_tree_mask(i,j) = 1
     nstation = nstation + 1
     ! print*, i,j
  END DO
  CLOSE(10)
  IF(verbose.ge.1) PRINT*, 'Number of stations = ', nstation

  trw_mask = trw_mask * max_tree_mask

  ! print*,SUM(max_tree_mask)
  ! print*,SUM(trw_mask)
  ! trw_mask = trw_mask * max_tree_mask
  ! trw_mask = trw_mask - trw_mask_new
  ! print*,SUM(trw_mask)
  ! call monitor_intg_2D(max_tree_mask,'max_tree_mask- trw_mask')
  ! print*,max_tree_mask
  ! read *

  ! print*,trw_mask

  CALL print_msg('Writing station indices')

  open(out_unit, FILE=station_file, FORM='FORMATTED')
  write(out_unit,'(A)') '  I  J'
  write(out_unit,'(A)') '------'

  n_stations = 0
  do j = 1,nlat
     do i = 1,nlon
        IF (trw_mask(i,j).EQ.1) THEN
           n_stations = n_stations + 1
           write(out_unit,'(2I3)'),i,j
        END IF
     enddo
  enddo
  close(out_unit)

  CALL monitor_intg_0D(n_stations,'Number of stations')

  CALL print_msg('File '//station_file//' was generated')
  CALL funct_closing(program_name)


  STOP
END PROGRAM generate_Breitenmosser_network
