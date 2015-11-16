!************************************************************************
	subroutine load_detector_map

    use shared_data
	implicit  none

    integer                                               :: read_status        ! read status from iostat
    integer                                               :: i                  ! loop counter
    integer                                               :: idum               ! dummy variable in read

    L_data_loaded=.false.
!   open .sav file if it exists
    write(*,*)' Looking for data file with detector map: Detector_map.sav'
    open(1,file='Detector_map.sav',status='old',iostat=read_status)
    if(read_status <= 0)then
      write(*,*)' Detector_map.sav file successfully opened'

!	  **** start input *****
      call rsharps(1)
      read(1,*) Ngamma_det
      call rsharps(1)   
      do i=1,Ngamma_det
        read(1,*)gamma_map(i)%Frame_pos,gamma_map(i)%ADC_ID,gamma_map(i)%ring_no,gamma_map(i)%theta_d,gamma_map(i)%phi_d,gamma_map(i)%notes
        call ucase(gamma_map(i)%notes)
        Ring_Angles(gamma_map(i)%ring_no)=gamma_map(i)%theta_d
      enddo
!      call rsharps(1)
      L_data_loaded=.true.
      write(*,*)' Detector_map.sav file has been read'
    endif
    close(1)


	return
	end 	subroutine load_detector_map

!*******************************************************************************
!       
    subroutine rsharps(iunit)
    implicit none
!
!  allows comments in input files. A comment line starts with the
!  symbol "#" in the input file.
!
    character*1 isharp
    integer ish,iunit
!
    ish=1
    do while(ish==1)
     ish=0
     read(iunit,'(a1)')isharp
     if(isharp.eq.'#')ish=1
    enddo
    backspace iunit
!
    return
    end
!*******************************************************************************
