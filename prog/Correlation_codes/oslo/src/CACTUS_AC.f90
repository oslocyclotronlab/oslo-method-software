	program CACTUS_AC
	
	use shared_data
	
	implicit none
    

    integer                      :: iop                ! output logical unit number for write

    character(len=4)             :: CMD='EXIT'         ! command
    character(len=80)            :: linin              ! line in
    integer                      :: Len                ! line length on read
    integer                      :: read_status        ! read status from iostat

    L_data_loaded = .false.


!   Open output files:
!
    OPEN (UNIT=10,File='CACTUS_AC.out',status='replace')

!
!   Print welcome
!

    do iop=6,10,4                                            
      write(iop,*)' ======================================================================='
      write(iop,*)' '
      write(iop,*)'           Welcome to CACTUS_AC code - Andrew Stuchbery'
      write(iop,*)'                          January 2014'
      write(iop,*)'    Angular correlation formulae:   Nuclear Physics A 723 (2003) 69'
      write(iop,*)' '
      write(iop,*)'          Looks for angle data in file: Detector_map.sav'
      write(iop,*)'     In this code the Z-axis is always in the direction of the beam'
      write(iop,*)' '
      call date4(DAY)
      call TIME(ITIME)
      write(iop,'(10x,a,A11,T40,6x,a,A8)')'  Date: ',DAY,'Time: ',ITIME
      write(iop,*)' ======================================================================='
      write(iop,*)' '
    enddo



!   load detector angles from file, if it is there
    call load_detector_map
    
!   calculate angle differences Iif data from file has been loaded)
    if(L_data_loaded)then
      call make_angle_differences
    else
      write(*,*)' Angle data has not been read' 
      write(*,*)'  - you can continue to calculate theoretical correlations'
      write(*,*)
    endif
    call help
!   Now come the user-selected commands:
      do
        write(*,*)
        do
          write(*,'(a,a,a)',advance='no')' Command [',CMD,'] >  '
          read(*,'(a)',advance='no',size=Len,iostat=read_status)linin
          if(read_status > 0)cycle
          if(Len.ne.0)read(linin(1:Len),*,iostat=read_status)CMD
          if(read_status > 0)cycle
          if(read_status <= 0)exit
        enddo
        call ucase(CMD)
        CMD=trim(CMD)

        commands: select case (CMD)
        case ('HELP')
          call help
        case('AC')
          call angular_correlations

        case('DCO')
          call directional_correlations

        case ('EXIT')
          close(10)
          exit
        case ('EX')
          close(10)
          exit
        
        case default
        write(*,*)' Invalid command'
        end select commands
      enddo

    
	end program CACTUS_AC
!========================================================================================	

!========================================================================================	
SUBROUTINE ucase ( string )
! 
!  Purpose: 
!    To shift a character string to upper case on any processor,
!    regardless of collating sequence.
! 
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/25/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Declare calling parameters:
CHARACTER(len=*), INTENT(INOUT) :: string  

! Declare local variables:
INTEGER :: i                 ! Loop index
INTEGER :: length            ! Length of input string
 
! Get length of string
length = LEN ( string )
 
! Now shift lower case letters to upper case.
DO i = 1, length
   IF ( LGE(string(i:i),'a') .AND. LLE(string(i:i),'z') ) THEN
      string(i:i) = ACHAR ( IACHAR ( string(i:i) ) - 32 )
   END IF
END DO

END SUBROUTINE ucase
!========================================================================================	
!========================================================================================	

    subroutine help

    write(*,*)' '
    write(*,*)' '
!    write(*,*)'-----------------------------------------------------------------'
    write(*,*)' '
    write(*,*)' Select from the following commands (not case sensitive):'
    write(*,*)' '
    write(*,*)' HELP - produces this listing'
    write(*,*)' AC - gamma-gamma angular correlation from isotropic source'
    write(*,*)' DCO - gamma-gamma angular correlation from aligned state'
    write(*,*)' (DCO also prints angular distribution from the 1st state)'
    write(*,*)' EXIT - exit program'
    write(*,*)' '


    write(*,*)'-----------------------------------------------------------------'
    write(*,*)' '
    end subroutine help
