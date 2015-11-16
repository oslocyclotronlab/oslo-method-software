    subroutine make_angle_differences
    
	use shared_data
    implicit none

    integer                                               :: i                  ! loop counter
    integer                                               :: j                  ! loop counter
    integer                                               :: ilist              ! list count for angle differences
    integer                                               :: id1                ! first detector id number
    integer                                               :: id2                ! second detector id number

    real(4)                                               :: thetai             ! theta value
    real(4)                                               :: thetaj             ! theta value
    real(4)                                               :: phii               ! phi value
    real(4)                                               :: phij               ! phi value
    real(4)                                               :: CTH                ! cos theta between detectors
!    real(4),dimension(30,30)                              :: cosij              ! cos theta arrays
    integer                                               :: iptr               ! pointer for sorting
    integer                                               :: last_ang           ! previous nearest integer angle difference in list

    type(angle_list)                                      :: temp               ! temp save to sort angles in order                  


!  gamma_map(i)%Frame_pos,gamma_map(i)%ADC_ID,gamma_map(i)%ring_no,gamma_map(i)%theta_d,gamma_map(i)%phi_d,gamma_map(i)%notes
    OPEN (UNIT=20,File='Angle_Differences.out',status='replace')

    write(*,*)' A list of angular separations for CACTUS is being generated...'

    ilist=0
    do i=1,Ngamma_det
      do j=i+1,Ngamma_det
        if(trim(gamma_map(i)%notes) == 'EMPTY')cycle
        if(trim(gamma_map(j)%notes) == 'EMPTY')cycle
        if(gamma_map(i)%Frame_pos == -99)cycle
        if(gamma_map(j)%Frame_pos == -99)cycle
        ilist=ilist+1
        thetai=gamma_map(i)%theta_d
        phii=gamma_map(i)%phi_d
        thetaj=gamma_map(j)%theta_d
        phij=gamma_map(j)%phi_d
      	call cosang(CTH,thetai,phii,0.,0.,thetaj,phij)
      	cosij(i,j)=CTH
!        write(*,*)i,j,CTH
        cosine_list(ilist)%idet=i
        cosine_list(ilist)%jdet=j
        cosine_list(ilist)%cos_angle=(CTH)
        cosine_list(ilist)%angle_diff=acos(CTH)*sf
      enddo
    enddo
    Nlist=ilist   ! saves the number of items in the list    
    
!   Now sort into order of increasing cosine-theta:

    do i=1,Nlist-1
      iptr = i
      do j = i+1, Nlist
        if( cosine_list(j)%cos_angle < cosine_list(iptr)%cos_angle ) iptr = j
      enddo
! iptr now points to the minimum value, so swap a(iptr) with a(i) if i /= iptr.
      swap: if ( i /= iptr ) then
         temp = cosine_list(i)
         cosine_list(i) = cosine_list(iptr)
         cosine_list(iptr) = temp
      endif swap
    enddo
  
!   Now list out:
!    write(20,*)' List of angle differences in the CACTUS array:'
!    write(20,*)' ----------------------------------------------'
!    write(20,*)'  Angle(deg)    cosine          G1         G2'
!    write(20,*)' ----------------------------------------------'
!!    last_ang=nint(acos(cosine_list(1)%cos_angle)*sf)
!    last_ang=nint(cosine_list(1)%angle_diff)
!
!    do i=1,Nlist
!      if(nint(cosine_list(i)%angle_diff) /= last_ang)write(20,*)
!!      if(nint(acos(cosine_list(i)%cos_angle)*sf) /= last_ang)write(10,*)
!      write(20,'(2x,f8.2,6x,f8.4,8x,i3,8x,i3)')acos(cosine_list(i)%cos_angle)*sf,cosine_list(i)%cos_angle,cosine_list(i)%idet,cosine_list(i)%jdet
!!      last_ang=nint(acos(cosine_list(i)%cos_angle)*sf)
!      last_ang=nint(cosine_list(i)%angle_diff)
!    enddo
!    write(20,*)' ----------------------------------------------'
    
    
    write(20,'(a)')' --------------------------------------------------------------------------------------'
    write(20,*)'                        List of angle differences in the CACTUS array'
    write(20,'(a)')'  Angle(deg)     cosine           G1  ( theta ,  phi  )         G2   ( theta ,  phi  )'
    write(20,'(a)')' --------------------------------------------------------------------------------------'

    last_ang=nint(cosine_list(1)%angle_diff)
    do i=1,Nlist
      if(nint(cosine_list(i)%angle_diff) /= last_ang)write(20,*)
      id1=cosine_list(i)%idet
      id2=cosine_list(i)%jdet
      thetai=gamma_map(id1)%theta_d
      phii=gamma_map(id1)%phi_d
      thetaj=gamma_map(id2)%theta_d
      phij=gamma_map(id2)%phi_d
      write(20,'(2x,f8.2,6x,f8.4,8x,i3,3x,a,f6.1,a,f6.1,a,8x,i3,3x,a,f6.1,a,f6.1,a)') &
        acos(cosine_list(i)%cos_angle)*sf, &
        cosine_list(i)%cos_angle, id1,'(',thetai,' ,',phii,' )',id2,'(',thetaj,' ,',phij,' )'
      last_ang=nint(cosine_list(i)%angle_diff)
    enddo
    write(20,'(a)')' --------------------------------------------------------------------------------------'

    write(*,*)
    write(*,*)' Angles between CACTUS detectors are listed in: Angle_Differences.out'   
    write(*,*)

    write(10,*)
    write(10,*)' Angles between CACTUS detectors are listed in: Angle_Differences.out'   
    write(10,*)
    
    
    end subroutine make_angle_differences
!************************************************************************
!
	subroutine cosang(CTH,thR,phR,thS,phS,thG,phG)
!
!	************     angles in degrees   ***************
!
!	routine to calculate cosine of angle between nucleus 
!	direction of motion and gamma ray emission   AES Mar 2004
!
!	it is set up for DSAM where there can be scattering of the recoil
!	direction see AES 2004 logbook No. 1 page 34ff
!
!	"subscripts" are  R for Recoil 
!	                  S for Scattering 
!	                  G for Gamma detector
!
!	************     angles in degrees   ***************
!

	implicit none


	real*4 CTH  ! cos of angle between nuclear velocity and gamma ray
	real*4 thR,phR  ! specify recoil direction
	real*4 thS,phS  ! subsequent nuclear scatter 
	real*4 thG,phG  ! direction of gamma emission/detection
	real*4 SF/57.2957795/

	real*4 cthR,sthR,cphR,sphR
	real*4 cthS,sthS,cphS,sphS
	real*4 cthG,sthG,cphG,sphG

	real*4 XX,YY,ZZ
	real*4 XG,YG,ZG


	cthR=cos(thR/sf)
	sthR=sin(thR/sf)
	cphR=cos(phR/sf)
	sphR=sin(phR/sf)

	cthS=cos(thS/sf)
	sthS=sin(thS/sf)
	cphS=cos(phS/sf)
	sphS=sin(phS/sf)

	cthG=cos(thG/sf)
	sthG=sin(thG/sf)
	cphG=cos(phG/sf)
	sphG=sin(phG/sf)

!	xyz coordinates of recoil direction after scattering:

	XX=cthS*cphS*sthR*cphR - sphS*sthR*sphR + cthR*sthS*cphS
	YY=cthS*sphS*sthR*cphR + cphS*sthR*sphR + cthR*sthS*sphS
	ZZ= -sthS*sthR*cphR + cthR*cthS

!	xyz coordinates of gamma detector direction:

	XG=sthG*cphG
	YG=sthG*sphG
	ZG=cthG

!	cosine of required angle is dot product:

	CTH = XX*XG + YY*YG + ZZ*ZG

	return
	end
!************************************************************************


