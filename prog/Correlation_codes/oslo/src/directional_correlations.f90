    subroutine directional_correlations

!   based on dcoplot

	use shared_data
	use shared_DCO_parameters
	implicit none

    type(ac_list)                                         :: temp               ! temp save to sort dco in order                  
    integer                                               :: iptr               ! pointer for sorting
    integer                                               :: id1                ! frame pos det number for 1st radiation
    integer                                               :: id2                ! frame pos det number for 1st radiation
    real(4)                                               :: sum                ! sum of DCO over array

    character(len=80)                                     :: linin              ! line in
    integer                                               :: Len                ! line length on read
    integer                                               :: read_status        ! read status from iostat
    complex(4),dimension(0:8,-8:8)                        :: AK                 ! Ak coefficient usually only RE A(2,0) and A(4,0) nonzero
    complex(4),dimension(0:8,-8:8)                        :: AQK                ! Ak coefficient including QK
    real(4)                                               :: A                  ! General A coefficient (mixed Fk's) 

	integer l,i,j
	integer nlev,nlvl
	integer mode,modeo
	integer k1,k1q
	integer k2,q1,q2,k,kq
	integer ithet,ilist,Ndco

    real*4 F
    real*8 FK
	real*4 Ilev(3),Imax
	real*4 L1(3),L2(3),delta(3)
	real*4 sigJ,sigma
	real*4 theta
	real*4 test,wdco

	real*4 RHOKT(0:68),JZJ,rk
	real*4 SI(69),PPI(69),AM0,AMI

!	real*4  SF/57.2957795/
!	real*4 pi/3.141592654/

	complex*8 rhoin(0:68,-68:68)
	complex*8 rhoout(0:68,-68:68)
	complex*8 z1/(1.,0.)/


	character line*80
	character cmode*1,cm_sav*1

    OPEN (UNIT=30,File='AngDist.pdat',status='replace')

!	default parameters

	nlev=3 ! number of levels
	modeo=0      ! oblate/prolate substate distribution (oblate is default)
	sigJ=0.35    ! gaussian parameter for substate distn

	mode=1
	cmode='y'

10	format(a)

!
!	To include finite detectors must modify code to read and replace Qk 
!	values for each observed transition.
!
	do k=0,6
	  Qk(k)=1.
	enddo
    Qk(2)=0.981
    Qk(4)=0.937

!	initialize spins and transitions etc to default values


    I1=3.
    I2=2.
    I3=0.
    Ilev(1)=I1
    Ilev(2)=I2
    Ilev(3)=I3

    L1_1=1
    L2_1=2
    del1=-0.14
    L1_2=2.
    L2_2=2.
    del2=1000.

    L1(1)=L1_1
    L2(1)=L2_1
    delta(1)=del1
    L1(2)=L1_2
    L2(2)=L2_2
    delta(2)=del2


    write(*,*)

!260	  write(6,261)Qk(2),Qk(4)
!261	  format('  Q2,Q4 ['2(f8.4,2x)']',$)
!	  read(5,10,advance='no',size=l,iostat=read_status)line
!	  if(l.ne.0)read(line(1:l),*,err=260)Qk(2),Qk(4)

    do
      write(*,'(a,2(f8.4,2x),a)',advance='no')'  Q2,Q4 [',Qk(2),Qk(4),']'
      read(*,'(a)',advance='no',size=Len,iostat=read_status)linin
      if(read_status > 0)cycle
      if(Len.ne.0)then
        read(linin(1:Len),*,iostat=read_status)Qk(2),Qk(4)
        if(read_status > 0)cycle
        exit
      else
        exit
      endif
    enddo
    write(*,*)



	write(6,*)' '
	write(6,*)' '
	write(6,*)' Level 1 is the initial state for which the alignment from'
	write(6,*)' the reaction is specified.'
	write(6,*)' Gamma decay then proceeds:'
	write(*,*)' level 1 --> level 2 --> level 3'
	write(6,*)' '
	write(6,*)' You do a manual calculation for a pair of detection angles first'
	write(6,*)'  - then the code steps through the CACTUS detector angles'
	write(6,*)' '


!200	write(6,201)nlev
!201	format('  Number of levels in cascade path (3 or 0 to exit) ['(I3)']',$)
!	read(5,10,advance='no',size=l,iostat=read_status)line
!	if(l.ne.0)read(line(1:l),*,err=200)nlev
!	if(nlev.gt.3) nlev=3
!	if(nlev.le.0) return

    do

    do
      write(*,'(a,i3,a)',advance='no')'  Number of levels in cascade path (3 or 0 to exit) [',nlev,']'
      read(*,'(a)',advance='no',size=Len,iostat=read_status)linin
      if(read_status > 0)cycle
      if(Len.ne.0)then
        read(linin(1:Len),*,iostat=read_status)nlev
        if(read_status > 0)cycle
        exit
      else
        exit
      endif
    enddo
	if(nlev.gt.3) nlev=3
	if(nlev.le.0) exit


!	read in levels and spins and find largest spin:

	Imax=0.0
	do nlvl=1,nlev
210	  write(6,211)nlvl,Ilev(nlvl)
211	  format('  Spin of level number 'i4' ['f8.2']',$)
	  read(5,10,advance='no',size=l,iostat=read_status)line
	  if(l.ne.0)read(line(1:l),*,err=210)Ilev(nlvl)
	  if(Ilev(nlvl).lt.0.) cycle !goto 200
	  if(Ilev(nlvl).gt.34.) then
	    write(6,*)'  Sorry - only spins up to 34 are allowed at present!'
	    goto 210
	  endif
	  Imax=max(Imax,Ilev(nlvl))
	enddo
	write(6,*)'  Imax = ',Imax


!	kmax is kept general here. If only L=1 and L=2 multipolarities
!	are present in any radiation, then 
!	  kmax = 4*(no. of transitions observed)
!
	kmax=nint(2.*Imax)   ! this is correct in general 
!	kmax=min(12,2*(nint(2*Imax)/2)) !  OK in most applications


!	set up tensors  for initial state

	write(6,*)' Specify aligment of initial state:'
	write(6,*)' Oblate alignment means mostly m = 0'
!	write(6,*)' Prolate alignment means mostly m = ± I'
	write(6,*)' Prolate alignment means mostly m = +/- I'
	write(6,*)' '
220	write(6,221)modeo
221	format('  prolate [1]/oblate [0] alignment ['(i4,2x)']',$)
	read(5,10,advance='no',size=l,iostat=read_status)line
	if(l.ne.0)read(line(1:l),*,err=220)modeo
	write(6,*)' '
	write(6,*)' Alignment parameter is sigma/I (see AES NPA 723(2003)69)'

!	"Gamma-ray angular distributions and correlations after projectile 
!	fragmentation reactions" by A.E. Stuchbery, NPA8228, is available
!	on-line as an uncorrected proof since 3-April-2003.
!
!   REFERENCE IS:   Nuclear Physics A 723 (2003) 69–92.
!
!	Nuclear Physics A paper, in press, sets out philosophy for evaluation
!	of the statistical tensors of the initial state. This is a general 
!	routine that allows prolate or oblate alignment, or polarization, in
!	the initial state.

230	write(6,231)sigJ
231	format('  sigma/I ['(g10.4)']',$)
	read(5,10,advance='no',size=l,iostat=read_status)line
	if(l.ne.0)read(line(1:l),*,err=230)sigJ
	sigma=sigJ*Ilev(1)
	I1=Ilev(1)

!
!	get rho_k values for initial state
!
    CALL RHOGEN(I1,SIGMA,RHOKT,KMAX,JZJ,MODEO,SI,PPI,AM0,AMI)

!   copy rhokt to rhoin
    do k=0,kmax,1
      rk=float(k)
      do kq=-k,k,1
        rhoin(k,kq)=(0.,0.)
        if(kq.eq.0)rhoin(k,kq)=z1*rhokt(k)/sqrt(2.*rk+1.)
        rhoI1(k,kq)=rhoin(k,kq)
      enddo
    enddo

    write(*,*)
	write(6,*)' Initial rho_k values:'
	write(6,6010)
	write(6,6011)
	do k1=0,kmax,2
	  do k1q=-k1,k1,1
	    if(rhoin(k1,k1q).ne.0.)then
	      write(6,6000)k1,k1q,rhoin(k1,k1q)
	    endif
	  enddo
	enddo
6000	format('  'I5'   'I5'   '2(f12.6,x))
6010	format('      K     Kappa           rho(K,kappa) ')
6011	format('                         Re           Im  ')

!	write(6,6030)
!	write(10,6030)
!	write(6,6031)
!	write(10,6031)
!	do k1=0,kmax,2
!	  do k1q=-k1,k1,1
!	      write(6,6020)k1,k1q,rhoin(k1,k1q),rhoI1(k1,k1q)
!	      write(10,6020)k1,k1q,rhoin(k1,k1q),rhoI1(k1,k1q)
!	  enddo
!	enddo


!	read transition properties and propagate alignment along path
    write(*,*)
	write(6,*)' Transition properties will now be read in'
	write(6,*)' Mixing ratios, delta, are for L2/L1.'
    write(*,*)

	do nlvl=1,nlev-2

239	  write(6,240)nlvl,Ilev(nlvl),nlvl+1,Ilev(nlvl+1)
240	  format('  Transition from level ',i3,' spin ',f6.1,' to level ',i3,' spin ',f6.1,':')

	  I1=Ilev(nlvl)
	  I2=Ilev(nlvl+1)

250	  write(6,251)L1(nlvl),L2(nlvl),delta(nlvl)
251	  format('  L1,L2,delta ['2(f8.4,2x),g12.3 ']',$)
	  read(5,10,advance='no',size=l,iostat=read_status)line
	  if(l.ne.0)read(line(1:l),*,err=250)L1(nlvl),L2(nlvl),delta(nlvl)

	  L1_1=L1(nlvl)
	  L2_1=L2(nlvl)
	  del1=delta(nlvl)

	  if( abs(I1-I2).gt.min(L1_1,L2_1) )then
	    WRITE(6,*)' WARNING: this mixed transition is not possible!'
	    goto 239
	  endif

!260	  write(6,261)mode
!261	  format(' Transition observed [1] or not [0]? ['i3']',$)
!	  read(5,10,advance='no',size=l,iostat=read_status)line
!	  if(l.ne.0)read(line(1:l),*,err=260)mode
!	  modei(nlvl)=mode

!	  if(mode.eq.1)then
270	    write(6,271)theta1,phi1
!271	    format(' Detection angles: theta1,phi1 ['2(f8.2,2x)'°]',$)
271	    format(' Detection angles: theta1,phi1 ['2(f8.2,2x)'degrees]',$)

	    read(5,10,advance='no',size=l,iostat=read_status)line
	    if(l.ne.0)read(line(1:l),*,err=270)theta1,phi1
!	  thetai(nlvl)=theta1
!	  phii(nlvl)=phi1

!
!	calculate statistical tensor for next state
!
	 call rhomQ(mode,I1,I2,L1_1,L2_1,del1,theta1,phi1,rhoin,rhoout,kmax,Qk)

    write(*,*)
	write(6,*)' Modified rho_k values:'
!	write(10,*)' Modified rho_k values:'
	write(6,6030)
!	write(10,6030)
	write(6,6031)
!	write(10,6031)
	do k1=0,kmax,2
	  do k1q=-k1,k1,1
	    test=sqrt((real(rhoout(k1,k1q)))**2+(aimag(rhoout(k1,k1q)))**2)
	    if(test.gt.1.e-6)then
	      write(6,6020)k1,k1q,rhoin(k1,k1q),rhoout(k1,k1q)
!	      write(10,6020)k1,k1q,rhoin(k1,k1q),rhoout(k1,k1q)
	    endif
	  enddo
	enddo
6020	format('  'I5'   'I5'   '2(f12.6,x)'  '2(f12.6,x))
6030	format('      K     Kappa'2(12X,'rho(K,kappa)'))
6031	format('         ',5x,2(10x,' Re           Im  '))
!
!
	enddo

!
!	now for observation of last radiation
!
!	rhoout contains the relevant statistical tensor
!

	write(6,*)' '
	write(6,*)' Observation of last radiation:'

279	write(6,280)nlev-1,Ilev(nlev-1),nlev,Ilev(nlev)
280	format('  Transition from level ',i3,' spin ',f8.1,'to level ',i3,' spin ',f8.1,':')

	I2=Ilev(nlev-1)
	I3=Ilev(nlev)
!	modei(nlev-1)=1

290	write(6,291)L1(nlev-1),L2(nlev-1),delta(nlev-1)
291	format('  L1,L2,delta ['2(f8.4,2x),g12.3 ']',$)
	read(5,10,advance='no',size=l,iostat=read_status)line
	if(l.ne.0)read(line(1:l),*,err=290)L1(nlev-1),L2(nlev-1),delta(nlev-1)

	L1_2=L1(nlvl)
	L2_2=L2(nlvl)
	del2=delta(nlvl)

	if( abs(I2-I3).gt.min(L1_2,L2_2) )then
	  WRITE(6,*)' WARNING: this mixed transition is not possible!'
	  goto 279
	endif
!
!	The dco value for observation in a specified direction (theta,phi)
!	is evaluated and printed first
!

300	write(6,301)theta2,phi2
!301	format(' Detection angles: theta2,phi2 ['2(f8.2,2x)'°]',$)
301	format(' Detection angles: theta2,phi2 ['2(f8.2,2x)'degrees]',$)
	read(5,10,advance='no',size=l,iostat=read_status)line
	if(l.ne.0)read(line(1:l),*,err=300)theta2,phi2
!	thetai(nlev-1)=theta2
!	phii(nlev-1)=phi2

	call wobsQ(I2,I3,L1_2,L2_2,del2,theta2,phi2,rhoout,wdco,kmax,Qk)
	write(6,*)' wdco =',wdco


!
!   evaluate angular distribution coefficients for first transition
!

    do k=0,kmax,2
      do kq=-k,k,1
        rk=float(k)
        AK(k,kq)=sqrt(float(2*k+1))*A(rk,L1_1,L2_1,I1,I2,del1)*rhoin(k,kq)
        AQK(k,kq)=AK(k,kq)*Qk(k)
      enddo
    enddo

    write(*,*)
    write(*,*)
    write(*,*) '     ANGULAR DISTRIBUTION COEFFICIENTS FOR FIRST TRANSITION:'
    write(*,*)
    write(*,*)'     K     Kappa             AK(K,kappa)            AQK(K,kappa)'
    write(*,*)'                           Re           Im        Re           Im  '

	do k=0,kmax,2
	  do kq=-k,k,1
	    test=sqrt( (real(AK(k,kq)) )**2+(aimag(AK(k,kq)))**2)
	    if(test.gt.1.e-6)then
	      write(6,6020)k,kq,AK(k,kq),AQK(k,kq)
!	      write(10,6020)k1,k1q,rhoin(k1,k1q),rhoout(k1,k1q)
	    endif
	  enddo
	enddo



    if(L_data_loaded)then
!
!     Now make the output for CACTUS detectors
! 
      write(10,*)' -----------------------------------------------------------------------------'
      write(10,'(a)')'   DCO values in CACTUS evaluated for the following transitions:'
      write(10,'(a,g12.6)')'   sigma/J for the initial state is ',sigJ
      write(10,'(a,f8.4,a,f8.4)')'   For all detectors Q2 = ',Qk(2),' and Q4 = ',Qk(4)
      write(10,'(a,4x,a,6x,a,5x,a,5x,a,5x,a)')'   Transition','I_i','I_f','L1','L2','delta'

	  do nlvl=1,nlev-1
        write(10,426)nlvl,Ilev(nlvl),Ilev(nlvl+1),L1(nlvl),L2(nlvl),delta(nlvl)
	  enddo

      ilist=0
      do i=1,Ngamma_det
        do j=i+1,Ngamma_det
          if(trim(gamma_map(i)%notes) == 'EMPTY')cycle
          if(trim(gamma_map(j)%notes) == 'EMPTY')cycle
          if(gamma_map(i)%Frame_pos == -99)cycle
          if(gamma_map(j)%Frame_pos == -99)cycle
          ilist=ilist+1
          theta1=gamma_map(i)%theta_d
          phi1=gamma_map(i)%phi_d
          theta2=gamma_map(j)%theta_d
          phi2=gamma_map(j)%phi_d
      	  call DCO_3_levels(wdco)
 !        write(10,*)i,j,wdco
          dco_list(ilist)%idet=i
          dco_list(ilist)%jdet=j
          dco_list(ilist)%wdc=wdco
        enddo
      enddo
      Ndco=ilist
!     Now sort into order of increasing dco:

      do i=1,Nlist-1
        iptr = i
        do j = i+1, Nlist
          if( dco_list(j)%wdc < dco_list(iptr)%wdc ) iptr = j
        enddo
!       iptr now points to the minimum value, so swap a(iptr) with a(i) if i /= iptr.
        swap: if ( i /= iptr ) then
           temp = dco_list(i)
           dco_list(i) = dco_list(iptr)
           dco_list(iptr) = temp
        endif swap
      enddo
  !   Now list out:
!     write(10,*)' List of dco values:'
      write(10,*)' -----------------------------------------------------------------------------'
      write(10,*)'       DCO value       G1  ( theta , phi )           G2   ( theta ,  phi  )'
      write(10,*)' -----------------------------------------------------------------------------'
!     last_ang=nint(cosine_list(1)%angle_diff)

      sum=0.
      do i=1,Nlist
        id1=dco_list(i)%idet
        id2=dco_list(i)%jdet
        theta1=gamma_map(id1)%theta_d
        phi1=gamma_map(id1)%phi_d
        theta2=gamma_map(id2)%theta_d
        phi2=gamma_map(id2)%phi_d

        write(10,'(6x,f8.4,8x,i3,3x,a,f6.1,a,f6.1,a,8x,i3,3x,a,f6.1,a,f6.1,a)')dco_list(i)%wdc, &
           id1,'(',theta1,' ,',phi1,' )',id2,'(',theta2,' ,',phi2,' )'   !,cosij(id1,id2)

        sum=sum+dco_list(i)%wdc
      enddo
      write(10,*)' ------------------------------------------------------------------------------'
      write(10,*)'  The average value of the DCO is ',sum/float(Nlist)
    endif
!
!   plot angular distribution for first state
!
!   heading and information into plot data file
!
    write(*,*)
    write(*,*)'  Angular distribution for the first state written to AngDist.pdat' 
    write(10,*)'  Angular distribution for the first state written to AngDist.pdat' 
    write(*,*)
	write(30,400)
	write(30,410)sigJ
	write(30,415)

	nlvl=1
    write(30,425)nlvl,Ilev(nlvl),Ilev(nlvl+1),L1(nlvl),L2(nlvl),delta(nlvl)
	write(30,500) 0.,0.,0.,-99
    if(L_data_loaded)then
      do ithet=1,6
	    theta=Ring_Angles(ithet)
	    call wobsQ(I1,I2,L1_1,L2_1,del1,theta,0.0,rhoI1,wdco,kmax,Qk)
	    write(30,500)theta,wdco,0.0,-51
      enddo
    endif
	write(30,500) 0.,0.,0.,-99
	do ithet=0,180,1
	  theta=float(ithet)
	  call wobsQ(I1,I2,L1_1,L2_1,del1,theta,0.0,rhoI1,wdco,kmax,Qk)
	  write(30,500)theta,wdco,0.0,0
	enddo
	write(30,500) 0.,0.,0.,-99
500	format(' '3(f10.4,2x),' 'i5)

400	FORMAT('! Angular Distribution: Plot versus THETA for FIRST observed  radiation')
410	format('! sigma/J for the initial state is ',g12.6)
415	format('!  Transition',4x,'I_i',6x,'I_f',5x,'L1',5x,'L2',5x,'delta')
425	format('!    ',i3,6x,f5.1,' --> ',f5.1,' ',f5.0,'  ',f5.0,' ',f10.3)
426	format('      ',i3,6x,f5.1,' -->',f5.1,' ',f5.0,'  ',f5.0,' ',f10.3)


    write(*,*)

    enddo
!	goto 200
	end subroutine directional_correlations
!==============================================================================

