    subroutine angular_correlations

!   Use DCO formulation with no initial alignment and first transition detected at (0,0)


	use shared_data
	implicit none

	integer l,read_status
	integer nlev,nlvl
	integer mode,modeo,modei(10)
	integer kmax,k1,k1q
	integer k2,q1,q2,k,kq
	integer ithet

!    real*4 F
 !   real*8 FK
	real*4 Ilev(10),Imax
	real*4 L1(10),L2(10),delta(10)
	real*4 I1,I2,I3,L1_1,L2_1,del1,L1_2,L2_2,del2
	real*4 thetai(10),phii(10)
	real*4 theta1,phi1
	real*4 theta2,phi2
!	real*4 sigJ,sigma
	real*4 theta
	real*4 test,wdco,sum

!	real*4 t3j1,t3j2,j1,j2,j3,m1,m2,m3,threej
!	real*8 D3JR3

!	real*4 RHOKT(0:68),JZJ,rk
!	real*4 SI(69),PI(69),AM0,AMI
	real*4 Qk(0:6)

!	real*4  SF/57.2957795/

!	real*4 FACLG

	complex*8 rhoin(0:68,-68:68)
	complex*8 rhoout(0:68,-68:68)
	complex*8 z1/(1.,0.)/

!	real*4 pi/3.141592654/

	character line*80
	character cmode*1,cm_sav*1

    OPEN (UNIT=25,File='AngCorr_theta.pdat',status='replace')
    OPEN (UNIT=26,File='AngCorr_phi.pdat',status='replace')


!	default parameters

	nlev=3 ! number of levels


	theta1=0.0
	phi1=0.0
	theta2=90.0
	phi2=0.0

	mode=1
	cmode='y'

10	format(a)


!
!	This code assumes point detectors. i.e. all solid angle attenuation
!	coefficients are unity. 
!	Note the max k for Qk is 6 for L=3 transitions, e.g. E3.
!
!	To include finite detectors must modify code to read and replace Qk 
!	values for each observed transition.
!
	do k=0,6
	  Qk(k)=1.
	enddo
    Qk(2)=0.981
    Qk(4)=0.937

!	initialize spins and transitions etc to default/zero values
	do nlvl=1,10
	  Ilev(nlvl)=float(20-nlvl+1)
	  L1(nlvl)=2.
	  L2(nlvl)=2.
	  delta(nlvl)=1000.
	  modei(nlvl)=0
	  thetai(nlvl)=0.0
	  phii(nlvl)=0.0
	enddo
    Ilev(1)=0.
    Ilev(2)=2.
    Ilev(3)=0.

!	read number of levels in sequence (include initial and final states)

	write(6,*)' '
	write(6,*)' '
	write(6,*)'           Make Angular Correlation '
	write(6,*)' '
	write(6,*)' '


	write(6,*)' The number of levels in the cascade (NLEV) includes the' 
	write(6,*)' initial and final states. (Max value of NLEV = 10)'
	write(6,*)' Use NLEV = 0 to return.'
	write(6,*)' Level 1 is the initial state - assumed to have no'
	write(6,*)' orientation e.g. from a source or 0+ state.'
	write(6,*)' Gamma decay then proceeds along the path from level 1'
	write(6,*)' in sequential order of the levels terminating at level'
	write(6,*)' number NLEV'
	write(6,*)' '
	write(6,*)' To set up the necessary input parameters, the code will step'
	write(*,*)' though the input parameters for a single chosen pair of'
	write(6,*)' angles between the first and last transition. It will'
	write(6,*)' then print the angular correlations for the second transition'
	write(*,*)' around both the theta and phi directions.'
	write(6,*)' '
	write(6,*)' '

200	write(6,201)nlev
201	format('  Number of levels in cascade path (max 10) ['(I3)']',$)
	read(5,10,advance='no',size=l,iostat=read_status)line
	if(l.ne.0)read(line(1:l),*,err=200)nlev
	if(nlev.gt.10) goto 200
	if(nlev.le.0) return

!	read in levels and spins and find largest spin:

	Imax=0.0
	do nlvl=1,nlev
210	  write(6,211)nlvl,Ilev(nlvl)
211	  format('  Spin of level number 'i4' ['f8.2']',$)
	  read(5,10,advance='no',size=l,iostat=read_status)line
	  if(l.ne.0)read(line(1:l),*,err=200)Ilev(nlvl)
	  if(Ilev(nlvl).lt.0.) goto 200
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
!
!    we just have isotropic case here:
!     thus make rho_k values for initial state
!

    rhoin=(0.,0.)
    rhoin(0,0)=z1


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

    write(*,*)
220	  write(6,221)Qk(2),Qk(4)
221	  format('  Q2,Q4 ['2(f8.4,2x)']',$)
	  read(5,10,advance='no',size=l,iostat=read_status)line
	  if(l.ne.0)read(line(1:l),*,err=220)Qk(2),Qk(4)
    write(*,*)


!	read transition properties and propagate alignment along path

	write(6,*)' Transition properties will now be read in'
	write(6,*)' Mixing ratios, delta, are for L2/L1.'
	write(*,*)

	do nlvl=1,nlev-2


239	  write(6,240)nlvl,Ilev(nlvl),nlvl+1,Ilev(nlvl+1)
240	  format('  Transition from level ',i3,' spin ',f8.1,' to level ',i3,' spin ',f8.1,':')

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

260	  write(6,261)mode
261	  format(' Transition observed [1] or not [0]? ['i3']',$)
	  read(5,10,advance='no',size=l,iostat=read_status)line
	  if(l.ne.0)read(line(1:l),*,err=260)mode
	  modei(nlvl)=mode

	  if(mode.eq.1)then
270	    write(6,271)theta1,phi1
!271	    format(' Detection angles: theta1,phi1 ['2(f8.2,2x)'°]',$)
271	    format(' Detection angles: theta1,phi1 ['2(f8.2,2x)'degrees]',$)

	    read(5,10,advance='no',size=l,iostat=read_status)line
	    if(l.ne.0)read(line(1:l),*,err=270)theta1,phi1
	  else
	    theta1=0.
	    phi1=0.
	  endif
	  thetai(nlvl)=theta1
	  phii(nlvl)=phi1
!
!	calculate statistical tensor for next state
!
	 call rhomQ(mode,I1,I2,L1_1,L2_1,del1,theta1,phi1,rhoin,rhoout,kmax,Qk)

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
!	copy rhoout to rhoin 
!
	  do k1=0,kmax,2
	    do k1q=-k1,k1,1
	      rhoin(k1,k1q)=rhoout(k1,k1q)
	    enddo
	  enddo
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
	modei(nlev-1)=1

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
	thetai(nlev-1)=theta2
	phii(nlev-1)=phi2

	call wobsQ(I2,I3,L1_2,L2_2,del2,theta2,phi2,rhoout,wdco,kmax,Qk)
	write(6,*)' wdco =',wdco

!
!	Now plot in multifig style:
!
!	Multifig is an ANU plotting program.
!	It wants the data as a list of numbers in the form:
!	x,y,dy,pc
!	where x and y are the ordinate and abscissa, 
!	dy is the error bar (zero in these plots),
!	pc is a point code (normally 0)
!
	cm_sav=cmode
310     write(6,311)cmode
311     format('  Make multifig data plot files? ['a1']' ,$)
        read(5,313)cmode
313	format(a1)
        if(cmode.eq.' ')cmode=cm_sav
        if((cmode.eq.'y').or.(cmode.eq.'Y'))goto 330
        if((cmode.eq.'n').or.(cmode.eq.'N'))goto 200
        goto 310
330     continue

    write(10,*)' Angular correlations around theta and phi for the last transition'
    write(10,*)' are in AngCorr_theta.pdat and AngCorr_phi.pdat'


	write(6,*)' '
	write(6,*)' Plot angular distribution for final radiation.'
	write(6,*)' First, plot of 0< theta <180 for fixed phi:'
340	write(6,341)phi2
341	format(' Fixed phi value for theta plot ['f8.2']',$)
	read(5,10,advance='no',size=l,iostat=read_status)line
	if(l.ne.0)read(line(1:l),*,err=340)phi2

!	write(6,*)' Making angular distribution plots '

	write(25,400)
	write(25,405)phi2
!	write(25,410)sigJ
	write(25,415)

	do nlvl=1,nlev-1
	  if(modei(nlvl).eq.0)then
	    write(25,420)nlvl,Ilev(nlvl),Ilev(nlvl+1),L1(nlvl),L2(nlvl),delta(nlvl)
	  endif

	  if(modei(nlvl).eq.1)then
	    write(25,425)nlvl,Ilev(nlvl),Ilev(nlvl+1), &
	                 L1(nlvl),L2(nlvl),delta(nlvl),thetai(nlvl),phii(nlvl)
	  endif

	enddo

400	FORMAT('! Plot versus THETA for last observed  radiation')
405	format('! phi is fixed at ',f8.2,'°')
410	format('! sigma/J for the initial state is ',g12.6)
415	format('!  Transition',4x,'I_i',6x,'I_f',5x,'L1',5x,'L2',5x,'delta',7x,'theta',6x,'phi')
420	format('!    ',i3,6x,f5.1,' --> ',f5.1,' ',f5.0,'  ',f5.0,' ',f10.3,'     not observed ')
425	format('!    ',i3,6x,f5.1,' --> ',f5.1,' ',f5.0,'  ',f5.0,' ',f10.3,'    ',f8.3,'  ',f8.3)

!   plot the data:
    if(L_data_loaded)then
      sum=0.
      do ithet=1,Nlist
	    theta=cosine_list(ithet)%angle_diff
	    call wobsQ(I2,I3,L1_2,L2_2,del2,theta,phi2,rhoout,wdco,kmax,Qk)
	    sum=sum+wdco
	    write(25,500)theta,wdco,0.0,-51
      enddo
      write(*,*)' Sum of gamma-gamma angular correlation is :',sum/float(Nlist)
      write(10,*)' Sum of gamma-gamma angular correlation is :',sum/float(Nlist)
    endif

	write(25,500) 0.,0.,0.,-99
	do ithet=0,180,1
	  theta=float(ithet)
	  call wobsQ(I2,I3,L1_2,L2_2,del2,theta,phi2,rhoout,wdco,kmax,Qk)
	  write(25,500)theta,wdco,0.0,0
	enddo
	write(25,500) 0.,0.,0.,-99

	write(6,*)' '
	write(6,*)' Second, plot of 0< phi <360 for fixed theta:'
350	write(6,351)theta2
351	format(' Fixed theta value for phi plot ['f8.2']',$)
	read(5,10,advance='no',size=l,iostat=read_status)line
	if(l.ne.0)read(line(1:l),*,err=350)theta2

	write(26,450)
	write(26,455)theta2
!	write(26,410)sigJ
	write(26,415)

	do nlvl=1,nlev-1
	  if(modei(nlvl).eq.0)then
	    write(26,420)nlvl,Ilev(nlvl),Ilev(nlvl+1),L1(nlvl),L2(nlvl),delta(nlvl)
	  endif

	  if(modei(nlvl).eq.1)then
	    write(26,425)nlvl,Ilev(nlvl),Ilev(nlvl+1),L1(nlvl),L2(nlvl),delta(nlvl),thetai(nlvl),phii(nlvl)
	  endif

	enddo

450	FORMAT('! Plot versus PHI for last observed  radiation')
455	format('! theta is fixed at ',f8.2,'°')

	write(26,500) 0.,0.,0.,-99
	do ithet=0,360,1
	  theta=float(ithet)
! this puts phi2 (called theta) from 0 to 360 degrees
	  call wobsQ(I2,I3,L1_2,L2_2,del2,theta2,theta,rhoout,wdco,kmax,Qk)
	  write(26,500)theta,wdco,0.0,0
	enddo
	write(26,500) 0.,0.,0.,-99

500	format(' '3(f10.4,2x),' 'i5)

    write(*,*)
	goto 200
	end subroutine angular_correlations
!==============================================================================

