	program pedaplot
!
!
!	program to calculate and plot angular distributions for pedagogical purposes
!
!
!
!	August 2005ff                      Andrew E Stuchbery.
!
!
!	Please report any bugs or errors to Andrew.Stuchbery@anu.edu.au
!	

	implicit none

	integer l
	integer nlev,nlvl,itest/0/,iloop
	integer mode,modeo,modei(10)
	integer kmax,k1,k1q
	integer k2,q1,q2,k,kq,kqq
	integer ithet,iop

	real*4 Ilev(10),Imax
	real*4 L1(10),L2(10),delta(10)
	real*4 I1,I2,I3,L1_1,L2_1,del1,L1_2,L2_2,del2
	real*4 thetai(10),phii(10)
	real*4 theta1,phi1
	real*4 theta2,phi2
	real*4 theta2plot,phi2plot
	real*4 G2,G4
	real*4 sigJ,sigma
	real*4 theta
	real*4 test,wdco,sum2,sum4
    real(4)                                               :: A                  ! General A coefficient (mixed Fk's) 
    complex(4),dimension(0:8,-8:8)                        :: AK                 ! Ak coefficient usually only RE A(2,0) and A(4,0) nonzero
    complex(4),dimension(0:8)                             :: AKL                ! Ak coefficient usually only real part is non zero


	real*4 alpha/0./,beta/0./,gamma/0./
!	real*4 alpha/-90./,beta/-90./,gamma/0./

	real*4 t3j1,t3j2,j1,j2,j3,m1,m2,m3,threej
	real*8 D3JR3

	real*4 RHOKT(0:68),JZJ,rk,rkq,rkqq
	real*4 SI(69),PI(69),AM0,AMI
	real*4 Qk(0:68), QkGk(0:68)

	real*4  SF/57.2957795/

	real*4 FACLG

	complex*8 BIG_D,rhosum
	complex*8 rhoin(0:68,-68:68)
	complex*8 rhoout(0:68,-68:68)
	complex*8 rhoring(0:68,-68:68)
	complex*8 z1/(1.,0.)/

!	real*4 pi/3.141592654/

	character line*80
	character cmode*1,cm_sav*1

!	COMMON /RACAH/ FACLG(500)


!	initialize factorial arrays
!	call faclog
!	call blocks

!	default parameters

	nlev=2 ! number of levels

	modeo=0      ! oblate/prolate substate distribution
	sigJ=0.35    ! gaussian parameter for substate distn

	theta1=0.0
	phi1=0.0
	theta2=90.0
	phi2=0.0
	
	G2=1.
	G4=1.

	mode=1
	cmode='y'

10	format(q,a)


!
!	This code assumes point detectors. i.e. all solid angle attenuation
!	coefficients are unity. 
!	Note the max k for Qk is 6 for L=3 transitions, e.g. E3.
!
!	To include finite detectors must modify code to read and replace Qk 
!	values for each observed transition.
!
	do k=0,68
	  Qk(k)=1.
	  QkGk(k)=1.
	enddo


!	initialize spins and transitions etc to default/zero values
	do nlvl=1,2
	  Ilev(nlvl)=float(4-2*nlvl)
	  L1(nlvl)=2.
	  L2(nlvl)=2.
	  delta(nlvl)=1000.
	  modei(nlvl)=0
	  thetai(nlvl)=0.0
	  phii(nlvl)=0.0
	enddo

    rhoin=(0.,0.)
    rhoout=(0.,0.)
    rhoring=(0.,0.)

!	read number of levels in sequence (include initial and final states)

	write(6,*)' '
	write(6,*)' '
	write(6,*)'           Welcome to PEDAPLOT '
	write(6,*)' '
	write(6,*)' '


	write(6,*)' The number of levels in the cascade (NLEV) includes the' 
	write(6,*)' initial and final states. (Max value of NLEV = 10)'
	write(6,*)' Use NLEV = 0 to stop.'
	write(6,*)' Level 1 is the initial state for which the alignment from'
	write(6,*)' the reaction is specified.'
	write(6,*)' Gamma decay then proceeds along the path from level 1'
	write(6,*)' in sequential order of the levels terminating at level'
	write(6,*)' number NLEV'
	write(6,*)' '
	write(6,*)' '


200	write(6,201)nlev
201	format('  Number of levels in cascade path (max 10) ['(I3)']',$)
	read(5,10)l,line
	if(l.ne.0)read(line(1:l),*,err=200)nlev
	if(nlev.gt.10) goto 200
	if(nlev.le.0) stop

!	read in levels and spins and find largest spin:

	Imax=0.0
	do nlvl=1,nlev
210	  write(6,211)nlvl,Ilev(nlvl)
211	  format('  Spin of level number 'i4' ['f8.2']',$)
	  read(5,10)l,line
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

	write(6,*)' Specify aligment of initial state:'
	write(6,*)' Oblate alignment means mostly m = 0'
	write(6,*)' Prolate alignment means mostly m = +/- I'
	write(6,*)' '
220	write(6,221)modeo
221	format('  prolate [1]/oblate [0] alignment ['(i4,2x)']',$)
	read(5,10)l,line
	if(l.ne.0)read(line(1:l),*,err=220)modeo
	write(6,*)' '
	write(6,*)' Alignment parameter is sigma/I (see NPA 723, 69 (2003))'

!	"Gamma-ray angular distributions and correlations after projectile 
!	fragmentation reactions" by A.E. Stuchbery, NPA 723 (2003) 69.

!	Nuclear Physics A paper (as above) sets out philosophy for evaluation
!	of the statistical tensors of the initial state. This is a general 
!	routine that allows prolate or oblate alignment, or polarization, in
!	the initial state, based on gaussian sub-state distributions.

230	write(6,231)sigJ
231	format('  sigma/I ['(g10.4)']',$)
	read(5,10)l,line
	if(l.ne.0)read(line(1:l),*,err=230)sigJ
	sigma=sigJ*Ilev(1)
	I1=Ilev(1)
!=================================================================================
!	explore behavior of statistical tensors
      if(itest==1)then

 	sum2=0.
	sum4=0.

	do iloop=-nint(I1),nint(I1),1
	 do k=1,69
	  SI(k)=0.
	  PI(k)=0.
	 enddo
	 SI(1)=float(iloop)
	 PI(1)=1.0

        CALL RHOGEN(I1,SIGMA,RHOKT,KMAX,JZJ,3,SI,PI,AM0,AMI)
	  write(6,*)' P(',iloop,') = 1.0'
	write(10,*)iloop,rhokt(2),rhokt(4)
	sum2=sum2+rhokt(2)
	sum4=sum4+rhokt(4)
	enddo
	write(10,*)' sum2 = ',sum2
	write(10,*)' sum4 = ',sum4


	sigma=sigJ*Ilev(1)
	I1=Ilev(1)
      CALL RHOGEN(I1,SIGMA,RHOKT,KMAX,JZJ,MODEO,SI,PI,AM0,AMI)

	DO Iloop=1,nint(2.*I1+1.)
	  WRITE(10,*)Iloop,SI(Iloop),PI(Iloop)
	ENDDO

	write(10,*)'  rho2 and rho4 vs sigma/J:'
	do iloop=0,100
	 sigJ=float(iloop)/50.
	 sigma=sigJ*Ilev(1)
       CALL RHOGEN(I1,SIGMA,RHOKT,KMAX,JZJ,MODEO,SI,PI,AM0,AMI)
	 write(10,*)sigJ,rhokt(2),rhokt(4)
	enddo

      endif
	if(itest.eq.1)goto 200
!=================================================================================
!
!	get rho_k values for initial state
!
        CALL RHOGEN(I1,SIGMA,RHOKT,KMAX,JZJ,MODEO,SI,PI,AM0,AMI)

!       copy rhokt to rhoin
        do k=0,kmax,1
          rk=float(k)
          do kq=-k,k,1
            rhoin(k,kq)=(0.,0.)
            if(kq.eq.0)rhoin(k,kq)=z1*rhokt(k)/sqrt(2.*rk+1.)
          enddo
        enddo

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

!
!	rotate frame by any Euler rotation
!
	write(6,*)' The statistical tensor can be rotated.' 
	write(6,*)' The z-axis must be defined such that the usual spherical'
	write(6,*)' polar angles (theta,phi) are defined properly for the'
	write(6,*)' subsequent gamma-ray decay.'
	write(6,*)' To point the spin perpendicular to the reaction plane'
	write(6,*)' in Clarion-Hyball use (-90,-90,0)'
	write(6,*)' For the gammasphere-microball application where theta'
	write(6,*)' is the angle between the nuclear spin and the gamma ray'
	write(6,*)' use (0,0,0) - i.e. the spin direction is the z-axis.'
	write(6,*)' '
	write(6,*)' *** there is a high-spin limit on this rotation ***'
	write(6,*)'   the program will crash if the spin is too high'
	write(6,*)' '

600	write(6,601)alpha,beta,gamma
601	format(' Euler rotation angles in degrees  ['3(g10.4)']',$)
	read(5,10)l,line
	if(l.ne.0)read(line(1:l),*,err=600)alpha,beta,gamma

	if((alpha.ne.0.).or.(beta.ne.0.).or.(gamma.ne.0.))then
	      do k=0,kmax,2
	        rk=float(k)
	        do kq=-k,k
	          rkq=float(kq)
	          rhosum=(0.,0.)
	          do kQQ=-k,k
	            rkQQ=float(kQQ)
	            rhosum=rhosum+rhoin(k,kQQ)*BIG_D(alpha,beta,gamma,rk,rkq,rkQQ)
	          enddo
	          rhoout(k,kq)=rhosum
	        enddo
	      enddo
	else
	      do k=0,kmax,2
	        do kq=-k,k
	          rhoout(k,kq)=rhoin(k,kq)
	        enddo
	      enddo
	endif

	write(6,*)' Statistical tensors after Euler rotation:'
	write(6,6010)
	write(6,6011)
	do k1=0,kmax,2
	  do k1q=-k1,k1,1
	    if(rhoout(k1,k1q).ne.0.)then
	      write(6,6000)k1,k1q,rhoout(k1,k1q)
	    endif
	  enddo
	enddo
!	copy rhoout to rhoin:
	do k1=0,kmax,2
	  do k1q=-k1,k1,1
	    rhoin(k1,k1q)=rhoout(k1,k1q)
	  enddo
	enddo

!	read transition properties and propagate alignment along path

	write(6,*)' Transition properties will now be read in'
	write(6,*)' Mixing ratios, delta, are for L2/L1.'

	do nlvl=1,nlev-2


239	  write(6,240)nlvl,Ilev(nlvl),nlvl+1,Ilev(nlvl+1)
240	  format('  Transition from level 'i3' spin 'f8.1,'to level 'i3' spin 'f8.1':')

	  I1=Ilev(nlvl)
	  I2=Ilev(nlvl+1)

250	  write(6,251)L1(nlvl),L2(nlvl),delta(nlvl)
251	  format('  L1,L2,delta ['2(f8.4,2x),g12.3 ']',$)
	  read(5,10)l,line
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
	  read(5,10)l,line
	  if(l.ne.0)read(line(1:l),*,err=260)mode
	  modei(nlvl)=mode

	  if(mode.eq.1)then
270	    write(6,271)theta1,phi1
271	    format(' Detection angles: theta1,phi1 ['2(f8.2,2x)']',$)
	    read(5,10)l,line
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
280	format('  Transition from level 'i3' spin 'f8.1,'to level 'i3' spin 'f8.1':')

	I2=Ilev(nlev-1)
	I3=Ilev(nlev)
	modei(nlev-1)=1

290	write(6,291)L1(nlev-1),L2(nlev-1),delta(nlev-1)
291	format('  L1,L2,delta ['2(f8.4,2x),g12.3 ']',$)
	read(5,10)l,line
	if(l.ne.0)read(line(1:l),*,err=290) L1(nlev-1),L2(nlev-1),delta(nlev-1)

!	L1_2=L1(nlvl)
!	L2_2=L2(nlvl)
!	del2=delta(nlvl)

	L1_2=L1(nlev-1)
	L2_2=L2(nlev-1)
	del2=delta(nlev-1)

	if( abs(I2-I3).gt.min(L1_2,L2_2) )then
	  WRITE(6,*)' WARNING: this mixed transition is not possible!'
	  goto 279
	endif
!
!	The dco value for observation in a specified direction (theta,phi)
!	is evaluated and printed first
!
!      write(*,*)
!      write(*,*)' Now print BkFk for last transition:'

!
!   evaluate angular distribution coefficients for first transition
!

    do k=0,kmax,2
      do kq=-k,k,1
        rk=float(k)
        AK(k,kq)=sqrt(float(2*k+1))*A(rk,L1_2,L2_2,I2,I3,del2)*rhoout(k,kq)
      enddo
    enddo

    write(*,*)
    write(*,*)
    write(*,*) '     ANGULAR DISTRIBUTION COEFFICIENTS FOR LAST TRANSITION:'
    write(*,*)
    write(*,*)'     K     Kappa             AK(K,kappa)'
    write(*,*)'                           Re           Im'

	do k=0,kmax,2
	  do kq=-k,k,1
	    test=sqrt( (real(AK(k,kq)) )**2+(aimag(AK(k,kq)))**2)
	    if(test.gt.1.e-6)then
	      write(6,6021)k,kq,AK(k,kq)
	    endif
	  enddo
	enddo
6021  format('  ',I5,'   ',I5,'   ',2(f12.6,x))
    write(*,*)
    write(*,*)

300	write(6,301)theta2,phi2
301	format(' Detection angles: theta2,phi2 ['2(f8.2,2x)']',$)
	read(5,10)l,line
	if(l.ne.0)read(line(1:l),*,err=300)theta2,phi2
	thetai(nlev-1)=theta2
	phii(nlev-1)=phi2

	call wobsQ(I2,I3,L1_2,L2_2,del2,theta2,phi2,rhoout,wdco,kmax,Qk)
	write(6,*)' The value of the angular correlation is: ',wdco




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

      phi2plot=phi2
      theta2plot=theta2
      do
      
	write(6,*)' '
	write(6,*)' Plot angular distribution for final radiation.'
	write(6,*)' First, plot of 0< theta <180 for fixed phi:'
340	write(6,341)phi2plot
341	format(' Fixed phi value for theta plot ['f8.2']',$)
	read(5,10)l,line
	if(l.ne.0)read(line(1:l),*,err=340)phi2plot

      write(*,*)' Can multiply in G2 and G4 (-ve to exit):'


360	write(6,361)G2,G4
361	format(' G2 and G4 values  ['2(g10.4)']',$)
	read(5,10)l,line
	if(l.ne.0)read(line(1:l),*,err=360)G2,G4
	if(G2 < 0.)exit
	if(G4 < 0.)exit
      QkGk(2)=Qk(2)*G2
      QkGk(4)=Qk(4)*G4

!	write(6,*)' Making angular distribution plots '

	write(25,400)
	write(25,405)phi2plot
	write(25,410)sigJ
	write(25,415)

	do nlvl=1,nlev-1
	  if(modei(nlvl).eq.0)then
	    write(25,420)nlvl,Ilev(nlvl),Ilev(nlvl+1),L1(nlvl),L2(nlvl),delta(nlvl)
	  endif

	  if(modei(nlvl).eq.1)then
	    write(25,425)nlvl,Ilev(nlvl),Ilev(nlvl+1),L1(nlvl),L2(nlvl),delta(nlvl), &
                     thetai(nlvl),phii(nlvl)
	  endif

	enddo

400	FORMAT('! Plot versus THETA for last observed  radiation')
405	format('! phi is fixed at 'f8.2'deg')
410	format('! sigma/J for the initial state is 'g12.6)
415	format('!  Transition'4x'I_i'6x'I_f'5x'L1'5x'L2',5x'delta'7x'theta'6x'phi')
420	format('!    'i3,6x,f5.1' --> 'f5.1' 'f5.0'  'f5.0' 'f10.3,'     not observed ')
425	format('!    'i3,6x,f5.1' --> 'f5.1' 'f5.0'  'f5.0' 'f10.3,'    'f8.3'  'f8.3)

	write(25,500) 0.,0.,0.,-99
!	write(10,*)' starting the plot:'
	do ithet=0,180,1
	  theta=float(ithet)
	  call wobsQ(I2,I3,L1_2,L2_2,del2,theta,phi2plot,rhoout,wdco,kmax,QkGk)
	  write(25,500)theta,wdco,0.0,0
	enddo
	write(25,500) 0.,0.,0.,-99

!------------------------------------------------------------------
!   set up tensors for ring counter (or ring of counters):
!   i.e. only kq=0 terms are non zero
	do k1=0,kmax,2
	    rhoring(k1,0)=rhoout(k1,0)
	enddo
!------------------------------------------------------------------
!   plot theta correlation for ring counter
	write(27,401)
	write(27,402)alpha,beta,gamma
	write(27,410)sigJ
	write(27,415)
401	FORMAT('! Plot versus THETA for last observed  radiation and ring counter')
402 format('! Euler rotation [',f8.2,',',f8.2,',',f8.2,']')
	write(27,500) 0.,0.,0.,-99
	do ithet=0,180,1
	  theta=float(ithet)
	  call wobsQ(I2,I3,L1_2,L2_2,del2,theta,phi2plot,rhoring,wdco,kmax,QkGk)
	  write(27,500)theta,wdco,0.0,0
	enddo
	write(27,500) 0.,0.,0.,-99


!
!   evaluate angular distribution coefficients for last transition and ring counter
!
!	  call wobsQ(I2,I3,L1_2,L2_2,del2,theta,phi2plot,rhoring,wdco,kmax,QkGk)

    do k=0,kmax,2
      rk=float(k)
      AKL(k)=sqrt(float(2*k+1))*A(rk,L1_2,L2_2,I2,I3,del2)*rhoring(k,0)
    enddo

    write(*,*)
    write(*,*)
    write(*,*) '     ANGULAR DISTRIBUTION COEFFICIENTS FOR RING COUNTER:'
    write(*,*)
    write(*,*)'     K                       AK(K)'
    write(*,*)'                         Re           Im '

	do k=0,kmax,2
      write(6,'(3x,i5,3x,2(f12.5,x))')k,AKL(k)
	enddo


	write(6,*)' '
	write(6,*)' Second, plot of 0< phi <360 for fixed theta:'
350	write(6,351)theta2plot
351	format(' Fixed theta value for phi plot ['f8.2']',$)
	read(5,10)l,line
	if(l.ne.0)read(line(1:l),*,err=350)theta2plot

	write(26,450)
	write(26,455)theta2plot
	write(26,410)sigJ
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
455	format('! theta is fixed at 'f8.2'deg')

	write(26,500) 0.,0.,0.,-99
	do ithet=0,360,1
	  theta=float(ithet)
! this puts phi2 (called theta) from 0 to 360 degrees
	call wobsQ(I2,I3,L1_2,L2_2,del2,theta2plot,theta,rhoout,wdco,kmax,QkGk)
	  write(26,500)theta,wdco,0.0,0
	enddo
	write(26,500) 0.,0.,0.,-99

!	clarion angular distributions:




	do iop=30,32
	 if(iop.eq.30)theta2=90.
	 if(iop.eq.31)theta2=132.
	 if(iop.eq.32)theta2=155.

	 write(iop,450)
	 write(iop,455)theta2
	 write(iop,410)sigJ
	 write(iop,415)

	 do nlvl=1,nlev-1
	  if(modei(nlvl).eq.0)then
	    write(iop,420)nlvl,Ilev(nlvl),Ilev(nlvl+1),L1(nlvl),L2(nlvl),delta(nlvl)
	  endif

	  if(modei(nlvl).eq.1)then
	    write(iop,425)nlvl,Ilev(nlvl),Ilev(nlvl+1),L1(nlvl),L2(nlvl),delta(nlvl),thetai(nlvl),phii(nlvl)
	  endif

	 enddo


	 write(iop,500) 0.,0.,0.,-99
	 do ithet=0,360,1
	  theta=float(ithet)
! this puts phi2 (called theta) from 0 to 360 degrees
	  call wobsQ(I2,I3,L1_2,L2_2,del2,theta2,theta,rhoout,wdco,kmax,QkGk)
	  write(iop,500)theta,wdco,0.0,0
	 enddo
	 write(iop,500) 0.,0.,0.,-99
	enddo

!	theta2=0.0

      enddo

500	format(' '3(f10.4,2x),' 'i5)

	goto 200
	end
!==============================================================================
