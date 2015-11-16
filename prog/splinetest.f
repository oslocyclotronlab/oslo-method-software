      PROGRAM xsplint
C     driver for routine splint, which calls spline
      INTEGER NP
      REAL PI
      PARAMETER(NP=10,PI=3.141593)
      INTEGER i,nfunc
      REAL f,x,y,yp1,ypn,xa(NP),ya(NP),y2(NP)
      do 14 nfunc=1,2
        if (nfunc.eq.1) then
          write(*,*) 'Sine function from 0 to PI'
          do 11 i=1,NP
            xa(i)=i*PI/NP
            ya(i)=sin(xa(i))
11        continue
          yp1=cos(xa(1))
          ypn=cos(xa(NP))
        else if (nfunc.eq.2) then
          write(*,*) 'Exponential function from 0 to 1'
          do 12 i=1,NP
            xa(i)=1.0*i/NP
            ya(i)=exp(xa(i))
12        continue
          yp1=exp(xa(1))
          ypn=exp(xa(NP))
        else
          stop
        endif

C call SPLINE to get second derivatives
        call spline(xa,ya,NP,yp1,ypn,y2)

C call SPLINT for interpolations
        write(*,'(1x,t10,a1,t20,a4,t28,a13)') 'x','f(x)','interpolation'
        do 13 i=1,10
          if (nfunc.eq.1) then
            x=(-0.05+i/10.0)*PI
            f=sin(x)
            x1=0.6*x
            f1=sin(x1)
          else if (nfunc.eq.2) then
            x=-0.05+i/10.0
            f=exp(x)
            x1=0.6*x
            f1=exp(x1)
          endif
          call splint(xa,ya,y2,NP,x,y)
          write(*,'(1x,3f12.6)') x,f,y

          call splint(xa,ya,y2,NP,x1,y)
          write(*,'(1x,3f12.6)') x1,f1,y

13      continue
        write(*,*) '***********************************'
        write(*,*) 'Press RETURN'
        read(*,*)
14    continue
      END

      SUBROUTINE splint(xa,ya,y2a,n,x,y)
      INTEGER n
      REAL x,y,xa(n),y2a(n),ya(n)
      INTEGER k,khi,klo
      REAL a,b,h
      klo=1
      khi=n
1     if (khi-klo.gt.1) then
        k=(khi+klo)/2
        if(xa(k).gt.x)then
          khi=k
        else
          klo=k
        endif
      goto 1
      endif
      h=xa(khi)-xa(klo)
      if (h.eq.0.) pause 'bad xa input in splint'
      a=(xa(khi)-x)/h
      b=(x-xa(klo))/h
      y=a*ya(klo)+b*ya(khi)+((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**
     *2)/6.
      return
      END

      SUBROUTINE spline(x,y,n,yp1,ypn,y2)
      INTEGER n,NMAX
      REAL yp1,ypn,x(n),y(n),y2(n)
      PARAMETER (NMAX=500)
      INTEGER i,k
      REAL p,qn,sig,un,u(NMAX)
      if (yp1.gt..99e30) then
        y2(1)=0.
        u(1)=0.
      else
        y2(1)=-0.5
        u(1)=(3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
      endif
      do 11 i=2,n-1
        sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
        p=sig*y2(i-1)+2.
        y2(i)=(sig-1.)/p
        u(i)=(6.*((y(i+1)-y(i))/(x(i+
     *1)-x(i))-(y(i)-y(i-1))/(x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*
     *u(i-1))/p
11    continue
      if (ypn.gt..99e30) then
        qn=0.
        un=0.
      else
        qn=0.5
        un=(3./(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
      endif
      y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.)
      do 12 k=n-1,1,-1
        y2(k)=y2(k)*y2(k+1)+u(k)
12    continue
      return
      END
