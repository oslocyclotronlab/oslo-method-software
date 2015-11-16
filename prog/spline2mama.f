      PROGRAM spline2mama
C Read/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,comment*60
      CHARACTER outfile*20,APP*4
      REAL Spec(0:8191)
      REAL Calib(6)

      CHARACTER FILNAM*80

C Stuff for spline part
      INTEGER NP
      REAL xx,yy,yp1,ypn,xa(8192),ya(8192),y2(8192)

      WRITE(6,*)'     _____________________________________________'
      WRITE(6,*)'    |                                             |'
      WRITE(6,*)'    |               SPLINE2MAMA 1.0               |'
      WRITE(6,*)'    |                                             |'
      WRITE(6,*)'    | Program to transfer a set of x(i) and y(i)  |'
      WRITE(6,*)'    |   to a MAMA spectrum of given calibration   |'
      WRITE(6,*)'    |   A spline curve is interpolated between    |'
      WRITE(6,*)'    |  all y(i) points. The new y(j) will then    |'
      WRITE(6,*)'    | have values in all channels of the spectrum |'
      WRITE(6,*)'    | Created:  02/09/99                          |'
      WRITE(6,*)'    | Modified: 02/09/99                          |'
      WRITE(6,*)'    |          Oslo Cyclotron Laboratory          |'
      WRITE(6,*)'    |               Magne Guttormsen              |'
      WRITE(6,*)'    |_____________________________________________|'


C Reading in (x,y) pairs
      WRITE(6,*)' '
      WRITE(6,*)'Please, from an ordinary text file read in a set of '
      WRITE(6,*)'x(i) and y(i) values. Each line should have two numbers,'
      WRITE(6,*)'and the file should end up with the last two numbers.'
      WRITE(6,*)'No x-values must be equal; the result will be NaN'
      WRITE(6,*)'Here, is an example of three (x,y) pairs:'
      WRITE(6,*)'1.0233 17.1234'
      WRITE(6,*)'2 1222'
      WRITE(6,*)'1.2323E02 2.322'
   
      FILNAM = 'TEST'
      WRITE(6,2)FILNAM(1:4)
  2   FORMAT('Give name of file with (x,y) pairs <',A,'>:',$)
      CALL READA(5,FILNAM)
      IF(Istatus.NE.0)GO TO 999
      OPEN(20,FILE=FILNAM,ACCESS='SEQUENTIAL',status='old',ERR=999)
      GO TO 4
999   WRITE(6,*)'Sorry, filename do not excist'
      CLOSE(20)
      STOP
   
  4   CONTINUE  
      DO i=1,8192
         READ(20,*,ERR=100)xa(i),ya(i)
      ENDDO
100   NP = i-1
      CLOSE(20)

      WRITE(6,6)NP
  6   FORMAT('Number of (x,y) pairs read is: ',I4)    
      IF(NP.LE.1)THEN
         WRITE(6,*)'Sorry, need at least 2 points to interpolate'
         STOP
      ENDIF
       
C Calculates reasonable new calibration a0 and a1
      xlow = +1.0E+38
      xhigh= -1.0E+38
      DO i = 1,NP
         IF(xa(i).LT.xlow )xlow =xa(i)
         IF(xa(i).GT.xhigh)xhigh=xa(i)
         WRITE(6,*)'old i, x, y: ',i,xa(i),ya(i)
      ENDDO

      WRITE(6,8)xlow
 8    FORMAT('Give lower x-value for new MAMA spectrum                   <',E10.3,'>:',$)
      CALL READF(5,xlow)
      WRITE(6,10)xhigh
 10   FORMAT('Give higher x-value for new MAMA spectrum                  <',E10.3,'>:',$)
      CALL READF(5,xhigh)

      a1 = (xhigh-xlow)/NP
      a0 = xlow
      WRITE(6,12)a0
 12   FORMAT('Give calibration constant for new MAMA spectrum a0(keV)    <',E10.3,'>:',$)
      CALL READF(5,a0)
      WRITE(6,14)a1
 14   FORMAT('Give calibration constant a1(keV/ch) for new MAMA spectrum <',E10.3,'>:',$)
      CALL READF(5,a1)
      MAXCH = INT(((xhigh-a0)/a1)+0.5)
      MAXCH = MIN(MAXCH,8191)

      WRITE(6,16)a0,a1,MAXCH+1,xlow,xhigh
16    FORMAT(/,'New calibration: a0=',E10.3,'keV and a1=',E10.3,'keV/ch',
     + /,      'Dimension:',I4,', interpolating between: ',E10.3,' - ',E10.3)
        
C Interpolating with spline-method (From the book: Numerical Recepies)
      yp1 = 0                               !Calculates the derivatives at end-points
      ypn = 0
      dx=xa(2)-xa(1)
      dy=ya(2)-ya(1)
      IF(dx.NE.0)yp1=dy/dx
      dx=xa(NP)-xa(NP-1)
      dy=ya(NP)-ya(NP-1)
      IF(dx.NE.0)ypn=dy/dx
      CALL spline(xa,ya,NP,yp1,ypn,y2)      !Finding second derivatives
      DO i=0,MAXCH
        xx = a0 + a1*FLOAT(i)               !Excitation energy bin of spectra
        CALL splint(xa,ya,y2,NP,xx,yy)      !Interpolating
        Spec(i) = yy
        WRITE(6,*)'new i x y: ',i,xx,yy
      ENDDO

C  Writing mama spectrum to file
      Calib(1)=a0
      Calib(2)=a1
      Calib(3)=0.0
      length=MAXCH+1
      outfile='spec.s2m'
      comment='Splined set of (x,y) pairs -> mama spectrum'
      OPEN(21,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw1dim(21,comment,length,Spec,Calib)
      CLOSE(21)
      WRITE(6,62) outfile
62    FORMAT('Splined data -> mama is written to file: ',A11)

      CLOSE(21)

      GO TO 98  
99    WRITE(6,*)'Could not open file'
98    CONTINUE
      END

      SUBROUTINE splint(xa,ya,y2a,n,x,y)
      INTEGER n
      REAL x,y,xa(8192),y2a(8192),ya(8192)
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
      y=a*ya(klo)+b*ya(khi)+((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**2)/6.
      return
      END


      SUBROUTINE spline(x,y,n,yp1,ypn,y2)
      INTEGER n
      REAL yp1,ypn,x(8192),y(8192),y2(8192)
      INTEGER i,k
      REAL p,qn,sig,un,u(8192)
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
        u(i)=(6.*((y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1))/(x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*u(i-1))/p
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
