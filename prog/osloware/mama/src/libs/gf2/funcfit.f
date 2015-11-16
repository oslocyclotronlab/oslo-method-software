      SUBROUTINE FUNCFIT 
      CHARACTER*40 ANS
      REAL x(1:200),y(1:200),logy(1:200),a0(0:3),a1(0:3),a2(0:3),a3(0:3)
      REAL*8 XMAT(4,4),sx1,sx2,sx3,sx4,sx5,sx6,sx0y,sx1y,sx2y,sx3y
      INTEGER typefunc,typeinput
      COMMON/commonfit/NA

      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      INTEGER COLORMAP(19),Color(0:19)
      COMMON /COLORMAP/ COLORMAP,Limit,Color
      REAL Limit(0:19)

      INTEGER XDIM,YDIM
      CHARACTER APP*4
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      COMMON/DisType/Idistype,OLlow,OLhigh,OLlocnt,OLhicnt
      INTEGER                 OLlow,OLhigh

      COMMON/pofit/iii

      EXTERNAL fhyp,fexp,lexp,dE,fFermi1,lFermi1,fFermi2,lFermi2,fPol
      REAL lFermi1,lFermi2,lexp
      DIMENSION FIT(200),SIGMAY(200),A(5),DELTAA(5),SIGMAA(5),CH(500)
      DIMENSION bSIGMAA(5),bA(5)
      CHARACTER*8 chd
      COMMON/THICK/d,ifunc
      DIMENSION Spec(0:8191),Calib(6)
      CHARACTER NAME*80,COMMEN*60,CHR*40
      INTEGER dim

C patched by Andreas Schiller September 10 2003.
C reason: make more standard repetitive formats

C added the following variables:
      CHARACTER*16 F1STR,F2STR,F3STR,F4STR,F5STR,F6STR,F7STR,F8STR,F9STR,F0STR

      iblue    =COLORMAP(1)
      igreen   =COLORMAP(4)
      ibrown   =COLORMAP(7)
      ired     =COLORMAP(10)
      iwhite   =19
      iblack   =20

C Testing if called from PO command. Then simplified tests
      IF(iii.EQ.1)THEN
        typefunc=0
        typeinput=1
        iii=0
        GO TO 7777
      ENDIF
C Asking what type of fit and input by mouse or typing
      WRITE(6,*)'Give function to fit data points:'
      WRITE(6,*)'Polynomial  y=a+bx+cxx+dxxx ....(0)'
      WRITE(6,*)'Hyperbolic  y=a+b/x ............(1)'
      WRITE(6,*)'Exponential y=a+be^(cx) ........(2)'
      WRITE(6,*)'Range R(E) from dE-E plot ......(3)'
      WRITE(6,*)'A3+A2*EXP(2*sqrt(A1*x))/x*x.....(4)'
      WRITE(6,*)'A3+A2*EXP(2*sqrt(A1*x))/x**3/2..(5)'
      WRITE(6,*)'Return to mama .................(6)'
      ians=0
      WRITE(6,1)ians
 1    FORMAT('Give your answer <',I1,'>:',$)
      CALL READI(5,ians)
      IF(ians.LT.0.OR.ians.GT.5)RETURN
      IF(Istatus.NE.0)RETURN
      typefunc=ians
    
      IF(typefunc.EQ.3)THEN
        WRITE(6,*)' '
        WRITE(6,*)'You should now have displayed a dE-E matrix'
        WRITE(6,*)'where you may click along the banana corresponding'
        WRITE(6,*)'to the type of particles you want to optimize for.'
        WRITE(6,*)'Use many points (max=200) and be sure to click'
        WRITE(6,*)'for x y pairs at the ends of the banana. Blow'
        WRITE(6,*)'up your mama graphic window in order to make'
        WRITE(6,*)'accurate clicking. If you have not the proper'
        WRITE(6,*)'dE-E plot displayed, you may now return to mama.'
      ENDIF

      WRITE(6,*)' '
      WRITE(6,*)'Give input values (x,y) by clicking or typing:'
      WRITE(6,*)'Clicking in displayed matrix/spectrum .....(0)'
      WRITE(6,*)'Typing x y pairs from keyboard ............(1)'
      WRITE(6,*)'Return to mama ............................(2)'
      ians=0
      WRITE(6,1)ians
      CALL READI(5,ians)
      IF(ians.LT.0.OR.ians.GT.1)RETURN
      IF(Istatus.NE.0)RETURN
      typeinput=ians

7777  CONTINUE
      DO i=0,3
        a0(i)=0.
        a1(i)=0.
        a2(i)=0.        
        a3(i)=0.
      ENDDO
C First locating where the green button is in pixels (see curse.f)
      CALL INITG(nx,ny)             !Pixel size of window
      is=18

      mx1=2
      mx2=mx1+2*is
      mx3=mx2+4
      mx4=mx3+2*is
      mx5=mx4+4
      mx6=mx5+3*is
      mx7=mx6+4
      mx8=mx7+3*is
      mx10=nx-1
      mx9=mx10-13

      my1=ny+42
      my2=my1-0.7*is
      my3=my2-0.3*is
      my33=my3-4    !An extra -4 because we never get iy > spec. area

C Input by clicking in proper spectrum
      i=0
      IF(typeinput.EQ.0)THEN
      Istatus=-7     !to flag that green button is active
      CALL CLEANUP
        IF(i.GT.200)THEN
          WRITE(6,*)'Maximum (200) allowed datapoints reached'
          GO TO 98
        ENDIF  
        WRITE(6,*)'Type X or click on green button to exit'
999     CALL RETIC(Xp,Yp,ANS)
        i=i+1
        CALL CVXY(Xp,Yp,ix,iy,1)
        IF(ANS(1:1).EQ.'X'.OR.ANS(1:1).EQ.'x')THEN
          Istatus=0    !to flag that green button is active
          CALL CLEANUP
          GO TO 98
        ENDIF
        IF(ix.GE.mx9.AND.ix.LE.mx10.AND.iy.GE.my33)THEN
          Istatus=0    !to flag that green button is active
          CALL CLEANUP
          GO TO 98
        ENDIF
C Finding x y position in matrix or spectrum
        IF(ITYPE.GT.1)THEN                        ! matrix
          x(i)=Xp
          y(i)=Yp
        ENDIF
        IF(ITYPE.EQ.1)THEN                        ! spectrum
          x(i)=Xp
          iix=Xp
          IF(iix.GE.0.AND.iix.LE.8191)THEN
            y(i)=rSPEC(IDEST,iix)     
          ELSE
            i=i-1           ! not a valid (x,y) point
          ENDIF
        ENDIF
        WRITE(6,4)i,x(i),y(i)
        GO TO 999
      ENDIF

C Input by typing from keyboard
      IF(typeinput.EQ.1)THEN
        WRITE(6,2)
 2      FORMAT(/'Type pairs of x and y values for the data set.',/,
     +  'The numbers in a pair should be separated with space,',/,
     +  'and each pair should end with RETURN. Type characters',/,       
     +  'instead of numbers to terminate the inputs',/,   
     +  'Example:',/,       
     +  '1.2   5.7',/,    
     +  '2.6   9.7',/,       
     +  '3.1   8.5',/,    
     +  'x',/,
     +   '(Maximum 200 pairs)')
C Reading in the data set of x and y values:
        DO i=1,200
          CALL ASK('Type x y pair (character for stop): ',36,ANS,K)
          CALL FFIN(ANS,K,x(i),y(i),ydum,&98)
        ENDDO
      ENDIF

C Listing current data points and testing for doubletts
 98   imax=i-1
 99   WRITE(6,3)
      xave=0
      yave=0
  3   FORMAT(/'   Pair no.          X            Y')
      DO i=1,imax
        WRITE(6,4)i,x(i),y(i)
  4     FORMAT(4X,I3,4X,2E13.6)
        xave=xave+x(i)
        yave=yave+y(i)
      ENDDO
      idouble=0
      DO i=1,imax-1
        DO ii=i+1,imax
          IF((x(i).EQ.x(ii)).AND.(y(i).EQ.y(ii)))THEN !identical points
            idouble=idouble+1
            WRITE(6,*)'Warning, you have identical pairs'
          ENDIF
        ENDDO
      ENDDO

      xave=xave/imax
      yave=yave/imax
      WRITE(6,5)xave,yave
  5   FORMAT('Average:   ',2E13.6)
      xl=x(1)
      xh=x(1)
      x1=x(1)
      x2=x(1)
      yl=y(1)
      yh=y(1)
      DO i=2,imax                       !finding display limits
        IF(x(i).LT.xl)x1=x(i)
        IF(x(i).GT.xh)x2=x(i)
        IF(y(i).LT.yl)THEN
          yl=y(i)
          xl=x(i)
        ENDIF
        IF(y(i).GT.yh)THEN
          yh=y(i)
          xh=x(i)
        ENDIF
      ENDDO

C Displaying current data points
      IF(typeinput.EQ.0)THEN              !old display
        CALL DSfunc(x,y,imax,0,0,0,0,1)
      ENDIF
      IF(typeinput.EQ.1)THEN              !new display
        dx=(x2-x1)/10.                    !some air on the sides
        dy=(yh-yl)/10.
        CALL DSfunc(x,y,imax,x1-dx,x2+dx,yl-dy,yh+dy,1)
      ENDIF

C Asking if values OK, or modify them
77    ians=0
      Istatus=0
      WRITE(6,*)'You may modify one or more x y data points'
      WRITE(6,*)'in the list by typing from keyboard:'
      WRITE(6,*)'Input values are OK, start fitting ....(0)'
      WRITE(6,*)'Delete an  x y pair ...................(1)'
      WRITE(6,*)'Replace an x y pair ...................(2)'
      WRITE(6,*)'Add a new  x y pair....................(3)'
      WRITE(6,*)'Return to mama ........................(4)'
      WRITE(6,1)ians
      CALL READI(5,ians)
      IF(ians.EQ.4)RETURN
      IF(ians.LT.0.OR.ians.GT.4)GO TO 77
      IF(Istatus.NE.0)GO TO 77

      IF(ians.EQ.1)THEN                             !delete
        Ipair=imax
        WRITE(6,10)Ipair
 10     FORMAT(/'Delete pair no <',I3,'>:',$)
        CALL READI(5,Ipair)
        IF(Ipair.LT.1.OR.Ipair.GT.200)GO TO 99
        IF(Istatus.NE.0)GO TO 99
        DO i=max0(2,Ipair+1),imax
          x(i-1)=x(i)
          y(i-1)=y(i)
        ENDDO
        imax=imax-1
        GO TO 99
      ENDIF
      IF(ians.EQ.2)THEN                             !replace
        Ipair=imax
        WRITE(6,11)Ipair
 11     FORMAT(/'Replace pair no <',I3,'>:',$)
        CALL READI(5,Ipair)
        IF(Ipair.LT.1.OR.Ipair.GT.200)GO TO 99
        IF(Istatus.NE.0)GO TO 99
        WRITE(6,12)x(Ipair),y(Ipair)
 12     FORMAT(  'Old values are:                 ',2E13.6)
        CALL ASK('Type modified x y and (RETURN): ',32,ANS,K)
        CALL FFIN(ANS,K,x(Ipair),y(Ipair),ydum,&99)
        IF(Ipair.GT.imax)imax=Ipair
        GO TO 99
      ENDIF
      IF(ians.EQ.3)THEN                             !insert
        Ipair=imax+1
        CALL ASK('Type new x y and (RETURN): ',27,ANS,K)
        CALL FFIN(ANS,K,x(Ipair),y(Ipair),ydum,&99)
        imax=imax+1
        IF(imax.GT.200)imax=200
        GO TO 99
      ENDIF
      IF(imax.GT.200)THEN
        imax=200
        WRITE(6,*)'Maximum (200) allowed datapoints reached'
      ENDIF
      IF(imax-idouble.LT.2)RETURN

C Now comes the fitting according to the fit-function chosen  
C The number of points to fit is imax


C*********************************************************************
      IF(typefunc.EQ.0)THEN  !Polynomial, treated specially, not GRIDLS
        sx1=0.               !See p.277 in M.R. Spiegel Prob & Stat
        sx2=0.
        sx3=0.
        sx4=0.
        sx5=0.
        sx6=0.
        sx0y=0.
        sx1y=0.
        sx2y=0.
        sx3y=0.
       
        DO i=1,imax
          sx1=sx1+x(i)
          sx2=sx2+x(i)*x(i)
          sx3=sx3+x(i)*x(i)*x(i)
          sx4=sx4+x(i)*x(i)*x(i)*x(i)
          sx5=sx5+x(i)*x(i)*x(i)*x(i)*x(i)
          sx6=sx6+x(i)*x(i)*x(i)*x(i)*x(i)*x(i)
          sx0y=sx0y+y(i)
          sx1y=sx1y+x(i)*y(i)
          sx2y=sx2y+x(i)*x(i)*y(i)
          sx3y=sx3y+x(i)*x(i)*x(i)*y(i)
        ENDDO

C Constant fit
        IF(imax-idouble.GE.1)THEN
          XMAT(1,1)=imax
          XMAT(2,1)=sx1
          XMAT(1,2)=sx1
          XMAT(2,2)=sx2
          CALL MATINV(XMAT,1,4)
          a0(0)=XMAT(1,1)*sx0y
        ENDIF

C Linear fit
        IF(imax-idouble.GE.2)THEN
          XMAT(1,1)=imax
          XMAT(2,1)=sx1
          XMAT(1,2)=sx1
          XMAT(2,2)=sx2
          CALL MATINV(XMAT,2,4)
          a0(1)=XMAT(1,1)*sx0y+XMAT(2,1)*sx1y
          a1(1)=XMAT(1,2)*sx0y+XMAT(2,2)*sx1y
        ENDIF

C Quadratic fit
        IF(imax-idouble.GE.3)THEN       
          XMAT(1,1)=imax
          XMAT(2,1)=sx1
          XMAT(3,1)=sx2
          XMAT(1,2)=sx1
          XMAT(2,2)=sx2
          XMAT(3,2)=sx3
          XMAT(1,3)=sx2
          XMAT(2,3)=sx3
          XMAT(3,3)=sx4
          CALL MATINV(XMAT,3,4)
          a0(2)=XMAT(1,1)*sx0y+XMAT(2,1)*sx1y+XMAT(3,1)*sx2y
          a1(2)=XMAT(1,2)*sx0y+XMAT(2,2)*sx1y+XMAT(3,2)*sx2y
          a2(2)=XMAT(1,3)*sx0y+XMAT(2,3)*sx1y+XMAT(3,3)*sx2y
        ENDIF

C Cubic fit
        IF(imax-idouble.GE.4)THEN
          XMAT(1,1)=imax
          XMAT(2,1)=sx1
          XMAT(3,1)=sx2
          XMAT(4,1)=sx3
          XMAT(1,2)=sx1
          XMAT(2,2)=sx2
          XMAT(3,2)=sx3
          XMAT(4,2)=sx4
          XMAT(1,3)=sx2
          XMAT(2,3)=sx3
          XMAT(3,3)=sx4
          XMAT(4,3)=sx5
          XMAT(1,4)=sx3
          XMAT(2,4)=sx4
          XMAT(3,4)=sx5
          XMAT(4,4)=sx6      
          CALL MATINV(XMAT,4,4)
          a0(3)=XMAT(1,1)*sx0y+XMAT(2,1)*sx1y+XMAT(3,1)*sx2y+XMAT(4,1)*sx3y
          a1(3)=XMAT(1,2)*sx0y+XMAT(2,2)*sx1y+XMAT(3,2)*sx2y+XMAT(4,2)*sx3y
          a2(3)=XMAT(1,3)*sx0y+XMAT(2,3)*sx1y+XMAT(3,3)*sx2y+XMAT(4,3)*sx3y
          a3(3)=XMAT(1,4)*sx0y+XMAT(2,4)*sx1y+XMAT(3,4)*sx2y+XMAT(4,4)*sx3y
        ENDIF

C Outputs
        chi0=0.
        chi1=0.
        chi2=0.
        chi3=0.    
        WRITE(6,20)                                            
 20   FORMAT(/'Pair no.    X         Y    Constant  Linear  Quadratic    Cubic')
        DO i=1,imax
          y0=a0(0)
          chi0=chi0+(y(i)-y0)*(y(i)-y0)          
          y1=a0(1)+a1(1)*x(i)
          chi1=chi1+(y(i)-y1)*(y(i)-y1)
          y2=a0(2)+a1(2)*x(i)+a2(2)*x(i)*x(i)
          chi2=chi2+(y(i)-y2)*(y(i)-y2)
          y3=a0(3)+a1(3)*x(i)+a2(3)*x(i)*x(i)+a3(3)*x(i)*x(i)*x(i)
          chi3=chi3+(y(i)-y3)*(y(i)-y3)
          fy0=9.999
          fy1=9.999
          fy2=9.999
          fy3=9.999
          IF(imax.GE.1)fy0=y0
          IF(imax.GE.2)fy1=y1
          IF(imax.GE.3)fy2=y2
          IF(imax.GE.4)fy3=y3

          WRITE(6,21)i,x(i),y(i),fy0,fy1,fy2,fy3
 21       FORMAT(I4,6F10.3)
        ENDDO

        ch0=9.999
        ch1=9.999
        ch2=9.999
        ch3=9.999
        IF(imax.GE.1)ch0=chi0/(imax-1.+0.1)
        IF(imax.GE.2)ch1=chi1/(imax-2.+0.1)
        IF(imax.GE.3)ch2=chi2/(imax-3.+0.1)
        IF(imax.GE.4)ch3=chi3/(imax-4.+0.1)
        WRITE(6,22)ch0,ch1,ch2,ch3
 22     FORMAT('Chisquare (Ysigma=1):   ',4E10.3)

        IF(imax-idouble.GE.1)THEN
          WRITE(6,13)a0(0)
 13       FORMAT('Const.: a=',F12.4)
          CALL SETCOLOR(ired)
          CALL CVXY(xx,yy,1,iy,2)
          yy=a0(0)
          CALL CVXY(xx,yy,ix,iy,1)
          CALL KTRAS(1,iy,0)
          DO ix=2,nx-30
            CALL CVXY(xx,yy,ix,iy,2)
            yy=a0(0)
            CALL CVXY(xx,yy,ix,iy,1)
            CALL KTRAS(ix,iy,1)
          ENDDO
          IF(iy.GT.ny-20)iy=ny-50
          IF(iy.LT.10)   iy=50
          CALL KTRAS(ix,iy,0)
          CALL PUTG('Const.',6,1,1)
          CALL FINIG
        ENDIF
        IF(imax-idouble.GE.2)THEN
          WRITE(6,23)a0(1),a1(1)
 23       FORMAT('Linear: a=',F12.4,' b=',F12.4)
          CALL SETCOLOR(iblue)
          CALL CVXY(xx,yy,1,iy,2)
          yy=a0(1)+a1(1)*xx
          CALL CVXY(xx,yy,ix,iy,1)
          CALL KTRAS(1,iy,0)
          DO ix=2,nx-30
            CALL CVXY(xx,yy,ix,iy,2)
            yy=a0(1)+a1(1)*xx
            CALL CVXY(xx,yy,ix,iy,1)
            CALL KTRAS(ix,iy,1)
          ENDDO
          IF(iy.GT.ny-20)iy=ny-50
          IF(iy.LT.10)   iy=50
          CALL KTRAS(ix,iy,0)
          CALL PUTG('Linear',6,1,1)
          CALL FINIG
        ENDIF
        IF(imax-idouble.GE.3)THEN
          WRITE(6,24)a0(2),a1(2),a2(2)
  24      FORMAT('Quadr.: a=',F12.4,' b=',F12.4,' c= ',E13.6)
          CALL SETCOLOR(igreen)
          CALL CVXY(xx,yy,1,iy,2)
          yy=a0(2)+a1(2)*xx+a2(2)*xx*xx
          CALL CVXY(xx,yy,ix,iy,1)
          CALL KTRAS(1,iy,0)
          DO ix=2,nx-30
            CALL CVXY(xx,yy,ix,iy,2)
            yy=a0(2)+a1(2)*xx+a2(2)*xx*xx
            CALL CVXY(xx,yy,ix,iy,1)
            CALL KTRAS(ix,iy,1)
          ENDDO
          IF(iy.GT.ny-20)iy=ny-40
          IF(iy.LT.10)   iy=40
          CALL KTRAS(ix,iy,0)
          CALL PUTG('Quadr.',6,1,1)
          CALL FINIG
        ENDIF
        IF(imax-idouble.GE.4)THEN
          WRITE(6,25)a0(3),a1(3),a2(3),a3(3)
  25      FORMAT('Cubic : a=',F12.4,' b=',F12.4,' c= ',E13.6,
     +    ' d= ',E14.8)
          CALL SETCOLOR(ibrown)
          CALL CVXY(xx,yy,1,iy,2)
          yy=a0(3)+a1(3)*xx+a2(3)*xx*xx+a3(3)*xx*xx*xx
          CALL CVXY(xx,yy,ix,iy,1)
          CALL KTRAS(1,iy,0)
          DO ix=2,nx-30
            CALL CVXY(xx,yy,ix,iy,2)
            yy=a0(3)+a1(3)*xx+a2(3)*xx*xx+a3(3)*xx*xx*xx
            CALL CVXY(xx,yy,ix,iy,1)
            CALL KTRAS(ix,iy,1)
          ENDDO
          IF(iy.GT.ny-20)iy=ny-30
          IF(iy.LT.10)   iy=30
          CALL KTRAS(ix,iy,0)
          CALL PUTG('Cubic ',6,1,1)
          CALL FINIG
        ENDIF
        CALL WriteFit(fpol,typefunc,a0,a1,a2,a3)
      ENDIF


C*********************************************************************
      IF(typefunc.EQ.1)THEN
        iOK = 0
        NA  = 1
        IF(Istatus.NE.0)GO TO 99
        IF(NA.GT.imax-idouble)THEN
          WRITE(6,*)'Number of parameters < independent data points'
          RETURN
        ENDIF
C Estimate parameters to be fitted
        A(1)=xave*yave
        IF(A(1).EQ.0.)A(1)=0.1
        A(2)=0.1

        WRITE(6,100)
 100    FORMAT(/'In order to start fit, start-values for the parameters',/,
     +         'have to be given. The default values are estimated from',/,
     +         'the y(x) values for the first (xl) and last (xh) point.',/,
     +         'You may start fitting with these values, or simply obtain',/,
     +         'the function with the given parameters (without fit).',/,
     +         ' ',/,
     +         'Your present fit-function looks like this (x=channels):',/,
     +         'Hyperbola(x) = A2 + A1/x                               ')
 
 177    CONTINUE
        WRITE(6,401)  A(1)
 101    FORMAT(/'Give start value for A1 parameter  <',E11.4,'>:',$)
        CALL READF(5,A(1))
        IF(Istatus.NE.0)RETURN
        WRITE(6,402)  A(2)
 102    FORMAT( 'Give start value for A2 parameter  <',E11.4,'>:',$)
        CALL READF(5,A(2))
        IF(Istatus.NE.0)RETURN

 199    IF(iOK.EQ.0)GO TO 188           !no fit yet
        ians=0
        Istatus=0
        WRITE(6,*)' '
        WRITE(6,*)'Give type of Chi**2 weighting:'
        WRITE(6,*)'Weighting with 1/Yexp(i) ............(0)'
        WRITE(6,*)'Weighting with 1.0 for all Yexp(i)...(1)'
        WRITE(6,*)'Return to mama ......................(2)'
        WRITE(6,1)ians
        CALL READI(5,ians)
        IF(ians.EQ.2)RETURN
        IF(ians.LT.0.OR.ians.GT.2)GO TO 177
        IF(Istatus.NE.0)GO TO 177
        IF(ians.EQ.0)MODE=-1
        IF(ians.EQ.1)MODE=0
 196    PROG  =0.0001
        M     =0
        CHISQR=1.0E+10
        bCH   =1.0E+10
        NBAD  =0
        DO i=1,NA
          SIGMAA(i)=A(i)/10.
          DELTAA(i)=A(i)/10.
        ENDDO

        IF(iOK.EQ.0)GO TO 188
 123    CALL GRIDLS(x,y,SIGMAY,imax,MODE,fhyp,A
     +  ,DELTAA,SIGMAA,NA,FIT,CHISQR)
        M=M+1
        CH(M)=CHISQR
        IF(M.LT.2) GO TO 123
        VER=ABS(CH(M)-CH(M-1))/CH(M-1)
        IF(CH(M).LT.bCH)THEN
          bCH=CH(M)               !saving best values
          DO i=1,NA
            bSIGMAA(i)=SIGMAA(i)
            bA(i)     =A(i)
          ENDDO
        ENDIF
        IF(CH(M).GT.CH(M-1))NBAD=NBAD+1
        IF(NBAD.GT.4.OR.M.GE.500)GO TO 124
        IF(VER.GT.PROG) GO TO 123
 124    CONTINUE

        DO i=1,NA      !replacing best values
          SIGMAA(i)=bSIGMAA(i)
          A(i)     =bA(i)
        ENDDO

C Presenting results, both numerical and graphical
        WRITE(6,120)                                            
 120    FORMAT(/'Pair no.       X            Y          FIT')
        DO i=1,imax
          WRITE(6,121)i,x(i),y(i),fhyp(x(i),A)
 121      FORMAT(I4,3E13.6)
        ENDDO

        ch1=0.000
        IF(imax.GT.NA)ch1=bCH
        WRITE(6,131)ch1
 131     FORMAT('Chisquare:',E12.3)

C changed the following 6 lines A.S.
        WRITE(F1STR,132)NA
        WRITE(6,FMT=F1STR)'Fit parameters  a,   b,... =',(a(i),i=NA,1,-1)
 132    FORMAT('(A28,',I5.5,'E12.4)')
        WRITE(F2STR,133)NA
        WRITE(6,FMT=F2STR)'Uncertainty  siga,sigb,... =',(sigmaa(i),i=NA,1,-1)
 133    FORMAT('(A28,',I5.5,'E12.4)')

 188    IF(NA.EQ.1)THEN
          CALL SETCOLOR(ired+iOK*10)
          CALL PUTG('   a/x',6,1,1)
          a0(0)=a(1)
        ENDIF
        IF(NA.EQ.2)THEN
          CALL SETCOLOR(igreen-iOK*3)
          CALL PUTG(' a+b/x',6,1,1)
          a0(1)=a(2)
          a1(1)=a(1)
        ENDIF

        CALL CVXY(xx,yy,1,iy,2)
        yy=fhyp(xx,A)
        CALL CVXY(xx,yy,ix,iy,1)
        CALL KTRAS(1,iy,0)
        DO ix=2,nx-30
          CALL CVXY(xx,yy,ix,iy,2)
          yy=fhyp(xx,A)
          CALL CVXY(xx,yy,ix,iy,1)
          CALL KTRAS(ix,iy,1)
        ENDDO
        IF(iy.GT.ny-20)iy=ny-NA*20
        IF(iy.LT.10)   iy=NA*20
        CALL KTRAS(ix,iy,0)
        CALL FINIG

        IF(NA.EQ.1.AND.imax-idouble.GE.2)THEN
          NA=2
          A(2)=yl
          IF(A(2).EQ.0)A(2)=0.1
          GO TO 196  ! a new fit with one more parameter
        ENDIF

        IF(iOK.EQ.1)GO TO 143
        NA   = 1
        iOK  = 0        ! no fit
        ians = 0        ! start fitting
        WRITE(6,*)' '
        WRITE(6,*)'Start fitting........................(0)'
        WRITE(6,*)'Modify the start parameters..........(1)'
        WRITE(6,*)'The function looks OK, do not fit....(2)'
        WRITE(6,*)'Return to mama.......................(3)'
        WRITE(6,1)ians
        CALL READI(5,ians)
        IF(Istatus.NE.0)GO TO 177
        IF(ians.EQ.0)iOK=1
        IF(ians.EQ.1)GO TO 177
        IF(ians.EQ.2)iOK=0
        IF(ians.EQ.3)RETURN
        IF(ians.LT.0.OR.ians.GT.3)GO TO 177
        IF(iOK.EQ.1)GO TO 199
        IF(iOK.EQ.0)GO TO 144

 143    WRITE(6,140)
 140    FORMAT('Final result:')
        WRITE(6,141)a0(0)
 141    FORMAT('y=a/x   : a=',E14.6)
        WRITE(6,142)a0(1),a1(1)
 142    FORMAT('y=a+b/x : a=',E14.6,' b=',E14.6)
 144    CALL WriteFit(fhyp,typefunc,a0,a1,a2,a3)
      ENDIF


C*********************************************************************
      IF(typefunc.EQ.2)THEN
        iOK = 0
        NA  = 2
        IF(Istatus.NE.0)GO TO 99
        IF(NA.GT.imax-idouble)THEN
          WRITE(6,*)'Number of parameters < independent data points'
          RETURN
        ENDIF

C Estimate parameters to be fitted
        IF(yl.LT.0)yl=0
        IF(yh.LT.0)yh=0
        A(1)=(alog(yl)-alog(yh))/(xl-xh)
        IF(A(1).EQ.0)A(1)=0.1
        A(2)=yh/(exp(xh*A(1)))
        IF(A(2).EQ.0)A(2)=0.1
        A(3)=yl

        WRITE(6,200)
 200    FORMAT(/'In order to start fit, start-values for the parameters',/,
     +         'have to be given. The default values are estimated from',/,
     +         'the y(x) values for the first (xl) and last (xh) point.',/,
     +         'You may start fitting with these values, or simply obtain',/,
     +         'the function with the given parameters (without fit).',/,
     +         ' ',/,
     +         'Your present fit-function looks like this (x=channels):',/,
     +         'Exponential(x) = A3 + A2 * EXP(A1*x)')
 
 277    CONTINUE
        WRITE(6,401)  A(1)
 201    FORMAT(/'Give start value for A1 parameter  <',E11.4,'>:',$)
        CALL READF(5,A(1))
        IF(Istatus.NE.0)RETURN
        WRITE(6,402)  A(2)
 202    FORMAT( 'Give start value for A2 parameter  <',E11.4,'>:',$)
        CALL READF(5,A(2))
        IF(Istatus.NE.0)RETURN
        WRITE(6,403)  A(3)
 203    FORMAT( 'Give start value for A3 parameter  <',E11.4,'>:',$)
        CALL READF(5,A(3))
        IF(Istatus.NE.0)RETURN

 299    IF(iOK.EQ.0)GO TO 288
        ians=1
        Istatus=0
        WRITE(6,*)' '
        WRITE(6,*)'Give type of Chi**2 weighting:'
        WRITE(6,*)'Weighting with 1/Yexp(i) ............(0)'
        WRITE(6,*)'Weighting with 1.0 for all Yexp(i)...(1)'
        WRITE(6,*)'Return to mama ......................(2)'
        WRITE(6,1)ians
        CALL READI(5,ians)
        IF(ians.EQ.2)RETURN
        IF(ians.LT.0.OR.ians.GT.2)GO TO 277
        IF(Istatus.NE.0)GO TO 277
        IF(ians.EQ.0)MODE=-1
        IF(ians.EQ.1)MODE=0
 296    PROG  =0.0001
        M=0
        CHISQR=1.0E+10
        bCH   =1.0E+10
        NBAD  =0
        DO i=1,NA
          SIGMAA(i)=A(i)/10.
          DELTAA(i)=A(i)/10.
        ENDDO
        DO i=1,imax
          logy(i)=0.
          IF(y(i).GT.0)logy(i)=log(y(i))
        ENDDO

        IF(iOK.EQ.0)GO TO 288
 223    CALL GRIDLS(x,logy,SIGMAY,imax,MODE,lexp,A,DELTAA,SIGMAA,NA,FIT,CHISQR)
        M=M+1
        CH(M)=CHISQR
        IF(M.LT.2) GO TO 223
        VER=ABS(CH(M)-CH(M-1))/CH(M-1)
        IF(CH(M).LT.bCH)THEN
          bCH=CH(M)               !saving best values
          DO i=1,NA
            bSIGMAA(i)=SIGMAA(i)
            bA(i)     =A(i)
          ENDDO
        ENDIF
        IF(CH(M).GT.CH(M-1))NBAD=NBAD+1
        IF(NBAD.GT.4.OR.M.GE.500)GO TO 224
        IF(VER.GT.PROG) GO TO 223
 224    CONTINUE

        DO i=1,NA                !replacing best values
          SIGMAA(i)=bSIGMAA(i)
          A(i)     =bA(i)
        ENDDO

C Presenting results, both numerical and graphical
        WRITE(6,220)                                            
 220    FORMAT(/'Pair no.       X            Y          FIT')
        DO i=1,imax
          WRITE(6,221)i,x(i),y(i),fexp(x(i),A)
 221      FORMAT(I4,3E13.3)
        ENDDO

        ch1=0.000
        IF(imax.GT.NA)ch1=bCH
        WRITE(6,231)ch1
 231     FORMAT('Chisquare:',E12.3)

C changed the following 6 lines A.S.
        WRITE(F3STR,232)NA
        WRITE(6,FMT=F3STR)'Fit parameters  a,   b,... =',(a(i),i=NA,1,-1)
 232    FORMAT('(A28,',I5.5,'E12.4)')
        WRITE(F4STR,233)NA
        WRITE(6,FMT=F4STR)'Uncertainty  siga,sigb,... =',(sigmaa(i),i=NA,1,-1)
 233    FORMAT('(A28,',I5.5,'E12.4)')

 288    IF(NA.EQ.2)THEN
        CALL SETCOLOR(ired+iOK*10)
        CALL PUTG('ae^(bx)',7,1,1)
          a0(1)=a(2)
          a1(1)=a(1)
        ENDIF
        IF(NA.EQ.3)THEN
          CALL SETCOLOR(igreen-iOK*3)
          CALL PUTG('a+be^(cx)',9,1,1)
          a0(2)=a(3)
          a1(2)=a(2)
          a2(2)=a(1)
        ENDIF

        CALL CVXY(xx,yy,1,iy,2)
        yy=fexp(xx,A)
        CALL CVXY(xx,yy,ix,iy,1)
        CALL KTRAS(1,iy,0)
        DO ix=2,nx-30
          CALL CVXY(xx,yy,ix,iy,2)
          yy=fexp(xx,A)
          CALL CVXY(xx,yy,ix,iy,1)
          CALL KTRAS(ix,iy,1)
        ENDDO
        IF(iy.GT.ny-20)iy=ny-(NA+1)*10
        IF(iy.LT.10)   iy=NA*10
        CALL KTRAS(ix-20,iy+10,0)
        CALL FINIG

        IF(NA.EQ.2.AND.imax-idouble.GE.3)THEN
          NA=3
          IF(A(3).EQ.0)A(3)=0.1
          GO TO 296     ! a new fit with one more parameter
        ENDIF

        IF(iOK.EQ.1)GO TO 243
        NA   = 2
        iOK  = 0        ! no fit
        ians = 0        ! start fitting
        WRITE(6,*)' '
        WRITE(6,*)'Start fitting........................(0)'
        WRITE(6,*)'Modify the start parameters..........(1)'
        WRITE(6,*)'The function looks OK, do not fit....(2)'
        WRITE(6,*)'Return to mama.......................(3)'
        WRITE(6,1)ians
        CALL READI(5,ians)
        IF(Istatus.NE.0)GO TO 277
        IF(ians.EQ.0)iOK=1
        IF(ians.EQ.1)GO TO 277
        IF(ians.EQ.2)iOK=0
        IF(ians.EQ.3)RETURN
        IF(ians.LT.0.OR.ians.GT.3)GO TO 277
        IF(iOK.EQ.1)GO TO 299
        IF(iOK.EQ.0)GO TO 244


 243    WRITE(6,240)
 240    FORMAT(/,'Final result:')
        WRITE(6,241)a0(1),a1(1)
 241    FORMAT('y=ae^(bx)   : a=',E14.6,' b=',E14.6)
        WRITE(6,242)a0(2),a1(2),a2(2)
 242    FORMAT('y=a+be^(cx) : a=',E14.6,' b=',E14.6,' c=',E14.6)
 244    CALL WriteFit(fexp,typefunc,a0,a1,a2,a3)
      ENDIF
     

C*********************************************************************
      IF(typefunc.EQ.3)THEN
        IF(Istatus.NE.0)GO TO 99
        IF(NA.GT.imax-idouble)THEN
          WRITE(6,*)'Number of parameters < independent data points'
          RETURN
        ENDIF

C Setting up defaults values
 300    ICAL=2                 !calibration is quadratic
        dim=2048
        d=130.
        WRITE(6,301)d
 301    FORMAT(/'Give thickness of Si ',
     +          'dE-detector (um) <',F6.1,'>:',$)
        CALL READF(5,d)
        IF(d.LE.1.OR.d.GT.3000)GO TO 300

C Choosing range function
      ifunc=1
      WRITE(6,361)ifunc
 361  FORMAT('Choose type of range curve:',/,
     +       'E(a1+a2*E) +a3*/(E+a4)    (1)',/,
     +       'a1*E**a2                  (2)',/,
     +       'a1*E**(a2+a3*log(E))      (3)',/,
     +       'exp(a1+a2*lnE+a3*lnE*lnE) (4) <',I1,'>:',$)
 
        CALL READI(5,ifunc)
        IF(ifunc.LT.1.OR.ifunc.GT.4)THEN
          Istatus=1
          RETURN
        ENDIF

 305    ab1=40.
        WRITE(6,302)ab1
 302    FORMAT('Type calibration a1 (keV/ch)',/, 
     +  'of the dE-E banana you want to fit <',F5.2,'>:',$)
        CALL READF(5,ab1)
        Calib(1)=0.
        Calib(2)=ab1
        Calib(3)=0.
        xc=20./ab1
        IF(ab1.LT.2.OR.ab1.GT.1000)GO TO 305

 304    CHR='alp'
        WRITE(6,303)CHR(1:3)
 303    FORMAT('Range curve for:',/,
     +         '                proton  (pro)',/,
     +         '                deuteron(deu)',/,
     +         '                triton  (tri)',/,
     +         '                tau     (tau)',/,
     +         '                alpha   (alp) <',A3,'>:',$)
        CALL READA(5,CHR)
        A(1)=0

        IF(ifunc.EQ.1)THEN
          NA=4
          IF(CHR.EQ.'pro'.OR.CHR.EQ.'PRO'.OR.
     +       CHR.EQ.'deu'.OR.CHR.EQ.'DEU'.OR.
     +       CHR.EQ.'tri'.OR.CHR.EQ.'TRI')THEN
            A(1)=0.81743/xc
            A(2)=0.0008742/(xc*xc)
            A(3)=-409.254
            A(4)=566.175*xc
          ENDIF
          IF(CHR.EQ.'tau'.OR.CHR.EQ.'TAU'.OR.
     +       CHR.EQ.'alp'.OR.CHR.EQ.'ALP')THEN
c            A(1)=0.29871/xc                     ! old values from Finn
c            A(2)=0.000133/(xc*xc)
c            A(3)=-474.768
c            A(4)=1864.*xc
            A(1)=0.756/xc                        ! new values from Magne
            A(2)=6.853E-05/(xc*xc)
            A(3)=-6720.
            A(4)=9650.*xc 
          ENDIF
        ENDIF

        IF(ifunc.EQ.2)THEN
          NA=2
          IF(CHR.EQ.'pro'.OR.CHR.EQ.'PRO'.OR.
     +       CHR.EQ.'deu'.OR.CHR.EQ.'DEU'.OR.
     +       CHR.EQ.'tri'.OR.CHR.EQ.'TRI')THEN
            A(1)=0.006
            A(2)=1.
            A(3)=0
            A(4)=0
          ENDIF
          IF(CHR.EQ.'tau'.OR.CHR.EQ.'TAU'.OR.
     +       CHR.EQ.'alp'.OR.CHR.EQ.'ALP')THEN
            A(1)=0.006    
            A(2)=1.
            A(3)=0
            A(4)=0 
          ENDIF
        ENDIF

        IF(ifunc.EQ.3)THEN
          NA=3
          IF(CHR.EQ.'pro'.OR.CHR.EQ.'PRO'.OR.
     +       CHR.EQ.'deu'.OR.CHR.EQ.'DEU'.OR.
     +       CHR.EQ.'tri'.OR.CHR.EQ.'TRI')THEN
            A(1)=0.0006
            A(2)=1.0
            A(3)=0.01
          ENDIF
          IF(CHR.EQ.'tau'.OR.CHR.EQ.'TAU'.OR.
     +       CHR.EQ.'alp'.OR.CHR.EQ.'ALP')THEN
            A(1)=0.0006
            A(2)=1.0
            A(3)=0.01
          ENDIF
        ENDIF

        IF(ifunc.EQ.4)THEN
          NA=3
          IF(CHR.EQ.'pro'.OR.CHR.EQ.'PRO'.OR.
     +       CHR.EQ.'deu'.OR.CHR.EQ.'DEU'.OR.
     +       CHR.EQ.'tri'.OR.CHR.EQ.'TRI')THEN
            A(1)=1.
            A(2)=0.1
            A(3)=0.01
          ENDIF
          IF(CHR.EQ.'tau'.OR.CHR.EQ.'TAU'.OR.
     +       CHR.EQ.'alp'.OR.CHR.EQ.'ALP')THEN
            A(1)=1.
            A(2)=0.1
            A(3)=0.01
          ENDIF
        ENDIF

        IF(A(1).EQ.0)GO TO 304
       
        IF(Istatus.NE.0)GO TO 99
        M     =0
        PROG  =0.00000001
        MODE  =0
        CHISQR=1.0E+10
        bCH   =1.0E+10
        NBAD  =0

C Estimate parameters to be fitted
        DO i=1,NA
          SIGMAA(i)=A(i)/10.
          DELTAA(i)=A(i)/10.
        ENDDO
 
  323   CALL GRIDLS(x,y,SIGMAY,imax,MODE,dE,A
     +  ,DELTAA,SIGMAA,NA,FIT,CHISQR)
        M=M+1
        CH(M)=CHISQR
        IF(M.LT.2) GO TO 323
        VER=ABS(CH(M)-CH(M-1))/CH(M-1)
        IF(CH(M).LT.bCH)THEN
          bCH=CH(M)                            !saving best values
          DO i=1,NA
            bSIGMAA(i)=SIGMAA(i)
            bA(i)     =A(i)
          ENDDO
        ENDIF
        IF(CH(M).GT.CH(M-1))NBAD=NBAD+1
        IF(M.EQ.20)THEN                 ! Just to try new values
          DO i=1,NA
            A(i)=(0.3*A(i)+0.7*bA(i))
          ENDDO
        ENDIF
        IF(M.EQ.40)THEN    
          DO i=1,NA
            A(i)=0.5*bA(i)
          ENDDO
        ENDIF
        IF(M.EQ.60)THEN    
          DO i=1,NA
            A(i)=1.5*bA(i)
          ENDDO
        ENDIF        
        IF(M.EQ.80)THEN     
          DO i=1,NA
            A(i)=bA(i)
          ENDDO
          A(1)=0.5*bA(1)
          A(2)=1.5*bA(2)
        ENDIF
        IF(M.EQ.100)THEN  
          DO i=1,NA
            A(i)=bA(i)
          ENDDO
          A(1)=1.5*bA(1)
          A(2)=0.5*bA(2)
        ENDIF
        IF(M.EQ.120)THEN  
          DO i=1,NA
            A(i)=bA(i)
          ENDDO
        ENDIF

        IF(NBAD.GT.100.OR.M.GE.500)GO TO 324
        IF(VER.GT.PROG) GO TO 323
 324    CONTINUE

        DO i=1,NA                              !replacing best values
          SIGMAA(i)=bSIGMAA(i)
          A(i)     =bA(i)
        ENDDO
C Presenting results, both numerical and graphical
        WRITE(6,320)                                            
 320    FORMAT(/'Pair no.       X            Y          FIT')
        DO i=1,imax
          WRITE(6,321)i,x(i),y(i),dE(x(i),A)
 321      FORMAT(I4,3E13.6)
        ENDDO

        ch1=0.000
        IF(imax.GT.NA)ch1=bCH
        WRITE(6,331)ch1
 331     FORMAT('Chisquare (Ysigma=):',E12.3)

C changed the following 6 lines A.S.
        WRITE(F5STR,332)NA
        WRITE(6,FMT=F5STR)'Fit parameters  a,   b,... =',(a(i),i=1,NA)
 332    FORMAT('(A28,',I5.5,'E13.4)')
        WRITE(F6STR,333)NA
        WRITE(6,FMT=F6STR)'Uncertainty  siga,sigb,... =',(sigmaa(i),i=1,NA)
 333    FORMAT('(A28,',I5.5,'E13.4)')

        dsave=d
        d=20.
        DO i=1,7
          d=d+i*10.
          CALL SETCOLOR(ibrown)
          CALL CVXY(xx,yy,5,iy,2)
          yy=dE(xx,A)
          CALL CVXY(xx,yy,ix,iy,1)
          IF(i.EQ.1)i0=iy/2
          ip=i0+i*15
          CALL KTRAS(5,iy,0)
          DO ix=6,nx-25
            CALL CVXY(xx,yy,ix,iy,2)
            yy=dE(xx,A)
            CALL CVXY(xx,yy,ix,iy,1)
            CALL KTRAS(ix,iy,1)
          ENDDO
          write(chd,344,err=398)d
 344      format(F8.1) 
          CALL PUTG(chd,8,1,1)
 398      CONTINUE
        ENDDO
        d=dsave
        CALL SETCOLOR(ired)
        CALL CVXY(xx,yy,5,iy,2)
        yy=dE(xx,A)
        CALL CVXY(xx,yy,ix,iy,1)
        ip=0.8*iy
        CALL KTRAS(5,iy,0)
        DO ix=6,nx-25
          CALL CVXY(xx,yy,ix,iy,2)
          yy=dE(xx,A)
          CALL CVXY(xx,yy,ix,iy,1)
          CALL KTRAS(ix,iy,1)
        ENDDO
        write(chd,345,err=399)d
 345    format(F8.1) 
        CALL PUTG(chd,8,1,1)
 399    CONTINUE
        CALL FINIG

        IF(typeinput.EQ.0.AND.ITYPE.GT.1)THEN
          DO i=0,8191
            Spec(i)=0
          ENDDO
          WRITE(6,*)' '
          WRITE(6,*)'A thickness spectrum of the dE-E matrix'
          WRITE(6,*)'will now automatically be written to file.'
          DO j=0,YDIM-1
            ydE=j
            DO i=0,XDIM-1
              xE=i
              id=range(xE+ydE,A,ifunc)-range(xE,A,ifunc)
              IF(id.GE.1.AND.id.LE.512)THEN
                Spec(id)=Spec(id)+rMAT(IDEST,i,j)
              ENDIF
            ENDDO
          ENDDO
          NAME='thick-'//CHR
          WRITE(6,348)NAME(1:9)
 348      FORMAT(' Type filename <',A,'>:',$)
          CALL READA(5,NAME)
          dim=512
          OPEN(20,FILE=NAME,ACCESS='SEQUENTIAL',IOSTAT=IOS)
          IF(IOS.NE.0)THEN
            WRITE(6,*) 'No file access'
            RETURN
          ENDIF
          COMMEN='Thickness spectrum using range for '//CHR(1:3)//' on silicon'
          CALL norw1dim(20,COMMEN,dim,Spec,Calib)
          CLOSE(20)
        ENDIF

       WRITE(6,*)' '
       WRITE(6,*)'The extracted range curve (this is not the red curve'
       WRITE(6,*)'seen in the window) will now be saved as file. You'
       WRITE(6,*)'may first give another calibration for the range'
       WRITE(6,*)'curve since the dE-E plot might have been compressed:'
        
 347    ar1=ab1
        WRITE(6,*)' '
        WRITE(6,346)ar1
 346    FORMAT('Type calibration a1 (keV/ch) of range curve',
     +         ' <',F5.2,'>:',$)
        CALL READF(5,ar1)
        IF(ar1.LT.2.OR.ar1.GT.1000)GO TO 347
        xxc=ab1/ar1
        A(1)=A(1)/xxc
        A(2)=A(2)/(xxc*xxc)
        A(3)=A(3)
        A(4)=A(4)*xxc

        WRITE(6,349)ar1
 349    FORMAT(/,'Fit parameters after going to a1= ',F8.2,' keV is:')
        WRITE(6,*)(a(i),i=1,NA)

C  Making the range curve spectrum
        DO i=0,8191
          Spec(i)=0
        ENDDO
        dim=2048
        DO i=0,dim-1
          xx=i+1
          Spec(i)=range(xx,A,ifunc)
        ENDDO

C Writing on file
        NAME='range-'//CHR
        WRITE(6,350)NAME(1:9)
 350    FORMAT('Type filename <',A,'>:',$)
        CALL READA(5,NAME)
        OPEN(20,FILE=NAME,ACCESS='SEQUENTIAL',IOSTAT=IOS)
        IF(IOS.NE.0)THEN
          WRITE(6,*) 'No file access'
          RETURN
        ENDIF
        COMMEN='Range curve for '//CHR(1:3)//' on silicon'
        CALL norw1dim(20,COMMEN,dim,Spec,Calib)
        CLOSE(20)
      ENDIF
 

C*********************************************************************
      IF(typefunc.EQ.4)THEN
        iOK = 0
        NA  = 2
        IF(Istatus.NE.0)GO TO 99
        IF(NA.GT.imax-idouble)THEN
          WRITE(6,*)'Number of parameters < independent data points'
          RETURN
        ENDIF

C Estimate parameters to be fitted
        IF(yl.LT.0)yl=0
        IF(yh.LT.0)yh=0
        A(1)=((0.5*alog((yh*xh*xh)/(yl*xl*xl)))**2)/(xh-2.*sqrt(xl*xh)+xl)
        A(2)=yl*xl*xl/exp(2.*sqrt(A(1)*xl))
        A(3)=yl

        WRITE(6,400)
 400    FORMAT(/'In order to start fit, start-values for the parameters',/,
     +         'have to be given. The default values are estimated from',/,
     +         'the y(x) values for the first (xl) and last (xh) point.',/,
     +         'You may start fitting with these values, or simply obtain',/,
     +         'the function with the given parameters (without fit).',/,
     +         ' ',/,
     +         'Your present fit-function looks like this (x=channels):',/,
     +         'Fermi(x) = A3 + A2 * EXP(2 * sqrt(A1*x)) / x*x')
 
 477    CONTINUE
        WRITE(6,401)  A(1)
 401    FORMAT(/'Give start value for A1 parameter  <',E11.4,'>:',$)
        CALL READF(5,A(1))
        IF(Istatus.NE.0)RETURN
        WRITE(6,402)  A(2)
 402    FORMAT( 'Give start value for A2 parameter  <',E11.4,'>:',$)
        CALL READF(5,A(2))
        IF(Istatus.NE.0)RETURN
        WRITE(6,403)  A(3)
 403    FORMAT( 'Give start value for A3 parameter  <',E11.4,'>:',$)
        CALL READF(5,A(3))
        IF(Istatus.NE.0)RETURN

 499    IF(iOK.EQ.0)GO TO 488           !no fit yet
        ians=1
        Istatus=0
        WRITE(6,*)' '
        WRITE(6,*)'Give type of Chi**2 weighting:'
        WRITE(6,*)'Weighting with 1/Yexp(i) ............(0)'
        WRITE(6,*)'Weighting with 1.0 for all Yexp(i)...(1)'
        WRITE(6,*)'Return to mama ......................(2)'
        WRITE(6,1)ians
        CALL READI(5,ians)
        IF(ians.EQ.2)RETURN
        IF(ians.LT.0.OR.ians.GT.2)GO TO 477
        IF(Istatus.NE.0)GO TO 477
        IF(ians.EQ.0)MODE=-1
        IF(ians.EQ.1)MODE=0
 496    PROG  =0.0001
        M=0
        CHISQR=1.0E+10
        bCH   =1.0E+10
        NBAD  =0

        SIGMAA(1)=A(1)/10.
        DELTAA(1)=A(1)/10.
        SIGMAA(2)=A(2)/10.
        DELTAA(2)=A(2)/10.
        SIGMAA(3)=A(3)/10.
        DELTAA(3)=A(3)/10.
        DO i=1,imax
          logy(i)=0.
          IF(y(i).GT.0)logy(i)=log(y(i))
        ENDDO
        IF(iOK.EQ.0)GO TO 488           !no fit yet
 423    CALL GRIDLS(x,logy,SIGMAY,imax,MODE,lFermi1,A,DELTAA,SIGMAA,NA,FIT,CHISQR)
        M=M+1
        CH(M)=CHISQR
        IF(M.LT.2) GO TO 423
        VER=ABS(CH(M)-CH(M-1))/CH(M-1)
        IF(CH(M).LT.bCH)THEN
          bCH=CH(M)               !saving best values
          DO i=1,NA
            bSIGMAA(i)=SIGMAA(i)
            bA(i)     =A(i)
          ENDDO
        ENDIF
        IF(CH(M).GT.CH(M-1))NBAD=NBAD+1
        IF(NBAD.GT.4.OR.M.GE.500)GO TO 424
        IF(VER.GT.PROG) GO TO 423
 424    CONTINUE

        DO i=1,NA                !replacing best values
          SIGMAA(i)=bSIGMAA(i)
          A(i)     =bA(i)
        ENDDO

C Presenting results, both numerical and graphical
        WRITE(6,420)                                            
 420    FORMAT(/'Pair no.       X            Y          FIT')
        DO i=1,imax
          WRITE(6,421)i,x(i),y(i),Ffermi1(x(i),A)
 421      FORMAT(I4,3E13.3)
        ENDDO

        ch1=0.000
        IF(imax.GT.NA)ch1=bCH
        WRITE(6,431)ch1
 431    FORMAT('Chisquare:',E12.3)

C changed the following 6 lines A.S.
        WRITE(F7STR,432)NA
        WRITE(6,FMT=F7STR)'Fit parameters  a,   b,... =',(a(i),i=NA,1,-1)
 432    FORMAT('(A28,',I5.5,'E12.4)')
        WRITE(F8STR,433)NA
        WRITE(6,FMT=F8STR)'Uncertainty  siga,sigb,... =',(sigmaa(i),i=NA,1,-1)
 433    FORMAT('(A28,',I5.5,'E12.4)')

 488    IF(NA.EQ.2)THEN
          CALL SETCOLOR(ired+iOK*10)
          CALL PUTG('FermiRho',8,1,1)
          a0(1)=a(2)
          a1(1)=a(1)
        ENDIF
        IF(NA.EQ.3)THEN
          CALL SETCOLOR(igreen-iOK*3)
          CALL PUTG('a+FermiRho',10,1,1)
          a0(2)=a(3)
          a1(2)=a(2)
          a2(2)=a(1)
        ENDIF

        CALL CVXY(xx,yy,1,iy,2)
        yy=fFermi1(xx,A)
        CALL CVXY(xx,yy,ix,iy,1)
        CALL KTRAS(1,iy,0)
        DO ix=2,nx-30
          CALL CVXY(xx,yy,ix,iy,2)
          yy=fFermi1(xx,A)
          CALL CVXY(xx,yy,ix,iy,1)
          CALL KTRAS(ix,iy,1)
        ENDDO
        IF(iy.GT.ny-20)iy=ny-(NA+1)*10
        IF(iy.LT.10)   iy=NA*10
        CALL KTRAS(ix-20,iy+10,0)
        CALL FINIG

        IF(NA.EQ.2.AND.imax-idouble.GE.3)THEN
          NA=3
          IF(A(3).EQ.0)A(3)=0.1
          GO TO 496     ! a new fit with one more parameter
        ENDIF
        
        IF(iOK.EQ.1)GO TO 443
        NA   = 2
        iOK  = 0        ! no fit
        ians = 0        ! start fitting
        WRITE(6,*)' '
        WRITE(6,*)'Start fitting........................(0)'
        WRITE(6,*)'Modify the start parameters..........(1)'
        WRITE(6,*)'The function looks OK, do not fit....(2)'
        WRITE(6,*)'Return to mama.......................(3)'
        WRITE(6,1)ians
        CALL READI(5,ians)
        IF(Istatus.NE.0)GO TO 477
        IF(ians.EQ.0)iOK=1
        IF(ians.EQ.1)GO TO 477
        IF(ians.EQ.2)iOK=0
        IF(ians.EQ.3)RETURN
        IF(ians.LT.0.OR.ians.GT.3)GO TO 477
        IF(iOK.EQ.1)GO TO 499
        IF(iOK.EQ.0)GO TO 444

 443    WRITE(6,440)
 440    FORMAT(/,'Final result:')
        WRITE(6,441)a0(1),a1(1)
 441    FORMAT('y=a(x^-2)e^2sqrt(bx)   : a=',E14.6,' b=',E14.6)
        WRITE(6,442)a0(2),a1(2),a2(2)
 442    FORMAT('y=a+b(x^-2)e^2sqrt(cx) : a=',E14.6,' b=',E14.6,' c=',E14.6)
 444    CALL WriteFit(fFermi1,typefunc,a0,a1,a2,a3)
      ENDIF


C*********************************************************************
      IF(typefunc.EQ.5)THEN
        iOK = 0
        NA  = 2
        IF(Istatus.NE.0)GO TO 99
        IF(NA.GT.imax-idouble)THEN
          WRITE(6,*)'Number of parameters < independent data points'
          RETURN
        ENDIF

C Estimate parameters to be fitted
        IF(yl.LT.0)yl=0
        IF(yh.LT.0)yh=0
        A(1)=( (0.5*alog((yl/yh)*((xl/xh)**(3./2.))))**2 )/(xl-2.*sqrt(xl*xh)+xh)
        A(2)=yl*(xl**(3./2.))/exp(2.*sqrt(A(1)*xl))
        A(3)=yl

        WRITE(6,500)
 500    FORMAT(/'In order to start fit, start-values for the parameters',/,
     +         'have to be given. The default values are estimated from',/,
     +         'the y(x) values for the first (xl) and last (xh) point.',/,
     +         'You may start fitting with these values, or simply obtain',/,
     +         'the function with the given parameters (without fit).',/,
     +         ' ',/,
     +         'Your present fit-function looks like this (x=channels):',/,
     +         'Fermi(x) = A3 + A2 * EXP(2 * sqrt(A1*x)) / x**3/2')
 
 577    CONTINUE
        WRITE(6,501)  A(1)
 501    FORMAT(/'Give start value for A1 parameter  <',E11.4,'>:',$)
        CALL READF(5,A(1))
        IF(Istatus.NE.0)RETURN
        WRITE(6,502)  A(2)
 502    FORMAT( 'Give start value for A2 parameter  <',E11.4,'>:',$)
        CALL READF(5,A(2))
        IF(Istatus.NE.0)RETURN
        WRITE(6,503)  A(3)
 503    FORMAT( 'Give start value for A3 parameter  <',E11.4,'>:',$)
        CALL READF(5,A(3))
        IF(Istatus.NE.0)RETURN

 599    IF(iOK.EQ.0)GO TO 588           !no fit yet
        ians=1
        Istatus=0
        WRITE(6,*)' '
        WRITE(6,*)'Give type of Chi**2 weighting:'
        WRITE(6,*)'Weighting with 1/Yexp(i) ............(0)'
        WRITE(6,*)'Weighting with 1.0 for all Yexp(i)...(1)'
        WRITE(6,*)'Return to mama ......................(2)'
        WRITE(6,1)ians
        CALL READI(5,ians)
        IF(ians.EQ.2)RETURN
        IF(ians.LT.0.OR.ians.GT.2)GO TO 577
        IF(Istatus.NE.0)GO TO 577
        IF(ians.EQ.0)MODE=-1
        IF(ians.EQ.1)MODE=0
 596    PROG  =0.0001
        M=0
        CHISQR=1.0E+10
        bCH   =1.0E+10
        NBAD  =0

        SIGMAA(1)=A(1)/10.
        DELTAA(1)=A(1)/10.
        SIGMAA(2)=A(2)/10.
        DELTAA(2)=A(2)/10.
        SIGMAA(3)=A(3)/10.
        DELTAA(3)=A(3)/10.
        DO i=1,imax
          logy(i)=0.
          IF(y(i).GT.0)logy(i)=log(y(i))
        ENDDO
        IF(iOK.EQ.0)GO TO 588           !no fit yet
 523    CALL GRIDLS(x,logy,SIGMAY,imax,MODE,lFermi2,A,DELTAA,SIGMAA,NA,FIT,CHISQR)
        M=M+1
        CH(M)=CHISQR
        IF(M.LT.2) GO TO 523
        VER=ABS(CH(M)-CH(M-1))/CH(M-1)
        IF(CH(M).LT.bCH)THEN
          bCH=CH(M)               !saving best values
          DO i=1,NA
            bSIGMAA(i)=SIGMAA(i)
            bA(i)     =A(i)
          ENDDO
        ENDIF
        IF(CH(M).GT.CH(M-1))NBAD=NBAD+1
        IF(NBAD.GT.4.OR.M.GE.500)GO TO 524
        IF(VER.GT.PROG) GO TO 523
 524    CONTINUE

        DO i=1,NA                !replacing best values
          SIGMAA(i)=bSIGMAA(i)
          A(i)     =bA(i)
        ENDDO

C Presenting results, both numerical and graphical
        WRITE(6,520)                                            
 520    FORMAT(/'Pair no.       X            Y          FIT')
        DO i=1,imax
          WRITE(6,521)i,x(i),y(i),fFermi2(x(i),A)
 521      FORMAT(I4,3E13.3)
        ENDDO

        ch1=0.000
        IF(imax.GT.NA)ch1=bCH
        WRITE(6,531)ch1
 531    FORMAT('Chisquare:',E12.3)

C changed the following 6 lines A.S.
        WRITE(F9STR,532)NA
        WRITE(6,FMT=F9STR)'Fit parameters  a,   b,... =',(a(i),i=NA,1,-1)
 532    FORMAT('(A28,',I5.5,'E12.4)')
        WRITE(F0STR,533)NA
        WRITE(6,FMT=F0STR)'Uncertainty  siga,sigb,... =',(sigmaa(i),i=NA,1,-1)
 533    FORMAT('(A28,',I5.5,'E12.4)')

 588    IF(NA.EQ.2)THEN
          CALL SETCOLOR(ired+iOK*10)
          CALL PUTG('FermiRho',8,1,1)
          a0(1)=a(2)
          a1(1)=a(1)
        ENDIF
        IF(NA.EQ.3)THEN
          CALL SETCOLOR(igreen-iOK*3)
          CALL PUTG('a+FermiRho',10,1,1)
          a0(2)=a(3)
          a1(2)=a(2)
          a2(2)=a(1)
        ENDIF

        CALL CVXY(xx,yy,1,iy,2)
        yy=fFermi2(xx,A)
        CALL CVXY(xx,yy,ix,iy,1)
        CALL KTRAS(1,iy,0)
        DO ix=2,nx-30
          CALL CVXY(xx,yy,ix,iy,2)
          yy=fFermi2(xx,A)
          CALL CVXY(xx,yy,ix,iy,1)
          CALL KTRAS(ix,iy,1)
        ENDDO
        IF(iy.GT.ny-20)iy=ny-(NA+1)*10
        IF(iy.LT.10)   iy=NA*10
        CALL KTRAS(ix-20,iy+10,0)
        CALL FINIG

        IF(NA.EQ.2.AND.imax-idouble.GE.3)THEN
          NA=3
          IF(A(3).EQ.0)A(3)=0.1
          GO TO 596     ! a new fit with one more parameter
        ENDIF
        
        IF(iOK.EQ.1)GO TO 543
        NA   = 2
        iOK  = 0        ! no fit
        ians = 0        ! start fitting
        WRITE(6,*)' '
        WRITE(6,*)'Start fitting........................(0)'
        WRITE(6,*)'Modify the start parameters..........(1)'
        WRITE(6,*)'The function looks OK, do not fit....(2)'
        WRITE(6,*)'Return to mama.......................(3)'
        WRITE(6,1)ians
        CALL READI(5,ians)
        IF(Istatus.NE.0)GO TO 577
        IF(ians.EQ.0)iOK=1
        IF(ians.EQ.1)GO TO 577
        IF(ians.EQ.2)iOK=0
        IF(ians.EQ.3)RETURN
        IF(ians.LT.0.OR.ians.GT.3)GO TO 577
        IF(iOK.EQ.1)GO TO 599
        IF(iOK.EQ.0)GO TO 544

 543    WRITE(6,540)
 540    FORMAT(/,'Final result:')
        WRITE(6,541)a0(1),a1(1)
 541    FORMAT('y=a(x^-3/2)e^2sqrt(bx)   : a=',E14.6,' b=',E14.6)
        WRITE(6,542)a0(2),a1(2),a2(2)
 542    FORMAT('y=a+b(x^-3/2)e^2sqrt(cx) : a=',E14.6,' b=',E14.6,' c=',E14.6)
 544    CALL WriteFit(fFermi2,typefunc,a0,a1,a2,a3)
      ENDIF



      RETURN
      END


      SUBROUTINE WriteFit(func,typefunc,a0,a1,a2,a3)
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      CHARACTER fname*8,comm*60

      CHARACTER COMMEN*60, ANS*1, NAME*80, FILNAM*80
      DIMENSION A(5), Spec(0:8191)
      DIMENSION a0(0:3),a1(0:3),a2(0:3),a3(0:3), Calib(6)
      INTEGER dim, typefunc
      COMMON/commonfit/NA


      DO i = 0,8191
        Spec(i) = 0.
      ENDDO
      DO i = 1,5
        A(i) = 0.
      ENDDO

      ANS='y'
      WRITE(6,1)ANS
 1    FORMAT(/'Save your fit-spectrum (y/n) <',A1,'>:',$)
      CALL READA1(5,ANS)
      IF(ANS.EQ.'N'.OR.ANS.EQ.'n') RETURN
      IF(Istatus.NE.0)RETURN

      NA = 2
      WRITE(6,2)NA
 2    FORMAT( 'How many free parameters     <',I1,'>:',$)
      CALL READI(5,NA)

      IF(typefunc.EQ.0)THEN
        IF(NA.LT.1.OR.NA.GT.4)THEN
          WRITE(6,*)'Sorry, you must have: 0 < number of parameters < 5'
          Istatus = 1
        ENDIF
      ENDIF
      IF(typefunc.EQ.1)THEN
        IF(NA.LT.1.OR.NA.GT.2)THEN
          WRITE(6,*)'Sorry, you must have: 0 < number of parameters < 3'
          Istatus = 1
        ENDIF
      ENDIF
      IF(typefunc.EQ.2)THEN
        IF(NA.LT.2.OR.NA.GT.3)THEN
          WRITE(6,*)'Sorry, you must have: 1 < number of parameters < 4'
          Istatus = 1
        ENDIF
      ENDIF
      IF(typefunc.EQ.4)THEN
        IF(NA.LT.2.OR.NA.GT.3)THEN
          WRITE(6,*)'Sorry, you must have: 1 < number of parameters < 4'
          Istatus = 1
        ENDIF
      ENDIF
      IF(typefunc.EQ.5)THEN
        IF(NA.LT.2.OR.NA.GT.3)THEN
          WRITE(6,*)'Sorry, you must have: 1 < number of parameters < 4'
          Istatus = 1
        ENDIF
      ENDIF

      IF(NA.GT.4.OR.NA.LT.1.OR.Istatus.NE.0)RETURN

      IF(NA.EQ.1)THEN
        A(1)=a0(0)
        A(2)=0
        A(3)=0
        A(4)=0
      ENDIF
      IF(NA.EQ.2)THEN
        A(1)=a1(1)
        A(2)=a0(1)
        A(3)=0
        A(4)=0
      ENDIF      
      IF(NA.EQ.3)THEN
        A(1)=a2(2)
        A(2)=a1(2)
        A(3)=a0(2)
        A(4)=0
      ENDIF
      IF(NA.EQ.4)THEN
        A(1)=a3(3)
        A(2)=a2(3)
        A(3)=a1(3)
        A(4)=a0(3)
      ENDIF

      IF(Istatus.NE.0)RETURN

      WRITE(6,23)  cal(2,IDEST,1,1)
      CALL READF(5,cal(2,IDEST,1,1))
      IF(Istatus.NE.0)RETURN
      WRITE(6,24)  cal(2,IDEST,1,2)
      CALL READF(5,cal(2,IDEST,1,2))
      IF(Istatus.NE.0)RETURN
      WRITE(6,25)  cal(2,IDEST,1,3)
      CALL READF(5,cal(2,IDEST,1,3))
 23   FORMAT(/'Cal. coeff. a0 (keV) on x-axis     <',F11.1,'>:',$)
 24   FORMAT( 'Cal. coeff. a1 (keV/ch) on x-axis  <',F11.3,'>:',$)
 25   FORMAT( 'Cal. coeff. a2 (keV/ch2) on x-axis <',E11.4,'>:',$)
      IF(Istatus.NE.0)RETURN

      m=0
      DO i=1,1
        DO j=1,3
          m=m+1
          Calib(m)=cal(2,IDEST,i,j)
        ENDDO
      ENDDO
      iCal=m

      LIN=MAX(64,MAXCH+1)
      WRITE(6,4)LIN
    4 FORMAT( 'Length of output-spectrum                 <',I4,'>:',$)
      CALL READI(5,LIN) 
      IF(Istatus.NE.0)RETURN
      dim=LIN

C  Making the fit curve spectrum
      DO i = 0,dim-1
        xx = i
        Spec(i)=func(xx,A)
      ENDDO

C Writing the file
      NAME='SPEC'
      WRITE(6,5)NAME(1:4)
  5   FORMAT( 'Filename                                  <',A,'>:',$)
      CALL READA(5,NAME)
      IF(Istatus.NE.0)RETURN
      CALL LENGDE(NAME,LIN)
      FILNAM=NAME(1:LIN) 
      OPEN(20,FILE=FILNAM,ACCESS='SEQUENTIAL',IOSTAT=IOS)
      IF(IOS.NE.0)THEN
        WRITE(6,*) 'No file access'
        RETURN
      ENDIF
      COMMEN='Fit-spectrum obtained with MAMA FD-command'
      CALL norw1dim(20,COMMEN,dim,Spec,Calib)
      CLOSE(20)

      END


      SUBROUTINE DSfunc(x,y,n,x1,x2,yl,yh,type)
      Integer type
      REAL x(1:200),y(1:200)
      CHARACTER*28 HEADING
C y      -vector containing the Y data points
C x      -vector containing the X data points
C n      -number of points
C x1,x2  -lower and higher limits for x-axis, if=0's, old display
C yl,yh  -lower and higher limits for y-axis, if=0's, old display
C type   -determines type of drawing:
C         = 0 draw line between points (theory) 
C         = 1 draw crosses for points (experiment)

      iblack=20
      CALL SETCOLOR(iblack)
      CALL INITG(NX,NY)
      IYAXIS=1
      IF(abs(x1)+abs(x2)+abs(yl)+abs(yh).GT.0)THEN ! New display
        CALL ERASE
        CALL LIMG(NX,0,NY,0)
        X0=x1
        DX=x2-x1
        Y0=yl
        DY=yh-yl
        CALL TRAX(DX,X0,DY,Y0,IYAXIS)  ! Draw axis
        CALL MSPOT(NX-4,-2)            ! Units on x-axis
        CALL PUTG('x',1,8,1)
        CALL MSPOT(7,NY+3)             ! Units on y-axis
        CALL PUTG('y',1,2,1)
        CALL DATETIME(HEADING)
        CALL MSPOT(NX/2,NY-20)
        CALL PUTG(HEADING,11,8,1)
        DISP = .TRUE.
      ENDIF

      IF(type.EQ.0)THEN
        CALL PSPOT(x(1),y(1))
        DO i=2,n
          CALL VECT(x(i),y(i))
        ENDDO
      ELSE
        DO i=1,n
          CALL SYMBG(5,x(i),y(i),8,0.)   ! 5 = cross
        ENDDO                            ! 8 = size in pixels
      ENDIF

      CALL FINIG
      RETURN
      END


      FUNCTION dE(E,A)
      COMMON /THICK/d,ifunc
      DIMENSION A(5)
      INTEGER try
C Solving dE from the implicite equation d=R(E+dE)-R(E), where
C d is the thickness of the front detector. Making search by 
C Newton's method (I think), which is only working for
C monotonically functions. New estimate made by interpolation.
      try=0
      IF(E.LT.100)THEN    !Initializing
        x1=5.*(E+50.)
        x2=2.*x1
      ELSE
        x1=E/10               
        x2=2.*x1
      ENDIF

      rE =range(E,A,ifunc)
      dE =x2

101   y1 =range(E+x1,A,ifunc)-rE
      y2 =range(E+x2,A,ifunc)-rE
      IF(ABS(y1-y2).LT.0.001)GO TO 100
      
99    try=try+1
      x0 =(d-y1)*(x2-x1)/(y2-y1)+x1   !interpolating new guess
      IF(x0.LT.1.AND.try.LT.50)THEN   !desperate new guess
        x1=200
        x2=400
        dE=x2
        GO TO  101
      ENDIF

      y0  =range(E+x0,A,ifunc)-rE
  
      IF(ABS(y1-d).GT.ABS(y2-d))THEN  !storing best pair of values
        x1=x2
        y1=y2
      ENDIF
      x2=x0
      y2=y0
      IF(ABS(y2-d).GT.0.1.AND.try.LT.50)GO TO 99  ! 1/10 ch. accuracy

100   dE=x2

      RETURN
      END

      FUNCTION range(x,A,ifunc)
      DIMENSION A(5)
      range=0.
      IF(ABS(x+A(4)).LT.0.0000001)RETURN
      IF(ifunc.EQ.1)range=x*(A(1)+A(2)*x)+A(3)*x/(x+A(4))
      IF(ifunc.EQ.2)range=A(1)*(x** A(2))
      IF(ifunc.EQ.3)range=A(1)*(x**(A(2)+A(3)*log(x)))
      IF(ifunc.EQ.4)range=exp(A(1)+A(2)*log(x)+A(3)*log(x)*log(x))
      RETURN
      END

      FUNCTION fPol(x,A)
      DIMENSION A(5)
      COMMON/commonfit/NA
C Sorry, for this stupid A(i) assignments, but too much work to change
C all other functions accordingly
      fPol=0.
      IF(NA.EQ.1)fPol=A(1)
      IF(NA.EQ.2)fPol=A(2)+A(1)*x
      IF(NA.EQ.3)fPol=A(3)+A(2)*x+A(1)*x*x
      IF(NA.EQ.4)fPol=A(4)+A(3)*x+A(2)*x*x+A(1)*x*x*x
      IF(NA.EQ.5)fPol=A(5)+A(4)*x+A(3)*x*x+A(2)*x*x*x+A(1)*x*x*x*x
      RETURN
      END

      FUNCTION fhyp(x,A)
      DIMENSION A(5)
      COMMON/commonfit/NA
      fhyp =0.
      IF(x.EQ.0)RETURN
      A2   =0.
      IF(NA.GE.2)A2=A(2)
      fhyp=A2+(A(1)/x)
      RETURN
      END

      FUNCTION fexp(x,A)
      DIMENSION A(5)
      COMMON/commonfit/NA
      fexp = 0.
      A3   = 0.
      IF(NA.GE.3)A3=A(3)
      z = A(1)*x
      IF(z.GT.88.)z=88.       ! we have single precision E+/-38
      IF(z.GT.-88.)THEN      
         fexp=A3+(A(2)*EXP(z))
      ENDIF
      RETURN
      END

      FUNCTION lexp(x,A)
      DIMENSION A(5)
      COMMON/commonfit/NA
      REAL lexp
      lexp = 0.
      xexp = 0.
      A3   = 0.
      IF(NA.GE.3)A3=A(3)
      z = A(1)*x
      IF(z.GT.88.)z=88.       ! we have single precision E+/-38
      IF(z.GT.-88.)THEN      
         xexp=A3+(A(2)*EXP(z))
      ENDIF
      IF(xexp.GT.0)THEN
        lexp=log(xexp)
      ELSE
        lexp=0.
      ENDIF    
      RETURN
      END

      FUNCTION fFermi1(x,A)
      DIMENSION A(5)
      COMMON/commonfit/NA
      fFermi1 = 0.
      A3      = 0.
      IF(NA.GE.3)A3=A(3)
      y=A(1)*x
      IF(y.GT.0.)THEN
        z=2.0*SQRT(y)
        IF(z.GT.88.)z=88.       ! we have single precision E+/-38
        IF(z.GT.-88.)THEN      
          fFermi1=A3+(A(2)*EXP(z)/(x*x))
        ENDIF
      ENDIF
      RETURN
      END

      FUNCTION lFermi1(x,A)
      DIMENSION A(5)
      COMMON/commonfit/NA
      REAL lFermi1
      lFermi1 = 0.
      Fermi1  = 0.
      A3      = 0.
      IF(NA.GE.3)A3=A(3)
      y=A(1)*x
      IF(y.GT.0.)THEN
        z=2.0*SQRT(y)
        IF(z.GT.88.)z=88.       ! we have single precision E+/-38
        IF(z.GT.-88.)THEN      
          Fermi1=A3+(A(2)*EXP(z)/(x*x))
        ENDIF
      ENDIF
      IF(Fermi1.GT.0)THEN
        lFermi1=log(Fermi1)
      ELSE
        lFermi1=0.
      ENDIF    
      RETURN
      END

      FUNCTION fFermi2(x,A)
      DIMENSION A(5)
      COMMON/commonfit/NA
      fFermi2 = 0.
      A3      = 0.
      IF(NA.GE.3)A3=A(3)
      y=A(1)*x
      IF(y.GT.0.)THEN
        z=2.0*SQRT(y)
        IF(z.GT.88.)z=88.       ! we have single precision E+/-38
        IF(z.GT.-88.)THEN      
          fFermi2=A3+(A(2)*EXP(z)/(x**(3./2.)))
        ENDIF
      ENDIF
      END
  
      FUNCTION lFermi2(x,A)
      DIMENSION A(5)
      COMMON/commonfit/NA
      REAL lFermi2
      lFermi2 = 0.
      Fermi2  = 0.
      A3      = 0.
      IF(NA.GE.3)A3=A(3)
      y=A(1)*x
      IF(y.GT.0.)THEN
        z=2.0*SQRT(y)
        IF(z.GT.88.)z=88.       ! we have single precision E+/-38
        IF(z.GT.-88.)THEN      
          Fermi2=A3+(A(2)*EXP(z)/(x**(3./2.)))
        ENDIF
      ENDIF
      IF(Fermi2.GT.0)THEN
        lFermi2=log(Fermi2)
      ELSE
        lFermi2=0.
      ENDIF      
      RETURN
      END
