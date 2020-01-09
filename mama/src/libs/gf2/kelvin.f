      SUBROUTINE ARITHMETIC
      INTEGER XDIM,YDIM
      CHARACTER*40 ANS
      CHARACTER answ*1
      CHARACTER CH(2)*1
      CHARACTER APP*4
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,xcomm*60
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      DIMENSION dydx(0:8191),yy(0:8191),xx(0:8191)
      CHARACTER func*3

C patched by Andreas Schiller, September 10 2003
C reason: make more standard repetitive format

C Added the following variable A.S.
      CHARACTER*8 F1STR
      imax=10
      jmax=10

      WRITE(6,1)
 1    FORMAT(/'Write your expression:            ',/,
     +        'adding spectra               1=1+2', /,      
     +        'adding constant              2=1+120.0',/,   
     +        'subtracting spectra          2=1-2',/,       
     +        'subtract constant            2=1-120.0',/,   
     +        'multiply spectra             2=1*2',/,       
     +        'multiply by constant         1=2*1.17',/,    
     +        'divide spectra               2=1/2',/,       
     +        'divide by constant           2=2/1.17',/,
     +        'multiply spectrum*matrix     1=1x1',/,
     +        'natural logarithm            2=2log',/,
     +        'derivate                     2=2der',/,
     +        'NOTE: Integer means spectra and real means constant')

C Reading in the answer as a string
      CALL CASK('Type your expression:',ANS,NC)
      IF(NC.LT.5)GO TO 99
         

C Scanning through the string and removing blanks
      NTOP=NC
      DO i=1,NC
        IF(ANS(i:i).EQ.' ')THEN
          NTOP=NTOP-1
          DO j=i,NTOP
            ANS(j:j)=ANS(j+1:j+1)          
          ENDDO
        ENDIF
      ENDDO
      DO i=NTOP+1,NC       
        ANS(i:i)=' '
      ENDDO
   
C Assumes manipulation only with spectra (no constants) (NTOP=5)
      CH(1) = ' '
      CH(2) = ' '
      func  = ' '
      I1    = 0
      I2    = 0
      IF(NTOP.EQ.5)THEN 
        READ(ANS(1:5),10,ERR=99)IDEST,CH(1),I1,CH(2),I2
 10     FORMAT(I1,A1,I1,A1,I1)
        IF(CH(1).NE.'=')GO TO 99
        IF(I2.GT.2.OR.I2.LT.1)GO TO 11 !It looks that you forgot
                                       !real for constant - interprets
                                       !as real and go to 11
        IF(IDEST.LT.1.OR.IDEST.GT.2.OR.
     +        I1.LT.1.OR.   I1.GT.2.OR.
     +        I2.LT.1.OR.   I2.GT.2)GO TO 99

C Adding
        IF(CH(2).EQ.'+')THEN
          iChange=1
          IF(ITYPE.GT.1)THEN
            DO j=0,2047 
              DO i=0,4095
                rMAT(IDEST,i,j)=rMAT(I1,i,j)+rMAT(I2,i,j)
                IF(rMAT(IDEST,i,j).NE.0.AND.j.GT.jmax)jmax=j
                IF(rMAT(IDEST,i,j).NE.0.AND.i.GT.imax)imax=i
              ENDDO
            ENDDO
          ELSE
            DO i=0,8191
              rSPEC(IDEST,i)=rSPEC(I1,i)+rSPEC(I2,i)
              IF(rSPEC(IDEST,i).NE.0.AND.i.GT.imax)imax=i
            ENDDO
          ENDIF
        ENDIF

C Subtracting
        IF(CH(2).EQ.'-')THEN
          iChange=1
          IF(ITYPE.GT.1)THEN
            DO j=0,2047 
              DO i=0,4095
                rMAT(IDEST,i,j)=rMAT(I1,i,j)-rMAT(I2,i,j)         
                IF(rMAT(IDEST,i,j).NE.0.AND.j.GT.jmax)jmax=j
                IF(rMAT(IDEST,i,j).NE.0.AND.i.GT.imax)imax=i
              ENDDO
            ENDDO
          ELSE
            DO i=0,8191
              rSPEC(IDEST,i)=rSPEC(I1,i)-rSPEC(I2,i)
              IF(rSPEC(IDEST,i).NE.0.AND.i.GT.imax)imax=i
            ENDDO
          ENDIF
        ENDIF

C Multiplying
        IF(CH(2).EQ.'*')THEN
          iChange=1
          IF(ITYPE.GT.1)THEN
            DO j=0,2047 
              DO i=0,4095
                rMAT(IDEST,i,j)=rMAT(I1,i,j)*rMAT(I2,i,j)
                IF(rMAT(IDEST,i,j).NE.0.AND.j.GT.jmax)jmax=j
                IF(rMAT(IDEST,i,j).NE.0.AND.i.GT.imax)imax=i
              ENDDO
            ENDDO
          ELSE
            DO i=0,8191
              rSPEC(IDEST,i)=rSPEC(I1,i)*rSPEC(I2,i)
              IF(rSPEC(IDEST,i).NE.0.AND.i.GT.imax)imax=i
            ENDDO
          ENDIF
        ENDIF

C Multiplying matrix with spectrum
        IF(CH(2).EQ.'x')THEN
          iChange=1
          answ='x'
          WRITE(6,3)answ
   3      FORMAT('Multiply along x or y-axis        <',A1,'>:',$)
          CALL READA1(5,answ)
          IF(Istatus.NE.0)RETURN

          WRITE(6,4)I1
   4      FORMAT('Source single spectrum            <',I1,'>:',$)
          CALL READI(5,I1)
          IF(Istatus.NE.0)RETURN

          WRITE(6,5)I2
   5      FORMAT('Source matrix                     <',I1,'>:',$)
          CALL READI(5,I2)
          IF(Istatus.NE.0)RETURN
          ITYPE=3
          IF(answ.EQ.'x'.OR.answ.EQ.'X')THEN
            DO j=0,2047 
              DO i=0,4095
                rMAT(IDEST,i,j)=rSPEC(I1,i)*rMAT(I2,i,j)
              ENDDO
            ENDDO
          ENDIF
          IF(answ.EQ.'y'.OR.answ.EQ.'Y')THEN
            DO j=0,2047 
              DO i=0,4095
                rMAT(IDEST,i,j)=rSPEC(I1,j)*rMAT(I2,i,j)
              ENDDO
            ENDDO
          ENDIF
        ENDIF

C Dividing
        IF(CH(2).EQ.'/')THEN
          iChange=1
          IF(ITYPE.GT.1)THEN
            DO j=0,2047 
              DO i=0,4095
                x=rMAT(I2,i,j)
                IF(ABS(x).GT.0.00000001)THEN
                  x=rMAT(I1,i,j)/x
                  rMAT(IDEST,i,j)=x
                  IF(x.NE.0.AND.j.GT.jmax)jmax=j
                  IF(x.NE.0.AND.i.GT.imax)imax=i
                ELSE
                  rMAT(IDEST,i,j)=0
                ENDIF
              ENDDO
            ENDDO
          ELSE
            DO i=0,8191
              x=rSPEC(I2,i)
              IF(ABS(x).GT.0.00000001)THEN
                x=rSPEC(I1,i)/x
                rSPEC(IDEST,i)=x
                IF(x.NE.0.AND.i.GT.imax)imax=i
              ELSE
                rSPEC(IDEST,i)=0
              ENDIF
            ENDDO
          ENDIF
        ENDIF
        IF(iChange.EQ.0)GO TO 99 
      ENDIF


C Assumes manipulation with constants (NTOP>5)
11    IF(NTOP.GT.5.OR.I2.GT.2.OR.I2.LT.1)THEN 
        READ(ANS(1:4),12,ERR=99)IDEST,CH(1),I1,CH(2)
12      FORMAT(I1,A1,I1,A1)
        READ(ANS(5:NTOP),*,ERR=97)const
97      READ(ANS(4:6),*,ERR=99)func !It was no constant, but maybe a function
        IF(CH(1).NE.'=')GO TO 99
        IF(IDEST.LT.1.OR.IDEST.GT.2.OR.
     +        I1.LT.1.OR.   I1.GT.2)GO TO 99

C Adding
        IF(CH(2).EQ.'+')THEN
          iChange=1
          IF(ITYPE.GT.1)THEN
            DO j=0,2047 
              DO i=0,4095
                x=rMAT(I1,i,j)+const
                rMAT(IDEST,i,j)=x
              ENDDO
            ENDDO
          ELSE
            DO i=0,8191
              x=rSPEC(I1,i)+const
              rSPEC(IDEST,i)=x
            ENDDO
          ENDIF
        ENDIF

C Subtracting
        IF(CH(2).EQ.'-')THEN
          iChange=1
          IF(ITYPE.GT.1)THEN
            DO j=0,2047 
              DO i=0,4095
                x=rMAT(I1,i,j)-const
                rMAT(IDEST,i,j)=x
              ENDDO
            ENDDO
          ELSE
            DO i=0,8191
              x=rSPEC(I1,i)-const
              IF(x.NE.0)imax=i
              rSPEC(IDEST,i)=x
            ENDDO
          ENDIF
        ENDIF 

C Multiplying with constant
        IF(CH(2).EQ.'*')THEN
          iChange=1
          IF(ITYPE.GT.1)THEN
            DO j=0,2047
              DO i=0,4095
                x=rMAT(I1,i,j)*const
                rMAT(IDEST,i,j)=x
              ENDDO
            ENDDO
          ELSE
            DO i=0,8191
              x=rSPEC(I1,i)*const
              rSPEC(IDEST,i)=x
            ENDDO
          ENDIF  
        ENDIF

C Dividing with constant
        IF(CH(2).EQ.'/')THEN
          iChange=1
          IF(ITYPE.GT.1)THEN
            DO j=0,2047
              DO i=0,4095
                IF(ABS(const).GT.0.00000001)THEN
                  x=rMAT(I1,i,j)/const
                  rMAT(IDEST,i,j)=x
                ELSE
                  rMAT(IDEST,i,j)=0
                ENDIF
              ENDDO
            ENDDO
          ELSE
            DO i=0,8191
              IF(ABS(const).GT.0.00000001)THEN
                x=rSPEC(I1,i)/const
                rSPEC(IDEST,i)=x
              ELSE
                rSPEC(IDEST,i)=0
              ENDIF
            ENDDO
          ENDIF   
        ENDIF
      ENDIF

C Section with functions applied to one spectrum/matrix
C Natural logarithm
      IF(func.EQ.'log'.OR.func.EQ.'LOG')THEN
        iChange=1
        IF(ITYPE.GT.1)THEN
          DO j=0,2047
            DO i=0,4095
              y=rMAT(I1,i,j)
              IF(y.GT.0)THEN
                x=log(y)
                rMAT(IDEST,i,j)=x
              ELSE
                rMAT(IDEST,i,j)=0
              ENDIF
            ENDDO
          ENDDO
        ELSE
          DO i=0,8191
            y=rSPEC(I1,i)
            IF(y.GT.0)THEN
              x=log(y)
              rSPEC(IDEST,i)=x
            ELSE
              rSPEC(IDEST,i)=0
            ENDIF
          ENDDO
        ENDIF  
      ENDIF

C Derivate spectrum
      IF(func.EQ.'der'.OR.func.EQ.'DER')THEN
        iChange=1
        imax=2               !number of datapoints in fit for derivate
C Asking for the method to be applied
        WRITE(6,*)' '
        WRITE(6,*)'The algorithm steps through channel by channel'
        WRITE(6,*)'and calculates the derivate at that channel. '
        WRITE(6,*)'You can choose between the following methods: '
        WRITE(6,*)'---------------------------------------------------- '
        WRITE(6,*)'dy(i)/dx = (y(i) - y(i-1)) / (x(i) - x(i-1))     (1)'
        WRITE(6,*)'dy(i)/dx = (y(i+1) - y(i)) / (x(i+1) - x(i))     (2)'
        WRITE(6,*)'dy(i)/dx = (y(i+1)-y(i-1)) / (x(i+1)-x(i-1))     (3)'
        WRITE(6,*)'A least square fit of the linear or quadratic '
        WRITE(6,*)'function y(i) = b0 + b1*x(i) + b2*x(i)*x(i) is'
        WRITE(6,*)'performed for channels around the center-channel'
        WRITE(6,*)'giving dy(i)/dx = b1 + 2*b2*x(i).                (4)'
        WRITE(6,*)'---------------------------------------------------- '
        mode=4
        WRITE(6,20)mode
 20     FORMAT('Choose your method                                <',I1,'>:',$)
        CALL READI(5,mode)
        WRITE(6,*) ' '
        IF(Istatus.NE.0.OR.mode.LT.1.or.mode.GT.4)THEN
          WRITE(6,*) 'Illegal parameter, valid region:'
          WRITE(6,*) '0 < method < 5'
          RETURN
        ENDIF

        IF(mode.EQ.4)THEN
          iPol=1
          imax=4
          IF(imax.LT.3)imax=3
          WRITE(6,22)iPol
 22       FORMAT('Degree of polynom (linear = 1, quadratic = 2)     <',I1,'>:',$)
          CALL READI(5,iPol)
          imax=MAX(imax,iPol+1)
          WRITE(6,24)imax
 24       FORMAT('Number of datapoints                             <',I2,'>:',$)
          CALL READI(5,imax)
          IF(Istatus.NE.0.OR.iPol.LT.1.or.iPol.GT.2.
     +    OR.imax.GT.100.OR.imax.LT.2.OR.iPol.GE.imax)THEN
            WRITE(6,*) 'Illegal parameters, valid region:' 
            WRITE(6,*) '0 < polynom <   3'
            WRITE(6,*) '1 < points  < 101'
            WRITE(6,*) 'polynom < points'
            RETURN
          ENDIF
          IF(iPol.EQ.1)mode=4
          IF(iPol.EQ.2)mode=5
        ENDIF

        iDer=1
        WRITE(6,26)iDer
 26     FORMAT('Derivate with respect to channels (dx=dch) (1)',/,
     +         'or energies (dx=dE(keV)) - if calibrated   (2)    <',I1,'>:',$)
        CALL READI(5,iDer)

        IF(ITYPE.GT.1)THEN
          answ='x'
          WRITE(6,28)answ
 28       FORMAT('Derivate along x or y-axis                        <',A1,'>:',$)
          CALL READA1(5,answ)
          IF(Istatus.NE.0)RETURN
          IF(answ.EQ.'x'.OR.answ.EQ.'X')THEN
            DO i=0,4095
              xx(i)=i
              IF(iDer.EQ.2)xx(i)=cal(1,I1,1,1)+cal(1,I1,1,2)*FLOAT(i)+ cal(1,I1,1,3)*FLOAT(i)*FLOAT(i)
            ENDDO
            DO j=0,2047 
              JT=(j/50)*50
              IF(JT.EQ.j)THEN
                write(6,FMT='(A1,$)')'.'
                call flush(6)
              ENDIF
              DO i=0,4095
                yy(i)=rMAT(I1,i,j)
              ENDDO
              CALL Derivate(dydx,yy,xx,4096,mode,imax)
              DO i=0,4095
                x=dydx(i)
                rMAT(IDEST,i,j)=x
              ENDDO
            ENDDO
          ENDIF
          IF(answ.EQ.'y'.OR.answ.EQ.'Y')THEN
            DO j=0,2047
              xx(j)=j
              IF(iDer.EQ.2)xx(j)=cal(1,I1,2,1)+cal(1,I1,2,2)*FLOAT(j)+ cal(1,I1,2,3)*FLOAT(j)*FLOAT(j)
            ENDDO
            DO i=0,4095 
              IT=(i/400)*400
              IF(IT.EQ.i)THEN
                write(6,FMT='(A1,$)')'.'
                call flush(6)
              ENDIF
              DO j=0,2047
                yy(j)=rMAT(I1,i,j)
              ENDDO
              CALL Derivate(dydx,yy,xx,2048,mode,imax)
              DO j=0,2047
                x=dydx(j)
                rMAT(IDEST,i,j)=x
              ENDDO
            ENDDO
          ENDIF
          WRITE(6,*)' '
        ELSE
          DO i=0,8191
            xx(i)=i
            IF(iDer.EQ.2)xx(i)=cal(2,I1,1,1)+cal(2,I1,1,2)*FLOAT(i)+ cal(2,I1,1,3)*FLOAT(i)*FLOAT(i)
          ENDDO
          DO i=0,8191
            yy(i)=rSPEC(I1,i)
          ENDDO
          CALL Derivate(dydx,yy,xx,8192,mode,imax)
          DO i=0,8191
            x=dydx(i)
            rSPEC(IDEST,i)=x
          ENDDO
        ENDIF  
        IF(iChange.EQ.0)GO TO 99 
      ENDIF

C Checking if consistent calibration and dimensions  
      IF(NTOP.EQ.5)THEN                                  !case:matrices/spectra
        IF(ITYPE.GT.1)THEN 
          Itesting=IDEST+I1+I2
          IF(Itesting.NE.3.OR.Itesting.NE.6)THEN 
            ax=cal(1,1,1,1)+cal(1,1,1,2)+cal(1,1,1,3)    !matrices
            bx=cal(1,2,1,1)+cal(1,2,1,2)+cal(1,2,1,3)
            ay=cal(1,1,2,1)+cal(1,1,2,2)+cal(1,1,2,3)
            by=cal(1,2,2,1)+cal(1,2,2,2)+cal(1,2,2,3)
            IF((ax.NE.bx).OR.(ay.NE.by))WRITE(6,*)
     +      'Warning, different cal. for matrix 1 and 2'
          ELSE
            ax=cal(1,1,1,1)+cal(1,1,1,2)+cal(1,1,1,3)    !singles
            bx=cal(1,2,1,1)+cal(1,2,1,2)+cal(1,2,1,3)
            IF(ax.NE.bx)WRITE(6,*)
     +      'Warning, different cal. for spec. 1 and 2'
          ENDIF
        ENDIF
      ENDIF

      IF(NTOP.GT.5)THEN                        !case: constant    
        IF(IDEST.NE.I1)THEN  
          IF(ITYPE.GT.1)THEN 
            ax=cal(1,1,1,1)+cal(1,1,1,2)+cal(1,1,1,3)
            bx=cal(1,2,1,1)+cal(1,2,1,2)+cal(1,2,1,3)
            ay=cal(1,1,2,1)+cal(1,1,2,2)+cal(1,1,2,3)
            by=cal(1,2,2,1)+cal(1,2,2,2)+cal(1,2,2,3)
            IF((ax.NE.bx).OR.(ay.NE.by))WRITE(6,*)
     +      'Warning, different cal. for matrix 1 and 2'
          ELSE
            ax=cal(1,1,1,1)+cal(1,1,1,2)+cal(1,1,1,3)
            bx=cal(1,2,1,1)+cal(1,2,1,2)+cal(1,2,1,3)
            IF(ax.NE.bx)WRITE(6,*)
     +      'Warning, different cal. for spec. 1 and 2'
          ENDIF
        ENDIF
      ENDIF

C Updating the calibration and dimensions
      IF(ITYPE.GT.1)THEN
        I12=1
        IF(IDEST.EQ.1)I12=2
        IF((cal(1,I12,1,2)+cal(1,I12,2,2)).NE.2.)THEN !looks like I12 is calibr.
          IF(I1.EQ.I12.OR.I2.EQ.I12)THEN
            cal(1,IDEST,1,1)=cal(1,I12,1,1)
            cal(1,IDEST,1,2)=cal(1,I12,1,2)
            cal(1,IDEST,1,3)=cal(1,I12,1,3)
            cal(1,IDEST,2,1)=cal(1,I12,2,1)
            cal(1,IDEST,2,2)=cal(1,I12,2,2)
            cal(1,IDEST,2,3)=cal(1,I12,2,3)
          ENDIF
        ENDIF
        XDIM=Idim(1,I1,1) !Choosing highest dimension
        YDIM=Idim(1,I1,2)
        IF(I1.GT.0.AND.I2.GT.0)THEN
          XDIM=MAX0(Idim(1,1,1),Idim(1,2,1))
          YDIM=MAX0(Idim(1,1,2),Idim(1,2,2))
          IF(imax.LE.Idim(1,1,1).AND.Idim(1,1,1).LE.Idim(1,2,1))XDIM=Idim(1,1,1)
          IF(imax.LE.Idim(1,2,1).AND.Idim(1,2,1).LE.Idim(1,1,1))XDIM=Idim(1,2,1)
          IF(jmax.LE.Idim(1,1,2).AND.Idim(1,1,2).LE.Idim(1,2,2))YDIM=Idim(1,1,2)
          IF(jmax.LE.Idim(1,2,2).AND.Idim(1,2,2).LE.Idim(1,1,2))YDIM=Idim(1,2,2)    
        ENDIF 
        CALL SetMarker(1,1,1)
      ELSE
        I12=1
        IF(IDEST.EQ.1)I12=2
        IF(cal(2,I12,1,2).NE.1.)THEN                 !looks like I12 is calibr.
          IF(I1.EQ.I12.OR.I2.EQ.I12)THEN
            cal(2,IDEST,1,1)=cal(2,I1,1,1)
            cal(2,IDEST,1,2)=cal(2,I1,1,2)
            cal(2,IDEST,1,3)=cal(2,I1,1,3)
          ENDIF
        ENDIF
        MAXCH=Idim(2,I1,1)-1
        IF(I1.GT.0.AND.I2.GT.0)THEN
          MAXCH=MAX0(Idim(2,1,1)-1,Idim(2,2,1)-1)
          IF(imax.LE.Idim(2,1,1).AND.Idim(2,1,1).LE.Idim(2,2,1))MAXCH=Idim(2,1,1)-1
          IF(imax.LE.Idim(2,2,1).AND.Idim(2,2,1).LE.Idim(2,1,1))MAXCH=Idim(2,2,1)-1
        ENDIF
        CALL SetMarker(1,2,0)

      ENDIF

C Updating comment in the heading of spectrum file
      xcomm(1:3)='AR:'
C changed the next three lines A.S.
      WRITE(F1STR,991)NTOP
      write(xcomm(4:4+NTOP),FMT=F1STR,ERR=997)ans
991   FORMAT('(A',I5.5,')')

997   I12=1
      IF(IDEST.EQ.1)I12=2
      IF(IDEST.NE.I1.AND.IDEST.NE.I2)THEN 
        IF(ITYPE.GT.1)THEN
          comm(1,IDEST) =comm(1,I12) !Taking over comments if everything is new
          fname(1,IDEST)=fname(1,I12)!Taking over name if everything is new
        ELSE
          comm(2,IDEST) =comm(2,I12)
          fname(2,IDEST)=fname(2,I12)
        ENDIF
      ENDIF
      CALL AddComment(xcomm,4+NTOP)

      RETURN

 99   Istatus=2
      WRITE(6,*)'Sorry, illegal syntax of expression: ',ANS
      RETURN
      END


      SUBROUTINE BINNING
      INTEGER XDIM,YDIM 
      CHARACTER APP*4
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,xcomm*60
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH

		IF(ITYPE.GT.1)THEN
        WRITE(6,3)
   3    FORMAT(/'Binning applies only to 1-dim singles spectra')
	     Istatus=1
        RETURN
		ENDIF

      IDUM=1
      IF(IDEST.EQ.1)IDUM=2
      IDEST=IDUM
      WRITE(6,1)IDEST
   1  FORMAT('Destination spectrum <',I1,'>:',$)
      CALL READI(5,IDEST)
      ISP=1
      IF(IDEST.EQ.1)ISP=2
      WRITE(6,2)ISP
   2  FORMAT( 'Source spectrum      <',I1,'>:',$)
      CALL READI(5,ISP)
      IF(IDEST.LT.1.OR.IDEST.GT.2)Istatus=1
      IF(ISP  .LT.1.OR.ISP  .GT.2)Istatus=1

      IF(ISP.EQ.IDEST)THEN
        WRITE(6,*)'Destination must be another spectrum'
        Istatus=1
        RETURN
      ENDIF
 
      AX0=50.
		AX1=240.
      AX2=0.
		WRITE(6,*)'The binning is performed on the present data vector. Below you type'
		WRITE(6,*)'the calibration wanted for the binning. The units are chosen'
		WRITE(6,*)'to be keV, but it might be MeV, ns or other units. Be sure'
		WRITE(6,*)'that your binned spectrum has place within channels 0 - 8191.'
		WRITE(6,*)'If you have a large ch**2 coeff., you may increment the spectrum'
		WRITE(6,*)'at two different channels.'

      WRITE(6,10)AX0
  10  FORMAT(/'Cal. coeff. a0 (keV) on x-axis for binned spectrum       <',F8.3,'>:',$)
      CALL READF(5,AX0)
      WRITE(6,11)AX1
  11  FORMAT( 'Cal. coeff. a1 (keV/ch) on x-axis for binned spectrum    <',F8.3,'>:',$)
      CALL READF(5,AX1)
		WRITE(6,12)AX2
  12  FORMAT( 'Cal. coeff. a2 (keV/ch**2) on x-axis for binned spectrum <',E8.3,'>:',$)
      CALL READF(5,AX2)

		DO i=0,8191
		  rSPEC(IDEST,i)=0
		ENDDO
		
		DO i=0,8191
		  xx=rSPEC(ISP,i)
		  DO j=0,8191
		    IF(xx.EQ.0.AND.i.GT.0) GOTO 20
		    El=AX0+AX1*(FLOAT(j)-0.5)+AX2*(FLOAT(j)-0.5)*(FLOAT(j)-0.5)
		    Eh=AX0+AX1*(FLOAT(j)+0.5)+AX2*(FLOAT(j)+0.5)*(FLOAT(j)+0.5)
          IF(xx.GE.El.AND.xx.LT.Eh)rSPEC(IDEST,j)=rSPEC(IDEST,j)+1
  20      CONTINUE
		  ENDDO
		ENDDO
		
		isum1=0
		sum2=0
		DO i=0,8191
		  IF(rSPEC(ISP,i).NE.0.)isum1=i
		  sum2=sum2 + rSPEC(IDEST,i)
		ENDDO
		isum2=INT(sum2+0.5)
		WRITE(6,21)isum1+1
		WRITE(6,22)isum2
  21  FORMAT( 'Number of data points before binning: ',I8)
  22  FORMAT( 'Number of data points after binning:  ',I8)
		WRITE(6,*)'There may be different numbers before and after binning due to:'
		WRITE(6,*)' - some data are two times incremented because of large a2'
		WRITE(6,*)' - some data fall into channels outside 0 - 8191.'
		WRITE(6,*)' - channels with the value 0 are counted only once if it appear'
		WRITE(6,*)'   as the first number of the sequence,'
		WRITE(6,*)'   e.g.:  0., 1.21, 244.3, 311.3 etc., else 0 is not counted.'
  		WRITE(6,*)'   (the most secure way is to write 0 as e.g. 0.000001)'

C Estimate the spectrum length
      DO i=0,8191
		  IF(rSPEC(IDEST,i).NE.0)MAXCH=i
		ENDDO
		MAXCH=MAXCH+10
		IF(MAXCH.GT.8191)MAXCH=8191
		WRITE(6,30)MAXCH
  30  FORMAT('Dimension of spectrum is (0:',I4,')')
  
		cal(2,IDEST,1,1)=AX0
		cal(2,IDEST,1,2)=AX1
		cal(2,IDEST,1,3)=AX2

C Updating comment in the heading of spectrum file
		xcomm(1:3)='BI:'
		comm(2,IDEST)(1:60)=comm(2,ISP)(1:60)
998   CALL AddComment(xcomm,3)
		fname(2,IDEST)(1:8)=fname(2,ISP)(1:8)
		CALL SetMarker(1,2,0)
      END
		

      SUBROUTINE Derivate(dydx,y,x,dim,mode,imax)
C Routine to derivate spectra. The program calls the very
C old and nice SUBROUTINE MATINV(). User can choose between
C 5 algorithms.
C Magne Guttormsen, Oslo Cyclotron Laboratory, April 1998
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      DIMENSION dydx(0:8191),y(0:8191),x(0:8191)
      REAL*8 XMAT(3,3),sx1,sx2,sx3,sx4,sx0y,sx1y,sx2y
      INTEGER dim

C Initializing
      DO i=0,dim-1
        dydx(i)=0.
      ENDDO

C Finding mLow and mHigh markers
      mLow =0
      mHigh=dim-1
      DO i=0,dim-1
         IF(y(i).NE.0.AND.mLow.EQ.0)mLow=i
      ENDDO
      DO i=dim-1,0,-1
         IF(y(i).NE.0.AND.mHigh.EQ.dim-1)mHigh=i
      ENDDO
 
C The three simple methods
      IF(mode.EQ.1)THEN
         DO i=mLow+1,mHigh
            dx=x(i)-x(i-1)
            dy=y(i)-y(i-1)
            IF(dx.NE.0)dydx(i)=dy/dx
         ENDDO
         dydx(mLow)=dydx(mLow+1)
      ENDIF
      IF(mode.EQ.2)THEN
         DO i=mLow,mHigh-1
            dx=x(i+1)-x(i)
            dy=y(i+1)-y(i)
            IF(dx.NE.0)dydx(i)=dy/dx
         ENDDO
         dydx(mHigh)=dydx(mHigh-1)
      ENDIF
      IF(mode.EQ.3)THEN
         DO i=mLow+1,mHigh-1
            dx=x(i+1)-x(i-1)
            dy=y(i+1)-y(i-1)
            IF(dx.NE.0)dydx(i)=dy/dx
         ENDDO
         dydx(mLow) =dydx(mLow+1)
         dydx(mHigh)=dydx(mHigh-1)
      ENDIF

C The advanced method (see p.277 in M.R. Spiegel Prob & Stat)
C The number of points to fit is imax 
C Here is mode = 4 linear and mode = 5 quadratic
      IF(mode.EQ.4)THEN                     ! Linear fit
         DO i=mLow,mHigh
            sx1=0.
            sx2=0.
            sx0y=0.
            sx1y=0.
            dxdy=0.
            xi=(imax-1.)/2.
            xxi1=FLOAT(i)-xi
            xxi2=FLOAT(i)+xi
            i1=xxi1+0.5
            i2=xxi2+0.5
            IF(xxi1.LT.0)i1=xxi1-0.5
            IF(xxi2.LT.0)i2=xxi2-0.5
            IF(i1.LT.mLow)THEN
               i2=i2+(mLow-i1)
               i1=i1+(mLow-i1)
            ENDIF
            IF(i2.GT.mHigh)THEN
               i1=i1-(i2-mHigh)
               i2=i2-(i2-mHigh)
            ENDIF
            DO ii=i1,i2
               sx1=sx1+x(ii)
               sx2=sx2+x(ii)*x(ii)
               sx0y=sx0y+y(ii)
               sx1y=sx1y+x(ii)*y(ii)
            ENDDO
            XMAT(1,1)=imax
            XMAT(2,1)=sx1
            XMAT(1,2)=sx1
            XMAT(2,2)=sx2
            CALL MATINV(XMAT,2,3)
            a0=XMAT(1,1)*sx0y+XMAT(2,1)*sx1y
            a1=XMAT(1,2)*sx0y+XMAT(2,2)*sx1y
            dydx(i) = a1
         ENDDO
      ENDIF
      
      IF(mode.EQ.5)THEN                   ! Quadratic fit
         DO i=mLow,mHigh
            sx1=0.
            sx2=0.
            sx3=0.
            sx4=0.
            sx0y=0.
            sx1y=0.
            sx2y=0.
            xi=(imax-1.)/2.
            xxi1=FLOAT(i)-xi
            xxi2=FLOAT(i)+xi
            i1=xxi1+0.5
            i2=xxi2+0.5
            IF(xxi1.LT.0)i1=xxi1-0.5
            IF(xxi2.LT.0)i2=xxi2-0.5
            IF(i1.LT.mLow)THEN
               i2=i2+(mLow-i1)
               i1=i1+(mLow-i1)
            ENDIF
            IF(i2.GT.mHigh)THEN
               i1=i1-(i2-mHigh)
               i2=i2-(i2-mHigh)
            ENDIF
            DO ii=i1,i2
               sx1=sx1+x(ii)
               sx2=sx2+x(ii)*x(ii)
               sx3=sx3+x(ii)*x(ii)*x(ii)
               sx4=sx4+x(ii)*x(ii)*x(ii)*x(ii)
               sx0y=sx0y+y(ii)
               sx1y=sx1y+x(ii)*y(ii)
               sx2y=sx2y+x(ii)*x(ii)*y(ii)
            ENDDO
            XMAT(1,1)=imax
            XMAT(2,1)=sx1
            XMAT(3,1)=sx2
            XMAT(1,2)=sx1
            XMAT(2,2)=sx2
            XMAT(3,2)=sx3
            XMAT(1,3)=sx2
            XMAT(2,3)=sx3
            XMAT(3,3)=sx4
            CALL MATINV(XMAT,3,3)
            a0=XMAT(1,1)*sx0y+XMAT(2,1)*sx1y+XMAT(3,1)*sx2y
            a1=XMAT(1,2)*sx0y+XMAT(2,2)*sx1y+XMAT(3,2)*sx2y
            a2=XMAT(1,3)*sx0y+XMAT(2,3)*sx1y+XMAT(3,3)*sx2y
            dydx(i) = a1 + 2.0*a2*x(i)
         ENDDO
      ENDIF

      RETURN
      END

           
       SUBROUTINE AVERAGE(W,A1)
C REPLACES COUNTS IN SPECTRUM W(I) BY THE AVERAGE OVER FWHM=0.12
C AT 662 KEV, ETC.(OSLO 4/3-1988 /M. GUTTORMSEN)
       DIMENSION W(0:2047),X(0:2047),Y(0:2047)
       NL=1
       IFW=0
       EGAM=A1
       IFW=.12*(SQRT(662.*EGAM))/A1
       IF(IFW.LT.2)IFW=2
       EGAM=(IFW/2)*A1
       IFW=.12*(SQRT(662.*EGAM))/A1
       IF(IFW.LT.2)IFW=2
       NH=NL+IFW
       L=0

   22  TEMP=0
       L=L+1
       DO 20 I=NL,NH
   20  TEMP=W(I)+TEMP
       Y(L)=TEMP/(NH-NL+1)
       X(L)=(NL+NH)/2.0
       NL=NH+1
       EGAM=(NL+IFW/2)*A1
       IFW=0.12*(SQRT(662.*EGAM))/A1
       IF(IFW.LT.2)IFW=2
       NH=NL+IFW
       IF(NH.LT.2047) GO TO 22

       DO I=0,2047
        DO J=0,L
         IF(I.LT.X(J)) THEN
          JJ=J-1
          GO TO 40
         ENDIF
        ENDDO
   40   CONTINUE
        IF(I.LE.X(1).OR.I.GE.X(L))THEN
         GO TO 41
        ENDIF
       W(I)=Y(JJ)+(Y(JJ+1)-Y(JJ))*(I-X(JJ))/(X(JJ+1)-X(JJ))
   41  CONTINUE
       ENDDO
       END


      SUBROUTINE CALIBRATE
      INTEGER XDIM,YDIM
      CHARACTER APP*4
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60

      IR=5
      IW=6
      IID=IDEST
      IIT=ITYPE
      WRITE(6,*)'Choose spectrum type and spectrum number'
      WRITE(6,2)
   2  FORMAT(/,
     1          '      singles spectrum               1',/,
     2          '      set of spectra NA-1, NA-2,...  2',/,
     3          '      2-dimensional spectrum         3')
      WRITE(6,3)IIT
   3  FORMAT(/'Please, choose your type       <',I1,'>:',$)
      CALL READI(5,IIT)
 
      WRITE(6,1)IID
   1  FORMAT( 'Spectrum to calibrate          <',I1,'>:',$)
      CALL READI(5,IID)
      IF(IID.LE.0.OR.IID.GE.3.OR.IIT.LE.0.OR.IIT.GE.4)THEN
        Istatus=2
      ENDIF
      IF(Istatus.NE.0)RETURN

      IDEST=IID
      ITYPE=IIT

      IF(ITYPE.GT.1)THEN
        WRITE(IW,23)cal(1,IDEST,1,1)
 23     FORMAT(/'Cal. coeff. a0 (keV) on x-axis    <',F11.1,'>:',$)
        CALL READF(IR,cal(1,IDEST,1,1))
        WRITE(IW,24)cal(1,IDEST,1,2)
 24     FORMAT( 'Cal. coeff. a1 (keV/ch) on x-axis <',F11.3,'>:',$)
        CALL READF(IR,cal(1,IDEST,1,2))
        WRITE(IW,25)cal(1,IDEST,1,3)
 25     FORMAT( 'Cal. coeff. a2 (keV/ch2) on x-axis<',E11.2,'>:',$)
        CALL READF(IR,cal(1,IDEST,1,3))
        IF(Istatus.NE.0)RETURN
        WRITE(IW,26)cal(1,IDEST,2,1)
 26     FORMAT(/'Cal. coeff. a0 (keV) on y-axis    <',F11.1,'>:',$)
        CALL READF(IR,cal(1,IDEST,2,1))
        WRITE(IW,27)cal(1,IDEST,2,2)
 27     FORMAT( 'Cal. coeff. a1 (keV/ch) on y-axis <',F11.3,'>:',$)
        CALL READF(IR,cal(1,IDEST,2,2))
        WRITE(IW,28)cal(1,IDEST,2,3)
 28     FORMAT( 'Cal. coeff. a2 (keV/ch2) on y-axis<',E11.2,'>:',$)
        CALL READF(IR,cal(1,IDEST,2,3))
        IF(Istatus.NE.0)RETURN

        XDIM=Idim(1,IDEST,1)
        YDIM=Idim(1,IDEST,2)
        WRITE(IW,29)XDIM
 29     FORMAT(/'Dimension along x-axis of matrix <',I4,'>:',$)
        CALL READI(IR,XDIM)
        WRITE(IW,30)YDIM
 30     FORMAT( 'Dimension along y-axis of matrix <',I4,'>:',$)
        CALL READI(IR,YDIM)
        IF(Istatus.NE.0)RETURN
        IF(XDIM.GT.4096)XDIM=4096
        IF(YDIM.GT. 2048)YDIM=2048
        IF(XDIM.NE.Idim(1,IDEST,1))CALL SetMarker(1,0,0)
        IF(YDIM.NE.Idim(1,IDEST,2))CALL SetMarker(0,1,0)

      ELSE

        WRITE(IW,23)  cal(2,IDEST,1,1)
        CALL READF(IR,cal(2,IDEST,1,1))
        WRITE(IW,24)  cal(2,IDEST,1,2)
        CALL READF(IR,cal(2,IDEST,1,2))
        WRITE(IW,25)  cal(2,IDEST,1,3)
        CALL READF(IR,cal(2,IDEST,1,3))
        IF(Istatus.NE.0)RETURN

        IMAXCH=Idim(2,IDEST,1)
        WRITE(6,5)IMAXCH
  5     FORMAT(/'Dimension of singles spectrum <',I5,'>:',$)
        CALL READI(5,IMAXCH)
        MAXCH=IMAXCH-1
        IF(MAXCH.GT.8191)MAXCH=8191
        IF(IMAXCH.NE.Idim(2,IDEST,1))CALL SetMarker(1,0,0)
      ENDIF
      END


      SUBROUTINE COMP
      INTEGER XDIM,YDIM 
      CHARACTER APP*4
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,xcomm*60
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH

      IDUM=1
      IF(IDEST.EQ.1)IDUM=2
      IDEST=IDUM
      WRITE(6,1)IDEST
   1  FORMAT('Destination spectrum <',I1,'>:',$)
      CALL READI(5,IDEST)
      ISP=1
      IF(IDEST.EQ.1)ISP=2
      WRITE(6,2)ISP
   2  FORMAT( 'Source spectrum      <',I1,'>:',$)
      CALL READI(5,ISP)
      IF(IDEST.LT.1.OR.IDEST.GT.2)Istatus=1
      IF(ISP  .LT.1.OR.ISP  .GT.2)Istatus=1

      IF(ISP.EQ.IDEST)THEN
        WRITE(6,*)'Destination must be another spectrum'
        Istatus=1
        RETURN
      ENDIF
      WRITE(6,*)'Compression factors must be integer. With a factor'
      WRITE(6,*)'of e.g. 3, channels 0,1,2,3 goes to 0,0,0,1'
      ICX=1
      ICY=1
 
      IF(ITYPE.GT.1)THEN
        WRITE(6,3)ICX
   3    FORMAT(/'Compression along x-axis <',I3,'>:',$)
        CALL READI(5,ICX)
        WRITE(6,4)ICY
   4    FORMAT( 'Compression along y-axis <',I3,'>:',$)
        CALL READI(5,ICY)
      
        IF(Istatus.NE.0)RETURN
        DO J=0,2047
          DO I=0,4095
            rMAT(IDEST,I,J)=0
          ENDDO
        ENDDO
        DO J=0,2047
          jt=((j+1)/30)*30
          IF(jt.EQ.j+1)THEN
            write(6,FMT='(A1,$)')'.'
            call flush(6)
          ENDIF
          JJ=J/ICY
          DO I=0,4095
            II=I/ICX
           rMAT(IDEST,II,JJ)=rMAT(IDEST,II,JJ)+rMAT(ISP,I,J)
          ENDDO
        ENDDO
        XDIM=Idim(1,ISP,1)/ICX
        YDIM=Idim(1,ISP,2)/ICY
        WRITE(6,*)' '
        WRITE(6,5)XDIM-1,YDIM-1
   5    FORMAT('New dimension (0:',I4,',0:',I4,')')

        ax0=cal(1,ISP,1,1)
        ax1=cal(1,ISP,1,2)
        ax2=cal(1,ISP,1,3)
        ay0=cal(1,ISP,2,1)
        ay1=cal(1,ISP,2,2)
        ay2=cal(1,ISP,2,3)

C Remember compression makes nontrivial changes in a0 and a1 (not a2)          
        b=(ICX-1.)/2.
        cal(1,IDEST,1,1)=ax0+ax1*b+ax2*b*b
        cal(1,IDEST,1,2)=ICX*(ax1+2.*ax2*b)
        cal(1,IDEST,1,3)=ICX*ICX*ax2

        b=(ICY-1.)/2.
        cal(1,IDEST,2,1)=ay0+ay1*b+ay2*b*b
        cal(1,IDEST,2,2)=ICY*(ay1+2.*ay2*b)
        cal(1,IDEST,2,3)=ICY*ICY*ay2

C Updating comment in the heading of spectrum file
        xcomm(1:3)='CO:'
        write(xcomm(4:7),991,ERR=997)ICX
991     FORMAT(I4)
        xcomm(8:8)='-'
        write(xcomm(9:12),991,ERR=997)ICY
        comm(1,IDEST)(1:60)=comm(1,ISP)(1:60)
997     CALL AddComment(xcomm,12)
        fname(1,IDEST)(1:8)=fname(1,ISP)(1:8)
        CALL SetMarker(1,1,2)

      ELSE
        WRITE(6,3)ICX
        CALL READI(5,ICX)
        IF(Istatus.NE.0)RETURN
        DO I=0,8191
          rSPEC(IDEST,I)=0
        ENDDO
        DO I=0,8191
          II=I/ICX
          rSPEC(IDEST,II)=rSPEC(IDEST,II)+rSPEC(ISP,I)
        ENDDO
          
        ax0=cal(2,ISP,1,1)
        ax1=cal(2,ISP,1,2)
        ax2=cal(2,ISP,1,3)
        b=(FLOAT(ICX)-1.)/2.
        cal(2,IDEST,1,1)=ax0+ax1*b+ax2*b*b
        cal(2,IDEST,1,2)=ICX*(ax1+2.*ax2*b)
        cal(2,IDEST,1,3)=ICX*ICX*ax2 
        MAXCH=(Idim(2,ISP,1)-1)/ICX
        WRITE(6,55)MAXCH
  55    FORMAT('New dimension (0:',I4,')')

C Updating comment in the heading of spectrum file
        xcomm(1:3)='CO:'
        write(xcomm(4:7),991,ERR=998)ICX
        comm(2,IDEST)(1:60)=comm(2,ISP)(1:60)
998     CALL AddComment(xcomm,7)
        fname(2,IDEST)(1:8)=fname(2,ISP)(1:8)
        CALL SetMarker(1,2,0)
      ENDIF
      END


      SUBROUTINE CUTPLANE
      INTEGER XDIM,YDIM,MX,MY,SX,SY
      CHARACTER APP*4
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,xcomm*60
        
      IF(ITYPE.EQ.1)ITYPE=3
      IDUM=1
      IF(IDEST.EQ.1)IDUM=2
      IDEST=IDUM
      WRITE(6,1)IDEST
 1    FORMAT('Destination spectrum <',I1,'>:',$)
      CALL READI(5,IDEST)
      ISP=1
      IF(IDEST.EQ.1)ISP=2
      WRITE(6,2)ISP
 2    FORMAT( 'Source spectrum      <',I1,'>:',$)
      CALL READI(5,ISP)
      IF(IDEST.LT.1.OR.IDEST.GT.2)Istatus=1
      IF(ISP  .LT.1.OR.ISP  .GT.2)Istatus=1

      IF(ISP.EQ.IDEST)THEN
        WRITE(6,*)'Destination must be another spectrum'
        Istatus=1
        RETURN
      ENDIF
      IF(Istatus.NE.0)RETURN
             
      ISP=1
      IF(IDEST.EQ.1)ISP=2

C READING IN VARIOUS PARAMETERS TO BE USED
      AX0=cal(1,ISP,1,1)
      AX1=cal(1,ISP,1,2) 
      AY0=cal(1,ISP,2,1)
      AY1=cal(1,ISP,2,2)
      IF(AX0+AX1.EQ.1.)THEN
        AX0=11.
        AX1=20.
      ENDIF
      IF(AY0+AY1.EQ.1.)THEN
        AY0=9660.
        AY1=-120.
      ENDIF

      FACX=0.
      FACY=-1.
      WRITE(6,*)'The following default values correspond to the'
      WRITE(6,*)'transformation  Ex -> Ex  and  Ey -> Ey-Ex.'
      WRITE(6,*)'The calibrations on x- and y-axis are conserved.'

      WRITE(6,10)AX0
  10  FORMAT(/'Cal. coeff. a0 (keV) on x-axis   <',F8.1,'>:',$)
      CALL READF(5,AX0)
      WRITE(6,11)AX1
  11  FORMAT( 'Cal. coeff. a1 (keV/ch) on x-axis<',F8.1,'>:',$)
      CALL READF(5,AX1)

      WRITE(6,12)AY0
  12  FORMAT(/'Cal. coeff. a0 (keV) on y-axis   <',F8.1,'>:',$)
      CALL READF(5,AY0)
      WRITE(6,13)AY1
  13  FORMAT( 'Cal. coeff. a1 (keV/ch) on y-axis<',F8.1,'>:',$)
      CALL READF(5,AY1)

      IF(Istatus.NE.0)RETURN

      WRITE(6,14)FACX
  14  FORMAT(/'Give factor in transf.Ex=Ex+f*Ey, f=<',F5.1,'>:',$)
      CALL READF(5,FACX)
      WRITE(6,15)FACY
  15  FORMAT( 'Give factor in transf.Ey=Ey+f*Ex, f=<',F5.1,'>:',$)
      CALL READF(5,FACY)

      EXMAX=8000.0
      WRITE(6,16)EXMAX
  16  FORMAT(/'Give upper energy on x-axis (keV)<',F8.1,'>:',$)
      CALL READF(5,EXMAX)
      EYMAX=EXMAX
      WRITE(6,17)EYMAX
  17  FORMAT( 'Give upper energy on y-axis (keV)<',F8.1,'>:',$)
      CALL READF(5,EYMAX)
      IF(Istatus.NE.0)RETURN
      IMAX=((EXMAX-AX0)/AX1+0.5)
      JMAX=((EYMAX-AY0)/AY1+0.5)
      IMIN=(-AX0/AX1+0.5)
      JMIN=(-AY0/AY1+0.5)
      IF(IMIN.GT.IMAX)THEN
        I=IMIN
        IMIN=IMAX
        IMAX=I
      ENDIF
      IF(JMIN.GT.JMAX)THEN
        J=JMIN
        JMIN=JMAX
        JMAX=J
      ENDIF

      IF(IMAX.GT.4095)IMAX=4095
      IF(JMAX.GT.2047 )JMAX=2047
      IF(IMIN.LT.0   )IMIN=0
      IF(JMIN.LT.0   )JMIN=0
      WRITE(6,18)IMIN,IMAX,JMIN,JMAX
  18  FORMAT(/'Data taken from Chx= ',I4,'-',I4,
     +' and Chy= ',I4,'-',I4,/)

      DO J=0,2047
        DO I=0,4095
          rMAT(IDEST,I,J)=0
        ENDDO
      ENDDO

      XDIM=8
      YDIM=8
      DO J=JMIN,JMAX
        DO I=IMIN,IMAX
          EEX=AX0+AX1*I
          EEY=AY0+AY1*J
          EX=EEX+FACX*EEY
          EY=EEY+FACY*EEX
C WE NOW DISTRIBUTE THE COUNTS INTO APPROPRIATE CHANNELS, GENERALLY 4.
C FINDING FIRST THE MAIN CHANNEL (MX,MY) AND PUT SOME FRACTION INTO THE 3
C OTHER SIDE-CHANNELS (SX,MY), (MX,SY) AND (SX,SY)
          CX=(EX-AX0)/AX1
          CY=(EY-AY0)/AY1
          MX=CX+0.5
          MY=CY+0.5
          FX=CX-MX
          FY=CY-MY
          SX=MX+1
          SY=MY+1
          IF(FX.LT.0)SX=MX-1
          IF(FY.LT.0)SY=MY-1
          FX=ABS(FX)
          FY=ABS(FY)

C CALCULATING THE M1AREAS THAT A SQUARE AROUND (Ex,Ey) COVERS THE 4 CH.
          AX =FX     *(1.-FY)
          AY =(1.-FX)*FY
          AXY=FX     *FY

C CALCULATING THE NUMBER OF COUNTS TO BE PUT IN
          ITOT=rMAT(ISP,I,J)
          IX =AX *ITOT
          IY =AY *ITOT
          IXY=AXY*ITOT
          IM=ITOT-IX-IY-IXY

          IF(MX.GE.0.AND.MX.LE.4095.AND.MY.GE.0.AND.MY.LE. 2047)THEN
            IF(MX.GT.XDIM)XDIM=MX  !Finding x- and y-dimensions
            IF(MY.GT.YDIM)YDIM=MY
            rMAT(IDEST,MX,MY)=rMAT(IDEST,MX,MY)+IM
            IF(SX.GE.0.AND.SX.LE.2047)
     +      rMAT(IDEST,SX,MY)=rMAT(IDEST,SX,MY)+IX
            IF(SY.GE.0.AND.SY.LE.2047 )
     +      rMAT(IDEST,MX,SY)=rMAT(IDEST,MX,SY)+IY
          ENDIF
          IF(SX.GE.0.AND.SX.LE.4095.AND.SY.GE.0.AND.SY.LE. 2047)THEN
            rMAT(IDEST,SX,SY)=rMAT(IDEST,SX,SY)+IXY
          ENDIF
        ENDDO
      ENDDO
      cal(1,IDEST,1,1)=cal(1,ISP,1,1)
      cal(1,IDEST,1,2)=cal(1,ISP,1,2)
      cal(1,IDEST,1,3)=cal(1,ISP,1,3)
      cal(1,IDEST,2,1)=cal(1,ISP,2,1)
      cal(1,IDEST,2,2)=cal(1,ISP,2,2)
      cal(1,IDEST,2,3)=cal(1,ISP,2,3)

C Updating comment in the heading of spectrum file
      xcomm(1:3)='CU:'
      write(xcomm(4:11),991,ERR=997)FACx
991   FORMAT(F8.1)
      xcomm(12:12)='-'
      write(xcomm(13:20),991,ERR=997)FACy
997   fname(1,IDEST)='CU'//fname(1,ISP)(1:6)
      comm(1,IDEST)=comm(1,ISP)
      CALL AddComment(xcomm,20)
      CALL SetMarker(1,1,1)
      END


      SUBROUTINE ELASTICM
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      INTEGER XDIM,YDIM  
      CHARACTER APP*4
      CHARACTER ANS*1
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,xcomm*60
      DIMENSION Fi(0:8191),Ff(0:8191)

      A0f=0.
      A1f=0.
      IDUM=1
      IF(IDEST.EQ.1)IDUM=2
      IDEST=IDUM
      WRITE(6,1)IDEST
   1  FORMAT('Destination spectrum <',I1,'>:',$)
      CALL READI(5,IDEST)
      ISP=1
      IF(IDEST.EQ.1)ISP=2
      WRITE(6,2)ISP
   2  FORMAT( 'Source spectrum      <',I1,'>:',$)
      CALL READI(5,ISP)
      IF(IDEST.LT.1.OR.IDEST.GT.2)Istatus=1
      IF(ISP  .LT.1.OR.ISP  .GT.2)Istatus=1

      IF(ISP.EQ.IDEST)THEN
        WRITE(6,*)'Destination must be another spectrum'
        Istatus=1
        RETURN
      ENDIF
      IF(Istatus.NE.0)RETURN

C Zeroing matrices
      IF(ITYPE.GT.1)THEN
        DO j=0,2047
           DO i=0,4095
             rMAT(IDEST,i,j)=0
           ENDDO
        ENDDO
      ELSE
        DO i=0,8191
          rSPEC(IDEST,i)=0
        ENDDO
      ENDIF
      DO i=0,8191
        Fi(i)=0.
        Ff(i)=0.
      ENDDO

C Setting default values
      ANS='x'
      IF(ITYPE.GT.1)THEN
        WRITE(6,3)ANS
   3    FORMAT(/,'Stretch/compress along x or y axis <',A1,'>:',$)
        CALL READA1(5,ANS)
      ENDIF

      IF(ANS.EQ.'X'.OR.ANS.EQ.'x')THEN
        IF(ITYPE.GT.1)THEN
          A0i=cal(1,ISP,1,1)
          A1i=cal(1,ISP,1,2)
        ELSE
          A0i=cal(2,ISP,1,1)
          A1i=cal(2,ISP,1,2)
        ENDIF
        A0f=A0i
        A1f=A1i
        WRITE(6,4)A0i
   4    FORMAT(/'Old a0 on x-axis  <',F8.3,'>:',$)
        CALL READF(5,A0i)
        WRITE(6,5)A1i
   5    FORMAT( 'Old a1 on x-axis  <',F8.3,'>:',$)
        CALL READF(5,A1i)
        WRITE(6,6)A0f
   6    FORMAT(/'New a0 on x-axis  <',F8.3,'>:',$)
        CALL READF(5,A0f)
        WRITE(6,7)A1f
   7    FORMAT( 'New a1 on x-axis  <',F8.3,'>:',$)
        CALL READF(5,A1f)
        IF(Istatus.NE.0)RETURN
        IF(A0f.EQ.0..AND.A1f.EQ.0.)RETURN

C Matrices
        IF(ITYPE.GT.1)THEN
          DO j=0,2047
            JT=(J/10)*10
            IF(JT.EQ.J)THEN
              write(6,FMT='(A1,$)')'.'
              call flush(6)
            ENDIF
            Sum=0.
            DO i=0,4095
              Fi(i)=rMAT(ISP,i,j)     ! Fi(i) and Ff(i) real type
              Sum=Sum+Fi(i)
            ENDDO
            IF(Sum.NE.0)THEN
              CALL ELASTIC(Fi,Ff,A0i,A1i,A0f,A1f,4096,4096)
              DO i=0,4095
                rMAT(IDEST,i,j)=Ff(i)
                Fi(i)=0.
              ENDDO
            ENDIF
          ENDDO
          cal(1,IDEST,1,1)=A0f
          cal(1,IDEST,1,2)=A1f
          cal(1,IDEST,1,3)=0.
          cal(1,IDEST,2,1)=cal(1,ISP,2,1)
          cal(1,IDEST,2,2)=cal(1,ISP,2,2)
          cal(1,IDEST,2,3)=cal(1,ISP,2,3)
C Finding new dimension
          E1=A0i+A1i*0   
          E2=A0i+A1i*Idim(1,ISP,1)
          IC1=(E1-A0f)/A1f  +10     
          IC2=(E2-A0f)/A1f  +10    
          XDIM=MAX0(IC1,IC2)
          XDIM=MIN0(XDIM,4096)
          IF(XDIM.LT.1)XDIM=64
C Updating comment in the heading of spectrum file
          xcomm(1:4)='ELx:'
          fname(1,IDEST)=fname(1,ISP)
          comm(1,IDEST)=comm(1,ISP)
          CALL AddComment(xcomm,4)
          CALL SetMarker(1,1,2)
          WRITE(6,*)' '
        ELSE
C Singles spectrum
          Sum=0.
          DO i=0,8191
            Fi(i)=rSPEC(ISP,i)
            Sum=Sum+Fi(i)
          ENDDO
          IF(Sum.NE.0)THEN
            CALL ELASTIC(Fi,Ff,A0i,A1i,A0f,A1f,8192,8192)
            DO i=0,8191
              rSPEC(IDEST,i)=Ff(i)
              Fi(i)=0.
            ENDDO
          ENDIF
        ENDIF
        cal(2,IDEST,1,1)=A0f
        cal(2,IDEST,1,2)=A1f
        cal(2,IDEST,1,3)=0.
C Finding new dimension
        E1=A0i+A1i*0   
        E2=A0i+A1i*Idim(2,ISP,1)
        IC1=(E1-A0f)/A1f  +10     
        IC2=(E2-A0f)/A1f  +10    
        MAXCH=MAX0(IC1,IC2)
        MAXCH=MIN0(MAXCH,8191)
        IF(MAXCH.LT.1)MAXCH=4095
C Updating comment in the heading of spectrum file
         xcomm(1:4)='ELx:'
         fname(2,IDEST)=fname(2,ISP)
         comm(2,IDEST)=comm(2,ISP)
         CALL AddComment(xcomm,4)
        CALL SetMarker(1,2,0)
      ENDIF      
           
      IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')THEN
        A0i=cal(1,ISP,2,1)
        A1i=cal(1,ISP,2,2)
        A0f=A0i
        A1f=A1i

        WRITE(6,14)A0i
  14    FORMAT(/'Old a0 on y-axis  <',F8.3,'>:',$)
        CALL READF(5,A0i)
        WRITE(6,15)A1i
  15    FORMAT( 'Old a1 on y-axis  <',F8.3,'>:',$)
        CALL READF(5,A1i)
        WRITE(6,16)A0f
  16    FORMAT(/'New a0 on y-axis  <',F8.3,'>:',$)
        CALL READF(5,A0f)
        WRITE(6,17)A1f
  17    FORMAT( 'New a1 on y-axis  <',F8.3,'>:',$)
        CALL READF(5,A1f)
        IF(Istatus.NE.0)RETURN
        IF(A0f.EQ.0..AND.A1f.EQ.0.)RETURN

C Only matrices have y-axis with more than one channel
        DO i=0,4095          
          IT=(I/100)*100
          IF(IT.EQ.I)THEN
            write(6,FMT='(A1,$)')'.'
            call flush(6)
          ENDIF

          Sum=0.
          DO j=0,2047
            Fi(j)=rMAT(ISP,i,j)
            Sum=Sum+Fi(j)
          ENDDO
          IF(Sum.NE.0)THEN
            CALL ELASTIC(Fi,Ff,A0i,A1i,A0f,A1f,2048,2048)
            DO j=0,2047
               rMAT(IDEST,i,j)=Ff(j)
               Fi(j)=0.
            ENDDO
          ENDIF
        ENDDO
        cal(1,IDEST,2,1)=A0f
        cal(1,IDEST,2,2)=A1f
        cal(1,IDEST,2,3)=0.
        cal(1,IDEST,1,1)=cal(1,ISP,1,1)
        cal(1,IDEST,1,2)=cal(1,ISP,1,2)
        cal(1,IDEST,1,3)=cal(1,ISP,1,3)
C Finding new dimension
        E1=A0i+A1i*0   
        E2=A0i+A1i*Idim(1,ISP,2)
        IC1=(E1-A0f)/A1f  +10     
        IC2=(E2-A0f)/A1f  +10    
        YDIM=MAX0(IC1,IC2)
        YDIM=MIN0(YDIM,2048)
        IF(YDIM.LT.1)YDIM=64
C Updating comment in the heading of spectrum file
          xcomm(1:4)='ELy:'
          fname(1,IDEST)=fname(1,ISP)
          comm(1,IDEST)=comm(1,ISP)
          CALL AddComment(xcomm,4)
          CALL SetMarker(1,1,2)
        WRITE(6,*)' '
      ENDIF
      END


      SUBROUTINE ELASTIC(Fi,Ff,A0i,A1i,A0f,A1f,Di,Df)
C The most magnificant stretch- and compress-routine
C ever created by a human beeing. It is complicated, but works! 
C The routine streches or compresses spectrum from initial
C calibration (A0i,A1i) to final (A0f,A1f). The dimensions
C of the real spectra are Di and Df. First channel is 0, so
C that the spectra occupy (0:D-1) channels.
C August 1994, Oslo Cyclotron Laboratory, Magne Guttormsen
      INTEGER Di,Df
      DIMENSION Fi(0:Di-1),Ff(0:Df-1)
C Testing
      IF(A1i.EQ.0.OR.A1f.EQ.0)RETURN
C Zeroing final spectrum
      DO i=0,Df-1
        Ff(i)=0.  
      ENDDO

C Case where no action is taken
      IF(A0i.EQ.A0f.AND.A1i.EQ.A1f)THEN
        DO i=0,MIN0(Di-1,Df-1)
          Ff(i)=Fi(i)
        ENDDO
        RETURN
      ENDIF

C Taking counts in initial spectrum and find where
C to put it in final spectrum. Then it is distributed on
C the channel(s). The loop goes through all the
C channels i of the initial spectrum

      IF(ABS(A1i/A1f).LT.2)THEN
        DO i=0,Di-1
          IF(Fi(i).EQ.0)GO TO 99
          EiL=A0i+A1i*(i-0.5)      !Step 1.0 chs left and right
          EiH=A0i+A1i*(i+0.5)
          CHf1=(EiL-A0f)/A1f       !CHf1 and CHf2 define limits where
          CHf2=(EiH-A0f)/A1f       !to put the counts in final spc.
          CHlength=ABS(CHf1-CHf2)  !Number of channels (float)
          CountCH=Fi(i)/CHlength   !Number of counts pr.ch unit
          CHfL=CHf1
          CHfH=CHf2
          IF(CHfL.GT.CHfH)THEN
            CHfL=CHf2
            CHfH=CHf1
          ENDIF
          j1=CHfL+0.5              
          j2=CHfH+0.5              !filling with CHwidth*CountCH 
          IF(j1.GE.Df.OR.j2.LT.0)GO TO 99
          nch=j2-j1+1
          IF(nch.EQ.1)THEN         !One channel to fill
            IF(j1.GE.0)Ff(j1)=Ff(j1)+Fi(i)
            GO TO 99
          ENDIF
          IF(nch.GT.1)THEN  !Two or three channels to fill
            Counts=CountCH*(j1+0.5-CHfL) !Fraction of left channel
            IF(j1.GE.0)Ff(j1)=Ff(j1)+Counts       
            Counts=CountCH*(CHfH+0.5-j2) !Fraction of right channel
            IF(j2.LE.Df-1)Ff(j2)=Ff(j2)+Counts
            DO j=j1+1,j2-1           !Filling in for whole chs.
              IF(j.GE.0.AND.j.LE.Df-1)Ff(j)=Ff(j)+CountCH
            ENDDO
          ENDIF
  99      CONTINUE
        ENDDO
      ELSE

C The counts will be distributed in the streching procedure as a triangle,
C which has its left and right tail overlaping with the center of the 
C next triangle. The heighth and basis of the triangle is called h and b:
C               x         x          x
C             x   x     x   x      x   x
C           x       x x       x   x      x
C         x          0          0          x
C       x          x   x      x  x           x
C     x          x       x  x      x           x
C   x          x           0         x           x

        b=2.0*A1i/A1f   !basis of triangle
        h=2.0/b         !height of triangle in order to get area=1
        alpha=h/(b/2.)  !slope of triangle tails
        DO i=0,Di-1
          IF(Fi(i).EQ.0)GO TO 98
          EiL=A0i+A1i*(i-1.)      !Step 1.0 chs left and right
          EiH=A0i+A1i*(i+1.)
          CHf1=(EiL-A0f)/A1f       !CHf1 and CHf2 define limits where
          CHf2=(EiH-A0f)/A1f       !to put the counts in final spc.
          CHfL=CHf1
          CHfH=CHf2
          IF(CHfL.GT.CHfH)THEN
            CHfL=CHf2
            CHfH=CHf1
          ENDIF
          j1=CHfL+1             
          j2=CHfH              
          IF(j1.GE.Df.OR.j2.LT.0)GO TO 98
          w=0.
          DO j=j1,j2
            IF(j.LT.CHfL+(b/2.))THEN
              w=alpha*(j-CHfL)                !up going slope
            ELSE 
              w=h-alpha*(j-(CHfL+(b/2.)))     !down going slope
            ENDIF
            IF(w.LT.-0.1)WRITE(6,*)'Warning, weight w < 0 : ',w
            IF(w.LT.0)w=0.
            IF(j.GE.0.AND.j.LE.Df-1)Ff(j)=Ff(j)+w*Fi(i)
          ENDDO
  98      CONTINUE
        ENDDO
      ENDIF
      END


      SUBROUTINE EXPONENT(NC)
      REAL N,NC,MASS
      COMMON/CALxy/AX0,AX1,AY0,AY1
      COMMON/CNUTE/EX,EXL,EXH,XJ,EGAP,MASS,EXPN(0:4095),N(0:4095),FWXG
      DOUBLE PRECISION GAMW(0:4095)

C SUBROUTINE CALLED FROM NUTE TO CALCULATE THE EXPONENT N(ch). IT MIGHT
C BE CONSTANT (IF NC.NE.0) OR VARIABEL ACCORDING TO AXEL

      IF(NC.LT.0)THEN
        WRITE(6,*)'Exponent cannot be negative'
        Istatus=1
        RETURN
      ENDIF

      IF(NC.GT.0)THEN
        DO I=0,4095
           N(I)=NC
           EXPN(I)=(AX0+FLOAT(I)*AX1)**NC
        ENDDO
        RETURN
      ENDIF

C METHOD OF AXEL. USES THE TAIL OF A GIANT RESSONANCE WITH A WITDH OF
C 5000 KEV. WE PUT N=4 UP TO EX=3000 KEV. NB! THE FOLLOWING FORMULA HAS
C BEEN CALCULATED IN MEV-UNITS IN ORDER TO AVOID USE OF DOUBLE PRECISION
      IF(NC.EQ.0)THEN
        I3MEV=((3000.-AX0)/AX1 +0.5)
        E3MEV=(AX0+FLOAT(I3MEV)*AX1)/1000.
        FWHM=5.
        ER=80.*(MASS**(-1./3.))

        DO I=0,4095
          E1=(AX0+FLOAT(I)*AX1)/1000.
          N(I)=4.0
          IF(I.GE.I3MEV.AND.I.LT.10*I3MEV)THEN
            GAMW(I)=E1**4*FWHM/
     +      ((ER**2-E1**2)**2+(E1**2)*(FWHM**2))
            C=GAMW(I3MEV)/E3MEV**4
            N(I)=LOG(GAMW(I)/C)/LOG(E1)
          ENDIF
          EXPN(I)=(AX0+FLOAT(I)*AX1)**N(I)
        ENDDO

C WRITING THE N-VALUES IN THE CASE OF AXEL'S APPROACH
        ER=ER*1000.
        FWHM=FWHM*1000.
        WRITE(6,10)ER,FWHM
  10    FORMAT(/,' ASSUMED GIANT DIPOLE RESONANCE AT',F7.0,' keV',
     +' WITH A WIDTH OF',F6.0,' keV.')
        WRITE(6,11)
  11    FORMAT(' EXPONENT AS A FUNCTION OF GAMMA-ENERGY n(E):')
        EDELTA=1000.
        EG4=-EDELTA
        DO I=1,5
          EG1=EG4+EDELTA
          EG2=EG1+EDELTA
          EG3=EG2+EDELTA
          EG4=EG3+EDELTA
          I1=(EG1-AX0)/AX1+0.5
          I2=(EG2-AX0)/AX1+0.5
          I3=(EG3-AX0)/AX1+0.5
          I4=(EG4-AX0)/AX1+0.5
          IF(I1.LT.1)I1=1
          IF(I2.LT.1)I2=1
          IF(I3.LT.1)I3=1
          IF(I4.LT.1)I4=1
         WRITE(6,12)N(I1),EG1,N(I2),EG2,N(I3),EG3,N(I4),EG4
  12      FORMAT(2X,4(2X,F4.2,'(',F6.0,' keV)'))
        ENDDO
      ENDIF
      RETURN
      END


      SUBROUTINE FILLNEG
      INTEGER XDIM,YDIM
      CHARACTER APP*4
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,xcomm*60

      IDUM=1
      IF(IDEST.EQ.1)IDUM=2
      IDEST=IDUM
      WRITE(6,1)IDEST
 1    FORMAT('Destination spectrum <',I1,'>:',$)
      CALL READI(5,IDEST)
      ISP=1
      IF(IDEST.EQ.1)ISP=2
      WRITE(6,2)ISP
 2    FORMAT( 'Source spectrum      <',I1,'>:',$)
      CALL READI(5,ISP)
      IF(IDEST.LT.1.OR.IDEST.GT.2)Istatus=1
      IF(ISP  .LT.1.OR.ISP  .GT.2)Istatus=1

      IF(ISP.EQ.IDEST)THEN
        WRITE(6,*)'Destination must be another spectrum'
        Istatus=1
        RETURN
      ENDIF

      IF(Istatus.NE.0)RETURN
      NEGN1=0
      xNEGS1=0
      NEGN2=0
      xNEGS2=0


      IF(ITYPE.GT.1)THEN
        XDIM=Idim(1,ISP,1)
        YDIM=Idim(1,ISP,2)

        WRITE(6,5)XDIM
 5      FORMAT(/'Dimension along x-axis <',I4,'>:',$)
        CALL READI(5,XDIM)
        WRITE(6,6)YDIM
 6      FORMAT( 'Dimension along y-axis <',I4,'>:',$)
        CALL READI(5,YDIM)
        DO J=0,2047
          DO I=0,4095
            xx=rMAT(ISP,I,J)
            IF(xx.LT.0)THEN
              NEGN1=NEGN1+1
              xNEGS1=xNEGS1+xx
            ENDIF
          ENDDO
        ENDDO
        CALL FILL
        DO J=0,2047
          DO I=0,4095
            xx=rMAT(IDEST,I,J)
            IF(xx.LT.0)THEN
              NEGN2=NEGN2+1
              xNEGS2=xNEGS2+xx
            ENDIF
          ENDDO
        ENDDO
        cal(1,IDEST,1,1)=cal(1,ISP,1,1)
        cal(1,IDEST,1,2)=cal(1,ISP,1,2)
        cal(1,IDEST,1,3)=cal(1,ISP,1,3)
        cal(1,IDEST,2,1)=cal(1,ISP,2,1)
        cal(1,IDEST,2,2)=cal(1,ISP,2,2)
        cal(1,IDEST,2,3)=cal(1,ISP,2,3)
C Updating comment in the heading of spectrum file
        xcomm(1:3)='FN:'
        fname(1,IDEST)=fname(1,ISP)
        comm(1,IDEST)=comm(1,ISP)
        CALL AddComment(xcomm,3)

      ELSE
        MAXCH=Idim(2,ISP,1)-1
        IMAXCH=MAXCH+1
        WRITE(6,7)IMAXCH
 7      FORMAT(/'Dimension of spectrum <',I4,'>:',$)
        CALL READI(5,IMAXCH)
        MAXCH=IMAXCH-1
        DO I=0,8191
          xx=rSPEC(ISP,I)
          IF(xx.LT.0)THEN
            NEGN1=NEGN1+1
            xNEGS1=xNEGS1+xx
          ENDIF
        ENDDO
        CALL FILL
        DO I=0,8191
          xx=rSPEC(IDEST,I)
          IF(xx.LT.0)THEN
            NEGN2=NEGN2+1
            xNEGS2=xNEGS2+xx
          ENDIF
        ENDDO
        cal(2,IDEST,1,1)=cal(2,ISP,1,1)
        cal(2,IDEST,1,2)=cal(2,ISP,1,2)
        cal(2,IDEST,1,3)=cal(2,ISP,1,3)
C Updating comment in the heading of spectrum file
        xcomm(1:3)='FN:'
        fname(2,IDEST)=fname(2,ISP)
        comm(2,IDEST)=comm(2,ISP)
        CALL AddComment(xcomm,3)
      ENDIF 

      WRITE(6,3)NEGN1,INT(xNEGS1)
  3   FORMAT(/'Before number of neg. ch. was:',I7,', with total counts:',I10)
      WRITE(6,4)NEGN2,INT(xNEGS2)
  4   FORMAT( 'After  number of neg. ch. is: ',I7,', with total counts:',I10)
      END
    

      SUBROUTINE FILL
      DIMENSION WEIGHT(-100:100,-100:100)
      INTEGER XDIM,YDIM,DELX,DELY
      REAL NEG, POS
      CHARACTER APP*4,ANS*1
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      CHARACTER answ*1
      ISP=1
      IF(IDEST.EQ.1)ISP=2

C Deleting very negative numbers first
      izero = 0
      IF(ITYPE.GT.1)THEN
        zmax = -1000000000.
        zmin =  1000000000.
        DO i = 0,XDIM-1
          DO j = 0,YDIM-1
            IF(rMAT(ISP,i,j).GT.zmax)zmax=rMAT(ISP,i,j)
            IF(rMAT(ISP,i,j).LT.zmin)zmin=rMAT(ISP,i,j)
          ENDDO
        ENDDO
        IF(zmin.GT.zmax)zmin=zmax
        WRITE(6,50)zmin,zmax
50      FORMAT(/,'Lowest and highest counts are (min,max) = (',E9.2,E9.2,')')
        answ='n'
        IF(zmin.LT.0.AND.ABS(zmin).GT.(0.1*ABS(zmax)))answ='y'
        WRITE(6,51)
51      FORMAT('You may now delete the most negative numbers before filling')
        WRITE(6,52)answ
52      FORMAT('Do you want to delete these negative counts     <',A1,'>:',$)
        CALL READA1(5,answ)
        IF(answ.EQ.'y'.OR.answ.EQ.'Y')THEN  
          izero = 1
          zmin = -(0.1*ABS(zmax))        
          WRITE(6,53)zmin
53        FORMAT('Counts < Limit will be deleted, Limit = <',E9.2,'>:',$)
          CALL READF(5,zmin)
        ENDIF
        IF(Istatus.NE.0)RETURN
      ELSE
        zmax = -1000000000.
        zmin =  1000000000.
        DO i = 0,MAXCH
            IF(rSPEC(ISP,i).GT.zmax)zmax=rSPEC(ISP,i)
            IF(rSPEC(ISP,i).LT.zmin)zmin=rSPEC(ISP,i)
        ENDDO
        IF(zmin.GT.zmax)zmin=zmax
        WRITE(6,60)zmin,zmax
60      FORMAT(/,'Lowest and highest counts are (min,max) = (',E9.2,E9.2,')')
        answ='n'
        IF(zmin.LT.0.AND.ABS(zmin).GT.ABS(zmax))answ='y'
        WRITE(6,61)
61      FORMAT('You may now delete the most negative numbers before filling')
        WRITE(6,62)answ
62      FORMAT('Do you want to delete these negative counts     <',A1,'>:',$)
        CALL READA1(5,answ)
        IF(answ.EQ.'y'.OR.answ.EQ.'Y')THEN  
          izero = 1
          zmin = -ABS(zmax)        
          WRITE(6,63)zmin
63        FORMAT('Counts < Limit will be deleted, Limit = <',E9.2,'>:',$)
          CALL READF(5,zmin)
        ENDIF
        IF(Istatus.NE.0)RETURN
      ENDIF

      IF(ITYPE.GT.1)THEN
        IXL=(XDIM/10.)+.5
        IXH=(XDIM-IXL)
        IYL=(YDIM/10.)+.5
        IYH=(YDIM-IYL)
        IF(IXL.LT.1)IXL=1
        IF(IXH.LT.1)IXH=1
        IF(IYL.LT.1)IYL=1
        IF(IYH.LT.1)IYH=1

        FWXL=1.
        FWXH=FWXL*SQRT(float(IXH/IXL))
        FWYL=1.
        FWYH=1.
        IF(XDIM.LT.2.OR.YDIM.LT.2)THEN
          WRITE(6,*)'Too small matrix'
          Istatus=1
          RETURN
        ENDIF
C New estimate from gamma-energies - if known (1MeV = 6% and 8MeV = 3 %)
        a0cal = cal(1,ISP,1,1)
        a1cal = cal(1,ISP,1,2)
        IF(a0cal + a1cal.NE.1)THEN
          Energ1 = a0cal + a1cal*FLOAT(IXL)
          Energ2 = a0cal + a1cal*FLOAT(IXH)
          fwh1 = 0.06 + ((0.03-0.06)*(Energ1-1000.)/(8000.-1000.))
          fwh2 = 0.06 + ((0.03-0.06)*(Energ2-1000.)/(8000.-1000.))
          FWXL = fwh1*(Energ1-a0cal)/a1cal
          FWXH = fwh2*(Energ2-a0cal)/a1cal
        ENDIF
      ELSE
        IXL=((MAXCH+1.)/10.)+.5
        IXH=((MAXCH+1.)-IXL)
        IF(IXL.LT.1)IXL=1
        IF(IXH.LT.1)IXH=1
        IYL=0
        IYH=0
        FWXL=1.
        FWXH=FWXL*SQRT(float(IXH/IXL))
        FWYL=0
        FWYH=0
        IF(MAXCH.LE.1)THEN
          WRITE(6,*)'Too short spectrum'
          Istatus=1
          RETURN
        ENDIF
C New estimate from gamma-energies - if known
        a0cal = cal(2,ISP,1,1)
        a1cal = cal(2,ISP,1,2)
        IF(a0cal+a1cal.NE.1)THEN
          Energ1 = a0cal + a1cal*FLOAT(IXL)
          Energ2 = a0cal + a1cal*FLOAT(IXH)
          fwh1 = 0.06 + ((0.03-0.06)*(Energ1-1000.)/(8000.-1000.))
          fwh2 = 0.06 + ((0.03-0.06)*(Energ2-1000.)/(8000.-1000.))
          FWXL = fwh1*(Energ1-a0cal)/a1cal
          FWXH = fwh2*(Energ2-a0cal)/a1cal
        ENDIF
      ENDIF

 
 34   CONTINUE
      WRITE(6,10)IXL,FWXL
 10   FORMAT(/'Write FWHMx (ch) around ch x= ',I4,' <',F6.1,'>:',$)
      CALL READF(5,FWXL)
      WRITE(6,11)IXH,FWXH
 11   FORMAT( 'Write FWHMx (ch) around ch x= ',I4,' <',F6.1,'>:',$)
      CALL READF(5,FWXH)
      IF(Istatus.NE.0)RETURN

      IF(ITYPE.GT.1)THEN
        WRITE(6,20)IYL,FWYL
 20     FORMAT(/'Write FWHMy (ch) around ch y= ',I4,' <',F6.1,'>:',$)
        CALL READF(5,FWYL)
        WRITE(6,21)IYH,FWYH
 21     FORMAT( 'Write FWHMy (ch) around ch y= ',I4,' <',F6.1,'>:',$)
        CALL READF(5,FWYH)
      ELSE
        FWYL=0.
        FWYH=0.
      ENDIF
      IF(Istatus.NE.0)RETURN
      
      IF(ITYPE.GT.1)THEN
C Finding parametrization of fwhm.: FWHM = A + B * SQRT(ch)
        AY=0
        BY=0
        IF(IXL.NE.IXH)THEN
          BX=(FWXL-FWXH)/(SQRT(float(IXL))-SQRT(float(IXH)))
        ELSE
          BX=0
        ENDIF
        AX=FWXL-BX*SQRT(float(IXL))
        IF(ITYPE.GT.1)THEN
          IF(IYL.NE.IYH)THEN
            BY=(FWYL-FWYH)/(SQRT(float(IYL))-SQRT(float(IYH)))
          ELSE
            BY=0
          ENDIF
          AY=FWYL-BY*SQRT(float(IYL))
        ENDIF
        WRITE(6,*)'FWHM have been expressed by A + B * SQRT(ch):'
        WRITE(6,22)AX,BX,AY,BY
 22     FORMAT('Ax=',F8.4,'  Bx=',F8.4,'     Ay=',F8.4,' By=',F8.4)

C Displaying probability matrix at (xl,yl) and (xh,yh)
        IX=IXL
        IY=IYL
        CALL GAUSSR(IX,IY,AX,BX,AY,BY,DELX,DELY,WEIGHT)
        WRITE(6,31)IX,IY
 31     FORMAT('Probability-matrix around (x,y)=(',I4,',',I4,'):')
        DO J=-4,4
          WRITE(6,30)(WEIGHT(I,J),I=-7,7)
        ENDDO

        IX=IXH
        IY=IYH
        IF(Istatus.NE.0)RETURN
        CALL GAUSSR(IX,IY,AX,BX,AY,BY,DELX,DELY,WEIGHT)
        WRITE(6,32)IX,IY
 32     FORMAT(/' Probability-matrix around (x,y)=(',I4,',',I4,'):')
        DO J=-4,4
          WRITE(6,30)(WEIGHT(I,J),I=-7,7)
        ENDDO
 30     FORMAT(1X,15F5.3)

        ANS='y'
        WRITE(6,33)ANS
 33     FORMAT(/,'Probability-matrix OK? (y/n) <',A1,'>:',$)
        CALL READA1(5,ANS)
        IF(ANS.EQ.'N'.OR.ANS.EQ.'n') GO TO 34
        IF(Istatus.NE.0)RETURN

C COPYING SOURCE SPECTRUM TO DESTINATION SPECTRUM
        DO J=0,2047
          DO I=0,4095
            rMAT(IDEST,I,J)=rMAT(ISP,I,J)
            IF(izero.EQ.1.AND.rMAT(IDEST,I,J).LT.zmin)rMAT(IDEST,I,J)=0
          ENDDO
        ENDDO

C   STARTING TO ELIMINATE NEGATIVE NUMBERS
        DO J=0,YDIM-1
          JT=(J/10)*10
          IF(JT.EQ.J)THEN
            write(6,FMT='(A1,$)')'.'
            call flush(6)
          ENDIF
          DO I=0,XDIM-1
            IT=(I/10)*10
            IF(IT.EQ.I.OR.JT.EQ.J)THEN
              CALL GAUSSR(I,J,AX,BX,AY,BY,DELX,DELY,WEIGHT)
            ENDIF
   40       NEG=rMAT(IDEST,I,J)
            IF(NEG.GE.0)GO TO 41


C FINDING CHANNEL WITH LARGEST WEIGHTED POSITIVE VALUE
            TEST=0.
            DO JJ=-DELY,DELY
              DO II=-DELX,DELX
                III=I+II
                JJJ=J+JJ
      IF(III.LT.0.OR.III.GE.XDIM.OR.JJJ.LT.0.OR.JJJ.GE.YDIM)THEN
                  GO TO 42
                ENDIF
                W=WEIGHT(II,JJ)*rMAT(IDEST,III,JJJ)
                IF(W.GT.TEST)THEN
                  TEST=W
                  POS=rMAT(IDEST,III,JJJ)
                  IP=III
                  JP=JJJ
                ENDIF
  42            CONTINUE
              ENDDO
            ENDDO
            IF(TEST.EQ.0)GO TO 41

C FILLING FROM CH (IP,JP) INTO (I,J)
            IF(ABS(NEG).GT.POS)THEN
              rMAT(IDEST,I,J)=NEG+POS
              rMAT(IDEST,IP,JP)=0
              GO TO 40
            ENDIF
            IF(ABS(NEG).LE.POS)THEN
              rMAT(IDEST,I,J)=0
              rMAT(IDEST,IP,JP)=POS+NEG
            ENDIF

  41      CONTINUE
          ENDDO
        ENDDO
      ELSE
C Finding parametrization of fwhm.: FWHM = A + B * SQRT(ch)
        J=0
        AY=0
        BY=0
        BX=(FWXL-FWXH)/(SQRT(float(IXL))-SQRT(float(IXH)))
        AX=FWXL-BX*SQRT(float(IXL))
        WRITE(6,*)'FWHM have been expressed by A + B * SQRT(ch):'
        WRITE(6,122)AX,BX
 122    FORMAT('Ax=',F8.4,'  Bx=',F8.4)

C Displaying probability matrix at (xl,yl) and (xh,yh)
        IX=IXL
        IY=IYL
        CALL GAUSSR(IX,IY,AX,BX,AY,BY,DELX,DELY,WEIGHT)
        WRITE(6,131)IX
 131    FORMAT('Probability-function around (x)=(',I4,'):')
        WRITE(6,130)(WEIGHT(I,0),I=-7,7)     
        IX=IXH
        IY=IYH
        IF(Istatus.NE.0)RETURN
        CALL GAUSSR(IX,IY,AX,BX,AY,BY,DELX,DELY,WEIGHT)
        WRITE(6,131)IX
        WRITE(6,130)(WEIGHT(I,0),I=-7,7)
 130    FORMAT(1X,15F5.3)
        ANS='y'
        WRITE(6,133)ANS
 133    FORMAT(/,'Probability-function OK? (y/n) <',A1,'>:',$)
        CALL READA1(5,ANS)
        IF(ANS.EQ.'N'.OR.ANS.EQ.'n') GO TO 34
        IF(Istatus.NE.0)RETURN

C COPYING SOURCE SPECTRUM TO DESTINATION SPECTRUM
        DO I=0,8191
          rSPEC(IDEST,I)=rSPEC(ISP,I)
          IF(izero.EQ.1.AND.rSPEC(IDEST,I).LT.zmin)rSPEC(IDEST,I)=0
        ENDDO

C   STARTING TO ELIMINATE NEGATIVE NUMBERS
        DO I=0,MAXCH
          IT=(I/10)*10
          IF(IT.EQ.I)THEN
            CALL GAUSSR(I,0,AX,BX,AY,BY,DELX,DELY,WEIGHT)
           ENDIF
  140     NEG=rSPEC(IDEST,I)
          IF(NEG.GE.0)GO TO 141

C FINDING CHANNEL WITH LARGEST WEIGHTED POSITIVE VALUE
          TEST=0. 
          DO II=-DELX,DELX
            III=I+II
            IF(III.LT.0.OR.III.GE.(MAXCH+1))GO TO 142
            W=WEIGHT(II,0)*rSPEC(IDEST,III)
            IF(W.GT.TEST)THEN
              TEST=W
              POS=rSPEC(IDEST,III)
              IP=III
            ENDIF
  142       CONTINUE
          ENDDO
          IF(TEST.EQ.0)GO TO 141

C FILLING FROM CH (IP) INTO (I)
          IF(ABS(NEG).GT.POS)THEN
            rSPEC(IDEST,I)=NEG+POS
            rSPEC(IDEST,IP)=0
            GO TO 140
          ENDIF
          IF(ABS(NEG).LE.POS)THEN
            rSPEC(IDEST,I)=0
            rSPEC(IDEST,IP)=POS+NEG
          ENDIF
  141     CONTINUE
        ENDDO
      ENDIF
      END

             

      SUBROUTINE FITan
C ROUTINE TO FIND BY LEAST SQUARE FIT THE EXPONENT N FROM  FIRST-GEN. SPECTRA.
C THE IDEA IS TO FIND THIS FOR ONE AND THE SAME LANDING REGION.
C THE EXPERIMENTAL GAMMA-SPECTRA ARE FIRST NOMALIZED T, AND
C THEN A LINEAR TRANSFORMATION IS MADE WITH Ex->Ex AND Ey->
C OF EACH Ey IS NORMALIZED TO A FUNCTION F(Ey), BEFORE THE FIT OF A(1)Ex**n
C IS MADE. THE SPECTRUM R(I,J) IS USED AS HELP-SPECTRUM, AND WILL BE ZEROED.

      INTEGER XDIM,YDIM,RDIM,MX,MY,SX,SY
      CHARACTER APP*4
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,xcomm*60
      COMMON/CALxy/AX0,AX1,AY0,AY1
      COMMON/CNUTE/EX,EXL,EXH,XJ,EGAP,MASS,EXPN(0:4095),N(0:4095),FWXG
      COMMON/response1/R(0:2047,0:2047),RDIM,A0,A1,FWHM
      REAL N,MASS,NC,P(2),FEX(0:4095),AI(4096),NI(4096),EEGG(4096)
      EXTERNAL FUNC2,FUNC3,FUNC5
      DIMENSION FIT(4096)
      DIMENSION X(4096),Y(4096),SIGMAY(4096),
     +A(5),DELTAA(5),SIGMAA(5),CH(500)

      IDUM=1
      IF(IDEST.EQ.1)IDUM=2
      IDEST=IDUM
      WRITE(6,1)IDEST
 1    FORMAT('Dest. spectr. for manipulated data <',I1,'>:',$)
      CALL READI(5,IDEST)
      ISP=1
      IF(IDEST.EQ.1)ISP=2
      WRITE(6,3)ISP
 3    FORMAT( 'Source spectrum                    <',I1,'>:',$)
      CALL READI(5,ISP)
      IF(IDEST.LT.1.OR.IDEST.GT.2)Istatus=1
      IF(ISP  .LT.1.OR.ISP  .GT.2)Istatus=1

      IF(ISP.EQ.IDEST)THEN
        WRITE(6,*)'Destination must be another spectrum'
        Istatus=1
        RETURN
      ENDIF
      IF(Istatus.NE.0)RETURN
      XDIM=Idim(1,ISP,1)
      YDIM=Idim(1,ISP,2)
      MASS=172.
      EGAP=1830.
      NC=4.0
      XJ=4.
      FWXG=500.
      NA=2
      JTYPE=1
      IANS=0

C ZEROING SPECTRA
  999 CONTINUE
      WRITE(6,*)'Zeroing destination spectrum and response matrix'
      DO J=0,2047
        DO I=0,4095
          rMAT(IDEST,I,J)=0
        ENDDO
      ENDDO
      DO J=0,2047
        DO I=0,2047
          R(I,J)=0.
        ENDDO
      ENDDO


C FIRST NORMALIZE SOURCE SPECTRUM AND PUT IT TEMPOR. IN IDEST
      WRITE(6,*)'Deleting negative counts and normalizing'
      DO J=0,YDIM-1
        SUM=0
        DO I=0,XDIM-1
          IF(rMAT(ISP,I,J).GT.0)SUM=SUM+rMAT(ISP,I,J)
        ENDDO
        DO I=0,XDIM-1
          IF(SUM.NE.0.AND.rMAT(ISP,I,J).GE.0)THEN
            rMAT(IDEST,I,J)=rMAT(ISP,I,J)/SUM
          ELSE
            rMAT(IDEST,I,J)=0
          ENDIF
        ENDDO
      ENDDO
      IF(IANS.EQ.1)GO TO 9991

C READING IN VARIOUS PARAMETERS TO BE USED
      bx=cal(1,ISP,1,1)+cal(1,ISP,1,2)+cal(1,ISP,1,3)
      by=cal(1,ISP,2,1)+cal(1,ISP,2,2)+cal(1,ISP,2,3)
      IF(bx+by.EQ.2.)THEN
        AX0=11.
        AX1=20.
        AY0=9660.
        AY1=-120.
      ELSE
        AX0=cal(1,ISP,1,1)
        AX1=cal(1,ISP,1,2)
        AY0=cal(1,ISP,2,1)
        AY1=cal(1,ISP,2,2)
      ENDIF

      FACX=0.
      FACY=-1.
      WRITE(6,*)' '
      WRITE(6,*)'The following default values correspond to'
      WRITE(6,*)'an experiment on 172Yb from june 1989.'
      WRITE(6,10)AX0
  10  FORMAT(/'Cal. coeff. a0 (keV) on x-axis   <',F8.1,'>:',$)
      CALL READF(5,AX0)
      WRITE(6,11)AX1
  11  FORMAT( 'Cal. coeff. a1 (keV/ch) on x-axis<',F8.1,'>:',$)
      CALL READF(5,AX1)

      WRITE(6,12)AY0
  12  FORMAT(/'Cal. coeff. a0 (keV) on y-axis   <',F8.1,'>:',$)
      CALL READF(5,AY0)
      WRITE(6,13)AY1
  13  FORMAT( 'Cal. coeff. a1 (keV/ch) on y-axis<',F8.1,'>:',$)
      CALL READF(5,AY1)

      WRITE(6,*)'Give limits for the transformation of the matrix'
      EXUPPER=8000.0
      WRITE(6,14)EXUPPER
  14  FORMAT(/'Give upper energy on x-axis (keV)<',F8.1,'>:',$)
      CALL READF(5,EXUPPER)
      EYUPPER=EXUPPER
      WRITE(6,15)EYUPPER
  15  FORMAT( 'Give upper energy on y-axis (keV)<',F8.1,'>:',$)
      CALL READF(5,EYUPPER)
      IF(Istatus.NE.0)RETURN
     
      IUPPER=((EXUPPER-AX0)/AX1+0.5)
      JUPPER=((EYUPPER-AY0)/AY1+0.5)
      ILOWER=(-AX0/AX1+0.5)
      JLOWER=(-AY0/AY1+0.5)
      IF(ILOWER.GT.IUPPER)THEN
        I=ILOWER
        ILOWER=IUPPER
        IUPPER=I
      ENDIF
      IF(JLOWER.GT.JUPPER)THEN
        J=JLOWER
        JLOWER=JUPPER
        JUPPER=J
      ENDIF

      IF(IUPPER.GT.2047)IUPPER=2047
      IF(JUPPER.GT.2047)JUPPER=2047
      IF(ILOWER.LT.0  )ILOWER=0
      IF(JLOWER.LT.0  )JLOWER=0
      WRITE(6,16)ILOWER,IUPPER,JLOWER,JUPPER
  16  FORMAT(/' Data taken from Chx= ',I4,'-',I4,' and Chy= ',I4,'-',I4,/)
      WRITE(6,*)'Transforming Ex -> Ex and Ey -> Ey - Ex'
9991  CONTINUE
      DO J=JLOWER,JUPPER
        DO I=ILOWER,IUPPER
          EEX=AX0+AX1*I
          EEY=AY0+AY1*J
          EX=EEX+FACX*EEY
          EY=EEY+FACY*EEX

C WE NOW DISTRIBUTE THE COUNTS INTO APPROPRIATE CHANNELS, GENERALLY 4.
C FINDING FIRST THE MAIN CHANNEL (MX,MY) AND PUT SOME FRACTION INTO THE 3
C OTHER SIDE-CHANNELS (SX,MY), (MX,SY) AND (SX,SY)
          CX=(EX-AX0)/AX1
          CY=(EY-AY0)/AY1
          MX=CX+0.5
          MY=CY+0.5
          FX=CX-MX
          FY=CY-MY
          SX=MX+1
          SY=MY+1
          IF(FX.LT.0)SX=MX-1
          IF(FY.LT.0)SY=MY-1
          FX=ABS(FX)
          FY=ABS(FY)

C CALCU THAT A SQUARE AROUND (Ex,Ey) COVERS THE 4 CH.
          AX =FX     *(1.-FY)
          AY =(1.-FX)*FY
          AXY=FX     *FY

C CALCULATING THE NUMBER OF COUNTS TO BE PUT IN
          ITOT=rMAT(IDEST,I,J)
          IX =(AX *ITOT)
          IY =(AY *ITOT)
          IXY=(AXY*ITOT)
          IM=ITOT-IX-IY-IXY

          IF(MX.GE.0.AND.MX.LE.2047.AND.MY.GE.0.AND.MY.LE.2047)THEN
            R(MX,MY)=R(MX,MY)+IM
            IF(SX.GE.0.AND.SX.LE.2047)
     +      R(SX,MY)=R(SX,MY)+IX
            IF(SY.GE.0.AND.SY.LE.2047 )
     +      R(MX,SY)=R(MX,SY)+IY
          ENDIF
         IF(SX.GE.0.AND.SX.LE.2047.AND.SY.GE.0.AND.SY.LE.2047)THEN
            R(SX,SY)=R(SX,SY)+IXY
          ENDIF
        ENDDO
      ENDDO

C PUTTING IT TEMPORARELY INTO MATRIX rMAT(IDEST,I,J) AND ZEROING R(I,J)
      DO J=0,2047
        DO I=0,2047
          rMAT(IDEST,I,J)=R(I,J)
          R(I,J)=0
        ENDDO
      ENDDO

C CALCULATING THE NORMALIZ. FUNCTION F(Ex). WE HAVE P(Ex,Eg)= F(Ex)*
C Eg**n * Rho(Ex-Eg). FOR EXC. ENERGIES BELOW Egap, WE ESTIMATE THE
C MISSING FRACTION FROM THE EXPERIMENTAL SPECTRUM
      IF(IANS.EQ.1)GO TO 131
      WRITE(6,*)'Give values to be used for the Fermi-gas estimate'
      WRITE(6,17)MASS
  17  FORMAT(/'Mass number',13X,'                    <',F4.0,'>:',$)
      CALL READF(5,MASS)
      GAP=1000.*2.*12.*MASS**(-0.5)
      WRITE(6,18)GAP,EGAP
  18  FORMAT(/'Pairing gap (2*12/sqrt(A)=',F5.0,')',12X,'<',F5.0,'>:',$)
      CALL READF(5,EGAP)
      EXMIN=EGAP+200.
      WRITE(6,*)'The highest gamma-energy to be used is'
      WRITE(6,*)'limited by the final exc. energy (Ex-Eg).'
      WRITE(6,*)'This value must be equal or higher than Egap.'
      WRITE(6,19)EXMIN
  19  FORMAT(/'Lower final exc. energy (keV) >=Egap <',F6.0,'>:',$)
      CALL READF(5,EXMIN)
      EXMINF=EXMIN+200.

      WRITE(6,20)XJ
  20  FORMAT(/'Give average spin populated',18X,'<',F4.1,'>:',$)
      CALL READF(5,XJ)

      WRITE(6,21)FWXG
  21  FORMAT(/'Energy FWHM    for Ex-Eg (keV)',12X,'<',F6.1,'>:',$)
      CALL READF(5,FWXG)

  131 CONTINUE
      WRITE(6,22)NC
  22  FORMAT(/'Exponent n                 <',F4.2,'>:',$)
      CALL READF(5,NC)
      IF(NC.GT.0)THEN
        DO I=0,4095
           N(I)=NC
           EXPN(I)=(AX0+FLOAT(I)*AX1)**NC
        ENDDO
      ELSE
        WRITE(6,*)'Exponent must be positive'
        GO TO 131
      ENDIF
      AA=MASS/8000.
      WRITE(6,23)AA
  23  FORMAT( 'Level density parameter a <',F6.3,'>:',$)
      CALL READF(5,AA)
      IF(Istatus.NE.0)RETURN
      P(2)=AA
      P(1)=1.

C CALCULATING F(Ex)
      FRGAS=0.01
      SUM=0
      DO J=JLOWER,JUPPER
        EX =J*AY1+AY0
        EXL=EX-FWXG/2.
        EXH=EX+FWXG/2.
        IF(EXL.LT.0)EXL=0.
        IMAX=((EX-EXMIN-AX0)/AX1+0.5)
        FEX(J)=0.
        GAS=0.
        GSB=0.
        DO I=0,IMAX
          XX=AX1*I+AX0
          IF(XX.GT.500.)FEX(J)=FEX(J)+FUNC1(XX,P)
          IF(XX.GT.500.)GAS=GAS+rMAT(ISP,I,J)
        ENDDO
        DO I=IMAX+1,IUPPER
          IF(XX.GT.500.)GSB=GSB+rMAT(ISP,I,J)
        ENDDO
C CORRECT FOR DECAY TO STATES BELOW FERMI-GAS. (TAKEN FROM EXP. SPECTRA)
        FRAC=0.
        IF(GAS.GT.0.)FRAC=GSB/GAS
        FRAC=1.+FRAC
        FEX(J)=FEX(J)*FRAC

C FINDING FRACTION IN FERMI GAS. WEIGHTED WITH CHANNELS IN FERMI GAS
        IF(GAS+GSB.GT.0)THEN
          SUM=SUM+IMAX
           FRGAS=FRGAS+FLOAT(IMAX)*GAS/(GAS+GSB)
        ENDIF
      ENDDO
      IF(SUM.GT.0)FRGAS=FRGAS/SUM
      WRITE(6,25)FRGAS
  25  FORMAT(/'Average  weight in Fermi gas region <',F6.4,'>:',$)
      CALL READF(5,FRGAS)

      DO J=JLOWER,JUPPER
        EX=J*AY1+AY0
        FEX(J)=FEX(J)/FRGAS
        IF(FEX(J).GT.0)WRITE(6,24)J,EX,FEX(J)
  24    FORMAT('y ch.=',I5,' Exc.=',F5.0,
     +    ' F(Ex)=',E12.5)
      ENDDO

C CORRECT FOR F(Ex) AND GET SOMETHING PROPORTIONAL TO Eg**N
      DO J=JLOWER,JUPPER
        DO I=ILOWER,IUPPER
          EG= FLOAT(I)*AX1+AX0
          EXF=FLOAT(J)*AY1+AY0
          EXI=EXF+EG
          FAC=0.
          XX=(EXI-AY0)/AY1
          IF(XX.GT.0.AND.XX.LT.2047.)THEN
            I1=XX
            I2=I1+1
            FAC=FEX(I1)+(FEX(I2)-FEX(I1))*(I1-XX)
          ENDIF
          IF(FAC.LE.0)FAC=0.
          R(I,J)=rMAT(IDEST,I,J)*FAC
        ENDDO
      ENDDO
  99  CONTINUE


C BLOCKS FOR EITHER FIT n ON X-AXIS (JTYPE=1) OR a ON Y-AXIS (JTYPE=2)
      IF(JTYPE.EQ.1)THEN

C ZEROING DESTINATION MATRIX
      DO J=0,2047
        DO I=0,4095
          rMAT(IDEST,I,J)=0
        ENDDO
      ENDDO

C PREPARING FOR FIT. FINDING LIMITS FOR FIT
        IMIN=((800.-AX0)/AX1+0.5)
        EGMIN=AX1*IMIN+AX0
        WRITE(6,101)EGMIN,IMIN
 101    FORMAT(/'Lower ch. for fit on x-axis (Eg=',F5.0,
     +  ' keV)   <',I4,'>:',$)
        CALL READI(5,IMIN)
        EGMIN=AX1*IMIN+AX0
 
 103    CONTINUE
        WRITE(6,102)EXMINF
 102    FORMAT( 'Lower final excitation energy (keV) >=Egap <',F6.0,
     +  '>:',$)
        CALL READF(5,EXMINF)
        IF(EXMINF.LT.EXMIN)THEN
          WRITE(6,104)EXMIN
 104      FORMAT('Too low, must be higher or equal ',F7.1, 'keV')
          GO TO 103
        ENDIF

        JMIN=((EXMINF-AY0)/AY1+0.5)
        JMAX=((EYUPPER-EGMIN-AY0)/AY1+0.5)
        IF(JMIN.GT.JMAX)THEN
          JWAIT=JMIN
          JMIN=JMAX
          JMAX=JWAIT
        ENDIF
        JMIN=JMIN+4
        IF(JMIN.LT.0)JMIN=0
        IF(JMAX.GE.YDIM-1)JMAX=YDIM-1

        EYMIN=AY1*JMIN+AY0
        EYMAX=AY1*JMAX+AY0
        WRITE(6,*)'Spectra to be fitted, give low and high y-channel'
        WRITE(6,105)EYMIN,JMIN
 105    FORMAT(/'Lower ch. (excit. ',F8.2,' keV)  <',I4,'>:',$)
        CALL READI(5,JMIN)
        WRITE(6,106)EYMAX,JMAX
 106    FORMAT( 'Higher ch. (excit. ',F8.2,' keV) <',I4,'>:',$)
        CALL READI(5,JMAX)
        IF(JMIN.GT.JMAX)THEN
          WRITE(6,*)'Lower ch. > Higher ch.'
          Istatus=1
          RETURN
        ENDIF
        IF(Istatus.NE.0)RETURN

C ALL LIMITS NOW DETERMINED FOR FIT. STARTING THE FIT PROCEDURE
 132    A1=3.
        A2=NC
        WRITE(6,107)A2
 107    FORMAT(/'Starting value for exponent n  <',F5.2,'>:',$)
        CALL READF(5,A2)

        WRITE(6,108)NA
 108    FORMAT( 'Number of parameters in fit (max=2)<',I1,'>:',$)
        CALL READI(5,NA)
        IF(Istatus.NE.0)RETURN

        cal(1,ISP,1,1)=  AX0
        cal(1,ISP,1,2)=  AX1
        cal(1,ISP,2,1)=  AY0
        cal(1,ISP,2,2)=  AY1
        cal(1,IDEST,1,1)=AX0
        cal(1,IDEST,1,2)=AX1
        cal(1,IDEST,2,1)=AY0
        cal(1,IDEST,2,2)=AY1

C*********************************************************************
C   FITIING LOOP STARTS
C*********************************************************************
 109    PROG=0.0001
        MODE=1
        DO J=JMIN,JMAX
          M=0
          A(1)=A1
          A(2)=A2
          EGMAX=EYUPPER-(J*AY1+AY0)
          IMAX=((EGMAX-AX0)/AX1+0.5)

C SETTING UP THE HISTOGRAM TO BE FITIED, BOTH X(I) AND Y(I)
          EXF=J*AY1+AY0
          IF(IMAX.LE.IMIN+NA+1)THEN
            WRITE(6,*)'Too few data for fit'
            GO TO 100
          ENDIF
          NPTS=0
          THRSX=10.E+10
          THRSY=10.E+10
          DO I=IMIN,IMAX
            IF(R(I,J).GT.0)THEN
              NPTS=NPTS+1
              EGG=AX1*I+AX0
              EEGG(NPTS)=EGG
              X(NPTS)=ALOG(EGG)
              Y(NPTS)=ALOG(R(I,J))
              EXL=EXF-FWXG/2.
              EXH=EXF+FWXG/2.

              IF(FUNC5(P).NE.0)THEN
                NI(NPTS)=ALOG(R(I,J)/FUNC5(P))/ALOG(EGG)
              ELSE
                NI(NPTS)=0
              ENDIF

              U=(FLOAT(J)*AY1+AY0)-EGAP-300.
              IF(U.GT.100.) THEN
                DIVID=EXPN(I)*(2.*XJ+1.)*SQRT(P(2))
               AI(NPTS)=((ALOG(R(I,J)*U*U/DIVID))**2)/(4.*U)
              ELSE
                AI(NPTS)=0
              ENDIF

              SIGMAY(NPTS)=0.1
              IF(THRSX.GT.X(NPTS))THRSX=X(NPTS)
              IF(THRSY.GT.Y(NPTS))THRSY=Y(NPTS)
            ENDIF
          ENDDO

C SHIFTING THE DATA-POINTS DOWN TO ORIGO
          DO I=1,NPTS
            X(I)=X(I)-THRSX
            Y(I)=Y(I)-THRSY
           ENDDO

C ESTIMATE PARAMETERS TO BE FITIED
          DO I=1,2
            SIGMAA(I)=A(I)/10.
            DELTAA(I)=A(I)/10.
          ENDDO
          CHISQR=999999.
          BEST  =999999.
          NBAD  =0

C     STARTING FIT **********************************************
          WRITE(6,120)
 120      FORMAT('  LOOP     CHISQR       A(1)         A(2)')
          IF(M.LT.4)WRITE(6,*)' '
          WRITE(6,121)M,CHISQR,(A(JJ),JJ=1,NA)
 123      CALL GRIDLS(X,Y,SIGMAY,NPTS,MODE,FUNC2,A
     +    ,DELTAA,SIGMAA,NA,FIT,CHISQR)
          M=M+1
          WRITE(6,121)M,CHISQR,(A(JJ),JJ=1,NA)
 121      FORMAT(I4,3(1X,E12.5))
          CH(M)=CHISQR
          IF(M.LT.2) GO TO 123
          VER=ABS(CH(M)-CH(M-1))/CH(M-1)
          IF(CHISQR.LT.BEST)THEN
            BEST=CHISQR
            AA1=A(1)
            AA2=A(2)
            SIGAA1=SIGMAA(1)
            SIGAA2=SIGMAA(2)
          ENDIF
          IF(CH(M).GT.CH(M-1))NBAD=NBAD+1
          IF(NBAD.GT.4)GO TO 124
          IF(M.GE.500)GO TO 124
          IF(VER.GT.PROG) GO TO 123
 124      CONTINUE

C     FIT IS FINISHED *********************************************
          A(1)=AA1
          SIGMAA(1)=SIGAA1
          A(2)=AA2
          SIGMAA(2)=SIGAA2

          WRITE(6,111)
 111      FORMAT('       Eg     ln(Eg)    ln(Q/N)      Fit   ',
     +    ' n=ln... a=(ln..)**2')
          AVN=0
          AVA=0
          SUMN=0
          SUMA=0
          DO I=1,NPTS                           
            X1=X(I)+THRSX
            Y1=Y(I)+THRSY
            F1=FIT(I)+THRSY
            WRITE(6,110)EEGG(I),X1,Y1,F1,NI(I),AI(I)
             IF(NI(I).GT.0)THEN
              SUMN=SUMN+1
              AVN=AVN+NI(I)
            ENDIF
            IF(AI(I).GT.0)THEN
              SUMA=SUMA+1
              AVA=AVA+AI(I)
            ENDIF
          ENDDO
          AVN=AVN/SUMN
          AVA=AVA/SUMA
 110      FORMAT(F10.1,3F10.3,2F10.5)
          WRITE(6,112)AVN,AVA
 112      FORMAT(' (Average n and a from ln-calc.:',8X,2F10.5,')')

          WRITE(6,129)J,EXF,IMIN,IMAX,BEST
 129       FORMAT(/' Results for y-ch:',I3,' (Exf=',F6.0,
     +    ' keV) and x-chs:',I3,' -',I3,',  Chisqr.=',F8.3)
          WRITE(6,128)
 128      FORMAT(3('--------------------------'))
          AXX=A(1)+THRSX
          WRITE(6,127)  AXX,SIGMAA(1)
          WRITE(6,126)  A(2),SIGMAA(2)
 127      FORMAT(' Normalisation parameter A(1)=',E10.4,' +/-',E10.4)
 126      FORMAT(' Exponent n              A(2)= ',F9.5,' +/-',F9.5)
          WRITE(6,128)

C PUTTING THE DATA INTO MATRIX IDEST. MULTIPLYING X AND Z
C WITH 25 AND 250, RESPECTIVELY
          DO I=0,XDIM-1
            DIF=50.
            DO II=1,NPTS
              DIFX=ABS(FLOAT(I)-25.*X(II))
              IF(DIFX.LT.DIF)THEN
                DIF=DIFX
                I1=II
              ENDIF
            ENDDO
            IF(25.*X(I1).GT.I)I1=I1-1
            I2=I1+1
            IF(I1.GE.1.AND.I2.LE.NPTS)THEN
              X1=25.*X(I1)
              X2=25.*X(I2)
              rMAT(IDEST,I,J)=250.*
     +        (Y(I1)+(Y(I2)-Y(I1))*(I-X1)/(X2-X1))
             ENDIF
          ENDDO

 100      IANS=0
          WRITE(6,*)'Next y-ch. with same parameters:  0'
          WRITE(6,*)'New F(Ex)-function                1'
          WRITE(6,*)'New initial fit parameters:       2'
          WRITE(6,*)'Fit parameter a on Y-axis:        3'
          WRITE(6,*)'Return to main menu:              4'
          WRITE(6,130)IANS
  130      FORMAT(/'Please, type your choice (0,,,,4) <',I1,'>:',$)
          CALL READI(5,IANS)
          IF(IANS.EQ.1)GO TO 999
          IF(IANS.EQ.2)GO TO 132
          IF(IANS.EQ.3)THEN
            JTYPE=2
            GO TO 99
          ENDIF
          IF(IANS.EQ.4)GO TO 64
        ENDDO

      ELSE

C       PREPARING FOR FIT. FINDING LIMITS FOR FIT
C ZEROING DESTINATION MATRIX
        DO J=0,2047
          DO I=0,4095
            rMAT(IDEST,I,J)=0
          ENDDO
        ENDDO

        IMIN=((800.-AX0)/AX1+0.5)
        EGMIN=AX1*IMIN+AX0
        WRITE(6,201)EGMIN,IMIN
 201    FORMAT(/'Lower ch. on x-axis (Eg=',F5.0,
     +  ' keV)           <',I4,'>:',$)
        CALL READI(5,IMIN)
        EGMIN=AX1*IMIN+AX0

        JJ=JMIN
        IF(AY1.LT.0)JJ=JMAX
        EGMAX=EYUPPER-(JJ*AY1+AY0)
        IMAX=((EGMAX-AX0)/AX1+0.5)
        EGMAX=AX1*IMAX+AX0
        WRITE(6,202)EGMAX,IMAX
 202    FORMAT( 'Higher ch. on x-axis (Eg=',F5.0,
     +  ' keV)          <',I4,'>:',$)
        CALL READI(5,IMAX)
        EGMAX=AX1*IMAX+AX0

 203    CONTINUE
        JJ1=((EXMINF-AY0)/AY1+0.5)
        WRITE(6,204)EXMINF,JJ1
 204    FORMAT(/'Y-ch. for lower exc. energy ',F8.2,
     +  ' keV)    <',I4,'>:',$)
        CALL READI(5,JJ1)
        YTEST=AY1*JJ1+AY0
        IF(YTEST.LT.EXMIN)THEN
          WRITE(6,205)EXMIN
 205      FORMAT('Too low energy, must be higher than ',F7.1, 'keV')
          GO TO 203
        ENDIF
 

 232    CONTINUE
        A2=AA
        WRITE(6,207)A2
 207    FORMAT(/'Starting value for level density param. a  <',
     +  F5.3,'>:',$)
        CALL READF(5,A2)

        WRITE(6,208)NA
 208    FORMAT(/'Number of parameters in fit (max=2)',12X,'<',I1,'>:',$)
        CALL READI(5,NA)
        IF(Istatus.NE.0)RETURN

C********************************************************************
C   FITIING LOOP STARTS
C********************************************************************
        PROG=0.0001
        MODE=1
        DO I=IMIN,IMAX
          M=0
        
C SETTING UP THE HISTOGRAM TO BE FITIED, BOTH X(I) AND Y(I)
          EG=I*AX1+AX0
          JJ2=((EYUPPER-EG-AY0)/AY1+0.5)
          J1=MIN(JJ1,JJ2)
          J2=MAX(JJ1,JJ2)
          IF(J1.LT.0)J1=0
          IF(J2.GE.YDIM-1)J2=YDIM-1

          IF(J2.LE.J1+NA+1)THEN
            WRITE(6,*)'Too few data for fit'
            GO TO 200
          ENDIF
          NPTS=0

          DO J=J1,J2

            IF(R(I,J).GT.0)THEN
              NPTS=NPTS+1
              EGG=AX1*I+AX0
              EXF=FLOAT(J)*AY1+AY0
              X(NPTS)=EXF
              Y(NPTS)=ALOG(R(I,J))
              EXL=EXF-FWXG/2.
              EXH=EXF+FWXG/2.

              IF(FUNC5(P).NE.0)THEN
                NI(NPTS)=ALOG(R(I,J)/FUNC5(P))/ALOG(EGG)
              ELSE
                NI(NPTS)=0
              ENDIF

              U=(FLOAT(J)*AY1+AY0)-EGAP-300.
               IF(U.GT.100.) THEN
                DIVID=EXPN(I)*(2.*XJ+1.)*SQRT(P(2))
               AI(NPTS)=((ALOG(R(I,J)*U*U/DIVID))**2)/(4.*U)
              ELSE
                AI(NPTS)=0
              ENDIF

              SIGMAY(NPTS)=.1
              IF(THRSX.GT.X(NPTS))THRSX=X(NPTS)
              IF(THRSY.GT.Y(NPTS))THRSY=Y(NPTS)
            ENDIF

          ENDDO

          A(1)=1.
          A(2)=AA
          EXPSUM=0.
          TEOSUM=0.
          DO JJ=1,NPTS
            EXPSUM=EXPSUM+Y(JJ)
            XX=X(JJ)
            TEOSUM=TEOSUM+FUNC3(XX,A)
          ENDDO
          XPT=NPTS
          XPTLNA1=TEOSUM-EXPSUM
          A(1)=1./EXP(XPTLNA1/XPT)

C ESTIMATE PARAMETERS TO BE FITIED
          DO J=1,2
            SIGMAA(J)=A(J)/10.
            DELTAA(J)=A(J)/10.
          ENDDO
          CHISQR=999999.
          BEST  =999999.
          NBAD  =0

C     STARTING FIT **********************************************
          WRITE(6,220)
 220      FORMAT('  LOOP     CHISQR       A(1)         A(2)')
          IF(M.LT.4)WRITE(6,*)' '
          WRITE(6,221)M,CHISQR,(A(JJ),JJ=1,NA)
 223      CALL GRIDLS(X,Y,SIGMAY,NPTS,MODE,FUNC3,A
     +    ,DELTAA,SIGMAA,NA,FIT,CHISQR)
          M=M+1
          WRITE(6,221)M,CHISQR,(A(JJ),JJ=1,NA)
 221      FORMAT(I4,3(1X,E12.5))
          CH(M)=CHISQR
          IF(M.LT.2) GO TO 223
          VER=ABS(CH(M)-CH(M-1))/CH(M-1)
          IF(CHISQR.LT.BEST)THEN
            BEST=CHISQR
            AA1=A(1)
            AA2=A(2)
            SIGAA1=SIGMAA(1)
            SIGAA2=SIGMAA(2)
          ENDIF
          IF(CH(M).GT.CH(M-1))NBAD=NBAD+1
           IF(NBAD.GT.4)GO TO 224
          IF(M.GE.500) GO TO 224
          IF(VER.GT.PROG) GO TO 223
 224      CONTINUE

C     FIT IS FINISHED *********************************************
          A(1)=AA1
          SIGMAA(1)=SIGAA1
          A(2)=AA2
          SIGMAA(2)=SIGAA2

          WRITE(6,211)
 211      FORMAT('     Exf     ln(Q/N)      Fit   ',
     +    ' n=ln... a=(ln..)**2')
          AVN=0
          AVA=0
          SUMN=0
          SUMA=0
          DO J=1,NPTS
            WRITE(6,110)X(J),Y(J),FIT(J),NI(J),AI(J)
            IF(NI(J).GT.0)THEN
              SUMN=SUMN+1
              AVN=AVN+NI(J)
            ENDIF
            IF(AI(J).GT.0)THEN
              SUMA=SUMA+1
              AVA=AVA+AI(J)
            ENDIF
          ENDDO
          AVN=AVN/SUMN
          AVA=AVA/SUMA
 210      FORMAT(F10.1,2F10.3,2F10.5)
          WRITE(6,212)AVN,AVA
 212      FORMAT(' (Aver. n and a from ln-calc.:',2F10.5,')')

          WRITE(6,229)I,EG,J1,J2,BEST
 229       FORMAT(/' Results for x-ch:',I3,' (Eg =',F6.0,
     +    ' keV) and y-chs:',I3,' -',I3,',  Chisqr.=',F8.3)
          WRITE(6,228)
 228      FORMAT(3('--------------------------'))
          WRITE(6,227)  A(1),SIGMAA(1)
          WRITE(6,226)  A(2),SIGMAA(2)
 227      FORMAT(' Normalisation parameter A(1)=',E10.4,' +/-',E10.4)
 226      FORMAT(' Level density param. a  A(2)= ',F9.5,' +/-',F9.5)
          WRITE(6,228)

C PUTTING THE DATA INTO MATRIX IDEST

          DO II=1,NPTS
            JJ=((X(II)-AY0)/AY1)+0.5
            rMAT(IDEST,I,JJ)=Y(II)
          ENDDO

 200      IANS=0
          WRITE(6,*)'Next x-ch. with same parameters:  0'
          WRITE(6,*)'New F(Ex)-function:               1'
          WRITE(6,*)'New start fit parameters:         2'
          WRITE(6,*)'Fit parameter n on x-axis:        3'
          WRITE(6,*)'Return to main menu:              4'
          WRITE(6,230)IANS
 230      FORMAT(/'Please, type your choice (0,,,,4) <',I1,'>:',$)
          CALL READI(5,IANS)
          IF(IANS.EQ.1)GO TO 999
          IF(IANS.EQ.2)GO TO 232
          IF(IANS.EQ.3)THEN
            JTYPE=1
            GO TO 99
          ENDIF
          IF(IANS.EQ.4)GO TO 64
        ENDDO

      ENDIF

C RESETTING HELPING SPECTRUM R(I,J) TO ZERO
  64  CONTINUE
      DO J=0,2047
        DO I=0,2047
          R(I,J)=0.
        ENDDO
      ENDDO
      RDIM=0
C Updating comment in the heading of spectrum file
      xcomm(1:3)='AN:'
      fname(1,IDEST)(1:8)='AN'//fname(1,ISP)(1:6)
      comm(1,IDEST)=comm(1,ISP)
      CALL AddComment(xcomm,3)

      RETURN
      END



      SUBROUTINE GAUSSR(IX,IY,AX,BX,AY,BY,DELX,DELY,WEIGHT)
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      CHARACTER APP*4
      INTEGER XDIM,YDIM,DELX,DELY
      DIMENSION WEIGHT(-100:100,-100:100)

      DO I=-100,100
        DO J=-100,100
          WEIGHT(I,J)=0.
        ENDDO
      ENDDO

      FWX=AX+BX*SQRT(float(IX))+0.5
      FWY=AY+BY*SQRT(float(IY))+0.5
      IF(FWX.LT.0)FWX=0.
      IF(FWY.LT.0)FWY=0.
      SX=FWX/2.35             !SIGMAx
      SY=FWY/2.35             !SIGMAy
      XN=1.4142*SX            !sqrt(2)=1.4142
      YN=1.4142*SY
      DELX=3.03*SX+0.5        !3.03 gives exp(-(DELX/(1.4142*SX))**2)=0.010
      DELY=3.03*SY+0.5        !which means walking out to 0.010 of max = 1
      IF(DELX.GT.100)DELX=100 !3.25 gives 0.005 and 3.72 gives 0.001
      IF(DELY.GT.100)DELY=100

      SUM=0

      DO I=-DELX,DELX
        X1=0.
        IF(XN.NE.0.)X1=(FLOAT(I)/XN)**2
        DO J=-DELY,DELY
          Y1=0.
          IF(YN.NE.0.)Y1=(FLOAT(J)/YN)**2
          XY1=X1+Y1
          IF(XY1.LT.4.605.AND.XY1.GT.0.0001)THEN
            H=EXP(-XY1)                             ! 4.605 gives H=0.010
          ELSE                                      ! 5.298 gives H=0.005
            H=0.                                    ! 6.907 gives H=0.001
          ENDIF
          IF(XY1.LE.0.0001)H=1
          WEIGHT(I,J)=H
          SUM=SUM+H
        ENDDO
      ENDDO
      IF(SUM.EQ.0)THEN
        SUM=1.
        WEIGHT(0,0)=1.
      ENDIF
      DO I=-DELX,DELX
        DO J=-DELY,DELY
          IF(SUM.GT.0.00000001)WEIGHT(I,J)=WEIGHT(I,J)/SUM
        ENDDO
      ENDDO
      END


      SUBROUTINE INTERCH
      INTEGER XDIM,YDIM
      CHARACTER APP*4
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,xcomm*60

      IF(ITYPE.EQ.1)ITYPE=3
      WRITE(6,1)IDEST
   1  FORMAT('Destination spectrum <',I1,'>:',$)
      CALL READI(5,IDEST)
      ISP=IDEST
      WRITE(6,2)ISP
   2  FORMAT( 'Source spectrum      <',I1,'>:',$)
      CALL READI(5,ISP)
      IF(IDEST.LT.1.OR.IDEST.GT.2)Istatus=1
      IF(ISP  .LT.1.OR.ISP  .GT.2)Istatus=1

      IF(Istatus.NE.0)RETURN

      MAXDIM=2048
      DO I=0,MAXDIM-1
        DO J=I,MAXDIM-1
          IWAIT1=rMAT(ISP,I,J)
          IWAIT2=rMAT(ISP,J,I)
          rMAT(IDEST,I,J)=IWAIT2
          rMAT(IDEST,J,I)=IWAIT1
        ENDDO
      ENDDO

      ay0=cal(1,ISP,2,1)
      ay1=cal(1,ISP,2,2)
      ay2=cal(1,ISP,2,3)

      ax0=cal(1,ISP,1,1)
      ax1=cal(1,ISP,1,2)
      ax2=cal(1,ISP,1,3)

      cal(1,IDEST,1,1)=ay0
      cal(1,IDEST,1,2)=ay1
      cal(1,IDEST,1,3)=ay2

      cal(1,IDEST,2,1)=ax0
      cal(1,IDEST,2,2)=ax1
      cal(1,IDEST,2,3)=ax2

      YDIM=Idim(1,ISP,1)
      XDIM=Idim(1,ISP,2)
      IF(XDIM.GT.2048)XDIM=2048
      IF(YDIM.GT.2048)YDIM=2048

      WRITE(6,*)'x- and y-axis have been interchanged.'

C Updating comment in the heading of spectrum file
      xcomm(1:3)='XY:'
      fname(1,IDEST)='XY'//fname(1,ISP)(1:6)
      comm(1,IDEST)=comm(1,ISP)
      CALL AddComment(xcomm,3)
      CALL SetMarker(1,1,1)

      END

 
      SUBROUTINE LANDSCAPE
      EXTERNAL FUNC4,EXPONENT
      CHARACTER APP*4
      DIMENSION FIT(50000)
      DIMENSION X(50000),Y(50000),SIGMAY(50000),A(5),DELTAA(5),SIGMAA(5),CH(500)
      INTEGER XDIM,YDIM
      REAL N,MASS,NC
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,xcomm*60
      COMMON/CALxy/AX0,AX1,AY0,AY1
      COMMON/CNUTE/EX,EXL,EXH,XJ,EGAP,MASS,EXPN(0:4095),N(0:4095),FWXG
      COMMON/CTOTFIT/XEN(50000),YEN(50000),NPTS,EXV,FX,EXMIN,ISP1

      IDUM=1
      IF(IDEST.EQ.1)IDUM=2
      IDEST=IDUM
      WRITE(6,1)IDEST
 1    FORMAT('Dest. spectr. for theoretical data <',I1,'>:',$)
      CALL READI(5,IDEST)
      ISP=1
      IF(IDEST.EQ.1)ISP=2
      WRITE(6,3)ISP
 3    FORMAT( 'Source spectrum                    <',I1,'>:',$)
      CALL READI(5,ISP)
      IF(ISP.EQ.IDEST)THEN
        WRITE(6,*)'Destination must be another spectrum'
        Istatus=1
        RETURN
      ENDIF
      IF(IDEST.LT.1.OR.IDEST.GT.2)Istatus=1
      IF(ISP  .LT.1.OR.ISP  .GT.2)Istatus=1
      IF(Istatus.NE.0)RETURN

C     SETTING DEFAULT VALUES AND
C     READING IN VARIOUS PARAMETERS TO BE USED
      ISP=1
      IF(IDEST.EQ.1)ISP=2

      ISP1=ISP
      MASS=172.
      EGAP=1830.
      NC=4.
      XJ=4.
      FWXG=500.
      FX=0.
      EXV=0.
      NA=2
      XDIM=Idim(1,ISP,1)
      YDIM=Idim(1,ISP,2)
      SUM=0.
      DO J=0,YDIM-1
        DO I=0,XDIM-1
          SUM=SUM+rMAT(ISP,I,J)
          rMAT(IDEST,I,J)=0
        ENDDO
      ENDDO
      IF(SUM.EQ.0)THEN
        WRITE(6,*)'Forgotten to read in source spectrum'
        Istatus=1
        RETURN
      ENDIF

C NORMALIZE SOURCE SPECTRUM
      WRITE(6,*)'Deleting negative counts and normalizing...'
      ySUM=0
      DO J=0,YDIM-1
        xSUM=0
        DO I=0,XDIM-1
          IF(rMAT(ISP,I,J).GT.0)xSUM=xSUM+rMAT(ISP,I,J)
        ENDDO
        DO I=0,XDIM-1
          IF(xSUM.NE.0.AND.rMAT(ISP,I,J).GE.0)THEN
            rMAT(ISP,I,J)=rMAT(ISP,I,J)/xSUM
            ySUM=ySUM+rMAT(ISP,I,J)
          ELSE
            rMAT(ISP,I,J)=0
           ENDIF
        ENDDO
      ENDDO

      bx=cal(1,ISP,1,1)+cal(1,ISP,1,2)+cal(1,ISP,1,3)
      by=cal(1,ISP,2,1)+cal(1,ISP,2,2)+cal(1,ISP,2,3)
      IF(bx+by.EQ.2.)THEN
        AX0=11.
        AX1=20.
        AY0=9660.
        AY1=-120.
      ELSE
        AX0=cal(1,ISP,1,1)
        AX1=cal(1,ISP,1,2)
        AY0=cal(1,ISP,2,1)
        AY1=cal(1,ISP,2,2)
      ENDIF

  99  CONTINUE
      WRITE(6,10)AX0
  10  FORMAT(//'Cal. coeff. a0 (keV) on x-axis    <',F8.1,'>:',$)
      CALL READF(5,AX0)
      WRITE(6,11)AX1
  11  FORMAT( 'Cal. coeff. a1 (keV/ch) on x-axis <',F8.1,'>:',$)
      CALL READF(5,AX1)

      WRITE(6,12)AY0
  12  FORMAT( /'Cal. coeff. a0 (keV) on y-axis    <',F8.1,'>:',$)
      CALL READF(5,AY0)
      WRITE(6,13)AY1
  13  FORMAT( 'Cal. coeff. a1 (keV/ch) on y-axis <',F8.1,'>:',$)
      CALL READF(5,AY1)
      IF(Istatus.NE.0)RETURN

      WRITE(6,14)MASS
  14  FORMAT(/'Mass number',14X,'                    <',F4.0,'>:',$)
      CALL READF(5,MASS)
      AD=MASS/8000.
      AA=AD

      GAP=1000.*2.*12.*MASS**(-0.5)
      WRITE(6,15)GAP,EGAP
  15  FORMAT(/'Pairing gap (2*12/sqrt(A)=',F5.0,')',12X,'<',F5.0,'>:',$)
      CALL READF(5,EGAP)

      JMIN=((EGAP -AY0)/AY1+0.5)
      JMAX=((8000.-AY0)/AY1+0.5)
      IF(JMIN.GT.JMAX)THEN
        JWAIT=JMIN
        JMIN=JMAX
        JMAX=JWAIT
      ENDIF
      IF(JMIN.LT.0)JMIN=0
      IF(JMAX.GE.YDIM-1)JMAX=YDIM-1
      EYMIN=AY1*JMIN+AY0
      EYMAX=AY1*JMAX+AY0
      WRITE(6,*)'Spectra to be fitted, give low and high y-channel'
      WRITE(6,16)EYMIN,JMIN
  16  FORMAT(/'Lower ch. (excitation ',F8.2,' keV)',9X,' <',I4,'>:',$)
      CALL READI(5,JMIN)
      WRITE(6,17)EYMAX,JMAX
  17  FORMAT( 'Higher ch. (excitation ',F8.2,' keV)',8X,' <',I4,'>:',$)
      CALL READI(5,JMAX)
      IF(JMIN.GT.JMAX)THEN
         WRITE(6,*)'Lower channel > Higher channel'
         GO TO 99
      ENDIF
      IF(Istatus.NE.0)RETURN

 999  CONTINUE

      EXMIN=EGAP+400.
      WRITE(6,*)'The highest gamma-energy to be fitted is'
      WRITE(6,*)'limited by the final excitation energy (Ex-Eg).'
      WRITE(6,*)'This value must be equal or higher than Egap.'
      WRITE(6,18)EXMIN
  18  FORMAT(/'Lower final excit. energy (keV) >=Egap     <',
     1F6.0,'>:',$)
      CALL READF(5,EXMIN)

      IMIN=((800.-AX0)/AX1+0.5)
      EGMIN=AX1*IMIN+AX0
      WRITE(6,19)EGMIN,IMIN
  19  FORMAT(/'Lower ch. for fit on x-axis (Eg=',F5.0,
     +' keV)   <',I4,'>:',$)
      CALL READI(5,IMIN)

      WRITE(6,*)'Give exponent for: Egam**n. Use n=4.0 to 5.0, or'
      WRITE(6,20)NC
  20  FORMAT('n=0 according to appr. of Axel',15X,'<',F4.2,'>:',$)
      CALL READF(5,NC)
      CALL EXPONENT(NC)

      WRITE(6,21)AD,AA
  21  FORMAT('Starting level dens. par. a (A/8000=',F6.3,')',
     +'<',F6.3,'>:',$)
      CALL READF(5,AA)

      WRITE(6,22)XJ
  22  FORMAT('Give average spin populated',18X,'<',F4.1,'>:',$)
      CALL READF(5,XJ)

      WRITE(6,23)FWXG
  23  FORMAT('Energy FWHM for Ex-Eg (keV)',16X,'<',F6.1,'>:',$)
      CALL READF(5,FWXG)

      WRITE(6,24)NA
  24  FORMAT('Number of parameters in fit (max=2)',12X,'<',I1,'>:',$)
      CALL READI(5,NA)
      IF(Istatus.NE.0)RETURN

C SETTING UP THE HISTOGRAM TO BE FITIED, BOTH X(I) AND Y(I)
      NPTS=0
      DO J=JMIN,JMAX
        EX =J*AY1+AY0
        IMAX=((EX-EXMIN-AX0)/AX1+0.5)
        DO I=IMIN,IMAX
          NPTS=NPTS+1
          IF(NPTS.LE.50000)THEN
            X(NPTS)=NPTS
            XEN(NPTS)=AX1*I+AX0
            YEN(NPTS)=AY1*J+AY0
            Y(NPTS)=rMAT(ISP,I,J)
          ENDIF
        ENDDO
      ENDDO

      IF(NPTS.GT.50000)NPTS=50000
      WRITE(6,26)NPTS
  26  FORMAT('Number of datapoints in fit (max=50000)= ',I5)

      IF(NPTS.LT.NA+4)THEN
        WRITE(6,*)'Too few datapoints for fit'
        GO TO 25
      ENDIF

C ESTIMATE PARAMETERS TO BE FITTED
      A(1)=1.
      A(2)=AA
      EXPSUM=0.
      TEOSUM=0.
      DO JJ=1,NPTS
        EXPSUM=EXPSUM+Y(JJ)
        XX=JJ
        TEOSUM=TEOSUM+FUNC4(XX,A)
      ENDDO
      A(1)=EXPSUM/TEOSUM

      DO I=1,2
        SIGMAA(I)=A(I)/10.
      ENDDO

      DO I=1,NPTS
        SIGMAY(I)=100.
      ENDDO

      DELTAA(1)=A(1)/10.
      DELTAA(2)=A(2)/100.
      IF(Istatus.NE.0)RETURN
      
      cal(1,ISP,1,1)=  AX0
      cal(1,ISP,1,2)=  AX1
      cal(1,ISP,2,1)=  AY0
      cal(1,ISP,2,2)=  AY1
      cal(1,IDEST,1,1)=AX0
      cal(1,IDEST,1,2)=AX1
      cal(1,IDEST,2,1)=AY0
      cal(1,IDEST,2,2)=AY1
      

C*********************************************************************
C   FITING LOOP STARTS
C*********************************************************************
      M     =0
      PROG  =0.00005
      MODE  =1
      CHISQR=999999.
      BEST  =999999.
      NBAD  =0

C     STARTING FIT **********************************************
      WRITE(6,30)
  30  FORMAT('  LOOP   CHISQR     A(1)       A(2)')
      IF(M.LT.4)WRITE(6,*)' '
      WRITE(6,31)M,CHISQR,(A(JJ),JJ=1,NA)
  33  CALL GRIDLS(X,Y,SIGMAY,NPTS,MODE,FUNC4,A
     +,DELTAA,SIGMAA,NA,FIT,CHISQR)
      M=M+1
      WRITE(6,31)M,CHISQR,(A(JJ),JJ=1,NA)
  31  FORMAT(1X,I3,3(1X,E10.5))
      CH(M)=CHISQR
      IF(M.LT.2) GO TO 33
      VER=ABS(CH(M)-CH(M-1))/CH(M-1)
      IF(CHISQR.LT.BEST)THEN
        BEST=CHISQR
        AA1=A(1)
        AA2=A(2)
        SIGAA1=SIGMAA(1)
        SIGAA2=SIGMAA(2)
      ENDIF
      IF(CH(M).GT.CH(M-1))NBAD=NBAD+1
      IF(NBAD.GT.4)GO TO 34
      IF(M.GE.500)GO TO 34
      IF(VER.GT.PROG) GO TO 33
  34  CONTINUE

C     FIT IS FINISHED *********************************************
      A(1)=AA1
      SIGMAA(1)=SIGAA1
      A(2)=AA2
      SIGMAA(2)=SIGAA2

      WRITE(6,39)NC,BEST
  39  FORMAT(/' Results: n= ',F4.1,' (0 means Axel) with Chisqr1.=',
     +F8.3)
      WRITE(6,40)
  40  FORMAT(3('--------------------------'))
      WRITE(6,41)  A(1),SIGMAA(1)
      WRITE(6,42)  A(2),SIGMAA(2)
  41  FORMAT(' Normalisation parameter A(1)=',E10.4,' +/-',E10.4)
  42  FORMAT(' Level density parameter A(2)= ',F9.5,' +/-',F9.5,
     +' /keV ')
      WRITE(6,40)

      EXPSUM=0.
      TEOSUM=0.
      DO JJ=1,NPTS
        EXPSUM=EXPSUM+Y(JJ)
        XX=JJ
        TEOSUM=TEOSUM+FUNC4(XX,A)
      ENDDO

      TRUE=0.
      XCHI=0.
      XNORM=EXPSUM/TEOSUM
      A(1)=A(1)*XNORM
      DO JJ=1,NPTS
        I=((XEN(JJ)-AX0)/AX1+0.5)
        J=((YEN(JJ)-AY0)/AY1+0.5)
        XX=JJ
        YY=FUNC4(XX,A)
        rMAT(IDEST,I,J)=YY
        SIG2=10.
        IF(YY.GT.1)SIG2=100.**2
       IF(rMAT(ISP,I,J).GT.0.OR.rMAT(IDEST,I,J).GT.0)THEN
          TRUE=TRUE+1.
          XCHI=XCHI+((ABS(rMAT(ISP,I,J)-rMAT(IDEST,I,J)))**2)/SIG2
        ENDIF
      ENDDO

      XCHI=XCHI/(TRUE-NA-1.)
      ITRUE=TRUE
      IEXP=EXPSUM+0.5
      WRITE(6,58)IEXP,A(1),XCHI,ITRUE,NPTS
  58  FORMAT(' After renormalization to ',I6,' counts: A(1)= ',E10.4,/,
     +' Chisqr2.= ',F8.3,' (based on ',I4,' out of ',I4,' points)')

  25  IDUM=1
      WRITE(6,57)
  57  FORMAT(/'Press 1 for new try, or 0 for return to mama')
      CALL READI(5,IDUM)
      IF(IDUM.EQ.1)GO TO 999

C Updating comment in the heading of spectrum file
      xcomm(1:11)='TF:'//fname(1,ISP)(1:8)
      fname(1,IDEST)='TF'//fname(1,ISP)(1:6)
      comm(1,IDEST)=''
      CALL AddComment(xcomm,11)

      RETURN
      END


      SUBROUTINE MAKEPART
      INTEGER XDIM,YDIM 
      CHARACTER APP*4
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,xcomm*60
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH

      IDUM=1
      IF(IDEST.EQ.1)IDUM=2
      IDEST=IDUM
      WRITE(6,1)IDEST
   1  FORMAT('Destination spectrum <',I1,'>:',$)
      CALL READI(5,IDEST)
      ISP=1
      IF(IDEST.EQ.1)ISP=2 
      WRITE(6,2)ISP
   2  FORMAT( 'Source spectrum      <',I1,'>:',$)
      CALL READI(5,ISP)
      IF(ISP.EQ.IDEST)THEN
        WRITE(6,*)'Destination must be another spectrum'
        Istatus=1
        RETURN
      ENDIF     
      IF(IDEST.LT.1.OR.IDEST.GT.2)Istatus=1
      IF(ISP  .LT.1.OR.ISP  .GT.2)Istatus=1

      IF(ITYPE.GT.1)THEN
        IXL=0
        IXH=Idim(1,ISP,1)-1
        IYL=0
        IYH=Idim(1,ISP,2)-1
        WRITE(6,3)IXL
   3    FORMAT(/'Lower marker on x-axis   <',I5,'>:',$)
        CALL READI(5,IXL)
        WRITE(6,4)IXH
   4    FORMAT( 'Higher marker on x-axis  <',I5,'>:',$)
        CALL READI(5,IXH)
        WRITE(6,5)IYL
   5    FORMAT(/'Lower marker on y-axis   <',I5,'>:',$)
        CALL READI(5,IYL)
        WRITE(6,6)IYH
   6    FORMAT( 'Higher marker on y-axis  <',I5,'>:',$)
        CALL READI(5,IYH)
        IF(IXL.GT.IXH)THEN
          WRITE(6,*)' Lower x-marker > x-higher marker'
          Istatus=1
        ENDIF
        IF(IYL.GT.IYH)THEN
          WRITE(6,*)' Lower y-marker > y-higher marker'
          Istatus=1
        ENDIF

        IF(Istatus.NE.0)RETURN
        
        XDIM=IXH-IXL+1
        YDIM=IYH-IYL+1
        IF(XDIM.GT.4096)XDIM=4096
        IF(YDIM.GT.2048) YDIM=2048
        DO J=0,YDIM-1
          DO I=0,XDIM-1
            I1=I+IXL
            J1=J+IYL
        IF(I1.GE.0.AND.I1.LE.4095.AND.J1.GE.0.AND.J1.LE.2047)THEN
              rMAT(IDEST,I,J)=rMAT(ISP,I1,J1)
            ENDIF
          ENDDO
        ENDDO
        
        ax0=cal(1,ISP,1,1)
        ax1=cal(1,ISP,1,2)
        ax2=cal(1,ISP,1,3)
        cal(1,IDEST,1,1)=ax0+IXL*ax1+IXL*IXL*ax2
        cal(1,IDEST,1,2)=ax1+2.*IXL*ax2
        cal(1,IDEST,1,3)=ax2

        ay0=cal(1,ISP,2,1)
        ay1=cal(1,ISP,2,2)
        ay2=cal(1,ISP,2,3)
        cal(1,IDEST,2,1)=ay0+IYL*ay1+IYL*IYL*ay2
        cal(1,IDEST,2,2)=ay1+2.*IYL*ay2
        cal(1,IDEST,2,3)=ay2
        CALL SetMarker(1,1,1)
        WRITE(6,7)XDIM-1,YDIM-1
    7   FORMAT('New dimension (0:',I4,',0:',I4,')')

C Updating comment in the heading of spectrum file
        xcomm(1:3)='PA:'
        fname(1,IDEST)=fname(1,ISP)
        write(xcomm(4:22),991,ERR=997)IXL,'-',IXH,',',IYL,'-',IYH
991     FORMAT(I4,A1,I4,A1,I4,A1,I4)
        comm(1,IDEST)=comm(1,ISP)
997     CALL AddComment(xcomm,22)
      ELSE

        IXL=0
        IXH=Idim(2,ISP,1)-1
        WRITE(6,3)IXL
        CALL READI(5,IXL)
        WRITE(6,4)IXH
        CALL READI(5,IXH)
        IF(IXL.GT.IXH)THEN
          WRITE(6,*)' Lower marker > higher marker'
          Istatus=1
        ENDIF
        IF(Istatus.NE.0)RETURN

        MAXCH=IXH-IXL
        IF(MAXCH.GT.8191)MAXCH=8191
        DO I=0,MAXCH
          I1=I+IXL
          IF(I1.GE.0.AND.I1.LE.8191)THEN
             rSPEC(IDEST,I)=rSPEC(ISP,I1)
          ENDIF
        ENDDO
        ax0=cal(2,ISP,1,1)
        ax1=cal(2,ISP,1,2)
        ax2=cal(2,ISP,1,3)
        cal(2,IDEST,1,1)=ax0+IXL*ax1+IXL*IXL*ax2
        cal(2,IDEST,1,2)=ax1+2.*IXL*ax2
        cal(2,IDEST,1,3)=ax2
        CALL SetMarker(1,2,0)       
        WRITE(6,8)MAXCH
    8   FORMAT('New dimension (0:',I4,')')

C Updating comment in the heading of spectrum file
        xcomm(1:3)='PA:'
        fname(2,IDEST)=fname(2,ISP)
        write(xcomm(4:12),992,ERR=998)IXL,'-',IXH
992     FORMAT(I4,A1,I4)
        comm(2,IDEST)=comm(2,ISP)
998     CALL AddComment(xcomm,12)
      ENDIF
      END
                       

      SUBROUTINE NORMALIZE
      INTEGER XDIM,YDIM
      CHARACTER APP*4
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,xcomm*60
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH

      WRITE(6,1)IDEST
   1  FORMAT('Destination spectrum <',I1,'>:',$)
      CALL READI(5,IDEST)
      ISP=IDEST
      WRITE(6,2)ISP
   2  FORMAT( 'Source spectrum      <',I1,'>:',$)
      CALL READI(5,ISP)
      IF(IDEST.LT.1.OR.IDEST.GT.2)Istatus=1
      IF(ISP  .LT.1.OR.ISP  .GT.2)Istatus=1

      IF(Istatus.NE.0)RETURN
        
      IF(ITYPE.GT.1)THEN
        XDIM=Idim(1,ISP,1)
        YDIM=Idim(1,ISP,2)
        DO J=0,2047
          SUM=0
          DO I=0,4095
            SUM=SUM+rMAT(ISP,I,J)
          ENDDO
          DO I=0,4095
            IF(SUM.NE.0)THEN
             rMAT(IDEST,I,J)=rMAT(ISP,I,J)/SUM
            ELSE
              rMAT(IDEST,I,J)=0
             ENDIF
          ENDDO
        ENDDO
        cal(1,IDEST,1,1)=cal(1,ISP,1,1)
        cal(1,IDEST,1,2)=cal(1,ISP,1,2)
        cal(1,IDEST,1,3)=cal(1,ISP,1,3)
        cal(1,IDEST,2,1)=cal(1,ISP,2,1)
        cal(1,IDEST,2,2)=cal(1,ISP,2,2)
        cal(1,IDEST,2,3)=cal(1,ISP,2,3)

C Updating comment in the heading of spectrum file
        xcomm(1:3)='NO:'
        fname(1,IDEST)=fname(1,ISP)
        comm(1,IDEST)=comm(1,ISP)
        CALL AddComment(xcomm,3)

      ELSE
        MAXCH=Idim(2,ISP,1)-1
        SUM=0
        DO I=0,8191
          SUM=SUM+rSPEC(ISP,I)
        ENDDO
        DO I=0,4095
          IF(SUM.NE.0)THEN
            rSPEC(IDEST,I)=rSPEC(ISP,I)/SUM
          ELSE
            rSPEC(IDEST,I)=0
          ENDIF
        ENDDO
        cal(2,IDEST,1,1)=cal(2,ISP,1,1)
        cal(2,IDEST,1,2)=cal(2,ISP,1,2)
        cal(2,IDEST,1,3)=cal(2,ISP,1,3)

C Updating comment in the heading of spectrum file
        xcomm(1:3)='NO:'
        fname(2,IDEST)=fname(2,ISP)
        comm(2,IDEST)=comm(2,ISP)
        CALL AddComment(xcomm,3)

      ENDIF
      END


      SUBROUTINE NUTE
      CHARACTER APP*4
      EXTERNAL FUNC1,EXPONENT
      DIMENSION FIT(4096)
      DIMENSION X(4096),Y(4096),SIGMAY(4096),A(5),DELTAA(5),SIGMAA(5),CH(500)
      REAL N,MASS,NC
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,xcomm*60
      COMMON/CALxy/AX0,AX1,AY0,AY1
      COMMON/CNUTE/EX,EXL,EXH,XJ,EGAP,MASS,EXPN(0:4095),N(0:4095),FWXG
      INTEGER XDIM,YDIM
      IDUM=1
      IF(IDEST.EQ.1)IDUM=2
      IDEST=IDUM
     
      WRITE(6,2)IDEST
 2    FORMAT('Dest. spectr. for theoretical fit <',I1,'>:',$)
      CALL READI(5,IDEST)
      ISP=1
      IF(IDEST.EQ.1)ISP=2
      WRITE(6,3)ISP
 3    FORMAT( 'Source spectrum                   <',I1,'>:',$)
      CALL READI(5,ISP)
      IF(ISP.EQ.IDEST)THEN
        WRITE(6,*)'Destination must be another spectrum'
        Istatus=1
        RETURN
      ENDIF
      IF(IDEST.LT.1.OR.IDEST.GT.2)Istatus=1
      IF(ISP  .LT.1.OR.ISP  .GT.2)Istatus=1

      IF(Istatus.NE.0)RETURN
      XDIM=Idim(1,ISP,1)
      YDIM=Idim(1,ISP,2)
 
C     SETTING DEFAULT VALUES AND
C     READING IN VARIOUS PARAMETERS TO BE USED
      SUM=0.
      DO J=0,YDIM-1
        DO I=0,XDIM-1
          SUM=SUM+rMAT(ISP,I,J)
          rMAT(IDEST,I,J)=0
        ENDDO
      ENDDO
      IF(SUM.EQ.0)THEN
        WRITE(6,*)'Forgotten to read in source spectrum'
        Istatus=1
        RETURN
      ENDIF
      MASS=172.
      EGAP=1830.
      NC=4.
      AA=0.012
      XJ=4.
      FWXG=500.
      NA=2
      
      bx=cal(1,ISP,1,1)+cal(1,ISP,1,2)+cal(1,ISP,1,3)
      by=cal(1,ISP,2,1)+cal(1,ISP,2,2)+cal(1,ISP,2,3)
      IF(bx+by.EQ.2.)THEN
        AX0=11.
        AX1=20.
        AY0=9660.
        AY1=-120.
      ELSE
        AX0=cal(1,ISP,1,1)
        AX1=cal(1,ISP,1,2)
        AY0=cal(1,ISP,2,1)
        AY1=cal(1,ISP,2,2)
      ENDIF

  99  CONTINUE
      WRITE(6,10)AX0
  10  FORMAT(//'Cal. coeff. a0 (keV) on x-axis    <',F11.1,'>:',$)
      CALL READF(5,AX0)
      WRITE(6,11)AX1
  11  FORMAT(  'Cal. coeff. a1 (keV/ch) on  x-axis<',F11.1,'>:',$)
      CALL READF(5,AX1)

      WRITE(6,12)AY0
  12  FORMAT(/'Cal. coeff. a0 (keV) on y-axis   <',F11.1,'>:',$)
      CALL READF(5,AY0)
      WRITE(6,13)AY1
  13  FORMAT( 'Cal. coeff. a1 (keV/ch) on y-axis<',F11.1,'>:',$)
      CALL READF(5,AY1)

      WRITE(6,14)MASS
  14  FORMAT(/'Mass number',5X,'                    <',F4.0,'>:',$)
      CALL READF(5,MASS)

      GAP=1000.*2.*12.*MASS**(-0.5)
      WRITE(6,15)GAP,EGAP
  15  FORMAT('Pairing gap (2*12/sqrt(A)=',F5.0,')',4X,'<',F5.0,'>:',$)
      CALL READF(5,EGAP)
      IF(Istatus.NE.0)RETURN

      JMIN=((EGAP -AY0)/AY1+0.5)
      JMAX=((8000.-AY0)/AY1+0.5)
      IF(JMIN.GT.JMAX)THEN
        JWAIT=JMIN
        JMIN=JMAX
        JMAX=JWAIT
      ENDIF
      IF(JMIN.LT.0)JMIN=0
      IF(JMAX.GE.YDIM-1)JMAX=YDIM-1
      EYMIN=AY1*JMIN+AY0
      EYMAX=AY1*JMAX+AY0
      WRITE(6,*)'Spectra to be fitted, give low and high y-channel'
      WRITE(6,16)EYMIN,JMIN
  16  FORMAT(/'Lower ch. (excitation ',F8.2,' keV)',8X,' <',I4,'>:',$)
      CALL READI(5,JMIN)
      WRITE(6,17)EYMAX,JMAX
  17  FORMAT( 'Higher ch. (excitation ',F8.2,' keV)',8X,'<',I4,'>:',$)
      CALL READI(5,JMAX)
      IF(JMIN.GT.JMAX)THEN
         WRITE(6,*)'Lower ch. > Higher ch.'
         GO TO 99
      ENDIF
      IF(Istatus.NE.0)RETURN

      EXMIN=EGAP+200.
      WRITE(6,*)'The highest gamma-energy to be fitted is'
      WRITE(6,*)'limited by the final exc. energy (Ex-Eg).'
      WRITE(6,*)'This value must be equal or higher than Egap.'
      WRITE(6,18)EXMIN
  18  FORMAT(/'Lower final excit. energy (keV) >=Egap <',F6.0,'>:',$)
    
      CALL READF(5,EXMIN)

      IMIN=((800.-AX0)/AX1+0.5)
      EGMIN=AX1*IMIN+AX0
      WRITE(6,19)EGMIN,IMIN
  19  FORMAT(/'Lower ch. for fit on x-axis (Eg=',F5.0,
     +' keV)   <',I4,'>:',$)
      CALL READI(5,IMIN)

      WRITE(6,*)'Give exponent for: Egam**n. Use n=4.0 to 5.0, or'
      WRITE(6,20)NC
  20  FORMAT('n=0 according to appr. of Axel',12X,'<',F4.2,'>:',$)
      CALL READF(5,NC)
      CALL EXPONENT(NC)

      AD=MASS/8000.
      WRITE(6,21)AD,AA
  21  FORMAT('Init. level density param. a (A/8000=',F5.3,')',
     +' <',F5.3,'>:',$)
      CALL READF(5,AA)

      WRITE(6,22)XJ
  22  FORMAT('Give average spin populated',6X,'<',F4.1,'>:',$)
      CALL READF(5,XJ)

      WRITE(6,23)FWXG
  23  FORMAT('Energy FWHM  for Ex-Eg (keV)<',F6.1,'>:',$)
      CALL READF(5,FWXG)

      WRITE(6,24)NA
  24  FORMAT('Number of parameters in fit (max=2)<',I1,'>:',$)
      CALL READI(5,NA)
      IF(Istatus.NE.0)RETURN

      cal(1,ISP,1,1)=  AX0
      cal(1,ISP,1,2)=  AX1
      cal(1,ISP,2,1)=  AY0
      cal(1,ISP,2,2)=  AY1
      cal(1,IDEST,1,1)=AX0
      cal(1,IDEST,1,2)=AX1
      cal(1,IDEST,2,1)=AY0
      cal(1,IDEST,2,2)=AY1

C******************************************************* 
C   FITIING LOOP STARTS
C******************************************************* 

      PROG=0.0001
      MODE=1
      CHITOT=0
      DO J=JMIN,JMAX
        M=0
C SETTING UP THE HISTOGRAM TO BE FITIED, BOTH X(I) AND Y(I)
        EX =J*AY1+AY0
        EXL=EX-FWXG/2.
        EXH=EX+FWXG/2.
         IF(EXL.LT.0)EXL=0.
        IMAX=((EX-EXMIN-AX0)/AX1+0.5)
        IF(IMAX.LE.IMIN+NA+1)THEN
          WRITE(6,*)'Too few data for fit'
          GO TO 25
        ENDIF
        NPTS=0
        DO I=IMIN,IMAX
          NPTS=NPTS+1
          X(NPTS)=AX1*I+AX0
          Y(NPTS)=rMAT(ISP,I,J)
        ENDDO

C ESTIMATE PARAMETERS TO BE FITIED
        A(1)=1.0
        A(2)=AA
        DO I=1,2
          SIGMAA(I)=A(I)/10.
        ENDDO
        DELTAA(2)=A(2)/100.
        EXPSUM=0.
        TEOSUM=0.
        DO I=1,NPTS
          EXPSUM=EXPSUM+Y(I)
        ENDDO
        YAVE=EXPSUM/(NPTS+1)
        DO I=1,NPTS
          SIGMAY(I)=SQRT(3.*9.*YAVE)
          XX=X(I)
          TEOSUM=TEOSUM+FUNC1(XX,A)
        ENDDO
        FACTOR=EXPSUM/TEOSUM
        A(1)=A(1)*FACTOR
        DELTAA(1)=A(1)/10.
        CHISQR=999999.
        BEST=999999.
        NBAD=0

C     STARTING FIT **********************************************
        WRITE(6,30)
  30    FORMAT('  LOOP   CHISQR     A(1)       A(2)')
        IF(M.LT.4)WRITE(6,*)' '
        WRITE(6,31)M,CHISQR,(A(JJ),JJ=1,NA)
  33    CALL GRIDLS(X,Y,SIGMAY,NPTS,MODE,FUNC1,A
     +  ,DELTAA,SIGMAA,NA,FIT,CHISQR)
        M=M+1
        WRITE(6,31)M,CHISQR,(A(JJ),JJ=1,NA)
  31    FORMAT(I4,3(1X,E10.5))
        CH(M)=CHISQR
        IF(M.LT.2) GO TO 33
        VER=ABS(CH(M)-CH(M-1))/CH(M-1)
        IF(CHISQR.LT.BEST)THEN
          BEST=CHISQR
          AA1=A(1)
          AA2=A(2)
          SIGAA1=SIGMAA(1)
          SIGAA2=SIGMAA(2)
         ENDIF
        IF(CH(M).GT.CH(M-1))NBAD=NBAD+1
        IF(NBAD.GT.4)GO TO 34
        IF(M.GE.500)GO TO 34
        IF(VER.GT.PROG) GO TO 33
  34    CONTINUE

C     FIT IS FINISHED *********************************************
        A(1)=AA1
        SIGMAA(1)=SIGAA1
        A(2)=AA2
        SIGMAA(2)=SIGAA2

        WRITE(6,39)J,EX,IMIN,IMAX,BEST
  39    FORMAT(/' Results for y-ch:',I3,' (Exi=',F6.0,
     +  ' keV) and x-chs:',I3,' -',I3,',  Chisqr.=',F8.3)
        CHITOT=CHITOT+BEST

        WRITE(6,40)
  40    FORMAT(3('--------------------------'))
        WRITE(6,41)  A(1),SIGMAA(1)
        WRITE(6,42)  A(2),SIGMAA(2)
  41    FORMAT(' Normalisation parameter A(1)=',E10.4,' +/-',E10.4)
  42    FORMAT(' Level density parameter A(2)= ',F9.5,' +/-',F9.5,
     +  ' /keV ')
        WRITE(6,40)
        EXPSUM=0.
        TEOSUM=0.
        DO I=1,NPTS
          EXPSUM=EXPSUM+Y(I)
          XX=X(I)
          TEOSUM=TEOSUM+FUNC1(XX,A)
        ENDDO
        IEXP=EXPSUM+0.5
        ITEO=TEOSUM+0.5

        ASUM=0.
        CENT=0.
        DO I=0,IMAX
          XX=AX1*I+AX0
          rMAT(IDEST,I,J)=FUNC1(XX,A)+0.5
          CENT=CENT+XX*FUNC1(XX,A)
          ASUM=ASUM+FUNC1(XX,A)
        ENDDO
        CENT=CENT/ASUM
    
        WRITE (1,50)CENT
  50    FORMAT(' Average theor. gamma-energy in f.g.=',F7.1,' keV')
        EXFINAL=EX-CENT
        WRITE(6,51)EXFINAL
  51    FORMAT(' Final average excit. energy in f.g.=',F7.1,' keV')

        IF(A(2).GT.0.AND.EX-CENT-EGAP-300..GT.0)THEN
           TT=SQRT((EX-CENT-EGAP-300.)/A(2))
        ELSE
          TT=0
        ENDIF
        WRITE(6,52)TT
  52    FORMAT(' Temperature                        =',F7.1,' keV')
        WRITE(6,56)IEXP,ITEO
  56    FORMAT(' Areas in fit-region:  Exp.:',
     +  I6,' Theor.:',I6)

        LOOPS=J-JMIN+1
        CHIAV=CHITOT/LOOPS
        WRITE(6,53)LOOPS,CHIAV
  53    FORMAT(' Average chisq. after ',I2,' spectra is ',F8.3)

  25    IF(J.LT.JMAX)THEN
          WRITE(6,57)
  57      FORMAT(/'Press return for fit of next y-ch, or 0 for stop:')
          IDUM=1
          CALL READI(5,IDUM)
          IF(IDUM.EQ.0)THEN
C Updating comment in the heading of spectrum file
            xcomm(1:11)='NU:'//fname(1,ISP)(1:8)
            fname(1,IDEST)='NU'//fname(1,ISP)(1:6)
            comm(1,IDEST)=''
            CALL AddComment(xcomm,11)
            RETURN
          ENDIF
        ENDIF
      ENDDO
      RETURN
      END


      SUBROUTINE PUTCONST
      INTEGER XDIM,YDIM 
      CHARACTER APP*4
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,xcomm*60
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH

      CONST=0
      IXL=0
      IXH=4095
      IYL=0
      IYH=2047

      IF(ITYPE.GT.1)THEN
        WRITE(6,1)IDEST
   1    FORMAT('Destination spectrum <',I1,'>:',$)
        CALL READI(5,IDEST)
        IF(IDEST.LT.1.OR.IDEST.GT.2)Istatus=1
        IF(Istatus.NE.0)RETURN
        XDIM=Idim(1,IDEST,1)
        YDIM=Idim(1,IDEST,2)
        WRITE(6,3)IXL
   3    FORMAT(/'Lower marker on x-axis <',I5,'>:',$)
        CALL READI(5,IXL)
        WRITE(6,4)IXH
   4    FORMAT( 'Higher marker on x-axis<',I5,'>:',$)
        CALL READI(5,IXH)
        WRITE(6,5)IYL
   5    FORMAT(/'Lower marker on y-axis <',I5,'>:',$)
        CALL READI(5,IYL)
        WRITE(6,6)IYH
   6    FORMAT( 'Higher marker on y-axis<',I5,'>:',$)
        CALL READI(5,IYH)
		  
		  

        WRITE(6,7)CONST
   7    FORMAT(/'Type constant <',F3.0,'>:',$)
        CALL READF(5,CONST)
        IF(Istatus.NE.0)RETURN
        IF(IXL.GT.4095.OR.IXH.GT.4095.OR.IYL.GT.2047.OR.IYH.GT.2047)THEN
          WRITE(6,*)'x > 4095 or y > 2047'
          RETURN
        ENDIF
		  cLL = rMAT(IDEST,IXL,IYL)
		  cLH = rMAT(IDEST,IXL,IYH)
		  cHL = rMAT(IDEST,IXH,IYL)
		  cHH = rMAT(IDEST,IXH,IYH)
        DO J=IYL,IYH
          DO I=IXL,IXH
            rMAT(IDEST,I,J)=CONST
          ENDDO
        ENDDO
      ELSE
        WRITE(6,1)IDEST
        CALL READI(5,IDEST)
        IF(IDEST.LT.1.OR.IDEST.GT.2)Istatus=1
        IF(Istatus.NE.0)RETURN
        MAXCH=Idim(2,IDEST,1)-1
        WRITE(6,3)IXL
        CALL READI(5,IXL)
        WRITE(6,4)IXH
        CALL READI(5,IXH)
		  ians = 0
		  WRITE(6,8)ians
   8    FORMAT(/'Put constant (0) or linear interpolation (1) <',I1,'>:',$)
        CALL READI(5,ians)
		  IF(ians.EQ.0)THEN
           WRITE(6,7)CONST
           CALL READF(5,CONST)
		  ENDIF
        IF(Istatus.NE.0)RETURN
        DO I=IXL,IXH
          if(ians.EQ.0)rSPEC(IDEST,I)=CONST
			 if(ians.EQ.1)rSPEC(IDEST,I)=(rSPEC(IDEST,IXL)*FLOAT(I-IXH)/FLOAT(IXL-IXH))+
     + (rSPEC(IDEST,IXH)*FLOAT(I-IXL)/FLOAT(IXH-IXL))
        ENDDO
      ENDIF

C Updating comment in the heading of spectrum file
      xcomm(1:3)='PC:'
      CALL AddComment(xcomm,3)
      END


      SUBROUTINE PROJ
      COMMON/FREEZE/ifreeze
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      INTEGER XDIM,YDIM
      CHARACTER APP*4,ANS*1
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,xcomm*60
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LOCH,HICH
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      REAL LDZ,HDZ,LOCNT,HICNT
	  COMMON/SAVEPM/J1,J2,I1,I2
      REAL OLhi(64),OLlo(64),OLlc(64),OLhc(64)
	  
	  COMMON/PMchan/ch1,ch2
      INTEGER ch1,ch2
	  
	  m1=-1
	  m2=-1
      ISP=IDEST
      WRITE(6,1)IDEST
   1  FORMAT('Destination singles spectrum    <',I1,'>:',$)
      CALL READI(5,IDEST)
      WRITE(6,2)ISP
   2  FORMAT( 'Source matrix                   <',I1,'>:',$)
      CALL READI(5,ISP)
      IF(IDEST.LT.1.OR.IDEST.GT.2)Istatus=1
      IF(ISP  .LT.1.OR.ISP  .GT.2)Istatus=1
      IF(Istatus.NE.0)RETURN
      DO I=0,8191
        rSPEC(IDEST,I)=0
      ENDDO
      IF(ANS.NE.'x'.AND.ANS.NE.'X'.AND.ANS.NE.'y'.AND.ANS.NE.'Y')ANS='x'
      WRITE(6,3)ANS
   3  FORMAT('Projection down to x or y axis  <',A1,'>:',$)
      CALL READA1(5,ANS)
      IISTAT=0

      IF(ANS.EQ.'X'.OR.ANS.EQ.'x')THEN
        IF(J1.EQ.0.AND.J2.EQ.0)THEN
          J1=0
          J2=Idim(1,ISP,2)-1
        ENDIF
        IF(ITYPE.EQ.3)THEN
          J1=LDY
          J2=HDY
        ENDIF
        WRITE(6,4)J1
   4    FORMAT('Lower marker on y-axis       <',I4,'>:',$)
        CALL READI(5,J1)
        WRITE(6,5)J2
   5    FORMAT('Higher marker on y-axis      <',I4,'>:',$)
        CALL READI(5,J2)
        IF(J1.GT.2047.OR.J2.GT.2047)Istatus=1
        IF(Istatus.NE.0)RETURN
        IISTAT=1
        IXY=1
        DO I=0,4095
          DO J=J1,J2
            rSPEC(IDEST,I)=rSPEC(IDEST,I)+rMAT(ISP,I,J)
          ENDDO
        ENDDO
		ch1=J1
		ch2=J2
        cal(2,IDEST,1,1)=cal(1,ISP,1,1)
        cal(2,IDEST,1,2)=cal(1,ISP,1,2)
        cal(2,IDEST,1,3)=cal(1,ISP,1,3)
        ITYPE=1
        MAXCH=Idim(1,ISP,1)-1

C Updating comment in the heading of spectrum file
        comm(2,IDEST)(1:60)=''
        xcomm(1:12)=fname(1,ISP)(1:8)//'PMx:'
        write(xcomm(13:16),991,ERR=997)J1
991     FORMAT(I4)
        xcomm(17:17)='-'
        write(xcomm(18:21),991,ERR=997)J2
        CALL AddComment(xcomm,21)
997     fname(2,IDEST)='PM'//fname(1,ISP)(1:6)

      ENDIF

      IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')THEN
        IISTAT=1
        IF(I1.EQ.0.AND.I2.EQ.0)THEN
          I1=0
          I2=Idim(1,ISP,1)-1
        ENDIF
        IF(ITYPE.EQ.3)THEN
          I1=LDX
          I2=HDX
        ENDIF
        WRITE(6,6)I1
   6    FORMAT('Lower marker on x-axis       <',I4,'>:',$)
        CALL READI(5,I1)
        WRITE(6,7)I2
   7    FORMAT('Higher marker on x-axis      <',I4,'>:',$)
        CALL READI(5,I2)
        IF(Istatus.NE.0)RETURN
        IISTAT=1
        IXY=2
        DO J=0,2047
          IF(I1.GT.4095.OR.I2.GT.4095)Istatus=1
          IF(Istatus.NE.0)RETURN
          DO I=I1,I2
            rSPEC(IDEST,J)=rSPEC(IDEST,J)+rMAT(ISP,I,J)
          ENDDO
        ENDDO
		ch1=I1
		ch2=I2
        cal(2,IDEST,1,1)=cal(1,ISP,2,1)
        cal(2,IDEST,1,2)=cal(1,ISP,2,2)
        cal(2,IDEST,1,3)=cal(1,ISP,2,3)
        ITYPE=1
        MAXCH=Idim(1,ISP,2)-1

C Updating comment in the heading of spectrum file
        comm(2,IDEST)(1:60)=''
        xcomm(1:12)=fname(1,ISP)(1:8)//'PMy:'
        write(xcomm(13:16),991,ERR=998)I1
        xcomm(17:17)='-'
        write(xcomm(18:21),991,ERR=998)I2
        CALL AddComment(xcomm,21)
998     fname(2,IDEST)='PM'//fname(1,ISP)(1:6)
      ENDIF

      IF(IISTAT.EQ.0)Istatus=2
      IF(Istatus.NE.0)RETURN
      
C Trying to find out if PM is made in a serie - then displaymarkers are kept the same
      IF(IDEST.NE.IDESTold.OR.HICH.NE.HICHold.OR.IXY.NE.IXYold.OR.
     +   HICNT.EQ.1000.OR.HICH.EQ.8191)THEN
        IF(ifreeze.EQ.1)THEN
          CALL SetMarker(0,0,0)
        ELSE
          CALL SetMarker(1,2,0) !changed from 2,2,0 -> 1,2,0
        ENDIF
      ELSE
        CALL SetMarker(0,0,0)
      ENDIF
   
      WRITE(6,8)IDEST
   8  FORMAT('Projection is stored in singles spectrum',I2)
      IDESTold=IDEST               !Remember for next PM
      HICHold =HICH
      IXYold  =IXY
      RETURN
      END


      SUBROUTINE RANDOMIZE
      INTEGER XDIM,YDIM
      CHARACTER APP*4
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,xcomm*60
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      INTEGER time
      
      IDUM=1
      IF(IDEST.EQ.1)IDUM=2
      IDEST=IDUM
      WRITE(6,1)IDEST
 1    FORMAT('Destination spectrum <',I1,'>:',$)
      CALL READI(5,IDEST)
      ISP=1
      IF(IDEST.EQ.1)ISP=2
      WRITE(6,2)ISP
 2    FORMAT( 'Source spectrum      <',I1,'>:',$)
      CALL READI(5,ISP)

      IF(IDEST.LT.1.OR.IDEST.GT.2)Istatus=1
      IF(ISP  .LT.1.OR.ISP  .GT.2)Istatus=1
      r=rand(time())

C   STARTING THE SMOOTHING PROCEDURE
      IF(ITYPE.GT.1)THEN
        DO J=0,YDIM-1                 ! 3-dim matrices
          JT=(J/30)*30
          IF(JT.EQ.J)THEN
            write(6,FMT='(A1,$)')'.'
            call flush(6)
          ENDIF
          DO I=0,XDIM-1
            r=rand(0) 
            z=Finvert(r)
            rMAT(IDEST,I,J)=rMAT(ISP,I,J)+z*SQRT(ABS(rMAT(ISP,I,J)))
          ENDDO
        ENDDO
        cal(1,IDEST,1,1)=cal(1,ISP,1,1)
        cal(1,IDEST,1,2)=cal(1,ISP,1,2)
        cal(1,IDEST,1,3)=cal(1,ISP,1,3)
        cal(1,IDEST,2,1)=cal(1,ISP,2,1)
        cal(1,IDEST,2,2)=cal(1,ISP,2,2)
        cal(1,IDEST,2,3)=cal(1,ISP,2,3)
C Updating comment in the heading of spectrum file
        xcomm(1:3)='RA:'
        fname(1,IDEST)=fname(1,ISP)
        comm(1,IDEST)=comm(1,ISP)
        CALL AddComment(xcomm,3)
        WRITE(6,*)' '
      ELSE
        DO I=0,MAXCH                  ! 1-dim spectra
          r=rand(0) 
          z=Finvert(r)
          rSPEC(IDEST,I)=rSPEC(ISP,I)+z*SQRT(ABS(rSPEC(ISP,I)))
        ENDDO
        cal(2,IDEST,1,1)=cal(2,ISP,1,1)
        cal(2,IDEST,1,2)=cal(2,ISP,1,2)
        cal(2,IDEST,1,3)=cal(2,ISP,1,3)

C Updating comment in the heading of spectrum file
        xcomm(1:3)='RA:'
        fname(1,IDEST)=fname(1,ISP)
        comm(1,IDEST)=comm(1,ISP)
        CALL AddComment(xcomm,3)
      ENDIF
      END


      FUNCTION Finvert(y0)
C Inverting the monoton increasing function r=F(z) -> z=Finv(r) 
C This means to find the z-value giving the value y0 
C The function F is the cummulative Gauss function F=1/2(1+erf(z/sqrt(2)))
      REAL xl,xh,yl,yh,x,y,y0
      xl =-3.0
      xh = 3.0
      x  = 0.0
      yl = 0.0
      yh = 1.0
      y  = 0.5
      DO WHILE (ABS(y-y0).GT.0.001)
        x = xl + (xh-xl)*(y0-yl)/(yh-yl)
        y = 0.5*(1.+erf(x/1.414213562))
        IF(y.GT.y0)THEN
          yl=y
          xl=x
        ELSE
          yh=y
          xh=x
        ENDIF
      ENDDO
      Finvert=x
      RETURN
      END


      SUBROUTINE REPLACE
      INTEGER XDIM,YDIM
      CHARACTER APP*4
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,xcomm*60
      REAL Mmin, Mmax

      IDUM=1
      IF(IDEST.EQ.1)IDUM=2
      IDEST=IDUM
      WRITE(6,1)IDEST
 1    FORMAT('Destination spectrum <',I1,'>:',$)
      CALL READI(5,IDEST)
      ISP=1
      IF(IDEST.EQ.1)ISP=2
      WRITE(6,2)ISP
 2    FORMAT( 'Source spectrum      <',I1,'>:',$)
      CALL READI(5,ISP)
      IF(IDEST.LT.1.OR.IDEST.GT.2)Istatus=1
      IF(ISP  .LT.1.OR.ISP  .GT.2)Istatus=1

      IF(Istatus.NE.0)RETURN

C Asking for the method to be applied
      WRITE(6,*)' '
      WRITE(6,*)'With this command you may replace the counts in the'
      WRITE(6,*)'spectrum/matrix with a given value. The options are:'
      WRITE(6,*)'Replace counts if they are below a certain value, or,'
      WRITE(6,*)'if the counts are in-between two values, or if the'
      WRITE(6,*)'counts exceed a certain value. The defaults are:'
      WRITE(6,*)'Choose (1), replace-value = 0 and Max = 0 which will'
      WRITE(6,*)'delete all negative numbers.'
      WRITE(6,*)'You may also change floating point values -> integers.'
      WRITE(6,*)'----------------------------------------------------- '
      WRITE(6,*)'      Counts < Max is replaced........... choose: (1)'
      WRITE(6,*)'Min < Counts < Max is replaced........... choose: (2)'
      WRITE(6,*)'Min < Counts       is replaced........... choose: (3)'
      WRITE(6,*)'Replace floating counts by integers...... choose: (4)'
      WRITE(6,*)'Return to MAMA........................... choose: (5)'
      WRITE(6,*)'----------------------------------------------------- '

      NuReplaced = 0
      TotOld = 0.
      TotNew = 0.

      mode = 1
      WRITE(6,4)mode
  4   FORMAT(' Choose your option                               <',I1,'>:',$)
      CALL READI(5,mode)
      IF(mode.EQ.5)RETURN
     
      IF(Istatus.NE.0.OR.mode.LT.1.or.mode.GT.4)THEN
        WRITE(6,*) 'Illegal parameter, valid region:'
        WRITE(6,*) '0 < method < 6'
        RETURN
      ENDIF

      IF(mode.NE.4)THEN
        ReplaceValue = 0.
        WRITE(6,14)ReplaceValue
  14    FORMAT(/'Give new value for the counts to be replaced <',F6.2,'>:',$)
        CALL READF(5,ReplaceValue)

      ENDIF

      IF(Istatus.NE.0)RETURN

      IF(mode.EQ.1)THEN
        Mmax = 0.
        WRITE(6,10)Mmax
 10     FORMAT('Delete counts with value lower than          <',F6.2,'>:',$)
        CALL READF(5,Mmax)
      ENDIF
      IF(mode.EQ.2)THEN
        WRITE(6,15)
 15     FORMAT('Answer two questions for lower and higher limits:')
        Mmin = -4.
        WRITE(6,11)Mmin
 11     FORMAT('Delete counts with value higher than         <',F6.2,'>:',$)
        CALL READF(5,Mmin)
        Mmax = MIN(ABS(Mmin),4.)
        WRITE(6,12)Mmax
 12     FORMAT('AND with value lower than                    <',F6.2,'>:',$)
        CALL READF(5,Mmax)
        IF(Mmax.LT.Mmin)THEN
          Write(6,*)'Mmax < Mmin, try again...'
          Istatus=1
        ENDIF
      ENDIF
      IF(mode.EQ.3)THEN
        Mmin = 100000.
        WRITE(6,13)Mmin
 13     FORMAT('Delete counts with value higher than      <',F9.1,'>:',$)
        CALL READF(5,Mmin)
      ENDIF

      IF(Istatus.NE.0)RETURN

      IF(ITYPE.GT.1)THEN
        XDIM=Idim(1,ISP,1)
        YDIM=Idim(1,ISP,2)
        DO J=0,YDIM-1
          DO I=0,XDIM-1
            xx=rMAT(ISP,I,J)
            TotOld=TotOld+xx
          ENDDO
        ENDDO

        DO J=0,YDIM-1
          DO I=0,XDIM-1
            xx=rMAT(ISP,I,J)
            IF(mode.EQ.1)THEN
              IF(xx.LT.Mmax)THEN
                NuReplaced=NuReplaced+1
                xx=ReplaceValue
              ENDIF
            ENDIF
            IF(mode.EQ.2)THEN
              IF(xx.GT.Mmin.AND.xx.LT.Mmax)THEN
                NuReplaced=NuReplaced+1
                xx=ReplaceValue
              ENDIF
            ENDIF
            IF(mode.EQ.3)THEN
              IF(xx.GT.Mmin)THEN
                NuReplaced=NuReplaced+1
                xx=ReplaceValue
              ENDIF
            ENDIF
            IF(mode.EQ.4)THEN
              NuReplaced=NuReplaced+1
              xx=INT(xx+0.5)
            ENDIF
            rMAT(IDEST,I,J)=xx
            TotNew = TotNew + xx
          ENDDO
        ENDDO

        cal(1,IDEST,1,1)=cal(1,ISP,1,1)
        cal(1,IDEST,1,2)=cal(1,ISP,1,2)
        cal(1,IDEST,1,3)=cal(1,ISP,1,3)
        cal(1,IDEST,2,1)=cal(1,ISP,2,1)
        cal(1,IDEST,2,2)=cal(1,ISP,2,2)
        cal(1,IDEST,2,3)=cal(1,ISP,2,3)
C Updating comment in the heading of spectrum file
        xcomm(1:3)='RN:'
        fname(1,IDEST)=fname(1,ISP)
        comm(1,IDEST)=comm(1,ISP)
        CALL AddComment(xcomm,3)
      ELSE
        MAXCH=Idim(2,ISP,1)-1
        DO I=0,MAXCH
          xx=rSPEC(ISP,I)
          TotOld=TotOld+xx
        ENDDO
        DO I=0,MAXCH
          xx=rSPEC(ISP,I)
          IF(mode.EQ.1)THEN
            IF(xx.LT.Mmax)THEN
              NuReplaced=NuReplaced+1
              xx=ReplaceValue
            ENDIF
          ENDIF
          IF(mode.EQ.2)THEN
            IF(xx.GT.Mmin.AND.xx.LT.Mmax)THEN
              NuReplaced=NuReplaced+1
              xx=ReplaceValue
            ENDIF
          ENDIF
          IF(mode.EQ.3)THEN
            IF(xx.GT.Mmin)THEN
              NuReplaced=NuReplaced+1
              xx=ReplaceValue
            ENDIF
          ENDIF
          IF(mode.EQ.4)THEN
            NuReplaced=NuReplaced+1
            xx=INT(xx+0.5)
          ENDIF
          rSPEC(IDEST,I)=xx
          TotNew = TotNew + xx
        ENDDO
        cal(2,IDEST,1,1)=cal(2,ISP,1,1)
        cal(2,IDEST,1,2)=cal(2,ISP,1,2)
        cal(2,IDEST,1,3)=cal(2,ISP,1,3)
C Updating comment in the heading of spectrum file
        xcomm(1:3)='RN:'
        fname(2,IDEST)=fname(2,ISP)
        comm(2,IDEST)=comm(2,ISP)
        CALL AddComment(xcomm,3)
      ENDIF

      WRITE(6,20)NuReplaced
  20  FORMAT(/'Number of channels replaced:',I14)
      WRITE(6,21)TotOld
  21  FORMAT('Number of counts before:     ',F13.1)
      WRITE(6,22)TotNew
  22  FORMAT('Number of counts after:      ',F13.1)
      WRITE(6,23)TotNew-TotOld
  23  FORMAT('Increase of counts:          ',F13.1)

      END



      SUBROUTINE SMOOTH
      DIMENSION WEIGHT(-100:100,-100:100)
      INTEGER XDIM,YDIM,DELX,DELY
      CHARACTER APP*4,ANS*1
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,xcomm*60
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      
      IDUM=1
      IF(IDEST.EQ.1)IDUM=2
      IDEST=IDUM
      WRITE(6,1)IDEST
 1    FORMAT('Destination spectrum <',I1,'>:',$)
      CALL READI(5,IDEST)
      ISP=1
      IF(IDEST.EQ.1)ISP=2
      WRITE(6,2)ISP
 2    FORMAT( 'Source spectrum      <',I1,'>:',$)
      CALL READI(5,ISP)
      IF(ISP.EQ.IDEST)THEN
        WRITE(6,*)'Destination must be another spectrum'
        Istatus=1
        RETURN
      ENDIF
      IF(IDEST.LT.1.OR.IDEST.GT.2)Istatus=1
      IF(ISP  .LT.1.OR.ISP  .GT.2)Istatus=1

      IF(ITYPE.GT.1)THEN
        XDIM=Idim(1,ISP,1)
        YDIM=Idim(1,ISP,2)

        WRITE(6,3)XDIM
 3      FORMAT(/'Dimension along x-axis <',I4,'>:',$)
        CALL READI(5,XDIM)       
        WRITE(6,4)YDIM
 4      FORMAT( 'Dimension along y-axis <',I4,'>:',$)
        CALL READI(5,YDIM)
        IF(Istatus.NE.0)RETURN
        IXL=(XDIM/10.)+.5
        IXH=(XDIM-IXL)
        IYL=(YDIM/10.)+.5
        IYH=(YDIM-IYL)
        IF(IXL.LT.1)IXL=1
        IF(IXH.LT.1)IXH=1
        IF(IYL.LT.1)IYL=1
        IF(IYH.LT.1)IYH=1

        FWXL=1.
        FWXH=FWXL*SQRT(float(IXH/IXL))
        FWYL=1.
        FWYH=1.
        IF(XDIM.LT.2.OR.YDIM.LT.2)THEN
          WRITE(6,*)'Too small matrix'
          Istatus=1
          RETURN
        ENDIF

      ELSE
        MAXCH=Idim(2,ISP,1)-1
        IMAXCH=MAXCH+1
        WRITE(6,111)IMAXCH
 111    FORMAT(/'Dimension of spectrum <',I4,'>:',$)
        CALL READI(5,IMAXCH)
        IF(Istatus.NE.0)RETURN
        IXL=(IMAXCH/10.)+.5
        IXH=(IMAXCH-IXL)
        IF(IXL.LT.1)IXL=1
        IF(IXH.LT.1)IXH=1
        MAXCH=IMAXCH-1
        IYL=0
        IYH=0
        FWXL=1.
        FWXH=FWXL*SQRT(float(IXH/IXL))
        FWYL=0
        FWYH=0
        IF(MAXCH.LE.1)THEN
          WRITE(6,*)'Too short spectrum'
          Istatus=1
          RETURN
        ENDIF

      ENDIF
    

 34   CONTINUE
      WRITE(6,10)IXL,FWXL
 10   FORMAT(/'Write FWHMx (ch) around ch x= ',I4,' <',F6.1,'>:',$)
      CALL READF(5,FWXL)
      WRITE(6,11)IXH,FWXH
 11   FORMAT( 'Write FWHMx (ch) around ch x= ',I4,' <',F6.1,'>:',$)
      CALL READF(5,FWXH)

      IF(ITYPE.EQ.1)THEN
        FWYL=0.
        FWYH=0.
      ELSE
        WRITE(6,20)IYL,FWYL
 20     FORMAT(/'Write FWHMy (ch) around ch y= ',I4,' <',F6.1,'>:',$)
        CALL READF(5,FWYL)
        WRITE(6,21)IYH,FWYH
 21     FORMAT( 'Write FWHMy (ch) around ch y= ',I4,' <',F6.1,'>:',$)
        CALL READF(5,FWYH)
      ENDIF
      IF(Istatus.NE.0)RETURN

C Finding parametrization of fwhm.: FWHM = A + B * SQRT(ch)
      AY=0
      BY=0
      IF(IXL.NE.IXH)THEN
        BX=(FWXL-FWXH)/(SQRT(float(IXL))-SQRT(float(IXH)))
      ELSE
        BX=0
      ENDIF
      AX=FWXL-BX*SQRT(float(IXL))
      IF(ITYPE.GT.1)THEN
        IF(IYL.NE.IYH)THEN
          BY=(FWYL-FWYH)/(SQRT(float(IYL))-SQRT(float(IYH)))
        ELSE
          BY=0
        ENDIF
        AY=FWYL-BY*SQRT(float(IYL))
      ENDIF
      WRITE(6,*)'FWHM have been expressed by A + B * SQRT(ch):'
      WRITE(6,22)AX,BX,AY,BY
 22   FORMAT('Ax=',F8.4,'  Bx=',F8.4,'       Ay=',F8.4,' By=',F8.4)

C Displaying probability matrix at (xl,yl) and (xh,yh)

      IX=IXL
      IY=IYL
      CALL GAUSSR(IX,IY,AX,BX,AY,BY,DELX,DELY,WEIGHT)
      WRITE(6,31)IX,IY
 31   FORMAT('Smoothing-matrix around (x,y)=(',I4,',',I4,'):')
      DO J=-4,4
        WRITE(6,30)(WEIGHT(I,J),I=-7,7)
      ENDDO

      IX=IXH
      IY=IYH
      CALL GAUSSR(IX,IY,AX,BX,AY,BY,DELX,DELY,WEIGHT)
      WRITE(6,32)IX,IY
 32   FORMAT(/'Smoothing-matrix around (x,y)=(',I4,',',I4,'):')
      DO J=-4,4
        WRITE(6,30)(WEIGHT(I,J),I=-7,7)
      ENDDO
 30   FORMAT(1X,15F5.3)
      ANS='y'
      WRITE(6,33)ANS
 33   FORMAT(/,'Smoothing-matrix OK? (y/n) <',A1,'>:',$)
      CALL READA1(5,ANS)
      IF(ANS.EQ.'N'.OR.ANS.EQ.'n') GO TO 34
      IF(Istatus.NE.0)RETURN

C   STARTING THE SMOOTHING PROCEDURE
      IF(ITYPE.GT.1)THEN
        DO J=0,YDIM-1                 ! 3-dim matrices
          JT=(J/10)*10
          IF(JT.EQ.J)THEN
            write(6,FMT='(A1,$)')'.'
            call flush(6)
          ENDIF
          DO I=0,XDIM-1
            IT=(I/10)*10
            IF(IT.EQ.I.OR.JT.EQ.J)THEN
              CALL GAUSSR(I,J,AX,BX,AY,BY,DELX,DELY,WEIGHT)
            ENDIF
            H=0.
            W=0.
            DO JJ=-DELY,DELY
              DO II=-DELX,DELX
                III=I+II
                JJJ=J+JJ
             IF(III.LT.0.OR.III.GE.XDIM.OR.JJJ.LT.0.OR.JJJ.GE.YDIM)THEN
                  W=W+WEIGHT(II,JJ)
                ELSE
                  H=H+rMAT(ISP,III,JJJ)*WEIGHT(II,JJ)
                ENDIF
              ENDDO
            ENDDO
            cmW=MAX((1.-W),0.25)
            Z=H/cmW                   !Compensate for lost W, max 4x
            rMAT(IDEST,I,J)=Z
          ENDDO
        ENDDO
        cal(1,IDEST,1,1)=cal(1,ISP,1,1)
        cal(1,IDEST,1,2)=cal(1,ISP,1,2)
        cal(1,IDEST,1,3)=cal(1,ISP,1,3)
        cal(1,IDEST,2,1)=cal(1,ISP,2,1)
        cal(1,IDEST,2,2)=cal(1,ISP,2,2)
        cal(1,IDEST,2,3)=cal(1,ISP,2,3)

C Updating comment in the heading of spectrum file
        xcomm(1:3)='SM:'
        fname(1,IDEST)=fname(1,ISP)
        comm(1,IDEST)=comm(1,ISP)
        CALL AddComment(xcomm,3)
        WRITE(6,*)' '

      ELSE

        DO I=0,MAXCH                  ! 1-dim spectra
          IT=(I/10)*10
          IF(IT.EQ.I)THEN
            CALL GAUSSR(I,0,AX,BX,0.,0.,DELX,DELY,WEIGHT)
          ENDIF
          H=0.
          W=0.
          DO II=-DELX,DELX
            III=I+II
            IF(III.LT.0.OR.III.GE.MAXCH+1)THEN
              W=W+WEIGHT(II,0)
            ELSE
              H=H+rSPEC(ISP,III)*WEIGHT(II,0)
            ENDIF
          ENDDO
          cmW=MAX((1.-W),0.50)
          Z=H/cmW                     !Compensate for lost W, max 2x
          rSPEC(IDEST,I)=Z
        ENDDO
        cal(2,IDEST,1,1)=cal(2,ISP,1,1)
        cal(2,IDEST,1,2)=cal(2,ISP,1,2)
        cal(2,IDEST,1,3)=cal(2,ISP,1,3)

C Updating comment in the heading of spectrum file
        xcomm(1:3)='SM:'
        fname(1,IDEST)=fname(1,ISP)
        comm(1,IDEST)=comm(1,ISP)
        CALL AddComment(xcomm,3)
      ENDIF
      END


      SUBROUTINE SHOW
      DIMENSION IAXIS(12)
      INTEGER XDIM,YDIM
      CHARACTER APP*4
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60

      IX=0
      IY=0

      IF(ITYPE.GT.1)THEN                           !Matrix
         WRITE(6,1)IDEST
   1    FORMAT(/'Show matrix no <',I1,'>:',$)
        CALL READI(5,IDEST)
        IF(IDEST.LT.1.OR.IDEST.GT.2)Istatus=1
        IF(Istatus.NE.0)RETURN

        XDIM=Idim(1,IDEST,1)
        YDIM=Idim(1,IDEST,2)
        WRITE(6,2)IX
   2    FORMAT(/'around x-channel <',I5,'>:',$)
        CALL READI(5,IX)
        WRITE(6,3)IY
   3    FORMAT( 'and y-channel    <',I5,'>:',$)
        CALL READI(5,IY)
        IF(Istatus.NE.0)RETURN
        I1=IX-5
        I2=IX+6
        IF(I1.LT.0)THEN
          I1=0
          I2=11
        ENDIF
        IF(I2.GT.4095)THEN
          I2=4095
          I1=I2-11
        ENDIF
        J1=IY-10
        J2=IY+9
        IF(J1.LT.0)THEN
          J1=0
          J2=19
        ENDIF
        IF(J2.GT.2047)THEN
          J2=2047
          J1=2047-19
        ENDIF
        II=0
        DO I=I1,I2
          II=II+1
          IAXIS(II)=I
        ENDDO
        WRITE(6,4)(IAXIS(II),II=1,12)
   4    FORMAT('   Y / X',12I6)
        WRITE(6,*)' '
        DO J=J1,J2
          WRITE(6,5)J,(rMAT(IDEST,I,J),I=I1,I2)
   5      FORMAT(2X,I3,3X,12F6.0)
        ENDDO

      ELSE
                                            !Singles spectrum
        WRITE(6,11)IDEST
  11    FORMAT(/'Show spectrum no <',I1,'>:',$)
        CALL READI(5,IDEST)
        IF(IDEST.LT.1.OR.IDEST.GT.2)Istatus=1
        IF(Istatus.NE.0)RETURN

        MAXCH=Idim(2,IDEST,1)-1
        WRITE(6,12)IX
  12    FORMAT(/'around channel <',I5,'>:',$)
        CALL READI(5,IX)
        IF(Istatus.NE.0)RETURN
        I1=IX-5
        I2=IX+6
        IF(I1.LT.0)THEN
          I1=0
          I2=11
        ENDIF
        IF(I2.GT.8191)THEN
          I2=8191
          I1=I2-11
        ENDIF  
        II=0
        DO I=I1,I2
          II=II+1
          IAXIS(II)=I
        ENDDO

        WRITE(6,*)' Channel    Counts'
        DO i=1,12
           ii=I1-1+i
           IF(ii.GT.8191)GO TO 15
           WRITE(6,14)IAXIS(i),rSPEC(IDEST,ii)
  14       FORMAT(I6,E14.6)
        ENDDO
  15    WRITE(6,*)' ' 

      ENDIF
      END


      SUBROUTINE TOUCH
      INTEGER XDIM,YDIM
      CHARACTER APP*4
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH

      DIMENSION xxA(2,2)
  
C The IIAs contain the areas of the 2 matrices and spectra
      IF(ITYPE.EQ.1)IIT=2      !Singles spectrum
      IF(ITYPE.NE.1)IIT=1      !2-dim. matrix

      IF(IIT.EQ.1)THEN
        XDIM=Idim(1,IDEST,1)
        YDIM=Idim(1,IDEST,2)
            
        xxA(IIT,IDEST)=0
        DO J=0,YDIM-1
          JT=(J/50)*50
          IF(JT.EQ.J)THEN
            write(6,FMT='(A1,$)')'.'
            call flush(6)
          ENDIF
          DO I=0,XDIM-1
            xxA(IIT,IDEST)=xxA(IIT,IDEST)+rMAT(IDEST,I,J)
          ENDDO
        ENDDO
        WRITE(6,*)' '
        WRITE(6,*)'Name:   ',fname(1,IDEST)
        WRITE(6,*)'Comment:',comm(1,IDEST)
        WRITE(6,6)IDEST,XDIM,YDIM,
     1  (cal(1,IDEST,1,i),i=1,3),(cal(1,IDEST,2,i),i=1,3)
6       FORMAT(' Matrix ',I1,' has dimension ',
     1  I4,'*',I3,' with calibration:',/,
     1 ' Along x-axis: a0=',E13.6,' a1=',E13.6,' and a2=',E13.6,/,
     1 ' Along y-axis: a0=',E13.6,' a1=',E13.6,' and a2=',E13.6)

        WRITE(6,7)XDIM-1,YDIM-1,xxA(IIT,IDEST)
7       FORMAT(' Counts in matrix (0:',I4,',0:',I3,') = ',E13.6)
        WRITE(6,*)' '
      ELSE
        MAXCH=Idim(2,IDEST,1)-1
        IMAXCH=MAXCH+1
        WRITE(6,*)'Name:   ',fname(2,IDEST)
        WRITE(6,*)'Comment:',comm(2,IDEST)
        WRITE(6,8)IDEST,IMAXCH,
     1  (cal(2,IDEST,1,i),i=1,3)
8       FORMAT(' Spectrum ',I1,' has dimension ',
     1 I5,' with calibration:',/,
     1  ' a0=',E13.6,' a1=',E13.6,' and a2=',E13.6)
        xxA(IIT,IDEST)=0
        DO I=0,MAXCH
          xxA(IIT,IDEST)=xxA(IIT,IDEST)+rSPEC(IDEST,I)
        ENDDO
        WRITE(6,9)MAXCH,xxA(IIT,IDEST)
9       FORMAT(' Counts in spectrum (0:',I4,')= ',E13.5)
        WRITE(6,*)' '
      ENDIF
      END


      SUBROUTINE TRANS2MATRIX
C Transforms a singles spectrum into a matrix of 1 or several equal rows
      INTEGER XDIM,YDIM
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,xcomm*60
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH

      COMMON/LASTj/jnext,jstat           ! To remember next j-value

      WRITE(6,1)IDEST
 1    FORMAT('Destination matrix      <',I1,'>:',$)
      CALL READI(5,IDEST)
      ISP=IDEST
      WRITE(6,2)ISP
 2    FORMAT( 'Source singles spectrum <',I1,'>:',$)
      CALL READI(5,ISP)
      IF(IDEST.LT.1.OR.IDEST.GT.2)Istatus=1
      IF(ISP  .LT.1.OR.ISP  .GT.2)Istatus=1
      IF(jnext.GT.2047)jnext=2047
      IF(jnext.LT.0)  jnext=0
      IF(Istatus.NE.0)RETURN

      WRITE(6,*)'The spectrum can be copied to 1 or several rows'
      WRITE(6,*)'Transform spectrum to rows (y-chs) between:' 

      IF(jstat.EQ.1)jlow =jnext    !The routine has been accessed before
      WRITE(6,3)jlow
 3    FORMAT(/' Lower row  <',I3,'>:',$)
      CALL READI(5,jlow)
      jhigh=jlow
      WRITE(6,4)jhigh
 4    FORMAT( ' Higher row <',I3,'>:',$)
      CALL READI(5,jhigh)
      IF(jhigh.GT.2047)jhigh=2047
      IF(Istatus.NE.0)RETURN

      XDIM=Idim(2,ISP,1)
      IF(XDIM.GT.4096)XDIM=4096
      YDIM=jhigh+1

      DO j=jlow,jhigh
        DO i=0,XDIM-1
          rMAT(IDEST,i,j)=rSPEC(ISP,i)
        ENDDO
      ENDDO

      ITYPE=3
      CALL SetMarker(1,1,1)

      cal(1,IDEST,1,1)=cal(2,ISP,1,1)
      cal(1,IDEST,1,2)=cal(2,ISP,1,2)
      cal(1,IDEST,1,3)=cal(2,ISP,1,3)
      cal(1,IDEST,2,1)=0
      cal(1,IDEST,2,2)=1.
      cal(1,IDEST,2,3)=0.
      jnext=jhigh+1
      jstat=1

C Updating comment in the heading of spectrum file
      xcomm(1:3)='TR:'
      CALL AddComment(xcomm,3)
      fname(2,IDEST)='TR'//fname(1,ISP)(1:6)
      RETURN
      END


      SUBROUTINE UNIXCMD
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      CHARACTER string*40
      INTEGER status, system
      string='ls'
      WRITE(6,1)string(1:2)
 1    FORMAT(/'Type your UNIX command <',A,'>:',$)
      CALL READA(5,string)
      IF(Istatus.NE.0)RETURN

      status=SYSTEM(string)
      IF(status.NE.0)WRITE(6,*)'Error, command not understood'
      RETURN
 99   WRITE(6,*)'Error, command not understood'
      END
      

      FUNCTION CORR(EG,THETA)
C Correction to number of counts due to DeltaTheta
      REAL EG,THETA,A,B,C
      A = EG*(EG/511.)*SIN(THETA)
      B = 1. + (EG/511.)*(1. - COS(THETA))
      C = A/(B**2.)
      CORR = C
      END

      FUNCTION FERMI(AA,XJ,EX,EGAP)
C CALLED FROM FUNC1, FUNC4, FUNC3 AND FUNC5. FINDS THE LEVEL DENSITY 
C AS FUNCTION OF
C INTRINSIC EXC. ENERGY=EX-EGAP-300. WE ASSUME YRAST TO BE 300 keV.
C CALCULATE DOWN TO ENERGY U=4/a ,WHICH IS U = ca.200 keV.
      FERMI=0
      U=EX-EGAP-300.
      UMIN=4./AA
      IF(UMIN.LT.200)UMIN=200.

      IF(U.LE.0)RETURN
      IF(U.GT.UMIN.AND.U.GT.0)THEN
        FERMI=((2.*XJ+1.)*SQRT(AA)*EXP(2.*SQRT(AA*U)))/(U*U)
        RETURN
      ENDIF
      IF(U.LE.UMIN.AND.U.GT.0)THEN
       FERMI=((2.*XJ+1.)*SQRT(AA)*EXP(2.*SQRT(AA*UMIN)))/(UMIN*UMIN)
        RETURN
      ENDIF
      RETURN
      END


      FUNCTION FUNC1(EG,A)
C CALCULATES THE THE FIRST GENERATION GAMMA-SPECTRUM AS A(1)*EXPN*RHO
C WE AVERAGE OVER 6 EXCITATION ENERGIES BETWEEN EXL TO EXH, DUE TO DETECTOR
C RESOLUTION. ENERGY UNITS IN MEV
      REAL A(2),N,MASS
      COMMON/CNUTE/EX,EXL,EXH,XJ,EGAP,MASS,EXPN(0:4095),N(0:4095),FWXG
      COMMON/CALxy/AX0,AX1,AY0,AY1
      FUNC1=0
      IX=((EG-AX0)/AX1+0.5)
      IF(IX.LT.0)RETURN
      AA=A(2)
      DEL=EXH-EXL
      DELST=DEL/5.
      DO I=1,6
        EXX=EXL+FLOAT(I-1)*DELST-EG
         RHO=FERMI(AA,XJ,EXX,EGAP)
        FUNC1=A(1)*EXPN(IX)*RHO+FUNC1
      ENDDO
      FUNC1=FUNC1/6.
      RETURN         
      END


      FUNCTION FUNC2(ALOGEG,A)
      DIMENSION A(2)
      FUNC2=A(1)+A(2)*ALOGEG
      RETURN
      END


      FUNCTION FUNC3(EXF,A)
      REAL N,A(2),MASS
      COMMON/CNUTE/EX,EXL,EXH,XJ,EGAP,MASS,EXPN(0:4095),N(0:4095),FWXG
      FUNC3=0
      AA=A(2)
      VENT=FWXG
      FWXG=0
      EXX=EXF-FWXG/2.
      DELST=FWXG/5.
      FWXG=VENT
      DO I=1,6
        EXX=EXX+FLOAT(I-1)*DELST
        RHO=FERMI(AA,XJ,EXX,EGAP)
        FUNC3=A(1)*RHO+FUNC3
      ENDDO
      FUNC3=FUNC3/6.
      FUNC3=ALOG(FUNC3)
      RETURN         
      END


      FUNCTION FUNC4(XX,A)
C CALCULATES THE THE FIRST GENERATION GAMMA-SPECTRUM AS A(1)*EXPN*RHO
C WE AVERAGE OVER 6 EXCITATION ENERGIES BETWEEN EXL TO EXH, DUE TO DETECTOR
C RESOLUTION. ENERGYUNITS ARE IN MEV. PARAMETER XX IS ACTUAL NUMBER OF
C DATAPOINT

      CHARACTER APP*4
      INTEGER XDIM,YDIM
      REAL A(2),P(2),N,MASS
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      COMMON/CNUTE/EX,EXL,EXH,XJ,EGAP,MASS,EXPN(0:4095),N(0:4095),FWXG
      COMMON/CALxy/AX0,AX1,AY0,AY1
      COMMON/CTOTFIT/XEN(50000),YEN(50000),NPTS,EXV,FX,EXMIN,ISP
      EXTERNAL FUNC1,FERMI
      ISP=1
      IF(IDEST.EQ.1)ISP=2
      FUNC4=0.
      III=XX+0.5
      EG=XEN(III)
      EX=YEN(III)
      EXL=EX-FWXG/2.
      EXH=EX+FWXG/2.
      IF(EXL.LT.0)EXL=0.
      IEG=((EG-AX0)/AX1+0.5)
      JEX=((EX-AY0)/AY1+0.5)
      IF(IEG.LT.0.OR.JEX.LT.0)RETURN

      AA=A(2)
      DEL=EXH-EXL
      DELST=DEL/5.
      DO I=1,6
        EXX=EXL+FLOAT(I-1)*DELST-EG
        RHO=FERMI(AA,XJ,EXX,EGAP)
        FUNC4=FUNC4+A(1)*EXPN(IEG)*RHO
      ENDDO
         
C CALCULATING F(Ex)
      IF(EX.NE.EXV.OR.FX.EQ.0)THEN
        IMAX  =((EX-EXMIN-AX0)/AX1+0.5)
        IUPPER=((EXH-AX0)/AX1+0.5)
        IF(IMAX.GT.2047)IMAX=2047
        IF(IUPPER.GT.2047)IUPPER=2047
        FX=0.
        GAS=0.
        GSB=0.
        P(1)=1.
        P(2)=A(2)
        DO I=0,IMAX
          EE=AX1*I+AX0
          IF(EE.GT.500.)FX=FX+FUNC1(EE,P)
          IF(EE.GT.500.)GAS=GAS+rMAT(ISP,I,JEX)
        ENDDO
        DO I=IMAX+1,IUPPER
          IF(EE.GT.500.)GSB=GSB+rMAT(ISP,I,JEX)
        ENDDO
C CORRECT FOR DECAY TO STATES BELOW FERMI-GAS. (TAKEN FROM EXP. SPECTRA)
        FRAC=0.
        IF(GAS.GT.0.)FRAC=GSB/GAS
        FRAC=1.+FRAC
        FX=FX*FRAC
C        IF(FX.GT.0)WRITE(6,24)EX,FRAC,FX
C 24    FORMAT('Exc.=',F5.0,' Corr. fac.=',
C     +  F5.2,' F(Ex)=',E12.5)
      EXV=EX
      ENDIF
      IF(FX.GT.0)FUNC4=FUNC4/(6.*FX)
      RETURN         
      END


      FUNCTION FUNC5(A)
C CALCULATES A(1)*RHO
C WE AVERAGE OVER 6 EXCITATION ENERGIES BETWEEN EXL TO EXH, DUE TO DETECTOR
C RESOLUTION. ENERGY UNITS ARE IN MEV
      REAL A(2),N,MASS
      COMMON/CNUTE/EX,EXL,EXH,XJ,EGAP,MASS,EXPN(0:4095),N(0:4095),FWXG
      FUNC5=0
      AA=A(2)
      DEL=EXH-EXL
      DELST=DEL/5.
      DO I=1,6
        EXX=EXL+FLOAT(I-1)*DELST
        RHO=FERMI(AA,XJ,EXX,EGAP)
        FUNC5=A(1)*RHO+FUNC5
      ENDDO
      FUNC5=FUNC5/6.
      RETURN         
      END
	  
	  SUBROUTINE CutDiag
      CHARACTER APP*4,ANS
      INTEGER XDIM,YDIM,RDIM,UPPER(0:2047)
      INTEGER LOW,HIGH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,xcomm*60
      IF(ITYPE.GT.1)THEN
        LEN = XDIM -1
      ELSE
       Istatus = 1
      ENDIF
      IF(Istatus.NE.0)RETURN
     
C Defining upper border for the unfolding and chisq-test
      Ix1=XDIM-1
      Ix2=XDIM-1
      Iy1=YDIM-1
      Iy2=0
  
	  WRITE(6,*)'Give limits for the cut. Data at higher x-values' 
	  WRITE(6,*)'(to the right of the diagonal) are zeroed. The diagonal'
	  WRITE(6,*)'is given by interpolation between (x1,y1) and (x2,y2)'
	  WRITE(6,*)' '
	  WRITE(6,*)'    (x1,y1)  first point'
	  WRITE(6,*)'xxxxxxx'
	  WRITE(6,*)'xxxxxxxxx'
	  WRITE(6,*)'xxxxxxxxxxx'
	  WRITE(6,*)'xx matrix xxx'
	  WRITE(6,*)'xxxxxxxxxxxxxxx'
	  WRITE(6,*)'            (x2,y2)  second point'
	  WRITE(6,*)' '

      WRITE(6,123)Ix1
 123  FORMAT(/'First point x1  <',I5,'>:',$)
      CALL READI(5,Ix1)
      WRITE(6,124)Iy1
 124  FORMAT( 'First point y1  <',I5,'>:',$)
      CALL READI(5,Iy1)
      WRITE(6,125)Ix2
 125  FORMAT( 'Second point x2 <',I5,'>:',$)
      CALL READI(5,Ix2)
      WRITE(6,126)Iy2
 126  FORMAT( 'Second point y2 <',I5,'>:',$)
      CALL READI(5,Iy2)
      IF(Istatus.NE.0)RETURN

      CF=0.
      Dx12=Ix2-Ix1
      Dy12=Iy2-Iy1
      IF(Iy2.NE.Iy1)CF=Dx12/Dy12
      DO J=0,2047
        upper(J)=Ix1-CF*(FLOAT(Iy1-J))+0.5
        IF(upper(J).LT.0  )upper(J)=0
        IF(upper(J).GT.LEN-1)upper(J)=LEN-1
      ENDDO
      DO J=0,2047                 !MAIN LOOP
        HIGH=UPPER(J)
        DO I=0,4095
          IF(I.GT.HIGH)rMAT(IDEST,I,J)=0
        ENDDO
	  ENDDO
C Updating comment in the heading of spectrum file
	  xcomm(1:3)='CD:'
	  CALL AddComment(xcomm,3)

      END


