      SUBROUTINE OUTLAY(iOL)
C Routine to display part of a matrix as a number of singles spectra
      LOGICAL DISP
      INTEGER XDIM,YDIM
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LOCH,HICH
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      REAL LDZ,HDZ,LOCNT,HICNT
      COMMON/OL/iOLnew,iRC,m1,m2
      COMMON/DisType/Idistype,OLlow,OLhigh,OLlocnt,OLhicnt
      INTEGER                 OLlow,OLhigh

      REAL dex(14)
    
      CHARACTER*28 HEADING
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,oldname*8
      REAL Spec(4096)
      CHARACTER ANS*1,TEX1*3,TEX2*4,TEX*9

      COMMON/AXIS/iCE,itext,UNITx,UNITy,UNITx0,UNITy0
      CHARACTER UNITx*3,UNITy*3,UNITx0*3,UNITy0*3

      INTEGER COLORMAP(20),Color(0:19)
      COMMON /COLORMAP/ COLORMAP,Limit,Color
      REAL Limit(0:19)

      COMMON/SAVEOUTLAY/OLhi,OLlo,OLlc,OLhc
      REAL OLhi(64),OLlo(64),OLlc(64),OLhc(64)

      iblack=20

      iOLnew=iOL
      IF(iOL.LT.0.OR.iOL.GT.3)Istatus=1
      IF(Istatus.NE.0)RETURN

      WRITE(6,1)IDEST
 1    FORMAT(/'Display multiple spectra of matrix <',I2,'>:',$)
      CALL READI(5,IDEST)
      IF(Istatus.NE.0)RETURN
      ITYPE=3
     
      IF(fname(1,IDEST).NE.oldname)THEN
        m1=0
        m2=63
      ENDIF
      oldname=fname(1,IDEST)

      ANS='R'
      WRITE(6,3)ANS
 3    FORMAT(/'Show rows (R) or coloumns (C) as spectra <',A1,'>:',$)
      CALL READA1(5,ANS)
      IF(Istatus.NE.0)RETURN

      IISTAT=0
      IF(ANS.EQ.'R'.OR.ANS.EQ.'r')THEN
        WRITE(6,*)'Choose spectra (or y-channels, max=64)'
        m1=MAX0(0,m1)
        WRITE(6,4)m1
 4      FORMAT(/'Lower channel on y-axis  <',I5,'>:',$)
        CALL READI(5,m1)
        IF(m2.GE.m1.AND.m2.LE.m1+63)THEN
          m2=m2
        ELSE
          m2=m1+63
        ENDIF
        IF(m2.GT.YDIM-1)m2=YDIM-1
        WRITE(6,5)m2
 5      FORMAT( 'Higher channel on y-axis <',I5,'>:',$)
        CALL READI(5,m2)
        IF(m1.LT.  0.OR.m2.LT.  0)Istatus=2
        IF(m1.GT.511.OR.m2.GT.511)Istatus=2
        I2=ABS(m1-m2)+1            ! Number of spectra
        maxdim=XDIM                ! Max dimension of spectra
        TEX1='Y: '
        IF(I2.GT.64)Istatus=2
        IF(Istatus.NE.0)RETURN
        IISTAT=1
      ENDIF

      IF(ANS.EQ.'C'.OR.ANS.EQ.'c')THEN
        WRITE(6,*)'Choose spectra (or x-channels, max=64)'
        m1=MAX0(0,m1)
        WRITE(6,6)m1
 6      FORMAT(/'Lower channel on x-axis  <',I5,'>:',$)
        CALL READI(5,m1)
        IF(m2.GE.m1.AND.m2.LE.m1+63)THEN
          m2=m2
        ELSE
          m2=m1+63
        ENDIF
        IF(m2.GT.XDIM-1)m2=XDIM-1
        WRITE(6,7)m2
 7      FORMAT( 'Higher channel on x-axis <',I5,'>:',$)
        CALL READI(5,m2)
        IF(m1.LT.   0.OR.m2.LT.   0)Istatus=2
        IF(m1.GT.4095.OR.m2.GT.4095)Istatus=2
        I2=ABS(m1-m2)+1           ! Number of spectra
        maxdim=YDIM               ! Max dimension of spectra
        TEX1='X: '
        IF(I2.GT.64)Istatus=2
        IF(Istatus.NE.0)RETURN
        IISTAT=1
      ENDIF

C Want to display I2 spectra 
C Parting the screen into inx*iny parts
      inx=1
      iny=1      
      IF(I2.GT. 1.AND.I2.LE. 2)THEN !2*1 spectra
        inx=2
        iny=1
      ENDIF
      IF(I2.GT. 2.AND.I2.LE. 4)THEN !2*2 spectra
        inx=2
        iny=2
      ENDIF
      IF(I2.GT. 4.AND.I2.LE. 6)THEN !3*2 spectra
        inx=3
        iny=2
      ENDIF
      IF(I2.GT. 6.AND.I2.LE. 9)THEN !3*3 spectra
        inx=3
        iny=3
      ENDIF
      IF(I2.GT. 9.AND.I2.LE.12)THEN !4*3 spectra
        inx=4
        iny=3
      ENDIF
      IF(I2.GT.12.AND.I2.LE.16)THEN !4*4 spectra
        inx=4
        iny=4
      ENDIF
      IF(I2.GT.16.AND.I2.LE.20)THEN !5*4 spectra
        inx=5
        iny=4
      ENDIF
      IF(I2.GT.20.AND.I2.LE.25)THEN !5*5 spectra
        inx=5
        iny=5
      ENDIF
      IF(I2.GT.25.AND.I2.LE.30)THEN !6*5 spectra
        inx=6
        iny=5
      ENDIF
      IF(I2.GT.30.AND.I2.LE.36)THEN !6*6 spectra
        inx=6
        iny=6
      ENDIF           
      IF(I2.GT.36.AND.I2.LE.42)THEN !7*6 spectra
        inx=7
        iny=6
      ENDIF
      IF(I2.GT.42.AND.I2.LE.49)THEN !7*7 spectra
        inx=7
        iny=7
      ENDIF
      IF(I2.GT.49.AND.I2.LE.56)THEN !8*7 spectra
        inx=8
        iny=7
      ENDIF
      IF(I2.GT.56.AND.I2.LE.64)THEN !8*8 spectra
        inx=8
        iny=8
      ENDIF

      IF(IISTAT.EQ.0)Istatus=2
      IF(Istatus.NE.0)RETURN

      CALL INITG(NX0,NY0) 
      NY=NY0/iny           !Number of y-pixels pr. spectrum
      NX=NX0/inx           !Number of x-pixels pr. spectrum

C Drawing grid and frame for inx*iny spectra
      CALL SETCOLOR(iblack)
      DO i=0,inx
        CALL KTRAS(i*NX,20    ,0)
        CALL KTRAS(i*NX,NY0+18,1)
      ENDDO
      DO i=0,iny-1
        CALL KTRAS(0  ,i*NY+20,0)
        CALL KTRAS(NX0,i*NY+20,1)
      ENDDO
                
      IF(ANS.EQ.'R'.OR.ANS.EQ.'r')THEN
        IF(iOL.EQ.1)THEN           !Full scale for display
          CALL SetMarker(1,0,1)
        ENDIF    
        IF(iOL.EQ.2)THEN           !Autoscaling
          CALL SetMarker(2,0,1)                                      
        ENDIF
        OLlow =LDX            
        OLhigh=HDX                 
      ENDIF
      IF(ANS.EQ.'C'.OR.ANS.EQ.'c')THEN
        IF(iOL.EQ.1)THEN           !Full scale for display
          CALL SetMarker(0,1,1)
        ENDIF    
        IF(iOL.EQ.2)THEN           !Autoscaling
          CALL SetMarker(0,2,1)                                      
        ENDIF
        OLlow =LDY               
        OLhigh=HDY
      ENDIF

      Idistype=2                   !telling CR that it is an OL spectrum

C Starting the loop of drawing I2 spectra
      IF(m2.LT.m1)THEN     !Assures right order: m1<m2
        mwait=m1
        m1=m2
        m2=mwait 
      ENDIF
      I1=0

C Spectrum loop starts
      DO m=m1,m2
        I1=I1+1

        IF(iOL.EQ.0)THEN
          OLlow  =min(OLlc(I1),ABS(maxdim-11))    !Using old values
          OLhigh =max(OLhc(I1),10)
          OLlocnt=min(0.,OLlo(I1))
          OLhicnt=max(0.000001,OLhi(I1))
        ENDIF
        IF(ANS.EQ.'R'.OR.ANS.EQ.'r')THEN
          DO i=0,maxdim-1              
            Spec(i+1)=rMAT(IDEST,i,m)
          ENDDO
        ENDIF
        IF(ANS.EQ.'C'.OR.ANS.EQ.'c')THEN
          DO i=0,maxdim-1              
            Spec(i+1)=rMAT(IDEST,m,i)
          ENDDO
        ENDIF
        IF(iOL.EQ.2)THEN
          DO i=maxdim,10,-1
            IF(Spec(i).NE.0)GO TO 99
          ENDDO
 99       a=0
          b=i
          b=b*1.01
          CALL GRAX(a,b,dex,nv,1)
          OLlow=a
          OLhigh=dex(nv)*(nv+1)/nv
        ENDIF

        IF(OLlow.lt.0)OLlow=0
        IF(OLhigh.GT.maxdim-1)OLhigh=maxdim-1
        IF(OLlow. GT.OLhigh-1)GO TO 999

        NCC =(OLhigh-OLlow)/NX+1
        NCHS=OLhigh-OLlow+1

        IF(NCC.EQ.1)THEN
          IF(iOL.GE.1)THEN  !Autoscale of z-axis with pixels > channels
            OLlocnt=0
            OLhicnt=0.000001
            ii1=MAX0(OLlow+2,OLlow+0.01*NCHS)
            ii2=MIN0(OLhigh,OLhigh-0.01*NCHS)
            IF(ii2-ii1.lt.10)THEN
              ii1=OLlow+1
              ii2=OLhigh+1
            ENDIF
            DO i=ii1,ii2
              IF(OLhicnt.LT.Spec(i))OLhicnt=Spec(i)
              IF(OLlocnt.GT.Spec(i))OLlocnt=Spec(i)
            ENDDO
c            OLlocnt=0      !By brutal force
          ENDIF
        ELSE
          IF(iOL.GE.1)THEN  !Autoscale of z-axis with pixels < channels
            OLlocnt=0
            OLhicnt=0.000001
            ii1=MAX0(OLlow+2,OLlow+0.01*NCHS)
            ii2=MIN0(OLhigh,OLhigh-0.01*NCHS)
            IF(ii2-ii1.lt.10)THEN
              ii1=OLlow+1
              ii2=OLhigh+1
            ENDIF
            DO ICH=OLlow,OLlow+(NCHS/NCC-1)*NCC,NCC
              Yc=0.
              DO I=1,NCC
                Yc=Yc+Spec(ICH+I-1)
              ENDDO
              Yc=Yc/(FLOAT(NCC))
              IF((OLhicnt.LT.Yc).AND.(ICH.GT.ii1).AND.(ICH.LT.ii2))OLhicnt=Yc
              IF((OLlocnt.GT.Yc).AND.(ICH.GT.ii1).AND.(ICH.LT.ii2))OLlocnt=Yc
            ENDDO
c            OLlocnt=0      !By brutal force
          ENDIF
        ENDIF

        OLhi(I1)=OLhicnt            !Storing for next display
        OLlo(I1)=OLlocnt
        OLlc(I1)=OLlow
        OLhc(I1)=OLhigh

C Finding (x,y) pixel origo for spectrum I1
        LOY=NY*((I1-1)/inx)           !Pixelstart in y-direction
        II1=I1
        DO WHILE (II1.GT.inx)
          II1=II1-inx
        ENDDO
        LOX=NX*(II1-1)                !Pixelstart in x-direction
        CALL LIMG(NX,LOX,NY,LOY)      !Informing screen window

C Making axis
        X0=OLlow
        DX=NCHS
        Y0=OLlocnt
        DY0=OLhicnt-OLlocnt
        DY=1.05*DY0
        CALL SETCOLOR(iblack)
        CALL TRAX(DX,X0,DY,Y0,IYAXIS)

        ICOL = COLORMAP(1)         !blue as default

        CALL SETCOLOR(ICOL)
        X=OLlow
        IF(NCC.EQ.1)THEN
          s=Spec(OLlow+1)
          CALL PSPOT(X,s)
          DO i=OLlow+1,OLhigh+1
             s=Spec(i)
             CALL VECT(X,s)
             X=X+1.0
             CALL VECT(X,s)
          ENDDO 
        ELSE
          Y=0.
          DO i=1,NCC
            Y=Y+Spec(OLlow+i)
          ENDDO
          FNCC=FLOAT(NCC)
          Y=Y/FNCC
          CALL PSPOT(X,Y)
          DO ICH=OLlow,OLlow+(NCHS/NCC-1)*NCC,NCC
            Y=0.
            DO i=1,NCC
              Y=Y+Spec(ICH+i)
            ENDDO
            Y=Y/FNCC
            CALL VECT(X,Y)
            X=X+FNCC
            CALL VECT(X,Y)
          ENDDO
        ENDIF

        CALL SETCOLOR(iblack)
        CALL KTRAS(LOX+NX-10,LOY+NY-10,0)
        WRITE(TEX2,20)m
   20   FORMAT(I4)  
        TEX=TEX1//TEX2
c        IF(itext.EQ.1)CALL PUTG(TEX,7,8,1)
        IF(itext.EQ.1)CALL PUTG(TEX2,7,8,1)
      ENDDO
      CALL DATETIME(HEADING)
      IF(itext.EQ.1)THEN
        CALL MSPOT(NX0-60,LOY+NY+5)
        CALL PUTG(fname(1,IDEST)(1:8),8,1,1)
        CALL MSPOT(NX0-60,LOY+NY-6)
        CALL PUTG(HEADING(1:11),11,1,1)
      ENDIF
      CALL FINIG
      DISP=.TRUE.

C Putting on unit on axis
      CALL INITG(NX,NY)
C Units on x-axis
      CALL MSPOT(NX-8,1)
      CALL PUTG(UNITx,3,8,1)

C Writing information
      IF(ANS.EQ.'R'.OR.ANS.EQ.'r')THEN
        WRITE(6,*)'Use DX and DZ to display new channels and counts'
      ENDIF
      IF(ANS.EQ.'C'.OR.ANS.EQ.'c')THEN
        WRITE(6,*)'Use DY and DZ to display new channels and counts'
      ENDIF
      WRITE(6,*)' '

      RETURN

999   CALL FINIG
      RETURN
      END
