      SUBROUTINE SetMarker(ix,iy,iz)
C Routine to set display markers:
C        ix=-1    Take markers from spec.,  test that x-markers are valid
C        ix=0     USE LDX (or HDX) OR new LDX given by the DX-command
C        ix=1     set full scale for x-marker according to dimension
C        ix=2     autoscale according to counts in matrix or spectrum
C        iy,iz    as above as for x-axis. iz active only for matrices

C     ldx,hdx=    markers on x-axis for 2-dim plot
C     ldy,hdy=    markers on y-axis for 2-dim plot
C     ldz,hdz=    markers on z-axis for 2-dim plot
C     loch,hich=  markers on x-axis for 1-dim spectrum
C     locnt,hicnt=markers on y-axis for 1-dim spectrum

      LOGICAL DISP
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LOCH,HICH
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      REAL LDZ,HDZ,LOCNT,HICNT
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      INTEGER XDIM,YDIM
      CHARACTER APP*4
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      REAL dex(14)
      COMMON/mem1/iLDX(2),iHDX(2),iLDY(2),iHDY(2),iLDZ(2),iHDZ(2)
      COMMON/mem2/iLOCH(2),iHICH(2),iLOCNT(2),iHICNT(2)
      REAL iLDZ,iHDZ,iLOCNT,iHICNT

      IF(IDEST.LT.1.OR.IDEST.GT.2)THEN
        WRITE(6,*)'Warning, IDEST has wrong value= ',IDEST,', Reset to 1' 
        IDEST=1
        RETURN
      ENDIF
      IF(ITYPE.LT.1.OR.ITYPE.GT.3)THEN
        WRITE(6,*)'Warning, ITYPE has wrong value= ',ITYPE,', Reset to 3' 
        ITYPE=3
        RETURN
      ENDIF

      IF(ITYPE.GT.1)THEN          !As default is taken the last value
        IF(ix.EQ.-1)LDX =iLDX(IDEST)
        IF(iy.EQ.-1)LDY =iLDY(IDEST)
        IF(iz.EQ.-1)LDZ =iLDZ(IDEST)
        IF(ix.EQ.-1)HDX =iHDX(IDEST)
        IF(iy.EQ.-1)HDY =iHDY(IDEST)
        IF(iz.EQ.-1)HDZ =iHDZ(IDEST)
      ELSE
        IF(ix.EQ.-1)LOCH =iLOCH(IDEST)
        IF(ix.EQ.-1)HICH =iHICH(IDEST)
        IF(iy.EQ.-1)LOCNT=iLOCNT(IDEST)
        IF(iy.EQ.-1)HICNT=iHICNT(IDEST)
      ENDIF

      IF(ITYPE.GT.1)THEN          !matrix
        IF(XDIM.GT.4096)XDIM=4096
        IF(YDIM.GT. 2048)YDIM=2048
        IF(ix.EQ.1)THEN           !Reset full scale
          LDX=0
          HDX=XDIM-1
        ENDIF
        IF(iy.EQ.1)THEN
          LDY=0
          HDY=YDIM-1
        ENDIF
        IF(iz.EQ.1)THEN
          LDZ=0.1
          HDZ=10000
        ENDIF
        
        IF(ix.EQ.2)THEN             !Autoscale
          DO i=XDIM-1,10,-2         !Finding LDX,HDX
            iT=(i/1000)*1000
            IF(iT.EQ.i)THEN
              write(6,FMT='(A1,$)')'.'
              call flush(6)
            ENDIF
            DO j=0,YDIM-1,1
              IF(rMAT(IDEST,i,j).NE.0)GO TO 1
            ENDDO
          ENDDO
 1        a=0
          b=i
          b=b*1.01
          CALL GRAX(a,b,dex,nv,1)
          LDX=a
          HDX=dex(nv)*(nv+1)/nv
          IF(HDX.GT.XDIM-1)HDX=XDIM-1
        ENDIF
        IF(iy.EQ.2)THEN
          DO j=YDIM-1,10,-1         !Finding LDY,HDY
            JT=(j/100)*100
            IF(JT.EQ.J)THEN
              write(6,FMT='(A1,$)')'.'
              call flush(6)
            ENDIF
            DO i=0,XDIM-1,2
              IF(rMAT(IDEST,i,j).NE.0)GO TO 2
            ENDDO
          ENDDO
 2        a=0
          b=j
          b=j*1.01
          CALL GRAX(a,b,dex,nv,1)
          LDY=a
          HDY=dex(nv)*(nv+1)/nv
          IF(HDY.GT.YDIM-1)HDY=YDIM-1
        ENDIF
        IF(iz.EQ.2)THEN
          LDZ=4.                   !Finding LDZ,HDZ
          HDZ=0.00001
          do j=LDY,HDY,1           !Testing each y-channel
          jt=(j/100)*100
            IF(jt.EQ.j+1)THEN
              write(6,FMT='(A1,$)')'.'
              call flush(6)
            ENDIF
            do i=LDX,HDX,2         !Testing only every second x-channel
              rr=ABS(rMAT(IDEST,i,j))
              IF(rr.GT.HDZ)HDZ=rr*1.1 !Use 1.1 to get color
              IF(rr.LT.LDZ.AND.rr.GT.0)THEN
                LDZ=ABS(rr)
              ENDIF
            enddo
          enddo
          LDZ=AMIN1(0.1,(HDZ)/1000.)
c          IF(HDZ.GT.50 )LDZ=2
c          IF(HDZ.GT.500)LDZ=4
        WRITE(6,*)' '
        ENDIF

      ELSE                        !singles
        DO i=0,8191
          IF(ABS(rSPEC(IDEST,i)).GT. 1.0E+36)rSPEC(IDEST,i)=0.
          IF(ABS(rSPEC(IDEST,i)).LT. 1.0E-36)rSPEC(IDEST,i)=0.
        ENDDO
        IF(MAXCH.GT.8191)MAXCH=8191
        IF(ix.EQ.1)THEN           !Reset full scale
          LOCH=0
          HICH=MAXCH
        ENDIF 
        ia=LOCH
        ib=HICH                  
        IF(ix.EQ.2)THEN           !Autoscale
          DO i=8191,10,-1         !Finding LOCH,HICH
            IF(rSPEC(IDEST,i).NE.0)GO TO 3
          ENDDO
 3        a=0
          b=i
          b=b*1.01
          IF(b.GT.MAXCH)b=MAXCH
          ia=a
          ib=b
          CALL GRAX(a,b,dex,nv,1)
          LOCH=a
          HICH=dex(nv)*(nv+1)/nv
          IF(HICH.LE.   10)HICH=10
          IF(HICH.GT.MAXCH)HICH=MAXCH
        ENDIF

        IF(iy.EQ.1.OR.iy.EQ.2)THEN
          LOCNT=0                 !Finding LOCNT,HICNT
          HICNT=0.00001
          CALL INITG(NX,NY)
          NCC =(HICH-LOCH)/NX+1
          NCHS=HICH-LOCH+1
          ii1=MAX0(ia+2,INT(ia+0.01*NCHS))
          ii2=MIN0(ib,  INT(ib-0.01*NCHS))
          IF(ii2-ii1.LT.10)THEN
            ii1=LOCH+1
            ii2=HICH
          ENDIF
          IF(NCC.EQ.1)THEN                         !pixels > channels 
            DO I=ii1,ii2
              IF(HICNT.LT.rSPEC(IDEST,I))HICNT=rSPEC(IDEST,I)
              IF(LOCNT.GT.rSPEC(IDEST,I))LOCNT=rSPEC(IDEST,I)
            ENDDO
          ELSE                                     !pixels < channels
            DO ICH=LOCH,LOCH+(NCHS/NCC-1)*NCC,NCC
              Yc=0.
              DO I=1,NCC
                Yc=Yc+rSPEC(IDEST,ICH+I-1)
              ENDDO
              Yc=Yc/(FLOAT(NCC))
              IF((HICNT.LT.Yc).AND.(ICH.GT.ii1).AND.(ICH.LT.ii2))HICNT=Yc
              IF((LOCNT.GT.Yc).AND.(ICH.GT.ii1).AND.(ICH.LT.ii2))LOCNT=Yc
            ENDDO
          ENDIF
c          LOCNT=0                 !Brutal force
        ENDIF
      ENDIF


C Last test that everything is OK (option ix,iy,iz=0)
      IF(ITYPE.GT.1)THEN
        LDX  =max0(0,LDX)
        HDX  =min0(4095,HDX)
        IF(LDX.GE.HDX)THEN
          WRITE(6,*)'Warning, x-display markers reset'
          LDX=0
          HDX=4095
        ENDIF
        LDY  =max0(0,LDY)
        HDY  =min0(2047,HDY)
        IF(LDY.GE.HDY)THEN
          WRITE(6,*)'Warning, y-display markers reset'
          LDY=0
          HDY=2047
        ENDIF
        IF(LDZ.EQ.0)LDZ=0.1
        LDZ  =max(0.0000000000001,LDZ)
        HDZ  =max(0.000000000001,HDZ)
        IF(LDZ.GE.HDZ)THEN
          WRITE(6,*)'Warning, z-display limits reset'
          LDZ=0.1
          HDZ=10000
        ENDIF
      ELSE
        LOCH =max0(0,   LOCH)
        HICH =min0(8191,HICH)
        IF(LOCH.GE.HICH)THEN
          WRITE(6,*)'Warning, display markers reset'
          LOCH=0
          HICH=8191
        ENDIF
      ENDIF

      IF(ITYPE.GT.1)THEN          !Remember last displaymarkers
        iLDX(IDEST)  =LDX
        iLDY(IDEST)  =LDY
        iLDZ(IDEST)  =LDZ
        iHDX(IDEST)  =HDX
        iHDY(IDEST)  =HDY
        iHDZ(IDEST)  =HDZ
      ELSE
        iLOCH(IDEST) =LOCH
        iHICH(IDEST) =HICH
        iLOCNT(IDEST)=LOCNT
        iHICNT(IDEST)=HICNT
      ENDIF

c        write(6,*)'IYAXIS',IYAXIS
c        write(6,*)'IDEST,ITYPE,XDIM,YDIM,MAXCH',IDEST,ITYPE,XDIM,YDIM,MAXCH
c        write(6,*)'LDX,HDX,LDY,HDY,LDZ,HDZ',LDX,HDX,LDY,HDY,LDZ,HDZ
c        write(6,*)'LOCH,HICH,LOCNT,HICNT',LOCH,HICH,LOCNT,HICNT

      RETURN
      END
