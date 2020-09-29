      SUBROUTINE removepeak
C     Removes 2-dim peak from matrices by linear
C     interpolation of the background under the peak.
C
C     First the shape of the gamma-peak is determined from projection
C     down to x-axis. Secondly, the shape spectrum is normalized to area = 1.
C     Then a linear background between x1 - x2 at y is determined and
C     the area (number of counts) above this line. Finally, we take the total
C     spectrum and subtract the shape spectrum having the correct area.
C     This seems to work nicely, and we obtain reasonable scattering
C     of the intensities in the corrected region of interest.
C     To be sure that the contamination peak structure (shape spectrum) is
C     correctly determined, the user may choose a part of the region
C     on the y-axis where one believes there is no underlying peak from the
C     "good and wanted" gammas.

      CHARACTER APP*4,ANS
      INTEGER XDIM,YDIM,RDIM
      INTEGER LOW,HIGH, i,j
      REAL Shape(0:4095)
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      COMMON/rd/i1,i2,j1,j2,l1,l2

      CHARACTER fname*8,comm*60,xcomm*60

      IF(ITYPE.EQ.1)THEN
        WRITE(6,*)'This command works only for 2-dim matrices'
        RETURN 
      ENDIF

      ISP=1
      IF(IDEST.EQ.1)ISP=2


C Defining background for the 2-dim peak
      WRITE(6,*)'Give coordinates for the corners of the rectangel'
      WRITE(6,*)'that defines the background of the 2-dim peak'
      WRITE(6,*)' '
      WRITE(6,*)'         --------------(x2,y2)'
      WRITE(6,*)'        |      xxx       |'
      WRITE(6,*)'        |      xxx       |'
      WRITE(6,*)'        |   xxxxxxxxxx   |'
      WRITE(6,*)'        |  xxxxxxxxxxxx  |'
      WRITE(6,*)'        |  xxxxxxxxxxxx  |'
      WRITE(6,*)'        |   xxxxxxxxxx   |'
      WRITE(6,*)'        |      xxxx      |'
      WRITE(6,*)'    (x1,y1)-------------- '
      WRITE(6,*)' '


      WRITE(6,123)i1
 123  FORMAT(/'Give x1 <',I4,'>:',$)
      CALL READI(5,i1)
      WRITE(6,124)j1
 124  FORMAT( 'Give y1 <',I4,'>:',$)
      CALL READI(5,j1)
      WRITE(6,125)i2
 125  FORMAT( 'Give x2 <',I4,'>:',$)
      CALL READI(5,i2)
      WRITE(6,126)j2
 126  FORMAT( 'Give y2 <',I4,'>:',$)
      CALL READI(5,j2)
      IF(i1.EQ.i2.OR.j1.EQ.j2)Istatus=1
      IF(Istatus.NE.0)RETURN

      WRITE(6,*)'We need a region on y-axis where the shape of the gamma-peak can be determined'
      WRITE(6,*)'Give lower (L1) and higher (L2) channel on y-axis'
      WRITE(6,*)' '
      WRITE(6,*)'         --------------(x2,y2)'
      WRITE(6,*)'        |      xxx       |'
      WRITE(6,*)'        |      xxx       |'
      WRITE(6,*)'    L2  |   xxxxxxxxxx   |'
      WRITE(6,*)'        |  xxxxxxxxxxxx  |'
      WRITE(6,*)'        |  xxxxxxxxxxxx  |'
      WRITE(6,*)'    L1  |   xxxxxxxxxx   |'
      WRITE(6,*)'        |      xxxx      |'
      WRITE(6,*)'    (x1,y1)-------------- '
      WRITE(6,*)' '

C Make suggestions for L1 and L2 if they have bizarre values
      IF(l1.LT.j1.OR.l1.GT.j2.OR.l2.LT.j1.OR.l2.GT.j2)THEN
         l1 = j1 + (j2-j1)/3
         l2 = j2 - (j2-j1)/3
         IF(l1.GT.l2)THEN
            l1 = j1
            l2 = j2
         ENDIF
      ENDIF
      WRITE(6,127)l1
 127  FORMAT(/'Give L1 <',I4,'>:',$)
      CALL READI(5,l1)
      WRITE(6,128)l2
 128  FORMAT( 'Give L2 <',I4,'>:',$)
      CALL READI(5,l2)

      IF(L1.GT.L2.OR.L1.LT.j1.OR.L2.GT.j2)THEN
      WRITE(6,*)'Sorry, wrong channels. You need y1 <= L1 <= L2 <= y2'
      RETURN
      ENDIF

C MAKE SURE ORDERING IS OK
      IF(i1.GT.i2)THEN
        Itemp=i1
        i1=i2
        i2=Itemp
      ENDIF
      IF(j1.GT.j2)THEN
        Itemp=j1
        j1=j2
        j2=Itemp
      ENDIF

C New IDEST
      iTemp=ISP
      ISP=IDEST
      IDEST=iTemp

C Zeroing destination spectrum
      DO I=0,4095
        Shape(i)=0.
        DO J=0,2047
          rMAT(IDEST,I,J)=0
        ENDDO
      ENDDO
      
C Getting the general shape of the gamma-peak structure (does not have to be Gaussian)
      DO j=0,YDIM-1
        DO i=0,XDIM-1
          IF(j.GE.l1.AND.j.LE.l2.AND.i.GT.i1.AND.i.LT.i2)THEN
            ztot  =rMAT(ISP,i,j)
            zback =rMAT(ISP,i1,j)+((rMAT(ISP,i2,j)-rMAT(ISP,i1,j))/(i2-i1))*(i-i1)
            znetto=ztot-zback
            Shape(i)=Shape(i)+znetto
          ENDIF
        ENDDO
      ENDDO

C Normalizing average Shape(i) to unity (=1.0)
      sum = 0.
      DO i=0,XDIM-1
        IF(i.GT.i1.AND.i.LT.i2)sum = sum + Shape(i)
c        WRITE(6,*)i,Shape(i)
      ENDDO
      sum = ABS(sum)
      IF(sum.NE.0.)THEN
        DO i=0,XDIM-1
          Shape(i) = Shape(i)/sum
        ENDDO
      ELSE
        Write(6,*)'Sorry, no netto counts in region of interest'
        RETURN
      ENDIF

C PUTTING THE MODIFIED DATA INTO DESTINATION MATRIX
C Linear interpolations: z(i,j) =(z(i1,j) + z(i2,j)-z(i1,j)/(i2-i1))*(i-i1)
      DO j=0,YDIM-1
        sum = 0.
        DO i=0,XDIM-1
          IF(j.GT.j1.AND.j.LT.j2.AND.i.GT.i1.AND.i.LT.i2)THEN
            ztot  =rMAT(ISP,i,j)
            z1    =(rMAT(ISP,i1,j-1) + rMAT(ISP,i1,j) + rMAT(ISP,i1,j+1))/3.0
            z2    =(rMAT(ISP,i2,j-1) + rMAT(ISP,i2,j) + rMAT(ISP,i2,j+1))/3.0
            zback =z1 + ((z2-z1)/(i2-i1))*(i-i1)
            znetto=ztot-zback
            sum = sum + znetto
          ENDIF
        ENDDO
        DO i=0,XDIM-1
          rMAT(IDEST,I,J)=rMAT(ISP,I,J)
          IF(j.GT.j1.AND.j.LT.j2.AND.i.GT.i1.AND.i.LT.i2)THEN
            IF(sum.GT.0.)rMat(IDEST,i,j)= rMAT(ISP,i,j) - Shape(i)*(sum/1.0)
          ENDIF
        ENDDO
      ENDDO

      CALL CLEANUP
      cal(1,IDEST,1,1)=cal(1,ISP,1,1)
      cal(1,IDEST,1,2)=cal(1,ISP,1,2)
      cal(1,IDEST,1,3)=cal(1,ISP,1,3)
      cal(1,IDEST,2,1)=cal(1,ISP,2,1)
      cal(1,IDEST,2,2)=cal(1,ISP,2,2)
      cal(1,IDEST,2,3)=cal(1,ISP,2,3)

C Updating comment in the heading of spectrum file
      xcomm(1:3)='RD:'
      fname(1,IDEST)(1:8)='RD'//fname(1,ISP)(1:6)
      comm(1,IDEST)=comm(1,ISP)
      CALL AddComment(xcomm,3)

      END
