      SUBROUTINE removepeak
C     Removes 2-dim peak from matrices by linear
C     interpolation of the background under the peak

      CHARACTER APP*4,ANS
      INTEGER XDIM,YDIM,RDIM,UPPER(0:2047)
      INTEGER LOW,HIGH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,xcomm*60

      IF(ITYPE.EQ.1)THEN
        WRITE(6,*)'This command works only for 2-dim matrices'
        RETURN 
      ENDIF

      ISP=1
      IF(IDEST.EQ.1)ISP=2


C Defining background to the 2-dim peak
      WRITE(6,*)'Give coordinates for the corners of the rectangel'
      WRITE(6,*)'that defines the background of the 2-dim peak'
      WRITE(6,*)' '
      WRITE(6,*)'    (x1,y2)-----------(x2,y2)'
      WRITE(6,*)'        |      xxx       |'
      WRITE(6,*)'        |   xxxxxxxxxx   |'
      WRITE(6,*)'        |  xxxxxxxxxxxx  |'
      WRITE(6,*)'        |   xxxxxxxxxx   |'
      WRITE(6,*)'        |      xxxx      |'
      WRITE(6,*)'    (x1,y1)-----------(x2,y1)'
      WRITE(6,*)' '

      WRITE(6,123)
 123  FORMAT(/'Give x1:',$)
      CALL READI(5,i1)
      WRITE(6,124)
 124  FORMAT( 'Give x2:',$)
      CALL READI(5,i2)
      WRITE(6,125)
 125  FORMAT( 'Give y1:',$)
      CALL READI(5,j1)
      WRITE(6,126)
 126  FORMAT( 'Give y2:',$)
      CALL READI(5,j2)
      IF(i1.EQ.i2.OR.j1.EQ.j2)Istatus=1
      IF(Istatus.NE.0)RETURN

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
        DO J=0,2047
          rMAT(IDEST,I,J)=0
        ENDDO
      ENDDO

C PUTTING THE MODIFIED DATA INTO DESTINATION MATRIX
C Linear interpolations: z(i,j) =(z(i1,j) + (z(i2,j)-z(i1,j)/(i2-i1))*(i-i1)
C                                +z(i,j1) + (z(i,j2)-z(i,j1)/(j2-j1))*(j1-j1))/2.
      DO j=0,YDIM-1  
        DO i=0,XDIM-1
          rMAT(IDEST,I,J)=rMAT(ISP,I,J)
          IF(j.GT.j1.AND.j.LT.j2.AND.i.GT.i1.AND.i.LT.i2)THEN
            z1=rMAT(ISP,i1,j)+((rMAT(ISP,i2,j)-rMAT(ISP,i1,j))/(i2-i1))*(i-i1)
            z2=rMAT(ISP,i,j1)+((rMAT(ISP,i,j2)-rMAT(ISP,i,j1))/(j2-j1))*(j-j1)
            rMat(IDEST,i,j)=(z1+z2)/2.
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
