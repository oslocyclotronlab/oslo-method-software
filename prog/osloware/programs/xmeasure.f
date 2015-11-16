      PROGRAM XMEASURE
C Read/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      CHARACTER APP*4

      MAXCH=4191
      XDIM=512
      YDIM=512
      Istatus=0
 
      CALL READFILE
      CALL READFILE

      ichi=0
      ipoints=0

      IF(ITYPE.GT.1)THEN
        DO j=0,511 
          DO i=0,2047
            m1=rMAT(1,i,j)
            m2=rMAT(2,i,j)
            IF(m1.NE.0.AND.m2.NE.0)THEN 
              ichi=ichi+(ABS(m1-m2))**2
              ipoints=ipoints+1
            ENDIF
          ENDDO
        ENDDO
      ELSE
        DO i=0,8191
          m1=rSPEC(1,i)
          m2=rSPEC(2,i)            
          IF(m1.NE.0.AND.m2.NE.0)THEN
            ichi=ichi+(ABS(m1-m2))**2
            ipoints=ipoints+1
          ENDIF
        ENDDO
      ENDIF
      
      IF(ipoints.EQ.0)GO TO 99 
      WRITE(6,10)ipoints,ichi,FLOAT(ichi)/FLOAT(ipoints)
  10  FORMAT('Number n of non-zero channels compaired:',I8,/,
     +'(f(i)-g(i))**2=   ',I16,/,'(f(i)-g(i))**2/n= ',E16.3)  

 99   CONTINUE
      END

           
