
      SUBROUTINE Uncorrelation
      INTEGER XDIM,YDIM,RDIM,Sij,Si,Sj
      CHARACTER APP*4, ans*1
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/RESPONE/R(0:2047,0:2047),RDIM,A0,A1,FWHM

      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,xcomm*60

      ans='y'
      IDUM=1
      IF(IDEST.EQ.1)IDUM=2
      WRITE(6,1)IDUM
   1  FORMAT(/'Destination matrix <',I1,'>:',$)
      CALL READI(5,IDUM)
      ISP=1
      IF(IDUM.EQ.1)ISP=2 
      WRITE(6,2)ISP
   2  FORMAT( 'Source matrix      <',I1,'>:',$)
      CALL READI(5,ISP)
      IF(ISP.EQ.IDUM)THEN
        WRITE(6,*)'Destination must be another spectrum'
        Istatus=1
        RETURN
      ENDIF
       
      IF(Istatus.NE.0)RETURN
      IDEST=IDUM
      ITYPE=3
      XDIM=Idim(1,ISP,1)
      YDIM=Idim(1,ISP,2)
      cal(1,IDEST,1,1)=cal(1,ISP,1,1)
      cal(1,IDEST,1,2)=cal(1,ISP,1,2)
      cal(1,IDEST,1,3)=cal(1,ISP,1,3)
      cal(1,IDEST,2,1)=cal(1,ISP,2,1)
      cal(1,IDEST,2,2)=cal(1,ISP,2,2)
      cal(1,IDEST,2,3)=cal(1,ISP,2,3)
      RDIM=MAX0(XDIM,YDIM)    
      IF(RDIM.LT.0.OR.RDIM.GT.2047)RDIM=2048

C The subtraction of uncorrelated background starts
      Sij=0
      DO j=0,RDIM-1
        DO i=0,RDIM-1
          R(i,j)=rMAT(ISP,i,j)
          Sij=Sij+R(i,j)
        ENDDO
      ENDDO 
      WRITE(6,3)Sij
   3  FORMAT( 'Counts in matrix before correction   ',I12)
      IF(Sij.LE.0)GO TO 999

  99  n=0   
      DO j=0,RDIM-1
        jt=((j)/30)*30
        IF(jt.EQ.j)THEN
          write(6,FMT='(A1,$)')'.'
          call flush(6)
        ENDIF

        DO i=0,RDIM-1
          Sj=0
          DO ii=0,RDIM-1
            Sj=Sj+R(ii,j)
          ENDDO 
          Si=0
          DO jj=0,RDIM-1
            Si=Si+R(i,jj)
          ENDDO
          IF(Sij.EQ.0)GO TO 999
          Bij=(Si*Sj)/Sij
          rMAT(IDEST,I,J)=rMAT(ISP,I,J)-0.9*Bij
          R(i,j)=rMAT(IDEST,i,j)
          n=n+R(i,j)
        ENDDO
      ENDDO
      Sij=n
      
      WRITE(6,*)' '
      WRITE(6,4)Sij
   4  FORMAT( 'Counts in backgroundcorrected matrix ',I12)

      WRITE(6,5)ans
   5  FORMAT( 'Do you want a new iteration (y/n) <',A1,'>:',$)
      CALL READA1(5,ans)

      IF(Istatus.NE.0)RETURN

      IF(ans.EQ.'y'.OR.ans.EQ.'Y')GO TO 99

      XDIM=RDIM
      YDIM=RDIM

C Updating comment in the heading of spectrum file
      xcomm(1:4)='UN:'
      CALL AddComment(xcomm,3)
      CALL SetMarker(1,1,1)

999   RDIM=0              !Meaning no respons function present

      RETURN
      END
                       

