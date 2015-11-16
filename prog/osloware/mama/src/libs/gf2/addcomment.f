      SUBROUTINE AddComment(xcomm,iextra)
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,xcomm*60
C Routine to add new comments (xcomm) to the comments (comm) that already exist
C The iextra value is an upper estimate of the number of characters in xcomm 

      I1=2                   !singels
      IF(ITYPE.GT.1)I1=1     !matrix
c      write(6,*)'xcomm,iextra foer :',xcomm//'*',iextra

C Removing blanks in the comment to be added (xcomm)
      ix=iextra       
      DO i=1,iextra
C Finding number of blanks
        nblank=0
        DO k=i,ix
          IF(xcomm(k:k).NE.' '.OR.xcomm(k:k).NE.'')GO TO 1
          nblank=nblank+1
        ENDDO
1       CONTINUE
        IF(nblank.GT.0)THEN
          DO j=i+nblank,ix
            xcomm(j-nblank:j-nblank)=xcomm(j:j)
          ENDDO
          ix=ix-nblank
        ENDIF
      ENDDO
      

C Finding size of present comment array. Looking from back
      ifill=0
      DO i=60,1,-1
        IF(comm(I1,IDEST)(i:i).NE.' '.OR.comm(I1,IDEST)(i:i).NE.'')GO TO 2
      ENDDO
      GO TO 3
   2  ifill=i
       
   3  ishift=(ifill+ix)-60
      IF(ishift.LT.0)ishift=0
      IF(ishift.GT.0)THEN
        DO i=1+ishift,60
          comm(I1,IDEST)(i-ishift:i-ishift)=comm(I1,IDEST)(i:i)
        ENDDO
      ENDIF      
      DO i=1+ifill-ishift,60
        comm(I1,IDEST)(i:i)=''
      ENDDO

C Checking if we have to add (will not repeat last command if equal)
      mm=ifill-ishift
      IF(mm-ix+1.GE.1)THEN
        IF(comm(I1,IDEST)(mm-ix+1:mm).EQ.xcomm(m+1:m+ix))GO TO 99  !not repeating
      ENDIF
      IF(mm+1.LT.1.OR.mm+ix.GT.60)GO TO 99
      DO i=1,ix
        comm(I1,IDEST)(mm+i:mm+i)=xcomm(i:i)
      ENDDO

c      write(6,*)'xcomm,iextra etter:',xcomm//'*',ix
c      write(6,*)'comment ble etter :',comm(I1,IDEST)//'*'

 99   xcomm(1:60)=''
      RETURN
      END

