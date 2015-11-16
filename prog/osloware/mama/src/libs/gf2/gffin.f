
C=======================================================================

      SUBROUTINE GFFIN(MODE)

      REAL          PARS(51), ERRS(51)
      INTEGER       IFIXED(51), NPARS, NFP, NPKS, IRELW, IRELPOS
      COMMON /PARA/ IRELW,PARS,IFIXED,NPARS,NFP,ERRS,NPKS,IRELPOS

      REAL          AREAS(15),DAREAS(15),CENTS(15)
      COMMON /AREA/ AREAS,DAREAS,CENTS
      LOGICAL DISP
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LOCH,HICH
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      REAL LDZ,HDZ,LOCNT,HICNT
C       calc. areas, centroids and errors....

      R=PARS(4)/50.0
      R1=1.0-0.5*R
      BET=PARS(5)
      DO 320 I=1,NPKS
         IC=3*I+4
         Y=PARS(IC+1)/(BET*3.33021838)
         D=EXP(-Y*Y)/ERFC(Y)
         A=R*BET*D+1.06446705*PARS(IC+1)*R1
         AREAS(I)=A*PARS(IC+2)
         EH=A*ERRS(IC+2)
         ER=(2.0*BET*D-1.06446705*PARS(IC+1))*ERRS(4)/100.0
         EB=R*D*(1.0+2.0*Y*Y-1.12837917*D*Y)*ERRS(5)
         EW=(R1*1.06446705+0.600561216*R*D*(D/1.77245385-Y))*ERRS(IC+1)
         DAREAS(I)=SQRT(EH*EH+PARS(IC+2)*PARS(IC+2)*(ER*ER+EB*EB+EW*EW))
         CENTS(I)=PARS(IC)-R*BET*D*BET/A
320   CONTINUE
      IF (DISP) THEN
         IF (MODE.GT.0) CALL ASKYN(18HDisplay fit? (Y/N),18,&780)
C           display fit....
         CALL DSPFIT
      ENDIF

C     CALL ASKYN(22HType parameters? (Y/N),22,&800)
780   CALL TYPEIT(2)
      CALL TYPEIT(3)
c800   ans='n'
c      WRITE(6,401)ans
c401   FORMAT('Print parameters (y/n) <',A1,'>:',$)
c      CALL READA1(5,ans)
c      IF(ans.EQ.'n'.OR.ans.EQ.'N')CALL TYPEIT(3)

      RETURN
      END
