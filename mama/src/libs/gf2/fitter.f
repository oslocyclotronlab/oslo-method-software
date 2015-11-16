
C=======================================================================

      SUBROUTINE FITTER(MAXITS,IALT_RET)

C        this subroutine is a modified version of 'CURFIT', in Bevington....
C        ....see page 237....

C          designed for use with GF2, Version IV
C          D.C. Radford        March 1984

      INTEGER       MCH(2)
      REAL          PPOS(15)
      COMMON /MKRS/ MCH,PPOS

      REAL          PARS(51), ERRS(51)
      INTEGER       IFIXED(51), NPARS, NFP, NPKS, IRELW, IRELPOS
      COMMON /PARA/ IRELW,PARS,IFIXED,NPARS,NFP,ERRS,NPKS,IRELPOS

      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60


      INTEGER         IWMODE
      CHARACTER*8     NWTSP
      REAL            WTSP(8192)
      COMMON /WTMODE/ IWMODE,NWTSP,WTSP

      REAL           DERIVS(51)
      COMMON /DERIV/ DERIVS

      REAL         BETA(51),DELTA(51),B(51),ERS(51),FIXED(51)
      INTEGER      NEXTP(51)
      LOGICAL      CONV,TEST,RWFIXED,RPFIXED
      REAL*8       ARRAY(51,51),ALPHA(51,51),DDAT,FLAMDA

      CHARACTER*12 WTC(3)
      CHARACTER*26 DATTIM

      COMMON /LUS/ IR,IW,IP,IG

      DATA         WTC /'fit.','data.','sp. '/

      
	  DO i=1,51
		DO j=1,51
			ARRAY(i,j)=0.
			ALPHA(i,j)=0.
		ENDDO
	  ENDDO

      DDAT=0.
	  FLAMDA=0.
      IALT_RET=0
      ILO=MCH(1)+1
      IHI=MCH(2)+1
      NIP=NPARS-NFP
C            NIP=no. of independent (non-fixed) pars
C            NFP=no. of fixed pars
C            NPARS=total no. of pars = 3 * no.of peaks + 6
C            NDF=no. of degrees of freedom....

      IF (IWMODE.GT.0) WTC(3)(5:12) = NWTSP

      DO I=7,NPARS
         FIXED(I)=IFIXED(I)
      ENDDO

C           set up fixed relative widths....

      RWFIXED=.FALSE.
      IF (IRELW.LE.0) THEN
         NIW=0
         DO 20 J=8,NPARS-1,3
            IF (IFIXED(J).EQ.1) MIW=J
C                MIW=highest fitted (non-fixed) width par. no.....
            NIW=NIW+IFIXED(J)
20       CONTINUE
C                NIW=no. of fitted (non-fixed) widths....
         IF (NIW.GT.1) THEN
            DO J=8,MIW,3
               IFIXED(J)=0
            ENDDO
            RWFIXED=.TRUE.
            NIP=NIP-NIW+1
            NIP1=NIP
         ENDIF
      ENDIF

C           set up fixed relative positions....

      RPFIXED=.FALSE.
      IF (IRELPOS.LE.0) THEN
         NIPOS=0
         DO 40 J=7,NPARS-2,3
            IF (IFIXED(J).EQ.1) MIP=J
C                MIP=highest fitted (non-fixed) position par. no.....
            NIPOS=NIPOS+IFIXED(J)
40       CONTINUE
C                NIPOS=no. of fitted (non-fixed) positions....
         IF (NIPOS.GT.1) THEN
            DO J=7,MIP,3
               IFIXED(J)=0
            ENDDO
            RPFIXED=.TRUE.
            NIP=NIP-NIPOS+1
            NIP1=NIP-1
         ENDIF
      ENDIF

      NDF=IHI-ILO+1-NIP
      IF (NDF.LT.1) GO TO 510
      IF (NIP.LT.2) GO TO 530

C          set up array nextp, pointing to free pars....

      K=0
      DO 50 J=1,NPARS
         IF (IFIXED(J).EQ.0) GO TO 50
         K=K+1
         NEXTP(K)=J
50    CONTINUE
      IF (RWFIXED) THEN
         K=K+1
         NEXTP(K)=MIW
      ENDIF
      IF (RPFIXED) THEN
         K=K+1
         NEXTP(K)=MIP
      ENDIF
      IF (K.NE.NIP) GO TO 570

C           initialise for fitting....

      FLAMDA=0.001
      NITS=0
      TEST=.FALSE.
      DERIVS(1)=1.0
      DO 60 I=1,NPARS
         ERRS(I)=0.0
         B(I)=PARS(I)
60    CONTINUE

C        evaluate fit, alpha & beta matrices, & chisq....
70    DO 80 J=1,NIP
         BETA(J)=0.0
         DO 75 K=1,J
            ALPHA(J,K)=0.0
75       CONTINUE
80    CONTINUE

      CHISQ1=0.0
      CALL EVAL(PARS,IFIXED(4),FIT,NPKS,-9)
      DO 180 I=ILO,IHI
         CALL EVAL(PARS,I,FIT,NPKS,1)
         DIFF=rSPEC(IDEST,I-1)-FIT
         
C          weight with fit/data/weight sp. for iwmode=-1/0/1....
         IF (IWMODE.LT.0) THEN
            DAT=FIT
         ELSEIF (IWMODE.EQ.0) THEN
            DAT=rSPEC(IDEST,I-1)
         ELSE
            DAT=WTSP(I)
         ENDIF
         IF (DAT.LT.1.0) DAT=1.0
         DDAT=DBLE(DAT)
         CHISQ1=CHISQ1+DIFF*DIFF/DAT
         IF (RWFIXED) THEN
            DO 140 K=8,MIW-3,3
               DERIVS(MIW)=DERIVS(MIW)+FIXED(K)*DERIVS(K)
140         CONTINUE
         ENDIF
         IF (RPFIXED) THEN
            DO 150 K=7,MIP-3,3
               DERIVS(MIP)=DERIVS(MIP)+FIXED(K)*DERIVS(K)
150         CONTINUE
         ENDIF
         DO 170 L=1,NIP
            J=NEXTP(L)
            BETA(L)=BETA(L)+DIFF*DERIVS(J)/DAT
            DO 160 M=1,L
               ALPHA(L,M) = ALPHA(L,M)
     +            + DBLE(DERIVS(J))*DBLE(DERIVS(NEXTP(M)))/DDAT
160         CONTINUE
170      CONTINUE
180   CONTINUE
      CHISQ1=CHISQ1/FLOAT(NDF)
     
C        invert modified curvature matrix to find new parameters....

190   ARRAY(1,1)=1.0D0+FLAMDA
      DO 210 J=2,NIP
         DO 200 K=1,J-1
            IF (ALPHA(J,J)*ALPHA(K,K).EQ.0.0D0) GO TO 590
            ARRAY(J,K)=ALPHA(J,K)/DSQRT(ALPHA(J,J)*ALPHA(K,K))
            ARRAY(K,J)=ARRAY(J,K)
200      CONTINUE
         ARRAY(J,J)=1.0D0+FLAMDA
210   CONTINUE

      CALL MATINV(ARRAY,NIP,51)
      IF (TEST) GO TO 350
      DO 240 J=1,NIP
         IF (ALPHA(J,J)*ALPHA(J,J).EQ.0.0D0) GO TO 590
         DELTA(J)=0.0
         DO 230 K=1,NIP
            DELTA(J) = DELTA(J)+ BETA(K)*ARRAY(J,K)/DSQRT(ALPHA(J,J)*ALPHA(K,K))
230      CONTINUE
240   CONTINUE

C              calculate new par. values....

      DO 260 L=1,NIP
         J=NEXTP(L)
         B(J)=PARS(J)+DELTA(L)
260   CONTINUE

      IF (RWFIXED) THEN
         DO 270 J=8,MIW-3,3
            B(J)=PARS(J)+FIXED(J)*DELTA(NIP1)
270      CONTINUE
      ENDIF
      IF (RPFIXED) THEN
         DO 290 J=7,MIP-3,3
            B(J)=PARS(J)+FIXED(J)*DELTA(NIP)
290      CONTINUE
      ENDIF

C         if chisq increased, increase flamda and try again....

      CHISQ=0.0
      CALL EVAL(B,IFIXED(4),FIT,NPKS,-9)
      DO 340 I=ILO,IHI
         CALL EVAL(B,I,FIT,NPKS,0)
         DIFF=rSPEC(IDEST,I-1)-FIT
C          weight with fit/data/weight sp. for iwmode=-1/0/1....
         IF (IWMODE.LT.0) THEN
            DAT=FIT
         ELSEIF (IWMODE.EQ.0) THEN
            DAT=rSPEC(IDEST,I-1)
         ELSE
            DAT=WTSP(I)
         ENDIF
         IF (DAT.LT.1.0) DAT=1.0
         CHISQ=CHISQ+DIFF*DIFF/DAT
340   CONTINUE
      CHISQ=CHISQ/FLOAT(NDF)
      IF (CHISQ.GT.CHISQ1 .AND. FLAMDA.LT.2.0) THEN
         FLAMDA=FLAMDA*10.0
         GO TO 190
      ENDIF

C        evaluate parameters and errors....
C        test for convergence....

350   CONV=.TRUE.
      DO 360 J=1,NIP
         IF (ARRAY(J,J).LT.0.D0) ARRAY(J,J)=0.D0
         ERS(J)=DSQRT(ARRAY(J,J)/ALPHA(J,J))*SQRT(1.0+FLAMDA)
         IF (ABS(DELTA(J)).GE.(ERS(J)/100.)) CONV=.FALSE.
360   CONTINUE

      IF (.NOT.TEST) THEN
         DO J=1,NPARS
            PARS(J)=B(J)
         ENDDO
         FLAMDA=FLAMDA/10.0
         NITS=NITS+1
         IF ((.NOT.CONV) .AND. NITS.LT.MAXITS) GO TO 70

C             re-do matrix inversion with FLAMDA=0
C                  to calculate errors....

         FLAMDA=0.
         TEST=.TRUE.
         GO TO 190
      ENDIF

C          list data and exit....

      DO L=1,NIP
         ERRS(NEXTP(L))=ERS(L)
      ENDDO
      IF (RWFIXED) THEN
         DO 400 J=8,MIW,3
            IFIXED(J)=FIXED(J)
            ERRS(J)=FIXED(J)*ERS(NIP1)
400      CONTINUE
      ENDIF
      IF (RPFIXED) THEN
         DO 410 J=7,MIP,3
            IFIXED(J)=FIXED(J)
            ERRS(J)=FIXED(J)*ERS(NIP)
410      CONTINUE
      ENDIF

      WRITE(6,*)' '
      CALL DATETIME(DATTIM)
      WRITE(IW,420,ERR=430)fname(2,IDEST),DATTIM,MCH,NPKS,NIP,NDF,WTC(IWMODE+2)
420   FORMAT('File ',A,5X,A/
     +'Fitted chs',I5,' to',I5,I8,' Peaks'/I4,
     +' indept. pars',I7,' degrees of freedom weighted with ',3A4)


430   IF (RPFIXED.AND.RWFIXED) THEN
         WRITE(IW,*) 'Relative peak positions and widths fixed.'
      ELSEIF (RPFIXED) THEN
         WRITE(IW,*) 'Relative peak positions fixed.'
      ELSEIF (RWFIXED) THEN
         WRITE(IW,*) 'Relative widths fixed.'
      ENDIF

      IF (CONV) THEN
         WRITE(IW,440,ERR=450)NITS,CHISQ
440      FORMAT(1X,I3,' iterations,  Chisq/d.o.f.= ',F6.3)
450      RETURN
      ENDIF

      WRITE(IW,500,ERR=505)NITS,CHISQ
500   FORMAT(' Failed to converge after',I3,' iterations,  Chisq/',
     +'d.o.f.= ',F6.3/'    Warning - do not believe quoted errors.')
505   IALT_RET=2
      RETURN 

C       error messages....

510   WRITE(IW,*) 'No d.o.f.'
      GO TO 550
530   WRITE(IW,*) 'Too many fixed pars.'
550   IF (RWFIXED.OR.RPFIXED) THEN
         DO 560 I=7,NPARS
            IFIXED(I)=FIXED(I)
560      CONTINUE
      ENDIF
      IALT_RET=1
      RETURN 
570   WRITE(IW,*) 'NIP.NE.sum(IFIXED)'
      GO TO 550
590   WRITE(IW,*) 'Cannot - diag. element eq. to zero.'
      GO TO 550
      END
