      REAL FUNCTION GETMKRCHNL(MN)
C     note: a function not a SUBROUTINE ....
C     above line is a flag for searches....

C        gets a channel that is within the acceptable fitting range....
C        INPUT: MN the Marker Number.
C        OUTPUT: GETMKRCHNL the channel number.
C        CALLed by CHNGMARK

C        local variables....
C                    MN is the Marker Number....
      INTEGER        MN
      CHARACTER*40   ANS

C        common blocks....

      INTEGER       MCH(2)
      REAL          PPOS(15)
      COMMON /MKRS/ MCH,PPOS

      REAL          PARS(51), ERRS(51)
      INTEGER       IFIXED(51), NPARS, NFP, NPKS, IRELW, IRELPOS
      COMMON /PARA/ IRELW,PARS,IFIXED,NPARS,NFP,ERRS,NPKS,IRELPOS
      LOGICAL DISP
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LOCH,HICH
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      REAL LDZ,HDZ,LOCNT,HICNT
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
 
      COMMON /LUS/ IR,IW,IP,IG

      itry=0
10    IF (DISP .AND. HDX.GE.MCH(2) .AND. LDX.LE.MCH(1)) THEN
         CALL RETIC(X,Y,ANS)
         IF (ANS(1:1).NE.'T' .AND. ANS(1:1).NE.'t') GO TO 30
      ENDIF
20    CALL ASK(33HNew position=?(rtn for old value),33,ANS,JK)
      IF (JK.EQ.0) THEN
         IF (MN.GT.2) THEN
            GETMKRCHNL = PPOS(MN-2)
         ELSE
            GETMKRCHNL = MCH(MN)
         ENDIF
         RETURN
      ENDIF
      CALL FFIN(ANS,JK,X,RJ1,RJ2,*20)
      X = X + 0.5
      IF (X.LT.0.5 .OR. IFIX(X).GT.MAXCH) THEN
         WRITE (IW,*) ' Marker ch. outside spectrum - try again'
                  itry=itry+1
                  IF(itry.GE.3)THEN
                    WRITE(6,*)'Sorry, not your day today'
                    RETURN
                  ENDIF

         GO TO 10
      ENDIF
30    IF (MN.GT.2) THEN
         IF (MCH(1).GT.IFIX(X) .OR. MCH(2).LT.IFIX(X)) THEN
            WRITE (IW,*) ' Peaks must be within limits - try again'
                  itry=itry+1
                  IF(itry.GE.3)THEN
                    WRITE(6,*)'Sorry, not your day today'
                    RETURN
                  ENDIF

            GO TO 10
         ENDIF
         GETMKRCHNL = X - 0.5
      ELSE
C        boundary markers....
         DO 40 I = 1, NPKS
            IF ((MN.EQ.1 .AND. IFIX(X).GT.IFIX(PPOS(I))) .OR. 
     +          (MN.EQ.2 .AND. IFIX(X).LT.IFIX(PPOS(I)))) THEN
               WRITE(IW,*)' Peaks must be within limits, try again'
                  itry=itry+1
                  IF(itry.GE.3)THEN
                    WRITE(6,*)'Sorry, not your day today'
                    RETURN
                  ENDIF

               GO TO 10
            ENDIF
40       CONTINUE
         GETMKRCHNL = X
      ENDIF
      END
