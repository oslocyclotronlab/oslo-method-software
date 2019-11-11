
C=======================================================================

      SUBROUTINE FIX_PARA(PARAM,FIX_FLAG)
      
C     fixes or frees, depending on the value of FIX_FLAG, 
C     a parameter to an inputed value....
C     CALLED by FIXORFREE
      
      INTEGER PARAM
      LOGICAL FIX_FLAG
      
      REAL          PARS(51), ERRS(51)
      INTEGER       IFIXED(51), NPARS, NFP, NPKS, IRELW, IRELPOS
      COMMON /PARA/ IRELW,PARS,IFIXED,NPARS,NFP,ERRS,NPKS,IRELPOS
      
      LOGICAL         DISP
      INTEGER         LOCH,HICH,LOCNT,NCHS,NCNTS,IYAXIS,LOX,NUMX
      COMMON /DISPLA/ DISP,LOCH,HICH,LOCNT,NCHS,NCNTS,IYAXIS,LOX,NUMX
      
      CHARACTER*80 ANS
      COMMON /LUS/ IR,IW,IP,IG
      
      
      IF (FIX_FLAG) THEN
C                 fix parameter....
         IF (PARAM.LE.0) THEN
C                 no input or negative number so....
            RETURN
         ELSEIF (PARAM.EQ.101) THEN
            IRELPOS = 0
            WRITE (IW,*) 'Relative peak positions fixed.'
         ELSEIF (PARAM.EQ.102) THEN
            IRELW = 0
            WRITE (IW,*) 'Relative widths fixed.'
         ELSEIF (PARAM.GT.NPARS) THEN
            WRITE (IW,*) 'Parameter number too large, try again.'
         ELSEIF ((NFP+IFIXED(PARAM)).EQ.(NPARS-1)) THEN
            WRITE (IW,*) '     Cannot - too many fixed pars.'

         ELSE
            NFP = NFP + IFIXED(PARAM)
            IFIXED(PARAM) = 0
 60         CALL CASK('Value=?(rtn for present value)',ANS,NC)
            IF (NC.GT.0) THEN
               CALL FFIN(ANS,NC,VAL,RJ1,RJ2,IALT_RET)
               IF (IALT_RET.EQ.1) GO TO 60
               IF (VAL.EQ.0.0 .AND.
C                    ...    .AND. if PARAM is a width or beta....
     +             ((PARAM+1)/3)*3.EQ.PARAM+1 .AND. PARAM.NE.2) THEN
                  WRITE (IW,*) 'Value must be nonzero.'
                  GO TO 60
               ENDIF
            ELSE
               VAL = PARS(PARAM)
            ENDIF

            IF (PARAM.NE.4 .OR. VAL.NE.0.0) THEN
               PARS(PARAM) = VAL
            ELSEIF ((NFP+IFIXED(5)).EQ.(NPARS-1)) THEN
               WRITE (IW,*) '     Cannot - too many fixed pars.'
               GO TO 60
            ELSE
               NFP = NFP + IFIXED(5)
               IFIXED(5) = 0
               WRITE (IW,'(A,F10.3)') ' >>> Beta fixed at',PARS(5)
               PARS(PARAM) = VAL
            ENDIF
         ENDIF
      
      ELSE
C                 free parameter....

         IF (PARAM.GT.0) THEN
            IF (PARAM.EQ.101) THEN
               IRELPOS = 1
               WRITE (IW,*) 'Relative peak positions free to vary.'
            ELSEIF (PARAM.EQ.102) THEN
               IRELW = 1
               WRITE (IW,*) 'Relative widths free to vary.'
            ELSEIF (PARAM.GT.NPARS) THEN
               WRITE (IW,*) 'Parameter number too large, try again.'
            ELSE
               NFP = IFIXED(PARAM) + NFP - 1
               IFIXED(PARAM) = 1
            ENDIF
         ENDIF
         
      ENDIF
      RETURN
      END
