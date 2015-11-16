
C=======================================================================

      SUBROUTINE CHNGMARK(IDATA)

C        Allows the user to change the fitting limits and/or 
C        the peak positions and to optionally reset the free parameters 
C        to their initial estimates.
C
C        INPUT:            - the marker number     - IDATA .
C        OUTPUT: Changes:  - the marker[s]         - MCH, and/or 
C                          - the peak position[s]  - PPOS.
C        CALLed by GFEXEC.
C        CALLs function GETMKRCHNL

C        local variables....
      INTEGER        MN
      REAL           GETMKRCHNL

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

      COMMON /LUS/ IR,IW,IP,IG


      MN = IDATA
C        MN is the Marker Number....
C        MCH(MN),    (MN = 1,2) are the lower and upper bounds
C                                 of the fitting region....
C        PPOS(MN-2), (MN = 3,4,5..,NPKS+2) are the peak positions....

      IF (MN.LE.0 .OR. MN.GT.NPKS+2) THEN
c           the data passed does not correspond to a marker
c           or there was no data passed....
         CALL ASKYN(20HChange limits? (Y/N),20,&30)
         DO 20 MN = 1, 2
            MCH(MN) = NINT(GETMKRCHNL(MN))
            IF (DISP) CALL DSPMKR(MN)
20       CONTINUE
30       CALL ASKYN(28HChange peak positions? (Y/N),28,&100)
         DO 40 MN = 3, NPKS + 2
            PPOS(MN-2) = GETMKRCHNL(MN)
            IF (DISP) CALL DSPMKR(MN)
40       CONTINUE
      ELSE
c           if the user knows what s/he wants to change, do it, &return....
         IF (MN.LE.2) THEN
            WRITE (IW,*) '     New marker position? (type T for type)'
            MCH(MN) = GETMKRCHNL(MN)
         ELSEIF (MN.GT.2) THEN
            WRITE (IW,*) '     New peak position? (type T for type)'
            PPOS(MN-2) = GETMKRCHNL(MN)
         ENDIF
         IF (DISP) CALL DSPMKR(MN)
      ENDIF

100   CALL ASKYN(44HReset free pars. to initial estimates? (Y/N),44,
     +                                                           &110)
      CALL PARSET(0)
110   RETURN
      END
