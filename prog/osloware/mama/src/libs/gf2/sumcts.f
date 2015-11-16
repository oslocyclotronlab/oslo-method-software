
      SUBROUTINE SUMCTS(K,ILO,IHI)

      INTEGER       MCH(2)
      REAL          PPOS(15)
      COMMON /MKRS/ MCH,PPOS
      LOGICAL DISP
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LOCH,HICH
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      REAL LDZ,HDZ,LOCNT,HICNT
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60

      INTEGER         IWMODE
      CHARACTER*8     NWTSP
      REAL            WTSP(8192)
      COMMON /WTMODE/ IWMODE,NWTSP,WTSP

      CHARACTER*40 ANS
      COMMON /LUS/ IR,IW,IP,IG

C  K=0: sum without background subtraction
C  K=1: sum with    background subtraction     K=1 always, Magne

      IF(ILO.GE.0.AND.IHI.GT.0.AND.ILO.LE.MAXCH.AND.IHI.LE.MAXCH)THEN
        Y1=rSPEC(IDEST,ILO)
        Y2=rSPEC(IDEST,IHI)
      ELSE
        IF(.NOT.DISP) THEN
10        CALL ASK(25HLimits for integration = ,25,ANS,NC)
          IF (NC.EQ.0) RETURN
          CALL ININ(ANS,NC,ILO,IHI,J2,&10)
          IF(ILO.GE.0.AND.IHI.GT.0
     +    .AND. ILO.LE.MAXCH.AND.IHI.LE.MAXCH)THEN
            Y1=rSPEC(IDEST,ILO)
            Y2=rSPEC(IDEST,IHI)
          ELSE
            GO TO 10
          ENDIF
        ELSE
          WRITE(IW,*) 'Use cursor to define limits, T to type limits'
          CALL RETIC(X,Y,ANS)
          IF (ANS(1:1).EQ.'T'.OR.ANS(1:1).EQ.'t') GO TO 10
          ILO=X
          Y1=Y
          CALL RETIC(X,Y,ANS)
          IF (ANS(1:1).EQ.'T'.OR.ANS(1:1).EQ.'t') GO TO 10
          IHI=X
          Y2=Y
        ENDIF
      ENDIF

C Checking that everything OK
      IF (ILO.GT.IHI)THEN
         ISAV=IHI
         IHI=ILO
         ILO=ISAV
         YSAVE=Y2
         Y2=Y1
         Y1=YSAVE
      ENDIF
      Dch=IHI-ILO
      IF (Dch.LT.0.5)Dch=1.0

C Display limits
      IF(DISP)THEN
         ISAVE=MCH(1)
         MCH(1)=ILO
         CALL DSPMKR(1)
         MCH(1)=IHI
         CALL DSPMKR(1)
         MCH(1)=ISAVE
      ENDIF

C Initializing
      a0=cal(2,IDEST,1,1)  !calibration coeff.
      a1=cal(2,IDEST,1,2)
      a3=cal(2,IDEST,1,3)
      Atot=0.              !areas
      Abac=0.
      Anet=0.
      xtot=0.
      xbac=0.
      xnet=0.
      Ctot=0.              !centroides
      Cbac=0.
      Cnet=0.
      ECtot=9999.99
      ECbac=9999.99
      ECnet=9999.99
      Stot=0.              !standard variations (sigma's)
      Sbac=0.
      Snet=0.
      EStot=9999.99
      ESbac=9999.99
      ESnet=9999.99

C Displaying background
      CALL INITG(NX,NY)
      X=FLOAT(ILO)+0.5
      CALL PSPOT(X,Y1)
      X=FLOAT(IHI)+0.5
      CALL VECT(X,Y2)
      CALL FINIG

      DO I=ILO,IHI
        c=rSPEC(IDEST,I)
        xtot=xtot+c
        IF(c.LT.0)c=0.
        Atot=Atot+c
        Ctot=Ctot+c*FLOAT(I)
        Stot=Stot+c*(FLOAT(I)*FLOAT(I))

        c=Y1+(Y2-Y1)*FLOAT(I-ILO)/Dch
        xbac=xbac+c
        IF(c.LT.0)c=0.
        Abac=Abac+c
        Cbac=Cbac+c*FLOAT(I)
        Sbac=Sbac+c*(FLOAT(I)*FLOAT(I))

        c=rSPEC(IDEST,I)-(Y1+(Y2-Y1)*FLOAT(I-ILO)/Dch)
        xnet=xnet+c
        IF(c.LT.0)c=0.
        Anet=Anet+c
        Cnet=Cnet+c*FLOAT(I)
        Snet=Snet+c*(FLOAT(I)*FLOAT(I))
      ENDDO
 
      IF(Atot.GT.0)THEN
        Ctot=(Ctot/Atot)
        ECtot=a0+a1*Ctot+a2*Ctot*Ctot
        Stot=(Stot/Atot)
        Stot=Stot-Ctot*Ctot
        IF(Stot.GT.0)THEN
          Stot=SQRT(Stot)
          EStot=(a1+2.0*a2*Ctot)*Stot
        ELSE
          Stot= 9999.99
          EStot=9999.99
        ENDIF
      ELSE
        Ctot= 9999.99
        ECtot=9999.99
        Stot= 9999.99
        EStot=9999.99
      ENDIF

      IF(Abac.GT.0)THEN
        Cbac=(Cbac/Abac)
        ECbac=a0+a1*Cbac+a2*Cbac*Cbac
        Sbac=(Sbac/Abac)
        Sbac=Sbac-Cbac*Cbac
        IF(Sbac.GT.0)THEN
          Sbac=SQRT(Sbac)
          ESbac=(a1+2.0*a2*Cbac)*Sbac
        ELSE
          Sbac= 9999.99
          ESbac=9999.99
        ENDIF
      ELSE
        Cbac= 9999.99
        ECbac=9999.99
        Sbac= 9999.99
        ESbac=9999.99
      ENDIF


       IF(Anet.GT.0)THEN
        Cnet=(Cnet/Anet)
        ECnet=a0+a1*Cnet+a2*Cnet*Cnet
        Snet=(Snet/Anet)
        Snet=Snet-Cnet*Cnet
        IF(Snet.GT.0)THEN
          Snet=SQRT(Snet)
          ESnet=(a1+2.0*a2*Cnet)*Snet
        ELSE
          Snet= 9999.99
          ESnet=9999.99
        ENDIF
      ELSE
        Cnet= 9999.99
        ECnet=9999.99
        Snet= 9999.99
        ESnet=9999.99
      ENDIF

      
C Write results
 13   WRITE(6,140)ILO,IHI
      WRITE(6,138)
      WRITE(6,141)
      WRITE(6,139)
      WRITE(6,142)Atot,Ctot,ECtot,Stot,ABS(EStot)
      WRITE(6,143)Abac,Cbac,ECbac,Sbac,ABS(ESbac)
      WRITE(6,144)Anet,Cnet,ECnet,Snet,ABS(ESnet)
      WRITE(6,139)
140   FORMAT(/'           Region ',I5,' -',I5,' channels')
138   FORMAT( '           Assuming only channels with positive counts')
141   FORMAT( '             Counts    Centr.(ch)  Centr.(keV)  St.dev(ch) St.dev(keV)')
139   FORMAT( '______________________________________________________________________')
142   FORMAT( 'Total   ',E13.6,F10.2,F13.2,F10.2,F13.2)
143   FORMAT( 'Backgr. ',E13.6,F10.2,F13.2,F10.2,F13.2)
144   FORMAT( 'Netto   ',E13.6,F10.2,F13.2,F10.2,F13.2)
      WRITE(6,145)xtot,xbac,xnet
145   FORMAT( '(Assuming +/- counts: Total=',E13.6,' Backgr.=',E13.6,' Netto=',E13.6,' )')
180   RETURN
      END
