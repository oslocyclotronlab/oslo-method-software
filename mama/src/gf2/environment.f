      SUBROUTINE ENVIRONMENT

      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60

      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LOCH,HICH
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      REAL LDZ,HDZ,LOCNT,HICNT
      COMMON/OL/I3,iRC,m1,m2
      COMMON/DisType/Idistype,OLlow,OLhigh,OLlocnt,OLhicnt
      INTEGER                 OLlow,OLhigh
      COMMON/AXIS/iCE,itext,UNITx,UNITy,UNITx0,UNITy0
      CHARACTER UNITx*3,UNITy*3,UNITx0*3,UNITy0*3,UNIT*3
      COMMON/sdod/ntelesc
      
      WRITE(6,4)
   4  FORMAT(/,'The y-axis for ESP, DESP, EDESP and THICKSP',/,
     +         'should be set according to # of telescopes',/,
     +         'in the SIRIUS or OFFLINE sorting routines.')
      n=ntelesc
      WRITE(6,1)n
   1  FORMAT(  'Number of telescopes (8, 9, ...64)    <',I2,'>:',$)
      CALL READI(5,n)
      IF(Istatus.NE.0)RETURN
      ntelesc=n
      IF(ntelesc.LT.1 )ntelesc=8
      IF(ntelesc.GT.64)ntelesc=64
      
      UNIT=UNITx0
      WRITE(6,2)UNIT
   2  FORMAT(  'Units or name on x-axis ( < 4 char)  <',A3,'>:',$)
      CALL READA(5,UNIT)
      IF(Istatus.NE.0)RETURN
      UNITx0=UNIT

      UNIT=UNITy0
      WRITE(6,3)UNIT
   3  FORMAT(  'Units or name on y-axis ( < 4 char)  <',A3,'>:',$)
      CALL READA(5,UNIT) 
      IF(Istatus.NE.0)RETURN
      UNITy0=UNIT

      RETURN
      END
        
      
