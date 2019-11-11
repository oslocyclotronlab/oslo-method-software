      SUBROUTINE CHANGECOL
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      INTEGER COLORMAP(20),Colorc(0:19)
      COMMON /COLORMAP/ COLORMAP,Limit,Colorc
      REAL Limit(0:19)
      CHARACTER ANS*60, TEX*2
 
      iblack=20

      ictype=MAX0(ictype,1)       !Present type of color combinations
      icmd=1
      WRITE(6,1)icmd
 1    FORMAT('Choose color composition for 3D-plot.. (1)',
     +     /,'Interchange certain colors............ (2)',
     +     /,'Reset all colors to default values.... (3)',
     +     /,'Return to mama........................ (4) <',I1,'>:',$)
      CALL READI(5,icmd)
      IF(icmd.LT.1.OR.icmd.GT.3)Istatus=1
      IF(Istatus.NE.0)RETURN

      IF(icmd.EQ.1)THEN
        CALL ERASE
        CALL INITG(NX,NY)                    !Finding window size (nx,ny) 
        CALL LIMG(NX,LOX,NY,LOY)             !Informing screen window
        mx1=0      
        my1=(NY/2)+90
        myl=my1
        myh=myl+10
        DO it=1,10                           !10 types Color combinations
          CALL ColComp(it)
          mxl=mx1+10
          DO ic=1,14
            mxh=mxl+20
            CALL SETCOLOR(Colorc(ic-1))
            DO j=myl,myh              
              CALL KTRAS(mxl,j,0)     
              CALL KTRAS(mxh,j,1)
            ENDDO
            mxl=mxl+20
          ENDDO
          CALL SETCOLOR(iblack)
          CALL KTRAS(mxl+20,myl,0)
          WRITE(TEX,8)it
 8        FORMAT(I2)
          CALL PUTG(TEX,8,1)
          myl=myl-15
          myh=myl+10
        ENDDO
        CALL FINIG
        ians=ictype
        WRITE(6,7)ians
 7      FORMAT('Choose color composition (1-10) for 3D-plot <',I2,'>:',$)
        CALL READI(5,ians)
        IF(ians.LT.1.OR.iians.GT.10)Istatus=1
        IF(Istatus.NE.0)RETURN
        ictype=ians
        CALL ColComp(ictype)
      ENDIF

      IF(icmd.EQ.2)THEN
        CALL ERASE
        CALL INITG(NX,NY)                    !Finding window size (nx,ny) 
        CALL LIMG(NX,LOX,NY,LOY)             !Informing screen window
        mx1=5      
        my1=NY/2
        myl=my1
        myh=myl+10
        mxl=mx1
        DO ic=1,20
          mxh=mxl+18
          CALL SETCOLOR(COLORMAP(ic))
          DO j=myl,myh              
            CALL KTRAS(mxl,j,0)     
            CALL KTRAS(mxh,j,1)
          ENDDO
          CALL SETCOLOR(iblack)
          CALL KTRAS(mxl+6,myl-20,0)
          WRITE(TEX,2)ic
  2       FORMAT(I2)
          CALL PUTG(TEX,8,1)
          mxl=mxl+20
        ENDDO
        CALL FINIG
        WRITE(IW,4) (COLORMAP(I),I=1,20)
  4     FORMAT(  'Present Colors:',20(I3))
        CALL CASK('New Colors:      ',ANS,NC)
        IHI = -1
        DO IMAP=1,20
          LO = IHI + 2
          IF (LO.GT.NC) RETURN
          DO I=LO,NC                         !Find first non-blank character
            IF (ANS(I:I).NE.' ') GO TO 5
          ENDDO
          RETURN
  5       LO = I
          IF (ANS(LO:LO).EQ.',') THEN        !Comma only; no value entered
            IHI = LO - 1
            GO TO 99
          ENDIF
          DO I=LO,NC                         !Find upper delimiter (blank or comma)
            IF (ANS(I:I).EQ.' '.OR.ANS(I:I).EQ.',') GO TO 6
          ENDDO
          I = NC + 1
  6       IHI = I - 1
          CALL ININ(ANS(LO:IHI),IHI-LO+1,IDATA,J1,J2,*99) !Decode value
          COLORMAP(IMAP) = MOD(IDATA-1,20)+1
        ENDDO
  99    RETURN
      ENDIF

      IF(icmd.EQ.3)THEN
        DO ic=1,20
          COLORMAP(ic)=ic
        ENDDO
        ictype=1
        CALL ColComp(ictype)
      ENDIF

      CALL SETCOLOR(iblack)                      !Reset foreground to black
      RETURN
      END


      SUBROUTINE ColComp(ictype)
      INTEGER COLORMAP(20),Colorc(0:19)
      COMMON /COLORMAP/ COLORMAP,Limit,Colorc
      REAL Limit(0:19)

      IF(ictype.LT.1.or.ictype.gt.10)RETURN

C Following 20 colors are defined in minig_x_c.c
C The color brown is put in foreground by CALL SETCOLOR(7), and so on
c      1  = BLUE
c      2  = DEEP SKY BLUE
c      3  = LIGHT SKY BLUE
c      4  = SEA GREEN
c      5  = MEDIUM SEA GREEN
c      6  = GREEN
c      7  = BROWN
c      8  = CHOCOLATE
c      9  = SANDY BROWN
c      10 = RED
c      11 = CORAL
c      12 = ORANGE
c      13 = YELLOW3
c      14 = YELLOW2
c      15 = YELLOW
c      16 = PEACH PUFF
c      17 = PAPAYA WHIP
c      18 = OLD LACE
c      19 = WHITE
c      20 = BLACK


C Numbers taken from COLORMAP gives as default the same number: COLORMAP(7)=7
C However, these values can be changed by the user. Absolute white/black is stored
C in Color(18) and Color(19) - and can not be changed
      Colorc(18) =19              !white     (6 upper colors, not used in 3D-plot)
      Colorc(19) =20              !black
      Colorc(14) =COLORMAP(1 )    !blue
      Colorc(15) =COLORMAP(19)    !white
      Colorc(16) =COLORMAP(2 )    !deep sky blue
      Colorc(17) =COLORMAP(3 )    !light sky blue

      IF(ictype.EQ.1)THEN                     !the default
        Colorc(0) =COLORMAP(1 )    !blue
        Colorc(1) =COLORMAP(19)    !white
        Colorc(2) =COLORMAP(2 )    !deep sky blue
        Colorc(3) =COLORMAP(3 )    !light sky blue

        Colorc(4) =COLORMAP(4 )    !sea green
        Colorc(5) =COLORMAP(5 )    !medium sea green
        Colorc(6) =COLORMAP(6 )    !green

        Colorc(7)=COLORMAP(13)     !yellow3
        Colorc(8)=COLORMAP(15)     !yellow
        Colorc(9) =COLORMAP(12)    !orange

        Colorc(10) =COLORMAP(8 )    !chocolate
        Colorc(11) =COLORMAP(11)    !coral
        Colorc(12) =COLORMAP(10)    !red
        Colorc(13) =COLORMAP(19)    !white
      ENDIF

      IF(ictype.EQ.2)THEN                     !start with sand
        Colorc(0) =COLORMAP(16)    !peach puff
        Colorc(1) =COLORMAP(19)    !white
        Colorc(2) =COLORMAP(17)    !papaya whip
        Colorc(3) =COLORMAP(18)    !old lace

        Colorc(4) =COLORMAP(7 )    !brown
        Colorc(5) =COLORMAP(8 )    !chocolate
        Colorc(6) =COLORMAP(9 )    !sandy brown

        Colorc(7) =COLORMAP(10)    !red
        Colorc(8) =COLORMAP(11)    !coral
        Colorc(9) =COLORMAP(12)    !orange

        Colorc(10)=COLORMAP(13)    !yellow3
        Colorc(11)=COLORMAP(14)    !yellow2
        Colorc(12)=COLORMAP(15)    !yellow
        Colorc(13)=COLORMAP(19)    !white
      ENDIF

      IF(ictype.EQ.3)THEN                     !interchange red and yellow
        Colorc(0) =COLORMAP(1 )    !blue
        Colorc(1) =COLORMAP(19)    !white
        Colorc(2) =COLORMAP(2 )    !deep sky blue
        Colorc(3) =COLORMAP(3 )    !light sky blue

        Colorc(4) =COLORMAP(7 )    !brown
        Colorc(5) =COLORMAP(8 )    !chocolate
        Colorc(6) =COLORMAP(9 )    !sandy brown

        Colorc(7) =COLORMAP(13)    !yellow3
        Colorc(8) =COLORMAP(14)    !yellow2
        Colorc(9) =COLORMAP(15)    !yellow

        Colorc(10)=COLORMAP(10)    !red
        Colorc(11)=COLORMAP(11)    !coral
        Colorc(12)=COLORMAP(12)    !orange
        Colorc(13)=COLORMAP(16)    !peach puff
      ENDIF

      IF(ictype.EQ.4)THEN                     !end with white
        Colorc(0) =COLORMAP(1 )    !blue
        Colorc(1) =COLORMAP(19)    !white
        Colorc(2) =COLORMAP(2 )    !deep sky blue
        Colorc(3) =COLORMAP(3 )    !light sky blue

        Colorc(4) =COLORMAP(4 )    !sea green
        Colorc(5) =COLORMAP(5 )    !medium sea green
        Colorc(6) =COLORMAP(6 )    !green

        Colorc(7) =COLORMAP(10)    !red
        Colorc(8) =COLORMAP(11)    !coral
        Colorc(9) =COLORMAP(12)    !orange

        Colorc(10)=COLORMAP(13)    !yellow3
        Colorc(11)=COLORMAP(14)    !yellow2
        Colorc(12)=COLORMAP(19)    !yellow
        Colorc(13)=COLORMAP(19)    !white
      ENDIF

      IF(ictype.EQ.5)THEN                     !for black/white
        Colorc(0) =COLORMAP(1 )    !blue
        Colorc(1) =COLORMAP(19)    !white
        Colorc(2) =COLORMAP(7 )    !brown
        Colorc(3) =COLORMAP(10)    !red

        Colorc(4) =COLORMAP(4 )    !sea green
        Colorc(5) =COLORMAP(6 )    !green
        Colorc(6) =COLORMAP(8 )    !chocolate

        Colorc(7) =COLORMAP(2 )    !deep sky blue
        Colorc(8) =COLORMAP(3 )    !light sky blue
        Colorc(9) =COLORMAP(12)    !orange

        Colorc(10)=COLORMAP(13)    !yellow3
        Colorc(11)=COLORMAP(14)    !yellow2
        Colorc(12)=COLORMAP(18)    !old lace
        Colorc(13)=COLORMAP(19)    !white
      ENDIF

      IF(ictype.EQ.6)THEN                     !sandy colors
        Colorc(0) =COLORMAP(7 )    !brown
        Colorc(1) =COLORMAP(19)    !white
        Colorc(2) =COLORMAP(8 )    !chocolate
        Colorc(3) =COLORMAP(9 )    !sandy brown

        Colorc(4) =COLORMAP(4 )    !sea green
        Colorc(5) =COLORMAP(5 )    !medium sea green
        Colorc(6) =COLORMAP(6 )    !green

        Colorc(7) =COLORMAP(13)    !yellow3
        Colorc(8) =COLORMAP(14)    !yellow2
        Colorc(9) =COLORMAP(15)    !yellow

        Colorc(10)=COLORMAP(16)    !peach puff
        Colorc(11)=COLORMAP(17)    !papaya whip
        Colorc(12)=COLORMAP(18)    !old lace
        Colorc(13)=COLORMAP(19)    !white
      ENDIF

      IF(ictype.EQ.7)THEN                     !blue colors
        Colorc(0) =COLORMAP(1 )    !blue
        Colorc(1) =COLORMAP(19)    !white
        Colorc(2) =COLORMAP(2 )    !deep sky blue
        Colorc(3) =COLORMAP(3 )    !light sky blue

        Colorc(4) =COLORMAP(4 )    !sea green
        Colorc(5) =COLORMAP(5 )    !medium sea green
        Colorc(6) =COLORMAP(6 )    !green

        Colorc(7) =COLORMAP(7 )    !brown
        Colorc(8) =COLORMAP(8 )    !chocolate
        Colorc(9) =COLORMAP(9 )    !sandy brown

        Colorc(10)=COLORMAP(10)    !red
        Colorc(11)=COLORMAP(11)    !coral
        Colorc(12)=COLORMAP(12)    !orange
        Colorc(13)=COLORMAP(19)    !white
      ENDIF

      IF(ictype.EQ.8)THEN                     !hot colors
        Colorc(0) =COLORMAP(4 )    !sea green
        Colorc(1) =COLORMAP(19)    !white
        Colorc(2) =COLORMAP(5 )    !medium sea green
        Colorc(3) =COLORMAP(6 )    !green

        Colorc(4) =COLORMAP(7 )    !brown
        Colorc(5) =COLORMAP(8 )    !chocolate
        Colorc(6) =COLORMAP(9 )    !sandy brown

        Colorc(7) =COLORMAP(10)    !red
        Colorc(8) =COLORMAP(11)    !coral
        Colorc(9) =COLORMAP(12)    !orange

        Colorc(10)=COLORMAP(13)    !yellow3
        Colorc(11)=COLORMAP(14)    !yellow2
        Colorc(12)=COLORMAP(15)    !yellow
        Colorc(13)=COLORMAP(19)    !white
      ENDIF

      IF(ictype.EQ.9)THEN                     !cold colors
        Colorc(0) =COLORMAP(1 )    !blue
        Colorc(1) =COLORMAP(19)    !white
        Colorc(2) =COLORMAP(2 )    !deep sky blue
        Colorc(3) =COLORMAP(3 )    !light sky blue

        Colorc(4) =COLORMAP(4 )    !sea green
        Colorc(5) =COLORMAP(5 )    !medium sea green
        Colorc(6) =COLORMAP(6 )    !green

        Colorc(7) =COLORMAP(7 )    !brown
        Colorc(8) =COLORMAP(8 )    !chocolate
        Colorc(9) =COLORMAP(9 )    !sandy brown

        Colorc(10)=COLORMAP(13)    !yellow3
        Colorc(11)=COLORMAP(14)    !yellow2
        Colorc(12)=COLORMAP(15)    !yellow
        Colorc(13)=COLORMAP(19)    !white
      ENDIF

      IF(ictype.EQ.10)THEN                    !strong color variations
        Colorc(0) =COLORMAP(1 )    !blue
        Colorc(1) =COLORMAP(19)    !white
        Colorc(2) =COLORMAP(6 )    !green
        Colorc(3) =COLORMAP(7 )    !brown

        Colorc(4) =COLORMAP(10)    !red
        Colorc(5) =COLORMAP(15)    !yellow
        Colorc(6) =COLORMAP(2 )    !deep sky blue

        Colorc(7) =COLORMAP(3 )    !light sky blue
        Colorc(8) =COLORMAP(4 )    !sea green
        Colorc(9) =COLORMAP(5 )    !medium sea green

        Colorc(10)=COLORMAP(8 )    !chocolate
        Colorc(11)=COLORMAP(9 )    !sandy brown
        Colorc(12)=COLORMAP(11)    !coral
        Colorc(13)=COLORMAP(19)    !white
      ENDIF

      CALL FINIG
      RETURN
      END
