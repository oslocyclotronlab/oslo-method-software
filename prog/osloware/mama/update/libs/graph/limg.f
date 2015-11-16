
C=======================================================================

          SUBROUTINE LIMG(NX,IX,NY,IY)

C      define region of screen to hold display (i.e. screen window)....

      INTEGER       NX,IX,NY,IY

      REAL               FDX,FX0,FDY,FY0
      INTEGER            IDX,IX0,IDY,IY0,IYFLAG,ITERM
      COMMON /MINIG_DAT/ FDX,FX0,FDY,FY0,IDX,IX0,IDY,IY0,IYFLAG,ITERM


      IDX = NX
      IX0 = IX
      IDY = NY
      IY0 = IY+20
      RETURN
      END
