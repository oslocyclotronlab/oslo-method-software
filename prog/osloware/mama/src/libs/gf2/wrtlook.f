
C=======================================================================

      SUBROUTINE WRTLOOK

C          write out look-up table to disk file (l.u. 2)....

      INTEGER*2 LOOKTAB(8192)
      INTEGER   NCLOOK,LOOKMIN,LOOKMAX
      COMMON /LOOK/ NCLOOK,LOOKMIN,LOOKMAX,LOOKTAB

      COMMON /LUS/ IR,IW,IP,IG


      REWIND(13)
      WRITE (13,ERR=800) NCLOOK,LOOKMIN,LOOKMAX
      WRITE (13,ERR=800) (LOOKTAB(I),I=1,NCLOOK)
      RETURN

C           error messages....

800   WRITE(IW,*) 'Error: cannot write to look-up file.'
      RETURN
      END
