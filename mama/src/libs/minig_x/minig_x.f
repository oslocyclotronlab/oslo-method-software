      PROGRAM MINIG_X

C             GRAPHICS PACKAGE FROM STRASBOURG
C             version 2.1; modified by D.C.Radford    March 1992
C                 for color SUN workstation running Xwindows....

      CHARACTER*40 ANS
      CALL SET_XWG('standard')
      CALL INITG(NX,NY)
            
      CALL TXTCLR(0)
      CALL ERASE
      nx1=nx
      nx2=0
      ny1=ny/2
      ny2=ny/2
      write(6,*)nx,ny

      CALL LIMG(nx1,nx2,ny1,ny2)
      CALL TRAX(100.,0.,110.,0.,1)
      LG=   NX/80*2
      DO 10 N=1,20
         X=5*N
         Y=X
         CALL SETCOLOR(N+1)
         CALL SYMBG(2,X,Y,LG)
10    CONTINUE

      CALL MSPOT(0,NY)
      
      CALL PUTG(' MINIG sample graph.',20,3)
      CALL FINIG
      WRITE(6,15)
15    FORMAT(' '//' MINIG sample graph.')

C          call cursor....

40    WRITE(6,*) ' Type any character; X to exit'
60    CALL RETIC(X,Y,ANS)
      WRITE(6,90) ANS(1:1),X,Y
90    FORMAT(5X,A1,'   X =',F10.0,'   Y =',F10.0)
      IF (ANS(1:1).NE.'X' .AND. ANS(1:1).NE.'x') GO TO 60

C     Erase the graphics window....
      CALL ERASE
      print *, 'The graphics screen was just cleared.'
      WRITE(6,*) ' Type any character; X to exit'
      CALL RETIC(X,Y,ANS)
      CALL SAVE_XWG('standard')

      END
