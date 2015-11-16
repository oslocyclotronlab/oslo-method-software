      PROGRAM webrho
      REAL ex,s,ds,dum
      CHARACTER FILUT*7,FILIN*7

      idim = 25

      FILIN='gsf.dat'
      FILUT='rsf.txt'
      
      OPEN(28,FILE=FILIN,ACCESS='SEQUENTIAL',STATUS='OLD')
      OPEN(29,FILE=FILUT,ACCESS='SEQUENTIAL')
      WRITE(29,*)'  No    Eg(MeV)    f(MeV**-3)    df(MeV**-3)'
      DO i=1,idim
         READ(UNIT=28,FMT=*)ex,s,dum,ds
         WRITE(29,50)i,ex,s,ds  
 50      FORMAT(I4,F11.3,'',2E15.3)
      ENDDO
      CLOSE(29)
      CLOSE(28)
      END
