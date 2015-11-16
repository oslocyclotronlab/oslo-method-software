      PROGRAM Mama2paw
      CHARACTER line*65,n*2,e*9,de*6,w*7,dw*6,a*9,da*5,filename*80
      WRITE(6,*)'     __________________________________________'
      WRITE(6,*)'    |                                          |'
      WRITE(6,*)'    |           M A M A 2 P A W  1.0           |'
      WRITE(6,*)'    |                                          |'
      WRITE(6,*)'    |   Program to change the output of mama   |'
      WRITE(6,*)'    |   peakfit.out  into something readable   |'
      WRITE(6,*)'    |                 for PAW.                 |'
      WRITE(6,*)'    |  Lawrence Livermore National Laboratory  |'
      WRITE(6,*)'    |                                          |'
      WRITE(6,*)'    |          Created:  06/18 - 2002          |'
      WRITE(6,*)'    |             Andreas Schiller             |'
      WRITE(6,*)'    |__________________________________________|'
      filename='peaks'
      WRITE(6,1)filename
 1    FORMAT('Filename <',A5,'>:',$)
      CALL READA(5,filename)
      OPEN(10,ERR=98,FILE=filename,STATUS='OLD')
      OPEN(11,ERR=98,FILE='mama2paw.paw')
 2    CONTINUE
      READ(10,'(A65)',IOSTAT=I,ERR=99)line
      IF(I.LT.0)GOTO 4
      n(:)=line(:2)
      e(:)=line(42:50)
      de(:)=line(52:57)
      w(:)=line(3:9)
      dw(:)=line(11:16)
      a(:)=line(18:26)
      da(:)=line(28:32)
      WRITE(11,3)n,e,de,w,dw,a,da
 3    FORMAT(A2,A9,A6,A7,A6,A9,A5)
      GOTO 2
 4    WRITE(6,'(A31)')'Result written to: mama2paw.paw'
      STOP
 98   STOP'Error during opening file'
 99   STOP'Error during reading line'
      END


