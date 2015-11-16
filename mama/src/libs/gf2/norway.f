      SUBROUTINE NORWAY
C Routine to display norwegian flag
C M.Guttormsen november 1993
      LOGICAL DISP
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LOCH,HICH
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      REAL LDZ,HDZ,LOCNT,HICNT
      INTEGER COLORMAP(20),Colorc(0:19)
      COMMON /COLORMAP/ COLORMAP,Limit,Colorc
      REAL Limit(0:19)
      REAL               FDX,FX0,FDY,FY0
      INTEGER            IDX,IX0,IDY,IY0,IYFLAG,ITERM
      COMMON /MINIG_DAT/ FDX,FX0,FDY,FY0,IDX,IX0,IDY,IY0,IYFLAG,ITERM

      
C Setting up colors for 3-dim landscape
      ictype=1
      CALL INITG(NX,NY)    !Finding window size (nx,ny) 
      CALL ColComp(ictype) !Puts default color-vaues to Color(0:19)

      NX0=NX
      NY0=NY
      
      I1=5
      n=9
      LOY=0
      LOX=0
    
      NY=NY/3             !Number of y-pixels pr. spectrum
      NX=NX/3
      NY=(16./22.)*NX
      LOY=NY              !Pixelstart in y-direction
      LOX=NX              !Pixelstart in x-direction
      
      CALL LIMG(NX,LOX,NY,LOY) !Informing screen window

      MX1=LOX
      MX2=MX1+NX*(6./22.)
      MX3=MX1+NX*(7./22.)
      MX4=MX1+NX*(9./22.)
      MX5=MX1+NX*(10./22.)
      MX6=MX1+NX
      
      MY1=LOY
      MY2=MY1+NY*(6./16.)
      MY3=MY1+NY*(7./16.)
      MY4=MY1+NY*(9./16.)
      MY5=MY1+NY*(10./16.)
      MY6=MY1+NY
     
      CALL SETCOLOR(Colorc(12))
      DO j=MY1,MY6               
        CALL KTRAS(MX1,j,0)     
        CALL KTRAS(MX6,j,1)
      ENDDO

      CALL SETCOLOR(Colorc(18))
      DO j=MY2,MY5               
        CALL KTRAS(MX1,j,0)      
        CALL KTRAS(MX6,j,1)
      ENDDO
      DO i=MX2,MX5               
        CALL KTRAS(i,MY1,0)      
        CALL KTRAS(i,MY6,1)
      ENDDO

      CALL SETCOLOR(Colorc(0))
      DO j=MY3,MY4               
        CALL KTRAS(MX1,j,0)      
        CALL KTRAS(MX6,j,1)
      ENDDO
      DO i=MX3,MX4               
        CALL KTRAS(i,MY1,0)      
        CALL KTRAS(i,MY6,1)
      ENDDO

      CALL FINIG

      DO i=1,10000000                    !dummy and artificial wait
        x=SIN(.022)
        x=SIN(.022)
        x=SIN(.022)
      ENDDO 

c      isystem=sleep(1)
      CALL SLEEP(1)

C Fadeing out
      CALL SETCOLOR(Colorc(18))     !White dots all over
c      DO i=0,NX*NY
c        ix=LOX+rand(0)*NX+0.5
c        iy=LOY+rand(0)*NY+0.5
c        CALL KTRAS(ix,iy,2)
c      ENDDO
      DO j=MY1,MY6                !Total delete
        CALL KTRAS(MX1,j,0)     
        CALL KTRAS(MX6,j,1)
      ENDDO

C Tests colors
c      mxl=mx1-10
c      call ktras(mx1,my1,0)
c      do ic=1,20
c        mxl=mxl+10
c        mxh=mxl+10
c        call setcolor(colormap(ic))
c        DO j=MY1,MY6              
c          CALL KTRAS(MXl,j,0)     
c          CALL KTRAS(MXh,j,1)
c        ENDDO
c      enddo

      CALL SETCOLOR(19)
      
C Going back to full window
      CALL LIMG(NX0,0,NY0,0) !Informing screen window
      CALL INITG(NX,NY)
C Gets window parameters for use for F77 routines through
C COMMON /MINIG_DAT/ FDX,FX0,FDY,FY0,IDX,IX0,IDY,IY0,IYFLAG,ITERM
      CALL GETGLOBALS(FDX,FX0,FDY,FY0,IDX,IX0,IDY,IY0,IYFLAG)

      END

