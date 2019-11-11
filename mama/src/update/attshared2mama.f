C     =========================================================%
      subroutine  sirius_spectra( TYPE,specna )
C     =========================================================%
C     PURPOSE : Attach the SIRIUS shared memory histogram      %
C               area to the mama program
C     =========================================================%
      IMPLICIT NONE

      integer TYPE, I1
      integer specno
      character specna*8
      integer i, j, result

C --  Commons used by mama applications
      COMMON/Sp1Dim/MSPEC(0:8191),MAXCH
      COMMON/Sp2Dim/MAT(0:4095,0:511),APP(512),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      integer Istatus, ITYPE, IDEST, Idim, MAXCH, MSPEC
      real cal
      integer MAT, XDIM, YDIM      
      character APP*4

      LOGICAL DISP
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT

C --  Define spectra 
      integer singles(0:4095,0:9)
      integer esp(0:2047,0:63)			
      integer desp(0:2047,0:63)
      integer edesp(0:2047,0:63)	
      integer thicksp(0:2047,0:63)
      integer gesp(0:4095,0:5)
      integer tgesp(0:511,0:5)
      integer nasp(0:2047,0:31)
      integer tnasp(0:511,0:31)
      integer alfna(0:2047,0:511)
      integer alfge(0:2047,0:511)
      integer xmat(0:2047,0:63)

C --  Attach SIRIUS acq spectrum area shared memory
      integer attspec2mama
      external attspec2mama !$pragma C( attspec2mama )	

C --  ---------------------------------------------------------------------
C --  Copy the spectrum to be displayed into mama spectrum matrix 1
C --  ---------------------------------------------------------------------

C --  Spectrum SINGLES
      if ( specna .EQ. 'SINGLES') then
         specno = 1
         result = attspec2mama( %VAL(specno), singles ) 	
         if ( result .EQ. -1) then
            write(*,*)'*** ERROR ***  Attach singles spectrum area failed !'
            return
         end if
         TYPE = 2	! 2-D spectrum
         xdim = 4096
         ydim = 10
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = singles(i,j)
            end do
         end do

C --  E-counter  matrix 
      else if  ( specna .EQ. 'ESP') then
         specno = 2
         result = attspec2mama( %VAL(specno), esp ) 	
         if ( result .EQ. -1) then
            write(*,*)'*** ERROR ***  Attach singles spectrum area failed !'
            return
         end if
         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 64
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = esp(i,j)
            end do
         end do

C --  DE-counter  matrix 
      else if ( specna .EQ. 'DESP') then

         specno = 3
         result = attspec2mama( %VAL(specno), desp ) 	
         if ( result .EQ. -1) then
            write(*,*)'*** ERROR ***  Attach singles spectrum area failed !'
            return
         end if
         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 64
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = desp(i,j)
            end do
         end do

C --  E-DE-spectrum matrix to file 
      else if ( specna .EQ. 'EDESP') then
         specno = 4
         result = attspec2mama( %VAL(specno), edesp ) 	
         if ( result .EQ. -1) then
            write(*,*)'*** ERROR ***  Attach singles spectrum area failed !'
            return
         end if
         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 64
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = edesp(i,j)
            end do
         end do
 
C --  Thickness  matrix
      else if ( specna .EQ. 'THICKSP') then
         specno = 5
         result = attspec2mama( %VAL(specno), thicksp ) 	
         if ( result .EQ. -1) then
            write(*,*)'*** ERROR ***  Attach singles spectrum area failed !'
            return
         end if
         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 64
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = thicksp(i,j)
            end do
         end do

C --  GE-spectrum matrix to file
      else if ( specna .EQ. 'GESP') then
         specno = 6
         result = attspec2mama( %VAL(specno), gesp ) 	
         if ( result .EQ. -1) then
            write(*,*)'*** ERROR ***  Attach singles spectrum area failed !'
            return
         end if
         TYPE = 2	! 2-D spectrum
         xdim = 4096
         ydim = 6
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = gesp(i,j)
            end do
         end do

C --  Ge-T spectrum matrix to file
      else if ( specna .EQ. 'TGESP') then
         specno = 7
         result = attspec2mama( %VAL(specno), tgesp ) 	
         if ( result .EQ. -1) then
            write(*,*)'*** ERROR ***  Attach singles spectrum area failed !'
            return
         end if
         TYPE = 2	! 2-D spectrum
         xdim = 512
         ydim = 6
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = tgesp(i,j)
            end do
         end do

C --  NaI spectrum matrix to file
      else if ( specna .EQ. 'NASP') then
         specno = 8
         result = attspec2mama( %VAL(specno), nasp ) 	
         if ( result .EQ. -1) then
            write(*,*)'*** ERROR ***  Attach singles spectrum area failed !'
            return
         end if
         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 32
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = nasp(i,j)
            end do
         end do

C --  NaI-T spectrum matrix to file
      else if ( specna .EQ. 'TNASP') then
         specno = 9
         result = attspec2mama( %VAL(specno), tnasp ) 	
         if ( result .EQ. -1) then
            write(*,*)'*** ERROR ***  Attach singles spectrum area failed !'
            return
         end if
         TYPE = 2	! 2-D spectrum
         xdim = 512
         ydim = 32
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = tnasp(i,j)
            end do
         end do

C --  Spectrum ALFNA
      else if ( specna .EQ. 'ALFNA') then
         specno = 10
         result = attspec2mama( %VAL(specno), alfna ) 	
         if ( result .EQ. -1) then
            write(*,*)'*** ERROR ***  Attach singles spectrum area failed !'
            return
         end if
         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 512
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = alfna(i,j)
            end do
         end do

C --  Spectrum ALFGE
      else if ( specna .EQ. 'ALFGE') then
         specno = 11
         result = attspec2mama( %VAL(specno), alfge ) 	
         if ( result .EQ. -1) then
            write(*,*)'*** ERROR ***  Attach singles spectrum area failed !'
            return
         end if
         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 512
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = alfge(i,j)
            end do
         end do

C --  Spectrum MAT
      else if ( specna .EQ. 'MAT') then
         specno = 12
         result = attspec2mama( %VAL(specno), xmat ) 	
         if ( result .EQ. -1) then
            write(*,*)'*** ERROR ***  Attach singles spectrum area failed !'
            return
         end if
         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 64
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = xmat(i,j)
            end do
         end do


      else
         write (*,*) ' *** ERROR *** Unknown spectrum name '	
         TYPE = -1
      end if 

      if(TYPE.EQ.2)then
        ITYPE=3
        I1=1
      endif
      if(TYPE.EQ.1)then        
        ITYPE=1
        I1=2
      endif
      if((TYPE.NE.-1).AND.(fname(I1,1).NE.specna))then
        fname(I1,1)=specna
      endif

      end  	! End-Of-Subroutine sirius_spectra



C     =========================================================%
      subroutine  offline_spectra( TYPE,specna )
C     =========================================================%
C     PURPOSE : Attach the OFFLINE shared memory histogram     %
C               area to the mama program
C     =========================================================%
      IMPLICIT NONE

      integer TYPE, I1
      integer specno
      character specna*8
      integer i, j, result

C --  Commons used by mama applications
      COMMON/Sp1Dim/MSPEC(0:8191),MAXCH
      COMMON/Sp2Dim/MAT(0:4095,0:511),APP(512),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      integer Istatus, ITYPE, IDEST, Idim,MAXCH,MSPEC
      real cal
      integer MAT, XDIM, YDIM      
      character APP*4

      LOGICAL DISP
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT


C --  Define spectra 
      integer singles(0:4095,0:9)
      integer esp(0:2047,0:63)			
      integer desp(0:2047,0:63)
      integer edesp(0:2047,0:63)	
      integer thicksp(0:2047,0:63)
      integer gesp(0:4095,0:5)
      integer tgesp(0:511,0:5)
      integer nasp(0:2047,0:31)
      integer tnasp(0:511,0:31)
      integer alfna(0:2047,0:511)
      integer alfge(0:2047,0:511)
      integer xmat(0:2047,0:63)

C --  Attach spectrum area shared memory
      integer offspec2mama
      external offspec2mama !$pragma C( offspec2mama )

C --  ---------------------------------------------------------------------
C --  Copy the spectrum to be displayed into mama spectrum matrix 1
C --  ---------------------------------------------------------------------

C --  Spectrum SINGLES
      if ( specna .EQ. 'SINGLES') then
         specno = 1
         result = offspec2mama( %VAL(specno), singles ) 	
         if ( result .EQ. -1) then
            write(*,*)'*** ERROR ***  Attach singles spectrum area failed !'
            return
         end if
         TYPE = 2	! 2-D spectrum
         xdim = 4096
         ydim = 10
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = singles(i,j)
            end do
         end do

C --  E-counter  matrix 
      else if  ( specna .EQ. 'ESP') then
         specno = 2
         result = offspec2mama( %VAL(specno), esp ) 	
         if ( result .EQ. -1) then
            write(*,*)'*** ERROR ***  Attach singles spectrum area failed !'
            return
         end if
         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 64
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = esp(i,j)
            end do
         end do

C --  DE-counter  matrix 
      else if ( specna .EQ. 'DESP') then

         specno = 3
         result = offspec2mama( %VAL(specno), desp ) 	
         if ( result .EQ. -1) then
            write(*,*)'*** ERROR ***  Attach singles spectrum area failed !'
            return
         end if
         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 64
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = desp(i,j)
            end do
         end do

C --  E-DE-spectrum matrix to file 
      else if ( specna .EQ. 'EDESP') then
         specno = 4
         result = offspec2mama( %VAL(specno), edesp ) 	
         if ( result .EQ. -1) then
            write(*,*)'*** ERROR ***  Attach singles spectrum area failed !'
            return
         end if
         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 64
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = edesp(i,j)
            end do
         end do
 
C --  Thickness  matrix
      else if ( specna .EQ. 'THICKSP') then
         specno = 5
         result = offspec2mama( %VAL(specno), thicksp ) 	
         if ( result .EQ. -1) then
            write(*,*)'*** ERROR ***  Attach singles spectrum area failed !'
            return
         end if
         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 64
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = thicksp(i,j)
            end do
         end do

C --  GE-spectrum matrix to file
      else if ( specna .EQ. 'GESP') then
         specno = 6
         result = offspec2mama( %VAL(specno), gesp ) 	
         if ( result .EQ. -1) then
            write(*,*)'*** ERROR ***  Attach singles spectrum area failed !'
            return
         end if
         TYPE = 2	! 2-D spectrum
         xdim = 4096
         ydim = 6
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = gesp(i,j)
            end do
         end do

C --  Ge-T spectrum matrix to file
      else if ( specna .EQ. 'TGESP') then
         specno = 7
         result = offspec2mama( %VAL(specno), tgesp ) 	
         if ( result .EQ. -1) then
            write(*,*)'*** ERROR ***  Attach singles spectrum area failed !'
            return
         end if
         TYPE = 2	! 2-D spectrum
         xdim = 512
         ydim = 6
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = tgesp(i,j)
            end do
         end do

C --  NaI spectrum matrix to file
      else if ( specna .EQ. 'NASP') then
         specno = 8
         result = offspec2mama( %VAL(specno), nasp ) 	
         if ( result .EQ. -1) then
            write(*,*)'*** ERROR ***  Attach singles spectrum area failed !'
            return
         end if
         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 32
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = nasp(i,j)
            end do
         end do

C --  NaI-T spectrum matrix to file
      else if ( specna .EQ. 'TNASP') then
         specno = 9
         result = offspec2mama( %VAL(specno), tnasp ) 	
         if ( result .EQ. -1) then
            write(*,*)'*** ERROR ***  Attach singles spectrum area failed !'
            return
         end if
         TYPE = 2	! 2-D spectrum
         xdim = 512
         ydim = 32
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = tnasp(i,j)
            end do
         end do

C --  Spectrum ALFNA
      else if ( specna .EQ. 'ALFNA') then
         specno = 10
         result = offspec2mama( %VAL(specno), alfna ) 	
         if ( result .EQ. -1) then
            write(*,*)'*** ERROR ***  Attach singles spectrum area failed !'
            return
         end if
         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 512
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = alfna(i,j)
            end do
         end do

C --  Spectrum ALFGE
      else if ( specna .EQ. 'ALFGE') then
         specno = 11
         result = offspec2mama( %VAL(specno), alfge ) 	
         if ( result .EQ. -1) then
            write(*,*)'*** ERROR ***  Attach singles spectrum area failed !'
            return
         end if
         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 512
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = alfge(i,j)
            end do
         end do

C --  Spectrum MAT
      else if ( specna .EQ. 'MAT') then
         specno = 12
         result = offspec2mama( %VAL(specno), xmat ) 	
         if ( result .EQ. -1) then
            write(*,*)'*** ERROR ***  Attach singles spectrum area failed !'
            return
         end if
         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 64
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = xmat(i,j)
            end do
         end do


      else
         write (*,*) ' *** ERROR *** Unknown spectrum name '	
         TYPE = -1
      end if  

      if(TYPE.EQ.2)then
        ITYPE=3
        I1=1
      endif
      if(TYPE.EQ.1)then        
        ITYPE=1
        I1=2
      endif
      if((TYPE.NE.-1).AND.(fname(I1,1).NE.specna))then
        fname(I1,1)=specna
      endif


      end  	! End-Of-Subroutine offline_spectra
