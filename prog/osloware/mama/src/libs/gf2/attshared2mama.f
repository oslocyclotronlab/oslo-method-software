C     =========================================================%
      subroutine  sirius_spectra( TYPE )
C     =========================================================%
C     PURPOSE : Attach the SIRIUS shared memory histogram      %
C               area to the mama program
C     =========================================================%

      integer TYPE, I1, ntelesc
      integer specno, called
      character specna*8
      integer i, j
      integer lastno, lnblnk

C --  Commons used by mama applications
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      integer Istatus, ITYPE, IDEST, Idim, MAXCH
      real cal
      integer XDIM, YDIM      
      character APP*4

      LOGICAL DISP
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LOCH,HICH
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      REAL LDZ,HDZ,LOCNT,HICNT

      COMMON/sdod/ntelesc


C --  Define spectra 
      integer esp(0:2047,0:63)			
      integer desp(0:2047,0:63)
      integer edesp(0:2047,0:63)	
      integer thicksp(0:2047,0:63)
      integer gesp(0:4095,0:5)
      integer tgesp(0:511,0:5)
      integer nasp(0:2047,0:31)
      integer tnasp(0:511,0:31)
      integer singles(0:4095,0:9)

      pointer (pesp, esp)
      pointer (pdesp, desp)
      pointer (pedesp, edesp)
      pointer (pthicksp, thicksp)
      pointer (pgesp, gesp)
      pointer (ptgesp, tgesp)
      pointer (pnasp, nasp)
      pointer (ptnasp, tnasp)
      pointer (psingles , singles)


C --  Attach SIRIUS acq spectrum area shared memory
      integer attspec
      external attspec !$pragma C( attspec )	

C --  Detach SIRIUS acq spectrum area shared memory
      integer detshared
      external detshared !$pragma C( detshared ) 

      if ( called .LT. 1 ) then
         specna = 'NASP'
      end if
  

C --  ---------------------------------------------------------------------
C --  User input of spectrum name, previous is default
C --  ---------------------------------------------------------------------

      lastno = lnblnk( specna )
      write (6,1) specna(1:lastno)
  1   format ('Filename           <',A,'>:',$)

      call getname ( specna )




C --  ---------------------------------------------------------------------
C --  Copy the spectrum to be displayed into mama spectrum matrix 1
C --  ---------------------------------------------------------------------

      

C --  E-counter  matrix 
      if      ( specna .EQ. 'ESP') then

         specno = 2
         pesp = attspec( %VAL(specno) ) 	
         if ( pesp  .EQ. 0) then
            write (*,*) '**** ERROR ****  Attach  E-spectra failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = ntelesc
         do j = 0,ydim-1
            do i = 0,xdim-1
               rMAT(1,i,j) = esp(i,j)
            end do
         end do

         if ( detshared( %VAL(pesp) ) .EQ. -1) then
            write (*,*) '**** ERROR ****  Detach E spectra failed'
         end if


C --  DE-counter  matrix 
      else if ( specna .EQ. 'DESP') then

         specno = 3
         pdesp = attspec( %VAL(specno) ) 	
         if ( pdesp  .EQ. 0) then
            write (*,*) '**** ERROR ****  Attach DE-spectra failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = ntelesc
         do j = 0,ydim-1
            do i = 0,xdim-1
               rMAT(1,i,j) = desp(i,j)
            end do
         end do

         if ( detshared( %VAL(pdesp) ) .EQ. -1) then
            write (*,*) '**** ERROR ****  Detach DE spectra failed'
         end if


C --  E-DE-spectrum matrix to file 
      else if ( specna .EQ. 'EDESP') then

         specno = 4
         pedesp = attspec( %VAL(specno) ) 	
         if ( pedesp  .EQ. 0) then
            write (*,*) '**** ERROR ****  Attach E-DE-spectra failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = ntelesc
         do j = 0,ydim-1
            do i = 0,xdim-1
               rMAT(1,i,j) = edesp(i,j)
            end do
         end do
 
         if ( detshared( %VAL(pedesp) ) .EQ. -1) then
            write (*,*) '**** ERROR ****  Detach E-DE spectra failed'
         end if

C --  Thickness  matrix
      else if ( specna .EQ. 'THICKSP') then

         specno = 5
         pthicksp = attspec( %VAL(specno) ) 	
         if ( pthicksp  .EQ. 0) then
            write(*,*)'**** ERROR ****  Attach thickness spectra failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = ntelesc
         do j = 0,ydim-1
            do i = 0,xdim-1
               rMAT(1,i,j) = thicksp(i,j)
            end do
         end do

         if ( detshared( %VAL(pthicksp) ) .EQ. -1) then
            write (*,*) '**** ERROR ****  Detach Thick spectra failed'
         end if

C --  GE-spectrum matrix to file
      else if ( specna .EQ. 'GESP') then

         specno = 6
         pgesp = attspec( %VAL(specno) ) 	
         if ( pgesp  .EQ. 0) then
            write (*,*) '**** ERROR ****  Attach Ge-spectra failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 4096
         ydim = 6
         do j = 0,ydim-1
            do i = 0,xdim-1
               rMAT(1,i,j) = gesp(i,j)
            end do
         end do

         if ( detshared( %VAL(pgesp) ) .EQ. -1) then
            write (*,*) '**** ERROR ****  Detach Ge spectra failed'
         end if


C --  Ge-T spectrum matrix to file
      else if ( specna .EQ. 'TGESP') then

         specno = 7
         ptgesp = attspec( %VAL(specno) ) 	
         if ( ptgesp  .EQ. 0) then
            write (*,*) '**** ERROR ****  Attach Ge-T-spectra failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 512
         ydim = 6
         do j = 0,ydim-1
            do i = 0,xdim-1
               rMAT(1,i,j) = tgesp(i,j)
            end do
         end do

         if ( detshared( %VAL(ptgesp) ) .EQ. -1) then
            write (*,*) '**** ERROR ****  Detach Ge-T spectra failed'
         end if

C --  NaI spectrum matrix to file
      else if ( specna .EQ. 'NASP') then

         specno = 8
         pnasp = attspec( %VAL(specno) ) 	
         if ( pnasp  .EQ. 0) then
            write (*,*) '**** ERROR ****  Attach NaI spectra failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 32
         do j = 0,ydim-1
            do i = 0,xdim-1
               rMAT(1,i,j) = nasp(i,j)
            end do
         end do

         if ( detshared( %VAL(pnasp) ) .EQ. -1) then
            write (*,*) '**** ERROR ****  Detach NaI spectra failed'
         end if

C --  NaI-T spectrum matrix to file
      else if ( specna .EQ. 'TNASP') then

         specno = 9
         ptnasp = attspec( %VAL(specno) ) 	
         if ( ptnasp  .EQ. 0) then
            write (*,*) '**** ERROR ****  Attach NaI-T spectra failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 512
         ydim = 32
         do j = 0,ydim-1
            do i = 0,xdim-1
               rMAT(1,i,j) = tnasp(i,j)
            end do
         end do

         if ( detshared( %VAL(ptnasp) ) .EQ. -1) then
            write (*,*) '**** ERROR ****  Detach NaI-T spectra failed'
         end if

C --  Spectrum SINGLES
      else if ( specna .EQ. 'SINGLES') then

         specno = 1
         psingles = attspec( %VAL(specno) ) 	
         if ( psingles  .EQ. 0) then
            write(*,*)'*** ERROR ***  Attach singles spectrum area failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 4096
         ydim = 10
         do j = 0,ydim-1
            do i = 0,xdim-1
               rMAT(1,i,j) = singles(i,j)
            end do
         end do

         if ( detshared( %VAL(psingles) ) .EQ. -1) then
            write (*,*) '**** ERROR **** Detach Singles spectra failed'
         end if


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
        CALL SetMarker(1,1,1)
        fname(I1,1)=specna
      endif
      if(TYPE.NE.-1)comm(I1,1)='|SD:'//specna
      
      called = called + 1


      end  	! End-Of-Subroutine sirius_spectra



C     =========================================================%
      subroutine  offline_spectra( TYPE )
C     =========================================================%
C     PURPOSE : Attach the OFFLINE shared memory histogram     %
C               area to the mama program
C     =========================================================%

      integer TYPE, I1, ntelesc
      integer specno, called
      character specna*8
      integer i, j
      integer lastno, lnblnk

C --  Commons used by mama applications
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      integer Istatus, ITYPE, IDEST, Idim, MAXCH
      real cal
      integer XDIM, YDIM      
      character APP*4

      LOGICAL DISP
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LOCH,HICH
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      REAL LDZ,HDZ,LOCNT,HICNT

      COMMON/sdod/ntelesc

C --  Define spectra 
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
      integer singles(0:4095,0:9)
			
      pointer (pesp, esp)
      pointer (pdesp, desp)
      pointer (pedesp, edesp)
      pointer (pthicksp, thicksp)
      pointer (pgesp, gesp)
      pointer (ptgesp, tgesp)
      pointer (pnasp, nasp)
      pointer (ptnasp, tnasp)
      pointer (psingles , singles)
      pointer (pansp, alfna)
      pointer (pagsp, alfge)
      pointer (pmtsp, xmat)


C --  Attach spectrum area shared memory
      integer offspec
      external offspec !$pragma C( offspec )	


C --  Detach spectrum area shared memory
      integer detshared
      external detshared !$pragma C( detshared ) 


      if ( called .LT. 1 ) then
         specna = 'NASP'
      end if


C --  ---------------------------------------------------------------------
C --  User input of spectrum name, previous is default
C --  ---------------------------------------------------------------------

      lastno = lnblnk( specna )
      write (6,1) specna(1:lastno)
  1   format ('Filename           <',A,'>:',$)

      call getname ( specna )




C --  ---------------------------------------------------------------------
C --  Copy the spectrum to be displayed into mama spectrum matrix 1
C --  ---------------------------------------------------------------------

      

C --  E-counter  matrix 
      if      ( specna .EQ. 'ESP') then

         specno = 2
         pesp = offspec( %VAL(specno) ) 	
         if ( pesp  .EQ. 0) then
            write (*,*) '**** ERROR ****  Attach  E-spectra failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = ntelesc
         do j = 0,ydim-1
            do i = 0,xdim-1
               rMAT(1,i,j) = esp(i,j)
            end do
         end do

         if ( detshared( %VAL(pesp) ) .EQ. -1) then
            write (*,*) '**** ERROR ****  Detach E spectra failed'
         end if


C --  DE-counter  matrix 
      else if ( specna .EQ. 'DESP') then

         specno = 3
         pdesp = offspec( %VAL(specno) ) 	
         if ( pdesp  .EQ. 0) then
            write (*,*) '**** ERROR ****  Attach DE-spectra failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = ntelesc
         do j = 0,ydim-1
            do i = 0,xdim-1
               rMAT(1,i,j) = desp(i,j)
            end do
         end do

         if ( detshared( %VAL(pdesp) ) .EQ. -1) then
            write (*,*) '**** ERROR ****  Detach DE spectra failed'
         end if

C --  E-DE-spectrum matrix to file 
      else if ( specna .EQ. 'EDESP') then

         specno = 4
         pedesp = offspec( %VAL(specno) ) 	
         if ( pedesp  .EQ. 0) then
            write (*,*) '**** ERROR ****  Attach E-DE-spectra failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = ntelesc
         do j = 0,ydim-1
            do i = 0,xdim-1
               rMAT(1,i,j) = edesp(i,j)
            end do
         end do
 
         if ( detshared( %VAL(pedesp) ) .EQ. -1) then
            write (*,*) '**** ERROR ****  Detach E-DE spectra failed'
         end if

C --  Thickness  matrix
      else if ( specna .EQ. 'THICKSP') then

         specno = 5
         pthicksp = offspec( %VAL(specno) ) 	
         if ( pthicksp  .EQ. 0) then
            write(*,*)'**** ERROR ****  Attach thickness spectra failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = ntelesc
         do j = 0,ydim-1
            do i = 0,xdim-1
               rMAT(1,i,j) = thicksp(i,j)
            end do
         end do

         if ( detshared( %VAL(pthicksp) ) .EQ. -1) then
            write (*,*) '**** ERROR ****  Detach Thick spectra failed'
         end if


C --  GE-spectrum matrix to file
      else if ( specna .EQ. 'GESP') then

         specno = 6
         pgesp = offspec( %VAL(specno) ) 	
         if ( pgesp  .EQ. 0) then
            write (*,*) '**** ERROR ****  Attach Ge-spectra failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 4096
         ydim = 6
         do j = 0,ydim-1
            do i = 0,xdim-1
               rMAT(1,i,j) = gesp(i,j)
            end do
         end do

         if ( detshared( %VAL(pgesp) ) .EQ. -1) then
            write (*,*) '**** ERROR ****  Detach Ge spectra failed'
         end if


C --  Ge-T spectrum matrix to file
      else if ( specna .EQ. 'TGESP') then

         specno = 7
         ptgesp = offspec( %VAL(specno) ) 	
         if ( ptgesp  .EQ. 0) then
            write (*,*) '**** ERROR ****  Attach Ge-T-spectra failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 512
         ydim = 6
         do j = 0,ydim-1
            do i = 0,xdim-1
               rMAT(1,i,j) = tgesp(i,j)
            end do
         end do

         if ( detshared( %VAL(ptgesp) ) .EQ. -1) then
            write (*,*) '**** ERROR ****  Detach Ge-T spectra failed'
         end if

C --  NaI spectrum matrix 
      else if ( specna .EQ. 'NASP') then

         specno = 8
         pnasp = offspec( %VAL(specno) ) 	
         if ( pnasp  .EQ. 0) then
            write (*,*) '**** ERROR ****  Attach NaI spectra failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 32
         do j = 0,ydim-1
            do i = 0,xdim-1
               rMAT(1,i,j) = nasp(i,j)
            end do
         end do

         if ( detshared( %VAL(pnasp) ) .EQ. -1) then
            write (*,*) '**** ERROR ****  Detach NaI spectra failed'
         end if

C --  NaI-T spectrum matrix
      else if ( specna .EQ. 'TNASP') then

         specno = 9
         ptnasp = offspec( %VAL(specno) ) 	
         if ( ptnasp  .EQ. 0) then
            write (*,*) '**** ERROR ****  Attach NaI-T spectra failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 512
         ydim = 32
         do j = 0,ydim-1
            do i = 0,xdim-1
               rMAT(1,i,j) = tnasp(i,j)
            end do
         end do

         if ( detshared( %VAL(ptnasp) ) .EQ. -1) then
            write (*,*) '**** ERROR ****  Detach NaI-T spectra failed'
         end if

C --  Spectrum SINGLES
      else if ( specna .EQ. 'SINGLES') then

         specno = 1
         psingles = offspec( %VAL(specno) ) 	
         if ( psingles  .EQ. 0) then
            write(*,*)'*** ERROR ***  Attach singles spectrum area failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 4096
         ydim = 10
         do j = 0,ydim-1
            do i = 0,xdim-1
               rMAT(1,i,j) = singles(i,j)
            end do
         end do

         if ( detshared( %VAL(psingles) ) .EQ. -1) then
            write (*,*) '**** ERROR **** Detach Singles spectra failed'
         end if

C --  Spectrum ALFNA
      else if ( specna .EQ. 'ALFNA') then
         specno = 10
         pansp = offspec( %VAL(specno) ) 	
         if ( pansp  .EQ. 0) then
            write(*,*)'*** ERROR ***  Attach Alpha-NaI spectrum area failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 512
         do j = 0,ydim-1
            do i = 0,xdim-1
               rMAT(1,i,j) = alfna(i,j)
            end do
         end do

         if ( detshared( %VAL(pansp) ) .EQ. -1) then
            write (*,*) '**** ERROR **** Detach Alpha-NaI spectra failed'
         end if

C --  Spectrum ALFGE
      else if ( specna .EQ. 'ALFGE') then
         specno = 11
         pagsp = offspec( %VAL(specno) ) 	
         if ( pagsp  .EQ. 0) then
            write(*,*)'*** ERROR ***  Attach Alpha-Ge spectrum area failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 512
         do j = 0,ydim-1
            do i = 0,xdim-1
               rMAT(1,i,j) = alfge(i,j)
            end do
         end do

         if ( detshared( %VAL(pagsp) ) .EQ. -1) then
            write (*,*) '**** ERROR **** Detach Alpha-Ge spectra failed'
         end if

C --  Spectrum MAT
      else if ( specna .EQ. 'MAT') then
         specno = 12
         pmtsp = offspec( %VAL(specno) ) 	
         if ( pmtsp  .EQ. 0) then
            write(*,*)'*** ERROR ***  Attach General Purpose Matrix area failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 64
         do j = 0,ydim-1
            do i = 0,xdim-1
               rMAT(1,i,j) = xmat(i,j)
            end do
         end do

         if ( detshared( %VAL(pmtsp) ) .EQ. -1) then
            write (*,*) '**** ERROR **** Detach  General Purpose Matrix spectra failed'
         end if


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
        CALL SetMarker(1,1,1)
        fname(I1,1)=specna
      endif
      if(TYPE.NE.-1)comm(I1,1)='|OD:'//specna

      called = called + 1


      end  	! End-Of-Subroutine offline_spectra



C  ------------------------------------------------------------------
      subroutine getname( specna )

      integer lastno, lnblnk

      character x*8
      character specna*8
      character string*8

      read (*, 1) x

      if ( x .EQ. '' ) return

      read (x, 1) string
      
      lastno = lnblnk( string )
      specna = string(1:lastno)


    1 format (8A) 

      return 
      end


