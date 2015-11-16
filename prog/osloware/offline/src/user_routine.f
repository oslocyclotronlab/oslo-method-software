C     -----------------------------------------
C     apos4b.f Sorting routine for SIRIUS
C
C     Written by : Andreas Schiller
C     Date       : 21 Mar 1997
C     Exp. name  : 8 Tel, 32 NaI, 3 Ge, Pile up
C     Comment    : All Purpose Online Sorting
C     -----------------------------------------*

C     ---------------------------------------------------------
C     All purpose online sorting program for fast sorting of
C     8 Telescope spectra, 32 NaI spectra and 3 Ge spectra.
C     For the first time, advanced pile up handling included. 
C     The sorting program can be used for particle-gamma
C     coincidences as well as for gamma-gamma coincidences.
C     ---------------------------------------------------------
C     Spectra sorted:
C
C         ESP 0- 7, ch 0000-2047: Energy of end counters
C        DESP 0- 7, ch 0000-2047: Energy of front counters
C       EDESP 0- 7, ch 0000-2047: Total energy of telescopes
C     THICKSP 0- 7, ch 0000-2047: Thickness of telescopes
C        GESP 0- 2, ch 0000-4095: Energy of Ge detectors
C        GESP 3- 5, ch 0000-4095: Pile up of Ge detectors
C       TGESP 0- 2, ch 0000-0511: Time of Ge detectors
C       TGESP 3- 5, ch 0000-0511: Time of Ge pile up events
C        NASP 0-31, ch 0000-2047: Energy of NaI detectors
C       TNASP 0-31, ch 0000-0511: Time of NaI detectors
C     SINGLES 0- 7, ch 0000-2047: Pile up of front counters
C     SINGLES 0- 7, ch 2048-4095: Pile up of end counters
C     SINGLES  8  , ch 0000-2047: Pile up of telescopes group A
C     SINGLES  9  , ch 0000-2047: Pile up of telescopes group B
C     SINGLES  8  , ch 2048-4095: Pile up of thickness group A
C     SINGLES  9  , ch 2048-4095: Pile up of thickness group B
C     ---------------------------------------------------------
C     Hard wired requirements:
C
C     CAMAC Pile Up Rejection Module
C           Use input number 2
C           Use ch  0- 7 for front counters
C           Use ch  8-10 for Ge detectors
C           Use ch 14-15 for end counter groups A and B
C
C     Master TPU
C           Use ch  0 for telescope group A
C           Use ch  2 for telescope group B
C           Use ch  4 for Ge detector 1
C           Use ch  6 for Ge detector 2
C           Use ch  8 for Ge detector 3
C           Use ch 10 for Ge time information
C
C     ADC's
C           Use 4k spectra for Ge detectors
C           Use 1k spectra for Ge time information
C     ---------------------------------------------------------

C     ---------------------Variables used----------------------

      SUBROUTINE eventsort(singles,esp,desp,edesp,thicksp,gesp,
     +tgesp,nasp,tnasp)
      IMPLICIT NONE
      INCLUDE 'common_defs.decl'
      INCLUDE 'spec_params.decl'      
      REAL r,rand,fe,fde
      INTEGER i,j,e,de,ede,thick,tn,ena,tna,ege,tge,geo
      LOGICAL pu,pe,pde

C     ---------------------Start-------------------------------

      j=0
      pe=.FALSE.
      pde=.FALSE.
      r=rand(0)-0.5
C     Initialize some variables
      pu=(m(9,0).EQ.1)
C     Look if pile up has occured

C     ---------------------TPU 1-------------------------------

      IF (m(0,0).EQ.1) THEN
C     Look if TPU 1 has pattern

C     --------------------------Particles----------------------

C     -----------------------------------Group A---------------

       IF (btest(m(0,1),0)) THEN
C       Look if Group A has signal
        de=m(1,0)
        e=m(2,0)
C       Get channel of front and end detectors 
        tn=e/2048
        i=de/2048
C       Calculate telescope number
        IF (tn.NE.i) GOTO 100
C       Look if #front detector = #end detector
        de=de-tn*2048
        e=e-tn*2048
C       Subtract offset
        j=j+1
C       Increment number of telescopes fired
       END IF

C     -----------------------------------Group B---------------

       IF (btest(m(0,1),2)) THEN
C       Look if Group B has signal
        de=m(1,2)
        e=m(2,2)
C       Get channel of front and end detectors
        tn=e/2048
        i=de/2048
C       Calculate relative telescope number
        IF (tn.NE.i) GOTO 100
C       Look if #front detector = #end detector
        de=de-tn*2048
        e=e-tn*2048
C       Subtract offset
        tn=tn+4
C       Calculate true telescope number
        j=j+1
C       Increment number of telescopes fired
       END IF

C     -----------------------------------Group A+B-------------

       IF (j.EQ.1) THEN
C       Look if only one telescope has signal
        fe=(REAL(e)+r)*gaine(tn)+shifte(tn)
        fde=(REAL(de)+r)*gainde(tn)+shiftde(tn)
C       Align spectra
        e=INT(fe+0.5)
        de=INT(fde+0.5)
C       Restore spectra
        ede=INT(fe+fde+0.5)
C       Make total energy spectra
        IF ((e.LT.0).OR.(e.GT.2047)) e=0
        IF ((de.LT.0).OR.(de.GT.2047)) de=0
        IF ((ede.LT.0).OR.(ede.GT.2047)) ede=0
C       Look if all spectra are in range
        thick=range(ede)-range(e)
C       Make thickness spectra
        IF ((thick.LT.0).OR.(thick.GT.2047)) thick=0
C       Look if thickness spectra are in range
        IF (pu.EQ..TRUE.) THEN
         IF (btest(m(10,1),14+tn/4)) pe=.TRUE.
         IF (btest(m(10,1),tn)) pde=.TRUE. 
C        Look for pile up in individual channels
        END IF
        IF ((pe.EQ..TRUE.).OR.(pde.EQ..TRUE.)) THEN
         IF (pde.EQ..TRUE.) singles(de,tn)=singles(de,tn)+1
         IF (pe.EQ..TRUE.) singles(e+2048,tn)=singles(e+2048,tn)+1
         singles(ede,8+tn/4)=singles(ede,8+tn/4)+1
         singles(thick+2048,8+tn/4)=singles(thick+2048,8+tn/4)+1
C        Increment pile up spectra using singles spectra
        ELSE
         esp(e,tn)=esp(e,tn)+1     
         desp(de,tn)=desp(de,tn)+1
         edesp(ede,tn)=edesp(ede,tn)+1  
         thicksp(thick,tn)=thicksp(thick,tn)+1
C        Increment regular spectra 
        END IF
       END IF

C     --------------------------Germanium----------------------

C     -----------------------------------Ge 1------------------

 100   IF (btest(m(0,1),4)) THEN
C       Look if Ge 1 has signal
        ege=INT((REAL(m(1,4))+r)*gainge(0)+shiftge(0)+0.5)
C       Get channel of Ge 1 and align spectra
        IF ((ege.LT.0).OR.(ege.GT.4095)) ege=0
C       Look if Ge 1 spectrum is in range 
        geo=0
        IF ((pu.EQ..TRUE.).AND.(btest(m(10,1),8))) geo=3
C       Look for pile up in Ge 1
        gesp(ege,geo)=gesp(ege,geo)+1
C       Increment Ge 1 spectrum
        IF (btest(m(0,1),10)) THEN
C       Look for Ge time spectrum
         tge=m(1,10)/2+shifttge(0)
C        Get channel of Ge time and align spectra
         IF ((tge.LT.0).OR.(tge.GT.511)) tge=0
C        Look if time spectrum is in range
         tgesp(tge,geo)=tgesp(tge,geo)+1
C        Increment time spectrum
        END IF
       END IF

C     -----------------------------------Ge 2------------------

       IF (btest(m(0,1),6)) THEN
C       Look if Ge 2 has signal
        ege=INT((REAL(m(1,6))+r)*gainge(1)+shiftge(1)+0.5)
C       Get channel of Ge 2 and align spectra
        IF ((ege.LT.0).OR.(ege.GT.4095)) ege=0
C       Look if Ge 2 spectrum is in range
        geo=1
        IF ((pu.EQ..TRUE.).AND.(btest(m(10,1),9))) geo=4
C       Look for pile up in Ge 2
        gesp(ege,geo)=gesp(ege,geo)+1
C       Increment Ge 2 spectrum
        IF (btest(m(0,1),10)) THEN
C        Look for Ge time spectrum
         tge=m(1,10)/2+shifttge(1)
C        Get channel of Ge time and align spectra
         IF ((tge.LT.0).OR.(tge.GT.511)) tge=0
C        Look if time spectrum is in range
         tgesp(tge,geo)=tgesp(tge,geo)+1
C        Increment time spectrum
        END IF
       END IF

C     -----------------------------------Ge 3------------------

       IF (btest(m(0,1),8)) THEN
C       Look if Ge 3 has signal
        ege=INT((REAL(m(1,8))+r)*gainge(2)+shiftge(2)+0.5)
C       Get channel of Ge 3 and align spectra
        IF ((ege.LT.0).OR.(ege.GT.4095)) ege=0
C       Look if Ge 3 spectrum is in range
        geo=2
        IF ((pu.EQ..TRUE.).AND.(btest(m(10,1),10))) geo=5
C       Look for pile up in Ge 3
        gesp(ege,geo)=gesp(ege,geo)+1
C       Increment Ge 3 spectrum
        IF (btest(m(0,1),10)) THEN
C        Look for Ge time spectrum
         tge=m(1,10)/2+shifttge(2)
C        Get channel of Ge time and align spectra
         IF ((tge.LT.0).OR.(tge.GT.511)) tge=0
C        Look if time spectrum is in range
         tgesp(tge,geo)=tgesp(tge,geo)+1
C        Increment time spectrum
        END IF
       END IF

C     ---------------------TPU 2-------------------------------

C     --------------------------NaI(Tl)------------------------

       IF (m(3,0).EQ.1) THEN
C      Look if TPU 2 has signal
        DO 200,i=0,15
         IF (btest(m(3,1),i)) THEN
C         Look if individual channel i has signal
          ena=INT((REAL(m(4,i))+r)*gainna(i)/2.0+shiftna(i)+0.5)
C         Get channel of NaI i and align spectra
          IF ((ena.LT.0).OR.(ena.GT.2047)) ena=0
C         Look if spectrum i is in range
          nasp(ena,i)=nasp(ena,i)+1   
C         Increment spectrum i
          tna=m(5,i)/8+shifttna(i)
C         Get channel of NaI i time and align spectra
          IF ((tna.LT.0).OR.(tna.GT.511)) tna=0
C         Look if time spectrum i is in range
          tnasp(tna,i)=tnasp(tna,i)+1
C         Increment time spectrum i
         END IF
 200    CONTINUE   
       END IF

C     ---------------------TPU 3-------------------------------

C     --------------------------NaI(Tl)------------------------

       IF (m(6,0).EQ.1) THEN
C       Look if TPU 3 has signal
        DO 300,i=0,15
         j=i+16
C        Calculate true channel number j
         IF (btest(m(6,1),i)) THEN
C         Look if individual channel j has signal
          ena=INT((REAL(m(7,i))+r)*gainna(j)/2.0+shiftna(j)+0.5)
C         Get channel of NaI j and align spectra
          IF ((ena.LT.0).OR.(ena.GT.2047)) ena=0
C         Look if spectrum j is in range
          nasp(ena,j)=nasp(ena,j)+1
C         Increment spectrum j
          tna=m(8,i)/8+shifttna(j)
C         Get channel of NaI j time and align spectra
          IF ((tna.LT.0).OR.(tna.GT.511)) tna=0
C         Look if time spectrum j is in range
          tnasp(tna,j)=tnasp(tna,j)+1
C         Increment time spectrum j
         END IF
 300    CONTINUE   
       END IF 
      END IF

C     ---------------------END---------------------------------

      RETURN
      END

