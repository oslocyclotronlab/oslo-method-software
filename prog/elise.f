      character*4 a3
      character*2 a1,nucs
      character*2 nuc
      character*11 a2
      character*10 c
      character*16 d,din
      character*1 f1
      character*3 at
      character*30 f2,filee
      character filnam*255

      iop = 0
      ia = 0
      iz = 0
      call mass(iop,ia,iz,xbab)
      iop = 1


      write(6, 123)
123   format('Type Emax (MeV):')
      read(5, *) emax
      if(emax.eq.0.) emax = 10.

300   continue
      write(6, 120)
120   format('Type A,Z:')
      read(5,*)ia,iz

      open(unit=21,file='toi.dao')
      open(unit=20,file='level.tmp')   

      call mass(iop,ia,iz,xb)
      call mass(iop,ia-1,iz,xbn)
      call mass(iop,ia-1,iz-1,xbp)

      call mass(iop,ia+1,iz,xbnu)
      call mass(iop,ia+1,iz+1,xbpu)

      call mass(iop,ia-2,iz,xb2n)
      call mass(iop,ia-2,iz-2,xb2p)

      call mass(iop,ia+2,iz,xb2nu)
      call mass(iop,ia+2,iz+2,xb2pu)

      call mass(iop,ia-4,iz-2,xba)
      call mass(iop,ia+4,iz+2,xbau)
      call mass(iop,4,2,xb4)
      ibn = 0
      ibp = 0
      ib2n = 0
      ib2p = 0
      iba = 0
      if(xbn.ne.0.) xbn = xb-xbn
      if(xbp.ne.0.) xbp = xb-xbp
      if(xbnu.ne.0.) xbnu = xb-xbnu
      if(xbpu.ne.0.) xbpu = xb-xbpu
      if(xb2n.ne.0.) xb2n = xb-xb2n
      if(xb2p.ne.0.) xb2p = xb-xb2p
      if(xb2nu.ne.0.) xb2nu = xb-xb2nu
      if(xb2pu.ne.0.) xb2pu = xb-xb2pu
      if(xba.ne.0.) xba = xb-xba-xb4
      if(xbau.ne.0.) xbau = xb-xbau+xb4

      write(6, 180) xb
      write(21,180) xb
180   format('          Energy T   J    BE = ,'f10.3,' MeV',/)

      nuc = ' '
      call anuc(iz,nuc)



      if(ia.ge.1.and.ia.le.50) filee  = 'ens-1-50.dat'
      if(ia.ge.51.and.ia.le.100) filee = 'ens-51-100.dat'
      if(ia.ge.101.and.ia.le.150) filee = 'ens-101-150.dat'
      if(ia.ge.151.and.ia.le.200) filee = 'ens-151-200.dat'
      if(ia.ge.201.and.ia.le.293) filee = 'ens-201-293.dat'
      if(ia.gt.293) stop
c      type 7777,filee
c7777  format(1x,a30)


      call makepath("UIO_APPLICATIONS","prog/lib/elise09/"//filee,filnam)
      WRITE(6,*)'Reading file '//filnam

      open(unit=10,file=filnam,status='old',err=702)
      go to 701
702   continue
      write(6, 703) filee
703   format(1x,a30,' file not found')
      stop
701   continue




      if(nuc.eq.' ') stop
c      nuc='10B '
100   continue
      read(10,103,end=200) iai,a1,a2
103   format(i3,a2,a11)
c      type 102,a
      if(iai.gt.ia) go to 200
      if(iai.eq.ia.and.a1.eq.nuc.and.a2.eq.'    ADOPTED') go to 101
      go to 100
101   continue


      nucs = ' '
      k = 0
105   continue
      read(10,104,end=200) iai,a1,a3,c,din
104   format(i3,a2,a4,a10,2x,a16)
      if(iai.gt.ia) go to 200
      if(nucs.ne.' '.and.a1.ne.nuc) go to 200

      if(iai.eq.ia.and.a1.eq.nuc.and.a3.eq.'  L ') go to 130
      go to 105
130   continue
      nucs = nuc
      at = ' '
      read(10,1041,end=200) f1,f2
1041  format(5x,a1,3x,a30)
      if(f1.eq.'2') go to 1042
      go to 1043
1042  continue
      do 1044 i = 1,28
      if(f2(i:i).eq.'I'.and.f2(i+1:i+1).eq.'N'.and.f2(i+2:i+2).eq.'=')
     1 go to 1045
1044  continue
      go to 1043
1045  continue
      imin = i+3
      at(1:1) = f2(imin:imin)
      at(2:2) = f2(imin+1:imin+1)
      at(3:3) = f2(imin+2:imin+2)
      do 1048 i = 1,3
      if(at(i:i).eq.'$') at (i:i) = ' ' 
      if(at(i:i).eq.'W') at (i:i) = ' ' 
1048  continue
1043  continue
      backspace(unit=10)
      ir = 0
      do 131 i = 1,10
131   if(c(i:i).eq.'.') ir = 1
      write(20,150) c
150   format(a10)
      backspace(unit=20)
c      type 7777,c,ir
c7777  format(1x,a10,1x,i3)
      xc = 99999.
      if(ir.eq.0) read(20,*,err=170) xc
      if(ir.eq.1) read(20,*,err=170) ixc
      if(ir.eq.1) xc = ixc
      xc = xc/1000.
      if(xc.gt.emax) go to 105

      if(xc.gt.xba.and.iba.eq.0) write(6,720) xba
      if(xc.gt.xba.and.iba.eq.0) write(21,720) xba
720   format('S-alpha= ',f7.3,
     1' ---------------------------------------------')
      if(xc.gt.xba.and.iba.eq.0) iba = 1

      if(xc.gt.xbp.and.ibp.eq.0) write(6,721) xbp
      if(xc.gt.xbp.and.ibp.eq.0) write(21,721) xbp
721   format('S-p    = ',f7.3,
     1' ---------------------------------------------')
      if(xc.gt.xbp.and.ibp.eq.0) ibp = 1

      if(xc.gt.xbn.and.ibn.eq.0) write(6, 722) xbn
      if(xc.gt.xbn.and.ibn.eq.0) write(21,722) xbn
722   format('S-n    = ',f7.3,
     1' ---------------------------------------------')
      if(xc.gt.xbn.and.ibn.eq.0) ibn = 1

      if(xc.gt.xb2p.and.ib2p.eq.0) write(6, 723) xb2p
      if(xc.gt.xb2p.and.ib2p.eq.0) write(21,723) xb2p
723   format('S-2p   = ',f7.3,
     1' ---------------------------------------------')
      if(xc.gt.xb2p.and.ib2p.eq.0) ib2p = 1

      if(xc.gt.xb2n.and.ib2n.eq.0) write(6, 724) xb2n
      if(xc.gt.xb2n.and.ib2n.eq.0) write(21,724) xb2n
724   format('S-2n   = ',f7.3,
     1' ---------------------------------------------')
      if(xc.gt.xb2n.and.ib2n.eq.0) ib2n = 9999.




      d = ' '
      do 1335 i = 1,16
      if(din(i:i).ne.' ') go to 1336
1335  continue
1336  continue
      imin = i
      do 1337 i = imin,16
      ii = i-imin+1
1337  d(ii:ii) = din(i:i)
      k = k + 1
      do 133 i = 1,16
      if(d(i:i).eq.'(') go to 145
133   continue
      do 1333 i = 1,16
      if(d(i:i).eq.'-') go to 147
1333  continue
      do 1334 i = 1,16
      if(d(i:i).eq.'+') go to 132
1334  continue
      go to 145
147   continue
c      type 7777,d
c7777  format(a16)
      do 148 i = 1,16
      if(d(i:i).eq.'+') go to 145
148   continue
      go to 140

c      do 131 i = 1,10
c131   continue
      go to 145

132   continue
      write(6, 102) iai,a1,k,xc,at,d
      write(21,102) iai,a1,k,xc,at,d
      go to 105
102   format(i3,a2,1x,i3,f7.3,1x,a3,1x,a16)

140   continue
      write(6, 141) iai,a1,k,xc,at,d
      write(21,141) iai,a1,k,xc,at,d
141   format(i3,a2,1x,i3,15x,f10.3,1x,a3,1x,a16)
      go to 105

145   continue
      write(6, 146) iai,a1,k,xc,at,d
      write(21,146) iai,a1,k,xc,at,d
146   format(i3,a2,1x,i3,33x,f10.3,1x,a3,1x,a16)
      go to 105


170   continue
      write(6, 171) iai,a1,k,c,at,d
      write(21,171) iai,a1,k,c,at,d
171   format(i3,a2,1x,i3,33x,a10,1x,a3,1x,a16)
      go to 105


200   continue

      write(6, 1720)
1720  format(/)
      write(6, 721) xbp
      write(6, 722) xbn
      write(6, 723) xb2p
      write(6, 724) xb2n
      write(6, 720) xba
      write(6, 725) xbpu
725   format(/,'S+p       = ',f7.3)
      write(6, 726) xbnu
726   format('S+n       = ',f7.3)
      write(6, 1725) xb2pu
1725  format('S+2p      = ',f7.3)
      write(6, 1726) xb2nu
1726  format('S+2n      = ',f7.3)
      write(6, 2726) xbau
2726  format('S+alpha   = ',f7.3)
      gapp = xbp+xbpu
      gapn = xbn+xbnu
      gap2p = xb2p+xb2pu
      gap2n = xb2n+xb2nu
      gapa = xba+xbau
      write(6, 727) gapp
727   format(/,'gap p     = ',f7.3)
      write(6, 728) gapn
728   format('gap n     = ',f7.3)
      write(6, 1727) gap2p
1727  format('gap 2p    = ',f7.3)
      write(6, 1728) gap2n
1728  format('gap 2n    = ',f7.3)
      write(6, 2728) gapa
2728  format('gap alpha = ',f7.3)


      write(21,1720)
      write(21,721) xbp
      write(21,722) xbn
      write(21,723) xb2p
      write(21,724) xb2n
      write(21,720) xba
      write(21,725) xbpu
      write(21,726) xbnu
      write(21,1725) xb2pu
      write(21,1726) xb2nu
      write(21,2726) xbau
      write(21,727) gapp
      write(21,728) gapp
      write(21,1727) gap2p
      write(21,1728) gap2n
      write(21,2728) gapa



      close(unit=10)
      close(unit=21)
      close(unit=20,STATUS='DELETE')
      go to 300
      end

      subroutine anuc(iz,a)
      character*2 a
      if(iz.eq.1) a = 'H '
      if(iz.eq.2) a = 'HE'
      if(iz.eq.3) a = 'LI'
      if(iz.eq.4) a = 'BE'
      if(iz.eq.5) a = 'B '
      if(iz.eq.6) a = 'C '
      if(iz.eq.7) a = 'N '
      if(iz.eq.8) a = 'O '
      if(iz.eq.9) a = 'F '
      if(iz.eq.10) a = 'NE'
      if(iz.eq.11) a = 'NA'
      if(iz.eq.12) a = 'MG'
      if(iz.eq.13) a = 'AL'
      if(iz.eq.14) a = 'SI'
      if(iz.eq.15) a = 'P '
      if(iz.eq.16) a = 'S '
      if(iz.eq.17) a = 'CL'
      if(iz.eq.18) a = 'AR'
      if(iz.eq.19) a = 'K '
      if(iz.eq.20) a = 'CA'
      if(iz.eq.21) a = 'SC'
      if(iz.eq.22) a = 'TI'
      if(iz.eq.23) a = 'V '
      if(iz.eq.24) a = 'CR'
      if(iz.eq.25) a = 'MN'
      if(iz.eq.26) a = 'FE'
      if(iz.eq.27) a = 'CO'
      if(iz.eq.28) a = 'NI'
      if(iz.eq.29) a = 'CU'
      if(iz.eq.30) a = 'ZN'
      if(iz.eq.31) a = 'GA'
      if(iz.eq.32) a = 'GE'
      if(iz.eq.33) a = 'AS'
      if(iz.eq.34) a = 'SE'
      if(iz.eq.35) a = 'BR'
      if(iz.eq.36) a = 'KR'
      if(iz.eq.37) a = 'RB'
      if(iz.eq.38) a = 'SR'
      if(iz.eq.39) a = 'Y '
      if(iz.eq.40) a = 'ZR'
      if(iz.eq.41) a = 'NB'
      if(iz.eq.42) a = 'MO'
      if(iz.eq.43) a = 'TC'
      if(iz.eq.44) a = 'RU'
      if(iz.eq.45) a = 'RH'
      if(iz.eq.46) a = 'PD'
      if(iz.eq.47) a = 'AG'
      if(iz.eq.48) a = 'CD'
      if(iz.eq.49) a = 'IN'
      if(iz.eq.50) a = 'SN'
      if(iz.eq.51) a = 'SB'
      if(iz.eq.52) a = 'TE'
      if(iz.eq.53) a = 'I '
      if(iz.eq.54) a = 'XE'
      if(iz.eq.55) a = 'CS'
      if(iz.eq.56) a = 'BA'
      if(iz.eq.57) a = 'LA'
      if(iz.eq.58) a = 'CE'
      if(iz.eq.59) a = 'PR'
      if(iz.eq.60) a = 'ND'
      if(iz.eq.61) a = 'PM'
      if(iz.eq.62) a = 'SM'
      if(iz.eq.63) a = 'EU'
      if(iz.eq.64) a = 'GD'
      if(iz.eq.65) a = 'TB'
      if(iz.eq.66) a = 'DY'
      if(iz.eq.67) a = 'HO'
      if(iz.eq.68) a = 'ER'
      if(iz.eq.69) a = 'TM'
      if(iz.eq.70) a = 'YB'
      if(iz.eq.71) a = 'LU'
      if(iz.eq.72) a = 'HF'
      if(iz.eq.73) a = 'TA'
      if(iz.eq.74) a = 'W '
      if(iz.eq.75) a = 'RE'
      if(iz.eq.76) a = 'OS'
      if(iz.eq.77) a = 'IR'
      if(iz.eq.78) a = 'PT'
      if(iz.eq.79) a = 'AU'
      if(iz.eq.80) a = 'HG'
      if(iz.eq.81) a = 'TL'
      if(iz.eq.82) a = 'PB'
      if(iz.eq.83) a = 'BI'
      if(iz.eq.84) a = 'PO'
      if(iz.eq.85) a = 'AT'
      if(iz.eq.86) a = 'RN'
      if(iz.eq.87) a = 'FR'
      if(iz.eq.88) a = 'RA'
      if(iz.eq.89) a = 'AC'
      if(iz.eq.90) a = 'TH'
      if(iz.eq.91) a = 'PA'
      if(iz.eq.92) a = 'U '
      if(iz.eq.93) a = 'NP'
      if(iz.eq.94) a = 'PU'
      if(iz.eq.95) a = 'AM'
      if(iz.eq.96) a = 'CM'
      if(iz.eq.97) a = 'BK'
      if(iz.eq.98) a = 'CF'
      if(iz.eq.99) a = 'ES'
      if(iz.eq.100) a = 'FM'
      if(iz.eq.101) a = 'MD'
      if(iz.eq.102) a = 'NO'
      if(iz.eq.103) a = 'LR'
      if(iz.eq.104) a = 'RF'
      return
      end

      subroutine mass(iop,ia,iz,xbab)
      dimension be(200,200),bee(200,200)
      character filnam*255
      if(iop.eq.0) go to 500
      if(iop.eq.1) go to 501
500   continue
c zero the arrays
      do 90 in = 1,200
      do 90 iz = 1,200
      be(in,iz) = 0.
90    bee(in,iz) = 0.
  
c read the data
      call makepath("UIO_APPLICATIONS","prog/lib/elise09/aud03.dat",filnam)
      WRITE(6,*)'Reading file '//filnam

      open(unit=10,file=filnam,status='old',err=702)
      go to 701
702   continue
      write(6, 703)
703   format(1x,'aud03.dats file not found')
      stop
701   continue

 


      read(10,*)
      read(10,*)
      read(10,*)
      read(10,*)
      read(10,*)
      read(10,*)
100   continue
      read(10,*,end=200) iz,ia,x1,x2,i1,in
      be(iz,in)  = x1/1000.
      bee(iz,in) = x2/1000.
      go to 100
200   continue
      close(unit=10)
      return

501   continue
      in = ia-iz
      xbab = be(iz,in)
      return
      end

