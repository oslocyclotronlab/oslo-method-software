      SUBROUTINE gain_calc( goalp, adfac, telescopeno, esp, centroid )
C ==============================================================
C                for each E-counter. Exctract the gain adjust-   
C               ment factors, adfac(0-63)                       
C                                                             
C                                                             
C     INPUT  via parameters  :                           
C     OUTPUT via parameters  :                         
C                                                                          

C ==============================================================%
      IMPLICIT NONE

      INCLUDE 'common_defs.decl'
      INCLUDE 'spec_params.decl'      

      integer teleno,j,k,low,high,temp,chan,max,i1,i2,istep 
      integer telescopeno			! Number of particle telescopes 8/64
      real goalp(0:63)				! The goal peak values, read from file
      real adfac(0:63)				! Gain adjustment factors, returned from gaincalc
      real centroid(0:63)
      real count,weigthcount,sumcount,sumweigthcount 
   

      do teleno = 0, telescopeno-1                           
         
C --     Find peak search limits           
         low  = goalp(teleno) - 400 
         high = goalp(teleno) + 400
         IF (low .LT. 0) low = 10                !to avoid spikes in ch 0 (Magne)
         IF (high .GT. 2047) high = 2047

C --     Find highest peak within limits
         max = 0
         do j = low, high
            temp = esp(j,teleno)
            if (temp .GT. max) then
               max = temp
               chan = j
            end if    
         end do 

C Find low/high marker for centroid-calculations (before istep=50) (Magne)
         low  = chan - 50
         high = chan + 50
         if (low .LT. 0) low = 0
         if (high .GT. 2047) high = 2047
         do j=low,chan
            if(esp(j,teleno).LT.max/2)i1=j
         enddo
         do j=chan,high
            if(esp(j,teleno).GT.max/2)i2=j
         enddo
         istep=INT((FLOAT(i2-i1)*1.5)+0.5)
         IF(istep.LT. 1)istep=1
         IF(istep.GT.50)istep=50  

C --     Calculate centroid of peak 
         low  = chan - istep
         high = chan + istep
         if (low .LT. 0) low = 0
         if (high .GT. 2047) high = 2047 
         sumcount = 0.0
         sumweigthcount = 0.0

         do k = low, high
            count = FLOAT (esp(k,teleno) )
            weigthcount = count * k
            sumcount = sumcount + count
            sumweigthcount = sumweigthcount + weigthcount
         end do 
         if (sumcount .GT. 0.0) then
            centroid(teleno) = sumweigthcount / sumcount
         else
            centroid(teleno) = 0.0
         end if

C --     Calculate gain adjustment factor based on goal peak value 
         if (centroid(teleno) .GT. 0.0) then
            adfac(teleno) =  goalp(teleno) / centroid(teleno)
         else
            adfac(teleno) = 1.0
         end if

      end do
 
      return
      end          ! End-Of-Subroutine gain_calc
