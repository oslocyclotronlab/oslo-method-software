      PROGRAM SIRIANGLE
      INTEGER i,j,k
      REAL h,w,alpha,L,xpi

      WRITE(6,*)' ________________________________'
      WRITE(6,*)'|                                |'
      WRITE(6,*)'|          SIRIANGLE 1.0         |'
      WRITE(6,*)'|                                |'
      WRITE(6,*)'| Program to calculate positions |'
      WRITE(6,*)'|   of stripes on SIRI trapez    |'
      WRITE(6,*)'|   h and w are the distance     |'
      WRITE(6,*)'|    from the point where the    |'
      WRITE(6,*)'|   ejectile hits perpendicular  |'
      WRITE(6,*)'|     on the trapez after a      |'
      WRITE(6,*)'|    scattered angle of alpha    |'
      WRITE(6,*)'|   L = target trapez distance   |'
      WRITE(6,*)'|                                |'
      WRITE(6,*)'|   Oslo Cyclotron Laboratory    |'
      WRITE(6,*)'|       Magne Guttormsen         |'
      WRITE(6,*)'|                                |'
      WRITE(6,*)'|     Created: June 11th 2005    |'
      WRITE(6,*)'|________________________________|'
      WRITE(6,*)' '

      xpi      = 3.1415926
      alphadgr = 47.
      alpha    = alphadgr*((2.*xpi)/360.)
      L        = 50.
      i1       = 39
      i2       = 55
      WRITE(6,*)' '
      WRITE(6,*)'Alpha(dgr)          = ',alphadgr
      WRITE(6,*)'L(mm)               = ',L
      WRITE(6,*)'Theta1, Theta2(dgr) = ',i1,i2
      WRITE(6,*)' '

      DO i=i1,i2
         theta=FLOAT(i)
         w=-1.
         DO j=0,15
            w=w+1.
            h=-15.
            diffold=1000.       !best difference to a given theta
            DO k=0,30000
               h=h+0.001
               theta=FLOAT(i)
               CALL Angle(xtheta,L,alpha,w,h)
               diff=ABS(theta-xtheta)
               IF(diff.LT.diffold)THEN
                  besttheta=xtheta
                  besth=h
                  diffold=diff
               ENDIF
            ENDDO
            WRITE(6,2)theta,besttheta,w,besth
 2          FORMAT('Theta=',F7.3,'(',F7.3,')','   w=',F6.2,'   h=',F6.2)
         ENDDO
      ENDDO
      END

      SUBROUTINE Angle(xtheta,L,alpha,w,h)
      REAL h,w,alpha,L,A,B,x,y, xpi
      xpi = 3.1415926
      x = sin(alpha)
      y = cos(alpha)
      A = SQRT((L*x + h*y)**2 + w**2)
      B = L*y - h*x
      xtheta = (atan(A/B))*(360./(2.*xpi))
      RETURN
      END

