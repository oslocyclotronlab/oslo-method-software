        PROGRAM THETANGLE
C  Program to calculate angle between two vektors
        DIMENSION x1(2),x2(2),y1(2),y2(2),z1(2),z2(2)
        CHARACTER ans*1

C Setting up defaults values
        DO i=1,2
          x1(i)=0
          x2(i)=0
          y1(i)=0
          y2(i)=0
          z1(i)=0
          z2(i)=0
        ENDDO

  99    ans='y'
C Reading parameters
        i=1
        WRITE(6,300)
 300    FORMAT(/' Give coordinates for first vector')

        WRITE(6,101)x1(i)
 101    FORMAT(' Type x1 <',F10.4,'>:',$)
        CALL READF(5,x1(i))
        WRITE(6,102)y1(i)
 102    FORMAT(' Type y1 <',F10.4,'>:',$)
        CALL READF(5,y1(i))
        WRITE(6,103)z1(i)
 103    FORMAT(' Type z1 <',F10.4,'>:',$)
        CALL READF(5,z1(i))

        WRITE(6,104)x2(i)
 104    FORMAT(' Type x2 <',F10.4,'>:',$)
        CALL READF(5,x2(i))
        WRITE(6,105)y2(i)
 105    FORMAT(' Type y2 <',F10.4,'>:',$)
        CALL READF(5,y2(i))
        WRITE(6,106)z2(i)
 106    FORMAT(' Type z2 <',F10.4,'>:',$)
        CALL READF(5,z2(i))


        i=2
        WRITE(6,301)
 301    FORMAT(/' Give coordinates for second vector')

        WRITE(6,101)x1(i)
        CALL READF(5,x1(i))
        WRITE(6,102)y1(i)
        CALL READF(5,y1(i))
        WRITE(6,103)z1(i)
        CALL READF(5,z1(i))

        WRITE(6,104)x2(i)
        CALL READF(5,x2(i))
        WRITE(6,105)y2(i)
        CALL READF(5,y2(i))
        WRITE(6,106)z2(i)
        CALL READF(5,z2(i))
       

C Printing inputs
        i=1
        WRITE(6,200)
 200    FORMAT(/' Coordinates for first vector are:')

        WRITE(6,110)x1(i),y1(i),z1(i)
 110    FORMAT(' (x1,y1,z1) = (',F10.4,',',F10.4,',',F10.4,')')
        WRITE(6,111)x2(i),y2(i),z2(i)
 111    FORMAT(' (x2,y2,z2) = (',F10.4,',',F10.4,',',F10.4,')')

        i=2
        WRITE(6,201)
 201    FORMAT(/' Coordinates for second vector are:')

        WRITE(6,110)x1(i),y1(i),z1(i)
        WRITE(6,111)x2(i),y2(i),z2(i)

        WRITE(6,205)ans
 205    FORMAT(/' Are you satisfied with inputs (y/n) <',A,'>:',$)
        CALL READA1(5,ans)
        IF(ans.NE.'y')GO TO 99

C Now calculating the angle from the formula ab=|a||b|cosQ
        ax=x2(1)-x1(1)
        ay=y2(1)-y1(1)
        az=z2(1)-z1(1)
        bx=x2(2)-x1(2)
        by=y2(2)-y1(2)
        bz=z2(2)-z1(2)
        ab=ax*bx+ay*by+az*bz
        a =SQRT(ax*ax+ay*ay+az*az)
        b =SQRT(bx*bx+by*by+bz*bz)
        alpha =ab/(a*b)
        IF(alpha.GT. 1.)THEN
          write(6,*)'ACOS term was ',alpha, ', corrected to 1.00000'
          alpha= 1.
        ENDIF
        IF(alpha.LT.-1.)THEN
          write(6,*)'ACOS term was ',alpha, ', corrected to -1.00000'
          alpha=-1.
        ENDIF
        theta=ACOS(alpha)
        write(6,*)'ab=',ab,'  |a|=',a,'  |b|=',b,'  radtheta=',theta

        theta=(180./3.141592654)*theta

        WRITE(6,500)theta
 500    FORMAT(' Angle (dgr.) between vectors is: ',F8.3)

        ans='y'
        WRITE(6,501)ans
 501    FORMAT(/' New calculation (y/n) <',A,'>:',$)
        CALL READA1(5,ans)
        IF(ans.EQ.'y')GO TO 99

        END

