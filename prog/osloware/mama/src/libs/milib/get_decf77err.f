C$PROG GET_DECF77ERR
C
      subroutine GET_DECF77ERR(ierr,msg)
C
C    For DEC fortran for mips risc machines version 3.0    JRB 1/93
C    Should replace as soon as DEC provides a way to access their
C    message catalog.
C
      character*80 fmsg(400), dummy
      character*80 msg
      data dummy/'        '/
      data fmsg(  1)/'Not a FORTRAN-specific error'/
      data fmsg(  9)/'Permission to access file denied'/
      data fmsg( 10)/'Cannot overwrite existing file'/
      data fmsg( 11)/'Unit not connected'/
      data fmsg( 17)/'Syntax error in NAMELIST input'/
      data fmsg( 18)/'Too many values for NAMELIST variable'/
      data fmsg( 19)/'Invalid reference to variable in NAMELIST input'/
      data fmsg( 20)/'REWIND error'/
      data fmsg( 21)/'Duplicate file specifications'/
      data fmsg( 22)/'Input record too long'/
      data fmsg( 23)/'BACKSPACE error'/
      data fmsg( 24)/'End-of-file during read'/
      data fmsg( 25)/'Record number outside range'/
      data fmsg( 26)/'OPEN or DEFINE FILE required'/
      data fmsg( 27)/'Too many records in I/O statement'/
      data fmsg( 28)/'CLOSE error'/
      data fmsg( 29)/'File not found'/
      data fmsg( 30)/'Open failure'/
      data fmsg( 31)/'Mixed file access modes'/
      data fmsg( 32)/'Invalid logical unit number'/
      data fmsg( 33)/'ENDFILE error'/
      data fmsg( 34)/'Unit already open'/
      data fmsg( 35)/'Segmented record format error'/
      data fmsg( 36)/'Attempt to access non-existent record'/
      data fmsg( 37)/'Inconsistent record length'/
      data fmsg( 38)/'Error during write'/
      data fmsg( 39)/'Error during read'/
      data fmsg( 40)/'Recursive I/O operation'/
      data fmsg( 41)/'Insufficient virtual memory'/
      data fmsg( 42)/'No such device'/
      data fmsg( 43)/'File name specification error'/
      data fmsg( 44)/'Inconsistent record type'/
      data fmsg( 45)/'Keyword value error in OPEN statement'/
      data fmsg( 46)/'Inconsistent OPEN/CLOSE parameters'/
      data fmsg( 47)/'Write to READONLY file'/
      data fmsg( 48)/'Invalid argument to FORTRAN Run-Time Library'/
      data fmsg( 51)/'Inconsistent file organization'/
      data fmsg( 55)/'DELETE error'/
      data fmsg( 57)/'FIND error'/
      data fmsg( 59)/'List-directed I/O syntax error'/
      data fmsg( 60)/'Infinite format loop'/
      data fmsg( 61)/'Format/variable-type mismatch'/
      data fmsg( 62)/'Syntax error in format'/
      data fmsg( 63)/'Output conversion error'/
      data fmsg( 64)/'Input conversion error'/
      data fmsg( 65)/'Floating invalid'/
      data fmsg( 66)/'Output statement overflows record'/
      data fmsg( 67)/'Input statement requires too much data'/
      data fmsg( 68)/'Variable format expression value error'/
      data fmsg( 69)/'Process interrupted (SIGINT)'/
      data fmsg( 70)/'Integer overflow'/
      data fmsg( 71)/'Integer zero divide'/
      data fmsg( 72)/'Floating overflow'/
      data fmsg( 73)/'Floating divide by zero'/
      data fmsg( 74)/'Floating underflow'/
      data fmsg( 75)/'Floating point exception'/
      data fmsg( 76)/'IOT trap signal'/
      data fmsg( 77)/'Subscript out of range'/
      data fmsg( 78)/'Process killed (SIGTERM)'/
      data fmsg( 79)/'Process quit (SIGQUIT)'/
      data fmsg( 80)/'Wrong number of argments'/
      data fmsg( 81)/'Invalid argument to math library'/
      data fmsg( 82)/'Undefined exponentiation'/
      data fmsg( 83)/'Logarithm of zero or negative value'/
      data fmsg( 84)/'Square root of negative value'/
      data fmsg( 87)/'Significance lost in math library'/
      data fmsg( 88)/'Floating overflow in math library'/
      data fmsg( 89)/'Floating underflow in math library'/
      data fmsg( 93)/'Adjustable array dimension error'/
      data fmsg(108)/'Cannot stat file'/
      data fmsg(120)/'Operation requires seek ability'/
      data fmsg(265)/'FORTRAN abort routine called'/
      data fmsg(297)/'nn floating invalid traps'/
      data fmsg(298)/'nn floating overflow traps'/
      data fmsg(299)/'nn floating divide-by-zero traps'/
      data fmsg(300)/'nn floating underflow traps'/
      if (icall.eq.0)then
          do i=1,300
             if(fmsg(i)(1:1).lt.'A'.or.fmsg(i)(1:1).gt.'z')then
                  fmsg(i)='   '
             endif
             icall=1
          enddo
      endif
           
      if(ierr.lt.1.or.ierr.gt.300)then
           write(msg,'(''f77ERRNO '',i3,'':: '',a)')ierr,dummy(1:60)
      else
           write(msg,'(''f77ERRNO '',i3,'':: '',a)')
     *            ierr,fmsg(ierr)(1:60)
      endif
      return
      end
