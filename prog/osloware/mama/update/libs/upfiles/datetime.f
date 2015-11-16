
       SUBROUTINE DATETIME(DATTIM)
       CHARACTER*24 ctime, DATTIM, string
       INTEGER n, time
       n=time()
       string=ctime(n)
       DATTIM(1:10)=string(9:10)//string(5:7)//string(12:16)
       DATTIM(11:20)='          '
       RETURN
       END
