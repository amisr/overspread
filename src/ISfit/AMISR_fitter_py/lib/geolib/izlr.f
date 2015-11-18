C     $Id: izlr.f,v 1.1 2003/01/08 18:30:47 brideout Exp $
C
      SUBROUTINE IZLR(DAY,MONTH,YEAR,WDAY,IER)
C
C     Returns day-of-the-week value from DAY, MONTH and YEAR.
C
C      Input:
C           DAY - Day of month (1-31)
C         MONTH - Month of year (1-12)
C          YEAR - Year (e.g. 1977)
C
C      Output:
C          WDAY - Day of week (1-7)
C           IER - If (IER.NE.0) an error has occurred.
C
C     .. Scalar Arguments ..
      INTEGER DAY,IER,MONTH,WDAY,YEAR
C     ..
C     .. Local Scalars ..
      INTEGER II,JJ,KK,L
C     ..
C     .. External Functions ..
      INTEGER IDMYCK
      EXTERNAL IDMYCK
C     ..
      IER = IDMYCK(DAY,MONTH,YEAR)
      IF (IER.EQ.1) RETURN
      II = (13*(MONTH+10-(MONTH+10)/13*12)-1)/5 + DAY + 77
      L = 5*(YEAR+(MONTH-14)/12-(YEAR+(MONTH-14)/12)/100*100)
      JJ = L/4
      KK = (YEAR+(MONTH-14)/12)/400 - (YEAR+(MONTH-14)/12)/100*2
      L = II + JJ + KK
      II = L/7
      JJ = II*7
      WDAY = L - JJ + 1
      RETURN
C
      END
