C     $Id: jday.f,v 1.1 2003/01/08 18:29:41 brideout Exp $
C
      SUBROUTINE JDAY(DAY,MONTH,YEAR,JDAYNO,IER)
C
C     Returns Julian day from DAY, MONTH, YEAR (See JDATER for
C     inverse).
C
C      Input:
C           DAY - Day of month (1-31)
C         MONTH - Month of year (1-12)
C          YEAR - Year (e.g. 1977)
C
C      Output:
C        JDAYNO - Julian day (e.g. 2447892)
C           IER - If (IER.NE.0) an error has occurred.
C
C     .. Scalar Arguments ..
      INTEGER DAY,IER,JDAYNO,MONTH,YEAR
C     ..
C     .. Local Scalars ..
      INTEGER C,M,Y,YA
C     ..
C     .. External Functions ..
      INTEGER IDMYCK
      EXTERNAL IDMYCK
C     ..
      IER = IDMYCK(DAY,MONTH,YEAR)
      IF (IER.EQ.1) RETURN
      M = MONTH
      IF (M.LE.2) GO TO 10
      M = MONTH - 3
      Y = YEAR
      GO TO 20
   10 M = MONTH + 9
      Y = YEAR - 1
   20 C = Y/100
      YA = Y - 100*C
      JDAYNO = (146097*C)/4 + (1461*YA)/4 + (153*M+2)/5 + DAY + 1721119
      RETURN
C
      END
