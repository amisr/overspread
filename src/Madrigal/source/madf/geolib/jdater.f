C     $Id: jdater.f,v 1.1 2003/01/08 18:18:24 brideout Exp $
C
      SUBROUTINE JDATER(JDAYNO,DAY,MONTH,YEAR,IER)
C
C     Returns DAY, MONTH, YEAR from Julian day (See JDAY for inverse).
C
C      Input:
C        JDAYNO - Julian day (e.g. 2447892)
C
C      Output:
C           DAY - Day of month (1-31)
C         MONTH - Month of year (1-12)
C          YEAR - Year (e.g. 1977)
C           IER - If (IER.NE.0) an error has occurred.
C
C     .. Scalar Arguments ..
      INTEGER DAY,IER,JDAYNO,MONTH,YEAR
C     ..
C     .. Local Scalars ..
      INTEGER J
C     ..
      IF (JDAYNO.LT.0) GO TO 20
      J = JDAYNO - 1721119
      YEAR = (4*J-1)/146097
      J = 4*J - 1 - 146097*YEAR
      DAY = J/4
      J = (4*DAY+3)/1461
      DAY = 4*DAY + 3 - 1461*J
      DAY = (DAY+4)/4
      MONTH = (5*DAY-3)/153
      DAY = 5*DAY - 3 - 153*MONTH
      DAY = (DAY+5)/5
      YEAR = 100*YEAR + J
      IER = 0
      IF (MONTH.GE.10) GO TO 10
      MONTH = MONTH + 3
      RETURN
   10 MONTH = MONTH - 9
      YEAR = YEAR + 1
      RETURN
   20 IER = 1
      RETURN
C
      END
