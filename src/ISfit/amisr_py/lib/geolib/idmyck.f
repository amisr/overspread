C     $Id: idmyck.f,v 1.1 2003/01/08 18:59:15 brideout Exp $
C
      INTEGER FUNCTION IDMYCK(DAY,MONTH,YEAR)
C
C     Returns 1 if DAY, MONTH and YEAR are legal values, 0 otherwise.
C
C      Input:
C           DAY - Day of month (1-31)
C         MONTH - Month of year (1-12)
C          YEAR - Year (e.g. 1977)
C
C     .. Scalar Arguments ..
      INTEGER DAY,MONTH,YEAR
C     ..
C     .. Local Scalars ..
      INTEGER L
C     ..
C     .. Local Arrays ..
      INTEGER NUMDAY(12)
C     ..
C     .. Data statements ..
      DATA NUMDAY/31,28,31,30,31,30,31,31,30,31,30,31/
C     ..
C
      IDMYCK = 0
      IF (DAY.LT.1 .OR. MONTH.LT.1 .OR. MONTH.GT.12 .OR. YEAR.LT.0 .OR.
     *    YEAR.GT.9999) GO TO 10
      L = 0
      IF (MONTH.EQ.2 .AND. YEAR/4*4.EQ.YEAR .AND.
     *    (YEAR/400*400.EQ.YEAR.OR.YEAR/100*100.NE.YEAR)) L = 1
      IF (DAY.GT.NUMDAY(MONTH)+L) GO TO 10
      GO TO 20
   10 IDMYCK = 1
   20 RETURN
C
      END
