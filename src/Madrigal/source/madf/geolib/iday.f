C     $Id: iday.f,v 1.1 2003/01/08 18:30:00 brideout Exp $
C
      SUBROUTINE IDAY(DAY,MONTH,YEAR,DAYNO,IER)
C
C     Returns Day-of-year from DAY, MONTH, YEAR.
C
C      Input:
C        IDBFIL - Madrigal file number (see EXDCON)
C           DAY - Day of month (1-31)
C         MONTH - Month of year (1-12)
C          YEAR - Year (e.g. 1977)
C
C      Output:
C         DAYNO - Day-of-year (1-356)
C           IER - If (IER.NE.0) an error has occurred.
C
C     .. Scalar Arguments ..
      INTEGER DAY,DAYNO,IER,MONTH,YEAR
C     ..
C     .. Local Scalars ..
      INTEGER II,JJ,KK
C     ..
C     .. External Functions ..
      INTEGER IDMYCK
      EXTERNAL IDMYCK
C     ..
      IER = IDMYCK(DAY,MONTH,YEAR)
      IF (IER.EQ.1) RETURN
      II = 3055*(MONTH+2)/100 - (MONTH+10)/13*2 - 91
      JJ = (1-((YEAR-YEAR/4*4+3)/4)+((YEAR-YEAR/100*100+99)/100)-
     *     (YEAR-YEAR/400*400+399)/400)
      KK = (MONTH+10)/13
      DAYNO = II + JJ*KK + DAY
      RETURN
C
      END
