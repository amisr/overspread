C     $Id: monum.f,v 1.1 2003/01/08 19:17:00 brideout Exp $
C
      SUBROUTINE MONUM(MSTR,MONTH,IER)
C
C     Returns Month integer from Month string (case insignificant).
C
C      Input:
C         MSTR - Month string (e.g. 'January')
C
C      Output:
C        MONTH - Month of year (1-12)
C          IER - If (IER.NE.0) an error has occurred.
C
C     .. Scalar Arguments ..
      INTEGER IER,MONTH
      CHARACTER*(*) MSTR
C     ..
C     .. Local Scalars ..
      INTEGER ICH,JCH
      CHARACTER*75 MNAME
C     ..
C     .. Local Arrays ..
      INTEGER MCHAR(13)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC CHAR,ICHAR
C     ..
C     .. Data statements ..
      DATA MCHAR/1,8,16,21,26,29,33,37,43,52,59,67,75/
C     ..
      MNAME = 'JANUARYFEBRUARYMARCHAPRILMAYJUNEJULYAUGUSTSEPTEMBER'//
     *        'OCTOBERNOVEMBERDECEMBER'
C
      IER = 0
      DO 30 MONTH = 1,12
         ICH = 1
         JCH = MCHAR(MONTH)
         IF (MSTR(ICH:ICH).NE.MNAME(JCH:JCH) .AND.
     *       CHAR(ICHAR(MSTR(ICH:ICH))-32).NE.
     *       MNAME(JCH:JCH)) GO TO 30
   10    ICH = ICH + 1
         IF (ICH.EQ.4) GO TO 40
   20    JCH = JCH + 1
         IF (JCH.GE.MCHAR(MONTH+1)) GO TO 30
         IF (MSTR(ICH:ICH).NE.MNAME(JCH:JCH) .AND.
     *       CHAR(ICHAR(MSTR(ICH:ICH))-32).NE.MNAME(JCH:JCH)) GO TO 20
         GO TO 10
   30 CONTINUE
      MONTH = 0
      IER = 1
   40 RETURN
C
      END
