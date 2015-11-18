C     $Id: moname.f,v 1.1 2003/01/08 18:30:21 brideout Exp $
C
      SUBROUTINE MONAME(MONTH,MSTR,IER)
C
C     Returns Month string from Month integer.
C
C      Input:
C        MONTH - Month of year (1-12)
C
C      Output:
C         MSTR - Month string (e.g. 'JANUARY')
C          IER - If (IER.NE.0) an error has occurred.
C
C     .. Scalar Arguments ..
      INTEGER IER,MONTH
      CHARACTER*(*) MSTR
C     ..
C     .. Local Scalars ..
      INTEGER ICH,JCHAR,NCHAR
      CHARACTER*75 MNAME
C     ..
C     .. Local Arrays ..
      INTEGER MCHAR(13)
C     ..
C     .. Data statements ..
      DATA MCHAR/1,8,16,21,26,29,33,37,43,52,59,67,75/
C     ..
      MNAME = 'JANUARYFEBRUARYMARCHAPRILMAYJUNEJULYAUGUSTSEPTEMBER'//
     *        'OCTOBERNOVEMBERDECEMBER'
C
      ICH = 1
      IF (MONTH.LT.1 .OR. MONTH.GT.12) GO TO 30
      NCHAR = 9
      DO 10 ICH = 1,NCHAR
         JCHAR = MCHAR(MONTH) + ICH - 1
         IF (JCHAR.GE.MCHAR(MONTH+1)) GO TO 20
         MSTR(ICH:ICH) = MNAME(JCHAR:JCHAR)
   10 CONTINUE
      RETURN
   20 MSTR(ICH:NCHAR) = ' '
      IER = 0
      RETURN
   30 IER = 1
      RETURN
C
      END
