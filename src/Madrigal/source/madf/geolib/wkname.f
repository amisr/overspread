C     $Id: wkname.f,v 1.1 2003/01/08 18:31:07 brideout Exp $
C
      SUBROUTINE WKNAME(WDAY,IER,WSTR)
C
C     Returns day of the week string from day of the week number.
C
C       Input:
C         WDAY - week day number (1-7)
C
C      Output:
C         IER  - If (IER.NE.0) an error has occurred.
C         WSTR - day of the week string (e.g. 'SUNDAY').
C
C     .. Scalar Arguments ..
      INTEGER IER,WDAY
      CHARACTER*(*) WSTR
C     ..
C     .. Local Scalars ..
      CHARACTER*51 WNAME
C     ..
C     .. Local Arrays ..
      INTEGER WCHAR(8)
C     ..
C     .. Data statements ..
      DATA WNAME/'SUNDAYMONDAYTUESDAYWEDNESDAYTHURSDAYFRIDAYSATURDAY'/
      DATA WCHAR/1,7,13,20,29,37,43,51/
C     ..
C
      IF (WDAY.LT.1 .OR. WDAY.GT.7) GO TO 10
      IER = 0
      WSTR = WNAME(WCHAR(WDAY) :WCHAR(WDAY+1)-1)
      RETURN
   10 IER = 1
      RETURN
C
      END
