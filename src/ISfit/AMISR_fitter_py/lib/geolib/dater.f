C     $Id: dater.f,v 1.2 2003/01/08 19:28:01 brideout Exp $
C
      SUBROUTINE DATER(DATE,NCHAR,DAY,MONTH,YEAR,IER)
C
C     SUBROUTINES DATER, MONAME, MONUM, WKNAME, IDAY, CALNDR, JDAY,
C     JDATER, IZLR, IDMYCK AND DATES COMPRISE A COMPREHENSIVE DATE 
C     MANIPULATION
C     PACKAGE. THE FOLLOWING VARIABLES APPEAR IN THE CALLING SEQUENCE
C     OF ONE OR MORE SUBROUTINES. ALL ARE TYPED INTEGER. THE VALUE
C     CORRESPONDING TO MARCH 1, 1977 IS SHOWN IN PARENTHESES.
C
C           DAY - DAY OF THE MONTH (1)
C         MONTH - MONTH NUMBER (3)
C          YEAR - YEAR (1977)
C         DAYNO - DAY OF THE YEAR (60)
C        JDAYNO - JULIAN DAY NUMBER (2443204)
C          WDAY - WEEKDAY NUMBER (3)
C          DATE - DATE AS A STRING OF ALPHANUMERIC CHARS, 3 CHARS/WORD.
C                 WHEN AN INPUT VARIABLE, DATE MAY BE IN ANY REASONABLE
C                 FORMAT, E.G. MARCH 1 1977, 3/1/77, ETC. IF EXPRESSED
C                 AS THREE NUMERIC FIELDS, ORDER IS PRESUMED TO BE
C                 MONTH, DAY, YEAR. WHEN AN OUTPUT VARIABLE, THE FORMAT
C                 IS DETERMINED BY IOPT, AND SIX WORDS SHOULD BE
C                 RESERVED IN THE CALLING PROGRAM.
C
C          MSTR - MONTH, AS A STRING OF UPPER CASE ALPHABETIC
C                 CHARACTERS, 3 CHARACTERS/WORD. THREE WORDS SHOULD BE
C                 RESERVED IN THE CALLING PROGRAM. (MARCH)
C          WSTR - DAY OF THE WEEK, AS A STRING OF UPPER CASE ALPHABETIC
C                 CHARACTERS, 3 CHARACTERS/WORD. THREE WORDS SHOULD BE
C                 RESERVED IN THE CALLING PROGRAM. (TUESDAY)
C          IOPT - FORMAT INDICATOR WHEN DATE IS AN OUTPUT VARIABLE
C                  1 - 03/01/77
C                  3 - 01,MAR,1977
C                  4 - 1 MARCH, 1977
C                  5 - MARCH 1, 1977
C         NCHAR - NUMBER OF CHARACTERS TO BE SCANNED IN AN INPUT STRING.
C           IER - ERROR INDICATOR. IER IS RETURNED BY ALL ROUTINES IN
C                 THE PACKAGE. IER=0 IF NO ERRORS ARE DETECTED. IER=1
C                 IF AN ERROR IS DETECTED.
C
C     .. Scalar Arguments ..
      INTEGER DAY,IER,MONTH,NCHAR,YEAR
      CHARACTER*(*) DATE
C     ..
C     .. Local Scalars ..
      INTEGER I,IA,ICH,IFLAST,IOS,J,JCHAR,MCHAR,N,NA,NI
      CHARACTER*4 CFMT
      CHARACTER*9 ITEMP
      CHARACTER*12 NUMER
      CHARACTER*27 IALPH
C     ..
C     .. Local Arrays ..
      INTEGER IFIELD(4,3),IT(3)
C     ..
C     .. External Functions ..
      INTEGER IDMYCK
      EXTERNAL IDMYCK
C     ..
C     .. External Subroutines ..
      EXTERNAL MONUM
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC CHAR,ICHAR,MIN0
C     ..
C     .. Data statements ..
      DATA IALPH/'ABCDEFGHIJKLMNOPQRSTUVWXYZ '/
      DATA NUMER/'0123456789  '/
      DATA CFMT/'(I )'/
C     ..
      IER = 0
      IA = 0
      IOS = 0
C     .....DETERMINE FIRST THREE ALPHANUMERIC FIELDS.....
      N = 1
      IFLAST = 3
      DO 40 I = 1,NCHAR + 1
         IF (I.EQ.NCHAR+1) GO TO 30
         DO 10 J = 1,10
            IF (DATE(I:I).NE.NUMER(J:J)) GO TO 10
            IF (IFLAST.EQ.1) GO TO 40
            IF (N.GT.3) THEN
               IER = 1
               RETURN
            END IF
            IFIELD(1,N) = 1
            IFIELD(2,N) = I
            IF (IFLAST.NE.3 .AND. N.GT.1) IFIELD(3,N-1) = I - 1
            IFLAST = 1
            N = N + 1
            GO TO 30
   10    CONTINUE
C        .....ALPHABETIC FIELDS.....
         DO 20 J = 1,26
C           IF (DATE(I:I).NE.IALPH(J:J)) GO TO 30
            IF (DATE(I:I).NE.IALPH(J:J) .AND.
     *          CHAR(ICHAR(DATE(I:I))-32).NE.IALPH(J:J)) GO TO 20
            IF (IFLAST.EQ.2) GO TO 40
            IF (N.GT.3) THEN
               IER = 1
               RETURN
            END IF
            IFIELD(1,N) = 2
            IFIELD(2,N) = I
            IF (IFLAST.NE.3 .AND. N.GT.1) IFIELD(3,N-1) = I - 1
            IFLAST = 2
            N = N + 1
            GO TO 30
   20    CONTINUE
C        .....OTHER FIELDS.....
         IF (IFLAST.NE.3 .AND. N.GT.1) IFIELD(3,N-1) = I - 1
         IFLAST = 3
   30    IF (N.EQ.5 .OR. N.EQ.4 .AND. IFLAST.EQ.3) GO TO 50
   40 CONTINUE
      IFIELD(3,3) = NCHAR
   50 IF (N.LT.4) GO TO 160
C
      NA = 0
      NI = 0
      DO 70 I = 1,3
         IFIELD(4,I) = 0
         IF (IFIELD(1,I).EQ.2) GO TO 60
         NI = NI + 1
         GO TO 70
   60    NA = NA + 1
         IA = I
   70 CONTINUE
      IF (NA.EQ.1) GO TO 80
      IF (NA.EQ.0) GO TO 100
      GO TO 160
C
   80 IFIELD(4,IA) = 2
      ICH = IFIELD(2,IA)
      MCHAR = IFIELD(3,IA) - IFIELD(2,IA) + 1
      MCHAR = MIN0(MCHAR,12)
      DO 90 I = 1,MCHAR
         JCHAR = ICH + I - 1
         ITEMP(I:I) = DATE(JCHAR:JCHAR)
   90 CONTINUE
      CALL MONUM(ITEMP,MONTH,IER)
      IF (MONTH.LT.1 .OR. MONTH.GT.12) GO TO 160
      GO TO 110
C
  100 IFIELD(4,1) = 2
      WRITE (UNIT=CFMT(3:3),FMT='(I1)') IFIELD(3,1) - IFIELD(2,1) + 1
      READ (UNIT=DATE(IFIELD(2,1) :IFIELD(3,1)),FMT=CFMT,
     *  IOSTAT=IOS) MONTH
      IF (IOS.NE.0 .OR. MONTH.LT.1 .OR. MONTH.GT.12) GO TO 160
  110 J = 0
      DO 120 I = 1,3
         IF (IFIELD(4,I).EQ.2) GO TO 120
         J = J + 1
         WRITE (UNIT=CFMT(3:3),FMT='(I1)') IFIELD(3,I) - IFIELD(2,I) + 1
         READ (UNIT=DATE(IFIELD(2,I) :IFIELD(3,I)),FMT=CFMT,
     *     IOSTAT=IOS) IT(J)
  120 CONTINUE
      IF (IOS.NE.0) GO TO 160
      IF (IT(1).GT.31 .AND. IT(2).GT.31) GO TO 160
      IF (IT(1).LT.0 .OR. IT(2).LT.0) GO TO 160
      IF (IT(1).EQ.0 .AND. IT(2).EQ.0) GO TO 160
      IF (IT(1).GT.31 .AND. IT(2).LE.31) GO TO 130
      GO TO 140
  130 DAY = IT(2)
      YEAR = IT(1)
      GO TO 150
  140 DAY = IT(1)
      YEAR = IT(2)
  150 IF (YEAR.GE.0 .AND. YEAR.LE.99) THEN
         IF (YEAR.GE.50 .AND. YEAR.LE.99) THEN
            YEAR = 1900 + YEAR
         ELSE
            YEAR = 2000 + YEAR
         END IF
      END IF
      GO TO 170
  160 IER = 1
      RETURN
  170 IER = IDMYCK(DAY,MONTH,YEAR)
      RETURN
C
      END
