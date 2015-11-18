C     $Id: dates.f,v 1.2 2003/01/08 19:28:01 brideout Exp $
C
      SUBROUTINE DATES(DAY,MONTH,YEAR,IOPT,IER,DATE)
C
C     Returns DATE String in various formats given DAY, MONTH, YEAR
C     and IOPT (which specifies the desired format).
C
C     The following variables appear in the calling sequence
C
C       Input:
C           DAY - DAY OF THE MONTH (1)
C         MONTH - MONTH NUMBER (3)
C          YEAR - YEAR (1977)
C          IOPT - FORMAT INDICATOR WHEN DATE IS AN OUTPUT VARIABLE
C                  1 - 01/01/97
C                  2 - 01,JAN,1997
C                  3 - 1 JANUARY, 1997
C                  4 - JANUARY 1 1997
C                  5 - 01JAN97
C
C       Output:
C           IER - ERROR INDICATOR. IER IS RETURNED BY ALL ROUTINES IN
C                 THE PACKAGE. IER=0 IF NO ERRORS ARE DETECTED. IER=1
C                 IF AN ERROR IS DETECTED.
C          DATE - DATE AS A STRING OF ALPHANUMERIC CHARACTERS, 3
C                 CHARACTERS/WORD. WHEN AN INPUT VARIABLE, DATE MAY BE
C                 IN ANY REASONABLE FORMAT, E.G. MARCH 1 1977, 3/1/77,
C                 ETC. IF EXPRESSED AS THREE NUMERIC FIELDS, ORDER IS
C                 PRESUMED TO BE MONTH, DAY, YEAR. WHEN AN OUTPUT
C                 VARIABLE, THE FORMAT IS DETERMINED BY IOPT, AND SIX
C                 WORDS SHOULD BE RESERVED IN THE CALLING PROGRAM.
C
C     .. Scalar Arguments ..
      INTEGER DAY,IER,IOPT,MONTH,YEAR
      CHARACTER*(*) DATE
C     ..
C     .. Local Scalars ..
      INTEGER IC
      CHARACTER*2 CDAY,CMONTH
      CHARACTER*4 CYEAR
      CHARACTER*9 MSTR
C     ..
C     .. Local Arrays ..
      INTEGER MOCHAR(12)
C     ..
C     .. External Subroutines ..
      EXTERNAL MONAME
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC CHAR
C     ..
C     .. Data statements ..
      DATA MOCHAR/7,8,5,5,3,4,4,6,9,7,8,8/
C     ..
C
      IER = 0
      IC = 0
      WRITE (UNIT=CDAY,FMT='(I2)') DAY
      WRITE (UNIT=CMONTH,FMT='(I2)') MONTH
      WRITE (UNIT=CYEAR,FMT='(I4)') YEAR
      GO TO (10,20,30,40,50) IOPT
C
C     ..01/01/97
   10 DATE(1:2) = CMONTH
      IF (MONTH.LT.10) DATE(1:1) = '0'
      DATE(3:3) = '/'
      DATE(4:5) = CDAY
      IF (DAY.LT.10) DATE(4:4) = '0'
      DATE(6:6) = '/'
      DATE(7:8) = CYEAR(3:4)
      DATE(8:8) = CHAR(0)
      GO TO 80
C
C     ..01,JAN,1997
   20 DATE(1:2) = CDAY(1:2)
      IF (DAY.LT.10) DATE(1:1) = '0'
      CALL MONAME(MONTH,MSTR,IER)
      DATE(3:3) = ','
      DATE(4:6) = MSTR(1:3)
      DATE(7:7) = ','
      DATE(8:11) = CYEAR
      DATE(12:12) = CHAR(0)
      GO TO 80
C
C     ..1 JANUARY, 1997
   30 IF (DAY.LT.10) DATE(1:1) = CDAY(2:2)
      IF (DAY.GE.10) DATE(1:2) = CDAY(1:2)
      IF (DAY.LT.10) IC = 2
      IF (DAY.GE.10) IC = 3
      DATE(IC:IC) = ' '
      IC = IC + 1
      CALL MONAME(MONTH,MSTR,IER)
      DATE(IC:IC+MOCHAR(MONTH)-1) = MSTR
      IC = IC + MOCHAR(MONTH)
      DATE(IC:IC) = ','
      IC = IC + 1
      DATE(IC:IC) = ' '
      IC = IC + 1
      DATE(IC:IC+3) = CYEAR
      IC = IC + 4
      DATE(IC:IC) = CHAR(0)
      GO TO 80
C
C     ..JANUARY 1 1997
   40 IC = 1
      CALL MONAME(MONTH,MSTR,IER)
      DATE(1:MOCHAR(MONTH)) = MSTR
      IC = IC + MOCHAR(MONTH)
      DATE(IC:IC) = ' '
      IC = IC + 1
      IF (DAY.GE.10) GO TO 60
      DATE(IC:IC) = CDAY(2:2)
      IC = IC + 1
      GO TO 70
C
C     ..01JAN97
   50 DATE(1:2) = CDAY(1:2)
      IF (DAY.LT.10) DATE(1:1) = '0'
      CALL MONAME(MONTH,MSTR,IER)
      DATE(3:5) = MSTR(1:3)
      DATE(6:7) = CYEAR(3:4)
      DATE(8:8) = CHAR(0)
      GO TO 80
C
   60 DATE(IC:IC+1) = CDAY
      IC = IC + 2
      DATE(IC:IC) = ','
      IC = IC + 1
   70 DATE(IC:IC) = ' '
      IC = IC + 1
      DATE(IC:IC+3) = CYEAR
      IC = IC + 4
      DATE(IC:IC) = CHAR(0)
C
   80 RETURN
C
      END
