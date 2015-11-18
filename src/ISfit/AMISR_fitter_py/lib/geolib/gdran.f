C     $Id: gdran.f,v 1.2 2001/03/12 22:31:49 sjc Exp $
C
      DOUBLE PRECISION FUNCTION GDRAN(SR,SLATGC,SLON,AZ,EL,ALT)
C
C     GDRAN uses a half-interval (binary) search technique to determine
C     the geodetic range to a point of observation given in terms of
C     azimuth, elevation, and geodetic altitude.
C
C       Input:
C            SR - radial distance of station from center of earth
C        SLATGC - station geocentric latitude
C          SLON - station longitude
C            AZ - radar azimuth
C            EL - radar elevation
C           ALT - geodetic altitude
C
C     harris fortran 77
C     rgm - 8/85
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION ALT,AZ,EL,SLATGC,SLON,SR
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION ALT1,ALT2,GCLAT,GDALT,GDLAT,GLONG,PR,R1,R2,RFRST,
     *                 TINTER
C     ..
C     .. External Functions ..
      DOUBLE PRECISION RFUN
      EXTERNAL RFUN
C     ..
C     .. External Subroutines ..
      EXTERNAL CONVRT,POINT
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,SIGN
C     ..
C
C     determine two ranges which bound the actual range.  the first
C     range is obtained by making a spherical earth range calculation
C     using the supplied station radius.  this range will be on one
C     side or the other of the actual range and will be added to or
C     subtracted from to determine the second range until the second
C     range is on the opposite side of the actual range as the first
C     range (whew!).
C
      R1 = RFUN(SR,EL,ALT)
      R2 = R1
      CALL POINT(SR,SLATGC,SLON,AZ,EL,R2,PR,GCLAT,GLONG)
      CALL CONVRT(2,GDLAT,ALT1,GCLAT,PR)
   10 CONTINUE
      R2 = R2 + SIGN(1.D0,ALT-ALT1)
      CALL POINT(SR,SLATGC,SLON,AZ,EL,R2,PR,GCLAT,GLONG)
      CALL CONVRT(2,GDLAT,ALT2,GCLAT,PR)
      IF (SIGN(1.D0,ALT-ALT1).EQ.SIGN(1.D0,ALT-ALT2)) GO TO 10
C
C     use the center of the range boundary as a first guess. the
C     half-interval is initially 1/4 of the total range boundary.
C
      RFRST = (R1+R2)/2.D0
      TINTER = ABS(R2-R1)/4.D0
C
C     add to or subtract from the current range guess the current
C     half-interval, halving the half-interval on each iteration until
C     the actual range is approached to within a tolerance of .01 km.
C
   20 CONTINUE
      CALL POINT(SR,SLATGC,SLON,AZ,EL,RFRST,PR,GCLAT,GLONG)
      CALL CONVRT(2,GDLAT,GDALT,GCLAT,PR)
      RFRST = RFRST + SIGN(TINTER,ALT-GDALT)
      TINTER = TINTER/2.D0
      IF (ABS(GDALT-ALT).GE.1.D-2) GO TO 20
      GDRAN = RFRST
      RETURN
      END
