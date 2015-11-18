C     $Id: hfun.f,v 1.2 2001/03/12 23:46:50 sjc Exp $
C
      DOUBLE PRECISION FUNCTION HFUN(SR,EL,RANGE)
C
C     jmh - 11/79  ans fortran 66
C
C     HFUN computes the height above a sphere (radius SR) of an
C     observation point at a specified elevation (EL) and range
C     (RANGE). SR and RANGE should be positive and EL should be in the
C     range 0.0 to 90.0.
C
C       Input:
C           SR - radial distance of station from center of earth
C           EL - radar elevation
C        RANGE - range to observation point
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION EL,RANGE,SR
C     ..
C     .. Scalars in Common ..
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION DTR,SE
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DSIN,DSQRT
C     ..
C     .. Data statements ..
      DATA DTR/.0174532925199D0/
C     ..
C
      IF (SR.LT.1.D0 .OR. RANGE.LE.0.D0 .OR. EL.LT.0.D0 .OR.
     *    EL.GT.90.D0) THEN
         HFUN = 0.D0
      ELSE
         SE = DSIN(DTR*EL)
         HFUN = DSQRT(SR*SR+RANGE*RANGE+2.D0*SR*RANGE*SE) - SR
      END IF
      RETURN
C
      END
