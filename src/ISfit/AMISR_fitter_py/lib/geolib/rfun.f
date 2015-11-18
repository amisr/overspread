C     $Id: rfun.f,v 1.2 2001/03/16 20:49:46 sjc Exp $
C
      DOUBLE PRECISION FUNCTION RFUN(SR,EL,H)
C
C     jmh - 11/79  ans fortran 66
C
C     RFUN computes the range to an observation point at a specified
C     elevation (EL) and distance (H) above a sphere of radius SR.
C     SR and H should be positive and EL should be in the range
C     0.0 to 90.0.
C
C       Input:
C         SR - radius of sphere (km)
C         EL - elevation (deg)
C          H - distance above sphere (km)
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION EL,H,SR
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
      IF (SR.LT.0.D0 .OR. H.LT.0.D0 .OR. EL.LT.0.D0 .OR.
     *    EL.GT.90.D0) THEN
         RFUN = 0.D0
      ELSE
         SE = DSIN(DTR*EL)
         RFUN = DSQRT(SR*SR*SE*SE+2.D0*SR*H+H*H) - SR*SE
      END IF
      RETURN
C
      END
