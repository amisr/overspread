C     $Id: rpcart.f,v 1.2 2001/03/16 20:49:48 sjc Exp $
C
      SUBROUTINE RPCART(SR,SLAT,SLON,AZ,EL,RANGE,RFX,RFY,RFZ,PFX,PFY,
     *                  PFZ)
C
C     jmh - 11/79  ans fortran 66
C
C     RPCART computes the components (RFX,RFY,RFZ) relative to an earth
C     centered cartesian coordinate system of the radar line of sight
C     vector from a radar with coordinates SR (distance from center
C     of earth), SLAT (geocentric latitude) and SLON (longitude). the
C     observation point is specified by AZ (azimuth), EL (elevation) and
C     RANGE (range). the cartesian coordinates of the observation
C     point are returned in (PFX,PFY,PFZ).
C
C
C     Input:
C       SR    - distance of station from center of earth (km)
C       SLAT  - geocentric latitude of station (deg)
C       SLON  - longitude of station (deg)
C       AZ    - radar azimuth (deg)
C       EL    - radar elevation (deg)
C       RANGE - radar range (km)
C
C     Output:
C       RFX,RFY,RFZ - earth centered cartesian coordinate components
C                     of radar line of sight.
C       PFX,PFY,PFZ - earth centered cartesian coordinate components
C                     of observation point.
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION AZ,EL,PFX,PFY,PFZ,RANGE,RFX,RFY,RFZ,SLAT,SLON,SR
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION A,CA,CE,DTR,E,RFP,RFR,RFT,RPHI,RR,RTHETA,RX,RY,
     *                 RZ,SA,SE
C     ..
C     .. External Subroutines ..
      EXTERNAL VCTCNV
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DCOS,DSIN
C     ..
C     .. Data statements ..
      DATA DTR/.0174532925199D0/
C     ..
C
      RR = SR
      RTHETA = 90.D0 - SLAT
      RPHI = SLON
      A = DTR*(180.D0-AZ)
      E = DTR*(90.D0-EL)
      CA = DCOS(A)
      SA = DSIN(A)
      CE = DCOS(E)
      SE = DSIN(E)
      RFR = RANGE*CE
      RFT = RANGE*SE*CA
      RFP = RANGE*SE*SA
      CALL VCTCNV(RFX,RFY,RFZ,RX,RY,RZ,RFR,RFT,RFP,RR,RTHETA,RPHI,2)
      PFX = RX + RFX
      PFY = RY + RFY
      PFZ = RZ + RFZ
      RETURN
C
      END
