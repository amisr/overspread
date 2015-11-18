C     $Id: gaspct.f,v 1.2 2001/03/12 22:31:46 sjc Exp $
C
      SUBROUTINE GASPCT(SLATGD,SLON,SR,SLATGC,TM,AZ,EL,RANGE,GDLAT,GLON,
     *                  GDALT,B,CASPCT,ASPCT)
C
C     jmh - 3/88
C
C     *** warning *** this routing used to be called aspect. the name
C                     was changed on 3/2/88 to avoid conflict with a
C                     new routine of the same name in the geophysics
C                     library
C
C     Calculates the aspect angle between a radar beam and the
C     geomagnetic field at a specified point. the point may be
C     specified either by slatgd, slon, sr, slatgc, tm, az, el, range
C     (range .ge. 0.) or by gdlat, glon, gdalt (range .lt. 0.)
C
C     Input:
C        SLATGD - station geodetic latitude
C        SLON   - station longitude
C        SR     - radial distance of station from center of earth
C        SLATGC - station geocentric latitude
C        TM     - time in years (e.g. 1975.2)
C        AZ     - radar azimuth
C        EL     - radar elevation
C        RANGE  - range to observation point
C     Input, output:
C         GDLAT - geodetic latitude of observation point
C         GLON  - longitude of observation point
C         GDALT - altitude above spheroid of observation point
C     Output:
C        B      - geomagnetic field magnitude
C        CASPCT - cosine of aspect angle
C        ASPCT  - aspect angle (degrees)
C
C
C
C     .....subroutine parameter specifications.....
C
C     .....local specifications.....
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION ASPCT,AZ,B,CASPCT,EL,GDALT,GDLAT,GLON,RANGE,
     *                 SLATGC,SLATGD,SLON,SR,TM
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION BFX,BFY,BFZ,BP,BR,BT,CP,CT,DTR,GCLAT,P,PFX,PFY,
     *                 PFZ,PX,PY,PZ,RFX,RFY,RFZ,RKM,SP,ST,T,XB,YB,ZB
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION BF(3),PF(3),RF(3)
C     ..
C     .. External Functions ..
      DOUBLE PRECISION SPROD,VMAG
      EXTERNAL SPROD,VMAG
C     ..
C     .. External Subroutines ..
      EXTERNAL CONVRT,GDV,LOOK,MILMAG,POINT,RPCART,VCTCNV
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ACOS,DCOS,DSIN
C     ..
C     .. Equivalences ..
      EQUIVALENCE (RFX,RF(1)),(RFY,RF(2)),(RFZ,RF(3))
      EQUIVALENCE (PFX,PF(1)),(PFY,PF(2)),(PFZ,PF(3))
      EQUIVALENCE (BFX,BF(1)),(BFY,BF(2)),(BFZ,BF(3))
C     ..
C     .. Data statements ..
      DATA DTR/.0174532925199D0/
C     ..
C
C     .....calculate observation point coordinates.....
      IF (RANGE.GE.0.D0) THEN
         CALL POINT(SR,SLATGC,SLON,AZ,EL,RANGE,RKM,GCLAT,GLON)
         CALL CONVRT(2,GDLAT,GDALT,GCLAT,RKM)
      ELSE
         CALL CONVRT(1,GDLAT,GDALT,GCLAT,RKM)
         CALL LOOK(SR,SLATGC,SLON,RKM,GCLAT,GLON,AZ,EL,RANGE)
      END IF
C
C     .....calculate magnetic field at observation point
      T = DTR*(90.D0-GCLAT)
      CT = DCOS(T)
      ST = DSIN(T)
      P = DTR*GLON
      CP = DCOS(P)
      SP = DSIN(P)
      CALL MILMAG(TM,RKM,ST,CT,SP,CP,BR,BT,BP,B)
      CALL GDV(GDLAT,GCLAT,BR,BT,BP,XB,YB,ZB)
C
C     .....convert radar propagation vector and observation point
C          position to earth centered cartesian coordinates.....
      CALL RPCART(SR,SLATGC,SLON,AZ,EL,RANGE,RFX,RFY,RFZ,PFX,PFY,PFZ)
C
C     .....convert southward, eastward, upward components of magnetic
C          field at observation point to earth centered cartesian
C          coordinates.....
      CALL VCTCNV(BFX,BFY,BFZ,PX,PY,PZ,BR,BT,BP,RKM,90.D0-GCLAT,GLON,2)
C
C     .....calculate aspect angle.....
      CASPCT = SPROD(RF,BF)/(VMAG(RF)*VMAG(BF))
      ASPCT = ACOS(CASPCT)/DTR
      RETURN
      END
