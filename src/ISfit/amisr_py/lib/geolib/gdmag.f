C     $Id: gdmag.f,v 1.2 2001/03/12 22:31:47 sjc Exp $
C
      SUBROUTINE GDMAG(TM,GDLAT,GLON,GDALT,X,Y,Z,F,H,DEC,AINC)
C
C     jmh - 1/80  ans fortran 66
C
C     Evaluates the geomagnetic field at a point specified by its
C     geodetic coordinates. the reference geoid is that adopted by the
C     iau in 1964.
C
C     input:
C           TM - time in years for desired field (e.g. 1971.25)
C        GDLAT - geodetic latitude (degrees)
C         GLON - east longitude (degrees)
C        GDALT - altitude above geoid (km)
C
C     output:
C        X,Y,Z - geodetic field components (gauss)
C            F - magnitude of field (gauss)
C            H - horizontal intensity (gauss)
C          DEC - declination (degrees)
C         AINC - inclination (degrees)
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION AINC,DEC,F,GDALT,GDLAT,GLON,H,TM,X,Y,Z
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION BP,BR,BT,CP,CT,DTR,GCLAT,P,RKM,SP,ST,T
C     ..
C     .. External Subroutines ..
      EXTERNAL CONVRT,GDV,MILMAG
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DATAN2,DCOS,DSIN,DSQRT
C     ..
C     .. Data statements ..
      DATA DTR/.0174532925199D0/
C     ..
C
      CALL CONVRT(1,GDLAT,GDALT,GCLAT,RKM)
      T = DTR*(90.D0-GCLAT)
      CT = DCOS(T)
      ST = DSIN(T)
      P = DTR*GLON
      CP = DCOS(P)
      SP = DSIN(P)
      CALL MILMAG(TM,RKM,ST,CT,SP,CP,BR,BT,BP,F)
      CALL GDV(GDLAT,GCLAT,BR,BT,BP,X,Y,Z)
      H = DSQRT(X*X+Y*Y)
      DEC = DATAN2(Y,X)/DTR
      AINC = DATAN2(Z,H)/DTR
      RETURN
C
      END
