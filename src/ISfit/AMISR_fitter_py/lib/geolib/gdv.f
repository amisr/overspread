C     $Id: gdv.f,v 1.2 2001/03/12 23:46:45 sjc Exp $
C
      SUBROUTINE GDV(GDLAT,GCLAT,FR,FT,FP,FX,FY,FZ)
C
C     jmh - 11/79  ans fortran 66
C
C     GDV converts a vector field f at geodetic latitude GDLAT and
C     geocentric latitude GCLAT from a geocentric based representation
C     to a geodetic based representation. the geocentric components
C     are FR (radial outward), FT (increasing geocentric colatitude,
C     e.g. southward) and FP (increasing east longitude). the
C     geodetic components are FX (northward, parallel to surface of
C     earth), FY (eastward, parallel to surface of earth) and FZ
C     (downward, perpendicular to surface of earth). FR,FT,FP thus
C     correspond to spherical coordinates r,theta,phi, with their
C     origin at the center of the earth. x,y,z are the coordinates
C     customarily used to describe the three components of the
C     geomagnetic field. FP and FY are the same.
C
C       Input:
C         GDLAT - geodetic latitude (degrees)
C         GCLAT - geocentric latitude (degrees)
C            FR - radial outward (geocentric).
C            FT - increasing geocentric colatitude (southward).
C            FP - increasing east longitude.
C
C      Output:
C            FX - northward, parallel to surface of earth (geodetic).
C            FY - eastward, parallel to surface of earth.
C            FZ - downward, perpendicular to surface of earth.
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION FP,FR,FT,FX,FY,FZ,GCLAT,GDLAT
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION COSD,COSLAT,CT,DTR,GDL,SIND,SINLAT,ST,T
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DCOS,DSIN
C     ..
C     .. Data statements ..
      DATA DTR/.0174532925199D0/
C     ..
C
      GDL = DTR*GDLAT
      SINLAT = DSIN(GDL)
      COSLAT = DCOS(GDL)
      T = DTR*(90.D0-GCLAT)
      CT = DCOS(T)
      ST = DSIN(T)
      SIND = ST*SINLAT - CT*COSLAT
      COSD = CT*SINLAT + ST*COSLAT
      FX = -FT*COSD - FR*SIND
      FY = FP
      FZ = FT*SIND - FR*COSD
      RETURN
C
      END
