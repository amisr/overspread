C     $Id: milmag.f,v 1.6 2010/08/11 14:22:18 brideout Exp $
C
      SUBROUTINE MILMAG(TM,RKM,ST,CT,SPH,CPH,BR,BT,BP,B)
C
C     MILMAG evaluates the geomagnetic field at a point specified by
C     its geocentric coordinates.
C
C     Modified by B. Rideout - Aug. 10, 2010
C     This method is now simply a thin wrapper around igrf11.f,
C     method igrf11syn.  See igrf11.f for details
C
C     Modified by B. Rideout - Aug. 31, 2005
C     This method is now simply a thin wrapper around Geopack-2005 code,
C     method igrf_geo.  See Geopack-2005.f for details
C
C     Modified by B. Rideout - Dec. 26, 2002
C     This method is now simply a thin wrapper around geo-cgm code,
C     method igrf.  See geo-cgm.f for details


C
C       Input:
C              TM - time in years for desired field (e.g. 1971.25)
C             RKM - geocentric distance (km)
C           ST,CT - sin and cos of geocentric colatitude
C         SPH,CPH - sin and cos of east longitude
C
C      Output:
C        BR,BT,BP - geocentric field components (gauss)
C               B - magnitude of field (gauss)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION B,BP,BR,BT,CPH,CT,RKM,SPH,ST,TM
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION GCLAT,GLON
      DOUBLE PRECISION R,T,F,RBR,RBT,RBP,RB
      INTEGER IYEAR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ASIN,ANINT,SQRT
C      .. external functions
      EXTERNAL igrf11syn

C     convert from sin/cos to GCLAT, GLON
      IF (ST .GE. 0.0 .AND. CT .GE. 0.0) THEN
          GCLAT = ASIN(ST)*57.2958
      ELSE IF (ST .GE. 0.0 .AND. CT .LE. 0.0) THEN
          GCLAT = 180 - ASIN(ST)*57.2958
      ELSE IF (ST .LE. 0.0 .AND. CT .LE. 0.0) THEN
          GCLAT = 180 - ASIN(ST)*57.2958
      ELSE
          GCLAT = ASIN(ST)*57.2958 + 360.0
      END IF

C     leave GCLAT as degrees

      IF (SPH .GE. 0.0 .AND. CPH .GE. 0.0) THEN
          GLON = ASIN(SPH)*57.2958
      ELSE IF (SPH .GE. 0.0 .AND. CPH .LE. 0.0) THEN
          GLON = 180 - ASIN(SPH)*57.2958
      ELSE IF (SPH .LE. 0.0 .AND. CPH .LE. 0.0) THEN
          GLON = 180 - ASIN(SPH)*57.2958
      ELSE
          GLON = ASIN(SPH)*57.2958 + 360.0
      END IF

C     leave GLON as degrees

      CALL igrf11syn(0,TM,2,RKM,GCLAT,GLON,RBT,RBP,RBR,RB)

C     Set outputs as double percision
      BR=DBLE(RBR)*(-1E-5)
      BT=DBLE(RBT)*(-1E-5)
      BP=DBLE(RBP)*1E-5
      B  = SQRT(BT*BT + BP*BP + BR*BR)

      RETURN
C
      END
