C     $Id: dsf.f,v 1.2 2001/03/12 21:55:31 sjc Exp $
C
      SUBROUTINE DSF(GCLAT,GLON,RKM,ALT,HALT,ISTOP,DS)
C
C     Calculates an optimum integration step size for geomagnetic
C     field line tracing routine LINTRA, as an empirical function of
C     the geomagnetic dipole coordinates of the starting point. when
C     start and end points are in the same hemisphere and
C     abs(halt-alt)<10000, the empirical formula is modified so that
C     ds is no greater than 1/100 of the altitude difference between
C     start and end points.
C
C     input:
C         GCLAT - start point geocentric latitude (deg)
C          GLON - start point geocentric longitude (deg)
C           RKM - start point radial distance (km)
C           ALT - starting point geodetic altitude
C          HALT - tuning parameter for altitude test. 
C         ISTOP - set to -1 to enable abs(halt-alt)<10000 test
C     output:
C            DS - step size
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION ALT,DS,GCLAT,GLON,HALT,RKM
      INTEGER ISTOP
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION RAD,SINGML
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,DCOS,DSIN
C     ..
C     .. Data statements ..
      DATA RAD/57.2957795D0/
C     ..
C
      SINGML = .98D0*DSIN(GCLAT/RAD) +
     *         .199D0*DCOS(GCLAT/RAD)*DCOS((GLON+69.D0)/RAD)
      DS = .06D0*RKM/(1.D0-SINGML*SINGML) - 370.D0
      IF (DS.GT.3000.D0) DS = 3000.D0
      IF (ISTOP.EQ.-1 .AND. ABS(ALT-HALT).LT.
     *    10000.D0) DS = ABS(ALT-HALT)/100.D0
      RETURN
C
      END
