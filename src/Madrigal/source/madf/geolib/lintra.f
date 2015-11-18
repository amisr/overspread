C     $Id: lintra.f,v 1.4 2003/01/10 16:01:07 brideout Exp $
C
      SUBROUTINE LINTRA(TM,GCLAT,GLON,RKM,ALT,HALT,PLAT,PLON,PRKM,ARC,
     *                  ARAD,ALAT,ALON,ISTOP,NPR,INIT,IER)
C
C     jmh - 11/79  ans fortran 66
C
C     LINTRA traces the geomagnetic field line from a specified
C     starting point to either a specified altitude in either hemisphere
C     or to the apex of the field line. in either case, if the apex
C     is passed, the apex coordinates of the field line are calculated.
C     when points are found on either side of the end point or apex,
C     the final result is calculated by quadratic interpolation.
C
C       Input:
C            TM - time for desired field (years)
C         GCLAT - start point geocentric latitude (deg)
C         GCLON - start point geocentric longitude (deg)
C           RKM - start point radial distance (km)
C           ALT - start point geodetic altitude (km)
C         ISTOP = -1 - trace to altitude halt in same hemisphere
C         ISTOP = 0 - trace to apex of field line
C         ISTOP = +1 - trace to altitude halt in opposite hemisphere....
C         NPR=1 - return to calling program after each step
C
C       Input, Output:
C          HALT - end point geodetic altitude (km)
C        INIT=1 - set by calling program when returning to lintra after
C                 receiving intermediate results
C             2 - set by lintra when trace is complete
C
C      Output:
C          PLAT - end point geocentric latitude (deg)
C          PLON - end point geocentric longitude (deg)
C          PRKM - end point radial distance (km)
C           ARC - arc length of field line traced (km)
C          ARAD - apex radius of field line (earth radii)
C          ALAT - apex latitude of field line (deg)
C          ALON - apex longitude of field line (deg)
C           IER = 1 - error, number of steps exceeds maxs
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION ALAT,ALON,ALT,ARAD,ARC,GCLAT,GLON,HALT,PLAT,PLON,
     *                 PRKM,RKM,TM
      INTEGER IER,INIT,ISTOP,NPR
C     ..
C     .. Scalars in Common ..
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION A1,A2,A3,A4,A5,A6,A7,C1,CN,CP,CT,DIR,DLATMN,
     *                 DLATPP,DLONPP,H,HINT,HPP,RA,RAD,RE,SP
      INTEGER IAP,IEND,MAXS
C     ..
C     .. External Subroutines ..
      EXTERNAL DSF,ITERAT,MILMAG
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ACOS,DCOS,DSIN,DSQRT,SIGN
C     ..
C     .. Common blocks ..
      COMMON /ITER/R,DLAT,DLON,RP,DLATP,DLONP,HP,BR,BT,BP,B,ST,SGN,DS,L
      DOUBLE PRECISION B,BP,BR,BT,DLAT,DLATP,DLON,DLONP,DS,HP,R,RP,SGN,
     *                 ST
      INTEGER L
C     ..
C     .. Statement Functions ..
      DOUBLE PRECISION PINTER,PMIN
C     ..
C     .. Save statement ..
      SAVE A1,A2,A3,A4,A5,A6,A7,C1,CN,CP,CT,DIR,DLATMN,DLATPP,DLONPP,H,
     *     HINT,HPP,RA,RAD,RE,SP,IAP,IEND,MAXS
C     ..
C     .. Data statements ..
      DATA RAD/57.2957795D0/,C1/.0067397D0/,RA/6378.16D0/,MAXS/8000/,
     *     H/0.0D0/
C     ..
C     .. Statement Function definitions ..
      PINTER(A1,A2,A3,A4,A5,A6,A7) = ((A2-A3)*(A7-A2)*(A7-A3)*A4-
     *                               (A1-A3)*(A7-A1)*(A7-A3)*A5+
     *                               (A1-A2)*(A7-A1)*(A7-A2)*A6)/
     *                               ((A1-A2)*(A1-A3)*(A2-A3))
      PMIN(A1,A2,A3,A4,A5,A6) = .5D0*((A3-A2)*(A3+A2)*A4+
     *                          (A1-A3)*(A1+A3)*A5+(A2-A1)*(A2+A1)*A6)/
     *                          ((A3-A2)*A4+(A1-A3)*A5+(A2-A1)*A6)
C     ..
C
      IF (INIT.EQ.1) GO TO 20
C
C     .....initialization.....
      CALL DSF(GCLAT,GLON,RKM,ALT,HALT,ISTOP,DS)
      DIR = +1.D0
      IF (ISTOP.EQ.-1 .AND. HALT.LT.ALT) DIR = -1.D0
      DLAT = GCLAT
      DLON = GLON
      R = RKM
      L = 0
      IAP = 0
      IEND = 0
      RP = 0.0D0
      DLATP = 0.0D0
      DLONP = 0.0D0
C
C     .....calculate trig functions of currect coordinates.....
   10 CONTINUE
      CT = DSIN(DLAT/RAD)
      ST = DCOS(DLAT/RAD)
      SP = DSIN(DLON/RAD)
      CP = DCOS(DLON/RAD)
C
C     .....evaluate geomagnetic field.....
      CALL MILMAG(TM,R,ST,CT,SP,CP,BR,BT,BP,B)
C
C     .....if first step, set correct direction.....
      IF (L.EQ.0) SGN = SIGN(1.D0,BR*DIR)
C
C     .....increment step count.....
      L = L + 1
C
C     .....if step count > maxs, error return.....
      IF (L.GE.MAXS) GO TO 70
C
C     .....store previous coordinates and do next integration step.....
      DLATPP = DLATP
      DLONPP = DLONP
      HPP = HP
      CALL ITERAT
      HP = H
      H = R - RA/DSQRT(1.D0+C1*CT*CT)
C
C     .....return if npr=1.....
      IF (NPR.EQ.1) GO TO 80
   20 CONTINUE
C
C     .....did this step pass apex.....
      IF (IAP.EQ.0 .AND. DIR.EQ.1.D0 .AND. R.LT.RP) GO TO 50
C
C     .....did this step pass halt.....
   30 CONTINUE
      IF (ISTOP.EQ.-1 .AND. (ALT.LE.HALT.AND.H.GT.HALT) .OR.
     *    (ALT.GT.HALT.AND.H.LT.HALT)) IEND = 1
C     ...has conjugate been found...
      IF (ISTOP.EQ.1 .AND. H.LT.HALT .AND. R.LT.RP) IEND = 1
      IF (IEND.EQ.0) GO TO 10
      HINT = HALT
      PLAT = PINTER(HPP,HP,H,DLATPP,DLATP,DLAT,HINT)
      PLON = PINTER(HPP,HP,H,DLONPP,DLONP,DLON,HINT)
   40 CONTINUE
      PRKM = HINT + RA/DSQRT(1.D0+C1*DSIN(PLAT/RAD)**2)
      ARC = DS*((L-5)+(HP-HINT)/(HP-H))
      do 1 i=1,99999
          if (plon .ge. -180.0 .and. plon .le. 180.0) goto 2
          IF (PLON.LT.-180.D0) PLON = PLON + 360.D0
          IF (PLON.GT.180.D0) PLON = PLON - 360.D0
    1 continue
    2 continue
      IF (IEND.EQ.1 .AND. ISTOP.NE.0) GO TO 60
      CN = DSIN(PLAT/RAD)
      RE = RA/DSQRT(1.D0+C1*CN*CN)
      ARAD = 1.D0 + HINT/RE
      ALAT = RAD*ACOS(1.D0/DSQRT(ARAD))
      ALON = PLON
      IAP = 2
      IF (ISTOP.EQ.0) GO TO 60
      GO TO 30
C
C     .....interpolate to find end coordinates, arc length.....
   50 CONTINUE
      DLATMN = PMIN(DLATPP,DLATP,DLAT,HPP,HP,H)
      HINT = PINTER(DLATPP,DLATP,DLAT,HPP,HP,H,DLATMN)
      PLAT = DLATMN
      PLON = PINTER(DLATPP,DLATP,DLAT,DLONPP,DLONP,DLON,PLAT)
      IF (ISTOP.EQ.0) IEND = 1
      GO TO 40
   60 CONTINUE
      INIT = 2
      GO TO 80
   70 CONTINUE
      IER = 1
      INIT = 2
   80 CONTINUE
      RETURN
C
      END
