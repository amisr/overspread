C     $Id: carmel.f,v 1.3 2001/03/15 22:10:30 sjc Exp $
C
      SUBROUTINE CARMEL(B,XI,VL)
C
C     Private/Internal subroutine. Part of Apex coordinate computation
C     package. See COORD for public API. Computes scaler VL as a
C     function of B and XI.
C
C       Input:
C           B - Scaler field strength value.
C          XI - integral invariant (see INTEG).
C
C      Output:
C          VL - McIlwain's L-shell parameter i.e. Invariant Latitude
C               = ACOS(DSQRT(1.0D0/VL))/DTR
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION B,VL,XI
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION GG,XX
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DEXP,DLOG
C     ..
      IF (XI.GT.1.0D-36) THEN
         XX = 3.0D0*DLOG(XI)
         XX = XX + DLOG(B/0.311653D0)
         IF (XX.LE.-22.D0) THEN
            GG = .333338D0*XX + .30062102D0
         ELSE IF (XX.LE.-3.D0) THEN
            GG = ((((((((-8.1537735D-14*XX+8.3232531D-13)*XX+
     *           1.0066362D-9)*XX+8.1048663D-8)*XX+3.2916354D-6)*XX+
     *           8.2711096D-5)*XX+1.3714667D-3)*XX+.015017245D0)*XX+
     *           .43432642D0)*XX + .62337691D0
         ELSE IF (XX.LE.3.D0) THEN
            GG = ((((((((2.6047023D-10*XX+2.3028767D-9)*XX-
     *           2.1997983D-8)*XX-5.3977642D-7)*XX-3.3408822D-6)*XX+
     *           3.8379917D-5)*XX+1.1784234D-3)*XX+1.4492441D-2)*XX+
     *           .43352788D0)*XX + .6228644D0
         ELSE IF (XX.LE.11.7D0) THEN
            GG = ((((((((6.3271665D-10*XX-3.958306D-8)*XX+
     *           9.9766148D-07)*XX-1.2531932D-5)*XX+7.9451313D-5)*XX-
     *           3.2077032D-4)*XX+2.1680398D-3)*XX+1.2817956D-2)*XX+
     *           .43510529D0)*XX + .6222355D0
         ELSE IF (XX.GT.23.D0) THEN
            GG = XX - 3.0460681D0
         ELSE
            GG = (((((2.8212095D-8*XX-3.8049276D-6)*XX+2.170224D-4)*XX-
     *           6.7310339D-3)*XX+.12038224D0)*XX-.18461796D0)*XX +
     *           2.0007187D0
         END IF
         VL = (((1.0D0+DEXP(GG))*0.311653D0)/B)**(1.D0/3.D0)
C        end compute l
      ELSE
         VL = (0.311653D0/B)**(1.D0/3.D0)
      END IF
      RETURN
C
      END
