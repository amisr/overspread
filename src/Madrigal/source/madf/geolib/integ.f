C     $Id: integ.f,v 1.3 2001/03/16 17:21:40 sjc Exp $
C
      SUBROUTINE INTEG(ARC,BEG,BEND,B,JEP,ECO,FI)
C
C     Private/Internal subroutine. Part of Apex coordinate computation
C     package. See COORD for public API. INTEG determines the value of
C     the integral invariant FI by numerically integrating along the
C     field line from the specified point of interest to its conjugate
C     point.
C
C       Input:
C         ARC - Altitudes in earth radii (array).
C         BEG - floating point array.
C        BEND - floating point array.
C           B - Magnitude of field (array)
C         ECO - floating point array.
C         JEP - floating point scaler.
C
C      Output:
C          FI - floating point scaler.
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION FI
      INTEGER JEP
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION ARC(200),B(200),BEG(200),BEND(200),ECO(200)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION A,ARG1,ASUM,BB,C,DN,T,TB,TE,X2,X3
      INTEGER I,KK
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,DLOG,DSQRT
C     ..
      KK = JEP
      IF (KK.GE.4) THEN
         IF (KK.GT.4) THEN
            T = DSQRT(1.0D0-BEND(2)/B(2))
            FI = (2.0D0*T-DLOG((1.0D0+T)/(1.0D0-T)))/ECO(2)
            IF (B(2).GT.BEND(KK)) KK = KK + 1
            T = DSQRT(ABS(1.0D0-BEG(KK)/B(2)))
            FI = FI - (2.0D0*T-DLOG((1.0D0+T)/(1.0D0-T)))/ECO(KK)
            KK = KK - 1
            DO 10 I = 3,KK
               ARG1 = 1.D0 - BEND(I)/B(2)
               IF (ARG1.GT.0) THEN
                  TE = DSQRT(ARG1)
               ELSE
                  TE = 1.D-5
               END IF
               ARG1 = 1.D0 - BEG(I)/B(2)
               IF (ARG1.LE.0) THEN
                  TB = 1.D-5
               ELSE
                  TB = DSQRT(ARG1)
               END IF
               IF (ABS(ECO(I)).GT.2.D-5) THEN
                  FI = FI + (2.D0*(TE-TB)-DLOG((1.D0+TE)*(1.D0-
     *                 TB)/((1.D0-TE)*(1.D0+TB))))/ECO(I)
               ELSE
                  FI = FI + ((TE+TB)*(ARC(I)+ARC(I+1)))/4.D0
               END IF
   10       CONTINUE
            GO TO 20
         ELSE
            KK = KK - 1
         END IF
      END IF
      A = B(KK-1)/B(2)
      X2 = B(KK)/B(2)
      X3 = B(KK+1)/B(2)
      ASUM = ARC(KK) + ARC(KK+1)
      DN = ARC(KK)*ARC(KK+1)*ASUM
      BB = (-A*ARC(KK+1)*(ARC(KK)+ASUM)+X2*ASUM**2-X3*ARC(KK)**2)/DN
      C = (A*ARC(KK+1)-X2*ASUM+X3*ARC(KK))/DN
      FI = .157079632D01*(1.0D0-A+BB*BB/(4.0D0*C))/DSQRT(ABS(C))
   20 CONTINUE
      RETURN
C
      END
