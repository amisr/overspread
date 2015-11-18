C     $Id: invar.f,v 1.2 2001/03/14 22:46:03 sjc Exp $
C
      SUBROUTINE INVAR(TM,FLAT,FLONG,ALT,ERR,BB,FL)
C
C     Private/Internal subroutine. Part of Apex coordinate computation
C     package. See COORD for public API. INVAR converts coordinates
C     TM, FLAT, FLON and ALT to L-shell coordinates FL and BB. The
C     uncertainty in FL is typically less than 10.*ERR*FL (percent)
C
C       Input:
C           TM - time in years for desired field (e.g. 1971.25)
C         FLAT - geocentric latitude (degrees)
C        FLONG - east longitude
C          ALT - altitude (km)
C          ERR - tolerance factor
C
C      Output:
C           BB - Magnetic Field strength at point.
C           FL - McIlwain's L-shell parameter i.e. 
C                Invariant Latitude = ACOS(DSQRT(1.0D0/FL))/DTR
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION ALT,BB,ERR,FL,FLAT,FLONG,TM
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION ASUM,BCO,CCO,DCLT,DCO,DN,DX,FLINT,SA,SC
      INTEGER I,J,JEP,JUP
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION ARC(200),B(200),BEG(200),BEND(200),BLOG(200),
     *                 ECO(200),R1(3),R2(3),R3(3),V(3,3),VN(3),VP(3)
C     ..
C     .. External Subroutines ..
      EXTERNAL CARMEL,INTEG,LINES,STARTR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,DCOS,DEXP,DLOG,DSQRT
C     ..
      V(1,2) = ALT/6371.2D0
      V(2,2) = (90.D0-FLAT)/57.2957795D0
      V(3,2) = FLONG/57.2957795D0
      ARC(1) = 0.D0
      ARC(2) = (1.0D0+V(1,2))*DSQRT(ERR)*0.3D0
      DCLT = 1.5708D0 - 0.2007D0*DCOS(V(3,2)+1.239D0)
      IF (V(2,2).GT.DCLT) ARC(2) = -ARC(2)
      CALL STARTR(R1,R2,R3,B,ARC,V,TM)
      DO 10 I = 1,3
         VP(I) = V(I,2)
         VN(I) = V(I,3)
   10 CONTINUE
      CALL LINES(R1,R2,R3,B,ARC,ERR,J,VP,VN,TM)
      IF (J.LT.200) THEN
         JUP = J
         DO 20 J = 1,JUP
            ARC(J) = ABS(ARC(J))
            BLOG(J) = DLOG(B(J))
   20    CONTINUE
         JEP = JUP - 1
         DO 30 J = 2,JEP
            ASUM = ARC(J) + ARC(J+1)
            DX = BLOG(J-1) - BLOG(J)
            DN = ASUM*ARC(J)*ARC(J+1)
            BCO = ((BLOG(J-1)-BLOG(J+1))*ARC(J)**2-DX*ASUM**2)/DN
            CCO = (DX*ARC(J+1)-(BLOG(J)-BLOG(J+1))*ARC(J))/DN
            SA = .75D0*ARC(J)
            SC = SA + .25D0*ASUM
            DCO = BLOG(J-1) - CCO*SA*SC
            ECO(J) = BCO + CCO*(SA+SC)
            BEG(J) = DEXP(DCO+ECO(J)*.5D0*ARC(J))
            BEND(J) = DEXP(DCO+ECO(J)*.5D0*(ASUM+ARC(J)))
   30    CONTINUE
         BEG(JUP) = BEND(JEP)
         BEND(JUP) = B(JUP)
         ECO(JUP) = (2.0D0/ARC(JUP))*DLOG(BEND(JUP)/BEG(JUP))
         CALL INTEG(ARC,BEG,BEND,B,JEP,ECO,FLINT)
         CALL CARMEL(B(2),FLINT,FL)
      ELSE
         FL = -1.0D0
      END IF
      BB = B(2)
      RETURN
C
      END
