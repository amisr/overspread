C     $Id: startr.f,v 1.2 2001/03/16 20:49:51 sjc Exp $
C
      SUBROUTINE STARTR(R1,R2,R3,B,ARC,V,TM)
C
C     Private/Internal subroutine. Part of Apex coordinate computation
C     package. See COORD for public API.
C
C       Input:
C           TM - time in years for desired field (e.g. 1971.25)
C
C       Input, Output:
C          ARC - Altitudes in earth radii (array).
C            V - floating point array.
C
C      Output:
C     R1,R2,R3 - field strength in geocentric directions.
C            B - Magnitude of field (array)
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION TM
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION ARC(200),B(200),R1(3),R2(3),R3(3),V(3,3)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION AER,BP,BR,BT,COP,COT,DN,OER,RKM,SIP,SIT,SSQ
      INTEGER I,IS
C     ..
C     .. External Subroutines ..
      EXTERNAL MILMAG
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,DCOS,DSIN
C     ..
      SIT = ABS(DSIN(V(2,2)))
      AER = V(1,2)
      SSQ = SIT*SIT
      OER = (6356.912D0+SSQ*(21.3677D0+.108D0*SSQ))/6371.2D0
      V(1,2) = AER + OER
   10 CONTINUE
      IF (V(3,2).LT.0) THEN
         V(3,2) = V(3,2) + 6.283185307D0
         GO TO 10
      END IF
      RKM = V(1,2)*6371.2D0
C     if(model.eq.6) rkm=rkm+14.288-ssq*(21.3677+.108*ssq)
      COT = DCOS(V(2,2))
      SIP = DSIN(V(3,2))
      COP = DCOS(V(3,2))
      CALL MILMAG(TM,RKM,SIT,COT,SIP,COP,BR,BT,BP,B(2))
      R2(1) = BR/B(2)
      DN = B(2)*V(1,2)
      R2(2) = BT/DN
      R2(3) = BP/(DN*SIT)
      IS = 0
   20 CONTINUE
      DO 30 I = 1,3
         V(I,1) = V(I,2) - ARC(2)*R2(I)
   30 CONTINUE
      SIT = ABS(DSIN(V(2,1)))
   40 CONTINUE
      RKM = V(1,1)*6371.2D0
      SSQ = SIT*SIT
C     if(model.eq.6) rkm=rkm+14.288-ssq*(21.3677+.108*ssq)
      COT = DCOS(V(2,1))
      SIP = DSIN(V(3,1))
      COP = DCOS(V(3,1))
      CALL MILMAG(TM,RKM,SIT,COT,SIP,COP,BR,BT,BP,B(1))
      IF (B(1).GE.B(2)) THEN
         R1(1) = BR/B(1)
         ARC(3) = ARC(2)
         DN = B(1)*V(1,1)
         R1(2) = BT/DN
         R1(3) = BP/(DN*SIT)
         DO 50 I = 1,3
            V(I,1) = V(I,2) - ARC(2)*(R1(I)+R2(I))/2.D0
   50    CONTINUE
         SIT = ABS(DSIN(V(2,1)))
         IS = IS + 1
         IF (IS.EQ.1) GO TO 40
         GO TO 60
      END IF
      ARC(2) = -ARC(2)
      GO TO 20
   60 CONTINUE
      DO 70 I = 1,3
         V(I,3) = V(I,2) + ARC(3)*((1.5D0)*R2(I)-.5D0*R1(I))
   70 CONTINUE
      R3(1) = 0.0D0
      R3(2) = 0.0D0
      R3(3) = 0.0D0
      RETURN
C
      END
