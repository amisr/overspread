C     $Id: lines.f,v 1.3 2003/01/08 19:30:59 brideout Exp $
C
      SUBROUTINE LINES(R1,R2,R3,B,ARC,ERR,J,VP,VN,TM)
C
C     Private/Internal subroutine. Part of Apex coordinate computation
C     package. See COORD for public API. Makes repeated calls to the
C     IGRF, tracing magnetic field line to minimum B.
C
C       Input:
C          ERR - tolerance factor (see INVAR)
C           TM - Time in floating point years (e.g. 1995.7)
C
C       Input, Output:
C     R1,R2,R3 - field strength in geocentric directions.
C            B - Magnitude of field (array)
C          ARC - Altitudes in earth radii (array)
C           VP - Geocentric latitude (array)
C           VN - Geocentric longitude (array)
C
C      Output:
C            J - Number of points in trace.
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION ERR,TM
      INTEGER J
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION ARC(200),B(200),R1(3),R2(3),R3(3),VN(3),VP(3)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION A1,A2,A3,AA,AAB,AD,AM,AO6,ARCJ,ASUM,BB,BD,BP,BR,
     *                 BT,CC,CD,COP,COT,CRE,DD,DN,PRE1,PRE2,PRE3,QRT,
     *                 RBAR,RKM,RT,SIP,SIT,SNA,X
      INTEGER I,ILP,IS
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION RA(3)
C     ..
C     .. External Subroutines ..
      EXTERNAL MILMAG
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,DCOS,DSIN,DSQRT
C     ..
C     .. Data statements ..
      DATA PRE1,PRE2,PRE3,RA/0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0/
C     .. initialize
      AA=0.0D0
      AD=0.0D0
      ARCJ=0.0D0
      ASUM=0.0D0
      BB=0.0D0
      BD=0.0D0
      CC=0.0D0
      CD=0.0D0
      
      CRE = 0.25D0
      IF (ERR.LT.0.15625D0) CRE = (ERR**0.333333333D0)
      A3 = ARC(3)
      AAB = ABS(A3)
      SNA = A3/AAB
      A1 = ARC(1)
      A2 = ARC(2)
      AO6 = A3*A3/6.0D0
      J = 3
      ILP = 1
      IS = 1
   10 CONTINUE
      IF (VN(2).LT.0) VN(2) = -VN(2)
   20 CONTINUE
      IF (VN(2).GT.3.141592653D0) THEN
         VN(2) = 6.283185307D0 - VN(2)
         GO TO 20
      END IF
   30 CONTINUE
      IF (VN(3).LT.0) THEN
         VN(3) = VN(3) + 6.283185307D0
         GO TO 30
      END IF
   40 CONTINUE
      IF (VN(3).GT.6.283185307D0) THEN
         VN(3) = VN(3) - 6.283185307D0
         GO TO 40
      END IF
      IF (IS.EQ.2) THEN
         SIT = ABS(DSIN(VN(2)))
         B(J) = B(J)*((PRE1/VN(1))**3)
         QRT = 0.5D0*ABS(R3(1))/(0.1D0+ABS(R3(2)*VN(1)))
         X = (ABS(VN(1)-PRE1)+QRT*ABS(VN(1)*VN(2)-PRE2)+
     *       ABS(VN(1)*SIT*VN(3)-PRE3))/(AAB*ERR*DSQRT(1.D0+QRT*QRT))
         IF (.NOT.(ILP.EQ.1.OR.ILP.EQ.3)) THEN
            IF (X.GE.3.3D0) THEN
               A3 = A3*0.2D0*(8.0D0+X)/(0.8D0+X)
               J = J - 1
               ILP = 3
               ASUM = A2 + A1
               AA = ASUM*A1
               BB = A2*A1
               CC = ASUM*A2
               DO 50 I = 1,3
                  VN(I) = VP(I)
                  R3(I) = R2(I)
                  R2(I) = R1(I)
                  R1(I) = RA(I)
   50          CONTINUE
               GO TO 60
            END IF
         END IF
         IF (J.GE.200) GO TO 80
         A1 = A2
         IF (B(J).GT.B(2)) GO TO 80
         ILP = 2
         A2 = A3
         A3 = A3*.2D0*(8.D0+X)/(.8D0+X)
         AM = (2.D0-R3(2)*VN(1))*VN(1)*CRE
         IF (ABS(A3).GT.AM) A3 = SNA*AM
         IF (SNA*R3(1)+.5D0.LE.0) THEN
            AM = -.5D0*SNA*VN(1)/R3(1)
            IF (ABS(A3).GT.AM) A3 = SNA*AM
         END IF
   60    CONTINUE
         ARC(J+1) = A3
         AAB = ABS(A3)
         IS = 1
         J = J + 1
         AO6 = A3*A3/6.0D0
         ARCJ = A1 + A2 + A3
         AD = (ASUM+A1)/AA
         BD = ASUM/BB
         CD = A1/CC
      ELSE
         SIT = ABS(DSIN(VN(2)))
         PRE1 = VN(1)
         PRE2 = PRE1*VN(2)
         PRE3 = PRE1*SIT*VN(3)
         RKM = VN(1)*6371.2D0
         COT = DCOS(VN(2))
         SIP = DSIN(VN(3))
         COP = DCOS(VN(3))
         CALL MILMAG(TM,RKM,SIT,COT,SIP,COP,BR,BT,BP,B(J))
         R3(1) = BR/B(J)
         DN = B(J)*VN(1)
         R3(2) = BT/DN
         R3(3) = BP/(DN*SIT)
         ASUM = A3 + A2
         AA = ASUM*A2
         BB = A3*A2
         CC = ASUM*A3
         IS = 2
      END IF
      DO 70 I = 1,3
         DD = R1(I)/AA - R2(I)/BB + R3(I)/CC
         IF (IS.NE.2) THEN
            RT = R1(I) - (AD*R1(I)-BD*R2(I)+CD*R3(I)-DD*ARCJ)*ARCJ
            RA(I) = R1(I)
            R1(I) = R2(I)
            R2(I) = R3(I)
            R3(I) = RT
            VP(I) = VN(I)
         END IF
         RBAR = (R2(I)+R3(I))/2.D0 - DD*AO6
         VN(I) = VP(I) + A3*RBAR
   70 CONTINUE
      GO TO 10
   80 CONTINUE
      RETURN
C
      END
