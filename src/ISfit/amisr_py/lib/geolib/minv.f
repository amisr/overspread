C     $Id: minv.f,v 1.2 2001/03/16 20:20:48 sjc Exp $
C
      SUBROUTINE MINV(A,N,D,L,M)
C
C     Inverts general matrix A (overwrites A) using standard
C     gauss-jordan method. The determinant is also calculated. a
C     determinant of zero indicates that the matrix is singular.
C
C      Input:
C        n - order of matrix a
C        l - work vector of length n
C        m - work vector of length n
C
C      Input, Output:
C        a - input matrix, destroyed in computation and replaced by
C            resultant inverse.
C
C      Output:
C        d - resultant determinant
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION D
      INTEGER N
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION A(*)
      INTEGER L(*),M(*)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION BIGA,HOLD
      INTEGER I,IJ,IK,IZ,J,JI,JK,JP,JQ,JR,K,KI,KJ,KK,NK
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS
C     ..
C     search for largest element
      D = 1.0D0
      NK = -N
      DO 90 K = 1,N
         NK = NK + N
         L(K) = K
         M(K) = K
         KK = NK + K
         BIGA = A(KK)
         DO 20 J = K,N
            IZ = N*(J-1)
            DO 10 I = K,N
               IJ = IZ + I
               IF (ABS(BIGA).LT.ABS(A(IJ))) THEN
                  BIGA = A(IJ)
                  L(K) = I
                  M(K) = J
               END IF
   10       CONTINUE
   20    CONTINUE
C
C        interchange rows
C
         J = L(K)
         IF (J.GT.K) THEN
            KI = K - N
            DO 30 I = 1,N
               KI = KI + N
               HOLD = -A(KI)
               JI = KI - K + J
               A(KI) = A(JI)
               A(JI) = HOLD
   30       CONTINUE
         END IF
C
C        interchange columns
C
         I = M(K)
         IF (I.GT.K) THEN
            JP = N*(I-1)
            DO 40 J = 1,N
               JK = NK + J
               JI = JP + J
               HOLD = -A(JK)
               A(JK) = A(JI)
               A(JI) = HOLD
   40       CONTINUE
         END IF
C
C        divide column by minus pivot (value of pivot element is
C        contained in biga)
C
         IF (BIGA.EQ.0) GO TO 130
         DO 50 I = 1,N
            IF (I.NE.K) THEN
               IK = NK + I
               A(IK) = A(IK)/(-BIGA)
            END IF
   50    CONTINUE
C
C        reduce matrix
C
         DO 70 I = 1,N
            IK = NK + I
            HOLD = A(IK)
            IJ = I - N
            DO 60 J = 1,N
               IJ = IJ + N
               IF (I.NE.K) THEN
                  IF (J.NE.K) THEN
                     KJ = IJ - I + K
                     A(IJ) = HOLD*A(KJ) + A(IJ)
                  END IF
               END IF
   60       CONTINUE
   70    CONTINUE
C
C        divide row by pivot
C
         KJ = K - N
         DO 80 J = 1,N
            KJ = KJ + N
            IF (J.NE.K) A(KJ) = A(KJ)/BIGA
   80    CONTINUE
C
C        product of pivots
C
         D = D*BIGA
C
C        replace pivot by reciprocal
C
         A(KK) = 1.0D0/BIGA
   90 CONTINUE
C
C        final row and column interchange
C
      K = N
  100 CONTINUE
      K = (K-1)
      IF (K.LE.0) GO TO 140
      I = L(K)
      IF (I.GT.K) THEN
         JQ = N*(K-1)
         JR = N*(I-1)
         DO 110 J = 1,N
            JK = JQ + J
            HOLD = A(JK)
            JI = JR + J
            A(JK) = -A(JI)
            A(JI) = HOLD
  110    CONTINUE
      END IF
      J = M(K)
      IF (J.GT.K) THEN
         KI = K - N
         DO 120 I = 1,N
            KI = KI + N
            HOLD = A(KI)
            JI = KI - K + J
            A(KI) = -A(JI)
            A(JI) = HOLD
  120    CONTINUE
      END IF
      GO TO 100
  130 CONTINUE
      D = 0.0D0
  140 CONTINUE
      RETURN
      END
