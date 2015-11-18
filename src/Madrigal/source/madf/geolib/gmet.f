C     $Id: gmet.f,v 1.2 2001/03/12 23:46:47 sjc Exp $
C
      SUBROUTINE GMET(DXDQ,G)
C
C     jmh - 10/79  ans fortran 66
C
C     GMET calculates the metric tensor G of a coordinate system q
C     for which dx(i)/dq(j)=dxdq(i,j).
C
C       Input:
C         DXDQ - coordinate system array.
C
C      Output:
C            G - metric tensor.
C
C     .. Array Arguments ..
      DOUBLE PRECISION DXDQ(3,3),G(3,3)
C     ..
C     .. Local Scalars ..
      INTEGER I,J,K
C     ..
      DO 30 I = 1,3
         DO 20 J = 1,3
            G(I,J) = 0.D0
            DO 10 K = 1,3
               G(I,J) = G(I,J) + DXDQ(K,I)*DXDQ(K,J)
   10       CONTINUE
   20    CONTINUE
   30 CONTINUE
      RETURN
C
      END
