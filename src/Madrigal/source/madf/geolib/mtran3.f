C     $Id: mtran3.f,v 1.2 2001/03/16 20:20:49 sjc Exp $
C
      SUBROUTINE MTRAN3(A)
C
C     jmh - 1/29/80  ans fortran 66
C
C     MTRAN3 calculates the transpose of a 3 x 3 matrix a
C
C       Input, Output:
C           A - input matrix, replaced with transpose.
C
C     .. Array Arguments ..
      DOUBLE PRECISION A(3,3)
C     ..
C     .. Local Scalars ..
      INTEGER I,J
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION B(3,3)
C     ..
      DO 20 I = 1,3
         DO 10 J = 1,3
            B(I,J) = A(J,I)
   10    CONTINUE
   20 CONTINUE
      DO 40 I = 1,3
         DO 30 J = 1,3
            A(I,J) = B(I,J)
   30    CONTINUE
   40 CONTINUE
      RETURN
C
      END
