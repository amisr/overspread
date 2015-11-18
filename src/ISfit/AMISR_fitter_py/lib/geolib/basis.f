C     Written by Shunrong Zhang
C     Imported into Madrigal from modelRecovery/fullAnalytic/
C     basis.f on 10/21/2005 - Was revision 1.2
C
C     $Id: basis.f,v 1.2 2005/10/25 14:16:18 brideout Exp $
C
      SUBROUTINE POLBAS(N,IX,X,F)
C
C     JMH - 9/88
C
C     POLBAS COMPUTES THE FIRST N MONOMIALS AT X, EG. 1.0,X,X**2 ETC.
C     THE RESULTS ARE RETURNED IN F. IX IS NOT USED. SEE FTL1L2 FOR AN
C     EXAMPLE OF A SUBROUTINE WHICH CALLS BASIS FUNCTION ROUTINES OF
C     THIS FORM.
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
      INTEGER IX,N
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION F(*)
C     ..
C     .. Local Scalars ..
      INTEGER I
C     ..
      F(1) = 1.0D0
      DO 10 I = 2,N
         F(I) = X*F(I-1)
   10 CONTINUE
      RETURN
      END
C
C
      SUBROUTINE HRMBAS(N,IX,X,F)
C
C     JMH - 9/88
C
C     HRMBAS COMPUTES THE FIRST N TERMS OF A HARMONIC EXPANSION AT X, EG
C     1.0, COS(X), SIN(X), COS(2*X), SIN(2*X), ...  THE RESULTS ARE
C     RETURNED IN F. IX IS NOT USED.
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
      INTEGER IX,N
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION F(*)
C     ..
C     .. Local Scalars ..
      INTEGER I
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC COS,SIN
C     ..
      F(1) = 1.0D0
      DO 10 I = 2,N,2
         F(I) = COS((I/2)*X)
   10 CONTINUE
      DO 20 I = 3,N,2
         F(I) = SIN((I/2)*X)
   20 CONTINUE
      RETURN
      END
C
      SUBROUTINE SPLBAS(N,IX,X,F)
C
C     JMH - 9/88
C     JMH - 7/02 - UPDATED
C
C     SPLBAS COMPUTES THE N B-SPLINES OF ORDER K AND KNOT SEQUENCE K AT
C     X.  THE SPLINE AND ITS DERIVATIVES ARE RETURNED IN F. SEE FTL1L2
C     FOR AN EXAMPLE OF A SUBROUTINE WHICH CALLS BASIS FUNCTION
C     ROUTINES OF THIS FORM.  COMMON/SPFTCM/ (IN basis.h) MUST BE SET UP
C     BEFORE SPLBAS IS CALLED.  SEE SUBROUTINE KNOTS1. SPLBAS CALLS
C     SUBROUTINES INTERV AND BSPLVD WHICH ARE FROM CARL DEBOOR'S
C     B-SPLINE PACKAGE.
C
      INCLUDE 'basis.h'
      INCLUDE 'basis1.h'
C
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
      INTEGER IX,N
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION F(NDIM,*)
C     ..
C     .. Local Scalars ..
      INTEGER I,ILEFT,ILFTMK,J,K,MFLAG,MI,NDERIV
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION VAL(4,4)
C     ..
C     .. External Subroutines ..
      EXTERNAL BSPLVD,INTERV
C     ..
      K = 4
      NDERIV = 4
      DO 20 I = 1,N
         DO 10 J = 1,K
            F(I,J) = 0.0D0
   10    CONTINUE
   20 CONTINUE
      CALL INTERV(T,N+K,X,ILEFT,MFLAG)
      IF (ILEFT.GT.N) ILEFT = N
      ILFTMK = ILEFT - K
      CALL BSPLVD(T,K,X,ILEFT,VAL,NDERIV)
      DO 40 MI = 1,K
         I = ILFTMK + MI
         DO 30 J = 1,K
            F(I,J) = VAL(MI,J)
   30    CONTINUE
   40 CONTINUE
      RETURN
C
      END
C
      SUBROUTINE SPLBASP(N,IX,X,F)
C
C     JMH - 2/02
C
C     SPLBASP COMPUTES THE N PERIODIC B-SPLINES OF ORDER K AND KNOT
C     SEQUENCE K AT X.  THE RESULTS OR ONE OF THE FIRST THREE
C     DERIVATIVES ARE RETURNED IN F. IX SPECIFIES THE ORDER OF THE
C     DERIVATIVE, I.E IX=1 FOR THE FIRST DERIVATIVE. SEE FTL1L2 FOR AN
C     EXAMPLE OF A SUBROUTINE WHICH CALLS BASIS FUNCTION ROUTINES OF
C     THIS FORM.  COMMON/SPFTCM/ MUST BE SET UP BEFORE SPLBAS IS
C     CALLED.  SEE SUBROUTINE KNOTSP IN THE MATH LIBRARY FOR AN EXAMPLE
C     OF A ROUTINE WHICH DOES THIS. SPLBASP CALLS SUBROUTINES INTERV
C     AND BSPLVD WHICH ARE FROM CARL DEBOOR'S B-SPLINE PACKAGE AND ARE
C     ALSO IN THE MATH LIBRARY.
C
      INCLUDE 'basis.h'
      INCLUDE 'basis1.h'
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
      INTEGER IX,N
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION F(NDIM,*)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION XP
      INTEGER I,ILEFT,ILFTMK,J,K,MFLAG,MI,NDERIV,NN
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION VAL(4,4)
C     ..
C     .. External Subroutines ..
      EXTERNAL BSPLVD,INTERV
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DMOD
C     ..
      NN = N + 3
      K = 4
      NDERIV = 4
      DO 20 I = 1,NN
         DO 10 J = 1,K
            F(I,J) = 0.0D0
   10    CONTINUE
   20 CONTINUE
      XP = DMOD(X,T(NN+1))
      CALL INTERV(T,NN+K,X,ILEFT,MFLAG)
      IF (ILEFT.GT.NN) ILEFT = NN
      ILFTMK = ILEFT - K
      CALL BSPLVD(T,K,X,ILEFT,VAL,NDERIV)
      DO 40 MI = 1,K
         I = ILFTMK + MI
         IF (I.GT.N) I = I - N
         DO 30 J = 1,K
            F(I,J) = VAL(MI,J)
   30    CONTINUE
   40 CONTINUE
      RETURN
C
      END
C

      SUBROUTINE SPLBASP3(N,IX,X,F)
C
C     SRZ - 7/02
C
C     SPLBASP3 COMPUTES THE 3-DIMENSIONAL TENSOR PRODUCT OF BASIS
C     FUNCTIONS SPLBASP AND SPLBAS.
C
      INCLUDE 'basis.h'
      INCLUDE 'basis1.h'
      INCLUDE 'basis3.h'
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
      INTEGER IX,N
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION F(NDIM,*)
C     ..
C     .. Subroutine Arguments ..
C     ..
C     .. Local Scalars ..
      INTEGER I,IJ,J,L,LL
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION F1(NDIM,4),F2(NDIM,4),F3(NDIM,4)
C     ..
C     .. External Subroutines ..
      EXTERNAL SPLBAS,SPLBASP
C     ..
      KS = 4
      DO 20 I = 1,N
         DO 10 J = 1,KS
            F(I,J) = 0.0D0
   10    CONTINUE
   20 CONTINUE
      DO 30 I = 1,N1 + 4
         T(I) = T1(I)
   30 CONTINUE
      CALL SPLBASP(N1,IX,X1(IX1(IX)),F1)
      DO 40 I = 1,N2 + 4
         T(I) = T2(I)
   40 CONTINUE
      CALL SPLBAS(N2,IX,X2(IX2(IX)),F2)
      DO I = 1,N3 + 4
         T(I) = T3(I)
      END DO
      CALL SPLBASP(N3,IX,X3(IX3(IX)),F3)

      IJ = 0
      DO LL = 1,N3
         DO 70 I = 1,N1
            DO 60 J = 1,N2
               IJ = IJ + 1
               DO 50 L = 1,KS
                  F(IJ,L) = F1(I,L)*F2(J,L)*F3(LL,L)
   50          CONTINUE
   60       CONTINUE
   70    CONTINUE
      END DO
C
C
      RETURN
      END
c
      SUBROUTINE SPLBASP35(N,IX,X,F)
C
C     SRZ - 7/02
C
C     SPLBASP3 COMPUTES THE 3-DIMENSIONAL TENSOR PRODUCT OF BASIS
C     FUNCTIONS SPLBASP AND SPLBAS.
C
      INCLUDE 'basis.h'
      INCLUDE 'basis1.h'
      INCLUDE 'basis3.h'
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
      INTEGER IX,N
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION F(NDIM,*)
C     ..
C     .. Subroutine Arguments ..
C     ..
C     .. Local Scalars ..
      INTEGER I,IJ,J,L,LL
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION F1(NDIM,4),F2(NDIM,4),F3(NDIM,4)
C     ..
C     .. External Subroutines ..
      EXTERNAL POLBAS,SPLBAS,SPLBASP
C     ..
      KS = 4
      DO 20 I = 1,N
         DO 10 J = 1,KS
            F(I,J) = 0.0D0
   10    CONTINUE
   20 CONTINUE
      DO 30 I = 1,N1 + 4
         T(I) = T1(I)
   30 CONTINUE
      CALL SPLBASP(N1,IX,X1(IX1(IX)),F1)
      DO 40 I = 1,N2 + 4
         T(I) = T2(I)
   40 CONTINUE
      CALL SPLBAS(N2,IX,X2(IX2(IX)),F2)
      DO I = 1,N3 + 4
         T(I) = T3(I)
      END DO
      CALL POLBAS(N3,IX,X3(IX3(IX)),F3)

      IJ = 0
      DO LL = 1,N3
         DO 70 I = 1,N1
            DO 60 J = 1,N2
               IJ = IJ + 1
               DO 50 L = 1,KS
                  F(IJ,L) = F1(I,L)*F2(J,L)*F3(LL,L)
   50          CONTINUE
   60       CONTINUE
   70    CONTINUE
      END DO
C
C
      RETURN
      END

      SUBROUTINE SPLBASH3(N,IX,X,F)
C
C     SRZ - 7/02
C
C     SPLBASP3 COMPUTES THE 3-DIMENSIONAL TENSOR PRODUCT OF BASIS
C     FUNCTIONS SPLBASP AND HRMBAS.
C
      INCLUDE 'basis.h'
      INCLUDE 'basis1.h'
      INCLUDE 'basis3.h'
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
      INTEGER IX,N
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION F(NDIM,*)
C     ..
C     .. Subroutine Arguments ..
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION PI
      INTEGER I,IJ,J,L,LL
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION F1(NDIM,4),F2(NDIM,4),F3(NDIM,4)
C     ..
C     .. External Subroutines ..
      EXTERNAL HRMBAS,SPLBAS
C     ..
      KS = 4
      PI = 3.1415926535897d0
      DO 20 I = 1,N
         DO 10 J = 1,KS
            F(I,J) = 0.0D0
   10    CONTINUE
   20 CONTINUE
      DO 30 I = 1,N1 + 4
         T(I) = T1(I)
   30 CONTINUE
      T(1) = 2.d0*PI/T1(1)
      CALL HRMBAS(N1,IX,X1(IX1(IX))*T(1),F1)
      DO 40 I = 1,N2 + 4
         T(I) = T2(I)
   40 CONTINUE
      CALL SPLBAS(N2,IX,X2(IX2(IX)),F2)
      DO I = 1,N3 + 4
         T(I) = T3(I)
      END DO
      T(1) = 2.d0*PI/T3(1)
      CALL HRMBAS(N3,IX,X3(IX3(IX))*T(1),F3)

      IJ = 0
      DO LL = 1,N3
         DO 70 I = 1,N1
            DO 60 J = 1,N2
               IJ = IJ + 1
               DO 50 L = 1,KS
                  F(IJ,L) = F1(I,L)*F2(J,L)*F3(LL,L)
   50          CONTINUE
   60       CONTINUE
   70    CONTINUE
      END DO
C
C
      RETURN
      END


      SUBROUTINE SPLBASP4(N,IX,X,F)
C
C     SRZ - 7/02
C
C     SPLBASP4 COMPUTES THE 4-DIMENSIONAL TENSOR PRODUCT OF BASIS
C     FUNCTIONS HRMBAS, SPLBAS, HRMBAS AND POLBAS
C
C     SRZ -4/05 updated to consider partial derivatives of the second
C               variable (SPLBAS)
C
      INCLUDE 'basis.h'
      INCLUDE 'basis1.h'
      INCLUDE 'basis4.h'
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
      INTEGER IX,N
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION F(NDIM,*)
C     ..
C     .. Subroutine Arguments ..
C     ..
C     .. Local Scalars ..
      INTEGER I,IJ,J,L,L3,L4
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION F1(NDIM,4),F2(NDIM,4),F3(NDIM,4),F4(NDIM,4)
C     ..
C     .. External Subroutines ..
      EXTERNAL HRMBAS,POLBAS,SPLBAS
C     ..
      KS = 4
      DO 20 I = 1,N
         DO 10 J = 1,KS
            F(I,J) = 0.0D0
   10    CONTINUE
   20 CONTINUE
      DO 30 I = 1,N1 + 4
         T(I) = T1(I)
   30 CONTINUE
C     PERIODIC B-SPLINE
C      CALL SPLBASP(N1,IX,X1(IX1(IX)),F1)
C     HARMONIC BASIS
      CALL HRMBAS(N1,IX,X1(IX1(IX)),F1)
      DO 40 I = 1,N2 + 4
         T(I) = T2(I)
   40 CONTINUE
C     B-SPLINE
      CALL SPLBAS(N2,IX,X2(IX2(IX)),F2)
      DO 50 I = 1,N3 + 4
         T(I) = T3(I)
   50 CONTINUE
C     HARMONIC BASIS
      CALL HRMBAS(N3,IX,X3(IX3(IX)),F3)
C     PERIODIC B-SPLINE
C      CALL SPLBASP(N3,IX,X3(IX3(IX)),F3)
      DO 60 I = 1,N4 + 4
         T(I) = T4(I)
   60 CONTINUE
C     B-SPLINE 
      IF (T4(1).NE.12345D0) THEN
          CALL SPLBAS(N4,IX,X4(IX4(IX)),F4)
      ELSE
C     POLYNOMIAL
          CALL POLBAS(N4,IX,X4(IX4(IX)),F4)
      ENDIF

      IJ = 0
      DO 110 L4 = 1,N4
         DO 100 L3 = 1,N3
            DO 90 I = 1,N1
               DO 80 J = 1,N2
                  IJ = IJ + 1
                  DO 70 L = 1,KS
                     F(IJ,L) = F1(I,L)*F2(J,L)*F3(L3,L)*F4(L4,L)
C                    PARTIAL DERIVATIVES FOR  VARIABLE 2
                     IF (L.GT.1) F(IJ,L) = F1(I,1)*F2(J,L)*F3(L3,1)*
     *                                     F4(L4,1)
   70             CONTINUE
   80          CONTINUE
   90       CONTINUE
  100    CONTINUE
  110 CONTINUE
C
C
      RETURN
      END
C
C
      SUBROUTINE KNOTS2(K,N,X1,X2,T)
C
C     JMH -  9/88
C     JMH - 11/01  MODIFIED FROM KNOTS
C
C     F:KNOTS2 - COMPUTES AN ARRAY OF KNOTS FOR A SPLINE FUNCTION.
C                THE FIRST AND LAST INTER-KNOT SPACINGS ARE HALF AS
C                LONG AS THE EQUAL INTERIOR KNOT SPACINGS.
C
C     INPUT PARAMETERS:
C        K      - ORDER (=DEGREE+1) OF B-SPLINES (K=4 FOR CUBIC SPLINES)
C        X1     - LOCATION OF FIRST SPLINE KNOT
C        X2     - LOCATION OF LAST SPLINE KNOT
C
C     OUTPUT PARAMETERS
C        T      - ARRAY OF KNOTS
C        N      - NUMBER OF B-SPLINES OF ORDER K FOR THE GIVEN KNOT
C                 SEQUENCE (N = NBKPT+K-2).
C
C     OTHER VARIABLES
C        NBKPT  - NUMBER OF BREAKPOINTS
C
C     EXAMPLE (K=4, NBKPT=5, N=7):
C
C      I1=1                             I2=8
C        |     |          |         |     |
C        |     |  <-DT->  |         |     |
C        |     |          |         |     |
C        1     5          6         7     8
C        2                                9
C        3                               10
C        4                               11
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION X1,X2
      INTEGER K,N
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION T(*)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION DT,EPS
      INTEGER I,I1,I2,NBKPT
C     ..
C     .. Data statements ..
      DATA EPS/1.D-6/
C     ..
C
      NBKPT = N - K + 2
      DT = (X2-X1)/(NBKPT-2)
      I1 = K
      I2 = NBKPT + K - 1
      T(I1) = X1 - EPS
      T(I1+1) = X1 + 0.5D0*DT
      DO 10 I = I1 + 2,I2 - 1
         T(I) = T(I-1) + DT
   10 CONTINUE
      T(I2) = X2 + EPS
      DO 20 I = I1 - 1,1,-1
         T(I) = T(I1)
   20 CONTINUE
      DO 30 I = I2 + 1,I2 + K - 1,1
         T(I) = T(I2)
   30 CONTINUE
      N = NBKPT + K - 2
      RETURN
      END
C
      SUBROUTINE KNOTSP(K,N,X1,X2,T)
C
C     JMH -  6/02
C
C     F:KNOTSP - COMPUTES AN ARRAY OF KNOTS FOR A PERIODIC SPLINE
C                FUNCTION.
C
C     INPUT PARAMETERS:
C        K      - ORDER (=DEGREE+1) OF B-SPLINES (K=4 FOR CUBIC SPLINES)
C        X1     - LOCATION OF FIRST SPLINE KNOT
C        X2     - LOCATION OF LAST SPLINE KNOT
C
C     OUTPUT PARAMETERS
C        T      - ARRAY OF KNOTS
C        N      - NUMBER OF B-SPLINES OF ORDER K FOR THE GIVEN KNOT
C                 SEQUENCE (N = NBKPT+K-2).
C
C     OTHER VARIABLES
C        NBKPT  - NUMBER OF BREAKPOINTS
C
C     EXAMPLE (K=4, NBKPT=5, N=7):
C
C      I1=1                             I2=8
C        |     |          |         |     |
C        |     |  <-DT->  |         |     |
C        |     |          |         |     |
C        1     5          6         7     8
C        2                                9
C        3                               10
C        4                               11
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION X1,X2
      INTEGER K,N
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION T(*)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION DT,PI,TWOPI
      INTEGER I,I1,I2,J,NBKPT
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DBLE
C     ..
C     .. Data statements ..
      DATA PI/3.141592653D0/,TWOPI/6.283185307D0/
C     ..
C
      PI = PI
      NBKPT = N - K + 5
      DT = TWOPI/DBLE(NBKPT-1)
      I1 = K
      I2 = NBKPT + K - 1
      T(I1) = 0.D0
      DO 10 I = I1 + 1,I2
         T(I) = T(I-1) + DT
   10 CONTINUE
      T(I2) = TWOPI
      DO 20 I = I1 - 1,1,-1
         J = I1 - I
         T(I) = T(I1) - (T(I2)-T(I2-J))
   20 CONTINUE
      DO 30 I = I2 + 1,I2 + K - 1,1
         J = I - I2
         T(I) = T(I2) + (T(I1+J)-T(I1))
   30 CONTINUE
C      DO 40 I = 1,NBKPT + 2*(K-1)
C         WRITE (6,FMT='('' I,T(I) ='',I4,F10.3)') I,T(I)
C     40 CONTINUE
      RETURN
      END
C
      SUBROUTINE INTERV(XT,LXT,X,ILEFT,MFLAG)
C     OMPUTES LARGEST ILEFT IN (1,LXT) SUCH THAT XT(ILEFT) .LE. X
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
      INTEGER ILEFT,LXT,MFLAG
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION XT(LXT)
C     ..
C     .. Local Scalars ..
      INTEGER IHI,ILO,ISTEP,MIDDLE
C     ..
C     .. Data statements ..
      DATA ILO/1/
C     ..
      IHI = ILO + 1
      IF (IHI.LT.LXT) GO TO 10
      IF (X.GE.XT(LXT)) GO TO 120
      IF (LXT.LE.1) GO TO 100
      ILO = LXT - 1
      IHI = LXT
C
   10 IF (X.GE.XT(IHI)) GO TO 50
      IF (X.GE.XT(ILO)) GO TO 110
C
C     **** NOW X .LT. XT(IHI) . FIND LOWER BOUND
   20 ISTEP = 1
   30 IHI = ILO
      ILO = IHI - ISTEP
      IF (ILO.LE.1) GO TO 40
      IF (X.GE.XT(ILO)) GO TO 80
      ISTEP = ISTEP*2
      GO TO 30
   40 ILO = 1
      IF (X.LT.XT(1)) GO TO 100
      GO TO 80
C     **** NOW X .GE. XT(ILO) . FIND UPPER BOUND
   50 ISTEP = 1
   60 ILO = IHI
      IHI = ILO + ISTEP
      IF (IHI.GE.LXT) GO TO 70
      IF (X.LT.XT(IHI)) GO TO 80
      ISTEP = ISTEP*2
      GO TO 60
   70 IF (X.GE.XT(LXT)) GO TO 120
      IHI = LXT
C
C     **** NOW XT(ILO) .LE. X .LT. XT(IHI) . NARROW THE INTERVAL
   80 MIDDLE = (ILO+IHI)/2
      IF (MIDDLE.EQ.ILO) GO TO 110
C     NOTE. IT IS ASSUMED THAT MIDDLE = ILO IN CASE IHI = ILO+1
      IF (X.LT.XT(MIDDLE)) GO TO 90
      ILO = MIDDLE
      GO TO 80
   90 IHI = MIDDLE
      GO TO 80
C     **** SET OUTPUT AND RETURN
  100 MFLAG = -1
      ILEFT = 1
      RETURN
  110 MFLAG = 0
      ILEFT = ILO
      RETURN
  120 MFLAG = 1
      ILEFT = LXT
      RETURN
      END
C
      SUBROUTINE BSPLVD(T,K,X,ILEFT,VNIKX,NDERIV)
C     CALCULATES VALUE AND DERIV.S OF ALL B-SPLINES WHICH DO NOT VANISH
C     AT X.  FILL VNIKX(J,IDERIV), J=IDERIV, ... ,K WITH NONZERO VALUES
C     OF B-SPLINES OF ORDER K+1-IDERIV , IDERIV=NDERIV, ... ,1, BY
C     REPEATED CALLS TO BSPLVN
C     DIMENSION T(ILEFT+K)
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
      INTEGER ILEFT,K,NDERIV
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION T(*),VNIKX(K,NDERIV)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION FACTOR,FKMD,V
      INTEGER I,IDERIV,IPKMD,J,JLOW,JP1MID,KMD,KP1,L,LDUMMY,M,MHIGH
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION A(5,5)
C     ..
C     .. External Subroutines ..
      EXTERNAL BSPLVN
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DBLE,MAX0,MIN0
C     ..
      IDERIV = MAX0(MIN0(NDERIV,K),1)
      KP1 = K + 1
      CALL BSPLVN(T,KP1-IDERIV,1,X,ILEFT,VNIKX)
      IF (IDERIV.EQ.1) GO TO 110
      MHIGH = IDERIV
      DO 20 M = 2,MHIGH
         JP1MID = 1
         DO 10 J = IDERIV,K
            VNIKX(J,IDERIV) = VNIKX(JP1MID,1)
            JP1MID = JP1MID + 1
   10    CONTINUE
         IDERIV = IDERIV - 1
         CALL BSPLVN(T,KP1-IDERIV,2,X,ILEFT,VNIKX)
   20 CONTINUE
C
      JLOW = 1
      DO 40 I = 1,K
         DO 30 J = JLOW,K
            A(I,J) = 0.D0
   30    CONTINUE
         JLOW = I
         A(I,I) = 1.D0
   40 CONTINUE
      KMD = K
      DO 100 M = 2,MHIGH
         KMD = KMD - 1
         FKMD = DBLE(KMD)
         I = ILEFT
         J = K
         DO 60 LDUMMY = 1,KMD
            IPKMD = I + KMD
            FACTOR = FKMD/(T(IPKMD)-T(I))
            DO 50 L = 1,J
               A(L,J) = (A(L,J)-A(L,J-1))*FACTOR
   50       CONTINUE
            I = I - 1
            J = J - 1
   60    CONTINUE
C
   70    DO 90 I = 1,K
            V = 0.D0
            JLOW = MAX0(I,M)
            DO 80 J = JLOW,K
               V = A(I,J)*VNIKX(J,M) + V
   80       CONTINUE
            VNIKX(I,M) = V
   90    CONTINUE
  100 CONTINUE
  110 RETURN
      END
C
      SUBROUTINE BSPLVN(T,JHIGH,INDEX,X,ILEFT,VNIKX)
C     ALCULATES THE VALUE OF ALL POSSIBLY NONZERO B-SPLINES AT 'X' OF
C     ORDER MAX(JHIGH,(J+1)(INDEX-1)) ON 'T'.
C     DIMENSION T(ILEFT+JHIGH)
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
      INTEGER ILEFT,INDEX,JHIGH
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION T(*),VNIKX(JHIGH)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION VM,VMPREV
      INTEGER IMJP1,IPJ,J,JP1,JP1ML,L
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION DELTAM(5),DELTAP(5)
C     ..
C     .. Data statements ..
      DATA J/1/
C     ..
C     ONTENT OF J, DELTAM, DELTAP IS EXPECTED UNCHANGED BETWEEN CALLS.
      GO TO (10,20) INDEX
   10 J = 1
      VNIKX(1) = 1.D0
      IF (J.GE.JHIGH) GO TO 40
C
   20 IPJ = ILEFT + J
      DELTAP(J) = T(IPJ) - X
      IMJP1 = ILEFT - J + 1
      DELTAM(J) = X - T(IMJP1)
      VMPREV = 0.D0
      JP1 = J + 1
      DO 30 L = 1,J
         JP1ML = JP1 - L
         VM = VNIKX(L)/(DELTAP(L)+DELTAM(JP1ML))
         VNIKX(L) = VM*DELTAP(L) + VMPREV
         VMPREV = VM*DELTAM(JP1ML)
   30 CONTINUE
      VNIKX(JP1) = VMPREV
      J = JP1
      IF (J.LT.JHIGH) GO TO 20
C
   40 RETURN
      END



      SUBROUTINE SPLBAS3(N,IX,X,F)
C
C     SRZ - 4/05
C
C     SPLBAS3 COMPUTES THE 3-DIMENSIONAL TENSOR PRODUCT OF BASIS
C     FUNCTIONS HRMBAS, SPLBAS AND HRMBAS
C
      INCLUDE 'basis.h'
      INCLUDE 'basis1.h'
      INCLUDE 'basis3.h'
C
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
      INTEGER IX,N
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION F(NDIM,*)
C     ..
C     .. Subroutine Arguments ..
C     ..
C     .. Local Scalars ..
      INTEGER I,IJ,J,L,L3
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION F1(NDIM,4),F2(NDIM,4),F3(NDIM,4)
C     ..
C     .. External Subroutines ..
      EXTERNAL HRMBAS,SPLBAS
C     ..
      KS = 4
      DO 20 I = 1,N
         DO 10 J = 1,KS
            F(I,J) = 0.0D0
   10    CONTINUE
   20 CONTINUE
      DO 30 I = 1,N1 + 4
         T(I) = T1(I)
   30 CONTINUE
C     PERIODIC B-SPLINE
C      CALL SPLBASP(N1,IX,X1(IX1(IX)),F1)
C     HARMONIC BASIS
      CALL HRMBAS(N1,IX,X1(IX1(IX)),F1)
      DO 40 I = 1,N2 + 4
         T(I) = T2(I)
   40 CONTINUE
C     B-SPLINE
      CALL SPLBAS(N2,IX,X2(IX2(IX)),F2)
      DO 50 I = 1,N3 + 4
         T(I) = T3(I)
   50 CONTINUE
C     HARMONIC BASIS
      CALL HRMBAS(N3,IX,X3(IX3(IX)),F3)
C     PERIODIC B-SPLINE
C      CALL SPLBASP(N3,IX,X3(IX3(IX)),F3)

      IJ = 0
         DO 100 L3 = 1,N3
            DO 90 I = 1,N1
               DO 80 J = 1,N2
                  IJ = IJ + 1
                  DO 70 L = 1,KS
                     F(IJ,L) = F1(I,L)*F2(J,L)*F3(L3,L)
C                    PARTIAL DERIVATIVES FOR  VARIABLE 2
                     IF (L.GT.1) F(IJ,L) = F1(I,1)*F2(J,L)*F3(L3,1)
   70             CONTINUE
   80          CONTINUE
   90       CONTINUE
  100    CONTINUE
C
C
      RETURN
      END
C
C
