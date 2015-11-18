      SUBROUTINE RYLM(D_COLAT,D_LON,I_ORDER,D_YLMVAL)
c
c     cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     $Revision: 1.1 $
c
c     The initial version of this subroutine was written by RADEX, INC.
c     for VAX/VMS systems.
c
c     Subsequent revisions for use with POSIX compliant systems
c     have been made by KBB at JHU/APL.  These revisions have
c     been managed using RCS, and a log of these revisions
c     follows.
c
c     cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     RCS   Revision History
c
c     $Log: rylm.f,v $
c     Revision 1.1  2004/07/08 18:01:55  brideout
c     files added to support AACGM conversion
c
c     Revision 1.3  1996/03/11  19:25:07  baker
c     Revisions for 1995 version of AACGM
c
c     Revision 1.2  1994/10/14  10:50:45  baker
c     Added the SAVE instruction to make the variables static.
c
c     Revision 1.1  94/10/12  15:24:21  15:24:21  baker (Kile Baker S1G)
c     Initial revision
c
c
c     cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
C     Purpose:
C
C         This subroutine computes the array of real spherical harmonic
C         function values Y(L,M) for a given colatitude and longitude up
C         to order I_ORDER. The function values are stored in the one-
C         dimensional array D_YLMVAL(N). The indexing scheme used is as
C         follows:
C
C     L    0  1  1  1  2  2  2  2  2  3  3  3  3  3  3  3  4
C     M    0 -1  0  1 -2 -1  0  1  2 -3 -2 -1  0  1  2  3 -4  etc.
C
C     N    1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
C
C     Input Arguments:
C
C        D_COLAT             - Double Precision  - The colatitude of the
C                               point for which the spherical harmonic
C                               Y(L,M) will be calculated
C
C         D_LON               - Double Precision  - The longitude of the
C                               point for which the spherical harmonic
C                               Y(L,M) will be calculated
C
C         I_ORDER             - Integer  - The order of the spherical
C                               harmonic function expansion. The total
C                               number of terms computed will be
C                               (I_ORDER + 1) * (I_ORDER + 1)
C
C     Output Argument:
C
C     D_YLMVAL            - Double Precision array of spherical harmonic
C                               functions at the point (D_COLAT, D_LON)
C
C     Local Variables:
C
C         Q_FAC               - Double Precision Complex number used for
C                               recursive computation of the spherical
C                               harmonic function values. Represents the
C                               longitude dependence in the recursion
C                               computation.
C
C         Q_VAL               - Double Precision Complex number used to
C                               store spherical harmonic function for
C                               current L and M values for recursive
C                               computation for the next set of L and M
C                               values
C
C         COS_THETA           - Double Precision Cosine of D_COLAT
C         SIN_THETA           - Double Precision Sine of D_COLAT
C                           used to compute Spherical Harmonic functions
C
C         COS_LON             - Double Precision Cosine of Longitude
C         SIN_LON             - Double Precision Sine of Longitude
C                           used to compute Spherical Harmonic functions
C
C        CA, CB              - Double Precision Coefficients used in the
C                               recursion computations
C
C        LA, LB, LC,         - Integer - Pointers for the D_YLMVAL array
C         LD, LE, LF            corresponding to values of Y(L,M) for
C                               particular values of L, M for use in the
C                               recursive computation of Y(L.M)
C
C
C     Constants:  None
C
C     Subroutines Required: None
C
C     Files Used at Compile Time:  None
C
C     Files Used at Run Time:  None
C
C     Databases Accessed:  None
C
C     Warnings:  None
C
C     Revision History:
C
C     Written by Radex, Inc., 3 Preston Court, Bedford, MA 01730
C
C
c
C
C
C
C     Local Variables:
C
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION D_COLAT,D_LON
      INTEGER I_ORDER
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION D_YLMVAL(*)
C     ..
C     .. Local Scalars ..
      DOUBLE COMPLEX Q_FAC,Q_VAL
      DOUBLE PRECISION CA,CB,COS_LON,COS_THETA,FAC,SIN_LON,SIN_THETA
      INTEGER L,LA,LB,LC,LD,LE,LF,M
C     ..
C     .. External Functions ..
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DCMPLX,DCOS,DIMAG,DSIN,DBLE
C     ..
C     .. Save statement ..
      SAVE
C     ..
      COS_THETA = DCOS(D_COLAT)
      SIN_THETA = DSIN(D_COLAT)
C
      COS_LON = DCOS(D_LON)
      SIN_LON = DSIN(D_LON)
C
      Q_FAC = -SIN_THETA*DCMPLX(COS_LON,SIN_LON)
C
C     Generate Zonal Harmonics (Y(L,0), L = 1, I_ORDER)
C     using Standard Recursion Relations (Equation 6.8.7, p 247,
C     Numerical Recipes in Fortran, 2. edition, by Press. W. et. al.
C     Cambridge University Press, 1992) for case for which m = 0
C
C
      D_YLMVAL(1) = 1.0D0
      D_YLMVAL(3) = COS_THETA
C
      DO 10 L = 1,I_ORDER - 1
         LA = (L-1)*L + 1
         LB = L*(L+1) + 1
         LC = (L+1)*(L+2) + 1
C
         CA = (2.0D0*L+1.0D0)/(L+1.0D0)
         CB = DBLE(L)/(L+1.0D0)
C
         D_YLMVAL(LC) = CA*COS_THETA*D_YLMVAL(LB) - CB*D_YLMVAL(LA)
C
   10 CONTINUE
C
C     Generate Y(L,L) for L = 1 to I_ORDER. Algorithm based upon
C     equation (6.8.8) in Press et al, but incorporates longitude
C     dependence.
C
      Q_VAL = Q_FAC
C
      D_YLMVAL(4) = DBLE(Q_VAL)
      D_YLMVAL(2) = -DIMAG(Q_VAL)
      DO 20 L = 2,I_ORDER
C
         Q_VAL = (2.0D0*L-1.0D0)*Q_FAC*Q_VAL
C
         LA = L*L + 2*L + 1
         LB = L*L + 1
C
         D_YLMVAL(LA) = DBLE(Q_VAL)
         D_YLMVAL(LB) = -DIMAG(Q_VAL)
C
   20 CONTINUE
C
C     Generate Y(L+1,L) to (Y(I_ORDER,L) function values.
C     Algorithm based upon equation (6.8.9) in Press et al, but
C     incorporates longitude dependence.
C
      DO 30 L = 2,I_ORDER
C
         LA = L*L
         LB = L*L - 2*(L-1)
C
         LC = L*L + 2*L
         LD = L*L + 2
C
         FAC = 2.0D0*L - 1.0D0
C
         D_YLMVAL(LC) = FAC*COS_THETA*D_YLMVAL(LA)
         D_YLMVAL(LD) = FAC*COS_THETA*D_YLMVAL(LB)
C
   30 CONTINUE
C
C     Generate remaining Y(L+2,L) to YL(I_ORDER,L) function values.
C     Algorithm based upon equation (6.8.7) in Press et al, but
C     incorporates longitude dependence.
C
      DO 50 M = 1,I_ORDER - 2
C
C        INITIALIZATION OF POINTERS FOR CURRENT VALUE OF M
C
         LA = (M+1)**2
         LB = (M+2)**2 - 1
         LC = (M+3)**2 - 2
C
         LD = LA - 2*M
         LE = LB - 2*M
         LF = LC - 2*M
C
         DO 40 L = M + 2,I_ORDER
C
C
            CA = DBLE(2*L-1)/DBLE(L-M)
            CB = DBLE(L+M-1)/DBLE(L-M)
C
            D_YLMVAL(LC) = CA*COS_THETA*D_YLMVAL(LB) - CB*D_YLMVAL(LA)
C
            D_YLMVAL(LF) = CA*COS_THETA*D_YLMVAL(LE) - CB*D_YLMVAL(LD)
C
C           UPDATE POINTERS FOR NEXT M VALUE
C
            LA = LB
            LB = LC
            LC = LB + 2*L + 2
C
            LD = LA - 2*M
            LE = LB - 2*M
            LF = LC - 2*M
C
   40    CONTINUE
C
   50 CONTINUE
C
C
C
      RETURN
      END
