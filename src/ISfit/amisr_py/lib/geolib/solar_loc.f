      SUBROUTINE SOLAR_LOC(YR,T1,MEAN_LONG,DECLINATION)
      IMPLICIT NONE
c
c     $Revision: 1.1 $
c
c
c     This subroutine converts the year and time of the year (from
c     cnv_mdhms_sec) to the mean solar longitude of the sun and the
c     solar declination.
C
C     Imported by Bill Rideout on April 21, 2010
C     Modified by nag_apt to convert all reals to double, int16 to ints
C
C     $Id: solar_loc.f,v 1.1 2010/04/21 14:03:32 brideout Exp $
c
c*****************************************************************
c     Log:	solar_loc.f,v
c Revision 1.2  94/10/17  12:37:51  12:37:51  baker (Kile Baker S1G)
c corrected some of the constants
c
c Revision 1.1  94/10/14  11:29:42  11:29:42  baker (Kile Baker S1G)
c Initial revision
c
c
c*****************************************************************
      INTEGER*4 YR,YR_STEP,DELTA_YR,I,IYR
      INTEGER*4 T1
      INTEGER*4 INDEX
      DOUBLE PRECISION MEAN_LONG,DECLINATION
      DOUBLE PRECISION D,LAMBDA,G,EPS,L
      DOUBLE PRECISION L0(10)
      DOUBLE PRECISION DL
      DOUBLE PRECISION G0(10)
      DOUBLE PRECISION DG
      DOUBLE PRECISION EPS0(10)
      DOUBLE PRECISION DE
      SAVE

      D = 0
      DATA L0/279.642D0,279.403D0,279.165D0,278.926D0,
     +     279.673D0,
     +     279.434D0,279.196D0,278.957D0,279.704D0,279.465D0/

      DL = 0.985647D0
      DATA G0/356.892984D0,356.637087D0,356.381191D0,
     +     356.125295D0,
     +     356.854999D0,356.599102D0,356.343206D0,356.087308D0,
     +     356.817011D0,356.561113D0/

      DG = 0.98560028D0
      DATA EPS0/23.440722D0,23.440592D0,23.440462D0,
     +     23.440332D0,
     +     23.440202D0,23.440072D0,23.439942D0,23.439811D0,
     +     23.439679D0,23.439548D0/

      DE = -0.00000036D0

      IF (YR.LT.1900) THEN
          INDEX = YR - 88
      ELSE
          INDEX = YR - 1988
      END IF
c     print *,'input values = ',yr,t1
c     print *, 'index = ',index
      IF (INDEX.LE.0) THEN
          DELTA_YR = INDEX - 1
      ELSE IF (INDEX.GT.10) THEN
          DELTA_YR = INDEX - 10
      ELSE
          DELTA_YR = 0
      END IF

      IF (INDEX.LE.0) INDEX = 1
      IF (INDEX.GT.10) INDEX = 10

      YR_STEP = SIGN(1,DELTA_YR)
      DELTA_YR = ABS(DELTA_YR)

c     print *,'delta_yr =',delta_yr
      DO I = 1,DELTA_YR
          IF (YR_STEP.GT.0) THEN
              IYR = 98 + I - 1
          ELSE
              IYR = 89 - I
          END IF
          IF (MOD(IYR,4).EQ.0) THEN
              D = D + 366*YR_STEP
          ELSE
              D = D + 365*YR_STEP
          END IF
      END DO
c     print *,'before using t1, d=',d
      D = D + T1/86400.D0
c     print *,'after using t1, d=',d
      L = L0(INDEX) + DL*D
      G = G0(INDEX) + DG*D
      DO WHILE (L.LT.0)
          L = L + 360.D0
      END DO
      DO WHILE (G.LT.0)
          G = G + 360.D0
      END DO
C*PT*WARNING* Constant already double-precision

      L = MOD(L,360.0d0)
C*PT*WARNING* Constant already double-precision
      G = MOD(G,360.0d0)

      LAMBDA = L + 1.915D0*DSIN(G) + 0.020D0*DSIN(2*G)
      EPS = EPS0(INDEX) + DE*D

      DECLINATION = DASIN(DSIN(EPS)*DSIN(LAMBDA))
      MEAN_LONG = L
      RETURN
      END
