C     $Id: sunstuf.f,v 1.2 2001/03/22 18:17:28 sjc Exp $
C
      SUBROUTINE SUNPOS(RDAY,MONTH,YEAR,ECLON)
C
C     Routine is an attempt at computing Ecliptic longitude, but,
C     doesn't work.. Some sort of fossil? - sjc 03/16/01
C
C       Input:
C           DAY - day of month
C         MONTH - month of year
C          YEAR - year
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION ECLON,RDAY
      INTEGER MONTH,YEAR
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION D,DTR,ECCEN,ECLONE,ECLONP,R0,RJDEPC,RM,THETA0
C     ..
C     .. External Functions ..
      DOUBLE PRECISION RJDF
      EXTERNAL RJDF
C     ..
C     .. Data statements ..
      DATA ECLONE/278.833540D0/
      DATA ECLONP/282.596403D0/
      DATA ECCEN/0.016718D0/
      DATA R0/1.495985D08/
      DATA THETA0/0.533128D0/
      DATA RJDEPC/2444238.5D0/
      DATA DTR/.0174532925199D0/
C     ..
C
      D = RJDF(RDAY,MONTH,YEAR) - RJDEPC
      RM = 360.D0*D/365.2422D0 + ECLONE - ECLONP
      RM = DTR*RM
      RETURN
      END
C
      SUBROUTINE ECTOEQ(ECLAT,ECLON,ECEPS,RASCEN,DECLIN)
C
C     Convert from ecliptic to equatorial coordinates.
C
C       Input:
C         ECLAT - ecliptic latitude
C         ECLON - ecliptic longtitude
C         ECEPS - obliquity of the ecliptic
C
C      Output:
C        RASCEN - right ascension (hours)
C        DECLIN - declination (deg)
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION DECLIN,ECEPS,ECLAT,ECLON,RASCEN
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION CECEPS,CECLAT,CECLON,DTR,ECEPSR,ECLATR,ECLONR,
     *                 SECEPS,SECLAT,SECLON,TECLAT
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ASIN,DATAN2,DCOS,DSIN
C     ..
C     .. Data statements ..
      DATA DTR/.0174532925199D0/
C     ..
C
      ECLATR = DTR*ECLAT
      ECLONR = DTR*ECLON
      ECEPSR = DTR*ECEPS
      SECLAT = DSIN(ECLATR)
      CECLAT = DCOS(ECLATR)
      TECLAT = SECLAT/CECLAT
      SECLON = DSIN(ECLONR)
      CECLON = DCOS(ECLONR)
      SECEPS = DSIN(ECEPSR)
      CECEPS = DCOS(ECEPSR)
      RASCEN = DATAN2(SECLON*CECEPS-TECLAT*SECEPS,CECLON)/(DTR*15.0D0)
      IF (RASCEN.LT.0.0D0) RASCEN = RASCEN + 24.0D0
      DECLIN = ASIN(SECLAT*CECEPS+CECLAT*SECEPS*SECLON)/DTR
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION RJDF(RDAY,MONTH,YEAR)
C
C     Computes Julian day from DAY, MONTH and YEAR
C
C       Input:
C           DAY - day of month
C         MONTH - month of year
C          YEAR - year
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION RDAY
      INTEGER MONTH,YEAR
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION A,B,C,D
      INTEGER M,Y
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC INT
C     ..
      Y = YEAR
      M = MONTH
      IF (M.EQ.1 .OR. M.EQ.2) THEN
         Y = Y - 1
         M = M + 12
      END IF
      A = Y/100
      IF (YEAR.GT.1582 .OR. (YEAR.EQ.1582.AND.MONTH.GT.10) .OR.
     *    (YEAR.EQ.1582.AND.MONTH.EQ.10.AND.RDAY.GT.15)) THEN
         B = 2 - A + INT(A/4)
      ELSE
         B = 0.D0
      END IF
      C = INT(365.25D0*Y)
      D = INT(30.6001D0*(M+1))
      RJDF = B + C + D + RDAY + 1720994.5D0
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION OBLIQF(RDAY,MONTH,YEAR)
C
C     Computes obliquity of the ecliptic at DAY, MONTH and YEAR
C
C       Input:
C          RDAY - day of month
C         MONTH - month of year
C          YEAR - year
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION RDAY
      INTEGER MONTH,YEAR
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION RJD,RJDREF,T
C     ..
C     .. External Functions ..
      DOUBLE PRECISION RJDF
      EXTERNAL RJDF
C     ..
C     .. Data statements ..
      DATA RJDREF/2415020.0D0/
C     ..
C
      RJD = RJDF(RDAY,MONTH,YEAR)
      T = (RJD-RJDREF)/36525.0D0
      OBLIQF = 23.452294D0 - (T*(0.0130125D0+T*(0.00000164D0-
     *         T*0.000000503D0)))
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION GMLONF(RDAY,MONTH,YEAR)
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION RDAY
      INTEGER MONTH,YEAR
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION RJD,RJDREF,T
      INTEGER N360
C     ..
C     .. External Functions ..
      DOUBLE PRECISION RJDF
      EXTERNAL RJDF
C     ..
C     .. Data statements ..
      DATA RJDREF/2415020.0D0/
C     ..
C
      RJD = RJDF(RDAY,MONTH,YEAR)
      T = (RJD-RJDREF)/36525.0D0
      GMLONF = 279.696678D0 + (T*(129602768.13D0+T*(1.089D0)))/3600.D0
      N360 = GMLONF/360.D0
      GMLONF = GMLONF - N360*360
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION PMLONF(RDAY,MONTH,YEAR)
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION RDAY
      INTEGER MONTH,YEAR
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION RJD,RJDREF,T
C     ..
C     .. External Functions ..
      DOUBLE PRECISION RJDF
      EXTERNAL RJDF
C     ..
C     .. Data statements ..
      DATA RJDREF/2415020.0D0/
C     ..
C
      RJD = RJDF(RDAY,MONTH,YEAR)
      T = (RJD-RJDREF)/36525.0D0
      PMLONF = 281.220833D0 + (T*(6189.03D0+T*(1.63D0+T*0.012D0)))/
     *         3600.D0
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION ECCENF(RDAY,MONTH,YEAR)
C
C     Computes eccentricity from date.
C
C       Input:
C          RDAY - day of month
C         MONTH - month of year
C          YEAR - year
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION RDAY
      INTEGER MONTH,YEAR
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION RJD,RJDREF,T
C     ..
C     .. External Functions ..
      DOUBLE PRECISION RJDF
      EXTERNAL RJDF
C     ..
C     .. Data statements ..
      DATA RJDREF/2415020.0D0/
C     ..
C
      RJD = RJDF(RDAY,MONTH,YEAR)
      T = (RJD-RJDREF)/36525.0D0
      ECCENF = 0.01675104D0 - T*(0.00004180D0+T*0.000000126D0)
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION ANMLYF(RDAY,MONTH,YEAR)
C
C     Computes mean anomoly from date.
C
C       Input:
C          RDAY - day of month
C         MONTH - month of year
C          YEAR - year
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION RDAY
      INTEGER MONTH,YEAR
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION RJD,RJDREF,T
      INTEGER N360
C     ..
C     .. External Functions ..
      DOUBLE PRECISION RJDF
      EXTERNAL RJDF
C     ..
C     .. Data statements ..
      DATA RJDREF/2415020.0D0/
C     ..
C
      RJD = RJDF(RDAY,MONTH,YEAR)
      T = (RJD-RJDREF)/36525.0D0
      ANMLYF = 358.475845D0 + (T*(129596579.10D0-T*(0.54D0+T*0.012D0)))/
     *         3600.D0
      N360 = ANMLYF/360.D0
      ANMLYF = ANMLYF - N360*360
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION GSTF(UT,RDAY,MONTH,YEAR)
C
C     Computes sidereal time at Greenwich from Universal time
C     and date.
C
C       Input:
C            UT - Universal time
C          RDAY - day of month
C         MONTH - month of year
C          YEAR - year
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION RDAY,UT
      INTEGER MONTH,YEAR
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION A,B,C,D,R,RJD,RJD0,RJDAY0,RJDREF,T,T0,T1,U
C     ..
C     .. External Functions ..
      DOUBLE PRECISION RJDF
      EXTERNAL RJDF
C     ..
C     .. Data statements ..
      DATA A/0.0657098D0/,C/1.002738D0/,D/0.997270D0/
      DATA RJDREF/2415020.0D0/
C     ..
C
      RJD = RJDF(RDAY,MONTH,YEAR)
      RJDAY0 = 0.0D0
      RJD0 = RJDF(RJDAY0,1,YEAR)
      T = (RJD0-RJDREF)/36525.0D0
      R = 6.6460656D0 + T*(2400.051262D0+T*0.00002581D0)
      U = R - (24*(YEAR-1900))
      B = 24.0D0 - U
      D = RJD - RJD0
      T0 = A*D - B
      T1 = T0 + C*UT
      IF (T1.GT.24.0D0) T1 = T1 - 24.D0
      IF (T1.LT.0.0D0) T1 = T1 + 24.D0
      GSTF = T1
      RETURN
      END
C
      SUBROUTINE KEPLER(RM,ECCEN,RNU)
C
C     Computes the angle RNU that the radius vector makes with the
C     major axis of a eliptic orbit defined by mean anomoly and 
C     eccentricity.
C
C       Input:
C           RM - mean anomaly
C        ECCEN - eccentricity
C
C      Output:
C         RNU - true anomaly
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION ECCEN,RM,RNU
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION DE,DELTA,DTR,E,EPS,RMR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ATAN,DCOS,DSIN,DSQRT,TAN
C     ..
C     .. Data statements ..
      DATA EPS/1.D-10/
      DATA DTR/.0174532925199D0/
C     ..
C
      RMR = DTR*RM
      E = RMR
   10 CONTINUE
      DELTA = E - ECCEN*DSIN(E) - RMR
      DE = DELTA/(1.D0-ECCEN*DCOS(E))
      E = E - DE
      IF (DELTA.GT.EPS) GO TO 10
      RNU = 2.0D0*ATAN(DSQRT((1.D0+ECCEN)/(1.D0-ECCEN))*TAN(E/2.D0))/DTR
      RETURN
      END
