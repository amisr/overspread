C     $Id: sun.f,v 1.2 2001/03/22 18:17:25 sjc Exp $
C
      SUBROUTINE SUN(DAY,MONTH,YEAR,UT,GLON,RASCEN,DECLIN,HRANGL)
C
C     jmh - 10/83
C
C     Computes right ascension, declination and hour-angle given
C     date, time and longitude.
C
C       Input:
C           DAY - day of month
C         MONTH - month of year
C          YEAR - year
C            UT - universal time (hours)
C          GLON - longitude (deg)
C
C      Output:
C        RASCEN - right ascension (hours)
C        DECLIN - declination (deg)
C        HRANGL - hour angle (hours)
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION DECLIN,GLON,HRANGL,RASCEN,UT
      INTEGER DAY,MONTH,YEAR
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION ANMLY,ECCEN,ECLON,GST,OBLIQ,PMLON,RDAY,RNU
C     ..
C     .. External Functions ..
      DOUBLE PRECISION ANMLYF,ECCENF,GSTF,OBLIQF,PMLONF
      EXTERNAL ANMLYF,ECCENF,GSTF,OBLIQF,PMLONF
C     ..
C     .. External Subroutines ..
      EXTERNAL ECTOEQ,KEPLER
C     ..
      RDAY = DAY
      ANMLY = ANMLYF(RDAY,MONTH,YEAR)
      ECCEN = ECCENF(RDAY,MONTH,YEAR)
      PMLON = PMLONF(RDAY,MONTH,YEAR)
      OBLIQ = OBLIQF(RDAY,MONTH,YEAR)
      CALL KEPLER(ANMLY,ECCEN,RNU)
      ECLON = RNU + PMLON
      CALL ECTOEQ(0.0D0,ECLON,OBLIQ,RASCEN,DECLIN)
      GST = GSTF(UT,RDAY,MONTH,YEAR)
      HRANGL = GST + GLON/15.D0 - RASCEN
      IF (HRANGL.LT.0.0D0) HRANGL = HRANGL + 24.0D0
      RETURN
      END
