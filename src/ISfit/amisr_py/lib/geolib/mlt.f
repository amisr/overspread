      DOUBLE PRECISION FUNCTION MLT(IYR,T0,MLONG,MSLONG)
c
c       $Revision: 1.2 $
c
c       This routine implements the calculation of Magnetic Local Time
c
c       It uses the Altitude Adjusted Corrected Geomag. Coordinates to
c       define the magnetic coordinates of the sun and the location of
c       the point where MLT is desired.
c
c       It also uses the equation of time from the Navy Ephemeris to
c       accurately calculate the positon of the sun.
c
c       The Inputs are:
c       iyr (integer*2)  year (may be in the form nn or 19nn or 20nn)
c        (changed by Bill Rideout to use integer*4)
c       t0  (integer*4)  time in seconds since the beginning of year
c       mlong (real*4)   the magnetic longitude of the observation pt.
c        (changed by Bill Rideout to use double precision)

c
c       The outputs are:
c     the function returns a real*4 value giving the MLT time in hours
c     the variable "mslong" returns the magnetic longitude of the sun.
c        (changed by Bill Rideout to use double precision)
c
c       functions called:
c           solar_loc, eqn_of_time, cnv_sec_mdhms, mlt1
c
C     Imported by Bill Rideout on April 21, 2010
C     Modified by nag_apt to convert all reals to double, int16 to ints
C     Also removed attempt to catch real -> integer conversion to
C      catch user error (defeating the point of C type checking)
c
c     $Id: mlt.f,v 1.2 2010/08/11 15:44:16 brideout Exp $
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c       Log:	mlt.f,v
c Revision 1.2  94/10/17  13:27:16  13:27:16  baker (Kile Baker S1G)
c added the year to the call to eqn_of_time
c
c Revision 1.1  94/10/14  11:28:29  11:28:29  baker (Kile Baker S1G)
c Initial revision
c
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      INTEGER*4 IYR,YR
      INTEGER*4 T0
      DOUBLE PRECISION MLONG,MSLONG
      DOUBLE PRECISION MEAN_LONG,DEC
      INTEGER*4 YRDAY,MO,DAY,HR,MINUT,SC
      DOUBLE PRECISION UT,ET,EQN_OF_TIME,APPARENT_TIME
      DOUBLE PRECISION MLT1
C     .. Constants
      PARAMETER (MISSING=1.0D-38)
      SAVE


      YR = IYR
      IF (IYR.GT.1900) YR = IYR - 1900
C     code only valid after 1988
      IF (IYR .LT. 1988) THEN
          MLT = MISSING
          RETURN
      END IF
      IF ((T0.LT.0.0D0) .OR. (T0.GT.86400*366)) THEN
          MLT = MISSING
          RETURN
      END IF
      CALL SOLAR_LOC(YR,T0,MEAN_LONG,DEC)
      ET = EQN_OF_TIME(MEAN_LONG,IYR)
      CALL CNV_SEC_MDHMS(YR,MO,DAY,HR,MINUT,SC,T0)
      UT = (HR*60.0D0+MINUT)*60.D0 + SC
      APPARENT_TIME = UT + ET
      MLT = MLT1(APPARENT_TIME,DEC,MLONG,MSLONG)
      RETURN
      END
