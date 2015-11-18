      DOUBLE PRECISION FUNCTION MLT1(T0,SOLAR_DEC,MLONG1,MSLONG)
c
c     $Revision: 1.1 $
c
c     Log: mlt1.f,v
c Revision 1.2  1996/02/09  14:15:16  baker
c This revision explicitly initializes some
c data values and changes the flag condition for
c forcing a complete calculation from 86400
c to 1.0e+12.  Why these changes are necessary
c is not clear, but they seem to cure a problem
c on VAXes and PCs.
c
c Revision 1.1  1994/10/14  11:28:49  baker
c Initial revision
C
C     Imported by Bill Rideout on April 21, 2010
C     Modified by nag_apt to convert all reals to double, int16 to ints
C
C     $Id: mlt1.f,v 1.1 2010/04/21 14:01:50 brideout Exp $
c
c
      IMPLICIT NONE
      DOUBLE PRECISION T0,TOLD,T2
      DOUBLE PRECISION SOLAR_DEC,MLONG1,MSLONG,MSLONG1,MSLONG2,MSLAT1,
     +                 MSLAT2
      DOUBLE PRECISION SLONG,SLONG1,SLONG2,MRAD,MSLAT,SOL_DEC_OLD,HEIGHT
      INTEGER*4 IER,ORDER,MFLAG
      SAVE

      TOLD = 1.0D+12
      SOL_DEC_OLD = 0.0D0

c
c     if the time hasn't changed by more than 10 minutes
c     interpolate the sun's magnetic position.
c
      IF ((ABS(SOLAR_DEC-SOL_DEC_OLD).GT..01D0) .OR.
     +    (SOL_DEC_OLD.EQ.0.0D0)) TOLD = 1.0D+12
      IF (ABS(MSLONG2-MSLONG1).GT.10.0D0) TOLD = 1.0D+12
      IF ((T0.GE.TOLD) .AND. (T0.LT.T2)) THEN
          MSLONG = MSLONG1 + (T0-TOLD)* (MSLONG2-MSLONG1)/ (T2-TOLD)
      ELSE
          TOLD = T0
          SOL_DEC_OLD = SOLAR_DEC
          SLONG1 = (12.D0*3600.D0-T0)*15.D0/3600.D0
          T2 = T0 + 600
          SLONG2 = (12.D0*3600.D0-T2)*15.D0/3600.D0
          HEIGHT = 450.0D0
          ORDER = 4
          MFLAG = 1
          CALL CNVCOORD(SOLAR_DEC,SLONG1,HEIGHT,ORDER,MSLAT1,MSLONG1,
     +                   MRAD,MFLAG,IER)
          CALL CNVCOORD(SOLAR_DEC,SLONG2,HEIGHT,ORDER,MSLAT2,MSLONG2,
     +                   MRAD,MFLAG,IER)
          MSLONG = MSLONG1
      END IF
      MLT1 = (MLONG1-MSLONG)/15.0D0 + 12.0D0
      IF (MLT1.GE.24.0D0) MLT1 = MLT1 - 24.0D0
      IF (MLT1.LT.0.D0) MLT1 = MLT1 + 24.0D0
      RETURN
      END
