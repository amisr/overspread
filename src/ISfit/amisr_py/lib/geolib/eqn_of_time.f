
      DOUBLE PRECISION FUNCTION EQN_OF_TIME(MEAN_LONG,IYR)
      IMPLICIT NONE
c
c     $Revision: 1.1 $
c
c     Log: eqn_of_time.f,v
c     Revision 1.4  1997/04/15 16:55:43  baker
c     Added a bit of code to handle the year 2000 problem.
c     If the year is less than 88, we assume it is 2000-something.
c     If the year is between 88 and 99 we assume it is 19xx.
c     If the year is greater than 100 but less than 1900 we
c     assume that it is 1900+the value.  If the year is
c     greater then 1900 we assume it is given correctly in
c     full 4-digit form.
c
c     Revision 1.3  1994/10/17 12:51:20  baker
c     some code had to be rearranged to convince the compiler it was
c     really OK.
c
c Revision 1.2  94/10/17  12:39:41  12:39:41  baker (Kile Baker S1G)
c added ephemerides for 1989 - 1998.  This also required code to be
c added to determine which constants to use.
c
c Revision 1.1  94/10/14  11:27:56  11:27:56  baker (Kile Baker S1G)
c Initial revision
C
C     Imported by Bill Rideout on April 21, 2010
C     Modified by nag_apt to convert all reals to double, int16 to ints
C
C     $Id: eqn_of_time.f,v 1.1 2010/04/21 14:05:01 brideout Exp $
c
c
      SAVE
      DOUBLE PRECISION MEAN_LONG
      INTEGER*4 IYR,INDEX
      DOUBLE PRECISION COEFS(7,10)
      DATA COEFS/-105.8D0,596.2D0,4.4D0,-12.7D0,-429.0D0,-2.1D0,19.3D0,
     +     -105.9D0,596.2D0,4.4D0,-12.7D0,-429.0D0,-2.1D0,19.3D0,
     +     -106.1D0,596.2D0,4.4D0,-12.7D0,-428.9D0,-2.1D0,19.3D0,
     +     -106.2D0,596.2D0,4.4D0,-12.7D0,-428.9D0,-2.1D0,19.3D0,
     +     -106.4D0,596.1D0,4.4D0,-12.7D0,-428.9D0,-2.1D0,19.3D0,
     +     -106.5D0,596.1D0,4.4D0,-12.7D0,-428.8D0,-2.1D0,19.3D0,
     +     -106.6D0,596.1D0,4.4D0,-12.7D0,-428.8D0,-2.1D0,19.3D0,
     +     -106.7D0,596.1D0,4.4D0,-12.7D0,-428.7D0,-2.1D0,19.3D0,
     +     -106.8D0,596.1D0,4.4D0,-12.7D0,-428.7D0,-2.1D0,19.3D0,
     +     -107.0D0,596.1D0,4.4D0,-12.7D0,-428.7D0,-2.1D0,19.3D0/

c
c       compute the index into the coeffiecients table from the year
c
      IF (IYR.LT.88) THEN
          INDEX = IYR + 2000 - 1988
      ELSE IF (IYR.GE.88 .AND. IYR.LT.100) THEN
          INDEX = IYR - 88
      ELSE IF (IYR.GE.100 .AND. IYR.LT.1900) THEN
          INDEX = IYR - 88
      ELSE
          INDEX = IYR - 1988
      END IF

      IF (INDEX.LE.0) INDEX = 1
      IF (INDEX.GT.10) INDEX = 10

      EQN_OF_TIME = COEFS(1,INDEX)*DSIN(MEAN_LONG) +
     +              COEFS(2,INDEX)*DSIN(2.0D0*MEAN_LONG) +
     +              COEFS(3,INDEX)*DSIN(3.0D0*MEAN_LONG) +
     +              COEFS(4,INDEX)*DSIN(4.0D0*MEAN_LONG) +
     +              COEFS(5,INDEX)*DCOS(MEAN_LONG) +
     +              COEFS(6,INDEX)*DCOS(2.0D0*MEAN_LONG) +
     +              COEFS(7,INDEX)*DCOS(3.0D0*MEAN_LONG)

      RETURN
      END
