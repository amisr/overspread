      SUBROUTINE ALTITUDE_TO_CGM(R_HEIGHT_IN,R_LAT_ALT,R_LAT_ADJ)
c    ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     Original version writen by RADEX, INC. for use on VAX/VMS systems.
c     The 1990 version of sfc_convert_geo_coord used a subroutine
c     called 'cg_alt_dip'.  This subroutine has been replaced
c     by two subroutines, cgm_to_altitude, and altitude_to_cgm.
c
c     Initial version for POSIX compliant systems made by KBB
c     at the Johns Hopkins Univ. Applied Physics Laboratory.
c     These revisions have been managed using the Revision Control
c     System (RCS).  The log of revisions follows:
c
c     cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     $Log: altitude_to_cgm.f,v $
c     Revision 1.1  2004/07/08 18:01:53  brideout
c     files added to support AACGM conversion
c
c     Revision 1.1  1996/03/11  19:23:22  baker
c     Initial revision
c
c
c     cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Purpose:
C
C     Computes Latitude Adjustment for R_HEIGHT_IN for use with the
C     Spherical Harmonic expansion computation.
C
C     Input Arguments:
C
C         R_HEIGHT_IN - Single Precision - Input height in km above
C                       earth mean radius
C
C         R_LAT_ALT   - Single Precision - R_HEIGHT_IN Corrected Dipole
C                       Latitude
C
C     Output Arguments:
C
C         R_LAT_ADJ   - Single Precision
C                       Corrected Geomagnetic Latitude for IFLAG = 1
C                       Height Adjusted Corrected Dipole Coordinates
C                       for IFLAG = 2
C
C     Local Variables:
C
C         RA, R0      - Single Precision - for intermediate results
C
C     Constants:
C
C         DEGRAD      - Double Precision - Conversion Factor
C                       degrees to radians
C
C         ERADIUS     - Single Precision - Earth Radius in km
C
C         EPS         - Single Precision - limit parameter, used to
C                       avoid computational difficulties
C
C         UNIM        - Single Precision - limit parameter used to
C                       avoid computational singularities
C
C     Revision History
C
C     Written by Radex, Inc., 3 Preston Court, Bedford, MA 01730 12/94
C
C
C     .. Parameters ..
      DOUBLE PRECISION DEGRAD
      PARAMETER (DEGRAD=1.745329251994330D-2)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION R_HEIGHT_IN,R_LAT_ADJ,R_LAT_ALT
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION EPS,ERADIUS,R0,RA,UNIM
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ACOS,COS,SIGN,SQRT
C     ..
C     .. Save statement ..
      SAVE
C     ..
C     .. Data statements ..
      DATA ERADIUS/6371.2D0/
      DATA EPS/1.0D-9/
      DATA UNIM/0.9999999D0/
C     ..
C
C     input R_LAT_ALT is an altitude adjusted dipole latitude. The
C     following code computes the corresponding 0 km altitude dipole
C     latitude (CGM latitude)
C
      RA = (COS(DEGRAD*R_LAT_ALT))**2
      IF (RA.LT.EPS) RA = EPS
      R0 = (1.0D0+R_HEIGHT_IN/ERADIUS)/RA
      IF (R0.LT.UNIM) R0 = UNIM
C
      R_LAT_ADJ = SIGN(ACOS(SQRT(1.0D0/R0)),R_LAT_ALT)/DEGRAD
C
      RETURN
      END
