      SUBROUTINE CGM_TO_ALTITUDE(R_HEIGHT_IN,R_LAT_IN,R_LAT_ADJ,I_ERR64)
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
c     $Log: cgm_to_altitude.f,v $
c     Revision 1.1  2004/07/08 18:01:54  brideout
c     files added to support AACGM conversion
c
c     Revision 1.1  1996/03/11  19:24:31  baker
c     Initial revision
c
c
c     cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
C     Purpose:
C
C     Computes Latitude Adjustment for R_HEIGHT_IN for use with the
C     Spherical Harmonic expansion computation.
C
C     Except for low R_HEIGHT_IN, the corrected geomagnetic latitude
C     is discontinuous in the vicinity of the magnetic equator. This
C     continuity would result in poor convergence of a spherical
C     harmonic expansion. To avoid this problem, it was necessary to
C     perform the spherical harmonic fit and expansion with respect
C     to dipole coordinates at the table R_HEIGHT_INs, and to compute
C     an appropriate latitude adjustment. This routine computes the
C     required adjustment.
C
C
C     Input Arguments:
C
C         R_HEIGHT_IN - Single Precision - Input height in km above
C                       earth mean radius
C
C         R_LAT_IN    - Single Precision - Corrected Geomagnetic
C                       Latitude
C
C     Output Arguments:
C
C         R_LAT_ADJ   - Single Precision
C                       Height Adjusted Corrected Dipole Coordinates
C                       corresponding to R_LAT_IN
C
C         I_ERR64     - Logical - Error Flag
C
C                       = .false.  normal return
C
C                       = .true.   the input R_LAT value for
C                         the IFLAG = 2 option is invalid for the
C                         input altitude R_HEIGHT_IN.
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
C         UNIM        - Single Precision - limit parameter, used to
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
      DOUBLE PRECISION R_HEIGHT_IN,R_LAT_ADJ,R_LAT_IN
      LOGICAL I_ERR64
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION ERADIUS,RA,UNIM
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ACOS,COS,SIGN,SQRT
C     ..
C     .. Save statement ..
      SAVE
C     ..
C     .. Data statements ..
C
C
C
      DATA ERADIUS/6371.2D0/
      DATA UNIM/1.0D0/
C     ..
C
      I_ERR64 = .false.
C
C     Compute the corresponding altitude adjusted dipole latitude.
C
      RA = (1.0D0+R_HEIGHT_IN/ERADIUS)*(COS(DEGRAD*R_LAT_IN))**2
      IF (RA.GT.UNIM) THEN
         RA = UNIM
         I_ERR64 = .TRUE.
      END IF
C
      R_LAT_ADJ = SIGN(ACOS(SQRT(RA)),R_LAT_IN)/DEGRAD
C
      RETURN
      END
