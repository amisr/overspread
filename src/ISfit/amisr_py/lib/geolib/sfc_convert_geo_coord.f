      SUBROUTINE CONVERT_GEO_COORD(R_LAT_IN,R_LON_IN,R_HEIGHT_IN,
     *                             R_LAT_OUT,R_LON_OUT,I_FLAG,I_ERROR)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     $Revision: 1.2 $
c     The initial version of this subroutine was written by RADEX, INC.
c     for use on VAX/VMS systems.
c
c     Subsequent revisions for UNIX systems have been made by KBB at
c   The Johns Hopkins Univ. Applied Physics Laboratory.  These revisions
c     have been managed using the Revision Control System (RCS) and a
c     log of the revisions will be found at the end of the comments.
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
C
C     Purpose:
C
C       This subroutine uses a set of spherical harmonic coefficients to
C       perform a coordinate conversion between geographic and corrected
C         geomagnetic coordinates (or vice versa).
C
C    The spherical harmonics for the geographic to corrected geomagnetic
C        coordinate system correspond to a conversion from geographic to
C         the corrected geomagnetic coordinates (expressed in terms of
C       centered dipole coordinates at the input altitude), and are then
C         transformed to ground level.
C
C         The spherical harmonic coefficients used for the inverse are
C         computed relative to centered dipole coordinates at the input
C        altitude. The input CGM coordinates are converted to equivalent
C         values in this coordinate sytem using the inverse altitude
C         algorithm before the evaluation of the geographic spherical
C         harmonic expansion.
C
C     Method:
C
C      This subroutine uses a five-step process in converting a position
C         in one coordinate system to a position in the other.
C         The five steps are as follows:
C
C         1.  The appropriate spherical harmonic coefficients for the
C             coordinate conversion are computed for the input altitude.
C
C      2.  The appropriate coordinates for use in the spherical harmonic
C             expansion are computed. For the geographic ==> corrected
C        geomagnetic coordinate version, these are geographic colatitude
C             and longitude. For the inverse coordinate conversion, the
C           input coordinates are first converted into equivalent dipole
C             coordinates at the input altitude.
C
C        3.  The Cartesian coordinates of the unit vector in the desired
C             coordinate system are then computed using the appropriate
C             spherical harmonic expansion.
C
C         4.  For the geographic ==> corrected geomagnetic coordinate
C             conversion, the dipole-equivalent coordinates at input
C             altitude are converted to their cgm cartesian coordinates.
C
C         5.  Standard trigonometric identities are used to compute the
C        latitude and longitude in the desired output coordinate system.
C
C     Input Arguments:
C
C         R_LAT_IN     - REAL*4 - The input latitude in degrees.
C                        This could be either geographic latitude
C                        or corrected geomagnetic latitude.  The
C                        acceptable range of values is (-90 to +90
C                        degrees).
C
C         R_LON_IN     - REAL*4 - The input longitude in degrees east.
C                        This could be either geographic longitude
C                        or corrected geomagnetic longitude.  The
C                      acceptable range of values is (0 to 360 degrees).
C
C         R_HEIGHT_IN  - REAL*4 - The height in kilometers for which the
C                        coordinate transformation will be accomplished.
C                        The acceptable range of values is (0 km to
C                        7200 km)
C
C         I_FLAG       - INTEGER - The flag that indicates which way
C                        the conversion will proceed.
C
C                        = 1  convert geographic to corrected
C                                 geomagnetic coordinates
C
C                        = 2  convert corrected geomagnetic
C                                 to geographic coordinates
C
C     Output Arguments:
C
C         R_LAT_OUT   - REAL*4 - The output latitude in degrees.
C                       This could be either the geographic latitude
C                       or corrected geomagnetic latitude.  This
C                       value will be between -90 and +90 degrees.
C
C         R_LON_OUT   - REAL*4 - The output longitude in degrees.
C                       This could be either geographic longitude or
C                       corrected geomagnetic longitude.  This
C                       value will be between 0 and 360 degrees.
C
C         I_ERROR     - INTEGER - The error flag
C
C                       =  0  normal processing
C                       = -2  R_HEIGHT_IN is outside the allowable range
C                       = -4  I_FLAG value is invalid
C                       = -8  R_LAT_IN is outside the allowable range
C                       = -16 R_LON_IN is outside the allowable range
C                       = -32 Magnitude of the "unit vector" of the
C                             target coordinate system deviates
C                             significantly (+/- 20% or more) from 1.
C                       = -64 For altitudes > 0, for the corrected
C                             geomagnetic to geographic coordinate
C                             conversion there is a range of latitudes
C                             for which the transformation is invalid.
C                             This flag is set when the requested input
C                             cgm latitude falls within the invalid
C                             region.
C
C     Local Variables:
C
C         C_CLASS          - Character String, used to identify message
C                            class in error messages.
C
C         C_FLAG           - Character String used to store I_FLAG
C                            as a character string for use in error
C                            message.
C
C         C_HEIGHT_IN      - Character String used to store R_HEIGHT_IN
C                            as a character string for use in error
C                            message.
C
C         C_LAT_IN         - Character String used to store R_LAT_IN
C                            as a character string for use in error
C                            message.
C
C         C_LON_IN         - Character String used to store R_LON_IN
C                            as a character string for use in error
C                            message.
C
C         C_R              - Character String used to store D_R
C                            as a character string for use in error
C                            message.
C
C         C_REP_PARAMS     - Character String used to store error
C                            message.
C
C
C         C_ROUTINE_NAME   - Character String containing name of
C                            subroutine generating error message.
C
C         C_UNDEFINED      - Character String used to store input data
C                            (R_HEIGHT, R_LAT_IN, R_LON_IN) as a
C                            character string for use in error message.
C
C         D_CINT(,,)       - Double Precision Array (3-D) -
C                           Contains the spherical harmonic coefficients
C                         interpolated to the input height, R_HEIGHT_IN.
C
C         D_COEF(,,,)      - Double Precision Array (3-D) -
C                           Contains the spherical harmonic coefficients
C                           used to compute the Cartesian coordinates of
C                           unit vector in the target coordinate system.
C
C                            First index: sp. harm. coeff. index
C                         Second       x, y, z components of unit vector
C                            Third        altitude indices
C                            Fourth       direction of conversion index
C
C                        coefficients for a given altitude have the form
C
C                            a0 + h a1 + h^2 * a2 + h^3 * a3
C                                 where h = alt/7000 [km]
C
C      D_COLAT_INPUT    - Double Precision - colatitude (radians) in the
C                            input coordinate system
C
C      D_COLAT_OUTPUT   - Double Precision - colatitude (radians) in the
C                            output coordinate system
C
C       D_LON_INPUT      - Double Precision - longitude (radians) in the
C                            input  input system
C
C       D_LON_OUTPUT     - Double Precision - longitude (radians) in the
C                            output coordinate system
C
C         D_R              - Double Precision - magnitude of the
C                            unit radius vector in the target coordinate
C                            system.  The target coordinate system is
C                            the system to which this subroutine is
C                            converting the latitude and longitude.
C
C         D_X              - Double Precision - the X-component of the
C                            unit radius vector in the target coordinate
C                            system
C
C         D_Y              - Double Precision - the Y-component of the
C                            unit radius vector in the target coordinate
C                            system
C
C         D_Z              - Double Precision - the Z-component of the
C                            unit radius vector in the target coordinate
C                            system
C
C         D_YLMVAL         - Double Precision - the array of spherical
C                            harmonic basis functions evaluated at
C                            a particular colatitude and longitude.
C
C
C     I_COND_VALUE     - Integer - the condition value returned from the
C                            call to SFC$$PUT_USER_MSG
C
C         I_ERR64          - Logical - error flag for CGM_TO_ALTITUDE
C                            Routine.
C
C
C        I_SYSTEM_ERR     - Integer  - Set to the value, SFC$$_NORMAL in
C                            all calls to SFC$$PUT_USER_MSG
C
C         K                - Integer  - first index of the D_CINT
C                            interpolated spherical harmonic coefficient
C                            array.
C
C         L                - Integer  - order of each spherical harmonic
C                           coefficient and spherical harmonic function.
C
C         M                - Integer  - zonal index of the spherical
C                            harmonic functions.
C
C         R_HEIGHT_OLD()   - Real*4 Array (1-D)  - Variable containing
C                         previous height (km) used to determine whether
C                            the interpolation to compute D_CINT() from
C                            D_COEF() needs to be done.
C
C         R_LAT_ADJ        - Real*4 Variable used to store result of
C                            ALTITUDE_TO_CGM or CGM_TO_ALTITUDE
C                            conversion.
C
C         R_LAT_ALT        - Real*4 Variable used to store at altitude
C                            dipole latitude.
C
C         R_R              - Real*4 - magnitude of unit vector (= D_R)
C                            (D_X, D_Y, D_Z) in single precision
C
C     Constants:
C
C
C         I_MAX_LENGTH     - Integer  - the length of the D_COEF(,,,)
C                            and D_YLMVAL spherical harmonic function
C                            arrays, corresponding to the order of the
C                            spherical harmonic expansion used.
C                           I_MAX_LENGTH = (I_ORDER + 1) * (I_ORDER + 1)
C
C       I_MSGID          - Integer  - contains a message shell number of
C                            4014 which will be used in all calls to
C                            SFC$$PUT_USER_MSG
C
C        I_NUM_AXES       - Integer  - the number of axes in a Cartesian
C                            coordinate system (3)
C
C         I_NUM_FLAG       - Integer  - the number of coordinate trans-
C                            formation flags available (2)
C
C         I_NUM_LEVEL      - Integer  - the number of grid levels
C                            available.  This is the number of
C                            distinct heights for which there are
C                            spherical harmonic coefficients available
C
C      I_ORDER          - Integer  - the order of the spherical harmonic
C                            expansion used.
C
C         DEGRAD           - Double Precision  - the multiplicative
C                            conversion factor for converting degrees
C                            to radians
C
C         PI               - Double Precision  - mathematical constant
C
C     Subroutines Required:
C
C     RYLM              - This subroutine returns the spherical harmonic
C                        function value array for a given colatitude and
C                       longitude.  The spherical harmonics are returned
C                            in the D_YLMVAL array.
C
C      CG_ALT_DIP       - Given altitude (km), latitude, and a direction
C                         flag, returns altitude corrected latitude, and
C                          an error flag. See comments in subroutine for
C                            additional information.
C
C
C  SFC$$PUT_USER_MSG    - This subroutine is used to send error messages
C                            to the Message Handling System.
C
C     Files Used at Compile Time:
C
C     SFCLIB$$DEFS$$:SFC$$SFCDEF/LIST
C
C                          - This include file contains useful system
C                          constants and definitions. (eg. this is where
C                            SFC$$_NORMAL is defined)
C
C
C     Files Used at Run Time:  None
C
C
C     Databases Accessed:  None
C
C
C     Warnings:
C
C     1. For certain values of Corrected Geomagnetic Coordinates, the
C        transformation to Geographic Coordinates (Geocentric) is
C        undefined. When this situation occurs, an error flag is set
C        I_ERROR = -64 and returned to the calling routine.
C
C
C     2. This subroutine contains the non-standard include statement
C
C
C
C
C     Revision History:
C
C      Original Routine, based upon the Kyle Baker routine was converted
C     to a standard library subroutine for SFC$$ by MFS on  14 OCT 1992.
C
C     09/17/93 SPR1993-0203 (SWS) The return condition value needs to
C          be initialized before coordinate conversion processing begins
C
C     08/18/93 SPR1993-0175 (SWS) Routine performed a comparison of
C             Z with a -1.D0. The variable Z should be D_Z.
C
C    The present routine represents the use of an improved algorithm for
C     the computation of corrected geomagnetic coordinates and (where
C     it exists) the inverse, together with the use of an updated
C     magnetic field model (IGRF 1995).
C
C     The present routine incorporates the functionality of Kyle Baker
C     routine, and, uses the same subroutine calls. Relevent portions of
C     the old FORTRAN code have been retained where possible.
C
C     The new algorithm and the current code have been developed by
C     Radex, Inc, 3 Preston Court, Bedford, MA 01730. A technical
C     report describing the improved algorithm, and discussing its
C     limitations is in preparation, and will be published in early
C     1997 as a Philips Laboratory technical report entitled:.
C
C     An Expanded Algorithm for the Computation of Corrected Geomagnetic
C     Coordinates, by C. Hein and K. Bhavnani.
C
C     The principal changes implementing the new algorithm and code are
C     as follows:
C
C        (1) The set of spherical harmonic function values are computed
C            recursively using a single call to a new subroutine RYLM
C            rather than using repetitive function call. The Kyle Baker
C          routine calculated each of the spherical harmonics separately
C            as needed.
C
C            Computationally the recursive caluclation is much faster.
C
C        (2) The spherical harmonic coefficients are based upon
C            the IGRF 95 magnetic field model. The order of the
C            spherical harmonic expansion used here is 10 (previously
C            was 4).
C
C        (3) The coefficients represent a cubic fit to the spherical
C           harmonic coefficients over a range of 24 altitudes from 0 to
C           7200 km. The previous (7/17/95) version used a quadratic fit
C            at 0, 300 and 1200 km altitude, and had an allowable
C            range of altitudes is 0 - 2000 km. The original Kyle Baker
C            routine used polynomial interpolations of the coefficients
C            at 0, 150, 300 and 450 km.
C
C        (4) The spherical harmonic coefficients are computed in an
C            auxilliary coordinate system, which is aligned with the
C            desired target coordinate system. This results in a
C            considerable simplification of the code, eliminating the
C            need to perform multiple coordinate system rotations
C            as was the case in the Kyle Baker routine.
C
C     07/17/95
C
C     MODIFIED FOR THE IGRF 95 MAGNETIC FIELD MODEL.
C
C     CHANGES: BLOCKDATA SECTION HAS BEEN REPLACED WITH COEFFICIENTS
C              APPROPRIATE FOR THE IGRF 1995 MODEL
C
C              REFERENCES TO IGRF 90 IN THE COMMENT LINES HAVE BEEN
C              CHANGED TO IGRF 95.
C
C     07/01/96
C
C     The BLOCKDATA section has been expanded to accomodate spherical
C     harmonic coefficients for a cubic interpolation, in order to
C     accomodate the expanded 0 - 7200 km altitude range. The
C     spherical harmonic coefficients were caluclated on the basis
C     of the IGRF 95 magnetic field model.
C
C **********************************************************************
C
C     The version provided here does not include the BLOCK DATA source
C     code. Instead individual BLOCK DATA files are provided as follows
C     for the following Magnetic Field Models:
C
C     Field Model  File Name
c
C     DGRF 1975    BLKDAT75.FOR
C     DGRF 1980    BLKDAT80.FOR
C     DGRF 1985    BLKDAT85.FOR
C     DGRF 1990    BLKDAT90.FOR
C     IGRF 1995    BLKDAT95.FOR
C
C     Line 3 of the BLKDATxx.FOR files contains an identification field
C     for the magnetic field model used to generate the coefficients.
C
C **********************************************************************
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     RCS revision log for POSIX systems
c
c     $Log: sfc_convert_geo_coord.f,v $
c     Revision 1.2  2005/09/01 15:41:07  brideout
c     removed unused variables to stop warnings
c
c     Revision 1.1  2004/07/08 18:01:56  brideout
c     files added to support AACGM conversion
c
c     Revision 1.8  1997/11/14 19:27:49  baker
c     implementation of modifications to extend the coordinate
c     system to 7200 km in altitude.
c
c     Revision 1.7  1997/10/28 21:04:24  baker
c     There was an error in the value of PI that has been fixed.
c
c     Revision 1.6  1996/03/20 22:53:03  baker
c     Explicitly declared the sizes of the arguments to the
c     subroutine.  This should help avoid compatibility
c     problems when mixing C and Fortran with different
c     machines and different compilers.
c
c     Revision 1.5  1996/03/12  18:59:18  baker
c     Added code to force a recomputation of the
c     conversion coefficients at a given ghheight if the
c     coordinates model has changed from the last call.
c
c     Revision 1.4  1996/03/11  19:25:36  baker
c     Modifications for the 1995 version of AACGM.
c
c
c     Revision 1.3  94/10/17  12:35:32  12:35:32  baker (Kile Baker S1G)
cadded error code -64 to indicate invalid magnetic coordinates specified
c     as input.  This also requires a change in the call to cg_alt_dip
c
c     Revision 1.2  94/10/14  10:53:36  10:53:36  baker (Kile Baker S1G)
c     Added the SAVE instruction to make variables static
c
c     Revision 1.1  94/10/12  15:28:38  15:28:38  baker (Kile Baker S1G)
c     Initial revision
c cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
C
C
C
C         Set the order used in this algorithm to 10.
C
C
C         Parameterize the maximum values of the indices of the
C         spherical harmonic coefficient array D_COEF( , , , ) and the
C      interpolated spherical harmonic coefficient array, D_CINT( , , ).
C
C
C
C
C
C
c
c     The APL version of this code needs to keep track of
c     the previous set of  coefficients defining the conversion
c     model (date).  This is done by looking at the first coefficient
c     in the model.
c
C
C      CHARACTER*300 C_REP_PARAMS
C      CHARACTER*8   C_LAT_IN
C      CHARACTER*8   C_LON_IN
C      CHARACTER*8   C_HEIGHT_IN
C      CHARACTER*2   C_FLAG
C      CHARACTER*6   C_R
C      CHARACTER*70  C_UNDEFINED
C
C     .. Parameters ..
      DOUBLE PRECISION PI
      PARAMETER (PI=3.141592653589793D0)
      DOUBLE PRECISION DEGRAD
      PARAMETER (DEGRAD=1.745329251994330D-2)
      INTEGER I_ORDER
      PARAMETER (I_ORDER=10)
      INTEGER I_NUM_TERMS
      PARAMETER (I_NUM_TERMS=121)
      INTEGER I_NUM_AXES
      PARAMETER (I_NUM_AXES=3)
      INTEGER I_NUM_LEVEL
      PARAMETER (I_NUM_LEVEL=5)
      INTEGER I_NUM_FLAG
      PARAMETER (I_NUM_FLAG=2)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION R_HEIGHT_IN,R_LAT_IN,R_LAT_OUT,R_LON_IN,R_LON_OUT
      INTEGER I_ERROR,I_FLAG
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION ALT_VAR,ALT_VAR_CU,ALT_VAR_QU,ALT_VAR_SQ,
     *                 D_COLAT_INPUT,D_COLAT_OUTPUT,D_COLAT_TEMP,
     *                 D_LON_INPUT,D_LON_OUTPUT,D_LON_TEMP,D_R,D_X,D_Y,
     *                 D_Z,FIRST_COEFF_OLD,R_LAT_ADJ,R_LAT_ALT,R_R
      INTEGER I,J,K,L,M
      LOGICAL I_ERR64
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION D_CINT(I_NUM_TERMS,I_NUM_AXES,I_NUM_FLAG),
     *                 D_YLMVAL(I_NUM_TERMS),R_HEIGHT_OLD(2)
C     ..
C     .. External Subroutines ..
      EXTERNAL ALTITUDE_TO_CGM,CGM_TO_ALTITUDE,RYLM
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,DACOS,DATAN2,DBLE,SQRT
C     ..
C     .. Common blocks ..
      COMMON /SPH_HARM_MODEL/D_COEF
      DOUBLE PRECISION D_COEF(I_NUM_TERMS,I_NUM_AXES,I_NUM_LEVEL,
     *                 I_NUM_FLAG)
C     ..
C     .. Save statement ..
      SAVE D_CINT,R_HEIGHT_OLD,FIRST_COEFF_OLD
C     ..
C     .. Data statements ..
C
C         Initialize R_HEIGHT_OLD() to impossible values.
C
C
C
C
      DATA R_HEIGHT_OLD(1)/-1.D0/
      DATA R_HEIGHT_OLD(2)/-1.D0/
C     ..
C     **** DATA I_SYSTEM_ERR /SFC$$_NORMAL/
C
C
C
C     The COMMON block / SPH_HARM_MODEL / array D_COEF contains
C     the spherical harmonic coefficients used to generate the
C     Cartesian coordinates of the unit vector in the target coordinate
C     system.  The target coordinate system is the system to which
C     the algorithm is converting and is the coordinate system
C     corresponding to the output variables, R_LAT_OUT and R_LON_OUT.
C
C     The coefficient set is stored in a 4-dimensional array with
C     indices defined as follows:
C
C         First Index  - Represents the number of terms used in the
C                        spherical harmonic expansion. Equal to
C                        (I_ORDER + 1) * (I_ORDER + 1)
C
C        Second Index - Represents the Cartesian coordinate a particular
C                     coefficient will be used to generate.  Indices are
C                        defined as follows:
C
C                        1  - X-coordinate (the X-axis points from the
C                             center of the earth to where the prime
C                             meridian crosses the equator.
C
C                        2  - Y-coordinate (the Y-axis points from the
C                             center of the earth to 90 degrees east
C                             of the X-axis)
C
C                        3  - Z-coordinate (the Z-axis points from the
C                             center of the earth to the north pole)
C
C         Third Index  - Represents the terms of a quadratic fit to
C                        altitude (independent variable h =
C                        altitude [km]/1200) for a given spharical
C                        harmonic coefficient. The indices are defined
C                        as follows:
C
C                        1  - Constant term: corresponds to the fit at
C                             0 km altitude
C                        2  - linear term
C                        3  - quadratic term
C                        4  - cubic term
C
C         Fourth Index - Represents the direction of the coordinate
C                       transformation.  Indices are defined as follows:
C
C                        1  - Conversion of geographic coordinates to
C                             corrected geomagnetic coordinates.
C
C                   2  - Conversion of corrected geomagnetic coordinates
C                             to geographic coordinates.
C
C
C     The Data for D_COEF is provided in a BLOCK DATA file (see below)
C
C
C     Initialize the error return condition indicator
C
      I_ERROR = 0
C
C     The APL version of this program allows the magnetic field
c     model to change during the execution of the program.
c     We therefore have to check to make sure that if the model
c     has been changed that we recalculate the coordinate
c     conversion coefficients.
C
C     This IF statement checks to see if the magnetic
C     coordinates model has been changed.  If so, we
C     have to force the recalculation of the conversion
C     coefficients, by resetting the values of the old height
C
      IF (FIRST_COEFF_OLD.NE.D_COEF(1,1,1,1)) THEN
         R_HEIGHT_OLD(1) = -1
         R_HEIGHT_OLD(2) = -1
      END IF
      FIRST_COEFF_OLD = D_COEF(1,1,1,1)
C
C
C     The following IF-THEN-ELSE block checks the input arguments to
C     ensure they are within allowable ranges.  If any argument is
C     outside the acceptable range, the error flag, I_ERROR will be
C     set to some non-zero value, and control will be returned to the
C     calling program.
C
      IF ((R_HEIGHT_IN.LT.0.D0) .OR. (R_HEIGHT_IN.GT.7200.D0)) THEN
C
C        The height in kilometers is outside the allowable range.
C
C        Convert the height value R_HEIGHT_IN to a character string
C        and send an error message to the user.
C
C        WRITE(UNIT = C_HEIGHT_IN,
C        $        FMT = '(F8.2)',
C        $        IOSTAT = I_STATUS) R_HEIGHT_IN
C
C        C_REP_PARAMS = C_ROUTINE_NAME//'\\HEIGHT\\'//C_HEIGHT_IN
C        PRINT *, C_REP_PARAMS
C        CALL PUT_USER_MSG(I_MSGID, C_CLASS, C_REP_PARAMS,
C        $                         I_SYSTEM_ERR, I_COND_VALUE)
C
C        Set the error flag to the appropriate value and return
C
         I_ERROR = -2
         RETURN
C
      ELSE IF ((I_FLAG.LT.1) .OR. (I_FLAG.GT.2)) THEN
C
C        The conversion flag is neither 1 nor 2.
C
C        Convert I_FLAG to a character string C_FLAG and send an error
C        message to the user
C
C        WRITE(UNIT = C_FLAG,
C        $        FMT = '(I2)',
C        $        IOSTAT = I_STATUS) I_FLAG
C
C        C_REP_PARAMS = C_ROUTINE_NAME//'\\CONVERSION FLAG\\'//C_FLAG
C        PRINT*,C_REP_PARAMS
C        CALL PUT_USER_MSG(I_MSGID, C_CLASS, C_REP_PARAMS,
C        $                         I_SYSTEM_ERR, I_COND_VALUE)
C
C        Set the error flag to the appropriate value and return
C
         I_ERROR = -4
         RETURN
C
      ELSE IF (ABS(R_LAT_IN).GT.90.D0) THEN
C
C        The latitude is outside the allowable range.
C
C        Convert R_LAT_IN to a character string C_LAT_IN and send
C        an error message to the user.
C
C        WRITE(UNIT = C_LAT_IN,
C        $        FMT = '(F8.2)',
C        $        IOSTAT = I_STATUS) R_LAT_IN
C
C        C_REP_PARAMS = C_ROUTINE_NAME//'\\LATITUDE\\'//C_LAT_IN
C        PRINT*,C_REP_PARAMS
C        CALL PUT_USER_MSG(I_MSGID, C_CLASS, C_REP_PARAMS,
C        $                         I_SYSTEM_ERR, I_COND_VALUE)
C
C        Set the error flag to the appropriate value and return
C
         I_ERROR = -8
         RETURN
C
      ELSE IF ((R_LON_IN.LT.0.D0) .OR. (R_LON_IN.GT.360.D0)) THEN
C
C        The longitude is outside the allowable range.
C
C        Convert R_LON_IN into a character string C_LON_IN and send
C        an error message to the user.
C
C        WRITE(UNIT = C_LON_IN,
C        $        FMT = '(F8.2)',
C        $        IOSTAT = I_STATUS) R_LON_IN
C
C        C_REP_PARAMS = C_ROUTINE_NAME//'\\LONGITUDE\\'//C_LON_IN
C        PRINT*,C_REP_PARAMS
C        CALL PUT_USER_MSG(I_MSGID, C_CLASS, C_REP_PARAMS,
C        $                         I_SYSTEM_ERR, I_COND_VALUE)
C
C        Set the error flag to the appropriate value and return
C
         I_ERROR = -16
         RETURN
C
      END IF
C
C     All input arguments are within allowable ranges.
C
C     Compute Spherical Harmonic Coefficients for current
C     altitude if required
C
      IF (R_HEIGHT_IN.NE.R_HEIGHT_OLD(I_FLAG)) THEN
C
         ALT_VAR = R_HEIGHT_IN/7200.0D0
         ALT_VAR_SQ = ALT_VAR*ALT_VAR
         ALT_VAR_CU = ALT_VAR*ALT_VAR_SQ
         ALT_VAR_QU = ALT_VAR*ALT_VAR_CU
C
         DO 20 I = 1,3
C
            DO 10 J = 1,121
C
               D_CINT(J,I,I_FLAG) = D_COEF(J,I,1,I_FLAG) +
     *                              ALT_VAR*D_COEF(J,I,2,I_FLAG) +
     *                              ALT_VAR_SQ*D_COEF(J,I,3,I_FLAG) +
     *                              ALT_VAR_CU*D_COEF(J,I,4,I_FLAG) +
     *                              ALT_VAR_QU*D_COEF(J,I,5,I_FLAG)
C
   10       CONTINUE
C
   20    CONTINUE
C
         R_HEIGHT_OLD(I_FLAG) = R_HEIGHT_IN
C
      END IF
C
C     Zero Sums for Spherical Harmonic Expansion Computation
C
      D_X = 0.0D0
      D_Y = 0.0D0
      D_Z = 0.0D0
C
C
C     Prepare for Spherical Harmonic Expansion Computation
C
C
      D_LON_INPUT = DBLE(R_LON_IN)*DEGRAD
C
      IF (I_FLAG.EQ.1) THEN
C
C        Computing CGM from Geographic Coordinates. No altitude
C        correction required
C
         D_COLAT_INPUT = (90.0D0-DBLE(R_LAT_IN))*DEGRAD
C
      ELSE
C
C        Computing Geographic Coordinates from CGM Coordinates.
C
C        Convert CGM Latitude to Dipole Latitude at Altitude R_HEIGHT
C
         CALL CGM_TO_ALTITUDE(R_HEIGHT_IN,R_LAT_IN,R_LAT_ADJ,I_ERR64)
C
         IF (I_ERR64 .EQV. .TRUE.) THEN
C
C            WRITE(UNIT = C_UNDEFINED,
C           $          FMT = 90,
C          $          IOSTAT = I_STATUS) R_LAT_IN, R_LON_IN, R_HEIGHT_IN
C
C            C_REP_PARAMS = C_ROUTINE_NAME//C_UNDEFINED
C            PRINT*,C_REP_PARAMS
C            CALL PUT_USER_MSG(I_MSGID, C_CLASS, C_REP_PARAMS,
C           $                           I_SYSTEM_ERR, I_COND_VALUE)
C
            I_ERROR = -64
            RETURN
         END IF
C
         D_COLAT_INPUT = (90.0D0-DBLE(R_LAT_ADJ))*DEGRAD
C
      END IF
C
C       Generate the spherical harmonics at the coordinate point
C
      CALL RYLM(D_COLAT_INPUT,D_LON_INPUT,I_ORDER,D_YLMVAL)
C
C       Calculate the Cartesian coordinates of the unit vector
C       in the target coordinate system.
C
      DO 40 L = 0,I_ORDER
         DO 30 M = -L,L
C
            K = L*(L+1) + M + 1
C
C           Add the contribution of each spherical harmonic to the
C           Cartesian components of the unit vector in the
C           appropriate coordinate system.
C
C
            D_X = D_X + D_CINT(K,1,I_FLAG)*D_YLMVAL(K)
            D_Y = D_Y + D_CINT(K,2,I_FLAG)*D_YLMVAL(K)
            D_Z = D_Z + D_CINT(K,3,I_FLAG)*D_YLMVAL(K)
C
   30    CONTINUE
   40 CONTINUE
C
C       Compute the magnitude of the Cartesian unit vector of the
C       magnetic dipole coordinate system.
C
      D_R = SQRT(D_X*D_X+D_Y*D_Y+D_Z*D_Z)
C
C       If the magnitude of the unit vector differs significantly
C       from 1, set the error flag and continue processing.
C
      IF ((D_R.LT.0.9D0) .OR. (D_R.GT.1.1D0)) THEN
C
C        The magnitude of the target unit vector differs significantly
C        from 1.  Convert the value D_R into the character string C_R,
C        send a routine message to the user, and continue processing.
C
         R_R = D_R
C
C          WRITE(UNIT = C_R,
C        $          FMT = '(F6.2)',
C        $          IOSTAT = I_STATUS) R_R
C
C          C_REP_PARAMS = C_ROUTINE_NAME//'\\UNIT VECTOR\\'//C_R
C        PRINT*,C_REP_PARAMS
C          CALL PUT_USER_MSG(I_MSGID, C_CLASS, C_REP_PARAMS,
C        $                           I_SYSTEM_ERR, I_COND_VALUE)
C
C        Set the error flag to the appropriate value and return.
C
         I_ERROR = -32
         RETURN
      END IF
C
C           Adjust the components so they do represent the components of
C           a unit vector.  If D_R is equal to 1.0D0, this step will not
C           change the values of D_X, D_Y, or D_Z.
C
      D_Z = D_Z/D_R
      D_X = D_X/D_R
      D_Y = D_Y/D_R
C
C           Obtain output co_latitude and longitude from the unit
C           vector using standard formulas
C
      IF (D_Z.GT.1.0D0) THEN
         D_COLAT_TEMP = 0.0D0
      ELSE IF (D_Z.LT.-1.0D0) THEN
         D_COLAT_TEMP = PI
      ELSE
         D_COLAT_TEMP = DACOS(D_Z)
      END IF
C
      IF ((ABS(D_X).LT.1.0D-8) .AND. (ABS(D_Y).LT.1.0D-8)) THEN
         D_LON_TEMP = 0.0D0
      ELSE
         D_LON_TEMP = DATAN2(D_Y,D_X)
      END IF
C
C       Prepare Latitude Data for Output
C
      D_LON_OUTPUT = D_LON_TEMP
C
      IF (I_FLAG.EQ.1) THEN
C
         R_LAT_ALT = 90.0D0 - D_COLAT_TEMP/DEGRAD
C
         CALL ALTITUDE_TO_CGM(R_HEIGHT_IN,R_LAT_ALT,R_LAT_ADJ)
C
         D_COLAT_OUTPUT = (90.0D0-DBLE(R_LAT_ADJ))*DEGRAD
C
      ELSE
C
         D_COLAT_OUTPUT = D_COLAT_TEMP
C
      END IF
C
C     Convert colatitude into latitude and convert both coordinates
C     from DOUBLE PRECISION radians to REAL degrees.
C
      R_LAT_OUT = 90.0D0 - D_COLAT_OUTPUT/DEGRAD
      R_LON_OUT = D_LON_OUTPUT/DEGRAD
C
      IF (R_LON_OUT.LT.0.0D0) R_LON_OUT = R_LON_OUT + 360.D0
C
C
      RETURN
 9000 FORMAT (' INVERSE UNDEFINED AT CGM LAT ',F6.2,' CGM LON ',F7.2,
     *       ' ALTITUDE ',F8.2)
      END
