      PROGRAM SFCCONV
c
c     $Revision: 1.1 $
c
csfcconv  - program to test the Geographic to/from Geomagnetic Coordinate
c             subprogram to be run at AFSFC
c
c
c Comments: This program and the SFC subroutine have been developed on a
c            DEC VAX 7000 using VMS FORTRAN and trying to maintain the
c         SFC style of coding.  The calling sequence is identical to the
c            previous version of the subprogram.
c
c    This version has been ported to the unix environment.  The original
c     program used a loop and ran through a set of altitudes from
c     0 km to 2000 km.  It opened the file SFCOUT.DAT for each height
c     and made use of the fact the VMS keeps multiple versions of
c     files to allow you to have a separate file for each altitude.
c
c     For the UNIX implementation the automatic loop through the
c     altitudes has been replaced by a request for the user to
c     input an arbitrary altitude to be used.
c
c     $Log: sfctest.f,v $
c     Revision 1.1  2004/07/08 18:01:56  brideout
c     files added to support AACGM conversion
c
c     Revision 1.1  94/10/14  10:59:07  10:59:07  baker (Kile Baker S1G)
c     Initial revision
c
c
C     BEGIN PROGRAM
C     RUN FOR ALTITUDES FROM 0 KM TO 2000 KM
C      DO 200 JH = 0, 20
C     .. Local Scalars ..
      DOUBLE PRECISION CGLAT,CGLON,GLAT,GLON,GMLAT_,GMLON_,R_HEIGHT_IN
      INTEGER I_ERROR,LAT,LON
C     ..
C     .. External Subroutines ..
      EXTERNAL CONVERT_GEO_COORD
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DBLE
C     ..
      R_HEIGHT_IN = 0.0D0
C      type *,'enter the height to be used'
C      accept *,r_height_in
C
C
      OPEN (1,FILE='SFCOUT.DAT',FORM='FORMATTED',STATUS='UNKNOWN')
CRUN FOR ALL GEOGRAPHICS LATITUDES (STEPPING BY 2 DEGREES) AND LONGITUDES
C     (STEPPING BY TEN DEGREES)
      DO 20 LAT = 67,69,2
         GLAT = DBLE(LAT)
         DO 10 LON = 309,319,10
            GLON = DBLE(LON)
C           Convert from Geograhic (Geocentric) to Geomagnetic
            CALL CONVERT_GEO_COORD(GLAT,GLON,R_HEIGHT_IN,CGLAT,CGLON,1,
     *                             I_ERROR)
            IF (I_ERROR.NE.0) THEN
               PRINT *,'err: glat, glong, i_err',GLAT,GLON,I_ERROR
            END IF
C           Convert back to Geographics as a sanity check
            CALL CONVERT_GEO_COORD(CGLAT,CGLON,R_HEIGHT_IN,GMLAT_,
     *                             GMLON_,2,I_ERROR)
            IF (I_ERROR.NE.0) THEN
               PRINT *,'err: mlat, mlong, i_err',CGLAT,CGLON,I_ERROR
            END IF
C
            WRITE (1,FMT=9000) R_HEIGHT_IN,GLAT,GLON,CGLAT,CGLON,
     *        GMLAT_,GMLON_
   10    CONTINUE
C
   20 CONTINUE
      CLOSE (1)
   30 CONTINUE
 9000 FORMAT (3f8.2,4F13.5)
      STOP
      END
