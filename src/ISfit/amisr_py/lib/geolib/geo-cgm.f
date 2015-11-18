C  =====================================================================
C     Downloaded from ftp://nssdcftp.gsfc.nasa.gov/models
C     /geomagnetic/geo_cgm/
C     by B. Rideout on Dec. 26, 2002.  A description of this
C     code can be found
C     at http://nssdc.gsfc.nasa.gov/space/cgm/cgm.html
C     The following changes were made from imported version:
C
C     1. Main method commented out
C     2. Prior to successfully running remake_file:
C      - Inline ! comments removed
C      - DFLOAT -> DBLE
C     3. remake_file script (nag_apt + tcl) run
C     4. Saved all common blocks, Saved arrays in igrf,
C        otherwise needed -fno-automatic flag for g77
C     5. Made the method GEOCGM01 return as soon as cgm lat and long done
C        for speed purpose (otherwise, its very slow)
C
C  PROGRAM GEO-CGM             Version 2001                    July 2001
C     This version is significantly redesigned and now it consist of two
C     major parts:
C
C  1. MAIN PROGRAM GEO-CGM which provides a dialog to key-entry the data
C
C     2. SUBROUTINE GEOCGM01 which does all necessary calculations
C
C   The user can write a new main program and run GEOCGM01 independently
C     RECENT UPDATES:
C   Jul 12, 2001  Correction in IGRF-extrapolation should be cycled 1,66
C  Jul 11, 2001  Corrected typo in IGRF-2000 G(2,0) coef.: it was -2167.
C     Jun 29, 2001  Correction in "Write out the results into the file:"
C                in IUH(n),IUM(n) - n = 1,2,3,4 in four lines
C     Apr 11, 2001  GEOLOW is modified to account for interpolation of
C                CGM meridians near equator across the 360/0 boundary
C     Jan 22, 2001  The code was extended to 2000-2005 (IGRF and RECALC)
C     Feb  8, 1999  The code was significantly modularized (v. 9.9)
C     Jan 22, 1999  The function OVL_ANG is added (v.9.1)
C     Jan 18, 1999  Main program is modified to call SUBR GEOCGM (v.9.0)
C    Jan 15, 1999  "Geographic" is replaced by "Geocentric" and positive
C                direction of the meridian_angle is corrected in the
C                Southern Hemisphere
C     Aug 26, 1997  IGRF is updated with IGRF-1995, SV 1995-2000, and
C                GEOPACK-1996
C     Dec 22, 1995  Output 132 characters and correction of BF=180-BF
C     Aug 15, 1995  GEOPACK-1994 was incorporated in the version 7.4
C  =====================================================================
C      DIMENSION DAT(11,4),PLA(4),PLO(4),IUH(4),IUM(4)
C      CHARACTER FOUT*20,FINP*20,HEAD*25,HST*5,HCJ*5
C      CHARACTER YS*1,COD*3,DIV*42,DVV*15
C      DATA DIV/'------------------------------------------'/
C      DATA DVV/'---------------'/
C
C     100 FORMAT(
C    *'                                                               '/
C    *' ----------------------- GEO <--> CGM -------------------------'/
C    *' |                                                            |'/
C    *' | The GEO-CGM code provides transformation between CORRECTED |'/
C    *' | GEOMAGNETIC (CGM) and geographic  (GEOCENTRIC) coordinates |'/
C    *' | of a given point utilizing the 1945-2000 DGRF/IGRF models. |'/
C    *' | The B-min approach is applied to calculate CGM coordinates |'/
C    *' | through the near-equator  area where the definition of CGM |'/
C    *' | coordinates is invalid. However, GEO<->GGM transformations |'/
C    *' | are not performed at certain regions where the CGM equator |'/
C    *' | cannot be defined at all (see Gustafsson et al. [1992] and |'/
C    *' | http://nssdc.gsfc.nasa.gov/space/cgm/ for details).        |'/
C    *' |                                                            |'/
C    *' |---AUTHORS:                                                 |'/
C    *' | Natalia Papitashvili (WDC-B2, Moscow, now at NASA/NSSDC) & |'/
C    *' | Vladimir Papitashvili (IZMIRAN, Moscow, now at SPRL, Univ. |'/
C    *' | of Michigan) with contributions from Boris Belov & Volodya |'/
C    *' | Popov (both at IZMIRAN) and from Therese Moretto (DMI,DSRI,|'/
C    *' | now at NASA/GSFC). The original version of the code can be |'/
C    *' | found in Tsyganenko et al. [1987]; the GEOPACK-96 software |'/
C    *' | package is utilized here with minor modifications.         |'/
C    *' |                                                            |'/
C    *' |---REFERENCES:                                              |'/
C    *' | Gustafsson, G., N. E. Papitashvili, and V. O. Papitashvili,|'/
C    *' |   A revised corrected geomagnetic coordinate system for    |'/
C    *' |   Epochs 1985 and 1990, J. Atmos. Terr. Phys., 54, 1609,   |'/
C    *' |   1992.                                                    |'/
C    *' | Tsyganenko, N. A., A. V. Usmanov, V. O. Papitashvili, N. E.|'/
C    *' |   Papitashvili, and V. A. Popov, Software for computations |'/
C    *' |   of geomagnetic field and related coordinate systems, SGC,|'/
C    *' |   Moscow, 58 pp., 1987.                                    |'/
C    *' |------------------------------------------------------------|'/
C    *' | Main GEO-CGM.FOR & subroutine GEOCGM01.FOR       July 2001 |'/
C    *' --------------------------------------------------------------'/
C    */' Type <M/m> if you need more information on input and output  '/
C     *' or hit <Enter> to proceed further')
C      WRITE (*,100)
C      READ  (*,'(A1)') YS
C        IF (YS.eq.'M'.or.YS.eq.'m') WRITE (*,110)
C     110 FORMAT(
C    *'                                                               '/
C    *' --------------------------------------------------------------'/
C    *' | INPUT:                                                     |'/
C    *' | - Geocentric or Corrected GeoMagnetic Latitude / Longitude |'/
C    *' | - Altitude above 1-Re (6371.2 km) surface: 40,000 km limit |'/
C    *' | - Year for the DGRF/IGRF model epochs from 1945 to 2005    |'/
C    *' |                                                            |'/
C    *' | OUTPUT:                                                    |'/
C    *' | - GEOCENTRIC, CGM, and AACGM coordinates of a given point  |'/
C    *' |   (see http://superdarn.jhuapl.edu/aacgm/ for definition   |'/
C    *' |    of the Altitude Adjusted CGM coordinates)               |'/
C    *' | - DGRF/IGRF magnetic field components at this point        |'/
C    *' | - Geocentric and CGM coordinates of the magnetically       |'/
C    *' |    conjugate point and the magnetic field line footprint   |'/
C    *' | - Apex of the magnetic field line (in Re)                  |'/
C    *' | - MLT midnight in UT (hr:mm) at the given point            |'/
C    *' | - Meridian_angle: the azimuth along a great-circle arc to  |'/
C    *' |    the North (South) CGM pole measured from the geographic |'/
C    *' |    North (South) meridian; positive to East (West)         |'/
C    *' | - Oval_angle: the angle between local tangents to the CGM  |'/
C    *' |    and geographic (geocentric) latitudes; this angle is    |'/
C    *' |    presented as the azimuth to the local "magnetic north"  |'/
C    *' |    ("magnetic south") if the eastward (westward) tangent   |'/
C    *' |    to the CGM latitude points southward (northward) from   |'/
C    *' |    local East (West); measured positive to East (West)     |'/
C    *' --------------------------------------------------------------'/
C     *)
C     IRD is an index to key-enter the input parameters or read the file
C          IRD = 0
C      WRITE (*,*)
C     * 'Enter <Y/y> to read the file or <Enter> for keyboard:'
C      READ  (*,'(A1)') YS
C      IF (YS.eq.'Y'.or.YS.eq.'y') THEN
C          IRD = 1
C        WRITE (*,*) 'Sample of the input data file  (A3,2F7.2,F8.1)'
C        WRITE (*,*) '(everything below this line, including header)'
C        WRITE (*,*) 'COD   Lat.  Long.   H, km'
C        WRITE (*,*) 'MOS  56.34 276.89      0.'
C        WRITE (*,*) 'SAT   1.23 150.00 36600.5'
C        WRITE (*,*) 'VOS -78.46 106.83     3.2'
C        WRITE (*,*)
C     11   WRITE (*,*) 'Enter input_file name:'
C        READ  (*,'(A20)/') FINP
C          OPEN(11,FILE=FINP,STATUS='OLD',IOSTAT=IOS)
C            if(IOS.NE.0) then
C              write(*,*) 'File ',FINP,' does not exist!'
C              goto 11
C            endif
C            READ(11,'(A25)') HEAD
C
C     12   WRITE (*,*)'Enter name of the output file to write results'
C        READ  (*,'(A20)') FOUT
C          OPEN(12,FILE=FOUT,STATUS='NEW',IOSTAT=IOS)
C            if(IOS.NE.0) then
C              write(*,*) 'File ',FOUT,' already exists!'
C              goto 12
C              endif
C      ENDIF
C
C     Read the input data from a keyboard
C
C     15   WRITE (*,*) 'Enter year <1945 to 2005> (enter 0 to quit)'
C        WRITE (*,*) '   or  /   to use previous value'
C        WRITE (*,*) '****'
C        READ  (*,*) iyear
C          if (iyear.lt.1) goto 111
C            if (iyear.lt.1945.or.iyear.gt.2005) then
C              write (*,*) '*** WARNING: Year is out of range! ***'
C              goto 15
C            endif
C        WRITE (*,*)'Enter  1  to compute GEO  --> CGM'
C        WRITE (*,*)'  or  -1  to compute GEO <--  CGM'
C        WRITE (*,*)'  or   /  to use previous value'
C        READ  (*,*) ICOR
C      IF(IRD.EQ.1) WRITE (12,250) IYEAR,DVV,DIV,DIV
C     20 CONTINUE
C     Nullify the parameter array before getting to the next point
C        DO I = 1,11
C          DO J = 1,4
C            DAT(I,J) = 0.
C          ENDDO
C        ENDDO
C      IF(IRD.EQ.1) THEN
C     Read data from file
C      IF (ICOR.EQ. 1) THEN
C              READ(11,240,END=111) COD,SLAR,SLOR,HI
C          DAT(1,1) = SLAR
C          DAT(2,1) = SLOR
C                            ELSE
C          READ(11,240,END=111) COD,CLAR,CLOR,HI
C              DAT(3,1) = CLAR
C          DAT(4,1) = CLOR
C            ENDIF
C          GOTO 50
C                     ELSE
C  30   WRITE(*,*)'Enter North/South Latitude and East/West Longitude'
C        WRITE(*,*)'<76.54 -123.48> or <-76.54 123.48>'
C        WRITE(*,*)'or enter  /  to use previous values'
C          IF (ICOR.EQ. 1) THEN
C          READ (*,*) SLAR,SLOR
C            DAT(1,1) = SLAR
C            DAT(2,1) = SLOR
C                          ELSE
C            READ (*,*) CLAR,CLOR
C            DAT(3,1) = CLAR
C            DAT(4,1) = CLOR
C          ENDIF
C     40   WRITE (*,*)'Enter altitude above 1-Re (6371.2 km) surface'
C        WRITE (*,*)'<0 to 40,000> km (or / to use previous value)'
C        READ  (*,*) HI
C          IF(HI.GT.40000.) THEN
C            WRITE(*,*) 'Altitude is too high...'
C            GOTO 40
C          ENDIF
C          WRITE (*,*)
C      ENDIF
C     50 CONTINUE
C    Call the subroutine GEOCGM01 where DAT - 11 input/output parameters
C  (slar,slor,clar,clor,rbm,btr,bfr,brr,ovl,azm,utm) for the start point
C    (*,1), for the conjugate point (*,2), and then for their footprints
C     at 1-Re surface - (*,3) and (*,4), respectively
C      CALL GEOCGM01(ICOR,IYEAR,HI,DAT,PLA,PLO)
C     Headers for the CGM pole coordinates
C        if(DAT(3,1).lt.0.) then
C          HST = 'South'
C                           else
C          HST = 'North'
C        endif
C        if(DAT(3,2).lt.0.) then
C          HCJ = 'South'
C                           else
C          HCJ = 'North'
C        endif
C     Convert UT to HHH:MM for the start and conjugate points
C        CALL UTHM(DAT(11,1),IUH(1),IUM(1))
C        CALL UTHM(DAT(11,2),IUH(2),IUM(2))
C Convert UT to HHH:MM for footprints of the start and conj points
C        IF(HI.GT.0.) THEN
C          CALL UTHM(DAT(11,3),IUH(3),IUM(3))
C          CALL UTHM(DAT(11,4),IUH(4),IUM(4))
C        ENDIF
C      IF (IRD.EQ.1) THEN
C     Write out the results into the file
C        WRITE (*,'(A1,A3,A17)') '   ',COD,' is processing...'
C        WRITE (12,260) COD,HI,(DAT(i,1),i=1,10),IUH(1),IUM(1)
C            IF(HI.GT.0.) THEN
C          WRITE (12,265) 0.,(DAT(i,3),i=1,10),IUH(3),IUM(3)
C          WRITE (12,270) 0.,(DAT(i,4),i=1,10),IUH(4),IUM(4)
C            ENDIF
C        WRITE (12,280) HI,(DAT(i,2),i=1,10),IUH(2),IUM(2)
C     Go to get a new point
C          GOTO 20
C                    ELSE
C     Write out the results on the display
C        WRITE (*,*)
C     Write parameters of the CGM pole(s) for the start point
C        WRITE (*,130) DIV,DIV
C        WRITE (*,140) IYEAR,HST,HI,PLA(1),PLO(1)
C          IF(HI.GT.0.) WRITE (*,145) 0.,PLA(3),PLO(3)
C        WRITE (*,130) DIV,DIV
C     Write parameters for the start point
C        WRITE (*,150) DIV,DIV,HI
C        WRITE (*,155) (DAT(i,1),i=1,10),IUH(1),IUM(1)
C     Write parameters for the footprints
C          IF(HI.GT.0.) THEN
C            WRITE (*,160)
C            WRITE (*,155) (DAT(i,3),i=1,10),IUH(3),IUM(3)
C            WRITE (*,170)
C            WRITE (*,155) (DAT(i,4),i=1,10),IUH(4),IUM(4)
C          ENDIF
C     Write parameters of the CGM pole(s) for the conj point
C        WRITE (*,180) HI
C        WRITE (*,155) (DAT(i,2),i=1,10),IUH(2),IUM(2)
C        WRITE (*,*)
C     Write parameters of the CGM pole(s) for the conj point
C        WRITE (*,130) DIV,DIV
C        WRITE (*,140) IYEAR,HCJ,HI,PLA(2),PLO(2)
C          IF(HI.GT.0.) WRITE (*,145) 0.,PLA(4),PLO(4)
C        WRITE (*,130) DIV,DIV
C        WRITE (*,*)
C     Go to get a new point
C        GOTO 15
C     Ending to write data
C      ENDIF
C     130 FORMAT(2X,2A42)
C     140 FORMAT('       Year: ',I4,4X,A5,' CGM pole at ',F7.1,
C     +' km:   Lat.=',F7.2,'   Long.=',F7.2)
C     145 FORMAT('                                    at ',F7.1,
C     +' km:        ',F7.2,'         ',F7.2)
C     150 FORMAT(
C     +'     Geocentric         CGM       L-value  IGRF Magnetic Field',
C     +'   Oval & Azimuth  MLTMN'/
C     +'    Lat.   Long.    Lat.   Long.    Re     H,nT   D,deg   Z,nT',
C     +'  angles N/S:+E/W  in UT'/2X,2A42//'  Starting point at ',
C     +F7.1,' km:'/)
C     155 FORMAT(4F8.2,F7.2,F8.0,F8.2,F8.0,2F8.2,2X,I2,':',I2)
C     160 FORMAT(/2X,
C     +'Footprint at 1-Re & AACGM coords of the starting point:'/)
C     170 FORMAT(/2X,
C     +'Footprint at 1-Re & AACGM coords of the conjugate point:'/)
C     180 FORMAT(/2X,'Conjugate point at ',F7.1,' km:'/)
C     240 FORMAT(A3,2F7.2,F8.1)
C  250 FORMAT(' Year  Altitude   Geocentric        CGM       L-value  ',
C     +'IGRF Magnetic Field    Oval & Azimuth  MLTMN'/
C     +I5,'    (km)    Lat.   Long.    Lat.   Long.   Re     H,nT    ',
C     +'D,deg   Z,nT  angles N/S:+E/W  in UT'/A15,2A42)
C     260 FORMAT(/
C     +      1X,A3,1X,F8.1,4F8.2,F7.2,F8.0,F8.2,F8.0,2F8.2,2X,I2,':',I2)
C  265 FORMAT('  FTP',F8.1,4F8.2,F7.2,F8.0,F8.2,F8.0,2F8.2,2X,I2,':',I2)
C  270 FORMAT('  CFT',F8.1,4F8.2,F7.2,F8.0,F8.2,F8.0,2F8.2,2X,I2,':',I2)
C  280 FORMAT(' CONJ',F8.1,4F8.2,F7.2,F8.0,F8.2,F8.0,2F8.2,2X,I2,':',I2)
C     111   STOP
C        END
C  *********************************************************************
      SUBROUTINE UTHM(UTM,IUH,IUM)
C     Converts UTM from the hour and fraction to HH:MM
C     .. Scalar Arguments ..
      DOUBLE PRECISION UTM
      INTEGER IUH,IUM
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC INT,NINT
C     ..
      IUH = INT(UTM)
      IF (IUH.EQ.99) THEN
         IUM = 99
      ELSE
         IUM = NINT((UTM-IUH)*60)
      END IF
      RETURN
      END
C  *********************************************************************
C  =====================================================================
      SUBROUTINE GEOCGM01(ICOR,IYEAR,HI,DAT,PLA,PLO)
C   Version 2001 for GEO-CGM.FOR                              April 2001
C     Apr 11, 2001  GEOLOW is modified to account for interpolation of
C                CGM meridians near equator across the 360/0 boundary
C     AUTHORS:
C     Natalia E. Papitashvili (WDC-B2, Moscow, Russia, now at NSSDC,
C     NASA/Goddard Space Flight Center, Greenbelt, Maryland)
C     Vladimir O. Papitashvili (IZMIRAN, Moscow, Russia, now at SPRL,
C     The University of Michigan, Ann Arbor)
C     Conributions from Boris A. Belov and Vladimir A. Popov (both at
C     IZMIRAN), as well as from Therese Moretto (DMI, DSRI, now at
C     NASA/GSFC).
C     The original version of this code is described in the brochure by
C   N.A. Tsyganenko, A.V. Usmanov, V.O. Papitashvili, N.E. Papitashvili,
C     and V.A. Popov, Software for computations of geomagnetic field and
C  related coordinate systems, Soviet Geophys. Committ., Moscow, 58 pp.,
C     1987. A number of subroutines from the revised GEOPACK-96 software
C     package developed by Nikolai A. Tsyganenko and Mauricio Peredo are
C     utilized in this code with some modifications (see full version of
C    GEOPACK-96 on http://www-spof.gsfc.nasa.gov/Modeling/geopack.html).
C     This code consists of the main subroutine GEOCGM99, five functions
C  (OVL_ANG, CGMGLA, CGMGLO, DFRIDR, and AZM_ANG), eigth new and revised
C     subroutines from the above-mentioned brochure (MLTUT, MFC, FTPRNT,
C     GEOLOW, CORGEO, GEOCOR, SHAG, and RIGHT), and 9 subroutines from
C   GEOPACK-96 (IGRF, SPHCAR, BSPCAR, GEOMAG, MAGSM, SMGSM, RECALC, SUN)
C  =====================================================================
C     Input parameters:
C     icor = +1    geo to cgm
C            -1    cgm to geo
C     iyr  = year
C     hi   = altitude
C     slar = geocentric latitude
C     slor = geocentric longitude (east +)
C     These two pairs can be either input or output parameters
C     clar = cgm latitude
C     clor = cgm longitude (east +)
C     Output parameters:
C     Array DAT(11,4) consists of 11 parameters (slar, slor, clar, clor,
C     rbm, btr, bfr, brr, ovl, azm, utm) organized for the start point
C     (*,1), its conjugate point (*,2), then for the footprints at 1-Re
C     of the start (*,3) and conjugate (*,4) points
C     Description of parameters used in the subroutine:
C     slac = conjugate geocentric latitude
C     sloc = conjugate geocentric longitude
C     slaf = footprint geocentric latitude
C     slof = footprint geocentric longitude
C     rbm  = apex of the magnetic field line in Re (Re=6371.2 km)
C            (this parameter approximately equals the McIlwain L-value)
C     btr  = IGRF Magnetic field H (nT)
C     bfr  = IGRF Magnetic field D (deg)
C     brr  = IGRF Magnetic field Z (nT)
C     ovl  = oval_angle as the azimuth to "magnetic north":
C                + east in Northern Hemisphere
C                + west in Southern Hemisphere
C     azm  = meridian_angle as the azimuth to the CGM pole:
C                + east in Northern Hemisphere
C                + west in Southern Hemisphere
C     utm  = magnetic local time (MLT) midnight in UT hours
C     pla  = array of geocentric latitude and
C     plo  = array of geocentric longitudes for the CGM poles
C            in the Northern and Southern hemispheres at a given
C            altitude (indices 1 and 2) and then at the Earth's
C            surface - 1-Re or zero altitude - (indices 3 and 4)
C     dla  = dipole latitude
C     dlo  = dipole longitude
C  =====================================================================
C     Year (for example, as for Epoch 1995.0 - no fraction of the year)
C     .. Scalar Arguments ..
      DOUBLE PRECISION HI
      INTEGER ICOR,IYEAR
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION DAT(11,4),PLA(4),PLO(4)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION ACLAC,ACLAR,ACLOC,ACLOR,AZM,BFR,BRR,BTR,CLAC,
     *                 CLAJ,CLAR,CLOC,CLOJ,CLOR,DAA,DLA,DLO,DOO,OVL,
     *                 PLAJ,PLAN,PLAN1,PLAS,PLAS1,PLOJ,PLON,PLON1,PLOS,
     *                 PLOS1,PMC,PMM,PMP,PMR,RBM,RE,RJ,SLAC,SLACF,SLAJ,
     *                 SLAL,SLAR,SLARF,SLOC,SLOCF,SLOJ,SLOL,SLOR,SLORF,
     *                 UT
      INTEGER I,ICOUNT,J
C     CHARACTER*12 STR
C     ..
C     .. External Functions ..
      DOUBLE PRECISION AZM_ANG,OVL_ANG
      EXTERNAL AZM_ANG,OVL_ANG
C     ..
C     .. External Subroutines ..
      EXTERNAL CORGEO,FTPRNT,GEOCOR,GEOLOW,MFC,MLTUT
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS
C     ..
C     .. Common blocks ..
      COMMON /C1/AA,II,BB
      COMMON /IYR/IYR
      COMMON /NM/NM
      COMMON /RZ/RH
      DOUBLE PRECISION RH
      INTEGER IYR,NM
      DOUBLE PRECISION AA(27),BB(8)
      INTEGER II(2)
      SAVE /C1/,/IYR/,/NM/,/RZ/
C     ..
      IYR = IYEAR
C     Earth's radius (km)
      RE = 6371.2D0
C     NM is the number of harmonics
      NM = 10
C     The radius of the sphere to compute the coordinates (in Re)
      RH = (RE+HI)/RE
C   Correction of latitudes and longitudes if they are entered beyond of
C     the limits (this actually does not affect coordinate calculations
C     but the oval/meridian angles and MLT midnight cannot be computed)
      IF (DAT(1,1).GT.90.D0) DAT(1,1) = 180.D0 - DAT(1,1)
      IF (DAT(1,1).LT.-90.D0) DAT(1,1) = -180.D0 - DAT(1,1)
      IF (DAT(3,1).GT.90.D0) DAT(3,1) = 180.D0 - DAT(3,1)
      IF (DAT(3,1).LT.-90.D0) DAT(3,1) = -180.D0 - DAT(3,1)
      IF (DAT(2,1).GT.360.D0) DAT(2,1) = DAT(2,1) - 360.D0
      IF (DAT(2,1).LT.-360.D0) DAT(2,1) = DAT(2,1) + 360.D0
      IF (DAT(4,1).GT.360.D0) DAT(4,1) = DAT(4,1) - 360.D0
      IF (DAT(4,1).LT.-360.D0) DAT(4,1) = DAT(4,1) + 360.D0
C     Computation of CGM coordinates from geocentric ones at high- and
C     middle latitudes
      IF (ICOR.EQ.1) THEN
         SLAR = DAT(1,1)
         SLOR = DAT(2,1)
         IF (ABS(SLAR).EQ.90.D0) SLOR = 360.D0
         CALL GEOCOR(SLAR,SLOR,RH,DLA,DLO,CLAR,CLOR,PMR)
         DAT(3,1) = CLAR
         DAT(4,1) = CLOR
      ELSE
C       Computation of geocentric coordinates from CGM ones at high- and
C        middle latitudes
         CLAR = DAT(3,1)
         CLOR = DAT(4,1)
         IF (ABS(CLAR).EQ.90.D0) CLOR = 360.D0
         CALL CORGEO(SLAR,SLOR,RH,DLA,DLO,CLAR,CLOR,PMR)
         DAT(1,1) = SLAR
         DAT(2,1) = SLOR
      END IF
C   PMI is L-shell parameter for the magnetic field line; limit to 16 Re
      IF (PMR.GE.16.D0) PMR = 999.99D0
      DAT(5,1) = PMR
C  Check if CGM_Lat has been calculated, then go for the conjugate point
      IF (CLAR.GT.999.D0) THEN
C    CGM_Lat has NOT been calculated, call GEOLOW for computation of the
C       CGM coordinates at low latitudes using the CBM approach (see the
C        reference in GEOLOW)
         CALL GEOLOW(SLAR,SLOR,RH,CLAR,CLOR,RBM,SLAC,SLOC)
         DAT(3,1) = CLAR
         DAT(4,1) = CLOR
         IF (RBM.GE.16.D0) RBM = 999.99D0
         DAT(5,1) = RBM
C        Conjugate point coordinates at low latitudes
C        these two lines seems to mess me up - change by brideout
C        WRITE (STR,FMT='(2F6.2)') SLAC,SLOC
C        READ (STR,FMT='(2F6.2)') SLAC,SLOC
         DAT(1,2) = SLAC
         DAT(2,2) = SLOC
         CALL GEOCOR(SLAC,SLOC,RH,DAA,DOO,CLAC,CLOC,RBM)
         IF (CLAC.GT.999.D0) CALL GEOLOW(SLAC,SLOC,RH,CLAC,CLOC,RBM,
     *                            SLAL,SLOL)
         DAT(3,2) = CLAC
         DAT(4,2) = CLOC
         DAT(5,2) = RBM
      ELSE
C        Computation of the magnetically conjugated point at high- and
C        middle latitudes
         CLAC = -CLAR
         CLOC = CLOR
         DAT(3,2) = CLAC
         DAT(4,2) = CLOC
         CALL CORGEO(SLAC,SLOC,RH,DAA,DOO,CLAC,CLOC,PMC)
         DAT(1,2) = SLAC
         DAT(2,2) = SLOC
         IF (PMC.GE.16.D0) PMC = 999.99D0
         DAT(5,2) = PMC
      END IF
C     Same RBM for footprints as for the starting and conjugate points
C
C     For the present we only need cgm lat and cgm long - for speed, return here
      IF (1.EQ.10) THEN
         RETURN
      END IF
      DAT(5,3) = DAT(5,1)
      DAT(5,4) = DAT(5,2)
C     Calculation of the magnetic field line footprint at the
C     Earth's surface for the starting point
      IF (RH.GT.1.D0 .AND. CLAR.LT.999.D0 .AND. CLAR.LT.999.D0) THEN
         CALL FTPRNT(RH,SLAR,SLOR,CLAR,CLOR,ACLAR,ACLOR,SLARF,SLORF,
     *               1.D0)
         DAT(1,3) = SLARF
         DAT(2,3) = SLORF
         DAT(3,3) = ACLAR
         DAT(4,3) = ACLOR
C        and for the conjugate point
         CALL FTPRNT(RH,SLAC,SLOC,CLAC,CLOC,ACLAC,ACLOC,SLACF,SLOCF,
     *               1.D0)
         DAT(1,4) = SLACF
         DAT(2,4) = SLOCF
         DAT(3,4) = ACLAC
         DAT(4,4) = ACLOC
      ELSE
         DO I = 1,4
            DO J = 3,4
               DAT(I,J) = 999.99D0
            END DO
         END DO
      END IF
C     Computation of geocentric coordinates of the North or South CGM
C   poles for a given year at the altitude RH and Earth's surface (1-Re)
      CALL CORGEO(PLAN,PLON,RH,DAA,DOO,90.D0,360.D0,PMP)
      PLAN1 = PLAN
      PLON1 = PLON
      CALL CORGEO(PLAS,PLOS,RH,DAA,DOO,-90.D0,360.D0,PMP)
      PLAS1 = PLAS
      PLOS1 = PLOS
      IF (RH.GT.1.D0) THEN
         CALL CORGEO(PLAN1,PLON1,1.D0,DAA,DOO,90.D0,360.D0,PMP)
         CALL CORGEO(PLAS1,PLOS1,1.D0,DAA,DOO,-90.D0,360.D0,PMM)
      END IF
      IF (CLAR.LT.0.D0) THEN
         PLA(1) = PLAS
         PLO(1) = PLOS
      ELSE
         PLA(1) = PLAN
         PLO(1) = PLON
      END IF
      IF (ACLAR.LT.0.D0) THEN
         PLA(3) = PLAS1
         PLO(3) = PLOS1
      ELSE
         PLA(3) = PLAN1
         PLO(3) = PLON1
      END IF
      IF (CLAC.LT.0.D0) THEN
         PLA(2) = PLAS
         PLO(2) = PLOS
      ELSE
         PLA(2) = PLAN
         PLO(2) = PLON
      END IF
      IF (ACLAC.LT.0.D0) THEN
         PLA(4) = PLAS1
         PLO(4) = PLOS1
      ELSE
         PLA(4) = PLAN1
         PLO(4) = PLON1
      END IF
      DO J = 1,4
         DAT(6,J) = 99999.D0
         DAT(7,J) = 999.99D0
         DAT(8,J) = 99999.D0
         DAT(9,J) = 999.99D0
         DAT(10,J) = 999.99D0
         DAT(11,J) = 99.99D0
      END DO
      ICOUNT = 2
      IF (RH.GT.1.D0) ICOUNT = 4
      RJ = RH
      DO J = 1,ICOUNT
         IF (J.GT.2) RJ = 1.D0
         PLAJ = PLA(J)
         PLOJ = PLO(J)
         SLAJ = DAT(1,J)
         SLOJ = DAT(2,J)
         CLAJ = DAT(3,J)
         CLOJ = DAT(4,J)
C        Computation of the IGRF components
         CALL MFC(SLAJ,SLOJ,RJ,BTR,BFR,BRR)
         DAT(6,J) = BTR
         DAT(7,J) = BFR
         DAT(8,J) = BRR
C        Computation of the oval_angle (OVL) between the tangents to
C    geographic and CGM latitudes at a given point (the code is slightly
C    modified from the source provided by Therese Morreto in 1994). Note
C      that rotation of OVL on 90 deg anticlockwise provides the azimuth
C        to the local "magnetic" north (south) measured from the local
C      geographic meridian. The OVL_ANG can be calculated only at middle
C        and high latitudes where CGM --> GEO is permitted.
         OVL = OVL_ANG(SLAJ,SLOJ,CLAJ,CLOJ,RJ)
         DAT(9,J) = OVL
C        Computation of the meridian_angle (AZM) between the geographic
C        meridian and direction (azimuth along the great-circle arc) to
C        the North (South) CGM pole
         AZM = AZM_ANG(SLAJ,SLOJ,CLAJ,PLAJ,PLOJ)
         DAT(10,J) = AZM
C        Computation of the MLT midnight (in UT)
         CALL MLTUT(SLAJ,SLOJ,CLAJ,PLAJ,PLOJ,UT)
         DAT(11,J) = UT
C        End of loop j = 1,icount
      END DO
      RETURN
      END
C  *********************************************************************
      DOUBLE PRECISION FUNCTION OVL_ANG(SLA,SLO,CLA,CLO,RR)
C   This function returns an estimate at the given location of the angle
C     (oval_angle) between the directions (tangents) along the constant
C     CGM and geographic latitudes by utilizing the function DFRIDR from
C     Numerical Recipes for FORTRAN.
C   This angle can be taken as the azimuth to the local "magnetic" north
C   (south) if the eastward (westward) tangent to the local CGM latitude
C     points south (north) from the local geographic latitude.
C  Written by Therese Moretto in August 1994 (revised by V. Papitashvili
C     in January 1999).
C   Ignore points which nearly coincide with the geographic or CGM poles
C    within 0.01 degree in latitudes; this also takes care if SLA or CLA
C     are dummy values (e.g., 999.99)
C     .. Scalar Arguments ..
      DOUBLE PRECISION CLA,CLO,RR,SLA,SLO
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION DENOM,ERR1,ERR2,HOM,STEP
C     ..
C     .. External Functions ..
      DOUBLE PRECISION CGMGLA,CGMGLO,DFRIDR
      EXTERNAL CGMGLA,CGMGLO,DFRIDR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,DATAN2,DCOS
C     ..
C     .. Common blocks ..
      COMMON /CGMGEO/CLAT,CR360,CR0,RH
      DOUBLE PRECISION CLAT,RH
      LOGICAL CR0,CR360
      SAVE /CGMGEO/
C     ..
      IF (ABS(SLA).GE.89.99D0 .OR. ABS(CLA).GE.89.99D0 .OR.
     *    ABS(SLA).LT.30.D0) THEN
         OVL_ANG = 999.99D0
         RETURN
      END IF
C     Initialize values for the cgmglo and cgmgla functions
      RH = RR
      CLAT = CLA
      CR360 = .false.
      CR0 = .false.
C     Judge if SLO may be crossing the 360-0 limit. If geocentric
C     longitude of the location is larger than 270 deg, then cr360 is
C     set "true"; if it is less than 90 deg, then cr0 is set "true".
      IF (SLO.GE.270.D0) CR360 = .true.
      IF (SLO.LE.90.D0) CR0 = .true.
C     An initial stepsize (in degrees)
      STEP = 10.D0
C     Note that in the near-pole region the functions CGMGLA and CGMGLO
C     could be called from DFRIDR with the CGM latitudes exceeded 90 or
C    -90 degrees (e.g., 98 or -98) when STEP is added or subtracted to a
C     given CGM latitude (CLA). This does not produce discontinuities in
C     the functions because GEOCOR calculates GEOLAT smoothly for the
C     points lying behind the pole (e.g., as for 82 or - 82 deg. in the
C     above-mentioned example). However, it could be discontinuity in
C     GEOLON if |GEOLAT| = 90 deg. - see CGMGLO for details.
      HOM = DFRIDR(CGMGLA,CLO,STEP,ERR1)
      DENOM = DFRIDR(CGMGLO,CLO,STEP,ERR2)
      DENOM = DENOM*COS(SLA*0.017453293D0)
      OVL_ANG = -ATAN2(HOM,DENOM)
      OVL_ANG = OVL_ANG*57.2957751D0
      RETURN
      END
C  *********************************************************************
      DOUBLE PRECISION FUNCTION CGMGLA(CLON)
C     This function returns the geocentric latitude as a function of CGM
C     longitude with the CGM latitude held in common block CGMGEO.
C     Essentially this function just calls the subroutine CORGEO.
C     .. Scalar Arguments ..
      DOUBLE PRECISION CLON
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION DLA,DLO,GEOLAT,GEOLON,PMI,RR
C     ..
C     .. External Subroutines ..
      EXTERNAL CORGEO
C     ..
C     .. Common blocks ..
      COMMON /CGMGEO/CCLAT,CR360,CR0,RH
      DOUBLE PRECISION CCLAT,RH
      LOGICAL CR0,CR360
      SAVE /CGMGEO/
C     ..
      RR = RH
      IF (CLON.GT.360.D0) CLON = CLON - 360.D0
      IF (CLON.LT.0.D0) CLON = CLON + 360.D0
      CALL CORGEO(GEOLAT,GEOLON,RR,DLA,DLO,CCLAT,CLON,PMI)
      CGMGLA = GEOLAT
      RETURN
      END
C  *********************************************************************
      DOUBLE PRECISION FUNCTION CGMGLO(CLON)
C     Same as the function CGMGLA but this returns the geocentric
C    longitude. If cr360 is true, geolon+360 deg is returned when geolon
C     is less than 90 deg. If cr0 is true, geolon-360 deg is returned
C     when geolon is larger than 270 degrees.
C     .. Scalar Arguments ..
      DOUBLE PRECISION CLON
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION DLA,DLO,GEOLAT,GEOLON,PMI,RR
C     ..
C     .. External Subroutines ..
      EXTERNAL CORGEO
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS
C     ..
C     .. Common blocks ..
      COMMON /CGMGEO/CCLAT,CR360,CR0,RH
      DOUBLE PRECISION CCLAT,RH
      LOGICAL CR0,CR360
      SAVE /CGMGEO/
C     ..
      RR = RH
      IF (CLON.GT.360.D0) CLON = CLON - 360.D0
      IF (CLON.LT.0.D0) CLON = CLON + 360.D0
   10 CONTINUE
      CALL CORGEO(GEOLAT,GEOLON,RR,DLA,DLO,CCLAT,CLON,PMI)
C   Geographic longitude geolon could be any number (e.g., discontinued)
C     when geolat is the geographic pole
      IF (ABS(GEOLAT).GE.89.99D0) THEN
         CLON = CLON - 0.01D0
         GO TO 10
      END IF
      IF (CR360 .AND. (GEOLON.LE.90.D0)) THEN
         CGMGLO = GEOLON + 360.D0
      ELSE
         IF (CR0 .AND. (GEOLON.GE.270.D0)) THEN
            CGMGLO = GEOLON - 360.D0
         ELSE
            CGMGLO = GEOLON
         END IF
      END IF
      RETURN
      END
      DOUBLE PRECISION
C **********************************************************************
     *  FUNCTION DFRIDR(FUNC,X,H,ERR)
C     Numerical Recipes Fortran 77 Version 2.07
C     Copyright (c) 1986-1995 by Numerical Recipes Software
C     .. Parameters ..
      DOUBLE PRECISION CON,CON2,BIG
      INTEGER NTAB
      DOUBLE PRECISION SAFE
      PARAMETER (CON=1.4D0,CON2=CON*CON,BIG=1.D30,NTAB=10,SAFE=2.D0)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION ERR,H,X
C     ..
C     .. Function Arguments ..
      DOUBLE PRECISION FUNC
      EXTERNAL FUNC
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION ERRT,FAC,HH
      INTEGER I,J
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION A(NTAB,NTAB)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,MAX
C     ..
      DFRIDR = 0.0D0
      HH = H
      A(1,1) = (FUNC(X+HH)-FUNC(X-HH))/(2.0D0*HH)
      ERR = BIG
      DO 20 I = 2,NTAB
         HH = HH/CON
         A(1,I) = (FUNC(X+HH)-FUNC(X-HH))/(2.0D0*HH)
         FAC = CON2
         DO 10 J = 2,I
            A(J,I) = (A(J-1,I)*FAC-A(J-1,I-1))/(FAC-1.D0)
            FAC = CON2*FAC
            ERRT = MAX(ABS(A(J,I)-A(J-1,I)),ABS(A(J,I)-A(J-1,I-1)))
            IF (ERRT.LE.ERR) THEN
               ERR = ERRT
               DFRIDR = A(J,I)
            END IF
   10    CONTINUE
         IF (ABS(A(I,I)-A(I-1,I-1)).GE.SAFE*ERR) RETURN
   20 CONTINUE
      RETURN
      END
C  *********************************************************************
      DOUBLE PRECISION FUNCTION AZM_ANG(SLA,SLO,CLA,PLA,PLO)
C     Computation of an angle between the north geographic meridian and
C     direction to the North (South) CGM pole: positive azimuth is
C     measured East (West) from geographic meridian, i.e., the angle is
C     measured between the great-circle arc directions to the geographic
C     and CGM poles. In this case the geomagnetic field components in
C     XYZ (NEV) system can be converted into the CGM system in both
C     hemispheres as:
C                           XM = X COS(alf) + Y SIN(alf)
C                           YM =-X SIN(alf) + Y COS(alf)
C   Written by V. O. Papitashvili in mid-1980s; revised in February 1999
C   Ignore points which nearly coincide with the geographic or CGM poles
C    within 0.01 degree in latitudes; this also takes care if SLA or CLA
C     are dummy values (e.g., 999.99)
C     .. Scalar Arguments ..
      DOUBLE PRECISION CLA,PLA,PLO,SLA,SLO
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION ALFA,AM,BET,CM,RAD,SB,SP,SS,ST
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,DATAN2,DCOS,SIGN,DSIN,TAN
C     ..
      IF (ABS(SLA).GE.89.99D0 .OR. ABS(CLA).GE.89.99D0) THEN
         AZM_ANG = 999.99D0
         RETURN
      END IF
      SP = 1.D0
      SS = 1.D0
C     IF (SIGN(SP,PLA).NE.SIGN(SS,CLA)) THEN
C        WRITE (*,FMT=9000) PLA,CLA
C     9000    FORMAT (/,'WARNING - The CGM pole PLA = ',f6.2,
C     *          ' and station CLAT = ',f6.2,
C     *          ' are not in the same hemisphere: AZM_ANG is incorrect!'
C     *          )
C     END IF
      RAD = 0.017453293D0
      AM = (90.D0-ABS(PLA))*RAD
      IF (SIGN(SP,PLA).EQ.SIGN(SS,SLA)) THEN
         CM = (90.D0-ABS(SLA))*RAD
      ELSE
         CM = (90.D0+ABS(SLA))*RAD
      END IF
      IF (SLA.GE.0.D0) THEN
         BET = (PLO-SLO)*RAD
      ELSE
         BET = (SLO-PLO)*RAD
      END IF
      SB = SIN(BET)
      ST = SIN(CM)/TAN(AM) - COS(CM)*COS(BET)
      ALFA = ATAN2(SB,ST)
      AZM_ANG = ALFA/RAD
      RETURN
      END
C  *********************************************************************
      SUBROUTINE MLTUT(SLA,SLO,CLA,PLA,PLO,UT)
C     Calculates the MLT midnight in UT hours
C     Definition of the MLT midnight (MLTMN) here is different from the
C     approach described elsewhere. This definition does not take into
C    account the geomagnetic meridian of the subsolar point which causes
C    seasonal variations of the MLTMN in UT time. The latter approach is
C     perfectly applicable to the dipole or eccentric dipole magnetic
C    coordinates but it fails with the CGM coordinates because there are
C     forbidden areas near the geomagnetic equator where CGM coordinates
C     cannot be calculated by definition [e.g., Gustafsson et al., JATP,
C     54, 1609, 1992].
C  In this code the MLT midnight is defined as location of a given point
C    on (or above) the Earth's surface strictly behind the North (South)
C     CGM pole in such the Sun, the pole, and the point are lined up.
C     This approach was originally proposed and coded by Boris Belov
C     sometime in the beginning of 1980s; here it is slightly edited by
C     Vladimir Papitashvili in February 1999.
C   Ignore points which nearly coincide with the geographic or CGM poles
C    within 0.01 degree in latitudes; this also takes care if SLA or CLA
C     are dummy values (e.g., 999.99)
C     .. Scalar Arguments ..
      DOUBLE PRECISION CLA,PLA,PLO,SLA,SLO,UT
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION A,BP,BT,CFF,CFT,QQ,QQU,QT,QTU,RAD,SP,SS,TPI,X,Y
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,DATAN2,DCOS,SIGN,DSIN
C     ..
      IF (ABS(SLA).GE.89.99D0 .OR. ABS(CLA).GE.89.99D0) THEN
         UT = 99.99D0
         RETURN
      END IF
      TPI = 6.283185307D0
      RAD = 0.017453293D0
      SP = 1.D0
      SS = 1.D0
C      IF (SIGN(SP,PLA).NE.SIGN(SS,CLA)) THEN
C        WRITE (*,FMT=9000) PLA,CLA
C     9000    FORMAT (/,'WARNING - The CGM pole PLA = ',f6.2,
C     *          ' and station CLAT = ',f6.2,
C     *          ' are not in the same hemisphere: MLTMN is incorrect!')
C      END IF
C     Solve the spherical triangle
      QQ = PLO*RAD
      CFF = 90.D0 - ABS(PLA)
      CFF = CFF*RAD
      IF (CFF.LT.0.0000001D0) CFF = 0.0000001D0
      IF (SIGN(SP,PLA).EQ.SIGN(SS,SLA)) THEN
         CFT = 90.D0 - ABS(SLA)
      ELSE
         CFT = 90.D0 + ABS(SLA)
      END IF
      CFT = CFT*RAD
      IF (CFT.LT.0.0000001D0) CFT = 0.0000001D0
      QT = SLO*RAD
      A = SIN(CFF)/SIN(CFT)
      Y = A*SIN(QQ) - SIN(QT)
      X = COS(QT) - A*COS(QQ)
      UT = ATAN2(Y,X)
      IF (UT.LT.0.D0) UT = UT + TPI
      QQU = QQ + UT
      QTU = QT + UT
      BP = SIN(CFF)*COS(QQU)
      BT = SIN(CFT)*COS(QTU)
      UT = UT/RAD
      UT = UT/15.D0
      IF (BP.LT.BT) GO TO 10
      IF (UT.LT.12.D0) UT = UT + 12.D0
      IF (UT.GT.12.D0) UT = UT - 12.D0
   10 CONTINUE
      RETURN
      END
C  *********************************************************************
      SUBROUTINE MFC(SLA,SLO,R,H,D,Z)
C     Computation of the IGRF magnetic field components
C     Extracted as a subroutine from the earlier version of GEO-CGM.FOR
C     V. Papitashvili, February 1999
C     This takes care if SLA or CLA are dummy values (e.g., 999.99)
C     .. Scalar Arguments ..
      DOUBLE PRECISION D,H,R,SLA,SLO,Z
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION BF,BR,BT,F,RLA,RLO,X,Y
      INTEGER I
C     ..
C     .. External Subroutines ..
      EXTERNAL IGRF
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DATAN2,SQRT
C     ..
C     .. Common blocks ..
      COMMON /IYR/IYR
      COMMON /NM/NM
      INTEGER IYR,NM
      SAVE /IYR/,/NM/
C     ..
      IF (SLA.GE.999.D0) THEN
         X = 99999.D0
         Y = 99999.D0
         Z = 99999.D0
         H = 99999.D0
         D = 999.99D0
         I = 999.99D0
         F = 99999.D0
         RETURN
      END IF
C     Computation of all geomagnetic field components
      RLA = (90.D0-SLA)*0.017453293D0
      RLO = SLO*0.017453293D0
      CALL IGRF(IYR,NM,R,RLA,RLO,BR,BT,BF)
      X = -BT
      Y = BF
      Z = -BR
      H = SQRT(X**2+Y**2)
      D = 57.2957751D0*ATAN2(Y,X)
      I = 57.2957751D0*ATAN2(Z,H)
      F = SQRT(H**2+Z**2)
      RETURN
      END
C  *********************************************************************
      SUBROUTINE FTPRNT(RH,SLA,SLO,CLA,CLO,ACLA,ACLO,SLAF,SLOF,RF)
C     Calculation of the magnetic field line footprint at the Earth's
C     (or any higher) surface.
C   Extracted as a subroutine from the earlier version of GEO-CGM.FOR by
C   V. Papitashvili in February 1999 but then the subroutine was revised
C    to obtain the Altitude Adjusted CGM coordinates. The AACGM approach
C     is proposed by Kile Baker of the JHU/APL, see their World Wide Web
C     site http://sd-www.jhuapl.edu/RADAR/AACGM/ for details.
C     If RF = 1-Re (i.e., at the Earth's surface), then the footprint
C     location is defined as the Altitude Adjusted (AA) CGM coordinates
C     for a given point (ACLA, ACLO).
C     If RF = 1.xx Re (i.e., at any altitude above or below the starting
C     point), then the conjunction between these two points can be found
C     along the field line.
C     This takes care if SLA or CLA are dummy values (e.g., 999.99)
C     .. Scalar Arguments ..
      DOUBLE PRECISION ACLA,ACLO,CLA,CLO,RF,RH,SLA,SLAF,SLO,SLOF
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION ACOL,COL,DLAF,DLOF,DR0,DR1,DR10,DS,FRAC,PMIF,R,
     *                 RF1,RL,RR,RSLA,RSLO,SN2,XF,XF1,YF,YF1,ZF,ZF1
C     ..
C     .. External Subroutines ..
      EXTERNAL CORGEO,SHAG,SPHCAR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,DASIN,DSIN,SQRT
C     ..
C     .. Common blocks ..
      COMMON /IYR/IYR
      COMMON /NM/NM
      INTEGER IYR,NM
      SAVE /IYR/,/NM/
C     ..
      IF (SLA.GT.999.D0 .OR. CLA.GT.999 .OR. RF.EQ.RH) THEN
         ACLA = 999.99D0
         ACLO = 999.99D0
         SLAF = 999.99D0
         SLOF = 999.99D0
         RETURN
      END IF
C     Defining the Altitude Adjusted CGM coordinates for a given point
      COL = (90.D0-CLA)*0.017453293D0
      SN2 = (SIN(COL))**2
      ACOL = ASIN(SQRT((SN2*RF)/RH))
      ACLA = 90.D0 - ACOL*57.29577951D0
      IF (CLA.LT.0.D0) ACLA = -ACLA
      ACLO = CLO
      CALL CORGEO(SLAF,SLOF,RF,DLAF,DLOF,ACLA,ACLO,PMIF)
      IF (SLAF.LT.999.D0) RETURN
C     Tracing the magnetic field line down to the Earth's surface at low
C    latitudes if CORGEO failed to calculate geocentric coordinates SLAF
C     and SLOF
      IF (SN2.LT.0.0000001D0) SN2 = 0.0000001D0
      RL = RH/SN2
      FRAC = 0.03D0/(1.D0+3.D0/(RL-0.6D0))
C     Checking direction of the magnetic field-line, so the step along
C     the field-line will go down, to the Earth surface
      IF (CLA.GE.0.D0) FRAC = -FRAC
      DS = RH*FRAC
   10 CONTINUE
C     Start from an initial point
      R = RH
      RSLA = (90.D0-SLA)*0.0174533D0
      RSLO = SLO*0.0174533D0
      CALL SPHCAR(R,RSLA,RSLO,XF,YF,ZF,1)
      RF1 = R
      XF1 = XF
      YF1 = YF
      ZF1 = ZF
   20 CALL SHAG(XF,YF,ZF,DS)
      RR = SQRT(XF**2+YF**2+ZF**2)
      IF (RR.GT.RH) THEN
         DS = -DS
         XF = XF1
         YF = YF1
         ZF = ZF1
         GO TO 10
      END IF
      IF (RR.GT.RF) THEN
         RF1 = RR
         XF1 = XF
         YF1 = YF
         ZF1 = ZF
         GO TO 20
      ELSE
         DR1 = ABS(RF1-RF)
         DR0 = ABS(RF-RR)
         DR10 = DR1 + DR0
         IF (DR10.NE.0.D0) THEN
            DS = DS*(DR1/DR10)
            CALL SHAG(XF1,YF1,ZF1,DS)
         END IF
         CALL SPHCAR(RR,SLAF,SLOF,XF1,YF1,ZF1,-1)
         SLAF = 90.D0 - SLAF*57.29578D0
         SLOF = SLOF*57.29578D0
      END IF
      RETURN
      END
C  *********************************************************************
      SUBROUTINE GEOLOW(SLAR,SLOR,RH,CLAR,CLOR,RBM,SLAC,SLOC)
C     Calculates CGM coordinates from geocentric ones at low latitudes
C    where the DGRF/IGRF magnetic field lines may never cross the dipole
C     equatorial plane and, therefore, the definition of CGM coordinates
C     becomes invalid.
C     The code is written by Natalia and Vladimir Papitashvili as a part
C   of the earlier versions of GEO-CGM.FOR; extracted as a subroutine by
C     V. Papitashvili in February 1999.
C     Apr 11, 2001  GEOLOW is modified to account for interpolation of
C                CGM meridians near equator across the 360/0 boundary
C     See the paper by  Gustafsson, G., N. E. Papitashvili, and V. O.
C    Papitashvili, A revised corrected geomagnetic coordinate system for
C     Epochs 1985 and 1990 [J. Atmos. Terr. Phys., 54, 1609-1631, 1992]
C     for detailed description of the B-min approach utilized here.
C     This takes care if SLA is a dummy value (e.g., 999.99)
C     .. Scalar Arguments ..
      DOUBLE PRECISION CLAR,CLOR,RBM,RH,SLAC,SLAR,SLOC,SLOR
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION B2,B3,BB2,BB3,BF,BM,BR,BT,CLA,CLO,COL,DAA,DD,
     *                 DELAT,DELCLA,DELCLO,DELON,DHH,DOO,DR0,DR1,DR10,
     *                 DS,DSD,DSLA,FRAC,PMM,R1,RBM1,RDEL,RL,RLA,RLAN,
     *                 RLAS,RLO,RM,RNLAT,RNLON,RR,RSLAT,RSLON,SLA,SLAN,
     *                 SLAS,SLL,SLM,SLO,SZ,X1,XBM,XBM1,XGEO,Y1,YBM,YBM1,
     *                 YGEO,Z1,ZBM,ZBM1,ZGEO
      INTEGER I,IH,IHEM,J,JC,JCN,JCS,JDEL,JDN,JDS,L1,L2,N999,NDIR,NOBM
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION ARLAT(181),ARLON(181),BC(2)
C     ..
C     .. External Subroutines ..
      EXTERNAL GEOCOR,IGRF,SHAG,SPHCAR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,INT,DSIN,SQRT

C     .. Common blocks ..
      COMMON /IYR/IYR
      COMMON /NM/NM
      INTEGER IYR,NM
      SAVE /IYR/,/NM/

C     initialize
      R1=0.0D0
      RLAN=0.0D0
      RLAS=0.0D0
      RNLAT=0.0D0
      RNLON=0.0D0
      RSLAT=0.0D0
      RSLON=0.0D0
      SLAN=0.0D0
      SLAS=0.0D0
      JCN=0
      JCS=0
C     ..
      IF (SLAR.GT.999.D0) THEN
         CLAR = 999.99D0
         CLOR = 999.99D0
         SLAC = 999.99D0
         SLOC = 999.99D0
         RBM = 999.99D0
         RETURN
      END IF
C   HH is an error (nT) to determine B-min along the magnetic field line
      DHH = 0.5D0
C    Filling the work arrays of CGM latitudes and longitudes with 999.99
C    Note that at certain geocentric longitudes in the very near-equator
C     region no "geomagnetic equator" can be defined at all.
      DO J = 61,121
         ARLAT(J) = 999.99D0
         ARLON(J) = 999.99D0
      END DO
      SLO = SLOR
      NDIR = 0
C     Finding the geomagnetic equator as a projection of the B-min point
C     found for the field lines started from the last latitude in each
C     hemisphere where the CGM coordinates were obtained from geocentric
C    ones (GEO --> CGM). First the CGM coordinates are calculated in the
C     Northern (NDIR=0) and then in the Southern hemispheres (NDIR=1)
   10 IF (NDIR.EQ.0) THEN
C     Program works from 30 deg. latitude down to the geographic equator
C        in the Northern Hemisphere
         DO JC = 61,91
            SLA = 90.D0 - (JC-1)
            CALL GEOCOR(SLA,SLO,RH,DAA,DOO,CLA,CLO,PMM)
            IF (CLA.GT.999.D0) THEN
               NDIR = 1
               GO TO 10
            END IF
            ARLAT(JC) = CLA
            ARLON(JC) = CLO
         END DO
         NDIR = 1
         GO TO 10
      ELSE
C    Program works from -30 deg. latitude down to the geographic equator
C        in the Southern Hemisphere
         DO JC = 121,92,-1
            SLA = 90.D0 - (JC-1)
            CALL GEOCOR(SLA,SLO,RH,DAA,DOO,CLA,CLO,PMM)
            IF (CLA.GT.999.D0) THEN
               NDIR = 0
               GO TO 20
            END IF
            ARLAT(JC) = CLA
            ARLON(JC) = CLO
         END DO
         NDIR = 0
      END IF
   20 CONTINUE
C     Finding last geographic latitudes along SLO where CGM coordinates
C     can be calculated
      N999 = 0
      NDIR = 0
      DO JC = 61,121
         IF (ARLAT(JC).GT.999.D0) THEN
            IF (NDIR.EQ.0) THEN
               JCN = JC - 1
               RNLAT = ARLAT(JCN)
               RNLON = ARLON(JCN)
               NDIR = 1
               N999 = 1
            END IF
         END IF
         IF (ARLAT(JC).LT.999.D0) THEN
            IF (NDIR.EQ.1) THEN
               JCS = JC
               RSLAT = ARLAT(JC)
               RSLON = ARLON(JC)
               NDIR = 0
               GO TO 30
            END IF
         END IF
      END DO
   30 CONTINUE
C     If there is no points with 999.99 found along the SLO meridian,
C     then the IHEM loop will start from 3; otherwise it starts from 1
      IF (N999.EQ.0) THEN
         IH = 3
         GO TO 40
      ELSE
         IH = 1
      END IF
C     Interpolation of the appropriate CGM longitudes between last
C     geocentric latitudes along SLO where CGM coordinates were defined
C   (modified by Freddy Christiansen of DMI to account for interpolation
C     across the 360/0 boundary - April 11, 2001)
      RDEL = JCS - JCN
      IF (RDEL.EQ.0.D0) THEN
         DELON = 0.D0
      ELSE
         IF (RSLON.GT.270.D0 .AND. RNLON.LT.90.D0) THEN
            DELON = (RSLON-(RNLON+360.D0))/RDEL
         ELSE
            IF (RSLON.LT.90.D0 .AND. RNLON.GT.270.D0) THEN
               DELON = (RSLON-(RNLON-360.D0))/RDEL
            ELSE
               DELON = (RSLON-RNLON)/RDEL
            END IF
         END IF
      END IF
      DO JC = JCN + 1,JCS - 1
         ARLON(JC) = RNLON + DELON*(JC-JCN)
         IF (ARLON(JC).LT.0.D0) ARLON(JC) = ARLON(JC) + 360.D0
      END DO
   40 CONTINUE
C     Finding the CGM equator at SLO on the sphere with radius RH
      NOBM = 0
      DO IHEM = IH,3
         RM = RH
C        Defining the real equator point from the Northern Hemisphere
         IF (IHEM.EQ.1) THEN
            CLA = RNLAT
            SLA = 90.D0 - (JCN-1.D0)
            SLAN = SLA
         END IF
C        Defining the real equator point from the Southern Hemisphere
         IF (IHEM.EQ.2) THEN
            CLA = RSLAT
            SLA = 90.D0 - (JCS-1)
            SLAS = SLA
         END IF
C        Defining the apex of the current magnetic field line
         IF (IHEM.EQ.3) THEN
            CLA = 0.D0
            SLA = SLAR
         END IF
C        Here CLA is used only to calculate FRAC
         COL = (90.D0-CLA)*0.017453293D0
         SLM = (90.D0-SLA)*0.017453293D0
         SLL = SLO*0.017453293D0
         CALL IGRF(IYR,NM,RM,SLM,SLL,BR,BT,BF)
         SZ = -BR
         CALL SPHCAR(RM,SLM,SLL,XGEO,YGEO,ZGEO,1)
         BM = SQRT(BR*BR+BT*BT+BF*BF)
         XBM = XGEO
         YBM = YGEO
         ZBM = ZGEO
         RL = 1.D0/(SIN(COL))**2
         FRAC = 0.03D0/(1.D0+3.D0/(RL-0.6D0))
         IF (SZ.LE.0.D0) FRAC = -FRAC
         DSD = RL*FRAC
         DS = DSD
   50    CONTINUE
C        Keep two consequently computed points to define B-min
         DO 80 I = 1,2
            DD = DS
            CALL SHAG(XGEO,YGEO,ZGEO,DD)
   60       IF (I.NE.1) GO TO 70
            XBM1 = XGEO
            YBM1 = YGEO
            ZBM1 = ZGEO
            RBM1 = SQRT(XBM1**2+YBM1**2+ZBM1**2)
   70       CONTINUE
            CALL SPHCAR(RM,SLM,SLL,XGEO,YGEO,ZGEO,-1)
            CALL IGRF(IYR,NM,RM,SLM,SLL,BR,BT,BF)
C       Go and compute the conjugate point if no B-min was found at this
C    magnetic field line (could happen at very near geomagnetic equator)
            IF (RM.LT.RH) THEN
               NOBM = 1
               GO TO 140
            END IF
            BC(I) = SQRT(BR*BR+BT*BT+BF*BF)
   80    CONTINUE
         B2 = BC(1)
         B3 = BC(2)
         IF (BM.GT.B2 .AND. B2.LT.B3) GO TO 90
         IF (BM.GE.B2 .AND. B2.LT.B3) GO TO 100
         IF (BM.GT.B2 .AND. B2.LE.B3) GO TO 100
         BM = BC(1)
         XGEO = XBM1
         YGEO = YBM1
         ZGEO = ZBM1
         XBM = XBM1
         YBM = YBM1
         ZBM = ZBM1
         GO TO 50
   90    BB3 = ABS(B3-B2)
         BB2 = ABS(BM-B2)
         IF (BB2.LT.DHH .AND. BB3.LT.DHH) GO TO 110
  100    BM = BM
         XGEO = XBM
         YGEO = YBM
         ZGEO = ZBM
         DS = DS/2.D0
         GO TO 50
  110    CONTINUE
         CALL SPHCAR(RBM1,RLA,RLO,XBM1,YBM1,ZBM1,-1)
         RLA = 90.D0 - RLA*57.2957751D0
         RLO = RLO*57.2957751D0
         IF (IHEM.EQ.1) RLAN = RLA
         IF (IHEM.EQ.2) RLAS = RLA
C       Computation of the magnetically conjugate point at low latitudes
  120    CONTINUE
         IF (IHEM.EQ.3) THEN
            RBM = RBM1
            RM = RBM
            DS = DSD
  130       CONTINUE
            CALL SHAG(XBM1,YBM1,ZBM1,DS)
            RR = SQRT(XBM1**2+YBM1**2+ZBM1**2)
            IF (RR.GT.RH) THEN
               R1 = RR
               X1 = XBM1
               Y1 = YBM1
               Z1 = ZBM1
               GO TO 130
            ELSE
               DR1 = ABS(RH-R1)
               DR0 = ABS(RH-RR)
               DR10 = DR1 + DR0
               IF (DR10.NE.0.D0) THEN
                  DS = DS*(DR1/DR10)
                  RM = R1
                  CALL SHAG(X1,Y1,Z1,DS)
               END IF
               CALL SPHCAR(RR,SLAC,SLOC,X1,Y1,Z1,-1)
               SLAC = 90.D0 - SLAC*57.2957751D0
               SLOC = SLOC*57.2957751D0
            END IF
         END IF
C        End of loop IHEM
  140    CONTINUE
      END DO
      IF (N999.EQ.0) GO TO 150
      IF (NOBM.EQ.1) THEN
C        Interpolation of CGM latitudes if there is no B-min at this
C        magnetic field line
         RDEL = JCS - JCN
         IF (RDEL.EQ.0.D0) THEN
            DELAT = 0.D0
         ELSE
            DELAT = (RSLAT-RNLAT)/RDEL
         END IF
         JDEL = 0
         DO JC = JCN + 1,JCS - 1
            JDEL = JDEL + 1
            ARLAT(JC) = RNLAT + DELAT*JDEL
         END DO
         RBM = 999.99D0
         SLAC = 999.99D0
         SLOC = 999.99D0
      ELSE
C        Geocentric latitude of the CGM equator
         RLA = (RLAN+RLAS)/2.D0
C        Interpolation of the CGM latitudes in the Northern hemisphere
         RDEL = SLAN - RLA
         IF (RDEL.EQ.0.D0) THEN
            DELAT = 0.D0
         ELSE
            DELAT = RNLAT/RDEL
         END IF
         JDN = ABS(RDEL)
         JDEL = 0
         DO JC = JCN + 1,JCN + JDN
            JDEL = JDEL + 1
            ARLAT(JC) = RNLAT - DELAT*JDEL
         END DO
C        Interpolation of the CGM latitudes in the Southern hemisphere
         RDEL = SLAS - RLA
         IF (RDEL.EQ.0.D0) THEN
            DELAT = 0.D0
         ELSE
            DELAT = RSLAT/RDEL
         END IF
         JDS = ABS(RDEL)
         JDEL = 0
         DO JC = JCS - 1,JCS - JDS,-1
            JDEL = JDEL + 1
            ARLAT(JC) = RSLAT + DELAT*JDEL
         END DO
      END IF
  150 CONTINUE
C     Defining by interpolation the exact values of the CGM latitude
C     and longitude between two adjacent values
      L1 = 90.D0 - SLAR + 1.D0
      IF (SLAR.LT.0.D0) THEN
         L2 = L1 - 1
      ELSE
         L2 = L1 + 1
      END IF
      DSLA = ABS(SLAR-INT(SLAR))
      DELCLA = ARLAT(L2) - ARLAT(L1)
      DELCLO = ARLON(L2) - ARLON(L1)
      CLAR = ARLAT(L1) + DELCLA*DSLA
      CLOR = ARLON(L1) + DELCLO*DSLA
      RETURN
      END
C  *********************************************************************
      SUBROUTINE CORGEO(SLA,SLO,RH,DLA,DLO,CLA,CLO,PMI)
C     Calculates geocentric coordinates from corrected geomagnetic ones.
C     The code is written by Vladimir Popov and Vladimir Papitashvili
C     in mid-1980s; revised by V. Papitashvili in February 1999
C     This takes care if CLA is a dummy value (e.g., 999.99)
C     .. Scalar Arguments ..
      DOUBLE PRECISION CLA,CLO,DLA,DLO,PMI,RH,SLA,SLO
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION AA10,CLAS,CLOS,COL,DLS,DR0,DR1,DR10,DS,FRAC,GTET,
     *                 GTH,GXLA,PF,PMS,R,R0,R1,RBM,RFI,RL,RLO,RM,SAA,
     *                 SAQ,SCLA,SLAC,SLOC,SN,SN2,TH,X,X1,XM,Y,Y1,YM,Z,
     *                 Z1,ZM
      INTEGER JC,NG
C     ..
C     .. External Subroutines ..
      EXTERNAL GEOCOR,GEOLOW,GEOMAG,SHAG,SPHCAR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,DATAN,DSIN,SQRT
C     ..
C     .. Common blocks ..
      COMMON /IYR/IYR
      COMMON /NM/NM
      INTEGER IYR,NM
      SAVE /IYR/,/NM/
C     ..
      JC = 0
      IF (ABS(CLA).LT.1.D0) THEN
C        WRITE (*,FMT=*)
C    *   'WARNING - No calculations within +/-1 degree near CGM equator'
         JC = 1
      END IF
      IF (CLA.GT.999.D0 .OR. JC.EQ.1) THEN
         SLA = 999.99D0
         SLO = 999.99D0
         DLA = 999.99D0
         DLO = 999.99D0
         PMI = 999.99D0
         RETURN
      END IF
      NG = NM
      COL = 90.D0 - CLA
      R = 10.D0
      R1 = R
      R0 = R
      COL = COL*0.017453293D0
      RLO = CLO*0.017453293D0
      SN = SIN(COL)
      SN2 = SN*SN
C     The CGM latitude should be at least 0.01 deg. away of the CGM pole
      IF (SN2.LT.0.000000003D0) SN2 = 0.000000003D0
C      RFI = 1./SN2
      RFI = RH/SN2
      PMI = RFI
      IF (PMI.GT.99.999D0) PMI = 999.99D0
      AA10 = R/RFI
C     RFI = R if COL = 90 deg.
      IF (RFI.LE.R) GO TO 10
      SAA = AA10/(1.D0-AA10)
      SAQ = SQRT(SAA)
      SCLA = ATAN(SAQ)
      IF (CLA.LT.0) SCLA = 3.14159265359D0 - SCLA
      GO TO 20
   10 SCLA = 1.57079632679D0
      R0 = RFI
   20 CALL SPHCAR(R0,SCLA,RLO,XM,YM,ZM,1)
      CALL GEOMAG(X,Y,Z,XM,YM,ZM,-1,IYR)
      RL = R0
      FRAC = -0.03D0/(1.D0+3.D0/(RL-0.6D0))
      IF (CLA.LT.0.D0) FRAC = -FRAC
      R = R0
   30 DS = R*FRAC
      NM = (1.D0+9.D0/R) + 0.5D0
      CALL SHAG(X,Y,Z,DS)
      R = SQRT(X**2+Y**2+Z**2)
      IF (R.LE.RH) GO TO 40
      R1 = R
      X1 = X
      Y1 = Y
      Z1 = Z
      GO TO 30
C     Define intersection with the start surface
   40 DR1 = ABS(RH-R1)
      DR0 = ABS(RH-R)
      DR10 = DR1 + DR0
      IF (DR10.NE.0.D0) THEN
         DS = DS*(DR1/DR10)
         CALL SHAG(X1,Y1,Z1,DS)
      END IF
      CALL SPHCAR(R,GTET,GXLA,X1,Y1,Z1,-1)
      GTH = GTET*57.2957751D0
      SLO = GXLA*57.2957751D0
      SLA = 90.D0 - GTH
      CALL GEOMAG(X1,Y1,Z1,XM,YM,ZM,1,IYR)
      CALL SPHCAR(RM,TH,PF,XM,YM,ZM,-1)
      DLO = PF*57.2957751D0
      DLA = 90.D0 - TH*57.2957751D0
      NM = NG
C     Because CORGEO cannot check if the CGM --> GEO transformation is
C    performed correctly in the equatorial area (that is, where the IGRF
C    field line may never cross the dipole equatorial plane). Therefore,
C     the backward check is required for geocentric latitudes lower than
C     30 degrees (see the paper referenced in GEOLOW)
      IF (ABS(SLA).LT.30.D0 .OR. ABS(CLA).LT.30.D0) THEN
         CALL GEOCOR(SLA,SLO,RH,DLS,DLS,CLAS,CLOS,PMS)
         IF (CLAS.GT.999.D0) CALL GEOLOW(SLA,SLO,RH,CLAS,CLOS,RBM,SLAC,
     *                            SLOC)
         IF (ABS(ABS(CLA)-ABS(CLAS)).GE.1.D0) THEN
C           WRITE (*,FMT=*) 'WARNING - Selected CGM_Lat.=',CLA,
C     *        ' is located in the ',
C     *        'near CGM equator area where the latter cannot be defined'
            SLA = 999.99D0
            SLO = 999.99D0
            PMI = 999.99D0
         END IF
      END IF
      RETURN
      END
C  *********************************************************************
      SUBROUTINE GEOCOR(SLA,SLO,RH,DLA,DLO,CLA,CLO,PMI)
C     Calculates corrected geomagnetic coordinates from geocentric ones
C     The code is written by Vladimir Popov and Vladimir Papitashvili
C     in mid-1980s; revised by V. Papitashvili in February 1999
C     This takes care if SLA is a dummy value (e.g., 999.99)
C     .. Scalar Arguments ..
      DOUBLE PRECISION CLA,CLO,DLA,DLO,PMI,RH,SLA,SLO
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION C,COL,DCL,DCO,DS,FRAC,GTET,GXLA,HHH,PF,R,R1,RL,
     *                 RLO,RM,RRH,RZM,S,SN,SSLA,ST,SZM,TH,X,X1,XM,Y,Y1,
     *                 YM,Z,Z1,ZM
      INTEGER NG
C     ..
C     .. External Subroutines ..
      EXTERNAL GEOMAG,SHAG,SPHCAR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,DATAN,DSIN,SQRT
C     ..
C     .. Common blocks ..
      COMMON /IYR/IYR
      COMMON /NM/NM
      INTEGER IYR,NM
      SAVE /IYR/,/NM/
C     ..
      IF (SLA.GT.999.D0) THEN
         CLA = 999.99D0
         CLO = 999.99D0
         DLA = 999.99D0
         DLO = 999.99D0
         PMI = 999.99D0
         RETURN
      END IF
      NG = NM
      COL = 90.D0 - SLA
      R = RH
      R1 = R
      COL = COL*0.017453293D0
      RLO = SLO*0.017453293D0
      CALL SPHCAR(R,COL,RLO,X,Y,Z,1)
      CALL GEOMAG(X,Y,Z,XM,YM,ZM,1,IYR)
      CALL SPHCAR(RM,TH,PF,XM,YM,ZM,-1)
      SZM = ZM
      DLO = PF*57.2957751D0
      DCO = TH*57.2957751D0
      DLA = 90.D0 - DCO
      RL = R/(SIN(TH))**2
      FRAC = 0.03D0/(1.D0+3.D0/(RL-0.6D0))
      IF (SZM.LT.0.D0) FRAC = -FRAC
C     Error to determine the dipole equtorial plane: aprox. 0.5 arc min
      HHH = 0.0001571D0
C     Trace the IGRF magnetic field line to the dipole equatorial plane
   10 DS = R*FRAC
   20 NM = (1.D0+9.D0/R) + 0.5D0
      R1 = R
      X1 = X
      Y1 = Y
      Z1 = Z
      CALL SHAG(X,Y,Z,DS)
      CALL GEOMAG(X,Y,Z,XM,YM,ZM,1,IYR)
      CALL SPHCAR(R,C,S,XM,YM,ZM,-1)
C     As tracing goes above (RH+10_Re), use the dipole field line
      IF (R.GT.10.D0+RH) GO TO 30
C    If the field line returns to the start surface without crossing the
C     dipole equatorial plane, no CGM coordinates can be calculated
      IF (R.LE.RH) GO TO 40
      DCL = C - 1.5707963268D0
      IF (ABS(DCL).LE.HHH) GO TO 30
      RZM = ZM
      IF (SZM.GT.0.D0 .AND. RZM.GT.0.D0) GO TO 10
      IF (SZM.LT.0.D0 .AND. RZM.LT.0.D0) GO TO 10
      R = R1
      X = X1
      Y = Y1
      Z = Z1
      DS = DS/2.D0
      GO TO 20
   30 CALL GEOMAG(X,Y,Z,XM,YM,ZM,1,IYR)
      CALL SPHCAR(R,GTET,GXLA,XM,YM,ZM,-1)
      ST = ABS(SIN(GTET))
      RRH = ABS(RH/(R-RH*ST**2))
      CLA = 1.5707963D0 - ATAN(ST*SQRT(RRH))
      CLA = CLA*57.2957751D0
      CLO = GXLA*57.2957751D0
      IF (SZM.LT.0.D0) CLA = -CLA
      SSLA = 90.D0 - CLA
      SSLA = SSLA*0.017453293D0
      SN = SIN(SSLA)
C       PMI = 1/(SN*SN)
      PMI = RH/(SN*SN)
      GO TO 50
   40 CLA = 999.99D0
      CLO = 999.99D0
      PMI = 999.99D0
   50 NM = NG
      RETURN
      END
C  *********************************************************************
      SUBROUTINE SHAG(X,Y,Z,DS)
C     Similar to SUBR STEP from GEOPACK-1996 but SHAG takes into account
C     only internal sources
C     The code is re-written from Tsyganenko's subroutine STEP by
C     Natalia and Vladimir Papitashvili in mid-1980s
C     .. Scalar Arguments ..
      DOUBLE PRECISION DS,X,Y,Z
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION R11,R12,R13,R21,R22,R23,R31,R32,R33,R41,R42,R43,
     *                 R51,R52,R53
C     ..
C     .. External Subroutines ..
      EXTERNAL RIGHT
C     ..
C     .. Common blocks ..
      COMMON /A5/DS3
      DOUBLE PRECISION DS3
      SAVE /A5/
C     ..
      DS3 = -DS/3.D0
      CALL RIGHT(X,Y,Z,R11,R12,R13)
      CALL RIGHT(X+R11,Y+R12,Z+R13,R21,R22,R23)
      CALL RIGHT(X+.5D0*(R11+R21),Y+.5D0*(R12+R22),Z+.5D0*(R13+R23),R31,
     *           R32,R33)
      CALL RIGHT(X+.375D0*(R11+3.D0*R31),Y+.375D0*(R12+3.D0*R32),
     *           Z+.375D0*(R13+3.D0*R33),R41,R42,R43)
      CALL RIGHT(X+1.5D0*(R11-3.D0*R31+4.D0*R41),
     *           Y+1.5D0*(R12-3.D0*R32+4.D0*R42),
     *           Z+1.5D0*(R13-3.D0*R33+4.D0*R43),R51,R52,R53)
      X = X + .5D0*(R11+4.D0*R41+R51)
      Y = Y + .5D0*(R12+4.D0*R42+R52)
      Z = Z + .5D0*(R13+4.D0*R43+R53)
      RETURN
      END
C  *********************************************************************
      SUBROUTINE RIGHT(X,Y,Z,R1,R2,R3)
C   Similar to SUBR RHAND from GEOPACK-1996 but RIGHT takes into account
C     only internal sources
C     The code is re-written from Tsyganenko's subroutine RHAND
C     by Natalia and Vladimir Papitashvili in mid-1980s
C     .. Scalar Arguments ..
      DOUBLE PRECISION R1,R2,R3,X,Y,Z
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION B,BF,BR,BT,BX,BY,BZ,F,R,T
C     ..
C     .. External Subroutines ..
      EXTERNAL BSPCAR,IGRF,SPHCAR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC SQRT
C     ..
C     .. Common blocks ..
      COMMON /A5/DS3
      COMMON /IYR/IYR
      COMMON /NM/NM
      DOUBLE PRECISION DS3
      INTEGER IYR,NM
      SAVE /A5/,/IYR/,/NM/
C     ..
      CALL SPHCAR(R,T,F,X,Y,Z,-1)
      CALL IGRF(IYR,NM,R,T,F,BR,BT,BF)
      CALL BSPCAR(T,F,BR,BT,BF,BX,BY,BZ)
      B = DS3/SQRT(BX**2+BY**2+BZ**2)
      R1 = BX*B
      R2 = BY*B
      R3 = BZ*B
      RETURN
      END
C  *********************************************************************
      SUBROUTINE IGRF(IY,NM,R,T,F,BR,BT,BF)
C     Modified by Bill Rideout to simply call igrf11syn from igrf11.f
C      See igrf11.f for details
C
C     ---INPUT PARAMETERS:
C     IY - YEAR NUMBER (FROM 1945 UP TO 1990)
C  NM - MAXIMAL ORDER OF HARMONICS TAKEN INTO ACCOUNT (NOT MORE THAN 10)
C   R,T,F - SPHERICAL COORDINATES OF THE POINT (R IN UNITS RE=6371.2 KM,
C     COLATITUDE T AND LONGITUDE F IN RADIANS)
C     ---OUTPUT PARAMETERS:
C     BR,BT,BF - SPHERICAL COMPONENTS OF MAIN GEOMAGNETIC FIELD (in nT)
C    AUTHOR: NIKOLAI A. TSYGANENKO, INSTITUTE OF PHYSICS, ST.-PETERSBURG
C      STATE UNIVERSITY, STARY PETERGOF 198904, ST.-PETERSBURG, RUSSIA
C      (now the NASA Goddard Space Fligth Center, Greenbelt, Maryland)
      IMPLICIT NONE
C     G0, G1, and H1 are used in SUBROUTINE DIP to calculate geodipole's
C     moment for a given year
C     .. Scalar Arguments ..
      DOUBLE PRECISION BF,BR,BT,B,F,R,T,FY
      DOUBLE PRECISION RKM,LAT,LON
      INTEGER IY,NM
C     .. External Subroutines ..
      external igrf11syn
C
      FY = DBLE(IY)
      RKM = R * 6371.2
      LAT = T*57.295773
      LON = F*57.295773
      CALL igrf11syn(0,FY,2,RKM,LAT,LON,BT,BF,BR,B)
      BR = BR * (-1.0D0)
      BT = BT * (-1.0D0)
      RETURN
      END
C  *********************************************************************
      SUBROUTINE RECALC(IYR,IDAY,IHOUR,MIN,ISEC)
C     THIS IS A MODIFIED VERSION OF THE SUBROUTINE RECOMP WRITTEN BY
C     N. A. TSYGANENKO. SINCE I WANT TO USE IT IN PLACE OF SUBROUTINE
C     RECALC, I HAVE RENAMED THIS ROUTINE RECALC AND ELIMINATED THE
C     ORIGINAL RECALC FROM THIS VERSION OF THE <GEOPACK.FOR> PACKAGE.
C    THIS WAY ALL ORIGINAL CALLS TO RECALC WILL CONTINUE TO WORK WITHOUT
C     HAVING TO CHANGE THEM TO CALLS TO RECOMP.
C     AN ALTERNATIVE VERSION OF THE SUBROUTINE RECALC FROM THE GEOPACK
C     PACKAGE BASED ON A DIFFERENT APPROACH TO DERIVATION OF ROTATION
C     MATRIX ELEMENTS
C     THIS SUBROUTINE WORKS BY 20% FASTER THAN RECALC AND IS EASIER TO
C     UNDERSTAND
C     #####################################################
C     #  WRITTEN BY  N.A. TSYGANENKO ON DECEMBER 1, 1991  #
C     #####################################################
C     Modified by Mauricio Peredo, Hughes STX at NASA/GSFC Code 695,
C     September 1992
c    Modified to accept years up to 2005 (V. Papitashvili, January 2001)
c  Modified to accept dates up to year 2000 and updated IGRF coeficients
c     from 1945 (updated by V. Papitashvili, February 1995)
C     OTHER SUBROUTINES CALLED BY THIS ONE: SUN
C     IYR = YEAR NUMBER (FOUR DIGITS)
C     IDAY = DAY OF YEAR (DAY 1 = JAN 1)
C     IHOUR = HOUR OF DAY (00 TO 23)
C     MIN = MINUTE OF HOUR (00 TO 59)
C     ISEC = SECONDS OF DAY(00 TO 59)
      IMPLICIT NONE
C     .. Scalar Arguments ..
      INTEGER IDAY,IHOUR,ISEC,IYR,MIN
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION CGST,DIP1,DIP2,DIP3,DJ,DT,DY1,DY2,DY3,DZ1,DZ2,
     *                 DZ3,EXMAGX,EXMAGY,EXMAGZ,EYMAGX,EYMAGY,F1,F2,G10,
     *                 G11,GST,H11,OBLIQ,S1,S2,S3,SDEC,SGST,SLONG,SQ,
     *                 SQQ,SQR,SRASN,T,Y,Y1,Y2,Y3,Z1,Z2,Z3
      INTEGER IDE,IPR,IYE
C     ..
C     .. External Subroutines ..
      EXTERNAL SUN
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DASIN,DATAN2,DCOS,DBLE,DSIN,SQRT
C     ..
C     .. Common blocks ..
      COMMON /C1/ST0,CT0,SL0,CL0,CTCL,STCL,CTSL,STSL,SFI,CFI,SPS,CPS,
     *       SHI,CHI,HI,PSI,XMUT,A11,A21,A31,A12,A22,A32,A13,A23,A33,
     *       DS3,K,IY,BA
      DOUBLE PRECISION A11,A12,A13,A21,A22,A23,A31,A32,A33,CFI,CHI,CL0,
     *                 CPS,CT0,CTCL,CTSL,DS3,HI,PSI,SFI,SHI,SL0,SPS,ST0,
     *                 STCL,STSL,XMUT
      INTEGER IY,K
      DOUBLE PRECISION BA(8)
      SAVE /C1/
C     ..
C     .. Data statements ..
      DATA IYE,IDE,IPR/3*0/
C     ..
      IF (IYR.EQ.IYE .AND. IDAY.EQ.IDE) GO TO 10
C     IYE AND IDE ARE THE CURRENT VALUES OF YEAR AND DAY NUMBER
      IY = IYR
      IDE = IDAY
      IF (IY.LT.1945) IY = 1945
      IF (IY.GT.2005) IY = 2005
C     WE ARE RESTRICTED BY THE INTERVAL 1945-2005, FOR WHICH THE IGRF
C     COEFFICIENTS ARE KNOWN; IF IYR IS OUTSIDE THIS INTERVAL, THE
C  SUBROUTINE GIVES A WARNING (BUT DOES NOT REPEAT IT AT THE NEXT CALLS)
C      IF (IY.NE.IYR .AND. IPR.EQ.0) PRINT 9000,IYR,IY
      IF (IY.NE.IYR) IPR = 1
      IYE = IY
C    LINEAR INTERPOLATION OF THE GEODIPOLE MOMENT COMPONENTS BETWEEN THE
C     VALUES FOR THE NEAREST EPOCHS:
      IF (IY.LT.1950) THEN
         F2 = (DBLE(IY)+DBLE(IDAY)/365.D0-1945.D0)/5.D0
         F1 = 1.D0 - F2
         G10 = 30594.D0*F1 + 30554.D0*F2
         G11 = -2285.D0*F1 - 2250.D0*F2
         H11 = 5810.D0*F1 + 5815.D0*F2
      ELSE IF (IY.LT.1955) THEN
         F2 = (DBLE(IY)+DBLE(IDAY)/365.D0-1950.D0)/5.D0
         F1 = 1.D0 - F2
         G10 = 30554.D0*F1 + 30500.D0*F2
         G11 = -2250.D0*F1 - 2215.D0*F2
         H11 = 5815.D0*F1 + 5820.D0*F2
      ELSE IF (IY.LT.1960) THEN
         F2 = (DBLE(IY)+DBLE(IDAY)/365.D0-1955.D0)/5.D0
         F1 = 1.D0 - F2
         G10 = 30500.D0*F1 + 30421.D0*F2
         G11 = -2215.D0*F1 - 2169.D0*F2
         H11 = 5820.D0*F1 + 5791.D0*F2
      ELSE IF (IY.LT.1965) THEN
         F2 = (DBLE(IY)+DBLE(IDAY)/365.D0-1960.D0)/5.D0
         F1 = 1.D0 - F2
         G10 = 30421.D0*F1 + 30334.D0*F2
         G11 = -2169.D0*F1 - 2119.D0*F2
         H11 = 5791.D0*F1 + 5776.D0*F2
      ELSE IF (IY.LT.1970) THEN
         F2 = (DBLE(IY)+DBLE(IDAY)/365.D0-1965.D0)/5.D0
         F1 = 1.D0 - F2
         G10 = 30334.D0*F1 + 30220.D0*F2
         G11 = -2119.D0*F1 - 2068.D0*F2
         H11 = 5776.D0*F1 + 5737.D0*F2
      ELSE IF (IY.LT.1975) THEN
         F2 = (DBLE(IY)+DBLE(IDAY)/365.D0-1970.D0)/5.D0
         F1 = 1.D0 - F2
         G10 = 30220.D0*F1 + 30100.D0*F2
         G11 = -2068.D0*F1 - 2013.D0*F2
         H11 = 5737.D0*F1 + 5675.D0*F2
      ELSE IF (IY.LT.1980) THEN
         F2 = (DBLE(IY)+DBLE(IDAY)/365.D0-1975.D0)/5.D0
         F1 = 1.D0 - F2
         G10 = 30100.D0*F1 + 29992.D0*F2
         G11 = -2013.D0*F1 - 1956.D0*F2
         H11 = 5675.D0*F1 + 5604.D0*F2
      ELSE IF (IY.LT.1985) THEN
         F2 = (DBLE(IY)+DBLE(IDAY)/365.D0-1980.D0)/5.D0
         F1 = 1.D0 - F2
         G10 = 29992.D0*F1 + 29873.D0*F2
         G11 = -1956.D0*F1 - 1905.D0*F2
         H11 = 5604.D0*F1 + 5500.D0*F2
      ELSE IF (IY.LT.1990) THEN
         F2 = (DBLE(IY)+DBLE(IDAY)/365.D0-1985.D0)/5.D0
         F1 = 1.D0 - F2
         G10 = 29873.D0*F1 + 29775.D0*F2
         G11 = -1905.D0*F1 - 1848.D0*F2
         H11 = 5500.D0*F1 + 5406.D0*F2
      ELSE IF (IY.LT.1995) THEN
         F2 = (DBLE(IY)+DBLE(IDAY)/365.D0-1990.D0)/5.D0
         F1 = 1.D0 - F2
         G10 = 29775.D0*F1 + 29682.D0*F2
         G11 = -1848.D0*F1 - 1789.D0*F2
         H11 = 5406.D0*F1 + 5318.D0*F2
      ELSE IF (IY.LT.2000) THEN
         F2 = (DBLE(IY)+DBLE(IDAY)/365.D0-1995.D0)/5.D0
         F1 = 1.D0 - F2
         G10 = 29682.D0*F1 + 29615.D0*F2
         G11 = -1789.D0*F1 - 1728.D0*F2
         H11 = 5318.D0*F1 + 5186.D0*F2
      ELSE
         DT = DBLE(IY) + DBLE(IDAY)/365.D0 - 2000.D0
         G10 = 29615.D0 - 14.6D0*DT
         G11 = -1728.D0 + 10.7D0*DT
         H11 = 5186.D0 - 22.5D0*DT
      END IF
C     NOW CALCULATE THE COMPONENTS OF THE UNIT VECTOR EzMAG IN GEO COORD
C     SYSTEM:
C     SIN(TETA0)*COS(LAMBDA0), SIN(TETA0)*SIN(LAMBDA0), AND COS(TETA0)
C         ST0 * CL0                ST0 * SL0                CT0
      SQ = G11**2 + H11**2
      SQQ = SQRT(SQ)
      SQR = SQRT(G10**2+SQ)
      SL0 = -H11/SQQ
      CL0 = -G11/SQQ
      ST0 = SQQ/SQR
      CT0 = G10/SQR
      STCL = ST0*CL0
      STSL = ST0*SL0
      CTSL = CT0*SL0
      CTCL = CT0*CL0
C     THE CALCULATIONS ARE TERMINATED IF ONLY GEO-MAG TRANSFORMATION
C     IS TO BE DONE  (IHOUR>24 IS THE AGREED CONDITION FOR THIS CASE):
   10 IF (IHOUR.GT.24) RETURN
      CALL SUN(IY,IDAY,IHOUR,MIN,ISEC,GST,SLONG,SRASN,SDEC)
C     S1,S2, AND S3 ARE THE COMPONENTS OF THE UNIT VECTOR EXGSM=EXGSE
C     IN THE SYSTEM GEI POINTING FROM THE EARTH'S CENTER TO THE SUN:
      S1 = COS(SRASN)*COS(SDEC)
      S2 = SIN(SRASN)*COS(SDEC)
      S3 = SIN(SDEC)
      CGST = COS(GST)
      SGST = SIN(GST)
C     DIP1, DIP2, AND DIP3 ARE THE COMPONENTS OF THE UNIT VECTOR
C     EZSM=EZMAG IN THE SYSTEM GEI:
      DIP1 = STCL*CGST - STSL*SGST
      DIP2 = STCL*SGST + STSL*CGST
      DIP3 = CT0
C    NOW CALCULATE THE COMPONENTS OF THE UNIT VECTOR EYGSM IN THE SYSTEM
C     GEI BY TAKING THE VECTOR PRODUCT D x S AND NORMALIZING IT TO UNIT
C     LENGTH:
      Y1 = DIP2*S3 - DIP3*S2
      Y2 = DIP3*S1 - DIP1*S3
      Y3 = DIP1*S2 - DIP2*S1
      Y = SQRT(Y1*Y1+Y2*Y2+Y3*Y3)
      Y1 = Y1/Y
      Y2 = Y2/Y
      Y3 = Y3/Y
C     THEN IN THE GEI SYSTEM THE UNIT VECTOR Z=EZGSM=EXGSM x EYGSM=S x Y
C     HAS THE COMPONENTS:
      Z1 = S2*Y3 - S3*Y2
      Z2 = S3*Y1 - S1*Y3
      Z3 = S1*Y2 - S2*Y1
C   THE VECTOR EZGSE (HERE DZ) IN GEI HAS THE COMPONENTS (0,-SIN(DELTA),
C COS(DELTA)) = (0.,-0.397823,0.917462); HERE DELTA = 23.44214 DEG FOR
C     THE EPOCH 1978 (SEE THE BOOK BY GUREVICH OR OTHER ASTRONOMICAL
C     HANDBOOKS). HERE THE MOST ACCURATE TIME-DEPENDENT FORMULA IS USED:
      DJ = DBLE(365*(IY-1900)+(IY-1901)/4+IDAY) - 0.5D0 +
     *     DBLE(ISEC)/86400.D0
      T = DJ/36525.D0
      OBLIQ = (23.45229D0-0.0130125D0*T)/57.2957795D0
      DZ1 = 0.D0
      DZ2 = -SIN(OBLIQ)
      DZ3 = COS(OBLIQ)
C  THEN THE UNIT VECTOR EYGSE IN GEI SYSTEM IS THE VECTOR PRODUCT DZ x S
      DY1 = DZ2*S3 - DZ3*S2
      DY2 = DZ3*S1 - DZ1*S3
      DY3 = DZ1*S2 - DZ2*S1
C     THE ELEMENTS OF THE MATRIX GSE TO GSM ARE THE SCALAR PRODUCTS:
C     CHI=EM22=(EYGSM,EYGSE), SHI=EM23=(EYGSM,EZGSE),
C     EM32=(EZGSM,EYGSE)=-EM23, AND EM33=(EZGSM,EZGSE)=EM22
      CHI = Y1*DY1 + Y2*DY2 + Y3*DY3
      SHI = Y1*DZ1 + Y2*DZ2 + Y3*DZ3
      HI = ASIN(SHI)
C     TILT ANGLE: PSI=ARCSIN(DIP,EXGSM)
      SPS = DIP1*S1 + DIP2*S2 + DIP3*S3
      CPS = SQRT(1.D0-SPS**2)
      PSI = ASIN(SPS)
C     THE ELEMENTS OF THE MATRIX MAG TO SM ARE THE SCALAR PRODUCTS:
C   CFI=GM22=(EYSM,EYMAG), SFI=GM23=(EYSM,EXMAG); THEY CAN BE DERIVED
C     AS FOLLOWS:
C     IN GEO THE VECTORS EXMAG AND EYMAG HAVE THE COMPONENTS
C   (CT0*CL0,CT0*SL0,-ST0) AND (-SL0,CL0,0), RESPECTIVELY. HENCE, IN
C     GEI SYSTEM THE COMPONENTS ARE:
C     EXMAG:    CT0*CL0*COS(GST)-CT0*SL0*SIN(GST)
C            CT0*CL0*SIN(GST)+CT0*SL0*COS(GST)
C            -ST0
C     EYMAG:    -SL0*COS(GST)-CL0*SIN(GST)
C            -SL0*SIN(GST)+CL0*COS(GST)
C             0
C   THE COMPONENTS OF EYSM IN GEI WERE FOUND ABOVE AS Y1, Y2, AND Y3;
C    NOW WE ONLY HAVE TO COMBINE THE QUANTITIES INTO SCALAR PRODUCTS:
      EXMAGX = CT0*(CL0*CGST-SL0*SGST)
      EXMAGY = CT0*(CL0*SGST+SL0*CGST)
      EXMAGZ = -ST0
      EYMAGX = -(SL0*CGST+CL0*SGST)
      EYMAGY = -(SL0*SGST-CL0*CGST)
      CFI = Y1*EYMAGX + Y2*EYMAGY
      SFI = Y1*EXMAGX + Y2*EXMAGY + Y3*EXMAGZ
      XMUT = (ATAN2(SFI,CFI)+3.1415926536D0)*3.8197186342D0
C     THE ELEMENTS OF THE MATRIX GEO TO GSM ARE THE SCALAR PRODUCTS:
C     A11=(EXGEO,EXGSM), A12=(EYGEO,EXGSM), A13=(EZGEO,EXGSM),
C     A21=(EXGEO,EYGSM), A22=(EYGEO,EYGSM), A23=(EZGEO,EYGSM),
C     A31=(EXGEO,EZGSM), A32=(EYGEO,EZGSM), A33=(EZGEO,EZGSM),
C     ALL THE UNIT VECTORS IN BRACKETS ARE ALREADY DEFINED IN GEI:
C     EXGEO=(CGST,SGST,0), EYGEO=(-SGST,CGST,0), EZGEO=(0,0,1)
C     EXGSM=(S1,S2,S3),  EYGSM=(Y1,Y2,Y3),   EZGSM=(Z1,Z2,Z3)
C     AND  THEREFORE:
      A11 = S1*CGST + S2*SGST
      A12 = -S1*SGST + S2*CGST
      A13 = S3
      A21 = Y1*CGST + Y2*SGST
      A22 = -Y1*SGST + Y2*CGST
      A23 = Y3
      A31 = Z1*CGST + Z2*SGST
      A32 = -Z1*SGST + Z2*CGST
      A33 = Z3
 9000 FORMAT (/,/,1X,
     *       '****RECALC WARNS: YEAR IS OUT OF INTERVAL 1945-2005: IYR='
     *       ,I4,/,6X,'CALCULATIONS WILL BE DONE FOR IYR=',I4,/)
      RETURN
      END
C  *********************************************************************
      SUBROUTINE SUN(IYR,IDAY,IHOUR,MIN,ISEC,GST,SLONG,SRASN,SDEC)
C    CALCULATES FOUR QUANTITIES NECESSARY FOR COORDINATE TRANSFORMATIONS
C     WHICH DEPEND ON SUN POSITION (AND, HENCE, ON UNIVERSAL TIME AND
C     SEASON)
C     ---INPUT PARAMETERS:
C     IYR,IDAY,IHOUR,MIN,ISEC - YEAR, DAY, AND UNIVERSAL TIME IN HOURS,
C     MINUTES, AND SECONDS  (IDAY=1 CORRESPONDS TO JANUARY 1).
C     ---OUTPUT PARAMETERS:
C   GST - GREENWICH MEAN SIDEREAL TIME, SLONG - LONGITUDE ALONG ECLIPTIC
C     SRASN - RIGHT ASCENSION,  SDEC - DECLINATION  OF THE SUN (RADIANS)
C     THIS SUBROUTINE HAS BEEN COMPILED FROM:
C     RUSSELL C.T., COSM.ELECTRODYN., 1971, V.2,PP.184-196.
C     AUTHOR: Gilbert D. Mead
      IMPLICIT NONE
C     .. Scalar Arguments ..
      DOUBLE PRECISION GST,SDEC,SLONG,SRASN
      INTEGER IDAY,IHOUR,ISEC,IYR,MIN
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION COSD,DJ,FDAY,G,OBLIQ,RAD,SC,SIND,SLP,SOB,T,VL
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DATAN,DATAN2,DCOS,DBLE,DMOD,DSIN,SQRT
C     ..
C     .. Data statements ..
      DATA RAD/57.295779513D0/
C     ..
      IF (IYR.LT.1901 .OR. IYR.GT.2099) RETURN
      FDAY = DBLE(IHOUR*3600+MIN*60+ISEC)/86400.D0
      DJ = 365*(IYR-1900) + (IYR-1901)/4 + IDAY - 0.5D0 + FDAY
      T = DJ/36525.D0
      VL = DMOD(279.696678D0+0.9856473354D0*DJ,360.D0)
      GST = DMOD(279.690983D0+.9856473354D0*DJ+360.D0*FDAY+180.D0,
     *      360.D0)/RAD
      G = DMOD(358.475845D0+0.985600267D0*DJ,360.D0)/RAD
      SLONG = (VL+(1.91946D0-0.004789D0*T)*SIN(G)+
     *        0.020094D0*SIN(2.D0*G))/RAD
      IF (SLONG.GT.6.2831853D0) SLONG = SLONG - 6.2831853D0
      IF (SLONG.LT.0.D0) SLONG = SLONG + 6.2831853D0
      OBLIQ = (23.45229D0-0.0130125D0*T)/RAD
      SOB = SIN(OBLIQ)
      SLP = SLONG - 9.924D-5
C     THE LAST CONSTANT IS A CORRECTION FOR THE ANGULAR ABERRATION
C     DUE TO THE ORBITAL MOTION OF THE EARTH
      SIND = SOB*SIN(SLP)
      COSD = SQRT(1.D0-SIND**2)
      SC = SIND/COSD
      SDEC = ATAN(SC)
      SRASN = 3.141592654D0 - ATAN2(COS(OBLIQ)/SOB*SC,-COS(SLP)/COSD)
      RETURN
      END
C  *********************************************************************
      SUBROUTINE SPHCAR(R,TETA,PHI,X,Y,Z,J)
C     CONVERTS SPHERICAL COORDS INTO CARTESIAN ONES AND VICA VERSA
C     (TETA AND PHI IN RADIANS).
C                  J>0            J<0
C     -----INPUT:   J,R,TETA,PHI     J,X,Y,Z
C     ----OUTPUT:      X,Y,Z        R,TETA,PHI
C    AUTHOR: NIKOLAI A. TSYGANENKO, INSTITUTE OF PHYSICS, ST.-PETERSBURG
C      STATE UNIVERSITY, STARY PETERGOF 198904, ST.-PETERSBURG, RUSSIA
C      (now the NASA Goddard Space Fligth Center, Greenbelt, Maryland)
      IMPLICIT NONE
C     .. Scalar Arguments ..
      DOUBLE PRECISION PHI,R,TETA,X,Y,Z
      INTEGER J
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION SQ
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DATAN2,DCOS,DSIN,SQRT
C     ..
      IF (J.GT.0) GO TO 30
      SQ = X**2 + Y**2
      R = SQRT(SQ+Z**2)
      IF (SQ.NE.0.D0) GO TO 20
      PHI = 0.D0
      IF (Z.LT.0.D0) GO TO 10
      TETA = 0.D0
      RETURN
   10 TETA = 3.141592654D0
      RETURN
   20 SQ = SQRT(SQ)
      PHI = ATAN2(Y,X)
      TETA = ATAN2(SQ,Z)
      IF (PHI.LT.0.D0) PHI = PHI + 6.28318531D0
      RETURN
   30 SQ = R*SIN(TETA)
      X = SQ*COS(PHI)
      Y = SQ*SIN(PHI)
      Z = R*COS(TETA)
      RETURN
      END
C  *********************************************************************
      SUBROUTINE BSPCAR(TETA,PHI,BR,BTET,BPHI,BX,BY,BZ)
C     CALCULATES CARTESIAN FIELD COMPONENTS FROM SPHERICAL ONES
C     -----INPUT:   TETA,PHI - SPHERICAL ANGLES OF THE POINT IN RADIANS
C              BR,BTET,BPHI -  SPHERICAL COMPONENTS OF THE FIELD
C     -----OUTPUT:  BX,BY,BZ - CARTESIAN COMPONENTS OF THE FIELD
C    AUTHOR: NIKOLAI A. TSYGANENKO, INSTITUTE OF PHYSICS, ST.-PETERSBURG
C      STATE UNIVERSITY, STARY PETERGOF 198904, ST.-PETERSBURG, RUSSIA
C      (now the NASA Goddard Space Fligth Center, Greenbelt, Maryland)
      IMPLICIT NONE
C     .. Scalar Arguments ..
      DOUBLE PRECISION BPHI,BR,BTET,BX,BY,BZ,PHI,TETA
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION BE,C,CF,S,SF
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DCOS,DSIN
C     ..
      S = SIN(TETA)
      C = COS(TETA)
      SF = SIN(PHI)
      CF = COS(PHI)
      BE = BR*S + BTET*C
      BX = BE*CF - BPHI*SF
      BY = BE*SF + BPHI*CF
      BZ = BR*C - BTET*S
      RETURN
      END
C  *********************************************************************
      SUBROUTINE GEOMAG(XGEO,YGEO,ZGEO,XMAG,YMAG,ZMAG,J,IYR)
C   CONVERTS GEOCENTRIC (GEO) TO DIPOLE (MAG) COORDINATES OR VICA VERSA.
C     IYR IS YEAR NUMBER (FOUR DIGITS).
C                           J>0                J<0
C     -----INPUT:  J,XGEO,YGEO,ZGEO,IYR   J,XMAG,YMAG,ZMAG,IYR
C     -----OUTPUT:    XMAG,YMAG,ZMAG        XGEO,YGEO,ZGEO
C    AUTHOR: NIKOLAI A. TSYGANENKO, INSTITUTE OF PHYSICS, ST.-PETERSBURG
C      STATE UNIVERSITY, STARY PETERGOF 198904, ST.-PETERSBURG, RUSSIA
C      (now the NASA Goddard Space Fligth Center, Greenbelt, Maryland)
      IMPLICIT NONE
C     .. Scalar Arguments ..
      DOUBLE PRECISION XGEO,XMAG,YGEO,YMAG,ZGEO,ZMAG
      INTEGER IYR,J
C     .. Modified by Bill Rideout to simply call Geopack_2005
      EXTERNAL TS_GEOMAG
      call TS_RECALC(IYR,180,0,0,0)
      CALL TS_GEOMAG(XGEO,YGEO,ZGEO,XMAG,YMAG,ZMAG,J)

      RETURN
      END
C  *********************************************************************
      SUBROUTINE MAGSM(XMAG,YMAG,ZMAG,XSM,YSM,ZSM,J)
C CONVERTS DIPOLE (MAG) TO SOLAR MAGNETIC (SM) COORDINATES OR VICA VERSA
C                    J>0              J<0
C     -----INPUT: J,XMAG,YMAG,ZMAG     J,XSM,YSM,ZSM
C     ----OUTPUT:    XSM,YSM,ZSM       XMAG,YMAG,ZMAG
C  ATTENTION: SUBROUTINE RECALC MUST BE CALLED BEFORE MAGSM IN TWO CASES
C     /A/  BEFORE THE FIRST USE OF MAGSM
C     /B/  IF THE CURRENT VALUES OF IYEAR,IDAY,IHOUR,MIN,ISEC ARE
C          DIFFERENT FROM THOSE IN THE PRECEDING CALL OF  MAGSM
C    AUTHOR: NIKOLAI A. TSYGANENKO, INSTITUTE OF PHYSICS, ST.-PETERSBURG
C      STATE UNIVERSITY, STARY PETERGOF 198904, ST.-PETERSBURG, RUSSIA
C      (now the NASA Goddard Space Fligth Center, Greenbelt, Maryland)
      IMPLICIT NONE
C     .. Scalar Arguments ..
      DOUBLE PRECISION XMAG,XSM,YMAG,YSM,ZMAG,ZSM
      INTEGER J
C     ..
C     .. Common blocks ..
      COMMON /C1/A,SFI,CFI,B,AB,K,IY,BA
      DOUBLE PRECISION CFI,SFI
      INTEGER IY,K
      DOUBLE PRECISION A(8),AB(10),B(7),BA(8)
      SAVE /C1/
C     ..
      IF (J.LT.0) GO TO 10
      XSM = XMAG*CFI - YMAG*SFI
      YSM = XMAG*SFI + YMAG*CFI
      ZSM = ZMAG
      RETURN
   10 XMAG = XSM*CFI + YSM*SFI
      YMAG = YSM*CFI - XSM*SFI
      ZMAG = ZSM
      RETURN
      END
C  *********************************************************************
      SUBROUTINE SMGSM(XSM,YSM,ZSM,XGSM,YGSM,ZGSM,J)
C CONVERTS SOLAR MAGNETIC (SM) TO SOLAR MAGNETOSPHERIC (GSM) COORDINATES
C     OR VICA VERSA.
C                  J>0                 J<0
C     -----INPUT: J,XSM,YSM,ZSM        J,XGSM,YGSM,ZGSM
C     ----OUTPUT:  XGSM,YGSM,ZGSM       XSM,YSM,ZSM
C  ATTENTION: SUBROUTINE RECALC MUST BE CALLED BEFORE SMGSM IN TWO CASES
C     /A/  BEFORE THE FIRST USE OF SMGSM
C     /B/  IF THE CURRENT VALUES OF IYEAR,IDAY,IHOUR,MIN,ISEC ARE
C          DIFFERENT FROM THOSE IN THE PRECEDING CALL OF SMGSM
C    AUTHOR: NIKOLAI A. TSYGANENKO, INSTITUTE OF PHYSICS, ST.-PETERSBURG
C      STATE UNIVERSITY, STARY PETERGOF 198904, ST.-PETERSBURG, RUSSIA
C      (now the NASA Goddard Space Fligth Center, Greenbelt, Maryland)
      IMPLICIT NONE
C     .. Scalar Arguments ..
      DOUBLE PRECISION XGSM,XSM,YGSM,YSM,ZGSM,ZSM
      INTEGER J
C     ..
C     .. Common blocks ..
      COMMON /C1/A,SPS,CPS,B,K,IY,AB
      DOUBLE PRECISION CPS,SPS
      INTEGER IY,K
      DOUBLE PRECISION A(10),AB(8),B(15)
      SAVE /C1/
C     ..
      IF (J.LT.0) GO TO 10
      XGSM = XSM*CPS + ZSM*SPS
      YGSM = YSM
      ZGSM = ZSM*CPS - XSM*SPS
      RETURN
   10 XSM = XGSM*CPS - ZGSM*SPS
      YSM = YGSM
      ZSM = XGSM*SPS + ZGSM*CPS
      RETURN
      END
