C     Written by Shunrong Zhang
C     Imported into Madrigal from modelRecovery/fullAnalytic/
C     isrim.f on 10/21/2005 - Was revision 1.6
C
C     Modified to change COEFDIR to Madrigal path, added comment
C     on 4 = ion drift
C
C     $Id: isrim.f,v 1.5 2009/01/20 20:19:56 brideout Exp $
C
C
      SUBROUTINE ISRIM(KINST,DOY,SLT,ALT,GLAT,F107,AP,IPAR,OUTPUT)
C
C     INCOHERENT SCATTER RADAR IONOSPHERIC MODEL (ISRIM)
C
C     ISRIMs have been developed from long-term datasets of seven 
C     incoherent scatter radars spanning invariant latitudes from 
C     25 to 75 degrees in American, European and Asian longitudes
C     at Svalbard, Tromso, Sondrestrom, Millstone Hill, St. Santin, 
C     Arecibo and Shigaraki. These models represent electron density, 
C     ion and electron temperatures, and ion drifts in the E and F 
C     regions, giving a comprehensive quantitative description of
C     ionospheric properties.
C
C     REFERENCES
C
C     Zhang, S.-R., J. M. Holt, A. P. van Eyken, M. McCready, C. 
C     Amory-Mazaudier, S. Fukao, and M. Sulzer, Ionospheric local model 
C     and climatology from long-term databases of multiple incoherent 
C     scatter radars, Geophys. Res. Lett. 32, L20102, doi:10.1029/
C     2005GL023603, 2005.
C
C     Zhang, S.-R., J. M. Holt, A. M. Zalucha, and C. Amory-Mazaudier, 
C     Mid-latitude ionospheric plasma temperature climatology and 
C     empirical model based on Saint Santin incoherent scatter radar data 
C     from 1966-1987, J. Geophys. Res., 109, A11311, 
C     doi:10.1029/2004JA010709, 2004.
C
C     Holt, J. M., S.-R. Zhang, and M. J. Buonsanto, Regional and local 
C     ionospheric models based on Millstone Hill incoherent scatter radar 
C     data, Geophys. Res. Lett.,, 29(8), 10.1029/2002GL014678, 2002.
C
C
C     ONLINE VERSION
C
C      http://madrigal.haystack.mit.edu/models/
C
C
C     CONTACT INFO
C
C       Dr. Shun-Rong Zhang (shunrong@haystack.mit.edu)
C       Dr. John M. Holt
C       MIT Haystack Observatory
C       Route 40,
C       Westford,  MA 01886
C       USA
C       PHONE: 781-981-5725; FAX: 781-981-5766
C
C
C     REVISIONS
C
C     Updated by SRZ, MAY 2007 --
C     ALLOW FOR KINST = 30 FOR MH LOCAL MODEL
C
C     Updated by SRZ, October 2005 --
C     READ IN ALL COEFFICIENT FILES, INSTEAD OF READING IN
C     AS NEEDED FOR A SPECIFIC PARAMETER. THIS CHANGE IS TO ACCOMMODATE
C     THE MADRIGAL APPLICATIONS TO IMPROVE EFFICIENCY WHEN MULTIPLE
C     PARAMETERS ARE COMPUTERED. 
C
C     Updated by SRZ, May 2005 ---
C     ADD KINST (INSTRUMENT CODE); REMOVE THE FUNCTION OF OUTPUT(5)
C
C     FIRST Version by SRZ, July 2002 ---
C     MILLSTONE HILL IONOSPHERIC MODEL
C     THIS SUBROUTINE RETURNS IONOSPHERIC PARAMETERS (NE, TE, AND TI)
C     BASED ON MILLSTONE HILL INCOHERENT SCATTER RADAR OBSERVATION MADE
C     DURING 1976-2001.
C
C
C     INPUTS
C
C        KINST     - STATION/INSTRUMENT CODE
C             31 -> MILLSTONE HILL STEERABLE (regional model)
C             32 -> MILLSTONE HILL ZENITH (local model)
C             30 -> MILLSTONE HILL ZENITH (local model)
C         !!  10 -> JICAMARCA - NOT AVAILABLE!!
C             20 -> ARECIBO
C             25 -> MU RADAR
C             40 -> ST. SANTIN
C             72 -> EISCAT TROMSO
C             80 -> SONDRESTROM (local)
C             95 -> EISCAT SVALBARD
C              0 -> AMERICA REGIONAL MODEL (GEODETC 15-70 DEGREES)
C              1 -> WESTERN eUROPEAN MODEL (GEODETIC 45-78 DEGREES)
C     DOY: DAY NUMBER OF YEAR;      DOUBLE PRECISION SCALAR
C            [ 1 - 365 ]
C     SLT: LOCAL TIME (HOUR);       DOUBLE PRECISION SCALAR
C            [ 0 - 24]
C     ALT: GEODETIC ALTITUDE (KM);  DOUBLE PRECISION SCALAR
C          LOCAL MODEL:    [100 - 1000]; for Millstone Hill
C          LOCAL MODEL:    [200 - 500]; for Shigaraki
C          LOCAL MODEL:    [100 - 600]; for other sites
C          REGIONAL MODEL: [100 - 600]: America/Europe
C          REGIONAL MODEL: [200 - 600]: Millstone Hill
C     GLAT: (ALSO AS OUTPUT IF LOCAL MODEL IS CALLED)
C          GEODETIC LATITUDE DEGREE, DOUBLE PRECISION SCALAR
C          NEEDED ONLY FOR REGIONAL MODELS
C     F107: SOLAR 10.7 CM FLUX FOR THE PREVIOUS DAY;
C                                   DOUBLE PRECISION SCALAR
C     AP: 3-HOUR AP INDEX AT 3 HOURS BEFORE THE CURRENT TIME;
C                                   DOUBLE PRECISION SCALAR
C     IPAR: IONOSPHERIC PARAMETER TO BE RETURNED;
C                                   INTEGER SCALAR
C           = 1, FOR NE
C           = 2, FOR TE
C           = 3, FOR TI
C           = 4, FOR PARALLEL ION DRIFT, + UPWARD
C     OUTPUT(4): FLAG FOR HMAX AND NMAX OUTPUT;
C                                    DOUBLE PRECISION SCALAR
C                SET OUTPUT(4) TO ANY VALUE >0, THEN OUTPUT(2) AND
C                OUTPUT(3) RETURN HMAX AND NMAX.
C
C     OUTPUT
C
C     OUTPUT: OUTPUT DATA;           DOUBLE PRECISION ARRAY, 10 DIMS
C        OUTPUT(1): OUTPUT DATA; NE 1/M^3 FOR IPAR=1
C                                TE, K FOR IPAR=2
C                                TI, K FOR IPAR=3
C        OUTPUT(2): HMAX, KM FOR IPAR=1 AND OUTPUT(4)>0
C        OUTPUT(3): NMAX, 1/M^3 FOR IPAR=1 AND OUTPUT(4)>0
C        OUTPUT(4): MODEL DIAGNOSTIC STATUS
C                   = 0  NORMAL
C                   = -1 STATION NOT INCLUDED
C                   = -2 PARAMETER NOT INCLUDED
C                   = -3 STATION AND PARAMETER NOT MATCH
C     GLAT: GEODETIC LATITUDE DEGREE, DOUBLE PRECISION SCALAR
C           ONLY WHEN A LOCAL MODEL IS COMPUTED
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION ALT,AP,DOY,F107,GLAT,SLT
      INTEGER IPAR,KINST
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION OUTPUT(10)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION DMAX,HMAX
      INTEGER I,IAREA,IIPAR,IKINST,ISN,J,NCALLED,IND
      CHARACTER*2 CPAR
      CHARACTER*128 COEFDIR,COEFILE,MADDIR
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION GLATS(11),WK1(1000),WK2(1000)
      INTEGER STA(4),STATUS(4)
      CHARACTER*2 SNAME(11)
C     ..
C     .. External Subroutines ..
      EXTERNAL FINDPEAK,PARAMETER3D,PARAMETER4D,READCOEF,SETKNOTS3D,
     *         SETKNOTS4D
C     ..
C     .. Save statement ..
      SAVE NCALLED,IKINST,IIPAR,STATUS
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC INDEX
C     ..
C     .. Data statements ..
C
      DATA SNAME/'AR','MU','MH','ST','TR','SO','ES','RM','EU','AM','M2'/
      DATA GLATS/18D0,35D0,43D0,45D0,70D0,67D0,79D0,43D0,67D0,0D0,43D0/
      DATA IKINST/999/,IIPAR/999/
C     ..
C     .. DIRECTORY OF COEFFICIENT FILES ...............
C      COEFDIR='coefiles/'
C     this line modified by B. Rideout
C     path should be automatically modified during installation
      MADDIR = '/Users/mnicolls/Documents/Work/Madrigal'
      IND = index(MADDIR, ' ') - 1
      COEFDIR = MADDIR(1:IND) // '/source/madf/geolib/3D4DCoef/'
C      COEFDIR='/opt/madrigal/empiricalModels/3D4DCoef/'
C     ..................................................
C
      IAREA = 2
      OUTPUT(4) = 0D0
      IF (KINST.EQ.30) KINST = 32
      IF (AP.GT.100) AP = 100D0
      IF (KINST.EQ.31) IAREA = 1
      IF (KINST.EQ.0) IAREA = 1
      IF (KINST.EQ.1) IAREA = 1
C     determine the cofficient file name to be read
      CPAR = 'NO'
      IF (IPAR.EQ.1) CPAR = 'Ne'
      IF (IPAR.EQ.2) CPAR = 'Te'
      IF (IPAR.EQ.3) CPAR = 'Ti'
      IF (IPAR.EQ.4) CPAR = 'Vo'
      ISN = 999
      IF (KINST.EQ.20) ISN = 1
      IF (KINST.EQ.25) ISN = 2
      IF (KINST.EQ.32) ISN = 3
      IF (KINST.EQ.40) ISN = 4
      IF (KINST.EQ.72) ISN = 5
      IF (KINST.EQ.80) ISN = 6
      IF (KINST.EQ.95) ISN = 7
      IF (KINST.EQ.31) ISN = 8
      IF (KINST.EQ.1) ISN = 9
      IF (KINST.EQ.0) ISN = 10
      IF (KINST.EQ.33) ISN = 11
C     .. invalid inputs ..
      IF (ISN.EQ.999) OUTPUT(4) = -1D0
      IF (CPAR.EQ.'NO') OUTPUT(4) = -2D0
      IF (KINST.EQ.25 .AND. CPAR.EQ.'Vo') OUTPUT(4) = -3D0
      IF (KINST.EQ.40 .AND. CPAR.EQ.'Vo') OUTPUT(4) = -3D0
      IF (KINST.EQ.72 .AND. CPAR.EQ.'Vo') OUTPUT(4) = -3D0
      IF (KINST.EQ.95 .AND. CPAR.EQ.'Vo') OUTPUT(4) = -3D0
      IF (KINST.EQ.1 .AND. CPAR.EQ.'Vo') OUTPUT(4) = -3D0
      IF (KINST.EQ.0 .AND. CPAR.EQ.'Vo') OUTPUT(4) = -3D0
      IF (KINST.EQ.31 .AND. CPAR.EQ.'Vo') OUTPUT(4) = -3D0
      IF (KINST.EQ.33 .AND. CPAR.EQ.'Vo') OUTPUT(4) = -3D0
      
      IF (ALT.LT.100D0.OR.ALT.GT.1000D0) OUTPUT(4) = -4D0
C      IF (ALT.LT.200D0.AND.IAREA.EQ.1) OUTPUT(4) = -4D0
       IF (ALT.LT.200D0.AND.KINST.EQ.31) OUTPUT(4) = -4D0
      IF (ALT.GT.600D0.AND.IAREA.EQ.1) OUTPUT(4) = -4D0
      IF (ALT.LT.200D0.AND.KINST.EQ.25) OUTPUT(4) = -4D0
      IF (ALT.GT.450D0.AND.KINST.EQ.25) OUTPUT(4) = -4D0
      
      IF (OUTPUT(4).LT.0D0) THEN
         OUTPUT(1) = -999D0
         RETURN
      END IF
C
      IF (IAREA.NE.1) GLAT = GLATS(ISN)
      COEFILE = COEFDIR(1:INDEX(COEFDIR,' ')-1)//SNAME(ISN)//'Coef3D'
      IF (IAREA.EQ.1) THEN
         COEFILE = COEFDIR(1:INDEX(COEFDIR,' ')-1)//SNAME(ISN)//'Coef4D'
      END IF
C     to check if the coefficient file has been read into common block

      IF (IKINST.NE.KINST) NCALLED = 0
      IF (NCALLED.NE.12345) THEN
         CALL READCOEF(0,COEFILE,STATUS)
         NCALLED = 12345
      END IF

      DO I = 1,4
         IF (STATUS(I).NE.0 .AND. IPAR.EQ.I) THEN
            OUTPUT(1) = -999D0
            OUTPUT(4) = -3D0
            RETURN
         END IF
      END DO

      IF (IIPAR.NE.IPAR .OR. KINST.NE.IKINST) THEN
         CALL READCOEF(IPAR,COEFILE,STA)
         IF (IAREA.EQ.2) CALL SETKNOTS3D
         IF (IAREA.EQ.1) CALL SETKNOTS4D
         IIPAR = IPAR
         IKINST = KINST
      END IF

      IF (IAREA.EQ.2) CALL PARAMETER3D(DOY,SLT,GLAT,ALT,F107,AP,
     *                     OUTPUT(1))
      IF (IAREA.EQ.1) CALL PARAMETER4D(DOY,SLT,GLAT,ALT,F107,AP,
     *                     OUTPUT(1))
      IF (IPAR.EQ.1 .AND. OUTPUT(1).LE.0D0) OUTPUT(4) = -5D0
      IF (IPAR.EQ.1 .AND. OUTPUT(1).LE.0D0) OUTPUT(1) = -OUTPUT(1)

      IF (IPAR.EQ.1 .AND. KINST.EQ.31) OUTPUT(1) = OUTPUT(1)*1D12
      IF (IPAR.EQ.1 .AND. KINST.NE.31) OUTPUT(1) = 10**OUTPUT(1)
C     find hmax and Nmax at 1 km resolution
      IF (IPAR.EQ.1 .AND. OUTPUT(4).GT.0.d0) THEN
         DO I = 1,1000
            WK1(I) = 0.d0
            WK2(I) = 0.d0
         END DO
         DO I = 200,500,1
            J = I - 199
            WK1(J) = I*1.d0
            IF (IAREA.EQ.2) CALL PARAMETER3D(DOY,SLT,GLAT,WK1(J),F107,
     *                           AP,WK2(J))
            IF (IAREA.EQ.1) CALL PARAMETER4D(DOY,SLT,GLAT,WK1(J),F107,
     *                           AP,WK2(J))
         END DO
         CALL FINDPEAK(SLT,WK1,WK2,HMAX,DMAX)
         OUTPUT(2) = HMAX
         IF (IPAR.EQ.1 .AND. KINST.EQ.31) OUTPUT(3) = DMAX*1D12
         IF (IPAR.EQ.1 .AND. KINST.NE.31) OUTPUT(3) = 10**DMAX
         OUTPUT(4) = -999.D0
      END IF
      RETURN
      END
C
C     -------------------------------------------------------------
C
      SUBROUTINE READCOEF(SW,COEFILEPATH,STATUS)
C
C     INPUT: SW, COEFILEPATH
C     OUTPUT: STATUS(4)
C     SW: SW=0 IF ONLY READ COEFFICIENTS FROM FILES
C         SW.NE.0 IF ONLY LOAD COEFFICIENTS INTO COMMON BLOCKS
C              FOR THE SPECIFIC PARAMETER; I.E.,
C              SW=1, NE
C              SW=2, TE
C              SW=3, TI
C              SW=4, VO
C
C     .. Scalar Arguments ..
      INTEGER SW
      CHARACTER*(*) COEFILEPATH
C     ..
C     .. Local Scalars ..
      INTEGER I,IAREA,IC,IPAR,J
      CHARACTER*2 CPAR
      CHARACTER*256 COEFILE
C     ..
C     .. Common blocks ..

      COMMON /BASINFO1/KKS,NX,NY,NS,NG
      COMMON /BASINFO14/KKS4,NX4,NY4,NS4,NG4
      COMMON /BASINFO2/XA,XB,YA,YB,SA,SB,GA,GB
      COMMON /BASINFO24/XA4,XB4,YA4,YB4,SA4,SB4,GA4,GB4
      COMMON /COEF/COEFS,N,M,ICC
      COMMON /COEF4/COEFS4,N4,M4,ICC4
      COMMON /NODES/TT,WL,NBKL
      COMMON /NODES4/TT4,WL4,NBKL4
      DOUBLE PRECISION GA,GB,SA,SB,XA,XB,YA,YB
      INTEGER ICC,KKS,M,N,NBKL,NG,NS,NX,NY
      DOUBLE PRECISION COEFS(10,5000),COEFS4(4,10,5000),GA4(4),GB4(4),
     *                 SA4(4),SB4(4),TT(100),TT4(4,100),WL(50),
     *                 WL4(4,50),XA4(4),XB4(4),YA4(4),YB4(4)
      INTEGER ICC4(4),KKS4(4),M4(4),N4(4),NBKL4(4),NG4(4),NS4(4),NX4(4),
     *        NY4(4)
C     ..
C     .. Common blocks ..
C     ..

C     .. Array Arguments ..
      INTEGER STATUS(4)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC INDEX
C     ..
      IF (SW.EQ.0) THEN
         DO 20 J = 1,4
            IF (J.EQ.1) CPAR = 'Ne'
            IF (J.EQ.2) CPAR = 'Te'
            IF (J.EQ.3) CPAR = 'Ti'
            IF (J.EQ.4) CPAR = 'Vo'
            COEFILE = COEFILEPATH(1:INDEX(COEFILEPATH,' ')-1)//CPAR//
     *                '.out'

            OPEN (1,FILE=COEFILE(1:INDEX(COEFILE,' ')-1),STATUS='OLD',
     *           IOSTAT=STATUS(J))
C      READ (1,FMT=9000) IPAR,IAREA,N,M,ICC,KKS,NX,NY,NS,NG,XA,XB,YA,YB,
C           *  SA,SB,GA,GB
            IF (STATUS(J).NE.0) GO TO 20
            READ (1,FMT=*) IPAR,IAREA,N4(J),M4(J),ICC4(J),KKS4(J),
     *        NX4(J),NY4(J),NS4(J),NG4(J),XA4(J),XB4(J),YA4(J),YB4(J),
     *        SA4(J),SB4(J),GA4(J),GB4(J)
            READ (1,FMT='(100F7.1)') (TT4(J,I),I=1,NY4(J)+KKS4(J))
            DO I = 1,N4(J)
               READ (1,FMT=9010) (COEFS4(J,IC,I),IC=1,ICC4(J))
            END DO
            NBKL4(J) = 0
            WL4(J,1) = 0D0
            READ (1,END=10,FMT='(I4)') NBKL4(J)
            IF (NBKL4(J).LE.0) GO TO 10
            READ (1,FMT='(100F7.1)') (WL4(J,I),I=1,
     *        NBKL4(J)+KKS4(J)+KKS4(J)-2)
   10       CLOSE (1)
 9000       FORMAT (2i3,I6,I7,6I3,8F8.2)
 9010       FORMAT (4D15.7)
   20    CONTINUE
      END IF

      IF (SW.NE.0) THEN
         KKS = KKS4(SW)
         NX = NX4(SW)
         NY = NY4(SW)
         NS = NS4(SW)
         NG = NG4(SW)
         XA = XA4(SW)
         XB = XB4(SW)
         YA = YA4(SW)
         YB = YB4(SW)
         SA = SA4(SW)
         SB = SB4(SW)
         GA = GA4(SW)
         GB = GB4(SW)
         N = N4(SW)
         M = M4(SW)
         ICC = ICC4(SW)
         NBKL = NBKL4(SW)
         DO I = 1,100
            TT(I) = TT4(SW,I)
         END DO
         DO I = 1,50
            WL(I) = WL4(SW,I)
         END DO
         DO I = 1,10
            DO J = 1,5000
               COEFS(I,J) = COEFS4(SW,I,J)
            END DO
         END DO
      END IF

      RETURN
      END
C
C     -------------------------------------------------------------
C
      SUBROUTINE SETKNOTS3D
      INCLUDE 'basis3.h'
C     .. Local Scalars ..
      INTEGER I,KS
C     ..
C     .. External Subroutines ..
C     ..
C     .. Common blocks ..
      COMMON /BASINFO1/KKS,NX,NY,NS,NG
      COMMON /BASINFO2/XA,XB,YA,YB,SA,SB,GA,GB
      COMMON /NODES/TT,WL,NBKL
      DOUBLE PRECISION GA,GB,SA,SB,XA,XB,YA,YB
      INTEGER KKS,NBKL,NG,NS,NX,NY
      DOUBLE PRECISION TT(100),WL(50)
C     ..
      KS = KKS
      DO I = 1,NY + KKS
         T2(I) = TT(I)
      END DO
      XA = 0.d0
      XB = 2.0D0*3.1415926535897D0
      SA = 0.d0
      SB = 2.0D0*3.1415926535897D0
      N1 = NX
      N2 = NY
      N3 = NS
      RETURN
      END
C
C     -------------------------------------------------------------
C
      SUBROUTINE SETKNOTS4D
      INCLUDE 'basis4.h'
C     .. Local Scalars ..
      INTEGER I,KS
C     ..
C     .. External Subroutines ..
C     ..
C     .. Common blocks ..
      COMMON /BASINFO1/KKS,NX,NY,NS,NG
      COMMON /BASINFO2/XA,XB,YA,YB,SA,SB,GA,GB
      COMMON /NODES/TT,WL,NBKL
      DOUBLE PRECISION GA,GB,SA,SB,XA,XB,YA,YB
      INTEGER KKS,NBKL,NG,NS,NX,NY
      DOUBLE PRECISION TT(100),WL(50)
C     ..
      KS = KKS
      DO I = 1,NY + KKS
         T2(I) = TT(I)
      END DO
C     ... IF LATITUDINAL VARIATION IS A B-SPLINE ..
C     ..   COMMENT OUT BELOW IF POLYNOMIA USED
      IF (NBKL.LE.0) THEN
         T4(1) = 12345D0
      ELSE
         DO I = 1,NBKL + KKS + KKS - 2
            T4(I) = WL(I)
         END DO
      END IF
C     ..
      XA = 0.d0
      XB = 2.0D0*3.1415926535897D0
      SA = 0.d0
      SB = 2.0D0*3.1415926535897D0
      N1 = NX
      N2 = NY
      N3 = NS
      N4 = NG
      RETURN
      END
C
C     -------------------------------------------------------------
C
      SUBROUTINE PARAMETER3D(DOY,SLT,LAT,ALT,F107,AP,V3D)
      INCLUDE 'basis3.h'

C     .. Scalar Arguments ..
      DOUBLE PRECISION ALT,AP,DOY,F107,LAT,SLT,V3D
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION APM,F107M,TWOPI
      INTEGER I,IC
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION BAS(30000),WK(10),WK2(10)
C     ..
C     .. External Subroutines ..
      EXTERNAL SPLBAS3
C     ..
C     .. Common blocks ..
      COMMON /COEF/COEFS,N,M,ICC
      INTEGER ICC,M,N
      DOUBLE PRECISION COEFS(10,5000)
C     ..
      TWOPI = 2.d0*3.1415926535897d0
      IX1(1) = 1
      IX2(1) = 1
      IX3(1) = 1
      X1(1) = SLT
      X2(1) = ALT
      X3(1) = DOY
C     for peridiocal spline
      X1(1) = X1(1)*TWOPI/24.d0
      X3(1) = X3(1)*TWOPI/365.d0
C     determine basis value, bas
      CALL SPLBAS3(N,1,X1(1),BAS)
      DO IC = 1,ICC
         WK(IC) = 0.d0
         DO I = 1,N
            WK(IC) = WK(IC) + BAS(I)*COEFS(IC,I)
         END DO
      END DO
      F107M = 135.D0
      APM = 15.D0
      DO IC = 1,ICC
         WK2(IC) = 0.d0
      END DO
      WK2(1) = 1.d0
      WK2(2) = (F107-F107M)/100.d0
      WK2(3) = (AP-APM)/10.d0
      WK2(4) = WK2(2)*WK2(3)
      V3D = 0.d0
      DO IC = 1,ICC
         V3D = V3D + WK(IC)*WK2(IC)
      END DO
      RETURN
      END
C
C     -------------------------------------------------------------
C
      SUBROUTINE PARAMETER4D(DOY,SLT,LAT,ALT,F107,AP,V4D)
      INCLUDE 'basis4.h'
C     .. Scalar Arguments ..
      DOUBLE PRECISION ALT,AP,DOY,F107,LAT,SLT,V4D
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION APM,F107M,TWOPI
      INTEGER I,IC
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION BAS(30000),WK(10),WK2(10)
C     ..
C     .. External Subroutines ..
      EXTERNAL SPLBASP4
C     ..
C     .. Common blocks ..
      COMMON /COEF/COEFS,N,M,ICC
      INTEGER ICC,M,N
      DOUBLE PRECISION COEFS(10,5000)
C     ..
      TWOPI = 2.d0*3.1415926535897d0
      IX1(1) = 1
      IX2(1) = 1
      IX3(1) = 1
      IX4(1) = 1
      X1(1) = SLT
      X2(1) = ALT
      X3(1) = DOY
      X4(1) = LAT
C     for peridiocal spline
      X1(1) = X1(1)*TWOPI/24.d0
      X3(1) = X3(1)*TWOPI/365.d0
C     determine basis value, bas
      CALL SPLBASP4(N,1,X1(1),BAS)
      DO IC = 1,ICC
         WK(IC) = 0.d0
         DO I = 1,N
            WK(IC) = WK(IC) + BAS(I)*COEFS(IC,I)
         END DO
      END DO
C     print*,t4(1),wk(1),coefs(1,1),coefs(2,1),coefs(3,1)
      F107M = 135.D0
      APM = 15.D0
      DO IC = 1,ICC
         WK2(IC) = 0.d0
      END DO
      WK2(1) = 1.d0
      WK2(2) = (F107-F107M)/100.d0
      WK2(3) = (AP-APM)/15.d0
      WK2(4) = WK2(2)*WK2(3)
      V4D = 0.d0
      DO IC = 1,ICC
         V4D = V4D + WK(IC)*WK2(IC)
      END DO
C      WRITE(18,'(F8.1,1000E15.7)')ALT,(BAS(I),I=1,N)
      RETURN
      END
C
C     -------------------------------------------------------------
C
      SUBROUTINE FINDPEAK(SLT,ALT,PROFILE,HMAX,DMAX)
C     INPUT    alt,profile,slt
C     OUTPUT   hmax,dmax
C     .. Scalar Arguments ..
      DOUBLE PRECISION DMAX,HMAX,SLT
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION ALT(1000),PROFILE(1000)
C     ..
C     .. Local Scalars ..
      INTEGER I
C     ..
      HMAX = 150.d0
      DMAX = -9.d0
      DO 10 I = 1,1000
         IF (ALT(I).GT.500.d0) RETURN
         IF ((SLT.LT.5.OR.SLT.GT.22) .AND. ALT(I).LT.250.d0) GO TO 10
         IF (PROFILE(I).GT.DMAX) HMAX = ALT(I)
         IF (PROFILE(I).GT.DMAX) DMAX = PROFILE(I)
   10 CONTINUE
      RETURN
      END
