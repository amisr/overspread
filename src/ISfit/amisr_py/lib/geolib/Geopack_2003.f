c     <pre>
c
C  =====================================================================
C     Downloaded from ftp://nssdcftp.gsfc.nasa.gov/models/magnetospheric/tsyganenko
C     by B. Rideout on Feb. 20, 2004.  A description of this
C     code can be found
C     at http://nssdc.gsfc.nasa.gov/space/model/magnetos/data-based/modeling.html
C     The following changes were made from imported version:
C
C     1. ran script modifyFortran.py to remove 
C        to avoid conflicts with geo-cgm
C     2. remake_file script (nag_apt + tcl) run
C     3. set any variables that generated uninitialized variable warnings to zero
C     4. added common block br_err to signal errors back to trace subroutine
C
c
c     ##########################################################################
c     #                                                                        #
c     #                             GEOPACK-2003                               #
c     #                     (MAIN SET OF FORTRAN CODES)                        #
c     #                                                                        #
c     ##########################################################################
C
c
cThis collection of subroutines is a result of several upgrades of the original package
cwritten by N. A. Tsyganenko in 1978-1979. This version is dated April 22, 2003.
c
cThis package represents an in-depth revision of the previous version, with significant
cchanges in the format of calling statements. Users should familiarize themselves with
cthe new formats and rules, and accordingly adjust their source codes, as specified
cbelow. Please consult the documentation file geopack-2003.doc (also available from this
c     site) for detailed descriptions of individual subroutines.
c
cThe following changes were made to the previous release of GEOPACK (Jan 5, 2001).
c
c     (1) Subroutine IGRF, calculating the Earth's main field:
c(a) Two versions of this subroutine are provided here. In the first one (IGRF_GSM)
cboth input (position) and output (field components) are in the Geocentric Solar-
cMagnetospheric Cartesian coordinates, while the second one (IGRF_GEO) uses sphe-
c rical geographical (geocentric) coordinates, as in the older releases.
c(b) updating of all expansion coefficients is now made separately in the s/r TS_RECALC,
cwhich also takes into account the secular change of the coefficients within
ca given year (at the Earth's surface, the rate of the change can reach 7 nT/month).
c(c) the optimal length of spherical harmonic expansions is now automatically set
cinside the code, based on the radial distance, so that the deviation from the
cfull-length approximation does not exceed 0.01 nT. (In the previous versions,
cthe upper limit NM of the order of harmonics had to be specified by users),
c
c(2) Subroutine DIP, calculating the Earth's field in the dipole approximation:
c(a) no longer accepts the tilt angle via the list of formal parameters. Instead,
cthe sine SPS and cosine CPS of that angle are now forwarded into DIP via the
cfirst common block /GEOPACK1/.  Accordingly, there are two options: (i) to
ccalculate SPS and CPS by calling TS_RECALC before calling DIP, or (ii) to specify
cthem explicitly. In the last case, SPS and CPS should be specified AFTER the
cinvocation of TS_RECALC (otherwise they would be overridden by those returned by
c     TS_RECALC).
c(b) the Earth's dipole moment is now calculated by TS_RECALC, based on the table of
cthe IGRF coefficients and their secular variation rates, for a given year and
cthe day of the year, and the obtained value of the moment is forwarded into DIP
cvia the second common block /GEOPACK2/. (In the previous versions, only a single
cfixed value was provided for the geodipole moment, corresponding to the most
c     recent epoch).
c
c(3) Subroutine TS_RECALC now consolidates in one module all calculations needed to
cinitialize and update the values of coefficients and quantities that vary in
ctime, either due to secular changes of the main geomagnetic field or as a result
cof Earth's diurnal rotation and orbital motion around Sun. That allowed us to
c     simplify the codes and make them more compiler-independent.
c
c(4) Subroutine TS_GEOMAG is now identical in its structure to other coordinate trans-
cformation subroutines. It no longer invokes TS_RECALC from within TS_GEOMAG, but uses
cprecalculated values of the rotation matrix elements, obtained by a separate
cexternal invocation of TS_RECALC. This eliminates possible interference of the
c     two subroutines in the old version of the package.
c
c     (5) Subroutine TRACE (and the subsidiary modules STEP and RHAND):
c
c(a) no longer needs to specify the highest order of spherical harmonics in the
cmain geomagnetic field expansion - it is now calculated automatically inside the
c     IGRF_GSM (or IGRF_GEO) subroutine.
c
c(b) the internal field model can now be explicitly chosen by specifying the para-
c      meter INNAME (either IGRF_GSM or DIP).
c
c(6) A new subroutine BCARSP was added, providing a conversion of Cartesian field
ccomponents into spherical ones (operation, inverse to that performed by the sub-
c     routine  TS_BSPCAR).
c
c(7) Two new subroutines were added, SHUETAL_MGNP and T96_MGNP, providing the position
cof the magnetopause, according to the model of Shue et al. [1998] and the one
c     used in the T96 magnetospheric magnetic field model.
c
c----------------------------------------------------------------------------------
c
      SUBROUTINE IGRF_GSM(XGSM,YGSM,ZGSM,HXGSM,HYGSM,HZGSM)
c
CCALCULATES COMPONENTS OF THE MAIN (INTERNAL) TS_GEOMAGNETIC FIELD IN THE GEOCENTRIC SOLAR
CMAGNETOSPHERIC COORDINATE SYSTEM, USING IAGA INTERNATIONAL TS_GEOMAGNETIC REFERENCE MODEL
C  COEFFICIENTS  (e.g., http://www.ngdc.noaa.gov/IAGA/wg8/igrf2000.html)
C
CBEFORE THE FIRST CALL OF THIS SUBROUTINE, OR IF THE DATE/TIME (IYEAR,IDAY,IHOUR,MIN,ISEC)
CWAS CHANGED, THE MODEL COEFFICIENTS AND GEO-GSM ROTATION MATRIX ELEMENTS SHOULD BE UPDATED
c     BY CALLING THE SUBROUTINE TS_RECALC
C
C     -----INPUT PARAMETERS:
C
C     XGSM,YGSM,ZGSM - CARTESIAN GSM COORDINATES (IN UNITS RE=6371.2 KM)
C
C     -----OUTPUT PARAMETERS:
C
CHXGSM,HYGSM,HZGSM - CARTESIAN GSM COMPONENTS OF THE MAIN TS_GEOMAGNETIC FIELD IN NANOTESLA
C
C     LAST MODIFICATION:  MARCH 30, 2003.
C     THIS VERSION OF THE  CODE ACCEPT DATES FROM 1965 THROUGH 2005.
c
C     AUTHOR: N. A. TSYGANENKO
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION HXGSM,HYGSM,HZGSM,XGSM,YGSM,ZGSM
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION AN,BBF,BBR,BBT,BF,BI,BR,BT,C,CF,D,D2,DP,E,HE,HH,
     *                 HXGEO,HYGEO,HZGEO,P,P2,PM,PP,Q,QQ,R,RHO,RHO2,S,
     *                 SF,W,X,XGEO,XK,Y,YGEO,Z,ZGEO
      INTEGER IRP3,K,M,MM,MN,N,NM
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION A(11),B(11)
C     ..
C     .. External Subroutines ..
      EXTERNAL GEOGSM
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC SQRT
C     ..
C     .. Common blocks ..
      COMMON /GEOPACK2/G,H,REC
      DOUBLE PRECISION G(66),H(66),REC(66)
C     ..
C     initialize to remove warnings
      X = 0.0
      Y = 0.0
      MM = 0
C
      CALL GEOGSM(XGEO,YGEO,ZGEO,XGSM,YGSM,ZGSM,-1)
      RHO2 = XGEO**2 + YGEO**2
      R = SQRT(RHO2+ZGEO**2)
      C = ZGEO/R
      RHO = SQRT(RHO2)
      S = RHO/R
      IF (S.LT.1.D-5) THEN
         CF = 1.D0
         SF = 0.D0
      ELSE
         CF = XGEO/RHO
         SF = YGEO/RHO
      END IF
C
      PP = 1.D0/R
      P = PP
C
CIN THIS NEW VERSION, THE OPTIMAL VALUE OF THE PARAMETER NM (MAXIMAL ORDER OF THE SPHERICAL
CHARMONIC EXPANSION) IS NOT USER-PRESCRIBED, BUT CALCULATED INSIDE THE SUBROUTINE, BASED
C      ON THE VALUE OF THE RADIAL DISTANCE R:
C
      IRP3 = R + 3
      NM = 4 + 30/IRP3
      IF (NM.GT.10) NM = 10
      K = NM + 1
      DO 10 N = 1,K
         P = P*PP
         A(N) = P
         B(N) = P*N
   10 CONTINUE
      P = 1.D0
      D = 0.D0
      BBR = 0.D0
      BBT = 0.D0
      BBF = 0.D0
      DO 60 M = 1,K
         IF (M.EQ.1) GO TO 20
         MM = M - 1
         W = X
         X = W*CF + Y*SF
         Y = Y*CF - W*SF
         GO TO 30
   20    X = 0.D0
         Y = 1.D0
   30    Q = P
         Z = D
         BI = 0.D0
         P2 = 0.D0
         D2 = 0.D0
         DO 50 N = M,K
            AN = A(N)
            MN = N*(N-1)/2 + M
            E = G(MN)
            HH = H(MN)
            W = E*Y + HH*X
            BBR = BBR + B(N)*W*Q
            BBT = BBT - AN*W*Z
            IF (M.EQ.1) GO TO 40
            QQ = Q
            IF (S.LT.1.D-5) QQ = Z
            BI = BI + AN*(E*X-HH*Y)*QQ
   40       XK = REC(MN)
            DP = C*Z - S*Q - XK*D2
            PM = C*Q - XK*P2
            D2 = Z
            P2 = Q
            Z = DP
            Q = PM
   50    CONTINUE
         D = S*D + C*P
         P = S*P
         IF (M.EQ.1) GO TO 60
         BI = BI*MM
         BBF = BBF + BI
   60 CONTINUE
C
      BR = BBR
      BT = BBT
      IF (S.LT.1.D-5) GO TO 70
      BF = BBF/S
      GO TO 80
   70 IF (C.LT.0.D0) BBF = -BBF
      BF = BBF
   80 HE = BR*S + BT*C
      HXGEO = HE*CF - BF*SF
      HYGEO = HE*SF + BF*CF
      HZGEO = BR*C - BT*S
      CALL GEOGSM(HXGEO,HYGEO,HZGEO,HXGSM,HYGSM,HZGSM,1)
      RETURN
      END
C
c==========================================================================================
C
c
      SUBROUTINE IGRF_GEO(R,THETA,PHI,BR,BTHETA,BPHI)
c
CCALCULATES COMPONENTS OF THE MAIN (INTERNAL) TS_GEOMAGNETIC FIELD IN THE SPHERICAL GEOGRAPHIC
C(GEOCENTRIC) COORDINATE SYSTEM, USING IAGA INTERNATIONAL TS_GEOMAGNETIC REFERENCE MODEL
C  COEFFICIENTS  (e.g., http://www.ngdc.noaa.gov/IAGA/wg8/igrf2000.html)
C
CBEFORE THE FIRST CALL OF THIS SUBROUTINE, OR IF THE DATE (IYEAR AND IDAY) WAS CHANGED,
CTHE MODEL COEFFICIENTS SHOULD BE UPDATED BY CALLING THE SUBROUTINE TS_RECALC
C
C     -----INPUT PARAMETERS:
C
C     R, THETA, PHI - SPHERICAL GEOGRAPHIC (GEOCENTRIC) COORDINATES:
CRADIAL DISTANCE R IN UNITS RE=6371.2 KM, COLATITUDE THETA AND LONGITUDE PHI IN RADIANS
C
C     -----OUTPUT PARAMETERS:
C
CBR, BTHETA, BPHI - SPHERICAL COMPONENTS OF THE MAIN TS_GEOMAGNETIC FIELD IN NANOTESLA
C      (POSITIVE BR OUTWARD, BTHETA SOUTHWARD, BPHI EASTWARD)
C
C     LAST MODIFICATION:  MARCH 30, 2003.
C     THIS VERSION OF THE  CODE ACCEPT DATES FROM 1965 THROUGH 2005.
c
C     AUTHOR: N. A. TSYGANENKO
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION BPHI,BR,BTHETA,PHI,R,THETA
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION AN,BBF,BBR,BBT,BI,C,CF,D,D2,DP,E,HH,P,P2,PM,PP,Q,
     *                 QQ,S,SF,W,X,XK,Y,Z
      INTEGER IRP3,K,M,MM,MN,N,NM
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION A(11),B(11)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC COS,SIN
C     ..
C     .. Common blocks ..
      COMMON /GEOPACK2/G,H,REC
      DOUBLE PRECISION G(66),H(66),REC(66)
C     ..
C     initialize to remove warnings
      X = 0.0
      Y = 0.0
      MM = 0
C
      C = COS(THETA)
      S = SIN(THETA)
      CF = COS(PHI)
      SF = SIN(PHI)
C
      PP = 1.D0/R
      P = PP
C
CIN THIS NEW VERSION, THE OPTIMAL VALUE OF THE PARAMETER NM (MAXIMAL ORDER OF THE SPHERICAL
CHARMONIC EXPANSION) IS NOT USER-PRESCRIBED, BUT CALCULATED INSIDE THE SUBROUTINE, BASED
C      ON THE VALUE OF THE RADIAL DISTANCE R:
C
      IRP3 = R + 3
      NM = 4 + 30/IRP3
      IF (NM.GT.10) NM = 10
      K = NM + 1
      DO 10 N = 1,K
         P = P*PP
         A(N) = P
         B(N) = P*N
   10 CONTINUE
      P = 1.D0
      D = 0.D0
      BBR = 0.D0
      BBT = 0.D0
      BBF = 0.D0
      DO 60 M = 1,K
         IF (M.EQ.1) GO TO 20
         MM = M - 1
         W = X
         X = W*CF + Y*SF
         Y = Y*CF - W*SF
         GO TO 30
   20    X = 0.D0
         Y = 1.D0
   30    Q = P
         Z = D
         BI = 0.D0
         P2 = 0.D0
         D2 = 0.D0
         DO 50 N = M,K
            AN = A(N)
            MN = N*(N-1)/2 + M
            E = G(MN)
            HH = H(MN)
            W = E*Y + HH*X
            BBR = BBR + B(N)*W*Q
            BBT = BBT - AN*W*Z
            IF (M.EQ.1) GO TO 40
            QQ = Q
            IF (S.LT.1.D-5) QQ = Z
            BI = BI + AN*(E*X-HH*Y)*QQ
   40       XK = REC(MN)
            DP = C*Z - S*Q - XK*D2
            PM = C*Q - XK*P2
            D2 = Z
            P2 = Q
            Z = DP
            Q = PM
   50    CONTINUE
         D = S*D + C*P
         P = S*P
         IF (M.EQ.1) GO TO 60
         BI = BI*MM
         BBF = BBF + BI
   60 CONTINUE
C
      BR = BBR
      BTHETA = BBT
      IF (S.LT.1.D-5) GO TO 70
      BPHI = BBF/S
      RETURN
   70 IF (C.LT.0.D0) BBF = -BBF
      BPHI = BBF
      RETURN
      END
C
c==========================================================================================
c
      SUBROUTINE DIP(XGSM,YGSM,ZGSM,BXGSM,BYGSM,BZGSM)
C
C  CALCULATES GSM COMPONENTS OF A GEODIPOLE FIELD WITH THE DIPOLE MOMENT
CCORRESPONDING TO THE EPOCH, SPECIFIED BY CALLING SUBROUTINE TS_RECALC (SHOULD BE
CINVOKED BEFORE THE FIRST USE OF THIS ONE AND IN CASE THE DATE/TIME WAS CHANGED).
C
C--INPUT PARAMETERS: XGSM,YGSM,ZGSM - GSM COORDINATES IN RE (1 RE = 6371.2 km)
C
C--OUTPUT PARAMETERS: BXGSM,BYGSM,BZGSM - FIELD COMPONENTS IN GSM SYSTEM, IN NANOTESLA.
C
C     LAST MODIFICATION: MARCH 31, 2003
C
C     AUTHOR: N. A. TSYGANENKO
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION BXGSM,BYGSM,BZGSM,XGSM,YGSM,ZGSM
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION DIPMOM,P,Q,T,U,V
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC SQRT
C     ..
C     .. Common blocks ..
      COMMON /GEOPACK1/AAA,SPS,CPS,BBB
      COMMON /GEOPACK2/G,H,REC
      DOUBLE PRECISION CPS,SPS
      DOUBLE PRECISION AAA(10),BBB(23),G(66),H(66),REC(66)
C     ..
      DIPMOM = SQRT(G(2)**2+G(3)**2+H(3)**2)
      P = XGSM**2
      U = ZGSM**2
      V = 3.D0*ZGSM*XGSM
      T = YGSM**2
      Q = DIPMOM/SQRT(P+T+U)**5
      BXGSM = Q*((T+U-2.D0*P)*SPS-V*CPS)
      BYGSM = -3.D0*YGSM*Q*(XGSM*SPS+ZGSM*CPS)
      BZGSM = Q*((P+T-2.D0*U)*CPS-V*SPS)
      RETURN
      END
C    *******************************************************************
c
      SUBROUTINE TS_SUN(IYEAR,IDAY,IHOUR,MIN,ISEC,GST,SLONG,SRASN,SDEC)
C
C    CALCULATES FOUR QUANTITIES NECESSARY FOR COORDINATE TRANSFORMATIONS
CWHICH DEPEND ON TS_SUN POSITION (AND, HENCE, ON UNIVERSAL TIME AND SEASON)
C
C     -------  INPUT PARAMETERS:
CIYR,IDAY,IHOUR,MIN,ISEC -  YEAR, DAY, AND UNIVERSAL TIME IN HOURS, MINUTES,
C     AND SECONDS  (IDAY=1 CORRESPONDS TO JANUARY 1).
C
C     -------  OUTPUT PARAMETERS:
C   GST - GREENWICH MEAN SIDEREAL TIME, SLONG - LONGITUDE ALONG ECLIPTIC
C  SRASN - RIGHT ASCENSION,  SDEC - DECLINATION  OF THE TS_SUN (RADIANS)
C     ORIGINAL VERSION OF THIS SUBROUTINE HAS BEEN COMPILED FROM:
C     RUSSELL, C.T., COSMIC ELECTRODYNAMICS, 1971, V.2, PP.184-196.
C
C     LAST MODIFICATION:  MARCH 31, 2003 (ONLY SOME NOTATION CHANGES)
C
C     ORIGINAL VERSION WRITTEN BY:    Gilbert D. Mead
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION GST,SDEC,SLONG,SRASN
      INTEGER IDAY,IHOUR,ISEC,IYEAR,MIN
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION COSD,DJ,FDAY,G,OBLIQ,RAD,SC,SIND,SLP,SOB,T,VL
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ATAN,ATAN2,COS,DMOD,SIN,SQRT
C     ..
C     .. Data statements ..
      DATA RAD/57.295779513D0/
C     ..
C
      IF (IYEAR.LT.1901 .OR. IYEAR.GT.2099) RETURN
      FDAY = DBLE(IHOUR*3600+MIN*60+ISEC)/86400.D0
      DJ = 365*(IYEAR-1900) + (IYEAR-1901)/4 + IDAY - 0.5D0 + FDAY
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
C
C   THE LAST CONSTANT IS A CORRECTION FOR THE ANGULAR ABERRATION  DUE TO
C     THE ORBITAL MOTION OF THE EARTH
C
      SIND = SOB*SIN(SLP)
      COSD = SQRT(1.D0-SIND**2)
      SC = SIND/COSD
      SDEC = ATAN(SC)
      SRASN = 3.141592654D0 - ATAN2(COS(OBLIQ)/SOB*SC,-COS(SLP)/COSD)
      RETURN
      END
C
C================================================================================
c
      SUBROUTINE TS_SPHCAR(R,THETA,PHI,X,Y,Z,J)
C
C     CONVERTS SPHERICAL COORDS INTO CARTESIAN ONES AND VICA VERSA
C     (THETA AND PHI IN RADIANS).
C
C                  J>0            J<0
C     -----INPUT:   J,R,THETA,PHI     J,X,Y,Z
C     ----OUTPUT:      X,Y,Z        R,THETA,PHI
C
C     NOTE: AT THE POLES (X=0 AND Y=0) WE ASSUME PHI=0 (WHEN CONVERTING
C        FROM CARTESIAN TO SPHERICAL COORDS, I.E., FOR J<0)
C
C LAST MOFIFICATION:  APRIL 1, 2003 (ONLY SOME NOTATION CHANGES AND MORE
C                         COMMENTS ADDED)
C
C     AUTHOR:  N. A. TSYGANENKO
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION PHI,R,THETA,X,Y,Z
      INTEGER J
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION SQ
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ATAN2,COS,SIN,SQRT
C     ..
      IF (J.GT.0) GO TO 30
      SQ = X**2 + Y**2
      R = SQRT(SQ+Z**2)
      IF (SQ.NE.0.D0) GO TO 20
      PHI = 0.D0
      IF (Z.LT.0.D0) GO TO 10
      THETA = 0.D0
      RETURN
   10 THETA = 3.141592654D0
      RETURN
   20 SQ = SQRT(SQ)
      PHI = ATAN2(Y,X)
      THETA = ATAN2(SQ,Z)
      IF (PHI.LT.0.D0) PHI = PHI + 6.28318531D0
      RETURN
   30 SQ = R*SIN(THETA)
      X = SQ*COS(PHI)
      Y = SQ*SIN(PHI)
      Z = R*COS(THETA)
      RETURN
      END
C
C===========================================================================
c
      SUBROUTINE TS_BSPCAR(THETA,PHI,BR,BTHETA,BPHI,BX,BY,BZ)
C
C     CALCULATES CARTESIAN FIELD COMPONENTS FROM SPHERICAL ONES
C     -----INPUT:   THETA,PHI - SPHERICAL ANGLES OF THE POINT IN RADIANS
C              BR,BTHETA,BPHI -  SPHERICAL COMPONENTS OF THE FIELD
C     -----OUTPUT:  BX,BY,BZ - CARTESIAN COMPONENTS OF THE FIELD
C
C     LAST MOFIFICATION:  APRIL 1, 2003 (ONLY SOME NOTATION CHANGES)
C
C     WRITTEN BY:  N. A. TSYGANENKO
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION BPHI,BR,BTHETA,BX,BY,BZ,PHI,THETA
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION BE,C,CF,S,SF
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC COS,SIN
C     ..
      S = SIN(THETA)
      C = COS(THETA)
      SF = SIN(PHI)
      CF = COS(PHI)
      BE = BR*S + BTHETA*C
      BX = BE*CF - BPHI*SF
      BY = BE*SF + BPHI*CF
      BZ = BR*C - BTHETA*S
      RETURN
      END
c
C==============================================================================
C
      SUBROUTINE BCARSP(X,Y,Z,BX,BY,BZ,BR,BTHETA,BPHI)
C
C    ALCULATES SPHERICAL FIELD COMPONENTS FROM THOSE IN CARTESIAN SYSTEM
C
C     -----INPUT:   X,Y,Z  - CARTESIAN COMPONENTS OF THE POSITION VECTOR
C              BX,BY,BZ - CARTESIAN COMPONENTS OF THE FIELD VECTOR
C-----OUTPUT:  BR,BTHETA,BPHI - SPHERICAL COMPONENTS OF THE FIELD VECTOR
C
C     NOTE: AT THE POLES (THETA=0 OR THETA=PI) WE ASSUME PHI=0,
C        AND HENCE BTHETA=BX, BPHI=BY
C
C     WRITTEN AND ADDED TO THIS PACKAGE:  APRIL 1, 2003,
C     AUTHOR:   N. A. TSYGANENKO
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION BPHI,BR,BTHETA,BX,BY,BZ,X,Y,Z
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION CPHI,CT,R,RHO,RHO2,SPHI,ST
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC SQRT
C     ..
      RHO2 = X**2 + Y**2
      R = SQRT(RHO2+Z**2)
      RHO = SQRT(RHO2)
      IF (RHO.NE.0.D0) THEN
         CPHI = X/RHO
         SPHI = Y/RHO
      ELSE
         CPHI = 1.D0
         SPHI = 0.D0
      END IF
      CT = Z/R
      ST = RHO/R
      BR = (X*BX+Y*BY+Z*BZ)/R
      BTHETA = (BX*CPHI+BY*SPHI)*CT - BZ*ST
      BPHI = BY*CPHI - BX*SPHI
      RETURN
      END
C
c=====================================================================================
C
      SUBROUTINE TS_RECALC(IYEAR,IDAY,IHOUR,MIN,ISEC)
C
C1. PREPARES ELEMENTS OF ROTATION MATRICES FOR TRANSFORMATIONS OF VECTORS BETWEEN
C     SEVERAL COORDINATE SYSTEMS, MOST FREQUENTLY USED IN SPACE PHYSICS.
C
C2. PREPARES COEFFICIENTS USED IN THE CALCULATION OF THE MAIN TS_GEOMAGNETIC FIELD
C      (IGRF MODEL)
C
CTHIS SUBROUTINE SHOULD BE INVOKED BEFORE USING THE FOLLOWING SUBROUTINES:
CIGRF_GEO, IGRF_GSM, DIP, TS_GEOMAG, GEOGSM, TS_MAGSM, TS_SMGSM, GSMGSE, GEIGEO.
C
CTHERE IS NO NEED TO REPEATEDLY INVOKE TS_RECALC, IF MULTIPLE CALCULATIONS ARE MADE
C     FOR THE SAME DATE AND TIME.
C
C     -----INPUT PARAMETERS:
C
C     IYEAR   -  YEAR NUMBER (FOUR DIGITS)
C     IDAY  -  DAY OF YEAR (DAY 1 = JAN 1)
C     IHOUR -  HOUR OF DAY (00 TO 23)
C     MIN   -  MINUTE OF HOUR (00 TO 59)
C     ISEC  -  SECONDS OF MINUTE (00 TO 59)
C
C     -----OUTPUT PARAMETERS:   NONE (ALL OUTPUT QUANTITIES ARE PLACED
C                      INTO THE COMMON BLOCKS /GEOPACK1/ AND /GEOPACK2/)
C
C     OTHER SUBROUTINES CALLED BY THIS ONE: TS_SUN
C
C     AUTHOR:  N.A. TSYGANENKO
C     DATE:    DEC.1, 1991
C
C     LAST REVISION: APRIL 3, 2003
c The code now includes preparation of the model coefficients for the subroutines
c IGRF and TS_GEOMAG. This eliminates the need for the SAVE statements, used in the
c   old versions, making the codes easier and more compiler-independent.
C
CTHE COMMON BLOCK /GEOPACK1/ CONTAINS ELEMENTS OF THE ROTATION MATRICES AND OTHER
CPARAMETERS RELATED TO THE COORDINATE TRANSFORMATIONS PERFORMED BY THIS PACKAGE
C
CTHE COMMON BLOCK /GEOPACK2/ CONTAINS COEFFICIENTS OF THE IGRF FIELD MODEL, CALCULATED
CFOR A GIVEN YEAR AND DAY FROM THEIR STANDARD EPOCH VALUES. THE ARRAY REC CONTAINS
CCOEFFICIENTS USED IN THE RECURSION RELATIONS FOR LEGENDRE ASSOCIATE POLYNOMIALS.
C
C     .. Scalar Arguments ..
      INTEGER IDAY,IHOUR,ISEC,IYEAR,MIN
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION AA,DIP1,DIP2,DIP3,DJ,DT,DY1,DY2,DY3,DZ1,DZ2,DZ3,
     *                 EXMAGX,EXMAGY,EXMAGZ,EYMAGX,EYMAGY,F1,F2,G10,G11,
     *                 GST,H11,OBLIQ,P,S,S1,S2,S3,SDEC,SLONG,SQ,SQQ,SQR,
     *                 SRASN,T,Y,Y1,Y2,Y3,Z1,Z2,Z3
      INTEGER IY,M,MN,MNN,N,N2
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION DG00(45),DH00(45),G00(66),G65(66),G70(66),
     *                 G75(66),G80(66),G85(66),G90(66),G95(66),H00(66),
     *                 H65(66),H70(66),H75(66),H80(66),H85(66),H90(66),
     *                 H95(66)
C     ..
C     .. External Subroutines ..
      EXTERNAL TS_SUN
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ASIN,ATAN2,COS,DBLE,SIN,SQRT
C     ..
C     .. Common blocks ..
      COMMON /GEOPACK1/ST0,CT0,SL0,CL0,CTCL,STCL,CTSL,STSL,SFI,CFI,SPS,
     *       CPS,SHI,CHI,HI,PSI,XMUT,A11,A21,A31,A12,A22,A32,A13,A23,
     *       A33,DS3,CGST,SGST,BA
      COMMON /GEOPACK2/G,H,REC
      DOUBLE PRECISION A11,A12,A13,A21,A22,A23,A31,A32,A33,CFI,CGST,CHI,
     *                 CL0,CPS,CT0,CTCL,CTSL,DS3,HI,PSI,SFI,SGST,SHI,
     *                 SL0,SPS,ST0,STCL,STSL,XMUT
      DOUBLE PRECISION BA(6),G(66),H(66),REC(66)
C     ..
C     .. Data statements ..
c
      DATA G65/0.D0,-30334.D0,-2119.D0,-1662.D0,2997.D0,1594.D0,1297.D0,
     *     -2038.D0,1292.D0,856.D0,957.D0,804.D0,479.D0,-390.D0,252.D0,
     *     -219.D0,358.D0,254.D0,-31.D0,-157.D0,-62.D0,45.D0,61.D0,8.D0,
     *     -228.D0,4.D0,1.D0,-111.D0,75.D0,-57.D0,4.D0,13.D0,-26.D0,
     *     -6.D0,13.D0,1.D0,13.D0,5.D0,-4.D0,-14.D0,0.D0,8.D0,-1.D0,
     *     11.D0,4.D0,8.D0,10.D0,2.D0,-13.D0,10.D0,-1.D0,-1.D0,5.D0,
     *     1.D0,-2.D0,-2.D0,-3.D0,2.D0,-5.D0,-2.D0,4.D0,4.D0,0.D0,2.D0,
     *     2.D0,0.D0/
      DATA H65/0.D0,0.D0,5776.D0,0.D0,-2016.D0,114.D0,0.D0,-404.D0,
     *     240.D0,-165.D0,0.D0,148.D0,-269.D0,13.D0,-269.D0,0.D0,19.D0,
     *     128.D0,-126.D0,-97.D0,81.D0,0.D0,-11.D0,100.D0,68.D0,-32.D0,
     *     -8.D0,-7.D0,0.D0,-61.D0,-27.D0,-2.D0,6.D0,26.D0,-23.D0,
     *     -12.D0,0.D0,7.D0,-12.D0,9.D0,-16.D0,4.D0,24.D0,-3.D0,-17.D0,
     *     0.D0,-22.D0,15.D0,7.D0,-4.D0,-5.D0,10.D0,10.D0,-4.D0,1.D0,
     *     0.D0,2.D0,1.D0,2.D0,6.D0,-4.D0,0.D0,-2.D0,3.D0,0.D0,-6.D0/
      DATA G70/0.D0,-30220.D0,-2068.D0,-1781.D0,3000.D0,1611.D0,1287.D0,
     *     -2091.D0,1278.D0,838.D0,952.D0,800.D0,461.D0,-395.D0,234.D0,
     *     -216.D0,359.D0,262.D0,-42.D0,-160.D0,-56.D0,43.D0,64.D0,
     *     15.D0,-212.D0,2.D0,3.D0,-112.D0,72.D0,-57.D0,1.D0,14.D0,
     *     -22.D0,-2.D0,13.D0,-2.D0,14.D0,6.D0,-2.D0,-13.D0,-3.D0,5.D0,
     *     0.D0,11.D0,3.D0,8.D0,10.D0,2.D0,-12.D0,10.D0,-1.D0,0.D0,3.D0,
     *     1.D0,-1.D0,-3.D0,-3.D0,2.D0,-5.D0,-1.D0,6.D0,4.D0,1.D0,0.D0,
     *     3.D0,-1.D0/
      DATA H70/0.D0,0.D0,5737.D0,0.D0,-2047.D0,25.D0,0.D0,-366.D0,
     *     251.D0,-196.D0,0.D0,167.D0,-266.D0,26.D0,-279.D0,0.D0,26.D0,
     *     139.D0,-139.D0,-91.D0,83.D0,0.D0,-12.D0,100.D0,72.D0,-37.D0,
     *     -6.D0,1.D0,0.D0,-70.D0,-27.D0,-4.D0,8.D0,23.D0,-23.D0,-11.D0,
     *     0.D0,7.D0,-15.D0,6.D0,-17.D0,6.D0,21.D0,-6.D0,-16.D0,0.D0,
     *     -21.D0,16.D0,6.D0,-4.D0,-5.D0,10.D0,11.D0,-2.D0,1.D0,0.D0,
     *     1.D0,1.D0,3.D0,4.D0,-4.D0,0.D0,-1.D0,3.D0,1.D0,-4.D0/
      DATA G75/0.D0,-30100.D0,-2013.D0,-1902.D0,3010.D0,1632.D0,1276.D0,
     *     -2144.D0,1260.D0,830.D0,946.D0,791.D0,438.D0,-405.D0,216.D0,
     *     -218.D0,356.D0,264.D0,-59.D0,-159.D0,-49.D0,45.D0,66.D0,
     *     28.D0,-198.D0,1.D0,6.D0,-111.D0,71.D0,-56.D0,1.D0,16.D0,
     *     -14.D0,0.D0,12.D0,-5.D0,14.D0,6.D0,-1.D0,-12.D0,-8.D0,4.D0,
     *     0.D0,10.D0,1.D0,7.D0,10.D0,2.D0,-12.D0,10.D0,-1.D0,-1.D0,
     *     4.D0,1.D0,-2.D0,-3.D0,-3.D0,2.D0,-5.D0,-2.D0,5.D0,4.D0,1.D0,
     *     0.D0,3.D0,-1.D0/
      DATA H75/0.D0,0.D0,5675.D0,0.D0,-2067.D0,-68.D0,0.D0,-333.D0,
     *     262.D0,-223.D0,0.D0,191.D0,-265.D0,39.D0,-288.D0,0.D0,31.D0,
     *     148.D0,-152.D0,-83.D0,88.D0,0.D0,-13.D0,99.D0,75.D0,-41.D0,
     *     -4.D0,11.D0,0.D0,-77.D0,-26.D0,-5.D0,10.D0,22.D0,-23.D0,
     *     -12.D0,0.D0,6.D0,-16.D0,4.D0,-19.D0,6.D0,18.D0,-10.D0,-17.D0,
     *     0.D0,-21.D0,16.D0,7.D0,-4.D0,-5.D0,10.D0,11.D0,-3.D0,1.D0,
     *     0.D0,1.D0,1.D0,3.D0,4.D0,-4.D0,-1.D0,-1.D0,3.D0,1.D0,-5.D0/
      DATA G80/0.D0,-29992.D0,-1956.D0,-1997.D0,3027.D0,1663.D0,1281.D0,
     *     -2180.D0,1251.D0,833.D0,938.D0,782.D0,398.D0,-419.D0,199.D0,
     *     -218.D0,357.D0,261.D0,-74.D0,-162.D0,-48.D0,48.D0,66.D0,
     *     42.D0,-192.D0,4.D0,14.D0,-108.D0,72.D0,-59.D0,2.D0,21.D0,
     *     -12.D0,1.D0,11.D0,-2.D0,18.D0,6.D0,0.D0,-11.D0,-7.D0,4.D0,
     *     3.D0,6.D0,-1.D0,5.D0,10.D0,1.D0,-12.D0,9.D0,-3.D0,-1.D0,7.D0,
     *     2.D0,-5.D0,-4.D0,-4.D0,2.D0,-5.D0,-2.D0,5.D0,3.D0,1.D0,2.D0,
     *     3.D0,0.D0/
      DATA H80/0.D0,0.D0,5604.D0,0.D0,-2129.D0,-200.D0,0.D0,-336.D0,
     *     271.D0,-252.D0,0.D0,212.D0,-257.D0,53.D0,-297.D0,0.D0,46.D0,
     *     150.D0,-151.D0,-78.D0,92.D0,0.D0,-15.D0,93.D0,71.D0,-43.D0,
     *     -2.D0,17.D0,0.D0,-82.D0,-27.D0,-5.D0,16.D0,18.D0,-23.D0,
     *     -10.D0,0.D0,7.D0,-18.D0,4.D0,-22.D0,9.D0,16.D0,-13.D0,-15.D0,
     *     0.D0,-21.D0,16.D0,9.D0,-5.D0,-6.D0,9.D0,10.D0,-6.D0,2.D0,
     *     0.D0,1.D0,0.D0,3.D0,6.D0,-4.D0,0.D0,-1.D0,4.D0,0.D0,-6.D0/
      DATA G85/0.D0,-29873.D0,-1905.D0,-2072.D0,3044.D0,1687.D0,1296.D0,
     *     -2208.D0,1247.D0,829.D0,936.D0,780.D0,361.D0,-424.D0,170.D0,
     *     -214.D0,355.D0,253.D0,-93.D0,-164.D0,-46.D0,53.D0,65.D0,
     *     51.D0,-185.D0,4.D0,16.D0,-102.D0,74.D0,-62.D0,3.D0,24.D0,
     *     -6.D0,4.D0,10.D0,0.D0,21.D0,6.D0,0.D0,-11.D0,-9.D0,4.D0,4.D0,
     *     4.D0,-4.D0,5.D0,10.D0,1.D0,-12.D0,9.D0,-3.D0,-1.D0,7.D0,1.D0,
     *     -5.D0,-4.D0,-4.D0,3.D0,-5.D0,-2.D0,5.D0,3.D0,1.D0,2.D0,3.D0,
     *     0.D0/
      DATA H85/0.D0,0.D0,5500.D0,0.D0,-2197.D0,-306.D0,0.D0,-310.D0,
     *     284.D0,-297.D0,0.D0,232.D0,-249.D0,69.D0,-297.D0,0.D0,47.D0,
     *     150.D0,-154.D0,-75.D0,95.D0,0.D0,-16.D0,88.D0,69.D0,-48.D0,
     *     -1.D0,21.D0,0.D0,-83.D0,-27.D0,-2.D0,20.D0,17.D0,-23.D0,
     *     -7.D0,0.D0,8.D0,-19.D0,5.D0,-23.D0,11.D0,14.D0,-15.D0,-11.D0,
     *     0.D0,-21.D0,15.D0,9.D0,-6.D0,-6.D0,9.D0,9.D0,-7.D0,2.D0,0.D0,
     *     1.D0,0.D0,3.D0,6.D0,-4.D0,0.D0,-1.D0,4.D0,0.D0,-6.D0/
      DATA G90/0.D0,-29775.D0,-1848.D0,-2131.D0,3059.D0,1686.D0,1314.D0,
     *     -2239.D0,1248.D0,802.D0,939.D0,780.D0,325.D0,-423.D0,141.D0,
     *     -214.D0,353.D0,245.D0,-109.D0,-165.D0,-36.D0,61.D0,65.D0,
     *     59.D0,-178.D0,3.D0,18.D0,-96.D0,77.D0,-64.D0,2.D0,26.D0,
     *     -1.D0,5.D0,9.D0,0.D0,23.D0,5.D0,-1.D0,-10.D0,-12.D0,3.D0,
     *     4.D0,2.D0,-6.D0,4.D0,9.D0,1.D0,-12.D0,9.D0,-4.D0,-2.D0,7.D0,
     *     1.D0,-6.D0,-3.D0,-4.D0,2.D0,-5.D0,-2.D0,4.D0,3.D0,1.D0,3.D0,
     *     3.D0,0.D0/
      DATA H90/0.D0,0.D0,5406.D0,0.D0,-2279.D0,-373.D0,0.D0,-284.D0,
     *     293.D0,-352.D0,0.D0,247.D0,-240.D0,84.D0,-299.D0,0.D0,46.D0,
     *     154.D0,-153.D0,-69.D0,97.D0,0.D0,-16.D0,82.D0,69.D0,-52.D0,
     *     1.D0,24.D0,0.D0,-80.D0,-26.D0,0.D0,21.D0,17.D0,-23.D0,-4.D0,
     *     0.D0,10.D0,-19.D0,6.D0,-22.D0,12.D0,12.D0,-16.D0,-10.D0,0.D0,
     *     -20.D0,15.D0,11.D0,-7.D0,-7.D0,9.D0,8.D0,-7.D0,2.D0,0.D0,
     *     2.D0,1.D0,3.D0,6.D0,-4.D0,0.D0,-2.D0,3.D0,-1.D0,-6.D0/
      DATA G95/0.D0,-29682.D0,-1789.D0,-2197.D0,3074.D0,1685.D0,1329.D0,
     *     -2268.D0,1249.D0,769.D0,941.D0,782.D0,291.D0,-421.D0,116.D0,
     *     -210.D0,352.D0,237.D0,-122.D0,-167.D0,-26.D0,66.D0,64.D0,
     *     65.D0,-172.D0,2.D0,17.D0,-94.D0,78.D0,-67.D0,1.D0,29.D0,4.D0,
     *     8.D0,10.D0,-2.D0,24.D0,4.D0,-1.D0,-9.D0,-14.D0,4.D0,5.D0,
     *     0.D0,-7.D0,4.D0,9.D0,1.D0,-12.D0,9.D0,-4.D0,-2.D0,7.D0,0.D0,
     *     -6.D0,-3.D0,-4.D0,2.D0,-5.D0,-2.D0,4.D0,3.D0,1.D0,3.D0,3.D0,
     *     0.D0/
      DATA H95/0.D0,0.D0,5318.D0,0.D0,-2356.D0,-425.D0,0.D0,-263.D0,
     *     302.D0,-406.D0,0.D0,262.D0,-232.D0,98.D0,-301.D0,0.D0,44.D0,
     *     157.D0,-152.D0,-64.D0,99.D0,0.D0,-16.D0,77.D0,67.D0,-57.D0,
     *     4.D0,28.D0,0.D0,-77.D0,-25.D0,3.D0,22.D0,16.D0,-23.D0,-3.D0,
     *     0.D0,12.D0,-20.D0,7.D0,-21.D0,12.D0,10.D0,-17.D0,-10.D0,0.D0,
     *     -19.D0,15.D0,11.D0,-7.D0,-7.D0,9.D0,7.D0,-8.D0,1.D0,0.D0,
     *     2.D0,1.D0,3.D0,6.D0,-4.D0,0.D0,-2.D0,3.D0,-1.D0,-6.D0/
      DATA G00/0.D0,-29615.D0,-1728.D0,-2267.D0,3072.D0,1672.D0,1341.D0,
     *     -2290.D0,1253.D0,715.D0,935.D0,787.D0,251.D0,-405.D0,110.D0,
     *     -217.D0,351.D0,222.D0,-131.D0,-169.D0,-12.D0,72.D0,68.D0,
     *     74.D0,-161.D0,-5.D0,17.D0,-91.D0,79.D0,-74.D0,0.D0,33.D0,
     *     9.D0,7.D0,8.D0,-2.D0,25.D0,6.D0,-9.D0,-8.D0,-17.D0,9.D0,7.D0,
     *     -8.D0,-7.D0,5.D0,9.D0,3.D0,-8.D0,6.D0,-9.D0,-2.D0,9.D0,-4.D0,
     *     -8.D0,-2.D0,-6.D0,2.D0,-3.D0,0.D0,4.D0,1.D0,2.D0,4.D0,0.D0,
     *     -1.D0/
      DATA H00/0.D0,0.D0,5186.D0,0.D0,-2478.D0,-458.D0,0.D0,-227.D0,
     *     296.D0,-492.D0,0.D0,272.D0,-232.D0,119.D0,-304.D0,0.D0,44.D0,
     *     172.D0,-134.D0,-40.D0,107.D0,0.D0,-17.D0,64.D0,65.D0,-61.D0,
     *     1.D0,44.D0,0.D0,-65.D0,-24.D0,6.D0,24.D0,15.D0,-25.D0,-6.D0,
     *     0.D0,12.D0,-22.D0,8.D0,-21.D0,15.D0,9.D0,-16.D0,-3.D0,0.D0,
     *     -20.D0,13.D0,12.D0,-6.D0,-8.D0,9.D0,4.D0,-8.D0,5.D0,0.D0,
     *     1.D0,0.D0,4.D0,5.D0,-6.D0,-1.D0,-3.D0,0.D0,-2.D0,-8.D0/
      DATA DG00/0.0D0,14.6D0,10.7D0,-12.4D0,1.1D0,-1.1D0,0.7D0,-5.4D0,
     *     0.9D0,-7.7D0,-1.3D0,1.6D0,-7.3D0,2.9D0,-3.2D0,0.0D0,-0.7D0,
     *     -2.1D0,-2.8D0,-0.8D0,2.5D0,1.0D0,-0.4D0,0.9D0,2.0D0,-0.6D0,
     *     -0.3D0,1.2D0,-0.4D0,-0.4D0,-0.3D0,1.1D0,1.1D0,-0.2D0,0.6D0,
     *     -0.9D0,-0.3D0,0.2D0,-0.3D0,0.4D0,-1.0D0,0.3D0,-0.5D0,-0.7D0,
     *     -0.4D0/
      DATA DH00/0.0D0,0.0D0,-22.5D0,0.0D0,-20.6D0,-9.6D0,0.0D0,6.0D0,
     *     -0.1D0,-14.2D0,0.0D0,2.1D0,1.3D0,5.0D0,0.3D0,0.0D0,-0.1D0,
     *     0.6D0,1.7D0,1.9D0,0.1D0,0.0D0,-0.2D0,-1.4D0,0.0D0,-0.8D0,
     *     0.0D0,0.9D0,0.0D0,1.1D0,0.0D0,0.3D0,-0.1D0,-0.6D0,-0.7D0,
     *     0.2D0,0.0D0,0.1D0,0.0D0,0.0D0,0.3D0,0.6D0,-0.4D0,0.3D0,0.7D0/
C     ..
C
      IY = IYEAR
C
C     WE ARE RESTRICTED BY THE INTERVAL 1965-2005,
CFOR WHICH THE IGRF COEFFICIENTS ARE KNOWN; IF IYEAR IS OUTSIDE THIS INTERVAL,
C   THE SUBROUTINE USES THE NEAREST LIMITING VALUE AND PRINTS A WARNING:
C
C   This warning surpressed by B. Rideout
      IF (IY.LT.1965) THEN
         IY = 1965
C         WRITE (*,FMT=9000) IYEAR,IY
      END IF
      IF (IY.GT.2005) THEN
         IY = 2005
C         WRITE (*,FMT=9000) IYEAR,IY
      END IF
C
CCALCULATE THE ARRAY REC, CONTAINING COEFFICIENTS FOR THE RECURSION RELATIONS,
CUSED IN THE IGRF SUBROUTINE FOR CALCULATING THE ASSOCIATE LEGENDRE POLYNOMIALS
C     AND THEIR DERIVATIVES:
c
      DO 20 N = 1,11
         N2 = 2*N - 1
         N2 = N2*(N2-2)
         DO 10 M = 1,N
            MN = N*(N-1)/2 + M
            REC(MN) = DBLE((N-M)*(N+M-2))/DBLE(N2)
   10    CONTINUE
   20 CONTINUE
C
      IF (IY.LT.1970) GO TO 40
      IF (IY.LT.1975) GO TO 60
      IF (IY.LT.1980) GO TO 80
      IF (IY.LT.1985) GO TO 100
      IF (IY.LT.1990) GO TO 120
      IF (IY.LT.1995) GO TO 140
      IF (IY.LT.2000) GO TO 160
C
C       EXTRAPOLATE BEYOND 2000:
C
      DT = DBLE(IY) + DBLE(IDAY-1)/365.25D0 - 2000.D0
      DO 30 N = 1,66
         G(N) = G00(N)
         H(N) = H00(N)
         IF (N.GT.45) GO TO 30
         G(N) = G(N) + DG00(N)*DT
         H(N) = H(N) + DH00(N)*DT
   30 CONTINUE
      GO TO 180
C
C       INTERPOLATE BETWEEEN 1965 - 1970:
C
   40 F2 = (DBLE(IY)+DBLE(IDAY-1)/365.25D0-1965)/5.D0
      F1 = 1.D0 - F2
      DO 50 N = 1,66
         G(N) = G65(N)*F1 + G70(N)*F2
         H(N) = H65(N)*F1 + H70(N)*F2
   50 CONTINUE
      GO TO 180
C
C       INTERPOLATE BETWEEN 1970 - 1975:
C
   60 F2 = (DBLE(IY)+DBLE(IDAY-1)/365.25D0-1970)/5.D0
      F1 = 1.D0 - F2
      DO 70 N = 1,66
         G(N) = G70(N)*F1 + G75(N)*F2
         H(N) = H70(N)*F1 + H75(N)*F2
   70 CONTINUE
      GO TO 180
C
C       INTERPOLATE BETWEEN 1975 - 1980:
C
   80 F2 = (DBLE(IY)+DBLE(IDAY-1)/365.25D0-1975)/5.D0
      F1 = 1.D0 - F2
      DO 90 N = 1,66
         G(N) = G75(N)*F1 + G80(N)*F2
         H(N) = H75(N)*F1 + H80(N)*F2
   90 CONTINUE
      GO TO 180
C
C       INTERPOLATE BETWEEN 1980 - 1985:
C
  100 F2 = (DBLE(IY)+DBLE(IDAY-1)/365.25D0-1980)/5.D0
      F1 = 1.D0 - F2
      DO 110 N = 1,66
         G(N) = G80(N)*F1 + G85(N)*F2
         H(N) = H80(N)*F1 + H85(N)*F2
  110 CONTINUE
      GO TO 180
C
C       INTERPOLATE BETWEEN 1985 - 1990:
C
  120 F2 = (DBLE(IY)+DBLE(IDAY-1)/365.25D0-1985)/5.D0
      F1 = 1.D0 - F2
      DO 130 N = 1,66
         G(N) = G85(N)*F1 + G90(N)*F2
         H(N) = H85(N)*F1 + H90(N)*F2
  130 CONTINUE
      GO TO 180
C
C       INTERPOLATE BETWEEN 1990 - 1995:
C
  140 F2 = (DBLE(IY)+DBLE(IDAY-1)/365.25D0-1990)/5.D0
      F1 = 1.D0 - F2
      DO 150 N = 1,66
         G(N) = G90(N)*F1 + G95(N)*F2
         H(N) = H90(N)*F1 + H95(N)*F2
  150 CONTINUE
      GO TO 180
C
C       INTERPOLATE BETWEEN 1995 - 2000:
C
  160 F2 = (DBLE(IY)+DBLE(IDAY-1)/365.25D0-1995)/5.D0
      F1 = 1.D0 - F2
      DO 170 N = 1,66
         G(N) = G95(N)*F1 + G00(N)*F2
         H(N) = H95(N)*F1 + H00(N)*F2
  170 CONTINUE
      GO TO 180
C
C     COEFFICIENTS FOR A GIVEN YEAR HAVE BEEN CALCULATED; NOW MULTIPLY
C     THEM BY SCHMIDT NORMALIZATION FACTORS:
C
  180 S = 1.D0
      DO 200 N = 2,11
         MN = N*(N-1)/2 + 1
         S = S*DBLE(2*N-3)/DBLE(N-1)
         G(MN) = G(MN)*S
         H(MN) = H(MN)*S
         P = S
         DO 190 M = 2,N
            AA = 1.D0
            IF (M.EQ.2) AA = 2.D0
            P = P*SQRT(AA*DBLE(N-M+1)/DBLE(N+M-2))
            MNN = MN + M - 1
            G(MNN) = G(MNN)*P
            H(MNN) = H(MNN)*P
  190    CONTINUE
  200 CONTINUE
      G10 = -G(2)
      G11 = G(3)
      H11 = H(3)
C
CNOW CALCULATE THE COMPONENTS OF THE UNIT VECTOR EzMAG IN GEO COORD.SYSTEM:
C     SIN(TETA0)*COS(LAMBDA0), SIN(TETA0)*SIN(LAMBDA0), AND COS(TETA0)
C         ST0 * CL0                ST0 * SL0                CT0
C
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
C
      CALL TS_SUN(IY,IDAY,IHOUR,MIN,ISEC,GST,SLONG,SRASN,SDEC)
C
C S1,S2, AND S3 ARE THE COMPONENTS OF THE UNIT VECTOR EXGSM=EXGSE IN THE
C     SYSTEM GEI POINTING FROM THE EARTH'S CENTER TO THE TS_SUN:
C
      S1 = COS(SRASN)*COS(SDEC)
      S2 = SIN(SRASN)*COS(SDEC)
      S3 = SIN(SDEC)
      CGST = COS(GST)
      SGST = SIN(GST)
C
C  DIP1, DIP2, AND DIP3 ARE THE COMPONENTS OF THE UNIT VECTOR EZSM=EZMAG
C     IN THE SYSTEM GEI:
C
      DIP1 = STCL*CGST - STSL*SGST
      DIP2 = STCL*SGST + STSL*CGST
      DIP3 = CT0
C
CNOW CALCULATE THE COMPONENTS OF THE UNIT VECTOR EYGSM IN THE SYSTEM GEI
C  BY TAKING THE VECTOR PRODUCT D x S AND NORMALIZING IT TO UNIT LENGTH:
C
      Y1 = DIP2*S3 - DIP3*S2
      Y2 = DIP3*S1 - DIP1*S3
      Y3 = DIP1*S2 - DIP2*S1
      Y = SQRT(Y1*Y1+Y2*Y2+Y3*Y3)
      Y1 = Y1/Y
      Y2 = Y2/Y
      Y3 = Y3/Y
C
CTHEN IN THE GEI SYSTEM THE UNIT VECTOR Z = EZGSM = EXGSM x EYGSM = S x Y
C     HAS THE COMPONENTS:
C
      Z1 = S2*Y3 - S3*Y2
      Z2 = S3*Y1 - S1*Y3
      Z3 = S1*Y2 - S2*Y1
C
C   THE VECTOR EZGSE (HERE DZ) IN GEI HAS THE COMPONENTS (0,-SIN(DELTA),
C   COS(DELTA)) = (0.,-0.397823,0.917462); HERE DELTA = 23.44214 DEG FOR
CTHE EPOCH 1978 (SEE THE BOOK BY GUREVICH OR OTHER ASTRONOMICAL HANDBOOKS).
C     HERE THE MOST ACCURATE TIME-DEPENDENT FORMULA IS USED:
C
      DJ = DBLE(365*(IY-1900)+(IY-1901)/4+IDAY) - 0.5D0 +
     *     DBLE(IHOUR*3600+MIN*60+ISEC)/86400.D0
      T = DJ/36525.D0
      OBLIQ = (23.45229D0-0.0130125D0*T)/57.2957795D0
      DZ1 = 0.D0
      DZ2 = -SIN(OBLIQ)
      DZ3 = COS(OBLIQ)
C
CTHEN THE UNIT VECTOR EYGSE IN GEI SYSTEM IS THE VECTOR PRODUCT DZ x S :
C
      DY1 = DZ2*S3 - DZ3*S2
      DY2 = DZ3*S1 - DZ1*S3
      DY3 = DZ1*S2 - DZ2*S1
C
C     THE ELEMENTS OF THE MATRIX GSE TO GSM ARE THE SCALAR PRODUCTS:
CCHI=EM22=(EYGSM,EYGSE), SHI=EM23=(EYGSM,EZGSE), EM32=(EZGSM,EYGSE)=-EM23,
C     AND EM33=(EZGSM,EZGSE)=EM22
C
      CHI = Y1*DY1 + Y2*DY2 + Y3*DY3
      SHI = Y1*DZ1 + Y2*DZ2 + Y3*DZ3
      HI = ASIN(SHI)
C
C     TILT ANGLE: PSI=ARCSIN(DIP,EXGSM)
C
      SPS = DIP1*S1 + DIP2*S2 + DIP3*S3
      CPS = SQRT(1.D0-SPS**2)
      PSI = ASIN(SPS)
C
C     THE ELEMENTS OF THE MATRIX MAG TO SM ARE THE SCALAR PRODUCTS:
CCFI=GM22=(EYSM,EYMAG), SFI=GM23=(EYSM,EXMAG); THEY CAN BE DERIVED AS FOLLOWS:
C
CIN GEO THE VECTORS EXMAG AND EYMAG HAVE THE COMPONENTS (CT0*CL0,CT0*SL0,-ST0)
C   AND (-SL0,CL0,0), RESPECTIVELY.    HENCE, IN GEI THE COMPONENTS ARE:
C     EXMAG:    CT0*CL0*COS(GST)-CT0*SL0*SIN(GST)
C            CT0*CL0*SIN(GST)+CT0*SL0*COS(GST)
C            -ST0
C     EYMAG:    -SL0*COS(GST)-CL0*SIN(GST)
C            -SL0*SIN(GST)+CL0*COS(GST)
C             0
C     THE COMPONENTS OF EYSM IN GEI WERE FOUND ABOVE AS Y1, Y2, AND Y3;
C     NOW WE ONLY HAVE TO COMBINE THE QUANTITIES INTO SCALAR PRODUCTS:
C
      EXMAGX = CT0*(CL0*CGST-SL0*SGST)
      EXMAGY = CT0*(CL0*SGST+SL0*CGST)
      EXMAGZ = -ST0
      EYMAGX = -(SL0*CGST+CL0*SGST)
      EYMAGY = -(SL0*SGST-CL0*CGST)
      CFI = Y1*EYMAGX + Y2*EYMAGY
      SFI = Y1*EXMAGX + Y2*EXMAGY + Y3*EXMAGZ
C
      XMUT = (ATAN2(SFI,CFI)+3.1415926536D0)*3.8197186342D0
C
C     THE ELEMENTS OF THE MATRIX GEO TO GSM ARE THE SCALAR PRODUCTS:
C
C     A11=(EXGEO,EXGSM), A12=(EYGEO,EXGSM), A13=(EZGEO,EXGSM),
C     A21=(EXGEO,EYGSM), A22=(EYGEO,EYGSM), A23=(EZGEO,EYGSM),
C     A31=(EXGEO,EZGSM), A32=(EYGEO,EZGSM), A33=(EZGEO,EZGSM),
C
C     ALL THE UNIT VECTORS IN BRACKETS ARE ALREADY DEFINED IN GEI:
C
C     EXGEO=(CGST,SGST,0), EYGEO=(-SGST,CGST,0), EZGEO=(0,0,1)
C     EXGSM=(S1,S2,S3),  EYGSM=(Y1,Y2,Y3),   EZGSM=(Z1,Z2,Z3)
C                                                        AND  THEREFORE:
C
      A11 = S1*CGST + S2*SGST
      A12 = -S1*SGST + S2*CGST
      A13 = S3
      A21 = Y1*CGST + Y2*SGST
      A22 = -Y1*SGST + Y2*CGST
      A23 = Y3
      A31 = Z1*CGST + Z2*SGST
      A32 = -Z1*SGST + Z2*CGST
      A33 = Z3
C
 9000 FORMAT (/,/,1X,
     * '**** TS_RECALC WARNS: YEAR IS OUT OF INTERVAL 1965-2005: IYEAR='
     *       ,/,6X,'CALCULATIONS WILL BE DONE FOR IYEAR=',I4,/)
      RETURN
      END
c
c   ====================================================================
C
      SUBROUTINE TS_GEOMAG(XGEO,YGEO,ZGEO,XMAG,YMAG,ZMAG,J)
C
C   CONVERTS GEOGRAPHIC (GEO) TO DIPOLE (MAG) COORDINATES OR VICA VERSA.
C
C                    J>0                       J<0
C     -----INPUT:  J,XGEO,YGEO,ZGEO           J,XMAG,YMAG,ZMAG
C     -----OUTPUT:    XMAG,YMAG,ZMAG           XGEO,YGEO,ZGEO
C
CATTENTION:  SUBROUTINE  TS_RECALC  MUST BE INVOKED BEFORE TS_GEOMAG IN TWO CASES:
C     /A/  BEFORE THE FIRST TRANSFORMATION OF COORDINATES
C     /B/  IF THE VALUES OF IYEAR AND/OR IDAY HAVE BEEN CHANGED
C
CLAST MOFIFICATION:  MARCH 30, 2003 (INVOCATION OF TS_RECALC INSIDE THIS S/R WAS REMOVED)
C
C     AUTHOR:  N. A. TSYGANENKO
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION XGEO,XMAG,YGEO,YMAG,ZGEO,ZMAG
      INTEGER J
C     ..
C     .. Common blocks ..
      COMMON /GEOPACK1/ST0,CT0,SL0,CL0,CTCL,STCL,CTSL,STSL,AB,BB
      DOUBLE PRECISION CL0,CT0,CTCL,CTSL,SL0,ST0,STCL,STSL
      DOUBLE PRECISION AB(19),BB(8)
C     ..
      IF (J.GT.0) THEN
         XMAG = XGEO*CTCL + YGEO*CTSL - ZGEO*ST0
         YMAG = YGEO*CL0 - XGEO*SL0
         ZMAG = XGEO*STCL + YGEO*STSL + ZGEO*CT0
      ELSE
         XGEO = XMAG*CTCL - YMAG*SL0 + ZMAG*STCL
         YGEO = XMAG*CTSL + YMAG*CL0 + ZMAG*STSL
         ZGEO = ZMAG*CT0 - XMAG*ST0
      END IF
      RETURN
      END
c
c=========================================================================================
c
      SUBROUTINE GEIGEO(XGEI,YGEI,ZGEI,XGEO,YGEO,ZGEO,J)
C
C     CONVERTS EQUATORIAL INERTIAL (GEI) TO GEOGRAPHICAL (GEO) COORDS
C     OR VICA VERSA.
C                    J>0                J<0
C     ----INPUT:  J,XGEI,YGEI,ZGEI    J,XGEO,YGEO,ZGEO
C     ----OUTPUT:   XGEO,YGEO,ZGEO      XGEI,YGEI,ZGEI
C
CATTENTION:  SUBROUTINE  TS_RECALC  MUST BE INVOKED BEFORE GEIGEO IN TWO CASES:
C     /A/  BEFORE THE FIRST TRANSFORMATION OF COORDINATES
C/B/  IF THE CURRENT VALUES OF IYEAR,IDAY,IHOUR,MIN,ISEC HAVE BEEN CHANGED
C
C     LAST MODIFICATION:  MARCH 31, 2003
C     AUTHOR:  N. A. TSYGANENKO
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION XGEI,XGEO,YGEI,YGEO,ZGEI,ZGEO
      INTEGER J
C     ..
C     .. Common blocks ..
      COMMON /GEOPACK1/A,CGST,SGST,B
      DOUBLE PRECISION CGST,SGST
      DOUBLE PRECISION A(27),B(6)
C     ..
      IF (J.GT.0) THEN
         XGEO = XGEI*CGST + YGEI*SGST
         YGEO = YGEI*CGST - XGEI*SGST
         ZGEO = ZGEI
      ELSE
         XGEI = XGEO*CGST - YGEO*SGST
         YGEI = YGEO*CGST + XGEO*SGST
         ZGEI = ZGEO
      END IF
      RETURN
      END
C
C=======================================================================================
C
      SUBROUTINE TS_MAGSM(XMAG,YMAG,ZMAG,XSM,YSM,ZSM,J)
C
C CONVERTS DIPOLE (MAG) TO SOLAR MAGNETIC (SM) COORDINATES OR VICA VERSA
C
C                    J>0              J<0
C     ----INPUT: J,XMAG,YMAG,ZMAG     J,XSM,YSM,ZSM
C     ----OUTPUT:    XSM,YSM,ZSM       XMAG,YMAG,ZMAG
C
CATTENTION:  SUBROUTINE  TS_RECALC  MUST BE INVOKED BEFORE TS_MAGSM IN TWO CASES:
C     /A/  BEFORE THE FIRST TRANSFORMATION OF COORDINATES
C     /B/  IF THE VALUES OF IYEAR,IDAY,IHOUR,MIN,ISEC HAVE BEEN CHANGED
C
C     LAST MODIFICATION:  MARCH 31, 2003
C
C     AUTHOR:  N. A. TSYGANENKO
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION XMAG,XSM,YMAG,YSM,ZMAG,ZSM
      INTEGER J
C     ..
C     .. Common blocks ..
      COMMON /GEOPACK1/A,SFI,CFI,B,AB,BA
      DOUBLE PRECISION CFI,SFI
      DOUBLE PRECISION A(8),AB(10),B(7),BA(8)
C     ..
      IF (J.GT.0) THEN
         XSM = XMAG*CFI - YMAG*SFI
         YSM = XMAG*SFI + YMAG*CFI
         ZSM = ZMAG
      ELSE
         XMAG = XSM*CFI + YSM*SFI
         YMAG = YSM*CFI - XSM*SFI
         ZMAG = ZSM
      END IF
      RETURN
      END
C
C=======================================================================================
C
      SUBROUTINE GSMGSE(XGSM,YGSM,ZGSM,XGSE,YGSE,ZGSE,J)
C
CCONVERTS GEOCENTRIC SOLAR MAGNETOSPHERIC (GSM) COORDS TO SOLAR ECLIPTIC (GSE) ONES
C     OR VICA VERSA.
C                    J>0                J<0
C     -----INPUT: J,XGSM,YGSM,ZGSM    J,XGSE,YGSE,ZGSE
C     ----OUTPUT:   XGSE,YGSE,ZGSE      XGSM,YGSM,ZGSM
C
CATTENTION:  SUBROUTINE  TS_RECALC  MUST BE INVOKED BEFORE GSMGSE IN TWO CASES:
C     /A/  BEFORE THE FIRST TRANSFORMATION OF COORDINATES
C     /B/  IF THE VALUES OF IYEAR,IDAY,IHOUR,MIN,ISEC HAVE BEEN CHANGED
C
C     LAST MODIFICATION:  MARCH 31, 2003
C
C     AUTHOR:  N. A. TSYGANENKO
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION XGSE,XGSM,YGSE,YGSM,ZGSE,ZGSM
      INTEGER J
C     ..
C     .. Common blocks ..
      COMMON /GEOPACK1/A,SHI,CHI,AB,BA
      DOUBLE PRECISION CHI,SHI
      DOUBLE PRECISION A(12),AB(13),BA(8)
C     ..
      IF (J.GT.0) THEN
         XGSE = XGSM
         YGSE = YGSM*CHI - ZGSM*SHI
         ZGSE = YGSM*SHI + ZGSM*CHI
      ELSE
         XGSM = XGSE
         YGSM = YGSE*CHI + ZGSE*SHI
         ZGSM = ZGSE*CHI - YGSE*SHI
      END IF
      RETURN
      END
C
C=====================================================================================
C
      SUBROUTINE TS_SMGSM(XSM,YSM,ZSM,XGSM,YGSM,ZGSM,J)
C
C     CONVERTS SOLAR MAGNETIC (SM) TO GEOCENTRIC SOLAR MAGNETOSPHERIC
C     (GSM) COORDINATES OR VICA VERSA.
C                  J>0                 J<0
C     -----INPUT: J,XSM,YSM,ZSM        J,XGSM,YGSM,ZGSM
C     ----OUTPUT:  XGSM,YGSM,ZGSM       XSM,YSM,ZSM
C
CATTENTION:  SUBROUTINE TS_RECALC  MUST BE INVOKED BEFORE TS_SMGSM IN TWO CASES:
C     /A/  BEFORE THE FIRST TRANSFORMATION OF COORDINATES
C     /B/  IF THE VALUES OF IYEAR,IDAY,IHOUR,MIN,ISEC HAVE BEEN CHANGED
C
C     LAST MODIFICATION:  MARCH 31, 2003
C
C     AUTHOR:  N. A. TSYGANENKO
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION XGSM,XSM,YGSM,YSM,ZGSM,ZSM
      INTEGER J
C     ..
C     .. Common blocks ..
      COMMON /GEOPACK1/A,SPS,CPS,B,AB
      DOUBLE PRECISION CPS,SPS
      DOUBLE PRECISION A(10),AB(8),B(15)
C     ..
      IF (J.GT.0) THEN
         XGSM = XSM*CPS + ZSM*SPS
         YGSM = YSM
         ZGSM = ZSM*CPS - XSM*SPS
      ELSE
         XSM = XGSM*CPS - ZGSM*SPS
         YSM = YGSM
         ZSM = XGSM*SPS + ZGSM*CPS
      END IF
      RETURN
      END
C
C==========================================================================================
C
      SUBROUTINE GEOGSM(XGEO,YGEO,ZGEO,XGSM,YGSM,ZGSM,J)
C
CCONVERTS GEOGRAPHIC (GEO) TO GEOCENTRIC SOLAR MAGNETOSPHERIC (GSM) COORDINATES
C     OR VICA VERSA.
C
C                   J>0                   J<0
C     ----- INPUT:  J,XGEO,YGEO,ZGEO    J,XGSM,YGSM,ZGSM
C     ---- OUTPUT:    XGSM,YGSM,ZGSM      XGEO,YGEO,ZGEO
C
CATTENTION:  SUBROUTINE  TS_RECALC  MUST BE INVOKED BEFORE GEOGSM IN TWO CASES:
C     /A/  BEFORE THE FIRST TRANSFORMATION OF COORDINATES
C     /B/  IF THE VALUES OF IYEAR,IDAY,IHOUR,MIN,ISEC  HAVE BEEN CHANGED
C
C     LAST MODIFICATION: MARCH 31, 2003
C
C     AUTHOR:  N. A. TSYGANENKO
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION XGEO,XGSM,YGEO,YGSM,ZGEO,ZGSM
      INTEGER J
C     ..
C     .. Common blocks ..
      COMMON /GEOPACK1/AA,A11,A21,A31,A12,A22,A32,A13,A23,A33,D,B
      DOUBLE PRECISION A11,A12,A13,A21,A22,A23,A31,A32,A33,D
      DOUBLE PRECISION AA(17),B(8)
C     ..
      IF (J.GT.0) THEN
         XGSM = A11*XGEO + A12*YGEO + A13*ZGEO
         YGSM = A21*XGEO + A22*YGEO + A23*ZGEO
         ZGSM = A31*XGEO + A32*YGEO + A33*ZGEO
      ELSE
         XGEO = A11*XGSM + A21*YGSM + A31*ZGSM
         YGEO = A12*XGSM + A22*YGSM + A32*ZGSM
         ZGEO = A13*XGSM + A23*YGSM + A33*ZGSM
      END IF
      RETURN
      END
C
C=====================================================================================
C
      SUBROUTINE RHAND(X,Y,Z,R1,R2,R3,IOPT,PARMOD,EXNAME,INNAME)
C
CCALCULATES THE COMPONENTS OF THE RIGHT HAND SIDE VECTOR IN THE TS_GEOMAGNETIC FIELD
C     LINE EQUATION  (a subsidiary subroutine for the subroutine STEP)
C
C     LAST MODIFICATION:  MARCH 31, 2003
C
C     AUTHOR:  N. A. TSYGANENKO
C
CEXNAME AND INNAME ARE NAMES OF SUBROUTINES FOR THE EXTERNAL AND INTERNAL
C     PARTS OF THE TOTAL FIELD
C
C     common block added by B. Rideout to signal error
      COMMON /BR_ERR/IERR
      INTEGER IERR
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION R1,R2,R3,X,Y,Z
      INTEGER IOPT
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION PARMOD(10)
C     ..
C     .. Subroutine Arguments ..
      EXTERNAL EXNAME,INNAME
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION B,BX,BXGSM,BY,BYGSM,BZ,BZGSM,HXGSM,HYGSM,HZGSM
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC SQRT
C     ..
C     .. Common blocks ..
      COMMON /GEOPACK1/A,PSI,AA,DS3,BB
      DOUBLE PRECISION DS3,PSI
      DOUBLE PRECISION A(15),AA(10),BB(8)
C     ..
      CALL EXNAME(IOPT,PARMOD,PSI,X,Y,Z,BXGSM,BYGSM,BZGSM)
C     error check added by brideout
      IF (IERR .EQ. 1) THEN
          RETURN
      END IF
      CALL INNAME(X,Y,Z,HXGSM,HYGSM,HZGSM)
      BX = BXGSM + HXGSM
      BY = BYGSM + HYGSM
      BZ = BZGSM + HZGSM
      B = DS3/SQRT(BX**2+BY**2+BZ**2)
      R1 = BX*B
      R2 = BY*B
      R3 = BZ*B
      RETURN
      END
C
C===================================================================================
C
      SUBROUTINE STEP(X,Y,Z,DS,ERRIN,IOPT,PARMOD,EXNAME,INNAME)
C
C     RE-CALCULATES {X,Y,Z}, MAKING A STEP ALONG A FIELD LINE.
CDS IS THE STEP SIZE, ERRIN IS PERMISSIBLE ERROR VALUE, IOPT SPECIFIES THE EXTERNAL
CMODEL VERSION, THE ARRAY PARMOD CONTAINS INPUT PARAMETERS FOR THAT MODEL
C     EXNAME IS THE NAME OF THE EXTERNAL FIELD SUBROUTINE
CINNAME IS THE NAME OF THE INTERNAL FIELD SUBROUTINE (EITHER DIP OR IGRF)
C
C ALL THE PARAMETERS ARE INPUT ONES; OUTPUT IS THE RENEWED TRIPLET X,Y,Z
C
C     LAST MODIFICATION:  MARCH 31, 2003
C
C     AUTHOR:  N. A. TSYGANENKO
C
C     common block added by B. Rideout to signal error
      COMMON /BR_ERR/IERR
      INTEGER IERR
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION DS,ERRIN,X,Y,Z
      INTEGER IOPT
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION PARMOD(10)
C     ..
C     .. Subroutine Arguments ..
      EXTERNAL EXNAME,INNAME
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION ERRCUR,R11,R12,R13,R21,R22,R23,R31,R32,R33,R41,
     *                 R42,R43,R51,R52,R53
C     ..
C     .. External Subroutines ..
      EXTERNAL RHAND
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS
C     ..
C     .. Common blocks ..
      COMMON /GEOPACK1/A,DS3,B
      DOUBLE PRECISION DS3
      DOUBLE PRECISION A(26),B(8)
C     ..
   10 DS3 = -DS/3.D0
      CALL RHAND(X,Y,Z,R11,R12,R13,IOPT,PARMOD,EXNAME,INNAME)
C     error check added by brideout
      IF (IERR .EQ. 1) THEN
          RETURN
      END IF
      CALL RHAND(X+R11,Y+R12,Z+R13,R21,R22,R23,IOPT,PARMOD,EXNAME,
     *           INNAME)
C     error check added by brideout
      IF (IERR .EQ. 1) THEN
          RETURN
      END IF
      CALL RHAND(X+.5D0*(R11+R21),Y+.5D0*(R12+R22),Z+.5D0*(R13+R23),R31,
     *           R32,R33,IOPT,PARMOD,EXNAME,INNAME)
C     error check added by brideout
      IF (IERR .EQ. 1) THEN
          RETURN
      END IF
      CALL RHAND(X+.375D0*(R11+3.D0*R31),Y+.375D0*(R12+3.D0*R32),
     *           Z+.375D0*(R13+3.D0*R33),R41,R42,R43,IOPT,PARMOD,EXNAME,
     *           INNAME)
C     error check added by brideout
      IF (IERR .EQ. 1) THEN
          RETURN
      END IF
      CALL RHAND(X+1.5D0*(R11-3.D0*R31+4.D0*R41),
     *           Y+1.5D0*(R12-3.D0*R32+4.D0*R42),
     *           Z+1.5D0*(R13-3.D0*R33+4.D0*R43),R51,R52,R53,IOPT,
     *           PARMOD,EXNAME,INNAME)
C     error check added by brideout
      IF (IERR .EQ. 1) THEN
          RETURN
      END IF
      ERRCUR = ABS(R11-4.5D0*R31+4.D0*R41-.5D0*R51) +
     *         ABS(R12-4.5D0*R32+4.D0*R42-.5D0*R52) +
     *         ABS(R13-4.5D0*R33+4.D0*R43-.5D0*R53)
      IF (ERRCUR.LT.ERRIN) GO TO 20
      DS = DS*.5D0
      GO TO 10
   20 X = X + .5D0*(R11+4.D0*R41+R51)
      Y = Y + .5D0*(R12+4.D0*R42+R52)
      Z = Z + .5D0*(R13+4.D0*R43+R53)
      IF (ERRCUR.LT.ERRIN*.04D0 .AND. ABS(DS).LT.1.33D0) DS = DS*1.5D0
      RETURN
      END
C
C==============================================================================
C
      SUBROUTINE TRACE(XI,YI,ZI,DIR,RLIM,R0,IOPT,PARMOD,EXNAME,INNAME,
     *                 XF,YF,ZF,XX,YY,ZZ,L)
C
C    TRACES A FIELD LINE FROM AN ARBITRARY POINT OF SPACE TO THE EARTH'S
C     SURFACE OR TO A MODEL LIMITING BOUNDARY.
C
CTHE HIGHEST ORDER OF SPHERICAL HARMONICS IN THE MAIN FIELD EXPANSION USED
CIN THE MAPPING IS CALCULATED AUTOMATICALLY. IF INNAME=IGRF_GSM, THEN AN IGRF MODEL
CFIELD WILL BE USED, AND IF INNAME=DIP, A PURE DIPOLE FIELD WILL BE USED.
CIN ANY CASE, BEFORE CALLING TRACE, ONE SHOULD INVOKE TS_RECALC, TO CALCULATE CORRECT
CVALUES OF THE IGRF COEFFICIENTS AND ALL QUANTITIES NEEDED FOR TRANSFORMATIONS
C     BETWEEN COORDINATE SYSTEMS INVOLVED IN THIS CALCULATIONS.
C
CALTERNATIVELY, THE SUBROUTINE TS_RECALC CAN BE INVOKED WITH THE DESIRED VALUES OF
CIYEAR AND IDAY (TO SPECIFY THE DIPOLE MOMENT), WHILE THE VALUES OF THE DIPOLE
CTILT ANGLE PSI (IN RADIANS) AND ITS SINE (SPS) AND COSINE (CPS) CAN BE EXPLICITLY
CSPECIFIED AND FORWARDED TO THE COMMON BLOCK GEOPACK1 (11th, 12th, AND 16th ELEMENTS, RESP.)
C
C     ------------- INPUT PARAMETERS:
C
CXI,YI,ZI - GSM COORDS OF INITIAL POINT (IN EARTH RADII, 1 RE = 6371.2 km),
C
CDIR - SIGN OF THE TRACING DIRECTION: IF DIR=1.0 THEN WE MOVE ANTIPARALLEL TO THE
C     FIELD VECTOR (E.G. FROM NORTHERN TO SOUTHERN CONJUGATE POINT),
C     AND IF DIR=-1.0 THEN THE TRACING GOES IN THE OPPOSITE DIRECTION.
C
CR0 -  RADIUS OF A SPHERE (IN RE) FOR WHICH THE FIELD LINE ENDPOINT COORDINATES
C     XF,YF,ZF  SHOULD BE CALCULATED.
C
CRLIM - UPPER LIMIT OF THE GEOCENTRIC DISTANCE, WHERE THE TRACING IS TERMINATED.
C
CIOPT - A MODEL INDEX; CAN BE USED FOR SPECIFYING AN OPTION OF THE EXTERNAL FIELD
C  MODEL (E.G., INTERVAL OF THE KP-INDEX). ALTERNATIVELY, ONE CAN USE THE ARRAY
C  PARMOD FOR THAT PURPOSE (SEE BELOW); IN THAT CASE IOPT IS JUST A DUMMY PARAMETER.
C
CPARMOD -  A 10-ELEMENT ARRAY CONTAINING MODEL PARAMETERS, NEEDED FOR A UNIQUE
C SPECIFICATION OF THE EXTERNAL FIELD. THE CONCRETE MEANING OF THE COMPONENTS
C   OF PARMOD DEPENDS ON A SPECIFIC VERSION OF THE EXTERNAL FIELD MODEL.
C
CEXNAME - NAME OF A SUBROUTINE PROVIDING COMPONENTS OF THE EXTERNAL MAGNETIC FIELD
C     (E.G., T96_01).
CINNAME - NAME OF A SUBROUTINE PROVIDING COMPONENTS OF THE INTERNAL MAGNETIC FIELD
C     (EITHER DIP OR IGRF_GSM).
C
C     -------------- OUTPUT PARAMETERS:
C
C     XF,YF,ZF - GSM COORDS OF THE LAST CALCULATED POINT OF A FIELD LINE
CXX,YY,ZZ - ARRAYS, CONTAINING COORDS OF FIELD LINE POINTS. HERE THEIR MAXIMAL LENGTH WAS
C      ASSUMED EQUAL TO 999.
CL - ACTUAL NUMBER OF THE CALCULATED FIELD LINE POINTS. IF L EXCEEDS 999, TRACING
C     TERMINATES, AND A WARNING IS DISPLAYED.
C
C     LAST MODIFICATION:  MARCH 31, 2003.
C
C     AUTHOR:  N. A. TSYGANENKO
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION DIR,R0,RLIM,XF,XI,YF,YI,ZF,ZI
      INTEGER IOPT,L
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION PARMOD(10),XX(1000),YY(1000),ZZ(1000)
C     ..
C     .. Subroutine Arguments ..
      EXTERNAL EXNAME,INNAME
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION AD,AL,DS,ERR,FC,R,R1,R2,R3,RR,RYZ,X,XR,Y,YR,Z,ZR
C     ..
C     .. External Subroutines ..
      EXTERNAL RHAND,STEP
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC SQRT
C     ..
C     .. Common blocks ..
      COMMON /GEOPACK1/AA,DD,BB
      DOUBLE PRECISION DD
      DOUBLE PRECISION AA(26),BB(8)
C
C     common block added by B. Rideout to signal error
      COMMON /BR_ERR/IERR
      INTEGER IERR
C     ..
C     initialize to remove warnings
      XR = 0.0
      YR = 0.0
      ZR = 0.0
C
C     initialize error flag
      IERR = 0
C
      ERR = 0.0001D0
      L = 0
      DS = 0.5D0*DIR
      X = XI
      Y = YI
      Z = ZI
      DD = DIR
      AL = 0.D0
c
chere we call RHAND just to find out the sign of the radial component of the field
cvector, and to determine the initial direction of the tracing (i.e., either away
c     or towards Earth):
c
      CALL RHAND(X,Y,Z,R1,R2,R3,IOPT,PARMOD,EXNAME,INNAME)
C     error check added by brideout
      IF (IERR .EQ. 1) THEN
          L = 999
          RETURN
      END IF
      AD = 0.01D0
      IF (X*R1+Y*R2+Z*R3.LT.0.D0) AD = -0.01D0
C
c     |AD|=0.01 and its sign follows the rule:
c(1) if DIR=1 (tracing antiparallel to B vector) then the sign of AD is the same as of Br
c(2) if DIR=-1 (tracing parallel to B vector) then the sign of AD is opposite to that of Br
cAD is defined in order to initialize the value of RR (radial distance at previous step):
      RR = SQRT(X**2+Y**2+Z**2) + AD
   10 L = L + 1
      IF (L.GT.999) RETURN
      XX(L) = X
      YY(L) = Y
      ZZ(L) = Z
      RYZ = Y**2 + Z**2
      R2 = X**2 + RYZ
      R = SQRT(R2)
ccheck if the line hit the outer tracing boundary; if yes, then terminate
c     the tracing (label 8):
      IF (R.GT.RLIM .OR. RYZ.GT.1600.D0 .OR. X.GT.20.D0) GO TO 70
c
ccheck whether or not the inner tracing boundary was crossed from outside,
cif yes, then calculate the footpoint position by interpolation (go to label 6):
c
      IF (R.LT.R0 .AND. RR.GT.R) GO TO 50
c  check if (i) we are moving outward, or (ii) we are still sufficiently
c     far from Earth (beyond R=5Re); if yes, proceed further:
c
      IF (R.GE.RR .OR. R.GT.5.D0) GO TO 40
cnow we moved closer inward (between R=3 and R=5); go to 3 and begin logging
cprevious values of X,Y,Z, to be used in the interpolation (after having
c     crossed the inner tracing boundary):
      IF (R.GE.3.D0) GO TO 20
c
cwe entered inside the sphere R=3: to avoid too large steps (and hence inaccurate
cinterpolated position of the footpoint), enforce the progressively smaller
c     stepsize values as we approach the inner boundary R=R0:
c
      FC = 0.2D0
      IF (R-R0.LT.0.05D0) FC = 0.05D0
      AL = FC*(R-R0+0.2D0)
      DS = DIR*AL
      GO TO 30
   20 DS = DIR
   30 XR = X
      YR = Y
      ZR = Z
   40 RR = R
      CALL STEP(X,Y,Z,DS,ERR,IOPT,PARMOD,EXNAME,INNAME)
C     error check added by brideout
      IF (IERR .EQ. 1) THEN
          L = 999
          RETURN
      END IF
      GO TO 10
c
cfind the footpoint position by interpolating between the current and previous
c     field line points:
c
   50 R1 = (R0-R)/(RR-R)
      X = X - (X-XR)*R1
      Y = Y - (Y-YR)*R1
      Z = Z - (Z-ZR)*R1
      GO TO 70
C     warning surpressed by B. Rideout - code needs to check if L >= 999
C   60 WRITE (*,FMT=9000)
C      L = 999
   70 XF = X
      YF = Y
      ZF = Z
      RETURN
 9000 FORMAT (/,/,1X,'**** COMPUTATIONS IN THE SUBROUTINE TRACE ARE',
     *    ' TERMINATED: THE CURRENT NUMBER OF POINTS EXCEEDED 1000 ****'
     *       ,/,/)
      END
c
C====================================================================================
C
      SUBROUTINE SHUETAL_MGNP(XN_PD,VEL,BZIMF,XGSM,YGSM,ZGSM,XMGNP,
     *                        YMGNP,ZMGNP,DIST,ID)
C
CFOR ANY POINT OF SPACE WITH COORDINATES (XGSM,YGSM,ZGSM) AND SPECIFIED CONDITIONS
C     IN THE INCOMING SOLAR WIND, THIS SUBROUTINE:
C
C(1) DETERMINES IF THE POINT (XGSM,YGSM,ZGSM) LIES INSIDE OR OUTSIDE THE
C      MODEL MAGNETOPAUSE OF SHUE ET AL. (JGR-A, V.103, P. 17691, 1998).
C
C(2) CALCULATES THE GSM POSITION OF A POINT {XMGNP,YMGNP,ZMGNP}, LYING AT THE MODEL
C MAGNETOPAUSE AND ASYMPTOTICALLY TENDING TO THE NEAREST BOUNDARY POINT WITH
C RESPECT TO THE OBSERVATION POINT {XGSM,YGSM,ZGSM}, AS IT APPROACHES THE MAGNETO-
C      PAUSE.
C
CINPUT: XN_PD - EITHER SOLAR WIND PROTON NUMBER DENSITY (PER C.C.) (IF VEL>0)
C               OR THE SOLAR WIND RAM PRESSURE IN NANOPASCALS   (IF VEL<0)
C         BZIMF - IMF BZ IN NANOTESLAS
C
C         VEL - EITHER SOLAR WIND VELOCITY (KM/SEC)
C              OR ANY NEGATIVE NUMBER, WHICH INDICATES THAT XN_PD STANDS
C                FOR THE SOLAR WIND PRESSURE, RATHER THAN FOR THE DENSITY
C
C    XGSM,YGSM,ZGSM - GSM POSITION OF THE OBSERVATION POINT IN EARTH RADII
C
C     OUTPUT: XMGNP,YMGNP,ZMGNP - GSM POSITION OF THE BOUNDARY POINT
C     DIST - DISTANCE (IN RE) BETWEEN THE OBSERVATION POINT (XGSM,YGSM,ZGSM)
C                 AND THE MODEL NAGNETOPAUSE
C      ID -  POSITION FLAG:  ID=+1 (-1) MEANS THAT THE OBSERVATION POINT
C         LIES INSIDE (OUTSIDE) OF THE MODEL MAGNETOPAUSE, RESPECTIVELY.
C
C     OTHER SUBROUTINES USED: T96_MGNP
C
c          AUTHOR:  N.A. TSYGANENKO,
C          DATE:    APRIL 4, 2003.
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION BZIMF,DIST,VEL,XGSM,XMGNP,XN_PD,YGSM,YMGNP,ZGSM,
     *                 ZMGNP
      INTEGER ID
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION ALPHA,CT,DR,DS,DT,F,GRADF,GRADF_R,GRADF_T,PD,PHI,
     *                 R,R0,RHO,RHO2,RM,ST,T,XMT96,YMT96,ZMT96
      INTEGER ID96,NIT
C     ..
C     .. External Subroutines ..
      EXTERNAL T96_MGNP
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ATAN2,COS,DLOG,SIN,SQRT,TANH
C     ..
      IF (VEL.LT.0.D0) THEN
         PD = XN_PD
      ELSE
         PD = 1.94D-6*XN_PD*VEL**2
      END IF
c
cDEFINE THE ANGLE PHI, MEASURED DUSKWARD FROM THE NOON-MIDNIGHT MERIDIAN PLANE;
CIF THE OBSERVATION POINT LIES ON THE X AXIS, THE ANGLE PHI CANNOT BE UNIQUELY
C     DEFINED, AND WE SET IT AT ZERO:
c
      IF (YGSM.NE.0.D0 .OR. ZGSM.NE.0.D0) THEN
         PHI = ATAN2(YGSM,ZGSM)
      ELSE
         PHI = 0.D0
      END IF
C
CFIRST, FIND OUT IF THE OBSERVATION POINT LIES INSIDE THE SHUE ET AL BDRY
C     AND SET THE VALUE OF THE ID FLAG:
C
      ID = -1
      R0 = (10.22D0+1.29D0*TANH(0.184D0*(BZIMF+8.14D0)))*
     *     PD**(-.15151515D0)
      ALPHA = (0.58D0-0.007D0*BZIMF)*(1.D0+0.024D0*DLOG(PD))
      R = SQRT(XGSM**2+YGSM**2+ZGSM**2)
      RM = R0*(2.D0/(1.D0+XGSM/R))**ALPHA
      IF (R.LE.RM) ID = +1
C
C   NOW, FIND THE CORRESPONDING T96 MAGNETOPAUSE POSITION, TO BE USED AS
C  A STARTING APPROXIMATION IN THE SEARCH OF A CORRESPONDING SHUE ET AL.
C     BOUNDARY POINT:
C
      CALL T96_MGNP(PD,-1.D0,XGSM,YGSM,ZGSM,XMT96,YMT96,ZMT96,DIST,ID96)
C
      RHO2 = YMT96**2 + ZMT96**2
      R = SQRT(RHO2+XMT96**2)
      ST = SQRT(RHO2)/R
      CT = XMT96/R
C
C    NOW, USE NEWTON'S ITERATIVE METHOD TO FIND THE NEAREST POINT AT THE
C     SHUE ET AL.'S BOUNDARY:
C
      NIT = 0
   10 T = ATAN2(ST,CT)
      RM = R0*(2.D0/(1.D0+CT))**ALPHA
      F = R - RM
      GRADF_R = 1.D0
      GRADF_T = -ALPHA/R*RM*ST/(1.D0+CT)
      GRADF = SQRT(GRADF_R**2+GRADF_T**2)
      DR = -F/GRADF**2
      DT = DR/R*GRADF_T
      R = R + DR
      T = T + DT
      ST = SIN(T)
      CT = COS(T)
      DS = SQRT(DR**2+(R*DT)**2)
      NIT = NIT + 1
      IF (NIT.GT.1000) THEN
         PRINT *,
     *  ' BOUNDARY POINT COULD NOT BE FOUND; ITERATIONS DO NOT CONVERGE'
      END IF
      IF (DS.GT.1.D-4) GO TO 10
      XMGNP = R*COS(T)
      RHO = R*SIN(T)
      YMGNP = RHO*SIN(PHI)
      ZMGNP = RHO*COS(PHI)
      DIST = SQRT((XGSM-XMGNP)**2+(YGSM-YMGNP)**2+(ZGSM-ZMGNP)**2)
      RETURN
      END
C
C=======================================================================================
C
      SUBROUTINE T96_MGNP(XN_PD,VEL,XGSM,YGSM,ZGSM,XMGNP,YMGNP,ZMGNP,
     *                    DIST,ID)
C
CFOR ANY POINT OF SPACE WITH GIVEN COORDINATES (XGSM,YGSM,ZGSM), THIS SUBROUTINE DEFINES
CTHE POSITION OF A POINT (XMGNP,YMGNP,ZMGNP) AT THE T96 MODEL MAGNETOPAUSE, HAVING THE
CSAME VALUE OF THE ELLIPSOIDAL TAU-COORDINATE, AND THE DISTANCE BETWEEN THEM.  THIS IS
CNOT THE SHORTEST DISTANCE D_MIN TO THE BOUNDARY, BUT DIST ASYMPTOTICALLY TENDS TO D_MIN,
C     AS THE OBSERVATION POINT GETS CLOSER TO THE MAGNETOPAUSE.
C
CINPUT: XN_PD - EITHER SOLAR WIND PROTON NUMBER DENSITY (PER C.C.) (IF VEL>0)
C               OR THE SOLAR WIND RAM PRESSURE IN NANOPASCALS   (IF VEL<0)
C         VEL - EITHER SOLAR WIND VELOCITY (KM/SEC)
C              OR ANY NEGATIVE NUMBER, WHICH INDICATES THAT XN_PD STANDS
C                FOR THE SOLAR WIND PRESSURE, RATHER THAN FOR THE DENSITY
C
C    XGSM,YGSM,ZGSM - COORDINATES OF THE OBSERVATION POINT IN EARTH RADII
C
COUTPUT: XMGNP,YMGNP,ZMGNP - GSM POSITION OF THE BOUNDARY POINT, HAVING THE SAME
C      VALUE OF TAU-COORDINATE AS THE OBSERVATION POINT (XGSM,YGSM,ZGSM)
C          DIST -  THE DISTANCE BETWEEN THE TWO POINTS, IN RE,
C     ID -    POSITION FLAG; ID=+1 (-1) MEANS THAT THE POINT (XGSM,YGSM,ZGSM)
C          LIES INSIDE (OUTSIDE) THE MODEL MAGNETOPAUSE, RESPECTIVELY.
C
C   THE PRESSURE-DEPENDENT MAGNETOPAUSE IS THAT USED IN THE T96_01 MODEL
C   (TSYGANENKO, JGR, V.100, P.5599, 1995; ESA SP-389, P.181, OCT. 1996)
C
c     AUTHOR:  N.A. TSYGANENKO
C     DATE:    AUG.1, 1995, REVISED APRIL 3, 2003.
C
CDEFINE SOLAR WIND DYNAMIC PRESSURE (NANOPASCALS, ASSUMING 4% OF ALPHA-PARTICLES),
C     IF NOT EXPLICITLY SPECIFIED IN THE INPUT:
C     .. Scalar Arguments ..
      DOUBLE PRECISION DIST,VEL,XGSM,XMGNP,XN_PD,YGSM,YMGNP,ZGSM,ZMGNP
      INTEGER ID
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION A,A0,ARG,PD,PHI,RAT,RAT16,RHO,RHOMGNP,S0,S00,
     *                 SIGMA,SQ1,SQ2,TAU,X0,X00,XDZT,XKSI,XM
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ATAN2,COS,SIN,SQRT
C     ..
      IF (VEL.LT.0.D0) THEN
         PD = XN_PD
      ELSE
         PD = 1.94D-6*XN_PD*VEL**2
C
      END IF
C
C     RATIO OF PD TO THE AVERAGE PRESSURE, ASSUMED EQUAL TO 2 nPa:
      RAT = PD/2.0D0
      RAT16 = RAT**0.14D0
C(THE POWER INDEX 0.14 IN THE SCALING FACTOR IS THE BEST-FIT VALUE OBTAINED FROM DATA
C     AND USED IN THE T96_01 VERSION)
C
C     VALUES OF THE MAGNETOPAUSE PARAMETERS FOR  PD = 2 nPa:
C
      A0 = 70.D0
      S00 = 1.08D0
      X00 = 5.48D0
C
C  VALUES OF THE MAGNETOPAUSE PARAMETERS, SCALED BY THE ACTUAL PRESSURE:
C
      A = A0/RAT16
      S0 = S00
      X0 = X00/RAT16
      XM = X0 - A
C
C(XM IS THE X-COORDINATE OF THE "SEAM" BETWEEN THE ELLIPSOID AND THE CYLINDER)
C
C     (FOR DETAILS ON THE ELLIPSOIDAL COORDINATES, SEE THE PAPER:
C      N.A.TSYGANENKO, SOLUTION OF CHAPMAN-FERRARO PROBLEM FOR AN
C      ELLIPSOIDAL MAGNETOPAUSE, PLANET.SPACE SCI., V.37, P.1037, 1989).
C
      IF (YGSM.NE.0.D0 .OR. ZGSM.NE.0.D0) THEN
         PHI = ATAN2(YGSM,ZGSM)
      ELSE
         PHI = 0.D0
      END IF
C
      RHO = SQRT(YGSM**2+ZGSM**2)
C
      IF (XGSM.LT.XM) THEN
         XMGNP = XGSM
         RHOMGNP = A*SQRT(S0**2-1)
         YMGNP = RHOMGNP*SIN(PHI)
         ZMGNP = RHOMGNP*COS(PHI)
         DIST = SQRT((XGSM-XMGNP)**2+(YGSM-YMGNP)**2+(ZGSM-ZMGNP)**2)
         IF (RHOMGNP.GT.RHO) ID = +1
         IF (RHOMGNP.LE.RHO) ID = -1
         RETURN
      END IF
C
      XKSI = (XGSM-X0)/A + 1.D0
      XDZT = RHO/A
      SQ1 = SQRT((1.D0+XKSI)**2+XDZT**2)
      SQ2 = SQRT((1.D0-XKSI)**2+XDZT**2)
      SIGMA = 0.5D0*(SQ1+SQ2)
      TAU = 0.5D0*(SQ1-SQ2)
C
C     NOW CALCULATE (X,Y,Z) FOR THE CLOSEST POINT AT THE MAGNETOPAUSE
C
      XMGNP = X0 - A*(1.D0-S0*TAU)
      ARG = (S0**2-1.D0)*(1.D0-TAU**2)
      IF (ARG.LT.0.D0) ARG = 0.D0
      RHOMGNP = A*SQRT(ARG)
      YMGNP = RHOMGNP*SIN(PHI)
      ZMGNP = RHOMGNP*COS(PHI)
C
CNOW CALCULATE THE DISTANCE BETWEEN THE POINTS {XGSM,YGSM,ZGSM} AND {XMGNP,YMGNP,ZMGNP}:
C(IN GENERAL, THIS IS NOT THE SHORTEST DISTANCE D_MIN, BUT DIST ASYMPTOTICALLY TENDS
C     TO D_MIN, AS WE ARE GETTING CLOSER TO THE MAGNETOPAUSE):
C
      DIST = SQRT((XGSM-XMGNP)**2+(YGSM-YMGNP)**2+(ZGSM-ZMGNP)**2)
C
      IF (SIGMA.GT.S0) ID = -1
      IF (SIGMA.LE.S0) ID = +1
C                                           THE MAGNETOSPHERE
      RETURN
      END
