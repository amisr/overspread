/*  $Id: geometry.c,v 1.7 2003/01/14 14:34:40 brideout Exp $ */

#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <ctype.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <cedarIO.h>
#include <madrec.h>
#include <cedar.h>
#include <date.h>

/* include code written by National Renewable Energy Lab */
#include <solpos00.h>   

/***********************************************************************
* 
* sprod calculates the scalar product of two vectors a and b,
* sprod = a .dot. b.
*/
double
sprod(double *a, double *b)
{
      double sp;

    sp = a[0]*b[0] + a[1]*b[1] + a[2]*b[2];
    return(sp);
}


/***********************************************************************
*      
* vadd calculates the sum of two vectors a and b, c = a + b.
*/
int 
vadd(double *a, double *b, double *c)
{
    c[0] = a[0] + b[0];
    c[1] = a[1] + b[1];
    c[2] = a[2] + b[2];
    return (0);
}


/***********************************************************************
*
* vsub calculates the difference of two vectors a and b, c = a - b.
*/
int
vsub(double *a, double *b, double *c)
{
    c[0] = a[0] - b[0];
    c[1] = a[1] - b[1];
    c[2] = a[2] - b[2];
    return(0);
}


/***********************************************************************
*
* csconv converts between cartesian coordinates x,y,z and spherical
* coordinates r,theta,phi. if imode=1, (x,y,z) -> (r,theta,phi).
* if imode=2, (r,theta,phi) -> (x,y,z). theta and phi are in
* degrees.
*/
int
csconv(double *xp, double *yp, double *zp,
       double *rp, double *thetap, double *phip,
       int imode)
{ 
    double dtr=0.0174532925199, rlmin=1.e-20,
           x, y, z, r, theta, phi,
           rho2, ct, t, st, cp, p, sp;

    if (imode == 1) {
        x = *xp;
        y = *yp;
        z = *zp;
	rho2 = x*x + y*y;
	r = sqrt(rho2 + z*z);
        if (z < rlmin && z > 0)
            z = rlmin;
        else if (z > -rlmin && z < 0)
            z = -rlmin;
        else
            z = z;
	theta = atan2(sqrt(rho2), z)/dtr;
        if (x < rlmin && x > 0)
            x = rlmin;
        else if (x > -rlmin && x < 0)
            x = -rlmin;
        else
            x = x;
	phi = atan2(y, x)/dtr;
        *rp = r;
        *thetap = theta;
        *phip = phi;
	return(0);
    }
    else if (imode == 2) {
        r = *rp;
        theta = *thetap;
        phi = *phip;
        t = dtr*theta;
        p = dtr*phi;
        ct = cos(t);
        st = sin(t);
        cp = cos(p);
        sp = sin(p);
        x = r*st*cp;
        y = r*st*sp;
        z = r*ct;
        *xp = x;
        *yp = y;
        *zp = z;
        return(0);
    }
    else {
        return(1);
    }
}

/***********************************************************************
*
* vctcnv converts between the cartesian and spherical coordinate
* representations of a vector field f. (fx,fy,fz) are the
* components of the field at (x,y,z). (fr,ft,fp) are the
* components of the field at (r,theta,phi) in the directions of
* increasing r, increasing theta and increasing phi. if imode=1,
* (fx,fy,fz,x,y,z) -> (fr,ft,fp,r,theta,phi). if imode=2,
* (fr,ft,fp,r,theta,phi) -> (fx,fy,fz,x,y,z). theta and phi are
* in degrees.
*/
int
vctcnv(double *fxp, double *fyp, double *fzp,
       double *xp, double *yp, double *zp,
       double *frp, double *ftp, double *fpp,
       double *rp, double *thetap, double *phip,
       int imode)
{
    double dtr=0.0174532925199, rlmin=1.e-20,
           fx, fy, fz, x, y, z, fr, ft, fp, r, theta, phi,
           rho2, ct, st, cp, sp, t, p;
    
    if (imode == 1) {  
        fx = *fxp;
        fy = *fyp;
        fz = *fzp;  
        x = *xp;
        y = *yp;
        z = *zp;
	rho2 = x*x + y*y;
	r = sqrt(rho2 + z*z);
        if (z < rlmin && z > 0)
            z = rlmin;
        else if (z > -rlmin && z < 0)
            z = -rlmin;
        else
            z = z;
	theta = atan2(sqrt(rho2), z);
        if (x < rlmin && x > 0)
            x = rlmin;
        else if (x > -rlmin && x < 0)
            x = -rlmin;
        else
            x = x;
	phi = atan2(y, x);
	ct = cos(theta);
	st = sin(theta);
	cp = cos(phi);
	sp = sin(phi);
	theta = theta/dtr;
	phi = phi/dtr;
	fr = st*cp*fx + st*sp*fy + ct*fz;
	ft = ct*cp*fx + ct*sp*fy - st*fz;
	fp = -sp*fx + cp*fy;
        *frp= fr;
        *ftp = ft;
        *fpp = fp;
        *rp= r;
        *thetap = theta;
        *phip = phi;
	return(0);
    }
    else if (imode == 2) {
        fr = *frp;
        ft = *ftp;
        fp = *fpp;
        r = *rp;
        theta = *thetap;
        phi = *phip;
	t = dtr*theta;
	p = dtr*phi;
	ct = cos(t);
	st = sin(t);
	cp = cos(p);
	sp = sin(p);
	x = r*st*cp;
	y = r*st*sp;
	z = r*ct;
	fx = st*cp*fr + ct*cp*ft - sp*fp;
	fy = st*sp*fr + ct*sp*ft + cp*fp;
	fz = ct*fr - st*ft;
        *fxp = fx;
        *fyp = fy;
        *fzp = fz;
        *xp = x;
        *yp = y;
        *zp = z;
	return(0);
    }
    else {
        return(1);
    }
}

/***********************************************************************
*
* point calculates the position of a point defined by the radar
* line-of sight vector to that point.
* 
* input parameters
*    sr    - distance of station from center of earth (km)
*    slat  - geocentric latitude of station (deg)
*    slon  - longitude of station (deg)
*    az    - radar azimuth (deg)
*    el    - radar elevation (deg)
*    range - radar range (km)
* 
* output parameters
*    pr    - distance from center of earth of observation point (km)
*   glat  - observation point geocentric latitude (deg)
*    glon  - observation point longitude (deg)
*/
int
point(double *srp, double *slatp, double *slonp,
      double *azp, double *elp, double *rangep,
      double *prp, double *glatp, double *glonp)
{
    double sr, slat, slon, az, el, range, pr, glat, glon,
           s[3], r[3], p[3], rt, rp, rr, t, el1, az1, slat1;

    sr = *srp;
    slat = *slatp;
    slon = *slonp;
    az = *azp;
    el = *elp;
    range = *rangep;

   /* calculate "line-of-sight" station centered cartesian coords */
    el1 = 90.0 - el;
    az1 = 180.0 - az;
    (void) csconv(&rt, &rp, &rr, &range, &el1, &az1, 2);
    
    /* calculate "line-of-sight" earth centered cartesian coords
	 and "station" earth centered cartesian coords */
    slat1 = 90.0 - slat;
    (void) vctcnv(&r[0], &r[1], &r[2], &s[0], &s[1], &s[2],
           &rr, &rt, &rp, &sr, &slat1, &slon, 2);
    
    /* calculate "observation-point" earth centered cartesian coords */
    (void) vadd(s, r, p);
    
    /* calculate "observation-point" earth centered spherical coords */
    (void) csconv(&p[0], &p[1], &p[2], &pr, &t, &glon, 1);
    glat = 90. - t;

    *prp = pr;
    *glatp = glat;
    *glonp = glon;
    return(0);
}


/***********************************************************************
*
* look calculates the azimuth, elevation and range from a radar
* of a specified point.
* 
* input parameters
*    sr    - distance of station from center of earth (km)
*    slat  - geocentric latitude of station (deg)
*    slon  - longitude of station (deg)
*    pr    - distance from center of earth of observation point (km)
*    glat  - observation point geocentric latitude (deg)
*    glon  - observation point longitude (deg)
* 
* output parameters
*    az    - radar azimuth (deg)
*    el    - radar elevation (deg)
*    range - radar range (km)
*/
int
look(double *srp, double *slatp, double *slonp,
     double *prp, double *glatp, double *glonp,
     double *azp, double *elp, double *rangep)
{
    double sr, slat, slon, pr, glat, glon, az, el, range,
           s[3], r[3], p[3], rr, rt, rp, sr1, st2, sp1, glat1, slat1;

    sr = *srp;
    slat = *slatp;
    slon = *slonp;
    pr = *prp;
    glat = *glatp;
    glon = *glonp;
    
    /*calculate "observation-point" earth centered cartesian coords */
    glat1 = 90.0 - glat;
    (void) csconv(&p[0], &p[1], &p[2], &pr, &glat1, &glon, 2);
    
    /*calculate "station" earth centered cartesian coordinates*/
    slat1 = 90.0 - slat;
    (void) csconv(&s[0], &s[1], &s[2], &sr, &slat1, &slon, 2);
    
    /*calculate "line-of-sight" earth centered cartesian coords*/
    (void) vsub(p, s, r);
    
    /*calculate "line-of-sight" station centered cartesian coords*/
    (void) vctcnv(&r[0], &r[1], &r[2], &s[0], &s[1], &s[2],
                  &rr, &rt, &rp, &sr1, &st2, &sp1, 1);
    
    /*calculate "line-of-sight" station centered spherical coords*/
    (void) csconv(&rt, &rp, &rr, &range, &el, &az, 1);
    el = 90. - el;
    az = 180. - az;

    *azp = az;
    *elp = el;
    *rangep = range;
    return(0);
}


/***********************************************************************
*
* convrt converts between geodetic and geocentric coordinates. the
* reference geoid is that adopted by the iau in 1964. a=6378.16,
* b=6356.7746, f=1/298.25. the equations for conversion from
* geocentric to geodetic are from astron. j., vol 66, 1961, p. 15.
*      i=1   geodetic to geocentric
*      i=2   geocentric to geodetic
*    gdlat   geodetic latitude (degrees)
*    gdalt   altitude above geoid (km)
*    gclat   geocentric latitude (degrees)
*      rkm   geocentric radial distance (km)
*/
int
convrt(int i, double *gdlatp, double *gdaltp,
       double *gclatp, double *rkmp)

{
    double gdlat, gdalt, gclat, rkm,
           sinlat,gdl,coslat,cl2,sb2,sinbet,cosbet,
           x,y,a2,rer,ccl,gcl,scl,s4cl,s2cl,c2cl,sl2,rgeoid,a4,a6,
           a8,c4cl,s8cl,s6cl,dltcl;
    double a=6378.16, ab2=1.0067397, ep2=0.0067397, dtr=0.0174532925199;

    /* geodetic to geocentric */
    if (i == 1) {  
        gdlat = *gdlatp;
        gdalt = *gdaltp;  
	gdl = dtr*gdlat;
	sinlat = sin(gdl);
	coslat = cos(gdl);
	sl2 = sinlat*sinlat;
	cl2 = ab2*coslat;
	cl2 = cl2*cl2;
	sinbet = sinlat/sqrt(sl2+cl2);
	sb2 = sinbet*sinbet;
        if (sb2 > 1.0) sb2=1.0;
	cosbet = sqrt(1. - sb2);
	rgeoid = a/sqrt(1. + ep2*sb2);
	x = rgeoid*cosbet + gdalt*coslat;
	y = rgeoid*sinbet + gdalt*sinlat;
	rkm = sqrt(x*x + y*y);
	gclat = atan2(y,x)/dtr;
        *gclatp = gclat;
        *rkmp = rkm;
	return(0);
    }
    /* geocentic to geodetic */
    else if (i == 2) {
        gclat = *gclatp;
        rkm = *rkmp;   
	rer = rkm/a;
	a2 = ((-1.4127348e-8/rer + .94339131e-8)/rer + .33523288e-2)/rer;
	a4 = (((-1.2545063e-10/rer + .11760996e-9)/rer +
		.11238084e-4)/rer - .2814244e-5)/rer;
	a6 = ((54.939685e-9/rer - 28.301730e-9)/rer + 3.5435979e-9)/rer;
	a8 = (((320./rer - 252.)/rer + 64.)/rer - 5.)/rer*.98008304e-12;
	gcl = dtr*gclat;
	ccl = cos(gcl);
	scl = sin(gcl);
	s2cl = 2.*scl*ccl;
	c2cl = 2.*ccl*ccl - 1.0;
	s4cl = 2.*s2cl*c2cl;
	c4cl = 2.*c2cl*c2cl - 1.0;
	s8cl = 2.*s4cl*c4cl;
	s6cl = s2cl*c4cl + c2cl*s4cl;
	dltcl = s2cl*a2 + s4cl*a4 + s6cl*a6 + s8cl*a8;
	gdlat = gclat + dltcl/dtr;
	gdalt = rkm - a/sqrt(1.+ep2*scl*scl);
        *gdlatp = gdlat;
        *gdaltp = gdalt;
	return(0);
    }
    else {
        return(1);
    }
}


/***********************************************************************
*
* rpcart computes the components (rfx,rfy,rfz) relative to an earth
* centered cartesian coordinate system of the radar line of sight
* vector from a radar with coordinates sr (distance from center
* of earth), slat (geocentric latitude) and slon (longitude). the
* observation point is specified by az (azimuth), el (elevation) and
* range (range). the cartesian coordinates of the observation
* point are returned in (pfx,pfy,pfz).
*    input - sr,slat,slon,az,el,range
*    output - rfx,rfy,rfz,pfx,pfy,pfz
*/
int
rpcart (double *srp,  double *slatp, double *slonp,
        double *azp,  double *elp,   double *rangep,
        double *rfxp, double *rfyp,  double *rfzp,
        double *pfxp, double *pfyp,  double *pfzp)
{
    double dtr=0.0174532925199, rr, rtheta, rphi, a, e,
            ca, sa, ce, se, rx, ry, rz, rfr, rft, rfp;

    rr = *srp;
    rtheta = 90. - *slatp;
    rphi = *slonp;
    a = dtr*(180. - *azp);
    e = dtr*(90. - *elp);
    ca = cos(a);
    sa = sin(a);
    ce = cos(e);
    se = sin(e);
    rfr = *rangep*ce;
    rft = *rangep*se*ca;
    rfp = *rangep*se*sa;
    (void) vctcnv(rfxp, rfyp, rfzp, &rx, &ry, &rz, &rfr, &rft, &rfp, &rr,
                  &rtheta, &rphi, 2);
    *pfxp = rx + *rfxp;
    *pfyp = ry + *rfyp;
    *pfzp = rz + *rfzp;
    return(0);
}


/***********************************************************************
*
* gdv converts a vector field f at geodetic latitude gdlat and
* geocentric latitude gclat from a geocentric based representation
* to a geodetic based representation. the geocentric components
* are fr (radial outward), ft (increasing geocentric colatitude,
* e.g. southward) and fp (increasing east longitude). the
* geodetic components are fx (northward, parallel to surface of
* earth), fy (eastward, parallel to surface of earth) and fz
* (downward, perpendicular to surface of earth). fr,ft,fp thus
* correspond to spherical coordinates r,theta,phi, with their
* origin at the center of the earth. x,y,z are the coordinates
* customarily used to describe the three components of the
* geomagnetic field. fp and fy are the same.
*/
int
gdv(double *gdlatp, double *gclatp,
    double *frp, double *ftp, double *fpp,
    double *fxp, double *fyp, double *fzp)
{
    double dtr=0.0174532925199, gdl, sinlat, coslat, t,
           ct, st, sind, cosd;

    gdl = dtr*(*gdlatp);
    sinlat = sin(gdl);
    coslat = cos(gdl);
    t = dtr*(90. - *gclatp);
    ct = cos(t);
    st = sin(t);
    sind = st*sinlat - ct*coslat;
    cosd = ct*sinlat + st*coslat;
    *fxp = -*ftp*cosd - *frp*sind;
    *fyp = *fpp;
    *fzp =  *ftp*sind - *frp*cosd;
    return (0);
}


/***********************************************************************
*
* los2geodetic calculates the position of a point defined by an instrument
* line-of sight vector to that point. This is a convenience routine in
* which the instrument location is specified by its CEDAR instrument code
* and which returns the geodetic coordinates of the point.
* 
* 
* input parameters
*    kinst - instrument code in metadata
*    az    - radar azimuth (deg)
*    el    - radar elevation (deg)
*    range - radar range (km)
* 
* output parameters
*    gdlat - observation point geodetic latitude (deg)
*    glon  - observation point longitude (deg)
*    gdalt - altitude above geoid (km)
*/

int los2geodetic(int kinst, double az, double el, double range,
             double *gdlatp, double *glonp, double *gdaltp)
{
    double slat, slon, salt, sgclat, sr, pr, pgclat, plat, plon, palt;

    cedarGetStationPos(kinst, &slat, &slon, &salt);

    /* check for missing info (salt never missing) */
    if (slat == missing || salt == missing) {
        *gdlatp = missing;
        *glonp =  missing;
        *gdaltp = missing;
        return(-1);
    }

    /* Convert station coordinates from geodetic to geocentric */
    (void) convrt(1, &slat, &salt, &sgclat, &sr);

    /* Compute geocentric coordinates of specified point */
    (void) point(&sr, &sgclat, &slon, &az, &el, &range,
                 &pr, &pgclat, &plon);

    /* Convert observation point coordinates from geocentric to geodetic */
    (void) convrt(2, &plat, &palt, &pgclat, &pr);

    *gdlatp = plat;
    *glonp = plon;
    *gdaltp = palt;

    return(0);
}


/***********************************************************************
*
* solarzen_az calculates the solar zenith and az angles for a given time, gdlat,
*          and glon.
* 
* 
* input parameters
*    double ut    - Universal time in seconds since 1950
*    double gdlat - geodetic latitude in degrees
*    double glon  - geodetic longitude in degrees
* 
* output parameters
*    szen - solar zenith angle (deg, 0=directly overhead)
*    saz  - solar azimuth angle (deg, N=0, E=90)
*    
*
*    Solar zenith angle is calculated at 0 alt, although this changes
*    very little with altitute.  No atmospheric correction is applied.
*    This method uses solpos.c, written by National Renewable Energy
*    Laboratory.
*
*    This method is a modified version of  stest.c found at 
*    http://rredc.nrel.gov/solar/codes_algs/solpos/
*/
void solarzen_az(double ut, double gdlat, double glon, double * szen, double * saz)
{
    struct posdata pd, *pdat; /* declare a posdata struct and a pointer for it  */
    
    int iyr = 0, imd = 0, ihm = 0, ics = 0;
    int month = 0;
    int day = 0;
    int hour = 0;
    int min = 0;
    int sec = 0;

    
    long retval;              /* to capture S_solpos return codes */
    pdat = &pd; /* point to the structure for convenience */
    S_init (pdat);
    

    /* convert UT */
    dinvmadptr(ut, &iyr, &imd, &ihm, &ics);
    month = imd/100;
    day = imd - 100*month;
    hour = ihm/100;
    min = ihm - 100*hour;
    sec = ics/100;

    pdat->longitude = glon;  
    pdat->latitude  = gdlat; 
    pdat->timezone  =  0.0;   /* Since we always use UT. */
    

    pdat->year      = iyr;   
    pdat->daynum    = madGetDayno(iyr, month, day);
    pdat->hour      = hour;
    pdat->minute    = min;
    pdat->second    = sec;

    retval = S_solpos (pdat);  /* S_solpos function call */
    
    /* check whether error occured */
    if (retval != 0)
    {
        *szen = missing;
        *saz  = missing;
    }
    else
    {
        *szen = pdat->zenetr;
        *saz  = pdat->azim;
    }
}


/***********************************************************************
*
* solardist calculates the distance in km from the center of the earth
*          to the center of the sun at time ut.
* 
* 
* input parameters
*    double ut    - Universal time in seconds since 1950
* 
* returns double - distance in km from the center of the earth
*          to the center of the sun at time ut
*
*    This method is taken from "Practical Astronomy with Your
*    Calculator" 2nd edition, Peter Duffett-Smith, p. 80-87.
*/
double solardist(double ut)
{
    /* constants defined in Practical Astronomy */
    const double daysPerYear     = 365.2422;
    const double eclipLongEpic   = 278.83354;
    const double eclipLongPer    = 282.596403;
    const double eccOrbit        = 0.016718;
    const double semiMajAxis     = 1.495985E8;
    
    /* We use epic at 1950, Practical Astronomy */
    /* uses epic at 1980, so conversion needed  */
    const double sec1950_1980 = 946684800.0;
    
    double numDays = 0.0;
    double numDeg  = 0.0;
    double meanAnom = 0.0;
    double eqCenter = 0.0;
    double trueAnom = 0.0;
    double distance = 0.0;
    
    /* Step one and two in book (p. 83) - computes days since 1/1/1980 */
    numDays = (ut-sec1950_1980)/(3600.0*24.0);
    
    /* Step three in book (p. 83) */
    numDeg = (360.0*numDays)/daysPerYear;
    while (numDeg < 0.0) numDeg += 360.0;
    while (numDeg > 360.0) numDeg -= 360.0;
    
    /* Step four in book (p. 83) */
    meanAnom = numDeg + eclipLongEpic - eclipLongPer;
    while (meanAnom < 0.0) meanAnom += 360.0;
    while (meanAnom > 360.0) meanAnom -= 360.0;
    
    /* Step five in book (p. 83) */
    eqCenter = (360.0*eccOrbit*sin(meanAnom/57.297))/3.1415;
    
    trueAnom = meanAnom + eqCenter;
    while (trueAnom < 0.0) trueAnom += 360.0;
    while (trueAnom > 360.0) trueAnom -= 360.0;
    
    /* from section 44, p. 87 */
    distance = semiMajAxis*(1-pow(eccOrbit, 2.0));
    distance = distance/(1+(eccOrbit*cos(trueAnom/57.297)));
    
    return (distance);
}


/***********************************************************************
*
* shadowheight calculates the distance directly above any gdlat and glon
*              for a given UT in km at which the earth's shadow terminates.
*              Will be 0.0 on dayside of earth.
* 
* 
* input parameters
*    double ut    - Universal time in seconds since 1950
*    double gdlat - geodetic latitude in degrees
*    double glon  - geodetic longitude in degrees
* 
* returns double - distance directly above any gdlat and glon
*              for a given UT in km at which the earth's shadow terminates.
*              Will be 0.0 on dayside of earth.
*
*    This method uses the results of solarzen and solardist to create a simple
*    cone on a sphere model of the earth's shadow.  Shadow height is defined as
*    the lowest elevation at which any part of the sun can be seen.  No atmospheric
*    bending of light is included. The radius of the earth is calculated at the 
*    tan point of the sun's rays.
*
*    Algorithm:
*
*    Solar Zenith Angle = Z
*    Solar Azimuth      =Az (0 = North, 90 = East)
*
*    Get latitude of tangent point of sun's rays:
*
*      = gdlat + cos(Az)*(Z-90.0)
*
*    Get earthRadius at that point using convrt at gdlat = 0 (sea level)
*
*    ConeHalfAngle = C = atan((sunRadius - earthRadius)/soldist)
*
*
*                               (cos Z tan C + 1 - sin Z)
*    Shadowheight = earthRadius -------------------------
*                                  (sin Z - cos Z tan C)
*
*    Daytime (Shadowheight = 0) if numerator negitive or if
*    Z <= 91.0.
*/
double shadowheight(double ut, double gdlat, double glon)
{
    /* constants in km  */
    const double sunRadius    = 695500.0;
    
    double soldist   = 0.0;
    double szen      = 0.0;
    double saz       = 0.0;
    double coneHAng  = 0.0;
    double numer     = 0.0;
    double denom     = 0.0;
    
    /* used to get earth radius at sun tangent point */
    double dellat     = 0.0; /* change in latitude from meas point to  */
                             /* sun tan point                          */
    double tan_gdlat  = 0.0; /* latitude of sun tangent crossing       */
    double tan_gdalt  = 0.0; /* sea level sun tangent assumed          */
    double gclat      = 0.0; /* don't care about geocentric lat        */
    double rkmp       = 0.0; /* radius of the earth at the sun tangent */
    
    /* get the solar zenith                   */
    /* if less than 91.0, return 0.0 (dayside) */
    solarzen_az(ut, gdlat, glon, &szen, &saz);
    if (szen <= 91.0)
        return (0.0);
        
    /* find the solar tangent lat */
    dellat =cos(saz/57.297)*(szen-90.0);
    tan_gdlat = gdlat + dellat;
    
    if (tan_gdlat > 90.0)
        tan_gdlat = tan_gdlat - 2*(tan_gdlat - 90.0);
    if (tan_gdlat < -90.0)
        tan_gdlat = tan_gdlat - 2*(tan_gdlat + 90.0);
        
    /* find the earth's radius at that point */
    convrt(1, &tan_gdlat, &tan_gdalt, &gclat, &rkmp); 
    
    
    /* get distance to sun at ut */
    soldist = solardist(ut);
    
    /* get cone 1/2 angle */
    coneHAng = atan((sunRadius - rkmp)/soldist);
    
    /* get numerator */
    numer = cos(szen/57.297)*tan(coneHAng/57.297) + 1 - sin(szen/57.297);
    
    if (numer < 0.0) /* sun still visible on ground */
        return (0.0);
        
    denom = sin(szen/57.297) - cos(szen/57.297)*tan(coneHAng/57.297);
        
    return (rkmp*numer/denom);
}


/***********************************************************************
*
* sunrise_set calculates the time UT  of ionospheric sunrise
*             and sunset.
* 
* 
* input parameters
*    double ut    - Universal time in seconds since 1950
*    double gdlat - geodetic latitude in degrees
*    double glon  - longitude in degrees
*    double gdalt - geodetic altitude in km
*    double * sunrise - pointer to double allocated by user to be
*                       set to sunrise time UT 
*    double * sunset - pointer to double allocated by user to be
*                       set to sunset time UT 
* 
* returns void
*
*    If either sunrise or sunset not found, set to missing.
*    All times in seconds since 1/1/1950
*
*  Algorithm:
*
*    Depends on shadowheight calculation, so limitations discussed
*    there apply (atmospheric bending of light ignored).
*
*    If glon < 0:
*      solar midnight = UT - glon*24/360
*      solar noon     = UT + 12 - glon*24/360
*
*    If sun is not set at solar midnight (defined by shadowheight > gdalt),
*    or sun not up at solar noon, check if any difference between 0 and 24 UT.
*    If so, find only one of sunrise and sunset as described below, and set
*    the other to missing.  If not, return missing for both, because that point
*    is either in the sun or in the shadow all day.  The user must determine
*    which by comparing shadowheight and gdalt.  Otherwise, seek sunrise
*    between solar midnight and solar noon, slice remaining time in half each
*    guess.  Stop when time step less than 1 minute.
*    
*            If sun is up at 0 UT that day:  Seek sunset between 0.0 and
*    solar midnight as above.
*            Else: Seek sunset between solar noon and 24.0 as above.
*    
*    Else if glon > 0:
*        solar midnight = UT + 24 - glon*24/360
*        solar noon     = UT + 12 - glon*24/360
*    
*    If sun is not set at solar midnight (defined by shadowheight > gdalt),
*    or sun not up at solar noon, check if any difference between 0 and 24 UT.
*    If so, find only one of sunrise and sunset as described below, and set
*    the other to missing.  If not, return missing for both, because that point
*    is either in the sun or in the shadow all day.  The user must determine
*    which by comparing shadowheight and gdalt.  Otherwise, seek sunset
*    between solar noon and solar midnight, slice remaining time in half each
*    guess.  Stop when time step less than 1 minute.
*    
*            If sun is up at 0 UT that day:  Seek sunrise between solar
*    midnight and 24.0 as above.
*    
*            Else: Seek sunrise between 0.0 and solar noon as above.
*
*   Note: day is divided 11 times to ensure greater than one minute resolution.
*/
void sunrise_set(double ut,
                 double gdlat, 
                 double glon,
                 double gdalt,
                 double * sunrise,
                 double * sunset)
{
    double startUT = 0.0;      /* time UT at UT hour 0  */
    double endUT = 0.0;        /* time UT at UT hour 24 */
    double solmidnight = 0.0;  /* solar midnight */
    double solnoon = 0.0;      /* solar noon */
    double startTime = 0.0;    /* loop start time */
    double endTime = 0.0;      /* loop end time */
    double tempTime = 0.0;     /* loop temp time */
    
    int iyr = 0, imd = 0, ihm = 0, ics = 0;
    int i = 0;    
    
    int upAt0UT = 0;           /* flag to indicate sun up at 0 UT  */
    int upAt24UT = 0;          /* flag to indicate sun up at 24 UT */
    

    /* first find UT at beginning and end of day */
    dinvmadptr(ut, &iyr, &imd, &ihm, &ics);
    
    startUT = dmadptr(iyr, imd, 0, 0);
    endUT = startUT + 24.0*3600.0;
    
    /* make sure glon between -180 and 180 */
    while (glon > 180.0) glon -= 360.0;
    while (glon < -180.0) glon += 360.0;
    
    if (glon < 0.0 )  /* west of 0 degrees lat */
    {
        solmidnight = startUT - glon*24.0*3600.0/360.0;
        solnoon = startUT - glon*24.0*3600.0/360.0 + 12.0*3600.0;
        
        /* check for normal sunrise/sunset */
        if (shadowheight(solmidnight, gdlat, glon) > gdalt &&
            shadowheight(solnoon, gdlat, glon) == 0.0)
        {
            /* sunrise must occur between solmidnight and solnoon */
            startTime = solmidnight;
            endTime = solnoon;
            for (i=0; i<11; i++)
            {
                tempTime = (startTime + endTime)/2.0;
                if (shadowheight(tempTime, gdlat, glon) < gdalt)
                {
                   /* sun has risen at temp time */
                   endTime = tempTime;
                }
                else /* sun hasn't risen yet */
                    startTime = tempTime;
            }
            *sunrise = (startTime + endTime)/2.0;
            
            /* check if sun is up at 0 UT */
            if (shadowheight(startUT, gdlat, glon) < gdalt)
            {
                /* seek sunset between startUT and solmidnight */
                startTime = startUT;
                endTime = solmidnight;
                for (i=0; i<11; i++)
                {
                    tempTime = (startTime + endTime)/2.0;
                    if (shadowheight(tempTime, gdlat, glon) < gdalt)
                    {
                       /* sun still up at temp time */
                       startTime = tempTime;
                    }
                    else /* sun has already set */
                        endTime = tempTime;
                }
                *sunset = (startTime + endTime)/2.0;
            }
            else 
            {
                /* seek sunset between solnoon and endUT */
                startTime = solnoon;
                endTime = endUT;
                for (i=0; i<11; i++)
                {
                    tempTime = (startTime + endTime)/2.0;
                    if (shadowheight(tempTime, gdlat, glon) < gdalt)
                    {
                       /* sun still up at temp time */
                       startTime = tempTime;
                    }
                    else /* sun has already set */
                        endTime = tempTime;
                }
                *sunset = (startTime + endTime)/2.0;
            }
        }
        else /* not normal - see if any transition occured */
        {
            upAt0UT  = shadowheight(startUT, gdlat, glon) < gdalt;
            upAt24UT = shadowheight(endUT, gdlat, glon) < gdalt;
            
            if (upAt0UT == upAt24UT) /* no transition */
            {
                *sunrise = missing;
                *sunset = missing;
            }
            else if (upAt0UT == 0 && upAt24UT == 1)
            {
                /* find sunrise only */
                startTime = startUT;
                endTime = endUT;
                for (i=0; i<11; i++)
                {
                    tempTime = (startTime + endTime)/2.0;
                    if (shadowheight(tempTime, gdlat, glon) < gdalt)
                    {
                       /* sun has risen at temp time */
                       endTime = tempTime;
                    }
                    else /* sun hasn't risen yet */
                        startTime = tempTime;
                }
                *sunrise = (startTime + endTime)/2.0;
                *sunset = missing;
            }
            else /* sunset only */
            {
                startTime = startUT;
                endTime = endUT;
                for (i=0; i<11; i++)
                {
                    tempTime = (startTime + endTime)/2.0;
                    if (shadowheight(tempTime, gdlat, glon) < gdalt)
                    {
                       /* sun still up at temp time */
                       startTime = tempTime;
                    }
                    else /* sun has set */
                        endTime = tempTime;
                }
                *sunset = (startTime + endTime)/2.0;
                *sunrise = missing;
            }
        }
    }
    else  /* west of 0 degrees lat */
    {
        solmidnight = startUT - glon*24.0*3600.0/360.0 + 24.0*3600.0;
        solnoon = startUT - glon*24.0*3600.0/360.0 + 12.0*3600.0;
        
        /* check for normal sunrise/sunset */
        if (shadowheight(solmidnight, gdlat, glon) > gdalt &&
            shadowheight(solnoon, gdlat, glon) == 0.0)
        {
            /* sunset must occur between solnoon and solmidnight */
            startTime = solnoon;
            endTime = solmidnight;
            for (i=0; i<11; i++)
            {
                tempTime = (startTime + endTime)/2.0;
                if (shadowheight(tempTime, gdlat, glon) < gdalt)
                {
                   /* sun has not set yet */
                   startTime = tempTime;
                }
                else /* sun already */
                    endTime = tempTime;
            }
            *sunset = (startTime + endTime)/2.0;
            
            /* check if sun is up at 0 UT */
            if (shadowheight(startUT, gdlat, glon) < gdalt)
            {
                /* seek sunrise between solmidnight and endUT */
                startTime = solmidnight;
                endTime = endUT;
                for (i=0; i<11; i++)
                {
                    tempTime = (startTime + endTime)/2.0;
                    if (shadowheight(tempTime, gdlat, glon) < gdalt)
                    {
                        /* sun already risen */
                        endTime = tempTime;
                    }
                    else /* sun not yet risen */
                        startTime = tempTime;
                }
                *sunrise = (startTime + endTime)/2.0;
            }
            else 
            {
                /* seek sunrise between startUT and solnoon */
                startTime = startUT;
                endTime = solnoon;
                for (i=0; i<11; i++)
                {
                    tempTime = (startTime + endTime)/2.0;
                    if (shadowheight(tempTime, gdlat, glon) < gdalt)
                    {
                        /* sun already risen */
                        endTime = tempTime;
                    }
                    else /* sun not yet risen */
                        startTime = tempTime;
                }
                *sunrise = (startTime + endTime)/2.0;
            }
        }
        else /* not normal - see if any transition occured */
        {
            upAt0UT  = shadowheight(startUT, gdlat, glon) < gdalt;
            upAt24UT = shadowheight(endUT, gdlat, glon) < gdalt;
            
            if (upAt0UT == upAt24UT) /* no transition */
            {
                *sunrise = missing;
                *sunset = missing;
            }
            else if (upAt0UT == 0 && upAt24UT == 1)
            {
                /* find sunrise only */
                startTime = startUT;
                endTime = endUT;
                for (i=0; i<11; i++)
                {
                    tempTime = (startTime + endTime)/2.0;
                    if (shadowheight(tempTime, gdlat, glon) < gdalt)
                    {
                       /* sun has risen at temp time */
                       endTime = tempTime;
                    }
                    else /* sun hasn't risen yet */
                        startTime = tempTime;
                }
                *sunrise = (startTime + endTime)/2.0;
                *sunset = missing;
            }
            else /* sunset only */
            {
                startTime = startUT;
                endTime = endUT;
                for (i=0; i<11; i++)
                {
                    tempTime = (startTime + endTime)/2.0;
                    if (shadowheight(tempTime, gdlat, glon) < gdalt)
                    {
                       /* sun still up at temp time */
                       startTime = tempTime;
                    }
                    else /* sun has set */
                        endTime = tempTime;
                }
                *sunset = (startTime + endTime)/2.0;
                *sunrise = missing;
            }
            
        }/* end check for normal sunrise/sunset */
        
    } /* end if glon < 0 */
        
}



