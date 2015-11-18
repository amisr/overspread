/*
  See madDeriveMethods.h for overview of this module.
  
  $Id: madDeriveMethods.c,v 1.62 2009/04/17 21:25:55 brideout Exp $
  
  Written 11/2002 by B. Rideout
*/


#include <madDeriveMethods.h>
#include <math.h>
#include <date.h>
#include <cedarIO.h>
#include <madrec.h>
#include <cedar.h>
#include <geometry.h>
#include <pthread.h>
#include <assert.h>

#define SEC_IN_DAY 86399.99

#define TS_TRACE_LEN 1000


/* global mutexes to allow only one thread to load the */
/* data from a file for any methods that depends on    */
/* data from a file (i.e., geo500101g.001 in getGeo)   */

pthread_mutex_t geo_mutex  = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t dst_mutex  = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t fof2_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t imf_mutex  = PTHREAD_MUTEX_INITIALIZER;



/* internal data structure used by getGeo - more */
/* compact than madrec                           */
typedef struct _Geo3Hour
{
    Int16 ap;
    Int16 f107;
    Int16 fbar;
    Int16 kp;
    Int16 ap3;
} Geo3Hour;

typedef struct geoDay
{
    double key;
    Geo3Hour geo3hour[8];
} GeoDay;



/* internal data structure used by getDst - more */
/* compact than madrec                           */
typedef struct _DstDay
{
    double key;
    Int16 dstHour[24];
} DstDay;


/* internal data structure used by getFof2 - more */
/* compact than madrec                            */
typedef struct _Fof2Day
{
    double key;
    Int16 fof2Hour[48];
} Fof2Day;

/* internal data structure used by getImf - more */
/* compact than madrec                           */
typedef struct _Imf1Hour
{
    Int16 bxgsm;
    Int16 bygsm;
    Int16 bzgsm;
    Int16 bygse;
    Int16 bzgse;
    Int16 swden;
    Int16 swspd;
    Int16 swq;
} Imf1Hour;


typedef struct imfDay
{
    double key;
    Imf1Hour imf1hour[24];
} ImfDay;




/* helper methods not called directly by madDeriveEngine */

/***********************************************************************
*
* checkErrorData - a helper method that looks at input data for methods
*                  that calculate error parameters to find assumed or
*                  knownbad special values.  If found, all outputs
*                  set to missing and return 1.  If not, return 0.
*
*   arguments: 
*      inCount - num inputs
*      inputArr - double array
*      outCount - num outputs
*      outputArr - double array 
*
*
*   returns - 1 if any input assumed or knownbad, 0 otherwise
*/
int checkErrorData(int inCount, 
                   double * inputArr, 
                   int outCount, 
                   double * outputArr)
{
    int i = 0;
    int isBad = 0;  /* set if assumed or knownbad found */
    
    for (i=0; i<inCount; i++)
    {
         if (inputArr[i] == assumed || inputArr[i] == knownbad)
         {
             isBad = 1;
             break;
         }
    }
    
    if (isBad)
    {
        for (i=0; i<outCount; i++)
            outputArr[i] = missing;
            
        return (1);
    }
    
    return(0);
}


/***********************************************************************
*
* getDebyeFactor - a helper method that finds the debye length g given
*                  Tr and Pfac.
*
*   arguments: 
*      double Tr - temperature ratio Te/Ti
*      double Pfac - a factor determined from the uncorrected electron density
*
*   Returns:
*      double g which is a solution to g(1+Tr+g)(1+g)-Pfac=0
*
*      if fails to converge, returns 0.0
*/
double getDebyeFactor(double Tr, double Pfac)
{
    double a1=0.0, a2=0.0, ddf=0.0, f=0.0, fpr=0.0;
    double neldeb = 0;
    int i = 0;
    
    a1 = 1.0 + Tr;
    a2 = 2.0 * (2.0 + Tr);
    neldeb = Pfac/a1;
    
    for (i=0; i<20; i++)
    {
        f = neldeb*(a1+neldeb)*(1.0+neldeb) - Pfac;
        fpr = neldeb*(3.0*neldeb + a2) + a1;
        ddf = f/fpr;
        neldeb = neldeb - ddf;
        if (ddf < 0.001)
            return (neldeb);
    }
    
    /* failed to converge in 20 steps */
    return (0.0);
}


/***********************************************************************
*
* getElecDensity - a helper method that finds the corrected electron density
*                  given Ti, Tr, the log10 of the uncorrected electron
*                  density Popl in lg(m^-3), and the aspect angle.
*
*   Algorithm from Fortran method NELCAL in madlib, modified with
*   aspect angle dependence using algorithm from F.S. Rodrigues and
*   Dave Hysell.  Rodriques/Hysell used if beam within 6 degrees of
*   perpendicular to magnetic field line (84 < aspect < 96).  Fails
*   if within 1 degree of perpendicular.
*
*   arguments: 
*      double Ti - ion temperature
*      double Tr - temperature ratio Te/Ti
*      double Popl - the uncorrected electron density Popl in lg(m^-3)
*      double aspect - magnetic field line
*
*   Returns:
*      double - the log10 of the corrected electron density in lg(m^-3)
*      if fails, returns missing
*/
double getElecDensity(double Ti, double Tr, double Popl, double aspect)
{
    const double rkfac = 1.61998e+6;
    /* the following constants are used in aspect angle dependence */
    const double p1 = 0.000026;
    const double p2 = -0.009683;
    const double p3 = 0.921712;
    double expon;
    double nnew;
    
    double cf1=0.0, df=0.0, Pfac=0.0, Te=0.0;
    
    if (Ti < 100.0 || Ti > 10000.0)
        return missing;
        
    if (Tr < 0.1 || Tr > 10.0)
        return missing;
        
    if (Popl <= 0.0 || Popl > 25.0)
        return missing;
        
    Te = Tr * Ti;
    
    /* determine whether to use the aspect angle method */
    aspect = fabs(90.0 - fabs(aspect));
    
    if (aspect < 1.0 && Tr > 1.5)
        return(missing);
    
    if (aspect < 6.0 && aspect >= 1.0)
    {
        /* convert to radians */
	aspect = aspect/57.2958;
	
	expon  = p1 * (Tr/aspect) * (Tr/aspect);
	expon += p2 * (Tr/aspect);
	expon += p3;
	nnew = (1.0 + pow(Tr, expon));
	cf1 = 0.5 * nnew * pow(10.0, Popl);
    }
    else /* standard method */
    {
	Pfac = 2.0 * rkfac * Te/pow(10.0,Popl);
	df = getDebyeFactor(Tr, Pfac);

	if (df == 0.0)
            return missing;

	cf1 = 0.5*(1.0 + Tr + df)*(1.0 + df)*pow(10.0, Popl);
    }
    
    if (cf1 > 1.0e25)
        cf1 = 1.0e25;
        
    if (cf1 < 1.0)
        cf1 = 1.0;
        
    return (log10(cf1));
}


/***********************************************************************
*
* getTsyganenkoField - a helper method that finds the XGSM and YGSM point
*     on the equatorial plane for the field line determined by the given
*     point in space and time using the Tsyganenko model.
*
*    Note that the 2001 Tsyganenko model uses IMF and solar wind
*    speed measurements taken every 5 minutes for a hour.  Since
*    Madrigal presently only has hourly measurements, we'll just average
*    two measurements instead of 12.
*
*    Since Tsyganenko uses globals, this code is not thread safe.
*
*
*   arguments: 
*      double time - time in seconds since 1/1/1950
*      double gdlat - geodetic latitude
*      double glon - geodetic longitude
*      double gdalt - geodetic altitude in km
*      double swspd_now - solar wind speed now in m/s
*      double swspd_1hour - solar wind speed 1 hour earlier in m/s
*      double imf_ygsm_now - imf in y gsm direction in nTesla measured now
*      double imf_ygsm_1hour - imf in y gsm direction in nTesla measured 1 hour ago
*      double imf_zgsm_now - imf in z gsm direction in nTesla measured now
*      double imf_zgsm_1hour - imf in z gsm direction in nTesla measured 1 hour ago
*      double swden - solar wind density in m^-3
*      double dst - dst in  nTesla
*           (the following are output parameters)
*      double * eq_xgsm - x point in equatorial plane where field line crosses (in GSM)
*      double * eq_ygsm - y point in equatorial plane where field line crosses (in GSM)
*      double * eq_xgse - x point in equatorial plane where field line crosses (in GSE)
*      double * eq_ygse - y point in equatorial plane where field line crosses (in GSE)
*
*      If failure, all b* parameters will be set to missing
* 
*
*   Returns:
*      double - 0 if success, -1 if failure
*/
int getTsyganenkoField(double time,
                       double gdlat,
		       double glon,
		       double gdalt,
		       double swspd_now,
		       double swspd_1hour,
		       double imf_ygsm_now,
		       double imf_ygsm_1hour,
		       double imf_zgsm_now,
		       double imf_zgsm_1hour,
		       double swden,
		       double dst,
		       double * eq_xgsm,
		       double * eq_ygsm,
		       double * eq_xgse,
		       double * eq_ygse)
{
    int iyr = 0, imd = 0, ihm = 0, ics = 0;
    int month = 0, day = 0, dayOfYear = 0, hour = 0, min = 0, sec = 0;  
    
    /* variables for conversion to geocentric coordinates */
    int imod = 1, i=0;
    double gclat = 0.0, rkm = 0.0;
    double xgeo = 0.0, ygeo = 0.0, zgeo = 0.0, earthRadii=0.0;
    
    /* variables to be passed to Tsyganenko */
    double parmod[10];
    double xgsm = 0.0, ygsm = 0.0, zgsm = 0.0;  /* starting point of field line trace */
    double xf=0.0, yf=0.0, zf=0.0; /* final point of field line trace */
    double xx[TS_TRACE_LEN]; /* x values of complete field line trace (GSM) */
    double yy[TS_TRACE_LEN]; /* y values of complete field line trace (GSM) */
    double zz[TS_TRACE_LEN]; /* z values of complete field line trace (GSM) */
    double xxe[TS_TRACE_LEN]; /* x values of complete field line trace (GSE) */
    double yye[TS_TRACE_LEN]; /* y values of complete field line trace (GSE) */
    double zze[TS_TRACE_LEN]; /* z values of complete field line trace (GSE) */
    double dir = 0.0; /* sets direction of trace */
    double rlim = 60.0; /* sets outer limit in earth radii of trace */
    double ro = 1.0; /* sets terminating point of trace in earth radii */
    int m = 0; /* number of points actually calculated in trace */
    
    /* variables used to weight two border points on the trace */
    double weight0 = 0.0, weight1 = 0.0;
    int sign_zze = 0;
    int foundSignChange = 0;

    
    /* check for missing inputs */
    if (time == missing ||
        gdlat == missing ||
	glon == missing ||
	gdalt == missing ||
	swspd_now == missing ||
	swspd_1hour == missing ||
	imf_ygsm_now == missing ||
	imf_ygsm_1hour == missing ||
	imf_zgsm_now == missing ||
	imf_zgsm_1hour == missing ||
	swden == missing ||
	dst == missing)
    {
        *eq_xgsm = missing;
        *eq_ygsm = missing;
	*eq_xgse = missing;
        *eq_ygse = missing;
        return -1;
    }
    
    /* initialize trace array to missing */
    for (i=0; i<TS_TRACE_LEN; i++)
    {
        xx[i] = missing;
	yy[i] = missing;
	zz[i] = missing;
    }

    
    /* convert time into required form */
    dinvmadptr(time, &iyr, &imd, &ihm, &ics);
    month = imd/100;
    day = imd - month*100;
    hour = ihm/100;
    min = ihm - hour*100;
    sec = ics/100;
    
    /* get day of year as required by Tsyganenko */
    dayOfYear = madGetDayno(iyr, month, day);
    
    /* set Tsyganenko common block data to the right time - not thread safe!!!! */
    TS_RECALC_F77(&iyr, &dayOfYear, &hour, &min, &sec);  
    
    /* set all the required parameter in parmod */
    /* solar wind pressure in nanoPascal = 1E9 * proton mass * swden * swspd^2 */
    parmod[0] = 1E9*PROTON_MASS * swden * pow(swspd_now, 2.0);
    parmod[1] = dst;
    parmod[2] = imf_ygsm_now;
    parmod[3] = imf_zgsm_now;
    parmod[4] = getTsyganenkoG1Index(swspd_now,
                                     swspd_1hour,
			             imf_ygsm_now,
				     imf_ygsm_1hour,
				     imf_zgsm_now,
				     imf_zgsm_1hour);
    parmod[5] = getTsyganenkoG2Index(swspd_now,
                                     swspd_1hour,
				     imf_zgsm_now,
				     imf_zgsm_1hour);			     
  
    /* get geocentric coordinates from geodetic */
    CONVRT_F77(&imod, &gdlat, &gdalt, &gclat, &rkm);
      
    /* convert from geocentric to spherical */
    gclat = (90.0 - gclat)*0.01745329;
    glon = glon*0.01745329;
    
    earthRadii = rkm/6371.2;
    
    /* get cartesian coordinates from spherical */
    TS_SPHCAR_F77(&earthRadii, &gclat, &glon, &xgeo, &ygeo, &zgeo, &imod);
    
    /* convert cartesian into solar magnetospheric ones */
    GEOGSM_F77(&xgeo, &ygeo, &zgeo, &xgsm, &ygsm, &zgsm, &imod);
    
    /* if zgsm is less that 0, we go north. otherwise south */
    if (zgsm < 0.0)
        dir = -1.0;
    else
        dir = 1.0;

    /* call main Tsyganenko method */
    TRACE_F77(&xgsm, 
           &ygsm, 
	   &zgsm,
	   &dir,
	   &rlim,
	   &ro,
	   &imod,
	   parmod,
	   T01_01_F77,
	   IGRF_GSM_F77,
	   &xf,
	   &yf,
	   &zf,
	   xx,
	   yy,
	   zz,
	   &m);
	   
	   
    /* check for failure */
    if (m >= TS_TRACE_LEN-1)
    {
        *eq_xgsm = missing;
        *eq_ygsm = missing;
	*eq_xgse = missing;
        *eq_ygse = missing;
        return -1;
    }
    
    /* first find GSM data, since that's what xx, yy, and zz are in */
	   
    /* loop through results to find the sign change */
    for (i=0; i<m; i++)
    {
	if (dir > 0.0)
	{
	    if (zz[i] < 0.0)
	        break;
	}
	else
	    if (zz[i] >= 0.0)
	        break;
    }
    
    /* check for failure */
    if (i >= TS_TRACE_LEN-2)
    {
        *eq_xgsm = missing;
        *eq_ygsm = missing;
	*eq_xgse = missing;
        *eq_ygse = missing;
        return -1;
    }
    
    if (i < m)
    {
        /* we did cross the GSM XY plane */
        /* set eq_xgsm and eq_ygsm as weighted average of last two border points */
        weight0 = fabs(zz[i-1])/(fabs(zz[i-1]) + fabs(zz[i]));
        weight1 = fabs(zz[i])/(fabs(zz[i-1]) + fabs(zz[i]));
    
        *eq_xgsm = weight0 * xx[i]  + weight1 * xx[i-1];
        *eq_ygsm = weight0 * yy[i]  + weight1 * yy[i-1];
    }
    else
    {
        /* we did NOT cross the GSM XY plane (but we still may cross the GSE plane) */
	*eq_xgsm = missing;
        *eq_ygsm = missing;
    }
    
    
    /* now find GSE data - if we're lucky, the present trace crosses the GSE XY plane */
    /* first convert xx, yy, zz to GSE */
    foundSignChange = 0;
    for (i=0; i<m; i++)
    {
        GSMGSE_F77(xx+i,yy+i,zz+i,xxe+i,yye+i,zze+i,&imod);
	if (i == 0)
	{
	    /* get sign of zze[0] */
	    if (zze[0] < 0.0)
	        sign_zze = -1;
	    else if (zze[0] > 0.0)
	        sign_zze = 1;
	    else
	    {
	        /* direct hit */
		*eq_xgse = xxe[0];
                *eq_ygse = yye[0];
		return 0;
	    }
	    continue;
	}
	
	/* see if we found a sign change */
	if (sign_zze == -1 && zze[i] >= 0.0)
	{
	    foundSignChange = 1;
	    break;
	}
	if (sign_zze == 1 && zze[i] <= 0.0)
	{
	    foundSignChange = 1;
	    break;
	}
    }
    
    if (foundSignChange == 1)
    {
        /* this trace DID cross the GSE XY plane, we don't need to rerun trace */
	/* return eq_xgse and eq_ygse as weighted average of last two border points */
	
        weight0 = fabs(zze[i-1])/(fabs(zze[i-1]) + fabs(zze[i]));
        weight1 = fabs(zze[i])/(fabs(zze[i-1]) + fabs(zze[i]));
    
        *eq_xgse = weight0 * xxe[i]  + weight1 * xxe[i-1];
        *eq_ygse = weight0 * yye[i]  + weight1 * yye[i-1];
	return 0;
    }
    
    /* this trace DID NOT cross the GSE XY plane, we need to rerun trace in the other direction */
    if (dir < 0.0)
        dir = 1.0;
    else
        dir = -1.0;
	
    /* call main Tsyganenko method a second time*/
    TRACE_F77(&xgsm, 
           &ygsm, 
	   &zgsm,
	   &dir,
	   &rlim,
	   &ro,
	   &imod,
	   parmod,
	   T01_01_F77,
	   IGRF_GSM_F77,
	   &xf,
	   &yf,
	   &zf,
	   xx,
	   yy,
	   zz,
	   &m);
	   
    /* check for failure */
    if (m >= TS_TRACE_LEN-1)
    {
        *eq_xgsm = missing;
        *eq_ygsm = missing;
	*eq_xgse = missing;
        *eq_ygse = missing;
        return -1;
    }
    
    /* again search the returned field line for the GSE XY crossing */
    foundSignChange = 0;
    for (i=0; i<m; i++)
    {
        GSMGSE_F77(xx+i,yy+i,zz+i,xxe+i,yye+i,zze+i,&imod);
	if (i == 0)
	{
	    /* get sign of zze[0] */
	    if (zze[0] < 0.0)
	        sign_zze = -1;
	    else if (zze[0] > 0.0)
	        sign_zze = 1;
	    else
	    {
	        /* direct hit */
		*eq_xgse = xxe[0];
                *eq_ygse = yye[0];
		return 0;
	    }
	    continue;
	}
	
	/* see if we found a sign change */
	if (sign_zze == -1 && zze[i] >= 0.0)
	{
	    foundSignChange = 1;
	    break;
	}
	if (sign_zze == 1 && zze[i] <= 0.0)
	{
	    foundSignChange = 1;
	    break;
	}
    }
    
    if (foundSignChange == 1)
    {
        /* this trace DID cross the GSE XY plane */
	/* return eq_xgse and eq_ygse as weighted average of last two border points */
	
        weight0 = fabs(zze[i-1])/(fabs(zze[i-1]) + fabs(zze[i]));
        weight1 = fabs(zze[i])/(fabs(zze[i-1]) + fabs(zze[i]));
    
        *eq_xgse = weight0 * xxe[i]  + weight1 * xxe[i-1];
        *eq_ygse = weight0 * yye[i]  + weight1 * yye[i-1];
	return 0;
    }
    
    /* again, we failed to cross the  GSE XY plane */
    *eq_xgsm = missing;
    *eq_ygsm = missing;
    *eq_xgse = missing;
    *eq_ygse = missing;
    return -1;
}


/***********************************************************************
*
* traceTsyganenkoField - a helper method that finds the point on the
*     magnetic field line determined by  the qualifiers: conjugate, north_alt, 
*     south_alt, apex, or GSM XY plane for the field line determined by the given
*     point in space and time using the Tsyganenko model.
*
*    Note that the 2001 Tsyganenko model uses IMF and solar wind
*    speed measurements taken every 5 minutes for a hour.  Since
*    Madrigal presently only has hourly measurements, we'll just average
*    two measurements instead of 12.
*
*    Since Tsyganenko uses globals, this code is not thread safe.
*
*   arguments: 
*      double time - time in seconds since 1/1/1950
*      double gdlat - input geodetic latitude
*      double glon - input geodetic longitude
*      double gdalt - input geodetic altitude in km
*      double swspd_now - solar wind speed now in m/s
*      double swspd_1hour - solar wind speed 1 hour earlier in m/s
*      double imf_ygsm_now - imf in y gsm direction in nTesla measured now
*      double imf_ygsm_1hour - imf in y gsm direction in nTesla measured 1 hour ago
*      double imf_zgsm_now - imf in z gsm direction in nTesla measured now
*      double imf_zgsm_1hour - imf in z gsm direction in nTesla measured 1 hour ago
*      double swden - solar wind density in m^-3
*      double dst - dst in  nTesla
*      int qualifier - 0 for conjugate, 1 for north_alt, 2 for south_alt, 3 for apex, 4 for GSM XY plane
*      double * stopAlt - altitude to stop trace at, if qualifier is north_alt or south_alt.
*                         If other qualifier, this parameter is ignored
*           (the following are output parameters)
*      double * end_gdlat (if qualifier == 4 (GSM XY plane), this will be XGSM instead)
*      double * end_glon (if qualifier == 4 (GSM XY plane), this will be YGSM  instead)
*      double * end_gdalt (if qualifier == 4 (GSM XY plane), this will be ZGSM = 0 instead)
*
*      If failure, all end* parameters will be set to missing
* 
*
*   Returns:
*      double - 0 if success, -1 if failure
*/
int traceTsyganenkoField(double time,
                         double gdlat,
		         double glon,
		         double gdalt,
		         double swspd_now,
		         double swspd_1hour,
		         double imf_ygsm_now,
		         double imf_ygsm_1hour,
		         double imf_zgsm_now,
		         double imf_zgsm_1hour,
		         double swden,
		         double dst,
			 int    qualifier,
			 double stopAlt,
		         double * end_gdlat,
		         double * end_glon,
		         double * end_gdalt)
{
    int iyr = 0, imd = 0, ihm = 0, ics = 0;
    int month = 0, day = 0, dayOfYear = 0, hour = 0, min = 0, sec = 0;  
    
    /* variables for conversion to geocentric coordinates */
    int imod = 1, i=0;
    double gclat = 0.0, rkm = 0.0;
    double xgeo = 0.0, ygeo = 0.0, zgeo = 0.0, earthRadii=0.0;
    
    /* variables to be passed to Tsyganenko */
    double parmod[10];
    double xgsm = 0.0, ygsm = 0.0, zgsm = 0.0;  /* starting point of field line trace */
    double xf=0.0, yf=0.0, zf=0.0; /* final point of field line trace */
    double xx[TS_TRACE_LEN]; /* x values of complete field line trace (GSM) */
    double yy[TS_TRACE_LEN]; /* y values of complete field line trace (GSM) */
    double zz[TS_TRACE_LEN]; /* z values of complete field line trace (GSM) */
    double xx2[TS_TRACE_LEN]; /* x values of complete field line trace - used in transformations */
    double yy2[TS_TRACE_LEN]; /* y values of complete field line trace - used in transformations */
    double zz2[TS_TRACE_LEN]; /* z values of complete field line trace - used in transformations */
    double dir = 0.0; /* sets direction of trace */
    double rlim = 60.0; /* sets outer limit in earth radii of trace */
    double ro = 1.0; /* sets terminating point of trace in earth radii */
    int m = 0; /* number of points actually calculated in trace */
    double ratio = 0.0; /* used to interpolated between last points */;
    
    /* check qualifier */
    if (qualifier < 0 || qualifier > 4)
    {
        fprintf(stderr, "Illegal qualifier %i passed in to traceTsyganenkoField\n", qualifier);
        *end_gdlat = missing;
        *end_glon = missing;
	*end_gdalt = missing;
        return -1;
    }
    
    /* check for missing inputs */
    if (time == missing ||
        gdlat == missing ||
	glon == missing ||
	gdalt == missing ||
	swspd_now == missing ||
	swspd_1hour == missing ||
	imf_ygsm_now == missing ||
	imf_ygsm_1hour == missing ||
	imf_zgsm_now == missing ||
	imf_zgsm_1hour == missing ||
	swden == missing ||
	dst == missing)
    {
        *end_gdlat = missing;
        *end_glon = missing;
	*end_gdalt = missing;
        return -1;
    }
    
    /* initialize trace array to missing */
    for (i=0; i<TS_TRACE_LEN; i++)
    {
        xx[i] = missing;
	yy[i] = missing;
	zz[i] = missing;
    }

    
    /* convert time into required form */
    dinvmadptr(time, &iyr, &imd, &ihm, &ics);
    month = imd/100;
    day = imd - month*100;
    hour = ihm/100;
    min = ihm - hour*100;
    sec = ics/100;
    
    /* get day of year as required by Tsyganenko */
    dayOfYear = madGetDayno(iyr, month, day);
    
    /* set Tsyganenko common block data to the right time - not thread safe!!!! */
    TS_RECALC_F77(&iyr, &dayOfYear, &hour, &min, &sec);  
    
    /* set all the required parameter in parmod */
    /* solar wind pressure in nanoPascal = 1E9 * proton mass * swden * swspd^2 */
    parmod[0] = 1E9*PROTON_MASS * swden * pow(swspd_now, 2.0);
    parmod[1] = dst;
    parmod[2] = imf_ygsm_now;
    parmod[3] = imf_zgsm_now;
    parmod[4] = getTsyganenkoG1Index(swspd_now,
                                     swspd_1hour,
			             imf_ygsm_now,
				     imf_ygsm_1hour,
				     imf_zgsm_now,
				     imf_zgsm_1hour);
    parmod[5] = getTsyganenkoG2Index(swspd_now,
                                     swspd_1hour,
				     imf_zgsm_now,
				     imf_zgsm_1hour);			     
  
    /* get geocentric coordinates from geodetic */
    CONVRT_F77(&imod, &gdlat, &gdalt, &gclat, &rkm);
      
    /* convert from geocentric to spherical */
    gclat = (90.0 - gclat)*0.01745329;
    glon = glon*0.01745329;
    
    earthRadii = rkm/6371.2;
    
    /* get cartesian coordinates from spherical */
    TS_SPHCAR_F77(&earthRadii, &gclat, &glon, &xgeo, &ygeo, &zgeo, &imod);
    
    /* convert cartesian into solar magnetospheric ones */
    GEOGSM_F77(&xgeo, &ygeo, &zgeo, &xgsm, &ygsm, &zgsm, &imod);

    /* guess that south is the right direction */
    dir = 1.0;

    /* call main Tsyganenko method */
    TRACE_F77(&xgsm, 
           &ygsm, 
	   &zgsm,
	   &dir,
	   &rlim,
	   &ro,
	   &imod,
	   parmod,
	   T01_01_F77,
	   IGRF_GSM_F77,
	   &xf,
	   &yf,
	   &zf,
	   xx,
	   yy,
	   zz,
	   &m);
  
	   
    /* check for failure */
    if (m >= TS_TRACE_LEN-1)
    {
        *end_gdlat = missing;
        *end_glon = missing;
	*end_gdalt = missing;
        return -1;
    }
    
    /* first convert trace to geodetic if qualifier != 4 */
    if (qualifier != 4)
    {
	for (i=0; i<m; i++)
	{
            imod = -1; /* converst gsm to cartesian */
	    GEOGSM_F77(xx2+i, yy2+i, zz2+i, xx+i, yy+i, zz+i, &imod);
	    /* convert cartesian to spherical */
	    TS_SPHCAR_F77(xx+i, yy+i, zz+i, xx2+i, yy2+i, zz2+i, &imod);
	    /* convert spherical to geocentric */
            xx2[i] = xx[i]*6371.2; 
	    yy2[i] = 90.0 - (yy[i]/0.01745329);
	    zz2[i] = zz[i]/0.01745329;
	    imod = 2; /* convert geocentric coordinates to geodetic */
	    zz[i] = zz2[i];
    	    CONVRT_F77(&imod, yy+i, xx+i, yy2+i, xx2+i);
	    /* now xx is gdalt, yy is gdlat, zz is glon */
	}
    }
    
    switch (qualifier)
    {
    	case 0: /* 0 for conjugate */
	
	    /* look for same altitude as start - starting at south */
	    /* skip last point so we don't select our starting point */
	    for (i=m-1; i>1; i--)
	    {
	      if (xx[i] <= gdalt && xx[i-1] >= gdalt)
	      {
	          ratio = (gdalt - xx[i])/(xx[i-1]-xx[i]);
	          *end_gdlat = yy[i]*(1-ratio) + yy[i-1]*ratio;
                  *end_glon = zz[i]*(1-ratio) + zz[i-1]*ratio;
	          *end_gdalt = xx[i]*(1-ratio) + xx[i-1]*ratio;
                  return 0;
	      }
	    }
	    break;
	
	case 1: /* north_alt - since we are going south, this can be the right direction only
	           if we start below the target altitude */
        
	    if (xx[0] > stopAlt)
	        break;
		
	    /* look for stopAlt heading south */
	    for (i=0; i<m-1; i++)
	    {
	      if (xx[i] <= stopAlt && xx[i+1] >= stopAlt)
	      {
	          ratio = (stopAlt - xx[i])/(xx[i+1]-xx[i]);
	          *end_gdlat = yy[i]*(1-ratio) + yy[i+1]*ratio;
                  *end_glon = zz[i]*(1-ratio) + zz[i+1]*ratio;
	          *end_gdalt = xx[i]*(1-ratio) + xx[i+1]*ratio;
                  return 0;
	      }
	    }
	    break;
	    
	case 2: /* south_alt - go north until we hit the target altitude */

		
	    /* look for stopAlt heading north */
	    for (i=m-1; i>0; i--)
	    {
	      if (xx[i] <= stopAlt && xx[i-1] >= stopAlt)
	      {
	          ratio = (stopAlt - xx[i])/(xx[i-1]-xx[i]);
	          *end_gdlat = yy[i]*(1-ratio) + yy[i-1]*ratio;
                  *end_glon = zz[i]*(1-ratio) + zz[i-1]*ratio;
	          *end_gdalt = xx[i]*(1-ratio) + xx[i-1]*ratio;
                  return 0;
	      }
	    }
	    break;
	    
	case 3: /* apex - just find the highest point, but don't return, because we might need
	                  to trace in the other direction to find a higher point */
			  
	    /* look for apex heading south */
	    *end_gdalt = 0.0;
	    for (i=0; i<m; i++)
	    {
	      if (xx[i] > *end_gdalt)
	      {
	          *end_gdlat = yy[i];
                  *end_glon = zz[i];
	          *end_gdalt = xx[i];
	      }
	    }
	    break;
	    
	case 4: /* GSM XY plane - coordinates are still GSM */
			  
	    /* look for change in sign heading south */
	    for (i=0; i<m-1; i++)
	    {
	      if (zz[i] >= 0.0 && zz[i+1] <= 0.0)
	      {
	          ratio = zz[i]/(zz[i+1]-zz[i]);
		  *end_gdlat = xx[i]*(1-ratio) + xx[i+1]*ratio;
                  *end_glon =  yy[i]*(1-ratio) + yy[i+1]*ratio;
	          *end_gdalt = 0.0;
		  return 0;
	      }
	    }
	    break;
    
    }
    
    
    
    /* we didn't return yet, so this time trace north */
    dir = -1.0;
	
    imod = 1;
	
    /* call main Tsyganenko method a second time*/
    TRACE_F77(&xgsm, 
           &ygsm, 
	   &zgsm,
	   &dir,
	   &rlim,
	   &ro,
	   &imod,
	   parmod,
	   T01_01_F77,
	   IGRF_GSM_F77,
	   &xf,
	   &yf,
	   &zf,
	   xx,
	   yy,
	   zz,
	   &m);
	   
    /* check for failure */
    if (m >= TS_TRACE_LEN-1)
    {
        *end_gdlat = missing;
        *end_glon = missing;
	*end_gdalt = missing;
        return -1;
    }
    
    /* again convert trace to geodetic if qualifier != 4 */
    if (qualifier != 4)
    {
	for (i=0; i<m; i++)
	{
            imod = -1; /* converst gsm to cartesian */
	    GEOGSM_F77(xx2+i, yy2+i, zz2+i, xx+i, yy+i, zz+i, &imod);
	    /* convert cartesian to spherical */
	    TS_SPHCAR_F77(xx+i, yy+i, zz+i, xx2+i, yy2+i, zz2+i, &imod);
	    /* convert spherical to geocentric */
            xx2[i] = xx[i]*6371.2; 
	    yy2[i] = 90.0 - (yy[i]/0.01745329);
	    zz2[i] = zz[i]/0.01745329;
	    imod = 2; /* convert geocentric coordinates to geodetic */
	    zz[i] = zz2[i];
    	    CONVRT_F77(&imod, yy+i, xx+i, yy2+i, xx2+i);
	    /* now xx is gdalt, yy is gdlat, zz is glon */
	}
    }
    
    switch (qualifier)
    {
    	case 0: /*0 for conjugate, 1 for north_alt, 2 for south_alt, 3 for apex*/
	
	    /* look for same altitude as start - starting at north */
	    /* skip last point so we don't select our starting point */
	    for (i=m-1; i>1; i--)
	    {
	      if (xx[i] <= gdalt && xx[i-1] >= gdalt)
	      {
	          ratio = (gdalt - xx[i])/(xx[i-1]-xx[i]);
	          *end_gdlat = yy[i]*(1-ratio) + yy[i-1]*ratio;
                  *end_glon = zz[i]*(1-ratio) + zz[i-1]*ratio;
	          *end_gdalt = xx[i]*(1-ratio) + xx[i-1]*ratio;
                  return 0;
	      }
	    }
	    break;
	
	case 1: /* north_alt - go south from end until we hit the target altitude */

		
	    /* look for stopAlt heading south */
	    for (i=m-1; i>0; i--)
	    {
	      if (xx[i] <= stopAlt && xx[i-1] >= stopAlt)
	      {
	          ratio = (stopAlt - xx[i])/(xx[i-1]-xx[i]);
	          *end_gdlat = yy[i]*(1-ratio) + yy[i-1]*ratio;
                  *end_glon = zz[i]*(1-ratio) + zz[i-1]*ratio;
	          *end_gdalt = xx[i]*(1-ratio) + xx[i-1]*ratio;
                  return 0;
	      }
	    }
	    break;
	    
	case 2: /* south_alt - since we are going north, this can be the right direction only
	           if we start below the target altitude */
        
	    if (xx[0] > stopAlt)
	        break;
		
	    /* look for stopAlt heading south */
	    for (i=0; i<m-1; i++)
	    {
	      if (xx[i] <= stopAlt && xx[i+1] >= stopAlt)
	      {
	          ratio = (stopAlt - xx[i])/(xx[i+1]-xx[i]);
	          *end_gdlat = yy[i]*(1-ratio) + yy[i+1]*ratio;
                  *end_glon = zz[i]*(1-ratio) + zz[i+1]*ratio;
	          *end_gdalt = xx[i]*(1-ratio) + xx[i+1]*ratio;
                  return 0;
	      }
	    }
	    break;
	    
	case 3: /* apex - see if we find a higher point than the south trace did */
			  
	    /* look for apex heading north */
	    for (i=0; i<m; i++)
	    {
	      if (xx[i] > *end_gdalt)
	      {
	          *end_gdlat = yy[i];
                  *end_glon = zz[i];
	          *end_gdalt = xx[i];
	      }
	    }
	    return 0;
	    
	case 4: /* GSM XY plane - coordinates are still GSM */
			  
	    /* look for change in sign heading north */
	    for (i=0; i<m-1; i++)
	    {
	      if (zz[i] <= 0.0 && zz[i+1] >= 0.0)
	      {
	          ratio = -1.0 * zz[i]/(zz[i+1]-zz[i]);
		  *end_gdlat = xx[i]*(1-ratio) + xx[i+1]*ratio;
                  *end_glon =  yy[i]*(1-ratio) + yy[i+1]*ratio;
	          *end_gdalt = 0.0;
		  return 0;
	      }
	    }
	    break;
    
    }
    
    /* if we haven't returned yet, we failed */
    
    *end_gdlat = missing;
    *end_glon = missing;
    *end_gdalt = missing;
    return -1;
}



/***********************************************************************
*
* getTsyganenkoG1Index - a helper method that calculates the Tsyganenko G1 Index
*    as defined in ftp://nssdcftp.gsfc.nasa.gov/models/magnetospheric/tsyganenko/
*                        2001paper/Paper2/2001ja000220.pdf.
*
*    Note that the 2001 Tsyganenko G1 index uses IMF and solar wind
*    speed measurements taken every 5 minutes for a hour.  Since
*    Madrigal presently only has hourly measurements, we'll just average
*    two measurements instead of 12.
*
*
*   arguments: 
*      double swspd_now - solar wind speed now in m/s
*      double swspd_1hour - solar wind speed 1 hour earlier in m/s
*      double imf_ygsm_now - imf in y gsm direction in nTesla measured now
*      double imf_ygsm_1hour - imf in y gsm direction in nTesla measured 1 hour ago
*      double imf_zgsm_now - imf in z gsm direction in nTesla measured now
*      double imf_zgsm_1hour - imf in z gsm direction in nTesla measured 1 hour ago
* 
*
*   Returns:
*      double - G1 index.  If problem, returns missing.
*/
double getTsyganenkoG1Index(double swspd_now,
                            double swspd_1hour,
			    double imf_ygsm_now,
			    double imf_ygsm_1hour,
			    double imf_zgsm_now,
			    double imf_zgsm_1hour)
{
    double clockAngleNow = 0.0, clockAngle1hour = 0.0;
    double bPerpNow = 0.0, bPerp1hour = 0.0;
    double hBNow = 0.0, hB1hour = 0.0;
    double sinTermNow = 0.0, sinTerm1hour = 0.0;
    
    double g1index = 0.0;
    
    /* return missing if any input missing */
    if (swspd_now == missing ||
        swspd_1hour == missing ||
	imf_ygsm_now == missing ||
	imf_ygsm_1hour == missing ||
	imf_zgsm_now == missing ||
	imf_zgsm_1hour == missing)
	return missing;
    
    /* return missing if both y and z components = 0 */
    if ((imf_ygsm_now == 0.0 && imf_zgsm_now == 0.0) ||
        (imf_ygsm_1hour == 0.0 && imf_zgsm_1hour == 0.0))
	return missing;
	
    /* get B perp terms */
    bPerpNow = sqrt(pow(imf_zgsm_now,2.0) + pow(imf_ygsm_now,2.0));
    bPerp1hour = sqrt(pow(imf_zgsm_1hour,2.0) + pow(imf_ygsm_1hour,2.0));
	
    /* get clock angle, which is arc cos(bz / bperp)  * sign(by)*/
    
    clockAngleNow = acos(imf_zgsm_now / bPerpNow);
    if (imf_ygsm_now > 0.0)
        clockAngleNow = -1.0 * clockAngleNow;
	
    clockAngle1hour = acos(imf_zgsm_1hour / bPerp1hour);
    if (imf_ygsm_1hour > 0.0)
        clockAngle1hour = -1.0 * clockAngle1hour;
	
    /* get hB terms */
    hBNow = pow((bPerpNow / 40.0), 2.0) / (1.0 + (bPerpNow / 40.0));
    hB1hour = pow((bPerp1hour / 40.0), 2.0) / (1.0 + (bPerp1hour / 40.0));
    
    /* get sin terms */
    sinTermNow = pow(sin(clockAngleNow/2.0), 3.0);
    sinTerm1hour = pow(sin(clockAngle1hour/2.0), 3.0);
    
    /* convert speeds to kilometers per second as required by paper */
    swspd_now = 1.0E-3 * swspd_now;
    swspd_1hour = 1.0E-3 * swspd_1hour;
    
    /* finally , get g1 index */
    g1index = swspd_now * hBNow * sinTermNow;
    g1index += swspd_1hour * hB1hour * sinTerm1hour;
    
    g1index = g1index / 2.0;
    
    return g1index;
}


/***********************************************************************
*
* getTsyganenkoG2Index - a helper method that calculates the Tsyganenko G2 Index
*    as defined in ftp://nssdcftp.gsfc.nasa.gov/models/magnetospheric/tsyganenko/
*                        2001paper/Paper2/2001ja000220.pdf.
*
*    Note that the 2001 Tsyganenko G2 index uses IMF and solar wind
*    speed measurements taken every 5 minutes for a hour.  Since
*    Madrigal presently only has hourly measurements, we'll just average
*    two measurements instead of 12.
*
*
*   arguments: 
*      double swspd_now - solar wind speed now in m/s
*      double swspd_1hour - solar wind speed 1 hour earlier in m/s
*      double imf_zgsm_now - imf in z gsm direction in nTesla measured now
*      double imf_zgsm_1hour - imf in z gsm direction in nTesla measured 1 hour ago
* 
*
*   Returns:
*      double - G1 index.  If problem, returns missing.
*/
double getTsyganenkoG2Index(double swspd_now,
                            double swspd_1hour,
			    double imf_zgsm_now,
			    double imf_zgsm_1hour)
{
    const double a = 0.005; /* constant from Tsyganenko paper */
    double g2index = 0.0;
    
    /* return missing if any input missing */
    if (swspd_now == missing ||
        swspd_1hour == missing ||
	imf_zgsm_now == missing ||
	imf_zgsm_1hour == missing)
	return missing;
	
    /* convert speeds to kilometers per second as required by paper */
    swspd_now = 1.0E-3 * swspd_now;
    swspd_1hour = 1.0E-3 * swspd_1hour;
	
    if (imf_zgsm_now < 0.0)
        g2index += -1 * imf_zgsm_now * swspd_now;
	
    if (imf_zgsm_1hour < 0.0)
        g2index += -1 * imf_zgsm_1hour * swspd_1hour;
    
    return a*g2index/2.0;
}

/***********************************************************************
*
* traceMagneticField - a public method used to support the Magnetic field
*                      line trace web service.
*
*
*
*   arguments: 
*      int year
*      int month
*      int day
*      int hour
*      int min
*      int sec
*      double gdlat - geodetic latitude of starting point
*      double glon - longitude of starting point
*      double gdalt - geodetic altitude of starting point
*      int model - 0 for Tsyganenko, 1 for IGRF
*      int qualifier - 0 for conjugate, 1 for north_alt, 2 for south_alt, 3 for apex, 4 for GSM XY plane
*            If qualifier == 4, model must be Tsyganenko.
*      double * stopAlt - altitude to stop trace at, if qualifier is north_alt or south_alt.
*                         If other qualifier, this parameter is ignored
*           (the following are output parameters)
*      double * end_gdlat (if qualifier == 4 (GSM XY plane), this will be XGSM instead)
*      double * end_glon (if qualifier == 4 (GSM XY plane), this will be YGSM  instead)
*      double * end_gdalt (if qualifier == 4 (GSM XY plane), this will be ZGSM = 0 instead)     
* 
*
*   Returns:
*      double - 0 if success, -1 if failure
*/
int traceMagneticField(int year,
                       int month,
		       int day,
		       int hour,
		       int min,
		       int sec,
                       double gdlat,
		       double glon,
		       double gdalt,
		       int model,
		       int qualifier,
		       double stopAlt,
		       double * end_gdlat,
		       double * end_glon,
		       double * end_gdalt)
{
    double mid_time = 0.0;
    int result = 0;
    double fyear = 0.0;
    
    /* environment parameters to look up */
    double swspd_now = 0.0;
    double swspd_1hour = 0.0;
    double imf_ygsm_now = 0.0;
    double imf_ygsm_1hour = 0.0;
    double imf_zgsm_now = 0.0;
    double imf_zgsm_1hour = 0.0;
    double swden = 0.0;
    double dst = 0.0;
    double imfArray[10];
    double timeArray[2];
    
    /* geocentric data */
    double gclat = 0.0;
    double rkm = 0.0;
    double plat = 0.0; /* end point geocentric latitude (deg) */
    double plon = 0.0; /* end point geocentric longitude (deg) */
    double prkm = 0.0; /* end point radial distance (km) */
    double arc = 0.0; /* arc length of field line traced (km) */
    double arad = 0.0; /* apex radius of field line (earth radii) */
    double alat = 0.0; /* apex latitude of field line (deg) */
    double alon = 0.0; /* apex longitude of field line (deg) */

    
    /* conversion direction */
    int imod = 0;
    int ier = 0; /* error indicator */
    int npr = 0; /* used by lintra */
    int init = 2; /* used by lintra */
    int istop = 0; /* sets direction of lintra trace */
    
    /* first handle Tsyganenko model */
    if (model == 0)
    {
    
	/* init imfArray to keep Purify happy */
	int i = 0;
	for (i=0; i<10; i++)
            imfArray[i] = 0.0;

	/* convert time to seconds since 1/1/1950 */
	mid_time = getKey(year, month, day, hour, min, sec);

	/* force glon to between -180 and 180 */
	while (glon < -180.0) glon += 360.0;
	while (glon > +180.0) glon -= 360.0;

	/* get imf data from an hour ago */
	timeArray[0] = mid_time - (3600.0);
	timeArray[1] = mid_time - (3600.0);

	getImf(2, timeArray, 10, imfArray, stderr);

	/* set values from imfArray (convert imf to nT) */
	if (imfArray[1] != missing &&
            imfArray[2] != missing &&
	    imfArray[8] != missing)
	{
            imf_ygsm_1hour = imfArray[1] * 1.0E9;
            imf_zgsm_1hour = imfArray[2] * 1.0E9;
            swspd_1hour = imfArray[8];
	}
	else
	{
            /* missing data */
	    *end_gdlat = missing;
	    *end_glon = missing;
	    *end_gdalt = missing;
	    return(-1);
	}

	/* get imf data from now */
	timeArray[0] = mid_time;
	timeArray[1] = mid_time;


	getImf(2, timeArray, 10, imfArray, stderr);

	/* set values from imfArray */
	if (imfArray[1] != missing &&
            imfArray[2] != missing &&
	    imfArray[8] != missing &&
	    imfArray[7] != missing)
	{
            imf_ygsm_now = imfArray[1] * 1.0E9;
            imf_zgsm_now = imfArray[2] * 1.0E9;
            swspd_now = imfArray[8];
            swden = imfArray[7];
	}
	else
	{
            /* missing data */
	    *end_gdlat = missing;
	    *end_glon = missing;
	    *end_gdalt = missing;
	    return(-1);
	}

	/* get dst data */
	getDst(2, timeArray, 1, &dst, stderr);
	if (dst == missing)
	{
            /* missing data */
	    *end_gdlat = missing;
	    *end_glon = missing;
	    *end_gdalt = missing;
	    return(-1);
	}
	
        result = traceTsyganenkoField(mid_time,
	                     gdlat,
			     glon,
			     gdalt,
			     swspd_now,
			     swspd_1hour,
			     imf_ygsm_now,
			     imf_ygsm_1hour,
			     imf_zgsm_now,
			     imf_zgsm_1hour,
			     swden,
			     dst,
			     qualifier,
			     stopAlt,
			     end_gdlat,
			     end_glon,
			     end_gdalt);

	return (result);
    }
    
    /* use igrf model */
    else if (model == 1)
    {
        if (qualifier == 4)
	{
	    /* can't do this with existing lintra code */
	    *end_gdlat = missing;
	    *end_glon = missing;
	    *end_gdalt = missing;
	    return(-1);
	}
        imod = 1; /* convert geodetic to geocentric */
        CONVRT_F77(&imod, &gdlat, &gdalt, &gclat, &rkm);
	fyear = (double)year;
	
	
	switch (qualifier)
	{
	    case 0: /* conjugate - try the opposite hemisphere first, but its possible
	               we'll need to try this hemisphere */
		       
	    istop = 1;
	
            LINTRA_F77(&fyear,
	            &gclat,
		    &glon,
		    &rkm,
		    &gdalt,
		    &gdalt,
		    &plat,
		    &plon,
		    &prkm,
		    &arc,
		    &arad,
		    &alat,
		    &alon,
		    &istop,
		    &npr,
		    &init,
		    &ier);
		    
	    /* convert back to geodetic */
	    imod = 2;
	    CONVRT_F77(&imod, end_gdlat, end_gdalt, &plat, &prkm);
	    
	    /* check for success - more than 50 km traced, and nearly the same altitude returned */
	    if (arc > 50.0)
	    {
	        if (fabs(*end_gdalt - gdalt) < 50.0)
		{
		    *end_glon = plon;
		    return (0);
		}
	    }
	    
	    /* opposite hemisphere failed - try the same hemisphere */
	    istop = -1;
	
            LINTRA_F77(&fyear,
	            &gclat,
		    &glon,
		    &rkm,
		    &gdalt,
		    &gdalt,
		    &plat,
		    &plon,
		    &prkm,
		    &arc,
		    &arad,
		    &alat,
		    &alon,
		    &istop,
		    &npr,
		    &init,
		    &ier);
		    
	    /* convert back to geodetic */
	    imod = 2;
	    CONVRT_F77(&imod, end_gdlat, end_gdalt, &plat, &prkm);
	    
	    /* check for success - more than 50 km traced, and nearly the same altitude returned */
	    if (arc > 50.0)
	    {
	        if (fabs(*end_gdalt - gdalt) < 50.0)
		{
		    *end_glon = plon;
		    return (0);
		}
	    }
	    
	    /* both hemispheres failed */
	    *end_gdlat = missing;
	    *end_glon = missing;
	    *end_gdalt = missing;
            return (-1);
	    
	case 1: /* north_alt  - randomly try opposite hemisphere first (50/50 guess)*/
	
	    istop = 1;
	
            LINTRA_F77(&fyear,
	            &gclat,
		    &glon,
		    &rkm,
		    &gdalt,
		    &stopAlt,
		    &plat,
		    &plon,
		    &prkm,
		    &arc,
		    &arad,
		    &alat,
		    &alon,
		    &istop,
		    &npr,
		    &init,
		    &ier);
		    
	    /* convert back to geodetic */
	    imod = 2;
	    CONVRT_F77(&imod, end_gdlat, end_gdalt, &plat, &prkm);
	    
	    /* check for success - more than 50 km traced, and north of starting point */
	    if (arc > 50.0)
	    {
	        if (*end_gdlat - gdlat > 0.1)
		{
		    *end_glon = plon;
		    return (0);
		}
		/* we may have started below the intercept */
		if (gdalt < stopAlt && *end_gdlat - gdlat < -0.1)
		{
		    *end_glon = plon;
		    return (0);
		}
	    }
	    
	    /* opposite hemisphere failed - try the same hemisphere */
	    istop = -1;
	
            LINTRA_F77(&fyear,
	            &gclat,
		    &glon,
		    &rkm,
		    &gdalt,
		    &stopAlt,
		    &plat,
		    &plon,
		    &prkm,
		    &arc,
		    &arad,
		    &alat,
		    &alon,
		    &istop,
		    &npr,
		    &init,
		    &ier);
		    
	    /* convert back to geodetic */
	    imod = 2;
	    CONVRT_F77(&imod, end_gdlat, end_gdalt, &plat, &prkm);
	    
	    /* check for success - more than 50 km traced, and north of starting point */
	    if (arc > 50.0)
	    {
	        if (*end_gdlat - gdlat > 0.1)
		{
		    *end_glon = plon;
		    return (0);
		}
		/* we may have started below the intercept */
		if (gdalt < stopAlt && *end_gdlat - gdlat < -0.1)
		{
		    *end_glon = plon;
		    return (0);
		}
	    }
	    
	    /* both hemispheres failed */
	    *end_gdlat = missing;
	    *end_glon = missing;
	    *end_gdalt = missing;
            return (-1);
	    
	case 2: /* south_alt  - randomly try opposite hemisphere first (50/50 guess)*/
	
	    istop = 1;
	
            LINTRA_F77(&fyear,
	            &gclat,
		    &glon,
		    &rkm,
		    &gdalt,
		    &stopAlt,
		    &plat,
		    &plon,
		    &prkm,
		    &arc,
		    &arad,
		    &alat,
		    &alon,
		    &istop,
		    &npr,
		    &init,
		    &ier);
		    
	    /* convert back to geodetic */
	    imod = 2;
	    CONVRT_F77(&imod, end_gdlat, end_gdalt, &plat, &prkm);
	    
	    /* check for success - more than 50 km traced, and south of starting point */
	    if (arc > 50.0)
	    {
	        if (*end_gdlat - gdlat < -0.1)
		{
		    *end_glon = plon;
		    return (0);
		}
		/* we may have started below the intercept */
		if (gdalt < stopAlt && *end_gdlat - gdlat > 0.1)
		{
		    *end_glon = plon;
		    return (0);
		}
	    }
	    
	    /* opposite hemisphere failed - try the same hemisphere */
	    istop = -1;
	
            LINTRA_F77(&fyear,
	            &gclat,
		    &glon,
		    &rkm,
		    &gdalt,
		    &stopAlt,
		    &plat,
		    &plon,
		    &prkm,
		    &arc,
		    &arad,
		    &alat,
		    &alon,
		    &istop,
		    &npr,
		    &init,
		    &ier);
		    
	    /* convert back to geodetic */
	    imod = 2;
	    CONVRT_F77(&imod, end_gdlat, end_gdalt, &plat, &prkm);
	    
	    /* check for success - more than 50 km traced, and south of starting point */
	    if (arc > 50.0)
	    {
	        if (*end_gdlat - gdlat < -0.1)
		{
		    *end_glon = plon;
		    return (0);
		}
		/* we may have started below the intercept */
		if (gdalt < stopAlt && *end_gdlat - gdlat > 0.1)
		{
		    *end_glon = plon;
		    return (0);
		}
	    }
	    
	    /* both hemispheres failed */
	    *end_gdlat = missing;
	    *end_glon = missing;
	    *end_gdalt = missing;
            return (-1);
	    
    case 3: /* apex - set istop = 0 */
		       
	    istop = 0;
	
            LINTRA_F77(&fyear,
	            &gclat,
		    &glon,
		    &rkm,
		    &gdalt,
		    &gdalt,
		    &plat,
		    &plon,
		    &prkm,
		    &arc,
		    &arad,
		    &alat,
		    &alon,
		    &istop,
		    &npr,
		    &init,
		    &ier);
		    
	    /* convert apex to geodetic */
	    imod = 2;
	    CONVRT_F77(&imod, end_gdlat, end_gdalt, &plat, &prkm);
	    *end_glon = alon;
	    return (0);
	    
	}

    }
    
    /* unknown model */
    else
    {
        fprintf(stderr, "in traceMagneticField, unknown model %i\n", model);
        *end_gdlat = missing;
	*end_glon = missing;
	*end_gdalt = missing;
        return (-1);
    }
    
    /* can't really get here, but compiler thinks we can */
    *end_gdlat = missing;
    *end_glon = missing;
    *end_gdalt = missing;
    return (-1);

}


/***********************************************************************
*
* run_iri - a public method used to simplify calling the iri model
*
*
*
*   input arguments: *      int year
*      int month
*      int day
*      int hour
*      int min
*      int sec
*      double gdlat - geodetic latitude of starting point
*      double glon - longitude of starting point
*      double gdalt - geodetic altitude of starting point
*      double * iri - array of 11 doubles containing returned data -                      
*                   allocated by user.  Contains :            
*            NE_IRI     Electron density in #/meter3.    units: m-3
*            NEL_IRI    Log of electron density in #/meter3.   units:log10(m-3)
*            TN_IRI  IRI Neutral temperature
*            TI_IRI   IRI Ion temperature
*            TE_IRI   IRI Electron temperature
*            PO+_IRI   IRI Composition - [O+]/Ne
*            PNO+_IRI   IRI Composition - [NO+]/Ne
*            PO2+_IRI   IRI Composition - [O2+]/Ne
*            PHE+_IRI   IRI Composition - [HE+]/Ne
*            PH+_IRI   IRI Composition - [H+]/Ne
*            PN+_IRI   IRI Composition - [N+]/Ne  
*   Returns:
*      double - 0 if success, -1 if failure
*
*   Calls IRI method iri_sub, for only one altitude.  Updated for IRI 2007 release.
*/
int run_iri(int year,
            int month,
            int day,
            int hour,
            int min,
            int sec,
            double gdlat,
            double glon,
            double gdalt,
            double * iri)
{
    /* IRI Flags */
    unsigned long JF[30] = {1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,0,1,1,1,1,0,0,0};
    int           JMAG   = 0;     /* use geographic as opposed to magnetic coords */
    int           MMDD   = 0;     /* MMDD */
    double        ut     = 0.0;   /* ut - double seconds since 1/1/1950 */
    double        thisUt = 0;     /* loop ut */
    float         DHOUR = 0.0;
    float         OUTF[100][20];  /* 2d iri output */
    float         OARR[50];       /* 1d iri output */
    float         HEIBEG = 0.0;   /* beginning height */
    float         HEIEND = 0.0;   /* ending height */
    float         HEISTP = 100.0; /* step height */
    float         ALATI = 0.0;
    float         ALONG = 0.0;

    int i = 0;
    int outputCount = 11;         /* size of returned array */
    int IAP3[13];                 /* an array of 13 contiguous ap3 values, with the last being the present one */
    float f107 = 0.0;            /* f10.7 value in 10^22 * W/m2/Hz to be passed to iri_sub */
    
    /* declaration of data to be passed into getGeo */
    double inArr[2];
    double outArr[5];
    
    /* initialize only needs to be called once, so use a static variable to keep track */
    static int isInitialized = 0;
    
    if (!isInitialized)
    {
        isInitialized = 1;
	initialize_();
    }

    /* apply IRI altitude rule */
    if (gdalt > 1000.0 || gdalt < 80.0)    
    {
        for (i=0; i<outputCount; i++)
            iri[i] = missing;
        return(0);
    }
      
      
    /* convert time */
    MMDD = month*100 + day;
    /* adding 25 to time indicates UT to IRI */
    DHOUR = 25.0 + (float)(hour) + (float)(min)/60.0 + (float)(sec)/3600.0;
    /* convert spatial parms */
    ALATI = (float)gdlat;
    ALONG = (float)glon;
    if (ALONG < 0.0)
        ALONG += 360.0;
    HEIBEG = (float)gdalt;
    HEIEND = HEIBEG + 1.0;
    
    /* load ap3 data into IAP3  - first find starting ut */
    ut = dmadptr(year, MMDD, hour*100 + min, sec*100);
    /* we need to start 12*3 hours earlier, and take 12 3 hour steps */
    for (i=0; i<13; i++)
    {
        thisUt = ut - (12-i)*(3600*3);
	inArr[0] = thisUt;
	inArr[1] = thisUt;
        getGeo(2, inArr, 5, outArr, stderr);
	IAP3[i] = (int)outArr[1];
	if (i==0)
	{
	    if (outArr[3] != missing)
	        f107 = (float)outArr[3]*10E22;
	    else
	        f107 = 0.0;
	}
    }
    
    IRI_SUB_F77(JF,
             &JMAG,
             &ALATI,
             &ALONG,
             &year,
             &MMDD,
             &DHOUR,
             &HEIBEG,
             &HEIEND,
             &HEISTP,
             OUTF,
             OARR,
	     IAP3,
	     &f107);
	     
    /* write output to iri array */
    iri[0] = (double)OUTF[0][0];     /* ne m-3 */
    iri[1] = (double)log10(iri[0]);  /* log10(ne m-3) */
    iri[2] = (double)OUTF[0][1];     /* tn (K) */
    iri[3] = (double)OUTF[0][2];     /* ti (K) */
    iri[4] = (double)OUTF[0][3];     /* te (K) */
    iri[5] = (double)OUTF[0][4];     /* IRI Composition - [O+]/Ne */
    iri[6] = (double)OUTF[0][8];     /* IRI Composition - [NO+]/Ne */
    iri[7] = (double)OUTF[0][7];     /* IRI Composition - [O2+]/Ne */
    iri[8] = (double)OUTF[0][6];     /* IRI Composition - [HE+]/Ne */
    iri[9] = (double)OUTF[0][5];     /* IRI Composition - [H+]/Ne */
    iri[10] = (double)OUTF[0][10];   /* IRI Composition - [N+]/Ne */
    
    return(0);
      
}




/***********************************************************************
*
* getByear   derives Byear from IBYR
*
*   arguments: 
*      inCount (num inputs) = 1 (IBYR)
*      inputArr - double array holding:
*                 IBYR - year of beginning of record
*      outCount (num outputs) = 1 (BYEAR)
*      outputArr - double array holding:
*                 BYEAR - year of beginning of record
*
*   Algorithm: BYEAR =  IBYR directly from prolog
*
*   returns - 0 (successful)
*/
int getByear(int inCount, 
             double * inputArr, 
             int outCount, 
             double * outputArr, 
             FILE * errFile)
{
    outputArr[0] = inputArr[0];
    return(0);
}


/***********************************************************************
*
* getTime   derives basic time parameters from all prolog time information.
*
*    All outputs set at average time between beginning 
*    and end of record.
*
*   arguments: 
*      inCount (num inputs) = 8 (IBYR, IBDT, IBHM, IBCS, IEYR, IEDT, IEHM, IECS)
*      inputArr - double array holding inputs from above
*      outCount (num outputs) = 7 (YEAR, MONTH, DAY, HOUR, MIN, SEC, CSEC)
*      outputArr - double array holding: 
*                  YEAR - year at avg time in record
*                  MONTH - month at avg time in record
*                  DAY - day at avg time in record
*                  HOUR - hour at avg time in record
*                  MIN - min at avg time in record
*                  SEC - sec at avg time in record
*                  CSEC - centisec at avg time in record
*
*   Algorithm: Determine average time in record, determine time
*
*   returns - 0 (successful)
*/
int getTime(int inCount, 
            double * inputArr, 
            int outCount, 
            double * outputArr, 
            FILE * errFile)
{
    int IBYR, IBDT, IBHM, IBCS, IEYR, IEDT, IEHM, IECS;
    int iyr = 0, imd = 0, ihm = 0, ics = 0;
    double beg_time = 0.0;
    double end_time = 0.0;
    double mid_time = 0.0;
    int month = 0;
    int day = 0;
    int hour = 0;
    int min = 0;
    int sec = 0;
    int csec = 0;
    
    IBYR = (int)inputArr[0];
    IBDT = (int)inputArr[1];
    IBHM = (int)inputArr[2];
    IBCS = (int)inputArr[3];
    IEYR = (int)inputArr[4];
    IEDT = (int)inputArr[5];
    IEHM = (int)inputArr[6];
    IECS = (int)inputArr[7];
    
    beg_time = dmadptr(IBYR, IBDT, IBHM, IBCS);
    end_time = dmadptr(IEYR, IEDT, IEHM, IECS);
    
    mid_time = (beg_time + end_time)/2.0;
    
    dinvmadptr(mid_time, &iyr, &imd, &ihm, &ics);
    
    month = imd/100;
    day = imd - month*100;
    hour = ihm/100;
    min = ihm - hour*100;
    sec = ics/100;
    csec = ics - sec*100;
    
    outputArr[0] = (double)iyr;
    outputArr[1] = (double)month;
    outputArr[2] = (double)day;
    outputArr[3] = (double)hour;
    outputArr[4] = (double)min;
    outputArr[5] = (double)sec;
    outputArr[6] = (double)csec;

    return(0);
}


/***********************************************************************
*
* getBmd   derives Bmd from IBDT
*
*   arguments: 
*      inCount (num inputs) = 1 (IBDT)
*      inputArr - double array holding IBDT - mmdd of beginning of record
*      outCount (num outputs) = 1 (BMD)
*      outputArr - double array holding BMD - mmdd of beginning of record
*
*   Algorithm: BMD =  IBDT directly from prolog
*
*   returns - 0 (successful)
*/
int getBmd(int inCount, 
           double * inputArr, 
           int outCount, 
           double * outputArr, 
           FILE * errFile)
{
    outputArr[0] = inputArr[0];
    return(0);
}



/***********************************************************************
*
* getBMonthDay   derives BMONTH and BDAY from IBDT
*
*   arguments: 
*      inCount (num inputs) = 1 (IBDT)
*      inputArr - double array holding IBDT - mmdd of beginning of record
*      outCount (num outputs) = 2 (BMONTH and BDAY)
*      outputArr - double array holding BMONTH and BDAY - month and day of beginning of record
*
*   Algorithm: BMonth =  IBDT directly from prolog / 100
*              BDay = IBDT - 100*BMonth
*
*   returns - 0 (successful)
*/
int getBMonthDay(int inCount, 
                 double * inputArr, 
                 int outCount, 
                 double * outputArr, 
                 FILE * errFile)
{   
    int bmonth=0, bday=0;
    
    bmonth = (int)inputArr[0]/100;
    bday = (int)inputArr[0] - bmonth*100;
    
    outputArr[0] = (double)bmonth;
    outputArr[1] = (double)bday;
    
    return(0);
}



/***********************************************************************
*
* getMd   derives mmdd from all prolog time information.
*
*    Mmdd is set as mmdd at average time between beginning 
*    and end of record.
*
*   arguments: 
*      inCount (num inputs) = 8 (IBYR, IBDT, IBHM, IBCS, IEYR, IEDT, IEHM, IECS)
*      inputArr - double array holding inputs from above
*      outCount (num outputs) = 1 (MD)
*      outputArr - double array holding: MD - mmdd at avg time in record
*
*   Algorithm: Determine average time in record, determine its mmdd
*
*   returns - 0 (successful)
*/
int getMd(int inCount, 
          double * inputArr, 
          int outCount, 
          double * outputArr, 
          FILE * errFile)
{
    int IBYR, IBDT, IBHM, IBCS, IEYR, IEDT, IEHM, IECS;
    int iyr = 0, imd = 0, ihm = 0, ics = 0;
    double beg_time = 0.0;
    double end_time = 0.0;
    double mid_time = 0.0;
    
    IBYR = (int)inputArr[0];
    IBDT = (int)inputArr[1];
    IBHM = (int)inputArr[2];
    IBCS = (int)inputArr[3];
    IEYR = (int)inputArr[4];
    IEDT = (int)inputArr[5];
    IEHM = (int)inputArr[6];
    IECS = (int)inputArr[7];
    
    beg_time = dmadptr(IBYR, IBDT, IBHM, IBCS);
    end_time = dmadptr(IEYR, IEDT, IEHM, IECS);
    
    mid_time = (beg_time + end_time)/2.0;
    
    dinvmadptr(mid_time, &iyr, &imd, &ihm, &ics);
    
    outputArr[0] = (double)imd;

    return(0);
}


/***********************************************************************
*
* getDayno   derives day number from all prolog time information.
*
*    dayno is set as the day number (1-366) at average time between beginning 
*    and end of record.
*
*   arguments: 
*      inCount (num inputs) = 8 (IBYR, IBDT, IBHM, IBCS, IEYR, IEDT, IEHM, IECS)
*      inputArr - double array holding inputs from above
*      outCount (num outputs) = 1 (DAYNO)
*      outputArr - double array holding: DAYNO - day number at avg time in record
*
*   Algorithm: Determine average time in record, determine its day number
*
*   returns - 0 (successful), -1 if error
*/
int getDayno(int inCount, 
             double * inputArr, 
             int outCount, 
             double * outputArr, 
             FILE * errFile)
{
    int IBYR, IBDT, IBHM, IBCS, IEYR, IEDT, IEHM, IECS;
    int iyr = 0, imd = 0, ihm = 0, ics = 0;
    double beg_time = 0.0;
    double end_time = 0.0;
    double mid_time = 0.0;
    int month = 0;
    int day = 0;
    
    IBYR = (int)inputArr[0];
    IBDT = (int)inputArr[1];
    IBHM = (int)inputArr[2];
    IBCS = (int)inputArr[3];
    IEYR = (int)inputArr[4];
    IEDT = (int)inputArr[5];
    IEHM = (int)inputArr[6];
    IECS = (int)inputArr[7];
    
    beg_time = dmadptr(IBYR, IBDT, IBHM, IBCS);
    end_time = dmadptr(IEYR, IEDT, IEHM, IECS);
    
    mid_time = (beg_time + end_time)/2.0;
    
    dinvmadptr(mid_time, &iyr, &imd, &ihm, &ics);
    
    month = imd/100;
    day = imd - 100*month;
    
    outputArr[0] = (double)madGetDayno(iyr, month, day);

    return(0);
}


/***********************************************************************
*
* getBhm   derives Bhm from IBHM
*
*   arguments: 
*      inCount (num inputs) = 1 (IBHM)
*      inputArr - double array holding IBHM - hhmm of beginning of record
*      outCount (num outputs) = 1 (BHM)
*      outputArr - double array holding BHM - hhmm of beginning of record
*
*   Algorithm: BHM =  IBHM directly from prolog
*
*   returns - 0 (successful)
*/
int getBhm(int inCount, 
           double * inputArr, 
           int outCount, 
           double * outputArr, 
           FILE * errFile)
{
    outputArr[0] = inputArr[0];
    return(0);
}


/***********************************************************************
*
* getBhhmmss   derives Bhhmmss from IBHM and IBCS
*
*   arguments: 
*      inCount (num inputs) = 2 (IBHM, IBCS)
*      inputArr - double array holding IBHM - hhmm of beginning of record
*                                      IBCS - centiseconds at beginning of record
*      outCount (num outputs) = 1 (BHHMMSS)
*      outputArr - double array holding BHHMMSS - hhmmss of beginning of record
*
*   Algorithm: BHHMMSS from  IBHM*100 + IBCS/100
*
*   returns - 0 (successful)
*/
int getBhhmmss(int inCount, 
               double * inputArr, 
               int outCount, 
               double * outputArr, 
               FILE * errFile)
{
    int IBHM, IBCS;
    int seconds = 0;
    double beg_time = 0.0;
    
    IBHM = (int)inputArr[0];
    IBCS = (int)inputArr[1];

    beg_time = 100.0 * IBHM;
    seconds = IBCS/100;

    beg_time += (double)seconds;

    outputArr[0] = beg_time;

    return(0);
}


/***********************************************************************
*
* getEhhmmss   derives Ehhmmss from IEHM and IECS
*
*   arguments: 
*      inCount (num inputs) = 2 (IEHM, IECS)
*      inputArr - double array holding IEHM - hhmm of ending of record
*                                      IECS - centiseconds at ending of record
*      outCount (num outputs) = 1 (EHHMMSS)
*      outputArr - double array holding EHHMMSS - hhmmss of ending of record
*
*   Algorithm: EHHMMSS from  IEHM*100 + IECS/100
*
*   returns - 0 (successful)
*/
int getEhhmmss(int inCount, 
               double * inputArr, 
               int outCount, 
               double * outputArr, 
               FILE * errFile)
{
    int IEHM, IECS;
    int seconds = 0;
    double end_time = 0.0;
    
    IEHM = (int)inputArr[0];
    IECS = (int)inputArr[1];

    end_time = 100.0 * IEHM;
    seconds = IECS/100;

    end_time += (double)seconds;

    outputArr[0] = end_time;

    return(0);
}


/***********************************************************************
*
* getHm   derives hhmm from all prolog time information.
*
*    HM is set as hhmm at average time between beginning 
*    and end of record.
*
*   arguments: 
*      inCount (num inputs) = 8 (IBYR, IBDT, IBHM, IBCS, IEYR, IEDT, IEHM, IECS)
*      inputArr - double array holding inputs from above
*      outCount (num outputs) = 1 (HM)
*      outputArr - double array holding: HM - hhmm at avg time in record
*
*   Algorithm: Determine average time in record, determine its hhmm
*
*   returns - 0 (successful)
*/
int getHm(int inCount, 
          double * inputArr, 
          int outCount, 
          double * outputArr, 
          FILE * errFile)
{
    int IBYR, IBDT, IBHM, IBCS, IEYR, IEDT, IEHM, IECS;
    int iyr = 0, imd = 0, ihm = 0, ics = 0;
    double beg_time = 0.0;
    double end_time = 0.0;
    double mid_time = 0.0;
    
    IBYR = (int)inputArr[0];
    IBDT = (int)inputArr[1];
    IBHM = (int)inputArr[2];
    IBCS = (int)inputArr[3];
    IEYR = (int)inputArr[4];
    IEDT = (int)inputArr[5];
    IEHM = (int)inputArr[6];
    IECS = (int)inputArr[7];
    
    beg_time = dmadptr(IBYR, IBDT, IBHM, IBCS);
    end_time = dmadptr(IEYR, IEDT, IEHM, IECS);
    
    mid_time = (beg_time + end_time)/2.0;
    
    dinvmadptr(mid_time, &iyr, &imd, &ihm, &ics);
    
    outputArr[0] = (double)ihm;

    return(0);
}


/***********************************************************************
*
* getUth   derives UTH (hours since midnight UT of first day of experiment)
*          from all prolog time information.
*
*    UTH is set as the number of hours at average time between beginning 
*    and end of record since midnight UT of first day of experiment.
*
*   arguments: 
*      inCount (num inputs) = 12 (FIRST_IBYR, FIRST_IBDT, FIRST_IBHM, FIRST_IBCS,
*                                 IBYR, IBDT, IBHM, IBCS, IEYR, IEDT, IEHM, IECS)
*      inputArr - double array holding inputs from above
*      outCount (num outputs) = 1 (UTH)
*      outputArr - double array holding: UTH - number of hours at avg time in record
*                  since midnight UT of first day of experiment.
*
*   Algorithm: Determine average time in record, determine when midnight of first day
*              was using FIRST_*, get number of hours since then
*
*   returns - 0 (successful)
*/
int getUth(int inCount, 
           double * inputArr, 
           int outCount, 
           double * outputArr, 
           FILE * errFile)
{
    int FIRST_IBYR, FIRST_IBDT, FIRST_IBHM, FIRST_IBCS;
    int IBYR, IBDT, IBHM, IBCS, IEYR, IEDT, IEHM, IECS;
    int iyr = 0, imd = 0, ihm = 0, ics = 0;
    double beg_time = 0.0;
    double end_time = 0.0;
    double mid_time = 0.0;
    double first_time = 0.0;
    double first_midnight_time = 0.0;
    double sec_since_midnight = 0.0;
    
    FIRST_IBYR = (int)inputArr[0];
    FIRST_IBDT = (int)inputArr[1];
    FIRST_IBHM = (int)inputArr[2];
    FIRST_IBCS = (int)inputArr[3];
    IBYR       = (int)inputArr[4];
    IBDT       = (int)inputArr[5];
    IBHM       = (int)inputArr[6];
    IBCS       = (int)inputArr[7];
    IEYR       = (int)inputArr[8];
    IEDT       = (int)inputArr[9];
    IEHM       = (int)inputArr[10];
    IECS       = (int)inputArr[11];
    
    first_time = dmadptr(FIRST_IBYR, FIRST_IBDT, FIRST_IBHM, FIRST_IBCS);
    beg_time = dmadptr(IBYR, IBDT, IBHM, IBCS);
    end_time = dmadptr(IEYR, IEDT, IEHM, IECS);
    
    mid_time = (beg_time + end_time)/2.0;
    
    /* get the time of the first record */
    dinvmadptr(first_time, &iyr, &imd, &ihm, &ics);
    
    /* use that time to get midnight of that day UT */
    first_midnight_time = dmadptr(iyr, imd, 0, 0);
    
    sec_since_midnight = mid_time - first_midnight_time;
    
    /* return sec_since_midnight as hours */
    outputArr[0] = sec_since_midnight/3600.0;

    return(0);
}


/***********************************************************************
*
* getUts   derives UTS (seconds since midnight UT of first day of experiment)
*          from all prolog time information.
*
*    UTS is set as the number of seconds at average time between beginning 
*    and end of record since midnight UT of first day of experiment.
*
*   arguments: 
*      inCount (num inputs) = 12 (FIRST_IBYR, FIRST_IBDT, FIRST_IBHM, FIRST_IBCS,
*                                 IBYR, IBDT, IBHM, IBCS, IEYR, IEDT, IEHM, IECS)
*      inputArr - double array holding inputs from above
*      outCount (num outputs) = 1 (UTS)
*      outputArr - double array holding: UTS - number of seconds at avg time in record
*                  since midnight UT of first day of experiment.
*
*   Algorithm: Determine average time in record, determine when midnight of first day
*              was using FIRST_*, get number of seconds since then
*
*   returns - 0 (successful)
*/
int getUts(int inCount, 
           double * inputArr, 
           int outCount, 
           double * outputArr, 
           FILE * errFile)
{
    int FIRST_IBYR, FIRST_IBDT, FIRST_IBHM, FIRST_IBCS;
    int IBYR, IBDT, IBHM, IBCS, IEYR, IEDT, IEHM, IECS;
    int iyr = 0, imd = 0, ihm = 0, ics = 0;
    double beg_time = 0.0;
    double end_time = 0.0;
    double mid_time = 0.0;
    double first_time = 0.0;
    double first_midnight_time = 0.0;
    double sec_since_midnight = 0.0;
    
    FIRST_IBYR = (int)inputArr[0];
    FIRST_IBDT = (int)inputArr[1];
    FIRST_IBHM = (int)inputArr[2];
    FIRST_IBCS = (int)inputArr[3];
    IBYR       = (int)inputArr[4];
    IBDT       = (int)inputArr[5];
    IBHM       = (int)inputArr[6];
    IBCS       = (int)inputArr[7];
    IEYR       = (int)inputArr[8];
    IEDT       = (int)inputArr[9];
    IEHM       = (int)inputArr[10];
    IECS       = (int)inputArr[11];
    
    first_time = dmadptr(FIRST_IBYR, FIRST_IBDT, FIRST_IBHM, FIRST_IBCS);
    beg_time = dmadptr(IBYR, IBDT, IBHM, IBCS);
    end_time = dmadptr(IEYR, IEDT, IEHM, IECS);
    
    mid_time = (beg_time + end_time)/2.0;
    
    /* get the time of the first record */
    dinvmadptr(first_time, &iyr, &imd, &ihm, &ics);
    
    /* use that time to get midnight of that day UT */
    first_midnight_time = dmadptr(iyr, imd, 0, 0);
    
    sec_since_midnight = mid_time - first_midnight_time;
    
    /* return sec_since_midnight */
    outputArr[0] = sec_since_midnight;

    return(0);
}


/***********************************************************************
*
* getBUth   derives B_UTH (hours until start time since midnight UT of first day of experiment)
*           from all prolog time information.
*
*    B_UTH is set as the number of hours at beginning time 
*    of record since midnight UT of first day of experiment.
*
*   arguments: 
*      inCount (num inputs) = 8 (FIRST_IBYR, FIRST_IBDT, FIRST_IBHM, FIRST_IBCS,
*                                IBYR, IBDT, IBHM, IBCS)
*      inputArr - double array holding inputs from above
*      outCount (num outputs) = 1 (B_UTH)
*      outputArr - double array holding: B_UTH - number of hours at record start time
*                  since midnight UT of first day of experiment.
*
*   Algorithm: Get start time in record, determine when midnight of first day
*              was using FIRST_*, get number of hours since then
*
*   returns - 0 (successful)
*/
int getBUth(int inCount, 
           double * inputArr, 
           int outCount, 
           double * outputArr, 
           FILE * errFile)
{
    int FIRST_IBYR, FIRST_IBDT, FIRST_IBHM, FIRST_IBCS;
    int IBYR, IBDT, IBHM, IBCS;
    int iyr = 0, imd = 0, ihm = 0, ics = 0;
    double beg_time = 0.0;
    double first_time = 0.0;
    double first_midnight_time = 0.0;
    double sec_since_midnight = 0.0;
    
    FIRST_IBYR = (int)inputArr[0];
    FIRST_IBDT = (int)inputArr[1];
    FIRST_IBHM = (int)inputArr[2];
    FIRST_IBCS = (int)inputArr[3];
    IBYR       = (int)inputArr[4];
    IBDT       = (int)inputArr[5];
    IBHM       = (int)inputArr[6];
    IBCS       = (int)inputArr[7];

    
    first_time = dmadptr(FIRST_IBYR, FIRST_IBDT, FIRST_IBHM, FIRST_IBCS);
    beg_time = dmadptr(IBYR, IBDT, IBHM, IBCS);
    
    /* get the time of the first record */
    dinvmadptr(first_time, &iyr, &imd, &ihm, &ics);
    
    /* use that time to get midnight of that day UT */
    first_midnight_time = dmadptr(iyr, imd, 0, 0);
    
    sec_since_midnight = beg_time - first_midnight_time;
    
    /* return sec_since_midnight as hours */
    outputArr[0] = sec_since_midnight/3600.0;

    return(0);
}


/***********************************************************************
*
* getInttms   derives integration time in seconds from all prolog time information.
*
*    INTTMS is set as the number of seconds between beginning 
*    and end of record.
*
*   arguments: 
*      inCount (num inputs) = 8 (IBYR, IBDT, IBHM, IBCS, IEYR, IEDT, IEHM, IECS)
*      inputArr - double array holding inputs from above
*      outCount (num outputs) = 1 (INTTMS)
*      outputArr - double array holding: INTTMS - number of seconds between beginning 
*                                        and end of record
*
*   Algorithm: Determine beg and end time in record, determine difference in seconds
*
*   returns - 0 (successful)
*/
int getInttms(int inCount, 
              double * inputArr, 
              int outCount, 
              double * outputArr, 
              FILE * errFile)
{
    int IBYR, IBDT, IBHM, IBCS, IEYR, IEDT, IEHM, IECS;
    double beg_time = 0.0;
    double end_time = 0.0;
    
    IBYR = (int)inputArr[0];
    IBDT = (int)inputArr[1];
    IBHM = (int)inputArr[2];
    IBCS = (int)inputArr[3];
    IEYR = (int)inputArr[4];
    IEDT = (int)inputArr[5];
    IEHM = (int)inputArr[6];
    IECS = (int)inputArr[7];
    
    beg_time = dmadptr(IBYR, IBDT, IBHM, IBCS);
    end_time = dmadptr(IEYR, IEDT, IEHM, IECS);
    
    
    outputArr[0] = end_time - beg_time;

    return(0);
}


/***********************************************************************
*
* getInttmm   derives integration time in minutes from all prolog time information.
*
*    INTTMM is set as the number of minutes between beginning 
*    and end of record.
*
*   arguments: 
*      inCount (num inputs) = 8 (IBYR, IBDT, IBHM, IBCS, IEYR, IEDT, IEHM, IECS)
*      inputArr - double array holding inputs from above
*      outCount (num outputs) = 1 (INTTMM)
*      outputArr - double array holding: INTTMM - number of minutes between beginning 
*                                        and end of record
*
*   Algorithm: Determine beg and end time in record, determine difference in minutes
*
*   returns - 0 (successful)
*/
int getInttmm(int inCount, 
              double * inputArr, 
              int outCount, 
              double * outputArr, 
              FILE * errFile)
{
    int IBYR, IBDT, IBHM, IBCS, IEYR, IEDT, IEHM, IECS;
    double beg_time = 0.0;
    double end_time = 0.0;
    
    IBYR = (int)inputArr[0];
    IBDT = (int)inputArr[1];
    IBHM = (int)inputArr[2];
    IBCS = (int)inputArr[3];
    IEYR = (int)inputArr[4];
    IEDT = (int)inputArr[5];
    IEHM = (int)inputArr[6];
    IECS = (int)inputArr[7];
    
    beg_time = dmadptr(IBYR, IBDT, IBHM, IBCS);
    end_time = dmadptr(IEYR, IEDT, IEHM, IECS);
    
    
    outputArr[0] = (end_time - beg_time)/60.0;

    return(0);
}


/***********************************************************************
*
* getDatntd   derives integration time in days from all prolog time information.
*
*    INTTMM is set as the number of days between beginning 
*    and end of record.
*
*   arguments: 
*      inCount (num inputs) = 8 (IBYR, IBDT, IBHM, IBCS, IEYR, IEDT, IEHM, IECS)
*      inputArr - double array holding inputs from above
*      outCount (num outputs) = 1 (DATNTD)
*      outputArr - double array holding: DATNTD - number of days between beginning 
*                                        and end of record
*
*   Algorithm: Determine beg and end time in record, determine difference in days
*
*   returns - 0 (successful)
*/
int getDatntd(int inCount, 
              double * inputArr, 
              int outCount, 
              double * outputArr, 
              FILE * errFile)
{
    int IBYR, IBDT, IBHM, IBCS, IEYR, IEDT, IEHM, IECS;
    double beg_time = 0.0;
    double end_time = 0.0;
    
    IBYR = (int)inputArr[0];
    IBDT = (int)inputArr[1];
    IBHM = (int)inputArr[2];
    IBCS = (int)inputArr[3];
    IEYR = (int)inputArr[4];
    IEDT = (int)inputArr[5];
    IEHM = (int)inputArr[6];
    IECS = (int)inputArr[7];
    
    beg_time = dmadptr(IBYR, IBDT, IBHM, IBCS);
    end_time = dmadptr(IEYR, IEDT, IEHM, IECS);
    
    
    outputArr[0] = (end_time - beg_time)/(60.0*60.0*24.0);

    return(0);
}


/***********************************************************************
*
* getUt   derives UT  (Hours since midnight of exp beg, MOD 24).
*
*
*   arguments: 
*      inCount (num inputs) = 1 (UTH)
*      inputArr - double array holding inputs from above
*      outCount (num outputs) = 1 (UT)
*      outputArr - double array holding: UT - Hours since midnight 
*                                        of exp beg, MOD 24
*
*   Algorithm: UT = fmod(UTH, 24.0)
*
*   returns - 0 (successful)
*/
int getUt(int inCount, 
          double * inputArr, 
          int outCount, 
          double * outputArr, 
          FILE * errFile)
{
    outputArr[0] = fmod(inputArr[0], 24.0);

    return(0);
}


/***********************************************************************
*
* getBegUt   derives Beg_UT  (Hours since midnight of exp beg, MOD 24, using start time).
*
*
*   arguments: 
*      inCount (num inputs) = 1 (B_UTH)
*      inputArr - double array holding inputs from above
*      outCount (num outputs) = 1 (BEG_UT)
*      outputArr - double array holding: BEG_UT - Hours since midnight 
*                                        of exp beg, MOD 24, using start time
*
*   Algorithm: BEG_UT = fmod(B_UTH, 24.0)
*
*   returns - 0 (successful)
*/
int getBegUt(int inCount, 
             double * inputArr, 
             int outCount, 
             double * outputArr, 
             FILE * errFile)
{
    outputArr[0] = fmod(inputArr[0], 24.0);

    return(0);
}


/***********************************************************************
*
* getJdayno   derives Julian day number from all prolog time information.
*
*    Jdayno is set as day number at average time between beginning 
*    and end of record.
*
*   arguments: 
*      inCount (num inputs) = 8 (IBYR, IBDT, IBHM, IBCS, IEYR, IEDT, IEHM, IECS)
*      inputArr - double array holding inputs from above
*      outCount (num outputs) = 1 (JDAYNO)
*      outputArr - double array holding: JDAYNO (Julian day number)
*
*   Algorithm: Determine average time in record, determine its Julian Day number 
*              via date method jday.
*
*   returns - 0 (successful), -1 if error
*/
int getJdayno(int inCount, 
              double * inputArr, 
              int outCount, 
              double * outputArr, 
              FILE * errFile)
{
    int IBYR, IBDT, IBHM, IBCS, IEYR, IEDT, IEHM, IECS;
    int iyr = 0, imd = 0, ihm = 0, ics = 0;
    int month = 0;
    int day = 0;
    double beg_time = 0.0;
    double end_time = 0.0;
    double mid_time = 0.0;
    
    IBYR = (int)inputArr[0];
    IBDT = (int)inputArr[1];
    IBHM = (int)inputArr[2];
    IBCS = (int)inputArr[3];
    IEYR = (int)inputArr[4];
    IEDT = (int)inputArr[5];
    IEHM = (int)inputArr[6];
    IECS = (int)inputArr[7];
    
    beg_time = dmadptr(IBYR, IBDT, IBHM, IBCS);
    end_time = dmadptr(IEYR, IEDT, IEHM, IECS);
    
    mid_time = (beg_time + end_time)/2.0;
    
    dinvmadptr(mid_time, &iyr, &imd, &ihm, &ics);
    
    /* get month and day */
    month = imd/100;
    day = imd - (month*100);
    
    outputArr[0] = (double)jday(day, month, iyr);
    
    if (outputArr[0] < 0.0)   /* error occured */
        return(-1);
    else
        return(0);
}


/***********************************************************************
*
* getUt1   derives UT1 from prolog start time.
*
*    UT1 is the number of seconds from 1/1/1950 to record start time.
*
*   arguments: 
*      inCount (num inputs) = 4 (IBYR, IBDT, IBHM, IBCS)
*      inputArr - double array holding inputs from above
*      outCount (num outputs) = 1 (UT1)
*      outputArr - double array holding: UT1 
*
*   Algorithm: Determine start time in record via dmadptr
*
*   returns - 0 (successful)
*/
int getUt1(int inCount, 
           double * inputArr, 
           int outCount, 
           double * outputArr, 
           FILE * errFile)
{
    int IBYR, IBDT, IBHM, IBCS;

    IBYR = (int)inputArr[0];
    IBDT = (int)inputArr[1];
    IBHM = (int)inputArr[2];
    IBCS = (int)inputArr[3];
    
    outputArr[0] = dmadptr(IBYR, IBDT, IBHM, IBCS);

    return(0);
}


/***********************************************************************
*
* getUt2   derives UT2 from prolog end time.
*
*    UT2 is the number of seconds from 1/1/1950 to record end time.
*
*   arguments: 
*      inCount (num inputs) = 4 (IEYR, IEDT, IEHM, IECS)
*      inputArr - double array holding inputs from above
*      outCount (num outputs) = 1 (UT2)
*      outputArr - double array holding: UT2 
*
*   Algorithm: Determine end time in record via dmadptr
*
*   returns - 0 (successful)
*/
int getUt2(int inCount, 
           double * inputArr, 
           int outCount, 
           double * outputArr, 
           FILE * errFile)
{
    int IEYR, IEDT, IEHM, IECS;

    IEYR = (int)inputArr[0];
    IEDT = (int)inputArr[1];
    IEHM = (int)inputArr[2];
    IECS = (int)inputArr[3];
    
    outputArr[0] = dmadptr(IEYR, IEDT, IEHM, IECS);

    return(0);
}



/***********************************************************************
*
* getDut21   derives Integration time in secs from UT2 - UT1.
*
*
*   arguments: 
*      inCount (num inputs) = 2 (UT1, UT2)
*      inputArr - double array holding inputs from above
*      outCount (num outputs) = 1 (DUT21)
*      outputArr - double array holding: DUT21 (record/integration
*                  time in seconds). 
*
*   Algorithm: DUT21 = UT2 - UT1
*
*   returns - 0 (successful)
*/
int getDut21(int inCount, 
             double * inputArr, 
             int outCount, 
             double * outputArr, 
             FILE * errFile)
{
    outputArr[0] = inputArr[1] - inputArr[0];

    return(0);
}


/***********************************************************************
*
* getFyear   derives average time as float year from all prolog time information.
*
*    Fyear is set at average time between beginning 
*    and end of record in units of years.
*
*   arguments: 
*      inCount (num inputs) = 8 (IBYR, IBDT, IBHM, IBCS, IEYR, IEDT, IEHM, IECS)
*      inputArr - double array holding inputs from above
*      outCount (num outputs) = 1 (FYEAR)
*      outputArr - double array holding: FYEAR - ave time in years
*
*   Algorithm: Determine average time in record, determine its fyear
*
*   returns - 0 (successful)
*/
int getFyear(int inCount, 
             double * inputArr, 
             int outCount, 
             double * outputArr, 
             FILE * errFile)
{
    int IBYR, IBDT, IBHM, IBCS, IEYR, IEDT, IEHM, IECS;
    int iyr = 0, imd = 0, ihm = 0, ics = 0;
    int month = 0, day = 0, hour = 0, min = 0;
    int dayOfYear = 0;
    int daysInYear = 0;
    double daysNow = 0.0;
    double beg_time = 0.0;
    double end_time = 0.0;
    double mid_time = 0.0;
    double fyear = 0.0;
    
    IBYR = (int)inputArr[0];
    IBDT = (int)inputArr[1];
    IBHM = (int)inputArr[2];
    IBCS = (int)inputArr[3];
    IEYR = (int)inputArr[4];
    IEDT = (int)inputArr[5];
    IEHM = (int)inputArr[6];
    IECS = (int)inputArr[7];
    
    beg_time = dmadptr(IBYR, IBDT, IBHM, IBCS);
    end_time = dmadptr(IEYR, IEDT, IEHM, IECS);
    
    mid_time = (beg_time + end_time)/2.0;
    
    dinvmadptr(mid_time, &iyr, &imd, &ihm, &ics);
    month = imd / 100;
    day = imd - (month*100);
    hour = ihm / 100;
    min = ihm - (hour*100);
    
    dayOfYear = madGetDayno(iyr, month, day);
    daysInYear = madGetDayno(iyr, 12, 31);
    
    fyear = (double)iyr;
    daysNow = ((double)(dayOfYear - 1));
    daysNow += ((double)(24 * hour + min))/(24.0*60.0);
    daysNow += ((double)ics)/ (100.0*3600.0*24.0);
    fyear +=  daysNow / ((double)daysInYear);
    
    outputArr[0] = fyear;

    return(0);
}


/***********************************************************************
*
* getStation   gets instrument lat, lon, and alt given kinst.
*
*
*   arguments: 
*      inCount (num inputs) = 1 (KINST)
*      inputArr - double array holding inputs from above
*      outCount (num outputs) = 3 (GDLATR, GDLONR, GALTR)
*      outputArr - double array holding:  
*                         GDLATR - Inst geod latitude (N hemi=pos) - deg
*                         GDLONR - Inst geod longitute - deg
*                         GALTR  - Inst altitute above sea level (km)
*
*   Algorithm: Gets all data from instTab.txt via cedarGetStationPos
*
*   returns - 0 (successful)
*/
int getStation(int inCount, 
               double * inputArr, 
               int outCount, 
               double * outputArr, 
               FILE * errFile)
{
    int kinst = 0;
    
    double GDLATR = 0.0;
    double GDLONR = 0.0;
    double GALTR  = 0.0;
    
    kinst = (int)inputArr[0];
    
    cedarGetStationPos(kinst, &GDLATR, &GDLONR, &GALTR);
    
    outputArr[0] = GDLATR;
    outputArr[1] = GDLONR;
    outputArr[2] = GALTR;

    return(0);
}


/***********************************************************************
*
* getAltInc   derives GDALT as  ALTB + (ROW*ALTAV)
*
*   arguments: 
*      inCount (num inputs) = 3 (ROW, ATLB, ALTAV)
*      inputArr - double array holding:
*                 ROW - 2D row number (start at 0)
*                 ALTB - starting altitude
*                 ALTAV - altitude increment per row
*      outCount (num outputs) = 1 (GDALT in km)
*      outputArr - double array holding:
*                 GDALT - Geodetic altitude in km
*
*   Algorithm: GDALT = ALTB + (ROW*ALTAV)
*
*   returns - 0 (successful)
*/
int getAltInc(int inCount, 
              double * inputArr, 
              int outCount, 
              double * outputArr, 
              FILE * errFile)
{
    outputArr[0] = inputArr[1] + (inputArr[0] * inputArr[2]);

    return 0;
}


/***********************************************************************
*
* getAveAlt   derives GDALT as average of ALTB and ALTE
*
*   arguments: 
*      inCount (num inputs) = 2 (ATLB, ALTE)
*      inputArr - double array holding:
*                 ALTB - starting altitude
*                 ALTE - ending altitude
*      outCount (num outputs) = 1 (GDALT in km)
*      outputArr - double array holding:
*                 GDALT - Geodetic altitude in km
*
*   Algorithm: GDALT = (ATLB + ALTE)/2
*
*   returns - 0 (successful)
*/
int getAveAlt(int inCount, 
              double * inputArr, 
              int outCount, 
              double * outputArr, 
              FILE * errFile)
{
    outputArr[0] = (inputArr[0] + inputArr[1])/2.0;

    return 0;
}


/***********************************************************************
*
* getAveDAlt   derives dGDALT given dALTB and dALTE
*
*   arguments: 
*      inCount (num inputs) = 2 (DATLB, DALTE)
*      inputArr - double array holding:
*                 DALTB - error in starting altitude (km)
*                 DALTE - error in sending altitude km)
*      outCount (num outputs) = 1 (DGDALT in km)
*      outputArr - double array holding:
*                 DGDALT - error in Geodetic altitude in km
*
*   Algorithm: dGDALT = 1/2(DATLB^2 + DALTE^2)^1/2
*
*   returns - 0 (successful)
*/
int getAveDAlt(int inCount, 
               double * inputArr, 
               int outCount, 
               double * outputArr, 
               FILE * errFile)
{
    /* if any error is special values assumed or knownbad, return missing */
    if(checkErrorData(inCount, inputArr, outCount, outputArr)) return (0);
    
    outputArr[0] = sqrt(pow(inputArr[0],2) + pow(inputArr[1],2))/2.0;

    return 0;
}


/***********************************************************************
*
* getResl   derives resl from mresl
*
*   A trivial method that equates the Millstone range resolution with
*   the standard range resolution
*
*   arguments: 
*      inCount (num inputs) = 1 (MRESL)
*      inputArr - double array holding:
*                 MRESL - Millstone specific range resolution in km
*      outCount (num outputs) = 1 (RESL in km)
*      outputArr - double array holding:
*                 RESL - range resolution in km
*
*   Algorithm: RESL = MRESL
*
*   returns - 0 (successful)
*/
int getResl(int inCount, 
            double * inputArr, 
            int outCount, 
            double * outputArr, 
            FILE * errFile)
{   
    outputArr[0] = inputArr[0];

    return 0;
}



/***********************************************************************
*
* getAzmDaz   derives AZM and DAZ from AZ1 and AZ2
*
*   arguments: 
*      inCount (num inputs) = 2 (AZ1, AZ2)
*      inputArr - double array holding:
*                 AZ1 - starting azimuth
*                 AZ2 - ending azimuth
*      outCount (num outputs) = 2 (AZM and DAZ)
*      outputArr - double array holding:
*                 AZM - median azimuth from -180 to 180 deg
*                 DAZ - Degrees of rotation from AZ1 to AZ2
*
*   Algorithm: The assumption is made that a clockwise motion
*   of the antenna always increases AZ, and counterclockwise
*   decreases AZ.  In case this convention is not followed,
*   a warning is printed to errFile whenever abs(DAZ) is greater
*   than 180 degrees.
*
*   This warning surpressed per request SRI on 1/15/2004
*
*   returns - 0 (successful)
*/
int getAzmDaz(int inCount, 
              double * inputArr, 
              int outCount, 
              double * outputArr, 
              FILE * errFile)
{

    double AZM = 0.0;
    double DAZ = 0.0;
    double AZ1 = 0.0;
    double AZ2 = 0.0;
    
    AZ1 = inputArr[0];
    AZ2 = inputArr[1];
    
    AZM = (AZ1 + AZ2)/2.0;
    DAZ = AZ2 - AZ1;
    
    /* force AZM to between -180 and 180 */
    while (AZM < -180.0) AZM += 360.0;
    while (AZM > +180.0) AZM -= 360.0;

    /* supress this warning
    if (fabs(DAZ) > 180.0)
        fprintf(errFile, "Warning: in getAzmDaz, DAZ found to be %f\n", DAZ);*/

    outputArr[0] = AZM;
    outputArr[1] = DAZ;

    return 0;
}


/***********************************************************************
*
* getDAzmDDaz   derives DAZM and DDAZ from DAZ1 and DAZ2
*
*   arguments: 
*      inCount (num inputs) = 2 (DAZ1, DAZ2)
*      inputArr - double array holding:
*                 DAZ1 - error in starting azimuth
*                 DAZ2 - error in ending azimuth
*      outCount (num outputs) = 2 (DAZM and DDAZ)
*      outputArr - double array holding:
*                 AZM - error in median azimuth from -180 to 180 deg
*                 DAZ - error in Degrees of rotation from AZ1 to AZ2
*
*   Algorithm: dAZM = 1/2(DDAZ1^2 + DDAZ2^2)^1/2
*              dDAZ = (DDAZ1^2 + DDAZ2^2)^1/2
*
*   returns - 0 (successful)
*/
int getDAzmDDaz(int inCount, 
                double * inputArr, 
                int outCount, 
                double * outputArr, 
                FILE * errFile)
{
    /* if any error is special values assumed or knownbad, return missing */
    if(checkErrorData(inCount, inputArr, outCount, outputArr)) return (0);
    
    outputArr[0] = sqrt(pow(inputArr[0],2) + pow(inputArr[1],2))/2.0;
    outputArr[1] = sqrt(pow(inputArr[0],2) + pow(inputArr[1],2));
    
    return 0;
}


/***********************************************************************
*
* getElmDel   derives ELM and DEL from EL1 and EL2
*
*   arguments: 
*      inCount (num inputs) = 2 (EL1, EL2)
*      inputArr - double array holding:
*                 EL1 - starting elevation in deg
*                 EL2 - ending elevation in deg
*      outCount (num outputs) = 2 (ELM, DEL)
*      outputArr - double array holding:
*                 ELM - median elevation in deg
*                 DEL - degrees of rotation between EL1 and EL2
*
*   Algorithm: ELM = EL1+EL2 / 2; DEL = EL2 - EL1
*
*   returns - 0 (successful)
*/
int getElmDel(int inCount, 
              double * inputArr, 
              int outCount, 
              double * outputArr, 
              FILE * errFile)
{
    /* ELM */
    outputArr[0] = (inputArr[0] + inputArr[1])/2.0;
    /* DEL */
    outputArr[1] = inputArr[1] - inputArr[0];

    return 0;
}


/***********************************************************************
*
* getDElmDDel   derives DELM and DDEL from DEL1 and DEL2
*
*   arguments: 
*      inCount (num inputs) = 2 (DEL1, DEL2)
*      inputArr - double array holding:
*                 DEL1 - error in starting elevation
*                 DEL2 - error in ending elevation
*      outCount (num outputs) = 2 (DELM and DDEL)
*      outputArr - double array holding:
*                 ELM - error in median elevation 
*                 DEL - error in Degrees of rotation from EL1 to EL2
*
*   Algorithm: dELM = 1/2(DDEL1^2 + DDEL2^2)^1/2
*              dDEL = (DDEL1^2 + DDEL2^2)^1/2
*
*   returns - 0 (successful)
*/
int getDElmDDel(int inCount, 
                double * inputArr, 
                int outCount, 
                double * outputArr, 
                FILE * errFile)
{
    /* if any error is special values assumed or knownbad, return missing */
    if(checkErrorData(inCount, inputArr, outCount, outputArr)) return (0);

    outputArr[0] = sqrt(pow(inputArr[0],2) + pow(inputArr[1],2))/2.0;
    outputArr[1] = sqrt(pow(inputArr[0],2) + pow(inputArr[1],2));
    
    return 0;
}


/***********************************************************************
*
* getGeod   derives Geodetic lat, long and alt from kinst, azm, elm, and range
*
*   arguments: 
*      inCount (num inputs) = 4 (KINST, AZM, ELM, RANGE)
*      inputArr - double array holding:
*                 KINST - instrument id
*                 AZM - mean azimuth in deg
*                 ELM - mean elevation in deg
*                 RANGE - range in km
*      outCount (num outputs) = 3 (GDLAT, GLON, GDALT)
*      outputArr - double array holding:
*                 GDLAT - geodetic latitude of measurement
*                 GLON - geodetic longitude of measurement
*                 GDALT - geodetic altitude of measurement
*
*   Algorithm: Calls los2geodetic from geometry.c
*
*   returns - 0 (successful)
*/
int getGeod(int inCount, 
            double * inputArr, 
            int outCount, 
            double * outputArr, 
            FILE * errFile)
{
    int kinst = 0;
    double azm = 0.0;
    double elm = 0.0;
    double range = 0.0;
    double gdlat = 0.0;
    double glon = 0.0;
    double gdalt = 0.0;
    
    /* get inputs */
    kinst = (int)inputArr[0];
    azm = inputArr[1];
    elm = inputArr[2];
    range = inputArr[3];
    
    los2geodetic(kinst, azm, elm, range, &gdlat, &glon, &gdalt);
    
    /* force glon to between -180 and 180 */
    while (glon < -180.0) glon += 360.0;
    while (glon > +180.0) glon -= 360.0;
    
    /* copy outputs */
    outputArr[0] = gdlat;
    outputArr[1] = glon;
    outputArr[2] = gdalt;

    return 0;
}


/***********************************************************************
*
* getDGeod   derives error in Geodetic lat, long and alt 
*            from azm, dazm, elm, delm, range, and drange
*
*   arguments: 
*      inCount (num inputs) = 7 (KINST, AZM, DAZM, ELM, DELM, RANGE, DRANGE)
*      inputArr - double array holding:
*                 KINST - instrument id
*                 AZM - mean azimuth in deg
*                 DAZM - error in mean azimuth
*                 ELM - mean elevation in deg
*                 DELM - error in mean elevation
*                 RANGE - range in km
*                 DRANGE - error in range
*      outCount (num outputs) = 3 (DGDLAT, DGLON, DGDALT)
*      outputArr - double array holding:
*                 DGDLAT - error in geodetic latitude
*                 DGLON - error in geodetic longitude
*                 DGDALT - error in geodetic altitude
*
*   Algorithm: Calculate derivatives with respect to AZM, ELM, and
*              RANGE by simply finding the difference with them
*              incremented by 1 degree or 1 km.  These derivitives
*              are:
*                 dlatdaz, dlatdel, dlatdrange
*                 dlondaz, dlondel, dlondrange
*                 daltdaz, daltdel, daltdrange
*
*              then:
*
*     dgdlat = ((dlatdaz*dazm)^2 + (dlatdel*delm)^2 + (dlatdrange*drange)^2)^1/2
*     dgdlon = ((dlondaz*dazm)^2 + (dlondel*delm)^2 + (dlondrange*drange)^2)^1/2
*     dgdalt = ((daltdaz*dazm)^2 + (daltdel*delm)^2 + (daltdrange*drange)^2)^1/2
*
*   returns - 0 (successful)
*/
int getDGeod(int inCount, 
             double * inputArr, 
             int outCount, 
             double * outputArr, 
             FILE * errFile)
{

    double azm = 0.0;
    double dazm = 0.0;
    double elm = 0.0;
    double delm = 0.0;
    double range = 0.0;
    double drange = 0.0;
    double dgdlat = 0.0;
    double dglon = 0.0;
    double dgdalt = 0.0;
    int kinst = 0;
    
    /* 9 derivatives */
    double dlatdaz = 0.0;
    double dlatdel = 0.0;
    double dlatdrange = 0.0;        
    double dlondaz = 0.0;
    double dlondel = 0.0;
    double dlondrange = 0.0;             
    double daltdaz = 0.0;
    double daltdel = 0.0;
    double daltdrange = 0.0;
    
    /* temp gdlat, glon, gdalt to calc derivatives */
    double start_gdlat = 0.0;
    double start_glon = 0.0;
    double start_gdalt = 0.0;
    double end_gdlat = 0.0;
    double end_glon = 0.0;
    double end_gdalt = 0.0;
    
    /* get inputs */
    kinst = (int)inputArr[0];
    azm  = inputArr[1];
    dazm = inputArr[2];
    elm  = inputArr[3];
    delm = inputArr[4];
    range = inputArr[5];
    drange = inputArr[6];
    
    /* calculate derivatives */
    los2geodetic(kinst, azm, elm, range, &start_gdlat, &start_glon, &start_gdalt);
    
    los2geodetic(kinst, azm+1, elm, range, &end_gdlat, &end_glon, &end_gdalt);
    dlatdaz = end_gdlat - start_gdlat;
    
    los2geodetic(kinst, azm, elm+1, range, &end_gdlat, &end_glon, &end_gdalt);
    dlatdel = end_gdlat - start_gdlat;
    
    los2geodetic(kinst, azm, elm, range+1, &end_gdlat, &end_glon, &end_gdalt);
    dlatdrange = end_gdlat - start_gdlat;  
    
    los2geodetic(kinst, azm+1, elm, range, &end_gdlat, &end_glon, &end_gdalt);      
    dlondaz = end_glon - start_glon;
    
    los2geodetic(kinst, azm, elm+1, range, &end_gdlat, &end_glon, &end_gdalt);
    dlondel = end_glon - start_glon;
    
    los2geodetic(kinst, azm, elm, range+1, &end_gdlat, &end_glon, &end_gdalt);
    dlondrange = end_glon - start_glon; 
    
    los2geodetic(kinst, azm+1, elm, range, &end_gdlat, &end_glon, &end_gdalt);            
    daltdaz = end_gdalt - start_gdalt;
    
    los2geodetic(kinst, azm, elm+1, range, &end_gdlat, &end_glon, &end_gdalt);
    daltdel = end_gdalt - start_gdalt;
    
    los2geodetic(kinst, azm, elm, range+1, &end_gdlat, &end_glon, &end_gdalt);
    daltdrange = end_gdalt - start_gdalt;
    
    dgdlat = sqrt(pow(dlatdaz*dazm,2.0) + pow(dlatdel*delm,2.0) + pow(dlatdrange*drange,2.0));
    dglon  = sqrt(pow(dlondaz*dazm,2.0) + pow(dlondel*delm,2.0) + pow(dlondrange*drange,2.0));
    dgdalt = sqrt(pow(daltdaz*dazm,2.0) + pow(daltdel*delm,2.0) + pow(daltdrange*drange,2.0));
    
    /* copy outputs */
    outputArr[0] = dgdlat;
    outputArr[1] = dglon;
    outputArr[2] = dgdalt;

    return 0;
}


/***********************************************************************
*
* getGeodGdalt   derives Geodetic lat, long and alt from kinst, azm, elm, and gdalt
*
*   arguments: 
*      inCount (num inputs) = 4 (KINST, AZM, ELM, GDALT)
*      inputArr - double array holding:
*                 KINST - instrument id
*                 AZM - mean azimuth in deg
*                 ELM - mean elevation in deg
*                 GDALT - alt in km
*      outCount (num outputs) = 3 (GDLAT, GLON)
*      outputArr - double array holding:
*                 GDLAT - geodetic latitude of measurement
*                 GLON - geodetic longitude of measurement
*
*   Algorithm: Calls los2geodetic from geometry.c after calculating range
*
*   returns - 0 (successful)
*/
int getGeodGdalt(int inCount, 
                 double * inputArr, 
                 int outCount, 
                 double * outputArr, 
                 FILE * errFile)
{
    int kinst = 0;
    double azm = 0.0;
    double elm = 0.0;
    double range = 0.0;
    double gdlat = 0.0;
    double glon = 0.0;
    double gdalt = 0.0;
    
    /* get inputs */
    kinst = (int)inputArr[0];
    azm = inputArr[1];
    elm = inputArr[2];
    range = inputArr[3];
    
    /* range was passed in as gdalt - convert */
    /* protect againt elm < 0.0001 degrees */
    if (elm < 0.0001 || elm > 90.0001)
    {
        outputArr[0] = missing;
        outputArr[1] = missing;
        return (-1);
    }
    range = range/sin(elm/57.29578);
    
    los2geodetic(kinst, azm, elm, range, &gdlat, &glon, &gdalt);
    
    /* force glon to between -180 and 180 */
    while (glon < -180.0) glon += 360.0;
    while (glon > +180.0) glon -= 360.0;
    
    /* copy outputs */
    outputArr[0] = gdlat;
    outputArr[1] = glon;

    return 0;
}


/***********************************************************************
*
* getGeodAlt   derives Geodetic lat, long  by assuming measured point is 
*              directly above instrument
*
*    This method will be called only if GDLAT, GLON cannot be derived 
*    any other way
*
*   arguments: 
*      inCount (num inputs) = 2 (GDLATR, GDLONR)
*      inputArr - double array holding:
*                 GDLATR - Inst geod latitude (N hemi=pos) - deg
*                 GDLONR - Inst geod longitute - deg
*      outCount (num outputs) = 2 (GDLAT, GLON)
*      outputArr - double array holding:
*                 GDLAT - geodetic latitude of measurement
*                 GLON - longitude of measurement
*
*   Algorithm: Sets GDLAT, GLON to station values
*
*   returns - 0 (successful)
*/
int getGeodAlt(int inCount, 
               double * inputArr, 
               int outCount, 
               double * outputArr, 
               FILE * errFile)
{
    outputArr[0] = inputArr[0];
    outputArr[1] = inputArr[1];
    
    /* force outputArr[1] to between -180 and 180 */
    while (outputArr[1] < -180.0) outputArr[1] += 360.0;
    while (outputArr[1] > +180.0) outputArr[1] -= 360.0;

    return 0;
}


/***********************************************************************
*
* getAzElRange   derives Azm, Elm, and Range given gdlat,glon,gdalt 
*                (point position) and gdlatr,glonr,galtr (station position)
*
*    This method will be called only if Azm, Elm, and Range cannot be derived 
*    any other way
*
*   arguments: 
*      inCount (num inputs) = 6 (GDLAT, GLON, GDALT, GDLATR, GDLONR, GALTR)
*      inputArr - double array holding:
*                 GDLAT - geodetic latitude of measurement
*                 GLON - longitude of measurement
*                 GDALT - geodetic altitude of measurement in km
*                 GDLATR - Inst geod latitude (N hemi=pos) - deg
*                 GDLONR - Inst geod longitute - deg
*                 GALTR - geodetic altitude of station in km
*      outCount (num outputs) = 3 (AZM, ELM, RANGE)
*      outputArr - double array holding:
*                 AZM - mean azimuth in deg
*                 ELM - mean elevation in deg
*                 RANGE - range in km
*
*   Algorithm: See look in geometry.c
*
*   returns - 0 (successful)
*/
int getAzElRange(int inCount, 
                 double * inputArr, 
                 int outCount, 
                 double * outputArr, 
                 FILE * errFile)
{
    double galtr = 0.0;   /* geodetic altitute of station                            */
    double sr = 0.0;      /* distance of station from center of earth (km)           */
    double gdlatr = 0.0;  /* geodetic latitude of station                            */
    double slat = 0.0;    /* geocentric latitude of station (deg)                    */
    double slon = 0.0;    /* longitude of station (deg)                              */
    double pr = 0.0;      /* distance from center of earth of observation point (km) */
    double gdalt = 0.0;   /* geodetic alt of observation point (km)                  */
    double glat = 0.0;    /* observation point geocentric latitude (deg)             */
    double gdlat = 0.0;   /* observation point geodetic latitude (deg)               */
    double glon = 0.0;    /* observation point longitude (deg)                       */
    double azm = 0.0;     /* radar azimuth (deg)                                     */
    double elm = 0.0;     /* radar elevation (deg)                                   */
    double range = 0.0;   /* radar range (km)                                        */
    
    gdlat  = inputArr[0];
    glon   = inputArr[1];
    gdalt  = inputArr[2];
    gdlatr = inputArr[3];
    slon   = inputArr[4];
    galtr  = inputArr[5];
    
    /* get geocentric coordinates of both points */
    convrt(1, &gdlat, &gdalt, &glat, &pr);  /* point in space */
    convrt(1, &gdlatr, &galtr, &slat, &sr); /* station */
    
    look(&sr, &slat, &slon, &pr, &glat, &glon, &azm, &elm, &range);
    
    /* set outputs */
    outputArr[0] = azm;
    outputArr[1] = elm;
    outputArr[2] = range;

    return 0;
}


/***********************************************************************
*
* getSZen   derives Solar zenith angle in deg (0 = overhead)
*
*   arguments: 
*      inCount (num inputs) = 4 (UT1, UT2, GDLAT, GLON)
*      inputArr - double array holding:
*                 UT1 - UT at record start
*                 UT2 - UT at record end
*                 GDLAT - geodetic latitude
*                 GLON - geodetic longitude
*      outCount (num outputs) = 1 (SZEN)
*      outputArr - double array holding:
*                 SZEN - Solar zenith angle in deg (0 = overhead)
*
*   Algorithm: See solarzen in geometry.c
*
*   returns - 0 (successful)
*/
int getSZen(int inCount, 
            double * inputArr, 
            int outCount, 
            double * outputArr, 
            FILE * errFile)
{
    double mid_time = 0.0;
    double gdlat = 0.0;
    double glon = 0.0;
    double szen = 0.0;
    double saz = 0.0;
    
    mid_time = (inputArr[0] + inputArr[1])/2.0;
    gdlat = inputArr[2];
    glon  = inputArr[3];
    
    /* force glon to between -180 and 180 */
    while (glon < -180.0) glon += 360.0;
    while (glon > +180.0) glon -= 360.0;
    
    solarzen_az(mid_time, gdlat, glon, &szen, &saz);
    
    outputArr[0] = szen;

    return(0);
}


/***********************************************************************
*
* getSltmut   derives SLTMUT (local solar time diff)
*
*   arguments: 
*      inCount (num inputs) = 3 (UT1, UT2, GLON)
*      inputArr - double array holding:
*                 UT1 - UT at record start
*                 UT2 - UT at record end
*                 GLON - geodetic longitude
*      outCount (num outputs) = 1 (SLTMUT)
*      outputArr - double array holding:
*                 SLTMUT - Local solar time diff (=SLT-UT) +E lon in hhmm
*
*   Algorithm: Add 3600*24*GLON/360 to average UT - convert to hhmm, where 
*              GLON goes from -180 to 180
*
*   returns - 0 (successful)
*/
int getSltmut(int inCount, 
              double * inputArr, 
              int outCount, 
              double * outputArr, 
              FILE * errFile)
{
    int iyr = 0, imd = 0, ihm = 0, ics = 0;
    double mid_time = 0.0;
    double glon = 0.0;
    
    glon = inputArr[2];
    
    /* force glon to between -180 and 180 */
    while (glon < -180.0) glon += 360.0;
    while (glon > +180.0) glon -= 360.0;
    
    mid_time = (inputArr[0] + inputArr[1])/2.0;
    
    mid_time += (3600.0*24.0/360.0)*glon;
    
    dinvmadptr(mid_time, &iyr, &imd, &ihm, &ics);
    
    outputArr[0] = (double)ihm;

    return(0);
}


/***********************************************************************
*
* getSlt   derives SLT (local solar time  in hours)
*
*   arguments: 
*      inCount (num inputs) = 3 (UT1, UT2, GLON)
*      inputArr - double array holding:
*                 UT1 - UT at record start
*                 UT2 - UT at record end
*                 GLON - geodetic longitude
*      outCount (num outputs) = 1 (SLT)
*      outputArr - double array holding:
*                 SLT - Local solar time in hours (0-24)
*
*   Algorithm: Add 3600*24*GLON/360 to average UT - convert to hours, where 
*              GLON goes from -180 to 180
*
*   returns - 0 (successful)
*/
int getSlt(int inCount, 
           double * inputArr, 
           int outCount, 
           double * outputArr, 
           FILE * errFile)
{
    int iyr = 0, imd = 0, ihm = 0, ics = 0;
    double mid_time = 0.0;
    int hour = 0;
    int min = 0;
    double glon = 0.0;
    
    glon = inputArr[2];
    
    /* force glon to between -180 and 180 */
    while (glon < -180.0) glon += 360.0;
    while (glon > +180.0) glon -= 360.0;
    
    mid_time = (inputArr[0] + inputArr[1])/2.0;
    
    mid_time += (3600.0*24.0/360.0)*glon;
    
    dinvmadptr(mid_time, &iyr, &imd, &ihm, &ics);
    
    hour = ihm/100;
    min  = ihm - 100*hour;
    
    outputArr[0] = (double)hour;
    outputArr[0] += (double)min/60.0;
    outputArr[0] += (double)ics/360000.0;

    return (0);
}



/***********************************************************************
*
* getSdwHt   derives shadowheight - the distance directly above any gdlat and glon
*              for a given UT in km at which the earth's shadow terminates.
*              Will be 0.0 on dayside of earth.
*
*   arguments: 
*      inCount (num inputs) = 4 (UT1, UT2, GDLAT, GLON)
*      inputArr - double array holding:
*                 UT1 - UT at record start
*                 UT2 - UT at record end
*                 GDLAT - geodetic latitude
*                 GLON - geodetic longitude
*      outCount (num outputs) = 1 (SDWHT)
*      outputArr - double array holding:
*                 SDWHT - the distance in km above the given gdlat and glon
*                         at which any part of the sun is visible (may be 0.0)
*
*   Algorithm: See shadowheight in geometry.c
*
*   returns - 0 (successful)
*/
int getSdwHt(int inCount, 
             double * inputArr, 
             int outCount, 
             double * outputArr, 
             FILE * errFile)
{
    double mid_time = 0.0;
    double gdlat = 0.0;
    double glon = 0.0;
    
    mid_time = (inputArr[0] + inputArr[1])/2.0;
    gdlat = inputArr[2];
    glon  = inputArr[3];
    
    /* force glon to between -180 and 180 */
    while (glon < -180.0) glon += 360.0;
    while (glon > +180.0) glon -= 360.0;
    
    outputArr[0] = shadowheight(mid_time, gdlat, glon);

    return(0);
}


/***********************************************************************
*
* getSuntime  derives sunset and sunrise for given point in space and time
*             for that day UT.
*
*   arguments: 
*      inCount (num inputs) = 5 (UT1, UT2, GDLAT, GLON, GDALT)
*      inputArr - double array holding:
*                 UT1 - UT at record start
*                 UT2 - UT at record end
*                 GDLAT - geodetic latitude
*                 GLON - geodetic longitude
*                 GDALT - geodetic altitude in km
*      outCount (num outputs) = 4 (SUNRISE, SUNSET, SUNRISE_HOUR, SUNSET_HOUR)
*      outputArr - double array holding:
*                 SUNRISE - time (UT) that day of sunrise at that point in space
*                 SUNSET - time (UT) that day of sunset at that point in space
*                 SUNRISE_HOUR - time (in hours UT) that day of sunrise at that point in space
*                 SUNSET_HOUR - time (in hours UT) that day of sunset at that point in space
*
*   Algorithm: See sunrise_set in geometry.c
*
*   returns - 0 (successful)
*/
int getSuntime(int inCount, 
               double * inputArr, 
               int outCount, 
               double * outputArr, 
               FILE * errFile)
{
    double mid_time = 0.0;
    double gdlat = 0.0;
    double glon = 0.0;
    double gdalt = 0.0;
    double sunrise = 0.0;
    double sunset = 0.0;
    double sunrise_h = 0.0;
    double sunset_h = 0.0;
    int iyr = 0, imd = 0, ihm = 0, ics = 0;
    int hour = 0, min = 0;
    
    mid_time = (inputArr[0] + inputArr[1])/2.0;
    gdlat = inputArr[2];
    glon  = inputArr[3];
    gdalt = inputArr[4];
    
    /* force glon to between -180 and 180 */
    while (glon < -180.0) glon += 360.0;
    while (glon > +180.0) glon -= 360.0;

    sunrise_set(mid_time,
                gdlat, 
                glon,
                gdalt,
                &sunrise,
                &sunset);
                
    if (sunrise != missing)
    {
        /* get sunrise in hours UT */
        dinvmadptr(sunrise, &iyr, &imd, &ihm, &ics);
        hour = ihm/100;
        min = ihm - hour*100;
        sunrise_h = (double)hour;
        sunrise_h += (double)min/60.0;
        sunrise_h += (double)ics/360000.0;
        outputArr[0] = sunrise;
        outputArr[2] = sunrise_h;
    }
    else
    {
        outputArr[0] = missing;
        outputArr[2] = missing;
    }
    
    if (sunset != missing)
    {
        /* get sunset in hours UT */
        dinvmadptr(sunset, &iyr, &imd, &ihm, &ics);
        hour = ihm/100;
        min = ihm - hour*100;
        sunset_h = (double)hour;
        sunset_h += (double)min/60.0;
        sunset_h += (double)ics/360000.0;
        outputArr[1] = sunset;
        outputArr[3] = sunset_h;
    }
    else
    {
        outputArr[1] = missing;
        outputArr[3] = missing;
    }
    return (0);
}


/***********************************************************************
*
* getTecGdalt  returns the altitude associated with a TEC measurement (350 km).
*
*   arguments: 
*      inCount (num inputs) = 3 (TEC, GDLAT, GLON)
*      inputArr - double array holding:
*                 TEC - Total electron content
*                 GDLAT - geodetic latitude
*                 GLON - geodetic longitude
*      outCount (num outputs) = 1 (GDALT)
*      outputArr - double array holding:
*                 GDALT - geodetic altitude in km
*
*   Algorithm: Hard-coded return of 350.0 km
*
*   returns - 0 (successful)
*/
int getTecGdalt(int inCount, 
                double * inputArr, 
                int outCount, 
                double * outputArr, 
                FILE * errFile)
{
    
    outputArr[0] = 350.0;
    return (0);
}


/***********************************************************************
*
* getGcdist  returns the great circle distance from the instrument to a point in space.
*
*   arguments:
*      inCount (num inputs) = 4 (GDLATR, GDLONR, GDLAT, GLON)
*      inputArr - double array holding:
*                 GDLATR - radar geodetic latitude
*                 GDLONR - radar geodetic longitude
*                 GDLAT - geodetic latitude
*                 GLON - geodetic longitude
*      outCount (num outputs) = 1 (GCDIST)
*      outputArr - double array holding:
*                 GCDIST - great circle distance from the instrument to a point in space
*                          relected onto the earth's surface (assumes spherical earth) in km.
*
*   Algorithm: See http://www.faqs.org/faqs/geography/infosystems-faq/ for great circle calculation
*
*   returns - 0 (successful)
*/
int getGcdist(int inCount,
              double * inputArr,
              int outCount,
              double * outputArr,
              FILE * errFile)
{
	double gdlatr = 0.0;
	double glonr = 0.0;
	double gdlat = 0.0;
    double glon = 0.0;
    double dlat = 0.0;
    double dlon = 0.0;
    double a = 0.0, c = 0.0;

    gdlatr = inputArr[0];
    glonr  = inputArr[1];
	gdlat = inputArr[2];
	glon  = inputArr[3];

	dlon = glonr/57.2958 - glon/57.2958;
	dlat = gdlatr/57.2958 - gdlat/57.2958;
	a = sin(dlat/2.0)*sin(dlat/2.0) + cos(gdlatr/57.2958)*cos(gdlat/57.2958) * sin(dlon/2.0)*sin(dlon/2.0);

	if (sqrt(a) < 1.0)
		c = 2.0 * asin(sqrt(a));
	else
		c = 2.0 * asin(1.0);

	outputArr[0] = c * 6371.2; /* 6371.2 = Earth radius */


    return (0);
}


/***********************************************************************
*
* getMag   derives magnetic parameters given a point in space and time.
*
*   arguments: 
*      inCount (num inputs) = 8 (UT1, UT2, GDLAT, GLON, GDALT)
*      inputArr - double array holding:
*                 UT1 - UT at record start
*                 UT2 - UT at record end
*                 GDLAT - geodetic latitude
*                 GLON - geodetic longitude
*                 GDALT - geodetic altitude
*      outCount (num outputs) = 16 
*      outputArr - double array holding:
*                 BN - Northward comp of geomag field (1E-8 T)
*                 BE - Eastward comp of geomag field (1E-8 T)
*                 BD - Downward comp of geomag field (1E-8 T)
*                 BMAG - geomag field strength (1E-8 T)
*                 BDEC - Geomag Field East Declination (deg)
*                 BINC - Geomag Field Downward Inclination (deg)
*                 LSHELL - L value in measurement volume
*                 DIPLAT - Dip latitude in measurement volume (deg)
*                 INVLAT - Invariant latitude in measurement volume (deg)
*                 APLAT - Apex latitude (deg)
*                 APLON - Apex longitude (deg)
*                 MAGCONJLAT - Magnetic conjugate geodetic latitude (deg)
*                 MAGCONJLON - Magnetic conjugate longitude (deg)
*                 CXR - MBperp. Direction Cosine (South [Apex]) m/s
*                 CYR - MBperp. Direction Cosine (East [Apex]) m/s
*                 CZR - MBperp. Direction Cosine (Up field line [Apex]) m/s
*
*   Algorithm: See coord in coord.f
*
*   returns - 0 (successful)
*/
int getMag(int inCount, 
           double * inputArr, 
           int outCount, 
           double * outputArr, 
           FILE * errFile)
{
    double mid_time = 0.0;
    double gdlat = 0.0;
    double glon = 0.0;
    double gdalt = 0.0;
    double slatgd = 0.0; /* station location is not used in any return value */
    double slon = 0.0;
    double salt = 6371.2;
    double sr = 0.0;
    double slatgc = 0.0;
    int iyr = 0, imd = 0, ihm = 0, ics = 0;
    double dyear = 0.0;
    double Rcor[38];
    
    /* arguments passed to coord but ignored */
    double az = 0.0;
    double el = 0.0;
    double range = -1.0; /* indicates not to use range */
    
    /* init Rcor to keep Purify happy */
    int i = 0;
    for (i=0; i<38; i++)
        Rcor[i] = 0.0;
    
    mid_time = (inputArr[0] + inputArr[1])/2.0;
    gdlat  = inputArr[2];
    glon   = inputArr[3];
    gdalt  = inputArr[4];
    
    /* get the point geocentric coords needed by geocgm01_ */
    convrt(1, &slatgd, &salt, &slatgc, &sr);
    
    /* force glon to between -180 and 180 */
    while (glon < -180.0) glon += 360.0;
    while (glon > +180.0) glon -= 360.0;
    
    dinvmadptr(mid_time, &iyr, &imd, &ihm, &ics);
    
    /* get fractional year using the month */
    dyear = (double)(iyr + ((double)(imd/100))/12.0);
    
    /* call coord */
    COORD_F77(&slatgd,&slon,&sr,&slatgc,&dyear,&az,&el,&range,
           &gdlat,&glon,&gdalt,Rcor);
    
    /* BN */
    outputArr[0] = -Rcor[8]*1E-4;
    /* BE */
    outputArr[1] =  Rcor[9]*1E-4;
    /* BD */
    outputArr[2] = -Rcor[7]*1E-4;
    /* BMAG */
    outputArr[3] =  Rcor[6]*1E-4;
    /* BDEC */
    outputArr[4] =  Rcor[29];
    /* BINC */
    outputArr[5] =  Rcor[28];
    /* LSHELL */
    outputArr[6] =  Rcor[12];
    /* DIPLAT */
    outputArr[7] =  Rcor[10];
    /* INVLAT */
    outputArr[8] =  Rcor[11];
    /* APLAT */
    outputArr[9] =  Rcor[13];
    /* APLON */
    outputArr[10] = Rcor[14];
    /* MAGCONJLAT */
    outputArr[11] = Rcor[33];
    /* check that magnetic conj point successfully found */
    if (outputArr[11] < -90.01 || outputArr[11] > 90.01)
    {
        outputArr[11] =  missing;
        outputArr[12] =  missing;
    }
    else
    {
        /* mag conj point okay */
        /* MAGCONJLON - set to -180 to 180 */
        if (Rcor[34] > 180.0)
            Rcor[34] = Rcor[34] - 360.0;
        outputArr[12] = Rcor[34];
    }
    /* CXR */
    outputArr[13] = Rcor[22];
    /* CYR */
    outputArr[14] = Rcor[23];
    /* CZR */
    outputArr[15] = Rcor[24];
    
    return (0);
}


/***********************************************************************
*
* getGeocgm   derives magnetic parameters given a point in space and time using CGM code.
*
*    Uses the GEO-CGM code available at http://nssdc.gsfc.nasa.gov/space/cgm/cgm.html
*
*    Since slower that coord.f, used only to find cgm_lat, cgm_long, and mlt
*
*   arguments: 
*      inCount (num inputs) = 5 (UT1, UT2, GDLAT, GLON, GDALT)
*      inputArr - double array holding:
*                 UT1 - UT at record start
*                 UT2 - UT at record end
*                 GDLAT - geodetic latitude
*                 GLON - geodetic longitude
*                 GDALT - geodetic altitude
*      outCount (num outputs) = 2 
*      outputArr - double array holding:
*                 CGM_LAT - Corrected geomagnetic latitude
*                 CGM_LONG - Corrected geomagnetic longitude
*
*   Algorithm: See geocgm01 in geo-cgm.f
*
*   returns - 0 (successful)
*/
int getGeocgm(int inCount, 
              double * inputArr, 
              int outCount, 
              double * outputArr, 
              FILE * errFile)
{
    double mid_time = 0.0;
    double gdlat = 0.0;
    double glon = 0.0;
    double gdalt = 0.0;
    double gclat = 0.0;
    double rkm = 0.0;
    int iyr = 0, imd = 0, ihm = 0, ics = 0;
    int geoToCgmIndicator = 1;
    double dat[44]; /* geocgm01_ returns 11 parameters (slar, slor, clar, clor,
                       rbm, btr, bfr, brr, ovl, azm, utm) organized for the start point
                       (*,1), its conjugate point (*,2), then for the footprints at 1-Re
                       of the start (*,3) and conjugate (*,4) points - however, to speed
		       the code up, I return after clar, clor calculated for original point,
		       so no other data available*/
    double pla[4];
    double plo[4];    /* pla  = array of geocentric latitude and
                         plo  = array of geocentric longitudes for the CGM poles
                         in the Northern and Southern hemispheres at a given
                         altitude (indices 1 and 2) and then at the Earth's
                         surface - 1-Re or zero altitude - (indices 3 and 4) */
			 
    /* init arrays to keep Purify happy */
    int i = 0;
    for (i=0; i<44; i++)
        dat[i] = 0.0;
    for (i=0; i<4; i++)
    {
        pla[i] = 0.0;
	plo[i] = 0.0;
    }
    
    
    mid_time = (inputArr[0] + inputArr[1])/2.0;
    
    gdlat  = inputArr[2];
    glon   = inputArr[3];
    gdalt  = inputArr[4];
    
    /* force glon to between -180 and 180 */
    while (glon < -180.0) glon += 360.0;
    while (glon > +180.0) glon -= 360.0;

    
    /* get the point geocentric coords needed by geocgm01_ */
    convrt(1, &gdlat, &gdalt, &gclat, &rkm);
    
    /* convert rkm to height */
    rkm = rkm - 6378.16;
    
    /* write gclat and glon into dat */
    dat[0] = gclat;
    dat[1] = glon;
    
    dinvmadptr(mid_time, &iyr, &imd, &ihm, &ics);

    /* call geocgm01 */
    GEOCGM01_F77(&geoToCgmIndicator, &iyr, &rkm, dat, pla, plo);
    
    /* CGM_LAT */
    outputArr[0] = dat[2];
    /* CGM_LONG */
    outputArr[1] = dat[3];
    /* force long to be between -180 and 180 if between 180 and 360 */
    if (outputArr[1] > 180.0 && outputArr[1] <= 360.0)
        outputArr[1] = outputArr[1] - 360.0;
    
    /* check that data successfully found */
    if (outputArr[0] < -90.01 || outputArr[0] > 90.01 || outputArr[1] < -180.01 || outputArr[0] > 180.01)
    {
        outputArr[0] =  missing;
        outputArr[1] =  missing;
    }
    
    return (0);
}


/***********************************************************************
*
* getTsygan   derives field line crossing points using Tsyganenko model.
*
*    Finds the crossing point of any given field line determined by
*    the input time and point of either the dipole equatorial plane (that
*    is, the XY plane of the GSM coordinate system), or of the equatorial plane (that
*    is, the XY plane of the GSE coordinate system).  Returns the crossing point
*    of the GSM XY plane as TSYG_EQ_XGSM and TSYG_EQ_YGSM (ZGSM is by definition zero),
*    and of the GSE XY plane as TSYG_EQ_XGSE and TSYG_EQ_YGSE (ZGSE is by definition zero).
*
*   arguments: 
*      inCount (num inputs) = 5 (UT1, UT2, GDLAT, GLON, GDALT)
*      inputArr - double array holding:
*                 UT1 - UT at record start
*                 UT2 - UT at record end
*                 GDLAT - geodetic latitude
*                 GLON - geodetic longitude
*                 GDALT - geodetic altitude
*      outCount (num outputs) = 4 
*      outputArr - double array holding:
*                 TSYG_EQ_XGSM - X GSM value where field line crosses GSM XY plane
*                 TSYG_EQ_YGSM - Y GSM value where field line crosses GSM XY plane
*                 TSYG_EQ_XGSE - X GSE value where field line crosses GSE XY plane
*                 TSYG_EQ_YGSE - Y GSE value where field line crosses GSE XY plane
*
*   Algorithm: See Geopack_2003.f, T01_01.f
*
*   returns - 0 (successful)
*/
int getTsygan(int inCount, 
              double * inputArr, 
              int outCount, 
              double * outputArr, 
              FILE * errFile)
{
    double mid_time = 0.0;
    double gdlat = 0.0;
    double glon = 0.0;
    double gdalt = 0.0;
    
    /* environment parameters to look up */
    double swspd_now = 0.0;
    double swspd_1hour = 0.0;
    double imf_ygsm_now = 0.0;
    double imf_ygsm_1hour = 0.0;
    double imf_zgsm_now = 0.0;
    double imf_zgsm_1hour = 0.0;
    double swden = 0.0;
    double dst = 0.0;
    double imfArray[10];
    double timeArray[2];
    
    /* init imfArray to keep Purify happy */
    int i = 0;
    for (i=0; i<10; i++)
        imfArray[i] = 0.0;
    
    mid_time = (inputArr[0] + inputArr[1])/2.0;
    
    gdlat  = inputArr[2];
    glon   = inputArr[3];
    gdalt  = inputArr[4];
    
    /* force glon to between -180 and 180 */
    while (glon < -180.0) glon += 360.0;
    while (glon > +180.0) glon -= 360.0;
    
    /* get imf data from an hour ago */
    timeArray[0] = mid_time - (3600.0);
    timeArray[1] = mid_time - (3600.0);
    
    getImf(2, timeArray, 10, imfArray, errFile);
    
    /* set values from imfArray (convert imf to nT) */
    if (imfArray[1] != missing &&
        imfArray[2] != missing &&
	imfArray[8] != missing)
    {
        imf_ygsm_1hour = imfArray[1] * 1.0E9;
        imf_zgsm_1hour = imfArray[2] * 1.0E9;
        swspd_1hour = imfArray[8];
    }
    else
    {
        /* missing data */
	outputArr[0] = missing;
	outputArr[1] = missing;
	outputArr[2] = missing;
	outputArr[3] = missing;
	return(0);
    }
    
    /* get imf data from now */
    timeArray[0] = mid_time;
    timeArray[1] = mid_time;

    
    getImf(2, timeArray, 10, imfArray, errFile);
    
    /* set values from imfArray */
    if (imfArray[1] != missing &&
        imfArray[2] != missing &&
	imfArray[8] != missing &&
	imfArray[7] != missing)
    {
        imf_ygsm_now = imfArray[1] * 1.0E9;
        imf_zgsm_now = imfArray[2] * 1.0E9;
        swspd_now = imfArray[8];
        swden = imfArray[7];
    }
    else
    {
        /* missing data */
	outputArr[0] = missing;
	outputArr[1] = missing;
	outputArr[2] = missing;
	outputArr[3] = missing;
	return(0);
    }
    
    /* get dst data */
    getDst(2, timeArray, 1, &dst, errFile);
    if (dst == missing)
    {
        /* missing data */
	outputArr[0] = missing;
	outputArr[1] = missing;
	outputArr[2] = missing;
	outputArr[3] = missing;
	return(0);
    }

    
    getTsyganenkoField(mid_time,
                       gdlat,
		       glon,
		       gdalt,
		       swspd_now,
		       swspd_1hour,
		       imf_ygsm_now,
		       imf_ygsm_1hour,
		       imf_zgsm_now,
		       imf_zgsm_1hour,
		       swden,
		       dst,
		       outputArr,
		       outputArr + 1,
		       outputArr + 2,
		       outputArr + 3);
    
    return (0);
}


/***********************************************************************
*
* getAacgm   derives AACGM (adjusted altitude corrected geomagnetic) or PACE coordinates.
*
*    Finds AACGM (adjusted altitude corrected geomagnetic) or PACE lat and lon
*    given geodetic lat and lon.
*
*   arguments: 
*      inCount (num inputs) = 3 (GDLAT, GLON, GDALT)
*      inputArr - double array holding:
*                 GDLAT - geodetic latitude
*                 GLON - geodetic longitude
*                 GDALT - geodetic altitude
*      outCount (num outputs) = 2 
*      outputArr - double array holding:
*                 AACGM (adjusted altitude corrected geomagnetic) or PACE latitude
*                 AACGM (adjusted altitude corrected geomagnetic) or PACE longitude
*
*   Algorithm: See sfc_convert_geo_coord.f
*
*   returns - 0 (successful)
*/
int getAacgm(int inCount, 
             double * inputArr, 
             int outCount, 
             double * outputArr, 
             FILE * errFile)
{
    double gdlat = 0.0;
    double glon = 0.0;
    double gdalt = 0.0;
    double gclat = 0.0;
    double rkm = 0.0;
    /* output */
    double cglat = 0.0;
    double cglon = 0.0;
    int imod = 1; /* signals conversion direction */
    int i_error = 0;
    
    
    gdlat  = inputArr[0];
    glon   = inputArr[1];
    gdalt  = inputArr[2];   
    
    /* force glon to between 0 and 360 */
    while (glon < 0.0) glon += 360.0;
    while (glon > +360.0) glon -= 360.0;
    
    /* get geocentric coordinates from geodetic */
    CONVRT_F77(&imod, &gdlat, &gdalt, &gclat, &rkm);
    
    CONVERT_GEO_COORD_F77(&gclat, &glon, &gdalt, &cglat, &cglon, &imod, &i_error);

    /* get success */
    if (i_error != 0)
    {
	outputArr[0] = missing;
	outputArr[1] = missing;
    }
    else
    {
        outputArr[0] = cglat;
	outputArr[1] = cglon;
    }

    
    
    return (0);
}


/***********************************************************************
*
* getEregion   derives parameters associated with field lines intercepting the E region.
*
*    Finds 6 parameters related to where the magnetic field line associated with a given
*      input point intersects the E region.
*
*   inCount (num inputs) = 5 (UT1, UT2, GDLAT, GLON, GDALT)
*      inputArr - double array holding:
*                 UT1 - UT at record start
*                 UT2 - UT at record end
*                 GDLAT - geodetic latitude
*                 GLON - geodetic longitude
*                 GDALT - geodetic altitude
*      outCount (num outputs) = 6 
*      outputArr - double array holding:
*                      E_REG_S_LAT - The latitude of the southern point where the magnetic 
*                                    field line defined by the input point hits the  E region (100 km)
*                      E_REG_S_LON - The longitude of the southern point where the magnetic 
*                                    field line defined by the input point hits the E region (100 km)
*                      E_REG_S_SDWHT - The shadow height of the southern point where the magnetic field 
*                                      line defined by the input point hits the E region (100 km).  
*                                      Shadow height is the altitude of the lowest point on the line of 
*                                      constant geodetic lat and lon in sunlight.
*                      E_REG_N_LAT - The latitude of the northern point where the magnetic 
*                                    field line defined by the input point hits the  E region (100 km)
*                      E_REG_N_LON - The longitude of the northern point where the magnetic 
*                                    field line defined by the input point hits the E region (100 km)
*                      E_REG_N_SDWHT - The shadow height of the northern point where the magnetic field 
*                                      line defined by the input point hits the E region (100 km).  
*                                      Shadow height is the altitude of the lowest point on the line of 
*                                      constant geodetic lat and lon in sunlight.
*                 
*
*   Algorithm: Uses traceMagneticField
*
*   returns - 0 (successful)
*/
int getEregion(int inCount, 
               double * inputArr, 
               int outCount, 
               double * outputArr, 
               FILE * errFile)
{
    double mid_time = 0.0;
    double gdlat = 0.0, end_gdlat = 0.0;
    double glon = 0.0, end_glon = 0.0;
    double gdalt = 0.0, end_gdalt = 0.0;
    int imd, ihm, ics;
    int year, month, day, hour, minute, second;
    int i;
    int result;
    int model = 1; /* tells traceMagneticField to use IGRF */
    int qualifier; /* 1 for north altitude intercept, 2 for south */
    double stopAlt = 100.0; /* E region altitude */
    
    mid_time = (inputArr[0] + inputArr[1])/2.0;
    
    gdlat  = inputArr[2];
    glon   = inputArr[3];
    gdalt  = inputArr[4];
    
    /* force glon to between -180 and 180 */
    while (glon < -180.0) glon += 360.0;
    while (glon > +180.0) glon -= 360.0;
    
    dinvmadptr(mid_time, &year, &imd, &ihm, &ics);
    month = imd / 100;
    day = imd - (month*100);
    hour = ihm / 100;
    minute = ihm - (hour*100);
    second = ics / 100;
    
    /* if gdalt < 100.0, set to missing */
    if (gdalt < 100.0)
    {
        for (i=0; i<6; i++)
	    outputArr[i] = missing;
	return(0);
    }
    
    /* first find southern data */
    qualifier = 2; 
    result = traceMagneticField(year, month, day,
		                hour, minute, second,
                                gdlat, glon, gdalt,
		                model, qualifier, stopAlt,
		                &end_gdlat, &end_glon, &end_gdalt);
				
    if (result)
    {
        for (i=0; i<6; i++)
	    outputArr[i] = missing;
	return(0);
    }
    
    outputArr[0] = end_gdlat;
    outputArr[1] = end_glon;
    outputArr[2] = shadowheight(mid_time, end_gdlat, end_glon);
    
    /* next find northern data */
    qualifier = 1; 
    result = traceMagneticField(year, month, day,
		                hour, minute, second,
                                gdlat, glon, gdalt,
		                model, qualifier, stopAlt,
		                &end_gdlat, &end_glon, &end_gdalt);
				
    if (result)
    {
        for (i=0; i<6; i++)
	    outputArr[i] = missing;
	return(0);
    }
    
    outputArr[3] = end_gdlat;
    outputArr[4] = end_glon;
    outputArr[5] = shadowheight(mid_time, end_gdlat, end_glon);
    
    return(0);
}


/***********************************************************************
*
* getAspect   derives magnetic aspect angle of radar beam and magnetic field
*
*   inCount (num inputs) = 8 (UT1, UT2, GDLATR, GDLONR, GALTR,
                              AZM, ELM, RANGE)
*      inputArr - double array holding:
*                 UT1 - UT at record start
*                 UT2 - UT at record end
*                 GDLATR - radar geodetic latitude
*                 GDLONR - radar geodetic longitude
*                 GALTR - radar geodetic altitude
*                 AZM - median azimuth
*                 ELM - median elevation
*                 RANGE - range to point in km
*      outCount (num outputs) = 1 
*      outputArr - double array holding:
*                      ASPECT - Magnetic aspect angle
*
*   returns - 0 (successful)
*/
int getAspect(int inCount, 
              double * inputArr, 
              int outCount, 
              double * outputArr, 
              FILE * errFile)
{
    double mid_time = 0.0;
    double gdlatr = 0.0;
    double gdlonr = 0.0;
    double galtr = 0.0;
    double azm = 0.0, elm = 0.0, range = 0.0;
    double sr = 0.0, slatgc = 0.0;
    int imd, ihm, ics;
    int year, month;
    double fyear = 0.0;
    double aspect, caspect, b;
    double gdlat, glon, gdalt;
    int i = 1;
    
    mid_time = (inputArr[0] + inputArr[1])/2.0;
    
    gdlatr = inputArr[2];
    gdlonr = inputArr[3];
    galtr  = inputArr[4];
    azm    = inputArr[5];
    elm    = inputArr[6];
    range  = inputArr[7];
    
    dinvmadptr(mid_time, &year, &imd, &ihm, &ics);
    month = imd / 100;
    fyear = year + (month-1)/12.0; /* close enough time for IGRF */ 
    
    /* get geocentric location */
    CONVRT_F77(&i, &gdlatr, &galtr, &slatgc, &sr);
    
    GASPCT_F77(&gdlatr, &gdlonr, &sr, &slatgc, &fyear, &azm, &elm, &range, &gdlat, &glon,
            &gdalt, &b, &caspect, &aspect);
    
    outputArr[0] = aspect;
    
    return(0);
}


/***********************************************************************
*
* getSltc   derives SLTC (local solar time at magnetic conjugate point in hours)
*
*   arguments: 
*      inCount (num inputs) = 3 (UT1, UT2, MAGCONJLON)
*      inputArr - double array holding:
*                 UT1 - UT at record start
*                 UT2 - UT at record end
*                 MAGCONJLON - magnetic conjugate longitude
*      outCount (num outputs) = 1 (SLTC)
*      outputArr - double array holding:
*                 SLTC - Local solar time at magnetic conjugate in hours (0-24)
*
*   Algorithm: Add 3600*24*MAGCONJLON/360 to average UT - convert to hours, where 
*              MAGCONJLON goes from -180 to 180
*
*   returns - 0 (successful)
*/
int getSltc(int inCount, 
            double * inputArr, 
            int outCount, 
            double * outputArr, 
            FILE * errFile)
{
    int iyr = 0, imd = 0, ihm = 0, ics = 0;
    double mid_time = 0.0;
    int hour = 0;
    int min = 0;
    
    mid_time = (inputArr[0] + inputArr[1])/2.0;
    
    mid_time += (3600.0*24.0/360.0)*inputArr[2];
    
    dinvmadptr(mid_time, &iyr, &imd, &ihm, &ics);
    
    hour = ihm/100;
    min  = ihm - 100*hour;
    
    outputArr[0] = (double)hour;
    outputArr[0] += (double)min/60.0;
    outputArr[0] += (double)ics/360000.0;

    return (0);
}


/***********************************************************************
*
* getAplt   derives APLT (local solar time at magnetic apex point in hours)
*
*   arguments: 
*      inCount (num inputs) = 3 (UT1, UT2, APLON)
*      inputArr - double array holding:
*                 UT1 - UT at record start
*                 UT2 - UT at record end
*                 APLON - magnetic apex longitude
*      outCount (num outputs) = 1 (APLT)
*      outputArr - double array holding:
*                 APLT - Local solar time at magnetic apex in hours (0-24)
*
*   Algorithm: Add 3600*24*APLON/360 to average UT - convert to hours, where 
*              MAGCONJLON goes from -180 to 180
*
*   returns - 0 (successful)
*/
int getAplt(int inCount, 
            double * inputArr, 
            int outCount, 
            double * outputArr, 
            FILE * errFile)
{
    int iyr = 0, imd = 0, ihm = 0, ics = 0;
    double mid_time = 0.0;
    int hour = 0;
    int min = 0;
    
    mid_time = (inputArr[0] + inputArr[1])/2.0;
    
    mid_time += (3600.0*24.0/360.0)*inputArr[2];
    
    dinvmadptr(mid_time, &iyr, &imd, &ihm, &ics);
    
    hour = ihm/100;
    min  = ihm - 100*hour;
    
    outputArr[0] = (double)hour;
    outputArr[0] += (double)min/60.0;
    outputArr[0] += (double)ics/360000.0;

    return (0);
}


/***********************************************************************
*
* getSZenc   derives Solar zenith angle at magnetic conjugate in deg (0 = overhead)
*
*   arguments: 
*      inCount (num inputs) = 4 (UT1, UT2, MAGCONJLAT, MAGCONJLON)
*      inputArr - double array holding:
*                 UT1 - UT at record start
*                 UT2 - UT at record end
*                 MAGCONJLAT - magnetic conjugate geodetic latitude
*                 MAGCONJLON - magnetic conjugate longitude
*      outCount (num outputs) = 1 (SZENC)
*      outputArr - double array holding:
*                 SZEN - Solar zenith angle at magnetic conjugate in deg (0 = overhead)
*
*   Algorithm: See solarzen in geometry.c
*
*   returns - 0 (successful)
*/
int getSZenc(int inCount, 
             double * inputArr, 
             int outCount, 
             double * outputArr, 
             FILE * errFile)
{
    double mid_time = 0.0;
    double gdlat = 0.0;
    double glon = 0.0;
    double szen = 0.0;
    double saz = 0.0;
    
    mid_time = (inputArr[0] + inputArr[1])/2.0;
    gdlat = inputArr[2];
    glon  = inputArr[3];
    
    /* force glon to between -180 and 180 */
    while (glon < -180.0) glon += 360.0;
    while (glon > +180.0) glon -= 360.0;
    
    solarzen_az(mid_time, gdlat, glon, &szen, &saz);
    
    outputArr[0] = szen;

    return(0);
}


/***********************************************************************
*
* getConjSun  derives sunset, sunrise, and shadow height for the magnetic conjugate 
*             point in space and time for that day UT.
*
*   arguments: 
*      inCount (num inputs) = 5 (UT1, UT2, MAGCONJLAT, MAGCONJLON, GDALT)
*      inputArr - double array holding:
*                 UT1 - UT at record start
*                 UT2 - UT at record end
*                 MAGCONJLAT - magnetic conjugate latitude
*                 MAGCONJLON - magnetic conjugate longitude
*                 GDALT - geodetic altitude in km
*      outCount (num outputs) = 5 (CONJ_SUNRISE, CONJ_SUNSET, CONJ_SUNRISE_H, CONJ_SUNSET_H, MAGCONJSDWHT)
*      outputArr - double array holding:
*                 CONJ_SUNRISE - time (UT) that day of sunrise at magnetic conjugate
*                 CONJ_SUNSET - time (UT) that day of sunset at magnetic conjugate
*                 CONJ_SUNRISE_H - time (in hours UT) that day of sunrise at magnetic conjugate
*                 CONJ_SUNSET_H - time (in hours UT) that day of sunset at magnetic conjugate
*                 MAGCONJSDWHT - shadow height at that time at magnetic conjugate
*
*   Algorithm: See sunrise_set, shadowheight in geometry.c
*
*   returns - 0 (successful)
*/
int getConjSun(int inCount, 
               double * inputArr, 
               int outCount, 
               double * outputArr, 
               FILE * errFile)
{
    double mid_time = 0.0;
    double gdlat = 0.0;
    double glon = 0.0;
    double gdalt = 0.0;
    double sunrise = 0.0;
    double sunset = 0.0;
    double sunrise_h = 0.0;
    double sunset_h = 0.0;
    int iyr = 0, imd = 0, ihm = 0, ics = 0;
    int hour = 0, min = 0;
    
    mid_time = (inputArr[0] + inputArr[1])/2.0;
    gdlat = inputArr[2];
    glon  = inputArr[3];
    gdalt = inputArr[4];
    
    /* force glon to between -180 and 180 */
    while (glon < -180.0) glon += 360.0;
    while (glon > +180.0) glon -= 360.0;

    sunrise_set(mid_time,
                gdlat, 
                glon,
                gdalt,
                &sunrise,
                &sunset);
                
    if (sunrise != missing)
    {
        /* get sunrise in hours UT */
        dinvmadptr(sunrise, &iyr, &imd, &ihm, &ics);
        hour = ihm/100;
        min = ihm - hour*100;
        sunrise_h = (double)hour;
        sunrise_h += (double)min/60.0;
        sunrise_h += (double)ics/360000.0;
        outputArr[0] = sunrise;
        outputArr[2] = sunrise_h;
    }
    else
    {
        outputArr[0] = missing;
        outputArr[2] = missing;
    }
    
    if (sunset != missing)
    {
        /* get sunset in hours UT */
        dinvmadptr(sunset, &iyr, &imd, &ihm, &ics);
        hour = ihm/100;
        min = ihm - hour*100;
        sunset_h = (double)hour;
        sunset_h += (double)min/60.0;
        sunset_h += (double)ics/360000.0;
        outputArr[1] = sunset;
        outputArr[3] = sunset_h;
    }
    else
    {
        outputArr[1] = missing;
        outputArr[3] = missing;
    }
    
    /* get shadow height at mag conj point */
    outputArr[4] = shadowheight(mid_time, gdlat, glon);
    
    return (0);
}


/***********************************************************************
*
* getGeo   derives geophysical parameter Kp, Ap3, Ap, f10.7, and fbar
*          given a time
*
*   arguments: 
*      inCount (num inputs) = 2 (UT1, UT2)
*      inputArr - double array holding:
*                 UT1 - UT at record start
*                 UT2 - UT at record end
*      outCount (num outputs) = 5 (KP, AP3, AP, F10.7, and FBAR)
*      outputArr - double array holding:
*                 KP - Kp Index
*                 AP3 - ap index (3-hourly)
*                 AP - AP index (daily)
*                 F10.7 - solar flux observed (Ottawa)
*                 FBAR - Multiday average observed (Ott)
*
*   Algorithm: Get data from geo500101g.002 at average UT.  Only
*              the first time any thread calls this method will the
*              data file be read into static variable geoDayArr.  Array
*              of GeoDay structs is used since it has a more compact
*              memory footprint than the madrec data structure, and
*              geo500101g.002 is a large file.  The old style file
*              geo500101g.001 will be used if geo500101g.002 is not found.
*
*   returns - 0 if successful, -1 if problem finding either file
*/
int getGeo(int inCount, 
           double * inputArr, 
           int outCount, 
           double * outputArr, 
           FILE * errFile)
{
    /* hard-coded path to geophysical files - note that we want to use the */
    /* file in standard Cedar format (geo500101g.002), but if not there,   */
    /* will try to use the old file (geo500101g.001)                       */
    static const char * geofile = "/experiments/1950/gpi/01jan50/geo500101g.002";
    
    static const char * geofile_old = "/experiments/1950/gpi/01jan50/geo500101g.001";
    
    /* hard coded parameter codes */
    static const int Kp_parcode   = 310;
    static const int Ap3_parcode  = 335;
    static const int Ap_parcode   = 340;
    static const int F107_parcode = 354;
    static const int Fbar_parcode = 356;
    
    Madrec * madrecp = NULL;
    
    /* static data so that file is only loaded once */
    static GeoDay * geoDayArr = NULL;       /* pointer to in-memory version of file */
    static int usingOldStyle = 0;
    static int numDaysInFile = 0;
    static double kp_scale = 0.0;
    static double ap3_scale = 0.0;
    static double ap_scale = 0.0;
    static double f107_scale = 0.0;
    static double fbar_scale = 0.0;
    
    char filename[1000] = "";
    int stat = 0;
    int index  = 0;  /* tell which of 8 daily readings to use */
    int iyr = 0, imd = 0, ihm = 0, ics = 0;
    double aveUt = 0.0;

    /* time variables */
    struct tm *timeNow; 
    int jdaynoToday = 0;
    int jdayno1950  = 0;
    int totalDays = 0;
    time_t secNow;
    
    /* binary search variables */
    int low = 0, mid = 0, high = 0;
    int keyFound = 0;
    double midvalue = 0.0; 
    
    aveUt = (inputArr[0] + inputArr[1])/2.0;
    
    /* for thread safety, get the geo_mutex on the way in */
    pthread_mutex_lock(&geo_mutex);
    
    /* check if data needs to be loaded */
    if (geoDayArr == NULL)
    {
        /* create filename */
        cedarGetMadroot(filename);
        strcat(filename, geofile);
        
        /* malloc an array large enough to hold enough data */
        /* to go from 1/1/1950 to today                     */
        /* First get today's Julian day number              */
        secNow = time(NULL);
        timeNow = gmtime(&secNow);
        /* note that tm_mon is 0-11 - add 1 to convert to 1-12 */
        jdaynoToday = jday(timeNow->tm_mday,
                           (timeNow->tm_mon) + 1,
                           (timeNow->tm_year) + 1900);
        jdayno1950 = jday(1,1,1950);
        totalDays = 1 + (jdaynoToday - jdayno1950);
        
        /* malloc geoDayArr */
        if ((geoDayArr = (GeoDay *)malloc(sizeof(GeoDay)*(totalDays)))==0)
        {
            perror("malloc");
            exit(-1);
        }
        
        
        /* Create a madrec object */
        madrecp = madrecCreate();
        if (strcmp(NO_ERR_STR, madrecGetError(madrecp)))
        {
            fprintf(errFile, "%s\n", madrecGetError(madrecp));
            fflush(errFile);
            outputArr[0] = missing;
            outputArr[1] = missing;
            outputArr[2] = missing;
            outputArr[3] = missing;
            outputArr[4] = missing;
            madrecp = NULL;
            /* release mutex */
            pthread_mutex_unlock(&geo_mutex);
            return (-1);
        }

        /* Read the parameter code table */
        cedarReadParCodes();

        /* Connect the madrec object to a madrigal file for sequential read*/
        madrecOpen(madrecp, 1, filename);
        if (strcmp(NO_ERR_STR, madrecGetError(madrecp)))
        {
            /* error opening file, try opening old style */
            madrecDestroy(madrecp);
            madrecp = NULL;
            madrecp = madrecCreate();
            cedarGetMadroot(filename);
            strcat(filename, geofile_old);
            usingOldStyle = 1;
            madrecOpen(madrecp, 1, filename);
            if (strcmp(NO_ERR_STR, madrecGetError(madrecp)))
            {
                fprintf(errFile, "Problem opening geofile %s: %s\n", filename, madrecGetError(madrecp));
                fflush(errFile);
                madrecDestroy(madrecp);
                outputArr[0] = missing;
                outputArr[1] = missing;
                outputArr[2] = missing;
                outputArr[3] = missing;
                outputArr[4] = missing;
                madrecp = NULL;
                /* release mutex */
                pthread_mutex_unlock(&geo_mutex);
                return (-1);
            }
         }
         
         if (usingOldStyle == 0)
         {
             /* load geo500101g.002 into geoDayArr */
             /* loop through eight records at a time */
             index = 0;
             while ((stat=madrecGetNextRec(madrecp)) == 0) 
             {
                 /* skip HEADER or CATALOG records */
                 if (!isDataRecord(madrecp->recordp))
                     continue;
                 if (index == 0)
                 {
                     /* start new geoDay */
                     numDaysInFile++;
                     assert(totalDays >= numDaysInFile);
                     geoDayArr[numDaysInFile - 1].key = cedarGetStartIndex(madrecp->recordp);
                 }
                 /* load data from this record */
                 geoDayArr[numDaysInFile - 1].geo3hour[index].ap = cedarGet1dInt(madrecp->recordp, Ap_parcode);
                 geoDayArr[numDaysInFile - 1].geo3hour[index].f107 = cedarGet1dInt(madrecp->recordp, F107_parcode);
                 geoDayArr[numDaysInFile - 1].geo3hour[index].fbar = cedarGet1dInt(madrecp->recordp, Fbar_parcode);
                 geoDayArr[numDaysInFile - 1].geo3hour[index].kp = cedarGet1dInt(madrecp->recordp, Kp_parcode);
                 geoDayArr[numDaysInFile - 1].geo3hour[index].ap3 = cedarGet1dInt(madrecp->recordp, Ap3_parcode);
                 
                 index++;
                 index = index % 8;
             }
         }
         
         else
         {
             /* load  old-style geo500101g.001 into geoDayArr */

             while ((stat=madrecGetNextRec(madrecp)) == 0) 
             {
                 /* skip HEADER or CATALOG records */
                 if (!isDataRecord(madrecp->recordp))
                     continue;
                     
                 numDaysInFile++;
                 assert(totalDays >= numDaysInFile);
                 geoDayArr[numDaysInFile - 1].key = cedarGetStartIndex(madrecp->recordp);

                 /* load data from this record */
                 for (index = 0; index < 8; index++)
                 {
                     geoDayArr[numDaysInFile - 1].geo3hour[index].ap = cedarGet2dIntValue(madrecp->recordp, Ap_parcode, index);
                     geoDayArr[numDaysInFile - 1].geo3hour[index].f107 = cedarGet2dIntValue(madrecp->recordp, F107_parcode, index);
                     geoDayArr[numDaysInFile - 1].geo3hour[index].fbar = cedarGet2dIntValue(madrecp->recordp, Fbar_parcode, index);
                     geoDayArr[numDaysInFile - 1].geo3hour[index].kp = cedarGet2dIntValue(madrecp->recordp, Kp_parcode, index);
                     geoDayArr[numDaysInFile - 1].geo3hour[index].ap3 = cedarGet2dIntValue(madrecp->recordp, Ap3_parcode, index);
                 }
             }
         
         }
         
         /* get scaling factors, since we're storing data as Int16's */
         kp_scale = cedarGetParScaleFactor(Kp_parcode);
         ap3_scale = cedarGetParScaleFactor(Ap3_parcode);
         ap_scale = cedarGetParScaleFactor(Ap_parcode);
         f107_scale = cedarGetParScaleFactor(F107_parcode);
         fbar_scale = cedarGetParScaleFactor(Fbar_parcode);
         
         /* release file, since now read into geoDayArr */
	 madrecClose(madrecp);
         madrecDestroy(madrecp);
         
    } /* data now loaded */
    
    /* release mutex */
    pthread_mutex_unlock(&geo_mutex);
    
    /* find right record using a binary search */
    low = 0;
    high = numDaysInFile - 1;
    keyFound = 0;
    while (low <= high)
    {
        mid = (low+high)/2;
        midvalue = geoDayArr[mid].key;
        if (aveUt >= midvalue && aveUt <= midvalue + SEC_IN_DAY)
        {
            /* right GeoDay found */
            keyFound = 1;
            break;
        }
        else if (aveUt < midvalue)
            high = mid - 1;
        else
            low = mid + 1;
    }
    if (keyFound == 0)
    {
        outputArr[0] = missing;
        outputArr[1] = missing;
        outputArr[2] = missing;
        outputArr[3] = missing;
        outputArr[4] = missing;
        return(0);
    }
    
    
    
    /* Since geoDay contains 8 3 hour readings for Kp and Ap3, we need    */
    /* to determine which of the 8 to use - get hhmm from  dinvmadptr     */
    dinvmadptr(aveUt, &iyr, &imd, &ihm, &ics);
    /* index will be 0-7 */
    index = ihm/300;
    
    /* kp */
    if (geoDayArr[mid].geo3hour[index].kp == missingData)
        outputArr[0] = missing;
    else
        outputArr[0] = kp_scale * geoDayArr[mid].geo3hour[index].kp;
    
    /* ap3 */
    if (geoDayArr[mid].geo3hour[index].ap3 == missingData)
        outputArr[1] = missing;
    else
        outputArr[1] = ap3_scale * geoDayArr[mid].geo3hour[index].ap3;
    
    /* ap */
    if (geoDayArr[mid].geo3hour[index].ap == missingData)
        outputArr[2] = missing;
    else
        outputArr[2] = ap_scale * geoDayArr[mid].geo3hour[index].ap;
    
    /* f10.7 */
    if (geoDayArr[mid].geo3hour[index].f107 == missingData)
        outputArr[3] = missing;
    else
        outputArr[3] = f107_scale * geoDayArr[mid].geo3hour[index].f107;
    
    /* fbar */
    if (geoDayArr[mid].geo3hour[index].fbar == missingData)
        outputArr[4] = missing;
    else
        outputArr[4] = fbar_scale * geoDayArr[mid].geo3hour[index].fbar;
    
    
    return (0);
}


/***********************************************************************
*
* getDst   derives geophysical parameter Dst given a time
*
*   arguments: 
*      inCount (num inputs) = 2 (UT1, UT2)
*      inputArr - double array holding:
*                 UT1 - UT at record start
*                 UT2 - UT at record end
*      outCount (num outputs) = 1 (DST)
*      outputArr - double array holding:
*                 DST - DST index in nT
*
*   Algorithm: Get data from dst570101g.002 at average UT.  Only
*              the first time any thread calls this method will the
*              data file be read into static variable dstDayArr. Array
*              of DstDay structs is used since it has a more compact
*              memory footprint than the madrec data structure, and
*              dst570101g.002 is a large file.  The old style file
*              dst570101g.001 will be used if dst570101g.002 is not found.
*
*   returns - 0 if successful, -1 if problem finding file
*/
int getDst(int inCount, 
           double * inputArr, 
           int outCount, 
           double * outputArr, 
           FILE * errFile)
{
    /* hard-coded paths to dst files - note that we want to use the        */
    /* file in standard Cedar format (dst570101g.002), but if not there,   */
    /* will try to use the old file (dst570101g.001)                       */
    static const char * dstfile = "/experiments/1957/dst/01jan57/dst570101g.002";
    
    static const char * dstfile_old = "/experiments/1957/dst/01jan57/dst570101g.001";
    
    /* hard coded parameter code */
    static const int Dst_parcode   = 330;
    static int usingOldStyle = 0;
    static DstDay * dstDayArr = NULL;       /* pointer to in-memory version of file */
    static int numDaysInFile = 0;
    static double dst_scale = 0.0;
    
    
    Madrec * madrecp = NULL;
    char filename[1000] = "";
    int stat = 0;
    int index  = 0;  /* tell which of 24 daily readings to use */
    int iyr = 0, imd = 0, ihm = 0, ics = 0;
    double aveUt = 0.0;
    
    /* time variables */
    struct tm *timeNow; 
    int jdaynoToday = 0;
    int jdayno1957  = 0;
    int totalDays = 0;
    time_t secNow;
    
    /* binary search variables */
    int low = 0, mid = 0, high = 0;
    int keyFound = 0;
    double midvalue = 0.0; 
    
    aveUt = (inputArr[0] + inputArr[1])/2.0;
    
    /* for thread safety, get the dst_mutex on the way in */
    pthread_mutex_lock(&dst_mutex);
    
    /* check if data needs to be loaded */
    if (dstDayArr == NULL)
    {
        /* create filename */
        cedarGetMadroot(filename);
        strcat(filename, dstfile);
        
        /* malloc an array large enough to hold enough data */
        /* to go from 1/1/1957 to today                     */
        /* First get today's Julian day number              */
        secNow = time(NULL);
        timeNow = gmtime(&secNow);
        /* note that tm_mon is 0-11 - add 1 to convert to 1-12 */
        jdaynoToday = jday(timeNow->tm_mday,
                           (timeNow->tm_mon) + 1,
                           (timeNow->tm_year) + 1900);
        jdayno1957 = jday(1,1,1957);
        totalDays = 1 + (jdaynoToday - jdayno1957);
        
        /* malloc dstDayArr */     
        if ((dstDayArr = (DstDay *)malloc(sizeof(DstDay)*(totalDays)))==0)
        {
            perror("malloc");
            exit(-1);
        }
        
        
        /* Create a madrec object */
        madrecp = madrecCreate();
        if (strcmp(NO_ERR_STR, madrecGetError(madrecp)))
        {
            fprintf(errFile, "%s\n", madrecGetError(madrecp));
            fflush(errFile);
            outputArr[0] = missing;
            madrecp = NULL;
            /* release mutex */
            pthread_mutex_unlock(&dst_mutex);
            return (-1);
        }

        /* Read the parameter code table */
        cedarReadParCodes();

        /* Connect the madrec object to a madrigal file for sequential read*/
        madrecOpen(madrecp, 1, filename);
        if (strcmp(NO_ERR_STR, madrecGetError(madrecp)))
        {
            /* error opening file, try opening old style */
            madrecDestroy(madrecp);
            madrecp = NULL;
            madrecp = madrecCreate();
            cedarGetMadroot(filename);
            strcat(filename, dstfile_old);
            usingOldStyle = 1;
            madrecOpen(madrecp, 1, filename);
            if (strcmp(NO_ERR_STR, madrecGetError(madrecp)))
            {
                fprintf(errFile, "Problem opening dstfile %s: %s\n", filename, madrecGetError(madrecp));
                fflush(errFile);
                madrecDestroy(madrecp);
                outputArr[0] = missing;
                madrecp = NULL;
                /* release mutex */
                pthread_mutex_unlock(&dst_mutex);
                return (-1);
            }
         }
         
         if (usingOldStyle == 0)
         {
             /* load dst570101g.002 into dstDayArr */
             /* loop through 24 records at a time */
             index = 0;
             while ((stat=madrecGetNextRec(madrecp)) == 0) 
             {
                 /* skip HEADER or CATALOG records */
                 if (!isDataRecord(madrecp->recordp))
                     continue;
                 if (index == 0)
                 {
                     /* start new dstDay */
                     numDaysInFile++;
                     assert(totalDays >= numDaysInFile);
                     dstDayArr[numDaysInFile - 1].key = cedarGetStartIndex(madrecp->recordp);
                 }
                 /* load data from this record */
                 dstDayArr[numDaysInFile - 1].dstHour[index] = cedarGet1dInt(madrecp->recordp, Dst_parcode);
                 
                 index++;
                 index = index % 24;
             }
         }
         
         else
         {
             /* load  old-style dst570101g.001 into dstDayArr */

             while ((stat=madrecGetNextRec(madrecp)) == 0) 
             {
                 /* skip HEADER or CATALOG records */
                 if (!isDataRecord(madrecp->recordp))
                     continue;
                     
                 numDaysInFile++;
                 assert(totalDays >= numDaysInFile);
                 dstDayArr[numDaysInFile - 1].key = cedarGetStartIndex(madrecp->recordp);

                 /* load data from this record */
                 for (index = 0; index < 24; index++)
                 {
                     dstDayArr[numDaysInFile - 1].dstHour[index] = cedarGet2dIntValue(madrecp->recordp, Dst_parcode, index);
                 }
             }
         
         }
         
         /* get scaling factors, since we're storing data as Int16's */
         dst_scale = cedarGetParScaleFactor(Dst_parcode);
         
         /* release file, since now read into dstDayArr */
	 madrecClose(madrecp);
         madrecDestroy(madrecp);

         
    } /* data now loaded */
    
    /* release mutex */
    pthread_mutex_unlock(&dst_mutex);
    
    /* find right record using a binary search */
    low = 0;
    high = numDaysInFile - 1;
    keyFound = 0;
    while (low <= high)
    {
        mid = (low+high)/2;
        midvalue = dstDayArr[mid].key;
        if (aveUt >= midvalue && aveUt <= midvalue + SEC_IN_DAY)
        {
            /* right DstDay found */
            keyFound = 1;
            break;
        }
        else if (aveUt < midvalue)
            high = mid - 1;
        else
            low = mid + 1;
    }
    if (keyFound == 0)
    {
        outputArr[0] = missing;
        return(0);
    }
    
    
    
    /* Since dstDay contains 24 1 hour readings, we need                  */
    /* to determine which of the 24 to use - get hhmm from  dinvmadptr    */
    dinvmadptr(aveUt, &iyr, &imd, &ihm, &ics);
    /* index will be 0-23 */
    index = ihm/100;
    
    /* dst */
    if (dstDayArr[mid].dstHour[index] == missingData)
        outputArr[0] = missing;
    else
        outputArr[0] = dst_scale * dstDayArr[mid].dstHour[index];
    
    return (0);
}


/***********************************************************************
*
* getFof2   derives geophysical parameter Fof2 (above particular station) given a time
*
*   arguments: 
*      inCount (num inputs) = 2 (UT1, UT2)
*      inputArr - double array holding:
*                 UT1 - UT at record start
*                 UT2 - UT at record end
*                 KINST - station kinst above which Fof2 is measured
*      outCount (num outputs) = 1 (FOF@)
*      outputArr - double array holding:
*                 FOF2 - Fof2 in MHz above station
*
*   Algorithm: For now, only works for Millstone, but can be extended
*              to work for any instrument for which data is available. For
*              kinst = 30,31,32, get data from uld761203g.002 at average UT.  Only
*              the first time any thread calls this method will the
*              data file be read into static variable fof2DayArr. Array
*              of Fof2Day structs is used since it has a more compact
*              memory footprint than the madrec data structure, and
*              uld761203g.002 is a large file.  The old style file
*              uld761203g.001 will be used if uld761203g.002 is not found.
*
*   returns - 0 if successful, -1 if problem finding file
*/
int getFof2(int inCount, 
            double * inputArr, 
            int outCount, 
            double * outputArr, 
            FILE * errFile)
{
    /* hard-coded paths to fof2 files - note that we want to use the        */
    /* file in standard Cedar format (uld761203g.002), but if not there,    */
    /* will try to use the old file (uld761203g.001)                        */
    static const char * fof2file = "/experiments/1976/uld/03dec76/uld761203g.002";
    
    static const char * fof2file_old = "/experiments/1976/uld/03dec76/uld761203g.001";
    
    /* hard coded parameter code */
    static const int Fof2_parcode   = 3369;
    static int usingOldStyle = 0;
    static Fof2Day * fof2DayArr = NULL;       /* pointer to in-memory version of file */
    static int numDaysInFile = 0;
    static double fof2_scale = 0.0;
    
    Madrec * madrecp = NULL;
    char filename[1000] = "";
    int stat = 0;
    int index = 0;               /* index of present 2D record */
    int iyr = 0, imd = 0, ihm = 0, ics = 0;
    int hour = 0, min = 0;
    double aveUt = 0.0;
    int kinst = 0;
    int i = 0;
    
    /* time variables */
    struct tm *timeNow; 
    int jdaynoToday = 0;
    int jdayno1976  = 0;
    int totalDays = 0;
    time_t secNow;
    
    /* binary search variables */
    int low = 0, mid = 0, high = 0;
    int keyFound = 0;
    double midvalue = 0.0; 
    
    aveUt = (inputArr[0] + inputArr[1])/2.0;
    kinst = (int)inputArr[2];
    
    /* if not millstone, return missing */
    /* modify this line to add other stations */
    if (kinst < 30 || kinst > 32)
    {
        outputArr[0] =  missing;
        return(0);
    }
    
    /* for thread safety, get the fof2_mutex on the way in */
    pthread_mutex_lock(&fof2_mutex);
    
    /* check if data needs to be loaded */
    if (fof2DayArr == NULL)
    {
        /* create filename */
        cedarGetMadroot(filename);
        strcat(filename, fof2file);
        
        /* malloc an array large enough to hold enough data */
        /* to go from 12/3/1976 to today                     */
        /* First get today's Julian day number              */
        secNow = time(NULL);
        timeNow = gmtime(&secNow);
        /* note that tm_mon is 0-11 - add 1 to convert to 1-12 */
        jdaynoToday = jday(timeNow->tm_mday,
                           (timeNow->tm_mon) + 1,
                           (timeNow->tm_year) + 1900);
        jdayno1976 = jday(3,12,1957);
        totalDays = 1 + (jdaynoToday - jdayno1976);
        
        /* malloc fof2DayArr */     
        if ((fof2DayArr = (Fof2Day *)malloc(sizeof(Fof2Day)*(totalDays)))==0)
        {
            perror("malloc");
            exit(-1);
        }
        
        
        /* Create a madrec object */
        madrecp = madrecCreate();
        if (strcmp(NO_ERR_STR, madrecGetError(madrecp)))
        {
            fprintf(errFile, "%s\n", madrecGetError(madrecp));
            fflush(errFile);
            outputArr[0] = missing;
            madrecp = NULL;
            /* release mutex */
            pthread_mutex_unlock(&fof2_mutex);
            return (-1);
        }

        /* Read the parameter code table */
        cedarReadParCodes();

        /* Connect the madrec object to a madrigal file for sequential read*/
        madrecOpen(madrecp, 1, filename);
        if (strcmp(NO_ERR_STR, madrecGetError(madrecp)))
        {
            /* error opening file, try opening old style */
            madrecDestroy(madrecp);
            madrecp = NULL;
            madrecp = madrecCreate();
            cedarGetMadroot(filename);
            strcat(filename, fof2file_old);
            usingOldStyle = 1;
            madrecOpen(madrecp, 1, filename);
            if (strcmp(NO_ERR_STR, madrecGetError(madrecp)))
            {
                fprintf(errFile, "Problem opening fof2file %s: %s\n", filename, madrecGetError(madrecp));
                fflush(errFile);
                madrecDestroy(madrecp);
                outputArr[0] = missing;
                madrecp = NULL;
                /* release mutex */
                pthread_mutex_unlock(&fof2_mutex);
                return (-1);
            }
         }
         
         if (usingOldStyle == 0)
         {
             /* load uld761203g.002 into fof2DayArr */
             /* loop through 48 records at a time */
	     /* note that now file may not contain 48 records */
             imd = 0;
             while ((stat=madrecGetNextRec(madrecp)) == 0) 
             {
                 /* skip HEADER or CATALOG records */
                 if (!isDataRecord(madrecp->recordp))
                     continue;
		 /* get ihm  and index*/
		 ihm = cedarGetIbhm(madrecp->recordp);
		 hour = ihm/100;
		 min = ihm - hour*100;
		 index = 2 * hour;
		 if (min >= 30)
		     index++;
                 if (imd != cedarGetIbdt(madrecp->recordp))
                 {
                     /* start new fof2Day */
		     imd = cedarGetIbdt(madrecp->recordp);
                     numDaysInFile++;
                     assert(totalDays >= numDaysInFile);
		     /* set all data to missing */
		     for (i=0; i<48; i++)
		         fof2DayArr[numDaysInFile - 1].fof2Hour[i] = missingData;
                     fof2DayArr[numDaysInFile - 1].key = cedarGetStartIndex(madrecp->recordp) - hour*3600.0 - min*60.0;
                 }
                 /* load data from this record */
                 fof2DayArr[numDaysInFile - 1].fof2Hour[index] = cedarGet1dInt(madrecp->recordp, Fof2_parcode);
             }
         }
         
         else
         {
             /* load  old-style uld761203g.001 into fof2DayArr */

             while ((stat=madrecGetNextRec(madrecp)) == 0) 
             {
                 /* skip HEADER or CATALOG records */
                 if (!isDataRecord(madrecp->recordp))
                     continue;
                     
                 numDaysInFile++;
                 assert(totalDays >= numDaysInFile);
                 fof2DayArr[numDaysInFile - 1].key = cedarGetStartIndex(madrecp->recordp);

                 /* load data from this record */
                 for (index = 0; index < 48; index++)
                 {
                     fof2DayArr[numDaysInFile - 1].fof2Hour[index] = cedarGet2dIntValue(madrecp->recordp, Fof2_parcode, index);
                 }
             }
         
         }
         
         /* get scaling factors, since we're storing data as Int16's */
         fof2_scale = cedarGetParScaleFactor(Fof2_parcode);
         
         /* release file, since now read into fof2DayArr */
	 madrecClose(madrecp);
         madrecDestroy(madrecp);

         
    } /* data now loaded */
    
    /* release mutex */
    pthread_mutex_unlock(&fof2_mutex);
    
    /* find right record using a binary search */
    low = 0;
    high = numDaysInFile - 1;
    keyFound = 0;
    while (low <= high)
    {
        mid = (low+high)/2;
        midvalue = fof2DayArr[mid].key;
        if (aveUt >= midvalue && aveUt <= midvalue + SEC_IN_DAY)
        {
            /* right Fof2Day found */
            keyFound = 1;
            break;
        }
        else if (aveUt < midvalue)
            high = mid - 1;
        else
            low = mid + 1;
    }
    if (keyFound == 0)
    {
        outputArr[0] = missing;
        return(0);
    }
    
    
    
    /* Since fof2Day contains 48 1/2 hour readings, we need               */
    /* to determine which of the 48 to use - get hhmm from  dinvmadptr    */
    dinvmadptr(aveUt, &iyr, &imd, &ihm, &ics);
    /* index will be 0-47 */
    hour = ihm/100;
    min  = ihm - hour*100;
    index = 2*hour + min/30;
    
    /* fof2 */
    if (fof2DayArr[mid].fof2Hour[index] == missingData)
        outputArr[0] = missing;
    else
        outputArr[0] = fof2_scale * fof2DayArr[mid].fof2Hour[index];

    return (0);
}


/***********************************************************************
*
* getPopl   derives Popl from log10(Pop).
*
*
*   arguments: 
*      inCount (num inputs) = 1 (POP)
*      inputArr - double array holding:
*         POP - Uncorrected electron density (Te/Ti=1) (m-3)
*      outCount (num outputs) = 1 (POPL)
*      outputArr - double array holding: 
*         POPL -  Log10(uncorrected electron density)  lg(m-3)
*
*   returns - 0 (successful)
*/
int getPopl(int inCount, 
            double * inputArr, 
            int outCount, 
            double * outputArr, 
            FILE * errFile)
{ 
    if (inputArr[0] > 0.0)
        outputArr[0] = log10(inputArr[0]);
    else
        outputArr[0] = missing;

    return(0);
}



/***********************************************************************
*
* getPop   derives Pop from 10^(Popl).
*
*
*   arguments: 
*      inCount (num inputs) = 1 (POPL)
*      inputArr - double array holding:
*         POPL -  Log10(uncorrected electron density) lg(m-3)
*      outCount (num outputs) = 1 (POP)
*      outputArr - double array holding: 
*         POP -  Uncorrected electron density (Te/Ti=1) (m-3)
*
*   returns - 0 (successful)
*/
int getPop(int inCount, 
           double * inputArr, 
           int outCount, 
           double * outputArr, 
           FILE * errFile)
{ 
    outputArr[0] = pow(10.0, inputArr[0]);

    return(0);
}


/***********************************************************************
*
* getNel   derives Nel from log10(Ne).
*
*
*   arguments: 
*      inCount (num inputs) = 1 (NE)
*      inputArr - double array holding:
*         NE - electron density (m-3) 
*      outCount (num outputs) = 1 (NEL)
*      outputArr - double array holding: 
*         NEL -  Log10(electron density) lg(m-3) 
*
*   returns - 0 (successful)
*/
int getNel(int inCount, 
            double * inputArr, 
            int outCount, 
            double * outputArr, 
            FILE * errFile)
{ 
    if (inputArr[0] > 0.0)
        outputArr[0] = log10(inputArr[0]);
    else
        outputArr[0] = missing;

    return(0);
}



/***********************************************************************
*
* getNe   derives Ne from 10^(Nel).
*
*
*   arguments: 
*      inCount (num inputs) = 1 (NEL)
*      inputArr - double array holding:
*         NEL -  Log10(electron density) lg(m-3)
*      outCount (num outputs) = 1 (NE)
*      outputArr - double array holding: 
*         NE -  electron density (m-3)
*
*   returns - 0 (successful)
*/
int getNe(int inCount, 
           double * inputArr, 
           int outCount, 
           double * outputArr, 
           FILE * errFile)
{ 
    outputArr[0] = pow(10.0, inputArr[0]);

    return(0);
}

/***********************************************************************
*
* getDNel   derives DNel from DNe.
*
*
*   arguments: 
*      inCount (num inputs) = 1 (DNE)
*      inputArr - double array holding:
*         DNE - error in electron density (m-3) 
*      outCount (num outputs) = 1 (DNEL)
*      outputArr - double array holding: 
*         DNEL -  Log10(error in electron density) lg(m-3) 
*
*   returns - 0 (successful)
*/
int getDNel(int inCount, 
            double * inputArr, 
            int outCount, 
            double * outputArr, 
            FILE * errFile)
{ 
    if (inputArr[0] > 0.0)
        outputArr[0] = log10(inputArr[0]);
    else
        outputArr[0] = missing;

    return(0);
}



/***********************************************************************
*
* getDNe   derives DNe from DNel.
*
*
*   arguments: 
*      inCount (num inputs) = 1 (DNEL)
*      inputArr - double array holding:
*         DNEL -  Log10(error in electron density) lg(m-3)
*      outCount (num outputs) = 1 (DNE)
*      outputArr - double array holding: 
*         DNE -  error in electron density (m-3)
*
*   returns - 0 (successful)
*/
int getDNe(int inCount, 
           double * inputArr, 
           int outCount, 
           double * outputArr, 
           FILE * errFile)
{ 
    outputArr[0] = pow(10.0, inputArr[0]);

    return(0);
}



/***********************************************************************
*
* getNemaxl   derives Nemaxl from log10(Nemax).
*
*
*   arguments: 
*      inCount (num inputs) = 1 (NEMAX)
*      inputArr - double array holding:
*         NEMAX - Maximum electron density (m-3)
*      outCount (num outputs) = 1 (NEMAXL)
*      outputArr - double array holding: 
*         NEMAXL -  Log10(maximum electron density) lg(m-3)
*
*   returns - 0 (successful)
*/
int getNemaxl(int inCount, 
            double * inputArr, 
            int outCount, 
            double * outputArr, 
            FILE * errFile)
{ 
    if (inputArr[0] > 0.0)
        outputArr[0] = log10(inputArr[0]);
    else
        outputArr[0] = missing;

    return(0);
}



/***********************************************************************
*
* getNemax   derives Nemax from 10^(Nemaxl).
*
*
*   arguments: 
*      inCount (num inputs) = 1 (NEMAXL)
*      inputArr - double array holding:
*         NEMAXL -  Log10(maximum electron density) lg(m-3)
*      outCount (num outputs) = 1 (NEMAX)
*      outputArr - double array holding: 
*         NEMAX -  Maximum electron density (m-3)
*
*   returns - 0 (successful)
*/
int getNemax(int inCount, 
           double * inputArr, 
           int outCount, 
           double * outputArr, 
           FILE * errFile)
{ 
    outputArr[0] = pow(10.0, inputArr[0]);

    return(0);
}


/***********************************************************************
*
* getTr   derives temperature ratio Te/Ti.
*
*
*   arguments: 
*      inCount (num inputs) = 2 (TE, TI)
*      inputArr - double array holding:
*         TE -  Electron temperature - K
*         TI -  Ion temperature - K
*      outCount (num outputs) = 1 (TR)
*      outputArr - double array holding: 
*         TR -  Temperature ratio (Te/Ti)
*
*   returns - 0 (successful)
*/
int getTr(int inCount, 
          double * inputArr, 
          int outCount, 
          double * outputArr, 
          FILE * errFile)
{
    if (inputArr[1] != 0.0)
        outputArr[0] = inputArr[0]/inputArr[1];
    else
        outputArr[0] = missing;

    return(0);
}


/***********************************************************************
*
* getTe   derives electron temperature Te from Tr*Ti.
*
*
*   arguments: 
*      inCount (num inputs) = 2 (TR, TI)
*      inputArr - double array holding:
*         TR -  Temperature ratio (Te/Ti)
*         TI -  Ion temperature - K
*      outCount (num outputs) = 1 (TE)
*      outputArr - double array holding: 
*         TE -  Electron temperature - K
*
*   returns - 0 (successful)
*/
int getTe(int inCount, 
          double * inputArr, 
          int outCount, 
          double * outputArr, 
          FILE * errFile)
{
    outputArr[0] = inputArr[0]*inputArr[1];

    return(0);
}


/***********************************************************************
*
* getTi   derives ion temperature Ti from Te/Tr.
*
*
*   arguments: 
*      inCount (num inputs) = 2 (TE, TR)
*      inputArr - double array holding:
*         TE -  Electron temperature - K
*         TR -  Temperature ratio (Te/Ti)
*      outCount (num outputs) = 1 (TI)
*      outputArr - double array holding: 
*         TI -  Ion temperature - K
*
*   returns - 0 (successful)
*/
int getTi(int inCount, 
          double * inputArr, 
          int outCount, 
          double * outputArr, 
          FILE * errFile)
{
    if (inputArr[1] != 0.0)
        outputArr[0] = inputArr[0]/inputArr[1];
    else
        outputArr[0] = missing;

    return(0);
}


/***********************************************************************
*
* getDteCctitr   derives error in electron temperature given CCTITR, TI, TR, DTI, and DTR.
*
*
*   arguments: 
*      inCount (num inputs) = 5 (CCTITR, TI, TR, DTI, DTR)
*      inputArr - double array holding:
*         CCTITR -  Ti, Tr correlation coefficient
*         TI -  Ion temperature - K
*         TR -  Temperature ratio (Te/Ti)
*         DTI -  Error in Ion temperature - K
*         DTR -  Error in Temperature ratio (Te/Ti)
*      outCount (num outputs) = 1 (DTE)
*      outputArr - double array holding: 
*         DTE -  Error in Electron temperature - K
*
*   Algorithm: DTE = SQRT(TR**2*DTI**2 + TI**2*DTR**2 + 2.0D0*TR*TI*CVTITR),
*       where CVTITR = DTI*DTE*CCTITR.
*
*   Uses Ti, Tr correlation coefficient coefficient, unlike getDte, which uses TE
*
*   returns - 0 (successful)
*/
int getDteCctitr(int inCount,
                 double * inputArr, 
                 int outCount, 
                 double * outputArr, 
                 FILE * errFile)
{
    double Cctitr,Ti,Tr,Dti,Dtr;
    
    Cctitr = inputArr[0];
    Ti = inputArr[1];
    Tr = inputArr[2];
    Dti = inputArr[3];
    Dtr = inputArr[4];
    
    /* make all data is greater than 0 */
    if (Ti <= 0.0 || Tr <= 0.0 ||
        Dti <= 0.0 || Dtr <= 0.0)
    {
        outputArr[0] = missing;
        return (0);
    }
    
    /* make sure all error values are valid */
    if (Dti == assumed || Dti == knownbad ||
        Dtr == assumed || Dtr == knownbad)
    {
        outputArr[0] = missing;
        return (0);
    }
    
    outputArr[0] = pow(Tr,2.0) * pow(Dti,2.0);
    outputArr[0] += pow(Ti,2.0) * pow(Dtr,2.0);
    outputArr[0] += 2.0 * Tr * Ti * Dti * Dtr * Cctitr;
    outputArr[0] = sqrt(outputArr[0]);

    return(0);
}



/***********************************************************************
*
* getDte   derives error in electron temperature given TE, TI, TR, DTI, and DTR.
*
*
*   arguments: 
*      inCount (num inputs) = 5 (TE, TI, TR, DTI, DTR)
*      inputArr - double array holding:
*         TE -  Electron temperature - K
*         TI -  Ion temperature - K
*         TR -  Temperature ratio (Te/Ti)
*         DTI -  Error in Ion temperature - K
*         DTR -  Error in Temperature ratio (Te/Ti)
*      outCount (num outputs) = 1 (DTE)
*      outputArr - double array holding: 
*         DTE -  Error in Electron temperature - K
*
*   Algorithm: DTE = TE * ((DTR/TR)**2 + (DTI/TI)**2)**1/2.
*
*   returns - 0 (successful)
*/
int getDte(int inCount, 
           double * inputArr, 
           int outCount, 
           double * outputArr, 
           FILE * errFile)
{
    double Te,Ti,Tr,Dti,Dtr;
    
    Te = inputArr[0];
    Ti = inputArr[1];
    Tr = inputArr[2];
    Dti = inputArr[3];
    Dtr = inputArr[4];
    
    /* make all data is greater than 0 */
    if (Te <= 0.0 || Ti <= 0.0 || Tr <= 0.0 ||
        Dti <= 0.0 || Dtr <= 0.0)
    {
        outputArr[0] = missing;
        return (0);
    }
    
    /* make sure all error values are valid */
    if (Dti == assumed || Dti == knownbad ||
        Dtr == assumed || Dtr == knownbad)
    {
        outputArr[0] = missing;
        return (0);
    }
    
    outputArr[0] = pow(Dtr/Tr,2.0) + pow(Dti/Ti,2.0);
    outputArr[0] = Te * pow(outputArr[0], 0.5);

    return(0);
}


/***********************************************************************
*
* getCol   derives Col from log10(Co).
*
*
*   arguments: 
*      inCount (num inputs) = 1 (CO)
*      inputArr - double array holding:
*         CO - Ion-neutral collision frequency - (s^-1)
*      outCount (num outputs) = 1 (COL)
*      outputArr - double array holding: 
*         COL -  Log10(Ion-neutral collision frequency) - lg(s^-1) 
*
*   returns - 0 (successful)
*/
int getCol(int inCount, 
            double * inputArr, 
            int outCount, 
            double * outputArr, 
            FILE * errFile)
{ 
    if (inputArr[0] > 0.0)
        outputArr[0] = log10(inputArr[0]);
    else
        outputArr[0] = missing;

    return(0);
}



/***********************************************************************
*
* getCo   derives Co from 10^(Col).
*
*
*   arguments: 
*      inCount (num inputs) = 1 (COL)
*      inputArr - double array holding:
*         COL -  Log10(Ion-neutral collision frequency) - lg(s^-1)
*      outCount (num outputs) = 1 (CO)
*      outputArr - double array holding: 
*         CO -  Ion-neutral collision frequency - (s^-1)
*
*   returns - 0 (successful)
*/
int getCo(int inCount, 
           double * inputArr, 
           int outCount, 
           double * outputArr, 
           FILE * errFile)
{ 
    outputArr[0] = pow(10.0, inputArr[0]);

    return(0);
}


/***********************************************************************
*
* getNeNel   derives Ne and Nel from Ti, Tr, Popl, and aspect angle.
*
*
*   arguments: 
*      inCount (num inputs) = 3 (TI, TR, POPL)
*      inputArr - double array holding:
*         TI -  Ion temperature - K
*         TR -  Temperature ratio (Te/Ti)
*         POPL -  Log10(uncorrected electron density) lg(m-3)
*         ASPECT - magnetic aspect angle (0 = up B)
*      outCount (num outputs) = 2 (NE, NEL)
*      outputArr - double array holding: 
*         NE -  Corrected electron density (m^-3)
*         NEL -  log10 of Corrected electron density lg(m^-3)
*
*   Algorithm - see getElecDensity
*
*   returns - 0 (successful)
*/
int getNeNel(int inCount, 
             double * inputArr, 
             int outCount, 
             double * outputArr, 
             FILE * errFile)
{ 
    double Nel = 0.0;
    
    Nel = getElecDensity(inputArr[0],
                         inputArr[1],
                         inputArr[2],
			 inputArr[3]);
    
    if (Nel == missing)
    {
        outputArr[0] = missing;
        outputArr[1] = missing;
    }
    else
    {
        outputArr[0] = pow(10.0, Nel);
        outputArr[1] = Nel;
    }
    return(0);
}


/***********************************************************************
*
* getVisrNe   derives Model Ne, Nel using Shunrong's model
*
*
*   arguments: 
*      inCount (num inputs) = 6 (UT1, KINST, SLT, GDALT, GDLAT, ELM)
*      inputArr - double array holding:
*         UT1 - UT at record start
*         KINST - instrument id
*         SLT - Local solar time in hours (0-24)
*         GDALT - geodetic altitude in km
*         GDLAT - geodetic latitude
*         ELM - mean elevation in degrees
*      outCount (num outputs) = 2 (NE_MODEL, NEL_MODEL)
*      outputArr - double array holding: 
*         NE_MODEL -  Model electron density (m^-3)
*         NEL_MODEL -  log10 of Model electron density lg(m^-3)
*
*   Algorithm - see Shunrong's model at http://madrigal.haystack.mit.edu/models/
*
*   returns - 0 (successful)
*/
int getVisrNe(int inCount, 
              double * inputArr, 
              int outCount, 
              double * outputArr, 
              FILE * errFile)
{
    int iyr=0, imd=0, ihm=0, ics=0;
    int month=0, day=0;
    int hour=0, min=0;
    int i;
    
    /* input parameters */
    int kinst = 0;
    int ipar = 0;
    double slt = 0.0;
    double gdalt = 0.0;
    double gdlat = 0.0;
    double elm = 0.0;
    
    /* parameters needed for input to Shunrong's model */
    double doy = 0.0;      /* day of year (1-366)     */
    double f107 = 0.0;     /* f10.7 in 10^-22 W/m2/Hz */
    double ap3 = 0.0;      /* Ap3                     */
    
    /* outputs of Shunrong's model */
    double output[4];

    /* declaration of data to be passed into getGeo */
    double inArr[2];
    double outArr[5];
    
    /* set inputs */
    kinst = (int)inputArr[1];
    slt = inputArr[2];
    gdalt = inputArr[3];
    gdlat = inputArr[4];
    elm = inputArr[5];
    
    /* by default, assume failure */
    for (i=0; i<2; i++)
        outputArr[i] = missing;
    
    /* set values in inArr for one day earlier to get f10.7 24 hous earlier */
    inArr[0] = inputArr[0] - 86400.0;
    inArr[1] = inputArr[0] - 86400.0;
    
    getGeo(2, inArr, 5, outArr, errFile);
        
    /* check if needed data found */
    if (outArr[3] == missing)
        return (1);
    else
	f107 = outArr[3] * 1.0E22;
	
    /* set values in inArr for 3 hours earlier to get ap3 3 hous earlier */
    inArr[0] = inputArr[0] - 10800.0;
    inArr[1] = inputArr[0] - 10800.0;
    
    getGeo(2, inArr, 5, outArr, errFile);
        
    /* check if needed data found */
    if (outArr[1] == missing)
        return (1);
    else
	ap3 = outArr[1];
	
    /* get doy */
    dinvmadptr(inputArr[0], &iyr, &imd, &ihm, &ics);
    month = imd/100;
    day = imd - (100*month);
    hour = ihm/100;
    min = ihm - (100*hour);
    doy = (double)madGetDayno(iyr, month, day) + hour/24.0 + min/1440.0;
    
    /* check that we have a valid kinst */
    if (kinst != 30 &&
        kinst != 31 &&
        kinst != 32 &&
	kinst != 20 &&
	kinst != 25 &&
	kinst != 40 &&
	kinst != 72 &&
	kinst != 80 &&
	kinst != 95)
        return(1);
	
    /* check that elevation is greater than 75 if only local model */
    if ((kinst == 72 ||
         kinst == 80 ||
	 kinst == 95) && (inputArr[4] < 75.0))
        return(1);
	
    /* call Shunrong's method */
    
    /* ne and nel */
    for (i=0; i<4; i++)
        output[i] = 0.0;
    ipar = 1; /* ne */
    ISRIM_F77(&kinst, &doy, &slt, &gdalt, &gdlat, &f107, &ap3, &ipar, output);
    if (output[3] < 0.0)
    {
        outputArr[0] = missing;
	outputArr[1] = missing;
    }
    else
    {
        
	outputArr[0] = output[0];
	outputArr[1] = log10(output[0]);
    }
    
   
    return(0);  
}


/***********************************************************************
*
* getVisrTe   derives Model Te using Shunrong's model
*
*
*   arguments: 
*      inCount (num inputs) = 6 (UT1, KINST, SLT, GDALT, GDLAT, ELM)
*      inputArr - double array holding:
*         UT1 - UT at record start
*         KINST - instrument id
*         SLT - Local solar time in hours (0-24)
*         GDALT - geodetic altitude in km
*         GDLAT - geodetic latitude
*         ELM - mean elevation in degrees
*      outCount (num outputs) = 1 (TE_MODEL)
*      outputArr - double array holding: 
*         TE_MODEL -  Model electron temperature (K)
*
*   Algorithm - see Shunrong's model at http://madrigal.haystack.mit.edu/models/
*
*   returns - 0 (successful)
*/
int getVisrTe(int inCount, 
              double * inputArr, 
              int outCount, 
              double * outputArr, 
              FILE * errFile)
{
    int iyr=0, imd=0, ihm=0, ics=0;
    int month=0, day=0;
    int hour=0, min=0;
    int i;
    
    /* input parameters */
    int kinst = 0;
    int ipar = 0;
    double slt = 0.0;
    double gdalt = 0.0;
    double gdlat = 0.0;
    double elm = 0.0;
    
    /* parameters needed for input to Shunrong's model */
    double doy = 0.0;      /* day of year (1-366)     */
    double f107 = 0.0;     /* f10.7 in 10^-22 W/m2/Hz */
    double ap3 = 0.0;      /* Ap3                     */
    
    /* outputs of Shunrong's model */
    double output[4];

    /* declaration of data to be passed into getGeo */
    double inArr[2];
    double outArr[5];
    
    /* set inputs */
    kinst = (int)inputArr[1];
    slt = inputArr[2];
    gdalt = inputArr[3];
    gdlat = inputArr[4];
    elm = inputArr[5];
    
    /* by default, assume failure */
    outputArr[0] = missing;
    
    /* set values in inArr for one day earlier to get f10.7 24 hous earlier */
    inArr[0] = inputArr[0] - 86400.0;
    inArr[1] = inputArr[0] - 86400.0;
    
    getGeo(2, inArr, 5, outArr, errFile);
        
    /* check if needed data found */
    if (outArr[3] == missing)
        return (1);
    else
	f107 = outArr[3] * 1.0E22;
	
    /* set values in inArr for 3 hours earlier to get ap3 3 hours earlier */
    inArr[0] = inputArr[0] - 10800.0;
    inArr[1] = inputArr[0] - 10800.0;
    
    getGeo(2, inArr, 5, outArr, errFile);
        
    /* check if needed data found */
    if (outArr[1] == missing)
        return (1);
    else
	ap3 = outArr[1];
	
    /* get doy */
    dinvmadptr(inputArr[0], &iyr, &imd, &ihm, &ics);
    month = imd/100;
    day = imd - (100*month);
    hour = ihm/100;
    min = ihm - (100*hour);
    doy = (double)madGetDayno(iyr, month, day) + hour/24.0 + min/1440.0;
    
    /* check that we have a valid kinst */
    if (kinst != 30 &&
        kinst != 31 &&
        kinst != 32 &&
	kinst != 20 &&
	kinst != 25 &&
	kinst != 40 &&
	kinst != 72 &&
	kinst != 80 &&
	kinst != 95)
        return(1);
	
    /* check that elevation is greater than 75 if only local model */
    if ((kinst == 72 ||
         kinst == 80 ||
	 kinst == 95) && (inputArr[4] < 75.0))
        return(1);
	
    /* call Shunrong's method */
    
    
    /* te */
    for (i=0; i<4; i++)
        output[i] = 0.0;
    ipar = 2; /* te */
    ISRIM_F77(&kinst, &doy, &slt, &gdalt, &gdlat, &f107, &ap3, &ipar, output);
    if (output[3] < 0.0)
    {
        outputArr[0] = missing;
    }
    else
    {
        
	outputArr[0] = output[0];
    }
    
    return(0);  
}


/***********************************************************************
*
* getVisrTi   derives Model Ti using Shunrong's model
*
*
*   arguments: 
*      inCount (num inputs) = 6 (UT1, KINST, SLT, GDALT, GDLAT, ELM)
*      inputArr - double array holding:
*         UT1 - UT at record start
*         KINST - instrument id
*         SLT - Local solar time in hours (0-24)
*         GDALT - geodetic altitude in km
*         GDLAT - geodetic latitude
*         ELM - mean elevation in degrees
*      outCount (num outputs) = 1 (TI_MODEL)
*      outputArr - double array holding: 
*         TI_MODEL -  Model ion temperature (K)
*
*   Algorithm - see Shunrong's model at http://madrigal.haystack.mit.edu/models/
*
*   returns - 0 (successful)
*/
int getVisrTi(int inCount, 
              double * inputArr, 
              int outCount, 
              double * outputArr, 
              FILE * errFile)
{
    int iyr=0, imd=0, ihm=0, ics=0;
    int month=0, day=0;
    int hour=0, min=0;
    int i;
    
    /* input parameters */
    int kinst = 0;
    int ipar = 0;
    double slt = 0.0;
    double gdalt = 0.0;
    double gdlat = 0.0;
    double elm = 0.0;
    
    /* parameters needed for input to Shunrong's model */
    double doy = 0.0;      /* day of year (1-366)     */
    double f107 = 0.0;     /* f10.7 in 10^-22 W/m2/Hz */
    double ap3 = 0.0;      /* Ap3                     */
    
    /* outputs of Shunrong's model */
    double output[4];

    /* declaration of data to be passed into getGeo */
    double inArr[2];
    double outArr[5];
    
    /* set inputs */
    kinst = (int)inputArr[1];
    slt = inputArr[2];
    gdalt = inputArr[3];
    gdlat = inputArr[4];
    elm = inputArr[5];
    
    /* by default, assume failure */
    outputArr[0] = missing;
    
    /* set values in inArr for one day earlier to get f10.7 24 hours earlier */
    inArr[0] = inputArr[0] - 86400.0;
    inArr[1] = inputArr[0] - 86400.0;
    
    getGeo(2, inArr, 5, outArr, errFile);
        
    /* check if needed data found */
    if (outArr[3] == missing)
        return (1);
    else
	f107 = outArr[3] * 1.0E22;
	
    /* set values in inArr for 3 hours earlier to get ap3 3 hous earlier */
    inArr[0] = inputArr[0] - 10800.0;
    inArr[1] = inputArr[0] - 10800.0;
    
    getGeo(2, inArr, 5, outArr, errFile);
        
    /* check if needed data found */
    if (outArr[1] == missing)
        return (1);
    else
	ap3 = outArr[1];
	
    /* get doy */
    dinvmadptr(inputArr[0], &iyr, &imd, &ihm, &ics);
    month = imd/100;
    day = imd - (100*month);
    hour = ihm/100;
    min = ihm - (100*hour);
    doy = (double)madGetDayno(iyr, month, day) + hour/24.0 + min/1440.0;
    
    /* check that we have a valid kinst */
    if (kinst != 30 &&
        kinst != 31 &&
        kinst != 32 &&
	kinst != 20 &&
	kinst != 25 &&
	kinst != 40 &&
	kinst != 72 &&
	kinst != 80 &&
	kinst != 95)
        return(1);
	
    /* check that elevation is greater than 75 if only local model */
    if ((kinst == 72 ||
         kinst == 80 ||
	 kinst == 95) && (inputArr[4] < 75.0))
        return(1);
	
    /* call Shunrong's method */
    
    /* ti */
    for (i=0; i<4; i++)
        output[i] = 0.0;
    ipar = 3; /* ti */
    ISRIM_F77(&kinst, &doy, &slt, &gdalt, &gdlat, &f107, &ap3, &ipar, output);
    if (output[3] < 0.0)
    {
        outputArr[0] = missing;
    }
    else
    {
        
	outputArr[0] = output[0];
    }
    
    return(0);  
}


/***********************************************************************
*
* getVisrVo   derives Model Vo using Shunrong's model
*
*
*   arguments: 
*      inCount (num inputs) = 6 (UT1, KINST, SLT, GDALT, GDLAT, ELM)
*      inputArr - double array holding:
*         UT1 - UT at record start
*         KINST - instrument id
*         SLT - Local solar time in hours (0-24)
*         GDALT - geodetic altitude in km
*         GDLAT - geodetic latitude
*         ELM - mean elevation in degrees
*      outCount (num outputs) = 1 (VO_MODEL)
*      outputArr - double array holding: 
*         VO_MODEL -  Model line-of sight velocity (up=+) (m/s)
*
*   Algorithm - see Shunrong's model at http://madrigal.haystack.mit.edu/models/
*
*   returns - 0 (successful)
*/
int getVisrVo(int inCount, 
              double * inputArr, 
              int outCount, 
              double * outputArr, 
              FILE * errFile)
{
    int iyr=0, imd=0, ihm=0, ics=0;
    int month=0, day=0;
    int hour=0, min=0;
    int i;
    
    /* input parameters */
    int kinst = 0;
    int ipar = 0;
    double slt = 0.0;
    double gdalt = 0.0;
    double gdlat = 0.0;
    double elm = 0.0;
    
    /* parameters needed for input to Shunrong's model */
    double doy = 0.0;      /* day of year (1-366)     */
    double f107 = 0.0;     /* f10.7 in 10^-22 W/m2/Hz */
    double ap3 = 0.0;      /* Ap3                     */
    
    /* outputs of Shunrong's model */
    double output[4];

    /* declaration of data to be passed into getGeo */
    double inArr[2];
    double outArr[5];
    
    /* set inputs */
    kinst = (int)inputArr[1];
    slt = inputArr[2];
    gdalt = inputArr[3];
    gdlat = inputArr[4];
    elm = inputArr[5];
    
    /* by default, assume failure */
    outputArr[0] = missing;
    
    /* set values in inArr for one day earlier to get f10.7 24 hous earlier */
    inArr[0] = inputArr[0] - 86400.0;
    inArr[1] = inputArr[0] - 86400.0;
    
    getGeo(2, inArr, 5, outArr, errFile);
        
    /* check if needed data found */
    if (outArr[3] == missing)
        return (1);
    else
	f107 = outArr[3] * 1.0E22;
	
    /* set values in inArr for 3 hours earlier to get ap3 3 hours earlier */
    inArr[0] = inputArr[0] - 10800.0;
    inArr[1] = inputArr[0] - 10800.0;
    
    getGeo(2, inArr, 5, outArr, errFile);
        
    /* check if needed data found */
    if (outArr[1] == missing)
        return (1);
    else
	ap3 = outArr[1];
	
    /* get doy */
    dinvmadptr(inputArr[0], &iyr, &imd, &ihm, &ics);
    month = imd/100;
    day = imd - (100*month);
    hour = ihm/100;
    min = ihm - (100*hour);
    doy = (double)madGetDayno(iyr, month, day) + hour/24.0 + min/1440.0;
    
    /* check that we have a valid kinst */
    if (kinst != 30 &&
        kinst != 31 &&
        kinst != 32 &&
	kinst != 20 &&
	kinst != 25 &&
	kinst != 40 &&
	kinst != 72 &&
	kinst != 80 &&
	kinst != 95)
        return(1);
	
    /* check that elevation is greater than 75 if only local model */
    if ((kinst == 72 ||
         kinst == 80 ||
	 kinst == 95) && (inputArr[4] < 75.0))
        return(1);
	
    /* call Shunrong's method */
    
    /* vo */
    for (i=0; i<4; i++)
        output[i] = 0.0;
    ipar = 4; /* vo */
    ISRIM_F77(&kinst, &doy, &slt, &gdalt, &gdlat, &f107, &ap3, &ipar, output);
    if (output[3] < 0.0)
    {
        outputArr[0] = missing;
    }
    else
    {
        
	outputArr[0] = output[0];
    }
    
    return(0);  
}


/***********************************************************************
*
* getVisrNeDiff   derives ne_modeldiff - the difference between measured and model ne
*
*
*   arguments: 
*      inCount (num inputs) = 2 (NE, NE_MODEL)
*      inputArr - double array holding:
*         NE -  Measured electron density (m^-3)
*         NE_MODEL -  Model electron density (m^-3)
*      outCount (num outputs) = 1 (NE_MODELDIFF)
*      outputArr - double array holding: 
*         NE_MODELDIFF -  Measured Ne - Model electron density (m^-3)
*
*   returns - 0 (successful)
*/	    
int getVisrNeDiff(int inCount, 
                  double * inputArr, 
                  int outCount, 
                  double * outputArr, 
                  FILE * errFile)
{
    if (inputArr[0] == missing || inputArr[1] == missing)
        outputArr[0] = missing;
    else
        outputArr[0] = inputArr[0] - inputArr[1];

    return(0);
}


/***********************************************************************
*
* getVisrNelDiff   derives nel_modeldiff - the difference between measured and model nel
*
*
*   arguments: 
*      inCount (num inputs) = 2 (NEL, NEL_MODEL)
*      inputArr - double array holding:
*         NEL -  Measured electron density log10(m^-3)
*         NEL_MODEL -  Model electron density log10(m^-3)
*      outCount (num outputs) = 1 (NEL_MODELDIFF)
*      outputArr - double array holding: 
*         NEL_MODELDIFF -  Measured Nel - Model electron density log10(m^-3)
*
*   returns - 0 (successful)
*/	    
int getVisrNelDiff(int inCount, 
                   double * inputArr, 
                   int outCount, 
                   double * outputArr, 
                   FILE * errFile)
{
    if (inputArr[0] == missing || inputArr[1] == missing)
        outputArr[0] = missing;
    else
        outputArr[0] = inputArr[0] - inputArr[1];

    return(0);
}


/***********************************************************************
*
* getVisrTeDiff   derives te_modeldiff - the difference between measured and model te
*
*
*   arguments: 
*      inCount (num inputs) = 2 (TE, TE_MODEL)
*      inputArr - double array holding:
*         TE -  Measured electron temperature (K)
*         TE_MODEL -  Model electron temperature (K)
*      outCount (num outputs) = 1 (TE_MODELDIFF)
*      outputArr - double array holding: 
*         TE_MODELDIFF -  Measured Te - Model electron temperature (K)
*
*   returns - 0 (successful)
*/	    
int getVisrTeDiff(int inCount, 
                  double * inputArr, 
                  int outCount, 
                  double * outputArr, 
                  FILE * errFile)
{
    if (inputArr[0] == missing || inputArr[1] == missing)
        outputArr[0] = missing;
    else
        outputArr[0] = inputArr[0] - inputArr[1];

    return(0);
}


/***********************************************************************
*
* getVisrTiDiff   derives ti_modeldiff - the difference between measured and model ti
*
*
*   arguments: 
*      inCount (num inputs) = 2 (TI, TI_MODEL)
*      inputArr - double array holding:
*         TI -  Measured ion temperature (K)
*         TI_MODEL -  Model ion temperature (K)
*      outCount (num outputs) = 1 (TI_MODELDIFF)
*      outputArr - double array holding: 
*         TI_MODELDIFF -  Measured TI - Model ion temperature (K)
*
*   returns - 0 (successful)
*/	    
int getVisrTiDiff(int inCount, 
                  double * inputArr, 
                  int outCount, 
                  double * outputArr, 
                  FILE * errFile)
{
    if (inputArr[0] == missing || inputArr[1] == missing)
        outputArr[0] = missing;
    else
        outputArr[0] = inputArr[0] - inputArr[1];

    return(0);
}


/***********************************************************************
*
* getVisrVoDiff   derives vo_modeldiff - the difference between measured and model vo
*
*
*   arguments: 
*      inCount (num inputs) = 2 (VO, VO_MODEL)
*      inputArr - double array holding:
*         VO -  Measured line-of sight velocity (up=+) (m/s)
*         VO_MODEL -  Model line-of sight velocity (up=+) (m/s)
*      outCount (num outputs) = 1 (VO_MODELDIFF)
*      outputArr - double array holding: 
*         VO_MODELDIFF -  Measured Vo - Model line-of sight velocity (up=+) (m/s)
*
*   returns - 0 (successful)
*/	    
int getVisrVoDiff(int inCount, 
                  double * inputArr, 
                  int outCount, 
                  double * outputArr, 
                  FILE * errFile)
{
    if (inputArr[0] == missing || inputArr[1] == missing)
        outputArr[0] = missing;
    else
        outputArr[0] = inputArr[0] - inputArr[1];

    return(0);
}


/***********************************************************************
*
* getSn   derives Sn given Snp3
*
*
*   arguments: 
*      inCount (num inputs) = 1 (SNP3)
*      inputArr - double array holding:
*         SNP3 - Signal to noise ratio
*      outCount (num outputs) = 1 (SN)
*      outputArr - double array holding: 
*         SN - Signal to noise ratio
*
*   Algorithm - SN and SNP3 differ only in Cedar scaling factor, which
*               is handled at a lower level
*
*   returns - 0 (successful)
*/
int getSn(int inCount, 
          double * inputArr, 
          int outCount, 
          double * outputArr, 
          FILE * errFile)
{
   outputArr[0] = inputArr[0];
   return(0);
}


/***********************************************************************
*
* getSnp3   derives Snp3 given Sn
*
*
*   arguments: 
*      inCount (num inputs) = 1 (SN)
*      inputArr - double array holding:
*         SN - Signal to noise ratio
*      outCount (num outputs) = 1 (SNP3)
*      outputArr - double array holding: 
*         SNP3 - Signal to noise ratio
*
*   Algorithm - SN and SNP3 differ only in Cedar scaling factor, which
*               is handled at a lower level
*
*   returns - 0 (successful)
*/
int getSnp3(int inCount, 
            double * inputArr, 
            int outCount, 
            double * outputArr, 
            FILE * errFile)
{
   outputArr[0] = inputArr[0];
   return(0);
}


/***********************************************************************
*
* getChip31   derives Chip3 given Chisq
*
*
*   arguments: 
*      inCount (num inputs) = 1 (CHISQ)
*      inputArr - double array holding:
*         CHISQ - Reduced-chi square of fit
*      outCount (num outputs) = 1 (CHIP3)
*      outputArr - double array holding: 
*         CHIP3 - Reduced-chi square of fit
*
*   Algorithm - CHISQ and CHIP3 differ only in Cedar scaling factor, which
*               is handled at a lower level
*
*   returns - 0 (successful)
*/
int getChip31(int inCount, 
              double * inputArr, 
              int outCount, 
              double * outputArr, 
              FILE * errFile)
{
   outputArr[0] = inputArr[0];
   return(0);
}


/***********************************************************************
*
* getWchsq1   derives Wchsq given Chip3
*
*
*   arguments: 
*      inCount (num inputs) = 1 (CHIP3)
*      inputArr - double array holding:
*         CHIP3 - Reduced-chi square of fit
*      outCount (num outputs) = 1 (WCHSQ)
*      outputArr - double array holding: 
*         WCHSQ - Reduced-chi square of fit
*
*   Algorithm - WCHSQ and CHIP3 differ only in Cedar scaling factor, which
*               is handled at a lower level
*
*   returns - 0 (successful)
*/
int getWchsq1(int inCount, 
              double * inputArr, 
              int outCount, 
              double * outputArr, 
              FILE * errFile)
{
   outputArr[0] = inputArr[0];
   return(0);
}


/***********************************************************************
*
* getChisq1   derives Chisq given Wchsq
*
*
*   arguments: 
*      inCount (num inputs) = 1 (WCHSQ)
*      inputArr - double array holding:
*         WCHSQ - Reduced-chi square of fit
*      outCount (num outputs) = 1 (CHISQ)
*      outputArr - double array holding: 
*         CHISQ - Reduced-chi square of fit
*
*   Algorithm - CHISQ and WCHSQ differ only in Cedar scaling factor, which
*               is handled at a lower level
*
*   returns - 0 (successful)
*/
int getChisq1(int inCount, 
              double * inputArr, 
              int outCount, 
              double * outputArr, 
              FILE * errFile)
{
   outputArr[0] = inputArr[0];
   return(0);
}


/***********************************************************************
*
* getChip32   derives Chip3 given Wchsq
*
*
*   arguments: 
*      inCount (num inputs) = 1 (WCHSQ)
*      inputArr - double array holding:
*         WCHSQ - Reduced-chi square of fit
*      outCount (num outputs) = 1 (CHIP3)
*      outputArr - double array holding: 
*         CHIP3 - Reduced-chi square of fit
*
*   Algorithm - WCHSQ and CHIP3 differ only in Cedar scaling factor, which
*               is handled at a lower level
*
*   returns - 0 (successful)
*/
int getChip32(int inCount, 
              double * inputArr, 
              int outCount, 
              double * outputArr, 
              FILE * errFile)
{
   outputArr[0] = inputArr[0];
   return(0);
}


/***********************************************************************
*
* getWchsq2   derives Wchsq given Chisq
*
*
*   arguments: 
*      inCount (num inputs) = 1 (CHISQ)
*      inputArr - double array holding:
*         CHISQ - Reduced-chi square of fit
*      outCount (num outputs) = 1 (WCHSQ)
*      outputArr - double array holding: 
*         WCHSQ - Reduced-chi square of fit
*
*   Algorithm - CHISQ and WCHSQ differ only in Cedar scaling factor, which
*               is handled at a lower level
*
*   returns - 0 (successful)
*/
int getWchsq2(int inCount, 
              double * inputArr, 
              int outCount, 
              double * outputArr, 
              FILE * errFile)
{
   outputArr[0] = inputArr[0];
   return(0);
}


/***********************************************************************
*
* getChisq2   derives Chisq given Chip3
*
*
*   arguments: 
*      inCount (num inputs) = 1 (CHIP3)
*      inputArr - double array holding:
*         CHIP3 - Reduced-chi square of fit
*      outCount (num outputs) = 1 (CHISQ)
*      outputArr - double array holding: 
*         CHISQ - Reduced-chi square of fit
*
*   Algorithm - CHISQ and CHIP3 differ only in Cedar scaling factor, which
*               is handled at a lower level
*
*   returns - 0 (successful)
*/
int getChisq2(int inCount, 
              double * inputArr, 
              int outCount, 
              double * outputArr, 
              FILE * errFile)
{
   outputArr[0] = inputArr[0];
   return(0);
}




/***********************************************************************
*
* getNeut  uses MSIS 2000 model to predict neutral atmosphere parameters based
*          on geophysical parameters.  Includes anomalous oxygen component,
*          and also uses array of historical and present AP values, rather than
*          simply present AP value.
*
*   arguments: 
*      inCount (num inputs) = 5 (UT1, UT2, GDLAT, GLON, GDALT)
*      inputArr - double array holding:
*                 UT1 - UT at record start
*                 UT2 - UT at record end
*                 GDLAT - geodetic latitude
*                 GLON - geodetic longitude
*                 GDALT - geodetic altitude
*      outCount (num outputs) = 13 (TNM, TINFM, MOL, NTOTL, NN2L,
*                                   NO2L, NOL, NARL, NHEL, NHL,
*                                   NN4SL, NPRESL, )
*      outputArr - double array holding:
*                 TNM - Model neutral atmosphere temperature
*                 TINFM - Model Exospheric temperature
*                 MOL - Log10 (nutrl atm mass density in kg/m3)
*                 NTOTL - Log10 (nutrl atm number density in m-3)
*                 NN2L - Nutrl atm compositn-log10([N2] in m-3)
*                 NO2L - Nutrl atm compositn-log10([O2] in m-3)
*                 NOL - Nutrl atm composition-log10([O] in m-3)
*                 NARL - Nutrl atm compositn-log10([AR] in m-3)
*                 NHEL - Nutrl atm compositn-log10([HE] in M-3)
*                 NHL - Nutrl atm composition-log10([H] in m-3)
*                 NN4SL - Nutrl atm compstn-log10([N(4S)] in m-3)
*                 NPRESL - Neutral atmospher log10(pressure in Pa)
*                 PSH - Pressure scale height (m)
*
*   Algorithm: Calls MSIS subroutine gtd7d after collecting geophysical data.
*              Includes AP data from present and up to 57 hours prior to
*              present.
*
*              Gets pressure via ideal gas law (p=nkT)
*              Gets scale height via kT/mg, where m is average mass
*
*   returns - 0 (successful)
*/
int getNeut(int inCount, 
            double * inputArr, 
            int outCount, 
            double * outputArr, 
            FILE * errFile)
{
    double mid_time = 0.0;
    double gdlat = 0.0;
    double glon = 0.0;
    double gdalt = 0.0;
    double ave_mass = 0.0;
    const double k = 1.3807e-23;
    const double g = 9.8;
    
    int i = 0;
    int iyr = 0, imd = 0, ihm = 0, ics = 0;
    int month = 0, day = 0, hour = 0, min = 0;
    
    /* flag to indicate geo data missing */
    int geoMissing = 0;
    
    /* declaration of data to be passed into gtd7d */
    int iyd = 0;        /* date in form YYDDD                */
    double sec = 0.0;   /* seconds since beginning of UT day */
    double slt = 0.0;   /* slt=sec/3600+glon/15              */
    double f107a = 0.0; /* 3 month average of F10.7 flux     */
    double f107 = 0.0;  /* daily F10.7 flux for previous day */
    double ap[7];       /* array of Ap values - see gtd7.f - this array */
                        /* includes AP values from present and up to    */
			/* 57 hours before present                      */
    int mass = 48;      /* tells gtd7d to calculate all       */
    
    /* declaration of data returned from gtd7d */
    double d[9];
    double t[2];
    
    /* declaration of data to be passed into getGeo */
    double inArr[2];
    double outArr[5];
    
    /* init arrays to keep purify happy */
    for (i=0; i<7; i++)
        ap[i] = 0.0;
    for (i=0; i<8; i++)
        d[i] = 0.0;
    for (i=0; i<2; i++)
        t[i] = 0.0;
    for (i=0; i<2; i++)
        inArr[i] = 0.0;
    for (i=0; i<5; i++)
        outArr[i] = 0.0;
    
    mid_time = (inputArr[0] + inputArr[1])/2.0;
    gdlat = inputArr[2];
    glon  = inputArr[3];
    gdalt = inputArr[4];
    
    /* force glon to between -180 and 180 */
    while (glon < -180.0) glon += 360.0;
    while (glon > +180.0) glon -= 360.0;
    
    /* set up input arguments */
    
    dinvmadptr(mid_time, &iyr, &imd, &ihm, &ics);
    month = imd/100;
    day = imd - month*100;
    hour = ihm/100;
    min = ihm - hour*100;
    
    iyd = (iyr%100)*1000;                /* set up year part of YYDDD */
    iyd += madGetDayno(iyr, month, day); /* set up day part of YYDDD  */
    
    sec = hour*3600.0;
    sec += min*60.0;
    sec += ics/100.0;
    
    slt = sec/3600.0 + glon/15.0;
    
    /* initialize ap array */
    for (i=0; i<7; i++) 
        ap[i] = 0.0;
    
    /* loop through all the preceding times to get needed geo data */
    for (i=0; i<20; i++)
    {
        inArr[0] = mid_time - (3.0*3600.0*i);
        inArr[1] = inArr[0];
        
        getGeo(2, inArr, 5, outArr, errFile);
        
        /* check if data found */
        if (outArr[1] == missing ||
            outArr[2] == missing ||
            outArr[3] == missing ||
            outArr[4] == missing)
        {
            geoMissing = 1;
            break;
        }
        
        switch (i)
        {
            case 0: /* present time */
                ap[0] = outArr[2];
                ap[1] = outArr[1];
                f107a = outArr[4]*1.0e22;
                break;
                
            case 1: /*  3 hours before */
                ap[2] = outArr[1];
                break;
                
            case 2: /*  6 hours before */
                ap[3] = outArr[1];
                break;
                
            case 3: /*  9 hours before */
                ap[4] = outArr[1];
                break;
            
            case 4:
            case 5:
            case 6:
            case 7:
            case 8: /* 24 hours before */
	        if (i == 8)
	            f107  = outArr[3]*1.0e22;
            case 9:
            case 10:
            case 11:  /* average 8 samples 12 to 33 h */
                ap[5] += outArr[1]/8.0;
                break;
                
            case 12:
            case 13:
            case 14:
            case 15:
            case 16:
            case 17:
            case 18:
            case 19:  /* average 8 samples 36 to 57 h */
                ap[6] += outArr[1]/8.0;
                break;
        } /* end switch */
    } /* end loop over times */
    
    /* check if all data found */
    if (geoMissing)
    {
        for (i=0; i<outCount; i++)
            outputArr[i] = missing;
            
        return (0);
    }
              
    /* call main MSIS method */
    GTD7D_F77(&iyd,
              &sec,
              &gdalt,
              &gdlat,
              &glon,
              &slt,
              &f107a,
              &f107,
              ap,
              &mass,
              d,
              t);
          
    /* get average mass in kg */
    if (d[0]+d[1]+d[2]+d[3]+d[4]+d[6]+d[7] > 0.0 && d[5] > 0.0)
    {
        ave_mass = d[5]/(d[0]+d[1]+d[2]+d[3]+d[4]+d[6]+d[7]);
        /* convert from grams to kg */
        ave_mass = ave_mass*1.e-3;
        /* get pressure */
        outputArr[11]=(d[0]+d[1]+d[2]+d[3]+d[4]+d[6]+d[7])*1.e6*k*t[1];
        /* get scale height */
        outputArr[12]=k*t[1]/(ave_mass*g);
    }
    else
    {
        outputArr[11]=missing;
        outputArr[12]=missing;
    }

    /* copy results */
    outputArr[0] = t[1];                    /* tnm */
    outputArr[1] = t[0];                    /* tinfm */
    
    if (d[5] > 0.0)
        outputArr[2] = log10(d[5]*1.e3);    /* mol */
    else
        outputArr[2] = missing;
        
    if (d[0]+d[1]+d[2]+d[3]+d[4]+d[6]+d[7] > 0.0)        
        outputArr[3] = log10(1.e6*(d[0]+d[1]+d[2]+d[3]+d[4]+d[6]+d[7])); /* neutral density */
    else
        outputArr[3] = missing;
              
    if (d[2] > 0.0)        
        outputArr[4] = log10(1.e6*d[2]);     /* N2 density */
    else
        outputArr[4] = missing;
              
    if (d[3] > 0.0)        
        outputArr[5] = log10(1.e6*d[3]);     /* O2 density */
    else
        outputArr[5] = missing;
              
    if (d[1] > 0.0)        
        outputArr[6] = log10(1.e6*d[1]);     /* O density */
    else
        outputArr[6] = missing;
              
    if (d[4] > 0.0)        
        outputArr[7] = log10(1.e6*d[4]);     /* Ar density */
    else
        outputArr[7] = missing;
              
    if (d[0] > 0.0)        
        outputArr[8] = log10(1.e6*d[0]);     /* He density */
    else
        outputArr[8] = missing;
              
    if (d[6] > 0.0)        
        outputArr[9] = log10(1.e6*d[6]);     /* H density */
    else
        outputArr[9] = missing;
        
    if (d[7] > 0.0)        
        outputArr[10] = log10(1.e6*d[7]);    /* N - stable density */
    else
        outputArr[10] = missing;
          
    return (0);
}


/***********************************************************************
*
* getTn  uses MSIS model and ISR data to derive Tn.
*
*   arguments: 
*      inCount (num inputs) = 9 (TI, TE, NE, PH+, NOL, NHL, NN4SL, NO2L, NHEL)
*      inputArr - double array holding:
*                 TI - Ion temperature (K)
*                 TE - Electron temperature (K)
*                 NE - Electron density (m^3)
*                 PHP - Composition - [HE+]/Ne 
*                 NOL - Nutrl atm composition-log10([O] in m-3) (MSIS)
*                 NHL - Nutrl atm composition-log10([H] in m-3) (MSIS)
*                 NN4SL - Nutrl atm compstn-log10([N(4S)] in m-3) (MSIS)
*                 NO2L - Nutrl atm compositn-log10([O2] in m-3) (MSIS)
*                 NHEL - Nutrl atm compositn-log10([HE] in M-3) (MSIS)
*      outCount (num outputs) = 1 (TN )
*      outputArr - double array holding:
*                 TN - Neutral atmosphere temperature (K)
*
*   Algorithm: See tnf.f in madf/geolib
*
*   returns - 0 (successful)
*/
int getTn(int inCount, 
          double * inputArr, 
          int outCount, 
          double * outputArr, 
          FILE * errFile)
{
    double ti = 0.0;
    double te = 0.0;
    double ne = 0.0;
    double php = 0.0;
    double nol = 0.0;
    double nhl = 0.0;
    double nn4sl = 0.0;
    double no2l = 0.0;
    double nhel = 0.0;
    
    int err = 0; /* error indicator */
    
    double tn = 0.0;

    /* get inputs */
    ti = inputArr[0];
    te = inputArr[1];
    ne = inputArr[2];
    php = inputArr[3];
    nol = inputArr[4];
    nhl = inputArr[5];
    nn4sl = inputArr[6];
    no2l = inputArr[7];
    nhel = inputArr[8];
    
    /* convert units to what tnf wants  */
    ne = ne / 1.0E6; /* from m^3 to cm^3 */
    php = php * ne; /* from ratio to cm^3 */
    nol = pow(10.0, nol) / 1.0E6; /* from ln(m^3) to cm^3 */
    nhl = pow(10.0, nhl) / 1.0E6; /* from ln(m^3) to cm^3 */
    nn4sl = pow(10.0, nn4sl) / 1.0E6; /* from ln(m^3) to cm^3 */
    no2l = pow(10.0, no2l) / 1.0E6; /* from ln(m^3) to cm^3 */
    nhel = pow(10.0, nhel) / 1.0E6; /* from ln(m^3) to cm^3 */
    
    /* call fortran tnf */
    tn = TNF_F77(&ti, &te, &ne, &php, &nol, &nhl, &nn4sl, &no2l, &nhel, &err);
    
    if (err != 0)
        outputArr[0] = missing;
    else
        outputArr[0] = tn;
	
    return(0);
}


/***********************************************************************
*
* getTnNoPhp  uses MSIS model and ISR data to derive Tn - uses Php = 0.
*
*   Meant to be called to get Tn if Php not in measured data
*
*   arguments: 
*      inCount (num inputs) = 8 (TI, TE, NE, NOL, NHL, NN4SL, NO2L, NHEL)
*      inputArr - double array holding:
*                 TI - Ion temperature (K)
*                 TE - Electron temperature (K)
*                 NE - Electron density (m^3)
*                 NOL - Nutrl atm composition-log10([O] in m-3) (MSIS)
*                 NHL - Nutrl atm composition-log10([H] in m-3) (MSIS)
*                 NN4SL - Nutrl atm compstn-log10([N(4S)] in m-3) (MSIS)
*                 NO2L - Nutrl atm compositn-log10([O2] in m-3) (MSIS)
*                 NHEL - Nutrl atm compositn-log10([HE] in M-3) (MSIS)
*      outCount (num outputs) = 1 (TN )
*      outputArr - double array holding:
*                 TN - Neutral atmosphere temperature (K)
*
*   Algorithm: See tnf.f in madf/geolib
*
*   returns - 0 (successful)
*/
int getTnNoPhp(int inCount,
               double * inputArr,
	       int outCount,
	       double * outputArr,
	       FILE * errFile)
{
    double ti = 0.0;
    double te = 0.0;
    double ne = 0.0;
    double php = 0.0;
    double nol = 0.0;
    double nhl = 0.0;
    double nn4sl = 0.0;
    double no2l = 0.0;
    double nhel = 0.0;
    
    int err = 0; /* error indicator */
    
    double tn = 0.0;

    /* get inputs */
    ti = inputArr[0];
    te = inputArr[1];
    ne = inputArr[2];
    nol = inputArr[3];
    nhl = inputArr[4];
    nn4sl = inputArr[5];
    no2l = inputArr[6];
    nhel = inputArr[7];
    
    /* convert units to what tnf wants  */
    ne = ne / 1.0E6; /* from m^3 to cm^3 */
    nol = pow(10.0, nol) / 1.0E6; /* from ln(m^3) to cm^3 */
    nhl = pow(10.0, nhl) / 1.0E6; /* from ln(m^3) to cm^3 */
    nn4sl = pow(10.0, nn4sl) / 1.0E6; /* from ln(m^3) to cm^3 */
    no2l = pow(10.0, no2l) / 1.0E6; /* from ln(m^3) to cm^3 */
    nhel = pow(10.0, nhel) / 1.0E6; /* from ln(m^3) to cm^3 */
    
    /* call fortran tnf */
    tn = TNF_F77(&ti, &te, &ne, &php, &nol, &nhl, &nn4sl, &no2l, &nhel, &err);
    
    if (err != 0)
        outputArr[0] = missing;
    else
        outputArr[0] = tn;
	
    return(0);
}


/***********************************************************************
*
* getCond  uses Shunrong's code from Schunk and Nagy to derive Pederson and Hall
*          local conductivities.
*
*   arguments: 
*      inCount (num inputs) = 10 (TI, TE, NE, PH+, PM, NOL, NN2L, NO2L, TN, BMAG)
*      inputArr - double array holding:
*                 TI - Ion temperature (K)
*                 TE - Electron temperature (K)
*                 NE - Electron density (m^3)
*                 PH+ - Composition - [H+]/Ne 
*                 PM - Comp - (ions with mol wt 28 to 32)/Ne
*                 NOL - Nutrl atm composition-log10([O] in m-3) (MSIS)
*                 NN2L - Nutrl atm compositn-log10([N2] in m-3) (MSIS)
*                 NO2L - Nutrl atm compositn-log10([O2] in m-3) (MSIS)
*                 TN - Neutral atmosphere temperature (K)
*                 BMAG - Geomagnetic field strength (Tesla)
*      outCount (num outputs) = 4 (PDCON, PDCONL, HLCON, HLCONL)
*      outputArr - double array holding:
*                 PDCON - Pedersen Conductivity in mho/m3
*                 PDCONL - Log10(Pedersen Conductivity in mho/m3)
*                 HLCON - Hall Conductivity in mho/m3
*                 HLCONL - Log10(Hall Conductivity in mho/m3)
*
*   Algorithm: See conduct.f in madf/geolib
*
*   returns - 0 (successful)
*/
int getCond(int inCount, 
            double * inputArr, 
            int outCount, 
            double * outputArr, 
            FILE * errFile)
{
    double ti = 0.0;
    double te = 0.0;
    double ne = 0.0;
    double php = 0.0;
    double ni[2];  /* ni[0] = O+, ni[1] = Molecular Ions */
    double n[3]; /* MSIS O,N2,O2 */
    double tn = 0.0;
    double bmag = 0.0;
    
    double pdcon = 0.0;
    double hlcon = 0.0;
    

    /* get inputs */
    ti = inputArr[0];
    te = inputArr[1];
    ne = inputArr[2];
    php = inputArr[3];
    ni[1] = inputArr[4];
    n[0] = pow(10.0, inputArr[5]);
    n[1] = pow(10.0, inputArr[6]);
    n[2] = pow(10.0, inputArr[7]);
    tn = inputArr[8];
    bmag = inputArr[9];
    
    /* apply rule that PH+ must less than 0.2 - per Phil Erickson */
    if (php >= 0.2)
    {
        outputArr[0] = missing;
	outputArr[1] = missing;
	outputArr[2] = missing;
	outputArr[3] = missing;
	
	return(0);
    }
    
    /* convert units to what convert.f wants  */
    ni[1] = ne * ni[1]; /* convert molec.ions from ratio to m^-3 */
    ni[0] = ne - ni[1]; /* assumes all ions are either molec. or O+ */
    
    /* call fortran conduct */
    CONDUCT_F77(&ne, ni, n, &te, &ti, &tn, &bmag, &hlcon, &pdcon);
    
    outputArr[0] = pdcon;
    if (pdcon <= 0.0)
        outputArr[1] = missing;
    else
        outputArr[1] = log10(pdcon);

    outputArr[2] = hlcon;
    if (hlcon <= 0.0)
        outputArr[3] = missing;
    else
        outputArr[3] = log10(hlcon);
	
    return(0);
}


/***********************************************************************
*
* getCondNoPM  uses Shunrong's code from Schunk and Nagy to derive Pederson and Hall
*              local conductivities.
*
*   Does not require PM, but always fails below 200 km gdalt
*
*   arguments: 
*      inCount (num inputs) = 10 (TI, TE, NE, PH+, NOL, NN2L, NO2L, TN, BMAG, GDALT)
*      inputArr - double array holding:
*                 TI - Ion temperature (K)
*                 TE - Electron temperature (K)
*                 NE - Electron density (m^3)
*                 PH+ - Composition - [H+]/Ne 
*                 NOL - Nutrl atm composition-log10([O] in m-3) (MSIS)
*                 NN2L - Nutrl atm compositn-log10([N2] in m-3) (MSIS)
*                 NO2L - Nutrl atm compositn-log10([O2] in m-3) (MSIS)
*                 TN - Neutral atmosphere temperature (K)
*                 BMAG - Geomagnetic field strength (Tesla)
*                 GDALT - Geodetic altitude in km
*      outCount (num outputs) = 4 (PDCON, PDCONL, HLCON, HLCONL)
*      outputArr - double array holding:
*                 PDCON - Pedersen Conductivity in mho/m3
*                 PDCONL - Log10(Pedersen Conductivity in mho/m3)
*                 HLCON - Hall Conductivity in mho/m3
*                 HLCONL - Log10(Hall Conductivity in mho/m3)
*
*   Algorithm: See conduct.f in madf/geolib
*
*   returns - 0 (successful)
*/
int getCondNoPM(int inCount, 
                double * inputArr, 
                int outCount, 
                double * outputArr, 
                FILE * errFile)
{
    double ti = 0.0;
    double te = 0.0;
    double ne = 0.0;
    double php = 0.0;
    double ni[2];  /* ni[0] = O+, ni[1] = Molecular Ions */
    double n[3]; /* MSIS O,N2,O2 */
    double tn = 0.0;
    double bmag = 0.0;
    double gdalt = 0.0;
    
    double pdcon = 0.0;
    double hlcon = 0.0;
    

    /* get inputs */
    ti = inputArr[0];
    te = inputArr[1];
    ne = inputArr[2];
    php = inputArr[3];
    ni[1] = 0.0;
    n[0] = pow(10.0, inputArr[4]);
    n[1] = pow(10.0, inputArr[5]);
    n[2] = pow(10.0, inputArr[6]);
    tn = inputArr[7];
    bmag = inputArr[8];
    gdalt = inputArr[9];
    
    /* apply rule that PH+ must less than 0.2 and gdalt > 200 km - per Phil Erickson */
    if (php >= 0.2 || gdalt < 200.0)
    {
        outputArr[0] = missing;
	outputArr[1] = missing;
	outputArr[2] = missing;
	outputArr[3] = missing;
	
	return(0);
    }
    
    /* convert units to what convert.f wants  */
    ni[1] = ne * ni[1]; /* convert molec.ions from ratio to m^-3 */
    ni[0] = ne - ni[1]; /* assumes all ions are either molec. or O+ */
    
    /* call fortran conduct */
    CONDUCT_F77(&ne, ni, n, &te, &ti, &tn, &bmag, &hlcon, &pdcon);
    
    outputArr[0] = pdcon;
    if (pdcon <= 0.0)
        outputArr[1] = missing;
    else
        outputArr[1] = log10(pdcon);

    outputArr[2] = hlcon;
    if (hlcon <= 0.0)
        outputArr[3] = missing;
    else
        outputArr[3] = log10(hlcon);
	
    return(0);
}


/***********************************************************************
*
* getCondEmpModel  uses Shunrong's code from Schunk and Nagy to derive Pederson and Hall
*              local conductivities.  Uses Shunrong's empirical model to get
*              basic radar parameters
*
*   Hard upper and lower limits of 500 and 200 km
*
*   arguments: 
*      inCount (num inputs) = 10 (TI_MODEL, TE_MODEL, NE_MODEL, NOL, NN2L, NO2L, TNM, BMAG, GDALT)
*      inputArr - double array holding:
*                 TI_MODEL - Shunrong's empirical model for Ion temperature (K)
*                 TE_MODEL - Shunrong's empirical model for Electron temperature (K)
*                 NE_MODEL - Shunrong's empirical model for Electron density (m^3)
*                 NOL - Nutrl atm composition-log10([O] in m-3) (MSIS)
*                 NN2L - Nutrl atm compositn-log10([N2] in m-3) (MSIS)
*                 NO2L - Nutrl atm compositn-log10([O2] in m-3) (MSIS)
*                 TNM - MSIS Neutral atmosphere temperature (K)
*                 BMAG - Geomagnetic field strength (Tesla)
*                 GDALT - Geodetic altitude in km
*      outCount (num outputs) = 4 (PDCON_MODEL, PDCONL_MODEL, HLCON_MODEL, HLCONL_MODEL)
*      outputArr - double array holding:
*                 PDCON_MODEL - Pedersen Conductivity in mho/m3
*                 PDCONL_MODEL - Log10(Pedersen Conductivity in mho/m3)
*                 HLCON_MODEL - Hall Conductivity in mho/m3
*                 HLCONL_MODEL - Log10(Hall Conductivity in mho/m3)
*
*   Algorithm: See conduct.f in madf/geolib
*
*   returns - 0 (successful)
*/
int getCondEmpModel(int inCount, 
                double * inputArr, 
                int outCount, 
                double * outputArr, 
                FILE * errFile)
{
    double ti = 0.0;
    double te = 0.0;
    double ne = 0.0;
    double ni[2];  /* ni[0] = O+, ni[1] = Molecular Ions */
    double n[3]; /* MSIS O,N2,O2 */
    double tn = 0.0;
    double bmag = 0.0;
    double gdalt = 0.0;
    
    double pdcon = 0.0;
    double hlcon = 0.0;
    
    const double upperAltLimit =  500.0;
    const double lowerAltLimit =  200.0;
    

    /* get inputs */
    ti = inputArr[0];
    te = inputArr[1];
    ne = inputArr[2];
    ni[1] = 0.0;
    n[0] = pow(10.0, inputArr[3]);
    n[1] = pow(10.0, inputArr[4]);
    n[2] = pow(10.0, inputArr[5]);
    tn = inputArr[6];
    bmag = inputArr[7];
    gdalt = inputArr[8];
    
    /* apply altitude rule */
    if (gdalt < lowerAltLimit || gdalt > upperAltLimit)
    {
        outputArr[0] = missing;
	outputArr[1] = missing;
	outputArr[2] = missing;
	outputArr[3] = missing;
	
	return(0);
    }
    
    /* convert units to what convert.f wants  */
    ni[1] = ne * ni[1]; /* convert molec.ions from ratio to m^-3 */
    ni[0] = ne - ni[1]; /* assumes all ions are either molec. or O+ */
    
    /* call fortran conduct */
    CONDUCT_F77(&ne, ni, n, &te, &ti, &tn, &bmag, &hlcon, &pdcon);
    
    outputArr[0] = pdcon;
    if (pdcon <= 0.0)
        outputArr[1] = missing;
    else
        outputArr[1] = log10(pdcon);

    outputArr[2] = hlcon;
    if (hlcon <= 0.0)
        outputArr[3] = missing;
    else
        outputArr[3] = log10(hlcon);
	
    return(0);
}




/***********************************************************************
*
* getImf   gets interplanetary magnetic field data given a time
*
*   arguments: 
*      inCount (num inputs) = 2 (UT1, UT2)
*      inputArr - double array holding:
*                 UT1 - UT at record start
*                 UT2 - UT at record end
*      outCount (num outputs) = 10 (BXGSM, BYGSM, BZGSM, BIMF,
*                                   BXGSE, BYGSE, BZGSE,
*                                   SWDEN, SWSPD, SWQ)
*      outputArr - double array holding:
*                 BXGSM - Interplanetary Mag Field (Bx GSM coord)
*                 BYGSM - Interplanetary Mag Field (By GSM coord)
*                 BYGSM - Interplanetary Mag Field (Bz GSM coord)
*                 BIMF - Interplanetary Mag Field strength
*                 BXGSE - Interplanetary Mag Field (Bx GSE coord)
*                 BYGSE - Interplanetary Mag Field (By GSE coord)
*                 BZGSE - Interplanetary Mag Field (Bz GSE coord)
*                 SWDEN - Solar Wind Plasma Density
*                 SWSPD - Solar Wind Plasma Speed
*                 SWQ - IMF/Solar Wind Qualifier
*
*
*   Algorithm: Get data from imf631127g.001 at average UT.  Only
*              the first time any thread calls this method will the
*              data file be read into static variable imfDayArr.  Array
*              of ImfDay structs is used since it has a more compact
*              memory footprint than the madrec data structure, and
*              imf631127g.002 is a large file.  The old style file
*              imf631127g.001 will be used if imf631127g.002 is not found. Note that
*              BXGSM and BXGSE are equal.
*
*   returns - 0 if successful, -1 if problem finding file
*/
int getImf(int inCount, 
           double * inputArr, 
           int outCount, 
           double * outputArr, 
           FILE * errFile)
{
    /* hard-coded paths to imf files - note that we want to use the         */
    /* file in standard Cedar format (imf631127g.002), but if not there,    */
    /* will try to use the old file (imf631127g.001)                        */
    static const char * imffile = "/experiments/1963/imf/27nov63/imf631127g.002";
    
    static const char * imffile_old = "/experiments/1963/imf/27nov63/imf631127g.001";
    
    /* hard coded parameter codes from file */
    static const int bxgsm_parcode   = 2204;
    static const int bygsm_parcode   = 2206;
    static const int bzgsm_parcode   = 2208;
    static const int bygse_parcode   = 2216;
    static const int bzgse_parcode   = 2218;
    static const int swden_parcode   = 2232;
    static const int swspd_parcode   = 2234;
    static const int swq_parcode     = 2236;
    
    /* static data so that file is only loaded once */
    static ImfDay * imfDayArr = NULL;       /* pointer to in-memory version of file */
    static int usingOldStyle = 0;
    static int numDaysInFile = 0;
    static double bxgsm_scale = 0.0;
    static double bygsm_scale = 0.0;
    static double bzgsm_scale = 0.0;
    static double bygse_scale = 0.0;
    static double bzgse_scale = 0.0;
    static double swden_scale = 0.0;
    static double swspd_scale = 0.0;
    static double swq_scale = 0.0;
    
    Madrec * madrecp = NULL;
    char filename[1000] = "";
    int stat = 0;
    int index  = 0;  /* tell which of 24 daily readings to use */
    int iyr = 0, imd = 0, ihm = 0, ics = 0;
    double aveUt = 0.0;
    
    /* time variables */
    struct tm *timeNow; 
    int jdaynoToday = 0;
    int jdayno1963  = 0;
    int totalDays = 0;
    time_t secNow;
    
    /* binary search variables */
    int low = 0, mid = 0, high = 0;
    int keyFound = 0;
    double midvalue = 0.0; 
    
    aveUt = (inputArr[0] + inputArr[1])/2.0;
    
    /* for thread safety, get the imf_mutex on the way in */
    pthread_mutex_lock(&imf_mutex);
    
    
    /* check if data needs to be loaded */
    if (imfDayArr == NULL)
    {
        /* create filename */
        cedarGetMadroot(filename);
        strcat(filename, imffile);
        
        /* malloc an array large enough to hold enough data */
        /* to go from 11/27/1963 to today                     */
        /* First get today's Julian day number              */
        secNow = time(NULL);
        timeNow = gmtime(&secNow);
        /* note that tm_mon is 0-11 - add 1 to convert to 1-12 */
        jdaynoToday = jday(timeNow->tm_mday,
                           (timeNow->tm_mon) + 1,
                           (timeNow->tm_year) + 1900);
        jdayno1963 = jday(27,11,1963);
	/* we may have data up to the end of the present year */
        totalDays = 367 + (jdaynoToday - jdayno1963);
        
        /* malloc imfDayArr */
        if ((imfDayArr = (ImfDay *)malloc(sizeof(ImfDay)*(totalDays)))==0)
        {
            perror("malloc");
            exit(-1);
        }
        
        
        /* Create a madrec object */
        madrecp = madrecCreate();
        if (strcmp(NO_ERR_STR, madrecGetError(madrecp)))
        {
            fprintf(errFile, "%s\n", madrecGetError(madrecp));
            fflush(errFile);
            outputArr[0] = missing;
            outputArr[1] = missing;
            outputArr[2] = missing;
            outputArr[3] = missing;
            outputArr[4] = missing;
            outputArr[5] = missing;
            outputArr[6] = missing;
            outputArr[7] = missing;
            outputArr[8] = missing;
            outputArr[9] = missing;
            madrecp = NULL;
            /* release mutex */
            pthread_mutex_unlock(&imf_mutex);
            return (-1);
        }

        /* Read the parameter code table */
        cedarReadParCodes();

        /* Connect the madrec object to a madrigal file for sequential read*/
        madrecOpen(madrecp, 1, filename);
        if (strcmp(NO_ERR_STR, madrecGetError(madrecp)))
        {
            /* error opening file, try opening old style */
            madrecDestroy(madrecp);
            madrecp = NULL;
            madrecp = madrecCreate();
            cedarGetMadroot(filename);
            strcat(filename, imffile_old);
            usingOldStyle = 1;
            madrecOpen(madrecp, 1, filename);
            if (strcmp(NO_ERR_STR, madrecGetError(madrecp)))
            {
                fprintf(errFile, "Problem opening imffile %s: %s\n", filename, madrecGetError(madrecp));
                fflush(errFile);
                madrecDestroy(madrecp);
                outputArr[0] = missing;
                outputArr[1] = missing;
                outputArr[2] = missing;
                outputArr[3] = missing;
                outputArr[4] = missing;
                outputArr[5] = missing;
                outputArr[6] = missing;
                outputArr[7] = missing;
                outputArr[8] = missing;
                outputArr[9] = missing;
                madrecp = NULL;
                /* release mutex */
                pthread_mutex_unlock(&imf_mutex);
                return (-1);
            }
         }
         
         if (usingOldStyle == 0)
         {
             /* load imf631127g.002 into imfDayArr */
             /* loop through 24 records at a time */
             index = 0;
             while ((stat=madrecGetNextRec(madrecp)) == 0) 
             {
                 /* skip HEADER or CATALOG records */
                 if (!isDataRecord(madrecp->recordp))
                     continue;
                 if (index == 0)
                 {
                     /* start new imfDay */
                     numDaysInFile++;
                     assert(totalDays >= numDaysInFile);
                     imfDayArr[numDaysInFile - 1].key = cedarGetStartIndex(madrecp->recordp);
                 }
                 /* load data from this record */
                 imfDayArr[numDaysInFile - 1].imf1hour[index].bxgsm = cedarGet1dInt(madrecp->recordp, bxgsm_parcode);
                 imfDayArr[numDaysInFile - 1].imf1hour[index].bygsm = cedarGet1dInt(madrecp->recordp, bygsm_parcode);
                 imfDayArr[numDaysInFile - 1].imf1hour[index].bzgsm = cedarGet1dInt(madrecp->recordp, bzgsm_parcode);
                 imfDayArr[numDaysInFile - 1].imf1hour[index].bygse = cedarGet1dInt(madrecp->recordp, bygse_parcode);
                 imfDayArr[numDaysInFile - 1].imf1hour[index].bzgse = cedarGet1dInt(madrecp->recordp, bzgse_parcode);
                 imfDayArr[numDaysInFile - 1].imf1hour[index].swden = cedarGet1dInt(madrecp->recordp, swden_parcode);
                 imfDayArr[numDaysInFile - 1].imf1hour[index].swspd = cedarGet1dInt(madrecp->recordp, swspd_parcode);
                 imfDayArr[numDaysInFile - 1].imf1hour[index].swq = cedarGet1dInt(madrecp->recordp, swq_parcode);
                 
                 index++;
                 index = index % 24;
             }
         }
         
         else
         {
             /* load  old-style imf631127g.001 into imfDayArr */

             while ((stat=madrecGetNextRec(madrecp)) == 0) 
             {
                 /* skip HEADER or CATALOG records */
                 if (!isDataRecord(madrecp->recordp))
                     continue;
                     
                 numDaysInFile++;
                 assert(totalDays >= numDaysInFile);
                 imfDayArr[numDaysInFile - 1].key = cedarGetStartIndex(madrecp->recordp);

                 /* load data from this record */
                 for (index = 0; index < 24; index++)
                 {
                     imfDayArr[numDaysInFile - 1].imf1hour[index].bxgsm = cedarGet2dIntValue(madrecp->recordp, bxgsm_parcode, index);
                     imfDayArr[numDaysInFile - 1].imf1hour[index].bygsm = cedarGet2dIntValue(madrecp->recordp, bygsm_parcode, index);
                     imfDayArr[numDaysInFile - 1].imf1hour[index].bzgsm = cedarGet2dIntValue(madrecp->recordp, bzgsm_parcode, index);
                     imfDayArr[numDaysInFile - 1].imf1hour[index].bygse = cedarGet2dIntValue(madrecp->recordp, bygse_parcode, index);
                     imfDayArr[numDaysInFile - 1].imf1hour[index].bzgse = cedarGet2dIntValue(madrecp->recordp, bzgse_parcode, index);
                     imfDayArr[numDaysInFile - 1].imf1hour[index].swden = cedarGet2dIntValue(madrecp->recordp, swden_parcode, index);
                     imfDayArr[numDaysInFile - 1].imf1hour[index].swspd = cedarGet2dIntValue(madrecp->recordp, swspd_parcode, index);
                     imfDayArr[numDaysInFile - 1].imf1hour[index].swq = cedarGet2dIntValue(madrecp->recordp, swq_parcode, index);
                 }
             }
         
         }
         
         /* get scaling factors, since we're storing data as Int16's */
         bxgsm_scale = cedarGetParScaleFactor(bxgsm_parcode);
         bygsm_scale = cedarGetParScaleFactor(bygsm_parcode);
         bzgsm_scale = cedarGetParScaleFactor(bzgsm_parcode);
         bygse_scale = cedarGetParScaleFactor(bygse_parcode);
         bzgse_scale = cedarGetParScaleFactor(bzgse_parcode);
         swden_scale = cedarGetParScaleFactor(swden_parcode);
         swspd_scale = cedarGetParScaleFactor(swspd_parcode);
         swq_scale = cedarGetParScaleFactor(swq_parcode);
         
         /* release file, since now read into imfDayArr */
	 madrecClose(madrecp);
         madrecDestroy(madrecp);
         
    } /* data now loaded */
    
    /* release mutex */
    pthread_mutex_unlock(&imf_mutex);
    
    /* find right record using a binary search */
    low = 0;
    high = numDaysInFile - 1;
    keyFound = 0;
    while (low <= high)
    {
        mid = (low+high)/2;
        midvalue = imfDayArr[mid].key;
        if (aveUt >= midvalue && aveUt <= midvalue + SEC_IN_DAY)
        {
            /* right ImfDay found */
            keyFound = 1;
            break;
        }
        else if (aveUt < midvalue)
            high = mid - 1;
        else
            low = mid + 1;
    }
    if (keyFound == 0)
    {
        outputArr[0] = missing;
        outputArr[1] = missing;
        outputArr[2] = missing;
        outputArr[3] = missing;
        outputArr[4] = missing;
        outputArr[5] = missing;
        outputArr[6] = missing;
        outputArr[7] = missing;
        outputArr[8] = missing;
        outputArr[9] = missing;
        return(0);
    }
    
    
    
    /* Since imfDay contains 24 1 hour readings, we need                  */
    /* to determine which of the 24 to use - get hhmm from  dinvmadptr     */
    dinvmadptr(aveUt, &iyr, &imd, &ihm, &ics);
    /* index will be 0-23 */
    index = ihm/100;
    
    
    /* bxgsm */
    if (imfDayArr[mid].imf1hour[index].bxgsm == missingData)
        outputArr[0] = missing;
    else
        outputArr[0] = bxgsm_scale * imfDayArr[mid].imf1hour[index].bxgsm;
    
    /* bygsm */
    if (imfDayArr[mid].imf1hour[index].bygsm == missingData)
        outputArr[1] = missing;
    else
        outputArr[1] = bygsm_scale * imfDayArr[mid].imf1hour[index].bygsm;
    
    /* bzgsm */
    if (imfDayArr[mid].imf1hour[index].bzgsm == missingData)
        outputArr[2] = missing;
    else
        outputArr[2] = bzgsm_scale * imfDayArr[mid].imf1hour[index].bzgsm;
    
    /* bimf */
    /* square root of sum of squares, if none missing */
    if (outputArr[0] != missing && outputArr[1] != missing && outputArr[2] != missing)
        outputArr[3] = sqrt(pow(outputArr[0],2.0) + pow(outputArr[1],2.0) + pow(outputArr[2],2.0));
    else
        outputArr[3] = missing;
        
    /* bxgse (equals bxgsm) */
    outputArr[4] = outputArr[0];
    
    /* bygse */
    if (imfDayArr[mid].imf1hour[index].bygse == missingData)
        outputArr[5] = missing;
    else
        outputArr[5] = bygse_scale * imfDayArr[mid].imf1hour[index].bygse;
    
    /* bzgse */
    if (imfDayArr[mid].imf1hour[index].bzgse == missingData)
        outputArr[6] = missing;
    else
        outputArr[6] = bzgse_scale * imfDayArr[mid].imf1hour[index].bzgse;
    
    /* swden */
    if (imfDayArr[mid].imf1hour[index].swden == missingData)
        outputArr[7] = missing;
    else
        outputArr[7] = swden_scale * imfDayArr[mid].imf1hour[index].swden;
    
    /* swspd */
    if (imfDayArr[mid].imf1hour[index].swspd == missingData)
        outputArr[8] = missing;
    else
        outputArr[8] = swspd_scale * imfDayArr[mid].imf1hour[index].swspd;
    
    /* swq */
    if (imfDayArr[mid].imf1hour[index].swq == missingData)
        outputArr[9] = missing;
    else
        outputArr[9] = swq_scale * imfDayArr[mid].imf1hour[index].swq;  
    
    return (0);
}


/***********************************************************************
*
* getIri 
*   arguments: 
*      inCount (num inputs) = 5 (UT1, UT2, GDLAT, GDLON, GDALT)
*      inputArr - double array holding:
*                 UT1 - UT at record start
		  UT2 - UT at record end
		  GDLAT - geodetic latitude
		  GLON - geodetic longitude
		  GDALT - geodetic altitude
		  
		  
*      outCount (num outputs) = 11 (NE_IRI, NEL_IRI, TN_IRI, TI_IRI, TE_IRI, 
*                                   PO+_IRI, PNO+_IRI, PO2+_IRI, PHE+_IRI, PH+_IRI, PN+_IRI)
*      outputArr - double array holding:
*                  NE_IRI - electron density
		   NEL_IRI - log of electron density
		   TN_IRI - IRI neutral temperature
		   TI_IRI - IRI ion temperature
		   TE_IRI - IRI electron temperature
		   PO+_IRI - IRI composition [O+]/Ne
		   PNO+_IRI - IRI compostion [NO+]/Ne
		   PO2_IRI - IRI composition [O2+]/Ne
		   PHE+_IRI - IRI composition [HE+]/Ne
		   PH+_IRI - IRI composition [H+]/Ne
		   PN+_IRI  - IRI composition [N+]/Ne
		   
*     Written by Alicia Fernandez, May 2007
*   
*
*   returns - 0 (successful)
*/
int getIri (int inCount, 
            double * inputArr, 
            int outCount, 
            double * outputArr, 
	    FILE * errFile)
{  
    int year;
    int month; 
    int day;
    int hour;
    int min; 
    int sec;
    int imd;
    int ihm;
    int ics;
    int time;
    double gdlat;
    double glon;
    double gdalt;
    double iri[11];
	
    int result = 0; 
    
    time = (int) inputArr[0];
     
     /* convert time into required form */
    dinvmadptr(time, &year, &imd, &ihm, &ics);
    month = imd/100;
    day = imd - month*100;
    hour = ihm/100;
    min = ihm - hour*100;
    sec = ics/100;

        
    gdlat = inputArr[2];
    glon  = inputArr[3];
    gdalt = inputArr[4];
      
    result = run_iri(year,
                     month,
                     day,
                     hour,
                     min,
                     sec,
                     gdlat,
                     glon,
                     gdalt,
                     iri);
     
      
    if (result != 0)
    {
        outputArr[0] =  missing;
	outputArr[1] =  missing;
    	outputArr[2] =  missing;
    	outputArr[3] =  missing;
    	outputArr[4] =  missing;
    	outputArr[5] =  missing; 
    	outputArr[6] =  missing;
    	outputArr[7] =  missing; 
    	outputArr[8] =  missing;
    	outputArr[9] =  missing;
    	outputArr[10] = missing;
    }
    else
    {   
        outputArr[0] =  iri[0];
        outputArr[1] =  iri[1];
        outputArr[2] =  iri[2];
    	outputArr[3] =  iri[3];
    	outputArr[4] =  iri[4];
        outputArr[5] =  iri[5]; 
    	outputArr[6] =  iri[6];
    	outputArr[7] =  iri[7]; 
    	outputArr[8] =  iri[8];
    	outputArr[9] =  iri[9];
    	outputArr[10] = iri[10];
     }	
     
     return (0);
    
}


/* * * * * * * Methods that use multiple rows * * * * * * */



/***********************************************************************
*
* getTestAveAlt   dummy method that prints UT, then gets lowest value
*                 of gdlat as 1d parameter and average value as 2D parameter
*
*   arguments: 
*      numRows - number of rows of 2D data in record
*      inCount (num inputs) = 2 (UT1, GDALT)
*      inputArr - array of double arrays holding:
*                 UT - 1D parameter
*                 GDALT - 2D parameter
*      outCount (num outputs) = 2 (LOW_GDALT, AVE_GDALT)
*      outputArr - array of double arrays holding:
*                 LOW_GDALT (len = 1)
*                 AVE_GDALT (len = numRows)
*
*   Algorithm: Average all GDALT values and find lowest.  This
*              is only a dummy method
*
*   returns - 0 
*/
int getTestAveAlt(int numRows,
                  int inCount,
                  double ** inputArr, 
                  int outCount, 
                  double ** outputArr, 
                  FILE * errFile)
{
    double low_gdlat = 1e20;
    double sum_gdalt = 0.0;
    double ave_gdalt = 0.0;
    int numGoodRows = 0;
    int i = 0;
    
    printf("Just called getTestAveAlt with UT = %f\n", inputArr[0][0]);
    
    /* loop over the rows */
    for (i=0; i<numRows; i++)
    {
        /* is this the lowest gdlat? */
        if (inputArr[1][i] < low_gdlat && inputArr[1][i] != missing)
            low_gdlat = inputArr[1][i];
            
        if (inputArr[1][i] != missing)
        {
            numGoodRows++;
            sum_gdalt += inputArr[1][i];
        }
    }
    
    /* calculate average if any data found */
    if (numGoodRows > 0)
        ave_gdalt = sum_gdalt/numGoodRows;
    else
        ave_gdalt = missing;
    
    
    /* set output values */
    
    /* set 1D LOW_GDALT */
    outputArr[0][0] = low_gdlat;
    
    /* set 2D AVE_GDALT */
    /* loop over the rows */
    for (i=0; i<numRows; i++)
        outputArr[1][i] = ave_gdalt;
    
    return (0);
}



