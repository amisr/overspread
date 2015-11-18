/*  $Id: geometry.h,v 1.4 2003/01/14 14:35:01 brideout Exp $ */

/* geometry definitions  */
/*				         */
/* jmh  06/06/00        original         */

#ifndef _GEOMETRYH_
#define _GEOMETRYH_
/* Function declarations */

double sprod(double a[3], double b[3]);
int vadd(double a[3], double  b[3], double c[3]);
int vsub(double a[3], double b[3], double c[3]);
int csconv(double *xp, double *yp, double *zp,
           double *rp, double *thetap, double *phip,
           int imode);
int vctcnv(double *fxp, double *fyp, double *fzp,
           double *xp, double *yp, double *zp,
           double *frp, double *ftp, double *fpp,
           double *rp, double *thetap, double *phip,
           int imode);
int point(double *srp, double *slatp, double *slonp,
          double *azp, double *elp, double *rangep,
          double *prp, double *glatp, double *glonp);
int look(double *srp, double *slatp, double *slonp,
         double *prp, double *glatp, double *glonp,
         double *azp, double *elp, double *rangep);
int convrt(int i, double *gdlatp, double *gdaltp,
           double *gclatp, double *rkmp);
int rpcart (double *srp,  double *slatp, double *slonp,
            double *azp,  double *elp,   double *rangep,
            double *rfxp, double *rfyp,  double *rfzp,
            double *pfxp, double *pfyp,  double *pfzp);
int gdv(double *gdlatp, double *gclatp,
        double *frp, double *ftp, double *fpp,
        double *fxp, double *fyp, double *fzp);
int los2geodetic(int kinst, double az, double el, double range,
                 double *gdlatp, double *glonp, double *gdaltp);
void solarzen_az(double ut, double gdlat, double glon, double * szen, double * saz);
double solardist(double ut);
double shadowheight(double ut, double gdlat, double glon);
void sunrise_set(double ut,
                 double gdlat, 
                 double glon,
                 double gdalt,
                 double * sunrise,
                 double * sunset);
#endif
