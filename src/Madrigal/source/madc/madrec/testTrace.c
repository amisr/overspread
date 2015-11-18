/*  $Id: testTrace.c,v 1.2 2004/12/22 19:56:26 brideout Exp $ */

#include <stdio.h>
#include <madDeriveMethods.h>

int
main(argc, argv)
int argc;
char *argv[];
{
    int result = 0;
    double gdlat, glon, gdalt, stopAlt, end_gdlat, end_glon, end_gdalt;
    int model, qualifier;


    printf("Test C Field trace package\n");
    
    printf("get 900 km NH intercept Tsyg starting from lat, lon, alt = 10, 290, 10000:\n");
    gdlat = 10.0;
    glon = 290.0;
    gdalt = 10000.0;
    stopAlt = 900.0;
    model = 0;
    qualifier = 1;

    result = traceMagneticField(2004,
                       1,
		       1,
		       0,
		       0,
		       0,
                       gdlat,
		       glon,
		       gdalt,
		       model,
		       qualifier,
		       stopAlt,
		       &end_gdlat,
		       &end_glon,
		       &end_gdalt);
    if (result == 0)	       
        printf("Result: gdlat = %f, glon = %f, gdalt = %f\n", end_gdlat,
	end_glon, end_gdalt);
    else
       printf("Result: failed\n");
       
       
    printf("get 900 km SH intercept Tsyg starting from lat, lon, alt = 10, 290, 10000:\n");
    qualifier = 2;

    result = traceMagneticField(2004,
                       1,
		       1,
		       0,
		       0,
		       0,
                       gdlat,
		       glon,
		       gdalt,
		       model,
		       qualifier,
		       stopAlt,
		       &end_gdlat,
		       &end_glon,
		       &end_gdalt);
    if (result == 0)	       
        printf("Result: gdlat = %f, glon = %f, gdalt = %f\n", end_gdlat,
	end_glon, end_gdalt);
    else
       printf("Result: failed\n"); 
       
    printf("get 900 km conjugate Tsyg starting from lat, lon, alt = 10, 290, 10000:\n");
    qualifier = 0;

    result = traceMagneticField(2004,
                       1,
		       1,
		       0,
		       0,
		       0,
                       gdlat,
		       glon,
		       gdalt,
		       model,
		       qualifier,
		       stopAlt,
		       &end_gdlat,
		       &end_glon,
		       &end_gdalt);
    if (result == 0)	       
        printf("Result: gdlat = %f, glon = %f, gdalt = %f\n", end_gdlat,
	end_glon, end_gdalt);
    else
       printf("Result: failed\n");   
       
       
    printf("get 900 km apex Tsyg starting from lat, lon, alt = 10, 290, 10000:\n");
    qualifier = 3;

    result = traceMagneticField(2004,
                       1,
		       1,
		       0,
		       0,
		       0,
                       gdlat,
		       glon,
		       gdalt,
		       model,
		       qualifier,
		       stopAlt,
		       &end_gdlat,
		       &end_glon,
		       &end_gdalt);
    if (result == 0)	       
        printf("Result: gdlat = %f, glon = %f, gdalt = %f\n", end_gdlat,
	end_glon, end_gdalt);
    else
       printf("Result: failed\n"); 
       
    printf("get 900 SH intercept Tsyg starting from lat, lon, alt = 10, 290, 900:\n");
    gdalt = 900.0;
    qualifier = 2;

    result = traceMagneticField(2004,
                       1,
		       1,
		       0,
		       0,
		       0,
                       gdlat,
		       glon,
		       gdalt,
		       model,
		       qualifier,
		       stopAlt,
		       &end_gdlat,
		       &end_glon,
		       &end_gdalt);
    if (result == 0)	       
        printf("Result: gdlat = %f, glon = %f, gdalt = %f\n", end_gdlat,
	end_glon, end_gdalt);
    else
       printf("Result: failed\n"); 
       
    printf("get 900 SH intercept Tsyg starting from lat, lon, alt = 70, 290, 900:\n");
    gdlat = 70.0;
    qualifier = 2;

    result = traceMagneticField(2004,
                       1,
		       1,
		       0,
		       0,
		       0,
                       gdlat,
		       glon,
		       gdalt,
		       model,
		       qualifier,
		       stopAlt,
		       &end_gdlat,
		       &end_glon,
		       &end_gdalt);
    if (result == 0)	       
        printf("Result: gdlat = %f, glon = %f, gdalt = %f\n", end_gdlat,
	end_glon, end_gdalt);
    else
    {
       printf("Result: failed\n");
       printf("Error result: gdlat = %g, glon = %g, gdalt = %g\n", end_gdlat,
	end_glon, end_gdalt);
    }
       
    printf("get 1000 conjugate IGRF starting from lat, lon, alt = 30, 290, 1000:\n");
    gdalt = 1000.0;
    gdlat = 30.0;
    qualifier = 0;
    model = 1;

    result = traceMagneticField(2004,
                       1,
		       1,
		       0,
		       0,
		       0,
                       gdlat,
		       glon,
		       gdalt,
		       model,
		       qualifier,
		       stopAlt,
		       &end_gdlat,
		       &end_glon,
		       &end_gdalt);
    if (result == 0)	       
        printf("Result: gdlat = %f, glon = %f, gdalt = %f\n", end_gdlat,
	end_glon, end_gdalt);
    else
       printf("Result: failed\n");
       
    printf("get SH 0 km intercept IGRF starting from lat, lon, alt = 30, 290, 1000:\n");
    stopAlt = 0.0;
    qualifier = 2;

    result = traceMagneticField(2004,
                       1,
		       1,
		       0,
		       0,
		       0,
                       gdlat,
		       glon,
		       gdalt,
		       model,
		       qualifier,
		       stopAlt,
		       &end_gdlat,
		       &end_glon,
		       &end_gdalt);
    if (result == 0)	       
        printf("Result: gdlat = %f, glon = %f, gdalt = %f\n", end_gdlat,
	end_glon, end_gdalt);
    else
       printf("Result: failed\n");
       
    printf("get NH 50 km intercept IGRF starting from lat, lon, alt = 30, 290, 1000:\n");
    stopAlt = 50.0;
    qualifier = 1;

    result = traceMagneticField(2004,
                       1,
		       1,
		       0,
		       0,
		       0,
                       gdlat,
		       glon,
		       gdalt,
		       model,
		       qualifier,
		       stopAlt,
		       &end_gdlat,
		       &end_glon,
		       &end_gdalt);
    if (result == 0)	       
        printf("Result: gdlat = %f, glon = %f, gdalt = %f\n", end_gdlat,
	end_glon, end_gdalt);
    else
       printf("Result: failed\n");
       
    printf("get apex IGRF starting from lat, lon, alt = 30, 290, 1000:\n");
    stopAlt = 50.0;
    qualifier = 3;

    result = traceMagneticField(2004,
                       1,
		       1,
		       0,
		       0,
		       0,
                       gdlat,
		       glon,
		       gdalt,
		       model,
		       qualifier,
		       stopAlt,
		       &end_gdlat,
		       &end_glon,
		       &end_gdalt);
    if (result == 0)	       
        printf("Result: gdlat = %f, glon = %f, gdalt = %f\n", end_gdlat,
	end_glon, end_gdalt);
    else
       printf("Result: failed\n");
       
    printf("get apex Tsyg starting from lat, lon, alt = 30, 290, 1000:\n");
    model = 0;

    result = traceMagneticField(2004,
                       1,
		       1,
		       0,
		       0,
		       0,
                       gdlat,
		       glon,
		       gdalt,
		       model,
		       qualifier,
		       stopAlt,
		       &end_gdlat,
		       &end_glon,
		       &end_gdalt);
    if (result == 0)	       
        printf("Result: gdlat = %f, glon = %f, gdalt = %f\n", end_gdlat,
	end_glon, end_gdalt);
    else
       printf("Result: failed\n");
       
    printf("get GSM XY Tsyg starting from lat, lon, alt = 30, 290, 1000:\n");
    model = 0;
    qualifier = 4;

    result = traceMagneticField(2004,
                       1,
		       1,
		       0,
		       0,
		       0,
                       gdlat,
		       glon,
		       gdalt,
		       model,
		       qualifier,
		       stopAlt,
		       &end_gdlat,
		       &end_glon,
		       &end_gdalt);
    if (result == 0)	       
        printf("Result: GSM X = %f, GSM Y = %f, GSM Z = %f\n", end_gdlat,
	end_glon, end_gdalt);
    else
       printf("Result: failed\n");
       
    return(0);
}
