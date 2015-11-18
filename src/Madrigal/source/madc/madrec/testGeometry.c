/*  $Id: testGeometry.c,v 1.3 2003/01/14 14:40:45 brideout Exp $ */

#include <stdio.h>
#include <geometry.h>

int
main(argc, argv)
int argc;
char *argv[];
{
    int kinst, i;
    double sprod();
    double a[3], b[3], c[3];
    double adota, adotb, bdota, bdotb;
    double fx, fy, fz, x, y, z, fr, ft, fp, r, theta, phi;
    double sr, slat, slon, az, el, range, pr, glat, glon,
          gdlat, gdalt, gclat, rkm, sgdlat, sgdalt;
    double rfx, rfy, rfz, pfx, pfy, pfz;
    double rfr, rft, rfp, rr, rt, rp, cst, csp, csr, cx, cy, cz;
    double sunrise = 0.0;
    double sunset = 0.0;

    printf("Test C geometry package\n");

    a[0] = 1.0;
    a[1] = 2.0;
    a[2] = 3.0;
    b[0] = 2.0;
    b[1] = 3.0;
    b[2] = 4.0;

    printf("test vprod\n");    
    adota = sprod(a, a);
    printf("adota =  %f\n", adota);
    adotb = sprod(a, b);
    printf("adotb =  %f\n", adotb);
    bdota = sprod(b, a);
    printf("bdota =  %f\n", bdota);
    bdotb = sprod(b, b);
    printf("bdotb =  %f\n", bdotb);
    printf("\n");

    printf("test vadd\n");    
    vadd(a,a,c);
    printf("aplusa = %f %f %f\n", c[0],c[1],c[2]);
    vadd(a,b,c);
    printf("aplusb = %f %f %f\n", c[0],c[1],c[2]);
    vadd(b,a,c);
    printf("bplusa = %f %f %f\n", c[0],c[1],c[2]);
    vadd(b,b,c);
    printf("bplusb = %f %f %f\n", c[0],c[1],c[2]);
    printf("\n");

    printf("test vsub\n");    
    vsub(a,a,c);
    printf("aminusa = %f %f %f\n", c[0],c[1],c[2]);
    vsub(a,b,c);
    printf("aminusb = %f %f %f\n", c[0],c[1],c[2]);
    vsub(b,a,c);
    printf("bminusa = %f %f %f\n", c[0],c[1],c[2]);
    vsub(b,b,c);
    printf("bminusb = %f %f %f\n", c[0],c[1],c[2]);
    printf("\n");

    printf("Test csconv\n");
    r = 6370.0;
    theta = 48.0;
    phi = 290.0;
    csconv(&x, &y, &z, &r, &theta, &phi, 2);
    printf("x, y, z = %f %f %f\n", x, y, z);
    csconv(&x, &y, &z, &r, &theta, &phi, 1);
    printf("r, theta, phi = %f %f %f\n", r, theta, phi);
    printf("\n");

    printf("Test vctcnv\n");
    fr = 200.0;
    ft = 1000.0;
    fp = 500.0;
    vctcnv(&fx, &fy, &fz, &x, &y, &z,
           &fr, &ft, &fp, &r, &theta, &phi, 2);
    printf("fx, fy, fz = %f %f %f\n", fx, fy, fz);
    printf("x, y, z = %f %f %f \n", x, y, z);
    vctcnv(&fx, &fy, &fz, &x, &y, &z,
           &fr, &ft, &fp, &r, &theta, &phi, 1);
    printf("fr, ft, fp = %f %f %f \n", fr, ft, fp);
    printf("r, theta, phi = %f %f %f\n", r, theta, phi);
    printf("\n");
    
    printf( "Test point and look\n");
    /* Chatanika */
    sr = 6361.29555;
    slat = 62.56771;
    slon = 221.04933;
    az = 0.0;
    el = 90.0;
    range = 0.0;
    az = 45.0;
    el = 45.0;
    range = 1000.0;

    printf("sr, slat, slon = %f %f %f\n", sr, slat, slon);
    printf("az, el, range = %f %f %f\n", az, el, range);
    point(&sr, &slat, &slon, &az, &el, &range, &pr, &glat, &glon);
    printf( "pr, glat, glon = %f %f %f\n", pr, glat, glon);
    look (&sr, &slat, &slon, &pr, &glat, &glon, &az, &el, &range);
    printf("az, el, range = %f %f %f\n", az, el, range);
    printf("\n");
    
    printf("Test convrt\n");
    gclat = glat;
    rkm = pr;
    printf("gclat rkm = %f %f\n", gclat, rkm);
    convrt(2, &gdlat, &gdalt, &gclat, &rkm);
    printf("gdlat gdalt = %f %f\n", gdlat, gdalt);
    convrt(1, &gdlat, &gdalt, &gclat, &rkm);
    printf("gclat rkm = %f %f\n", gclat, rkm);
    printf("\n");

    printf("Test rpcart\n");
    sgdlat = 42.620;
    slon = 288.508;
    sgdalt = 0.146;
    convrt(1, &sgdlat, &sgdalt, &slat, &sr);
    printf("slat sr = %f %f\n", slat, sr);
    az = 90.0;
    el = 10.0;
    range = 1000.0;
    /* Convert radar propagation vector and observation point position
       to earth centered Cartesian coordinates */
    rpcart(&sr, &slat, &slon, &az, &el, &range, 
           &rfx, &rfy, &rfz, &pfx, &pfy, &pfz);
    printf("rfx rfy rfz = %f %f %f\n", rfx, rfy, rfz);
    printf("pfx pfy pfz = %f %f %f\n", pfx, pfy, pfz);
    printf("\n");

    printf("Test gdv\n");
    /* Calculate direction cosines of radar beam with respect to
       geocentric south, east, up */
    vctcnv(&rfx, &rfy, &rfz, &pfx, &pfy, &pfz,
           &rfr, &rft, &rfp, &rr, &rt, &rp, 1);
    cst = rft/range;
    csp = rfp/range;
    csr = rfr/range;
    printf("cst,csp,csr = %f %f %f\n", cst, csp, csr);

    /* Compute geodetic direction cosines. The direction cosines
       are with respect to  x (north), y (east), z (down) */
    gdv(&gdlat, &gclat, &csr, &cst, &csp, &cx, &cy, &cz);
    printf("cx cy cz = %f %f %f\n", cx, cy, cz);
    printf("cx*cx+cy*cy+cz*cz = %f\n", cx*cx+cy*cy+cz*cz);
    printf("\n");

    /* Test los2geodetic */
    printf("Test los2geodetic\n");
    kinst = 31;
    az = 45.0;
    el = 10.0;
    range = 1000.0;
    los2geodetic(kinst, az, el, range, &gdlat, &glon, &gdalt);
    printf("gdlat, glon, gdalt = %f %f %f\n\n", gdlat, glon, gdalt);
    
    /* test solar distance */
    printf("Solar distance at 7/27/1980 is %g\n\n", 
            solardist(964656000.0));
            
    /* test shadowheight, sunrise_set through 24 h at 42.0, -70.0 */
    printf("Test shadowheight, sunrise_set through 24 h at 42.0, -70.0\n\n");
    for (i=0; i<25; i++)
    {
        printf("Shadow height at %i = %f\n", i,
                shadowheight(1516492800.0 + i*3600.0, 42.0, -70.0));
                
        sunrise_set(1516492800.0 + i*3600.0, 42.0, -70.0, 20.0, &sunrise, &sunset);
        printf("At 20 km, sunrise = %f and sunset = %f\n", (sunrise-1516492800.0)/3600.0, (sunset-1516492800.0)/3600.0);
        sunrise_set(1516492800.0 + i*3600.0, 42.0, -70.0, 2000.0, &sunrise, &sunset);
        printf("At 2000 km, sunrise = %f and sunset = %f\n\n", (sunrise-1516492800.0)/3600.0, (sunset-1516492800.0)/3600.0);
        
    }

    return(0);
}
