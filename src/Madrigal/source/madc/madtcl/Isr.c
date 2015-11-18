/*  $Id: Isr.c,v 1.2 2002/03/25 16:11:23 jmh Exp $ */

#include <stdio.h>
#include <geometry.h>
#include <tcl.h>

int
Isr_Init(interp)
    Tcl_Interp *interp;		/* Interpreter to add extra commands */
{
    int Isr_PointCmd(), Isr_LookCmd(), Isr_DircosCmd(),
        Isr_Geodetic2geocentricCmd(), Isr_Geocentric2geodeticCmd();

    Tcl_CreateCommand(interp, "isr_point", Isr_PointCmd,
        (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

    Tcl_CreateCommand(interp, "isr_look", Isr_LookCmd,
        (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

    Tcl_CreateCommand(interp, "isr_geodetic2geocentric", 
        Isr_Geodetic2geocentricCmd,
        (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

    Tcl_CreateCommand(interp, "isr_geocentric2geodetic", 
        Isr_Geocentric2geodeticCmd,
        (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

    Tcl_CreateCommand(interp, "isr_dircos", Isr_DircosCmd,
        (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

    return TCL_OK;
}


int Isr_PointCmd(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char *argv[];
{
    int nfound;
    double slat, slon, salt, az, el, range, plat, plon, palt;
    double sgclat, sr, pgclat, pr;

    if (argc != 7) {
        interp->result = "wrong number of arguments";
        return TCL_ERROR;
    }
    if ((nfound=sscanf(argv[1], "%lf", &slat)) != 1) {
        interp->result = "slat not a number";
        return TCL_ERROR;
    }
    if ((nfound=sscanf(argv[2], "%lf", &slon)) != 1) {
        interp->result = "slon not a number";
        return TCL_ERROR;
    }
    if ((nfound=sscanf(argv[3], "%lf", &salt)) != 1) {
        interp->result = "salt not a number";
        return TCL_ERROR;
    }
    if ((nfound=sscanf(argv[4], "%lf", &az)) != 1) {
        interp->result = "az not a number";
        return TCL_ERROR;
    }
    if ((nfound=sscanf(argv[5], "%lf", &el)) != 1) {
        interp->result = "el not a number";
        return TCL_ERROR;
    }
    if ((nfound=sscanf(argv[6], "%lf", &range)) != 1) {
        interp->result = "range not a number";
        return TCL_ERROR;
    }

    /* Convert station coordinates from geodetic to geocentric */
    (void) convrt(1, &slat, &salt, &sgclat, &sr);

    /* Compute geocentric coordinates of specified point */
    (void) point(&sr, &sgclat, &slon, &az, &el, &range,
                 &pr, &pgclat, &plon);

    /* Convert observation point coordinates from geocentric to geodetic */
    (void) convrt(2, &plat, &palt, &pgclat, &pr);

    sprintf(interp->result, "%f %f %f", plat, plon, palt);

    return TCL_OK;
}


int Isr_LookCmd(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char *argv[];
{
    int nfound;
    double slat, slon, salt, plat, plon, palt, az, el, range;
    double sgclat, sr, pgclat, pr;

    if (argc != 7) {
        interp->result = "wrong number of arguments";
        return TCL_ERROR;
    }
    if ((nfound=sscanf(argv[1], "%lf", &slat)) != 1) {
        interp->result = "slat not a number";
        return TCL_ERROR;
    }
    if ((nfound=sscanf(argv[2], "%lf", &slon)) != 1) {
        interp->result = "slon not a number";
        return TCL_ERROR;
    }
    if ((nfound=sscanf(argv[3], "%lf", &salt)) != 1) {
        interp->result = "salt not a number";
        return TCL_ERROR;
    }
    if ((nfound=sscanf(argv[4], "%lf", &plat)) != 1) {
        interp->result = "plat not a number";
        return TCL_ERROR;
    }
    if ((nfound=sscanf(argv[5], "%lf", &plon)) != 1) {
        interp->result = "plon not a number";
        return TCL_ERROR;
    }
    if ((nfound=sscanf(argv[6], "%lf", &palt)) != 1) {
        interp->result = "palt not a number";
        return TCL_ERROR;
    }

    /* Convert station coordinates from geodetic to geocentric */
    (void) convrt(1, &slat, &salt, &sgclat, &sr);

    /* Convert observation point coordinates from geodetic to geocentric */
    (void) convrt(1, &plat, &palt, &pgclat, &pr);

    /* Compute radar pointing coordinates */
    (void) look(&sr, &sgclat, &slon, &pr, &pgclat, &plon,
                 &az, &el, &range);

    sprintf(interp->result, "%f %f %f", az, el, range);

    return TCL_OK;
}


int Isr_Geodetic2geocentricCmd(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char *argv[];
{
    int nfound;
    double gdlat, lon, gdalt;
    double gclat, gcr;

    if (argc != 4) {
        interp->result = "wrong number of arguments";
        return TCL_ERROR;
    }
    if ((nfound=sscanf(argv[1], "%lf", &gdlat)) != 1) {
        interp->result = "gdlat not a number";
        return TCL_ERROR;
    }
    if ((nfound=sscanf(argv[2], "%lf", &lon)) != 1) {
        interp->result = "lon not a number";
        return TCL_ERROR;
    }
    if ((nfound=sscanf(argv[3], "%lf", &gdalt)) != 1) {
        interp->result = "gdalt not a number";
        return TCL_ERROR;
    }

    /* Convert coordinates from geodetic to geocentric */
    (void) convrt(1, &gdlat, &gdalt, &gclat, &gcr);

    sprintf(interp->result, "%f %f %f", gclat, lon, gcr);

    return TCL_OK;
}


int Isr_Geocentric2geodeticCmd(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char *argv[];
{
    int nfound;
    double gclat, gcr;
    double gdlat, lon, gdalt;

    if (argc != 4) {
        interp->result = "wrong number of arguments";
        return TCL_ERROR;
    }
    if ((nfound=sscanf(argv[1], "%lf", &gclat)) != 1) {
        interp->result = "gclat not a number";
        return TCL_ERROR;
    }
    if ((nfound=sscanf(argv[2], "%lf", &lon)) != 1) {
        interp->result = "lon not a number";
        return TCL_ERROR;
    }
    if ((nfound=sscanf(argv[3], "%lf", &gcr)) != 1) {
        interp->result = "gcr not a number";
        return TCL_ERROR;
    }

    /* Convert coordinates from geodetic to geocentric */
    (void) convrt(2, &gdlat, &gdalt, &gclat, &gcr);

    sprintf(interp->result, "%f %f %f", gdlat, lon, gdalt);

    return TCL_OK;
}


int Isr_DircosCmd(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char *argv[];
{
    int nfound;
    double slat, slon, salt, az, el, range, plat, plon, palt;
    double sgclat, sr, pgclat, pr;
    double rfx, rfy, rfz, pfx, pfy, pfz;
    double rfr, rft, rfp, rr, rt, rp, cst, csp, csr, cx, cy, cz;

    if (argc != 7) {
        interp->result = "wrong number of arguments";
        return TCL_ERROR;
    }
    if ((nfound=sscanf(argv[1], "%lf", &slat)) != 1) {
        interp->result = "slat not a number";
        return TCL_ERROR;
    }
    if ((nfound=sscanf(argv[2], "%lf", &slon)) != 1) {
        interp->result = "slon not a number";
        return TCL_ERROR;
    }
    if ((nfound=sscanf(argv[3], "%lf", &salt)) != 1) {
        interp->result = "salt not a number";
        return TCL_ERROR;
    }
    if ((nfound=sscanf(argv[4], "%lf", &az)) != 1) {
        interp->result = "az not a number";
        return TCL_ERROR;
    }
    if ((nfound=sscanf(argv[5], "%lf", &el)) != 1) {
        interp->result = "el not a number";
        return TCL_ERROR;
    }
    if ((nfound=sscanf(argv[6], "%lf", &range)) != 1) {
        interp->result = "range not a number";
        return TCL_ERROR;
    }

    /* Convert station coordinates from geodetic to geocentric */
    (void) convrt(1, &slat, &salt, &sgclat, &sr);

    /* Compute geocentric coordinates of specified point */
    (void) point(&sr, &sgclat, &slon, &az, &el, &range,
                 &pr, &pgclat, &plon);

    /* Convert observation point coordinates from geocentric to geodetic */
    (void) convrt(2, &plat, &palt, &pgclat, &pr);

    /* Convert radar propagation vector and observation point position
       to earth centered Cartesian coordinates */
    rpcart(&sr, &sgclat, &slon, &az, &el, &range, 
           &rfx, &rfy, &rfz, &pfx, &pfy, &pfz);

    /* Calculate direction cosines of radar beam with respect to
       geocentric south, east, up */
    vctcnv(&rfx, &rfy, &rfz, &pfx, &pfy, &pfz,
           &rfr, &rft, &rfp, &rr, &rt, &rp, 1);
    cst = rft/range;
    csp = rfp/range;
    csr = rfr/range;

    /* Compute geodetic direction cosines. The direction cosines
       are with respect to  x (north), y (east), z (down) */
    gdv(&plat, &pgclat, &csr, &cst, &csp, &cx, &cy, &cz);

    sprintf(interp->result, "%f %f %f", cx, cy, cz);

    return TCL_OK;
}
