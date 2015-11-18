/*  $Id: scanpl.h,v 1.3 2009/02/12 19:56:54 brideout Exp $ */

/* madrigal record structure definition  */
/*				         */
/* jmh  09/11/96        original         */

#ifndef _SCANPLH_
#define _SCANPLH_

#include <stdio.h>
#include <fcntl.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <time.h>
#include <madrec.h>
#include <cedar.h>
#include <gpl.h>
#include <axplot.h>

#define MAXGRD 4000
#define MAXPTS 200
#define MAXSCANS 2000
#define BADVAL 0.0

typedef struct scanpl {

    /* Bin Definitions */
    double xbmin;
    double xbmax;
    double xbdel;
    double ybmin;
    double ybmax;
    double ybdel;
    int nxb;
    int nyb;
    int ndb;
    double *xb;
    double *yb;
    double *datab;
} Scanpl;

typedef struct scantab {
    int nptst[2];
    double utt[2];
    double azt[2];
    double elt[2];
    double xt[2][MAXPTS];
    double yt[2][MAXPTS];
    double poplt[2][MAXPTS];
    double tit[2][MAXPTS];
    double tet[2][MAXPTS];
    double vot[2][MAXPTS];
} Scantab;

/* Miscellaneous definitions */
typedef struct
{
    int x;
    int y;
} Rpt;

typedef struct
{
    int x1;
    int y1;
    int x2;
    int y2;
} Rln;

/* Method declarations */
Scanpl *scanplCreate ();
int scanplDestroy (Scanpl *scanpl);

#endif
