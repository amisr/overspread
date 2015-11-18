#include <time.h>

#include <maddata.h>
#include <madDeriveEngine.h>

/* creates a test file with az1, daz1, az2, daz2, 
*  el1, del1, el2, del2, range, drange */

int main (int argc, char *argv[])
{
    MadfilterList * madFiltList;
    MadparmList * parmListUse;
    Maddata * maddata = NULL;
    double f1_lower = 30.0;
    double f1_upper = 60.0;
    
    Madrec *madrecp;    /* Output File - type 0-4 */
    int iotype=0, stat=0, record=0;
    int lprol=0, jpar=0, mpar=0, nrow=0, krec=0, kinst=0, kindat=0, ibyr=0, 
        ibmo=0, ibdy=0, ibh=0, ibm=0, ibs=0, ibcs=0, ieyr=0, iemo=0, iedy=0, 
        ieh=0, iem=0, ies=0, iecs=0;
    double range[8] = {100.0, 200.0, 300.0, 400.0, 500.0, 600.0, 700.0, 800.0};
    double drange[8] = {10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0};

    /* Create a madrec object for the output file */
    if ((madrecp = madrecCreate()) == (Madrec *) NULL) {
        fprintf(stderr, "create madrecw: %s\n", madrecGetError(madrecp));
        return(1);
    }

    /* Connect the output madrec object to a madrigal file */
    iotype = 20;
    stat = madrecOpen(madrecp, iotype, "madout");
    fprintf(stderr, "open madrecw: %s\n", madrecGetError(madrecp));

    lprol = 16;
    jpar = 8;
    mpar = 2;
    nrow = 8;
    krec = 1002;
    kinst = 30;
    kindat = 30007;
    ibyr = 2001;
    ibmo = 8;
    ibdy = 20;
    ibh = 0;
    ibm = 0;
    ibs = 0;
    ibcs = 0;
    ieyr = 2001;
    iemo = 8;
    iedy = 20;
    ieh = 23;
    iem = 59;
    ies = 59;
    iecs = 59;
    for (record=0; record<2; record++) {

        ibdy++;
        iedy++;

	/* Create a Cedar record in the madrec object */
        if (madrecp->recordp != (Int16 *)NULL) {
            free(madrecp->recordp);
        }
	madrecp->recordp = cedarCreateRecord(lprol, jpar, mpar, nrow, krec,
					     kinst, kindat, ibyr, ibmo, ibdy, 
					     ibh, ibm, ibs, ibcs, ieyr,
					     iemo, iedy, ieh, iem, ies,
					     iecs);
    
	/* Set 1d parameters */
	stat = cedarSet1dParm(madrecp->recordp, 132, 40.0, 0);
	stat = cedarSet1dParm(madrecp->recordp, -132, 1.0, 1);
        stat = cedarSet1dParm(madrecp->recordp, 133, 50.0, 2);
	stat = cedarSet1dParm(madrecp->recordp, -133, 1.0, 3);
	stat = cedarSet1dParm(madrecp->recordp, 142, 40.0, 4);
	stat = cedarSet1dParm(madrecp->recordp, -142, 1.0, 5);
        stat = cedarSet1dParm(madrecp->recordp, 143, 50.0, 6);
	stat = cedarSet1dParm(madrecp->recordp, -143, 1.0, 7);
    
	/* Set 2d parms */
	stat = cedarSet2dParm(madrecp->recordp, 120, range, 0);
	stat = cedarSet2dParm(madrecp->recordp, -120, drange, 1);
    
	cedarPrintRecord(madrecp->recordp); 
    
	stat = madrecPutNextRec(madrecp);
	fprintf(stderr, "putNextRec madrecw: %s\n", madrecGetError(madrecp));

    }

    stat = madrecClose(madrecp);      
    fprintf(stderr, "close madrecw: %s\n", madrecGetError(madrecp));

    madrecDestroy(madrecp);
    
    /* now use this new file in maddata */

    madFiltList = createMadfilterList();
    appendMadfilter(madFiltList,
                          SINGLE_FILT,
                          1,
                          &f1_lower,
                          &f1_upper,
                          "az1",
                          "");
                          
    /* create parmListUse */
    parmListUse = createMadparmList();
    appendMadparm(parmListUse, "gdlat");
    appendMadparm(parmListUse, "glon");
    appendMadparm(parmListUse, "gdalt");
    appendMadparm(parmListUse, "dgdlat");
    appendMadparm(parmListUse, "dglon");
    appendMadparm(parmListUse, "dgdalt");
    appendMadparm(parmListUse, "azm");
    appendMadparm(parmListUse, "elm");
    appendMadparm(parmListUse, "dazm");
    appendMadparm(parmListUse, "delm");
    appendMadparm(parmListUse, "range");
    appendMadparm(parmListUse, "drange");
    
    /* test of maddata */
    maddata = createMaddata("madout",
                            "Hi",
                            parmListUse,
                            madFiltList,
                            NULL);
    
    simpleMaddataPrint(maddata, stdout);
    
    /* clean up */
    destroyMaddata(maddata);
    destroyMadparmList(parmListUse);
    destroyMadfilterList(madFiltList);
    
    return(0);
}
