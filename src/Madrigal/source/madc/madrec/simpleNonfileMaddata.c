#include <maddata.h>

/*  This example program shows a simple example of the use of the Maddata library. */
/*  It shows how to use createNonfileMaddata to calculate data without a file.  */
/*  The following is input data: UT1 = 1/20/1998 12:30:00 ,                     */
/*     UT2 = 1/20/1998 12:30:30, kinst = 31 (MLH)                               */
/*  Measured 1D parameters: azm = 30, elm = 45                                   */
/*  Measured 2D parameters: range = {200, 300, 400, 500, 600}                   */
/*  Parameters to show: Azm, Elm, Ut1, Range, Gdalt, Bmag, Kp                   */

int main (int argc, char *argv[])
{

    MadparmList * parmListShow;  /* list of parameters to show */
    MadparmList * parmMeas1D;    /* list of 1D parameters for which data will be given */
    MadparmList * parmMeas2D;    /* list of 2D parameters for which data will be given */
    Maddata * maddata = NULL;
    
    /* input data */
    double input1D[] = {30.0, 45.0};  /* length = 2 since 2 1D parameters azm, elm */
    double * input2D[1] = {0};        /* length = 1 since only 1 2D parameter - range */
    double range[5] = {200.0, 300.0, 400.0, 500.0, 600.0};
    int kinst = 31;
    double ut1=0.0, ut2=0.0;
    
    input2D[0] = range;               /* if other 2D parameters, set them here */

    /* create parmListShow */
    parmListShow = createMadparmList();
    appendMadparm(parmListShow, "azm");
    appendMadparm(parmListShow, "elm");
    appendMadparm(parmListShow, "ut1");
    appendMadparm(parmListShow, "range");
    appendMadparm(parmListShow, "gdalt");
    appendMadparm(parmListShow, "bmag");
    appendMadparm(parmListShow, "kp");
    
    /* create parmMeas1D */
    parmMeas1D = createMadparmList();
    appendMadparm(parmMeas1D, "azm");
    appendMadparm(parmMeas1D, "elm");
    
    /* create parmMeas2D */
    parmMeas2D = createMadparmList();
    appendMadparm(parmMeas2D, "range");
    

    /* get ut1 and ut2 */
    ut1 = getKey(1998, 1, 20, 12, 30, 0);
    ut2 = getKey(1998, 1, 20, 12, 30, 30);

    /* create the data you want */
    maddata = createNonfileMaddata(parmListShow,
                                   ut1,
                                   ut2,
                                   kinst,
                                   parmMeas1D,
                                   parmMeas2D,
                                   5,
                                   input1D,
                                   input2D,
                                   stdout);
                                   
    printf("Finished createMaddata\n");

    
    /* print all data in isprint format */
    classicIsprint(maddata,
                   1,
                   1,
                   0,
                   "Missing",
                   "Assumed",
                   "KnownBad",
                   stdout);
    

                            
    /* clean up */
    destroyMaddata(maddata);
    destroyMadparmList(parmListShow);
    destroyMadparmList(parmMeas1D);
    destroyMadparmList(parmMeas2D);
    return(0);
}

