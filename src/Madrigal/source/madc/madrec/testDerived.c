#include <time.h>

#include <geometry.h>
#include <maddata.h>
#include <madDeriveEngine.h>

/*  This test program is designed to call all derived parameters
    possible given the standard MLH test file.
*/

int main (int argc, char *argv[])
{
    MadfilterList * madFiltList;
    MadparmList * parmListMeas1DFile = NULL;
    MadparmList * parmListMeas2DFile = NULL;
    MadparmList * parmListDerv1DFile = NULL;
    MadparmList * parmListDerv2DFile = NULL;
    MadparmList * parmListDervFile = NULL;

    int i;   
    char madfile[128] = "";

    int status = 0;
    Maddata * maddata = NULL;
    FILE * fp = NULL;
    double f1_lower = -100.0;
    double f1_upper = 180.0;
    double f2_lower = 1500.0;
    double f2_upper = 1505.0;
   
    
    fp = fopen("junk.txt", "w");


    /* test madFiltList */
    madFiltList = createMadfilterList();
    status = appendMadfilter(madFiltList,
                          SINGLE_FILT,
                          1,
                          &f1_lower,
                          &f1_upper,
                          "azm",
                          "");
    status = appendMadfilter(madFiltList,
                          SINGLE_FILT,
                          1,
                          &f2_lower,
                          &f2_upper,
                          "bhm",
                          "");


    /* get measured parameter list */
    cedarGetMadroot(madfile);
    /*strcat(madfile, "/experiments/1998/mlh/20jan98/mlh980120g.001");*/
    strcat(madfile, "/experiments/1998/mlh/20jan98/mil980120g.002");
    /*strcat(madfile, "/experiments/1997/son/06jan97/son970106g.001");*/
    /*strcat(madfile, "/experiments/1995/jro/01feb95/jic950201g.001");*/
    status = analyzeFileParms(madfile,
                              &parmListMeas1DFile,
                              &parmListMeas2DFile,
                              &parmListDerv1DFile,
                              &parmListDerv2DFile,
                              stdout);

    printf("analyzeFileParms returned status %i\n", status);
    
    /* create a list of all derived parameters */
    parmListDervFile = createMadparmList();
    for (i=0; i<parmListDerv1DFile->numParm; i++)
        appendMadparm(parmListDervFile, parmListDerv1DFile->mnemList[i]);
    for (i=0; i<parmListDerv2DFile->numParm; i++)
        appendMadparm(parmListDervFile, parmListDerv2DFile->mnemList[i]);
    
    if (parmListMeas1DFile == NULL)
    {
        printf("Error from analyzeFileParms\n");
        return -1;
    }


    /* all done with parmList*DFile */
    destroyMadparmList(parmListMeas1DFile);
    destroyMadparmList(parmListMeas2DFile);
    destroyMadparmList(parmListDerv1DFile);
    destroyMadparmList(parmListDerv2DFile);

    
    /* test of maddata */
    maddata = createMaddata(madfile,
                            "Hi",
                            parmListDervFile,
                            madFiltList,
                            fp);
                            

    printf("Finished createMaddata\n");
    
    printf("Number of cycles = %i\n", maddata->numCycles);

    if (maddata->numCycles)
        printf("Num recs in cyc 0 is %i\n", maddata->madCycleList[0]->numMadrecords);
    
    simpleMaddataPrint(maddata, stdout);
    
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
    destroyMadparmList(parmListDervFile);
    destroyMadfilterList(madFiltList);
    fclose(fp);
    

    return(0);
}

