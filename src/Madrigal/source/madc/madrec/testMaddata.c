#include <time.h>

#include <geometry.h>
#include <maddata.h>
#include <madDeriveEngine.h>

int main (int argc, char *argv[])
{
    MadfilterList * madFiltList;
    MadparmList * parmListUse;
    MadparmList * parmListMeas1DFile = NULL;
    MadparmList * parmListMeas2DFile = NULL;
    MadparmList * parmListDerv1DFile = NULL;
    MadparmList * parmListDerv2DFile = NULL;
    MadparmList * parmListDerv = NULL;
    MadparmList * parmListUseCopy;
    MadparmList * parmListMeas1D;
    MadparmList * parmListMeas2D;
    InfoDerived * infoDerived;
    InfoDervFile * infoDervFile = NULL;
    int i,j;   
    char madfile[128] = "";
    Madrec * madrecp = NULL;
    time_t starttime;
    time_t endtime;
    int status = 0;
    Maddata * maddata = NULL;
    FILE * fp = NULL;
    double f1_lower = -100.0;
    double f1_upper = 180.0;
    double f2_lower = 1500.0;
    double f2_upper = 1505.0;
    
    char * headerStr = NULL;
    char * mnemStr = NULL;
    char * labelStr = NULL;
    char * dataStr = NULL;
    
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
    printf("Number of filters = %i\n", madFiltList->numFilters);
    printf("Parm 2 of second filt = %s\n", madFiltList->madfilt_list[1].madParm2);


    /* create parmListUse */
    parmListUse = createMadparmList();
    appendMadparm(parmListUse, "byear");
    appendMadparm(parmListUse, "azm");
    appendMadparm(parmListUse, "bmd");
    appendMadparm(parmListUse, "kindat");
    appendMadparm(parmListUse, "32750");
    appendMadparm(parmListUse, "range");
    appendMadparm(parmListUse, "gdalt");
    appendMadparm(parmListUse, "bmag");
    appendMadparm(parmListUse, "no2l");
    appendMadparm(parmListUse, "uts");
    appendMadparm(parmListUse, "SUNRISE_HOUR");
    appendMadparm(parmListUse, "slt");
    appendMadparm(parmListUse, "f10.7");

    /* copy parmListUse to parmListUseCopy */
    parmListUseCopy = copyMadparmList(parmListUse);

    /* print out parameters in parmListUseCopy */
    for (i=0; i< parmListUseCopy->numParm; i++)
        printf("Next parameter in use = %s\n", parmListUseCopy->mnemList[i]);

    /* does list have "  uTh " ? */
    printf("List has \"  uTH \" = %i\n", hasParm(parmListUse, "  uTh "));
    printf("List has \"  kindat \" = %i\n", hasParm(parmListUse, "  kindat "));
    printf("Min value of uth = %e\n", getMinParm(parmListUse, "  uTh "));
    printf("Max value of uth = %e\n", getMaxParm(parmListUse, "  uTh "));


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

    if (parmListMeas1DFile == NULL)
    {
        printf("Error from analyzeFileParms\n");
        return -1;
    }
    for (i=0; i<parmListMeas1DFile->numParm; i++)
        printf("This meas 1D parm is %s\n", parmListMeas1DFile->mnemList[i]);
    for (i=0; i<parmListMeas2DFile->numParm; i++)
        printf("This meas 2D parm is %s\n", parmListMeas2DFile->mnemList[i]);
    for (i=0; i<parmListDerv1DFile->numParm; i++)
        printf("This derv 1D parm is %s\n", parmListDerv1DFile->mnemList[i]);
    for (i=0; i<parmListDerv2DFile->numParm; i++)
        printf("This derv 2D parm is %s\n", parmListDerv2DFile->mnemList[i]);


    /* all done with parmList*File */
    destroyMadparmList(parmListMeas1DFile);
    destroyMadparmList(parmListMeas2DFile);
    destroyMadparmList(parmListDerv1DFile);
    destroyMadparmList(parmListDerv2DFile);

    /* create parmListMeas1D */
    parmListMeas1D = createMadparmList();
    appendMadparm(parmListMeas1D, "FIRST_IBYR");
    appendMadparm(parmListMeas1D, "FIRST_IBDT");
    appendMadparm(parmListMeas1D, "FIRST_IBHM");
    appendMadparm(parmListMeas1D, "FIRST_IBCS");
    appendMadparm(parmListMeas1D, "KINST");
    appendMadparm(parmListMeas1D, "KINDAT");
    appendMadparm(parmListMeas1D, "IBYR");
    appendMadparm(parmListMeas1D, "IBDT");
    appendMadparm(parmListMeas1D, "IBHM");
    appendMadparm(parmListMeas1D, "IBCS");
    appendMadparm(parmListMeas1D, "IEYR");
    appendMadparm(parmListMeas1D, "IEDT");
    appendMadparm(parmListMeas1D, "IEHM");
    appendMadparm(parmListMeas1D, "IECS");
    appendMadparm(parmListMeas1D, "NROW");
    appendMadparm(parmListMeas1D, "az1");
    appendMadparm(parmListMeas1D, "az2");
    appendMadparm(parmListMeas1D, "el1");
    appendMadparm(parmListMeas1D, "el2");
    appendMadparm(parmListMeas1D, "elm");
    appendMadparm(parmListMeas1D, "year");
    appendMadparm(parmListMeas1D, "bmd");

    /* create parmListMeas2D */
    parmListMeas2D = createMadparmList();
    appendMadparm(parmListMeas2D, "ROW");
    appendMadparm(parmListMeas2D, "range");
    appendMadparm(parmListMeas2D, "ti");


    infoDerived = createInfoDerived(parmListMeas1D, parmListMeas2D, parmListUse, madFiltList);

    printf("Printing list of  %i 1D parameters used...\n", infoDerived->allUsed1DParmList->numParm);
    for (i=0; i<infoDerived->allUsed1DParmList->numParm; i++)
    {
        printf("This parm is %s\n", infoDerived->allUsed1DParmList->mnemList[i]);
    }
    
    printf("Printing list of  %i 1D parameters avail...\n", infoDerived->allAvail1DParmList->numParm);
    for (i=0; i<infoDerived->allAvail1DParmList->numParm; i++)
    {
        printf("This parm is %s\n", infoDerived->allAvail1DParmList->mnemList[i]);
    }

    printf("Printing list of %i 2D parameters used...\n", infoDerived->allUsed2DParmList->numParm);
    for (i=0; i<infoDerived->allUsed2DParmList->numParm; i++)
    {
        printf("This 2d parm is %s\n", infoDerived->allUsed2DParmList->mnemList[i]);
    }
    
    printf("Printing list of  %i 2D parameters avail...\n", infoDerived->allAvail2DParmList->numParm);
    for (i=0; i<infoDerived->allAvail2DParmList->numParm; i++)
    {
        printf("This 2d parm is %s\n", infoDerived->allAvail2DParmList->mnemList[i]);
    }

    printf("Printing list of %i unavailable parameters...\n", infoDerived->unavailParmList->numParm);
    for (i=0; i<infoDerived->unavailParmList->numParm; i++)
    {
        printf("This unavailable parm is %s\n", infoDerived->unavailParmList->mnemList[i]);
    }

    /* not legal, unless I make gCompExtList non-static 
    printf("Printing inputMap and outputMap for each needed method.\n");

    for (i=0; i < infoDerived->numDervMeth; i++)
    {
        printf("Method %i:\n", i);
        if (infoDerived->infoMethodArr[i]->isNeeded)
        {
            printf("This methis is 1D = %i\n", infoDerived->infoMethodArr[i]->all1D);
            for (j=0; j <gCompExtList[i].inputCount; j++)
            {
                printf("\tInput %s comes from %iD number %i\n", gCompExtList[i].inputMnemList[j], 
                        infoDerived->infoMethodArr[i]->inputMap[j*2],
                        infoDerived->infoMethodArr[i]->inputMap[j*2 + 1]);
            }
            for (j=0; j <gCompExtList[i].outputCount; j++)
            {
                printf("\tOutput %s goes to number %i\n", gCompExtList[i].outputMnemList[j], 
                        infoDerived->infoMethodArr[i]->outputMap[j]);
            }
        }
        else
           printf("Not needed\n\n"); 

    }*/

    destroyInfoDerived(infoDerived);

    /* test of InfoDervFile */

    /* start time test */
    starttime = time(NULL);

    /* Create a madrec object */
    madrecp = madrecCreate();
    printf("create: %s\n", madrecGetError(madrecp));

    /* Connect the madrec object to a madrigal file and load in memory */
    madrecOpen(madrecp, 50, madfile);
    printf("open: %s\n", madrecGetError(madrecp));


    infoDervFile = createInfoDervFile(madrecp, parmListUse, madFiltList, NULL);

    /* print out infoDervFile */
    printf("In file, num types found = %i\n", infoDervFile->numTypesRec);

    for (i=0; i<infoDervFile->numTypesRec; i++)
    {
        printf("This type is %i\n", i);
        printf("It has %i records of this type\n\n", infoDervFile->numEachTypeRec[i]);
        infoDerived = infoDervFile->infoDervList[i];

        printf("Printing list of  %i 1D parameters used...\n", infoDerived->allUsed1DParmList->numParm);
        for (j=0; j<infoDerived->allUsed1DParmList->numParm; j++)
        {
            printf("This 1D parm is %s\n", infoDerived->allUsed1DParmList->mnemList[j]);
        }
        
        printf("Printing list of  %i meas 1D parameters used and codes...\n", infoDerived->meas1DParmList->numParm - NUM_PROLOG_PARM);
        for (j=NUM_PROLOG_PARM; j<infoDerived->meas1DParmList->numParm; j++)
        {
            printf("This meas 1D parm is %s, code %i\n", infoDerived->meas1DParmList->mnemList[j],
                                                      infoDerived->meas1DCodeList[j - NUM_PROLOG_PARM]);
        }
        
        printf("Printing list of  %i 2D parameters used...\n", infoDerived->allUsed2DParmList->numParm);
        for (j=0; j<infoDerived->allUsed2DParmList->numParm; j++)
        {
            printf("This 2D parm is %s\n", infoDerived->allUsed2DParmList->mnemList[j]);
        }

        printf("Printing list of %i 2D parameters used...\n", infoDerived->allUsed2DParmList->numParm - NUM_2D_PROLOG_PARM);
        for (j=NUM_2D_PROLOG_PARM; j<infoDerived->allUsed2DParmList->numParm; j++)
        {
            printf("This 2d parm is %s\n", infoDerived->allUsed2DParmList->mnemList[j]);
        }

        printf("Printing list of %i unavailable parameters...\n", infoDerived->unavailParmList->numParm);
        for (j=0; j<infoDerived->unavailParmList->numParm; j++)
        {
            printf("This unavailable parm is %s\n", infoDerived->unavailParmList->mnemList[j]);
        }
        
        printf("Printing list of %i 1D parameters (includes unavailable ones) and map\n", infoDerived->req1DParmList->numParm);
        for (j=0; j<infoDerived->req1DParmList->numParm; j++)
        {
            printf("\tThis 1D parm is %s, and is mapped to %i\n", infoDerived->req1DParmList->mnemList[j],
                   infoDerived->mapReq1DParmList[j]);
        }
        
        printf("Printing list of %i 2D parameters and map\n", infoDerived->req2DParmList->numParm);
        for (j=0; j<infoDerived->req2DParmList->numParm; j++)
        {
            printf("\tThis 2D parm is %s, and is mapped to %i\n", infoDerived->req2DParmList->mnemList[j],
                   infoDerived->mapReq2DParmList[j]);
        }

        printf("Printing whether filters are valid: %i\n", infoDervFile->infoDervList[i]->validFilters);
        for (j=0; j<infoDervFile->infoDervList[i]->filt1DList->numFilters; j++)
        {
            printf("1D filter with %s and %s\n", infoDervFile->infoDervList[i]->filt1DList->madfilt_list[j].madParm1,
                                                 infoDervFile->infoDervList[i]->filt1DList->madfilt_list[j].madParm2);
            printf("Does this 1D filter use only measured data? %i\n", infoDervFile->infoDervList[i]->onlyMeas1DList[j]);
            printf("Map parm 1 = %i\n", infoDervFile->infoDervList[i]->mapFilt1DParm[2*j]);
            printf("Map parm 2 = %i\n", infoDervFile->infoDervList[i]->mapFilt1DParm[2*j + 1]);
        }
        for (j=0; j<infoDervFile->infoDervList[i]->filt2DList->numFilters; j++)
        {
            printf("2D filter with %s and %s\n", infoDervFile->infoDervList[i]->filt2DList->madfilt_list[j].madParm1,
                                                 infoDervFile->infoDervList[i]->filt2DList->madfilt_list[j].madParm2);
            printf("Does this 2D filter use only measured data? %i\n", infoDervFile->infoDervList[i]->onlyMeas2DList[j]);
            printf("Dim parm 1 = %i\n", infoDervFile->infoDervList[i]->mapFilt2DParm[4*j + 0]);
            printf("Map parm 1 = %i\n", infoDervFile->infoDervList[i]->mapFilt2DParm[4*j + 1]);
            printf("Dim parm 2 = %i\n", infoDervFile->infoDervList[i]->mapFilt2DParm[4*j + 2]);
            printf("Map parm 2 = %i\n", infoDervFile->infoDervList[i]->mapFilt2DParm[4*j + 3]);
        }
        

    }

    /* test cycle number */
    printf("Cycle number of record 1130 is %i\n", infoDervFile->cycleNum[1130]);
    
    /* all done with infoDervFile */
    destroyInfoDervFile(infoDervFile);

    /* Disconnect the madrigal file */
    madrecClose(madrecp);
    printf("close: %s\n", madrecGetError(madrecp));

    /* Destroy the madrec object */
    madrecDestroy(madrecp);


    /* end time test */
    endtime = time(NULL);

    /* Cannot print test time in a regression test - might vary */
    /*printf("Total test time = %i\n", (int)endtime - (int)starttime);*/

    /* test of maddata */
    maddata = createMaddata(madfile,
                            "Hi",
                            parmListUse,
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
                   
    parmListDerv = getDerivedParms(parmListUseCopy);
    printf("Printing list derivable parameters ...\n");
    for (j=0; j<parmListDerv->numParm; j++)
    {
        printf("This derivable parm is %s\n", parmListDerv->mnemList[j]);
    }
    
    printf("Printing getClassicMadrecordStrings:\n");
    getClassicMadrecordStrings(maddata, 
                               0, 
                               0, 
                               32000,
                               "missing",
                               "assumed",
                               "knownBad",
                               &headerStr,
                               &mnemStr,
                               &labelStr,
                               &dataStr);
     
    printf("headerStr is %s\n", headerStr);
    printf("mnemStr is %s\n", mnemStr); 
    printf("labelStr is %s\n", labelStr); 
    printf("dataStr is %s\n", dataStr); 
    
    free(headerStr);
    free(mnemStr);
    free(labelStr);
    free(dataStr);

                            
    /* clean up */
    destroyMaddata(maddata);
    destroyMadparmList(parmListUseCopy);
    destroyMadparmList(parmListMeas1D);
    destroyMadparmList(parmListMeas2D);
    destroyMadparmList(parmListUse);
    destroyMadparmList(parmListDerv);
    destroyMadfilterList(madFiltList);
    fclose(fp);
    

    return(0);
}

