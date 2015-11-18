#include <maddata.h>

/*  This example program shows a simple example of the use of the Maddata library. */
/*  It shows how to properly dispose of created objects when finished.             */
/*  Add or modify parameters in parmListUse to see any other Madrigal parameters.  */

int main (int argc, char *argv[])
{
    MadfilterList * madFiltList;
    MadparmList * parmListUse;
    char madfile[128] = "";
    Maddata * maddata = NULL;
    
    /* ranges for filters */
    double az_lower = 178.9;
    double az_upper = 180.0;
    double gdalt_lower[3] = { 50.0, 150.0, 250.0};
    double gdalt_upper[3] = {100.0, 200.0, 300.0};
    
    cedarGetMadroot(madfile);
    strcat(madfile, "/experiments/1998/mlh/20jan98/mlh980120g.001");


    /* create madFiltList */
    madFiltList = createMadfilterList();
    
    /* add a az1 filter with a single allowed range of 178.9 to 180.0 */
    /* filters can use single parameters, or any two parameters       */
    /* multiplied, divided, added, or subtracted                      */
    /* Filters can use one or more ranges.  If value in any range,    */
    /* filter accepts value.                                          */
    appendMadfilter(madFiltList,
                    SINGLE_FILT,
                    1,
                    &az_lower,
                    &az_upper,
                    "az1",
                    "");
                    
     /* now add a second gdlat filter that accepts any of the three   */
     /* ranges 50-100, 150-200, or 250-300 km                         */
     appendMadfilter(madFiltList,
                    SINGLE_FILT,
                    3,
                    gdalt_lower,
                    gdalt_upper,
                    "gdalt",
                    "");


    /* create parmListUse */
    parmListUse = createMadparmList();
    appendMadparm(parmListUse, "azm");
    appendMadparm(parmListUse, "elm");
    appendMadparm(parmListUse, "gdlat");
    appendMadparm(parmListUse, "glon");
    appendMadparm(parmListUse, "gdalt");
    appendMadparm(parmListUse, "range");
    appendMadparm(parmListUse, "sunrise_hour");
    appendMadparm(parmListUse, "bmag");



    /* create the data you want */
    maddata = createMaddata(madfile,
                            "",
                            parmListUse,
                            madFiltList,
                            NULL);

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
    destroyMadparmList(parmListUse);
    destroyMadfilterList(madFiltList);

    return(0);
}

