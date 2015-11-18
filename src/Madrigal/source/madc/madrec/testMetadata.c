

/* 
* USAGE: testMetadata
*    This program is a simple example illustrating the use of the cedar
*    methods to access metadata in parcods.tab, instTab.txt, and madCatTab.txt 
*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <madrec.h>
#include <cedar.h>

int
main (argc, argv)
     int     argc;
     char    *argv[];
{
    int i=0, status=0;
    int numCodes = 6;
    int parmList[] = {110, -110, 191, -191, 111, 5501};
    char mnemList[][MNEM_LEN] = {"  gdAlt ",
                         "DgdaLT    ",
                         "   MpaR",
                         " mhdqc2",
                         "byear",
                         "nonsuch   "};
    char tmpMnem[MNEM_LEN] = "";
    int tmpCode = -1;
    int numFilesFound;
    char * fileList;
    double * fileStarttime;
    double * fileEndtime;
    char * pStr = NULL;  /* this pointer must be freed after use to avoid memory leaks */

    /*  The following methods allocate memory:
    *         cedarGetParDescription
    *         madGetParDescription
    *         cedarGetParMnemonic
    *         cedarGetParFormat
    *         madGetParFormat
    *
    */
    if (cedarReadParCodes() != 0)
    {
        printf(cedarGetError());
        exit(-1);
    } 

    for (i=0; i< numCodes; i++)
    {
        printf("This code is %i\n", parmList[i]);
        pStr = cedarGetParMnemonic(parmList[i]);
        strcpy(tmpMnem, pStr);
        free(pStr);
        printf("Its mnenomic is '%s'\n", tmpMnem);
        pStr = cedarGetParCodeType (parmList[i]);
        printf("Type via cedar: %s\n", pStr);
        free(pStr);
        pStr = madGetParMnemType (tmpMnem);
        printf("Type via mad: %s\n", pStr);
        free(pStr);
        pStr = cedarGetParDescription (parmList[i]);
        printf("Desc via cedar: %s\n", pStr);
        free(pStr);
        pStr = madGetParDescription (tmpMnem);
        printf("Desc via mad: %s\n", pStr);
        free(pStr);
        pStr = cedarGetParInt16Description (parmList[i]);
        printf("Int16Desc via cedar: %s\n", pStr);
        free(pStr);
        pStr = madGetParInt16Description (tmpMnem);
        printf("Int16Desc via mad: %s\n", pStr);
        free(pStr);
        printf("Scale factor via cedar: %f\n", cedarGetParScaleFactor (parmList[i]));
        printf("Scale factor via mad: %f\n", madGetParScaleFactor (tmpMnem));
        pStr = cedarGetParUnits (parmList[i]);
        printf("Units via cedar: %s\n", pStr);
        free(pStr);
        pStr = madGetParUnits (tmpMnem);
        printf("Units via mad: %s\n", pStr);
        free(pStr);
        pStr = cedarGetParMnemonic (parmList[i]);
        printf("Mnemonic via cedar: %s\n", pStr);
        free(pStr);
        printf("Code via cedar mnem: %i\n", cedarGetParCodeFromMnemonic(tmpMnem));
        pStr = cedarGetParFormat(parmList[i]);
        if (pStr != NULL)
            printf("Format via cedar: %s\n", pStr);
        else
            printf("Format via cedar: \n");
        pStr = madGetParFormat (tmpMnem);
        if (pStr != NULL)
            printf("Format via mad: %s\n", pStr);
        else
            printf("Format via mad: \n");
        printf("Width via cedar: %i\n", cedarGetParWidth(parmList[i]));
        printf("Width via mad: %i\n", madGetParWidth (tmpMnem));
        printf("\n");
    }

    printf("\n\nNow test Madrigal methods\n\n");
    for (i=0; i< numCodes; i++)
    {
        tmpCode = cedarGetParCodeFromMnemonic(mnemList[i]);
        printf("This code is %i = %s\n", tmpCode, mnemList[i]);
        pStr = cedarGetParCodeType (tmpCode);
        printf("Type via cedar: %s\n", pStr);
        free(pStr);
        pStr = madGetParMnemType (mnemList[i]);
        printf("Type via mad: %s\n", pStr);
        free(pStr);
        pStr = cedarGetParDescription (tmpCode);
        printf("Desc via cedar: %s\n", pStr);
        free(pStr);
        pStr = madGetParDescription (mnemList[i]);
        printf("Desc via mad: %s\n", pStr);
        free(pStr);
        pStr = cedarGetParInt16Description (tmpCode);
        printf("Int16Desc via cedar: %s\n", pStr);
        free(pStr);
        pStr = madGetParInt16Description (mnemList[i]);
        printf("Int16Desc via mad: %s\n", pStr);
        free(pStr);
        printf("Scale factor via cedar: %f\n", cedarGetParScaleFactor (tmpCode));
        printf("Scale factor via mad: %f\n", madGetParScaleFactor (mnemList[i]));
        pStr = cedarGetParUnits (tmpCode);
        printf("Units via cedar: %s\n", pStr);
        free(pStr);
        pStr = madGetParUnits (mnemList[i]);
        printf("Units via mad: %s\n", pStr);
        free(pStr);
        pStr = cedarGetParMnemonic (tmpCode);
        printf("Mnemonic via cedar: %s\n", pStr);
        free(pStr);
        printf("Code via cedar mnem: %i\n", cedarGetParCodeFromMnemonic(mnemList[i]));
        pStr = cedarGetParFormat(tmpCode);
        if (pStr != NULL)
            printf("Format via cedar: %s\n", pStr);
        else
            printf("Format via cedar: \n");
        pStr = madGetParFormat (mnemList[i]);
        if (pStr != NULL)
            printf("Format via mad: %s\n", pStr);
        else
            printf("Format via mad: \n");
        printf("Width via cedar: %i\n", cedarGetParWidth(tmpCode));
        printf("Width via mad: %i\n", madGetParWidth (mnemList[i]));
        printf("\n");
    }

    /* test of searchFilesByDate */
    printf("About to call searchFilesByDate\n");
    status = searchFilesByDate(1420000000.0,
                      1520000000.0,
                      &numFilesFound,
                      &fileList,
                      &fileStarttime,
                      &fileEndtime);

    if (status == 0 && numFilesFound > 0)
    {
        printf("fileList is %s and file num is %i\n", fileList, numFilesFound);
        for (i=0; i<numFilesFound; i++)
            printf("Start = %f, end = %f\n", fileStarttime[i], fileEndtime[i]);
        free(fileList);
        free(fileStarttime);
        free(fileEndtime);
    }
    else
        printf("Number of files found = %i\n", numFilesFound);

    return(0);
}
