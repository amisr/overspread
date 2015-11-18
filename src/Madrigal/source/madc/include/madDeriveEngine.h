/******************************************************
*  MadDeriveEngine is the private module used to derive
*  all possible requested methods given the existence
*  of measured parameters and methods to derive parameters
*  as defined in madDeriveMethods.  This module is not
*  meant to be used by any end user - all public methods are
*  exposed via the maddata  module.
*
*  The madDeriveEngine contains no code whatsoever that
*  pertains to the meaning of any particular parameter - that
*  code is entirely in  madDeriveMethods. The purpose of
*  this code is to analyze at runtime the available measured
*  parameters and derivation methods to plan which methods need
*  to be called, given which parameters were requested.  For
*  that reason, this module should never need to be modified
*  to extend or modify the derivations methods, except to 
*  register new methods in the global gCompExtList.  The module
*  madDeriveMethods both documents how all derivation
*  methods work, and how to add more.  This module populates
*  the maddata data structures by calling the methods in
*  madDeriveMethods.  It is responsible for copying data into
*  input arrays for those methods, and copy out the data it needs.
*        
*  The madDeriveEngine was built to be both flexible and fast.
*  It tries to minimize the number of calls required to 
*  madDeriveMethods by testing filters as soon as possible in order
*  to drop records or 2D rows as soon as possible.  This basic
*  algorithm is as follows:
*    - if the analysis says a filter depends on an underivable 
*      parameter, the record is rejected immediately.
*    - 1D measured data is read in 
*    - Any filter that depends on only 1D measured data is applied
*    - all 1D data is derived
*    - Any filter that depends on only 1D data is applied
*        2D loop:
*    - 2D measured data is read in 
*    - Any filter that depends on only 2D measured data is applied
*    - all 2D data is derived
*    - Remaining filters are applied
*  
*  There are three basic data structures in this module: CompiledExt, 
*  InfoDerived, and InfoDerivedFile.  CompiledExt defines everything the
*  madDeriveEngine needs to know about a particular derivation method.
*  InfoDerived contains everything needed to handle one particular record
*  type, where a record type is defined by an ordered list of 1D and 2D
*  measured Madrigal parameters.  InfoDerivedFile contains a list of 
*  InfoDerived structs, one for each record type found in a file. Note that
*  only the InfoDerivedFile struct deals in any way with Cedar files; the
*  InfoDerived struct and its methods are storage neutral, and could be used
*  for data stored in another format such as a relational database.
*
*  These data structures contain a number of members that begin map* and
*  are int arrays.  These maps are designed for rapid copying of arrays
*  of doubles into and out of methods, and into new maddata objects when
*  they need to be created.
*
*  While this module is written in C and not C++; its methods
*  and design are as close as I could get to object-oriented.
*  Every data structure should be instantiated via a create*
*  method and released via destroy*.  All other methods take
*  the respective data structure pointer as the first argument.
*/

#ifndef _MAD_DERIVE_ENGINE_
#define _MAD_DERIVE_ENGINE_

#include <stdlib.h>
#include <stdio.h>
#include <cedarIO.h>
#include <madrec.h>
#include <cedar.h>
#include <maddata.h>

#define MAX_NEEDED_METH 500   /* upper limit to number of methods a method can depend on     */
#define NUM_PROLOG_PARM 18    /* The number of 1D parameters that are loaded via the prolog  */
#define NUM_2D_PROLOG_PARM 1  /* The number of 2D parameters that are loaded via the prolog  */
#define CYC_CODE        95    /* The only parameter read by the engine directly, since cycle */
                              /* is a fundamental part of the maddata structure              */
#define MAX_2D_ROWS     100000   /* maximum number of 2D rows a multi-row method can handle  */
                              



typedef struct compiledExt {

    /* this structure holds all data for a single compiled extension */
    
    int (*compExt) (int inCount, 
                    double * inputArr, 
                    int outCount, 
                    double * outputArr, 
                    FILE * errFile);                  

    int            inputCount;         /* number of input mnemonics                            */
    const char **  inputMnemList;      /* pointer to char array of MNEM names, len=inputCount  */
    int            outputCount;        /* number of output mnemonics                           */
    const char **  outputMnemList;     /* pointer to char array of MNEM names, len=outputCount */

} CompiledExt;


typedef struct compiledMultiRowExt {

    /* this structure holds all data for a single compiled extension that deals with multiple rows at once */
    
    int (*compExt) (int numRows,
                    int inCount,
                    double ** inputArr, 
                    int outCount, 
                    double ** outputArr, 
                    FILE * errFile);                  

    int            inputCount;        /* number of input mnemonics                                    */
    const char **  inputMnemList;     /* pointer to char array of MNEM names, len=inputCount          */
    const int  *   inputMnemType;     /* pointer to int array of MNEM types (1 or 2), len=inputCount  */
    int            outputCount;       /* number of output mnemonics                                   */
    const char **  outputMnemList;    /* pointer to char array of MNEM names, len=outputCount         */
    const int  *   outputMnemType;    /* pointer to int array of MNEM types (1 or 2), len=outputCount */

} CompiledMultiRowExt;



typedef struct _infoMethod {

   /* this structure is used to hold information about how to use each derived    */
   /* parameter method for a given record type.  Each InfoDerived struct contains */
   /* a list of these, with len = total number of derived methods                 */
  
  int numInputMethods;                 /* how many methods does this method depend on ?  */
  int neededMethods[MAX_NEEDED_METH];  /* which methods? (refers to gCompExtList)        */
  int all1D;                           /* are all inputs 1D?                             */
  int isAvailable;                     /* can all inputs be calculated?                  */
  int isNeeded;                        /* does method need to be called?                 */
  int * inputMap;                      /* where in InfoDerived.allUsed?DParmList does    */
                                       /* this input come from?  Length of this array is */
                                       /* 2* number of inputs for method.  First int in  */
                                       /* pair is either 1 or 2 depending on whether     */
                                       /* input is 1D or 2D, second is index into that   */
                                       /* array.                                         */
  int * outputMap;                     /* index into InfoDerived.allUsed?DParmList for   */
                                       /* output data.  If all1D, refers to 1DParmList,  */
                                       /* else refers to 2DParmList.  Length of this     */
                                       /* array is number of outputs for method. A       */
                                       /* value of -1 is used to indicate the output     */
                                       /* should be ignored because previously           */
                                       /* determined.                                    */

} InfoMethod;


typedef struct _infoMultiRowMethod {

   /* this structure is used to hold information about how to use each multi-row    */
   /* method for a given record type.  Each InfoDerived struct contains             */
   /* a list of these, with len = total number of derived multi-row methods         */
  
  int isNeeded;                        /* does method need to be called?                   */
  int numInputs;                       /* number of inputs for this method                 */
  int numOutputs;                      /* number of outputs for this method                */
  int * inputMap;                      /* where in InfoDerived.allUsed?DParmList does      */
                                       /* this input come from?  If  compiledMultiRowExt   */
                                       /* ->inputMnemType = 1, refers to 1DParmList,       */
                                       /* else refers to 2DParmList.  Length of this       */
                                       /* array is number of inputs for method.            */
  double ** inputArr;                  /* temp storage for 1 and 2D inputs,                */
                                       /* len = numInputs. If 2D, array len = MAX_2D_ROWS  */
  double ** outputArr;                 /* temp storage for 1 and 2D outputs,               */
                                       /* len = numOutputs. If 2D, array len = MAX_2D_ROWS */
} InfoMultiRowMethod;


typedef struct _infoDerived {

   /* this structure is used to hold all information used when */
   /* deriving parameters for a given record type, where a     */
   /* record type is defined by an ordered list of 1D and 2D   */
   /* measured parameters                                      */

   int           numDervMeth;                  /* total number of derived methods  */
   InfoMethod ** infoMethodArr;                /* array of InfoMethod about each derived method */
                                               /* length = numDervMeth */
   int           numMultiRowMeth;              /* total number of multi-row derived methods  */
   InfoMultiRowMethod ** infoMultiRowMethArr;  /* array of InfoMultiRowMethod about each multi-row derived method */
                                               /* length = numMultiRowMeth */
   int           anyMultRowNeeded;             /* int indicates whether any multi-row method needed */
   MadparmList * meas1DParmList;               /* list of 1D parameters measured */
   int         * meas1DCodeList;               /* list of 1D parameter codes - used for rapid access */
                                               /* Len = len of meas1DParmList - NUM_PROLOG_PARM */
   MadparmList * meas2DParmList;               /* list of 2D parameters measured */
   int         * meas2DCodeList;               /* list of 2D parameter codes - used for rapid access */
                                               /* Len = len of meas2DParmList */
   MadparmList * requestedParmList;            /* list of parameters requested */
   MadparmList * req1DParmList;                /* list of requested parameters found to be 1D -  includes unavailable parms */
   int         * mapReq1DParmList;             /* int array that specifies index into allUsed1DParmList of each 1D parm */
                                               /* If parameter unavailable, = -1                                        */
   MadparmList * req2DParmList;                /* list of requested parameters found to be 2D */
   int         * mapReq2DParmList;             /* int array that specifies index into allUsed2DParmList of each 2D parm */
   MadparmList * unavailParmList;              /* list of parameters found to not be available */
   MadparmList * allUsed1DParmList;            /* list of all 1D parameters used in derivation */
   MadparmList * allAvail1DParmList;           /* list of all 1D parameters that can be derived */
   MadparmList * allUsed2DParmList;            /* list of all 2D parameters used in derivation */
   MadparmList * allAvail2DParmList;           /* list of all 2D parameters that can be derived */
   double * all1DParm;                         /* array of doubles of len = len(allUsed1DParmList) */
   double * all2DParm;                         /* array of doubles of len = len(allUsed2DParmList) */
   double * tmpInputParm;                      /* array of doubles of len = len(allUsed1DParmList)  +  */
                                               /* len(allUsed2DParmList) - used to pass input parms to */
                                               /* methods                                              */
   double * tmpOutputParm;                     /* array of doubles of len = len(allUsed1DParmList)  +  */
                                               /* len(allUsed2DParmList) - used to get output parms    */
                                               /* from methods                                         */
   int validFilters;                           /* if 1, all filters can potentially be applied, if 0,  */
                                               /* some filter uses unavailParmList and always rejects  */
   MadfilterList * filt1DList;                 /* the list of zero or more 1D filters to apply     */
   int * onlyMeas1DList;                       /* a list of int of len=filt1DList len. If 1, 1D    */
                                               /* filter uses only measured parameters             */
   int * mapFilt1DParm;                        /* an int array used to map filter parameters from filt1DList to   */
                                               /* allUsed1DParmList.  Len = 2*filt1DList len, because each filter */
                                               /* has two parameters.  If filter has only one parameter, second   */
                                               /* value will be -1                                                */
   MadfilterList * filt2DList;                 /* the list of zero or more 2D filters to apply     */
   int * onlyMeas2DList;                       /* a list of int of len=filt2DList len. If 1, 2D    */
                                               /* filter uses only 2D measured parameters (and     */
                                               /* possibly a 1D parameter)                         */
   int * mapFilt2DParm;                        /* an int array used to map filter parameters from filt2DList to   */
                                               /* allUsed1DParmList and allUsed2DParmList. Len = 4*filt1DList     */ 
                                               /* len.  For each filter, there are four ints:                     */
                                               /*  1. 1 if first parm is 1D, 2 if 2D                              */
                                               /*  2. index into either allUsed1DParmList and allUsed2DParmList   */
                                               /*  3. 1 if 2nd parm is 1D, 2 if 2D (or -1 if single parameter)    */
                                               /*  4. index as in 2 for 2nd parameter (or -1 if single parameter) */
} InfoDerived;

typedef struct _infoDervFile {

   /* this structure is used to hold an array of InfoDerived   */
   /* for an entire file, and a list of records each           */
   /* InfoDerived is relevant to                               */

   int       numTypesRec;           /* number of different types of records in file  */
   int   *   numEachTypeRec;        /* an array of int of len=numTypesRec, each int  */
                                    /* gives the number of records of that type in   */
                                    /* file                                          */
   int   *   cycleNum;              /* an array of ints that give the cycle number   */
                                    /* of each data record. Len = # data records     */
   int  **   eachTypeList;          /* an array of int arrays, len=numTypesRec. Each */
                                    /* list's length is given by value in            */
                                    /* numEachTypeRec. Each list contains list of    */
                                    /* record numbers of that type in file           */
   InfoDerived ** infoDervList;     /* An array of InfoDerived structs, one for each */
                                    /* numTypesRec                                   */
} InfoDervFile;



/* Method declarations */

InfoMethod * createInfoMethod(int numInputs, int numOutputs);

void destroyInfoMethod(InfoMethod * infoMethod);

InfoMultiRowMethod * createInfoMultiRowMethod(int numInputs, 
                                              const int * inputType,
                                              int numOutputs,
                                              const int * outputType);

void destroyInfoMultiRowMethod(InfoMultiRowMethod * infoMultiRowMethod);

InfoDervFile * createInfoDervFile(Madrec * madrecp, 
                                  MadparmList * requestParm, 
                                  MadfilterList * filtList,
                                  FILE * errFile);

void destroyInfoDervFile(InfoDervFile * infoDervFile);

int getRecType(InfoDervFile * infoDervFile, int recno);

void load1DMeasData(InfoDervFile * infoDervFile, 
                    Madrec * madrecp, 
                    int recType,
                    double first_ibyr,
                    double first_ibdt,
                    double first_ibhm,
                    double first_ibcs);
                    
void load2DMeasData(InfoDervFile * infoDervFile, 
                    Madrec * madrecp, 
                    int recType,
                    int row);
                    
void load1DMultiRowData(InfoDervFile * infoDervFile,  
                        int recType);
                        
void load1DMultiRowDataNonfile(InfoDerived * infoDerived);
                        
void load2DMultiRowData(InfoDervFile * infoDervFile,  
                        int recType,
                        int count2D);
                        
void load2DMultiRowDataNonfile(InfoDerived * infoDerived,  
                               int count2D);
                    
int evaluateFilter(InfoDervFile * infoDervFile,
                   int recType,
                   int dim,
                   int filtIndex);

int appendMadrecord(Maddata * maddata,
                    InfoDerived * infoDerv,
                    int cycIndex,
                    int typeIndex,
                    Rec_type rectype,
                    char * text,
                    int kinst,
                    double starttime,
                    double endtime);
                    

int updateMadrecordWithMultiRow(Maddata * maddata,
                                InfoMultiRowMethod * infoMultiRowMeth,
                                int methIndex,
                                int cycIndex,
                                int recIndex,
                                int typeIndex,
                                int numRows);

int append2DRow(Maddata * maddata,
                InfoDerived * infoDerv,
                int cycIndex,
                int typeIndex,
                int recIndex);

InfoDerived * createInfoDerived(MadparmList * meas1DParmList, 
                                MadparmList * meas2DParmList,
                                MadparmList * requestedParmList,
                                MadfilterList * filtList);

void destroyInfoDerived(InfoDerived * info);

int hasOutput(const CompiledExt * exten, const char * mnem);

void checkIf1DMethodNeeded(InfoDerived * infoDeriv, 
                           int methIndex,
                           MadparmList * filtParmsNotRequestedList);

void checkIf2DMethodNeeded(InfoDerived * infoDeriv, 
                           int methIndex,
                           MadparmList * filtParmsNotRequestedList);

void make1DMethodNeeded(InfoDerived * infoDeriv, int methIndex);

void make2DMethodNeeded(InfoDerived * infoDeriv, int methIndex);

void checkIfMultiRowMethodNeeded(InfoDerived * infoDeriv, 
                           int methIndex);

int dispatchMethod(InfoDerived * infoDeriv, int methIndex, FILE * errFile);

int dispatchMultiRowMethod(InfoDerived * infoDeriv, 
                           int methIndex, 
                           int numRows,
                           FILE * errFile);

#endif

