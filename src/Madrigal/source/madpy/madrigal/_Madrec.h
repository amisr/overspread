#ifndef _MADREC_PY_H
#define _MADREC_PY_H

#include <Python.h>
#include <cedarIO.h>
#include <cedar.h>
#include <date.h>
#include <madrec.h>
#include <maddata.h>

#define ERR_STR_LEN 200

#define BINARY_DATA_KREC 1002
#define CHAR_DATA_KREC   1101
#define BINARY_CAT_KREC  2001
#define CHAR_CAT_KREC  2101
#define BINARY_HEAD_KREC  3002
#define CHAR_HEAD_KREC  3101

#define MAX_NUM_PARMS 500
#define PULSE_LEN_CODE 402

// function declarations

void init_Madrec(void);

static void addIntListToPythonList(int * intArray, PyObject * pyList);
static void setPyTime(PyObject * retDateTime, double sec);
static double getIntFromPy(PyObject * pyNum);
static double getDoubleFromPy(PyObject * pyNum);
static char * getMnemFromPy(PyObject * pyArg);
static int str_caseins_cmp(char * str1, char * str2);

#endif
