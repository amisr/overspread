/* date definitions */
/*  header file added by B. Rideout 11/7/2001 */

#ifndef _DATEH_
#define _DATEH_

#include <time.h>

/* Function declarations */
int jday(int day, int month, int year);
int jdater (int jdayno, int *day, int *month, int *year);
int idmyck(int day, int month, int year);
double dmadptr(int iyr, int imd, int ihm, int ics);
double getKey(int year, int month, int day,
           int hour, int minute, int second);
int dinvmadptr(double dmadptr, int * iyr, int * imd, int * ihm, int * ics);
int madGetDayno(int year, int month, int day);

#endif
