/*  $Id: date.c,v 1.9 2009/01/22 19:40:24 brideout Exp $ */


#include "date.h"
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

/***********************************************************************
*  jday  - returns Julian day number given day, month, and year
*  
*    Julian day 0 is Nov. 24, -4713
*
*    Returns -1 if illegal year, month, day passed in
*/
int jday(int day, int month, int year)
{

    int jdayno, y, c, ya, m;

    if (idmyck(day, month, year) != 0) return(-1);

    m = month;
    if (m > 2) {
        m = month - 3;
        y = year;
    } else {
        m = month + 9;
        y = year - 1;
    }
    c = y/100;
    ya = y - 100*c;
    jdayno = (146097*c)/4+(1461*ya)/4 + (153*m + 2)/5 + day + 1721119;
    return(jdayno);
}


/***********************************************************************
*  jdater - sets day, month and year given jdayno
* 
*    Inverse of jday method
*/
    int jdater (int jdayno, int *day, int *month, int *year)
{

    int j, d, m, y;
 
    if (jdayno < 0) return(1);
    j = jdayno - 1721119;
    y = (4*j - 1)/146097;
    j  =  4*j - 1 - 146097*y;
    d = j/4;
    j = (4*d + 3)/1461;
    d = 4*d + 3 - 1461*j;
    d = (d + 4)/4;
    m = (5*d - 3)/153;
    d = 5*d - 3 - 153*m;
    d = (d + 5)/5;
    y = 100*y + j;
    if (m < 10) {
        m = m + 3;
    }
    else {
        m = m - 9;
        y = y + 1;
    }
    *day = d;
    *month = m;
    *year = y;
    return(0);
}


/***********************************************************************
*
*  idmyck - returns 0 if valid day, month, and year
*
*/
int idmyck(int day, int month, int year)
{

    int l;
    int numday[12] = {31,28,31,30,31,30,31,31,30,31,30,31};

    if (day < 1 || month < 1 || month > 12 || year < 0 || year > 9999) {
        return(1);
    }

    if (month == 2 && year/4*4 == year &&
        (year/400*400 == year || year/100*100 != year))
        l = 1;
    else {
        l = 0;
    }

    if (day > numday[month-1]+l)
        return(1);

    return(0);
}


/***********************************************************************
*
* dmadptr - returns number of seconds since 1/1/1950 for iyr, imd, ihm, ics
*           as double
*
*    Can retain fractions of a second
*
*    Inputs: iyr - year
*            imd - month/day as integer mmdd
*            ihm - hour/min as integer hhmm
*            ics - centiseconds since last minute
*
*/
double dmadptr(int iyr, int imd, int ihm, int ics)
{
    int n1jan50=2433283, year, month, day, hour, minute;
    double second=0.0;
    double im1=0.0;

    year = iyr;
    month = imd/100;
    day = imd - 100*month;
    hour = ihm/100;
    minute = ihm - 100*hour;
    second = ics/100.0;
    im1 = (jday(day, month, year) - n1jan50)*86400.0 +
          hour*3600.0 + minute*60.0 + second;

    return (im1);
}


/***********************************************************************
*
* getKey - returns number of seconds since 1/1/1950 for year,
*          month, day, hour, minute, second
*
*/
double getKey(int year, int month, int day,
           int hour, int minute, int second)
{
    return(dmadptr(year, 100*month+day, 100*hour+minute, 100*second));
}


/***********************************************************************
*
* dinvmadptr - sets iyr, imd, ihm, ics given dmadptr (number of seconds 
*              as a double since 1/1/1950)
*
*    inverse of dmadptr.  
*
*    Returns 0 if success, 1 if failure (out of range time)
*    Uses time.h, and constant to shift from 1/1/1970 to 1/1/1950
*/
int dinvmadptr(double dmadptr, int * iyr, int * imd, int * ihm, int * ics)
{
    struct tm * now = NULL;
    
    time_t time_mad = (long)(dmadptr - 631152000.0);  /* number of seconds from 1/1/1950 to 1/1/1970 */
    
    now = gmtime(&time_mad);
    
    *iyr = 1900 + now->tm_year;                     /* since tm_year is years since 1900 */
    *imd = 100 * (1 + now->tm_mon) + now->tm_mday;  /* since tm_mon is 0-11 */
    *ihm = 100 * now->tm_hour + now->tm_min;
    *ics = 100 * now->tm_sec;
    
    /* add any fractional part of dmadptr to ics */
    *ics += (int)(fmod(dmadptr, 1.0)*100);
    
    return(0);
}


/***********************************************************************
*
* madGetDayno - gets day number (1-366) given year, month, and day
*
*
*    Returns -1 if illegal year, month, day passed in
*/
int madGetDayno(int year, int month, int day)
{
    time_t time_mad = 0;
    struct tm * now = NULL;
    int imd = 100*month + day;
    double madptr = 0.0;
    
    if (idmyck(day, month, year) != 0) return(-1);
    
    madptr = dmadptr(year, imd, 0, 0);
    
    time_mad = (long)(madptr - 631152000.0);  /* number of seconds from 1/1/1950 to 1/1/1970 */
    now = gmtime(&time_mad);
    return(now->tm_yday + 1);        /* tm_yday goes from 0 to 365  */
}


