#! /usr/bin/env python

"""
xxxxx

~M. Nicolls
last revised: xx/xx/2007

"""

import scipy
import ctypes

def convrt(ct_geolib,inL,inR,dir=1):
    #
    #   Converts between geodetic and geocentric coordinates. the
    #   reference geoid is that adopted by the iau in 1964. a=6378.16,
    #   b=6356.7746, f=1/298.25. the equations for conversion from
    #   geocentric to geodetic are from astron. j., vol 66, 1961, p. 15.
    #
    #   Input:
    #       dir - 1 geodetic to geocentric, 2 geocentric to geodetic
    #       inL - in latitude in degrees, geodetic (dir=1) or geocentric (dir=2)
    #       inR - in Range in km, altitude above geoid (dir=1) or geocentric radial distance (dir=2)
    #   Output:
    #       outL - out latitude in degrees, geodetic (dir=2) or geocentric (dir=1)
    #       outR - out Range in km, altitude above geoid (dir=2) or geocentric radial distance (dir=1)
    #

    convrt=ct_geolib.convrt_
    #print convrt
    convrt.argtypes=[ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_double)]
    in1=ctypes.c_int(dir); in2=ctypes.c_double(inL); in3=ctypes.c_double(inR)
    out1=ctypes.c_double(0.0); out2=ctypes.c_double(0.0)
    if dir==1:
        convrt(in1,in2,in3,out1,out2)
    else:
        convrt(in1,out1,out2,in2,in3)

    return out1.value,out2.value

def point(ct_geolib,SR,SLAT,SLON,AZ,EL,RANGE):
    #
    #     POINT calculates the position of a point defined by the radar
    #     line-of sight vector to that point.
    #
    #     Input:
    #       SR    - radial distance of station from center of earth (km)
    #       SLAT  - geocentric latitude of station (deg)
    #       SLON  - longitude of station (deg)
    #       AZ    - radar azimuth (deg)
    #       EL    - radar elevation (deg)
    #       RANGE - radar range (km)
    #
    #     Output:
    #       PR    - distance from center of earth to observation point (km)
    #       GCLAT  - observation point geocentric latitude (deg)
    #       GLON  - observation point longitude (deg)
    #       GDLAT  - observation point geodetic latitude (deg)
    #       GDALT - altitude above geoid (km)
    #
    
    point=ct_geolib.point_
    point.argtypes=[ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_double),
        ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_double),
        ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_double)]
    out1=ctypes.c_double(0.0); out2=ctypes.c_double(0.0); out3=ctypes.c_double(0.0)
    point(ctypes.c_double(SR),ctypes.c_double(SLAT),ctypes.c_double(SLON),ctypes.c_double(AZ),ctypes.c_double(EL),
        ctypes.c_double(RANGE),out1,out2,out3)
    PR=out1.value; GCLAT=out2.value; GLON=out3.value
    GDLAT,GDALT=convrt(ct_geolib,GCLAT,PR,2) # convert to geodetic

    return PR,GCLAT,GLON,GDLAT,GDALT    

def csconv(ct_geolib,X,Y,Z,dir=1):
    """
    C
    C     Converts between cartesian coordinates x,y,z and spherical
    C     coordinates r,theta,phi.  theta and phi are in degrees.
    C
    C       Input:
    C         IMODE - 1 (x,y,z) -> (r,theta,phi)
    C                 2 (r,theta,phi) -> (x,y,z)
    C
    C       Input, Output:
    C              X, Y, Z - cartesian coordinates
    C        R, THETA, PHI - spherical coordinates (degrees)
    """
    csconv=ct_geolib.csconv_
    csconv.argtypes=[ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_double),
        ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_double),
        ctypes.POINTER(ctypes.c_int)]
    in1=ctypes.c_int(dir); in2=ctypes.c_double(X); in3=ctypes.c_double(Y); in4=ctypes.c_double(Z)
    out1=ctypes.c_double(0.0); out2=ctypes.c_double(0.0); out3=ctypes.c_double(0.0)
    if dir==1:
        csconv(in2,in3,in4,out1,out2,out3,in1)
    else:
        csconv(out1,out2,out3,in2,in3,in4,in1)
        
    return out1.value,out2.value,out3.value

def ts_recalc(ct_geolib,iyr):
    ts_recalc=ct_geolib.ts_recalc_
    point.argtypes=[ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int),
        ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
    
    ts_recalc(ctypes.c_int(iyr),ctypes.c_int(1),ctypes.c_int(1),ctypes.c_int(1),ctypes.c_int(1))
    
    return
        
def ts_geomag(ct_geolib,X,Y,Z,TM,dir=1):
    #
    #   geocentric to geomag (dir>0)
    #
    
    ts_geomag=ct_geolib.ts_geomag_
    point.argtypes=[ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_double),
        ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_double),
        ctypes.POINTER(ctypes.c_int)]
    in1=ctypes.c_int(dir); in2=ctypes.c_double(X); in3=ctypes.c_double(Y); in4=ctypes.c_double(Z)
    out1=ctypes.c_double(0.0); out2=ctypes.c_double(0.0); out3=ctypes.c_double(0.0)
    
    ts_recalc(ct_geolib,TM)
    if dir>0:
        ts_geomag(in2,in3,in4,out1,out2,out3,in1)
    else:
        ts_geomag(out1,out2,out3,in2,in3,in4,in1)
    
    return out1.value,out2.value,out3.value
    

def coord(ct_geolib,SLATGD,SLON,SR,SLATGC,TM,AZ,EL,RANGE,GDLAT,GLON,GDALT):
    """
    C     Calculates the listed coordinates of a specified point. the
    C     point may be specified either by slatgd, slon, sr, slatgc, tm,
    C     az, el, range (range .ge. 0.) or by gdlat, glon, gdalt (range
    C     .lt. 0.)
    C
    C     Input:
    C        SLATGD - station geodetic latitude
    C        SLON   - station longitude
    C        SR     - radial distance of station from center of earth
    C        SLATGC - station geocentric latitude
    C        TM     - time in years (e.g. 1975.2)
    C        AZ     - radar azimuth
    C        EL     - radar elevation
    C        RANGE  - range to observation point
    C     Input, output:
    C        GDLAT  - geodetic latitude of observation point
    C        GLON   - longitude of observation point
    C        GDALT  - altitude above spheroid of observation point
    C     Output:
    C        RCOR(7)  - b     - magnitude of geomagnetic field
    C        RCOR(8)  - br    - radial component of geomagnetic field
    C        RCOR(9)  - bt    - southward component of geomagnetic field
    C        RCOR(10) - bp    - eastward component of geomagnetic field
    C        RCOR(11) - rlatm - dip latitude
    C        RCOR(12) - rlati - invariant latitude
    C        RCOR(13) - rl    - magnetic l parameter
    C        RCOR(14) - alat  - apex latitude
    C        RCOR(15) - alon  - apex longitude
    C
    C        RCOR(16) - g(1,1) magnetic coordinate system metric tensor,
    C                          upper half stored row-wise
    C        RCOR(17) - g(2,1) "                                       "
    C        RCOR(18) - g(2,1) "                                       "
    C        RCOR(19) - g(2,1) "                                       "
    C
    C        RCOR(20) - south-direction cosine w/respect to geodetic coords.
    C        RCOR(21) - east-direction cosine "                            "
    C        RCOR(22) - upward-direction cosine "                          "
    C
    C        RCOR(23) - perpendicular to b in magnetic n - s plane
    C                   (magnetic south)
    C        RCOR(24) - perpendicular to b in horizontal plane
    C                   (magnetic east)
    C        RCOR(25) - upward along magnetic field
    C
    C        RCOR(26) - x-direction cosine of a vector perpendicular to
    C                   l.o.s. w/respect to apex coords.
    C        RCOR(27) - y-direction cosine "                          "
    C        RCOR(28) - z-direction cosine "                          "
    C
    C        RCOR(29) - inclination of geomagnetic field
    C        RCOR(30) - declination of geomagnetic field
    C        RCOR(31) - gclat - geocentric latitude
    C        RCOR(32) - aspct - aspect angle
    C        RCOR(33) - conjugate geocentric latitude
    C        RCOR(34) - conjugate geodetic latitude
    C        RCOR(35) - conjugate longitude
    C
    """

    coord=ct_geolib.coord_
    coord.argtypes=[ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_double),
        ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_double),
        ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_double),
        ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_double)]
    #ctypes.POINTER(ctypes.ARRAY(ctypes.c_double,35))
    out1=(ctypes.c_double*35)()
    coord(ctypes.c_double(SLATGD),ctypes.c_double(SLON),ctypes.c_double(SR),ctypes.c_double(SLATGC),
        ctypes.c_double(TM),ctypes.c_double(AZ),ctypes.c_double(EL),ctypes.c_double(RANGE),
        ctypes.c_double(GDLAT),ctypes.c_double(GLON),ctypes.c_double(GDALT),out1)

    return out1
    
def geocgm01(ct_geolib,IYEAR,HI,GLAT,GLON,MAXYR=999999):

    if IYEAR>MAXYR:
        IYEAR=MAXYR

    geocgm=ct_geolib.geogcmiface_
    geocgm.argtypes=[ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_double),
        ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_double),
        ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_double)]
    
    dat1=(ctypes.c_double*11)(GLAT,GLON); dat2=(ctypes.c_double*11)(); dat3=(ctypes.c_double*11)(); dat4=(ctypes.c_double*11)()
    out2=(ctypes.c_double*4)(); out3=(ctypes.c_double*4)();
    #print IYEAR,HI,dat1[:],dat2[:],dat3[:],dat4[:],out2[:],out3[:]

    geocgm(ctypes.c_int(1),ctypes.c_int(int(IYEAR)),ctypes.c_double(HI),dat1,dat2,dat3,dat4,out2,out3)
    
    return dat1,dat2,dat3,dat4


    
    