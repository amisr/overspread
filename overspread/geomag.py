#! /usr/bin/env python

"""
xxxxx

~M. Nicolls
last revised: xx/xx/2007

"""

# import ctypes
import numpy as np
from datetime import datetime, timedelta

import pymap3d
import apexpy
import scipy.optimize

# geomag
def geomag(YR,beamcodes,CLAT,CLONG,CALT=0.0,rng=np.arange(0.,1050.,50.)):
    #
    # This function computes geomagnetic information for each of the beams.
    # Includes calls to .
    #
    
    num_beams = beamcodes.shape[0]
        
    # initialize all the output vars
    kvec        = np.zeros((num_beams,3),dtype=float) # k vector (geographic)
    lat         = np.zeros((num_beams,rng.shape[0]),dtype=float) # lat
    lon         = np.zeros((num_beams,rng.shape[0]),dtype=float) # long
    plat        = np.zeros((num_beams,rng.shape[0]),dtype=float) # mag lat
    plong       = np.zeros((num_beams,rng.shape[0]),dtype=float) # mag long
    dip         = np.zeros((num_beams,rng.shape[0]),dtype=float) # dip angle
    dec         = np.zeros((num_beams,rng.shape[0]),dtype=float) # dec angle
    ht          = np.zeros((num_beams,rng.shape[0]),dtype=float) # altitude
    kgmag       = np.zeros((num_beams,rng.shape[0],3),dtype=float)
    kpn         = np.zeros((num_beams,rng.shape[0]),dtype=float)
    kpe         = np.zeros((num_beams,rng.shape[0]),dtype=float)
    kpar        = np.zeros((num_beams,rng.shape[0]),dtype=float)
    kvece       = np.zeros((num_beams,rng.shape[0],3),dtype=float)
    kvece1      = np.zeros((num_beams,rng.shape[0]),dtype=float)
    kvece2      = np.zeros((num_beams,rng.shape[0]),dtype=float)
    kvece3      = np.zeros((num_beams,rng.shape[0]),dtype=float)
    kvecd       = np.zeros((num_beams,rng.shape[0],3),dtype=float)
    kvecd1      = np.zeros((num_beams,rng.shape[0]),dtype=float)
    kvecd2      = np.zeros((num_beams,rng.shape[0]),dtype=float)
    kvecd3      = np.zeros((num_beams,rng.shape[0]),dtype=float)
    kn          = np.zeros((num_beams,rng.shape[0]),dtype=float) # k component, north
    ke          = np.zeros((num_beams,rng.shape[0]),dtype=float) # k component, east
    kz          = np.zeros((num_beams,rng.shape[0]),dtype=float) # k component, up    
    kgeo        = np.zeros((num_beams,rng.shape[0],3),dtype=float) # k vector, geodetic coords
    Bx          = np.zeros((num_beams,rng.shape[0]),dtype=float) # B north
    By          = np.zeros((num_beams,rng.shape[0]),dtype=float) # B east
    Bz          = np.zeros((num_beams,rng.shape[0]),dtype=float) # B down
    B           = np.zeros((num_beams,rng.shape[0],3),dtype=float) # B vector
    Babs        = np.zeros((num_beams,rng.shape[0]),dtype=float) # magnitude of B
    Lshell      = np.zeros((num_beams,rng.shape[0]),dtype=float) # L shell value in Re
    MagMN       = np.zeros((num_beams,rng.shape[0]),dtype=float) # Magnetic local time midnight in UT hours
                
    # loop over beams
    for i in range(num_beams):
        az = beamcodes[i,1]
        el = beamcodes[i,2]

        for j in range(rng.shape[0]):

            output = geomagTime(YR,np.array([az]),np.array([el]),CLAT,CLONG,CALT,rng=np.array([rng[j]]))

            ht[i,j] = output['Altitude']
            lat[i,j] = output['Latitude']
            lon[i,j] = output['Longitude']
            plat[i,j] = output['MagneticLatitude']
            plong[i,j] = output['MagneticLongitude']
            kgmag[i,j,:] = output['kgmag']
            kpn[i,j] = output['kpn']
            kpe[i,j] = output['kpe']
            kpar[i,j] = output['kpar']
            kvece[i,j,:] = output['kapexe']
            kvece1[i,j] = output['kapexe1']
            kvece2[i,j] = output['kapexe2']
            kvece3[i,j] = output['kapexe3']
            kvecd[i,j,:] = output['kapexd']
            kvecd1[i,j] = output['kapexd1']
            kvecd2[i,j] = output['kapexd2']
            kvecd3[i,j] = output['kapexd3']
            kgeo[i,j,:] = output['kgeo']
            kn[i,j] = output['kn']
            ke[i,j] = output['ke']
            kz[i,j] = output['kz']
            kvec[i] = output['kvec']
            dip[i,j] = output['Dip']
            dec[i,j] = output['Declination']
            Bx[i,j] = output['Bx']
            By[i,j] = output['By']
            Bz[i,j] = output['Bz']
            B[i,j,:] = output['B']
            Babs[i,j] = output['Babs']
            Lshell[i,j] = output['LshellRe']
            MagMN[i,j] = output['MLTMidnightUT']

    # assign output parameters
    gmag = {}
    gmag['Range'] = rng
    gmag['Altitude'] = ht
    gmag['Latitude'] = lat
    gmag['Longitude'] = lon
    gmag['MagneticLatitude'] = plat
    gmag['MagneticLongitude'] = plong
    gmag['kgmag'] = kgmag
    gmag['kpn'] = kpn
    gmag['kpe'] = kpe
    gmag['kpar'] = kpar
    gmag['kapexe'] = kvece
    gmag['kapexe1'] = kvece1
    gmag['kapexe2'] = kvece2
    gmag['kapexe3'] = kvece3
    gmag['kapexd'] = kvecd
    gmag['kapexd1'] = kvecd1
    gmag['kapexd2'] = kvecd2
    gmag['kapexd3'] = kvecd3
    gmag['kgeo'] = kgeo
    gmag['kn'] = kn
    gmag['ke'] = ke
    gmag['kz'] = kz
    gmag['kvec'] = kvec
    gmag['kgmag'] = kgmag
    gmag['Dip'] = dip
    gmag['Declination'] = dec
    gmag['Bx'] = Bx
    gmag['By'] = By
    gmag['Bz'] = Bz
    gmag['B'] = B
    gmag['Babs'] = Babs
    gmag['LshellRe'] = Lshell
    gmag['MLTMidnightUT'] = MagMN

    return gmag

# geomag
def geomagTime(YR,az,el,CLAT,CLONG,CALT=0.0,rng=np.arange(0.,1050.,50.)):
    #
    # This function computes geomagnetic information for each of the beams.
    # Includes calls to .
    #
    num_times = az.shape[0]
    if rng.ndim == 1 :
        num_ranges = rng.shape[0]
    else:
        num_ranges = rng.shape[1]    
        
    # initialize all the output vars
    kvec        = np.zeros((num_times,3),float) * np.nan # k vector (geographic)
    lat         = np.zeros((num_times,num_ranges),float) * np.nan # lat
    lon         = np.zeros((num_times,num_ranges),float) * np.nan # long
    plat        = np.zeros((num_times,num_ranges),float) * np.nan # mag lat
    plong       = np.zeros((num_times,num_ranges),float) * np.nan # mag long
    dip         = np.zeros((num_times,num_ranges),float) * np.nan # dip angle
    dec         = np.zeros((num_times,num_ranges),float) * np.nan # dec angle
    ht          = np.zeros((num_times,num_ranges),float) * np.nan # altitude
    kgmag       = np.zeros((num_times,num_ranges,3),float) * np.nan
    kpn         = np.zeros((num_times,num_ranges),float) * np.nan
    kpe         = np.zeros((num_times,num_ranges),float) * np.nan
    kpar        = np.zeros((num_times,num_ranges),float) * np.nan
    kvece       = np.zeros((num_times,num_ranges,3),float) * np.nan
    kvece1      = np.zeros((num_times,num_ranges),float) * np.nan
    kvece2      = np.zeros((num_times,num_ranges),float) * np.nan
    kvece3      = np.zeros((num_times,num_ranges),float) * np.nan
    kvecd       = np.zeros((num_times,num_ranges,3),float) * np.nan
    kvecd1      = np.zeros((num_times,num_ranges),float) * np.nan
    kvecd2      = np.zeros((num_times,num_ranges),float) * np.nan
    kvecd3      = np.zeros((num_times,num_ranges),float) * np.nan
    kgeo        = np.zeros((num_times,num_ranges,3),float) * np.nan # k vector, geodetic coords
    kn          = np.zeros((num_times,num_ranges),float) * np.nan # k component, north
    ke          = np.zeros((num_times,num_ranges),float) * np.nan # k component, east
    kz          = np.zeros((num_times,num_ranges),float) * np.nan # k component, up    
    Bx          = np.zeros((num_times,num_ranges),float) * np.nan # B north
    By          = np.zeros((num_times,num_ranges),float) * np.nan # B east
    Bz          = np.zeros((num_times,num_ranges),float) * np.nan # B down
    B           = np.zeros((num_times,num_ranges,3),float) * np.nan # B vector
    Babs        = np.zeros((num_times,num_ranges),float) * np.nan # magnitude of B
    Lshell      = np.zeros((num_times,num_ranges),float) * np.nan # L shell value in Re
    MagMN       = np.zeros((num_times,num_ranges),float) * np.nan # Magnetic local time midnight in UT hours
                
    apex = apexpy.Apex(datetime(YR,1,1))
                    
    # loop over times
    for i in range(num_times):
        azp = az[i]
        elp = el[i]

        kvec[i] = np.array([np.cos(el[i] * np.pi / 180.) * np.cos(az[i] * np.pi / 180.),
                            np.cos(el[i] * np.pi / 180.) * np.sin(az[i] * np.pi / 180.),
                            np.sin(el[i] * np.pi / 180.)
                           ],dtype=float) # unit vector in k direction (geographic)
             
        if rng.ndim == 2:
            rngIt = np.squeeze(rng[i,:])
        else:
            rngIt = rng
                   
        # loop over ranges
        for j in range(num_ranges):
            
            if np.isfinite(rngIt[j]):
            
                # get geodetic lat, long, altitude of the range gate
                GDLAT,GLON,GDALT = pymap3d.aer2geodetic(azp,elp,rngIt[j]*1000.,CLAT,CLONG,CALT*1000,deg=True) # uses meters...

                ht[i,j] = GDALT
                lat[i,j] = GDLAT
                lon[i,j] = (GLON + 360) % 360  # return in 0-360 format

                # kgeo: get az, el from the range gate to site
                kaz, kel, _ = pymap3d.geodetic2aer(CLAT,CLONG,CALT*1000, GDLAT, GLON, GDALT, ell=None, deg=True)

                # kgeo is in opposite direction, hence the negative sign (E, N, U)
                kgeodetic = -1 * np.array([np.cos(kel*np.pi/180.)*np.sin(kaz*np.pi/180),
                                           np.cos(kel*np.pi/180.)*np.cos(kaz*np.pi/180),
                                           np.sin(kel*np.pi/180.)])

                # geodetic kvector
                kgeo[i,j,0] = kgeodetic[1]
                kn[i,j] = kgeodetic[1] # north
                kgeo[i,j,1] = kgeodetic[0]
                ke[i,j] = kgeodetic[0] # east
                kgeo[i,j,2] = kgeodetic[2]
                kz[i,j] = kgeodetic[2] # up

                # now, we figure out the magnetic coordinates and kvectors
                maglat, maglon = apex.convert(GDLAT, GLON, 'geo', 'apex', height=GDALT/1000., datetime=None)

                # magnetic latitude and longitude
                plat[i,j] = maglat
                plong[i,j] = maglon

                # get the magnetic field at the range gate (returned in Gauss)
                bn, be, bd, babs = apexpy.fortranapex.feldg(1,GDLAT,GLON,GDALT/1000.) # same as bx, by, bz

                # magnetic field
                Bx[i,j] = bn * 1e-4
                By[i,j] = be * 1e-4
                Bz[i,j] = bd * 1e-4
                B[i,j,0] = Bx[i,j]
                B[i,j,1] = By[i,j]
                B[i,j,2] = Bz[i,j]
                Babs[i,j] = babs * 1e-4

                # Get the apex height and convert to L-shell
                apex_alt = apex.get_apex(maglat, GDALT/1000.)
                l_shell = 1.0 + apex_alt / apex.RE

                # use Newton's method to find UT time of MLT=24
                magneticmidnightUT = find_magnetic_midnight_UT((maglon + 360) % 360,datetime(YR,1,1))

                # Lshell & mag midnight
                Lshell[i,j] = l_shell # apex of magnetic field line in Re
                MagMN[i,j] = magneticmidnightUT # local magnetic midnight in UT hours

                # dip and dec angles
                dip[i,j] = np.arcsin(bd / babs) * 180. / np.pi
                dec[i,j] = np.arctan2(be,bn) * 180. / np.pi

                # get apex direction cosines for the range gate, which enable vector conversion from geodetic to apex
                output = apex.basevectors_apex(GDLAT, GLON, GDALT/1000., coords='geo', precision=1e-10)
                d = output[6:9] # covariant (mag east, mag south, parallel)
                e = output[9:]  # contravariant (mag east, mag south, parallel)

                # See equations 60 and 61 of https://link.springer.com/content/pdf/10.1007/s11214-016-0275-y.pdf
                # doi: 10.1007/s11214-016-0275-y for definitions
                # ke kvector modified apex components
                ke1 = np.dot(kgeodetic,d[0])
                ke2 = np.dot(kgeodetic,d[1])
                ke3 = np.dot(kgeodetic,d[2])
                kvece[i,j,0] = ke1
                kvece1[i,j] = ke1
                kvece[i,j,1] = ke2
                kvece2[i,j] = ke2
                kvece[i,j,2] = ke3
                kvece3[i,j] = ke3

                # ke kvector modified apex components
                kd1 = np.dot(kgeodetic,e[0])
                kd2 = np.dot(kgeodetic,e[1])
                kd3 = np.dot(kgeodetic,e[2])
                kvecd[i,j,0] = kd1
                kvecd1[i,j] = kd1
                kvecd[i,j,1] = kd2
                kvecd2[i,j] = kd2
                kvecd[i,j,2] = kd3
                kvecd3[i,j] = kd3

                # geomagnetic kvectors
                # define anti-parallel as -ke3/|d3|, kpe as ke1/|d1|, and kpn as ke2/|d2|
                kgmag[i,j,0] = -ke3 / np.sqrt(np.sum(d[2]**2))
                kpar[i,j] = kgmag[i,j,0] # anti-parallel
                kgmag[i,j,1] = ke1 / np.sqrt(np.sum(d[0]**2))
                kpe[i,j] = kgmag[i,j,1] # perp-east
                kgmag[i,j,2] = -kd2 / np.sqrt(np.sum(e[1]**2))
                kpn[i,j] = kgmag[i,j,2] # perp-north



    # assign output parameters
    gmag = {}
    gmag['Range'] = rngIt * 1000.0
    gmag['Altitude'] = ht
    gmag['Latitude'] = lat
    gmag['Longitude'] = lon
    gmag['MagneticLatitude'] = plat
    gmag['MagneticLongitude'] = plong
    gmag['kgmag'] = kgmag
    gmag['kpn'] = kpn
    gmag['kpe'] = kpe
    gmag['kpar'] = kpar
    gmag['kapexe'] = kvece
    gmag['kapexe1'] = kvece1
    gmag['kapexe2'] = kvece2
    gmag['kapexe3'] = kvece3
    gmag['kapexd'] = kvecd
    gmag['kapexd1'] = kvecd1
    gmag['kapexd2'] = kvecd2
    gmag['kapexd3'] = kvecd3
    gmag['kgeo'] = kgeo
    gmag['kn'] = kn
    gmag['ke'] = ke
    gmag['kz'] = kz
    gmag['kvec'] = kvec
    gmag['Dip'] = dip
    gmag['Declination'] = dec
    gmag['Bx'] = Bx
    gmag['By'] = By
    gmag['Bz'] = Bz
    gmag['B'] = B
    gmag['Babs'] = Babs
    gmag['LshellRe'] = Lshell
    gmag['MLTMidnightUT'] = MagMN
        
    return gmag

   
def find_magnetic_midnight_UT(mlon,date):
    # Use Newton's method to search for
    # local magnetic midnight in UT
    # x_{n+1} = x_n - f(x_n)/f'(x_n)
    # We'll do this by updating the UT hours
    # until we obtain the input magnetic longitude
    starting_date = datetime(date.year,date.month,date.day)
    apex = apexpy.Apex(starting_date)
    # f = g(unixtime) - input mlon
    def f(seconds):
        dtime = starting_date + timedelta(seconds=float(seconds[0]))
        return apex.mlt2mlon(24, dtime) - mlon

    initial_guess = 12. * 3600.  # in seconds
    result = scipy.optimize.fsolve(f, np.array([initial_guess]),full_output=True,epsfcn=0.1,factor=0.1)

    # now calculate UT hours
    updated_date = starting_date + timedelta(seconds=result[0][0])

    ut_hours = (updated_date.hour + updated_date.minute / 60. + 
                (updated_date.second + updated_date.microsecond / 1e6) / 3600.)


    return ut_hours


def blankGmag(Nx=1,Ny=1):
    
    gmag = {}
    gmag['Range'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['Altitude'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['Latitude'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['Longitude'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['MagneticLatitude'] = np.zeros((Nx,Ny),dtype=float) * np.nan 
    gmag['MagneticLongitude'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['kgmag'] = np.zeros((Nx,Ny,3),float) * np.nan
    gmag['kpn'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['kpe'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['kpar'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['kapexe'] = np.zeros((Nx,Ny,3),float) * np.nan
    gmag['kapexe1'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['kapexe2'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['kapexe3'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['kapexd'] = np.zeros((Nx,Ny,3),float) * np.nan
    gmag['kapexd1'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['kapexd2'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['kapexd3'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['kvec'] = np.zeros((Nx,3),dtype=float) * np.nan
    gmag['kn'] = np.zeros((Nx,Ny),dtype=float) * np.nan 
    gmag['ke'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['kz'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['Dip'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['Declination'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['Bx'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['By'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['Bz'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['Babs'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['LshellRe'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['MLTMidnightUT'] = np.zeros((Nx,Ny),dtype=float) * np.nan

    return gmag	
    
