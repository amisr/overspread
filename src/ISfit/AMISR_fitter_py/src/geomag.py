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
    kpncovar    = np.zeros((num_beams,rng.shape[0]),dtype=float) # k component, perp north
    kpecovar    = np.zeros((num_beams,rng.shape[0]),dtype=float) # k component, perp east
    kparcovar   = np.zeros((num_beams,rng.shape[0]),dtype=float) # k component, anti-parallel
    kpncontra   = np.zeros((num_beams,rng.shape[0]),dtype=float) # k component, perp north
    kpecontra   = np.zeros((num_beams,rng.shape[0]),dtype=float) # k component, perp east
    kparcontra  = np.zeros((num_beams,rng.shape[0]),dtype=float) # k component, anti-parallel
    kn          = np.zeros((num_beams,rng.shape[0]),dtype=float) # k component, north
    ke          = np.zeros((num_beams,rng.shape[0]),dtype=float) # k component, east
    kz          = np.zeros((num_beams,rng.shape[0]),dtype=float) # k component, up    
    kgeo        = np.zeros((num_beams,rng.shape[0],3),dtype=float) # k vector, geodetic coords
    kgmagcovar  = np.zeros((num_beams,rng.shape[0],3),dtype=float) # k vector, geomag coords
    kgmagcontra = np.zeros((num_beams,rng.shape[0],3),dtype=float) # k vector, geomag coords
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
            kpncovar[i,j] = output['kpnCovariant']
            kpecovar[i,j] = output['kpeCovariant']
            kparcovar[i,j] = output['kparCovariant']
            kpncontra[i,j] = output['kpnContravariant']
            kpecontra[i,j] = output['kpeContravariant']
            kparcontra[i,j] = output['kparContravariant']
            kn[i,j] = output['kn']
            ke[i,j] = output['ke']
            kz[i,j] = output['kz']
            kvec[i] = output['kvec']
            kgmagcovar[i,j,:] = output['kgmagCovariant']
            kgmagcontra[i,j,:] = output['kgmagContravariant']
            kgeo[i,j,:] = output['kgeo']
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
    gmag['Range'] = rng * 1000.0
    gmag['Altitude'] = ht * 1000.0
    gmag['Latitude'] = lat
    gmag['Longitude'] = lon
    gmag['MagneticLatitude'] = plat
    gmag['MagneticLongitude'] = plong
    gmag['kpnCovariant'] = kpncovar
    gmag['kpeCovariant'] = kpecovar
    gmag['kparCovariant'] = kparcovar
    gmag['kpnContravariant'] = kpncontra
    gmag['kpeContravariant'] = kpecontra
    gmag['kparContravariant'] = kparcontra
    gmag['kn'] = kn
    gmag['ke'] = ke
    gmag['kz'] = kz
    gmag['kvec'] = kvec
    gmag['kgmagCovariant'] = kgmagcovar
    gmag['kgmagContravariant'] = kgmagcontra
    gmag['kgeo'] = kgeo
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
    kpncovar    = np.zeros((num_times,num_ranges),float) * np.nan # k component, perp north
    kpecovar    = np.zeros((num_times,num_ranges),float) * np.nan # k component, perp east
    kparcovar   = np.zeros((num_times,num_ranges),float) * np.nan # k component, anti-parallel
    kpncontra   = np.zeros((num_times,num_ranges),float) * np.nan # k component, perp north
    kpecontra   = np.zeros((num_times,num_ranges),float) * np.nan # k component, perp east
    kparcontra  = np.zeros((num_times,num_ranges),float) * np.nan # k component, anti-parallel
    kn          = np.zeros((num_times,num_ranges),float) * np.nan # k component, north
    ke          = np.zeros((num_times,num_ranges),float) * np.nan # k component, east
    kz          = np.zeros((num_times,num_ranges),float) * np.nan # k component, up    
    kgeo        = np.zeros((num_times,num_ranges,3),float) * np.nan # k vector, geodetic coords
    kgmagcovar  = np.zeros((num_times,num_ranges,3),float) * np.nan # k vector, geomag coords
    kgmagcontra = np.zeros((num_times,num_ranges,3),float) * np.nan # k vector, geomag coords
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

                # covariant (tangent to coordinate system direction) apex kvector
                covar_kpar = np.dot(kgeodetic,d[2])
                covar_kpe = np.dot(kgeodetic,d[0])
                covar_kpn = -np.dot(kgeodetic,d[1])

                # contravariant (normal to coordinate system direction) apex kvector
                contra_kpar = np.dot(kgeodetic,e[2])
                contra_kpe = np.dot(kgeodetic,e[0])
                contra_kpn = -np.dot(kgeodetic,e[1])

                # geomagnetic kvectors
                kgmagcovar[i,j,0] = -covar_kpar
                kparcovar[i,j] = -covar_kpar # anti-parallel
                kgmagcovar[i,j,1] = covar_kpe
                kpecovar[i,j] = covar_kpe # perp-east
                kgmagcovar[i,j,2] = covar_kpn
                kpncovar[i,j] = covar_kpn # perp-north

                kgmagcontra[i,j,0] = -contra_kpar
                kparcontra[i,j] = -contra_kpar # anti-parallel
                kgmagcontra[i,j,1] = contra_kpe
                kpecontra[i,j] = contra_kpe # perp-east
                kgmagcontra[i,j,2] = contra_kpn
                kpncontra[i,j] = contra_kpn # perp-north

    # assign output parameters
    gmag = {}
    gmag['Range'] = rngIt * 1000.0
    gmag['Altitude'] = ht
    gmag['Latitude'] = lat
    gmag['Longitude'] = lon
    gmag['MagneticLatitude'] = plat
    gmag['MagneticLongitude'] = plong
    gmag['kpnCovariant'] = kpncovar
    gmag['kpeCovariant'] = kpecovar
    gmag['kparCovariant'] = kparcovar
    gmag['kpnContravariant'] = kpncontra
    gmag['kpeContravariant'] = kpecontra
    gmag['kparContravariant'] = kparcontra
    gmag['kn'] = kn
    gmag['ke'] = ke
    gmag['kz'] = kz
    gmag['kvec'] = kvec
    gmag['kgmagCovariant'] = kgmagcovar
    gmag['kgmagContravariant'] = kgmagcontra
    gmag['kgeo'] = kgeo
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

    initial_guess=12.*3600.  # in seconds
    result = scipy.optimize.fsolve(f, np.array([initial_guess]),full_output=True,epsfcn=0.1,factor=0.1)

    # now calculate UT hours
    updated_date = starting_date + timedelta(seconds=result[0][0])

    ut_hours = (updated_date.hour + updated_date.minute / 60. + 
                (updated_date.second + updated_date.microsecond / 1e6) / 3600.)


    return ut_hours


def blankGmag(Nx=1,Ny=1):
    
    gmag={}
    gmag['Range'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['Altitude'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['Latitude'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['Longitude'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['MagneticLatitude'] = np.zeros((Nx,Ny),dtype=float) * np.nan 
    gmag['MagneticLongitude'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['kpnCovariant'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['kpeCovariant'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['kparCovariant'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['kpnContravariant'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['kpeContravariant'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['kparContravariant'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['kn'] = np.zeros((Nx,Ny),dtype=float) * np.nan 
    gmag['ke'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['kz'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['kvec'] = np.zeros((Nx,3),dtype=float) * np.nan
    gmag['Dip'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['Declination'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['Bx'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['By'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['Bz'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['Babs'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['LshellRe'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['MLTMidnightUT'] = np.zeros((Nx,Ny),dtype=float) * np.nan

    return gmag	
    
