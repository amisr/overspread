#! /usr/bin/env python

"""
xxxxx

~M. Nicolls
last revised: xx/xx/2007

"""

import ctypes
import numpy as np
import geolib

# geomag
def geomag(ct_geolib,YR,beamcodes,CLAT,CLONG,CALT=0.0,rng=np.arange(0.,1050.,50.)):
    #
    # This function computes geomagnetic information for each of the beams.
    # Includes calls to .
    #
    
    num_beams = beamcodes.shape[0]
        
    # initialize all the output vars
    kvec   = np.zeros((num_beams,3),dtype=float) # k vector (geographic)
    lat    = np.zeros((num_beams,rng.shape[0]),dtype=float) # lat
    lon    = np.zeros((num_beams,rng.shape[0]),dtype=float) # long
    plat   = np.zeros((num_beams,rng.shape[0]),dtype=float) # mag lat
    plong  = np.zeros((num_beams,rng.shape[0]),dtype=float) # mag long
    dip    = np.zeros((num_beams,rng.shape[0]),dtype=float) # dip angle
    dec    = np.zeros((num_beams,rng.shape[0]),dtype=float) # dec angle
    ht     = np.zeros((num_beams,rng.shape[0]),dtype=float) # altitude
    kpn    = np.zeros((num_beams,rng.shape[0]),dtype=float) # k component, perp north
    kpe    = np.zeros((num_beams,rng.shape[0]),dtype=float) # k component, perp east
    kpar   = np.zeros((num_beams,rng.shape[0]),dtype=float) # k component, anti-parallel
    kn     = np.zeros((num_beams,rng.shape[0]),dtype=float) # k component, north
    ke     = np.zeros((num_beams,rng.shape[0]),dtype=float) # k component, east
    kz     = np.zeros((num_beams,rng.shape[0]),dtype=float) # k component, up    
    kgeo   = np.zeros((num_beams,rng.shape[0],3),dtype=float) # k vector, geodetic coords
    kgmag  = np.zeros((num_beams,rng.shape[0],3),dtype=float) # k vector, geomag coords
    Bx     = np.zeros((num_beams,rng.shape[0]),dtype=float) # B north
    By     = np.zeros((num_beams,rng.shape[0]),dtype=float) # B east
    Bz     = np.zeros((num_beams,rng.shape[0]),dtype=float) # B down
    B      = np.zeros((num_beams,rng.shape[0],3),dtype=float) # B vector
    Babs   = np.zeros((num_beams,rng.shape[0]),dtype=float) # magnitude of B
    Lshell = np.zeros((num_beams,rng.shape[0]),dtype=float) # L shell value in Re
    MagMN  = np.zeros((num_beams,rng.shape[0]),dtype=float) # Magnetic local time midnight in UT hours
                
    # get station geocentric lat and distance 
    SLAT,SR=geolib.convrt(ct_geolib,CLAT,CALT,dir=1)

    # loop over beams
    for i in range(num_beams):
        az = beamcodes[i,1]
        el = beamcodes[i,2]

        kvec[i] = np.array([np.cos(el * np.pi / 180.) * np.cos(az * np.pi / 180.),
                            np.cos(el * np.pi / 180.) * np.sin(az * np.pi / 180.),
                            np.sin(el * np.pi / 180.)],dtype=float) # unit vector in k direction (geographic)
               
        # loop over ranges
        for j in range(rng.shape[0]):
            # geodetic lat, long, altitude
            PR,GCLAT,GLON,GDLAT,GDALT = geolib.point(ct_geolib,SR,SLAT,CLONG,az,el,rng[j])
            ht[i,j] = GDALT
            lat[i,j] = GDLAT
            lon[i,j] = GLON
            RCOR = geolib.coord(ct_geolib,CLAT,CLONG,SR,SLAT,YR,az,el,rng[j],GDLAT,GLON,GDALT)
            dat1,dat2,dat3,dat4 = geolib.geocgm01(ct_geolib,YR,GDALT,GCLAT,GLON)
            
            # magnetic field
            br = RCOR[7] * 1e-4
            bt = RCOR[8] * 1e-4
            bp = RCOR[9] * 1e-4 # r, theta, phi
            Bx[i,j] = -bt
            By[i,j] = bp
            Bz[i,j] = -br
            B[i,j,0]=Bx[i,j]
            B[i,j,1]=By[i,j]
            B[i,j,2]=Bz[i,j]
            Babs[i,j] = RCOR[6]*1e-4
                        
            # dip and dec angles
            dip[i,j] = RCOR[28]
            dec[i,j] = RCOR[29]        
            
            # magnetic latitude and longitude
            plat[i,j] = dat3[2] 
            plong[i,j] = dat3[3] 
            
            # Lshell & mag midnight
            Lshell[i,j] = dat3[4] # apex of magnetic field line in Re
            MagMN[i,j]  = dat1[10] # local magnetic midnight in UT hours
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
            # compute k vectors using direction cosines 
            kgmag[i,j,0] = RCOR[24]
            kpar[i,j]    = RCOR[24] # anti-parallel
            kgmag[i,j,1] = RCOR[23]
            kpe[i,j]     = RCOR[23] # perp-east
            kgmag[i,j,2] = -RCOR[22]
            kpn[i,j]     = -RCOR[22] # perp-north
            
            kgeo[i,j,0] = -RCOR[19]
            kn[i,j]     = -RCOR[19] # north
            kgeo[i,j,1] = RCOR[20]
            ke[i,j]     = RCOR[20] # east
            kgeo[i,j,2] = RCOR[21]
            kz[i,j]     = RCOR[21] # up

    # assign output parameters
    gmag = {}
    gmag['Range'] = rng * 1000.0
    gmag['Altitude'] = ht * 1000.0
    gmag['Latitude'] = lat
    gmag['Longitude'] = lon
    gmag['MagneticLatitude'] = plat
    gmag['MagneticLongitude'] = plong
    gmag['kpn'] = kpn
    gmag['kpe'] = kpe
    gmag['kpar'] = kpar
    gmag['kn'] = kn
    gmag['ke'] = ke
    gmag['kz'] = kz
    gmag['kvec'] = kvec
    gmag['kgmag'] = kgmag
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
def geomagTime(ct_geolib,YR,az,el,CLAT,CLONG,CALT=0.0,rng=np.arange(0.,1050.,50.)):
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
    kvec    = np.zeros((num_times,3),float) * np.nan # k vector (geographic)
    lat    = np.zeros((num_times,num_ranges),float) * np.nan # lat
    lon    = np.zeros((num_times,num_ranges),float) * np.nan # long
    plat   = np.zeros((num_times,num_ranges),float) * np.nan # mag lat
    plong  = np.zeros((num_times,num_ranges),float) * np.nan # mag long
    dip    = np.zeros((num_times,num_ranges),float) * np.nan # dip angle
    dec    = np.zeros((num_times,num_ranges),float) * np.nan # dec angle
    ht     = np.zeros((num_times,num_ranges),float) * np.nan # altitude
    kpn    = np.zeros((num_times,num_ranges),float) * np.nan # k component, perp north
    kpe    = np.zeros((num_times,num_ranges),float) * np.nan # k component, perp east
    kpar   = np.zeros((num_times,num_ranges),float) * np.nan # k component, anti-parallel
    kn     = np.zeros((num_times,num_ranges),float) * np.nan # k component, north
    ke     = np.zeros((num_times,num_ranges),float) * np.nan # k component, east
    kz     = np.zeros((num_times,num_ranges),float) * np.nan # k component, up    
    kgeo   = np.zeros((num_times,num_ranges,3),float) * np.nan # k vector, geodetic coords
    kgmag  = np.zeros((num_times,num_ranges,3),float) * np.nan # k vector, geomag coords
    Bx     = np.zeros((num_times,num_ranges),float) * np.nan # B north
    By     = np.zeros((num_times,num_ranges),float) * np.nan # B east
    Bz     = np.zeros((num_times,num_ranges),float) * np.nan # B down
    B      = np.zeros((num_times,num_ranges,3),float) * np.nan # B vector
    Babs   = np.zeros((num_times,num_ranges),float) * np.nan # magnitude of B
    Lshell = np.zeros((num_times,num_ranges),float) * np.nan # L shell value in Re
    MagMN  = np.zeros((num_times,num_ranges),float) * np.nan # Magnetic local time midnight in UT hours
                
    # get station geocentric lat and distance 
    SLAT, SR = geolib.convrt(ct_geolib,CLAT,CALT,dir=1)
                    
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
            
                # geodetic lat, long, altitude
                PR,GCLAT,GLON,GDLAT,GDALT = geolib.point(ct_geolib,SR,SLAT,CLONG,azp,elp,rngIt[j])
                ht[i,j] = GDALT
                lat[i,j] = GDLAT
                lon[i,j] = GLON

                RCOR = geolib.coord(ct_geolib,CLAT,CLONG,SR,SLAT,YR,azp,elp,rngIt[j],GDLAT,GLON,GDALT)
                dat1,dat2,dat3,dat4 = geolib.geocgm01(ct_geolib,YR,GDALT,GCLAT,GLON)
            
                # magnetic field
                br = RCOR[7]*1e-4
                bt = RCOR[8]*1e-4
                bp = RCOR[9]*1e-4 # r, theta, phi
                Bx[i,j] = -bt
                By[i,j] = bp
                Bz[i,j] = -br
                B[i,j,0] = Bx[i,j]
                B[i,j,1] = By[i,j]
                B[i,j,2] = Bz[i,j]
                Babs[i,j] = RCOR[6]*1e-4
                        
                # dip and dec angles
                dip[i,j] = RCOR[28]
                dec[i,j] = RCOR[29]        
            
                # magnetic latitude and longitude
                plat[i,j] = dat3[2] 
                plong[i,j] = dat3[3] 
            
                # Lshell & mag midnight
                Lshell[i,j] = dat3[4] # apex of magnetic field line in Re
                MagMN[i,j] = dat1[10] # local magnetic midnight in UT hours
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
                # compute k vectors using direction cosines 
                kgmag[i,j,0] = RCOR[24]
                kpar[i,j] = RCOR[24] # anti-parallel
                kgmag[i,j,1] = RCOR[23]
                kpe[i,j] = RCOR[23] # perp-east
                kgmag[i,j,2] = -RCOR[22]
                kpn[i,j] = -RCOR[22] # perp-north
            
                kgeo[i,j,0] = -RCOR[19]
                kn[i,j] = -RCOR[19] # north
                kgeo[i,j,1] = RCOR[20]
                ke[i,j] = RCOR[20] # east
                kgeo[i,j,2] = RCOR[21]
                kz[i,j] = RCOR[21] # up
      
    # assign output parameters
    gmag = {}
    gmag['Range'] = rngIt*1000.0
    gmag['Altitude'] = ht*1000.0
    gmag['Latitude'] = lat
    gmag['Longitude'] = lon
    gmag['MagneticLatitude'] = plat
    gmag['MagneticLongitude'] = plong
    gmag['kpn'] = kpn
    gmag['kpe'] = kpe
    gmag['kpar'] = kpar
    gmag['kn'] = kn
    gmag['ke'] = ke
    gmag['kz'] = kz
    gmag['kvec'] = kvec
    #gmag['kgmag'] = kgmag
    gmag['kgeo'] = kgeo
    gmag['Dip'] = dip
    gmag['Declination'] = dec
    gmag['Bx'] = Bx
    gmag['By'] = By
    gmag['Bz'] = Bz
    #gmag['B'] = B

    gmag['Babs'] = Babs
    gmag['LshellRe'] = Lshell
    gmag['MLTMidnightUT'] = MagMN
        
    return gmag	
    
def blankGmag(Nx=1,Ny=1):
    
    gmag={}
    gmag['Range'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['Altitude'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['Latitude'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['Longitude'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['MagneticLatitude'] = np.zeros((Nx,Ny),dtype=float) * np.nan 
    gmag['MagneticLongitude'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['kpn'] = np.zeros((Nx,Ny),dtype=float) * np.nan 
    gmag['kpe'] = np.zeros((Nx,Ny),dtype=float) * np.nan
    gmag['kpar'] = np.zeros((Nx,Ny),dtype=float) * np.nan
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
    
