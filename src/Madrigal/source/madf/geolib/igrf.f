c $Id: igrf.f,v 1.3 2008/08/14 17:02:24 brideout Exp $        
C
C Subroutine IGRF_SUB to compute IGRF parameters for IRI. 
C Rewritten by Bill Rideout to simply call other geolib methods.  
C
        subroutine igrf_sub(xlat,xlong,year,height,
     &          xl,icode,dipl,babs)
c-----------------------------------------------------------------------        
c INPUT:
c    xlat      geodatic latitude in degrees
c    xlong     geodatic longitude in degrees
c    year      decimal year (year+month/12.0-0.5 or 
c                  year+day-of-year/365 or ../366 if leap year) 
c    height    height in km
c OUTPUT:
c    xl        L value
c    icode      =1  L is correct; =2  L is not correct;
c               =3  an approximation is used
c    dipl      dip latitude in degrees
c    babs     magnetic field strength in gauss
c-----------------------------------------------------------------------        

      REAL              xlat,xlong,xl,dipl,babs,year,height
      INTEGER           icode
      
      DOUBLE PRECISION  TM,GDLAT,GLON,GDALT,GCLAT,RKM,T,CT,ST,P,CP,SP,
     *                  BR,BT,BP,B,XB,YB,ZB,GCALT,BB,RL,
     *                  BDOWN,BNORTH,BEAST
C     .. External Subroutines ..
      EXTERNAL CONVRT,MILMAG,INVAR
      
C     .. Data statements ..
      DATA DTR/0.0174532925199D0/
      
        
C     convert to doubles 
      TM = DBLE(year)
      GDLAT = DBLE(xlat)
      GLON = DBLE(xlong)
      GDALT = DBLE(height)
      CALL CONVRT(1,GDLAT,GDALT,GCLAT,RKM)
      
C     .....calculate magnetic field at observation point.....
      T = DTR*(90.D0-GCLAT)
      CT = DCOS(T)
      ST = DSIN(T)
      P = DTR*GLON
      CP = DCOS(P)
      SP = DSIN(P)
      CALL MILMAG(TM,RKM,ST,CT,SP,CP,BR,BT,BP,B)
      CALL GDV(GDLAT,GCLAT,BR,BT,BP,XB,YB,ZB)
      BNORTH = XB
      BEAST = YB
      BDOWN = ZB
C
C     .....calculate dip latitude.....
      DIPL=REAL(ATAN(BDOWN/2.0/sqrt(BNORTH*BNORTH+BEAST*BEAST))/DTR)
      
C     .....calculate l-shell parameter.....
      GCALT = RKM - 6378.16D0/DSQRT(1.D0+.0067397D0*ST*ST)
      CALL INVAR(TM,GCLAT,GLON,GCALT,0.01D0,BB,RL)    
      RL = MAX(RL,1.0D0)      
  
C     convert results
      xl=REAL(RL)
      icode = 1
      babs=REAL(B)
      RETURN
      END
C
C

C
      SUBROUTINE GEODIP(IYR,SLA,SLO,DLA,DLO,J)

C  Calculates dipole geomagnetic coordinates from geocentric coordinates
C  or vice versa.

C                     J=0           J=1
C		INPUT:     J,SLA,SLO     J,DLA,DLO
C		OUTPUT:     DLA,DLO       SLA,SLO

C  Last revision: November 2005 (Vladimir Papitashvili)
C  The code is modifed from GEOCOR written by V.Popov and V.Papitashvili
C  in mid-1980s. 
      REAL SLA,SLO,DLA,DLO
      INTEGER IYR,J
      DOUBLE PRECISION R,COL,RLO,X,Y,Z,RM,TH,PF,XM,YM,ZM
      DOUBLE PRECISION SZM,DCO

C     .. Data statements ..
      DATA UMR /0.0174532925199D0/ 

C  Earth's radius (km) RE = 6371.2

C  The radius of the sphere to compute the coordinates (in Re)
C        RH = (RE + HI)/RE
         R = 1.

         if(j.gt.0) goto 1234
        
         COL = DBLE((90.- SLA)*UMR)
         RLO = DBLE(SLO*UMR)
      CALL SPHCAR(R,COL,RLO,X,Y,Z,1)
      CALL GEOMAG(X,Y,Z,XM,YM,ZM,1,IYR)
      CALL SPHCAR(RM,TH,PF,XM,YM,ZM,-1)
         SZM = ZM
         DLO = REAL(PF/UMR)
         DCO = TH/UMR
         DLA = REAL(90.- DCO)
         RETURN
          
1234     continue
      COL = DBLE((90.- DLA)*UMR)
      RLO = DBLE(DLO*UMR)
      CALL SPHCAR(R,COL,RLO,XM,YM,ZM,1)
      CALL GEOMAG(X,Y,Z,XM,YM,ZM,-1,IYR)
      CALL SPHCAR(RM,TH,PF,X,Y,Z,-1)
        SZM = ZM
        SLO = REAL(PF/UMR)
        SCO = TH/UMR
        SLA = REAL(90.- SCO)

      RETURN
      END
c
c
C
      subroutine igrf_dip(xlat,xlong,year,height,dip,dipl,ymodip)
c-----------------------------------------------------------------------        
c INPUT:
c    xlat      geodatic latitude in degrees
c    xlong     geodatic longitude in degrees
c    year      decimal year (year+month/12.0-0.5 or 
c                  year+day-of-year/365 or ../366 if leap year) 
c    height    height in km
c OUTPUT:
c    dip       magnetic inclination (dip) in degrees
c    dipl      dip latitude in degrees
c    ymodip    modified dip latitude = asin{dip/sqrt[dip^2+cos(LATI)]} 
c-----------------------------------------------------------------------        
      REAL              xlat,xlong,dipl,year,height
      
      DOUBLE PRECISION  TM,GDLAT,GLON,GDALT,GCLAT,RKM,T,CT,ST,P,CP,SP,
     *                  BR,BT,BP,B,XB,YB,ZB,HB,AINC,
     *                  dipdiv,SMODIP,BDOWN,BNORTH,BEAST
C     .. External Subroutines ..
      EXTERNAL CONVRT,MILMAG,INVAR
      
C     .. Data statements ..
      DATA DTR/0.0174532925199D0/
      
        
C     convert to doubles to use coord 
      TM = DBLE(year)
      GDLAT = DBLE(xlat)
      GLON = DBLE(xlong)
      GDALT = DBLE(height)
      CALL CONVRT(1,GDLAT,GDALT,GCLAT,RKM)
      
C     .....calculate magnetic field at observation point.....
      T = DTR*(90.D0-GCLAT)
      CT = DCOS(T)
      ST = DSIN(T)
      P = DTR*GLON
      CP = DCOS(P)
      SP = DSIN(P)
      CALL MILMAG(TM,RKM,ST,CT,SP,CP,BR,BT,BP,B)
      CALL GDV(GDLAT,GCLAT,BR,BT,BP,XB,YB,ZB)
      BNORTH = XB
      BEAST = YB
      BDOWN = ZB      
C
C     .....calculate inclination.....
      HB = DSQRT(XB*XB+YB*YB)
      AINC = DATAN2(ZB,HB) 
      
      dipdiv=AINC/SQRT(AINC*AINC+cos(XLAT*DTR))
      IF(ABS(dipdiv).GT.1.D0) dipdiv=SIGN(1.D0,dipdiv)
      SMODIP=ASIN(dipdiv)

      DIPL=REAL(ATAN(BDOWN/2.0/sqrt(BNORTH*BNORTH+BEAST*BEAST))/DTR)
      YMODIP=REAL(SMODIP/DTR)                            
      DIP=REAL(AINC/DTR)
      RETURN
      END
