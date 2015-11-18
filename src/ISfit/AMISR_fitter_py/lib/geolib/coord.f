C     $Id: coord.f,v 1.10 2010/02/10 20:37:18 brideout Exp $
C
      SUBROUTINE COORD(SLATGD,SLON,SR,SLATGC,TM,AZ,EL,RANGE,GDLAT,GLON,
     *                 GDALT,RCOR)
C
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
C        RCOR(7)  - b     - magnitude of geomagnetic field (gauss)
C        RCOR(8)  - br    - radial component of geomagnetic field (gauss)
C        RCOR(9)  - bt    - southward component of geomagnetic field (gauss)
C        RCOR(10) - bp    - eastward component of geomagnetic field (gauss)
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
C     .. Scalar Arguments ..
      DOUBLE PRECISION AZ,EL,GDALT,GDLAT,GLON,RANGE,SLATGC,SLATGD,SLON,
     *                 SR,TM
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION RCOR(38)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION AINC,ALAT,ALON,ARAD,ARC,ASPCT,B,BB,BP,BR,BT,BX,
     *                 BY,BZ,CASPCT,COSCN1,COSCN2,COSCN3,COST1,COST2,
     *                 COST3,CP,CSP,CSR,CST,CT,CX,CY,CZ,D,DEC,DET,
     *                 DGCLAT,DGDALT,DGDLAT,DGLON,DRKM,DTR,ECN1M,ECN2M,
     *                 ECN3M,ECO1M,ECO2M,GCALT,GCLAT,HB,P,PFX,PFY,PFZ,
     *                 PLAT,PLON,PRKM,PX,PY,PZ,RFP,RFR,RFT,RFX,RFY,RFZ,
     *                 RKM,RL,RLATI,RLATM,RMAG,RP,RR,RT,SP,ST,T,TM1,TP,
     *                 TR,TT,X1,X2,X3,XB,XDUM,YB,ZB,GDLATS,
     *                 CGDLAT,CGDALT,CPLAT,CPLON,CPRKM,CARC,CARAD,
     *                 CALAT,CALON
      INTEGER I,IER,INIT,ISTOP,J,NPR
      LOGICAL QAPCAL
      DOUBLE PRECISION HI
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION BF(3),CTAB(6),DQDX(3,3),DXDQ(3,3),GCN(3,3),
     *                 GCO(3,3),GF(4),PF(3),Q(3),Q0(3),RF(3),SV(3),
     *                 TV(3),X(3),X0(3)
      INTEGER LW(3),MW(3)
C     ..
C     .. External Functions ..
      DOUBLE PRECISION SPROD,VMAG
      EXTERNAL SPROD,VMAG
C     ..
C     .. External Subroutines ..
      EXTERNAL CONVRT,CSCONV,GDV,GMET,INVAR,LINTRA,LOOK,MILMAG,MINV,
     *         MTRAN3,POINT,RPCART,VCTCNV
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,ACOS,ATAN,DATAN2,DCOS,DSIN,DSQRT
C     ..
C     .. Statement Functions ..
      DOUBLE PRECISION TAN
C     ..
C     .. Equivalences ..
      EQUIVALENCE (RFX,RF(1)),(RFY,RF(2)),(RFZ,RF(3))
      EQUIVALENCE (PFX,PF(1)),(PFY,PF(2)),(PFZ,PF(3))
      EQUIVALENCE (BX,BF(1)),(BY,BF(2)),(BZ,BF(3))
C     ..
C     .. Data statements ..
      DATA DTR/0.0174532925199D0/
C     ..
C     .. Constants
      PARAMETER(MISSING=1.0D-38)
C     err1 and err2 are error flags from geo-cgm.f
      PARAMETER(ERR1=999.99D0,ERR2=99.99D0)
C     ..
C     .. Statement Function definitions ..
      TAN(XDUM) = DSIN(XDUM)/(DCOS(XDUM)+1.D-38)
C     ..
C     initialize
      COST1=0.0D0
      COST2=0.0D0
      COST3=0.0D0
C     .....set apex coordinate computation flag.....
      QAPCAL = TM .GT. 0.D0
      TM1 = ABS(TM)
      IF (RANGE.LT.0.0D0) THEN
C
C        .....entry when gdlat,glon,gdalt are given.....
C     .....Calculation fails when gdlat is too close to 0 or 90.....
         GDLATS = GDLAT
         GDLAT = MIN(GDLAT,89.99)
         GDLAT = MAX(GDLAT,-89.99)
         IF (ABS(GDLAT) .LT. 0.01) THEN
             GDLAT=0.01
         END IF
         CALL CONVRT(1,GDLAT,GDALT,GCLAT,RKM)
         CALL LOOK(SR,SLATGC,SLON,RKM,GCLAT,GLON,AZ,EL,RANGE)
      ELSE
C
C        .....entry when az, el, range are given.....
         CALL POINT(SR,SLATGC,SLON,AZ,EL,RANGE,RKM,GCLAT,GLON)
         CALL CONVRT(2,GDLAT,GDALT,GCLAT,RKM)
         GDLATS = GDLAT
         GDLAT = MIN(GDLAT,89.99)
         GDLAT = MAX(GDLAT,-89.99)
         IF (ABS(GDLAT) .LT. 0.01) THEN
             GDLAT=0.01
         END IF
      END IF
C
C     .....calculate magnetic field at observation point.....
      T = DTR*(90.D0-GCLAT)
      CT = DCOS(T)
      ST = DSIN(T)
      P = DTR*GLON
      CP = DCOS(P)
      SP = DSIN(P)
      CALL MILMAG(TM1,RKM,ST,CT,SP,CP,BR,BT,BP,B)
      CALL GDV(GDLAT,GCLAT,BR,BT,BP,XB,YB,ZB)
C
C     .....calculate inclination.....
      HB = DSQRT(XB*XB+YB*YB)
      AINC = DATAN2(ZB,HB)/DTR
C
C     .....calculate declination.....
      DEC = DATAN2(YB,XB)/DTR
C
C     .....calculate l-shell parameter.....
      GCALT = RKM - 6378.16D0/DSQRT(1.D0+.0067397D0*ST*ST)
      CALL INVAR(TM1,GCLAT,GLON,GCALT,0.01D0,BB,RL)
      RL = MAX(RL,1.0D0)
C
C     .....calculate dip latitude.....
      RLATM = ATAN(.5D0*TAN(DTR*AINC))/DTR
C
C     .....calculate invariant latitude.....
      RLATI = ACOS(DSQRT(1.0D0/RL))/DTR
C
C     .....convert radar propagation vector and observation point
C          position to earth centered cartesian coordinates.....
      CALL RPCART(SR,SLATGC,SLON,AZ,EL,RANGE,RFX,RFY,RFZ,PFX,PFY,PFZ)
C
C     .....convert southward, eastward, upward components of magnetic
C          field at observation point to earth centered cartesian
C          coordinates.....
      CALL VCTCNV(BX,BY,BZ,PX,PY,PZ,BR,BT,BP,RKM,90.D0-GCLAT,GLON,2)
C
C     .....calculate aspect angle.....
      CASPCT = SPROD(RF,BF)/(VMAG(RF)*VMAG(BF))
      ASPCT = ACOS(CASPCT)/DTR
C
C     .....calculate direction cosines of radar beam with respect to
C          geocentric south, east, up.....
      CALL VCTCNV(RFX,RFY,RFZ,PFX,PFY,PFZ,RFR,RFT,RFP,RR,RT,RP,1)
      CST = RFT/RANGE
      CSP = RFP/RANGE
      CSR = RFR/RANGE
C
C     .....compute geodetic direction cosines. the direction cosines
C          are with respect to south, east, up rather than x (north),
C          y (east), z (down) as used in gdv.....
      CALL GDV(GDLAT,GCLAT,CSR,CST,CSP,CX,CY,CZ)

      CTAB(1) = -CX
      CTAB(2) = CY
      CTAB(3) = -CZ
C
      IF (QAPCAL) THEN
C        .....calculate observation point apex coordinates.....
         ISTOP = 0
         NPR = 0
         INIT = 0
         D = 1.0D-4
         CALL LINTRA(TM1,GCLAT,GLON,RKM,GDALT,0.0D0,PLAT,PLON,PRKM,ARC,
     *               ARAD,ALAT,ALON,ISTOP,NPR,INIT,IER)
         Q0(1) = DTR*(90.0D0-GCLAT)
         Q0(2) = DTR*GLON
         Q0(3) = RKM/6370.0D0
         Q0(1) = DTR*(90.0D0-ALAT)
         Q0(2) = DTR*ALON
C
C        .....calculate apex-cartesian direction cosines.....
         X0(1) = PX/6370.0D0
         X0(2) = PY/6370.0D0
         X0(3) = PZ/6370.0D0
         X(1) = X0(1)
         X(2) = X0(2)
         X(3) = X0(3)
         DO 20 J = 1,3
            X(J) = X(J) + D
            X1 = 6370.0D0*X(1)
            X2 = 6370.0D0*X(2)
            X3 = 6370.0D0*X(3)
            CALL CSCONV(X1,X2,X3,DRKM,DGCLAT,DGLON,1)
            IF (DGLON.LT.0.D0) DGLON = DGLON + 360.0D0
            DGCLAT = 90.0D0 - DGCLAT
            CALL CONVRT(2,DGDLAT,DGDALT,DGCLAT,DRKM)
            CALL LINTRA(TM1,DGCLAT,DGLON,DRKM,DGDALT,0.0D0,PLAT,PLON,
     *                  PRKM,ARC,ARAD,ALAT,ALON,ISTOP,NPR,INIT,IER)
            Q(1) = DTR*(90.0D0-DGCLAT)
            Q(2) = DTR*DGLON
            Q(3) = DRKM/6370.0D0
            Q(1) = DTR*(90.0D0-ALAT)
            Q(2) = DTR*ALON
            X(J) = X0(J)
            DO 10 I = 1,3
               DQDX(I,J) = (Q(I)-Q0(I))/D
   10       CONTINUE
   20    CONTINUE
         DQDX(3,1) = -BX
         DQDX(3,2) = -BY
         DQDX(3,3) = -BZ
         DO 40 I = 1,3
            DO 30 J = 1,3
               DXDQ(I,J) = DQDX(I,J)
   30       CONTINUE
   40    CONTINUE
         CALL MINV(DXDQ,3,DET,LW,MW)
C
C        .....calculate apex metric tensor.....
         CALL GMET(DXDQ,GCO)
C
C        .....calculate apex conjugate metric tensor.....
         DO 60 I = 1,3
            DO 50 J = 1,3
               GCN(I,J) = GCO(I,J)
   50       CONTINUE
   60    CONTINUE
         CALL MINV(GCN,3,DET,LW,MW)
         ECO1M = VMAG(DXDQ(1,1))
         ECO2M = VMAG(DXDQ(1,2))
C         ECO3M = VMAG(DXDQ(1,3))
         CALL MTRAN3(DQDX)
         COSCN1 = SPROD(DQDX(1,1),RF)
         COSCN2 = SPROD(DQDX(1,2),RF)
         COSCN3 = SPROD(DQDX(1,3),RF)
         ECN1M = VMAG(DQDX(1,1))
         ECN2M = VMAG(DQDX(1,2))
         ECN3M = VMAG(DQDX(1,3))
         RMAG = VMAG(RF)
         CTAB(4) = COSCN1/(ECN1M*RMAG)
         CTAB(5) = COSCN2/(ECN2M*RMAG)
         CTAB(6) = COSCN3/(ECN3M*RMAG)
         GF(1) = GCN(1,1)*ECO1M
         GF(2) = GCN(2,1)*ECO1M
         GF(3) = GCN(1,2)*ECO2M
         GF(4) = GCN(2,2)*ECO2M
C
C        .....calculate direction cosines of a vector perpendicular to
C             the line-of-sight with respect to the apex coordinates..
         TT = -DSIN(DTR*(180.D0-AZ))
         TP = DCOS(DTR*(180.D0-AZ))
         TR = 0.D0
         CALL VCTCNV(TV(1),TV(2),TV(3),SV(1),SV(2),SV(3),TR,TT,TP,SR,
     *               90.D0-SLATGC,SLON,2)
         COST1 = SPROD(DQDX(1,1),TV)/ECN1M
         COST2 = SPROD(DQDX(1,2),TV)/ECN2M
         COST3 = SPROD(DQDX(1,3),TV)/ECN3M
      END IF
C     Get conjugate point using lintra
      HI=DBLE(GDALT)
C     ... tell lintra to find mag conj, not apex...
      ISTOP = 1
      CALL LINTRA(TM,GCLAT,GLON,RKM,HI,GDALT,CPLAT,CPLON,CPRKM,CARC,
     *            CARAD,CALAT,CALON,ISTOP,NPR,INIT,IER)
C     get geodetic latitude of conjugate point
      CALL CONVRT(2,CGDLAT,CGDALT,CPLAT,CPRKM)
      IF (CPLON .LT. 0.0D0) THEN
          CPLON = CPLON + 360.0D0
      END IF
C
C     .....fill rcor with calculated coordinates.....
      GDLAT = GDLATS
      RCOR(1) = AZ
      RCOR(2) = EL
      RCOR(3) = RANGE
      RCOR(4) = GDLAT
      RCOR(5) = GLON
      RCOR(6) = GDALT
      RCOR(7) = B
      RCOR(8) = BR
      RCOR(9) = BT
      RCOR(10) = BP
      RCOR(11) = RLATM
      RCOR(12) = RLATI
      RCOR(13) = RL
      RCOR(14) = ALAT
      RCOR(15) = ALON
      RCOR(16) = GF(1)
      RCOR(17) = GF(2)
      RCOR(18) = GF(3)
      RCOR(19) = GF(4)
      RCOR(20) = CTAB(1)
      RCOR(21) = CTAB(2)
      RCOR(22) = CTAB(3)
      RCOR(23) = CTAB(4)
      RCOR(24) = CTAB(5)
      RCOR(25) = CTAB(6)
      RCOR(26) = COST1
      RCOR(27) = COST2
      RCOR(28) = COST3
      RCOR(29) = AINC
      RCOR(30) = DEC
      RCOR(31) = GCLAT
      RCOR(32) = ASPCT
      RCOR(33) = CPLAT
      RCOR(34) = CGDLAT
      RCOR(35) = CPLON

      RETURN
C
      END
