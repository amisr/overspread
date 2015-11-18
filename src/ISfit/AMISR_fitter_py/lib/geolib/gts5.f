C     $Id: gts5.f,v 1.3 2003/01/15 13:36:30 brideout Exp $
C    ******************************************************************
      SUBROUTINE GTS5(IYD,SEC,ALT,GLAT,GLONG,STL,F107A,F107,AP,MASS,D,T)
C        MSIS-86/CIRA 1986 Neutral Thermosphere Model
C         A.E.Hedin 3/15/85;2/26/87 (Variable Names Shortened)
C         9/21/87 M.E. Hagan (Non-Standard Statements Changed)
C     INPUT:
C        IYD - YEAR AND DAY AS YYDDD
C        SEC - UT(SEC)
C        ALT - ALTITUDE(KM) (GREATER THAN 85 KM)
C        GLAT - GEODETIC LATITUDE(DEG)
C        GLONG - GEODETIC LONGITUDE(DEG)
C        STL - LOCAL APPARENT SOLAR TIME(HRS)
C        F107A - 3 MONTH AVERAGE OF F10.7 FLUX
C        F107 - DAILY F10.7 FLUX FOR PREVIOUS DAY
C             UNITS:1.0e-22W/m2/Hz
C        AP - MAGNETIC INDEX(DAILY) OR WHEN SW(9)=-1. :
C           - ARRAY CONTAINING:
C             (1) DAILY AP
C             (2) 3 HR AP INDEX FOR CURRENT TIME
C             (3) 3 HR AP INDEX FOR 3 HRS BEFORE CURRENT TIME
C             (4) 3 HR AP INDEX FOR 6 HRS BEFORE CURRENT TIME
C             (5) 3 HR AP INDEX FOR 9 HRS BEFORE CURRENT TIME
C             (6) AVERAGE OF EIGHT 3 HR AP INDICIES FROM 12 TO 33 HRS
C                    PRIOR TO CURRENT TIME
C             (7) AVERAGE OF EIGHT 3 HR AP INDICIES FROM 36 TO 59 HRS
C                    PRIOR TO CURRENT TIME
C        MASS - MASS NUMBER (ONLY DENSITY FOR SELECTED GAS IS
C                 CALCULATED. MASS 0 IS TEMPERATURE. MASS 48 FOR ALL.
C
C     OUTPUT:
C        D(1) - HE NUMBER DENSITY(CM-3)
C        D(2) - O NUMBER DENSITY(CM-3)
C        D(3) - N2 NUMBER DENSITY(CM-3)
C        D(4) - O2 NUMBER DENSITY(CM-3)
C        D(5) - AR NUMBER DENSITY(CM-3)
C        D(6) - TOTAL MASS DENSITY(GM/CM3)
C        D(7) - H NUMBER DENSITY(CM-3)
C        D(8) - N NUMBER DENSITY(CM-3)
C        T(1) - EXOSPHERIC TEMPERATURE
C        T(2) - TEMPERATURE AT ALT
C
C      TO GET OUTPUT IN M-3 and KG/M3:   CALL METERS(.TRUE.)
C
C          ADDITIONAL COMMENTS
C           (1) LOWER BOUND QUANTITIES IN COMMON/GTS3C/
C           (2) TO TURN ON AND OFF PARTICULAR VARIATIONS CALL TSELEC(SW)
C               WHERE SW IS A 25 ELEMENT ARRAY CONTAINING 0. FOR OFF, 1.
C               FOR ON, OR 2. FOR MAIN EFFECTS OFF BUT CROSS TERMS ON
C               FOR THE FOLLOWING VARIATIONS
C               1 - F10.7 EFFECT ON MEAN  2 - TIME INDEPENDENT
C               3 - SYMMETRICAL ANNUAL    4 - SYMMETRICAL SEMIANNUAL
C               5 - ASYMMETRICAL ANNUAL   6 - ASYMMETRICAL SEMIANNUAL
C               7 - DIURNAL               8 - SEMIDIURNAL
C               9 - DAILY AP             10 - ALL UT/LONG EFFECTS
C              11 - LONGITUDINAL         12 - UT AND MIXED UT/LONG
C              13 - MIXED AP/UT/LONG     14 - TERDIURNAL
C              15 - DEPARTURES FROM DIFFUSIVE EQUILIBRIUM
C              16 - ALL TINF VAR         17 - ALL TLB VAR
C              18 - ALL T0 VAR           19 - ALL S VAR
C              20 - ALL Z0 VAR           21 - ALL NLB VAR
C              22 - ALL TR12 VAR         23 - TURBO SCALE HEIGHT VAR
C
C              To get current values of SW: CALL TRETRV(SW)
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION ALT,F107,F107A,GLAT,GLONG,SEC,STL
      INTEGER IYD,MASS
      LOGICAL METER
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION AP(*),D(8),T(2)
C     ..
C     .. Scalars in Common ..
C     ..
C     .. Arrays in Common ..
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION B01,B04,B14,B16,B28,B32,B40,DDUM,DM01,DM04,DM14,
     *                 DM16,DM28,DM32,DM40,G1,G14,G16,G28,G32,G4,G40,
     *                 HC01,HC04,HC14,HC16,HC32,HC40,HCC01,HCC14,HCC16,
     *                 RC01,RC14,RC16,TINF,TR12,TZ,XMD,XMM,YRD,ZC01,
     *                 ZC04,ZC14,ZC16,ZC32,ZC40,ZCC01,ZCC14,ZCC16,ZH01,
     *                 ZH04,ZH14,ZH16,ZH28,ZH32,ZH40,ZHM01,ZHM04,ZHM14,
     *                 ZHM16,ZHM28,ZHM32,ZHM40
      INTEGER I,IFL,IMR,J
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION ALTL(8)
      INTEGER IC86(2),MT(10)
C     ..
C     .. External Functions ..
      DOUBLE PRECISION CCOR,DENSS,DNET,GLOB5L,GLOBE5
      EXTERNAL CCOR,DENSS,DNET,GLOB5L,GLOBE5
C     ..
C     .. External Subroutines ..
      EXTERNAL PRMSG5
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DABS,DEXP,DLOG
C     ..
C     .. Common blocks ..
      COMMON /CSW/SW,SWC,ISW
      COMMON /DATIME/ISDATE,ISTIME,NAME
      COMMON /GTS3C/TLB,S,DB04,DB16,DB28,DB32,DB40,DB48,DB01,ZA,T0,Z0,
     *       G0,RL,DD,DB14
      COMMON /LOWER5/PTM,PDM
      COMMON /PARM5/PT,PD,PS,PDL
      DOUBLE PRECISION DB01,DB04,DB14,DB16,DB28,DB32,DB40,DB48,DD,G0,RL,
     *                 S,T0,TLB,Z0,ZA
      INTEGER ISW
      DOUBLE PRECISION PD(150,7),PDL(25,2),PDM(8,7),PS(150),PT(150),
     *                 PTM(8),SW(25),SWC(25)
      INTEGER ISDATE(3),ISTIME(2),NAME(2)
C     ..
C     .. Data statements ..
      DATA MT/48,0,4,16,28,32,40,1,49,14/,IFL/0/
      DATA ALTL/200.D0,300.D0,150.D0,200.D0,240.D0,450.D0,320.D0,450.D0/
      DATA IMR/0/,IC86/4HCIRA,4H-86 /
      DATA B01/0.0D0/,B04/0.0D0/,B14/0.0D0/,B16/0.0D0/,B28/0.0D0/
      DATA B32/0.0D0/,B40/0.0D0/
      DATA DDUM/0.0D0/
C     ..
      IF (IFL.EQ.1 .AND. (NAME(1).EQ.IC86(1)) .AND.
     *    (NAME(2).EQ.IC86(2))) GO TO 10
      CALL PRMSG5
      IFL = 1
   10 CONTINUE
      YRD = IYD
C       Eq. A7
      TINF = PTM(1)*(1.D0+SW(16)*GLOBE5(YRD,SEC,GLAT,GLONG,STL,F107A,
     *       F107,AP,PT))*PT(1)
      ZA = PTM(5)*PDL(16,2)
C       Eq. A9
      T0 = PTM(3)*PD(76,3)*(1.D0+SW(18)*GLOB5L(PD(76,3)))
C       Eq. A8
      TLB = PTM(2)*(1.D0+SW(17)*GLOB5L(PD(26,3)))*PD(26,3)
C       Eq. A10
      Z0 = PTM(7)*(1.D0+SW(20)*GLOB5L(PD(51,3)))*PD(51,3)
C       Eq. A6
      G0 = PTM(4)*PS(1)*(1.D0+SW(19)*GLOBE5(YRD,SEC,GLAT,GLONG,STL,
     *     F107A,F107,AP,PS))
C       Eq. A5
      S = G0/(TINF-TLB)
C       Eq. A11
      TR12 = PD(101,3)*(1.D0+SW(22)*GLOB5L(PD(101,3)))
      T(1) = TINF
      IF (MASS.EQ.0) GO TO 170
C       Eq. A18  N2
      G28 = SW(21)*GLOB5L(PD(1,3))
      YRD = IYD
      T(1) = TINF
      XMM = PDM(5,3)
      DO 20 J = 1,10
         IF (MASS.EQ.MT(J)) GO TO 30
   20 CONTINUE
      GO TO 180
   30 IF (ALT.GT.ALTL(6) .AND. MASS.NE.28 .AND. MASS.NE.48) GO TO 40
C
C       **** N2 DENSITY ****
C
C       Eq. A18
      DB28 = PDM(1,3)*DEXP(G28)*PD(1,3)
C       Eq. A13 - A17
      D(3) = DENSS(ALT,DB28,TINF,TLB,28.D0,0.D0,T(2),PTM(6),S,T0,ZA,Z0,
     *       TR12)
      DD = D(3)
C       Eq. A19
      ZH28 = PDM(3,3)
      ZHM28 = PDM(4,3)*PDL(6,2)
      XMD = 28.D0 - XMM
      B28 = DENSS(ZH28,DB28,TINF,TLB,XMD,-1.D0,TZ,PTM(6),S,T0,ZA,Z0,
     *      TR12)
      IF (ALT.GT.ALTL(3) .OR. SW(15).EQ.0.D0) GO TO 40
      DM28 = DENSS(ALT,B28,TINF,TLB,XMM,0.D0,TZ,PTM(6),S,T0,ZA,Z0,TR12)
C       Eq. A12
      D(3) = DNET(D(3),DM28,ZHM28,XMM,28.D0)
   40 CONTINUE
      GO TO (50,170,50,70,180,90,110,130,70,
     *       150) J
   50 CONTINUE
C
C       **** HE DENSITY ****
C
C       Eq. A18
      G4 = SW(21)*GLOBE5(YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP,PD(1,1))
      DB04 = PDM(1,1)*DEXP(G4)*PD(1,1)
C       Eq. A13 - A17
      D(1) = DENSS(ALT,DB04,TINF,TLB,4.D0,-.4D0,T(2),PTM(6),S,T0,ZA,Z0,
     *       TR12)
      DD = D(1)
      IF (ALT.GT.ALTL(1) .OR. SW(15).EQ.0.D0) GO TO 60
C       Eq. A19
      ZH04 = PDM(3,1)
      B04 = DENSS(ZH04,DB04,TINF,TLB,4.D0-XMM,-1.4D0,T(2),PTM(6),S,T0,
     *      ZA,Z0,TR12)
      DM04 = DENSS(ALT,B04,TINF,TLB,XMM,0.D0,T(2),PTM(6),S,T0,ZA,Z0,
     *       TR12)
C       Eq. A12
      ZHM04 = ZHM28
      D(1) = DNET(D(1),DM04,ZHM04,XMM,4.D0)
C       Eq. A20b
      RL = DLOG(B28*PDM(2,1)/B04)
C       Eq. A20a
      ZC04 = PDM(5,1)*PDL(1,2)
      HC04 = PDM(6,1)*PDL(2,2)
      D(1) = D(1)*CCOR(ALT,RL,HC04,ZC04)
   60 CONTINUE
      IF (MASS.NE.48) GO TO 180
   70 CONTINUE
C
C      **** O DENSITY ****
C
C       Eq. A18
      G16 = SW(21)*GLOBE5(YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP,PD(1,2))
      DB16 = PDM(1,2)*DEXP(G16)*PD(1,2)
C       Eq. A13 - A17
      D(2) = DENSS(ALT,DB16,TINF,TLB,16.D0,0.D0,T(2),PTM(6),S,T0,ZA,Z0,
     *       TR12)
      DD = D(2)
      IF (ALT.GT.ALTL(2) .OR. SW(15).EQ.0.D0) GO TO 80
C     Corrected from PDM(3,1) to PDM(3,2)  12/2/85
C       Eq. A19
      ZH16 = PDM(3,2)
      B16 = DENSS(ZH16,DB16,TINF,TLB,16-XMM,-1.D0,T(2),PTM(6),S,T0,ZA,
     *      Z0,TR12)
      DM16 = DENSS(ALT,B16,TINF,TLB,XMM,0.D0,T(2),PTM(6),S,T0,ZA,Z0,
     *       TR12)
C       Eq. A12
      ZHM16 = ZHM28
      D(2) = DNET(D(2),DM16,ZHM16,XMM,16.D0)
C       Eq. A20b
      RL = DLOG(B28*PDM(2,2)*DABS(PDL(17,2))/B16)
C       Eq. A20a
      HC16 = PDM(6,2)*PDL(4,2)
      ZC16 = PDM(5,2)*PDL(3,2)
      D(2) = D(2)*CCOR(ALT,RL,HC16,ZC16)
C       Eq. A21
      HCC16 = PDM(8,2)*PDL(14,2)
      ZCC16 = PDM(7,2)*PDL(13,2)
      RC16 = PDM(4,2)*PDL(15,2)
      D(2) = D(2)*CCOR(ALT,RC16,HCC16,ZCC16)
   80 CONTINUE
      IF (MASS.NE.48 .AND. MASS.NE.49) GO TO 180
   90 CONTINUE
C
C       **** O2 DENSITY ****
C
C       Eq. A18
      G32 = SW(21)*GLOBE5(YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP,PD(1,4))
      DB32 = PDM(1,4)*DEXP(G32)*PD(1,4)
C       Eq. A13 - A17
      D(4) = DENSS(ALT,DB32,TINF,TLB,32.D0,0.D0,T(2),PTM(6),S,T0,ZA,Z0,
     *       TR12)
      IF (MASS.EQ.49) THEN
         DD = DD + 2.D0*D(4)
      ELSE
         DD = D(4)
      END IF
      IF (ALT.GT.ALTL(4) .OR. SW(15).EQ.0.D0) GO TO 100
C       Eq. A19
      ZH32 = PDM(3,4)
      B32 = DENSS(ZH32,DB32,TINF,TLB,32.D0-XMM,-1.D0,T(2),PTM(6),S,T0,
     *      ZA,Z0,TR12)
      DM32 = DENSS(ALT,B32,TINF,TLB,XMM,0.D0,T(2),PTM(6),S,T0,ZA,Z0,
     *       TR12)
C       Eq. A12
      ZHM32 = ZHM28
      D(4) = DNET(D(4),DM32,ZHM32,XMM,32.D0)
C       Eq. A20b
      RL = DLOG(B28*PDM(2,4)/B32)
C       Eq. A20a
      HC32 = PDM(6,4)*PDL(8,2)
      ZC32 = PDM(5,4)*PDL(7,2)
      D(4) = D(4)*CCOR(ALT,RL,HC32,ZC32)
  100 CONTINUE
      IF (MASS.NE.48) GO TO 180
  110 CONTINUE
C
C       **** AR DENSITY ****
C
C       Eq. A18
      G40 = SW(21)*GLOBE5(YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP,PD(1,5))
      DB40 = PDM(1,5)*DEXP(G40)*PD(1,5)
C       Eq. A13 - A17
      D(5) = DENSS(ALT,DB40,TINF,TLB,40.D0,0.D0,T(2),PTM(6),S,T0,ZA,Z0,
     *       TR12)
      DD = D(5)
      IF (ALT.GT.ALTL(5) .OR. SW(15).EQ.0.D0) GO TO 120
C       Eq. A19
      ZH40 = PDM(3,5)
      B40 = DENSS(ZH40,DB40,TINF,TLB,40.D0-XMM,-1.D0,T(2),PTM(6),S,T0,
     *      ZA,Z0,TR12)
      DM40 = DENSS(ALT,B40,TINF,TLB,XMM,0.D0,T(2),PTM(6),S,T0,ZA,Z0,
     *       TR12)
C       Eq. A12
      ZHM40 = ZHM28
      D(5) = DNET(D(5),DM40,ZHM40,XMM,40.D0)
C       Eq. A20b
      RL = DLOG(B28*PDM(2,5)/B40)
C       Eq. A20a
      HC40 = PDM(6,5)*PDL(10,2)
      ZC40 = PDM(5,5)*PDL(9,2)
      D(5) = D(5)*CCOR(ALT,RL,HC40,ZC40)
  120 CONTINUE
      IF (MASS.NE.48) GO TO 180
  130 CONTINUE
C
C        **** HYDROGEN DENSITY ****
C
C       Eq. A18
      G1 = SW(21)*GLOBE5(YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP,PD(1,6))
      DB01 = PDM(1,6)*DEXP(G1)*PD(1,6)
C       Eq. A13 - A17
      D(7) = DENSS(ALT,DB01,TINF,TLB,1.D0,-.4D0,T(2),PTM(6),S,T0,ZA,Z0,
     *       TR12)
      DD = D(7)
      IF (ALT.GT.ALTL(7) .OR. SW(15).EQ.0.D0) GO TO 140
C       Eq. A19
      ZH01 = PDM(3,6)
      B01 = DENSS(ZH01,DB01,TINF,TLB,1.D0-XMM,-1.4D0,T(2),PTM(6),S,T0,
     *      ZA,Z0,TR12)
      DM01 = DENSS(ALT,B01,TINF,TLB,XMM,0.D0,T(2),PTM(6),S,T0,ZA,Z0,
     *       TR12)
C       Eq. A12
      ZHM01 = ZHM28
      D(7) = DNET(D(7),DM01,ZHM01,XMM,1.D0)
C       Eq. A20b
      RL = DLOG(B28*PDM(2,6)*DABS(PDL(18,2))/B01)
C       Eq. A20a
      HC01 = PDM(6,6)*PDL(12,2)
      ZC01 = PDM(5,6)*PDL(11,2)
      D(7) = D(7)*CCOR(ALT,RL,HC01,ZC01)
C       Eq. A21
      HCC01 = PDM(8,6)*PDL(20,2)
      ZCC01 = PDM(7,6)*PDL(19,2)
      RC01 = PDM(4,6)*PDL(21,2)
      D(7) = D(7)*CCOR(ALT,RC01,HCC01,ZCC01)
  140 CONTINUE
  150 CONTINUE
C
C        **** ATOMIC NITROGEN DENSITY ****
C
C       Eq. A18
      G14 = SW(21)*GLOBE5(YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP,PD(1,7))
      DB14 = PDM(1,7)*DEXP(G14)*PD(1,7)
C       Eq. A13 - A17
      D(8) = DENSS(ALT,DB14,TINF,TLB,14.D0,0.D0,T(2),PTM(6),S,T0,ZA,Z0,
     *       TR12)
      DD = D(8)
      IF (ALT.GT.ALTL(8) .OR. SW(15).EQ.0.D0) GO TO 160
C       Eq. A19
      ZH14 = PDM(3,7)
      B14 = DENSS(ZH14,DB14,TINF,TLB,14.D0-XMM,-1.D0,T(2),PTM(6),S,T0,
     *      ZA,Z0,TR12)
      DM14 = DENSS(ALT,B14,TINF,TLB,XMM,0.D0,T(2),PTM(6),S,T0,ZA,Z0,
     *       TR12)
C       Eq. A12
      ZHM14 = ZHM28
      D(8) = DNET(D(8),DM14,ZHM14,XMM,14.D0)
C       Eq. A20b
      RL = DLOG(B28*PDM(2,7)*DABS(PDL(3,1))/B14)
C       Eq. A20a
      HC14 = PDM(6,7)*PDL(2,1)
      ZC14 = PDM(5,7)*PDL(1,1)
      D(8) = D(8)*CCOR(ALT,RL,HC14,ZC14)
C       Eq. A21
      HCC14 = PDM(8,7)*PDL(5,1)
      ZCC14 = PDM(7,7)*PDL(4,1)
      RC14 = PDM(4,7)*PDL(6,1)
      D(8) = D(8)*CCOR(ALT,RC14,HCC14,ZCC14)
  160 CONTINUE
      IF (MASS.NE.48) GO TO 180
C
C       TOTAL MASS DENSITY
C
      D(6) = 1.66D-24*(4.D0*D(1)+16.D0*D(2)+28.D0*D(3)+32.D0*D(4)+
     *       40.D0*D(5)+D(7)+14.D0*D(8))
      DB48 = 1.66D-24*(4.D0*DB04+16.D0*DB16+28.D0*DB28+32.D0*DB32+
     *       40.D0*DB40+DB01+14.D0*DB14)
      GO TO 180
  170 DDUM = DENSS(ALT,1.D0,TINF,TLB,0.D0,0.D0,T(2),PTM(6),S,T0,ZA,Z0,
     *       TR12)
      GO TO 180
  180 CONTINUE
      IF (IMR.EQ.1) THEN
         DO 190 I = 1,8
            D(I) = D(I)*1.D6
  190    CONTINUE
         D(6) = D(6)/1000.D0
      END IF
      RETURN
      ENTRY METERS(METER)
      IMR = 0
      IF (METER) IMR = 1
      END
C     *PL*ERROR* Can't indent comment line
C*PL*ERROR* Can't indent comment line
C   --------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION DENSS(ALT,DLB,TINF,TLB,XM,ALPHA,TZ,ZLB,
     *                                S2,T0,ZA,Z0,TR12)
C       Calculate Temperature and Density Profiles for MSIS models
C     .. Scalar Arguments ..
      DOUBLE PRECISION ALPHA,ALT,DLB,S2,T0,TINF,TLB,TR12,TZ,XM,Z0,ZA,ZLB
C     ..
C     .. Scalars in Common ..
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION BB,CC,DD,DENSA,DTA,GAMM,GAMMA,GLB,RGAS,T12,TA,TT,
     *                 X,X2,Z,ZG0,ZG1,ZG2,ZL,ZZ
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DEXP,DMAX1
C     ..
C     .. Common blocks ..
      COMMON /FIT/TAF
      COMMON /PARMB/GSURF,RE
      DOUBLE PRECISION GSURF,RE,TAF
C     ..
C     .. Statement Functions ..
      DOUBLE PRECISION ZETA
C     ..
C     .. Data statements ..
      DATA RGAS/831.4D0/
C     ..
C     .. Statement Function definitions ..
      ZETA(ZZ,ZL) = (ZZ-ZL)*(RE+ZL)/(RE+ZZ)
C     ..
      BB = 0.0D0
      CC = 0.0D0
      DD = 0.0D0
      DENSA = 0.0D0
      DTA = 0.0D0
      GAMM = 0.0D0
      GAMMA = 0.0D0
      GLB = 0.0D0
      T12 = 0.0D0
      TA = 0.0D0
      TT = 0.0D0
      X = 0.0D0
      X2 = 0.0D0
      Z = 0.0D0
      ZG0 = 0.0D0
      ZG1 = 0.0D0
      ZG2 = 0.0D0
      ZL = 0.0D0
      ZZ = 0.0D0
C     ..
      DENSS = 1.D0
      Z = DMAX1(ALT,ZA)
C      Eq. A4a
      ZG2 = ZETA(Z,ZLB)
C      Eq. A1a
      TT = TINF - (TINF-TLB)*DEXP(-S2*ZG2)
      TA = TT
      TZ = TT
      DENSS = TZ
      IF (ALT.GE.ZA) GO TO 10
C      Eq. A4b
      ZG0 = ZETA(Z0,ZA)
C      Eq. A2b
      DTA = (TINF-TA)*S2*((RE+ZLB)/(RE+ZA))**2
C      Eq. A3e
      T12 = T0 + TR12*(TA-T0)
C      Eq. A4b
      ZG1 = ZETA(ALT,ZA)
C       CALCULATE TEMPERATURE BELOW ZA
C      Eq. A3a
      DD = 0.666666D0*ZG0*DTA/TA**2 - 3.11111D0*(1.D0/TA-1.D0/T0) +
     *     7.11111D0*(1.D0/T12-1.D0/T0)
C      Eq. A3b
      CC = ZG0*DTA/(2.D0*TA*TA) - (1.D0/TA-1.D0/T0) - 2.D0*DD
C      Eq. A3c
      BB = (1.D0/TA-1.D0/T0) - CC - DD
C      Eq. A3d
      X = (-(ZG1-ZG0)/ZG0)
C      Eq. A1b
      X2 = X*X
      TZ = 1.D0/(1.D0/T0+BB*X2+CC*X2*X2+DD*X2*X2*X2)
      DENSS = TZ
      TAF = (T12-T0)/(TA-T0)
   10 IF (XM.EQ.0.D0) GO TO 30
      IF (TA.GT.0.D0 .AND. TZ.GT.0.D0) GO TO 20
      TT = TLB
      TA = TLB
      TZ = TLB
   20 CONTINUE
C      CALCULATE DENSITY ABOVE ZA
C      Eq. A17a
      GLB = GSURF/(1.D0+ZLB/RE)**2
C      Eq. A16a
      GAMMA = XM*GLB/(S2*RGAS*TINF)
C      Eq. A13, A14a, & A15
      DENSA = DLB*(TLB/TT)**(1.D0+ALPHA+GAMMA)*DEXP(-S2*GAMMA*ZG2)
      DENSS = DENSA
      IF (ALT.GE.ZA) GO TO 30
C      CALCULATE DENSITY BELOW ZA
C      Eq. A17b
      GLB = GSURF/(1.D0+ZA/RE)**2
C      Eq. A16b
      GAMM = XM*GLB*ZG0/RGAS
C      Eq. A13, A14b, & A15
      DENSS = DENSA*(TA/TZ)**(1.D0+ALPHA)*
     *        DEXP(GAMM*((X-1)/T0+BB*(X*X2-1.D0)/3.D0+CC*(X2*X2*X-
     *        1.D0)/5.D0+DD*(X2*X2*X2*X-1.D0)/7.D0))
   30 CONTINUE
C     *PL*ERROR* Can't indent comment line
C*PL*ERROR* Can't indent comment line
C    CCCCCWRITE(6,100)CXM,ALT,ZA,TINF,TLB,S2,T0,S1,TA,TZ,DLB,DENSA,DENSS
C     C100 FORMAT(' D',1P13E10.2)
      RETURN
      END
C     *PL*ERROR* Can't indent comment line
C*PL*ERROR* Can't indent comment line
C   --------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION GLOBE5(YRD,SEC,LAT,LONG,TLOC,F107A,F107,
     *                 AP,P)
C       CALCULATE G(L) FUNCTION FOR MSIS-86/CIRA 1986
C       Upper Thermosphere Parameters
C     .. Scalar Arguments ..
      DOUBLE PRECISION F107,F107A,LAT,LONG,SEC,TLOC,YRD
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION AP(*),P(*)
C     ..
C     .. Scalars in Common ..
C     ..
C     .. Arrays in Common ..
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION A,C,C2,C2D14,C4,CD14,CD18,CD32,CD39,DAYL,DGTR,DR,
     *                 EX,EXP1,EXP2,F1,F2,HR,P14,P18,P32,P39,P44,P45,S,
     *                 S2,SR,T71,T72,T81,T82,TINF,TLL,XL
      INTEGER I,NSW
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION SV(25),T(15)
C     ..
C     .. External Subroutines ..
      EXTERNAL TSELEC
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DABS,DCOS,DEXP,DSIN
C     ..
C     .. Common blocks ..
      COMMON /CSW/SW,SWC,ISW
      COMMON /LPOLY/PLG,CTLOC,STLOC,C2TLOC,S2TLOC,C3TLOC,S3TLOC,DF,DFA,
     *       APD,APDF,APT,DAY,IYR
      DOUBLE PRECISION APD,APDF,C2TLOC,C3TLOC,CTLOC,DAY,DF,DFA,S2TLOC,
     *                 S3TLOC,STLOC
      INTEGER ISW,IYR
      DOUBLE PRECISION APT(4),PLG(9,4),SW(25),SWC(25)
C     ..
C     .. Statement Functions ..
      DOUBLE PRECISION G0,SG0,SUMEX
C     ..
C     .. Data statements ..
      DATA DGTR/1.74533D-2/,DR/1.72142D-2/,XL/1000.D0/,TLL/1000.D0/
      DATA DAYL/-1.D0/,P14/-1000.D0/,P18/-1000.D0/,P32/-1000.D0/
      DATA HR/.2618D0/,SR/7.2722D-5/,SV/25*1.D0/,NSW/14/,P39/-1000.D0/
C     ..
C     .. Statement Function definitions ..
C      Eq. A24d
C       Eq. A24c
C       Eq. A24a
      G0(A) = (A-4.D0+(P(26)-1.D0)*(A-4.D0+(DEXP(-DABS(P(25))*(A-4.D0))-
     *        1.D0)/DABS(P(25))))
      SUMEX(EX) = 1.D0 + (1.D0-EX**19)/(1.D0-EX)*EX**(.5D0)
      SG0(EX) = (G0(AP(2))+(G0(AP(3))*EX+G0(AP(4))*EX*EX+
     *          G0(AP(5))*EX**3+(G0(AP(6))*EX**4+
     *          G0(AP(7))*EX**12)*(1.D0-EX**8)/(1.D0-EX)))/SUMEX(EX)
C     ..
      A = 0.0D0
      C = 0.0D0
      C2 = 0.0D0
      C2D14 = 0.0D0
      C4 = 0.0D0
      CD14 = 0.0D0
      CD18 = 0.0D0
      CD32 = 0.0D0
      CD39 = 0.0D0
      EX = 0.0D0
      EXP1 = 0.0D0
      EXP2 = 0.0D0
      F1 = 0.0D0
      F2 = 0.0D0
      I = -2147483648
      P44 = 0.0D0
      P45 = 0.0D0
      S = 0.0D0
      S2 = 0.0D0
      T71 = 0.0D0
      T72 = 0.0D0
      T81 = 0.0D0
      T82 = 0.0D0
C     ..
      IF (ISW.NE.64999) CALL TSELEC(SV)
      T(10) = 0.D0
      T(11) = 0.D0
      T(12) = 0.D0
      T(13) = 0.D0
      CONTINUE
      IYR = YRD/1000.D0
      DAY = YRD - IYR*1000.D0
C      Eq. A22 (remainder of code)
      IF (XL.EQ.LAT) GO TO 10
C          CALCULATE LEGENDRE POLYNOMIALS
      C = DSIN(LAT*DGTR)
      S = DCOS(LAT*DGTR)
      C2 = C*C
      C4 = C2*C2
      S2 = S*S
      PLG(2,1) = C
      PLG(3,1) = 0.5D0*(3.D0*C2-1.D0)
      PLG(4,1) = 0.5D0*(5.D0*C*C2-3.D0*C)
      PLG(5,1) = (35.D0*C4-30.D0*C2+3.D0)/8.D0
      PLG(6,1) = (63.D0*C2*C2*C-70.D0*C2*C+15.D0*C)/8.D0
      PLG(7,1) = (11.D0*C*PLG(6,1)-5.D0*PLG(5,1))/6.D0
      PLG(2,2) = S
      PLG(3,2) = 3.D0*C*S
      PLG(4,2) = 1.5D0*(5.D0*C2-1.D0)*S
      PLG(5,2) = 2.5D0*(7.D0*C2*C-3.D0*C)*S
      PLG(6,2) = 1.875D0*(21.D0*C4-14.D0*C2+1.D0)*S
      PLG(7,2) = (11.D0*C*PLG(6,2)-6.D0*PLG(5,2))/5.D0
      PLG(3,3) = 3.D0*S2
      PLG(4,3) = 15.D0*S2*C
      PLG(5,3) = 7.5D0*(7.D0*C2-1.D0)*S2
      PLG(6,3) = 3.D0*C*PLG(5,3) - 2.D0*PLG(4,3)
      PLG(7,3) = (11.D0*C*PLG(6,3)-7.D0*PLG(5,3))/4.D0
      PLG(8,3) = (13.D0*C*PLG(7,3)-8.D0*PLG(6,3))/5.D0
      PLG(4,4) = 15.D0*S2*S
      PLG(5,4) = 105.D0*S2*S*C
      PLG(6,4) = (9.D0*C*PLG(5,4)-7.D0*PLG(4,4))/2.D0
      PLG(7,4) = (11.D0*C*PLG(6,4)-8.D0*PLG(5,4))/3.D0
      XL = LAT
   10 CONTINUE
      IF (TLL.EQ.TLOC) GO TO 20
      STLOC = DSIN(HR*TLOC)
      CTLOC = DCOS(HR*TLOC)
      S2TLOC = DSIN(2.D0*HR*TLOC)
      C2TLOC = DCOS(2.D0*HR*TLOC)
      S3TLOC = DSIN(3.D0*HR*TLOC)
      C3TLOC = DCOS(3.D0*HR*TLOC)
      TLL = TLOC
   20 CONTINUE
      IF (DAY.NE.DAYL .OR. P(14).NE.P14) CD14 = DCOS(DR*(DAY-P(14)))
      IF (DAY.NE.DAYL .OR. P(14).NE.P14) C2D14 = DCOS(DR*2*(DAY-P(14)))
      IF (DAY.NE.DAYL .OR. P(18).NE.P18) CD18 = DCOS(2.D0*DR*
     *    (DAY-P(18)))
      IF (DAY.NE.DAYL .OR. P(32).NE.P32) CD32 = DCOS(DR*(DAY-P(32)))
      IF (DAY.NE.DAYL .OR. P(39).NE.P39) CD39 = DCOS(2.D0*DR*
     *    (DAY-P(39)))
      DAYL = DAY
      P14 = P(14)
      P18 = P(18)
      P32 = P(32)
      P39 = P(39)
C         F10.7 EFFECT
      DF = F107 - F107A
      DFA = F107A - 150.D0
      T(1) = P(20)*DF + P(21)*DF*DF + P(22)*DFA + P(30)*DFA**2
      F1 = 1.D0 + (P(48)*DFA+P(20)*DF+P(21)*DF*DF)*SWC(1)
      F2 = 1.D0 + (P(50)*DFA+P(20)*DF+P(21)*DF*DF)*SWC(1)
C        TIME INDEPENDENT
      T(2) = (P(2)*PLG(3,1)+P(3)*PLG(5,1)+P(23)*PLG(7,1)) +
     *       (P(15)*PLG(3,1))*DFA*SWC(1) + P(27)*PLG(2,1)
C        SYMMETRICAL ANNUAL
      T(3) = (P(19))*CD32
C        SYMMETRICAL SEMIANNUAL
      T(4) = (P(16)+P(17)*PLG(3,1))*CD18
C        ASYMMETRICAL ANNUAL
      T(5) = F1*(P(10)*PLG(2,1)+P(11)*PLG(4,1))*CD14
C         ASYMMETRICAL SEMIANNUAL
      T(6) = P(38)*PLG(2,1)*CD39
C        DIURNAL
      T71 = (P(12)*PLG(3,2)+P(36)*PLG(2,2))*CD14*SWC(5)
      T72 = (P(13)*PLG(3,2)+P(37)*PLG(2,2))*CD14*SWC(5)
      T(7) = F2*((P(4)*PLG(2,2)+P(5)*PLG(4,2)+P(28)*PLG(6,2)+T71)*CTLOC+
     *       (P(7)*PLG(2,2)+P(8)*PLG(4,2)+P(29)*PLG(6,2)+T72)*STLOC)
C        SEMIDIURNAL
      T81 = (P(24)*PLG(4,3))*CD14*SWC(5)
      T82 = (P(34)*PLG(4,3))*CD14*SWC(5)
      T(8) = F2*((P(6)*PLG(3,3)+P(42)*PLG(5,3)+T81)*C2TLOC+
     *       (P(9)*PLG(3,3)+P(43)*PLG(5,3)+T82)*S2TLOC)
C        TERDIURNAL
      T(14) = F2*((P(40)*PLG(4,4)+(P(94)*PLG(5,4)+P(47)*PLG(7,
     *        4))*CD14*SWC(5))*S3TLOC+(P(41)*PLG(4,4)+(P(95)*PLG(5,
     *        4)+P(49)*PLG(7,4))*CD14*SWC(5))*C3TLOC)
C          MAGNETIC ACTIVITY BASED ON DAILY AP
      IF (SW(9).EQ.-1.D0 .AND. P(52).NE.0.D0) GO TO 30
      APD = (AP(1)-4.D0)
      P44 = P(44)
      P45 = P(45)
      IF (P44.LT.0) P44 = 1.D-5
      APDF = (APD+(P45-1.D0)*(APD+(DEXP(-P44*APD)-1.D0)/P44))
      T(9) = APDF*(P(33)+P(46)*PLG(3,1)+P(35)*PLG(5,1)+
     *       (P(101)*PLG(2,1)+P(102)*PLG(4,1)+P(103)*PLG(6,1))*CD14*
     *       SWC(5)+(P(122)*PLG(2,2)+P(123)*PLG(4,2)+P(124)*PLG(6,2))*
     *       SWC(7)*DCOS(HR*(TLOC-P(125))))
      GO TO 40
   30 CONTINUE
      EXP1 = DEXP(-10800.D0*DABS(P(52))/(1.D0+P(139)*(45.D0-DABS(LAT))))
      IF (EXP1.GT..99999D0) EXP1 = .99999D0
      EXP2 = DEXP(-10800.D0*DABS(P(54)))
      IF (EXP2.GT..99999D0) EXP2 = .99999D0
      IF (P(25).LT.1.D-4) P(25) = 1.D-4
      APT(1) = SG0(EXP1)
      APT(3) = SG0(EXP2)
      T(9) = APT(1)*(P(51)+P(97)*PLG(3,1)+P(55)*PLG(5,1)+
     *       (P(126)*PLG(2,1)+P(127)*PLG(4,1)+P(128)*PLG(6,1))*CD14*
     *       SWC(5)+(P(129)*PLG(2,2)+P(130)*PLG(4,2)+P(131)*PLG(6,2))*
     *       SWC(7)*DCOS(HR*(TLOC-P(132))))
   40 CONTINUE
      IF (SW(10).EQ.0 .OR. LONG.LE.-1000.D0) GO TO 70
C        LONGITUDINAL
      T(11) = (1.D0+P(90)*PLG(2,1))*(1.D0+P(81)*DFA*SWC(1))*
     *        ((P(65)*PLG(3,2)+P(66)*PLG(5,2)+P(67)*PLG(7,
     *        2)+P(104)*PLG(2,2)+P(105)*PLG(4,2)+P(106)*PLG(6,
     *        2)+SWC(5)*(P(110)*PLG(2,2)+P(111)*PLG(4,2)+P(112)*PLG(6,
     *        2))*CD14)*DCOS(DGTR*LONG)+(P(91)*PLG(3,2)+P(92)*PLG(5,
     *        2)+P(93)*PLG(7,2)+P(107)*PLG(2,2)+P(108)*PLG(4,
     *        2)+P(109)*PLG(6,2)+SWC(5)*(P(113)*PLG(2,2)+P(114)*PLG(4,
     *        2)+P(115)*PLG(6,2))*CD14)*DSIN(DGTR*LONG))
C        UT AND MIXED UT,LONGITUDE
      T(12) = (1.D0+P(96)*PLG(2,1))*(1.D0+P(82)*DFA*SWC(1))*
     *        (1.D0+P(120)*PLG(2,1)*SWC(5)*CD14)*
     *        ((P(69)*PLG(2,1)+P(70)*PLG(4,1)+P(71)*PLG(6,1))*
     *        DCOS(SR*(SEC-P(72))))
      T(12) = T(12) + SWC(11)*(P(77)*PLG(4,3)+P(78)*PLG(6,3)+
     *        P(79)*PLG(8,3))*DCOS(SR*(SEC-P(80))+2.D0*DGTR*LONG)*
     *        (1.D0+P(138)*DFA*SWC(1))
C        UT,LONGITUDE MAGNETIC ACTIVITY
      IF (SW(9).EQ.-1.D0 .AND. P(52).NE.0.D0) GO TO 50
      T(13) = APDF*SWC(11)*(1.D0+P(121)*PLG(2,1))*
     *        ((P(61)*PLG(3,2)+P(62)*PLG(5,2)+P(63)*PLG(7,2))*
     *        DCOS(DGTR*(LONG-P(64)))) + APDF*SWC(11)*SWC(5)*
     *        (P(116)*PLG(2,2)+P(117)*PLG(4,2)+P(118)*PLG(6,2))*CD14*
     *        DCOS(DGTR*(LONG-P(119))) + APDF*SWC(12)*
     *        (P(84)*PLG(2,1)+P(85)*PLG(4,1)+P(86)*PLG(6,1))*
     *        DCOS(SR*(SEC-P(76)))
      GO TO 60
   50 CONTINUE
      T(13) = APT(1)*SWC(11)*(1.D0+P(133)*PLG(2,1))*
     *        ((P(53)*PLG(3,2)+P(99)*PLG(5,2)+P(68)*PLG(7,2))*
     *        DCOS(DGTR*(LONG-P(98)))) + APT(1)*SWC(11)*SWC(5)*
     *        (P(134)*PLG(2,2)+P(135)*PLG(4,2)+P(136)*PLG(6,2))*CD14*
     *        DCOS(DGTR*(LONG-P(137))) + APT(1)*SWC(12)*
     *        (P(56)*PLG(2,1)+P(57)*PLG(4,1)+P(58)*PLG(6,1))*
     *        DCOS(SR*(SEC-P(59)))
   60 CONTINUE
C     PARMS NOT USED: 60,83,100,140-150
   70 TINF = 0.D0
      IF (SW(9).EQ.-1.D0) TINF = P(31)
      DO 80 I = 1,NSW
         TINF = TINF + DABS(SW(I))*T(I)
   80 CONTINUE
      GLOBE5 = TINF
      RETURN
      END
C     *PL*ERROR* Can't indent comment line
C*PL*ERROR* Can't indent comment line
C   --------------------------------------------------------------------
      SUBROUTINE TSELEC(SV)
C        SET SWITCHES
C     .. Array Arguments ..
      DOUBLE PRECISION SV(*)
C     ..
C     .. Scalars in Common ..
C     ..
C     .. Arrays in Common ..
C     ..
C     .. Local Scalars ..
      INTEGER I,M11111
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION SAV(25),SVV(25)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DABS,DMOD
C     ..
C     .. Common blocks ..
      COMMON /CSW/SW,SWC,ISW
      INTEGER ISW
      DOUBLE PRECISION SW(25),SWC(25)
C     ..
      DO 10 M11111 = 1,25
         SAV(M11111) = 0.0D0
   10 CONTINUE
      DO 20 M11111 = 1,25
         SVV(M11111) = 0.0D0
   20 CONTINUE
C     ..
      DO 30 I = 1,25
         SAV(I) = SV(I)
         SW(I) = DMOD(SV(I),2.D0)
         IF (DABS(SV(I)).GT.0.D0) THEN
            SWC(I) = 1.D0
         ELSE
            SWC(I) = 0.D0
         END IF
   30 CONTINUE
      ISW = 64999
      RETURN
      ENTRY TRETRV()
      DO 40 I = 1,25
         SVV(I) = SAV(I)
   40 CONTINUE
      END
C     *PL*ERROR* Can't indent comment line
C*PL*ERROR* Can't indent comment line
C   --------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION GLOB5L(P)
C      LIMITED PARAMETER VERSION OF GLOBE 9/2/82
C       CALCULATE G(L) FUNCTION FOR MSIS-86/CIRA 1986
C       Lower Thermosphere Parameters
C     .. Array Arguments ..
      DOUBLE PRECISION P(*)
C     ..
C     .. Scalars in Common ..
C     ..
C     .. Arrays in Common ..
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION CD11,CD7,CD9,DAYL,DR,P11,P7,P9,TT
      INTEGER I
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION T(15)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DABS,DCOS
C     ..
C     .. Common blocks ..
      COMMON /CSW/SW,SWC,ISW
      COMMON /LPOLY/PLG,CTLOC,STLOC,C2TLOC,S2TLOC,C3TLOC,S3TLOC,DF,DFA,
     *       APD,APDF,APT,DAY,IYR
      DOUBLE PRECISION APD,APDF,C2TLOC,C3TLOC,CTLOC,DAY,DF,DFA,S2TLOC,
     *                 S3TLOC,STLOC
      INTEGER ISW,IYR
      DOUBLE PRECISION APT(4),PLG(9,4),SW(25),SWC(25)
C     ..
C     .. Data statements ..
      DATA DR/1.72142D-2/,T/15*0.D0/
      DATA DAYL/-1.D0/,P7/-1000.D0/,P9/-1000.D0/,P11/-1000.D0/
C     ..
      CD11 = 0.0D0
      CD7 = 0.0D0
      CD9 = 0.0D0
      TT = 0.0D0
C     ..
      IF (DAY.NE.DAYL .OR. P7.NE.P(7)) CD7 = DCOS(DR*(DAY-P(7)))
      IF (DAY.NE.DAYL .OR. P9.NE.P(9)) CD9 = DCOS(2.D0*DR*(DAY-P(9)))
      IF (DAY.NE.DAYL .OR. P11.NE.P(11)) CD11 = DCOS(DR*(DAY-P(11)))
      DAYL = DAY
      P7 = P(7)
      P9 = P(9)
      P11 = P(11)
C
      T(1) = P(2)*DFA
      T(2) = P(4)*PLG(3,1)
      T(3) = P(6)*CD7
      T(4) = (P(8))*CD9
      T(5) = (P(10)*PLG(2,1)+P(22)*PLG(4,1))*CD11
      T(6) = 0.D0
      T(7) = (P(14)*PLG(2,2)*CTLOC+P(15)*PLG(2,2)*STLOC)
      T(8) = (P(16)*PLG(3,3)+P(18)*PLG(5,3)+
     *       (P(20)*PLG(6,3))*CD11*SWC(5))*C2TLOC +
     *       (P(17)*PLG(3,3)+P(19)*PLG(5,3)+
     *       (P(21)*PLG(6,3))*CD11*SWC(5))*S2TLOC
      T(14) = (P(12)*PLG(4,4)*C3TLOC+P(25)*PLG(4,4)*S3TLOC)
      IF (SW(9).EQ.1) T(9) = APDF*(P(23)+P(24)*PLG(3,1)*SWC(2))
      IF (SW(9).EQ.-1) T(9) = (P(3)*APT(3)+P(5)*PLG(3,1)*APT(3)*SWC(2))
C       PARMS NOT USED: 13
      TT = 0.D0
      DO 10 I = 1,14
         TT = TT + DABS(SW(I))*T(I)
   10 CONTINUE
      GLOB5L = TT
      RETURN
      END
C     *PL*ERROR* Can't indent comment line
C*PL*ERROR* Can't indent comment line
C   --------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION DNET(DD,DM,ZHM,XMM,XM)
C       8/20/80
C       TURBOPAUSE CORRECTION FOR MSIS MODELS
C       Eq. A12b
C     .. Scalar Arguments ..
      DOUBLE PRECISION DD,DM,XM,XMM,ZHM
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION A,YLOG
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DEXP,DLOG
C     ..
      A = 0.0D0
      YLOG = 0.0D0
C     ..
      A = ZHM/(XMM-XM)
C       Eq. A12a
      YLOG = A*DLOG(DM/DD)
      IF (YLOG.LT.-10.D0) GO TO 10
      IF (YLOG.GT.10.D0) GO TO 20
      DNET = DD*(1.D0+DEXP(YLOG))**(1/A)
      GO TO 30
   10 CONTINUE
      DNET = DD
      GO TO 30
   20 CONTINUE
      DNET = DM
      GO TO 30
   30 CONTINUE
      RETURN
      END
C     *PL*ERROR* Can't indent comment line
C*PL*ERROR* Can't indent comment line
C   --------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION CCOR(ALT,R,H1,ZH)
C        CHEMISTRY/DISSOCIATION CORRECTION FOR MSIS MODELS
C     Eq. A20a or Eq. A21
C     .. Scalar Arguments ..
      DOUBLE PRECISION ALT,H1,R,ZH
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION E,EX
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DEXP
C     ..
      E = 0.0D0
      EX = 0.0D0
C     ..
      E = (ALT-ZH)/H1
      IF (E.GT.70.D0) GO TO 20
      IF (E.LT.-70.D0) GO TO 10
      EX = DEXP(E)
      CCOR = R/(1.D0+EX)
      GO TO 30
   10 CCOR = R
      GO TO 30
   20 CCOR = 0.D0
      GO TO 30
   30 CONTINUE
      CCOR = DEXP(CCOR)
      RETURN
      END
C     *PL*ERROR* Can't indent comment line
C*PL*ERROR* Can't indent comment line
C   --------------------------------------------------------------------
      SUBROUTINE PRMSG5
C          CIRA     11-FEB-86
C     .. Arrays in Common ..
C     ..
C     .. Local Scalars ..
      INTEGER I
C     ..
C     .. Local Arrays ..
      INTEGER IC86(2),ISD(3),IST(2)
C     ..
C     .. Common blocks ..
C
      COMMON /DATIME/ISDATE,ISTIME,NAME
      INTEGER ISDATE(3),ISTIME(2),NAME(2)
C     ..
C     .. Data statements ..
      DATA ISD/4H11-F,4HEB-8,4H6   /,IST/4H18:2,4H3:31/
      DATA IC86/4HCIRA,4H-86 /
C     ..
      DO 10 I = 1,3
         ISDATE(I) = ISD(I)
   10 CONTINUE
      DO 20 I = 1,2
         ISTIME(I) = IST(I)
         NAME(I) = IC86(I)
   20 CONTINUE
      END
      BLOCK DATA BRMSG5
C          CIRA     11-FEB-86
C     .. Scalars in Common ..
C     ..
C     .. Arrays in Common ..
C     ..
C     .. Common blocks ..
      COMMON /LOWER5/PTM,PDM
      COMMON /PARM5/PT1,PT2,PT3,PA1,PA2,PA3,PB1,PB2,PB3,PC1,PC2,PC3,PD1,
     *       PD2,PD3,PE1,PE2,PE3,PF1,PF2,PF3,PG1,PG2,PG3,PH1,PH2,PH3,PI1
      COMMON /PARMB/GSURF,RE
      DOUBLE PRECISION GSURF,RE
      DOUBLE PRECISION PA1(50),PA2(50),PA3(50),PB1(50),PB2(50),PB3(50),
     *                 PC1(50),PC2(50),PC3(50),PD1(50),PD2(50),PD3(50),
     *                 PDM(8,7),PE1(50),PE2(50),PE3(50),PF1(50),PF2(50),
     *                 PF3(50),PG1(50),PG2(50),PG3(50),PH1(50),PH2(50),
     *                 PH3(50),PI1(50),PT1(50),PT2(50),PT3(50),PTM(8)
C     ..
C     .. Data statements ..
C         TEMPERATURE
C         HE DENSITY
C         O DENSITY
C         N2 DENSITY & TLB
C         Z0 & T0
C         TR
C         O2 DENSITY
C         AR DENSITY
C          H DENSITY
C          N DENSITY
C          S PARAM
C          TURBO
C         LOWER BOUNDARY
      DATA GSURF,RE/980.665D0,6356.77D0/
      DATA PT1/9.96040D-01,3.85528D-02,3.03445D-03,-1.05531D-01,
     *     -6.07134D-03,-5.16278D-04,-1.15622D-01,2.02240D-03,
     *     9.90156D-03,-1.27371D-01,-3.02449D-02,1.23512D-02,
     *     -5.26277D-03,-8.45398D+00,0.00000D+00,1.42370D-02,
     *     0.00000D+00,1.25818D+02,8.05486D-03,1.64419D-03,-6.21452D-06,
     *     3.11701D-03,0.00000D+00,3.86578D-03,1.32397D-01,2.13315D-01,
     *     0.00000D+00,0.00000D+00,0.00000D+00,-6.41110D-06,0.00000D+00,
     *     3.00150D+01,5.33297D-03,3.89146D-03,2.04725D-03,0.00000D+00,
     *     0.00000D+00,-1.92645D-02,2.75905D+00,1.47284D-03,3.41345D-04,
     *     -1.17388D-03,-3.54589D-04,1.13139D-01,1.69134D-01,
     *     5.08295D-03,3.65016D-05,4.26385D-03,1.15102D-04,5.11819D-03/
      DATA PT2/6.09108D-03,4.04995D-05,1.53049D-03,2.41470D-05,
     *     2.30764D-03,1.55267D-03,1.33722D-03,-1.82318D-03,
     *     -2.63007D+02,0.00000D+00,1.37337D-03,9.95774D-04,0.00000D+00,
     *     -1.08983D+02,5.62606D-03,5.94053D-03,1.09358D-03,0.00000D+00,
     *     -1.33410D-02,-2.43409D-02,-1.35688D-02,3.11370D+04,
     *     0.00000D+00,0.00000D+00,0.00000D+00,-2.83023D+03,8.45583D-04,
     *     5.38706D-04,0.00000D+00,2.47956D+02,2.92246D-03,0.00000D+00,
     *     0.00000D+00,7.47703D-05,8.87993D-04,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,-1.16540D-02,
     *     -4.49173D-03,-3.53189D-04,-1.73933D-04,-1.53218D-04,
     *     -5.65411D-01,7.77272D-03,-9.11784D+01,6.45187D-04,
     *     0.00000D+00/
      DATA PT3/-8.37685D-04,2.42318D-03,4.73796D-03,-3.01801D-03,
     *     -4.23564D-03,-2.48289D-03,9.19286D-04,2.16372D-03,
     *     8.63968D-04,1.89689D-03,4.15654D-03,0.00000D+00,1.18068D-02,
     *     3.31190D-03,0.00000D+00,1.20222D-03,0.00000D+00,0.00000D+00,
     *     -3.07246D+00,0.00000D+00,0.00000D+00,6.72403D-04,1.08930D-03,
     *     9.72278D-04,4.68242D+00,-3.15034D-04,4.00059D-03,5.15036D-03,
     *     1.62989D-03,1.08824D-03,9.95261D-04,4.18955D+00,-3.64059D-01,
     *     1.70182D-03,0.00000D+00,0.00000D+00,-3.20120D+00,0.00000D+00,
     *     5.80206D-03,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00/
      DATA PA1/1.04934D+00,-2.88362D-02,-2.07095D-01,-1.03314D-01,
     *     -7.02373D-03,1.29664D-02,4.08853D-01,-9.19895D-03,
     *     -1.88660D-02,1.40927D+00,1.75033D-01,1.87351D-02,1.10979D-01,
     *     -7.42871D+00,0.00000D+00,2.67143D-01,-5.95979D-02,
     *     1.05038D+02,-8.40963D-02,-6.97632D-04,2.06521D-06,
     *     7.65306D-04,0.00000D+00,0.00000D+00,1.26762D-01,1.28876D-01,
     *     -5.04479D-02,-1.30735D-02,-2.24348D-02,0.00000D+00,
     *     0.00000D+00,-1.50832D+02,-6.29928D-03,0.00000D+00,
     *     -4.07760D-03,0.00000D+00,0.00000D+00,5.25725D-02,
     *     -3.11486D+01,-3.13351D-03,2.75838D-03,0.00000D+00,
     *     0.00000D+00,1.11247D-01,1.08815D-01,-4.66713D-02,0.00000D+00,
     *     -3.29329D-03,0.00000D+00,1.67838D-03/
      DATA PA2/-9.16691D-03,3.45044D-05,-9.71806D-03,0.00000D+00,
     *     -2.04672D-03,-7.86899D-03,-7.98285D-03,5.36515D-03,
     *     -5.31172D+03,0.00000D+00,-6.42781D-03,-1.71690D-03,
     *     0.00000D+00,-6.79131D+01,-1.79912D-02,-1.58305D-02,
     *     -7.12313D-03,0.00000D+00,2.53477D-02,8.52960D-02,1.02163D-01,
     *     2.95009D+04,0.00000D+00,0.00000D+00,0.00000D+00,-6.84625D+03,
     *     -6.19098D-03,-2.69289D-03,0.00000D+00,-5.20231D+02,
     *     -6.33463D-03,0.00000D+00,0.00000D+00,-6.02428D-03,
     *     -4.07077D-03,5.42264D-03,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,4.07560D-02,2.82288D-02,9.08088D-03,0.00000D+00,
     *     0.00000D+00,-4.05204D-01,-5.97931D-02,-7.31823D+01,
     *     -2.06620D-03,0.00000D+00/
      DATA PA3/-3.72723D-03,-1.88146D-02,-1.01794D-02,8.04633D-03,
     *     1.01090D-02,8.73253D-03,2.38268D-02,4.80444D-03,1.71088D-03,
     *     3.96369D-02,-2.13809D-02,0.00000D+00,-1.02588D-01,
     *     -5.91702D-03,0.00000D+00,2.70923D-03,0.00000D+00,0.00000D+00,
     *     -1.75043D+02,6.03489D-01,-6.17589D-01,8.38098D-03,
     *     1.83871D-03,-7.05329D-04,-4.06644D+00,-5.09347D-03,
     *     -2.84344D-02,-1.24160D-02,1.33665D-02,3.93410D-03,
     *     -5.03723D-04,-4.57683D+00,-5.29542D-01,-4.25812D-03,
     *     0.00000D+00,0.00000D+00,1.91541D+01,0.00000D+00,3.23247D-03,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00/
      DATA PB1/9.31113D-01,-1.38721D-01,-1.33457D-01,-5.29542D-02,
     *     -4.44983D-03,1.35264D-02,5.98075D-02,-3.62880D-02,
     *     -3.12798D-02,3.72068D-01,2.95974D-02,1.20509D-02,5.21995D-02,
     *     -7.78888D+00,0.00000D+00,1.18634D-01,-2.04495D-02,
     *     1.03280D+02,9.82432D-02,4.77694D-04,0.00000D+00,2.74372D-03,
     *     0.00000D+00,0.00000D+00,7.57809D-02,1.71403D-01,-1.05205D-02,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,-8.73348D+00,
     *     -5.81094D-03,0.00000D+00,-8.14944D-03,0.00000D+00,
     *     0.00000D+00,5.17255D-02,-1.53028D+01,-3.48932D-03,
     *     9.61771D-04,5.57732D-03,-4.54180D-04,9.88213D-02,9.40456D-02,
     *     -3.18797D-02,0.00000D+00,0.00000D+00,0.00000D+00,2.32122D-03/
      DATA PB2/-6.00220D-03,2.77654D-05,-3.22019D-03,0.00000D+00,
     *     -3.78551D-03,-3.34809D-03,-1.70668D-03,0.00000D+00,
     *     6.36184D+03,0.00000D+00,1.59986D-03,-3.88204D-03,
     *     -1.64825D-03,-7.47955D+01,-1.05360D-02,-9.45723D-03,
     *     -1.59824D-03,-7.06730D-04,-1.68513D-02,-1.13023D-01,
     *     -6.36637D-02,-1.37709D+04,0.00000D+00,0.00000D+00,
     *     0.00000D+00,-1.52368D+04,-5.86061D-03,-2.53108D-03,
     *     0.00000D+00,-2.54837D+03,-3.28988D-03,0.00000D+00,
     *     0.00000D+00,-2.76364D-03,9.67923D-03,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,4.34255D-02,1.14020D-02,
     *     -6.18447D-03,0.00000D+00,0.00000D+00,-3.02568D-01,
     *     -3.27694D-02,-6.71589D+01,-2.28340D-03,0.00000D+00/
      DATA PB3/3.06230D-03,-4.65113D-03,-9.73421D-03,1.28326D-02,
     *     7.88553D-03,7.97197D-03,-1.20760D-02,-7.67547D-03,
     *     -1.20755D-03,-2.98523D-02,-1.26560D-02,0.00000D+00,
     *     -5.68350D-02,-1.53039D-02,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     2.42911D-03,-4.01347D-03,-2.19074D-03,3.11281D+00,
     *     3.23251D-03,-6.39523D-03,-6.63069D-03,-3.04403D-04,
     *     -4.01920D-03,-1.18708D-03,4.15211D+00,-2.01896D-01,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00/
      DATA PC1/1.06903D+00,3.77113D-04,0.00000D+00,0.00000D+00,
     *     0.00000D+00,8.98481D-02,-2.36325D+01,2.08180D-02,1.39638D+02,
     *     -1.19444D-01,-8.45398D+00,-3.99776D-06,0.00000D+00,
     *     3.66210D-03,-1.78929D-03,1.90412D-02,-3.92257D-02,
     *     6.32343D-03,5.48144D-03,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,-2.43022D-03,9.76619D-01,5.68478D-04,
     *     5.82026D-03,0.00000D+00,6.21998D-03,0.00000D+00,0.00000D+00,
     *     1.07674D-02,8.93820D+01,-1.92414D-02,-8.45398D+00,
     *     0.00000D+00,0.00000D+00,-2.00200D-02,-1.95833D-03,
     *     -9.38391D-03,1.31480D-02,-2.60147D-03,-8.08556D-04,
     *     5.11651D-05,2.55717D-03,0.00000D+00,4.66814D-03,6.64196D-03,
     *     0.00000D+00/
      DATA PC2/9.98594D-01,1.90038D-04,0.00000D+00,-2.43825D-02,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     5.22105D-02,-8.45398D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,7.67271D-03,5.64539D-03,-2.70623D-03,
     *     -5.26454D-04,1.37075D-03,1.33060D-03,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,9.49197D-01,0.00000D+00,0.00000D+00,
     *     -7.68008D-02,0.00000D+00,0.00000D+00,0.00000D+00,
     *     -1.37993D-02,-1.40136D+00,1.20481D-01,-8.45398D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,9.87746D-03,
     *     1.75330D-03,-6.88835D-04,2.87022D-03,0.00000D+00,0.00000D+00,
     *     7.44513D-02,0.00000D+00,0.00000D+00,0.00000D+00/
      DATA PC3/1.52840D-01,0.00000D+00,0.00000D+00,1.16252D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     -6.49190D-01,-8.45398D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,-5.84949D-02,-1.02105D-01,
     *     2.99153D-02,-4.86227D-02,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00/
      DATA PD1/9.31402D-01,1.37976D-01,0.00000D+00,3.23736D-04,
     *     0.00000D+00,-9.10906D-03,7.07506D-02,0.00000D+00,
     *     -5.16650D-02,6.89755D-02,0.00000D+00,0.00000D+00,0.00000D+00,
     *     -8.45398D+00,0.00000D+00,2.81140D-02,0.00000D+00,7.36009D+01,
     *     5.96604D-02,0.00000D+00,0.00000D+00,-1.51792D-03,0.00000D+00,
     *     0.00000D+00,1.32397D-01,2.13315D-01,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,9.48758D+00,8.84541D-03,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     1.13139D-01,1.69134D-01,1.45192D-02,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00/
      DATA PD2/1.07906D-02,2.99942D-05,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     -1.48930D-02,-7.87184D-03,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,-6.83420D-02,-4.41778D-02,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,2.29730D-02,
     *     0.00000D+00,0.00000D+00,0.00000D+00/
      DATA PD3/0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00/
      DATA PE1/8.68053D-01,2.36364D-01,1.34306D-01,1.03086D-02,
     *     0.00000D+00,-3.79164D-03,-1.57806D-01,0.00000D+00,
     *     -5.87644D-02,-3.12508D-01,0.00000D+00,4.37387D-02,
     *     -3.54091D-02,-2.23636D+01,0.00000D+00,-5.33976D-02,
     *     0.00000D+00,1.14091D+02,5.17497D-02,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,1.32397D-01,2.13315D-01,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     3.42702D+02,1.57033D-02,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,-3.66278D-03,
     *     -1.16193D-03,0.00000D+00,0.00000D+00,1.13139D-01,1.69134D-01,
     *     1.78431D-02,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00/
      DATA PE2/1.62864D-02,3.16963D-05,1.27968D-02,0.00000D+00,
     *     0.00000D+00,-7.04599D-03,2.07921D-03,6.36660D-03,2.29940D+04,
     *     0.00000D+00,1.27833D-02,-2.08036D-03,-4.61820D-03,
     *     -6.29391D+01,-1.20745D-02,1.36675D-02,1.36011D-02,
     *     -5.37162D-03,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,1.92509D+04,8.35522D-03,
     *     4.19439D-03,0.00000D+00,1.20366D+04,0.00000D+00,0.00000D+00,
     *     0.00000D+00,-1.00034D-02,-2.33267D-03,9.72374D-03,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,-2.65079D-02,
     *     -2.09125D-02,-1.09465D-02,0.00000D+00,0.00000D+00,
     *     0.00000D+00,2.17252D-02,-7.12385D+01,-1.89428D-03,
     *     0.00000D+00/
      DATA PE3/-6.02006D-03,1.69058D-02,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     2.90646D-02,3.48971D-03,0.00000D+00,5.01174D-02,5.50595D-02,
     *     0.00000D+00,-9.55897D-03,0.00000D+00,0.00000D+00,
     *     -1.51693D+03,0.00000D+00,0.00000D+00,1.29306D-02,2.69567D-03,
     *     0.00000D+00,3.92243D+00,-8.47690D-03,1.16896D-02,0.00000D+00,
     *     1.48967D-02,5.44521D-03,0.00000D+00,5.64918D+00,0.00000D+00,
     *     -7.72178D-03,0.00000D+00,0.00000D+00,-7.34042D+01,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00/
      DATA PF1/1.27515D+00,-2.10472D-01,-1.77924D-01,2.18900D-01,
     *     2.88436D-02,1.90077D-02,2.91001D-01,2.17437D-02,-1.05186D-02,
     *     4.36141D-01,1.07605D-01,3.30755D-02,4.00581D-02,-9.58051D+00,
     *     0.00000D+00,1.54028D-02,0.00000D+00,7.34194D+01,4.96540D-02,
     *     -5.95906D-03,3.84512D-05,-1.36000D-02,0.00000D+00,
     *     0.00000D+00,1.32397D-01,2.13315D-01,-4.16610D-02,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,1.46276D+02,-1.98408D-02,
     *     0.00000D+00,1.32530D-02,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,-1.04687D-04,-1.47562D-03,0.00000D+00,
     *     0.00000D+00,1.13139D-01,1.69134D-01,-1.26913D-02,0.00000D+00,
     *     0.00000D+00,0.00000D+00,-6.08370D-03/
      DATA PF2/-2.57587D-02,3.19022D-05,0.00000D+00,0.00000D+00,
     *     1.56644D-02,1.03640D-02,1.05771D-03,0.00000D+00,3.57949D+03,
     *     0.00000D+00,-1.25672D-03,1.52783D-03,1.30518D-03,7.55558D+00,
     *     -9.20341D-03,-2.09142D-02,-1.34106D-02,0.00000D+00,
     *     -4.83312D-02,8.30900D-02,9.88009D-02,-1.41148D+04,
     *     0.00000D+00,0.00000D+00,0.00000D+00,-1.05513D+03,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,6.73442D-03,2.01691D-03,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,5.98019D-02,6.33298D-03,
     *     -1.12871D-03,0.00000D+00,0.00000D+00,0.00000D+00,
     *     -1.28604D-02,0.00000D+00,0.00000D+00,0.00000D+00/
      DATA PF3/-4.94960D-03,-1.36415D-02,-1.15039D-02,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,-5.86860D-03,-1.41732D-03,
     *     2.13697D-03,2.63845D+00,-8.34186D-03,-1.87336D-02,
     *     -1.90870D-02,-8.03810D-03,-2.84279D-03,2.56722D-03,
     *     1.71429D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00/
      DATA PG1/5.73587D+01,-3.98747D-01,0.00000D+00,-5.29554D-01,
     *     -5.82186D-03,7.14177D-02,-6.79279D-01,-1.67715D-01,
     *     -6.42434D-02,-2.11569D-01,-1.59922D-01,-1.71024D-04,
     *     -1.15885D-01,6.51603D+00,0.00000D+00,-1.76683D-01,
     *     6.50395D-02,1.43504D+00,9.28208D-02,5.11662D-03,0.00000D+00,
     *     9.95121D-03,0.00000D+00,0.00000D+00,1.32397D-01,2.13315D-01,
     *     1.01451D-01,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     5.67667D+01,2.38192D-03,0.00000D+00,-1.88240D-02,0.00000D+00,
     *     0.00000D+00,4.76218D-02,2.35206D+01,4.75901D-03,5.76162D-03,
     *     1.51815D-02,-1.92730D-02,1.13139D-01,1.69134D-01,
     *     -2.88771D-02,0.00000D+00,0.00000D+00,0.00000D+00,1.18418D-03/
      DATA PG2/-3.68927D-03,3.14704D-05,8.82198D-03,0.00000D+00,
     *     -1.92562D-02,-2.58674D-03,-2.19913D-02,0.00000D+00,
     *     4.38655D+03,0.00000D+00,7.60126D-03,2.59438D-03,1.72310D-03,
     *     7.79204D+01,7.97786D-04,-7.70510D-03,1.90982D-03,2.72707D-03,
     *     1.01016D-02,1.16537D-01,-3.12236D-03,1.39783D+04,0.00000D+00,
     *     0.00000D+00,0.00000D+00,-1.30712D+03,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     -3.20544D-03,-2.06970D-02,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,1.59010D-02,-1.91427D-03,
     *     -3.42829D-02,0.00000D+00,0.00000D+00,0.00000D+00,
     *     -3.45379D-02,8.94518D+01,1.71556D-03,0.00000D+00/
      DATA PG3/-7.65278D-03,-2.08987D-04,-1.57393D-02,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,-8.60673D-03,-1.19922D-02,
     *     -6.46356D-03,-3.00107D+00,-9.32511D-03,-1.50205D-02,
     *     -8.67835D-03,-7.64801D-03,-1.31495D-02,-6.76720D-03,
     *     -1.82396D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00/
      DATA PH1/9.51363D-01,-4.67542D-02,1.20260D-01,0.00000D+00,
     *     0.00000D+00,1.91357D-02,0.00000D+00,0.00000D+00,1.25429D-03,
     *     -1.33240D-01,0.00000D+00,0.00000D+00,0.00000D+00,
     *     -8.45398D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,2.52317D-03,0.00000D+00,
     *     -9.73404D-03,1.32397D-01,2.13315D-01,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     -7.18482D-04,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,7.87683D-03,-2.33698D-03,
     *     1.13139D-01,1.69134D-01,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00/
      DATA PH2/0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00/
      DATA PH3/0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00/
      DATA PI1/9.33804D-01,5.47446D+00,1.53263D-01,9.19303D-01,
     *     1.64109D+01,4.27083D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,0.00000D+00,
     *     0.00000D+00,1.15897D+00,4.71094D-01,1.09459D+00,5.25012D+00,
     *     1.00000D+00,1.00000D+00,1.03999D+00,7.67132D-01,1.10514D+00,
     *     1.75636D+00,1.10845D+00,2.33439D+00,7.96532D-01,4.31520D+00,
     *     4.07300D+00,1.01885D+00,2.39547D-01,2.53791D-06,8.42931D-01,
     *     1.04192D+00,2.00202D+00,1.00000D+00,1.00000D+00,1.00000D+00,
     *     1.00000D+00/
      DATA PTM/1.04130D+03,3.86000D+02,1.90000D+02,1.66728D+01,
     *     1.15000D+02,1.20000D+02,9.45537D+01,0.00000D+00/
      DATA PDM/2.45600D+07,6.71072D-06,1.00000D+02,0.00000D+00,
     *     1.10000D+02,1.00000D+01,0.00000D+00,0.00000D+00,8.59400D+10,
     *     5.40000D-01,1.05000D+02,-8.00000D+00,1.10000D+02,1.00000D+01,
     *     9.00000D+01,2.00000D+00,2.81000D+11,0.00000D+00,1.05000D+02,
     *     2.80000D+01,2.89500D+01,0.00000D+00,0.00000D+00,0.00000D+00,
     *     3.30000D+10,2.68270D-01,1.05000D+02,0.00000D+00,1.10000D+02,
     *     1.00000D+01,0.00000D+00,0.00000D+00,1.33000D+09,1.19615D-02,
     *     1.05000D+02,0.00000D+00,1.10000D+02,1.00000D+01,0.00000D+00,
     *     0.00000D+00,1.76100D+05,1.00000D+00,9.50000D+01,-8.00000D+00,
     *     1.10000D+02,1.00000D+01,9.00000D+01,2.00000D+00,1.00000D+07,
     *     1.00000D+00,1.05000D+02,-8.00000D+00,1.10000D+02,1.00000D+01,
     *     9.00000D+01,2.00000D+00/
C     ..
      END
