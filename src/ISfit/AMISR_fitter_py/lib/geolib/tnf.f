C     $Id: tnf.f,v 1.4 2004/04/13 19:04:46 brideout Exp $
C
      DOUBLE PRECISION FUNCTION TNF(TI,TE,NE,NHP,NO,NH,NN2,NO2,NHE,IER)
C
C     TNF calculates the neutral temperature (tn) given the electron and
C     neutral temperatures (TE, TN) and the electron, o+, h+, o, h,
C     n2, o2 and he concentrations (NE, NOP, NHP, NO, NH, NN2, NO2,
C     NHE). o+ and h+ ions are assumed to be the only ions present.
C     only coulomb collisions and ion-neutral polarization and
C     charge-exchange interactions are considered in balancing
C     the ion gas energy source and sink terms.  the solution for
C     tn is an iterative procedure in which TI is the initial value
C     of tn for the first iteration. all concentrations are in
C     units of cm**-3.
C
C       Input:
C          TI - Ion Temperature (K)
C          TE - Electron Temperature
C          NE - Electron concentration (cm**-3)
C         NHP - H Ion concentration
C          NO - O concentration
C          NH - H concentration
C         NN2 - N2 concentration
C         NO2 - O2 concentration
C         NHE - HE concentration
C
C      Output:
C         IER - If (IER.NE.0) an error has occurred.
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION NE,NH,NHE,NHP,NN2,NO,NO2,TE,TI
      INTEGER IER
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION ALOGEI,ALOGT1,ALOGT2,ALOGT3,CEI,CHPH,CHPN,
     *                 CHPO,CONST,CONVRG,COPH,COPN,COPO,COULOG,DIVRG,
     *                 DTEST,FACTOR,FEI,FHPN,FOPN,NOP,QHPH,QHPO,QOPH,
     *                 QOPO,SQRTT1,T1,T2,T3,TECUBE,TEPTI,TN,TNSAVE
      INTEGER ITER,NITER
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,DEXP,DLOG,DSQRT
C     ..
C     .. Data statements ..
C
      DATA CONVRG/1.0D0/,NITER/10/,DIVRG/1.D20/
C     ..
      IER = 0
      TNF = 0.0D0
C
C     .....check input parameters.....
C     write(16,"(5e13.5)") ti,te,ne,nhp,no,nh,nn2,no2,nhe
      IF (TE.LT.TI .OR. NE.LT.1.D0 .OR. NE.GT.1.D10 .OR.
     *    NHP.LT.0.D0 .OR. NHP.GT.1.D10 .OR. NO.LT.0.D0 .OR.
     *    NO.GT.1.D20 .OR. NH.LT.0.D0 .OR. NH.GT.1.D20 .OR.
     *    NN2.LT.0.D0 .OR. NN2.GT.1.D20 .OR. NO2.LT.0.D0 .OR.
     *    NO2.GT.1.D20 .OR. NHE.LT.0.D0 .OR. NHE.GT.1.D20) THEN
         TNF = 0.0D0
         IER = 3
      ELSE
C
C            NHP - H+ CONCENTRATION
C
         NOP = NE - NHP
         TECUBE = TE**3
C            LEI COEFF. 
         CEI = (3.22D-8*NOP+51.12D-8*NHP)*NE/DSQRT(TECUBE)
         ALOGEI = DLOG(1.D0+TE/TI)
         TEPTI = TE + TI
C        .....lin.....
C            O+ POLARIZATION
         COPN = 6.6D-14*NN2 + 5.8D-14*NO2 + 2.8D-14*NHE
C            H+ POLARIZATION	 
         CHPN = 3.1D-14*NN2 + 2.8D-14*NO2 + 5.5D-14*NHE
C
         TN = TI
         ITER = 0
C
   10    CONTINUE
         TNSAVE = TN
         T1 = TI + TN
         IF (T1.LT.0.0D0) GO TO 60
         SQRTT1 = DSQRT(T1)
         ALOGT1 = DLOG(T1)
         T2 = TI + .063D0*TN
         IF (T2.LT.0.0D0) GO TO 50
         ALOGT2 = DLOG(T2)
         T3 = TN + .063D0*TI
         IF (T3.LT.0.0D0) GO TO 40
         ALOGT3 = DLOG(T3)
C            O+O CHARGE EXCHANGE
         COPO = .00627D-14*NO*SQRTT1
C            H+H CHARGE EXCHANGE
         CONST = .02498D-14*NH
         CHPH = CONST*SQRTT1
C            H+O CHARGE EXCHANGE
         CHPO = .02498D-14*NO*DSQRT(T2)
C        ..... o+h charge exchange coefficient.....
         COPH = CONST*DSQRT(T3)*(5.D0+3.D0*DEXP(-227.8D0/TE)+
     *          DEXP(-325.6D0/TE))/8.D0
C            Coulomb Logarithm from Itikawa (JATP, 1975)
         COULOG = 8.06 + 0.5D0 * DLOG(TECUBE/NE)
C          qopo = (7.47-.206*dlogt1)**2
         QOPO = 58.3D0
C            H+H
         QHPH = (10.2D0-.360D0*ALOGT1)**2
C            H+O	 
         QHPO = (4.26D0-.0869D0*ALOGT2)**2
C            O+H	 
         QOPH = (4.26D0-.0869D0*ALOGT3)**2
C            E-I HEAT EXCHANGE	 
         FEI = CEI*COULOG
C            O+ N HEAT EXCHANGE	 
         FOPN = (COPN+COPO*QOPO+COPH*QOPH)*NOP
C            H+ N HEAT EXCHANGE	 
         FHPN = (CHPN+CHPH*QHPH+CHPO*QHPO)*NHP
         FACTOR = FEI/(FOPN+FHPN)
         TN = (1.D0+FACTOR)*TI - FACTOR*TE
         DTEST = ABS(TN-TNSAVE)
         IF (DTEST.GE.DIVRG) GO TO 30
         ITER = ITER + 1
         IF (ITER.GT.NITER) GO TO 20
         IF (DTEST.GT.CONVRG) GO TO 10
         TNF = TN
         GO TO 70
   20    CONTINUE
         TN = 0.0D0
         IER = 2
         GO TO 70
   30    CONTINUE
         TN = 0.0D0
         IER = 1
         GO TO 70
   40    CONTINUE
         TN = 0.0D0
         IER = 1
         GO TO 70
   50    CONTINUE
         TN = 0.0D0
         IER = 1
         GO TO 70
   60    CONTINUE
         TN = 0.0D0
         IER = 1
      END IF
   70 CONTINUE
      RETURN
      END
