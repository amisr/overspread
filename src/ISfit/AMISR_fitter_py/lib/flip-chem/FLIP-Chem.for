C.................... FLIP-Chem.FOR .......................
C.... This is the test driver for the simple chemical model for ion densities.
C.... Developed by P. Richards, September 2009
C.... The chemistry is from the FLIP model and reproduces FLIP densities very
C.... This program uses several modules
C.... well.
C.... CHEMION is the main control routine.  
C.... The following files contain code that is needed by CHEMION
C....   They are from the FLIP model and not well documented.
C.... RSPRIM.FOR contains routines for photoionization
C.... RATES.FOR contains the reaction rates
C.... PESIMP.FOR is a simple photoelectron flux calculation
C.... KEMPRN.FOR contains the modules for calculating the chemical   
C....   equilibrium densities from the production and loss rates 
C.... The following files supply the F10.7, Ap indices, and neutral 
C.... densities and can be substituted by the user
C.... MSIS00.FOR is the NRLMSISE-00 model
C.... OPEN-FILE.FOR is used for associating file names with FORTRAN units.
C....    This is done in the FLIP-Chem.bat file
C.... FLIPAP.FOR is a routine for retrieving F10.7 and AP from F10KP-68.DDD
C.... which contains the F10.7 and Kp indices since 1968. FLIPAP converts Kp
C....  to Ap for MSIS.
C.... *.DDD files are permanent data files. *.txt files are temporary data files
C....   and can always be deleted without any problems
C.... The EUVAC model is used for solar EUV irradiances
C....
      IMPLICIT NONE
      INTEGER I             !.. loop control variables
      INTEGER ISW           !.. MSIS switch parameter
      INTEGER YYYYDDD       !.. Day of year in form YYYYDDD
      INTEGER JPRINT        !.. Turns on printing of production and loss
      INTEGER INEWT         !.. Signifies that Newton procedure fails (=1)
      REAL UTHRS            !.. Universal time in hours
      REAL NE               !.. Electron density (cm-3)
      REAL N2D              !.. N(2D) density (cm-3)
      REAL TE,TI            !.. Electron and ion temperature (K)
	REAL SUMIONS          !.. Sum of the ion densities (cm-3)
      REAL RATOX            !.. Ratio of atomic to molecular ions
      REAL TSTOP            !.. Used in FLIP = total length of run
      REAL D(9),T(2)        !.. MSIS densities (cm-3) and temperatures (K)
      REAL ENE,ETI          !.. Radar errors for [e], Ti
      REAL UTSEC            !.. Universal time in seconds
      REAL AP(7),F107,F107A !.. Magnetic and solar activity indices
      REAL AP3HR(8001)      !.. FLIP Array for holding up to one one year of Ap
      REAL ALT,GLATD,GLOND  !.. Altitude(km), latitude(D), longitude(D)
      REAL LTHRS,SZAD,SZAR  !.. Local time and solar zenith angle(D,R)
      REAL SOLDEC           !.. Solar declination
      REAL SW,SWC           !.. MSIS switches
	REAL OXN,O2N,N2N,HEN  !.. MSIS densities (cm-3)
	REAL N4S              !.. N4S should use 0.5*MSIS N density (cm-3)
      REAL TN               !.. MSIS temperature (K)
      REAL OXPLUS,O2PLUS    !.. O+ and O2+ densities (cm-3)
      REAL NOPLUS,N2PLUS    !.. NO+ and N2+ densities (cm-3)
      REAL NNO,USER_NO      !.. Calculated and user specified NO density (cm-3)
      REAL NPLUS,USER_NPLUS !.. Calculated and user specified N+ density (cm-3)

      COMMON/CSW/SW(25),ISW,SWC(25)  !.. Switches for MSIS model
      DATA ISW/1/, SW/25*1.0/        !.. Set switches for MSIS model
      DATA F107,F107A/72.0,81.0/, AP/7*10.0/, AP3HR/-1,8000*0/ 

      DATA JPRINT/1/    !.. Put =1/0 to turn on/off printing production and loss
      !.. User specified NO and N+ densities if positive. 
      DATA USER_NO,USER_NPLUS/-1.0,-1.0/   !.. Make negative to calculate

      WRITE(6,'(/A/)') '   ....... FLIP-Chem.FOR ......'

      CALL OPEN_FILE()      !... open files assigned in batch (.bat) file

      READ(5,*) JPRINT      !... get the print switch from the batch file

      !.. Pick up UT from PFISR Radar data file. It keeps 
      !.. looping until a valid data line is found
 10   READ(1,*,ERR=10,END=90) YYYYDDD,UTHRS,GLATD,GLOND

      UTSEC=UTHRS*3600   !.. Universal time in seconds

      !.. Given date and time, get the local time, solar zenith angle, 
      !..and solar declination
      CALL GETLTSZA(YYYYDDD,UTSEC,GLATD/57.29578,GLOND,LTHRS,SZAR,
     >   SOLDEC)
	SZAD=SZAR*57.29578   !.. convert SZA to degrees

      !.. FLIP routine to get the F10.7 and seven 3-hr Ap values
      CALL FINDAP(YYYYDDD,UTHRS,1.0,AP,AP3HR,SW,10.0,F107,F107A)

      WRITE(6,'(A,/I8,22F8.2)') 
     >  ' YYYYDDD   UTHRS   LTHRS   SZAD    GLATD  GLOND',
     >   YYYYDDD,UTHRS,LTHRS,SZAD,GLATD,GLOND
      WRITE(6,'(/A,/22F6.1)') ' F10.7  F107   <.....AP(1..7)..........',
     >   F107,F107A, (AP(I),I=1,7)
      WRITE(6,*) '  '
      
      !.. Pick up first line of densities from PFISR data file. It keeps 
      !.. looping until a valid data line is found
 15   READ(1,*,ERR=15,END=90) ALT,NE,ENE,TI,ETI,TE

      !************* Begin Processing loop here
      DO I=1,999
        NE=1.0E-6*NE     !.. convert electron density to cm-3

        !D(1)=-1000       !.. Turn on to to specify TINF in MSIS
        !.. Call the MSIS model to get neutral densities and temperatures.
        CALL GTD7(YYYYDDD,UTSEC,ALT,GLATD,GLOND,LTHRS,F107A,F107,AP,48,
     >    D,T)

        !.. This block transfers MSIS densities and temperatures to explicitly 
        !.. named variables to make things clearer
        HEN=D(1)                         !.. He density
        OXN=D(2)                         !.. O density
        N2N=D(3)                         !.. N2 density
        O2N=D(4)                         !.. O2 density
        TN=T(2)                          !.. MSIS Neutral temperature
        IF(ALT.LT.100.OR.TE.LT.TN) TE=TN !.. trap bad Te values 
        IF(ALT.LT.100.OR.TI.LT.TN) TI=TN !.. trap bad Ti values 
        !.. Halve MSIS N(4S) density because MSIS tends to overestimate
        N4S=0.5*D(8)

        !.. Get the calculated ion densities
        CALL CHEMION(JPRINT,YYYYDDD,ALT,GLATD,GLOND,AP,F107,F107A,TE,
     >    TI,TN,OXN,O2N,N2N,HEN,USER_NO,N4S,NE,USER_NPLUS,LTHRS,UTSEC,
     >    SZAD,
     >    OXPLUS,O2PLUS,NOPLUS,N2PLUS,NPLUS,     !.. Output params
     >    NNO,N2D,INEWT)                         !.. Output params

        !.. put the header in the main output file
        IF(I.EQ.1) WRITE(3,190) 
 190    FORMAT(' INEWT  ALT   UTHRS  LTHRS   SZA     TE    TI     TN'
     >   ,7X,'NE      SUMIONS   OXPLUS    O2PLUS    NOPLUS    N2PLUS'
     >   ,4X,'NPLUS       OX        O2        N2       NNO       N4S'
     >   ,6X,'RATOX    N2D')

        !.. Output the calculated densities and temperatures
        SUMIONS=OXPLUS+NOPLUS+O2PLUS+N2PLUS+NPLUS
        RATOX=OXPLUS/(NOPLUS+O2PLUS+N2PLUS)
        WRITE(3,'(I3,2X,4F7.2,3F7.1,1P,22E10.2)') INEWT,ALT,UTHRS,
     >    LTHRS,SZAD,TE,TI,TN,NE,SUMIONS,OXPLUS,O2PLUS,NOPLUS,
     >    N2PLUS,NPLUS,OXN,O2N,N2N,NNO,N4S,RATOX,N2D

        !.. Pick up next line of densities from the data file
        READ(1,*,ERR=90,END=90) ALT,NE,ENE,TI,ETI,TE
      ENDDO
      !************* End loop here

 90   CONTINUE
      STOP
      END

C:::::::::::::::::::::::::::: CHEMION :::::::::::::::::::::::::::
C... This routine was written by Phil Richards in August 2009
C... It returns O+, O2+, NO+, N2+, N+, NO, and N(2D) densities.These
C... densities generally agree well with the FLIP model densities.
C... The electron density must be specified as an input parameter.
C... All the densities except O+ are calculated from chemical equilibrium.
C... O+ is calculated using a Newton iterative procedure so that the total 
C... ion density matches the input electron density.
C... N+ and NO densities can either be user specified or calculated by the model.
C... Because of the neglect of diffusion, this model will generally 
C... underestimate N+ at most altitudes. Similarly, NO will be very good except 
C... below about 130 km where it will be underestimated due to neglect of diffusion. 
C... There is an artificial floor on the NO density to prevent it from getting too 
C... low below 130 km.
C... If the Newton procedure fails to converge, all ions including O+ are 
C... calculated from chemical equilibrium and then normalized to reproduce 
C... the input electron density. This generally works well.
C... The Newton procedure usually works if the total calculated molecular ion 
C... densities do not approach the input electron density. Difficulties are most   
C... likely to happen below ~150 km and especially at night. A Newton solution is 
C... usually found when the O+ density is not too much smaller than the modeled 
C... molecular ion density.
C... The EUVAC model is used for solar EUV irradiances
      SUBROUTINE CHEMION(JPRINT,  !.. Input: Turn file output on or off
     >                  YYYYDDD,  !.. Input: Day of year in the form YYYYDDD
     >          ALT,GLATD,GLOND,  !.. Input: Altitude(km), latitude(D), longitude(D)
     >            AP,F107,F107A,  !.. Input: Magnetic and solar activity indices
     >                 TE,TI,TN,  !.. Input: Electron and ion temperatures
     >          OXN,O2N,N2N,HEN,  !.. Input: O, O2, N2, and He densities (cm-3)
     >                  USER_NO,  !.. Input: User specified NO density (cm-3)
     >                      N4S,  !.. Input: N4S should be 0.5*MSIS N density (cm-3)
     >                       NE,  !.. Input: electron density (cm-3)
     >               USER_NPLUS,  !.. Input: User specified N+ density (cm-3)
     >         LTHRS,UTSEC,SZAD,  !.. Input: LT(hrs), UT(sec) and solar zenith angle(D)
     >     OXPLUS,O2PLUS,NOPLUS,  !.. OUTPUT: O+, O2+, NO+ densities (cm-3)
     >             N2PLUS,NPLUS,  !.. OUTPUT: N2+ and N+ densities (cm-3)
     >                  NNO,N2D,  !.. OUTPUT: NO and N(2D) density (cm-3)
     >                    INEWT)  !.. OUTPUT: Newton procedure fails if INEWT=0
      IMPLICIT NONE
      INTEGER I,J,K,ITERS   !.. loop control variables
      INTEGER YYYYDDD       !.. Day of year in the form YYYYDDD
      INTEGER IRATS         !.. Switch for different rates
      INTEGER INEWT         !.. Signifies when the Newton procedure fails
      INTEGER JPRINT        !.. Turns on printing of production and loss
	INTEGER ITS,JITER     !.. Variables for Newton procedure
      REAL TE,TN,TI         !.. Electron and ion temperatures
      REAL SECPRD(3,6)      !.. Secondary ion production rates
	!.. Geophysical parameters
      REAL UTSEC,F107,F107A,ALT,GLATD,GLOND,LTHRS,SZAD,AP(7)
	!.. Measured H+, He+, O+, N2+, NO+, O2+, N+, RPA ion density
      REAL HEPLUS,OXPLUS,N2PLUS,NOPLUS,O2PLUS,NPLUS,USER_NPLUS
      !.. O2,O,N2,NO,He,N4S, user specified NO
      REAL O2N,OXN,N2N,NNO,HEN,N4S,USER_NO
      !.. Ne, N(2P),N(2D),O+(2P),O+(2D) densities
      REAL NE,N2P,N2D,OP2D,OP2P
	!.. Total (photon & photoel) production rates O+(4S),O+(2P),O+(2D),O2+
      REAL TPROD1,PDISOP,TPROD2,TPROD3,TPROD5
	!.. Total Production rates from all sources for NO+, O2+, 
      REAL TPNOP,O2PPROD
	!.. Production rates hv(e*)+N2->N+, hv+N->N+, Lyman-a -> NO+ 
      REAL DISNP,PHOTN,PLYNOP
      REAL PSEC                     !.. generic PE production
      REAL RTS(99)                  !.. Reaction rates array
	REAL SECPN2PLUS,EUVN2PLUS     !.. N2+ total production
      REAL H,DEX,FEX(2)             !.. used in Newton solver
      REAL SUMIONS                  !.. Sum of the major ions
      REAL PNO,LNO,PDNOSR           !.. Production and loss of NO
	REAL N2A                      !.. N2(A) density    
      REAL VCON                     !.. FLIP N2(v) factor. Not used here
      REAL DISN2D,UVDISN,PN2D,LN2D  !.. Production and loss of N(2D)
	REAL ALTCHEM                  !.. altitude for chemistry calculation
      REAL N2APRD                   !.. PE production rate of N2(A)
      REAL PN4S,LN4S,DISN4S
      REAL OXPLUSAVE

	!.. various ionization and excitation rates by EUV and PE
      REAL EUVION,PEXCIT,PEPION,OTHPR2
      COMMON/EUVPRD/EUVION(3,12),PEXCIT(3,12),PEPION(3,12),OTHPR2(6)

      !.. initialize parameters
	DATA VCON/1.0/K/0/
      DATA PNO,LNO,PDNOSR,PLYNOP,N2A/5*0.0/
      DATA DISN2D,UVDISN/0.0,0.0/
	DATA HEPLUS/0.0/

      ALTCHEM=150  !.. Initial altitude for O+ for imposing chemistry
      JITER=0      !.. Counts the number of Newton iterations
      N2P=0.0      !.. N(2P) density

      CALL RATS(0,TE,TI,TN,RTS)  !.. Get the reaction rates

      !.. PRIMPR calculates solar EUV production rates
      CALL PRIMPR(1,ALT,OXN,N2N,O2N,HEN,SZAD*0.01745,TN,1,F107,F107A,
     >   AP,YYYYDDD,UTSEC,GLATD,GLOND,LTHRS,N4S)

      !.. Calculate secondary Production from photoelectrons
      CALL SECIPRD(ALT,SZAD,F107,F107A,AP,YYYYDDD,UTSEC,GLATD,GLOND,
     > LTHRS,TE,TN,OXN,O2N,N2N,NE,SECPRD,N2APRD)

   
      !**********  Come back here if Newton fails
 5    CONTINUE 
        HEPLUS=0.0
        OXPLUS=0.0
        N2PLUS=0.0
        NOPLUS=0.0
        O2PLUS=0.0
        NPLUS=0.0
        N2P=0.0
        N2D=0.0
        OP2D=0.0
        OP2P=0.0
        N2A=0.0

      !.. Iterate through chemistry twice in case O+ is chemical equilibrium
      DO ITERS=1,2
        !.. k counts number of iterations for printing headers in routines
        K=K+1
        !.. O+(2P) Calculate and print densities, production, loss
        PSEC=SECPRD(1,3)           !.. Photoelectron production
        TPROD3=EUVION(1,3)+PSEC    !.. Add EUV and photoelectrons
        CALL COP2P(JPRINT,7,K,ALT,RTS,OXN,O2N,N2N,NE,OP2P,TPROD3,PSEC
     >    ,HEPLUS,N4S,NNO,TE)

        !.. O+(2D) Calculate and print densities, production, loss
        PSEC=SECPRD(1,2)           !.. Photoelectron production
        TPROD2=EUVION(1,2)         !.. EUV
        CALL COP2D(JPRINT,8,K,ALT,RTS,OXN,O2N,N2N,NE,OP2D,TPROD2,OP2P
     >    ,HEPLUS,N4S,NNO,PSEC)

        !.. O+(4S) Calculate and print densities, production, loss. 
        TPROD1=EUVION(1,1)
        PDISOP=EUVION(2,4)+EUVION(2,5)+SECPRD(2,4)+SECPRD(2,5)
        CALL COP4S(JPRINT,4,K,ALT,RTS,OXN,O2N,N2N,NE,OXPLUS,TPROD1,OP2D
     >    ,OP2P,SECPRD(1,1),PDISOP,N2PLUS,N2D,NNO,1.0,HEPLUS)

        CALL CN2A(JPRINT,14,K,ALT,RTS,OXN,O2N,N2N,NE,N2A,N2APRD,0.0,
     >     0.0,0.0)

        !.. N(2D) Calculate and print densities, production, loss. 
        DISN2D=0.2*SECPRD(3,1)
        CALL CN2D(JPRINT,16,K,ALT,RTS,OXN,O2N,N2N,NOPLUS,NE,PN2D,LN2D
     >    ,N2PLUS,DISN2D,UVDISN,NPLUS,N2P,N2D,OXPLUS,NNO,N2A)
        N2D=PN2D/LN2D

        !.. N2+ Calculate and print densities, production, loss. 
        CALL CN2PLS(JPRINT,9,K,ALT,RTS,OXN,O2N,N2N,NE,N2PLUS,EUVION(3,1)
     >   ,EUVION(3,2),EUVION(3,3),SECPRD(3,1),SECPRD(3,2),SECPRD(3,3)
     >   ,OP2D,OP2P,HEPLUS,NPLUS,NNO,N4S)

        !.. N+ Calculate and print densities, production, loss. 
	  !.. Note that N(2D) is turned off in N+ solution 
        DISNP=EUVION(3,4)+EUVION(3,5)+EUVION(3,6)+SECPRD(3,6)
        PHOTN=OTHPR2(3)  !.. N+ prod
        CALL CNPLS(JPRINT,10,K,ALT,RTS,OXN,O2N,N2N,NE,DISNP,NPLUS,
     >    OXPLUS,N2D,OP2P,HEPLUS,PHOTN,O2PLUS,N4S,OP2D,N2PLUS,NNO)
        IF(USER_NPLUS.GT.0) NPLUS=USER_NPLUS  !.. User specified N+

        !.. NO Calculate and print densities, production, loss.
        CALL CNO(JPRINT,15,K,ALT,RTS,OXN,O2N,N2N,NE,PNO,LNO
     >    ,N2D,N4S,N2P,NNO,O2PLUS,OXPLUS,OTHPR2(2),OTHPR2(1),N2A,NPLUS)
        
        NNO=PNO/LNO     !.. NO chemical equilibrium density
        !.. Set a floor on NO density, which is needed below ~150 km at night 
        IF(NNO.LT.1.0E8*EXP((100-ALT)/20)) NNO=1.0E8*EXP((100-ALT)/20)
        IF(USER_NO.GT.1.0) NNO=USER_NO  !.. substitute user specified value

        !.. NO+ Calculate and print densities, production, loss. 
        CALL CNOP(JPRINT,11,K,ALT,RTS,OXN,O2N,N2N,NE,TPNOP,NOPLUS,OXPLUS
     >    ,N2PLUS,O2PLUS,N4S,NNO,NPLUS,N2P,PLYNOP,VCON,N2D,OP2D)

        !.. O2+ Calculate and print densities, production, loss. 
        !.. EUV + PE production
        TPROD5=EUVION(2,1)+EUVION(2,2)+EUVION(2,3)+SECPRD(2,1)+
     >       SECPRD(2,2)+SECPRD(2,3)
        CALL CO2P(JPRINT,12,K,ALT,RTS,OXN,O2N,N2N,NE,O2PPROD
     >    ,O2PLUS,TPROD5,OXPLUS,OP2D,N2PLUS,NPLUS,N4S,NNO,OP2P)
      ENDDO

      !.. This section for chemical equilibrium densities for all species 
      !.. including O+. It is used when the Newton procedure fails to get O+
      !.. Don't bother if molecular ions greater than Ne/2
      !.. It increments ALTCHEM to force this action. The ion densities are 
      !.. normalized to the input NE. N+ is excluded in case it is user specified
      INEWT=1
      SUMIONS=OXPLUS+NOPLUS+O2PLUS+NPLUS
      IF(ALT.LT.ALTCHEM.OR.NOPLUS+O2PLUS.GT.0.85*NE) THEN
         OXPLUS=OXPLUS*NE/SUMIONS
         NOPLUS=NOPLUS*NE/SUMIONS
         O2PLUS=O2PLUS*NE/SUMIONS
         NPLUS=NPLUS*NE/SUMIONS
         INEWT=0
         RETURN
      ENDIF

      !.. Newton solution for O+ density given the electron density (NE)
      !.. Go through twice to set up the derivative (F(x+h)-F(x))/h
      !.. First O+ guess for Newton. This is important for high altitudes
      !.. because Newton may converge on first try.
      OXPLUSAVE=OXPLUS
      IF(NE-NOPLUS-O2PLUS.GT.100) OXPLUS=NE-NOPLUS-O2PLUS
      !IF(SZAD.GT.90) OXPLUS=NE  !.. first guess at night    
 9    DO ITS=1,2
        IF(ITS.EQ.1) H=OXPLUS*0.0001         !.. increment for dn/dt
        IF(ITS.EQ.2) OXPLUS=OXPLUS+H       !.. increment N

        !.. N+ Calculate and print densities, production, loss. 
        CALL CNPLS(JPRINT,10,K,ALT,RTS,OXN,O2N,N2N,NE,DISNP,NPLUS,
     >    OXPLUS,N2D,OP2P,HEPLUS,PHOTN,O2PLUS,N4S,OP2D,N2PLUS,NNO)
        IF(USER_NPLUS.GT.0) NPLUS=USER_NPLUS  !.. User specified N+

        !.. N2+ Calculate and print densities, production, loss. 
        CALL CN2PLS(JPRINT,9,K,ALT,RTS,OXN,O2N,N2N,NE,N2PLUS,EUVION(3,1)
     >    ,EUVION(3,2),EUVION(3,3),SECPRD(3,1),SECPRD(3,2),SECPRD(3,3)
     >    ,OP2D,OP2P,HEPLUS,NPLUS,NNO,N4S)

        !.. NO+ Calculate and print densities, production, loss. 
        CALL CNOP(JPRINT,11,K,ALT,RTS,OXN,O2N,N2N,NE,TPNOP,NOPLUS,OXPLUS
     >   ,N2PLUS,O2PLUS,N4S,NNO,NPLUS,N2P,PLYNOP,VCON,N2D,OP2D)

        !.. O2+ Calculate and print densities, production, loss. 
        !.. EUV + PE production
        TPROD5=EUVION(2,1)+EUVION(2,2)+EUVION(2,3)+SECPRD(2,1)+
     >     SECPRD(2,2)+SECPRD(2,3)
        CALL CO2P(JPRINT,12,K,ALT,RTS,OXN,O2N,N2N,NE,O2PPROD
     >    ,O2PLUS,TPROD5,OXPLUS,OP2D,N2PLUS,NPLUS,N4S,NNO,OP2P)

        !.. N(2D) Calculate and print densities, production, loss. 
        CALL CN2D(JPRINT,16,K,ALT,RTS,OXN,O2N,N2N,NOPLUS,NE,PN2D,LN2D
     >     ,N2PLUS,DISN2D,UVDISN,NPLUS,N2P,N2D,OXPLUS,NNO,N2A)
        N2D=PN2D/LN2D

        !.. calculation of F(x) from the total ion concentration
        FEX(ITS)=OXPLUS+NOPLUS+O2PLUS+N2PLUS+NPLUS-NE
      ENDDO
        
      !.. Test for convergence and add increment to O+ if not
      JITER=JITER+1          !.. for stopping the iterations
      DEX=(FEX(2)-FEX(1))/H
      OXPLUS=OXPLUS-H-FEX(1)/DEX

      !.. If Newton fails, go back and calculate O+ chem equilibrium
      IF(JITER.GT.6.OR.OXPLUS.LT.0.0.OR.OXPLUS.GT.1.0E7) THEN
        ALTCHEM=ALT+1  !.. forces chemical equilibrium densities
        GOTO 5         !.. Go back to chemical eqilibrium
      ENDIF     

      !.. Test for convergence
      SUMIONS=OXPLUS+NOPLUS+O2PLUS+N2PLUS+NPLUS
      IF(ABS((NE-SUMIONS)/NE).GT.0.05) GOTO 9

      !.. Normalize ion densities to the input electron density
      OXPLUS=OXPLUS*NE/SUMIONS
      NOPLUS=NOPLUS*NE/SUMIONS
      O2PLUS=O2PLUS*NE/SUMIONS
      NPLUS=NPLUS*NE/SUMIONS

      !.. If O+ a very minor ion, Newton solution may not be good, force  
      !.. chemical equilibrium solution instead
      IF(OXPLUS/SUMIONS.LT.0.1) THEN
        ALTCHEM=ALT+1  !.. forces chemical equilibrium densities
	  GOTO 5         !.. Go back to chemical eqilibrium
      ENDIF

      RETURN
      END
