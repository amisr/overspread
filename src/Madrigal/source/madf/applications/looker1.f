C     $Id: looker1.f,v 1.8 2009/01/22 14:22:09 brideout Exp $
C
C     PROGRAM LOOKER1
C
C     JMH - 12/80  HARRIS FORTRAN 66
C     JMH -  3/02  ALL PARAMETERS ENTERED ON COMMAND LINE.
C                  DESIGNED TO BE DRIVEN BY A CGI SCRIPT.
C
C     LOOKER FINDS THE AZIMUTHS AND ELEVATIONS NEEDED TO OBSERVE
C     A SPECIFIED GEOMAGNETIC FIELD LINE.
C
C     e.g.: looker1 1997 42.6 288.5 0.14600 1 10.0 10 1000 0 1000 200
C
C     .. Local Scalars ..
      DOUBLE PRECISION AINC,ALAT,ALAT0,ALON,ALON0,ALT1,ALT2,ALT3,ARAD,
     *                 ARC,ASPCT,AZ,AZ0,DAL,DALAT,DALON,DEC,EL,EL0,
     *                 GCLAT,GCLAT0,GDALT,GDALT0,GDLAT,GDLAT0,GLON,
     *                 GLON0,HALT,P1,P2,P3,P4,P5,P6,P7,P8,P9,PLAT,PLON,
     *                 PR,PR0,PRKM,RANGE,RANGE0,RL,RLATI,RLATM,SALTGD,
     *                 SLATGC,SLATGD,SLON,SR,TM
      INTEGER I,IAL,IAL1,IAL2,IAL3,IER,INIT,ISTOP,ITER,ITMAX,J,K,NAL,
     *        NAZ,NEL,NLN,NLT,NPR,NRN,OPT
      CHARACTER*80 ARG
C     ..
C     ..
C     .. External Subroutines ..
      EXTERNAL CONVRT,COORD,DIPLAT,INVLAT,LINTRA,LOOK,POINT
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,MAX
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION RCOR(32)
C     ..
C     .. Data statements ..
      DATA ITMAX/20/,DAL/.00001D0/
C     ..
C
C     .....GET COMMAND LINE ARGUMENTS.....
      IF (IARGC().NE.14) THEN
         WRITE (6,FMT='(''looker1 requires 14 arguments'')')
         STOP
      END IF
      CALL GETARG(1,ARG)
      READ (ARG,FMT=*) OPT
      CALL GETARG(2,ARG)
      READ (ARG,FMT=*) TM
      CALL GETARG(3,ARG)
      READ (ARG,FMT=*) SLATGD
      CALL GETARG(4,ARG)
      READ (ARG,FMT=*) SLON
      CALL GETARG(5,ARG)
      READ (ARG,FMT=*) SALTGD
      CALL GETARG(6,ARG)
      READ (ARG,FMT=*) P1
      CALL GETARG(7,ARG)
      READ (ARG,FMT=*) P2
      CALL GETARG(8,ARG)
      READ (ARG,FMT=*) P3
      CALL GETARG(9,ARG)
      READ (ARG,FMT=*) P4
      CALL GETARG(10,ARG)
      READ (ARG,FMT=*) P5
      CALL GETARG(11,ARG)
      READ (ARG,FMT=*) P6
      CALL GETARG(12,ARG)
      READ (ARG,FMT=*) P7
      CALL GETARG(13,ARG)
      READ (ARG,FMT=*) P8
      CALL GETARG(14,ARG)
      READ (ARG,FMT=*) P9
C
c
c      write(6,"('opt = ', i4)") opt
C
      IF (OPT.EQ.1) THEN
         P3 = MAX(P3,1.D-2)
         P6 = MAX(P6,1.D-2)
         P9 = MAX(P9,1.D-2)
         CALL CONVRT(1,SLATGD,SALTGD,SLATGC,SR)
         NLT = (P2-P1)/P3 + 1
         NLN = (P5-P4)/P6 + 1
         NAL = (P8-P7)/P9 + 1
         WRITE (6,FMT=
     *'(''     LAT'',
     *  ''     LON'',
     *  ''     ALT'', ''      AZ'', ''      EL'',
     *  ''   RANGE'')')
         DO 30 I = 1,NLT
            DO 20 J = 1,NLN
               DO 10 K = 1,NAL
                  GDLAT = P1 + (I-1)*P3
                  GLON = P4 + (J-1)*P6
                  GDALT = P7 + (K-1)*P9
                  CALL CONVRT(1,GDLAT,GDALT,GCLAT,PR)
                  CALL LOOK(SR,SLATGC,SLON,PR,GCLAT,GLON,AZ,EL,RANGE)
                  WRITE (6,FMT='(6f8.2)') GDLAT,GLON,GDALT,AZ,EL,RANGE
   10          CONTINUE
   20       CONTINUE
   30    CONTINUE
C
      ELSE IF (OPT.EQ.2) THEN
         P3 = MAX(P3,1.D-2)
         P6 = MAX(P6,1.D-2)
         P9 = MAX(P9,1.D-2)
         CALL CONVRT(1,SLATGD,SALTGD,SLATGC,SR)
         NLT = (P2-P1)/P3 + 1
         NLN = (P5-P4)/P6 + 1
         NAL = (P8-P7)/P9 + 1
         WRITE (6,FMT=
     *'(''    ALAT'',
     *''    ALON'',                                                 ''
     *   LAT'',                                                     ''
     *   LON'',                                                     ''
     *   ALT'', ''      AZ'', ''      EL'',                         ''
     * RANGE'')')
         DO 70 I = 1,NLT
            DO 60 J = 1,NLN
               DO 50 K = 1,NAL
                  ALAT0 = P1 + (I-1)*P3
                  ALON0 = P4 + (J-1)*P6
                  GDLAT = ALAT0
                  GLON = ALON0
                  GDALT = 0.D0
                  CALL CONVRT(1,GDLAT,GDALT,GCLAT,PR)
                  ITER = 0
   40             ITER = ITER + 1
                  ISTOP = 0
                  NPR = 0
                  INIT = 0
                  CALL LINTRA(TM,GCLAT,GLON,PR,GDALT,0.D0,PLAT,PLON,
     *                        PRKM,ARC,ARAD,ALAT,ALON,ISTOP,NPR,INIT,
     *                        IER)
                  IF (ALON.LT.0.D0) ALON = ALON + 360.D0
                  DALAT = ALAT - ALAT0
                  DALON = ALON - ALON0
                  GDLAT = GDLAT - DALAT
                  GLON = GLON - DALON
                  CALL CONVRT(1,GDLAT,GDALT,GCLAT,PR)
                  IF (ITER.LT.ITMAX .AND. (ABS(DALAT).GT.DAL.AND.
     *                ABS(DALON).GT.DAL)) GO TO 40
                  GDLAT0 = GDLAT
                  GLON0 = GLON
                  GDALT0 = GDALT
                  GCLAT0 = GCLAT
                  PR0 = PR
                  HALT = P7 + (K-1)*P9
                  ISTOP = -1
                  NPR = 0
                  INIT = 0
                  CALL LINTRA(TM,GCLAT0,GLON0,PR0,GDALT0,HALT,GCLAT,
     *                        GLON,PR,ARC,ARAD,ALAT,ALON,ISTOP,NPR,INIT,
     *                        IER)
                  IF (GLON.LT.0.D0) GLON = GLON + 360.D0
                  CALL LOOK(SR,SLATGC,SLON,PR,GCLAT,GLON,AZ,EL,RANGE)
                  CALL CONVRT(2,GDLAT,GDALT,GCLAT,PR)
                  WRITE (6,FMT='(8f8.2)') ALAT0,ALON0,GDLAT,GLON,GDALT,
     *              AZ,EL,RANGE
   50          CONTINUE
   60       CONTINUE
   70    CONTINUE
C
      ELSE IF (OPT.EQ.3) THEN
         P3 = MAX(P3,1.D-2)
         P6 = MAX(P6,1.D-2)
         P9 = MAX(P9,1.D-2)
         NLT = (P2-P1)/P3 + 1
         NLN = (P5-P4)/P6 + 1
         NAL = (P8-P7)/P9 + 1
         WRITE (6,FMT=
     *'(''     LAT'',                                                ''
     *    LON'', ''     ALT'', ''  AP LAT'', ''  AP LON'', '' L-SHELL'',
     *       '' INV LAT'')')
         DO 100 I = 1,NLT
            DO 90 J = 1,NLN
               DO 80 K = 1,NAL
                  GDLAT = P1 + (I-1)*P3
                  GLON = P4 + (J-1)*P6
                  GDALT = P7 + (K-1)*P9
                  RANGE = -1.0D0
                  CALL COORD(SLATGD,SLON,SR,SLATGC,TM,AZ,EL,RANGE,GDLAT,
     *                       GLON,GDALT,RCOR)
                  ALAT = RCOR(14)
                  ALON = RCOR(15)
                  RL = RCOR(13)
                  RLATI = RCOR(12)
                  ASPCT = RCOR(32)
                  WRITE (6,FMT='(7f8.2)') GDLAT,GLON,GDALT,ALAT,ALON,RL,
     *              RLATI
   80          CONTINUE
   90       CONTINUE
  100    CONTINUE
C
      ELSE IF (OPT.EQ.4) THEN
         P3 = MAX(P3,1.D-5)
         P6 = MAX(P6,1.D-5)
         P9 = MAX(P9,1.D-5)
         NAZ = (P2-P1)/P3 + 1
         NEL = (P5-P4)/P6 + 1
         NRN = (P8-P7)/P9 + 1
         WRITE (6,FMT=
     *'(''      AZ'', ''      EL'', ''   RANGE'', ''     LAT'',
     *   ''     LON'', ''     ALT'', ''  AP LAT'', ''  AP LON'', '' L-SH
     *ELL'',       '' INV LAT'', ''  ASPECT'')')
         DO 130 I = 1,NAZ
            DO 120 J = 1,NEL
               DO 110 K = 1,NRN
                  AZ = P1 + (I-1)*P3
                  EL = P4 + (J-1)*P6
                  RANGE = P7 + (K-1)*P9
                  IF (RANGE.LT.0.001D0) RANGE = 0.001D0
                  CALL CONVRT(1,SLATGD,SALTGD,SLATGC,SR)
                  CALL POINT(SR,SLATGC,SLON,AZ,EL,RANGE,PR,GCLAT,GLON)
                  CALL CONVRT(2,GDLAT,GDALT,GCLAT,PR)
                  CALL COORD(SLATGD,SLON,SR,SLATGC,TM,AZ,EL,RANGE,GDLAT,
     *                       GLON,GDALT,RCOR)
                  ALAT = RCOR(14)
                  ALON = RCOR(15)
                  RL = RCOR(13)
                  RLATI = RCOR(12)
                  ASPCT = RCOR(32)
                  WRITE (6,FMT='(11f8.2)') AZ,EL,RANGE,GDLAT,GLON,GDALT,
     *              ALAT,ALON,RL,RLATI,ASPCT
  110          CONTINUE
  120       CONTINUE
  130    CONTINUE
C
      ELSE IF (OPT.EQ.5 .OR. OPT.EQ.6 .OR. OPT.EQ.7) THEN
C
C        .....ALTITUDES IN TABLE WILL BE MULTIPLES OF A KILOMETER.....
         ALT1 = P4
         ALT2 = P5
         ALT3 = P6
         IAL1 = ALT1 + .5D0
         IAL2 = ALT2 + .5D0
         IAL3 = ALT3 + .5D0
         GO TO (140,160,170) OPT - 4
C
C        .....LOCATE BASE AND APEX COORDINATES OF FIELD LINE.....
  140    AZ0 = P1
         EL0 = P2
         RANGE0 = P3
         IF (RANGE0.LT.0.001D0) RANGE0 = 0.001D0
         CALL CONVRT(1,SLATGD,SALTGD,SLATGC,SR)
c         write(6,"('az0,el0,range0,ial1,ial2,ial3 = ', 3e12.5,3i6)")
c        *      az0,el0,range0,ial1,ial2,ial3
         CALL POINT(SR,SLATGC,SLON,AZ0,EL0,RANGE0,PR,GCLAT,GLON)
         CALL CONVRT(2,GDLAT,GDALT,GCLAT,PR)
  150    ISTOP = 0
         NPR = 0
         INIT = 0
         CALL LINTRA(TM,GCLAT,GLON,PR,GDALT,0.D0,PLAT,PLON,PRKM,ARC,
     *               ARAD,ALAT0,ALON0,ISTOP,NPR,INIT,IER)
         IF (ALON0.LT.0.D0) ALON0 = ALON0 + 360.D0
         ISTOP = -1
         NPR = 0
         INIT = 0
         GDALT0 = 0
         CALL LINTRA(TM,GCLAT,GLON,PR,GDALT,GDALT0,GCLAT0,GLON0,PR0,ARC,
     *               ARAD,ALAT,ALON,ISTOP,NPR,INIT,IER)
         IF (GLON0.LT.0.D0) GLON0 = GLON0 + 360.D0
         CALL CONVRT(2,GDLAT0,GDALT,GCLAT0,PR0)
         GO TO 190
C
  160    GDLAT = P1
         GLON = P2
         GDALT = P3
	 CALL CONVRT(1,SLATGD,SALTGD,SLATGC,SR)
         CALL CONVRT(1,GDLAT,GDALT,GCLAT,PR)
         CALL LOOK(SR,SLATGC,SLON,PR,GCLAT,GLON,AZ0,EL0,RANGE0)
         GO TO 150
C
  170    ALAT0 = P1
         ALON0 = P2
         GDLAT = ALAT0
         GLON = ALON0
         GDALT = 0.D0
         CALL CONVRT(1,GDLAT,GDALT,GCLAT,PR)
         ITER = 0
  180    ITER = ITER + 1
         ISTOP = 0
         NPR = 0
         INIT = 0
         CALL LINTRA(TM,GCLAT,GLON,PR,GDALT,0.D0,PLAT,PLON,PRKM,ARC,
     *               ARAD,ALAT,ALON,ISTOP,NPR,INIT,IER)
         IF (ALON.LT.0.D0) ALON = ALON + 360.D0
         DALAT = ALAT - ALAT0
         DALON = ALON - ALON0
         GDLAT = GDLAT - DALAT
         GLON = GLON - DALON
         CALL CONVRT(1,GDLAT,GDALT,GCLAT,PR)
         IF (ITER.LT.ITMAX .AND. (ABS(DALAT).GT.DAL.AND.
     *       ABS(DALON).GT.DAL)) GO TO 180
         GDLAT0 = GDLAT
         GLON0 = GLON
         GDALT0 = GDALT
         GCLAT0 = GCLAT
         PR0 = PR
         GO TO 190
C
  190    WRITE (6,FMT=9000) SLATGD,SLON,SALTGD
 9000    FORMAT (/,/,/,'   STATION COORDINATES',/,
     *          ' GEODETIC LATITUDE            ',F12.5,/,
     *          ' LONGITUDE                    ',F12.5,/,
     *          ' GEODETIC ALTITUDE            ',F12.5)
         CALL DIPLAT(TM,GDLAT0,GLON0,GDALT0,AINC,DEC,RLATM)
         CALL INVLAT(TM,GDLAT0,GLON0,GDALT0,RL,RLATI)
         WRITE (6,FMT=9010) TM,GDLAT0,GLON0,GDALT0,GCLAT0,PR0,ALAT0,
     *     ALON0,RLATM,RL,RLATI
C
 9010    FORMAT (/,
     *          '   COORDINATES OF BASE OF FIELD LINE (IGRF-95 EPOCH: ',
     *          F7.2,')',/,' GEODETIC LATITUDE            ',F12.5,/,
     *          ' EAST LONGITUDE               ',F12.5,/,
     *          ' GEODETIC ALTITUDE            ',F12.5,/,
     *          ' GEOCENTRIC LATITUDE          ',F12.5,/,
     *          ' DISTANCE FROM CENTER OF EARTH',F12.5,/,
     *          ' APEX LATITUDE                ',F12.5,/,
     *          ' APEX LONGITUDE               ',F12.5,/,
     *          ' DIP LATITUDE                 ',F12.5,/,
     *          ' L                            ',F12.5,/,
     *          ' INVARIANT LATITUDE           ',F12.5)
         WRITE (6,FMT=9020)
 9020    FORMAT (/,3X,'GDLAT',8X,'LON',9X,'ALT',10X,'AZ',10X,'EL',8X,
     *          'RANGE')
C
C        .....DETERMINE AZIMUTH, ELEVATION, RANGE TABLE.....
         DO 200 IAL = IAL1,IAL2,IAL3
            HALT = IAL
            ISTOP = -1
            NPR = 0
            INIT = 0
            IF (IAL.GE.1) THEN
               CALL LINTRA(TM,GCLAT0,GLON0,PR0,GDALT0,HALT,GCLAT,GLON,
     *                     PR,ARC,ARAD,ALAT,ALON,ISTOP,NPR,INIT,IER)
               IF (GLON.LT.0.D0) GLON = GLON + 360.D0
               CALL LOOK(SR,SLATGC,SLON,PR,GCLAT,GLON,AZ,EL,RANGE)
               CALL CONVRT(2,GDLAT,GDALT,GCLAT,PR)
            ELSE
               GDLAT = GDLAT0
               GLON = GLON0
               GDALT = GDALT0
               CALL LOOK(SR,SLATGC,SLON,PR0,GCLAT0,GLON,AZ,EL,RANGE)
            END IF
            WRITE (6,FMT=9030) GDLAT,GLON,GDALT,AZ,EL,RANGE
 9030       FORMAT (1X,F9.5,5F12.5)
  200    CONTINUE
c
      END IF
C
      END
