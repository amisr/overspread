C     $Id: tmaddata_nofile_F77.f,v 1.2 2008/08/18 13:18:38 brideout Exp $
C
C     This sample program prints out all requested parameters
C     from a madrigal data passed in as arguments - no file needed.  The requested
C     parameters can be either measured or derived - the API
C     hides these details from the user.
C     
      PROGRAM TMADDATA_NOFILE
C
C     .. Local Scalars ..
      INTEGER RFMADD, KINST
      DOUBLE PRECISION UT
C     .. External Subroutines ..
      EXTERNAL CRNFMD,GTNROW,GTMADD,FRMADD
C     .. External Functions ..
      DOUBLE PRECISION GETKEY
C     ..
C     .. Local Arrays ..
      CHARACTER*300 ONED, TWOD
      CHARACTER PARMS*100
C     .. Local variables ..
      INTEGER STATUS
      DOUBLE PRECISION DROW(100)
      INTEGER NROW, ROW
C     ..
C     ..
      RFMADD = 0
      NUMREC = 0
      KINST=31
      NROW=0
      UT = GETKEY(1998,1,20,15,0,0)
C     the requested parms 
      PARMS = 'gdlat,glon,gdalt,bmag,kp,sdwht'
C     the one-D data (not used in this example, but can't hurt)
      ONED = 'pl=.001 sn=10'   
C     the two-D data
      TWOD="gdlat=45,45,50,50 glon=20,30,20,30 gdalt=500,500,500,500"
      CALL CRNFMD(PARMS,UT,KINST,ONED,TWOD,RFMADD,NROW)
      IF (NROW .EQ. 0) THEN
         PRINT*, "Create non-file Maddata failed"
         STOP
      END IF
C     print the record (only 1 returned, so its always rec 1)
      PRINT*,PARMS
      CALL GTNROW(RFMADD, 1, NROW) 
C     loop over all rows
      DO 20, ROW=1, NROW
         CALL GTMADD(RFMADD, 1, ROW, DROW, STATUS)
C        Note that data is stored in DROW in the order of parms
         PRINT*,DROW(1),DROW(2),DROW(3),DROW(4),DROW(5),DROW(6)
20    CONTINUE
C     free all data
      CALL FRMADD(RFMADD)
C
      END
