C     $Id: tmaddataF77_64.f,v 1.1 2009/01/21 20:37:32 brideout Exp $
C
C     This sample program prints out all requested parameters
C     from a madrigal file for given filters.  The requested
C     parameters can be either measured or derived - the API
C     hides these details from the user.
C
C     Note that RFMADD must be declared INTEGER or INTEGER*8, 
C     depending on whether 32-bit or 64-bit platform.  This
C     version is used with 64 bit machines.
C     
      PROGRAM TMADDATA
C
C     .. Local Scalars ..
      INTEGER*8 RFMADD
      INTEGER NUMREC
C     .. External Subroutines ..
      EXTERNAL CRMADD,GTNROW, GTMADD, FRMADD, STALOC
C     ..
C     .. Local Arrays ..
      CHARACTER*128 FILEC, MDROOT
      CHARACTER PARMS*100
      CHARACTER FLTSTR*1000
C     .. Local variables ..
      INTEGER STATUS
      DOUBLE PRECISION DATROW(100)
      DOUBLE PRECISION SLATGD,SLON,SALTGD,SLATGC,SR
      INTEGER REC, NROW, ROW, ISTAT, KINST
C     ..
C     ..
      RFMADD = 0
      NUMREC = 0
C     the requested madrigal file
      CALL GTROOT(MDROOT,LEN)
      FILEC=MDROOT(:LEN)//'/experiments/1998/mlh/20jan98/mil980120g.002'
C     the requested parms (measured or derived)
      PARMS = 'gdalt,ti,kp,nel'
C     the desired filters (exactly like isprint command line)
      FLTSTR = 'z=1000, filter=range,2500, filter=ti,1000,2000'
      CALL CRMADD(FILEC,PARMS,FLTSTR,RFMADD,NUMREC,STATUS)
      IF (STATUS .NE. 0) THEN
         PRINT*, "Create Maddata failed for ", FILEC
         STOP
      END IF
C     print all records (ignoring formatting)
      DO 30, REC = 1, NUMREC
         CALL GTNROW(RFMADD, REC, NROW) 
         PRINT*,PARMS
C        loop over all rows
         DO 20, ROW=1, NROW
            CALL GTMADD(RFMADD, REC, ROW, DATROW, STATUS)
C           Note that data is stored in datrow in the order of parms
            PRINT "(F10.5)",DATROW(1),DATROW(2),DATROW(3),DATROW(4)
20       CONTINUE
30    CONTINUE
C     free all data
      CALL FRMADD(RFMADD)
C
      KINST = 31
      CALL STALOC(KINST,SLATGD,SLON,SALTGD,SLATGC,SR,ISTAT)
      if (ISTAT .EQ. 0) THEN
         print*, 'Kinst 31:', SLATGD,SLON,SALTGD,SLATGC,SR,ISTAT
      else
         print *, 'now returned error code ', ISTAT, SLATGD
      END IF
      
      CALL STALOC(3331,SLATGD,SLON,SALTGD,SLATGC,SR,ISTAT)
      if (ISTAT .EQ. 0) THEN
         print*, 'Kinst 3331:', SLATGD,SLON,SALTGD,SLATGC,SR,ISTAT
      else
         print*, 'Kinst 3331 failed, as it should'
      END IF
      
      END
