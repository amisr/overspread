C     $Id:
C
      PROGRAM TMADREC
C
C     This program is a simple example illustrating the use of the madrec
C     module from Fortran - see madrecF77.c. This module is appropriate for
C     dealing with Madrigal files - writing them or modifying them.  To deal
C     with Madrigal data at a higher level, where the differences between
C     derived and measured data can be ignored, use maddataF77.c methods.
C
C     This program contain 4 examples:
C        1. Reading an existing madrigal file in sequentially
C        2. Writing a new madrigal file sequentially
C        3. Appending to an existing madrigal file using in mem file
C        4. Searching and summarizing a file in memory
C
C     .. Local Scalars ..
      INTEGER STATUS,NUMREC,LEN
      CHARACTER*128 ERROR,FNAME,MDROOT
      DOUBLE PRECISION PARM, KEY
      INTEGER LPROL, JPAR, MPAR, NROW, KREC, KINST, KINDAT
      INTEGER YEAR1, MONTH1, DAY1, HOUR1, MIN1, SEC1, CSEC1
      INTEGER YEAR2, MONTH2, DAY2, HOUR2, MIN2, SEC2, CSEC2
      INTEGER MADFIL, MADFL1, MADFL2
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION PARM2D(500)
C     ..
C     .. External Functions ..
      INTEGER MDOPEN,MDCLOS
      INTEGER MDREAD,MDISDR
      INTEGER MDWRIT,REWIND
      INTEGER MDCOPY,GRCKEY
      DOUBLE PRECISION GETKEY,GTPMIN,GTPMAX
C     ..
C     .. External Subroutines ..
      EXTERNAL MDGERR,MDG1DP,MDG2DP,MDCREA
      EXTERNAL MDS1DP,MDS2DP,GTROOT
C
      NUMREC = 0      
      CALL GTROOT(MDROOT,LEN)
      FNAME=MDROOT(:LEN)//'/experiments/1998/mlh/20jan98/mil980120g.002'
      ERROR=''
C
C
C     .....Example 1 - read a madrigal file.....
C
C
      PRINT*, "\nExample 1 - read in a madrigal file"
      MADFIL = MDOPEN(1, FNAME)
      IF (MADFIL.LT.0) THEN
         CALL MDGERR(ERROR)
         PRINT*,ERROR
         STOP
      END IF
C     loop through all the records
10    STATUS = MDREAD(MADFIL)
      IF (STATUS.NE.0) THEN
          GOTO 100
      END IF
C     count data records, ignore header or catalog
      IF (MDISDR(MADFIL).EQ.1) THEN
          NUMREC = NUMREC + 1
      END IF
C     print some data for record 5
      IF (NUMREC.EQ.5) THEN
          PRINT*,"The following is data for the 5th data record:"
          CALL MDG1DP(MADFIL,402, PARM)
          PRINT*,"   Pulse length is "
          PRINT "(F10.5)",PARM
          CALL MDG2DP(MADFIL, 550, PARM2D)
          PRINT*,"   The first 4 Ti values are:"
          PRINT "(F10.5)",PARM2D(1),PARM2D(2),PARM2D(3),PARM2D(4)
      END IF
      GOTO 10
C
100   PRINT*,"The number of data records found in file:"
      PRINT"(I6)",NUMREC
C     ..Close the file..
      STATUS = MDCLOS(MADFIL)
C
C
C     ....Example 2: create a new madrigal file ....
C
C
      PRINT*,""
      PRINT*, "Example 2 - create new file tMadrecF77.out"
      MADFIL = MDOPEN(20, 'tMadrecF77.out')
      IF (MADFIL.LT.0) THEN
         CALL MDGERR(ERROR)
         PRINT*,ERROR
         STOP
      END IF
C     create a new record
      LPROL = 16
      JPAR  = 2
      MPAR  = 1
      NROW  = 3
      KREC  = 1002
      KINST = 32
      KINDAT = 3408
      YEAR1 = 2003
      MONTH1 = 3
      DAY1   = 19
      HOUR1  = 1
      MIN1   = 0
      SEC1   = 0
      CSEC1  = 0
      YEAR2 = 2003
      MONTH2 = 3
      DAY2   = 19
      HOUR2  = 1
      MIN2   = 2
      SEC2   = 59
      CSEC2  = 99
      CALL MDCREA(MADFIL,
     *      LPROL, JPAR, MPAR, NROW,
     *      KREC, KINST, KINDAT,
     *      YEAR1, MONTH1, DAY1,
     *      HOUR1, MIN1, SEC1, CSEC1,
     *      YEAR2, MONTH2, DAY2,
     *      HOUR2, MIN2, SEC2, CSEC2)
C     set the two 1D values (pl and systmp)
      PARM = 1.28000e-03
      CALL MDS1DP(MADFIL, 402, PARM, 1)
      PARM = 151.0
      CALL MDS1DP(MADFIL, 482, PARM, 2)
C     set the 1 2D parm (range) with 3 rows
      PARM2D(1)=100.0
      PARM2D(2)=150.0
      PARM2D(3)=200.0
      CALL MDS2DP(MADFIL, 120, PARM2D, 1)
C     write the new record to file
      STATUS = MDWRIT(MADFIL)
      IF (STATUS.NE.0) THEN
         CALL MDGERR(ERROR)
         PRINT*,ERROR
         STOP
      END IF
C     ..Close the file..
      STATUS = MDCLOS(MADFIL)
C
C
C     ....Example 3: Append to an existing madrigal file ....
C
C      
      PRINT*,""
      PRINT*, "Example 3 - append to existing file"
      PRINT*, "  and save as tMadrecF77_append.out"
C     read the old file into memory
      MADFL1 = MDOPEN(50, FNAME)
      IF (MADFL1.LT.0) THEN
         CALL MDGERR(ERROR)
         PRINT*,ERROR
         STOP
      END IF
C     create a new record to append to it
      CALL MDCREA(MADFL1,
     *      LPROL, JPAR, MPAR, NROW,
     *      KREC, KINST, KINDAT,
     *      YEAR1, MONTH1, DAY1,
     *      HOUR1, MIN1, SEC1, CSEC1,
     *      YEAR2, MONTH2, DAY2,
     *      HOUR2, MIN2, SEC2, CSEC2)
C     set the two 1D values (pl and systmp)
      PARM = 1.28000e-03
      CALL MDS1DP(MADFL1, 402, PARM, 1)
      PARM = 151.0
      CALL MDS1DP(MADFL1, 482, PARM, 2)
C     set the 1 2D parm (range) with 3 rows
      PARM2D(1)=100.0
      PARM2D(2)=150.0
      PARM2D(3)=200.0
      CALL MDS2DP(MADFL1, 120, PARM2D, 1)
C     append the new record to the in-memory file
      STATUS = MDWRIT(MADFL1)
      IF (STATUS.NE.0) THEN
         CALL MDGERR(ERROR)
         PRINT*,ERROR
         STOP
      END IF
C     next rewind the in-memory file
      STATUS = REWIND(MADFL1)    
      IF (STATUS.NE.0) THEN
         CALL MDGERR(ERROR)
         PRINT*,ERROR
         STOP
      END IF
C     now we open another file to write the appended file to
      MADFL2 = MDOPEN(20, 'tMadrecF77_append.out')
      IF (MADFL2.LT.0) THEN
         CALL MDGERR(ERROR)
         PRINT*,ERROR
         STOP
      END IF
C     loop through all the in memory records, copy to MADFL2
20    STATUS = MDREAD(MADFL1)
      IF (STATUS.NE.0) THEN
          GOTO 200
      END IF
      STATUS = MDCOPY(MADFL1, MADFL2)
C     write the copied record to file
      STATUS = MDWRIT(MADFL2)      
      GOTO 20  
C    
200   STATUS = MDCLOS(MADFL1)     
      STATUS = MDCLOS(MADFL2)
C
C
C     ....Example 4: Manipulate an in memory file ....
C
C      
      PRINT*,""
      PRINT*, "Example 4: Manipulate an in memory file"
C     read the file into memory, this time with summary info
      MADFIL = MDOPEN(30, FNAME)
      IF (MADFIL.LT.0) THEN
         CALL MDGERR(ERROR)
         PRINT*,ERROR
         STOP
      END IF
      KEY = GETKEY(1998, 1, 20, 15, 0, 0)
      STATUS = GRCKEY(MADFIL,KEY)
      IF (STATUS.NE.0) THEN
         CALL MDGERR(ERROR)
         PRINT*,ERROR
         STOP
      END IF
C     print some data from this record
      PRINT*,"The following are min and max values of Ti in file:"
      PRINT "(F10.5)",GTPMIN(MADFIL,550),GTPMAX(MADFIL,550)
      PRINT*,"The following is data for key 1/20/1998 15:00:"
      CALL MDG1DP(MADFIL,402, PARM)
      PRINT*,"   Pulse length is "
      PRINT "(F10.5)",PARM
      CALL MDG2DP(MADFIL, 550, PARM2D)
      PRINT*,"   The first 4 Ti values are:"
      PRINT "(F10.5)",PARM2D(1),PARM2D(2),PARM2D(3),PARM2D(4)
      STATUS = MDCLOS(MADFIL)
C
      PRINT*,"\nTest complete" 
C
      END
