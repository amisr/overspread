C f77 -o testISRIM testISRIM.f tb3D4D.f isrim.f basis.f
C
      PROGRAM TESTISRIM
C     Written by Shunrong Zhang
C     Imported into Madrigal from modelRecovery/fullAnalytic/
C     testISRIM.f on 10/21/2005 - Was revision 1.1
C
C     Modified to remove user input - kinst and ipar hard-coded
C
C     $Id: testISRIM.f,v 1.1 2005/10/21 15:34:12 brideout Exp $
C
C     test isrim
C
C     .. Local Scalars ..
      DOUBLE PRECISION OUTPUT(10),Y,DAYNO,LT,Z,LA,F107,AP
	dayno=123d0
	lt=12d0
	z=300d0
	f107=135d0
	ap=15d0
	kinst=31
	ipar=1
	la=55d0
        CALL ISRIM(KINST,DAYNO,LT,Z,LA,F107,AP,IP,OUTPUT)
        Y = OUTPUT(1)
        IF (IP.EQ.1) Y = DLOG10(DABS(OUTPUT(1)))
	write(*,*)KINST,DAYNO,LT,Z,LA,F107,AP,IP,output(1),output(4)
      STOP
      END
