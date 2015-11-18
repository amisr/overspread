C     Written by Shunrong Zhang
C     Imported into Madrigal from modelRecovery/fullAnalytic/
C     basis3.h on 10/21/2005 - Was revision 1.1.1.1
C
C     $Id: basis3.h,v 1.1 2005/10/21 15:34:10 brideout Exp $
C
C     ..
C     .. Common blocks ..
      PARAMETER (NDDIM=90000)
      COMMON /BASIS3/X1,X2,X3,T1,T2,T3,N1,N2,N3,IX1,IX2,IX3
      INTEGER N1,N2,N3
      DOUBLE PRECISION T1(NDDIM),T2(NDDIM),T3(NDDIM)
      DOUBLE PRECISION X1(NDDIM),X2(NDDIM),X3(NDDIM)
      INTEGER IX1(NDDIM),IX2(NDDIM),IX3(NDDIM)
