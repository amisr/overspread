C     Written by Shunrong Zhang
C     Imported into Madrigal from modelRecovery/fullAnalytic/
C     basis4.h on 10/21/2005 - Was revision 1.2
C
C     $Id: basis4.h,v 1.1 2005/10/21 15:34:11 brideout Exp $
C
C     ..
C     .. Common blocks ..
      PARAMETER (NDDIM=60000)
      COMMON /BASIS4/X1,X2,X3,X4,T1,T2,T3,T4,N1,N2,N3,N4,IX1,IX2,IX3,IX4
      INTEGER N1,N2,N3,N4
      DOUBLE PRECISION T1(NDDIM),T2(NDDIM),T3(NDDIM),T4(NDDIM)
      DOUBLE PRECISION X1(NDDIM),X2(NDDIM),X3(NDDIM),X4(NDDIM)
      INTEGER IX1(NDDIM),IX2(NDDIM),IX3(NDDIM),IX4(NDDIM)
