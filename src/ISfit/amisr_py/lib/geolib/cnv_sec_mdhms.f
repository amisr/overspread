
      SUBROUTINE CNV_SEC_MDHMS(YR,MO,DAY,HR,MINUT,SC,T0)
C
C     $Id: cnv_sec_mdhms.f,v 1.1 2010/04/21 14:08:38 brideout Exp $
C
C     cnv_sec_mdshm is a subroutine to return month, day, hour,
C     minute and second given input year and number of seconds
C
C     Written because we didn't have SuperDARN's version
C
C     argument declaration
      INTEGER*4 YR,MO,DAY,HR,MINUT,SC,T0

C     local varaibles
      INTEGER*4 DAYNO,SEC_LEFT

      DAYNO = 1 + (T0 / 86400)

      CALL CALNDR(YR,DAYNO,DAY,MO,IER)

      SEC_LEFT = T0 - (DAYNO-1)*86400

      HR = SEC_LEFT/3600

      SEC_LEFT = SEC_LEFT - HR*3600

      MINUT = SEC_LEFT/60

      SEC_LEFT = SEC_LEFT - MINUT*60

      SC = SEC_LEFT

      return
      end
