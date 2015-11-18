C     A simple test prgram that calls IRI_SUB

C     $Id: testIRI.f,v 1.1 2010/12/16 14:12:32 brideout Exp $

C     Declare variables
      INTEGER JF(30),JMAG,IYYYY,MMDD,IAP3(13)
      REAL ALATI,ALONG,DHOUR,HEIBEG,HEIEND,HEISTP
      REAL F107,OUTF(20,100),OARR(50)

      call initialize

C     Set hard-coded values
      DATA JF/1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,
     * 1,1,1,1,1,0,1,0,1,1,1,1,0,0,0/
      DATA IAP3/5,7,27,32,67,32,22,22,27,22,39,56,56/
      JMAG=0
      IYYYY=2000
      MMDD=101
      ALATI=42.599998
      ALONG=288.50000
      DHOUR=25.0
      HEIBEG=270.0
      HEIEND=271.0
      HEISTP=100.0
      F107=1355.9999
      

      call IRI_SUB(JF,JMAG,ALATI,ALONG,IYYYY,MMDD,
     *           DHOUR,HEIBEG,HEIEND,HEISTP,OUTF,OARR,
     *	         IAP3,f107)

      print *,'First call: ', OUTF(2,1),OUTF(3,1),OUTF(4,1)
      

      call IRI_SUB(JF,JMAG,ALATI,ALONG,IYYYY,MMDD,
     *           DHOUR,HEIBEG,HEIEND,HEISTP,OUTF,OARR,
     *	         IAP3,f107)

      print *,'Second call: ', OUTF(2,1),OUTF(3,1),OUTF(4,1)
      END
