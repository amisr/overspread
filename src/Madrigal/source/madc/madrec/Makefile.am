# $Id: Makefile.am,v 1.1 2009/01/06 21:49:30 brideout Exp $
#
# Makefile.am for madrec library and test programs
#

AM_CFLAGS = -I../include

lib_LTLIBRARIES = libmadrec.la


# ----------------------------------------------------------------------
#       Madrec library source files
# ----------------------------------------------------------------------

libmadrec_la_SOURCES = madrec.c cedar.c cedarIO.c date.c geometry.c select.c \
          maddata.c madDeriveEngine.c madDeriveMethods.c solpos.c \
          madrecF77.c maddataF77.c 
	  
# installed test program testMadrec
bin_PROGRAMS =  testMadrec
LDADD = -L../../madf/geolib -lgeo -lmadrec $(FLIBS)

# test programs only
check_PROGRAMS = testMadrecSequentialFileIO testMadrecRandomFileIO \
     testCedarIO testCedarIOBoundaries testCedarMemIO \
     testMadrecMemIO testMadrecRand translateCedarFile testCreateRecord \
     compareFiles testParmArray testGeometry testMetadata fileDump repairHC \
     splitCbfFile testMaddata simpleMaddata simpleNonfileMaddata \
     testMaddataErrors testDerived testTrace

