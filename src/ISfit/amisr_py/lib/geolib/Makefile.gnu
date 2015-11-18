# $Id: Makefile.gnu,v 1.18 2008/07/24 20:47:28 brideout Exp $
#
# ----------------------------------------------------------------------
#       Gnu makefile for libgeo (Distribution version)
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
#       Site-dependent definitions - change these as needed
# ----------------------------------------------------------------------

# Madrigal root directory
MADROOT =  /home/hyperion/brideout/madR/madroot

# Solaris Make program
MAKE = /opt/local/bin/make

# GNU Fortran compiler
FC = /opt/local/bin/g77

# GNU C compiler
CC = /opt/local/bin/gcc

# Library directory
LIBDIR = $(MADROOT)/lib/gnu

# Binary directory
BINDIR = $(MADROOT)/bin/gnu

# ----------------------------------------------------------------------
#       End of site-dependent definitions
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
#       Print Command
# ----------------------------------------------------------------------

PRINT = lp

# ----------------------------------------------------------------------
#       Library directives
# ----------------------------------------------------------------------

LDLIBS = -L$(LIBDIR) -lgeo -lm -lnsl

# ----------------------------------------------------------------------
#       Fortran compiler options
# ----------------------------------------------------------------------

FFLAGS = -O -fPIC -Wall -Wuninitialized -fno-second-underscore

# ----------------------------------------------------------------------
#       Link editor options 
# ----------------------------------------------------------------------

LDFLAGS = -Xlinker -R$(LIBDIR)

# ----------------------------------------------------------------------
#       Fortran source files
# ----------------------------------------------------------------------

SOURCES = carmel.f convrt.f coord.f csconv.f diplat.f dsf.f gaspct.f \
          gdmag.f gdran.f gdv.f gmet.f gtd7.f hfun.f integ.f invar.f \
          invlat.f iterat.f  lines.f lintra.f look.f milmag.f minv.f \
          mtran3.f point.f rfun.f rpcart.f sprod.f startr.f          \
          tnf.f vadd.f vctcnv.f vmag.f vsub.f geo-cgm.f              \
          calndr.f dater.f dates.f iday.f izlr.f idmyck.f jdater.f   \
          jday.f moname.f monum.f wkname.f T01_01.f Geopack_2005.f   \
	  sfc_convert_geo_coord.f cgm_to_altitude.f altitude_to_cgm.f \
	  rylm.f blkdat95.f isrim.f basis.f conduct.f irisub.f       \
          irifun.f cira.f igrf.f iritec.f iridreg.f

# ----------------------------------------------------------------------
#       Object files
# ----------------------------------------------------------------------
OBJECTS = $(SOURCES:%.f=%.o)

# ----------------------------------------------------------------------
#       Libraries and Programs
# ----------------------------------------------------------------------

all: clean libgeo.a libgeo.so.1 testGeolib testGeolibs sfctest testISRIM verify

libgeo.a: $(OBJECTS)
	ar rv $@ $(OBJECTS)
	cp *.asc $(BINDIR)
	cp libgeo.a $(LIBDIR)

libgeo.so.1: $(OBJECTS)
	$(FC) -o $@ -shared $(OBJECTS)
	cp libgeo.so.1 $(LIBDIR)
	cp *.asc $(BINDIR)
	rm -f $(LIBDIR)/libgeo.so
	ln -s $(LIBDIR)/libgeo.so.1 $(LIBDIR)/libgeo.so

testGeolib: testGeolib.o
	$(LINK.f) testGeolib.o $(LDLIBS) -o $@
	cp testGeolib $(BINDIR)/testGeolib

testGeolibs: testGeolib.o
	$(LINK.f) testGeolib.o -static $(LDLIBS) -o $@
	cp testGeolibs $(BINDIR)/testGeolibs
	
sfctest: sfctest.o
	$(LINK.f) sfctest.o $(LDLIBS) -o $@
	
testISRIM: testISRIM.o
	$(LINK.f) testISRIM.o $(LDLIBS) -o $@

# ----------------------------------------------------------------------
#       Utility
# ----------------------------------------------------------------------
 
verify:
	./testGeolib
	@cmp -s testGeolib.out testGeolib.out.rock && \
           echo 'OK - testGeolib.out and testGeolib.out.rock are the same' || \
        (cmp -s testGeolib.out testGeolib.out.rock || \
           echo 'Fail - testGeolib.out and testGeolib.out.rock are different')
	./testGeolibs
	@cmp -s testGeolib.out testGeolib.out.rock && \
           echo 'OK - testGeolib.out and testGeolib.out.rock are the same' || \
        (cmp -s testGeolib.out testGeolib.out.rock || \
           echo 'Fail - testGeolib.out and testGeolib.out.rock are different')

clean cleanup:
	rm -f testGeolib testGeolibs libgeo.a libgeo.so.1 sfctest testISRIM \
              core *.o *.BAK *.CKP *%
print listing:
	$(PRINT) Makefile $(SOURCES) testGeolib.f testGeolib.out.rock
