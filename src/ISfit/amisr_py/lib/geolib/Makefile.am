# $Id: Makefile.am,v 1.4 2010/08/11 14:01:12 brideout Exp $
#
# Makefile.am for geo library

lib_LTLIBRARIES = libgeo.la


# ----------------------------------------------------------------------
#       Geolib Fortran source files
# ----------------------------------------------------------------------

libgeo_la_SOURCES = carmel.f convrt.f coord.f csconv.f diplat.f dsf.f gaspct.f \
          gdmag.f gdran.f gdv.f gmet.f gtd7.f hfun.f integ.f invar.f \
          invlat.f iterat.f  lines.f lintra.f look.f milmag.f minv.f \
          mtran3.f point.f rfun.f rpcart.f sprod.f startr.f          \
          tnf.f vadd.f vctcnv.f vmag.f vsub.f geo-cgm.f              \
          calndr.f dater.f dates.f iday.f izlr.f idmyck.f jdater.f   \
          jday.f moname.f monum.f wkname.f T01_01.f Geopack_2005.f   \
	  sfc_convert_geo_coord.f cgm_to_altitude.f altitude_to_cgm.f \
	  rylm.f blkdat95.f isrim.f basis.f conduct.f irisub.f       \
          irifun.f cira.f igrf.f iritec.f iridreg.f                  \
          mlt.f mlt1.f solar_loc.f eqn_of_time.f cnvcoord.f cnv_sec_mdhms.f \
          igrf11.f
	  
# installed test program testGeolib
bin_PROGRAMS =  testGeolib
testGeolib_SOURCES = testGeolib.f
testGeolib_LDADD = libgeo.la

# test programs only
check_PROGRAMS = sfctest testISRIM
sfctest_SOURCES = sfctest.f
testISRIM_SOURCES = testISRIM.f
sfctest_LDADD = libgeo.la
testISRIM_LDADD = libgeo.la
