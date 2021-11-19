##########################  COMPILATION NEEDED LIBRARIES  ##########################
compile: clean compile-spectra compile-geolib

compile-spectra:
	cd lib/spec_worker && ${MAKE} clean && ${MAKE} compile

compile-geolib:
	cd lib/geolib && ${MAKE} clean && ${MAKE}

clean:
	cd lib/spec_worker && ${MAKE} clean
	cd lib/geolib && ${MAKE} clean
###########################################################################################