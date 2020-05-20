##########################  COMPILATION NEEDED LIBRARIES  ##########################
compile: clean compile-spectra compile-msis compile-geolib compile-flipchem

compile-spectra:
	cd lib/spec_worker && ${MAKE} clean && ${MAKE} compile

compile-msis:
	cd lib/nrlmsise00/nrlmsis00_c_version && ${MAKE} clean && ${MAKE} msislib
	
compile-geolib:
	cd lib/geolib && ${MAKE} clean && ${MAKE}

compile-flipchem:
	cd lib/flip-chem && ${MAKE} clean && ${MAKE}
	
clean:
	cd lib/spec_worker && ${MAKE} clean
	cd lib/nrlmsise00/nrlmsis00_c_version && ${MAKE} clean
	cd lib/geolib && ${MAKE} clean
	cd lib/flip-chem && ${MAKE} clean
###########################################################################################