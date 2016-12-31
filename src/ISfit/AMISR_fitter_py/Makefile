INSTALLER_VERSION=1.1.0-0

# PYPACK_VERSION_DATEUTIL=2.2
# PYPACK_VERSION_FREETYPEPY=1.0
# PYPACK_VERSION_NUMPY=1.7.1
# PYPACK_VERSION_MATPLOTLIB=1.3.1
# PYPACK_VERSION_NUMEXPR=2.3.1
# PYPACK_VERSION_SCIPY=0.13.0
# PYPACK_VERSION_IPYTHON=1.2.1
# PYPACK_VERSION_CYTHON=0.20.1
# PYPACK_VERSION_H5PY=2.2.1
# PYPACK_VERSION_TABLES=2.4.0


###################################  LIBRARIES  ###################################
# install: install-dateutil install-freetype-py install-numpy install-matplotlib install-numexpr install-scipy install-ipython install-cython install-h5py install-tables

# install-clean:
# 	-yes | pip uninstall python-dateutil
# 	-yes | pip uninstall freetype-py
# 	-yes | pip uninstall numpy
# 	-yes | pip uninstall matplotlib
# 	-yes | pip uninstall numexpr
# 	-yes | pip uninstall scipy
# 	-yes | pip uninstall ipython
# 	-yes | pip uninstall Cython
# 	-yes | pip uninstall h5py
# 	-yes | pip uninstall tables

# install-dateutil:
# 	pip install python-dateutil==${PYPACK_VERSION_DATEUTIL}
	
# install-freetype-py:
# 	pip install freetype-py==${PYPACK_VERSION_FREETYPEPY}
	
# install-numpy:
# 	pip install numpy==${PYPACK_VERSION_NUMPY}
	
# install-matplotlib:
# 	pip install matplotlib==${PYPACK_VERSION_MATPLOTLIB}

# install-numexpr:
# 	pip install numexpr==${PYPACK_VERSION_NUMEXPR}

# install-scipy:
# 	pip install scipy==${PYPACK_VERSION_SCIPY}

# install-ipython:
# 	pip install ipython==${PYPACK_VERSION_IPYTHON}

# install-cython:
# 	pip install Cython==${PYPACK_VERSION_CYTHON}

# install-h5py:
# 	pip install h5py==${PYPACK_VERSION_H5PY}

# install-tables:
# 	pip install tables==${PYPACK_VERSION_TABLES}
#################################################################################


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