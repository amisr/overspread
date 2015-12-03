#!/bin/sh

if [ $# -ne 2 ]; then
	echo 1>&2 Usage: $0 EXPERIMENT EXP_PNAME
	exit 127
fi

python="python"

srcdir="$AMISR_FITTER_PATH/src" # should point to src directory in fitter directory

# configuration files
configsys="$AMISR_FITTER_PATH/config/config_sys.ini,$AMISR_FITTER_PATH/config/ßconfig_PFISR.ini" # system conf file
configfiles="$2/config_fit_BC.ini,$2/config_io_BC-dtc0.ini" # fit and io config files
configexp="$2/$1/config_exp.ini" # experiment specific config file(s)

# run the fitter
command="$python $srcdir/run_fitter.py -c $configsys,$configfiles,$configexp"
echo $command
$command
