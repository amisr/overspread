#!/bin/sh

if [ $# -ne 3 ]; then
	echo 1>&3 Usage: $0 EXPERIMENT BC_IO_FILE FIT_FILE
	exit 127
fi

python="python"

srcdir="$AMISR_FITTER_PATH/src" # should point to src directory in fitter directory

# configuration files
configsys="$AMISR_FITTER_PATH/config/config_sys.ini,$AMISR_FITTER_PATH/config/config_PFISR.ini" # system conf file
configfiles="$3,$2" # fit and io config files
configexp="$1/config_exp.ini" # experiment specific config file(s)

# run the fitter
command="$python $srcdir/run_fitter.py -c $configsys,$configfiles,$configexp"
echo $command
$command
