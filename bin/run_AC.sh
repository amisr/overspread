#!/bin/sh

FIT_CONFIG='config_fit_ACfl.ini'
IO_CONFIG='config_io_ACfl.ini'

if [ $# -gt 3 ]; then
    echo 1>&2 Usage: $0 EXPERIMENT [FIT_CONFIG] [IO_CONFIG]
    exit 127
fi

if [ $# -eq 2 ]; then
    FIT_CONFIG=$2
fi
if [ $# -eq 3 ]; then
    FIT_CONFIG=$2
    IO_CONFIG=$3
fi

# configuration files
configsys="$AMISR_FITTER_PATH/config/config_sys.ini,$AMISR_FITTER_PATH/config/config_PFISR.ini" # system conf file
configfiles="$FIT_CONFIG,$IO_CONFIG" # fit and io config files
configexp="$1/config_exp.ini" # experiment specific config file(s)

# run the fitter
command="run_fitter -c $configsys,$configfiles,$configexp"
echo $command
$command
