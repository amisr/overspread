#!/bin/sh

FIT_CONFIG='config_fit_LP.ini'
IO_CONFIG='config_io_LP.ini'

if [ $# -gt 4 ] || [ $# -eq 0 ]; then
    echo 1>&2 Usage: $0 RADAR EXPERIMENT [FIT_CONFIG] [IO_CONFIG]
    echo 1>&2        Valid RADAR: pfisr, risrn, risrc, sondre
    exit 127
fi

if [ $# -eq 3 ]; then
    FIT_CONFIG=$3
fi
if [ $# -eq 2 ]; then
    FIT_CONFIG=$3
    IO_CONFIG=$4
fi

# configuration files
configfiles="$FIT_CONFIG,$IO_CONFIG" # fit and io config files
configexp="$2/config_exp.ini" # experiment specific config file(s)

# run the fitter
command="run_fitter $1 -c $configfiles,$configexp"
echo $command
$command