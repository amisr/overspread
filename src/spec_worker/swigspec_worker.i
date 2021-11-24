%module c_spec_worker

%{
#define SWIG_FILE_WITH_INIT
#include "spec_worker.h"
%}

%include "spec_worker.h"

%include "carrays.i"
%array_class(double, doubleArray);