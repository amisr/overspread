#!/usr/bin/env python

#480 us long pulse with true filter impulse response corresponding to 4 us sampling

import sys
sys.path.append('/Users/mnicolls/Documents/Work/ISfit/AMISRtools/python')
import amb_func_utils

amb_func_utils.compute_lamb(0,'../filterfiles/blackman_04.00usec_020520.fco',[2,5],120,[4],'../lp480_4',lags=range(120))