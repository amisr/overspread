#!/usr/bin/env python

# 16 baud randomized strong alternating code with 20 us bauds and boxcar filter impulse response corresponding to 5 us sampling (fraction=4)

import sys, scipy
sys.path.append('/Users/mnicolls/Documents/Work/ISfit/AMISRtools/python')
import amb_func_utils

a16code,signs=amb_func_utils.a16rand()
amb_func_utils.compute_lamb(4,'../filterfiles/blackman_05.00usec_020525.fco',[2,5],scipy.transpose(signs),[20,4],'')

