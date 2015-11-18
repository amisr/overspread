#!/usr/bin/env python

"""

Defines summation rules

"""

# lpmodern
def lpmodern(zeroLagSum,Nlags):    
    """  
    modern long pulse summation rule 

        zeroLagSum - # of zero lags to sum
        Nlags - # of lags
    """

    irng=-int(zeroLagSum/2)
    return [range(irng-int(round(ilag/2.0)),irng+int(ilag/2.0)+zeroLagSum) for ilag in range(Nlags)]
        
