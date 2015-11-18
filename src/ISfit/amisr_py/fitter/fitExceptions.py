#!/usr/bin/env python

"""

Defines exceptions for fitter

"""

# Exception: composition error
class BadComposition(Exception):
    def __init__( self ):
        Exception.__init__(self, 'Composition not converging')
