#!/Users/mnicolls/Documents/Work/Madrigal/bin/python

"""isTrusted.py returns 1 if user trusted, 0 if not.

Simply calls madrigal.ui.web.MadrigalWeb.isTrusted.  Need only to be called from
tcl cgi scripts.

$Id: isTrusted.py,v 1.1 2008/08/27 09:28:18 brideout Exp $
"""

import os, os.path, sys

import madrigal.ui.web

webObj = madrigal.ui.web.MadrigalWeb()
print webObj.isTrusted()
