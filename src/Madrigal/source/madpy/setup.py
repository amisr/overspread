#!/usr/bin/env python

#
# This is a python DistUtils setup file for the
# Madrigal Python API.
#
# This Python module exists as an API to the madrigal
# database.  Much of its functionality comes from
# the underlying C API layer written by John Holt
# and Bill Rideout.
# 
# To build : 
# 
# 1.  Be sure $MADROOT is set (at MLH, set to /opt/madrigal)
# 2.  python setup.py build
#
# To install : 
# 
# 1.  Be sure $MADROOT is set (at MLH, set to /opt/madrigal)
# 2.  python setup.py install
#
import os, sys
import ConfigParser
import StringIO
from distutils.core import setup, Extension


# function to edit madrigal/metadata_original.py, substituting in values
# from madrigal.cfg in order to create metadata.py
# Also modifies __bgReport_original.py to create __bgReport.py
def createMetadataPyFile():

    # get data from madrigal.cfg
    madConfFile = open('../../madrigal.cfg', 'r')

    # create Parser using standard module ConfigParser
    parser = ConfigParser.ConfigParser()

    # read madrigal.cfg into a StringIO with "[madrigal]\n" section heading prepended
    strMadConfFile = StringIO.StringIO("[madrigal]\n" + madConfFile.read())

    # parse StringIO madrigal.cfg
    parser.readfp(strMadConfFile)

    # open metadata_original.py
    metadataOrgFile = open('madrigal/metadata_original.py', 'r')

    # open __bgReport_original.py
    bgOrgFile = open('madrigal/ui/__bgReport_original.py', 'r')

    # read file into string
    metadataStr = StringIO.StringIO(metadataOrgFile.read()).getvalue()
    bgStr = StringIO.StringIO(bgOrgFile.read()).getvalue()

    # substitute all values - order from longest to shortest for MADSERVER
    metadataStr = metadataStr.replace('MADSERVERDOCABS', parser.get("madrigal", 'MADSERVERDOCABS'))
    metadataStr = metadataStr.replace('MADSERVERCGIABS', parser.get("madrigal", 'MADSERVERCGIABS'))
    metadataStr = metadataStr.replace('MADSERVERROOT', parser.get("madrigal", 'MADSERVERROOT'))
    metadataStr = metadataStr.replace('MADSERVERCGI', parser.get("madrigal", 'MADSERVERCGI'))
    metadataStr = metadataStr.replace('MADSERVER', parser.get("madrigal", 'MADSERVER'))
    metadataStr = metadataStr.replace('MADROOT', parser.get("madrigal", 'MADROOT'))
    metadataStr = metadataStr.replace('SITEID', parser.get("madrigal", 'SITEID'))
    metadataStr = metadataStr.replace('HTMLSTYLE', parser.get("madrigal", 'HTMLSTYLE'))
    metadataStr = metadataStr.replace('INDEXHEAD', parser.get("madrigal", 'INDEXHEAD'))
    metadataStr = metadataStr.replace('CONTACT', parser.get("madrigal", 'CONTACT'))
    metadataStr = metadataStr.replace('MAILSERVER', parser.get("madrigal", 'MAILSERVER'))

    # only one substitution for bgReport
    bgStr = bgStr.replace('MADROOT', parser.get("madrigal", 'MADROOT'))

    # substitute MAXGLOBALQUERIES only if it exists, otherwise set to blank
    if parser.has_option("madrigal", 'MAXGLOBALQUERIES'):
        metadataStr = metadataStr.replace('MAXGLOBALQUERIES', parser.get("madrigal", 'MAXGLOBALQUERIES'))
    else:
        metadataStr = metadataStr.replace('MAXGLOBALQUERIES', '2')

    # substitute MAXTEMPREPORTS only if it exists, otherwise set to blank
    if parser.has_option("madrigal", 'MAXTEMPREPORTS'):
        metadataStr = metadataStr.replace('MAXTEMPREPORTS', parser.get("madrigal", 'MAXTEMPREPORTS'))
    else:
        metadataStr = metadataStr.replace('MAXTEMPREPORTS', '2')


    # write new string to metadata.py
    metadataNewFile = open('madrigal/metadata.py', 'w')
    metadataNewFile.write(metadataStr)
    metadataNewFile.close()
    metadataOrgFile.close()

    # write new string to __bgReport.py
    bgNewFile = open('madrigal/ui/__bgReport.py', 'w')
    bgNewFile.write(bgStr)
    bgNewFile.close()
    bgOrgFile.close()

    # close madrigal.cfg
    madConfFile.close()



# call the function above
createMetadataPyFile()
MADROOT='/Users/mnicolls/Documents/Work/Madrigal'

setup(name="madpy",
        version="1.0",
        description="Madrigal Python API - built on top of C API",
        author="Bill Rideout",
        author_email="wrideout@haystack.mit.edu",
        url="http://www.haystack.mit.edu/~brideout/",
        packages=['madrigal', 'madrigal.ui'],
        ext_modules=[Extension("madrigal/_Madrec",
                              ["madrigal/_Madrec.c"],
                              include_dirs=[MADROOT + "/source/madc/include",
                                            MADROOT + "/source"],
                              library_dirs =[MADROOT +"/lib"],
                              runtime_library_dirs = [MADROOT + "/lib"],
                              libraries=["madrec","geo"]
                              )
                    ]

           )
