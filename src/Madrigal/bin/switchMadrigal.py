#!/Users/mnicolls/Documents/Work/Madrigal/bin/python

import os, os.path, sys
import shutil
import glob
import getopt
import traceback

import madrigal.metadata

# $Id: switchMadrigal.py,v 1.9 2009/02/04 14:30:11 brideout Exp $

usage = """usage:\nswitchMadrigal.py [--newUrl] [--moveExp] <original_madroot> <new_madroot> 

where <original_madroot> is the madroot directory of the old Madrigal
installation, and <new_madroot> is the madroot directory of the new
Madrigal installation.  This script must be run from the new installation
of Madrigal.  If you are changing the url of your Madrigal installation
to the new url, specify --newUrl.  If that new url is on a different server, use
scp format of <username>@<server>:<original_madroot> for <original_madroot>.
If you want to move your experiments directory to the new server, rather
than the safer approach of just copying it, use the --moveExp flag.
"""

def updateMadrigalCfg(org_madroot, new_madroot):
    """updateMadrigalCfg copies the following fields from org_madroot
    to new_madroot: MADSERV + ERROOT, MADSERV + ERCGI, MADSERV + ERDOCABS,
    MADSERV + ERCGIABS (plus to avoid substitution

    Returns tuple of (new_MADSERV + ERDOCABS, new_MADSERV + ERCGIABS),
    used to removed scripts from new (temporary) web site.
    """
    # split string to avoid substitution
    fieldsToUpdate = ('MADSERV'+'ERROOT', 'MADSERV'+'ERCGI',
                      'MADSERV'+'ERDOCABS','MADSERV'+'ERCGIABS')

    oldDict = {}

    f = open(os.path.join(org_madroot, 'madrigal.cfg'))
    lines = f.readlines()
    f.close()

    org_siteid = None

    for line in lines:
        if line[0] == '#':
            continue
        items = line.split()
        if len(items) == 0:
            continue
        if items[0] in fieldsToUpdate:
            oldDict[items[0]] = items[2]
            if len(items) != 3:
                raise IOError, 'Illegal line in original madrigal.cfg: <%s>' % line
        if items[0] == 'SITE' + 'ID':
            org_siteid =  int(items[2])

    if len(oldDict.keys()) != len(fieldsToUpdate):
        raise IOError, 'Missing fields in original madrigal.cfg, only found %s' % \
              (str(oldDict.keys()))

    if org_siteid == None:
        raise IOError, 'site id not found in original madrigal.cfg'

    f = open(os.path.join(new_madroot, 'madrigal.cfg'))
    lines = f.readlines()
    f.close()

    f = open(os.path.join(new_madroot, 'madrigal.cfg'), 'w')

    keyCount = 0

    new_siteid = None

    for line in lines:
        if line[0] == '#':
            f.write(line)
            continue
        items = line.split()
        if len(items) != 3:
            f.write(line)
            continue
        if items[0]  == 'SITE' + 'ID':
            new_siteid =  int(items[2])
        if oldDict.has_key(items[0]):
            f.write('%s = %s\n' % (items[0], oldDict[items[0]]))
            keyCount += 1
            if items[0] == fieldsToUpdate[2]:
                madserverdocabs = items[2]
            if items[0] == fieldsToUpdate[3]:
                madservercgiabs = items[2]
        else:
            f.write(line)
            continue


    f.close()

    if keyCount != len(fieldsToUpdate):
        raise IOError, 'Failed to find all needed fields in new madrigal.cfg'

    if new_siteid != org_siteid:
        raise IOError, 'Mismatch in site id found new %s, old %s - must be the same' % (str(new_siteid), str(org_siteid))

    return((madserverdocabs, madservercgiabs))


def copyExperimentsDir(org_madroot, new_madroot, moveExp):
    """copyExperimentsDir either copies or moves the original experiments
    directory into the new_madroot directory, depending on whether
    moveExp is false or true.  
    
    If the new_madroot directory has any data outside of the standard
    test experiments and geo files, an error is raised.  The test
    experiments directory will be deleted before the copy or move of the
    original madroot experiments directory.    

    If org_madroot in scp form <username>@<server>:<original_madroot>, scp
    used instead.  Error is raised if moveExp is true.

    Will also copy madroot/local_rules_of_the_road.txt and trustedIPs.txt and
    madroot/geoStatus.dat and madroot/doc/siteSpecific.html if available.  Copies access log files to
    madroot/metadata/userdata/old_access_logs
    """
    newExpDir = os.path.join(new_madroot, 'experiments')
    orgExpDir = os.path.join(org_madroot, 'experiments')

    newMetaDir = os.path.join(new_madroot, 'metadata')
    orgMetaDir = os.path.join(org_madroot, 'metadata')

    newUserDir = os.path.join(new_madroot, 'metadata/userdata')
    orgUserDir = os.path.join(org_madroot, 'metadata/userdata')

    newDocDir = os.path.join(new_madroot, 'doc')
    orgDocDir = os.path.join(org_madroot, 'doc')

    backupAccessDir = os.path.join(newUserDir, 'old_access_logs')
    try:
        os.mkdir(backupAccessDir)
    except:
        pass

    newDirs = glob.glob(os.path.join(newExpDir, '*'))
    for thisDir in newDirs:
        if os.path.basename(thisDir) not in ('1950','1957','1963',
                                             '1995','1997','1998',
                                             'stage'):

            raise IOError, 'Real data found in %s' % (newExpDir)

    shutil.rmtree(newExpDir)

    if org_madroot.find(':') != -1:
        if moveExp:
            raise IOError, 'Cannot move experiment directory to remote machine'
        cmd = 'scp -p %s %s' % (orgExpDir, newExpDir)
        print 'About to run <%s> - step 1 of 6' % (cmd)
        os.system(cmd)
        # copy madroot/local_rules_of_the_road.txt
        cmd = 'scp -p %s %s' % (os.path.join(org_madroot, 'local_rules_of_the_road.txt'), new_madroot)
        print 'About to run <%s> - step 2 of 6' % (cmd)
        os.system(cmd)
        # copy madroot/geoStatus.dat
        cmd = 'scp -p %s %s' % (os.path.join(org_madroot, 'geoStatus.dat'), new_madroot)
        print 'About to run <%s> - step 3 of 6' % (cmd)
        os.system(cmd)
        # copy madroot/siteSpecific.html
        cmd = 'scp -p %s %s' % (os.path.join(orgDocDir, 'siteSpecific.html'), newDocDir)
        print 'About to run <%s> - step 4 of 6' % (cmd)
        os.system(cmd)
        # copy access logs
        cmd = 'scp -p %s %s' % (os.path.join(orgUserDir, 'access*log'), backupAccessDir)
        print 'About to run <%s> - step 5 of 6' % (cmd)
        os.system(cmd)
        # copy madroot/trustedIPs.txt
        cmd = 'scp -p %s %s' % (os.path.join(org_madroot, 'trustedIPs.txt'), new_madroot)
        print 'About to run <%s> - step 6 of 6' % (cmd)
        os.system(cmd)
    else:
        if moveExp:
            shutil.move(os.path.join(org_madroot, 'experiments'),
                        os.path.join(new_madroot, 'experiments'))
        else:
            shutil.copytree(os.path.join(org_madroot, 'experiments'),
                            os.path.join(new_madroot, 'experiments'))
        # copy madroot/local_rules_of_the_road.txt
        cmd = 'cp -p %s %s' % (os.path.join(org_madroot, 'local_rules_of_the_road.txt'), new_madroot)
        print 'About to run <%s>' % (cmd)
        os.system(cmd)
        # copy madroot/geoStatus.dat
        cmd = 'cp -p %s %s' % (os.path.join(org_madroot, 'geoStatus.dat'), new_madroot)
        print 'About to run <%s>' % (cmd)
        os.system(cmd)
        # copy madroot/siteSpecific.html
        cmd = 'cp -p %s %s' % (os.path.join(orgDocDir, 'siteSpecific.html'), newDocDir)
        print 'About to run <%s>' % (cmd)
        os.system(cmd)
        # copy access logs
        cmd = 'cp -p %s %s' % (os.path.join(orgUserDir, 'access*log'), backupAccessDir)
        print 'About to run <%s>' % (cmd)
        os.system(cmd)
        # copy madroot/trustedIPs.txt
        cmd = 'cp -p %s %s' % (os.path.join(org_madroot, 'trustedIPs.txt'), new_madroot)
        print 'About to run <%s>' % (cmd)
        os.system(cmd)
        
##### main script begins here #####

# get input arguments
try:
    opts, args = getopt.getopt(sys.argv[1:], "", ["newUrl", "moveExp"])
except:
    traceback.print_exc()
    print usage
    sys.exit(-1)

newUrl = False
moveExp = False

for o, a in opts:
    if o == "--newUrl":
        newUrl = True
    elif o == "--moveExp":
        moveExp = True
    else:
        print 'Illegal option %s' % (str(o))
        print usage
        sys.exit(-1)

if len(args) == 0:
    print usage
    sys.exit(-1)
    
org_madroot = args[0]
if org_madroot[-1] == '/':
    org_madroot = org_madroot[:-1]
new_madroot = args[1]
if new_madroot[-1] == '/':
    new_madroot = new_madroot[:-1]

# verify org_madroot remote only if newUrl:
if not newUrl and org_madroot.find(':') != -1:
    print 'Cannot move Madrigal to a remote server without setting --newUrl'
    sys.exit(-1)

for madroot in (org_madroot, new_madroot):
    if madroot.find(':') != -1:
        continue # cannot check remote site
    cfgFile = os.path.join(madroot, 'madrigal.cfg')
    if not os.access(cfgFile, os.R_OK):
        raise IOError, 'Could not open files %s' % (cfgFile)

# check this this script is being called from new_madroot
madDB = madrigal.metadata.MadrigalDB()
madroot = madDB.getMadroot()
if madroot[-1] == '/':
    madroot = madroot[:-1]
if madroot != new_madroot:
    raise IOError, 'This script not called from new_madroot %s' % (new_madroot)

if not moveExp:
    # copy experiment directory
    print 'Copying experiment directory from old to new madroot.  This may take some time.'
else:
    print 'Moving experiment directory from old to new madroot.'
copyExperimentsDir(org_madroot, new_madroot, moveExp)

if not newUrl:

    # copy siteTab.txt
    print '\nCopying old siteTab.txt\n'
    shutil.copy2(os.path.join(org_madroot, 'metadata/siteTab.txt'),
                 os.path.join(new_madroot, 'metadata/siteTab.txt'))

    # update madrigal.cfg
    print '\nUpdating madrigal.cfg in new madroot\n'
    madserverdocabs, madservercgiabs = updateMadrigalCfg(org_madroot, new_madroot)


    # run all needed scripts
    print '\nrunning configureHtml...\n'
    os.system('tclsh %s' % (os.path.join(madroot, 'configureHtml')))
    print '\nrunning configureScripts...\n'
    os.system('tclsh %s' % (os.path.join(madroot, 'configureScripts')))
    
else:
    print '\nrunning configureExperiments...\n'
    os.system(os.path.join(madroot, 'configureExperiments'))
    print '\nrunning configureHtml...\n' # needed because siteSpecific.html copied in
    os.system('tclsh %s' % (os.path.join(madroot, 'configureHtml')))

print '\nrunning updateMaster for the first time ...\n'
os.system(os.path.join(madroot, 'bin/updateMaster'))
    
print '\nrunning updateFileTab.py...\n'
os.system(os.path.join(madroot, 'bin/updateFileTab.py'))
    
print '\nrunning updateMaster for the second time...\n'
os.system(os.path.join(madroot, 'bin/updateMaster'))

print '\nrunning rebuildInstParmTab.py -all...\n'
os.system(os.path.join(madroot, 'bin/rebuildInstParmTab.py -all'))

print '\nswitchMadrigal.py complete - try the web site.'

if not newUrl:
    print 'If you wish to remove the temporary website, delete all files and directories from ' + \
      '%s and %s' % (madserverdocabs, madservercgiabs)




