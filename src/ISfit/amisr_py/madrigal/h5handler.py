#!/usr/bin/env python

"""

"""

import 

def parseExpId(expId):
    """parseExpId parses an experiment id in the form YYYYMMDD.<inst_code>.<number>, and
    returns a tuple of (datetime, YYYYMMSS string, instId, optional char associated with number,
    and full path to the Madrigal experiment.

    Inputs:  expId - experiment id string in the form YYYYMMDD.<inst_code>.<number>, where
    the date represents the first day of the experiment, <inst_code> is the instrument
    code, and the trailing <number> is between 0 and 26
    
    Returns: a tuple with 5 items: 1. datetime represented by YYYYMMDD, 2. YYYYMMSS string
    itself, 3) the inst id, 4) the optional char associated with the number (0='', 1='a', ...
    26='z'), and 5) the string representing the full path to the Madrigal experiment in form
    $MADROOT/experiments/YYYY/<3_letter_inst>/DDmmmYYY<char>.

    Raises ValueError if expId not in proper format, instrument code not found,
    or trailing number < 0 or > 26.
    """

    madDBObj = madrigal.metadata.MadrigalDB()
    madInstObj = madrigal.metadata.MadrigalInstrument(madDBObj)
    
    try:
        year = int(expId[0:4])
        month = int(expId[4:6])
        day = int(expId[6:8])
    except:
        traceback.print_exc()
        raise ValueError, 'expId not in form YYYYMMDD.<inst_code>.<number>: <%s>' % (str(expId))
    
    if year < 1900:
        raise ValueError, 'expId <%s> has too early year %i' % (str(expId), year)

    try:
        thisDate = datetime.datetime(year, month, day)
    except:
        traceback.print_exc()
        raise ValueError, 'expId not in form YYYYMMDD.<inst_code>.<number>: <%s>' % (str(expId))

    try:
        items = expId.split('.')
        instCode = int(items[1])
        num = int(items[2])
    except:
        traceback.print_exc()
        raise ValueError, 'expId not in form YYYYMMDD.<inst_code>.<number>: <%s>' % (str(expId))

    # get 3 letter instrument mnemonic
    mnem = madInstObj.getInstrumentMnemonic(instCode)

    if mnem == None:
        raise ValueError, 'unknown instrument code in expId: <%i>' % (instCode)

    if num < 0 or num > 26:
        raise ValueError, 'expId must end in number between 0 and 26, not %i' % (num)

    if num == 0:
        extChar = ''
    else:
        extChar = chr(96 + num)


    dirName = os.path.join(madDBObj.getMadroot(),
                           'experiments',
                           '%04i' % year,
                           mnem,
                           '%s%s' % (thisDate.strftime('%d%b%y').lower(), extChar))

    return((thisDate, items[0], instCode, extChar, dirName))


