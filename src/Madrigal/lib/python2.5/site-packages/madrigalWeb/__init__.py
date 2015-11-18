"""Python remote API for access to any Madrigal server


    Example::

        import madrigalWeb.madrigalWeb

        testData = madrigalWeb.madrigalWeb.MadrigalData('http://www.haystack.mit.edu')

        instList = testData.getAllInstruments()

        for inst in instList:

            print inst
"""
