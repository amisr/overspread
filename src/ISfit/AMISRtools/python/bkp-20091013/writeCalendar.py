#!/usr/bin/python

from icalendar import Calendar, Event
from datetime import datetime
from icalendar import UTC # timezone
import glob
import os
import tables

OUTPATH='/Volumes/AMISR_004/calendar_data' # path to calendar files
DISK_PATHS=['/Volumes/AMISR_005/Data AMISR Poker','/Volumes/AMISR_006/Data AMISR Poker'] # volumes where data files are

if __name__ == '__main__':

	YR='2007'; MN='10'

	OFNAME='PFISR-'+MN+'-'+YR+'.ics'
	DIR_WC=YR+MN+'*' # wild card to select exps from
	FILE_WC='*.h5'

	dirs=[]
	for id in range(len(DISK_PATHS)):
		dirs.extend(glob.glob(os.path.join(DISK_PATHS[id],DIR_WC)))
	Ndirs=len(dirs)
	dirs.sort()
	

	# create calendar
	cal = Calendar()
	cal.add('prodid', '-//My calendar product//mxm.dk//')
	cal.add('version', '2.0')

	for aaa in range(Ndirs):
		
		# get the setup file
		exp_file = glob.glob(os.path.join(dirs[aaa],'Setup/*.exp'))

		if len(exp_file)==1:		
			exp_file=exp_file[0]
			tmp1,tmp2,exp_file=exp_file.partition(os.path.join(dirs[aaa],'Setup/'))
			exp_file,tmp1,tmp2=exp_file.partition('.exp')

			# file list
			files=glob.glob(os.path.join(dirs[aaa],FILE_WC))
			files.sort()
			
			# read first and last file
			h5file=tables.openFile(files[0],"r"); UnixTime0=h5file.getNode('/Time/UnixTime').read(); h5file.close()
			h5file=tables.openFile(files[-1],"r"); UnixTime1=h5file.getNode('/Time/UnixTime').read(); h5file.close()
			tmp0=datetime.utcfromtimestamp(UnixTime0[0,0])
			tmp1=datetime.utcfromtimestamp(UnixTime1[-1,-1])

			# create event
			print 'Adding event %s: %d-%d %d:%d:%d - %d-%d %d:%d:%d' % (exp_file,tmp0.month,tmp0.day,tmp0.hour,tmp0.minute,tmp0.second,tmp1.month,tmp1.day,tmp1.hour,tmp1.minute,tmp1.second)
			event = Event()
			event.add('summary', exp_file)
			event.add('dtstart', tmp0)
			event.add('dtend', tmp1)
			event.add('dtstamp', tmp1)
			#event['uid'] = '20050115T101010/27346262376@mxm.dk'
			#event.add('priority', 5)
			cal.add_component(event)
			
	# write output
	f = open(os.path.join(OUTPATH,OFNAME), 'wb')
	f.write(cal.as_string())
	f.close()
