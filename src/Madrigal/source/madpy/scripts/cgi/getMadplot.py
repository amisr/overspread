#!PYTHONEXE
# list and convert the plots available from madrigal
#
# This file should be put in the cgi-bin directory, and the script that
# displays experiment information should run, for example:
#  ./getMadplot.py h 2003/eis/16dec03
# HTML links are generated for all images found belonging to that experiment.
# The links points back to this script, which converts from postscript the
# the requested fileformat on-the-fly.
#
# External programs required: gs, gzip

import sys, os, time

# Import the Madrigal classes
# check if pythonlibpath env variable exists
# written 'PYTHON' + 'LIBPATH' to stop automatic replacement during setup
temp = os.environ.get('PYTHON' + 'LIBPATH')
if temp: sys.path.append(temp)
sys.path.append('MADROOT/lib/python')

import madrigal.metadata
# realpath expands all symbolic links in MADROOT/experiments
exp_path = os.path.realpath(madrigal.metadata.MadrigalDB().getExperimentDir())

import madrigal.ui.web
madrigalweb = madrigal.ui.web.MadrigalWeb()

# make sure the server can find gs
paths = os.environ.get('PATH', '.')
paths = paths.split(':')
pathList = ('/usr/bin', '/usr/local/bin', '/bin',
            '/usr/sfw/bin', '/opt/bin', '/opt/local/bin',
            '/opt/sfw/bin')
for path in pathList:
    if path not in paths:
        os.environ['PATH'] = os.environ.get('PATH', '.') + ':' + path

# These seems to be neccesary on www.eiscat.com... ugly
os.environ['GS_FONTPATH'] = os.environ.get('GS_FONTPATH', '.')+':/usr/openwin/lib/X11/fonts/Type1/outline'
os.environ['GS_LIB'] = os.environ.get('GS_LIB', '.')+':/usr/openwin/lib/X11/fonts/Type1:/usr/openwin/lib/X11/fonts/Type3:/usr/local/share/ghostscript/8.00/lib/fonts'

# Here is the table of all known filetypes. It should be easy to add more.
# Each filetype have a 5-tuple;
#   extension
#   command to convert from postscript data on stdin
#   likewise for compressed postscript. Leave as None to use gzip in combination with above command.
#   Short desctiprion. None here omits it when the filetypes are listed.
#   whether the original file is used. Only IP:s listed in trustedIPs.txt may download these.
#      Set this to False for lossy conversions.

gs_base = "gs -dNOPAUSE -q -sDEVICE=%s -sOutputFile=- -_"
formats = (
	# (ext, ps-command, ps.gz-command, desc, orig)
	(".ps.gz", 'gzip -c', 'cat', 'Gzipped PostScript', True),
	(".eps.gz", 'gzip -c', 'cat', None, True),
	(".ps", 'cat', 'gunzip -c', 'PostScript', True),
	(".eps", 'cat', 'gunzip -c', None, True),
	(".png", gs_base%'png256 -r120 -g1230x1230',None,'PNG image', False),
	(".jpg", gs_base%'jpeg -g580x820',None,'JPEG image', False),
        (".jpeg", gs_base%'jpeg -g580x820',None,'JPEG image', False),
	(".pdf", gs_base%'pdfwrite -sPAPERSIZE=a4',None,'Adobe PDF', False),
	(".gif", None, None, 'GIF image', False),
)

script_name = os.path.basename(sys.argv[0])

class error(Exception):
	"Base class for errors generated in this file"
	pass

def exist_file(base, ext):
	path = base+ext
	if os.path.exists(path):
		return path
def exist_ps(base, ext = ''):
	for ext0 in ('.ps', '.eps'):
		path = exist_file(base+ext0,ext)
		if path: return path

def cgi_file():
	"Get the requested file from the CGI environment"
	file = os.environ.get('QUERY_STRING') or os.environ['PATH_INFO']
	if file[0] == '/': file = file[1:]
	return file

def validate_path(path):
	"Check the path for evil things like /../ and return a tuple (full_path, last_path)"
	file = os.path.realpath(os.path.join(exp_path,path))
	if not file.startswith(exp_path):
		raise error, "Illegal path"
	return file, file[len(exp_path)+1:]

english_weekdays = "Mon Tue Wed Thu Fri Sat Sun".split()
english_months = "Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec".split()

def rfc1123(date):
	"Convert a date to RFC1123 standard, to be used in HTTP headers"
	date = time.gmtime(date)
	return "%s, %02d %s %04d %02d:%02d:%02d GMT" % (english_weekdays[date[6]], date[2], english_months[date[1] - 1], date[0], date[3], date[4], date[5])

def process(file):
	file, _ = validate_path(file)
	req_file = os.path.basename(file)
	for ext, cmd, cmd_gz, desc, source in formats:
		if file.endswith(ext):
			break
	else:
		raise error, "Extension not supported"
	if not isTrusted and source:
		raise error, "You are not allowed to download source files"
	if os.path.exists(file):
		cmd = "cat"
		src = file
	else:
		file = file[:-len(ext)]
		src = exist_ps(file)
		if not src:
			if not cmd:
				raise error, "File not found"
			src = exist_ps(file, '.gz')
			if src:
				cmd = cmd_gz or 'gunzip -c | '+cmd
	if not src:
		raise error, "Source file not found"

	import mimetypes
	mimetype, encoding = mimetypes.guess_type(req_file, strict=0)

	print "Content-Type:",mimetype
	if encoding:
		print "Content-Encoding:", encoding
	print "Last-Modified:", rfc1123(os.path.getmtime(src))
	print
	cmd = cmd.split(' | ')
	cmd[0] += ' < '+src
	cmd = ' | '.join(cmd)
	return cmd

def list_formats():
	for ext, cmd, cmd_gz, desc, _ in formats:
		print ext

def list_files(base):
	path, base = validate_path(base)
	dict = {}
	for ext, _, _, desc, source in formats:
		if not os.path.exists(path+ext): continue
		if not source:
			if desc:
				dict[ext] = None
			continue
		for ext, cmd, _, desc, source in formats:
			if cmd and desc and (isTrusted or not source):
				dict[ext] = None
	if not dict:
		raise error, "File not found"
	return dict.keys()

def list_directory(dir):
	realdir, dir = validate_path(dir)
	if not os.path.isdir(realdir): raise error, "Not a directory"
	files = {}
	for file in os.listdir(realdir):
		for ext, _, _, desc, source in formats:
			if file.endswith(ext): break
		else: continue
		base = file[:-len(ext)]
		dict = files.setdefault(base, {})
		if not source:
			if desc:
				dict[ext] = None
			continue
		for ext, cmd, _, desc, source in formats:
			if cmd and desc and (isTrusted or not source):
				dict[ext] = None
	for base in files.keys():
		files[base] = files[base].keys()
	return files

class Style:
	def run_cmd(self, cmd):
		sys.stdout.flush()
		os.execl('/bin/sh', 'sh', '-c', cmd)

	def process_query(self, file):
		try:
			_, file = validate_path(file)
		except error, mess:
			self.die(mess)
		try:
			cmd = process(file)
		except error, mess0:
			try:
				files = list_directory(file)
			except error, mess1:
				try:
					files = list_files(file)
				except error, mess2:
					self.die('\n'.join(map(str, (mess0, mess1, mess2))))
				else:
					self.output_alternatives(file, files)
			else:
				self.output_directory(file, files)
		else:
			self.run_cmd(cmd)

	def output_directory(self, dir, files):
		for base, exts in files.items():
			self.output_alternatives(dir+'/'+base, exts)

	def output_alternatives(self, base, exts):
		print os.path.basename(base),
		for ext in exts:
			print '<a href="%s/%s%s">%s</a>'%(script_name, base, ext, ext),
		print

	def die(self, mess):
		print mess
		sys.exit()

class CGIStyle(Style):
	def output_directory(self, dir, files):
		print "Content-type: text/html\n"
		for base, exts in files.items():
			Style.output_alternatives(self, dir+'/'+base, exts)
			print '<br>'
	def output_alternatives(self, base, exts):
		print "Content-type: text/html\n"
		Style.output_alternatives(self, base, exts)
	def die(self, mess):
		print "Content-type: text/plain\n"
		Style.die(self, mess)

class HtmlStyle(Style):
	def process_query(self, file):
		try:
			_, file = validate_path(file)
		except error, mess:
			self.die(mess)
		try:
			files = list_directory(file)
		except error, mess1:
			try:
				files = list_files(file)
			except error, mess2:
				self.die('\n'.join(map(str, (mess1, mess2))))
			else:
				self.output_alternatives(file, files)
		else:
			self.output_directory(file, files)
	def run_cmd(self, cmd):
		print "ERROR - Tried to run",cmd
	
class TestStyle(CGIStyle):
	def __init__(self, file):
		self.outputfile = os.path.basename(file)
	def run_cmd(self, cmd):
		cmd += " | cat > /tmp/"+self.outputfile
		cmd += " ; file /tmp/"+self.outputfile
		print cmd
		sys.stdout.flush()
		os.execl('/bin/sh', 'sh', '-c', cmd)

if __name__ == '__main__':
	if len(sys.argv) > 1:
		if sys.argv[1] == 'l':	# list all available extensions.
			isTrusted = True
			list_formats()
			sys.exit()
		elif sys.argv[1] == 't':	# test; show the commands on stdout.
			isTrusted = True
			TestStyle(sys.argv[2]).process_query(sys.argv[2])
			sys.exit()
		elif sys.argv[1] == 'h':	# html. Generate links to the available formats.
			isTrusted = madrigalweb.isTrusted()
			HtmlStyle().process_query(sys.argv[2])
			sys.exit()
	if os.environ.get('GATEWAY_INTERFACE'):
		isTrusted = madrigalweb.isTrusted()
		CGIStyle().process_query(cgi_file())
	else:
		print "Use option t to test it from command line"
