#! /usr/bin/python
''' This script reads atf file and extracts 5th or specified column of numbers'''

import sys, getopt

#first exceptions
class Error(Exception):
	"""Base class for exceptions in this module."""
	pass


helpStr='''
usage: list_starts.py [-hn] file
options:
-h    print this help
-n    dump given column
'''

def ProcessCommandLine():
	"goes through command line, returns opts,FileName pair"
	#lets parse the arguments
	if len(sys.argv)<2:
		print helpStr
		sys.exit(1)
	else:
		try:
			opts,FileName=getopt.getopt(sys.argv[1:],"hn:")
		except getopt.error,details:
			print 'unrecognized optins:',details
			print helpStr
			sys.exit()

	if ('-h','') in opts:
		print helpStr
		sys.exit()

	#real options
	NColumn=5
	for (opt,par) in opts:
		if opt=='-n':
			NColumn = int(par)

	if FileName == []:
		print "file name should be given"
		sys.exit()

	return FileName[0],NColumn


def readAtf(FileName,N):
	"read file given by name and return list of numbers in N'th column"

	lines = open(FileName).readlines()
	if lines[0].strip() != "ATF\t1.0":
		print "invalid ATF header"
		sys.exit()

	#skip header
	NComments, NEpisodes = map(int,lines[1].split())
	#print "NComments=",NComments,",  NEpisodes=",NEpisodes
	#NEpisodes here includes x's
	lines = lines[NComments+3:]
	#print lines

	#form the list and fill with one column only
	starts=[]
	for line in lines:
		starts.append(line.split()[N-1])

	return starts



#main block
FileName,NColumn = ProcessCommandLine()
starts=readAtf(FileName,NColumn)

for item in starts:
	print item
