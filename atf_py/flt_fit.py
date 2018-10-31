#! /usr/bin/python
''' This script reads boltzman fit output and removes apparently diverged results'''

import sys, getopt, string

#first exceptions
class Error(Exception):
	"""Base class for exceptions in this module."""
	pass


helpStr='''
This script reads boltzman fit output and removes apparently diverged results

usage: list_starts.py [-ht:m:d:] file
options:
-h        print this help
-t [u,d]  transition direction (forces A1<>A2 relation)
-m f      min |A2-A1| difference
-d f      max dx
(better specify them all, as I did not do carefull processing of what is given)
'''

def ProcessCommandLine():
	"goes through command line, returns opts,FileName pair"
	#lets parse the arguments
	if len(sys.argv)<2:
		print helpStr
		sys.exit(1)
	else:
		try:
			opts,FileName=getopt.getopt(sys.argv[1:],"d:hm:t:")
		except getopt.error,details:
			print 'unrecognized optins:',details
			print helpStr
			sys.exit()

	if ('-h','') in opts:
		print helpStr
		sys.exit()

	#some defaults
	transDirection="up" # takes "up" or "down"
	MinAmplDiff = 0.01 #nA
	MaxT = 10.0 #mksec

	for (opt,par) in opts:
		if opt=='-t':
			if   par=='d':transDirection="down"
			elif par=='u':transDirection="up"
			else:
				print "-d parameter must be one of 'u' or 'd'"
				sys.exit()
		if opt=='-m':
			MinAmplDiff = float(par)
		if opt=='-d':
			MaxT = float(par)

	if FileName == []:
		print "file name should be given"
		sys.exit()

	return FileName[0],(transDirection,MinAmplDiff,MaxT)


def readAtf(FileName):
	"read file given by name and return list of lines split into columns"

	lines = open(FileName).readlines()[2:]
	#first two lines don'tcontain anything

	#get and process header
	if lines[0].strip() != "NEpisode,   x0,   dx,  T10-90,   A1,   A2,   epsilon":
		print "this format is not supported (yet)"
		sys.exit()
	header = map(string.strip,lines[0].split(","))
	#print "header=",header

	#form the list and fill with one column only
	data=[]
	for line in lines[1:]:
		data.append( map(float,line.split(",")) )

	return header,data



#main block
FileName,(transDirection,MinAmplDiff,MaxDx) = ProcessCommandLine()
#print FileName,(transDirection,MinAmplDiff,MaxDx)

header,data=readAtf(FileName)

#!! change header output here!
print "NEpisode,   x0,   dx,  T,   A1,   A2,   epsilon"
for item in data:
	#for readability unpack the record
	NEpisode,x0,dx,T,A1,A2,epsilon=item
	#do the checks
	if abs(A2-A1) < MinAmplDiff:continue
	if T > MaxDx:continue
	if (transDirection=="up")   and (A1 > A2):continue
	if (transDirection=="down") and (A1 < A2):continue
	#did not trigger anything so far
	print ", ".join(map(str,item))
