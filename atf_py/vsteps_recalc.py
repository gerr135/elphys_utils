#! /usr/bin/python
'''This small scripts reads raw average currents for step protocol
and performs necessary series resistance and junktion potential (offset) adjustments.
Output is either (total) patch conductance or NPopen

apparently incomplete'''

import sys, getopt

def printHelp:
	print '''
usage: %s [options] file
output goes to stdout

Options:
-h       print this help
-c f     renormalise results by give single-channel conductance
-r f     pitette resistance (for series R compensation)
''' % sys.argv[0]


def getParams:
	"read command line params"
	try:
		opts, args = getopt.gnu_getopt(sys.argv[1:], "hr:" )
	except getopt.GetoptError:
		# print help information and exit:
		printHelp()
		sys.exit(2)
	#now precess the params
	UnitaryConductance = 0
	for o, a in opts:
		if o == "-h":
			printHelp()
			sys.exit()
		if o == "-c":
			UnitaryConductance = float(a)
		if o == "-r":
			SeriesResistance   = float(a)
	if len(args) > 0:
		InputFile = args[0]
	else:
		printHelp()
		sys.exit()
	return SeriesResistance, UnitaryConductance, InputFile

#Main block
SeriesResistance, UnitaryConductance, InputFile = getParams()

data = readTable(InputFile)
