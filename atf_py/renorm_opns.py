#! /usr/bin/python
"this is a simple script to process (renormalize) the output of opncurve from abftools"

import sys,getopt,string

helpStr='''
This script reads output of opncurve from abftools and renormalizes it

usage: renorm_opns.py [-h] file
options:
-h     print this help
-f     perform boltzman fit and normalize accordingly
       (also subtracts "baseline")
'''

#may add direction - up or down :transition"

def ProcessCommandLine():
	"goes through command line, returns opts,FileName pair"
	#lets parse the arguments
	if len(sys.argv)<2:
		print helpStr
		sys.exit(1)
	else:
		try:
			opts,FileName=getopt.getopt(sys.argv[1:],"fh")
		except getopt.error,details:
			print 'unrecognized optins:',details
			print helpStr
			sys.exit()

	if ('-h','') in opts:
		print helpStr
		sys.exit()
	if ('-f','') in opts:doFit = 1
	else:doFit=0

	if FileName == []:
		print "file name should be given\n"
		print helpStr
		sys.exit()

	return FileName[0],doFit


def fitBoltzman(histo):
	"performs boltzman fit, returns x0, dx, A1, A2 tuple"
	import Simplex,math

	def goal(args):

		def Boltzman(x, x0,dx,A1,A2):
			return A2 + (A1-A2)/(1.0+math.exp((x-x0)/dx))

		S=0
		for (x,y) in histo:
			S = S + (y - Boltzman(x, args[0],args[1],args[2],args[3]))**2
		return S

	#get estimates
	x0,y0 = histo[-1]
	x1,y1 = histo[-2]
	smplx = Simplex.Simplex(goal,[x0,x0-x1,0,y0],[x0/5,(x0-x1)/5,y0/25,y0/25])
	return smplx.minimize()


#main block
FileName, doFit = ProcessCommandLine()
#print FileName,doFit

lines=open(FileName).readlines()
lines=map(string.strip,lines)
#first skip the header
for i in range(len(lines)):
	if lines[i] == "range1, range2, bin":
		lines = lines[i+2:]
		break
#print lines

#now get the hystogram
hysto = []
for line in lines:
	if line == "":break
	a,b, value = map(float,line.split(","))
	hysto.append(((a+b)/2,value))
#print hysto

if doFit:
	(x0,dx,A1,A2), err, iter = fitBoltzman(hysto)
	newHyst=[]
	for (center,value) in hysto:
		newHyst.append((center,(value - A1)/(A2-A1)))
	hysto = newHyst
else:
	#normalize to 1, no subtraction
	center, maxValue = hysto[0]
	for (center,value) in hysto[1:]:
		if value > maxValue:
				maxValue = value
	#now renormalize and reverse suction sign
	newHyst = []
	for (center,value) in hysto:
		newHyst.append((-center,value/maxValue))
	newHyst.reverse()
	hysto = newHyst

#now print the result
for (center,value) in hysto:
	print center, ", ", value

if doFit:
	#also output boltzman fit results
	print "\nfit resuts:"
	print "x0= %7.5f" % x0
	print "dx= %7.5f" % dx
	print "A1= %7.5f" % A1
	print "A2= %7.5f" % A2
	print "err= ", err
	print iter, " iterations"