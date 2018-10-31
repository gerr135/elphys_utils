#! /bin/sh
#
#this script is a wrtapper calling abfinfo, dumpdata and graph (plotutils)
#
#usage: abfplot fileName chanNum

#echo "dropped, use abfplot.py instead"
#exit

if [ -z $1 ]; then
	echo "usage: $0 fileName nCnanl"
	echo "if nChanl is omitted 0 will be used"
	exit;
fi

if [ -z $GAPFREE_TOOLS_ROOT ]; then GAPFREE_TOOLS_ROOT=$HOME/proj/elphys/abf/gapfree-tools; fi
echo "checked root"

#parse input and check if channel number is specified
FName=$1
if [ -z $2 ]; then chN=0; else chN=$2; fi
echo "parsed params..."

#now call abfinfo
TMPFILE=`mktemp -q /tmp/abfplot.XXXXXX`
if [ $? -ne 0 ]; then
	echo "$0: Can't create temp file to parse abfinfo output, exiting..."
	exit 1
fi
$GAPFREE_TOOLS_ROOT/util/abfinfo $FName > $TMPFILE
echo "abfinfo finished"

#extract time interval, num channels and data length
NChan=`grep -e "num channels:" $TMPFILE|cut -d " " -f 3`
SampleInt=`grep -e "sample intevals," $TMPFILE|cut -d ":" -f 2|cut -d "," -f 1|tr -d " "`
echo "SampleInt=$SampleInt"
let SampleInterval=$SampleInt*$NChan/1000
NSampl=`grep -e "ack length:" $TMPFILE|cut -d " " -f 3`

#so now we that we know timings and the lengsth, lets plot the data
graph -T X -I f -a $SampleInterval 0 < $GAPFREE_TOOLS_ROOT/util/dumpdata -n $chN $FName

