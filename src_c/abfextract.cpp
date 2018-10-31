/*
   abfextract - extracts fragments of specified length from original (gapfree) file given list of fragments (or their start times)

   Copyright (C) 2003 George Shapovalov

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  

*/

#include <stdio.h>
#include <sys/types.h>
#include <getopt.h>
#include <stdlib.h>

#include "config.h"

#include "abfheader.h"
#include "abffile.h"

#include <math.h>
#include <gsl/gsl_errno.h>

#define EXIT_FAILURE 1

extern "C" {
  char *xstrdup (char *p);
}

static void usage (int status);

/* The name the program was run with, stripped of any leading path. */
char *program_name;

/* Option flags and variables */


static struct option const long_options[] =
{
  {"help", no_argument, 0, 'h'},
  {"version", no_argument, 0, 'V'},
  {NULL, 0, NULL, 0}
};

static int decode_switches (int argc, char **argv);
static int readStartsTXT(); //reads start times from simple list (one number per row); forward;

//some global definitions
char* listFName=NULL;
char* FName=NULL;
int fragLength=0; //the fragment length (from the actual start)
int offsetLeft=0;  //offset x mksek to the left from listed start time for fragments
int nfrags=0;
float* starts; //will keep an array of atart times. readStarts* allocs it with 0..nfrags-1 floats

int
main (int argc, char **argv)
{
  int i;
  program_name = argv[0];
  //printf("starting decoding switches...");
  i = decode_switches (argc, argv);
  //printf("switches decoded; retcode=%d \n",i);

  /* do the work */
	readStartsTXT(); //updates globals nfrags and starts

	//open and process the data
	FileRec F;
	ABFFileHeader H;
	UINT nChanls; unsigned long nSamples;

	if (ABFOpenGapFree(FName,&F,&H,&nChanls,&nSamples)) {printf("cannot open file %s\n",FName);exit(1);};
	if (nChanls != 1) GSL_ERROR("multichannel bufer reads not implemented yet!",GSL_EFAILED);
	UINT uSampleSize=SampleSize(&H);
	if (uSampleSize != sizeof(float)) GSL_ERROR("Please convert your data into floating point abf format!",GSL_EFAILED);

	float* bufs[nfrags];
	for (int i=0; i<nfrags;i++) {
		bufs[i] = (float*)calloc(1,uSampleSize*fragLength);
		if (!bufs[i]) GSL_ERROR("could not allocate buffer!",GSL_EFAILED);
		//convert milisecs into points
		unsigned long pos=floor(starts[i]*H.fADCSampleInterval*1000) - offsetLeft;
		//starts are in ms, while sampleInterval in mks; offsetLeft is in mks
		UINT localLength=fragLength;
		//printf("i=%d,  pos=%d,  fragLen=%d\n",i,pos,localLength);
		ABFReadBlock(&F, &H, bufs[i], pos, &localLength);
	}
	fclose(F.h);

	//test-dump contents of buffs
	/*FILE* F1=fopen("dump.tst","w");
	for (int i=0; i<nfrags; i++){
		fprintf(F1,"\n\ni=%d\n",i);
		float* loc=bufs[i];
		//size_t numwrite=fwrite(bufs[i],fragLength*sizeof(float),1,F1);
		//for (int j=0;j<fragLength;j++) {
		//	fprintf(F1,"%f\t",loc);loc++;
		//}
	}
	fclose(F1);*/

	//now output the atf file.
	//use stdout for now
	printf("ATF\t1.0\n");
	printf("0\t%d\n",nfrags+1);

	//print units
	printf("\"Time (s)\"\t");
	for (int col=0; col <nfrags; col++) printf("\"Trace #%d (nA)\"\t",col);
	printf("\n");
	//here goes the data
	for (int row=0;row<fragLength;row++) {
		printf("%f\t",row*H.fADCSampleInterval);//time in mks
		for (int col=0; col<nfrags;col++)
			printf("%f\t",bufs[col][row]);
		printf("\n");
	}

  exit (0);
}


/*reads the presumed start times from txt file
This is a real read, as the correct list is stored in results wilndow
When copied in separate file, will have a column of numbers (one per row)*/

static int
readStartsTXT() {
	FILE* Fl=fopen(listFName,"r");
	if (!Fl) GSL_ERROR("could not open fragment list file!",GSL_EFAILED);

	char* line=NULL;
	size_t len=0;

	//calculate nfrags first
	nfrags=0;
	while (getline(&line,&len,Fl) != -1) nfrags++;

	//alloc appropriate space for starts
	starts=(float*)calloc(nfrags,sizeof(float));
	if (!starts) GSL_ERROR("could not calloc srats!",GSL_EFAILED);

	//seek to start and read lines now
	if (fseek(Fl,0,SEEK_SET)==-1) GSL_ERROR("could not seek to the beginning of the list!",GSL_EFAILED);
	for (int i=0;i<nfrags;i++) {
		ssize_t numread=getline(&line,&len,Fl);
		if (numread==-1) GSL_ERROR("unexpected problem reading list for the second time!",GSL_EFAILED);
		starts[i]=atof(line);
	}
	fclose(Fl);
}

/*reads the presumed start times from atf file
use this func to keep the code, in case will need it again*/

static int
readStartsATF() {
	//open list file and form list of start positions
	FILE* Fl = fopen(listFName,"r");
	if (!Fl) GSL_ERROR("could not open fragment list file!",GSL_EFAILED);
	int scanfReslt;
	char pHdr[5]; float ABFver;
	//get header
	char* line=NULL; size_t len=0;
	ssize_t numread=getline(&line,&len,Fl);
	//printf("numread=%d,  line=%s\n",len,line);

	//get ncomments and nfrags
	int ncomm;
	scanfReslt = fscanf(Fl,"%d\t%d", &ncomm, &nfrags);
	nfrags--;//col1 contains "x-axis"
	//printf("ncomm=%d, nfrags=%d\n", ncomm, nfrags);

	float starts[nfrags];//alloc space for start times

	//frag starts are listed somewhere in the comments, have to match a proper line..
	char* strStarts; char* strUnits;
	for (int i=0;i<ncomm;i++) {
		ssize_t numread=getline(&line,&len,Fl);
		if ( strstr(line,"SweepStartTimesMS") ) {
			strStarts=line;
			line=NULL;//otherwise getline reuses buffer and contents of strStarts gets overwritten
		}
	}
	numread=getline(&strUnits,&len,Fl);
	//printf("strUnits:  %s",strUnits);


	//process the start times
	line=strStarts;
	line+=19;//skip to the start of data
	//printf("sting to scan:   %s",line);
	float tmp1;
	for (int i=0;i<nfrags;i++) {
		scanfReslt=sscanf(line,"%f",&tmp1);
		if (scanfReslt != 1) {
			printf("Error while scanning start times string!  i=%d\n",i);
			exit(0);
		}
		starts[i]=tmp1;
		//if (i>nfrags-10) printf("start[%d]=%f, %f\n",i,starts[i],tmp1);
		if (i==nfrags-1) break;
		while (*line++ != ',') ;
		line++;
	}
	fclose(Fl);
}

/* Set all the option flags according to the switches specified.
   Return the index of the first non-option argument.  */

static int
decode_switches (int argc, char **argv)
{
  int c;

  while ((c = getopt_long (argc, argv, 
			   "h"	/* help */
			   "V"	/* version */
			   "l:n:s:",
			   long_options, (int *) 0)) != EOF)
    {
      switch (c)
	{
	case 'V':
	  printf ("abfextract %s\n", VERSION);
	  exit (0);

	case 'h': usage (0);break;

	case 'l': listFName = optarg;break;
	case 'n': fragLength = atoi(optarg);break;
	case 's': offsetLeft=atoi(optarg);break;

	default:
	usage (EXIT_FAILURE);
	}
    }

	if (optind < argc) {
		FName=argv[optind];
	} else {
		printf("Please specify name of the file where to get fragments from \n");
		exit(EXIT_FAILURE);
	}

	if (listFName == NULL) {
		printf("Please specify the name of the file containing the list of fragments \n");
		exit(EXIT_FAILURE);
	}

	if (fragLength == 0) {
		printf("Please specify fragment length \n");
		exit(EXIT_FAILURE);
	}

	//test print
	//printf("parsed input parameters, Fl=%s;  F=%s;  n=%d  ",listFName,FName,fragLength);

  return optind;
}


static void
usage (int status)
{
  printf ("%s - \
Extracts fragments of specified length from original (gapfree) file given list of fragments (or their start times)\n", program_name);
  printf ("Usage: %s [OPTION] FILE\n", program_name);
  printf ("\
Options: \n\
  -h, --help                 display this help and exit\n\
  -V, --version              output version information and exit\n\
  -n                         fragment length (in mks) \n\
  -s num                     offset num mkseconds to the left to start the fragments \n\
  -l list                    file containing list of fragment starts \n\
  FILE                       is a file where to get the fragments from, \n\
  output goes into stdout \n\
");
  exit (status);
}
