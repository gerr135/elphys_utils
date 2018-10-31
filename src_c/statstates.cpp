/***************************************************************************
                          statstates.cpp  -  description
                             -------------------
    begin                : Tue Apr 30 22:34:22 PDT 2002
    copyright            : (C) 2001 by George Shapovalov
    email                : gerr@gasser.caltech.edu
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/


/*
This program takes abf data file with two channels and file 
containing dwell times for the data in the first file 
(it should have the following format:
level#	eventStart eventEnd dwt
). 
For every dwt it will calculate average value of the specified channel during 
that event. Output:
New file is created which has dwell time entry per every line:
Nstate	dwell_time	mean	stddev
*/


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

//#include <iostream.h>
#define _GNU_SOURCE
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <stream.h>
#include <math.h>

void printHelp()
{
cout << 
"average pressure on dwell time intervals \n" <<
"usage: statstates [options] data-file dwt-file \n\n" <<
"options:\n"<<
"-h		print help and exit\n"<<
"-q		quiet execution \n"<<
"-p n	# channel containing pressure\n"<<
"-o fn	output goes into file fn (otherwise to stdout)\n";
}

#include "abfheader.h"
#include "abffile.h"
//#include <gsl/gsl_histogram.h>

int main(int argc, char *argv[])
{
	//lets parse parameters:
	int nChan=1;
	int beQuiet=0;
	char* fnBin=NULL;char* fnDwt=NULL;
	char* outFName=NULL;
	
	int c;
	while ((c = getopt (argc, argv, "hqp:o:")) != -1) switch (c) {
		case 'h': printHelp();break;
		case 'p': nChan = atoi(optarg);break;
		case 'q': beQuiet = 1;break;
		case 'o': outFName = optarg;break;
	};
	
	if (!beQuiet) printf("parsed parameters: Ch#=%d, outFN=%s\n",nChan,outFName);
	if (optind<argc) fnBin=argv[optind]; 
	else {printf("you must specify binary data file!\n"); exit(1);};
 	if (optind<argc-1) fnDwt=argv[optind+1]; 
	else {printf("you must specify dwt filename!");exit(1);};
	if (!beQuiet) printf("abfFile: %s, dwtFile: %s\n",fnBin,fnDwt);
 	
	//read the data
	FileRec F;
	ABFFileHeader pH;
	UINT nChanls;unsigned long nSamples;
	if (ABFOpenGapFree(fnBin,&F,&pH,&nChanls,&nSamples)) {printf("cannot open file %s\n",fnBin);exit(1);};

	ABFBufPtr pData;
	if (ABFReadChannel(&F,&pH,nChan,&pData,&nSamples)){
		printf("could not read data\n");fclose(F.h);exit(1);};
	fclose(F.h);
	float dt=pH.fADCSampleInterval*nChanls;

	//read and process dwt file
	FILE* Fdwt = fopen(fnDwt,"r");
	if (Fdwt==NULL) {printf("could not open dwt file: %s\n",fnDwt);exit(EXIT_FAILURE);};
	//now chwck where we output and open that file
	FILE* Fout;
	if (outFName) Fout = fopen(outFName,"w");
	else Fout=stdout;
		
	//dwt file is supposed to have 1 line header
	char* dwtHead=NULL;size_t dwtHeadLen=0;
	getline(&dwtHead,&dwtHeadLen,Fdwt);
	fprintf(Fout,"Nstate	dwell_time	mean	stddev\n");
	if (!beQuiet) printf("done headers, dt=%f\n",dt);
	
	//now parse the rest of dwt file and process line by line
	unsigned int level=0;
	float eStart,eEnd,dwt;//dt is in usec, dwt's are in msec
	unsigned long iStart,iEnd,iN; //1st and last index of dwt interval
	int si=0;
	while ((si=fscanf(Fdwt,"%u	%e	%e	%e\n",&level,&eStart,&eEnd,&dwt)) && (si!=EOF) ) 
	{
		iStart=ceil(eStart*1000/dt);iEnd=ceil(eEnd*1000/dt);iN=iEnd-iStart;
		if (!beQuiet && iStart<870000) printf("iStart=%d, iEnd=%d, iN=%d, eStart=%f, eEnd=%f\n",iStart,iEnd,iN,eStart,eEnd);
		if (!iN) continue;
		double sum=0;
		double sum2=0;
		for (unsigned long i=iStart;i<iEnd; i++) {
			sum+=pData.p[i];
			sum2+=pData.p[i]*pData.p[i];
		};
		sum=sum/iN;sum2=sum2/iN;
		float stdDev=sqrt(sum2-sum*sum);
		fprintf(Fout,"%d	%f	%f	%f\n",level,dwt,sum,stdDev);
	}
	fclose(Fdwt);
	fclose(Fout);
	
	
	//deallocate all buffers
	free(pData.p);

  return EXIT_SUCCESS;
}
