/***************************************************************************
                          chint.cpp  -  description
                             -------------------
    begin                : Sun Jun 24 19:41:22 MDT 2001
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
calculates an integral under the trace or selected part of it.
Normalization: can be set by time-unit and current-unit switches, pA*msec by default

usage: chint [options] filename

options:
-n nchanl    number of channel to analyze (default: 0)
-q           be quiet: no output except integral value
-h           print this help and exit
-t tu        time units, one of s,sec for seconds, ms,msec for millisec,
-c cu        units of current: pA, nA or mA
*/


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

//#include <iostream.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <stream.h>

#include "abfheader.h"
#include "abffile.h"
//#include <gsl/gsl_errno.h>


void printHelp()
{
cout<<
"calculates an integral under the trace or selected part of it.\n"<<
"Normalization: can be set by time-unit and current-unit switches, pA*msec by default\n\n"<<
"usage: chint [options] filename\n\n"<<
"options:\n"<<
"-n nchanl    number of channel to analyze (default: 0)\n"<<
"-q           be quiet: no output except integral value\n"<<
"-h           print this help and exit\n"<<
"-t tu        time units, one of s,sec for seconds, ms,msec for millisec,\n"<<
"-c cu        units of current: pA, nA or mA\n"<<
"-b float     baseline in specified current units\n"<<
"-x float     left boundry of integration, in units specified or ms otherwise\n"<<
"-y float     right integration boundry\n";
}

int main(int argc, char *argv[])
{
  int nChanl=0;
  int beQuiet=0;
  char* fName=NULL;
  int tUnits=tuMSEC;
  int cUnits=cuPA;//default values
  double baseline=0;
  float tx=0,ty=0;//integration region ([x,y])

  //lets parse parameters:
  int c;
  while ((c = getopt (argc, argv, "n:qht:c:b:x:y:")) != -1)
    switch (c) {
      case 'n': nChanl = atoi(optarg);break;
      case 'h': printHelp();break;
      case 'q': beQuiet=1;break;
	  case 't': if ( !( strcasecmp(optarg,"s") && strcasecmp(optarg,"sec")) ) tUnits=tuSEC; else
	            if ( !( strcasecmp(optarg,"ms")&& strcasecmp(optarg,"msec")) ) tUnits=tuMSEC;
				else {printf("invalid value specified for time units"); exit(1);} break;
	  case 'c': if (!strcasecmp(optarg,"pa")) cUnits=cuPA; else
	            if (!strcasecmp(optarg,"na")) cUnits=cuNA; else
	            if (!strcasecmp(optarg,"ma")) cUnits=cuMA;  else
					{printf("invalid value specified for units of current"); exit(1);} break;
	  case 'b': baseline = atof(optarg);break;
	  case 'x': tx=atof(optarg);break;
	  case 'y': ty=atof(optarg);break;
  };
  if (!beQuiet) printf("parsing parameters, will use ch#:%d; \n",nChanl);
  if (optind<argc) fName=argv[optind]; else fName=NULL;
  if (!beQuiet) printf("inFile: %s\n",fName);
  
  //check boundries
  if (tx<0) {tx=0;if (!beQuiet)printf("negative left integration boundry specified, assuming 0!!!\n");};
  if (ty<0) {ty=0;if (!beQuiet)printf("negative right integration boundry specified, assuming end_of_trace!!!\n");};
  if (ty<tx) {float prm=tx;tx=ty;ty=prm;};//or should I complain and abort here?
  //need to check tx,ty later on again, when file description is read.

  //now lets open the file
  FileRec F;
  ABFFileHeader pH;
  UINT nChanls;unsigned long nSamples;
  if (ABFOpenGapFree(fName,&F,&pH,&nChanls,&nSamples)) {printf("cannot open file %s\n",fName);exit(1);};

  //print some statistics on file
  if (!beQuiet) printf("data format:%d \n",pH.nDataFormat);

  //read requested channel
  ABFBufPtr pData;
  if (ABFReadChannel(&F,&pH,nChanl,&pData,&nSamples)){
     printf("could not read data\n");fclose(F.h);exit(1);};
  //remember, pData is now allocated

  //close file and unnecessary buffers
  fclose(F.h);

//perform the integration
  //there are two important things:
  //1. accuracy: since N of samples can easily reach 1E9, we loose accuracy and may even overload double (its mantisse at least)
  //   therefore we need to do "two stage" addition.
  //2. baseline: we don't want to integrate the baseline, do we?
  //   to subtract it, the baseline has to be specified on the comand line or
  //   the program need at least 50 points before the first transition
  //   (not to be very complicated - even at 1kHz this is just 50ms)
#define blNCheckPoints 50
#define NAtom 25 //number of non-checked additions

  //check if baseline was specified, otherwise get an estimate
  if (baseline==0) {
	double s=0;
	for (int i=0;i<blNCheckPoints;i++) s+=pData.p[i];
	baseline=s/blNCheckPoints;
  }

  //now lets do the integration
  unsigned long i=0;
  const double stepThreshold = 1E6 * fabs(baseline);
  double integral=0;

  while (i<nSamples-NAtom) {
	double s;
	while ( (i<nSamples-NAtom) && (fabs(s)<=stepThreshold) ){
		//to avoid too much overhead don't compare after every addition
		s=0;
		for (int j=0;j<NAtom;j++) s += pData.p[i+j];
		i += NAtom;
		integral += s - baseline*NAtom;
	}
  }
  //just need to finish-up the integration
  for (int j=0;i+j<nSamples;j++) integral += pData.p[i+j]-baseline;

  //deallocate all buffers
  free(pData.p);

  //need to normalize the sum
  //time units in file are always mks
  float tScale;
  if (tUnits==tuSEC) tScale=1e6; else {tScale=1000;tUnits=tuMSEC;};
  double normint = integral * pH.fADCSampleInterval * nChanls / tScale;
  //printf("integration complete, need to adjust current units\n");

  float cScale=ABFGetCScaleFactor(&pH,nChanl,cUnits);
  normint = cScale*normint;

  //output the integral
	char cuStr[3];
	char tuStr[4];
	switch (cUnits){
		case cuPA:strncpy(cuStr, "pA\0",3);break;
		case cuNA:strncpy(cuStr, "nA\0",3);break;
		case cuMA:strncpy(cuStr, "mA\0",3);break;
		case cuUNDEFINED:strncpy(cuStr,"??\0",3);break;
	}
	if (tUnits==tuSEC) strncpy(tuStr, "sec\0",4); else strncpy(tuStr, "ms\0",4);

  printf("int=%g (%s*%s)\n",normint,cuStr,tuStr);

  return EXIT_SUCCESS;
}
