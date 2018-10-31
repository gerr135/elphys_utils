/***************************************************************************
                          main.cpp  -  description
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

//#include <iostream.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

#include "abfheader.h"
#include "abffile.h"
#include <gsl/gsl_histogram.h>

int main(int argc, char *argv[])
{
//  cout << "Hello, World!" << endl;

  //lets parse parameters:
  int nchData=0;int nchPressure=1;
  float fLow=0;float fHigh=0;
  int nBins=13;int c;
  char* fName=NULL;char* outFName=NULL;
	
  while ((c = getopt (argc, argv, "d:p:l:h:n:")) != -1)
    switch (c) {
      case 'd': nchData = atoi(optarg);break;
      case 'p':	nchPressure = atoi(optarg);break;
      case 'l': fLow = atof(optarg);break;
      case 'h': fHigh = atof(optarg);break;
      case 'n': nBins = atoi(optarg);
  };
  printf("dataCh=%d, pressCh=%d, lowlim=%5.3f, hilim=%5.3f, nbins=%d\n",nchData,nchPressure,fLow,fHigh,nBins);
  if (optind<argc) fName=argv[optind]; else fName=NULL;
  if (optind<argc-1) outFName=argv[optind+1]; else outFName=NULL;
  printf("inFile: %s, outFile: %s\n",fName,outFName);
 	
  //now lets open the file
  FileRec F;
  ABFFileHeader pH;
  UINT nChanls;unsigned long nSamples;
  if (ABFOpenGapFree(fName,&F,&pH,&nChanls,&nSamples)) {printf("cannot open file %s\n",fName);exit(1);};

  //read elphys activity and pressure
  ABFBufPtr pData,pPress;
  if (ABFReadChannel(&F,&pH,nchData,&pData,&nSamples)){
     printf("could not read data\n");fclose(F.h);exit(1);};
  if (ABFSeekToDataStart(&F,&pH)){printf("cannot seek to start of data after reading channel0\n");exit(1);};
  if (ABFReadChannel(&F,&pH,nchPressure,&pPress,&nSamples)){
     printf("could not read data\n");fclose(F.h);exit(1);};
  //remember, pData and pPress are now allocated

  //close file and unnecessary buffers
  fclose(F.h);
	
  //allocate hystogram:
  //check if limits are specified or should evaluate min.max
  if ((fLow==0)&&(fHigh==0)) {
		//need to evaluate min/max
		printf("need to find min/max pressure...\n");
		float minP=pPress.p[0];float maxP=minP;
		for (int i=1;i<nSamples;i++) {
			if (minP>pPress.p[i]) minP=pPress.p[i];
			if (maxP<pPress.p[i]) maxP=pPress.p[i];};
		fLow=minP;fHigh=maxP;
		printf("minP=%5.3f, maxP=%5.3f\n\n",fLow,fHigh);
	};
  //then finally allocate histogram
  gsl_histogram* pCurrent=gsl_histogram_calloc_uniform(nBins,fLow,fHigh);
  gsl_histogram* pCounts =gsl_histogram_calloc_uniform(nBins,fLow,fHigh);
	
//cycle through data
  //we need to calculate mean current corresponding to each pressure bin
  for (UINT i=0;i<nSamples;i++) {
		gsl_histogram_accumulate(pCurrent,pPress.p[i],pData.p[i]);
		gsl_histogram_increment (pCounts, pPress.p[i]);
	};
  //now we need to normalize bins (divide every current entry by number of observations)
  gsl_histogram_div(pCurrent,pCounts);
	
//output hystogram
  //lets see if we write to stdout and open proper file:
  FILE* Fout;
  if ((outFName==NULL)||(outFName=="stdout")) Fout=stdout;	else Fout=fopen(outFName,"r");
  fprintf(Fout,"opening curve (mean current or NPopen vs suction applied)\n data file: %s\n\nrange1, range2, bin\n\n",fName);
  gsl_histogram_fprintf(Fout,pCurrent,"%6.2f,","%.3f");
  fprintf(Fout,"\ncounts:\n");	
  gsl_histogram_fprintf(Fout,pCounts,"%6.2f,","%.0f");
  fclose(Fout);
	
  //deallocate all buffers
  gsl_histogram_free(pCurrent);	gsl_histogram_free(pCounts);
  free(pData.p);free(pPress.p);

  return EXIT_SUCCESS;
}
