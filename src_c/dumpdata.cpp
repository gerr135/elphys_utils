/***************************************************************************
                          dumpdata.cpp  -  description
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
dumps data from one cannel in raw format (stream of ineger or float)
One can use sox for example to transform the data to some sound file

usage: dumpdata [options] filename

options:
-n nchanl	number of channel to dump (default: 0)
-o outfname	output file name, stdout if omitted
-q		be quiet: no output except data
*/


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
  int nChanl=0;
  int beQuiet=0;
  char* fName=NULL;char* outFName=NULL;
  int c;
  	
  while ((c = getopt (argc, argv, "n:o:q")) != -1)
    switch (c) {
      case 'n': nChanl = atoi(optarg);break;
      case 'o':	outFName = optarg;break;
      case 'q':beQuiet=1;break;
  };
  if (!beQuiet) printf("parsed parameters, will dump ch#:%d; \n",nChanl);
  if (optind<argc) fName=argv[optind]; else fName=NULL;
  //if (optind<argc-1) outFName=argv[optind+1]; else outFName=NULL;
  if (!beQuiet) printf("inFile: %s, outFile: %s\n",fName,outFName);
 	
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
	
//damp the data
  //lets see if we write to stdout and open proper file:
  FILE* Fout;
  if ((outFName==NULL)||(outFName=="stdout")) Fout=stdout; else Fout=fopen(outFName,"w");
  fwrite(pData.p,SampleSize(&pH),nSamples,Fout);
  fclose(Fout);
	
  //deallocate all buffers
  free(pData.p);

  return EXIT_SUCCESS;
}
