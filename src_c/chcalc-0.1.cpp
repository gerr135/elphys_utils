/***************************************************************************
                          chcalc.cpp  -  description
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
Channel arythmetics:
Calculates specified (by opcode) operation on two channels

usage: chcalc -opcode [options] -x file1 [[options] -y file2] [-z fileOut]

options:
-n nchanl   number of channel to dump (default: 0)
-t dt       specifies new time interval
-q          be quiet: no output except data
-h          print this help

-n is "local" option - it should precede -x or -y filenames, then it takes effect for that file,
after -{x,y} is processed it is cleared, so the following file will take its local value if specified.
Other that that, there are no ordering requirements: everything else can be in any order.

-opcode -  one of:
-a	   add         z_i = x_i + y_i
-s	   subtract    z_i = x_i - y_i
-m	   mul         z_i = x_i * y_i
-d	   divide      z_i = x_i / y_i
-b f   scale       z_i = x_i * f
-c f   shift       z_i = x_i + f

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_vector.h>

#include "lib/abfheader.h"
#include "lib/abffile.h"

//some opcode consts
#define OP_ADD 1
#define OP_SUB 2
#define OP_MUL 4
#define OP_DIV 8
#define OP_SCL 16	//"scaling" by predefined factor
#define OP_SFT 32	//"shift"


void printHelp(char* progName)
{
	printf("Channel arythmetics:\nCalculates specified (by opcode) operation on two channels\n\n");
	printf("usage: %s -opcode [options] -x file1 [[options] -y file2] [-z fileOut]\n\n",progName);

	printf("options:\n-n nchanl   number of channel to dump (default: 0)\n");
	printf("-t dt       specifies new time interval\n");
	printf("-q          be quiet: no output except data\n-h          print this help\n\n");
	printf("-n is 'local' option - it should precede -x or -y filenames, then it takes effect for that file,\n");
	printf("after -{x,y} is processed it is cleared, so the following file will take its local value if specified.\n");
	printf("Other that that, there are no ordering requirements: everything else can be in any order.\n\n");

	printf("-opcode -  one of:\n");
	printf("-a     add         z_i = x_i + y_i\n-s   subtract    z_i = x_i - y_i\n-m     mul         z_i = x_i * y_i\n-d     divide      z_i = x_i / y_i\n");
	printf("-b f   scale       z_i = x_i * f\n-c f   shift       z_i = x_i + f\n\n");
}


//this function resamples pBuf from specified time step dt0 to new time step dt
//nSamples is trace duration in points, gets recalculated
//just do linear interpolation here
//should be good for 5x and higher oversampling  and dt<=dt0
//Thus no need for time array and gsl procedures
int resampleBuf ( ABFBufPtr* pBuf, const float dt0, const float dt, unsigned long *nSamples )
{
	//recalculate nSamples
	unsigned long nSamplesNew=floor(*nSamples * dt0/dt);

	ABFBufPtr Buf1;
	Buf1.p=(float*)malloc(nSamplesNew*sizeof(float));
	if (!Buf1.p) GSL_ERROR("could not allocate buffer for resampling\n",GSL_ENOMEM);

	//calculation
	float t=0;
	for (unsigned long i=0;i<nSamplesNew;i++) {
		unsigned long i0=floor(i*dt/dt0);
		if (i0==*nSamples) {Buf1.p[i]=pBuf->p[i0];continue;};//need special handling for this
		//just breaking when i0 reaches Samples might cause undefined array values,
		//lets specifically check this condition
		//anyway, this overrun should not happen ever
		if (i0 > *nSamples) GSL_ERROR("should not get here!!",GSL_ESANITY);

		float dtt=i*dt - i0*dt0;
		Buf1.p[i] = pBuf->p[i0] + (pBuf->p[i0+1] - pBuf->p[i0]) * dtt/dt0;
	}

	//update pointers and free old buffers.
	free(pBuf->p);pBuf->p=Buf1.p;
	*nSamples=nSamplesNew;

	return EXIT_SUCCESS;
}


int main(int argc, char *argv[])
{

  int opCode=0;char opSymb=' ';
  int nChanl=0;int nChanl1=0; int nChanl2=0;
  int beQuiet=0;
  char* fn1=NULL;char* fn2=NULL;char* outFName=NULL;
  float dt1,dt2,dt=0;//time intervals - moved here since dt might be specified in the command line


  //parse parameters
  int c;
  while ((c = getopt (argc, argv, "admsb:c:qhn:t:x:y:z:")) != -1)
    switch (c) {
	  case 'a': opCode=OP_ADD;opSymb='+';break;
	  case 's': opCode=OP_SUB;opSymb='-';break;
	  case 'm': opCode=OP_MUL;opSymb='*';break;
	  case 'd': opCode=OP_DIV;opSymb='/';break;
	  case 'b': opCode=OP_SCL;opSymb='^';break;
	  case 'c': opCode=OP_SFT;opSymb='>';break;

	  case 'n': nChanl = atoi(optarg);break;
	  case 't': dt=atof(optarg);if (dt==0) GSL_ERROR("invalide value for time interval",GSL_EINVAL); break;
      case 'q': beQuiet=1;break;
	  case 'h': printHelp(argv[0]);exit(3);
      case 'x': fn1 = optarg;nChanl1=nChanl;nChanl=0;break;
      case 'y': fn2 = optarg;nChanl2=nChanl;nChanl=0;break;
      case 'z': outFName = optarg;break;
  };

  if ( (opCode & (OP_ADD|OP_SUB|OP_MUL|OP_DIV)) && (!fn1)&&(!fn2) ) {printf("you must specify two input files for this operation!\n");exit(2);};
  if ( (opCode & (OP_SCL|OP_SFT)) && (!fn1) ) {printf("you must specify input file name!\n");exit(2);};
  if (!beQuiet) printf(
    "\ncalcuating:\n %s#%d %c %s#%d -> %s\n\n",fn1,nChanl1,opSymb,fn2,nChanl2,outFName);

  //now lets open and read files
  FileRec F1,F2,F;
  ABFFileHeader H1,H2;
  UINT nChanls1,nChanls2;// num channels stored in every file
  unsigned long nSamples1,nSamples2,nSamples;
  const float ddt=0.1;//in mksec, possible accuracy, probably may set to 1mksec
  ABFBufPtr pData1,pData2,pData;


  if (ABFOpenGapFree(fn1,&F1,&H1,&nChanls1,&nSamples1)) {printf("cannot open file %s\n",fn1);exit(1);};
  if (ABFReadChannel(&F1,&H1,nChanl1,&pData1,&nSamples1)){
     printf("could not read data from file %s\n",fn1);fclose(F1.h);exit(1);};
  dt1=H1.fADCSampleInterval*nChanls1;//multiple channels per file are multiplexed.
  // So real spasing for every channel is a multiplicative
  //of specified time interval and N channels
  fclose(F1.h);
  if (ABFOpenGapFree(fn2,&F2,&H2,&nChanls2,&nSamples2)) {printf("cannot open file %s\n",fn2);exit(1);};
  if (ABFReadChannel(&F2,&H2,nChanl2,&pData2,&nSamples2)){
     printf("could not read data from file %s\n",fn2);fclose(F2.h);exit(1);};
  dt2=H2.fADCSampleInterval*nChanls2;
  fclose(F2.h);
  //sanity check (times are in mks):
  if ( (dt1<2) && (dt1>1000000) && (dt2<2) && (dt2>1000000) ) {printf("bad sample interval!\n");exit(2);}

  //check if some time interval is forsed
  if (dt!=0) {//dt is set, lets resample what's necessary
  	if ( fabs(dt1-dt)>ddt ) resampleBuf(&pData1,dt1,dt,&nSamples1);
  	if ( fabs(dt2-dt)>ddt ) resampleBuf(&pData2,dt2,dt,&nSamples2);
  }
  else // choose dt (use smller one), might need to resample one channel
  {
	dt= dt1<dt2 ? dt1 : dt2;
	//then just repeat above block
  	if ( fabs(dt1-dt)>ddt ) resampleBuf(&pData1,dt1,dt,&nSamples1);
  	if ( fabs(dt2-dt)>ddt ) resampleBuf(&pData2,dt2,dt,&nSamples2);
  }

  if (!beQuiet) printf("time intervals: dt1=%5.2f, dt2=%f5.2, dt=%f5.2\n\n",dt1,dt2,dt);

	//now, as input data has equal time inervals we can just add arrays,
	//first calculate necessary size and create new buffer
	nSamples = nSamples1 >? nSamples2; //try g++ extension, same as the line below
	//nSamples= nSamples1<nSamples2 ? nSamples2:nSamples1;

	pData.p=(float*)malloc(nSamples*sizeof(float));

	unsigned long ns= nSamples1<nSamples2 ? nSamples1 : nSamples2;//iteration max - has to be the lesser vaue

	gsl_vector_float_view vv1=gsl_vector_float_view_array(pData1.p,ns);
	gsl_vector_float_view vv2=gsl_vector_float_view_array(pData2.p,ns);
	gsl_vector_float_view vv=gsl_vector_float_view_array(pData.p,ns);

	if (nSamples1<nSamples2) {
		memcpy(pData.p,pData2.p,nSamples);
		switch (opCode) {
			case OP_ADD: if ( gsl_vector_float_add(&vv.vector,&vv1.vector) ) printf("problem adding vectors");break;
			case OP_SUB: if ( gsl_vector_float_sub(&vv.vector,&vv1.vector) ) printf("problem subtracting vectors");break;
			case OP_MUL: if ( gsl_vector_float_mul(&vv.vector,&vv1.vector) ) printf("problem multiplying vectors");break;
			case OP_DIV: if ( gsl_vector_float_div(&vv.vector,&vv1.vector) ) printf("problem dividing vectors");break;
		}
	} else {
		memcpy(pData.p,pData1.p,nSamples);
		switch (opCode) {
			case OP_ADD: if ( gsl_vector_float_add(&vv.vector,&vv2.vector) ) printf("problem adding vectors");break;
			case OP_SUB: if ( gsl_vector_float_sub(&vv.vector,&vv2.vector) ) printf("problem subtracting vectors");break;
			case OP_MUL: if ( gsl_vector_float_mul(&vv.vector,&vv2.vector) ) printf("problem multiplying vectors");break;
			case OP_DIV: if ( gsl_vector_float_div(&vv.vector,&vv2.vector) ) printf("problem dividing vectors");break;
		}
	}


  //deallocate original data buffers
  free(pData1.p);free(pData2.p);

  //lets output and free our data
  //create "empty" header and modify necessary entries
  ABFFileHeader* pH = ABFNewGapFreeHeader();
  pH->fADCSampleInterval=dt;
  pH->lActualAcqLength=nSamples;
  pH->nDataFormat = 1;//IEEE float
  //open file
	if (outFName=="stdout" || outFName=="" || outFName=="-"){F.h=stdout;F.fn="stdout";}
	else{
		F.fn=outFName;
		F.h=fopen(outFName,"w");//printf("file pointer: %p\n",F->h);
	}
	//check if opened Ok
	if(!F.h) GSL_ERROR ("cannot open out-file for writing", ABF_EOPENFILE );
  //write the header
  if ( !fwrite(pH,sizeof(ABFFileHeader),1,F.h) ) GSL_ERROR ("cannot write ABF header", ABFH_EHEADERWRITE);
  free(pH);

  //write the data
  unsigned long numWritten=fwrite(pData.p,sizeof(float),nSamples,F.h);
  if (!numWritten) GSL_ERROR("problem writing data!",GSL_FAILURE);
  if (numWritten!=nSamples) printf("\nATTENTION!!!\nCould not write all the data!\n\n");

  free(pData.p);fclose(F.h);

  return EXIT_SUCCESS;
}



