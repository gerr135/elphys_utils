/***************************************************************************
                          abffile.cpp  -  description
                             -------------------
    begin                : Sun Jun 17 2001
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

//#include "compdefs.h"
#include "abfheader.h"
#include "abffile.h"
#include <stdlib.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_message.h>


int ABFReadRawChannel(FileRec *F,
	ABFFileHeader *pH,	int nChanl,	void* pBuf,	unsigned long *pnSamples)
 {
	//check nChannel requested
	if (nChanl>pH->nADCNumChannels-1) GSL_ERROR("wrond nChannel requested", ABFH_CHANNELNOTSAMPLED );

/*
	//now lets find requested channel # as they are ordered in file:
	UINT chNInFile=ABF_ADCCOUNT;
	for (int i=0;i<pH->nADCNumChannels;i++) if (pH->nADCSamplingSeq[i]==nChanl) chNInFile=i;
	if (chNInFile==ABF_ADCCOUNT){printf("nChannel requested is not present in file\n");return 0;};
*/
	//set sample size
	UINT uSampleSize=SampleSize(pH);

  //check if there is only one channel in file,
  //if only one read it directly to passed buffer
	if (pH->nADCNumChannels==1) {
		UINT numRead=fread(pBuf,uSampleSize,*pnSamples,F->h);
		if (numRead==0 || ferror(F->h)) GSL_ERROR("could not read data",ABF_EREADDATA);
		if (numRead<*pnSamples) GSL_MESSAGE("less data than requested",GSL_MESSAGE_MASK_A);
		*pnSamples=numRead;
	}
	else
	//if there is more then one channel need to create temp. buffer,
	//read multiplexed data, decimate it and dispose of temporary buffer.
	{
		UINT sz=uSampleSize* (*pnSamples)*pH->nADCNumChannels;
		void* pTemp=malloc(sz);//printf("sz=%d",sz);
		if (!pTemp) GSL_ERROR("could not allocate temporary buffer",ABFH_ENOMEMORY);

		UINT numRead=fread(pTemp,uSampleSize*pH->nADCNumChannels,*pnSamples,F->h);
		if (numRead==0 || ferror(F->h)) GSL_ERROR("could not read data",ABF_EREADDATA);
		if (numRead<*pnSamples) GSL_MESSAGE("less data than requested",GSL_MESSAGE_MASK_A);
//		printf("ReadRaw: samples requested: %d,   samples read: %d\n",*nSamples,numRead);
		*pnSamples=numRead;

		//now lets decimate
		//pBuf is returned, pxBuf is temporary pointer to span pBuf contents
		if (uSampleSize==2){
			short* psBuf =(short *)pBuf;
			short* psTemp=(short *)pTemp;
			for (UINT i=nChanl;i<*pnSamples*pH->nADCNumChannels;i+=pH->nADCNumChannels)
				*psBuf++ = psTemp[i];
		}
		else {
			float* pfBuf =(float *)pBuf;
			float* pfTemp=(float *)pTemp;
			for (UINT i=nChanl;i<*pnSamples*pH->nADCNumChannels;i+=pH->nADCNumChannels)
				*pfBuf++ = pfTemp[i];
		}
		//and finally dispose of pTemp
		free(pTemp);
	}
	return 0;
}


//this function reads channel data (via ReadRaw) then scales it
//allocates necessary buffer and returns pointer to the buffer.
//checks trailing zeros and eliminates them (adjusts nSamples, reallocates buffer)
//it DOES NOT seek to the beginning of data, relying on ABFOpens.
int ABFReadChannel(FileRec *F,ABFFileHeader *pH,
                   int nChanl, ABFBufPtr* pBuf, unsigned long *pnSamples)
{
	//allocate necessary buffer
	UINT sSize=SampleSize(pH);
	UINT sz=*pnSamples * sSize;
	void* pTemp=malloc(sz);
	if (!pTemp) GSL_ERROR("could not allocate buffer",ABFH_ENOMEMORY);

	int status=ABFReadRawChannel(F,pH,nChanl,pTemp,pnSamples);
	if (status) GSL_ERROR("could not read data", status);

	//now we need to scale data
	float adc2uuFactor,adc2uuOffs;
	ABFGetFactors(pH,nChanl,&adc2uuFactor,&adc2uuOffs);
	if (pH->nDataFormat == ABF_INTEGERDATA) {
		//we need to transform this to a float representation, so lets allocate buffer
		float* pfTemp=(float*)malloc(*pnSamples*sizeof(float));
		if (!pfTemp) {free(pTemp);GSL_ERROR("could not allocate float array",ABFH_ENOMEMORY);};
		//store pointer to return it
		pBuf->p=pfTemp;
		//also need to typecast pTemp and make sure we keep original pointer
		short* psTemp=(short*)pTemp;
		//and finally copy and scale contents
		for (UINT i=0;i<*pnSamples;i++) *pfTemp++=adc2uuFactor*(*psTemp++)+adc2uuOffs;
		//dispose of original buffer as we no longer need it
		free(pTemp);
	}
	else {
	  //here it is easier as we don't need to reallocate array
	  //lets just recalculate it in this one (but steel need to typecast pTemp)
	  pBuf->p=(float*)pTemp;
/*  the data is already scaled (when was originally converted to floats)!	
	  float* pfTemp=(float*)pTemp;
	  for (UINT i=0;i<nSamples;i++) *pfTemp++=adc2uuFactor*(*pfTemp)+adc2uuOffs;
	  */
	};
	
/*
	//now lets trim zeros at the end;
	UINT nNonZero=nSamples-1;
//	printf("nNonZero=%d;   nSamples=%d\n",nNonZero,nSamples);
	while (nNonZero && pBuf->p[nNonZero]==0)nNonZero--;
//	printf("nNonZero=%d;   nSamples=%d\n",nNonZero,nSamples);
	nSamples=nNonZero;
//	printf("nNonZero=%d;   nSamples=%d\n",nNonZero,nSamples);
	float* pNewBuf=(float*)realloc(pBuf,nNonZero*sizeof(float));
//	printf("after realloc\n");
	pBuf->p=pNewBuf;
*/	
	return 0;
}


//Reads a block of a defined length at specified position
//(given as sample number), no demultiplexing is performed
//i.e.  block contains piece of "raw stream", so allocate appropriate space
//C does not chack overflows!!
int ABFReadBlock(FileRec *F, ABFFileHeader *pH, float* pBuf, UINT pos, UINT* count)
{
	UINT uSampleSize=SampleSize(pH);
	if (fseek(F->h, ABFGetDataOffset(pH) + pH->nADCNumChannels*uSampleSize*pos, SEEK_SET)==-1) {
		fclose(F->h);
		GSL_ERROR("cannot seek to requested block",ABFH_EINVALIDFILE);
	}
	UINT numRead=fread(pBuf, pH->nADCNumChannels*uSampleSize, *count, F->h);
	if (numRead==0 || ferror(F->h)) GSL_ERROR("could not read data",ABF_EREADDATA);
	if (numRead<*count) GSL_MESSAGE("less data than requested",GSL_MESSAGE_MASK_A);
	*count=numRead;
}

int ABFSetCurrentUnits(ABFFileHeader* pH, ABFBufPtr pBuf, int nChanl, unsigned long nSamples, int newcUnits)
{
	//determine the scale factor
	float scale=ABFGetCScaleFactor(pH,nChanl,newcUnits);
	for (int i=0; i<nSamples; pBuf.p[i++] *= scale);
}
