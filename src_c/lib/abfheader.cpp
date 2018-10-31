/***************************************************************************
                          abfheader.cpp  -  description
                             -------------------
    begin                : Sat Jun 16 2001
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

#include <errno.h>
#include <stdlib.h>
#include "abfheader.h"
//#include "abfinfo.h"
#include <gsl/gsl_errno.h>


void ABFPrintHeaderInfo(FILE *F,ABFFileHeader* pH)
{
fprintf(F,"\n contents of the file header:\n");

fprintf(F,"file sig: %#X\n",pH->lFileSignature);
fprintf(F,"version:  %3.1f \n",pH->fFileVersionNumber);
fprintf(F,"headr ver: %3.1f\n",pH->fHeaderVersionNumber);
fprintf(F,"file type: %d\n",pH->nFileType);
fprintf(F,"op mode:  %d\n",pH->nOperationMode);
fprintf(F,"exp type: %d\n",pH->nExperimentType);

fprintf(F,"\nscopes\nnum scopes: %d\n",pH->lNumScopes );
fprintf(F,"scope starts: %d\n",pH->lScopeConfigPtr);

fprintf(F,"\ndata\ndata format: %d\n",pH->nDataFormat);
fprintf(F,"num sweeps:  %d\n",pH->lActualEpisodes);
fprintf(F,"simult scan:  %d\n",pH->nSimultaneousScan);
fprintf(F,"data starts at: %d\n",pH-> lDataSectionPtr);
fprintf(F,"ignored at start: %d\n",pH->nNumPointsIgnored);
fprintf(F,"ack length: %d\n",pH->lActualAcqLength);
int numChan=pH->nADCNumChannels;
fprintf(F,"num channels: %d\n",numChan);
fprintf(F,"sample intevals, 1st: %7.2f, 2nd: %7.2f\n",pH->fADCSampleInterval, pH->fADCSecondSampleInterval);
fprintf(F,"requested length: %d\n",pH->fSecondsPerRun);
fprintf(F,"req num sweeps: %d\n",pH->lEpisodesPerRun);
fprintf(F,"req num trials: %d\n",pH->lRunsPerTrial);
fprintf(F,"averaging: %d\n",pH->nAveragingMode);

fprintf(F,"\ngain-offset\nADC range: %f\n",pH->fADCRange);
fprintf(F,"ADC resolution: %d\n",pH->lADCResolution);
int i;
fprintf(F,"cnahhels:     ");for (i=0;i<numChan;i++)fprintf(F,"%8d",i);
fprintf(F,"\nP2L chan map: ");for (i=0;i<numChan;i++)fprintf(F,"%8d",pH->nADCPtoLChannelMap[i]);
fprintf(F,"\nsampling seq: ");for (i=0;i<numChan;i++)fprintf(F,"%8d",pH->nADCSamplingSeq[i]);

char  chname[10];
fprintf(F,"\nchannl names: ");for (i=0;i<numChan;i++){strncpy(chname,pH->sDACChannelName[i], ABF_DACNAMELEN);fprintf(F,"%8s",&chname);};
char chUnits[8];
fprintf(F,"\nchannl units: ");for (i=0;i<numChan;i++){GetADCChannelUnits(pH,i,chUnits);fprintf(F,"%8s",&chUnits);};
//fprintf(F,"\nchannl units all: %s",pH->sADCUnits);

fprintf(F,"\nprogram gain: ");for (i=0;i<numChan;i++)fprintf(F,"%8.3f",pH->fADCProgrammableGain[i]);
fprintf(F,"\ninstr scale:  ");for (i=0;i<numChan;i++)fprintf(F,"%8.3f",pH->fInstrumentScaleFactor[i]);
fprintf(F,"\nsignal gain:  ");for (i=0;i<numChan;i++)fprintf(F,"%8.3f",pH->fSignalGain[i]);
fprintf(F,"\ncumulat gain: ");for (i=0;i<numChan;i++)fprintf(F,"%8.3f",pH->fADCProgrammableGain[i]*pH->fInstrumentScaleFactor[i]*pH->fSignalGain[i]);

fprintf(F,"\n\ninstr offset: ");for (i=0;i<numChan;i++)fprintf(F,"%8.3f",pH->fInstrumentOffset[i]);
fprintf(F,"\nsignal offs:  ");for (i=0;i<numChan;i++)fprintf(F,"%8.3f",pH->fSignalOffset[i]);
fprintf(F,"\ncumulat offs: ");for (i=0;i<numChan;i++)fprintf(F,"%8.3f",pH->fInstrumentOffset[i]+pH->fSignalOffset[i]);

fprintf(F,"\n\nlowpass flt:  ");for (i=0;i<numChan;i++)fprintf(F,"%8.0f",pH->fSignalLowpassFilter[i]);
fprintf(F,"\nhipass flt:   ");for (i=0;i<numChan;i++)fprintf(F,"%8.3f",pH->fSignalHighpassFilter[i]);
fprintf(F,"\n\n");

} //ABFPrintHeaderInfo

//get sample size used (short or float)
UINT SampleSize(const ABFFileHeader *pFH)
{
if(pFH->nDataFormat==ABF_INTEGERDATA) return 2;
else if (pFH->nDataFormat==ABF_FLOATDATA) return 4;
else return 0;
}

//===============================================================================================
// FUNCTION: GetDataOffset
// PURPOSE:  Get the file offset to the data allowing for "ignored" points from old AxoLab files.
//
long ABFGetDataOffset(const ABFFileHeader *pFH)
{
//   ABFH_ASSERT(pFH);
   long lDataOffset = pFH->lDataSectionPtr * ABF_BLOCKSIZE;

   // Adjust the data pointer for any garbage data words at the start of
   // the data portion of the file. (Created by AxoLab in continuous
   // files only)
   if (pFH->nOperationMode == ABF_GAPFREEFILE)
      lDataOffset += pFH->nNumPointsIgnored * SampleSize(pFH);

   //printf("data offset=%d",lDataOffset);fflush(stdout);
   return lDataOffset;
}

//calculates gain and offset factors from the header.
int ABFGetFactors(const ABFFileHeader *pFH,UINT nChannel,float *ADC2UUfactor, float *ADC2UUoffset)
{
	float fTotalScaleFactor = pFH->fInstrumentScaleFactor[nChannel] *
                             pFH->fADCProgrammableGain[nChannel];
  if (pFH->nSignalType != 0)
      fTotalScaleFactor *= pFH->fSignalGain[nChannel];
//  printf("\nABFGetFactors:\ntotalScaleFactor: %5.3f,  ",fTotalScaleFactor);

  // Adjust for the telegraphed gain.
  //need to check if we have recent version of file
  if( pFH->fFileVersionNumber>1.5 && pFH->nTelegraphEnable[nChannel] )
      fTotalScaleFactor *= pFH->fTelegraphAdditGain[nChannel];
  if (fTotalScaleFactor==0.0F)
      fTotalScaleFactor = 1.0F;

  // InputRange and InputOffset is the range and offset of the signal in
  // user units when it hits the Analog-to-Digital converter

  float fInputRange = pFH->fADCRange / fTotalScaleFactor;
  float fInputOffset= -pFH->fInstrumentOffset[nChannel];
//  printf("%5.3f,\nInputRange: %5.3f\n",fInputRange);

  if (pFH->nSignalType != 0)
      fInputOffset += pFH->fSignalOffset[nChannel];

  *ADC2UUfactor = fInputRange / pFH->lADCResolution;
  *ADC2UUoffset  = -fInputOffset;
//  printf("factor: %7.4f,\noffset: %7.4f",*ADC2UUfactor,*ADC2UUoffset);
  return 1;
}



int ABFFileOpen(char* fName,FileRec *F,ABFFileHeader *pH)
{
//lets try to open file
//check if stdin is requested
if (fName=="stdin" || fName==""){F->h=stdin;F->fn="stdin";}
else{
	F->fn=fName;
	F->h=fopen(fName,"r");//printf("file pointer: %p\n",F->h);
  }
//check if opened Ok
if(F->h){
	if ( !fread(pH,sizeof(ABFFileHeader),1,F->h) ){
		GSL_ERROR ("cannot read ABF header", ABFH_EHEADERREAD);
	}
	//check file signature
	if (pH->lFileSignature!=ABF_NATIVESIGNATURE){
		fclose(F->h);F->h=NULL;F=NULL;
		GSL_ERROR ("wrond ABF file signature", ABFH_EINVALIDFILE);
	}
  }
else GSL_ERROR ("cannot open file", ABFH_EHEADERREAD );

return 0;
}


int ABFOpenGapFree(char* fName,FileRec *F,ABFFileHeader *pH,UINT *nChanls,unsigned long *nSamples)
{
	if (ABFFileOpen(fName,F,pH)) GSL_ERROR ("cannot open ABF file", ABFH_EHEADERREAD );

	//check that this is a gap-free file
	if (pH->nOperationMode!=ABF_GAPFREEFILE){fclose(F->h);GSL_ERROR("not a gap-free file",ABFH_ENOTGAPFREE);};
	//some consistency checks
	//if (pH->lActualEpisodes!=1 || pH->lEpisodesPerRun!=1 || pH->lRunsPerTrial!=1) {fclose(F->h);GSL_ERROR("bad header options for gap-free!",ABFH_EINVALIDFILE);};
	//ignore lActualEpisodes, as Axon dosn't seem to honor their own specifications
	if (pH->lEpisodesPerRun!=1 || pH->lRunsPerTrial!=1) {fclose(F->h);GSL_ERROR("bad header options for gap-free!",ABFH_EINVALIDFILE);};

   // Check that the data file actually contains data.
  if (pH->lActualAcqLength <= 0 || pH->nADCNumChannels <= 0) {fclose(F->h);GSL_ERROR("no data in file!",ABFH_CHANNELNOTSAMPLED);};

  //now set nSamples&nChanls:
  *nChanls=pH->nADCNumChannels;//printf("nChan=%d\n",*nChanls);
  *nSamples=pH->lActualAcqLength/ *nChanls; //printf("nSampl=%d\n",*nSamples);
  //fflush(stdout);

  //seek to data-start
  if (fseek(F->h,ABFGetDataOffset(pH),SEEK_SET)==-1) {
	fclose(F->h);
	GSL_ERROR("cannot seek to data start",ABFH_EINVALIDFILE);
  };

  return 0;
}

int ABFSeekToDataStart(FileRec *F,ABFFileHeader* pH)
{
  if (fseek(F->h,ABFGetDataOffset(pH),SEEK_SET)==-1) {
	fclose(F->h);
	GSL_ERROR("cannot seek to data start",ABFH_EINVALIDFILE);
  };

  return 0;
}



//Output and data-init routines start here

#define DEFAULT_LEVEL_HYSTERESIS 64
#define DEFAULT_TIME_HYSTERESIS  1

ABFFileHeader* ABFNewGapFreeHeader()
{
   int i;

   // Alloc and zero fill all to start with.
   ABFFileHeader* pFH = (ABFFileHeader *) calloc(1,sizeof(ABFFileHeader));
   if ( !pFH ) GSL_ERROR_NULL("could not allocate header",ABFH_ENOMEMORY);

   // Blank fill all strings.
   ABF_BLANK_FILL(pFH->_sParamValueList);
   ABF_BLANK_FILL(pFH->sADCChannelName);
   ABF_BLANK_FILL(pFH->sADCUnits);
   ABF_BLANK_FILL(pFH->sDACChannelName);
   ABF_BLANK_FILL(pFH->sDACChannelUnits);
   ABF_BLANK_FILL(pFH->sDACFilePath[0]);
   ABF_BLANK_FILL(pFH->sDACFilePath[1]);
   //ABF_SET_STRING(pFH->sArithmeticOperator, "+");
   ABF_BLANK_FILL(pFH->sArithmeticUnits);

   pFH->lFileSignature        = ABF_NATIVESIGNATURE;
   pFH->fFileVersionNumber    = ABF_CURRENTVERSION;
   pFH->fHeaderVersionNumber  = ABF_CURRENTVERSION;
   pFH->nOperationMode        = ABF_GAPFREEFILE;
   pFH->nADCNumChannels       = 1;
   pFH->fADCSampleInterval    = 100.0F;
   pFH->lNumSamplesPerEpisode = 512;
   pFH->lEpisodesPerRun       = 1;
   pFH->lActualEpisodes        = 1;
   pFH->lRunsPerTrial          = 1;
   pFH->lDataSectionPtr       = sizeof(ABFFileHeader) / ABF_BLOCKSIZE;
   pFH->nMSBinFormat          = 0; //IEEE float

   pFH->nDrawingStrategy      = ABF_DRAW_REALTIME;
   pFH->nTiledDisplay         = ABF_DISPLAY_TILED;
   pFH->nEraseStrategy        = 1;
   pFH->nDataDisplayMode      = ABF_DRAW_LINES;
   pFH->nMultiColor           = TRUE;
   pFH->nFileType             = ABF_ABFFILE;
   pFH->nAutoTriggerStrategy  = 1;   // Allow auto triggering.
   pFH->nChannelStatsStrategy = 0;   // Don't calculate channel statistics.
   pFH->fStatisticsPeriod     = 1.0F;
   pFH->lCalculationPeriod    = long(pFH->fStatisticsPeriod / pFH->fADCSampleInterval * 1E3F);
   pFH->lStatisticsMeasurements = ABF_STATISTICS_ABOVETHRESHOLD | ABF_STATISTICS_MEANOPENTIME;

   pFH->lSamplesPerTrace      = 16384;
   pFH->lPreTriggerSamples    = 16;    // default to 16

   pFH->fADCRange             = 10.24F;
   pFH->fDACRange             = 10.24F;
   pFH->lADCResolution        = 32768L;
   pFH->lDACResolution        = 32768L;
   pFH->nExperimentType       = ABF_SIMPLEACQUISITION;


   ABF_BLANK_FILL(pFH->sCreatorInfo);
   ABF_BLANK_FILL(pFH->sFileComment);

   // ADC channel data
   for (i=0; i<ABF_ADCCOUNT; i++)
   {
      char szName[13];
      sprintf(szName, "AI #%-8d", i);
      strncpy(pFH->sADCChannelName[i], szName, ABF_ADCNAMELEN);
      strncpy(pFH->sADCUnits[i], "pA        ", ABF_ADCUNITLEN);

      pFH->nADCPtoLChannelMap[i]       = short(i);
      pFH->nADCSamplingSeq[i]          = ABF_UNUSED_CHANNEL;
      pFH->fADCProgrammableGain[i]     = 1.0F;
      pFH->fADCDisplayAmplification[i] = 1.0F;
      pFH->fInstrumentScaleFactor[i]   = 0.1F;
      pFH->fSignalGain[i]              = 1.0F;
      pFH->fSignalLowpassFilter[i]     = ABF_FILTERDISABLED;

// FIX FIX FIX PRC DEBUG Telegraph changes - check !
      pFH->fTelegraphAdditGain[i]      = 1.0F;
      pFH->fTelegraphFilter[i]         = 100000.0F;
   }
   pFH->nADCSamplingSeq[0] = 0;

   // DAC channel data
   for (i=0; i<ABF_DACCOUNT; i++)
   {
      char szName[13];
      sprintf(szName, "AO #%-8d", i);
      strncpy(pFH->sDACChannelName[i], szName, ABF_DACNAMELEN);
      strncpy(pFH->sDACChannelUnits[i], "mV        ", ABF_ADCUNITLEN);
      pFH->fDACScaleFactor[i] = 20.0F;
   }

   // DAC file settings
   for (i=0; i<ABF_WAVEFORMCOUNT; i++)
   {
      pFH->fDACFileScale[i] = 1.0F;
      pFH->nPNPolarity[i]   = ABF_PN_SAME_POLARITY;
   }
   pFH->nPNNumPulses        = 2;

   pFH->nAutopeakPolarity   = 1;
   pFH->nAutopeakSmoothing  = 1;
   pFH->nAutopeakSearchMode = ABF_PEAK_SEARCH_SPECIFIED;
   pFH->nAutopeakBaseline   = ABF_PEAK_BASELINE_NONE;
   pFH->lAutopeakMeasurements = ABF_PEAK_MEASURE_PEAK | ABF_PEAK_MEASURE_PEAKTIME;
   pFH->fArithmeticUpperLimit = 100.0F;
   pFH->fArithmeticLowerLimit = -100.0F;
   pFH->fArithmeticK1         = 1.0F;
   pFH->fArithmeticK3         = 1.0F;

   for (i=0; i<ABF_BELLCOUNT; i++)
   {
      pFH->nBellEnable[i] = 0;
      pFH->nBellLocation[i] = 1;
      pFH->nBellRepetitions[i] = 1;
   }
   pFH->nLevelHysteresis    = DEFAULT_LEVEL_HYSTERESIS;   // Two LSBits of level hysteresis.
   pFH->lTimeHysteresis     = DEFAULT_TIME_HYSTERESIS;    // Two sequences of time hysteresis.
   pFH->fAverageWeighting   = 0.1F;                       // Add 10% of trace to 90% of average.
   pFH->nTrialTriggerSource = ABF_TRIALTRIGGER_NONE;
   pFH->nExternalTagType    = ABF_EXTERNALTAG;

   pFH->lHeaderSize         = ABF_HEADERSIZE;
   pFH->nAutoAnalyseEnable  = ABF_AUTOANALYSE_DEFAULT;

   for( i=0; i<ABF_USERLISTCOUNT; i++ )
      ABF_BLANK_FILL( pFH->sULParamValueList[i] );

   // DAC Calibration Factors.
   for( i=0; i<ABF_DACCOUNT; i++ )
   {
      pFH->fDACCalibrationFactor[i] = 1.0F;
      pFH->fDACCalibrationOffset[i] = 0.0F;
   }

   return pFH;
}



// few axon utility functions
//===============================================================================================
// FUNCTION: StripSpace
// PURPOSE:  Strips leading and trailing space (ASCII 32) characters out of a string.
//
static char *StripSpace(char *psStr)
{
   if (psStr==NULL)
      return NULL;

   char *ps = psStr;
   while (*ps==' ')
      ps++;

   strcpy(psStr, ps);

   if (*psStr != '\0')
   {
      ps = psStr + strlen(psStr);
      while (*(ps-1)==' ')
         ps--;
      *ps = '\0';
   }
   return psStr;
}

//===============================================================================================
// FUNCTION: GetADCChannelUnits
// PURPOSE:  Fills the passed buffer with the channel units for a particular ADC channel.
//           Builds a zero terminated "C" string from a fixed length array of bytes.
//
//!! modified: to follow sample sequence
//szBuffer must be char[ABF_ADCUNITLEN+1]!!
int GetADCChannelUnits(const ABFFileHeader *pFH, int nADCChannel, char *szBuffer)
{
   int chNum = pFH->nADCSamplingSeq[nADCChannel];
   //printf("ssNum=%d\n",chNum);fflush(stdout);
   strncpy(szBuffer, pFH->sADCUnits[chNum], ABF_ADCUNITLEN);
   szBuffer[ABF_ADCUNITLEN] = '\0';
   StripSpace(szBuffer);
   return (szBuffer[0] != '\0');
}


//some more utility staff
int ABFGetCUnits(const ABFFileHeader* pH, UINT nChanl)
{
	//printf("entered ABFGetCUnits\n");fflush(stdout);
	char sBuf[ABF_ADCUNITLEN+1];
	GetADCChannelUnits(pH,nChanl,sBuf);
	int result;
	if (!strcasecmp(sBuf,"pa")) result=cuPA; else
	if (!strcasecmp(sBuf,"na")) result=cuNA; else
	if (!strcasecmp(sBuf,"ma")) result=cuMA; else
		result=cuUNDEFINED;
	return result;
}

int ABFSetCUnits(ABFFileHeader* pH, UINT nChanl, int cUnits)
{
	char sBuf[ABF_ADCUNITLEN+1];
	switch (cUnits){
		case cuPA:strncpy(sBuf, "pA        ", ABF_ADCUNITLEN);break;
		case cuNA:strncpy(sBuf, "nA        ",ABF_ADCUNITLEN);break;
		case cuMA:strncpy(sBuf, "mA        ",ABF_ADCUNITLEN);break;
		case cuUNDEFINED:strncpy(sBuf,"undef.    ",ABF_ADCUNITLEN);break;
	}
	int chNum = pH->nADCSamplingSeq[nChanl];
	strncpy(pH->sADCUnits[chNum], sBuf, ABF_ADCUNITLEN);
}

float ABFGetCScaleFactor (const ABFFileHeader* pH,UINT nChanl, int newcUnits)
{
  //check current units and adjust scale correspondingly
  int cUnits=ABFGetCUnits(pH,nChanl);
  if ( cUnits==cuNA ) {
  	if (newcUnits==cuMA) return 0.001; else
  	if (newcUnits==cuNA) return 1; else return 1000;
  } else
  if ( cUnits==cuMA ) {
  	if (newcUnits==cuMA) return 1; else
  	if (newcUnits==cuNA) return 1000; else return 1e6;
  } else
  if ( cUnits==cuPA ) {
  	if (newcUnits==cuMA) return 1e-6; else
  	if (newcUnits==cuNA) return 0.001; else return 1;
  } else {
	char sBuf[8];
	GetADCChannelUnits(pH,nChanl,sBuf);
	printf("!!cannot recognize current units supplied (%s), assuming pA!!\n", sBuf );
  	if (newcUnits==cuMA) return 1e-6; else
  	if (newcUnits==cuNA) return 0.001; else return 1;
  }
}
