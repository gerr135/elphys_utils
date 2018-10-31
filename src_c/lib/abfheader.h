/***************************************************************************
                          abfheader.h  -  description
                             -------------------
    begin                : Fri Jun 15 2001
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

#include <string.h>
#include <stdio.h>
#include "compdefs.h"


//define struct which will contain FILE*
#include <stdio.h>

//#include <gsl/gsl_histogram.h>
//needed to do this to open file in procedure
struct FileRec {
	char* fn;
	FILE* h;
};


//
// Constants used in defining the ABF file header
//

#define ABF_ADCCOUNT           16    // number of ADC channels supported.
#define ABF_DACCOUNT           4     // number of DAC channels supported.
#define ABF_WAVEFORMCOUNT      2     // number of DAC channels which support waveforms.
#define ABF_EPOCHCOUNT         10    // number of waveform epochs supported.
#define ABF_BELLCOUNT          2     // Number of auditory signals supported.
#define ABF_ADCUNITLEN         8     // length of ADC units strings
#define ABF_ADCNAMELEN         10    // length of ADC channel name strings
#define ABF_DACUNITLEN         8     // length of DAC units strings
#define ABF_DACNAMELEN         10    // length of DAC channel name strings
#define ABF_VARPARAMLISTLEN    80    // length of conditioning string
#define ABF_USERLISTLEN        256   // length of the user list (V1.6)
#define ABF_USERLISTCOUNT      4     // number of independent user lists (V1.6)
#define ABF_OLDFILECOMMENTLEN  56    // length of file comment string (pre V1.6)
#define ABF_FILECOMMENTLEN     128   // length of file comment string (V1.6)

#define ABF_CREATORINFOLEN     16    // length of file creator info string
#define ABF_OLDDACFILENAMELEN  12    // old length of the DACFile name string
#define ABF_OLDDACFILEPATHLEN  60    // old length of the DACFile path string
#define ABF_DACFILEPATHLEN     84    // length of full path for DACFile
#define ABF_PATHLEN            256   // length of full path, used for DACFile and Protocol name.
#define ABF_ARITHMETICOPLEN    2     // length of the Arithmetic operator field
#define ABF_ARITHMETICUNITSLEN 8     // length of arithmetic units string
#define ABF_TAGCOMMENTLEN      56    // length of tag comment string
#define ABF_LONGDESCRIPTIONLEN 56    // length of long description entry
#define ABF_NOTENAMELEN        10    // length of the name component of a note
#define ABF_NOTEVALUELEN       8     // length of the value component of a note
#define ABF_NOTEUNITSLEN       8     // length of the units component of a note
#define ABF_BLOCKSIZE          512   // Size of block alignment in ABF files.
#define ABF_MACRONAMELEN       64    // Size of a Clampfit macro name.

#define ABF_CURRENTVERSION     1.65F            // Current file format version number
#define ABF_PREVIOUSVERSION    1.5F            // Previous file format version number (for old header size)
#define ABF_V16                1.6F            // Version number when the header size changed.
#define ABF_HEADERSIZE         6144            // Size of a Version 1.6 or later header
#define ABF_OLDHEADERSIZE      2048            // Size of a Version 1.5 or earlier header
#define ABF_NATIVESIGNATURE    0x20464241      // PC="ABF ", MAC=" FBA"
#define ABF_REVERSESIGNATURE   0x41424620      // PC=" FBA", MAC="ABF "

#define PCLAMP6_MAXSWEEPLENGTH         16384   // Maximum multiplexed sweep length supported by pCLAMP6 apps.
#define PCLAMP7_MAXSWEEPLEN_PERCHAN    103224  // Maximum per channel sweep length supported by pCLAMP7 apps.

#define ABF_MAX_TRIAL_SAMPLES  0x7FFFFFFF    // Maximum length of acquisition supported (samples)
                                             // INT_MAX is used instead of UINT_MAX because of the signed
                                             // values in the ABF header.

#define ABF_MAX_SWEEPS_PER_AVERAGE 65500     // The maximum number of sweeps that can be combined into a
                                             // cumulative average (nAverageAlgorithm=ABF_INFINITEAVERAGE).

#ifdef _MAC
   #define ABF_OLDPCLAMP        ABF_REVERSESIGNATURE
#else
   #define ABF_OLDPCLAMP        ABF_NATIVESIGNATURE
#endif

//
// Constant definitions for nFileType
//
#define ABF_ABFFILE          1
#define ABF_FETCHEX          2
#define ABF_CLAMPEX          3

//
// Constant definitions for nDataFormat
//
#define ABF_INTEGERDATA      0
#define ABF_FLOATDATA        1

//
// Constant definitions for nOperationMode
//
#define ABF_VARLENEVENTS     1
#define ABF_FIXLENEVENTS     2     // (ABF_FIXLENEVENTS == ABF_LOSSFREEOSC)
#define ABF_LOSSFREEOSC      2
#define ABF_GAPFREEFILE      3
#define ABF_HIGHSPEEDOSC     4
#define ABF_WAVEFORMFILE     5

//
// Constant definitions for nParamToVary
//
#define ABF_CONDITNUMPULSES         0
#define ABF_CONDITBASELINEDURATION  1
#define ABF_CONDITBASELINELEVEL     2
#define ABF_CONDITSTEPDURATION      3
#define ABF_CONDITSTEPLEVEL         4
#define ABF_CONDITPOSTTRAINDURATION 5
#define ABF_CONDITPOSTTRAINLEVEL    6
#define ABF_EPISODESTARTTOSTART     7
#define ABF_INACTIVEHOLDING         8
#define ABF_DIGITALHOLDING          9
#define ABF_PNNUMPULSES             10
#define ABF_PARALLELVALUE           11
#define ABF_EPOCHINITLEVEL          21
#define ABF_EPOCHINITDURATION       31

//
// Constants for nAveragingMode
//
#define ABF_NOAVERAGING       0
#define ABF_SAVEAVERAGEONLY   1
#define ABF_AVERAGESAVEALL    2

//
// Constants for nAverageAlgorithm
//
#define ABF_INFINITEAVERAGE   0
#define ABF_SLIDINGAVERAGE    1

//
// Constants for nEpochType
//
#define ABF_EPOCHDISABLED     0               // disabled epoch
#define ABF_EPOCHSTEPPED      1               // stepped waveform
#define ABF_EPOCHRAMPED       2               // ramp waveform

//
// Constants for nWaveformSource
//
#define ABF_WAVEFORMDISABLED     0               // disabled waveform
#define ABF_EPOCHTABLEWAVEFORM   1
#define ABF_DACFILEWAVEFORM      2

//
// Constants for nInterEpisodeLevel & nDigitalInterEpisode
//
#define ABF_INTEREPI_USEHOLDING    0
#define ABF_INTEREPI_USELASTEPOCH  1

//
// Constants for nExperimentType
//
#define ABF_VOLTAGECLAMP         0
#define ABF_CURRENTCLAMP         1
#define ABF_SIMPLEACQUISITION    2

//
// Constants for nAutosampleEnable
//
#define ABF_AUTOSAMPLEDISABLED   0
#define ABF_AUTOSAMPLEAUTOMATIC  1
#define ABF_AUTOSAMPLEMANUAL     2

//
// Constants for nAutosampleInstrument
//
#define ABF_INST_UNKNOWN         0   // Unknown instrument (manual or user defined telegraph table).
#define ABF_INST_AXOPATCH1       1   // Axopatch-1 with CV-4-1/100
#define ABF_INST_AXOPATCH1_1     2   // Axopatch-1 with CV-4-0.1/100
#define ABF_INST_AXOPATCH1B      3   // Axopatch-1B(inv.) CV-4-1/100
#define ABF_INST_AXOPATCH1B_1    4   // Axopatch-1B(inv) CV-4-0.1/100
#define ABF_INST_AXOPATCH201     5   // Axopatch 200 with CV 201
#define ABF_INST_AXOPATCH202     6   // Axopatch 200 with CV 202
#define ABF_INST_GENECLAMP       7   // GeneClamp
#define ABF_INST_DAGAN3900       8   // Dagan 3900
#define ABF_INST_DAGAN3900A      9   // Dagan 3900A
#define ABF_INST_DAGANCA1_1      10  // Dagan CA-1  Im=0.1
#define ABF_INST_DAGANCA1        11  // Dagan CA-1  Im=1.0
#define ABF_INST_DAGANCA10       12  // Dagan CA-1  Im=10
#define ABF_INST_WARNER_OC725    13  // Warner OC-725
#define ABF_INST_WARNER_OC725C   14  // Warner OC-725
#define ABF_INST_AXOPATCH200B    15  // Axopatch 200B
#define ABF_INST_DAGANPCONE0_1   16  // Dagan PC-ONE  Im=0.1
#define ABF_INST_DAGANPCONE1     17  // Dagan PC-ONE  Im=1.0
#define ABF_INST_DAGANPCONE10    18  // Dagan PC-ONE  Im=10
#define ABF_INST_DAGANPCONE100   19  // Dagan PC-ONE  Im=100
#define ABF_INST_WARNER_BC525C   20  // Warner BC-525C
#define ABF_INST_WARNER_PC505    21  // Warner PC-505
#define ABF_INST_WARNER_PC501    22  // Warner PC-501
#define ABF_INST_DAGANCA1_05     23  // Dagan CA-1  Im=0.05
#define ABF_INST_MULTICLAMP700   24  // MultiClamp 700
#define ABF_INST_TURBO_TEC       25  // Turbo Tec

//
// Constants for nManualInfoStrategy
//
#define ABF_ENV_DONOTWRITE      0
#define ABF_ENV_WRITEEACHTRIAL  1
#define ABF_ENV_PROMPTEACHTRIAL 2

//
// Constants for nTriggerSource
//
#define ABF_TRIGGERLINEINPUT           -5   // Start on line trigger (DD1320 only)
#define ABF_TRIGGERTAGINPUT            -4
#define ABF_TRIGGERFIRSTCHANNEL        -3
#define ABF_TRIGGEREXTERNAL            -2
#define ABF_TRIGGERSPACEBAR            -1
// >=0 = ADC channel to trigger off.

//
// Constants for nTrialTriggerSource
//
#define ABF_TRIALTRIGGER_SWSTARTONLY   -6   // Start on software message, end when protocol ends.
#define ABF_TRIALTRIGGER_SWSTARTSTOP   -5   // Start and end on software messages.
#define ABF_TRIALTRIGGER_LINEINPUT     -4   // Start on line trigger (DD1320 only)
#define ABF_TRIALTRIGGER_SPACEBAR      -3   // Start on spacebar press.
#define ABF_TRIALTRIGGER_EXTERNAL      -2   // Start on external trigger high
#define ABF_TRIALTRIGGER_NONE          -1   // Start immediately (default).
// >=0 = ADC channel to trigger off.    // Not implemented as yet...

//
// Constants for nTriggerPolarity.
//
#define ABF_TRIGGER_RISINGEDGE  0
#define ABF_TRIGGER_FALLINGEDGE 1

//
// Constants for nTriggerAction
//
#define ABF_TRIGGER_STARTEPISODE 0
#define ABF_TRIGGER_STARTRUN     1
#define ABF_TRIGGER_STARTTRIAL   2    // N.B. Discontinued in favor of nTrialTriggerSource

//
// Constants for nDrawingStrategy
//
#define ABF_DRAW_NONE            0
#define ABF_DRAW_REALTIME        1
#define ABF_DRAW_FULLSCREEN      2
#define ABF_DRAW_ENDOFRUN        3

//
// Constants for nTiledDisplay
//
#define ABF_DISPLAY_SUPERIMPOSED 0
#define ABF_DISPLAY_TILED        1

//
// Constants for nDataDisplayMode
//
#define ABF_DRAW_POINTS       0
#define ABF_DRAW_LINES        1

//
// Constants for nArithmeticExpression
//
#define ABF_SIMPLE_EXPRESSION    0
#define ABF_RATIO_EXPRESSION     1

//
// Constants for nLowpassFilterType & nHighpassFilterType
//
#define ABF_FILTER_NONE          0
#define ABF_FILTER_EXTERNAL      1
#define ABF_FILTER_SIMPLE_RC     2
#define ABF_FILTER_BESSEL        3
#define ABF_FILTER_BUTTERWORTH   4

//
// Constants for nPNPosition
//
#define ABF_PN_BEFORE_EPISODE    0
#define ABF_PN_AFTER_EPISODE     1

//
// Constants for nPNPolarity
//
#define ABF_PN_OPPOSITE_POLARITY -1
#define ABF_PN_SAME_POLARITY     1

//
// Constants for nAutopeakPolarity
//
#define ABF_PEAK_NEGATIVE       -1
#define ABF_PEAK_ABSOLUTE        0
#define ABF_PEAK_POSITIVE        1

//
// Constants for nAutopeakSearchMode
//
#define ABF_PEAK_SEARCH_SPECIFIED       -2
#define ABF_PEAK_SEARCH_ALL             -1
// nAutopeakSearchMode 0..9   = epoch in waveform 0's epoch table
// nAutopeakSearchMode 10..19 = epoch in waveform 1's epoch table

//
// Constants for nAutopeakBaseline
//
#define ABF_PEAK_BASELINE_SPECIFIED    -3
#define ABF_PEAK_BASELINE_NONE 	      -2
#define ABF_PEAK_BASELINE_FIRSTHOLDING -1
#define ABF_PEAK_BASELINE_LASTHOLDING  -4

//
// Constants for lAutopeakMeasurements
//
#define ABF_PEAK_MEASURE_PEAK             0x00000001
#define ABF_PEAK_MEASURE_PEAKTIME         0x00000002
#define ABF_PEAK_MEASURE_ANTIPEAK         0x00000004
#define ABF_PEAK_MEASURE_ANTIPEAKTIME     0x00000008
#define ABF_PEAK_MEASURE_MEAN             0x00000010
#define ABF_PEAK_MEASURE_STDDEV           0x00000020
#define ABF_PEAK_MEASURE_INTEGRAL         0x00000040
#define ABF_PEAK_MEASURE_LEFTSLOPE        0x00000080
#define ABF_PEAK_MEASURE_LEFTSLOPETIME    0x00000100
#define ABF_PEAK_MEASURE_RIGHTSLOPE       0x00000200
#define ABF_PEAK_MEASURE_RIGHTSLOPETIME   0x00000400
#define ABF_PEAK_MEASURE_RISETIME         0x00000800
#define ABF_PEAK_MEASURE_DECAYTIME        0x00001000
#define ABF_PEAK_MEASURE_HALFWIDTH        0x00002000
#define ABF_PEAK_MEASURE_BASELINE         0x00004000
#define ABF_PEAK_MEASURE_ALL              0x00007FFF     // All the above OR'd together.

//
// Constants for lStatisticsMeasurements
//
#define ABF_STATISTICS_ABOVETHRESHOLD     0x00000001
#define ABF_STATISTICS_EVENTFREQUENCY     0x00000002
#define ABF_STATISTICS_MEANOPENTIME       0x00000004
#define ABF_STATISTICS_MEANCLOSEDTIME     0x00000008
#define ABF_STATISTICS_ALL                0x0000000F     // All the above OR'd together.

//
// Constants for nStatisticsSaveStrategy
//
#define ABF_STATISTICS_NOAUTOSAVE            0
#define ABF_STATISTICS_AUTOSAVE              1
#define ABF_STATISTICS_AUTOSAVE_AUTOCLEAR    2

//
// Constants for nStatisticsDisplayStrategy
//
#define ABF_STATISTICS_DISPLAY      0
#define ABF_STATISTICS_NODISPLAY    1

//
// Constants for nStatisticsClearStrategy
// determines whether to clear statistics after saving.
//
#define ABF_STATISTICS_NOCLEAR      0
#define ABF_STATISTICS_CLEAR        1

//
// Constants for nDACFileEpisodeNum
//
#define ABF_DACFILE_SKIPFIRSTSWEEP -1
#define ABF_DACFILE_USEALLSWEEPS    0
// >0 = The specific sweep number.

//
// Constants for nUndoPromptStrategy
//
#define ABF_UNDOPROMPT_ONABORT   0
#define ABF_UNDOPROMPT_ALWAYS    1

//
// Constants for nAutoAnalyseEnable
//
#define ABF_AUTOANALYSE_DISABLED   0
#define ABF_AUTOANALYSE_DEFAULT    1
#define ABF_AUTOANALYSE_RUNMACRO   2

//
// Miscellaneous constants
//
#define ABF_FILTERDISABLED  100000.0F     // Large frequency to disable lowpass filters
#define ABF_UNUSED_CHANNEL  -1            // Unused ADC and DAC channels.

//
// The output sampling sequence identifier for a seperate digital out channel.
//
#define ABF_DIGITAL_OUT_CHANNEL -1
#define ABF_PADDING_OUT_CHANNEL -2

// maximum values for various parameters (used by ABFH_CheckUserList).
#define ABF_CTPULSECOUNT_MAX           10000
#define ABF_CTBASELINEDURATION_MAX     100000.0F
#define ABF_CTSTEPDURATION_MAX         100000.0F
#define ABF_CTPOSTTRAINDURATION_MAX    100000.0F
#define ABF_SWEEPSTARTTOSTARTTIME_MAX  100000.0F
#define ABF_PNPULSECOUNT_MAX           8
#define ABF_DIGITALVALUE_MAX           0xFF
#define ABF_EPOCHDIGITALVALUE_MAX      0x0F

//
// pack structure on byte boundaries
//
#ifndef RC_INVOKED
#pragma pack(1)
#endif

//
// Definition of the ABF header structure.
//

struct ABFFileHeader               // The total header length = 5120 bytes.
{
   // GROUP #1 - File ID and size information. (40 bytes)
   long     lFileSignature;
   float    fFileVersionNumber;
   short    nOperationMode;
   long     lActualAcqLength;
   short    nNumPointsIgnored;
   long     lActualEpisodes;
   long     lFileStartDate;         // YYYYMMDD
   long     lFileStartTime;
   long     lStopwatchTime;
   float    fHeaderVersionNumber;
   short    nFileType;
   short    nMSBinFormat;

   // GROUP #2 - File Structure (80 bytes)
   long     lDataSectionPtr;
   long     lTagSectionPtr;
   long     lNumTagEntries;
   long     lScopeConfigPtr;
   long     lNumScopes;
   long     _lDACFilePtr;
   long     _lDACFileNumEpisodes;
   char     sUnused68[4];
   long     lDeltaArrayPtr;
   long     lNumDeltas;
   long     lVoiceTagPtr;
   long     lVoiceTagEntries;
   long     lUnused88;
   long     lSynchArrayPtr;
   long     lSynchArraySize;
   short    nDataFormat;
   short    nSimultaneousScan;
   long     lStatisticsConfigPtr;
   char     sUnused108[12];

   // GROUP #3 - Trial hierarchy information (80 bytes)
   short    nADCNumChannels;
   float    fADCSampleInterval;
   float    fADCSecondSampleInterval;
   float    fSynchTimeUnit;
   float    fSecondsPerRun;
   long     lNumSamplesPerEpisode;
   long     lPreTriggerSamples;
   long     lEpisodesPerRun;
   long     lRunsPerTrial;
   long     lNumberOfTrials;
   short    nAveragingMode;
   short    nUndoRunCount;
   short    nFirstEpisodeInRun;
   float    fTriggerThreshold;
   short    nTriggerSource;
   short    nTriggerAction;
   short    nTriggerPolarity;
   float    fScopeOutputInterval;
   float    fEpisodeStartToStart;
   float    fRunStartToStart;
   float    fTrialStartToStart;
   long     lAverageCount;
   long     lClockChange;
   short    nAutoTriggerStrategy;

   // GROUP #4 - Display Parameters (44 bytes)
   short    nDrawingStrategy;
   short    nTiledDisplay;
   short    nEraseStrategy;           // N.B. Discontinued. Use scope config entry instead.
   short    nDataDisplayMode;
   long     lDisplayAverageUpdate;
   short    nChannelStatsStrategy;
   long     lCalculationPeriod;       // N.B. Discontinued. Use fStatisticsPeriod.
   long     lSamplesPerTrace;
   long     lStartDisplayNum;
   long     lFinishDisplayNum;
   short    nMultiColor;
   short    nShowPNRawData;
   float    fStatisticsPeriod;
   long     lStatisticsMeasurements;
   short    nStatisticsSaveStrategy;

   // GROUP #5 - Hardware information (16 bytes)
   float    fADCRange;
   float    fDACRange;
   long     lADCResolution;
   long     lDACResolution;

   // GROUP #6 Environmental Information (118 bytes)
   short    nExperimentType;
   short    _nAutosampleEnable;
   short    _nAutosampleADCNum;
   short    _nAutosampleInstrument;
   float    _fAutosampleAdditGain;
   float    _fAutosampleFilter;
   float    _fAutosampleMembraneCap;
   short    nManualInfoStrategy;
   float    fCellID1;
   float    fCellID2;
   float    fCellID3;
   char     sCreatorInfo[ABF_CREATORINFOLEN];
   char     _sFileComment[ABF_OLDFILECOMMENTLEN];
   short    nFileStartMillisecs;    // Milliseconds portion of lFileStartTime
   short    nCommentsEnable;
   char     sUnused340[8];

   // GROUP #7 - Multi-channel information (1044 (160 + 384 + 488 + 12) bytes)
   short    nADCPtoLChannelMap[ABF_ADCCOUNT];
   short    nADCSamplingSeq[ABF_ADCCOUNT];
   char     sADCChannelName[ABF_ADCCOUNT][ABF_ADCNAMELEN];
   char     sADCUnits[ABF_ADCCOUNT][ABF_ADCUNITLEN];
   float    fADCProgrammableGain[ABF_ADCCOUNT];
   float    fADCDisplayAmplification[ABF_ADCCOUNT];
   float    fADCDisplayOffset[ABF_ADCCOUNT];
   float    fInstrumentScaleFactor[ABF_ADCCOUNT];
   float    fInstrumentOffset[ABF_ADCCOUNT];
   float    fSignalGain[ABF_ADCCOUNT];
   float    fSignalOffset[ABF_ADCCOUNT];
   float    fSignalLowpassFilter[ABF_ADCCOUNT];
   float    fSignalHighpassFilter[ABF_ADCCOUNT];
   char     sDACChannelName[ABF_DACCOUNT][ABF_DACNAMELEN];
   char     sDACChannelUnits[ABF_DACCOUNT][ABF_DACUNITLEN];
   float    fDACScaleFactor[ABF_DACCOUNT];
   float    fDACHoldingLevel[ABF_DACCOUNT];
   short    nSignalType;
   char     sUnused1412[10];

   // GROUP #8 - Synchronous timer outputs (14 bytes)
   short    nOUTEnable;
   short    nSampleNumberOUT1;
   short    nSampleNumberOUT2;
   short    nFirstEpisodeOUT;
   short    nLastEpisodeOUT;
   short    nPulseSamplesOUT1;
   short    nPulseSamplesOUT2;

   // GROUP #9 - Epoch Waveform and Pulses (184 bytes)
   short    nDigitalEnable;
   short    _nWaveformSource;
   short    nActiveDACChannel;
   short    _nInterEpisodeLevel;
   short    _nEpochType[ABF_EPOCHCOUNT];
   float    _fEpochInitLevel[ABF_EPOCHCOUNT];
   float    _fEpochLevelInc[ABF_EPOCHCOUNT];
   short    _nEpochInitDuration[ABF_EPOCHCOUNT];
   short    _nEpochDurationInc[ABF_EPOCHCOUNT];
   short    nDigitalHolding;
   short    nDigitalInterEpisode;
   short    nDigitalValue[ABF_EPOCHCOUNT];
   char     sUnavailable1608[4];    // was float fWaveformOffset;
   short    nDigitalDACChannel;
   char     sUnused1614[6];

   // GROUP #10 - DAC Output File (98 bytes)
   float    _fDACFileScale;
   float    _fDACFileOffset;
   char     sUnused1628[2];
   short    _nDACFileEpisodeNum;
   short    _nDACFileADCNum;
   char     _sDACFilePath[ABF_DACFILEPATHLEN];

   // GROUP #11 - Conditioning pulse train (44 bytes)
   short    _nConditEnable;
   short    _nConditChannel;
   long     _lConditNumPulses;
   float    _fBaselineDuration;
   float    _fBaselineLevel;
   float    _fStepDuration;
   float    _fStepLevel;
   float    _fPostTrainPeriod;
   float    _fPostTrainLevel;
   char     sUnused1750[12];

   // GROUP #12 - Variable parameter user list ( 82 bytes)
   short    _nParamToVary;
   char     _sParamValueList[ABF_VARPARAMLISTLEN];

   // GROUP #13 - Autopeak measurement (36 bytes)
   short    nAutopeakEnable;
   short    nAutopeakPolarity;
   short    nAutopeakADCNum;
   short    nAutopeakSearchMode;
   long     lAutopeakStart;
   long     lAutopeakEnd;
   short    nAutopeakSmoothing;
   short    nAutopeakBaseline;
   short    nAutopeakAverage;
   char     sUnavailable1866[2];     // Was nAutopeakSaveStrategy, use nStatisticsSaveStrategy
   long     lAutopeakBaselineStart;
   long     lAutopeakBaselineEnd;
   long     lAutopeakMeasurements;

   // GROUP #14 - Channel Arithmetic (52 bytes)
   short    nArithmeticEnable;
   float    fArithmeticUpperLimit;
   float    fArithmeticLowerLimit;
   short    nArithmeticADCNumA;
   short    nArithmeticADCNumB;
   float    fArithmeticK1;
   float    fArithmeticK2;
   float    fArithmeticK3;
   float    fArithmeticK4;
   char     sArithmeticOperator[ABF_ARITHMETICOPLEN];
   char     sArithmeticUnits[ABF_ARITHMETICUNITSLEN];
   float    fArithmeticK5;
   float    fArithmeticK6;
   short    nArithmeticExpression;
   char     sUnused1930[2];

   // GROUP #15 - On-line subtraction (34 bytes)
   short    _nPNEnable;
   short    nPNPosition;
   short    _nPNPolarity;
   short    nPNNumPulses;
   short    _nPNADCNum;
   float    _fPNHoldingLevel;
   float    fPNSettlingTime;
   float    fPNInterpulse;
   char     sUnused1954[12];

   // GROUP #16 - Unused space at end of header block (54 bytes)
   short    _nListEnable;

   short    nBellEnable[ABF_BELLCOUNT];
   short    nBellLocation[ABF_BELLCOUNT];
   short    nBellRepetitions[ABF_BELLCOUNT];

   short    nLevelHysteresis;
   long     lTimeHysteresis;
   short    nAllowExternalTags;

   char     nLowpassFilterType[ABF_ADCCOUNT];
   char     nHighpassFilterType[ABF_ADCCOUNT];
   short    nAverageAlgorithm;
   float    fAverageWeighting;
   short    nUndoPromptStrategy;
   short    nTrialTriggerSource;
   short    nStatisticsDisplayStrategy;
   short    nExternalTagType;
   long     lHeaderSize;
   double   dFileDuration;
   short    nStatisticsClearStrategy;
   // Size of v1.5 header = 2048

   // Extra parameters in v1.6

   // GROUP #2 - File Structure (8 * 2 = 16 + 100 bytes)
   long     lDACFilePtr[ABF_WAVEFORMCOUNT];
   long     lDACFileNumEpisodes[ABF_WAVEFORMCOUNT];
   char     sUnused2[10];

   // GROUP #7 - Multi-channel information (8 * 4 = 32 + 100 bytes)
   float    fDACCalibrationFactor[ABF_DACCOUNT];
   float    fDACCalibrationOffset[ABF_DACCOUNT];
   char     sUnused7[190];

   // GROUP #9 - Epoch Waveform and Pulses ( 186 * 2 = 368 + 100 bytes)
   short    nWaveformEnable[ABF_WAVEFORMCOUNT];
   short    nWaveformSource[ABF_WAVEFORMCOUNT];
   short    nInterEpisodeLevel[ABF_WAVEFORMCOUNT];
   short    nEpochType[ABF_WAVEFORMCOUNT][ABF_EPOCHCOUNT];
   float    fEpochInitLevel[ABF_WAVEFORMCOUNT][ABF_EPOCHCOUNT];
   float    fEpochLevelInc[ABF_WAVEFORMCOUNT][ABF_EPOCHCOUNT];
   long     lEpochInitDuration[ABF_WAVEFORMCOUNT][ABF_EPOCHCOUNT];
   long     lEpochDurationInc[ABF_WAVEFORMCOUNT][ABF_EPOCHCOUNT];
   char     sUnused9[40];

   // GROUP #10 - DAC Output File (270 * 2 = 540 + 12 bytes)
   float    fDACFileScale[ABF_WAVEFORMCOUNT];
   float    fDACFileOffset[ABF_WAVEFORMCOUNT];
   long     lDACFileEpisodeNum[ABF_WAVEFORMCOUNT];
   short    nDACFileADCNum[ABF_WAVEFORMCOUNT];
   char     sDACFilePath[ABF_WAVEFORMCOUNT][ABF_PATHLEN];
   char     sUnused10[12];

   // GROUP #11 - Conditioning pulse train (2 * 32 = 64 + 36 bytes)
   short    nConditEnable[ABF_WAVEFORMCOUNT];
   long     lConditNumPulses[ABF_WAVEFORMCOUNT];
   float    fBaselineDuration[ABF_WAVEFORMCOUNT];
   float    fBaselineLevel[ABF_WAVEFORMCOUNT];
   float    fStepDuration[ABF_WAVEFORMCOUNT];
   float    fStepLevel[ABF_WAVEFORMCOUNT];
   float    fPostTrainPeriod[ABF_WAVEFORMCOUNT];
   float    fPostTrainLevel[ABF_WAVEFORMCOUNT];
   short    nUnused11[ABF_WAVEFORMCOUNT];
   char     sUnused11[36];

   // GROUP #12 - Variable parameter user list (260 * 4 = 1040 + 56 bytes)
   short    nULEnable[ABF_USERLISTCOUNT];
   short    nULParamToVary[ABF_USERLISTCOUNT];
   char     sULParamValueList[ABF_USERLISTCOUNT][ABF_USERLISTLEN];
   char     sUnused12[56];

   // GROUP #15 - On-line subtraction (10 * 2 = 20 + 36 bytes)
   short    nPNEnable[ABF_WAVEFORMCOUNT];
   short    nPNPolarity[ABF_WAVEFORMCOUNT];
   short    nPNADCNum[ABF_WAVEFORMCOUNT];
   float    fPNHoldingLevel[ABF_WAVEFORMCOUNT];
   char     sUnused15[36];

   // GROUP #6 Environmental Information  ( (16 * 20) + 128 = 448 + 128 bytes)
   short    nTelegraphEnable[ABF_ADCCOUNT];
   short    nTelegraphInstrument[ABF_ADCCOUNT];
   float    fTelegraphAdditGain[ABF_ADCCOUNT];
   float    fTelegraphFilter[ABF_ADCCOUNT];
   float    fTelegraphMembraneCap[ABF_ADCCOUNT];
   short    nTelegraphMode[ABF_ADCCOUNT];
   short    nManualTelegraphStrategy[ABF_ADCCOUNT];

   short    nAutoAnalyseEnable;
   char     sAutoAnalysisMacroName[ABF_MACRONAMELEN];
   char     sProtocolPath[ABF_PATHLEN];

   char     sFileComment[ABF_FILECOMMENTLEN];
   char     sUnused6[128];

   char     sUnused2048[734];
   ABFFileHeader ();
};   // Size = 6144

inline ABFFileHeader::ABFFileHeader()
{
   // Set everything to 0.
   memset( this, 0, sizeof(ABFFileHeader) );

   // Set critical parameters so we can determine the version.
   lFileSignature       = ABF_NATIVESIGNATURE;
   fFileVersionNumber   = ABF_CURRENTVERSION;
   fHeaderVersionNumber = ABF_CURRENTVERSION;
   lHeaderSize          = ABF_HEADERSIZE;
}



//
// Scope descriptor format.
//
#define ABF_FACESIZE 32
struct ABFLogFont
{
   short nHeight;                // Height of the font in pixels.
//   short lWidth;               // use 0
//   short lEscapement;          // use 0
//   short lOrientation;         // use 0
   short nWeight;                // MSWindows font weight value.
//   char bItalic;               // use 0
//   char bUnderline;            // use 0
//   char bStrikeOut;            // use 0
//   char cCharSet;              // use ANSI_CHARSET (0)
//   char cOutPrecision;         // use OUT_TT_PRECIS
//   char cClipPrecision;        // use CLIP_DEFAULT_PRECIS
//   char cQuality;              // use PROOF_QUALITY
   char cPitchAndFamily;         // MSWindows pitch and family mask.
   char Unused[3];               // Unused space to maintain 4-byte packing.
   char szFaceName[ABF_FACESIZE];   // Face name of the font.
};     // Size = 40

struct ABFSignal
{
   char     szName[ABF_ADCNAMELEN+2];        // ABF name length + '\0' + 1 for alignment.
   short    nMxOffset;                       // Offset of the signal in the sampling sequence.
   DWORD    rgbColor;                        // Pen color used to draw trace.
   char     nPenWidth;                       // Pen width in pixels.
   char     bDrawPoints;                     // TRUE = Draw disconnected points
   char     bHidden;                         // TRUE = Hide the trace.
   char     bFloatData;                      // TRUE = Floating point pseudo channel
   float    fVertProportion;                 // Relative proportion of client area to use
   float    fDisplayGain;                    // Display gain of trace in UserUnits
   float    fDisplayOffset;                  // Display offset of trace in UserUnits

//   float    fUUTop;                          // Top of window in UserUnits
//   float    fUUBottom;                       // Bottom of window in UserUnits
};      // Size = 34

// Bit flags used in dwFlags field of ABFScopeConfig.
#define ABF_OVERLAPPED      0x00000001
#define ABF_DONTERASE       0x00000002
#define ABF_MONOCHROME      0x00000004
#define ABF_CLIPPING        0x00000008
#define ABF_HIDEHORZGRIDS   0x00000010
#define ABF_HIDEVERTGRIDS   0x00000020
#define ABF_FULLSCREEN      0x00000040
#define ABF_HIDEXAXIS       0x00000080
#define ABF_HIDEYAXIS       0x00000100
#define ABF_HIDEXSCROLL     0x00000200
#define ABF_HIDEYSCROLL     0x00000400
#define ABF_HIDESIGNALNAME  0x00000800
#define ABF_ENABLEZOOM      0x00001000
#define ABF_XSPINFROMCENTER 0x00002000
#define ABF_HIDEXSPINNER    0x00004000
#define ABF_LARGESPINNERS   0x00008000
#define ABF_PERSISTENCEMODE 0x00010000
#define ABF_CARDIACMODE     0x00020000
#define ABF_HIDETWIRLER     0x00040000
#define ABF_DISABLEUI       0x00080000

// Values for the wScopeMode field in ABFScopeConfig.
#define ABF_EPISODICMODE    0
#define ABF_CONTINUOUSMODE  1
//#define ABF_XYMODE          2

// Values for the nEraseStrategy field in ABFScopeConfig.
#define ABF_ERASE_EACHSWEEP   0
#define ABF_ERASE_EACHRUN     1
#define ABF_ERASE_EACHTRIAL   2
#define ABF_ERASE_DONTERASE   3

// Indexes into the rgbColor field of ABFScopeConfig.
#define ABF_BACKGROUNDCOLOR   0
#define ABF_GRIDCOLOR         1
#define ABF_THRESHOLDCOLOR    2
#define ABF_EVENTMARKERCOLOR  3
#define ABF_SEPARATORCOLOR    4
#define ABF_AVERAGECOLOR      5
#define ABF_OLDDATACOLOR      6
#define ABF_TEXTCOLOR         7
#define ABF_AXISCOLOR         8
#define ABF_ACTIVEAXISCOLOR   9
#define ABF_LASTCOLOR         ABF_ACTIVEAXISCOLOR
#define ABF_SCOPECOLORS       (ABF_LASTCOLOR+1)

// Values for the nDockState field in ABFScopeConfig
#define ABF_SCOPE_NOTDOCKED      0
#define ABF_SCOPE_DOCKED_TOP     1
#define ABF_SCOPE_DOCKED_LEFT    2
#define ABF_SCOPE_DOCKED_RIGHT   3
#define ABF_SCOPE_DOCKED_BOTTOM  4

struct ABFScopeConfig
{
   DWORD       dwFlags;                   // Flags that are meaningful to the scope.
   DWORD       rgbColor[ABF_SCOPECOLORS]; // Colors for the components of the scope.
   float       fDisplayStart;             // Start of the display area in ms.
   float       fDisplayEnd;               // End of the display area in ms.
   WORD        wScopeMode;                // Mode that the scope is in.
   char        bMaximized;                // TRUE = Scope parent is maximized.
   char        bMinimized;                // TRUE = Scope parent is minimized.
   short       xLeft;                     // Coordinate of the left edge.
   short       yTop;                      // Coordinate of the top edge.
   short       xRight;                    // Coordinate of the right edge.
   short       yBottom;                   // Coordinate of the bottom edge.
   ABFLogFont  LogFont;                   // Description of current font.
   ABFSignal   TraceList[ABF_ADCCOUNT];   // List of traces in current use.
   short       nYAxisWidth;               // Width of the YAxis region.
   short       nTraceCount;               // Number of traces described in TraceList.
   short       nEraseStrategy;            // Erase strategy.
   short       nDockState;                // Docked position.
}; // Size = 656

//
// Definition of the ABF synch array structure
//

struct ABFSynch
{
   long    lStart;            // Start of the episode/event in fSynchTimeUnit units.
   long    lLength;           // Length of the episode/event in multiplexed samples.
}; // Size = 8

//
// Constants for nTagType in the ABFTag structure.
//
#define ABF_TIMETAG              0
#define ABF_COMMENTTAG           1
#define ABF_EXTERNALTAG          2
#define ABF_VOICETAG             3

//
// Definition of the ABF Tag structure
//
struct ABFTag
{
   long    lTagTime;          // Time at which the tag was entered in fSynchTimeUnit units.
   char    sComment[ABF_TAGCOMMENTLEN];   // Optional tag comment.
   short   nTagType;          // Type of tag ABF_TIMETAG, ABF_COMMENTTAG, ABF_EXTERNALTAG or ABF_VOICETAG.
   short   nVoiceTagNumber;   // If nTagType=ABF_VOICETAG, this is the number of this voice tag.
}; // Size = 64

// Comment inserted for externally acquired tags (expanded with spaces to ABF_TAGCOMMENTLEN).
#define ABF_EXTERNALTAGCOMMENT   "<External>"
#define ABF_VOICETAGCOMMENT      "<Voice Tag>"

//
// Constants for nCompressionType in the ABFVoiceTagInfo structure.
//
#define ABF_COMPRESSION_NONE     0
#define ABF_COMPRESSION_PKWARE   1
//#define ABF_COMPRESSION_MPEG     2

//
// Definition of the ABFVoiceTagInfo structure.
//
struct ABFVoiceTagInfo
{
   long  lTagNumber;          // The tag number that corresponds to this VoiceTag
   long  lFileOffset;         // Offset to this tag within the VoiceTag block
   long  lUncompressedSize;   // Size of the voice tag expanded.
   long  lCompressedSize;     // Compressed size of the tag.
   short nCompressionType;    // Compression method used.
   short nSampleSize;         // Size of the samples acquired.
   long  lSamplesPerSecond;   // Rate at which the sound was acquired.
   DWORD dwCRC;               // CRC used to check data integrity.
   WORD  wChannels;           // Number of channels in the tag (usually 1).
   WORD  wUnused;             // Unused space.
}; // Size 32

//
// Constants for lParameterID in the ABFDelta structure.
//
// NOTE: If any changes are made to this list, the code in ABF_UpdateHeader must
//       be updated to include the new items.
#define ABF_DELTA_HOLDING0          0
#define ABF_DELTA_HOLDING1          1
#define ABF_DELTA_HOLDING2          2
#define ABF_DELTA_HOLDING3          3
#define ABF_DELTA_DIGITALOUTS       4
#define ABF_DELTA_THRESHOLD         5
#define ABF_DELTA_PRETRIGGER        6

// Because of lack of space, the Autosample Gain ID also contains the ADC number.
#define ABF_DELTA_AUTOSAMPLE_GAIN   100   // +ADC channel.

// Because of lack of space, the Signal Gain ID also contains the ADC number.
#define ABF_DELTA_SIGNAL_GAIN       200   // +ADC channel.

//
// Definition of the ABF Delta structure.
//

struct ABFDelta
{
   long    lDeltaTime;        // Time at which the parameter was changed in fSynchTimeUnit units.
   long    lParameterID;      // Identifier for the parameter changed
   union
   {
      long  lNewParamValue;   // Depending on the value of lParameterID
      float fNewParamValue;   // this entry may be either a float or a long.
   };
}; // Size = 12

#ifndef RC_INVOKED
#pragma pack()                      // return to default packing
#endif

//
// The size of the buffers to be passed to ABFH_GetWaveformVertor
//
#define ABFH_MAXVECTORS     30





//
// Error return values that may be returned by the ABFH_xxx functions.
//

#define ABFH_FIRSTERRORNUMBER          2001
#define ABFH_EHEADERREAD               2001
#define ABFH_EHEADERWRITE              2002
#define ABFH_EINVALIDFILE              2003
#define ABFH_EUNKNOWNFILETYPE          2004
#define ABFH_CHANNELNOTSAMPLED         2005
#define ABFH_EPOCHNOTPRESENT           2006
#define ABFH_ENOWAVEFORM               2007
#define ABFH_EDACFILEWAVEFORM          2008
#define ABFH_ENOMEMORY                 2009
#define ABFH_BADSAMPLEINTERVAL         2010
#define ABFH_BADSECONDSAMPLEINTERVAL   2011
#define ABFH_BADSAMPLEINTERVALS        2012
#define ABFH_ENOCONDITTRAINS           2013
#define ABFH_EMETADURATION             2014
#define ABFH_ECONDITNUMPULSES          2015
#define ABFH_ECONDITBASEDUR            2016
#define ABFH_ECONDITBASELEVEL          2017
#define ABFH_ECONDITPOSTTRAINDUR       2018
#define ABFH_ECONDITPOSTTRAINLEVEL     2019
#define ABFH_ESTART2START              2020
#define ABFH_EINACTIVEHOLDING          2021
#define ABFH_EINVALIDCHARS             2022
#define ABFH_ENODIG                    2023
#define ABFH_EDIGHOLDLEVEL             2024
#define ABFH_ENOPNPULSES               2025
#define ABFH_EPNNUMPULSES              2026
#define ABFH_ENOEPOCH                  2027
#define ABFH_EEPOCHLEN                 2028
#define ABFH_EEPOCHINITLEVEL           2029
#define ABFH_EDIGLEVEL                 2030
#define ABFH_ECONDITSTEPDUR            2031
#define ABFH_ECONDITSTEPLEVEL          2032
#define ABFH_EINVALIDBINARYCHARS       2033
#define ABFH_EBADWAVEFORM              2034



/*
error handling: use axon defined consts
if more error codes necessary, define below
*/
#define ABFH_ENOTGAPFREE               2035
#define ABFH_EBADHEADER	               2036


//some macros (from abfutil.h in axon code

#define ABF_BLANK_FILL(d)       (memset((void *)(d), ' ', sizeof(d)))
//#define ABF_SET_STRING(d, s)    (ABFU_SetABFString(d, s, sizeof(d)))
//#define ABF_GET_STRING(d, s, n) (ABFU_GetABFString(d, n, s, sizeof(s)))


 /*******************************************
 Error handling follows gsl/posix convention:
 if everything is Ok, function returns 0, otherwise error code (defined in abfheader.h) is returned
 */

//some input-output functions

//open file and checks if it is an ABF, if yes returns 0
//if not an ABF file closes it and returns ABFH_EINVALIDFILE
//reads and returns file header (if fails returns ABFH_EHEADERREAD)
//file can be closed with regular fclose()
int ABFFileOpen(char* fName,FileRec *F,ABFFileHeader *pH);

//as previous, plus checks that this is a gap-free file (otherwise returns ABFH_ENOTGAPFREE)
//(also does some header consistency checks)
//returns in addition nChanls, nSamples (per each channel, - that's for one channel, not aggregate!)
int ABFOpenGapFree(char* fName,FileRec *F,ABFFileHeader *pH,UINT *nChanls,unsigned long *nSamples);

//print some header info to a stream
void ABFPrintHeaderInfo(FILE *F,ABFFileHeader* pH);

//get offset of first data byte
long ABFGetDataOffset(const ABFFileHeader *pFH);

//seek to the start of data section in opened file
int ABFSeekToDataStart(FileRec *F,ABFFileHeader* pH);

//get sample size used (short or float)
UINT SampleSize(const ABFFileHeader *pFH);

//calculates gain and offset factors from the header.
//returned values:
//cnt2V - counts2Volts farctor (V=cnt2V*orig data); V2UU - Volts 2 UserUnits
//offset - cumulative offset in UU, SampleInterval - sampling interval in us
//int ABFGetFactors(const ABFFileHeader *pFH,UINT nChan,float& cnt2V, float& V2UU, float& offset, float& SampleInterval);
int ABFGetFactors(const ABFFileHeader *pFH,UINT nChannel,float *ADC2UUfactor, float *ADC2UUoffset);


// Some output and data initialization routines

// Initialize an ABFFileHeader structure to a consistent set of parameters
ABFFileHeader* ABFNewGapFreeHeader();


//time and current unit definitions

#define tuMSEC 0
#define tuSEC  1

#define cuUNDEFINED 0
#define cuPA   1
#define cuNA   2
#define cuMA   3

//calculates scale factor to convert data to new current units
//usage:
//f=ABFGetCScaleFactor(pH,newUnits);
//newVal = oldVal * f;
//units must be one of cuXX constants
float ABFGetCScaleFactor (const ABFFileHeader* pH,UINT nChanl, int newcUnits);

//returns one of cuXX constants
int ABFGetCUnits(const ABFFileHeader* pH, UINT nChanl);

//some axon utility routines
//===============================================================================================
// FUNCTION: GetADCChannelUnits
// PURPOSE:  Fills the passed buffer with the channel units for a particular ADC channel.
//           Builds a zero terminated "C" string from a fixed length array of bytes.
//
//modified to follow sample sequency, necessary because axon has weird conventions
//about where to keep data vs labels; more of a hack than clean solution
int GetADCChannelUnits(const ABFFileHeader *pFH, int nADCChannel, char *szBuffer);

//follows GetCUnits routine, sets the necessary string according to units specified
int ABFSetCUnits(ABFFileHeader* pH, UINT nChanl, int cUnits);

