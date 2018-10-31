with Interfaces.C;

private package ABF.Header_Struct is

	-- first we need to bring in interfaces and corresponding array types
	package C renames Interfaces.C;



	type charArray is new C.char_array;

	type shortArray is array(C.size_t range <>)of aliased C.short;
	pragma Convention(C,shortArray);
	type longArray  is array(C.size_t range <>)of aliased C.long;
	pragma Convention(C,longArray);
	type floatArray is array(C.size_t range <>)of aliased C.C_float;
	pragma Convention(C,floatArray);

	type char2DArray  is array(C.size_t range <>, C.size_t range <>)of aliased C.char;
	pragma Convention(C,char2DArray);
	type short2DArray is array(C.size_t range <>, C.size_t range <>)of aliased C.short;
	pragma Convention(C,short2DArray);
	type long2DArray  is array(C.size_t range <>, C.size_t range <>)of aliased C.long;
	pragma Convention(C,long2DArray);
	type float2DArray is array(C.size_t range <>, C.size_t range <>)of aliased C.C_float;
	pragma Convention(C,float2DArray);


--
--  Constants used in defining the ABF file header
--

ABF_ADCCOUNT: Constant := 16;
ABF_DACCOUNT: Constant := 4;
ABF_WAVEFORMCOUNT: Constant := 2;
ABF_EPOCHCOUNT: Constant := 10;
ABF_BELLCOUNT: Constant := 2;
ABF_ADCUNITLEN: Constant := 8;
ABF_ADCNAMELEN: Constant := 10;
ABF_DACUNITLEN: Constant := 8;
ABF_DACNAMELEN: Constant := 10;
ABF_VARPARAMLISTLEN: Constant := 80;
ABF_USERLISTLEN: Constant := 256;
ABF_USERLISTCOUNT: Constant := 4;
ABF_OLDFILECOMMENTLEN: Constant := 56;
ABF_FILECOMMENTLEN: Constant := 128;

ABF_CREATORINFOLEN: Constant := 16;
ABF_OLDDACFILENAMELEN: Constant := 12;
ABF_OLDDACFILEPATHLEN: Constant := 60;
ABF_DACFILEPATHLEN: Constant := 84;
ABF_PATHLEN: Constant := 256;
ABF_ARITHMETICOPLEN: Constant := 2;
ABF_ARITHMETICUNITSLEN: Constant := 8;
ABF_TAGCOMMENTLEN: Constant := 56;
ABF_LONGDESCRIPTIONLEN: Constant := 56;
ABF_NOTENAMELEN: Constant := 10;
ABF_NOTEVALUELEN: Constant := 8;
ABF_NOTEUNITSLEN: Constant := 8;
ABF_BLOCKSIZE: Constant := 512;
ABF_MACRONAMELEN: Constant := 64;

ABF_CURRENTVERSION: Constant := 1.65;
ABF_PREVIOUSVERSION: Constant := 1.5;
ABF_V16: Constant := 1.6;
ABF_HEADERSIZE: Constant := 6144;
ABF_OLDHEADERSIZE: Constant := 2048;
ABF_NATIVESIGNATURE: Constant := 16#20464241#;
ABF_REVERSESIGNATURE: Constant := 16#41424620#;

PCLAMP6_MAXSWEEPLENGTH: Constant := 16384;
PCLAMP7_MAXSWEEPLEN_PERCHAN: Constant := 103224;

ABF_MAX_TRIAL_SAMPLES: Constant := 16#7FFFFFFF#;
--  INT_MAX is used instead of UINT_MAX because of the signed
--  values in the ABF header.

ABF_MAX_SWEEPS_PER_AVERAGE: Constant := 65500;
--  cumulative average (nAverageAlgorithm=ABF_INFINITEAVERAGE).

ABF_OLDPCLAMP: Constant := ABF_NATIVESIGNATURE;

--
--  Constant definitions for nFileType
--
ABF_ABFFILE: Constant := 1;
ABF_FETCHEX: Constant := 2;
ABF_CLAMPEX: Constant := 3;

--
--  Constant definitions for nDataFormat
--
ABF_INTEGERDATA: Constant := 0;
ABF_FLOATDATA: Constant := 1;

--
--  Constant definitions for nOperationMode
--
ABF_VARLENEVENTS: Constant := 1;
ABF_FIXLENEVENTS: Constant := 2;
ABF_LOSSFREEOSC: Constant := 2;
ABF_GAPFREEFILE: Constant := 3;
ABF_HIGHSPEEDOSC: Constant := 4;
ABF_WAVEFORMFILE: Constant := 5;

--
--  Constant definitions for nParamToVary
--
ABF_CONDITNUMPULSES: Constant := 0;
ABF_CONDITBASELINEDURATION: Constant := 1;
ABF_CONDITBASELINELEVEL: Constant := 2;
ABF_CONDITSTEPDURATION: Constant := 3;
ABF_CONDITSTEPLEVEL: Constant := 4;
ABF_CONDITPOSTTRAINDURATION: Constant := 5;
ABF_CONDITPOSTTRAINLEVEL: Constant := 6;
ABF_EPISODESTARTTOSTART: Constant := 7;
ABF_INACTIVEHOLDING: Constant := 8;
ABF_DIGITALHOLDING: Constant := 9;
ABF_PNNUMPULSES: Constant := 10;
ABF_PARALLELVALUE: Constant := 11;
ABF_EPOCHINITLEVEL: Constant := 21;
ABF_EPOCHINITDURATION: Constant := 31;

--
--  Constants for nAveragingMode
--
ABF_NOAVERAGING: Constant := 0;
ABF_SAVEAVERAGEONLY: Constant := 1;
ABF_AVERAGESAVEALL: Constant := 2;

--
--  Constants for nAverageAlgorithm
--
ABF_INFINITEAVERAGE: Constant := 0;
ABF_SLIDINGAVERAGE: Constant := 1;

--
--  Constants for nEpochType
--
ABF_EPOCHDISABLED: Constant := 0;
ABF_EPOCHSTEPPED: Constant := 1;
ABF_EPOCHRAMPED: Constant := 2;

--
--  Constants for nWaveformSource
--
ABF_WAVEFORMDISABLED: Constant := 0;
ABF_EPOCHTABLEWAVEFORM: Constant := 1;
ABF_DACFILEWAVEFORM: Constant := 2;

--
--  Constants for nInterEpisodeLevel & nDigitalInterEpisode
--
ABF_INTEREPI_USEHOLDING: Constant := 0;
ABF_INTEREPI_USELASTEPOCH: Constant := 1;

--
--  Constants for nExperimentType
--
ABF_VOLTAGECLAMP: Constant := 0;
ABF_CURRENTCLAMP: Constant := 1;
ABF_SIMPLEACQUISITION: Constant := 2;

--
--  Constants for nAutosampleEnable
--
ABF_AUTOSAMPLEDISABLED: Constant := 0;
ABF_AUTOSAMPLEAUTOMATIC: Constant := 1;
ABF_AUTOSAMPLEMANUAL: Constant := 2;

--
--  Constants for nAutosampleInstrument
--
ABF_INST_UNKNOWN: Constant := 0;
ABF_INST_AXOPATCH1: Constant := 1;
ABF_INST_AXOPATCH1_1: Constant := 2;
ABF_INST_AXOPATCH1B: Constant := 3;
ABF_INST_AXOPATCH1B_1: Constant := 4;
ABF_INST_AXOPATCH201: Constant := 5;
ABF_INST_AXOPATCH202: Constant := 6;
ABF_INST_GENECLAMP: Constant := 7;
ABF_INST_DAGAN3900: Constant := 8;
ABF_INST_DAGAN3900A: Constant := 9;
ABF_INST_DAGANCA1_1: Constant := 10;
ABF_INST_DAGANCA1: Constant := 11;
ABF_INST_DAGANCA10: Constant := 12;
ABF_INST_WARNER_OC725: Constant := 13;
ABF_INST_WARNER_OC725C: Constant := 14;
ABF_INST_AXOPATCH200B: Constant := 15;
ABF_INST_DAGANPCONE0_1: Constant := 16;
ABF_INST_DAGANPCONE1: Constant := 17;
ABF_INST_DAGANPCONE10: Constant := 18;
ABF_INST_DAGANPCONE100: Constant := 19;
ABF_INST_WARNER_BC525C: Constant := 20;
ABF_INST_WARNER_PC505: Constant := 21;
ABF_INST_WARNER_PC501: Constant := 22;
ABF_INST_DAGANCA1_05: Constant := 23;
ABF_INST_MULTICLAMP700: Constant := 24;
ABF_INST_TURBO_TEC: Constant := 25;

--
--  Constants for nManualInfoStrategy
--
ABF_ENV_DONOTWRITE: Constant := 0;
ABF_ENV_WRITEEACHTRIAL: Constant := 1;
ABF_ENV_PROMPTEACHTRIAL: Constant := 2;

--
--  Constants for nTriggerSource
--
ABF_TRIGGERLINEINPUT: Constant := -5;
ABF_TRIGGERTAGINPUT: Constant := -4;
ABF_TRIGGERFIRSTCHANNEL: Constant := -3;
ABF_TRIGGEREXTERNAL: Constant := -2;
ABF_TRIGGERSPACEBAR: Constant := -1;
--  >=0 = ADC channel to trigger off.

--
--  Constants for nTrialTriggerSource
--
ABF_TRIALTRIGGER_SWSTARTONLY: Constant := -6;
ABF_TRIALTRIGGER_SWSTARTSTOP: Constant := -5;
ABF_TRIALTRIGGER_LINEINPUT: Constant := -4;
ABF_TRIALTRIGGER_SPACEBAR: Constant := -3;
ABF_TRIALTRIGGER_EXTERNAL: Constant := -2;
ABF_TRIALTRIGGER_NONE: Constant := -1;
--  >=0 = ADC channel to trigger off.    // Not implemented as yet...

--
--  Constants for nTriggerPolarity.
--
ABF_TRIGGER_RISINGEDGE: Constant := 0;
ABF_TRIGGER_FALLINGEDGE: Constant := 1;

--
--  Constants for nTriggerAction
--
ABF_TRIGGER_STARTEPISODE: Constant := 0;
ABF_TRIGGER_STARTRUN: Constant := 1;
ABF_TRIGGER_STARTTRIAL: Constant := 2;

--
--  Constants for nDrawingStrategy
--
ABF_DRAW_NONE: Constant := 0;
ABF_DRAW_REALTIME: Constant := 1;
ABF_DRAW_FULLSCREEN: Constant := 2;
ABF_DRAW_ENDOFRUN: Constant := 3;

--
--  Constants for nTiledDisplay
--
ABF_DISPLAY_SUPERIMPOSED: Constant := 0;
ABF_DISPLAY_TILED: Constant := 1;

--
--  Constants for nDataDisplayMode
--
ABF_DRAW_POINTS: Constant := 0;
ABF_DRAW_LINES: Constant := 1;

--
--  Constants for nArithmeticExpression
--
ABF_SIMPLE_EXPRESSION: Constant := 0;
ABF_RATIO_EXPRESSION: Constant := 1;

--
--  Constants for nLowpassFilterType & nHighpassFilterType
--
ABF_FILTER_NONE: Constant := 0;
ABF_FILTER_EXTERNAL: Constant := 1;
ABF_FILTER_SIMPLE_RC: Constant := 2;
ABF_FILTER_BESSEL: Constant := 3;
ABF_FILTER_BUTTERWORTH: Constant := 4;

--
--  Constants for nPNPosition
--
ABF_PN_BEFORE_EPISODE: Constant := 0;
ABF_PN_AFTER_EPISODE: Constant := 1;

--
--  Constants for nPNPolarity
--
ABF_PN_OPPOSITE_POLARITY: Constant := -1;
ABF_PN_SAME_POLARITY: Constant := 1;

--
--  Constants for nAutopeakPolarity
--
ABF_PEAK_NEGATIVE: Constant := -1;
ABF_PEAK_ABSOLUTE: Constant := 0;
ABF_PEAK_POSITIVE: Constant := 1;

--
--  Constants for nAutopeakSearchMode
--
ABF_PEAK_SEARCH_SPECIFIED: Constant := -2;
ABF_PEAK_SEARCH_ALL: Constant := -1;
--  nAutopeakSearchMode 0..9   = epoch in waveform 0's epoch table
--  nAutopeakSearchMode 10..19 = epoch in waveform 1's epoch table

--
--  Constants for nAutopeakBaseline
--
ABF_PEAK_BASELINE_SPECIFIED: Constant := -3;
ABF_PEAK_BASELINE_NONE: Constant := -2;
ABF_PEAK_BASELINE_FIRSTHOLDING: Constant := -1;
ABF_PEAK_BASELINE_LASTHOLDING: Constant := -4;

--
--  Constants for lAutopeakMeasurements
--
ABF_PEAK_MEASURE_PEAK: Constant := 16#00000001#;
ABF_PEAK_MEASURE_PEAKTIME: Constant := 16#00000002#;
ABF_PEAK_MEASURE_ANTIPEAK: Constant := 16#00000004#;
ABF_PEAK_MEASURE_ANTIPEAKTIME: Constant := 16#00000008#;
ABF_PEAK_MEASURE_MEAN: Constant := 16#00000010#;
ABF_PEAK_MEASURE_STDDEV: Constant := 16#00000020#;
ABF_PEAK_MEASURE_INTEGRAL: Constant := 16#00000040#;
ABF_PEAK_MEASURE_LEFTSLOPE: Constant := 16#00000080#;
ABF_PEAK_MEASURE_LEFTSLOPETIME: Constant := 16#00000100#;
ABF_PEAK_MEASURE_RIGHTSLOPE: Constant := 16#00000200#;
ABF_PEAK_MEASURE_RIGHTSLOPETIME: Constant := 16#00000400#;
ABF_PEAK_MEASURE_RISETIME: Constant := 16#00000800#;
ABF_PEAK_MEASURE_DECAYTIME: Constant := 16#00001000#;
ABF_PEAK_MEASURE_HALFWIDTH: Constant := 16#00002000#;
ABF_PEAK_MEASURE_BASELINE: Constant := 16#00004000#;
ABF_PEAK_MEASURE_ALL: Constant := 16#00007FFF#;

--
--  Constants for lStatisticsMeasurements
--
ABF_STATISTICS_ABOVETHRESHOLD: Constant := 16#00000001#;
ABF_STATISTICS_EVENTFREQUENCY: Constant := 16#00000002#;
ABF_STATISTICS_MEANOPENTIME: Constant := 16#00000004#;
ABF_STATISTICS_MEANCLOSEDTIME: Constant := 16#00000008#;
ABF_STATISTICS_ALL: Constant := 16#0000000F#;

--
--  Constants for nStatisticsSaveStrategy
--
ABF_STATISTICS_NOAUTOSAVE: Constant := 0;
ABF_STATISTICS_AUTOSAVE: Constant := 1;
ABF_STATISTICS_AUTOSAVE_AUTOCLEAR: Constant := 2;

--
--  Constants for nStatisticsDisplayStrategy
--
ABF_STATISTICS_DISPLAY: Constant := 0;
ABF_STATISTICS_NODISPLAY: Constant := 1;

--
--  Constants for nStatisticsClearStrategy
--  determines whether to clear statistics after saving.
--
ABF_STATISTICS_NOCLEAR: Constant := 0;
ABF_STATISTICS_CLEAR: Constant := 1;

--
--  Constants for nDACFileEpisodeNum
--
ABF_DACFILE_SKIPFIRSTSWEEP: Constant := -1;
ABF_DACFILE_USEALLSWEEPS: Constant := 0;
--  >0 = The specific sweep number.

--
--  Constants for nUndoPromptStrategy
--
ABF_UNDOPROMPT_ONABORT: Constant := 0;
ABF_UNDOPROMPT_ALWAYS: Constant := 1;

--
--  Constants for nAutoAnalyseEnable
--
ABF_AUTOANALYSE_DISABLED: Constant := 0;
ABF_AUTOANALYSE_DEFAULT: Constant := 1;
ABF_AUTOANALYSE_RUNMACRO: Constant := 2;

--
--  Miscellaneous constants
--
ABF_FILTERDISABLED: Constant := 100000.0;
ABF_UNUSED_CHANNEL: Constant := -1;

--
--  The output sampling sequence identifier for a seperate digital out channel.
--
ABF_DIGITAL_OUT_CHANNEL: Constant := -1;
ABF_PADDING_OUT_CHANNEL: Constant := -2;

--  maximum values for various parameters (used by ABFH_CheckUserList).
ABF_CTPULSECOUNT_MAX: Constant := 10000;
ABF_CTBASELINEDURATION_MAX: Constant := 100000.0;
ABF_CTSTEPDURATION_MAX: Constant := 100000.0;
ABF_CTPOSTTRAINDURATION_MAX: Constant := 100000.0;
ABF_SWEEPSTARTTOSTARTTIME_MAX: Constant := 100000.0;
ABF_PNPULSECOUNT_MAX: Constant := 8;
ABF_DIGITALVALUE_MAX: Constant := 16#FF#;
ABF_EPOCHDIGITALVALUE_MAX: Constant := 16#0F#;

--
--  pack structure on byte boundaries
--
-- #ifndef RC_INVOKED
-- #pragma pack(1)
-- #endif

--
--  Definition of the ABF header structure.
--

type ABFFileHeader is record
	-- The total header length = 5120 bytes.
	--  GROUP #1 - File ID and size information. (40 bytes)
	lFileSignature : C.long;
	fFileVersionNumber : C.C_float;
	nOperationMode : C.short;
	lActualAcqLength : C.long;
	nNumPointsIgnored : C.short;
	lActualEpisodes : C.long;
	lFileStartDate : C.long;  -- YYYYMMDD
	lFileStartTime : C.long;
	lStopwatchTime : C.long;
	fHeaderVersionNumber : C.C_float;
	nFileType : C.short;
	nMSBinFormat : C.short;

	--  GROUP #2 - File Structure (80 bytes)
	lDataSectionPtr : C.long;
	lTagSectionPtr : C.long;
	lNumTagEntries : C.long;
	lScopeConfigPtr : C.long;
	lNumScopes : C.long;
	A_lDACFilePtr : C.long;
	A_lDACFileNumEpisodes : C.long;
	sUnused68 : charArray(0..4);
	lDeltaArrayPtr : C.long;
	lNumDeltas : C.long;
	lVoiceTagPtr : C.long;
	lVoiceTagEntries : C.long;
	lUnused88 : C.long;
	lSynchArrayPtr : C.long;
	lSynchArraySize : C.long;
	nDataFormat : C.short;
	nSimultaneousScan : C.short;
	lStatisticsConfigPtr : C.long;
	sUnused108 : charArray(0..12);

	--  GROUP #3 - Trial hierarchy information (80 bytes)
	nADCNumChannels : C.short;
	fADCSampleInterval : C.C_float;
	fADCSecondSampleInterval : C.C_float;
	fSynchTimeUnit : C.C_float;
	fSecondsPerRun : C.C_float;
	lNumSamplesPerEpisode : C.long;
	lPreTriggerSamples : C.long;
	lEpisodesPerRun : C.long;
	lRunsPerTrial : C.long;
	lNumberOfTrials : C.long;
	nAveragingMode : C.short;
	nUndoRunCount : C.short;
	nFirstEpisodeInRun : C.short;
	fTriggerThreshold : C.C_float;
	nTriggerSource : C.short;
	nTriggerAction : C.short;
	nTriggerPolarity : C.short;
	fScopeOutputInterval : C.C_float;
	fEpisodeStartToStart : C.C_float;
	fRunStartToStart : C.C_float;
	fTrialStartToStart : C.C_float;
	lAverageCount : C.long;
	lClockChange : C.long;
	nAutoTriggerStrategy : C.short;

	--  GROUP #4 - Display Parameters (44 bytes)
	nDrawingStrategy : C.short;
	nTiledDisplay : C.short;
	nEraseStrategy : C.short;  -- N.B. Discontinued. Use scope config entry instead.
	nDataDisplayMode : C.short;
	lDisplayAverageUpdate : C.long;
	nChannelStatsStrategy : C.short;
	lCalculationPeriod : C.long;  -- N.B. Discontinued. Use fStatisticsPeriod.
	lSamplesPerTrace : C.long;
	lStartDisplayNum : C.long;
	lFinishDisplayNum : C.long;
	nMultiColor : C.short;
	nShowPNRawData : C.short;
	fStatisticsPeriod : C.C_float;
	lStatisticsMeasurements : C.long;
	nStatisticsSaveStrategy : C.short;

	--  GROUP #5 - Hardware information (16 bytes)
	fADCRange : C.C_float;
	fDACRange : C.C_float;
	lADCResolution : C.long;
	lDACResolution : C.long;

	--  GROUP #6 Environmental Information (118 bytes)
	nExperimentType : C.short;
	A_nAutosampleEnable : C.short;
	A_nAutosampleADCNum : C.short;
	A_nAutosampleInstrument : C.short;
	A_fAutosampleAdditGain : C.C_float;
	A_fAutosampleFilter : C.C_float;
	A_fAutosampleMembraneCap : C.C_float;
	nManualInfoStrategy : C.short;
	fCellID1 : C.C_float;
	fCellID2 : C.C_float;
	fCellID3 : C.C_float;
	sCreatorInfo : charArray(0..ABF_CREATORINFOLEN);
	A_sFileComment : charArray(0..ABF_OLDFILECOMMENTLEN);
	nFileStartMillisecs : C.short;  -- Milliseconds portion of lFileStartTime
	nCommentsEnable : C.short;
	sUnused340 : charArray(0..8);

	--  GROUP #7 - Multi-channel information (1044 (160 + 384 + 488 + 12) bytes)
	nADCPtoLChannelMap : shortArray(0..ABF_ADCCOUNT);
	nADCSamplingSeq : shortArray(0..ABF_ADCCOUNT);
	sADCChannelName : char2DArray(0..ABF_ADCCOUNT,0..ABF_ADCNAMELEN);
	sADCUnits : char2DArray(0..ABF_ADCCOUNT,0..ABF_ADCUNITLEN);
	fADCProgrammableGain : floatArray(0..ABF_ADCCOUNT);
	fADCDisplayAmplification : floatArray(0..ABF_ADCCOUNT);
	fADCDisplayOffset : floatArray(0..ABF_ADCCOUNT);
	fInstrumentScaleFactor : floatArray(0..ABF_ADCCOUNT);
	fInstrumentOffset : floatArray(0..ABF_ADCCOUNT);
	fSignalGain : floatArray(0..ABF_ADCCOUNT);
	fSignalOffset : floatArray(0..ABF_ADCCOUNT);
	fSignalLowpassFilter : floatArray(0..ABF_ADCCOUNT);
	fSignalHighpassFilter : floatArray(0..ABF_ADCCOUNT);
	sDACChannelName : char2DArray(0..ABF_DACCOUNT,0..ABF_DACNAMELEN);
	sDACChannelUnits : char2DArray(0..ABF_DACCOUNT,0..ABF_DACUNITLEN);
	fDACScaleFactor : floatArray(0..ABF_DACCOUNT);
	fDACHoldingLevel : floatArray(0..ABF_DACCOUNT);
	nSignalType : C.short;
	sUnused1412 : charArray(0..10);

	--  GROUP #8 - Synchronous timer outputs (14 bytes)
	nOUTEnable : C.short;
	nSampleNumberOUT1 : C.short;
	nSampleNumberOUT2 : C.short;
	nFirstEpisodeOUT : C.short;
	nLastEpisodeOUT : C.short;
	nPulseSamplesOUT1 : C.short;
	nPulseSamplesOUT2 : C.short;

	--  GROUP #9 - Epoch Waveform and Pulses (184 bytes)
	nDigitalEnable : C.short;
	A_nWaveformSource : C.short;
	nActiveDACChannel : C.short;
	A_nInterEpisodeLevel : C.short;
	A_nEpochType : shortArray(0..ABF_EPOCHCOUNT);
	A_fEpochInitLevel : floatArray(0..ABF_EPOCHCOUNT);
	A_fEpochLevelInc : floatArray(0..ABF_EPOCHCOUNT);
	A_nEpochInitDuration : shortArray(0..ABF_EPOCHCOUNT);
	A_nEpochDurationInc : shortArray(0..ABF_EPOCHCOUNT);
	nDigitalHolding : C.short;
	nDigitalInterEpisode : C.short;
	nDigitalValue : shortArray(0..ABF_EPOCHCOUNT);
	sUnavailable1608 : charArray(0..4);  -- was float fWaveformOffset;
	nDigitalDACChannel : C.short;
	sUnused1614 : charArray(0..6);

	--  GROUP #10 - DAC Output File (98 bytes)
	A_fDACFileScale : C.C_float;
	A_fDACFileOffset : C.C_float;
	sUnused1628 : charArray(0..2);
	A_nDACFileEpisodeNum : C.short;
	A_nDACFileADCNum : C.short;
	A_sDACFilePath : charArray(0..ABF_DACFILEPATHLEN);

	--  GROUP #11 - Conditioning pulse train (44 bytes)
	A_nConditEnable : C.short;
	A_nConditChannel : C.short;
	A_lConditNumPulses : C.long;
	A_fBaselineDuration : C.C_float;
	A_fBaselineLevel : C.C_float;
	A_fStepDuration : C.C_float;
	A_fStepLevel : C.C_float;
	A_fPostTrainPeriod : C.C_float;
	A_fPostTrainLevel : C.C_float;
	sUnused1750 : charArray(0..12);

	--  GROUP #12 - Variable parameter user list ( 82 bytes)
	A_nParamToVary : C.short;
	A_sParamValueList : charArray(0..ABF_VARPARAMLISTLEN);

	--  GROUP #13 - Autopeak measurement (36 bytes)
	nAutopeakEnable : C.short;
	nAutopeakPolarity : C.short;
	nAutopeakADCNum : C.short;
	nAutopeakSearchMode : C.short;
	lAutopeakStart : C.long;
	lAutopeakEnd : C.long;
	nAutopeakSmoothing : C.short;
	nAutopeakBaseline : C.short;
	nAutopeakAverage : C.short;
	sUnavailable1866 : charArray(0..2);  -- Was nAutopeakSaveStrategy, use nStatisticsSaveStrategy
	lAutopeakBaselineStart : C.long;
	lAutopeakBaselineEnd : C.long;
	lAutopeakMeasurements : C.long;

	--  GROUP #14 - Channel Arithmetic (52 bytes)
	nArithmeticEnable : C.short;
	fArithmeticUpperLimit : C.C_float;
	fArithmeticLowerLimit : C.C_float;
	nArithmeticADCNumA : C.short;
	nArithmeticADCNumB : C.short;
	fArithmeticK1 : C.C_float;
	fArithmeticK2 : C.C_float;
	fArithmeticK3 : C.C_float;
	fArithmeticK4 : C.C_float;
	sArithmeticOperator : charArray(0..ABF_ARITHMETICOPLEN);
	sArithmeticUnits : charArray(0..ABF_ARITHMETICUNITSLEN);
	fArithmeticK5 : C.C_float;
	fArithmeticK6 : C.C_float;
	nArithmeticExpression : C.short;
	sUnused1930 : charArray(0..2);

	--  GROUP #15 - On-line subtraction (34 bytes)
	A_nPNEnable : C.short;
	nPNPosition : C.short;
	A_nPNPolarity : C.short;
	nPNNumPulses : C.short;
	A_nPNADCNum : C.short;
	A_fPNHoldingLevel : C.C_float;
	fPNSettlingTime : C.C_float;
	fPNInterpulse : C.C_float;
	sUnused1954 : charArray(0..12);

	--  GROUP #16 - Unused space at end of header block (54 bytes)
	A_nListEnable : C.short;

	nBellEnable : shortArray(0..ABF_BELLCOUNT);
	nBellLocation : shortArray(0..ABF_BELLCOUNT);
	nBellRepetitions : shortArray(0..ABF_BELLCOUNT);

	nLevelHysteresis : C.short;
	lTimeHysteresis : C.long;
	nAllowExternalTags : C.short;

	nLowpassFilterType : charArray(0..ABF_ADCCOUNT);
	nHighpassFilterType : charArray(0..ABF_ADCCOUNT);
	nAverageAlgorithm : C.short;
	fAverageWeighting : C.C_float;
	nUndoPromptStrategy : C.short;
	nTrialTriggerSource : C.short;
	nStatisticsDisplayStrategy : C.short;
	nExternalTagType : C.short;
	lHeaderSize : C.long;
	dFileDuration : C.double;
	nStatisticsClearStrategy : C.short;
	--  Size of v1.5 header = 2048

	--  Extra parameters in v1.6

	--  GROUP #2 - File Structure (8 * 2 = 16 + 100 bytes)
	lDACFilePtr : longArray(0..ABF_WAVEFORMCOUNT);
	lDACFileNumEpisodes : longArray(0..ABF_WAVEFORMCOUNT);
	sUnused2 : charArray(0..10);

	--  GROUP #7 - Multi-channel information (8 * 4 = 32 + 100 bytes)
	fDACCalibrationFactor : floatArray(0..ABF_DACCOUNT);
	fDACCalibrationOffset : floatArray(0..ABF_DACCOUNT);
	sUnused7 : charArray(0..190);

	--  GROUP #9 - Epoch Waveform and Pulses ( 186 * 2 = 368 + 100 bytes)
	nWaveformEnable : shortArray(0..ABF_WAVEFORMCOUNT);
	nWaveformSource : shortArray(0..ABF_WAVEFORMCOUNT);
	nInterEpisodeLevel : shortArray(0..ABF_WAVEFORMCOUNT);
	nEpochType : short2DArray(0..ABF_WAVEFORMCOUNT,0..ABF_EPOCHCOUNT);
	fEpochInitLevel : float2DArray(0..ABF_WAVEFORMCOUNT,0..ABF_EPOCHCOUNT);
	fEpochLevelInc : float2DArray(0..ABF_WAVEFORMCOUNT,0..ABF_EPOCHCOUNT);
	lEpochInitDuration : long2DArray(0..ABF_WAVEFORMCOUNT,0..ABF_EPOCHCOUNT);
	lEpochDurationInc : long2DArray(0..ABF_WAVEFORMCOUNT,0..ABF_EPOCHCOUNT);
	sUnused9 : charArray(0..40);

	--  GROUP #10 - DAC Output File (270 * 2 = 540 + 12 bytes)
	fDACFileScale : floatArray(0..ABF_WAVEFORMCOUNT);
	fDACFileOffset : floatArray(0..ABF_WAVEFORMCOUNT);
	lDACFileEpisodeNum : longArray(0..ABF_WAVEFORMCOUNT);
	nDACFileADCNum : shortArray(0..ABF_WAVEFORMCOUNT);
	sDACFilePath : char2DArray(0..ABF_WAVEFORMCOUNT,0..ABF_PATHLEN);
	sUnused10 : charArray(0..12);

	--  GROUP #11 - Conditioning pulse train (2 * 32 = 64 + 36 bytes)
	nConditEnable : shortArray(0..ABF_WAVEFORMCOUNT);
	lConditNumPulses : longArray(0..ABF_WAVEFORMCOUNT);
	fBaselineDuration : floatArray(0..ABF_WAVEFORMCOUNT);
	fBaselineLevel : floatArray(0..ABF_WAVEFORMCOUNT);
	fStepDuration : floatArray(0..ABF_WAVEFORMCOUNT);
	fStepLevel : floatArray(0..ABF_WAVEFORMCOUNT);
	fPostTrainPeriod : floatArray(0..ABF_WAVEFORMCOUNT);
	fPostTrainLevel : floatArray(0..ABF_WAVEFORMCOUNT);
	nUnused11 : shortArray(0..ABF_WAVEFORMCOUNT);
	sUnused11 : charArray(0..36);

	--  GROUP #12 - Variable parameter user list (260 * 4 = 1040 + 56 bytes)
	nULEnable : shortArray(0..ABF_USERLISTCOUNT);
	nULParamToVary : shortArray(0..ABF_USERLISTCOUNT);
	sULParamValueList : char2DArray(0..ABF_USERLISTCOUNT,0..ABF_USERLISTLEN);
	sUnused12 : charArray(0..56);

	--  GROUP #15 - On-line subtraction (10 * 2 = 20 + 36 bytes)
	nPNEnable : shortArray(0..ABF_WAVEFORMCOUNT);
	nPNPolarity : shortArray(0..ABF_WAVEFORMCOUNT);
	nPNADCNum : shortArray(0..ABF_WAVEFORMCOUNT);
	fPNHoldingLevel : floatArray(0..ABF_WAVEFORMCOUNT);
	sUnused15 : charArray(0..36);

	--  GROUP #6 Environmental Information  ( (16 * 20) + 128 = 448 + 128 bytes)
	nTelegraphEnable : shortArray(0..ABF_ADCCOUNT);
	nTelegraphInstrument : shortArray(0..ABF_ADCCOUNT);
	fTelegraphAdditGain : floatArray(0..ABF_ADCCOUNT);
	fTelegraphFilter : floatArray(0..ABF_ADCCOUNT);
	fTelegraphMembraneCap : floatArray(0..ABF_ADCCOUNT);
	nTelegraphMode : shortArray(0..ABF_ADCCOUNT);
	nManualTelegraphStrategy : shortArray(0..ABF_ADCCOUNT);

	nAutoAnalyseEnable : C.short;
	sAutoAnalysisMacroName : charArray(0..ABF_MACRONAMELEN);
	sProtocolPath : charArray(0..ABF_PATHLEN);

	sFileComment : charArray(0..ABF_FILECOMMENTLEN);
	sUnused6 : charArray(0..128);

	sUnused2048 : charArray(0..734);
end record;  -- Size = 6144
pragma Pack(ABFFileHeader);

-- inline ABFFileHeader::ABFFileHeader()
-- {
--  Set everything to 0.
-- memset( this, 0, sizeof(ABFFileHeader) );
--
--  Set critical parameters so we can determine the version.
-- lFileSignature       = ABF_NATIVESIGNATURE;
-- fFileVersionNumber   = ABF_CURRENTVERSION;
-- fHeaderVersionNumber = ABF_CURRENTVERSION;
-- lHeaderSize          = ABF_HEADERSIZE;
-- }



--
--  Definition of the ABF synch array structure
--

type ABFSynch is record
	lStart : C.long;  -- Start of the episode/event in fSynchTimeUnit units.
	lLength : C.long;  -- Length of the episode/event in multiplexed samples.
end record;  -- Size = 8
pragma Pack(ABFSynch);

--
--  Constants for nTagType in the ABFTag structure.
--
ABF_TIMETAG: Constant := 0;
ABF_COMMENTTAG: Constant := 1;
ABF_EXTERNALTAG: Constant := 2;
ABF_VOICETAG: Constant := 3;

--
--  Definition of the ABF Tag structure
--
type ABFTag is record
	lTagTime : C.long;  -- Time at which the tag was entered in fSynchTimeUnit units.
	sComment : charArray(0..ABF_TAGCOMMENTLEN);  -- Optional tag comment.
	nTagType : C.short;  -- Type of tag ABF_TIMETAG, ABF_COMMENTTAG, ABF_EXTERNALTAG or ABF_VOICETAG.
	nVoiceTagNumber : C.short;  -- If nTagType=ABF_VOICETAG, this is the number of this voice tag.
end record;  -- Size = 64
pragma Pack(ABFTag);

--  Comment inserted for externally acquired tags (expanded with spaces to ABF_TAGCOMMENTLEN).
ABF_EXTERNALTAGCOMMENT : Constant charArray := "<External>";
ABF_VOICETAGCOMMENT    : Constant charArray := "<Voice Tag>";

--
--  Constants for nCompressionType in the ABFVoiceTagInfo structure.
--
ABF_COMPRESSION_NONE: Constant := 0;
ABF_COMPRESSION_PKWARE: Constant := 1;
--  #define ABF_COMPRESSION_MPEG     2

--
--  Definition of the ABFVoiceTagInfo structure.
--
type ABFVoiceTagInfo is record
	lTagNumber : C.long;  -- The tag number that corresponds to this VoiceTag
	lFileOffset : C.long;  -- Offset to this tag within the VoiceTag block
	lUncompressedSize : C.long;  -- Size of the voice tag expanded.
	lCompressedSize : C.long;  -- Compressed size of the tag.
	nCompressionType : C.short;  -- Compression method used.
	nSampleSize : C.short;  -- Size of the samples acquired.
	lSamplesPerSecond : C.long;  -- Rate at which the sound was acquired.
end record;  -- Size 32
pragma Pack(ABFVoiceTagInfo);

--
--  The size of the buffers to be passed to ABFH_GetWaveformVertor
--
ABFH_MAXVECTORS: Constant := 30;


--
--  Error return values that may be returned by the ABFH_xxx functions.
--

ABFH_FIRSTERRORNUMBER: Constant := 2001;
ABFH_EHEADERREAD: Constant := 2001;
ABFH_EHEADERWRITE: Constant := 2002;
ABFH_EINVALIDFILE: Constant := 2003;
ABFH_EUNKNOWNFILETYPE: Constant := 2004;
ABFH_CHANNELNOTSAMPLED: Constant := 2005;
ABFH_EPOCHNOTPRESENT: Constant := 2006;
ABFH_ENOWAVEFORM: Constant := 2007;
ABFH_EDACFILEWAVEFORM: Constant := 2008;
ABFH_ENOMEMORY: Constant := 2009;
ABFH_BADSAMPLEINTERVAL: Constant := 2010;
ABFH_BADSECONDSAMPLEINTERVAL: Constant := 2011;
ABFH_BADSAMPLEINTERVALS: Constant := 2012;
ABFH_ENOCONDITTRAINS: Constant := 2013;
ABFH_EMETADURATION: Constant := 2014;
ABFH_ECONDITNUMPULSES: Constant := 2015;
ABFH_ECONDITBASEDUR: Constant := 2016;
ABFH_ECONDITBASELEVEL: Constant := 2017;
ABFH_ECONDITPOSTTRAINDUR: Constant := 2018;
ABFH_ECONDITPOSTTRAINLEVEL: Constant := 2019;
ABFH_ESTART2START: Constant := 2020;
ABFH_EINACTIVEHOLDING: Constant := 2021;
ABFH_EINVALIDCHARS: Constant := 2022;
ABFH_ENODIG: Constant := 2023;
ABFH_EDIGHOLDLEVEL: Constant := 2024;
ABFH_ENOPNPULSES: Constant := 2025;
ABFH_EPNNUMPULSES: Constant := 2026;
ABFH_ENOEPOCH: Constant := 2027;
ABFH_EEPOCHLEN: Constant := 2028;
ABFH_EEPOCHINITLEVEL: Constant := 2029;
ABFH_EDIGLEVEL: Constant := 2030;
ABFH_ECONDITSTEPDUR: Constant := 2031;
ABFH_ECONDITSTEPLEVEL: Constant := 2032;
ABFH_EINVALIDBINARYCHARS: Constant := 2033;
ABFH_EBADWAVEFORM: Constant := 2034;

end ABF.Header_Struct;