/***************************************************************************
                          abffile.h  -  description
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

#include "compdefs.h"

// Error numbers for routines in this module.
// Only positive numbers are used.
// Use Axon defined numbers to match

#define ABF_SUCCESS                 0
#define ABF_EUNKNOWNFILETYPE        1001
#define ABF_EBADFILEINDEX           1002
#define ABF_TOOMANYFILESOPEN        1003
#define ABF_EOPENFILE               1004
#define ABF_EBADPARAMETERS          1005
#define ABF_EREADDATA               1006
#define ABF_OUTOFMEMORY             1008
#define ABF_EREADSYNCH              1009
#define ABF_EBADSYNCH               1010
#define ABF_EEPISODERANGE           1011
#define ABF_EINVALIDCHANNEL         1012
#define ABF_EEPISODESIZE            1013
#define ABF_EREADONLYFILE           1014
#define ABF_EDISKFULL               1015
#define ABF_ENOTAGS                 1016
#define ABF_EREADTAG                1017
#define ABF_ENOSYNCHPRESENT         1018
#define ABF_EREADDACEPISODE         1019
#define ABF_ENOWAVEFORM             1020
#define ABF_EBADWAVEFORM            1021
#define ABF_BADMATHCHANNEL          1022
#define ABF_BADTEMPFILE             1023
#define ABF_NODOSFILEHANDLES        1025
#define ABF_ENOSCOPESPRESENT        1026
#define ABF_EREADSCOPECONFIG        1027
#define ABF_EBADCRC                 1028
#define ABF_ENOCOMPRESSION          1029
#define ABF_EREADDELTA              1030
#define ABF_ENODELTAS               1031
#define ABF_EBADDELTAID             1032
#define ABF_EWRITEONLYFILE          1033
#define ABF_ENOSTATISTICSCONFIG     1034
#define ABF_EREADSTATISTICSCONFIG   1035
#define ABF_EWRITERAWDATAFILE       1036
#define ABF_EWRITEMATHCHANNEL       1037


//had to define this struct so this f$%king c can return pointer to an allocated buffer
struct ABFBufPtr {float* p;};


// this proc reads from opened (with ABFOpenGapFree) file nSamples for specified nChanl
//returns 0 if successful.
//pnSamples contains (pointer to) actual number of read samples
//(if less then requested were read). No seek is performed before the read.
// pBuf should be allocated before calling this procedure!
int ABFReadRawChannel(FileRec *F,ABFFileHeader *pH,int nChanl,void* pBuf,unsigned long *pnSamples);

//this function reads channel data (via ReadRaw) then scales it
//allocates necessary buffer and returns pointer to it
//it DOES NOT seek to the beginning of data, relying on ABFOpens, so that 
//consequtive reads will read data continuously.
int ABFReadChannel(FileRec *F,ABFFileHeader *pH,
                   int nChanl, ABFBufPtr* pBuf, unsigned long *pnSamples);

/* could not decide on passing multiple arrays
//this function opens specified file and reads all channels stored in it (file must contain gap-free mode acquisition)
//it allocates buffers for the arrays, reads and scales data to UserUnits
//(so that array of float is returned)
int ABFReadAll(char* fName,ABFFileHeader *pH,UINT& nChanls,unsigned long& nSamples, ABFBufPtr* pBuf[]);
*/

int ABFReadBlock(FileRec *F, ABFFileHeader *pH, float* pBuf, UINT pos, UINT* count);

//converts data to the specified current units
//presently used units are taken from associated header structure
//this header GETS MODIFIED! to indicate new units used.
int ABFSetCurrentUnits(ABFFileHeader *pH, ABFBufPtr* pBuf, int nChanl, unsigned long nSamples, int newcUnits);
