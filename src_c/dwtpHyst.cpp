/***************************************************************************
                          dwt-pHyst.cpp  -  description
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
Adds dwt's w calculated avg suction from .dwtp file to 
the dwt vs suct vs counts hystogram.
Reads hystogram from file and then writes to another.
Hystogram file is read and closed, so the output can go into the same file.
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define _GNU_SOURCE
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <stream.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_histogram2d.h>
#include <math.h>


void printHelp()
{
cout <<
"update dwt-p-count hystogram \n" <<
"usage: dwt-pHyst [options] hyst-file dwtp-file \n\n" <<
"options:\n"<<
"-h     print help and exit\n"<<
"-q     quiet execution \n"<<
"-o fn  output goes into file fn (otherwise to hyst-file)\n"<<
"-d n	# of dwt intervals (defaults to 13)\n"<<
"-p n	# of suction intervals (defaults to 7)"<<
"-n		use normal (non-logarithmic) scale for dwt intervals\n\n"<<
"-a	f	lower dwt boundary\n"<<
"-b f	upper dwt boundary\n"<<
"-x	f	lower (signed) suction boundary\n"<<
"-y f 	upper suction boundary\n";
}


//define struct to be read from dwtp file 
struct dwtpLine {
	unsigned int level;
	float dwt;
	float avgP;
	float stdDev;
	dwtpLine* next;
};


int main(int argc, char *argv[])
{
	const char cHystHeader[10] = "dwtpHyst\n";
	//lets parse the input
	int beQuiet=0;
	char* outFName=NULL;
	char* fnHyst=NULL;char* fnDwtp=NULL;
	unsigned int nP=7;unsigned int nD=13;//# of suction and dwt intervals
	int logDwts=1;//if dwt's are binned on a log scale
	float dwtA=0; float dwtB=0;
	float pA=0; float pB=0;//dwt and suction boundaries

	int c;
	while ((c = getopt (argc, argv, "hqo:d:p:na:b:x:y:")) != -1) switch (c) {
		case 'h': printHelp();exit(EXIT_SUCCESS);break;
		case 'q': beQuiet = 1;break;
		case 'o': outFName = optarg;break;
		case 'p': nP=atoi(optarg);break;
		case 'd': nD=atoi(optarg);break;
		case 'n': logDwts=0;break;
		case 'a': dwtA=atof(optarg);break;
		case 'b': dwtB=atof(optarg);break;
		case 'x': pA=atof(optarg);break;
		case 'y': pB=atof(optarg);break;
    };

	if (optind<argc) fnHyst=argv[optind];
	else {printf("you must specify hystogram filename!\n"); exit(1);};
	if (optind<argc-1) fnDwtp=argv[optind+1];
	else {printf("you must specify dwt filename!\n");exit(1);};

	
	//lets check if we have ranges specified and calc them otherwise
	//open and read dwtp file here.	
	dwtpLine* dwtpEntry = (dwtpLine*)malloc(sizeof(dwtpLine));
	dwtpLine* dwtpRoot;
	if (dwtpEntry) dwtpRoot=dwtpEntry; else GSL_ERROR("could not malloc dwtp entry",GSL_ENOMEM);


	FILE* Fdwtp;
	if ( Fdwtp = fopen(fnDwtp,"r") ){	
		char* dwtHead=NULL;size_t dwtHeadLen=0;
		ssize_t numread=getline(&dwtHead,&dwtHeadLen,Fdwtp);//get rid of header
			
		int si;
		unsigned int lvl;float dwt,avg,stddev;
		while ((si=fscanf(Fdwtp,"%u  %e  %e  %e\n",&lvl,&dwt,&avg,&stddev)) && (si!=EOF) ){
			dwtpEntry->next = (dwtpLine*)malloc(sizeof(dwtpLine));
			if (dwtpEntry->next) {
				dwtpEntry=dwtpEntry->next;
				dwtpEntry->level=lvl;
				dwtpEntry->dwt=dwt;
				dwtpEntry->avgP=avg;
				dwtpEntry->stdDev=stddev;
			} else GSL_ERROR("could not malloc dwtp entry",GSL_ENOMEM);
		}
		dwtpEntry->next=NULL;
		fclose(Fdwtp);
	}
	else  GSL_ERROR("could not open dwtp file!",GSL_EFAILED);

	if (dwtA==dwtB==0){//dwt range is not specified, need to calc min/max
		dwtpEntry=dwtpRoot->next;
		dwtA=dwtB=dwtpEntry->dwt;
		while (dwtpEntry==NULL) {
			if (dwtpEntry->dwt < dwtA) dwtA=dwtpEntry->dwt;
			if (dwtpEntry->dwt > dwtB) dwtB=dwtpEntry->dwt;
			dwtpEntry=dwtpEntry->next;
		};
	}
	if (pA==pB==0){//need to set suction range
		dwtpEntry=dwtpRoot->next;
		pA=pB=dwtpEntry->avgP;
		while (dwtpEntry!=NULL) {
			if (dwtpEntry->avgP < pA) pA=dwtpEntry->avgP;
			if (dwtpEntry->avgP > pB) pB=dwtpEntry->avgP;
			dwtpEntry=dwtpEntry->next;
		};
	}

	
	//now initialize/read the hystogram
	gsl_histogram2d* Hyst;
	
	//try to open the file
	if (FILE* Fhyst = fopen(fnHyst,"r")){ //non-NULL pointer, file probably exists
		char hystHeader[10];
		fread(hystHeader,sizeof(cHystHeader),1,Fhyst);
		if (hystHeader!=cHystHeader) {printf("hysto-file header mismatch!\n");exit(EXIT_FAILURE);};
		
		//gsl code does not store hyst sizes, so I have to do it myself..
		if (!fread(&nD,sizeof(nD),1,Fhyst)) GSL_ERROR("could not read dwt hystogram size",GSL_EFAILED);
		if (!fread(&nP,sizeof(nP),1,Fhyst)) GSL_ERROR("could not read P hystogram size",GSL_EFAILED);
		
		//time to alloc the hystogram
		//note the order: nP, nD - this way hyst is [internally] slices per =P
		if (Hyst = gsl_histogram2d_alloc(nP,nD)) GSL_ERROR("could not allocate hystogram!",GSL_ENOMEM);
		if (gsl_histogram2d_fread(Fhyst,Hyst)) GSL_ERROR("could not read hystogram!",GSL_EFAILED);

		//close hyst file here
		fclose(Fhyst);
		
	}else{ //file does not exist, need to create a fresh hystogram
		Hyst = gsl_histogram2d_alloc(nP,nD);

		//lets set ranges; need to check if normal scale for dwts is requested
		double dwtRange[nD+1];
		double pRange[nP+1];
		if (logDwts) for (int i=0;i<=nD;i++) dwtRange[i]=dwtA* pow(dwtB/dwtA,float(i)/nD);
		else for (int i=0;i<=nD;i++) dwtRange[i]=dwtA + (dwtB - dwtA)*float(i)/nD;
		for (int i=0;i<=nP;pRange[i++]=pA+(pB-pA)*float(i)/nP);

		gsl_histogram2d_set_ranges(Hyst,pRange,nP+1,dwtRange,nD+1);
	};
	
	
	//so lets update the hystogram finally
	dwtpEntry=dwtpRoot->next;
	while (dwtpEntry!=NULL) {
		gsl_histogram2d_increment(Hyst,dwtpEntry->avgP,dwtpEntry->dwt);
		dwtpEntry=dwtpEntry->next;
	};

	//write new hystogram
	if (FILE* Fhyst = fopen(fnHyst,"w")){
		fwrite(cHystHeader,sizeof(cHystHeader),1,Fhyst);
		if (!fwrite(&nD,sizeof(nD),1,Fhyst)) GSL_ERROR("could not write dwt hystogram size",GSL_EFAILED);
		if (!fwrite(&nP,sizeof(nP),1,Fhyst)) GSL_ERROR("could not write P hystogram size",GSL_EFAILED);
		if (gsl_histogram2d_fwrite(Fhyst,Hyst)) GSL_ERROR("could not write hystogram!",GSL_EFAILED);
		fclose(Fhyst);
	}
	else GSL_ERROR("could not open hystogram file for writing!",GSL_EFAILED);
}
