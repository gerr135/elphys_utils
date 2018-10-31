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

//usage: abfinfo [file.abf [out-file]]


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

//#include <iostream.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

#include "abfheader.h"
#include "abffile.h"

int main(int argc, char *argv[])
{
  char* fName=NULL;char* outFName=NULL;

  if (argc-1 > 0) fName=argv[1]; else fName=NULL;
  if (argc-2 > 0) outFName=argv[2]; else outFName=NULL;
  
  //lets open the file
  FileRec F;
  ABFFileHeader pH;
  if (ABFFileOpen(fName,&F,&pH)) {printf("cannot open file %s\n",fName);exit(1);};

  //close file and unnecessary buffers
  fclose(F.h);
	
  //lets see if we write to stdout and open proper file:
  FILE* Fout;
  if ((outFName==NULL)||(outFName=="stdout")) Fout=stdout; else Fout=fopen(outFName,"r");
  ABFPrintHeaderInfo(Fout,&pH);
  fclose(Fout);
	
  return EXIT_SUCCESS;
}
