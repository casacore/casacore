/************************************************************************/
/*									*/
/*	A collection of routines to manipulate scratch files.		*/
/*									*/
/*  History:								*/
/*   rjs Dark-ages Original version.					*/
/*   rjs   6nov94  Change item handle to an integer.			*/
/*   rjs  26oct95  Better messages on errors.				*/
/************************************************************************/

#include "io.h"

char *sprintf();

static int number=0;
/************************************************************************/
void scropen_c(handle)
int *handle;
/**scropen -- Open a scratch file.					*/
/*:scratch-i/o								*/
/*+  FORTRAN call sequence:

	subroutine scropen(tno)
	integer tno

  This opens a scratch file, and readies it for use.
  Output:
    tno		The handle of the scratch file.				*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  int iostat;
  char name[32];

  (void)sprintf(name,"scratch%d",number++);
  haccess_c(0,handle,name,"scratch",&iostat);
  if(iostat){
    bug_c(  'w',"Error opening scratch file");
    bugno_c('f',iostat);
  }
}
/************************************************************************/
void scrclose_c(handle)
int handle;
/**scrclose -- Close and delete a scratch file.				*/
/*:scratch-i/o								*/
/*+  FORTRAN call sequence:

	subroutine scrclose(tno)
	integer tno

  This closes and deletes a scratch file. The scratch file cannot be
  accessed again, after it is closed.
  Input:
    tno		The handle of the scratch file.				*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  int iostat;

  hdaccess_c(handle,&iostat);
  if(iostat){
    bug_c(  'w',"Error closing scratch file");
    bugno_c('f',iostat);
  }
}
/************************************************************************/
void scrread_c(handle,buffer,offset,length)
int handle,offset,length;
float *buffer;
/**scrread -- Read real data from a scratch file.			*/
/*:scratch-i/o								*/
/*+  FORTRAN call sequence:

	subroutine scrread(tno,buf,offset,length)
	integer tno,offset,length
	real buf(length)

  This reads real data from the scratch file.
  Input:
    tno		The handle of the scratch file.
    offset	The offset (measured in reals) into the scratch file
		to read. The first real has offset 0.
    length	The number of reals to read.
  Output:
    buf		The returned data.					*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  int iostat;

  hreadb_c(handle,(char *)buffer,
    sizeof(float)*offset,sizeof(float)*length,&iostat);
  if(iostat){
    bug_c(  'w',"Error reading from scratch file");
    bugno_c('f',iostat);
  }
}
/************************************************************************/
void scrwrite_c(handle,buffer,offset,length)
int handle,offset,length;
float *buffer;
/**scrwrite -- Write real data to the scratch file.			*/
/*:scratch-i/o								*/
/*+  FORTRAN call sequence:

	subroutine scrwrite(tno,buf,offset,length)
	integer tno,offset,length
	real buf(length)

  This writes real data to the scratch file.
  Input:
    tno		The handle of the scratch file.
    offset	The offset (measured in reals) into the scratch file
		to write. The first real has offset 0.
    length	The number of reals to write.
    buf		The data to write.					*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  int iostat;

  hwriteb_c(handle,(char *)buffer,
    sizeof(float)*offset,sizeof(float)*length,&iostat);
  if(iostat){
    bug_c(  'w',"Error writing to scratch file");
    bugno_c('f',iostat);
  }
}
