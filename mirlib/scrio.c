/************************************************************************/
/*									*/
/*	A collection of routines to manipulate scratch files.		*/
/*									*/
/*  History:								*/
/*   rjs Dark-ages Original version.					*/
/*   rjs   6nov94  Change item handle to an integer.			*/
/*   rjs  26oct95  Better messages on errors.				*/
/*   pjt  19jun02  MIR4 prototypes                                      */
/*   jwr  05nov04  Change file offsets to type off_t			*/
/*   rjs  03jan05  Include file rationalisation.                        */
/*   pjt  16feb07  Minor doc improvements                               */
/*   pjt  11dec07  More helpful message when scratch files fail         */
/*   rjs  01apr09  Add scrRecSz routine and associated work.		*/
/*   rjs  13may09  Make returned handle always positive (some tasks have*/
/*		   relied on this).                                     */
/*   pjt   7jan09  Merged in previous CARMA changes, long live CVS      */
/************************************************************************/

#if defined(HAVE_CONFIG_H) && HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include "miriad.h"
#include "io.h"


#define MAXITEMS 100
static int number=0;
static int items[MAXITEMS],first;
size_t recsiz[MAXITEMS];

/************************************************************************/
void scropen_c(int *handle)
/**scropen -- Open a scratch file.					*/
/*:scratch-i/o								*/
/*+  FORTRAN call sequence:

	subroutine scropen(tno)
	integer tno

  This opens a scratch file, and readies it for use.
  Scratch files will be removed when they are closed, multiple scratch
  files are allowed, and they always live in the current directory, unless
  the $TMPDIR environment variable points to another directory.

  Output:
    tno		The handle of the scratch file.				*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  int iostat,temp,i;
  char name[32];

/* Initialise the first time through. */

  if(number == 0){
    for(i=0;i<MAXITEMS-1;i++){
      items[i] = i+1;
    }
    items[MAXITEMS-1] = -1;
    first = 0;
  }

/* Get a handle. */

  if(first < 0)bug_c('w',"Exhausted the number of open scratch files");
  *handle = first+1;
  first = items[first];

/* Open the scratch file. */

  (void)sprintf(name,"scratch%d",number++);
  haccess_c(0,&temp,name,"scratch",&iostat);
  if(iostat){
    bug_c(  'w',"Error opening scratch file; check your $TMPDIR");
    bugno_c('f',iostat);
  }
  items[*handle-1] = temp;
  recsiz[*handle-1] = sizeof(float);
}
/************************************************************************/
void scrclose_c(int handle)
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
  handle--;
  hdaccess_c(items[handle],&iostat);
  items[handle] = first;
  first = handle;
  if(iostat){
    bug_c(  'w',"Error closing scratch file; check your $TMPDIR");
    bugno_c('f',iostat);
  }
}
/************************************************************************/
void scrrecsz_c(int handle,size_t recsize)
/**scrrecsz -- Set record size to be used.				*/
/*:scratch-i/o								*/
/*+  FORTRAN call sequence:

	subroutine scrrecsz(tno,recsize)
	integer tno,recsize

  This sets the record size to be used in future access operations.
  Input:
    tno		The handle of the scratch file.
    recsize	The record size (measured in reals).			*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  handle--;
  if(recsize <= 0)bug_c('f',"Invalid record size, in scrrecsz");
  recsiz[handle] = recsize*sizeof(float);
}
/************************************************************************/
void scrread_c(int handle,float *buffer,off_t offset,size_t length)
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
  int myhandle;
  off_t myoff;
  size_t mylen;

  handle--;
  myhandle = items[handle];
  mylen    = recsiz[handle]*length;
  myoff    = recsiz[handle]*offset;

  hreadb_c(myhandle,(char *)buffer,myoff,mylen,&iostat);
  if(iostat){
    bug_c(  'w',"Error reading from scratch file; check your $TMPDIR");
    bugno_c('f',iostat);
  }
}
/************************************************************************/
void scrwrite_c(int handle,Const float *buffer,off_t offset,size_t length)
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
  int myhandle;
  off_t myoff;
  size_t mylen;

  handle--;
  myhandle = items[handle];
  mylen    = recsiz[handle]*length;
  myoff    = recsiz[handle]*offset;

  hwriteb_c(myhandle,(char *)buffer,myoff,mylen,&iostat);
  if(iostat){
    bug_c(  'w',"Error writing to scratch file; check your $TMPDIR");
    bugno_c('f',iostat);
  }
}
