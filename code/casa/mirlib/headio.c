/************************************************************************/
/*									*/
/*	Routines to access "header" variables.				*/
/*									*/
/*--									*/
/* History:								*/
/*  rjs Dark_ages Original version					*/
/*  rjs 23aug89   Fixed char variable overrun problem, in hdprobe.	*/
/*  rjs 12feb90   Added some comments, to appease PJT.			*/
/*  rjs 21feb90   Corrected a comment.					*/
/*  rjs  7mar90   Added hisopen with status='write'			*/
/*  rjs 27apr90   Fixed bug in hdprobe, which got the lengths of items  */
/*		  less than ITEM_HDR_SIZE long wrong.			*/
/*  pjt 19mar91   output double prec variables in -20.10g		*/
/*  rjs 26aug92   Corrected hdprsnt declaration, and the value that	*/
/*		  it returns.						*/
/*  rjs 23feb93   Rename a defined parameter only.			*/
/*  rjs 10aug93   Use hexists in hdprsnt.				*/
/*  rjs  6nov94   Change "item handle" to an integer.			*/
/*  rjs 15may96   Fiddles with roundup macro.				*/
/************************************************************************/

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>

#include "io.h"
#define check(iostat) if(iostat)bugno_c('f',iostat)
#define MAXSIZE 1024
#define MAXLINE 80

int history[MAXOPEN];

void bugno_c(),bug_c();
#define Sprintf (void)sprintf
#define Strcpy  (void)strcpy
void hisopen_c(),hiswrite_c(),hisclose_c(),hdcopy_c(),hdprobe_c();
void rdhdr_c(),rdhdi_c(),rdhdd_c(),rdhda_c(),rdhdc_c();
void wrhdr_c(),wrhdd_c(),wrhdi_c(),wrhda_c(),wrhdc_c();
int hdprsnt_c();

/************************************************************************/
void hisopen_c(tno,status)
int tno;
char *status;
/** hisopen -- Open the history file.					*/
/*& mjs									*/
/*: header-i/o								*/
/*+ FORTRAN call sequence:

	subroutine hisopen(tno,status)
	integer tno
	character status

  This opens the history file, and readies it to be read or written.

  Inputs:
    tno		The handle of the open data set.
    status	Either "read", "write" or "append".			*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  int iostat;
  haccess_c(tno,&history[tno],"history",status,&iostat);	check(iostat);
}
/************************************************************************/
void hiswrite_c(tno,text)
int tno;
char *text;
/** hiswrite -- Write a line of text to the history file.		*/
/*& mjs									*/
/*: header-i/o								*/
/*+ FORTRAN call sequence:

	subroutine hiswrite(tno,line)
	integer tno
	character line*(*)

  This writes a text string to the history file associated with an open
  data set.

  Inputs:
    tno		The handle of the open data set.
    line	The string of text to be written to the history file.	*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  int iostat;
  hwritea_c(history[tno],text,strlen(text)+1,&iostat);		check(iostat);
}
/************************************************************************/
void hisread_c(tno,text,length,eof)
int tno,length,*eof;
char *text;
/** hisread -- Read a line of text from the history file.		*/
/*& mjs									*/
/*: header-i/o								*/
/*+ FORTRAN call sequence:

	subroutine hisread(tno,line,eof)
	integer tno
	character line*(*)
	logical eof

  This reads a line of text from the history file.

  Input:
    tno		The handle of the input data set.
  Output:
    line	The string to receive the read string.
    eof		This logical variable turns true when the end of the
		history file is reached.				*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  int iostat;
  hreada_c(history[tno],text,length,&iostat);
  if(iostat == 0) *eof = FORT_FALSE;
  else if(iostat == -1) *eof = FORT_TRUE;
  else bugno_c('f',iostat);
}
/************************************************************************/
void hisclose_c(tno)
int tno;
/** hisclose -- This closes the history file.				*/
/*& mjs									*/
/*: header-i/o								*/
/*+ FORTRAN call sequence:

	subroutine hisclose(tno
	integer tno

  This closes the history file associated with a particular data set.
  Input:
    tno		The handle of the data set.				*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  int iostat;
  hdaccess_c(history[tno],&iostat);				check(iostat);
}
/************************************************************************/
void wrhdr_c(thandle,keyword,value)
int thandle;
char *keyword;
double value;
/** wrhdr -- Write a real valued header variable.			*/
/*& mjs									*/
/*: header-i/o								*/
/*+ FORTRAN call sequence:

	subroutine wrhdr(tno,keyword,value)
	integer tno
	character keyword*(*)
	real value

  This writes a real-valued header keyword.
  Input:
    tno		Handle of the data set.
    keyword	Name of the keyword to write.
    value	The value of the keyword.				*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  int item;
  float temp;
  int iostat,offset;

  temp = value;
  haccess_c(thandle,&item,keyword,"write",&iostat);		check(iostat);
  hwriteb_c(item,real_item,0,ITEM_HDR_SIZE,&iostat);		check(iostat);
  offset = mroundup(ITEM_HDR_SIZE,H_REAL_SIZE);
  hwriter_c(item,&temp,offset,H_REAL_SIZE,&iostat);		check(iostat);
  hdaccess_c(item,&iostat);					check(iostat);
}
/************************************************************************/
void wrhdd_c(thandle,keyword,value)
int thandle;
char *keyword;
double value;
/** wrhdd -- Write a double precision valued header variable.		*/
/*& mjs									*/
/*: header-i/o								*/
/*+ FORTRAN call sequence:

	subroutine wrhdd(tno,keyword,value)
	integer tno
	character keyword*(*)
	double precision value

  Write the value of a header variable which has a double precision value.

  Input:
    tno		The handle of the data set.
    keyword	Name to the keyword.
    value	The double precision value.				*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  int item;
  int iostat,offset;

  haccess_c(thandle,&item,keyword,"write",&iostat);		check(iostat);
  hwriteb_c(item,dble_item,0,ITEM_HDR_SIZE,&iostat);		check(iostat);
  offset = mroundup(ITEM_HDR_SIZE,H_DBLE_SIZE);
  hwrited_c(item,&value,offset,H_DBLE_SIZE,&iostat);		check(iostat);
  hdaccess_c(item,&iostat);					check(iostat);
}
/************************************************************************/
void wrhdi_c(thandle,keyword,value)
int thandle;
char *keyword;
int value;
/** wrhdi -- Write an integer valued header variable.			*/
/*& mjs									*/
/*: header-i/o								*/
/*+ FORTRAN call sequence:

	subroutine wrhdi(tno,keyword,value)
	integer tno
	character keyword*(*)
	integer value

  Write an integer valued header variable.

  Input:
    tno		The handle of the data set.
    keyword	The name of the header variable.
    value	The integer value of the header variable.		*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  int item;
  int iostat,offset;

  haccess_c(thandle,&item,keyword,"write",&iostat);		check(iostat);
  hwriteb_c(item,int_item,0,ITEM_HDR_SIZE,&iostat);		check(iostat);
  offset = mroundup(ITEM_HDR_SIZE,H_INT_SIZE);
  hwritei_c(item,&value,offset,H_INT_SIZE,&iostat);		check(iostat);
  hdaccess_c(item,&iostat);					check(iostat);
}
/************************************************************************/
void wrhdc_c(thandle,keyword,value)
int thandle;
char *keyword;
float *value;
/** wrhdc -- Write a complex-valued header variable.			*/
/*& mjs									*/
/*: header-i/o								*/
/*+ FORTRAN call sequence:

	subroutine wrhdc(tno,keyword,value)
	integer tno
	character keyword*(*)
	complex value

  Write a complex valued header variable.
  Input:
    tno		The file handle fo the data set.
    keyword	The name of the header variable.
    value	The complex value of the header variable.		*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  int item;
  int iostat,offset;

  haccess_c(thandle,&item,keyword,"write",&iostat);		check(iostat);
  hwriteb_c(item,cmplx_item,0,ITEM_HDR_SIZE,&iostat);		check(iostat);
  offset = mroundup(ITEM_HDR_SIZE,H_CMPLX_SIZE);
  hwritec_c(item,value,offset,H_CMPLX_SIZE,&iostat);		check(iostat);
  hdaccess_c(item,&iostat);					check(iostat);
}
/************************************************************************/
void wrhda_c(thandle,keyword,value)
int thandle;
char *keyword;
char *value;
/** wrhda -- Write a string-valued header variable.			*/
/*& mjs									*/
/*: header-i/o								*/
/*+ FORTRAN call sequence:

	subroutine wrhda(tno,keyword,value)
	integer tno
	character keyword*(*)
	character value*(*)

  Write a string valued header variable.

  Input:
    tno		The file handle of the data set.
    keyword	The name of the header variable.
    value	The value of the header variable.			*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  int item;
  int iostat;

  haccess_c(thandle,&item,keyword,"write",&iostat);		check(iostat);
  hwriteb_c(item,char_item,0,ITEM_HDR_SIZE,&iostat);		check(iostat);
  hwriteb_c(item,value,ITEM_HDR_SIZE,strlen(value),&iostat);	check(iostat);
  hdaccess_c(item,&iostat);					check(iostat);
}
/************************************************************************/
void rdhdr_c(thandle,keyword,value,defval)
int thandle;
char *keyword;
float *value;
double defval;
/** rdhdr -- Read a real-valued header variable.			*/
/*& mjs									*/
/*: header-i/o								*/
/*+ FORTRAN call sequence:

	subroutine rdhdr(tno,keyword,value,default)
	integer tno
	character keyword*(*)
	real value,default

  Read a real valued header variable.

  Input:
    tno		The file handle of the data set.
    keyword	The name of the header variable.
    default	The default value to return, if the header variable
		is not found.
  Output:
    value	The value of the header variable. This will be the default
		value, if the variable is missing from the header.	*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  double dvalue,ddefval;
  ddefval = defval;
  rdhdd_c(thandle,keyword,&dvalue,ddefval);
  *value = dvalue;
}
/************************************************************************/
void rdhdi_c(thandle,keyword,value,defval)
int thandle;
char *keyword;
int *value,defval;
/** rdhdi -- Read an integer-valued header variable.			*/
/*& mjs									*/
/*: header-i/o								*/
/*+ FORTRAN call sequence:

	subroutine rdhdi(tno,keyword,value,default)
	integer tno
	character keyword*(*)
	integer value,default

  Read an integer valued header variable.

  Input:
    tno		The file handle of the data set.
    keyword	The name of the header variable.
    default	The default value to return, if the header variable
		is not found.
  Output:
    value	The value of the header variable. This will be the default
		value, if the variable is missing from the header.	*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  double dvalue,ddefval;
  ddefval = defval;
  rdhdd_c(thandle,keyword,&dvalue,ddefval);
  *value = dvalue;
}
/************************************************************************/
void rdhdd_c(thandle,keyword,value,defval)
int thandle;
char *keyword;
double *value,defval;
/** rdhdd -- Read a double precision-valued header variable.		*/
/*& mjs									*/
/*: header-i/o								*/
/*+ FORTRAN call sequence:

	subroutine rdhdd(tno,keyword,value,default)
	integer tno
	character keyword*(*)
	double precision value,default

  Read a double precision valued header variable.

  Input:
    tno		The file handle of the data set.
    keyword	The name of the header variable.
    default	The default value to return, if the header variable
		is not found.
  Output:
    value	The value of the header variable. This will be the default
		value, if the variable is missing from the header.	*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  int item;
  char s[ITEM_HDR_SIZE];
  int iostat,length,itemp,offset;
  float rtemp;

/* Firstly assume the variable is missing. Try to get it. If successful
   read it. */

  *value = defval;
  haccess_c(thandle,&item,keyword,"read",&iostat);	if(iostat)return;
  length = hsize_c(item);
  if(length >= 0){

/* Determine the type of the value, and convert it to double precision. */

    hreadb_c(item,s,0,ITEM_HDR_SIZE,&iostat);		check(iostat);
    iostat = 0;
    if(      !memcmp(s,int_item, ITEM_HDR_SIZE)){
      offset = mroundup(ITEM_HDR_SIZE,H_INT_SIZE);
      if(offset + H_INT_SIZE == length){
	hreadi_c(item,&itemp,offset,H_INT_SIZE,&iostat);
	*value = itemp;
      }
    } else if(!memcmp(s,real_item,ITEM_HDR_SIZE)){
      offset = mroundup(ITEM_HDR_SIZE,H_REAL_SIZE);
      if(offset + H_REAL_SIZE == length){
        hreadr_c(item,&rtemp,offset,H_REAL_SIZE,&iostat);
        *value = rtemp;
      }
    } else if(!memcmp(s,dble_item,ITEM_HDR_SIZE)){
      offset = mroundup(ITEM_HDR_SIZE,H_DBLE_SIZE);
      if(offset + H_DBLE_SIZE == length){
	hreadd_c(item,value, offset,H_DBLE_SIZE,&iostat);
      }
    }
    check(iostat);
  }
  hdaccess_c(item,&iostat);				check(iostat);
}
/************************************************************************/
void rdhdc_c(thandle,keyword,value,defval)
int thandle;
char *keyword;
float *value,*defval;
/** rdhdc -- Read a complex-valued header variable.			*/
/*& mjs									*/
/*: header-i/o								*/
/*+ FORTRAN call sequence:

	subroutine rdhdc(tno,keyword,value,default)
	integer tno
	character keyword*(*)
	complex value,default

  Read a complex valued header variable.

  Input:
    tno		The file handle of the data set.
    keyword	The name of the header variable.
    default	The default value to return, if the header variable
		is not found.
  Output:
    value	The value of the header variable. This will be the default
		value, if the variable is missing from the header.	*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  int item;
  char s[ITEM_HDR_SIZE];
  int iostat,length,offset;

/* Firstly assume the variable is missing. Try to get it. If successful
   read it. */

  *value = *defval;
  *(value+1) = *(defval+1);
  haccess_c(thandle,&item,keyword,"read",&iostat);	if(iostat)return;
  offset = mroundup(ITEM_HDR_SIZE,H_CMPLX_SIZE);
  length = hsize_c(item) - offset;
  if(length == H_CMPLX_SIZE){
    hreadb_c(item,s,0,ITEM_HDR_SIZE,&iostat);		check(iostat);
    iostat = 0;
    if(!memcmp(s,cmplx_item, ITEM_HDR_SIZE)){
      hreadc_c(item,value,offset,H_CMPLX_SIZE,&iostat);
    }
    check(iostat);
  }
  hdaccess_c(item,&iostat);				check(iostat);
}
/************************************************************************/
void rdhda_c(thandle,keyword,value,defval,len)
int thandle,len;
char *keyword;
char *value,*defval;
/** rdhda -- Read a string-valued header variable.			*/
/*& mjs									*/
/*: header-i/o								*/
/*+ FORTRAN call sequence:

	subroutine rdhda(tno,keyword,value,default)
	integer tno
	character keyword*(*)
	character value*(*),default*(*)

  Read a string valued header variable.

  Input:
    tno		The file handle of the data set.
    keyword	The name of the header variable.
    default	The default value to return, if the header variable
		is not found.
  Output:
    value	The value of the header variable. This will be the default
		value, if the variable is missing from the header.	*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  int item;
  char s[ITEM_HDR_SIZE];
  int iostat,dodef,length;

/* Firstly assume the variable is missing. Try to get it. If successful
   read it. */

  dodef = TRUE;
  haccess_c(thandle,&item,keyword,"read",&iostat);
  if(! iostat) {
    length = min( hsize_c(item) - ITEM_HDR_SIZE, len - 1);
    if(length > 0) {
      hreadb_c(item,s,0,ITEM_HDR_SIZE,&iostat);			check(iostat);
      if(!memcmp(s,char_item,ITEM_HDR_SIZE)){
        hreadb_c(item,value,ITEM_HDR_SIZE,length,&iostat);	check(iostat);
        dodef = FALSE;
      }
    }
    hdaccess_c(item,&iostat);					check(iostat);
  }
  if( dodef ) {
    length = min(strlen(defval),len-1);
    memcpy(value,defval,length);
  }
  *(value+length) = 0;
}
/************************************************************************/
void hdcopy_c(tin,tout,keyword)
int tin,tout;
char *keyword;
/** hdcopy -- Copy a headfer variable from one data set to another.	*/
/*& mjs									*/
/*: header-i/o								*/
/*+ FORTRAN call sequence:

	subroutine hdcopy(tin,tout,keyword)
	integer tin,tout
	character keyword*(*)

  Copy a header item from one data set to another.

  Input:
    tin		File handle of the input data set.
    tout	File handle of the output data set.
    keyword	Name of the header variable to be copied.		*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  char buf[MAXSIZE];
  int item_in,item_out;
  int length,offset,iostat,size;

  haccess_c(tin,&item_in,keyword,"read",&iostat);	if(iostat)return;
  haccess_c(tout,&item_out,keyword,"write",&iostat);	check(iostat);

  size = hsize_c(item_in);
  offset = 0;
  while(offset < size){
    length = min(size - offset, sizeof(buf));
    hreadb_c(item_in,buf,offset,length,&iostat);	check(iostat);
    hwriteb_c(item_out,buf,offset,length,&iostat);	check(iostat);
    offset += length;
  }
  hdaccess_c(item_in,&iostat);				check(iostat);
  hdaccess_c(item_out,&iostat);				check(iostat);
}
/************************************************************************/
int hdprsnt_c(tno,keyword)
int tno;
char *keyword;
/** hdprsnt -- Determine if a header variable is present.		*/
/*& mjs									*/
/*: header-i/o								*/
/*+ FORTRAN call sequence:

	logical function hdprsnt(tno,keyword)
	integer tno
	character keyword*(*)

  Check if a particular header variable is present in a data set.

  Input:
    tno		File handle of the data set to check.
    keyword	Name of the header variable to check for.		*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  if(hexists_c(tno,keyword))return(FORT_TRUE);
  else			    return(FORT_FALSE);
}
/************************************************************************/
void hdprobe_c(tno,keyword,descr,length,type,n)
int tno;
char *keyword,*descr,*type;
int *n,length;
/** hdprobe -- Determine characteristics of a header variable.		*/
/*& mjs									*/
/*: header-i/o								*/
/*+ FORTRAN call sequence:

	subroutine hdprobe(tno,keyword,descr,type,n)
	integer tno
	character keyword*(*),descr*(*),type*(*)
	integer n

  Determine characteristics of a particular header variable.
  Inputs:
    tno		Handle of the data set.
    keyword	Name of the header variable to probe.

  Outputs:
    descr	A formatted version of the item. For single numerics or
		short strings, this is the ascii encoding of the value. For
		large items, this is some message describing the item.
    type	One of:
		  'nonexistent'
		  'integer*2'
		  'integer'
		  'real'
		  'double'
		  'complex'
		  'character'
		  'text'
		  'binary'
    n		Number of elements in the item. Zero implies an error. One
		implies that "descr" is the ascii encoding of the value. */
/*--									*/
/*----------------------------------------------------------------------*/
{
  int item;
  char s[ITEM_HDR_SIZE];
  float rtemp,ctemp[2];
  int iostat,unknown,size,i,itemp,offset;
  double dtemp;
  int2 jtemp;

  haccess_c(tno,&item,keyword,"read",&iostat);
  *n = 0;
  Strcpy(type,"nonexistent");				if(iostat)return;
  size = hsize_c(item);
  unknown = FALSE;
  if(size <= ITEM_HDR_SIZE){
    unknown = TRUE;
    size -= ITEM_HDR_SIZE;
  } else {
    hreadb_c(item,s,0,ITEM_HDR_SIZE,&iostat);			check(iostat);
    if(!memcmp(s,real_item,ITEM_HDR_SIZE)){
      offset = mroundup(ITEM_HDR_SIZE,H_REAL_SIZE);
      size -= offset;
      Strcpy(type,"real");
      *n = size / H_REAL_SIZE;
      if(size % H_REAL_SIZE) unknown = TRUE;
      else if(size == H_REAL_SIZE){
	hreadr_c(item,&rtemp,offset,H_REAL_SIZE,&iostat);	check(iostat);
	Sprintf(descr,"%-14.7g",rtemp);
      }
    } else if(!memcmp(s,int_item,ITEM_HDR_SIZE)){
      offset = mroundup(ITEM_HDR_SIZE,H_INT_SIZE);
      size -= offset;
      Strcpy(type,"integer");
      *n = size / H_INT_SIZE;
      if(size % H_INT_SIZE) unknown = TRUE;
      else if(size == H_INT_SIZE){
	hreadi_c(item,&itemp,offset,H_INT_SIZE,&iostat);	check(iostat);
	Sprintf(descr,"%d",itemp);
      }
    } else if(!memcmp(s,int2_item,ITEM_HDR_SIZE)){
      offset = mroundup(ITEM_HDR_SIZE,H_INT2_SIZE);
      size -= offset;
      Strcpy(type,"integer*2");
      *n = size / H_INT2_SIZE;
      if(size % H_INT2_SIZE) unknown = TRUE;
      else if(size == H_INT2_SIZE){
	hreadj_c(item,&jtemp,offset,H_INT2_SIZE,&iostat);	check(iostat);
	Sprintf(descr,"%d",jtemp);
      }
    } else if(!memcmp(s,dble_item,ITEM_HDR_SIZE)){
      offset = mroundup(ITEM_HDR_SIZE,H_DBLE_SIZE);
      size -= offset;
      Strcpy(type,"double");
      *n = size / H_DBLE_SIZE;
      if(size % H_DBLE_SIZE) unknown = TRUE;
      else if(size == H_DBLE_SIZE){
	hreadd_c(item,&dtemp,offset,H_DBLE_SIZE,&iostat);	check(iostat);
	Sprintf(descr,"%-20.10g",dtemp);
      }
    } else if(!memcmp(s,cmplx_item,ITEM_HDR_SIZE)){
      offset = mroundup(ITEM_HDR_SIZE,H_CMPLX_SIZE);
      size -= offset;
      Strcpy(type,"complex");
      *n = size / H_CMPLX_SIZE;
      if(size % H_CMPLX_SIZE) unknown = TRUE;
      else if(size == H_CMPLX_SIZE){
	hreadr_c(item,ctemp,offset,H_CMPLX_SIZE,&iostat);	check(iostat);
	Sprintf(descr,"(%-14.7g,%-14.7g)",ctemp[0],ctemp[1]);
      }
    } else if(!memcmp(s,char_item,ITEM_HDR_SIZE)){
      offset = ITEM_HDR_SIZE;
      size -= offset;
      size = min(size,length-1);
      *n = 1;
      Strcpy(type,"character");
      hreadb_c(item,descr,ITEM_HDR_SIZE,size,&iostat);		check(iostat);
      *(descr+size) = 0;
    } else if(!memcmp(s,binary_item,ITEM_HDR_SIZE)){
      *n = size;
       Strcpy(type,"binary");
    } else{
      Strcpy(type,"text");
      *n = size + ITEM_HDR_SIZE;
      for(i=0; i < ITEM_HDR_SIZE; i++)
	if(!isspace(*(s+i)) && !isprint(*(s+i)))unknown = TRUE;
    }
  }
  hdaccess_c(item,&iostat);					check(iostat);
  if(unknown){
    Strcpy(type,"unknown");
    *n = size + ITEM_HDR_SIZE;
  }
}
