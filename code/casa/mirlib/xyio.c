/************************************************************************/
/*									*/
/*	Routines to access and manipulate an image.			*/
/*									*/
/*  History:								*/
/*    rjs  Dark-ages Original version.					*/
/*    rjs   6nov89   Neatly handle the case of a non-existent mask file.*/
/*    rjs   7feb90   Added comments, ready to be stripped out by "doc". */
/*    rjs  13jul92   Improved error messages in xyopen, to appease nebk.*/
/*    rjs  23feb93   Include maxdimc.h, which contains maxnax.		*/
/*    rjs   6nov94   Change item handle to an integer.			*/
/*    rjs  27feb96   Added xyflush.					*/
/*    rjs  15mar96   Inlcude an exrta include file.			*/
/*----------------------------------------------------------------------*/

#include <string.h>
#include "maxdimc.h"
#include "io.h"

#define OLD 1
#define NEW 2
#define MK_FLAGS 1
#define MK_RUNS  2
#define check(x) if(x)bugno_c('f',x)
#define CHECK(x,a) if(x) { bug_c('w',((void)sprintf a,message));	\
			   bugno_c('f',x);				\
			 }
#define ERROR(sev,a) bug_c(sev,((void)sprintf a,message))

static char message[132];

static struct { char *mask;
	 int image;
	 int naxis,axes[MAXNAX],offset,mask_exists,image_exists;} images[MAXOPEN];

#define Strcpy (void)strcpy
void bug_c(),bugno_c();
void rdhdi_c(),wrhdi_c();
void mkclose_c(),mkwrite_c();
char *mkopen_c();
int mkread_c();
static void xymkopen_c();
/************************************************************************/
void xyopen_c(thandle,name,status,naxis,axes)
int *thandle,naxis,axes[];
char *name,*status;
/**xyopen -- Open an image file.					*/
/*:image-i/o								*/
/*+ FORTRAN call sequence:

	subroutine xyopen(tno,name,status,naxis,axes)
	integer tno,naxis,axes(naxis)
	character name*(*),status*(*)

  This opens an image file. For an old file, determine the size of
  each axe. For a new file, it writes out this info.

  Input:
    name	The name of the file to be opened.
    status	Either 'old' or 'new'.
    naxis	The maximum number of axes that the calling program can
		handle. For an 'old' file, if the data file has fewer
		than naxis axes, the higher dimensions are treated as having
		only one element. If the data file has more than naxis
		axes, and the higher dimensions are more than 1 element
		deep, xyopen bombs out.
  Input or Output:
    axes	This is input for status='new' and output for status='old'.
		It gives the number of elements along each axis.
  Output:
    tno		The handle of the output file.				*/
/*----------------------------------------------------------------------*/
{
  int iostat,length,access,tno,i,ndim,npix,temp;
  char *mode,naxes[16],s[ITEM_HDR_SIZE];

  if(!strcmp("old",status))	   { access = OLD; mode = "read"; }
  else if(!strcmp("new",status))   { access = NEW; mode = "write"; }
  else
   ERROR('f',(message,"Unrecognised status when opening %s, in XYOPEN",name));

/* Access the image data. */

  hopen_c(&tno,name,status,&iostat);
  CHECK(iostat,(message,"Error opening %s, in XYOPEN",name));
  haccess_c(tno,&(images[tno].image),"image",mode,&iostat);
  CHECK(iostat,(message,"Error accessing pixel data of %s, in XYOPEN",name));

/*----------------------------------------------------------------------*/
/*									*/
/*	Handle an old image. Get number of axes, and then the length	*/
/*	of each axis. Also compute and check that the size of the 	*/
/*	image file looks OK.						*/
/*									*/
/*----------------------------------------------------------------------*/

  if(access == OLD){
    rdhdi_c(tno,"naxis",&ndim,0);
    if(ndim <= 0 || ndim > MAXNAX) 
        ERROR('f',(message,"Bad number of dimensions for %s in XYOPEN",name));

    Strcpy(naxes,"naxis0");
    length = strlen(naxes) - 1;
    npix = 1;
    for(i=0; i<max(ndim,naxis); i++){
      naxes[length] ++;
      if(i < ndim) rdhdi_c(tno,naxes,&temp,0);
      else temp = 1;
      if(temp <= 0)
	ERROR('f',(message,"Bad image dimension for %s, in XYOPEN",name));
      npix = npix * temp;
      if(i < naxis) axes[i] = temp;
      else if(temp > 1)
	ERROR('f',(message,"Too many dimensions for %s, in XYOPEN",name));
    }

/* Check the file size if OK and that it starts with the "real_item"
   sequence. */

    if(hsize_c(images[tno].image) < H_REAL_SIZE*npix+ITEM_HDR_SIZE)
      ERROR('f',(message,"Pixel data for %s appears too small, in XYOPEN",name));
    hreadb_c(images[tno].image,s,0,ITEM_HDR_SIZE,&iostat);
      CHECK(iostat,(message,"Error reading pixel data label for %s, in XYOPEN",name));
    if( memcmp(s,real_item,ITEM_HDR_SIZE) ) 
      ERROR('f',(message,"Bad pixel data label for %s, in XYOPEN",name));

/*----------------------------------------------------------------------*/
/*									*/
/*	A new image. Write out all the axes infomation, and initialise	*/
/*	the file with the "binary item" sequence.			*/
/*									*/
/*----------------------------------------------------------------------*/

  } else {
    wrhdi_c(tno,"naxis",naxis);
    Strcpy(naxes,"naxis0");
    length = strlen(naxes) - 1;
    for(i=0; i < naxis; i++){
      naxes[length] ++;
      wrhdi_c(tno,naxes,axes[i]);
    }
    hwriteb_c(images[tno].image,real_item,0,ITEM_HDR_SIZE,&iostat);
    CHECK(iostat,(message,"Error writing pixel data label for %s, in XYOPEN",name));
  }

/* Common to old and new. Copy the dimension info to the local description. */

  images[tno].offset = 0;
  images[tno].naxis  = naxis;
  for(i=0; i < naxis; i++) images[tno].axes[i] = axes[i];
  for(i = naxis; i < MAXNAX; i++) images[tno].axes[i] = 1;
  images[tno].mask = NULL;
  images[tno].image_exists = TRUE;
  images[tno].mask_exists = TRUE;
  *thandle = tno;
}
/************************************************************************/
void xyflush_c(thandle)
int thandle;
/**xyflush -- Flush out any image changes to disk.			*/
/*:image-i/o								*/
/*+ FORTRAN call sequence:

	subroutine xyflush(tno)
	implicit none

This flushes any changes to an image to disk.

  Input:
    tno		The handle of the image file.				*/
/*----------------------------------------------------------------------*/
{
  int iostat,i,offset,nbytes,length;
  float buf[MAXDIM];

/* Simply flush out the mask. */

  if(images[thandle].mask != NULL) mkflush_c(images[thandle].mask);

/* If its a new file, and not all the pixels have yet been written,
   write zero pixels. First determine the proper size. */

  nbytes = H_REAL_SIZE;
  for(i=0; i < images[thandle].naxis; i++) nbytes *= images[thandle].axes[i];
  nbytes += ITEM_HDR_SIZE;
  offset = hsize_c(images[thandle].image);

/* Determine the number of bytes to pad, and then pad it. */

  nbytes -= offset;
  if(nbytes > 0)for(i=0; i < MAXDIM; i++)buf[i] = 0.0;
  while(nbytes > 0){
    length = MAXDIM*H_REAL_SIZE;
    if(length > nbytes) length = nbytes;
    hwriter_c(images[thandle].image,buf,offset,length,&iostat);
    CHECK(iostat,(message,"Error allocating space for image"));
    offset += length;
    nbytes -= length;
  }

/* Do it all now. */

  hflush_c(thandle,&iostat); 			check(iostat);
}
/************************************************************************/
void xyclose_c(thandle)
int thandle;
/**xyclose -- Close up an image file.					*/
/*:image-i/o								*/
/*+ FORTRAN call sequence:

	subroutine xyclose(tno)
	integer tno

  This closes an image file.

  Input:
    tno		The handle of the image file.				*/
/*----------------------------------------------------------------------*/
{
  int iostat;

  hdaccess_c(images[thandle].image,&iostat);			check(iostat);
  if(images[thandle].mask != NULL) mkclose_c(images[thandle].mask);
  hclose_c(thandle);
}
/************************************************************************/
void xyread_c(thandle,index,array)
int thandle,index;
float array[];
/**xyread -- Read a row from an image.					*/
/*:image-i/o								*/
/*+ FORTRAN call sequence:

	subroutine xyread(tno,index,array)
	integer tno,index
	real array(*)

  This reads a single row from an image. This accesses the plane given
  by the last call to xysetpl.

  Input:
    tno		The image file handle, returned by xyopen.
    index	The row number to read. This varies from 1 to NAXIS2.
  Output:
    array	The read row. NAXIS1 elements are returned.		*/
/*----------------------------------------------------------------------*/
{
  int offset,length,iostat;

  length = H_REAL_SIZE * images[thandle].axes[0];
  offset = H_REAL_SIZE * images[thandle].offset + (index-1) * length +
			ITEM_HDR_SIZE;
  hreadr_c(images[thandle].image,array,offset,length,&iostat);
  check(iostat);
}
/************************************************************************/
void xywrite_c(thandle,index,array)
int thandle,index;
float array[];
/**xywrite -- Write a row to an image.					*/
/*:image-i/o								*/
/*+ FORTRAN call sequence:

	subroutine xywrite(tno,index,array)
	integer tno,index
	real array(*)

  This writes a single row to an image. This accesses the plane given
  by the last call to xysetpl.

  Input:
    tno		The image file handle, returned by xyopen.
    index	The row number to write. This varies from 1 to NAXIS2.
    array	The read row. NAXIS1 elements are written.		*/
/*----------------------------------------------------------------------*/
{
  int offset,length,iostat;

  length = H_REAL_SIZE * images[thandle].axes[0];
  offset = H_REAL_SIZE * images[thandle].offset + (index-1) * length +
		ITEM_HDR_SIZE;
  hwriter_c(images[thandle].image,array,offset,length,&iostat);
  check(iostat);
}
/************************************************************************/
void xymkrd_c(thandle,index,runs,n,nread)
int thandle,index,runs[],n,*nread;
/**xymkrd -- Read the masking information for an image (runs format).	*/
/*:image-i/o								*/
/*+ FORTRAN call sequence:

	subroutine xymkrd(tno,index,runs,n,nread)
	integer tno,index,n,nread
	integer runs(n)

  This reads the masking information associated with a row of an image,
  and returns it in "runs" format. 

  Input:
    tnoe	The handle associated with the image.
    index	The index of the row to determine mask info about. The
		last call to xysetpl determines which plane to access.
    n		The size of the array to receive the mask info.
  Output:
    runs	The mask info, in "runs" form. If "i" varies from 1 to
		nread/2, then pixels runs(2*i-1) to runs(2*i) are
		good, whereas pixels runs(2*i) to runs(2*i+1) are bad.
    nread	The number of "runs" read.				*/
/*----------------------------------------------------------------------*/
{
  int offset,length;
  if(images[thandle].mask == NULL && images[thandle].mask_exists)
						xymkopen_c(thandle,OLD);
  if(images[thandle].mask_exists){
    length = images[thandle].axes[0];
    offset = images[thandle].offset + (index-1) * length;
    *nread = mkread_c(images[thandle].mask,MK_RUNS,runs,offset,length,n);
  } else {
    if(n < 2) 
        bug_c('f',"xymkrd_c: Runs array overflow");
    runs[0] = 1;
    runs[1] = images[thandle].axes[0];
    *nread = 2;
  }
}
/************************************************************************/
void xymkwr_c(thandle,index,runs,n)
int thandle,index,n,runs[];
/**xymkwr -- write image masking information (runs format).		*/
/*:image-i/o								*/
/*+ FORTRAN call sequence:

	subroutine xymkwr(tno,index,runs,n)
	integer tno,index,n
	integer runs(n)

  This writes out the masking information associated with a row of an image.
  This information is passes in in "runs" format. 

  Input:
    tnoe	The handle associated with the image.
    index	The index of the row to determine mask info about. The
		last call to xysetpl determines which plane to access.
    n		The size of the array containing the mask info.
    runs	The mask info, in "runs" form. If "i" varies from 1 to
		nread/2, then pixels runs(2*i-1) to runs(2*i) are
		good, whereas pixels runs(2*i) to runs(2*i+1) are bad.	*/
/*----------------------------------------------------------------------*/
{
  int offset,length;
  if(images[thandle].mask == NULL) xymkopen_c(thandle,NEW);
  if(images[thandle].mask == NULL) 
    bug_c('f',"xymkwr_c: Error writing to image mask file");
  length = images[thandle].axes[0];
  offset = images[thandle].offset + (index-1) * length;
  mkwrite_c(images[thandle].mask,MK_RUNS,runs,offset,length,n);
}
/************************************************************************/
void xyflgwr_c(thandle,index,flags)
int thandle,index,flags[];
/**xyflgwr -- Write image masking information (flags format).		*/
/*:image-i/o								*/
/*+ FORTRAN call sequence:

	subroutine xyflgwr(tno,index,flags)
	integer tno,index
	logical flags(*)

  This writes image mask information. It is the counterpart of xywrite.
  Input:
    tno		Handle of the image file.
    index	The row in a plane to write out. This varies between 1 and
		NAXIS2. See xysetpl to set the which plane is to be
		accessed.
    flags	A logical array of NAXIS1 elements. A true value indicates
		that the pixel is good.					*/
/*----------------------------------------------------------------------*/
{
  int offset,length;
  if(images[thandle].mask == NULL)xymkopen_c(thandle,NEW);
  if(images[thandle].mask == NULL) 
    bug_c('f',"xyflgwr_c: Error writing to image mask file");
  length = images[thandle].axes[0];
  offset = images[thandle].offset + (index-1) * length;
  mkwrite_c(images[thandle].mask,MK_FLAGS,flags,offset,length,length);
}
/************************************************************************/
void xyflgrd_c(thandle,index,flags)
int thandle,index,flags[];
/**xyflgrd -- Read image masking information (flags format).		*/
/*:image-i/o								*/
/*+ FORTRAN call sequence:

	subroutine xyflgrd(tno,index,flags)
	integer tno,index
	logical flags(*)

  This reads image mask information. It is the counterpart of xyread.
  Input:
    tno		Handle of the image file.
    index	The row in a plane to read in. This varies betwen 1 and
		NAXIS2. Set xysetpl to change the plane being accessed.
  Output:
    flags	A logical array of NAXIS1 elements. A true value indicates
		that the pixel is good.					*/
/*----------------------------------------------------------------------*/
{
  int offset,length,n,i;
  if(images[thandle].mask == NULL && images[thandle].mask_exists)
						xymkopen_c(thandle,OLD);
  if(images[thandle].mask_exists){
    length = images[thandle].axes[0];
    offset = images[thandle].offset + (index-1) * length;
    n = mkread_c(images[thandle].mask,MK_FLAGS,flags,offset,length,length);
  } else {
    n = images[thandle].axes[0];
    for(i=0; i<n; i++) *flags++ = FORT_TRUE;
  }
}
/************************************************************************/
static void xymkopen_c(thandle,mode)
int thandle,mode;
/*
  This opens the masking file.

  Input:
    thandle	The handle of the image data-set.
    mode	Either OLD or NEW.
------------------------------------------------------------------------*/
{
  images[thandle].mask = mkopen_c(thandle,"mask",(mode == OLD ? "old" : "new"));
  if(images[thandle].mask == NULL) images[thandle].mask_exists = FALSE;
}
/************************************************************************/
void xysetpl_c(thandle,naxis,axes)
int thandle,naxis,axes[];
/**xysetpl -- Set which plane of a cube is to be accessed.		*/
/*:image-i/o								*/
/*+ FORTRAN call sequence:

	subroutine xysetpl(tno,naxis,nsize)
	integer tno,naxis,nsize(naxis)

  This sets up which plane of a cube is to be accessed.

  Input:
    tno		Handle of the image file.
    naxis	Size of the "nsize" array.
    nsize	This gives the indices, along the 3rd, 4th, 5th, etc
		dimensions, of the plane that is to be accessed. nsize(1)
		corresponds to the index along the 3rd dimension.	*/
/*----------------------------------------------------------------------*/
{
  int size,i;

  if(naxis+2 > MAXNAX)
     bug_c('f',"xysetpl_c: Too many dimensions");
  size = 0;
  for(i=naxis-1; i >= 0; i--){
    if(axes[i] < 1 || axes[i] > images[thandle].axes[i+2])
	bug_c('f',"Dimension error in XYSETPL");
    size = ( size + axes[i] - 1) * images[thandle].axes[i+1];
  }
  images[thandle].offset = size * images[thandle].axes[0];
}
