/************************************************************************/
/*									*/
/*	A package of routines to read and write masks (bitmaps)		*/
/*	These are used by MIRIAD for data flagging and blanking.	*/
/*									*/
/*	Masks are implemented as integer data items. The 32nd		*/
/*	bit in the integer is not used, as this could cause		*/
/*	portability problems. As it is, this software assumes that	*/
/*	the host integer is at least 32 bits long.			*/
/*									*/
/*  History:								*/
/*    rjs  Dark-ages Original version.					*/
/*    rjs   6nov89   Does not abort if the mask file is missing.	*/
/*    rjs   3mar93   Make mkflush a user-callable routine.		*/
/*    rjs  23dec93   Do not open in read/write mode unless necessary.	*/
/*    rjs   6nov94   Change item handle to an integer.			*/
/************************************************************************/

#define BUG(sev,a)   bug_c(sev,a)
#define ERROR(sev,a) bug_c(sev,((void)sprintf a,message))
#define CHECK(x) if(x) bugno_c('f',x)

#define private static

char *sprintf();
private void mkfill();
void mkflush_c();

static char message[128];

#define BITS_PER_INT 31

int bits[BITS_PER_INT] = {
		0x00000001,0x00000002,0x00000004,0x00000008,
		0x00000010,0x00000020,0x00000040,0x00000080,
		0x00000100,0x00000200,0x00000400,0x00000800,
		0x00001000,0x00002000,0x00004000,0x00008000,
		0x00010000,0x00020000,0x00040000,0x00080000,
		0x00100000,0x00200000,0x00400000,0x00800000,
		0x01000000,0x02000000,0x04000000,0x08000000,
		0x10000000,0x20000000,0x40000000};

int masks[BITS_PER_INT+1]={
		0x00000000,0x00000001,0x00000003,0x00000007,0x0000000F,
			   0x0000001F,0x0000003F,0x0000007F,0x000000FF,
			   0x000001FF,0x000003FF,0x000007FF,0x00000FFF,
			   0x00001FFF,0x00003FFF,0x00007FFF,0x0000FFFF,
			   0x0001FFFF,0x0003FFFF,0x0007FFFF,0x000FFFFF,
			   0x001FFFFF,0x003FFFFF,0x007FFFFF,0x00FFFFFF,
			   0x01FFFFFF,0x03FFFFFF,0x07FFFFFF,0x0FFFFFFF,
			   0x1FFFFFFF,0x3FFFFFFF,0x7FFFFFFF};

#include "io.h"

char *malloc();

#define MK_FLAGS 1
#define MK_RUNS 2
#define BUFFERSIZE 128
#define OFFSET (((ITEM_HDR_SIZE-1)/H_INT_SIZE + 1)*BITS_PER_INT)
typedef struct {int item;
		int buf[BUFFERSIZE],offset,length,size,modified,rdonly,tno;
		char name[32];
		} MASK_INFO;

/************************************************************************/
char *mkopen_c(tno,name,status)
char *name,*status;
int tno;
/*
  This opens a mask item, and readies it for access.

  Inputs:
    tno		The handle of the data set containing the item.
    name	Name of the item (something like "mask" or "flags").
    status	"old" or "new".

  Output:
    mkopen_c	This is a handle used in subsequent calls to the maskio
		routines. A NULL indicates that an error was encountered.

------------------------------------------------------------------------*/
{
  MASK_INFO *mask;
  int iostat;
  char s[ITEM_HDR_SIZE];

  mask = (MASK_INFO *)malloc((unsigned int)sizeof(MASK_INFO));

/* The case of an old mask file. Perform a number of checks to make sure
   the file looks OK. */

  if(!strcmp("old",status)) {
    haccess_c(tno,&(mask->item),name,"read",&iostat);
    if(iostat) { free((char *)mask); return(NULL); }
    mask->size = hsize_c(mask->item);
    if(mask->size <= H_INT_SIZE * (OFFSET/BITS_PER_INT))
      ERROR('f',(message,"Mask file %s appears bad",name));
    hreadb_c(mask->item,s,0,ITEM_HDR_SIZE,&iostat);		CHECK(iostat);
    if(memcmp(s,int_item,ITEM_HDR_SIZE)) 
      ERROR('f',(message,"Mask file %s is not integer valued",name));
    mask->rdonly   = TRUE;

/* The case of a new masl file. Create it and make it look nice. */

  } else if(!strcmp("new",status)) {
    haccess_c(tno,&(mask->item),name,"write",&iostat);		CHECK(iostat);
    hwriteb_c(mask->item,int_item,0,ITEM_HDR_SIZE,&iostat);	CHECK(iostat);
    mask->size = OFFSET/BITS_PER_INT * H_INT_SIZE;
    mask->rdonly   = FALSE;
  } else ERROR('f',(message,"Unrecognised status %s in MKOPEN",status));

/* Common to both old and new mask files. Initialise the structure
   describing the file. */

  mask->size = (mask->size/H_INT_SIZE) * BITS_PER_INT;
  mask->offset = -BUFFERSIZE*BITS_PER_INT;
  mask->length = 0;
  mask->modified = FALSE;
  mask->tno = tno;
  strcpy(mask->name,name);

  return((char *)mask);
}
/************************************************************************/
void mkclose_c(handle)
char *handle;
/*
  This writes out any stuff that we have buffered up, and then closes
  the mask file.

  Inputs:
    handle	Pointer to the structure describing the massk file.
------------------------------------------------------------------------*/
{
  MASK_INFO *mask;
  int iostat;

  mask = (MASK_INFO *)handle;
  if(mask->modified) mkflush_c(handle);
  hdaccess_c(mask->item,&iostat);				CHECK(iostat);
  free((char *)mask);
}
/************************************************************************/
int mkread_c(handle,mode,flags,offset,n,nsize)
char *handle;
int offset,n,*flags,nsize,mode;
/*
------------------------------------------------------------------------*/
{
#define SWITCH_STATE *flags++ = runs + (state ? 0 : 1 ); \
		     t = state; state = otherstate; otherstate = t

  MASK_INFO *mask;
  int i,len,boff,blen,bitmask,*buf,iostat,t,state,otherstate,runs;
  int *flags0;

  flags0 = flags;
  mask = (MASK_INFO *)handle;
  offset += OFFSET;
  state = 0;
  otherstate = 0x7FFFFFFF;
  runs = 0;


/* Get a buffer full of information if we need it. */

  while(n > 0){
    if(offset < mask->offset || offset >= mask->offset + mask->length) {
      if(mask->modified)mkflush_c(handle);
      mask->offset = (offset/BITS_PER_INT)*BITS_PER_INT;
      mask->length = min(mask->size - mask->offset,BUFFERSIZE*BITS_PER_INT);
      mask->modified = FALSE;
      if(mask->length == 0) BUG('f',"Read past end of mask file");
      hreadi_c(mask->item,mask->buf,
		(mask->offset/BITS_PER_INT)*H_INT_SIZE,
		(mask->length/BITS_PER_INT)*H_INT_SIZE,
					&iostat);	CHECK(iostat);
    }

/* Copy the flags to the output buffer. Use special sections of code
   to deal with all bits being set of clear. */

    boff = offset - mask->offset;
    t = boff/BITS_PER_INT;
    buf = mask->buf + t;
    len = min(mask->length - boff,n);
    boff -= t*BITS_PER_INT;

    n -= len;
    offset += len;

/* Copy to the output, in "flags" format. */

    if(mode == MK_FLAGS){
      while( len > 0){
	blen = min( BITS_PER_INT - boff,len);
	bitmask = *buf++;
	if(bitmask == 0x7FFFFFFF) for(i=0; i<blen; i++) *flags++ = FORT_TRUE;
	else if(bitmask == 0)     for(i=0; i<blen; i++) *flags++ = FORT_FALSE;
	else{
	  for(i=boff; i<boff+blen; i++)
	    *flags++ = ( bits[i] & bitmask ? FORT_TRUE : FORT_FALSE );
	}
	len -= blen;
	boff = 0;
      }

/* Copy to the output, in "runs" format. */

    } else {
      while( len > 0){
	blen = min( BITS_PER_INT - boff,len);
	bitmask = *buf++;
	if(bitmask == state ) runs += blen;
        else if(bitmask == otherstate ) { SWITCH_STATE; runs += blen; }
	else {
	  for(i=boff; i<boff+blen; i++){
	    if((bits[i] & bitmask) != (bits[i] & state)) { SWITCH_STATE; }
	    runs++;
	  }
	}
	len -= blen;
	boff = 0;
      }
    }
  }
  if(state) *flags++ = runs;
  nsize -= (flags - flags0);
  if(nsize < 0) bug_c('f',"Buffer overflow in MKREAD");
  return(flags - flags0);
}
/************************************************************************/
void mkwrite_c(handle,mode,flags,offset,n,nsize)
char *handle;
int offset,n,*flags,nsize;
/*
------------------------------------------------------------------------*/
{
  MASK_INFO *mask;
  int i,len,boff,blen,bitmask,*buf,t;
  int run,curr,state,iostat;

  curr = 0;
  state = 1;
  run = 0;

  mask = (MASK_INFO *)handle;
  offset += OFFSET;

/* If the mask is currently read-only, close it and reopen it as read/write. */

  if(mask->rdonly){
    hdaccess_c(mask->item,&iostat);
    haccess_c(mask->tno,&(mask->item),mask->name,"append",&iostat);
    if(iostat){
      bug_c('w',"Error opening mask/flagging file in read/write mode\n");
      bugno_c('f',iostat);
    }
    mask->rdonly = FALSE;
  }

/* Check if we have the right buffer. Flush if not. */

  while(n > 0){
    if(offset < mask->offset || offset >= mask->offset + BUFFERSIZE*BITS_PER_INT) {
      if(mask->modified)mkflush_c(handle);
      mask->offset = (offset/BITS_PER_INT)*BITS_PER_INT;
      mask->length = 0;
      mask->modified = FALSE;
    }

/* See if we have to read in any stuff to fill in between the last write
   and the current write. */

  if(offset > mask->offset + mask->length)mkfill(mask,offset);

/* Copy the flags to the output buffer. */

    boff = offset - mask->offset;
    t = boff/BITS_PER_INT;
    buf = mask->buf + t;
    len = min(BITS_PER_INT*BUFFERSIZE - boff,n);
    boff -= t*BITS_PER_INT;

    mask->length = max(mask->length,offset - mask->offset + len);
    mask->modified = TRUE;

    n -= len;
    offset += len;

/* Write to the file, assuming the input is in FLAGS format. */

    if(mode == MK_FLAGS){
      while( len > 0){
        blen = min( BITS_PER_INT - boff,len);
        bitmask = *buf;
        for(i=boff; i<boff+blen; i++){
	  if(*flags++ == FORT_FALSE) bitmask &= ~bits[i];
	  else			     bitmask |=  bits[i];
        }
        *buf++ = bitmask;
        len -= blen;
        boff = 0;
      }

/* Write to the file, assuming the input is in RUNS format. */

    } else {
      while( len > 0 ){
	while(run == 0){
	  if(nsize == 0) run = n + len;
	  else{
	    t = *flags++ - (state ? 1 : 0);
	    run = t - curr;
	    curr = t;
	    nsize --;
	  }
	  state ^= 1;
	}
	blen = min(run, min( BITS_PER_INT - boff, len));
	bitmask = masks[boff+blen] ^ masks[boff];
	if(state) *buf |=  bitmask;	/* Set the bits. */
	else	  *buf &= ~bitmask;	/* Clear the bits. */
	run -= blen;
	len -= blen;
	boff = (boff + blen) % BITS_PER_INT;
	if(!boff) buf++;
      }
    }
  }
}
/************************************************************************/
void mkflush_c(handle)
char *handle;
/*
  Flush out the data in the buffer. A complication is that the last
  integer in the buffer may not be completely filled. In this case we
  have to read in the value of this integer, and copy the old bits to
  the output.

  Input:
    mask	Pointer to the mask structure.
------------------------------------------------------------------------*/
{
  MASK_INFO *mask;
  int i,t,*buf,offset,iostat;

  mask = (MASK_INFO *)handle;

/* If we are writing at the end of the file, make sure the number we
   write is a multiple of BITS_PER_INT. Also update the size of the
   file. */

  if( mask->offset + mask->length >= mask->size) {
    mask->length = ((mask->length-1)/BITS_PER_INT + 1)*BITS_PER_INT;
    mask->size = mask->offset + mask->length;

/* If the last word is only partially filled, read in the rest of the
   word and transfer the bits. */

  } else if((mask->length % BITS_PER_INT) != 0) {
    offset = (mask->offset + mask->length) / BITS_PER_INT * H_INT_SIZE;
    hreadi_c(mask->item,&t,offset,H_INT_SIZE,&iostat);		CHECK(iostat);
    buf = mask->buf + mask->length/BITS_PER_INT;
    i = mask->length % BITS_PER_INT;
    *buf = ( t & ~masks[i] ) | (*buf & masks[i]);
    mask->length = ((mask->length-1)/BITS_PER_INT + 1)*BITS_PER_INT;
  }

/* Write out the stuff at last. */

  hwritei_c(mask->item,mask->buf,
		mask->offset/BITS_PER_INT*H_INT_SIZE,
		mask->length/BITS_PER_INT*H_INT_SIZE,&iostat);	CHECK(iostat);
  mask->modified = FALSE;
}
/************************************************************************/
private void mkfill(mask,offset)
MASK_INFO *mask;
int offset;
/*
  We have to fill in some bits in the current buffer.

  Inputs:
    mask	Pointer to the mask structure.
    offset	The first location that we want to write at.
------------------------------------------------------------------------*/
{
  int off,len,t,*buf,iostat,i;

  if(mask->offset+mask->length < mask->size) {
    buf = mask->buf + mask->length/BITS_PER_INT;
    t = *buf;
    off = (mask->offset + mask->length)/BITS_PER_INT;
    len = min(offset/BITS_PER_INT + 1,mask->size/BITS_PER_INT) - off;
    hreadi_c(mask->item,buf,
		off*H_INT_SIZE,len*H_INT_SIZE,&iostat);		CHECK(iostat);
    i = mask->length % BITS_PER_INT;
    *buf = ( t & masks[i] ) | (*buf & ~masks[i]);
    mask->length = (off + len)*BITS_PER_INT - mask->offset;
  }
}
