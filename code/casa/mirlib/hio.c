/*
  The routines to manipulate the file heirarchy.

	6-dec-89  pjt	extended bug() messages
       30-apr-90  rjs   Support for zero-length items. Added hdelete.
       15-jul-91  rjs   Check for valid item names in hopen and hdelete.
		        Some mods to some error messages.
       18-jul-91  rjs	Fixed the name checking to accept the "." file.
        2-aug-91  rjs   Fixed the name checking to accept '-'.
       16-oct-91  rjs	Truncated an item when it is opened for rewriting.
       12-oct-92  rjs   Changed "roundup" macro definition, for pjt.
	3-mar-93  rjs	Add hflush.
       10-aug-93  rjs   Add hexists.
       26-aug-93  rjs   Add habort,hrm.
       30-aug-93  rjs   Add hseek, htell.
        7-sep-93  rjs   Bug fix in habort.
       23-dec-93  rjs   hexists did not handle tno==0 correctly.
        5-jan-93  rjs   Added hmode to check access mode of dataset.
        4-nov-94  rjs	Changes to the way trees and items are stored.
       15-nov-94  rjs	Fixed bug affecting small items being rewritten
			before the dataset is closed.
       27-dec-94  pjt   Fixed (?) bug in hexist for regular files
			and documented this feature
       13-mar-95  rjs   Increase max number of open items.
       30-jun-95  rjs   Declaration to appease gcc.
       15-may-96  rjs	More fiddles with roundup macro.
*/


#include <stdlib.h>
#include <string.h>

#include "hio.h"

#define private static
#if !defined(NULL)
#  define NULL 0
#endif

#define MAXNAME 9
#define CACHESIZE 64			/* Max size of items to cache. */
#define CACHE_ENT 16			/* Alignment of cache items. */

#define IO_VALID      0			/* Set if the i/o buffer is valid. */
#define IO_ACTIVE     1
#define IO_MODIFIED   2
#define ITEM_READ     0x1
#define ITEM_WRITE    0x2
#define ITEM_SCRATCH  0x4
#define ITEM_APPEND   0x8
#define ACCESS_MODE (ITEM_READ|ITEM_WRITE|ITEM_SCRATCH|ITEM_APPEND)
#define ITEM_CACHE    0x10
#define ITEM_NOCACHE  0x20

#define TREE_CACHEMOD 0x1
#define TREE_NEW      0x2

#define RDWR_UNKNOWN 0
#define RDWR_RDONLY  1
#define RDWR_RDWR    2

typedef struct { int offset,length,state; char *buf;} IOB;

typedef struct item {	char *name;
			int handle,size,flags,fd,last,bsize,offset;
			struct tree *tree;
			IOB io[2];
			struct item *fwd; } ITEM;

typedef struct tree { char *name;
		 int handle,flags,rdwr,wriostat;
		 ITEM *itemlist; } TREE;

#define MAXITEM 1024

private int nitem,ntree;
private TREE *tree_addr[MAXOPEN];
private ITEM *item_addr[MAXITEM];

#define hget_tree(tno) (tree_addr[tno])
#define hget_item(tno) (item_addr[tno])

private int header_ok,expansion[10];
private int first=TRUE;

/* Macro to wait for I/O to complete. If its a synchronous i/o system,
   never bother calling the routine to wait for i/o completion.       */

#if BUFDBUFF
#define WAIT(item,iostat) 			\
    if((item)->io[0].state == IO_ACTIVE){	\
      dwait_c((item)->fd,iostat);		\
      (item)->io[0].state = IO_VALID;		\
    } else if((item)->io[1].state == IO_ACTIVE){	\
      dwait_c((item)->fd,iostat);		\
      (item)->io[1].state = IO_VALID;		\
    }
#else
#define WAIT(a,b)
#define dwait_c(a,b)
#endif

/* Declare a few private routines. */

private void hcheckbuf_c(),hcache_read_c(),hrelease_item_c(),
  hcache_create_c(),hwrite_fill_c(),hdir_c(),hinit_c();
private int hname_check();
private ITEM *hcreate_item_c();
private TREE *hcreate_tree_c();

#define check(iostat) if(iostat) bugno_c('f',iostat)

/* Define a few things so that I can avoid lint being pedantic. */

void bug_c(),bugno_c(),dopendir_c(),dclosedir_c(),dreaddir_c(),drmdir_c();
void ddelete_c(),pack16_c(),unpack16_c();
void dtrans_c(),dmkdir_c(),dopen_c(),dclose_c(),dread_c(),dwrite_c();

private int hfind_nl();

#define Malloc(a) malloc((size_t)(a))
#define Realloc(a,b) realloc((a),(size_t)(b))
#define Strcpy (void)strcpy
#define Strcat (void)strcat
#define Memcpy (void)memcpy

/************************************************************************/
void hopen_c(tno,name,status,iostat)
int *iostat,*tno;
char *name,*status;
/**hopen -- Open a data set.			 			*/
/*&mjs									*/
/*:low-level-i/o							*/
/*+ FORTRAN call sequence

	subroutine hopen(tno,name,status,iostat)
	integer tno,iostat
	character name*(*),status*(*)

  This opens a Miriad data-set, and readies it to be read or written.

  Input:
    name	The name of the data set.
    status	Either 'old' or 'new'.
  Output:
    tno		The file handle of the opened data set.
   iostat	I/O status indicator. 0 indicates success. Other values
		are standard system error numbers.			*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  char path[MAXPATH];
  TREE *t;

/* Initialise if its the first time through. */

  if(first)hinit_c();

/* Find a spare slot, and set the name etc. */

  dtrans_c(name,path,iostat);
  if(*iostat)return;
  t = hcreate_tree_c(path);

/* Either open an old cache, or create a new cache. */

  if(!strcmp(status,"old")){
    hcache_read_c(t,iostat);
    t->rdwr = RDWR_UNKNOWN;
  } else if(!strcmp(status,"new")){
    dmkdir_c(path,iostat);
    if(!*iostat)hcache_create_c(t,iostat);
    t->flags |= TREE_NEW;
    t->rdwr = RDWR_RDWR;
  } else *iostat = -1;

/* Tidy up before we return. Make sure things are tidy if an error
   occurred during the operation. */

  *tno = t->handle;
  if(*iostat) hclose_c(*tno);

}
/************************************************************************/
private void hinit_c()
/*
  Initialise everthing the first time through.
------------------------------------------------------------------------*/
{
  int i;

  nitem = ntree = 0;
  for(i=0; i < MAXITEM; i++)item_addr[i] = NULL;
  for(i=0; i < MAXOPEN; i++)tree_addr[i] = NULL;
  (void)hcreate_tree_c("");

  expansion[H_BYTE] = 1;
  expansion[H_INT]  = sizeof(int)/H_INT_SIZE;
  expansion[H_INT2] = sizeof(int2)/H_INT2_SIZE;
  expansion[H_REAL] = sizeof(float)/H_REAL_SIZE;
  expansion[H_DBLE] = sizeof(double)/H_DBLE_SIZE;
  expansion[H_CMPLX] = 2*sizeof(float)/H_CMPLX_SIZE;
  expansion[H_TXT]  = 1;
  first = FALSE;
  header_ok = FALSE;
}
/************************************************************************/
void hflush_c(tno,iostat)
int tno,*iostat;
/**hflush -- Close a Miriad data set.		 			*/
/*&mjs									*/
/*:low-level-i/o							*/
/*+ FORTRAN call sequence

	subroutine hflush(tno,iostat)
	integer tno,iostat

  Write to disk any changed items.

  Input:
    tno		The handle of the Miriad data set.			*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  TREE *t;
  ITEM *item;
  char s[CACHE_ENT];
  int offset,i,ihandle;

  t = hget_tree(tno);
  *iostat = 0;

/* Determine whether the cache needs to be rewritten, and write out
   any modified buffers. */

  for(item = t->itemlist; item != NULL ; item = item->fwd){
    if(!item->fd && !(item->flags & ITEM_NOCACHE) ){
      if(item->io[0].state == IO_MODIFIED) t->flags |= TREE_CACHEMOD;
    } else if(item->fd && !(item->flags & ITEM_SCRATCH) ){
      for(i=0; i<2; i++){
	if(item->io[i].state == IO_MODIFIED){
	  WAIT(item,iostat);
	  if(*iostat)return;
	  dwrite_c( item->fd, item->io[i].buf, item->io[i].offset,
				     item->io[i].length, iostat);
	  if(*iostat)return;
	  item->io[i].state = IO_ACTIVE;
        }
      }
    }
  }

/* If the cache has been modified, rewrite the cache. */

  if(t->flags & TREE_CACHEMOD){
    header_ok = TRUE;
    haccess_c(tno,&ihandle,"header","write",iostat);
    header_ok = FALSE;						if(*iostat)return;
    for(i=0; i < CACHE_ENT; i++)s[i] = 0;

    offset = 0;
    for(item = t->itemlist; item != NULL; item = item->fwd){
      if(!item->fd && !(item->flags & ITEM_NOCACHE)){
        Strcpy(s,item->name);
        s[CACHE_ENT-1] = item->size;
        hwriteb_c(ihandle,s,offset,CACHE_ENT,iostat);		if(*iostat)return;
        offset += CACHE_ENT;
        if(item->size > 0){
          hwriteb_c(ihandle,item->io[0].buf,offset,item->size,iostat);
								if(*iostat)return;
        }
	item->io[0].state = IO_VALID;
	item->flags |= ITEM_CACHE;
        offset += mroundup(item->size,CACHE_ENT);
      }
    }
    hdaccess_c(ihandle,iostat);					if(*iostat)return;
    t->flags &= ~TREE_CACHEMOD;
  }
  *iostat = 0;
}
/************************************************************************/
void habort_c()
/**habort -- Abort handling of all open data-sets.			*/
/*&mjs									*/
/*:low-level-i/o							*/
/*+ FORTRAN call sequence

	subroutine habort()

  This closes all open Miriad data-sets, and deletes any new ones. No
  buffers are flushed.							*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  int i,iostat;
  TREE *t;
  ITEM *it,*itfwd;
  char name[MAXPATH];

/* Don't do anything if the hio routines have never been called. */

  if(first)return;

/* Flush everything belonging to tree 0. */

  hflush_c(0,&iostat);

/* Check each possible tree. */

  for( i=0; i < MAXOPEN; i++){
    if( (t = hget_tree(i) ) != NULL){
      it = t->itemlist;
      while(it != NULL){
        itfwd = it->fwd;

/* Wait for any i/o to complete, and prevent further flushing of the buffers
   by pretending that nothing has been modified. */

        WAIT(it,&iostat);
        it->io[0].state = IO_VALID;
        it->io[1].state = IO_VALID;

/* If its an item opened in WRITE mode, remember its name. */

        if(it->flags & ITEM_WRITE)Strcpy(name,it->name);
        else name[0] = 0;

/* If the item is open, close it. */
/* If it was in write mode, and the name was known, delete it. */

        if(it->flags & ACCESS_MODE)hdaccess_c(it->handle,&iostat);
        if(*name)hdelete_c(t->handle,name,&iostat);
        it = itfwd;
      }
    
/* Pretend the cache has not changed and finish up. Completely delete
   trees that were opened as NEW. Otherwise finish up. */

      t->flags &= ~TREE_CACHEMOD;
      if(t->flags & TREE_NEW)hrm_c(t->handle);
      else hclose_c(t->handle);
    }
  }
}
/************************************************************************/
void hrm_c(tno)
int tno;
/**hrm -- Remove a data-set.						*/
/*&mjs									*/
/*:low-level-i/o							*/
/*+ FORTRAN call sequence

	subroutine hrm(tno)

	integer tno

  This completely removes a Miriad data-set.

  Input:
    tno		The file handle of the open data-set.			*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  char name[MAXPATH];
  int iostat,ihandle;
  TREE *t;

  haccess_c(tno,&ihandle,".","read",&iostat);
  if(iostat == 0){
    hreada_c(ihandle,name,MAXPATH,&iostat);
    while(iostat == 0){
      hdelete_c(tno,name,&iostat);
      hreada_c(ihandle,name,MAXPATH,&iostat);
    }
    hdaccess_c(ihandle,&iostat);
  }

/* Delete the "header" item. */

  header_ok = TRUE;
  hdelete_c(tno,"header",&iostat);
  header_ok = FALSE;

/* Delete the directory itself. */

  t = hget_tree(tno);
  drmdir_c(t->name,&iostat);
  hclose_c(tno);
}
/************************************************************************/
void hclose_c(tno)
int tno;
/**hclose -- Close a Miriad data set.		 			*/
/*&mjs									*/
/*:low-level-i/o							*/
/*+ FORTRAN call sequence

	subroutine hclose(tno)
	integer tno

  This closes a Miriad data set. The data set cannot be accessed after the
  close.

  Input:
    tno		The handle of the Miriad data set.			*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  TREE *t;
  ITEM *item,*it1,*it2;
  int iostat;
  char message[40];

/* Close any open items. */

  t = hget_tree(tno);
  for(item=t->itemlist; item != NULL; item = item->fwd){
    if(item->flags & ACCESS_MODE){
      Strcpy(message,"Closing item -- ");
      Strcat(message,item->name);
      bug_c('w',message);
      hdaccess_c(item->handle,&iostat);			check(iostat);
    }
  }

/* Flush out the header, if needed. */

  hflush_c(tno,&iostat);				check(iostat);

/* Release all allocated stuff. */

  it1 = t->itemlist;
  while(it1 != NULL){
    it2 = it1->fwd;
    hrelease_item_c(it1);
    it1 = it2;
  }
  tree_addr[tno] = NULL;
  free(t->name);
  free((char *)t);
  ntree--;
}
/************************************************************************/
void hdelete_c(tno,keyword,iostat)
int *iostat,tno;
char *keyword;
/**hdelete -- Delete an item from a data-set.				*/
/*&mjs									*/
/*:low-level-i/o							*/
/*+ FORTRAN call sequence

	subroutine hdelete(tno,keyword,iostat)
	integer tno,iostat
	character keyword*(*)


  This deletes an item from a Miriad data-set. The item must not be "accessed"
  when the hdelete routine is called.

  Input:
    tno		The handle of the data set.
    keyword	The name of the item.
  Output:
   iostat	I/O status indicator. 0 indicates success. Other values
		are standard system error numbers.			*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  char path[MAXPATH];
  ITEM *item;
  TREE *t;
  int ent_del;

  if(first)hinit_c();

  if(tno != 0) if(*iostat = hname_check(keyword)) return;

/* Check if the item is aleady here abouts. */

  t = hget_tree(tno);

  ent_del = FALSE;
  item = NULL;
  if(tno != 0)
    for(item=t->itemlist; item != NULL; item = item->fwd)
      if(!strcmp(keyword,item->name))break;

/* Delete the entry for this item, if there was one. */

  if(item != NULL){
    if(item->flags & ACCESS_MODE)
      bug_c('f',"hdelete: Attempt to delete an accessed item");
    if(item->flags & ITEM_CACHE) t->flags |= TREE_CACHEMOD;
    hrelease_item_c(item);
    ent_del = TRUE;
  }

/* Always try to delete a file associated with the item. */

  Strcpy(path,t->name);
  Strcat(path,keyword);
  ddelete_c(path,iostat);

/* If we have deleted it once already, do not give any errors if the
   second attempt failed. */

  if(ent_del) *iostat = 0;
}
/************************************************************************/
void haccess_c(tno,ihandle,keyword,status,iostat)
int *iostat,tno;
int *ihandle;
char *keyword,*status;
/**haccess -- Open an item of a data set for access.			*/
/*&mjs									*/
/*:low-level-i/o							*/
/*+ FORTRAN call sequence

	subroutine haccess(tno,itno,keyword,status,iostat)
	integer tno,itno,iostat
	character keyword*(*),status*(*)

  Miriad data sets consist of a collection of items. Before an item within
  a data set can be read/written, etc, it must be "opened" with the haccess
  routine.

  Input:
    tno		The handle of the data set.
    keyword	The name of the item.
    status	This can be 'read', 'write', 'append' or 'scratch'.
  Output:
    itno	The handle of the opened item. Note that item handles are
		quite distinct from data-set handles.
   iostat	I/O status indicator. 0 indicates success. Other values
		are standard system error numbers.			*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  char path[MAXPATH];
  ITEM *item;
  TREE *t;
  int mode;
  char string[3];

  if(first)hinit_c();

  if(!strcmp("read",status))	    mode = ITEM_READ;
  else if(!strcmp("write",status))  mode = ITEM_WRITE;
  else if(!strcmp("scratch",status))mode = ITEM_SCRATCH;
  else if(!strcmp("append",status)) mode = ITEM_APPEND;
  else bug_c('f',"haccess_c: Unrecognised STATUS");

  if(!strcmp("header",keyword) || !strcmp(".",keyword) ||
     !strcmp("history",keyword)|| tno == 0 	       ||
     (mode & ITEM_SCRATCH)		)mode |= ITEM_NOCACHE;

  if(tno != 0) if(*iostat = hname_check(keyword))return;
  t = hget_tree(tno);

/* If we are writing, check whether we have write permission. */

  if( !(mode & ITEM_READ) && !(mode & ITEM_NOCACHE) ){
    if(t->rdwr == RDWR_UNKNOWN)hmode_c(tno,string);
    *iostat = t->wriostat;
    if(*iostat) return;
  }

/* Check if the item is aleady here abouts. */

  item = NULL;
  if(tno != 0)
    for(item = t->itemlist; item != NULL; item = item->fwd)
      if(!strcmp(keyword,item->name))break;

/* If the item does not exist, create it. Otherwise the item must
   be cacheable, in which case we truncate its length to zero if needed. */

  if(item == NULL)item = hcreate_item_c(t,keyword);
  else if((mode & (ITEM_WRITE|ITEM_SCRATCH)) && item->size != 0){
    item->size = 0;
    item->io[0].length = item->io[1].length = 0;
    if(item->flags & ITEM_CACHE) t->flags |= TREE_CACHEMOD;
  }

/* Check and set the read/write flags. */

  if(item->flags & ACCESS_MODE) bug_c('f',"haccess_c: Multiple access to item");  
  item->flags |= mode;

/* Open the file if necessary. */

  *iostat = 0;
  item->offset = 0;
  if(!strcmp(keyword,".")){
    hdir_c(item);
  } else if(item->size == 0 && (!(mode & ITEM_WRITE) || (mode & ITEM_NOCACHE))
    			    && !(item->flags & ITEM_CACHE)){
    Strcpy(path,t->name);
    Strcat(path,keyword);
    dopen_c(&(item->fd),path,status,&(item->size),iostat);

    item->bsize = BUFSIZE;
    item->io[0].buf = Malloc(BUFSIZE);
    if(BUFDBUFF)item->io[1].buf = Malloc(BUFSIZE);
    if(mode & ITEM_APPEND) item->offset = item->size;

/* If we have opened a file in write mode, remember that this dataset is
   writeable. */

    if(!(mode & ITEM_READ)){
      if(*iostat == 0) t->rdwr = RDWR_RDWR;
      else	       t->rdwr = RDWR_RDONLY;
      t->wriostat = *iostat;
    }
  }
  *ihandle = item->handle;
  if(*iostat)hrelease_item_c(item);
}
/************************************************************************/
void hmode_c(tno,mode)
int tno;
char *mode;
/*									*/
/**hmode -- Return access modes of a dataset.				*/
/*&mjs									*/
/*:low-level-i/o							*/
/*+ FORTRAN call sequence

	subroutine hmode(tno,mode)
	integer tno
        character mode*(*)

  Determine the access modes of a data-set

  Input:
    tno		The handle of the data set.
  Output:
    mode	This will be either "" (unknown access mode),
				    "r" (read-only)
				    "rw" (read-write).			*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  int iostat;
  int ihandle;
  TREE *t;

/* If its tno==0, give up. */

  *mode = 0;
  if(tno == 0)return;

/* If we do not already know the read/write access, determine it the hard
   way. */

  t = hget_tree(tno);
  if(t->rdwr == RDWR_UNKNOWN){
    header_ok = TRUE;
    haccess_c(tno,&ihandle,"header","append",&iostat);
    header_ok = FALSE;
    if(!iostat)hdaccess_c(ihandle,&iostat);
  }

/* Return the info. */

  if(t->rdwr == RDWR_RDONLY)    Strcpy(mode,"r");
  else if(t->rdwr == RDWR_RDWR) Strcpy(mode,"rw");
  else bug_c('f',"Algorithmic failure, in HMODE");

}
/************************************************************************/
int hexists_c(tno,keyword)
int tno;
char *keyword;
/**hexists -- Check if an item exists.					*/
/*&mjs									*/
/*:low-level-i/o							*/
/*+ FORTRAN call sequence

	logical function hexists(tno,keyword)
	integer tno
	character keyword*(*)

  Check if a particular item exists in a Miriad data-set.
  By setting the input 'tno' to 0, one can also check for
  existence of any regular file.

  Input:
    tno		The handle of the data set. 0 also allowed.
    keyword	The name of the item or filename to check.
  Output:
    hexists	True if the item exists.				*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  char path[MAXPATH];
  int iostat,fd,size;
  ITEM *item;
  TREE *t;

/* Check for an invalid name. */

  if(tno != 0) if(hname_check(keyword)) return(FALSE);

/* Check if the item is aleady here abouts. */

  if(tno != 0){			/* miriad dataset */
    item = NULL;
    t = hget_tree(tno);
    for(item = t->itemlist; item != NULL; item = item->fwd)
      if(!strcmp(keyword,item->name))return(TRUE);
    Strcpy(path,t->name);
    Strcat(path,keyword);
  } else {
    Strcpy(path,keyword);	/* regular filename */
  }

/* It was not found in the items currently opened, nor the items that
   live in "header". Now try and open a file with this name. */


  dopen_c(&fd,path,"read",&size,&iostat);
  if(iostat)return(FALSE);
  dclose_c(fd,&iostat);
  if(iostat != 0)bug_c('f',"hexists_c: Error closing item");
  return(TRUE);
}
/************************************************************************/
void hdaccess_c(ihandle,iostat)
int ihandle;
int *iostat;
/**hdaccess -- Finish up access to an item.				*/
/*&mjs									*/
/*:low-level-i/o							*/
/*+ FORTRAN call sequence

	subroutine hdaccess(itno,iostat)
	integer itno,iostat

  This releases an item. It flushes buffers and waits for i/o to complete.
  For small items that are entirely in memory, these are saved until
  the whole tree is closed before they are written out.

  Input:
    itno	The handle of the item to close up.
    iostat	I/O status indicator. 0 indicates success. Other values
		are standard system error numbers.			*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  ITEM *item;
  int i,stat;

/* If it has an associated file, flush anything remaining to the file
   and close it up. */

  item = hget_item(ihandle);

/* May be a binary file. Flush modified buffers, wait for i/o to complete,
   and close up. */

  *iostat = 0;
  stat = 0;
  if(item->fd != 0){
    for(i=0; i<2 && !stat; i++){
      if(item->io[i].state == IO_MODIFIED && !(item->flags & ITEM_SCRATCH)){
	WAIT(item,&stat);
	if(!stat)dwrite_c( item->fd, item->io[i].buf, item->io[i].offset,
				     item->io[i].length, &stat);
	item->io[i].state = IO_ACTIVE;
      }
    }
    *iostat = stat;
    WAIT(item,&stat);
    if(stat) *iostat = stat;
    dclose_c(item->fd,&stat);
    if(stat) *iostat = stat;
    hrelease_item_c(item);

  } else if(item->flags & ITEM_NOCACHE){
    hrelease_item_c(item);

/* If it has not associated file, it must be small. Do not release it,
   as it will need to be written to the cache later on. */

  } else{
    item->flags &= ~ACCESS_MODE;
    if(item->io[0].state == IO_MODIFIED)item->tree->flags |= TREE_CACHEMOD;
    item->io[0].state = IO_VALID;
  }
}
/************************************************************************/
int hsize_c(ihandle)
int ihandle;
/**hsize -- Determine the size (in bytes) of an item. 			*/
/*&mjs									*/
/*:low-level-i/o							*/
/*+ FORTRAN call sequence

	integer function hsize(itno)
	integer itno

  This returns the size of an item, in bytes.

  Input:
    itno	The handle of the item of interest.
  Output:
    hsize	The size of the item in bytes.				*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  ITEM *item;
  item = hget_item(ihandle);
  return(item->size);
}
/************************************************************************/
void hio_c(ihandle,dowrite,type,buf,offset,length,iostat)
int ihandle;
int dowrite,type,offset,length,*iostat;
char *buf;
/**hread,hwrite -- Read and write items.	 			*/
/*&mjs									*/
/*:low-level-i/o							*/
/*+ FORTRAN call sequence

	subroutine hreada(itno,abuf,iostat)
	subroutine hreadb(itno,bbuf,offset,length,iostat)
	subroutine hreadj(itno,jbuf,offset,length,iostat)
	subroutine hreadi(itno,ibuf,offset,length,iostat)
	subroutine hreadr(itno,rbuf,offset,length,iostat)
	subroutine hreadd(itno,dbuf,offset,length,iostat)
	subroutine hwritea(itno,abuf,iostat)
	subroutine hwriteb(itno,bbuf,offset,length,iostat)
	subroutine hwritej(itno,jbuf,offset,length,iostat)
	subroutine hwritei(itno,ibuf,offset,length,iostat)
	subroutine hwriter(itno,rbuf,offset,length,iostat)
	subroutine hwrited(itno,dbuf,offset,length,iostat)
	integer itno,offset,length,iostat
	character abuf*(*),bbuf*(length)
	integer jbuf(*),ibuf(*)
	real rbuf(*)
	double precision dbuf(*)

  These routines read and write items of a Miriad data set. They
  differ in the sort of element that they read or write.
	hreada,hwritea	I/O on ascii text data (terminated by newline char).
	hreadb,hwriteb	I/O on ascii data.
	hreadj,hwritej	I/O on data stored externally as 16 bit integers.
	hreadi,hwritei	I/O on data stored externally as 32 bit integers.
	hreadr,hwriter	I/O on data stored externally as IEEE 32 bit reals.
	hreadd,hwrited	I/O on data stored externally as IEEE 64 bit reals.

  Note that hreada and hreadb differ in that:
    * hreada reads sequentially, terminating a read on a newline character.
      The output buffer is blank padded.
    * hreadb performs random reads. Newline characters have no special
      meaning to it. A fixed number of bytes are read, and the buffer is
      not blank padded.
   Hwritea and hwriteb differ in similar ways.

  Inputs:
    itno	The handle of the item to perform I/O on.
    offset	The byte offset into the item, where I/O is to be
		performed.
    length	The number of bytes to be read.

  "Offset" and "length" are offsets and lengths into the external file, always
  given in bytes.

  Note that "offset" and "length" must obey an alignment requirement. Both
  must be a multiple of the size of the element they are performing I/O on.
  For eaxample, they must be a multiple of 2 for hreadj,hwritej; a multiple
  of 4 for hreadi,hwritei,hreadr,hwriter; a multiple of 8 for hreadd,hwrited.

  Inputs(hwrite) or Outputs(hread):
    abuf,bbuf,jbuf,ibuf,rbuf,dbuf The buffer containing, or to receive,
		the data.
  Outputs:
    iostat	I/O status indicator. 0 indicates success. -1 indicates
		end-of-file. Other values are standard system
		error numbers.						*/
/*--									*/
/*----------------------------------------------------------------------*/
/*
  This performs either a read or write operation. It is somewhat involved,
  as it has to handle buffering. Possibly either one or two buffers are
  used (system dependent). Read-ahead, write-behind are attempted for
  systems which can perform this.

  This is intended to work in both a VMS and UNIX environment, which makes
  it quite involved (because of VMS).

  Because of caching of small items, buffers are not allocated until the
  last moment.								*/

/* Define a macro to determine if a offset maps into a buffer. */

#define WITHIN_BUF(b) (	(item->io[b].length > 0) && \
			(offset >= item->io[b].offset) && \
			(offset <  item->io[b].offset +  \
			(dowrite ? item->bsize : item->io[b].length)))

{
  int next,b,off,len;
  IOB *iob1,*iob2;
  ITEM *item;

  item = hget_item(ihandle);

/* Check various end-of-file conditions and for adequate buffers. */

  next = offset + (!dowrite && type == H_TXT ? 1 : length );
  if(!dowrite && type == H_TXT) length = min(length, item->size - offset);
  *iostat = -1;
  if(!dowrite && next > item->size)return;
  *iostat = 0;
  if(item->bsize < BUFSIZE && item->bsize < next)hcheckbuf_c(item,next,iostat);
  if(*iostat)return;

/*----------------------------------------------------------------------*/
/*									*/
/*	Loop until we have processed all the data required.		*/
/*	First determine which of the (possibly) two i/o buffers		*/
/*	to use. If we have only one buffer, we have no choice. If our	*/
/*	data is within the last used buffer, use that. Otherwise use	*/
/*	the least recent used buffer.					*/
/*									*/
/*----------------------------------------------------------------------*/

  while(length > 0){

    b = item->last;
    if(item->io[1].buf == NULL)	b = 0;
    else if(WITHIN_BUF(b)){
      if(WITHIN_BUF(1-b)) b = ( item->io[0].offset > item->io[1].offset ? 0 : 1);
    } else b = 1 - b;
    iob1 = &(item->io[b]);
    iob2 = &(item->io[1-b]);

/*----------------------------------------------------------------------*/
/*									*/
/*	Handle the case of a miss. Flush the i/o buffer if it has been	*/
/*	modified and read in any needed new data.			*/
/*									*/
/*----------------------------------------------------------------------*/

    if(!WITHIN_BUF(b)){
      if(iob1->state == IO_MODIFIED){
	next = iob1->offset + iob1->length;
        if(iob1->length%BUFALIGN && next < item->size)
	  {hwrite_fill_c(item,iob1,next,iostat);	if(*iostat) return;}
        WAIT(item,iostat);				if(*iostat) return;
        dwrite_c(item->fd,iob1->buf,iob1->offset,iob1->length,iostat);
        iob1->state = IO_ACTIVE;			if(*iostat) return;
      }
      iob1->offset = (offset/BUFALIGN) * BUFALIGN;
      iob1->length = 0;
      if(!dowrite){
        WAIT(item,iostat);				if(*iostat) return;
        iob1->length = min(item->bsize,item->size-iob1->offset);
	if(iob2->buf != NULL && iob1->offset < iob2->offset)
	  iob1->length = min(iob1->length, iob2->offset - iob1->offset);
	dread_c(item->fd,iob1->buf,iob1->offset,iob1->length,iostat);
	iob1->state = IO_ACTIVE;			if(*iostat) return;
      }
    }

/*----------------------------------------------------------------------*/
/*									*/
/*	Wait for any i/o and perform a read ahead or write-behind,	*/
/*	so that we are ready next time.	Do this before we copy the	*/
/*	data to/from the callers buffer, so that we can overlap		*/
/*	the copy and i/o operations. The next section is skipped if 	*/
/*	the underlying i/o is synchronous.				*/
/*									*/
/*----------------------------------------------------------------------*/

#if BUFDBUFF
    if(iob1->state == IO_ACTIVE)
      {WAIT(item,iostat);				if(*iostat)return;}

    if(iob2->buf != NULL && iob2->state != IO_ACTIVE){
      next = iob1->offset + iob1->length;

/* Write behind. */
      if(iob2->state == IO_MODIFIED && (!(iob2->length%BUFALIGN) ||
				   iob2->offset + iob2->length == item->size)){
        dwrite_c(item->fd,iob2->buf,iob2->offset,iob2->length,iostat);
        iob2->state = IO_ACTIVE;

/* Read ahead. */
      } else if(!dowrite && next < item->size && next != iob2->offset){
        iob2->offset = next;
        iob2->length = min( BUFSIZE, item->size - iob2->offset );
        dread_c (item->fd,iob2->buf,iob2->offset,iob2->length,iostat);
        iob2->state = IO_ACTIVE;
      }
    }
#endif

/*----------------------------------------------------------------------*/
/*									*/
/*	If its a write operation, possibly update the file size, and	*/
/*	handle possible non-aligned non-sequential write operations.	*/
/*									*/
/*----------------------------------------------------------------------*/

    if(dowrite){
      if(iob1->offset + iob1->length < offset &&
         iob1->offset + iob1->length < item->size)
	  {hwrite_fill_c(item,iob1,offset,iostat);	if(*iostat) return;}
      iob1->state = IO_MODIFIED;
      iob1->length = max(iob1->length,
			 min(length + offset - iob1->offset, item->bsize));
      item->size = max(item->size,iob1->offset + iob1->length);
    }

/*----------------------------------------------------------------------*/
/*									*/
/*	Copy between the i/o buffer and users buffer.			*/
/*									*/
/*----------------------------------------------------------------------*/

    off  = offset - iob1->offset;
    len = min(length, iob1->length - off);
    if(dowrite)switch(type){
      case H_BYTE: 	Memcpy(iob1->buf+off,buf,len);
			break;
      case H_INT:  	pack32_c((int *)buf,iob1->buf+off,len/H_INT_SIZE);
			break;
      case H_INT2:	pack16_c((int2 *)buf,iob1->buf+off,len/H_INT2_SIZE);
			break;
      case H_REAL:	packr_c((float *)buf,iob1->buf+off,len/H_REAL_SIZE);
			break;
      case H_DBLE:	packd_c((double *)buf,iob1->buf+off,len/H_DBLE_SIZE);
			break;
      case H_CMPLX:	packr_c((float *)buf,iob1->buf+off,(2*len)/H_CMPLX_SIZE);
			break;
      case H_TXT:	Memcpy(iob1->buf+off,buf,len);
			if(*(buf+len-1) == 0)*(iob1->buf+off+len-1) = '\n';
			break;
      default:		bug_c('f',"hio_c: Unrecognised type");
    } else      switch(type){
      case H_BYTE: 	Memcpy(buf,iob1->buf+off,len);
			break;
      case H_INT:  	unpack32_c(iob1->buf+off,(int *)buf,len/H_INT_SIZE);
			break;
      case H_INT2:	unpack16_c(iob1->buf+off,(int2 *)buf,len/H_INT2_SIZE);
			break;
      case H_REAL:	unpackr_c(iob1->buf+off,(float *)buf,len/H_REAL_SIZE);
			break;
      case H_DBLE:	unpackd_c(iob1->buf+off,(double *)buf,len/H_DBLE_SIZE);
			break;
      case H_CMPLX:	unpackr_c(iob1->buf+off,(float *)buf,(2*len)/H_CMPLX_SIZE);
			break;
      case H_TXT:	len = hfind_nl(iob1->buf+off,len);
			Memcpy(buf,iob1->buf+off,len);
			if(*(iob1->buf+off+len-1) == '\n'){
			  length = len;
			  *(buf+len-1) = 0;
			}
			break;
      default:		bug_c('f',"hio_c: Unrecognised type");
    }
    buf += expansion[type] * len;
    length -= len;
    offset += len;
    item->offset = offset;
    item->last = b;
  }
}
/************************************************************************/
private int hfind_nl(buf,len)
char *buf;
int len;
/*
  Return the character number of the first new-line character.
------------------------------------------------------------------------*/
{
  int i;
  for(i=1;i <= len; i++)if(*buf++ == '\n')return(i);
  return(len);
}
/************************************************************************/
private void hcheckbuf_c(item,next,iostat)
ITEM *item;
int next,*iostat;
/*
  Check to determine that we have adequate buffer space, and a file,
  if needed.
------------------------------------------------------------------------*/
{
  char *s,path[MAXPATH];
  TREE *t;

  *iostat = 0;
/* Allocate a small buffer if needed. */

  if(item->bsize < next && next <= CACHESIZE){
    s = Malloc(CACHESIZE);
    item->bsize = CACHESIZE;
    if(item->io[0].length > 0)Memcpy(s,item->io[0].buf,item->io[0].length);
    if(item->io[0].buf != NULL) free(item->io[0].buf);
    item->io[0].buf = s;

/* Allocate full sized buffers if needed. */

  } else if(item->bsize <= CACHESIZE && next > CACHESIZE){
    s = Malloc(BUFSIZE);
    item->bsize = BUFSIZE;
    if(item->io[0].length > 0)Memcpy(s,item->io[0].buf,item->io[0].length);
    if(item->io[0].buf != NULL) free(item->io[0].buf);
    item->io[0].buf = s;
    if(BUFDBUFF)item->io[1].buf = Malloc(BUFSIZE);
  }

/* Open a file if needed. */

  if(item->fd == 0 && item->bsize > CACHESIZE && !(item->flags & ITEM_NOCACHE)){
    t = item->tree;
    if(item->flags & ITEM_CACHE) t->flags |= TREE_CACHEMOD;
    item->flags &= ~ITEM_CACHE;
    Strcpy(path,t->name);
    Strcat(path,item->name);
    dopen_c(&(item->fd),path,"write",&(item->size),iostat);
    if(*iostat == 0) t->rdwr = RDWR_RDWR;
    else	     t->rdwr = RDWR_RDONLY;
    t->wriostat = *iostat;
  }
}
/************************************************************************/
private void hwrite_fill_c(item,iob,next,iostat)
ITEM *item;
IOB *iob;
int next,*iostat;
/*
  A nonaligned nonsequential write operation has been requested. Read in the
  portion that we are missing. We need to fill the i/o buffer up to at
  least offset - 1.

  Inputs:
    item	Descriptor of the thingo we are reading in.
    iob		Structure of the i/o buffer.
    next	Fill up to at least byte (next-1).

  Output:
    iostat	I/O status.
------------------------------------------------------------------------*/
{
  char buffer[BUFSIZE];
  int offset,length;

  offset = BUFALIGN * ((iob->offset + iob->length) / BUFALIGN);
  length = BUFALIGN * ((next-1)/BUFALIGN + 1) - offset;
  length = min(length, item->size - offset);

  WAIT(item,iostat);					if(*iostat)return;
  dread_c(item->fd,buffer,offset,length,iostat);	if(*iostat)return;
  dwait_c(item->fd,iostat);				if(*iostat)return;
  offset = iob->offset + iob->length - offset;
  length -= offset;
  Memcpy(iob->buf+iob->length,buffer+offset,length);
  iob->length += length;
}
/************************************************************************/
void hseek_c(ihandle,offset)
int ihandle;
int offset;
/**hseek -- Set default offset (in bytes) of an item. 			*/
/*&mjs									*/
/*:low-level-i/o							*/
/*+ FORTRAN call sequence

	integer function hseek(itno,offset)
	integer itno,offset

  This sets the default access point of an item. This Can be used to
  reposition an item when reading/writing using hreada/hwritea.

  Input:
    itno	The handle of the item of interest.
    offset	The new offset.						*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  ITEM *item;

  item = hget_item(ihandle);
  item->offset = offset;
}
/************************************************************************/
int htell_c(ihandle)
int ihandle;
/**htell -- Return the default offset (in bytes) of an item.		*/
/*&mjs									*/
/*:low-level-i/o							*/
/*+ FORTRAN call sequence

	integer function htell(itno)
	integer itno

  This returns the current default offset of an item, which is used
  when reading/writing using hreada/hwritea.

  Input:
    itno	The handle of the item of interest.			*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  ITEM *item;

  item = hget_item(ihandle);
  return(item->offset);
}
/************************************************************************/
void hreada_c(ihandle,line,length,iostat)
int ihandle;
int length,*iostat;
char *line;
/*----------------------------------------------------------------------*/
{
  ITEM *item;

  item = hget_item(ihandle);
  hio_c( ihandle, FALSE, H_TXT, line, item->offset, length, iostat);
}
/************************************************************************/
void hwritea_c(ihandle,line,length,iostat)
int ihandle;
int length,*iostat;
char *line;
/*----------------------------------------------------------------------*/
{
  ITEM *item;

  item = hget_item(ihandle);
  hio_c( ihandle ,TRUE, H_TXT, line, item->offset, length, iostat);
}
/************************************************************************/
private void hcache_create_c(t,iostat)
int *iostat;
TREE *t;
/*
  Create a cache.
------------------------------------------------------------------------*/
{
  int ihandle;
  header_ok = TRUE;
  haccess_c(t->handle,&ihandle,"header","write",iostat);
  header_ok = FALSE;
  if(!*iostat) hdaccess_c(ihandle,iostat);
}
/************************************************************************/
private void hcache_read_c(t,iostat)
int *iostat;
TREE *t;
/*
  Read in all small items, which are stored in the file "header".
  Errors should never happen when reading the cache. If they do,
  abort completely.
------------------------------------------------------------------------*/
{
  int offset;
  ITEM *item;
  char s[CACHE_ENT];
  int ihandle;

  header_ok = TRUE;
  haccess_c(t->handle,&ihandle,"header","read",iostat);
  header_ok = FALSE;						if(*iostat)return;

  offset = 0;
  while(hreadb_c(ihandle,s,offset,CACHE_ENT,iostat),!*iostat){
    offset += CACHE_ENT;
    item = hcreate_item_c(t,s);
    item->size = *(s+CACHE_ENT-1);
    item->bsize = item->size;
    item->flags = ITEM_CACHE;
    item->io[0].offset = 0;
    item->io[0].length = item->size;
    item->io[0].state = IO_VALID;
    item->io[0].buf = Malloc(item->size);
    hreadb_c(ihandle,item->io[0].buf,offset,item->size,iostat);	check(*iostat);
    offset += mroundup(item->size,CACHE_ENT);
  }
  if(*iostat != -1) bug_c('f',"hcache_read_c: Something wrong reading cache");
  hdaccess_c(ihandle,iostat);
}
/************************************************************************/
private int hname_check(name)
char *name;
/*
  This checks if the name of an item is OK. Generally an item must be 1 to
  8 characters, alphanumeric, starting with an alpha. Only lower case
  alpha is allowed. The name "header" is generally reserved.
------------------------------------------------------------------------*/
{
  int length,i;
  char c;

  length = strlen(name);
  if(length <= 0 || length >= MAXNAME) return(-1);
  if(length == 1 && *name == '.')return(0);
  if(*name < 'a' || *name > 'z')return(-1);
  if(!header_ok && length == 6 && !strcmp("header",name))return(-1);
  for(i=0; i < length; i++){
    c = *name++;
    if((c < 'a' || c > 'z') && (c < '0' || c > '9') && (c != '-'))return(-1);
  }
  return(0);
}
/************************************************************************/
private void hdir_c(item)
ITEM *item;
/*
  Read the directory contents into a buffer (make it look like a text
  file.
------------------------------------------------------------------------*/
{
  int length,plength,len;
  char *context,*s;
  ITEM *it;
  TREE *t;

#define MINLENGTH 128

/* Mark this item as not cachable. */

  item->flags |= ITEM_NOCACHE | ITEM_SCRATCH;

/* Get a buffer size which is guaranteed to hold all the items that come
   from the "header" file. */

  plength = 0;
  t = item->tree;
  for(it = t->itemlist; it != NULL; it = it->fwd)
    plength += strlen(it->name) + 1;
  plength = max(plength,2*MINLENGTH);
  s = Malloc(plength);

/* Copy the names of all the "header" items to this buffer. Exclude the "."
   itself. */

  length = 0;
  for(it=t->itemlist; it != NULL; it = it->fwd){
    if(it->fd == 0 && !(it->flags & ITEM_NOCACHE)){
      Strcpy(s+length,it->name);
      length += strlen(it->name);
      *(s+length++) = '\n';
    }
  }

/* Now read through the directory to get all external files. Skip the
   "header" file. The size of the buffer is doubled as we go, when it
   gets too small. */

  dopendir_c(&context,t->name);
  do{
    if(plength - length < MINLENGTH){
      plength *= 2;
      s = Realloc(s, plength);
    }
    dreaddir_c(context,s+length,plength-length);
    len = strlen(s+length);
    if(len > 0 && strcmp(s+length,"header")){
      length += len;
      *(s+length++) = '\n';
    }
  }while(len > 0);
  dclosedir_c(context);

/* Finish initialising the item now. */

  item->size = length;
  item->io[0].buf = s;
  item->io[0].offset = 0;
  item->io[0].length = length;
  item->bsize = plength;
}
/************************************************************************/
private void hrelease_item_c(item)
ITEM *item;
/*
  Release the item on the top of the list.
------------------------------------------------------------------------*/
{
  ITEM *it1,*it2;
  TREE *t;

/* Find the item. Less than attractive code. */

  t = item->tree;
  it2 = t->itemlist;
  if(item != it2){
    do{
      it1 = it2;
      it2 = it1->fwd;
    }while(item != it2);

    it1->fwd = it2->fwd;
  } else t->itemlist = item->fwd;

/* Release any memory associated with the item. */

  if(item->io[0].buf != NULL) free(item->io[0].buf);
  if(item->io[1].buf != NULL) free(item->io[1].buf);

  item_addr[item->handle] = NULL;
  free(item->name);
  free((char *)item);
  nitem--;
}
/************************************************************************/
private ITEM *hcreate_item_c(tree,name)
TREE *tree;
char *name;
/*
  Create an item, and initialise as much of it as possible.
------------------------------------------------------------------------*/
{
  ITEM *item;
  int i,hash;
  char *s;

/* Hash the name. */

  s = name;
  hash = nitem++;
  if(nitem > MAXITEM)bug_c('f',"Item address table overflow, in hio");
  while(*s) hash += *s++;
  hash %= MAXITEM;

/* Find a slot in the list of addresses, and allocate it. */

  while(hget_item(hash) != NULL) hash = (hash+1) % MAXITEM;
  item_addr[hash] = (ITEM *)Malloc(sizeof(ITEM));

/* Initialise it now. */

  item = hget_item(hash);
  item->name = Malloc(strlen(name) + 1);
  Strcpy(item->name,name);
  item->handle = hash;
  item->size = 0;
  item->flags = 0;
  item->fd = 0;
  item->last = 0;
  item->offset = 0;
  item->bsize = 0;
  item->tree = tree;
  for(i=0; i<2; i++){
    item->io[i].offset = 0;
    item->io[i].length = 0;
    item->io[i].state = 0;
    item->io[i].buf = NULL;
   }
  item->fwd = tree->itemlist;
  tree->itemlist = item;
  return(item);
}
/************************************************************************/
private TREE *hcreate_tree_c(name)
char *name;
/*
  Create an item, and initialise as much of it as possible.
------------------------------------------------------------------------*/
{
  TREE *t;
  int hash;
  char *s;

/* Hash the name. */

  s = name;
  hash = ntree++;
  if(ntree > MAXOPEN)bug_c('f',"Tree address table overflow, in hio");
  while(*s) hash += *s++;
  hash %= MAXOPEN;

/* Find a slot in the list of addresses, and allocate it. */

  while(hget_tree(hash) != NULL) hash = (hash+1) % MAXOPEN;
  tree_addr[hash] = (TREE *)Malloc(sizeof(TREE));

/* Initialise it. */

  t = hget_tree(hash);
  t->name = Malloc(strlen(name) + 1);
  Strcpy(t->name,name);
  t->handle = hash;
  t->flags = 0;
  t->itemlist = NULL;
  return(t);
}
