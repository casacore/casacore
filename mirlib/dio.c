/************************************************************************/
/*	DIO  -- Disk I/O routines for a Unix Enviromment.		*/
/*									*/
/*	Makes calls to the UNIX I/O and directory searching routines.	*/
/*	All of these get implemented in a pretty straight forward way.	*/
/*									*/
/*	Portability Notes:						*/
/*	These routines are intended to run on BSD UNIX and UNICOS. No	*/
/*	attempt has been made to make them any more portable than this.	*/
/*	There are some minor differences between the two, which are 	*/
/*	selectively compiled depending if BSD is defined.		*/
/*	1. The mkdir system service is not present on some systems, and	*/
/*	   may require superuser priveleges to implement using mknod.	*/
/*	   In this case, use 'popen("mkdir ...","r",...)'		*/
/*	2. The Berkeley directory searching routines are used. These	*/
/*	   can be relatively simply implemented in other UNIX's.	*/
/*									*/
/*	History:                                                        */
/*      dakr-ages  rjs Original version adapted from werong.		*/
/*	31-oct-89  pjt	_trace_ added as defined() option, errno	*/
/*        -nov-89  rjs dexpand_c routine				*/
/*       6-dec-89  pjt extended bug call				*/
/*      26-jan-90  rjs Reincluded <stdio.h>, which is needed by Unicos.	*/
/*	27-apr-90  rjs Added ddelete_c routine.				*/
/*      26-aug-93  rjs Added hrmdir.					*/
/*	 5-nov-94  rjs Improve POSIX compliance.			*/
/*	26-Oct-95  rjs Honour TMPDIR environment variable, if set.	*/
/*	10-Jan-96  rjs Make sure scratch file names are unique.		*/
/*      17-jun-02  pjt MIR4 changes, and proper prototypes              */
/*	 5-nov-04  jwr Changed a few size_t to ssize_t or off_t		*/
/*       3-jan-05  pjt ssize casting to appease the compiler            */
/*                     use SSIZE_MAX to protect from bad casting ?      */
/*       2-mar-05  pjt template->templat for C++, just in case          */
/*      02-dec-11 pkgw Fix semantics of I/O syscalls in dread, dwrite   */
/************************************************************************/

#if defined(HAVE_CONFIG_H) && HAVE_CONFIG_H
#include "config.h"
#endif

#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <limits.h>
#include <unistd.h>
#include <fcntl.h>
#include <dirent.h>
#define direct dirent
#include <stdio.h>
#include <errno.h>

#include "miriad.h"

#define MAXPATH 128

#ifndef NULL
#  define NULL 0
#endif

#define Malloc(x) malloc((unsigned)(x))
#define Strcat (void)strcat
#define Strcpy (void)strcpy
#define Lseek(a,b,c) (off_t)lseek(a,(off_t)(b),c)

struct dent { 
  char path[MAXPATH];
  DIR *dir;
};
/************************************************************************/
void ddelete_c(char *path,int *iostat)
/*
  This deletes a file, and returns an i/o status.
------------------------------------------------------------------------*/
{
  *iostat = ( unlink(path) ? errno : 0 );
}
/************************************************************************/
void dtrans_c(char *inpath,char *outpath,int *iostat)
/*
  Translate a directory spec into the local format. On a UNIX machine,
  this merely involves adding a slash to the end of the name.

  Input:
    inpath	Input directory spec.
  Output:
    outpath	Output directory spec.
    iostat	Error return.
------------------------------------------------------------------------*/
{
  char *s;

  *iostat = 0;
  Strcpy(outpath,inpath);
  s = outpath + strlen(outpath) - 1;
  if(*s != '/')Strcat(outpath,"/");
}
/************************************************************************/
void dmkdir_c(char *path,int *iostat)
/*
  Create a directory. This might be a privileged operation on some systems,
  in which case dmkdir_c will have to work by using popen(3) and mkdir(1).

  Input:
    path	Name of directory to create. This will usually have a
		trailing slash, which needs to be trimmed off.
  Output:
    iostat	Errror status.
------------------------------------------------------------------------*/
{
  char Path[MAXPATH],*s;

/* Usually the path will end in a '/', so get rid of it. */

  Strcpy(Path,path);
  s = Path + strlen(Path) - 1;
  if(*s == '/')*s = 0;

  *iostat = 0;
  if(mkdir(Path,0777) < 0) *iostat = errno;
}
/************************************************************************/
void drmdir_c(char *path,int *iostat)
/*
  Delete a directory. This might be a privileged operation on some systems,
  in which case drmdir_c will have to work by using popen(3) and rmdir(1).

  Input:
    path	Name of directory to remove. This will usually have a
		trailing slash, which needs to be trimmed off.
  Output:
    iostat	Errror status.
------------------------------------------------------------------------*/
{
  char Path[MAXPATH],*s;

/* Usually the path will end in a '/', so get rid of it. */

  Strcpy(Path,path);
  s = Path + strlen(Path) - 1;
  if(*s == '/')*s = 0;

  *iostat = 0;
  if(rmdir(Path) < 0) *iostat = errno;
}
/************************************************************************/
void dopen_c(int *fd,char *name,char *status,off_t *size,int *iostat)
/*
  Open a file.
  Input:
    name	Name of file to create (in host format).
    status	Either "read", "write", "append" or "scratch".
                "scratch" files are using $TMPDIR, if present, else current.

  Output:
    fd		File descriptor.
    size	Size of file.
    iostat	I/O status.

------------------------------------------------------------------------*/
{
  int is_scratch,pid,flags=0;
  char *s,sname[MAXPATH];

  is_scratch = *iostat = 0;
  s = name;

  if     (!strcmp(status,"read"))    flags = O_RDONLY;
  else if(!strcmp(status,"write"))   flags = O_CREAT|O_TRUNC|O_RDWR;
  else if(!strcmp(status,"append"))  flags = O_CREAT|O_RDWR;
  else if(!strcmp(status,"scratch")){
    flags = O_CREAT|O_TRUNC|O_RDWR;
    is_scratch = 1;
    s = getenv("TMPDIR");
    pid = getpid();
    if(s != NULL)sprintf(sname,"%s/%s.%d",s,name,pid);
    else         sprintf(sname,"%s.%d",name,pid);
    s = sname;
  } else bug_c('f',"dopen_c: Unrecognised status");
#ifdef O_LARGEFILE
  flags |= O_LARGEFILE;
#endif
  if((*fd = open(s,flags,0644)) < 0){*iostat = errno; return;}
  *size = Lseek(*fd,0,SEEK_END);

/* If its a scratch file, unlink it now, so that the file will disappear
   when it is closed (or this program crashes). */

  if(is_scratch)(void)unlink(s);
}
/************************************************************************/
void dclose_c(int fd,int *iostat)
/*
  This subroutine does unbelievably complex stuff.
------------------------------------------------------------------------*/
{
  *iostat = ( close(fd) < 0 ? errno : 0 );
}
/************************************************************************/
void dread_c(int fd, char *buffer,off_t offset,size_t length,int *iostat)
/*
  Read from a file.
------------------------------------------------------------------------*/
{
  ssize_t nread;
#ifdef debug
  if (length >= SSIZE_MAX) bugv_c('f',"dread_c: possible incomplete read");
#endif
  if(Lseek(fd,offset,SEEK_SET) < 0) { *iostat = errno; return; }

  while (length) {
    nread = read(fd,buffer,length);
    if(nread < 0) {
      if(errno == EINTR)
	nread = 0; /* should reattempt the system call identically */
      else {
	*iostat = errno;
	return;
      }
    } else if(nread == 0) {
      /* unexpected EOF -- no good errno code for this */
      *iostat = EIO;
      return;
    }
    length -= nread;
  }
}
/************************************************************************/
void dwrite_c(int fd, char *buffer,off_t offset,size_t length,int *iostat)
/*
  Write to a file.
------------------------------------------------------------------------*/
{
  ssize_t nwrite;
#ifdef debug
  if (length >= SSIZE_MAX) bugv_c('f',"dwrite_c: possible incomplete write");
#endif
  if(Lseek(fd,offset,SEEK_SET) < 0) { *iostat = errno; return; }

  while (length) {
    nwrite = write(fd,buffer,length);
    if(nwrite < 0) {
      if(errno == EINTR)
	nwrite = 0; /* should reattempt the system call identically */
      else {
	*iostat = errno;
	return;
      }
    }
    length -= nwrite;
  }
}
/************************************************************************/
/*ARGSUSED*/
void dwait_c(int fd,int *iostat)
/*
  This nominally waits for i/o to a file to finish. Things work synchronously
  in UNIX.
------------------------------------------------------------------------*/
{
  *iostat = 0;
}
/************************************************************************/
int dexpand_c(char *templat,char *output,int length)
/*
  This expands wildcards, matching them with files.

  Input:
    templat	The input character string, containing the wildcards.
    length	The length of the output buffer.
  Output:
    output	All the files matching "template". Filenames are separated
		by commas.
------------------------------------------------------------------------*/
{
  FILE *fd;
  char line[MAXPATH],*s;
  int l;

  Strcpy(line,"echo ");
  Strcat(line,templat);
  fd = popen(line,"r");
  if(fd == NULL) return(-1);
  s = output;
  while(fgets(s,length,fd)){
    l = strlen(s);
    if( length-l <= 1 ){(void)pclose(fd); return(-1);}
    *(s+l-1) = ',';
    s += l;
    length -= l;
  }
  if(s != output) *--s = 0;
  (void)pclose(fd);
  return(s-output);
}
/************************************************************************/
void dopendir_c(char **contxt,char *path)
/*
  Open a directory, and prepare to read from it.
------------------------------------------------------------------------*/
{
  struct dent *d;

  *contxt = Malloc(sizeof(struct dent));
  d = (struct dent *)*contxt;
  Strcpy(d->path,path);
  d->dir = opendir(path);
}
/************************************************************************/
void dclosedir_c(char *contxt)
/*
  Close a directory.
------------------------------------------------------------------------*/
{
  struct dent *d;
  d = (struct dent *)contxt;
  (void)closedir(d->dir);
  free(contxt);
}
/************************************************************************/
/*ARGSUSED*/
void dreaddir_c(char *contxt,char *path,int length)
/*
  Read a directory entry.
------------------------------------------------------------------------*/
{
  struct dent *d;
  struct direct *dp;
  struct stat buf;
  char npath[MAXPATH];

  d = (struct dent *)contxt;

  do dp = readdir(d->dir);
  while(dp != NULL && (!strcmp(dp->d_name,".") || !strcmp(dp->d_name,"..")));

  if(dp == NULL)
    *path = 0;
  else{
    Strcpy(path,dp->d_name);
    Strcpy(npath,d->path);    Strcat(npath,path);
    (void)stat(npath,&buf);
    if(S_IFDIR & buf.st_mode)Strcat(path,"/");
  }
}
