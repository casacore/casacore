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
/************************************************************************/

#include <sys/types.h>
#include <sys/stat.h>
#include <stddef.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <dirent.h>
#define direct dirent
#ifndef NULL
#  define NULL 0
#endif
#include <stdio.h>
#if defined(_trace_)
  extern int errno;
#endif
#include <errno.h>

#define MAXPATH 128

#define Malloc(x) malloc((unsigned)(x))
#define Strcat (void)strcat
#define Strcpy (void)strcpy
#define Lseek(a,b,c) (int)lseek(a,(off_t)(b),c)

struct dent { char path[MAXPATH];
		DIR *dir;};
/************************************************************************/
void ddelete_c(path,iostat)
char *path;
int *iostat;
/*
  This deletes a file, and returns an i/o status.
------------------------------------------------------------------------*/
{
  *iostat = ( unlink(path) ? errno : 0 );
}
/************************************************************************/
void dtrans_c(inpath,outpath,iostat)
char *inpath,*outpath;
int *iostat;
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
void dmkdir_c(path,iostat)
char *path;
int *iostat;
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
void drmdir_c(path,iostat)
char *path;
int *iostat;
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
void dopen_c(fd,name,status,size,iostat)
int *fd,*size,*iostat;
char *name,*status;
/*
  Open a file.
  Input:
    name	Name of file to create (in host format).
    status	Either "read", "write", "append" or "scratch".

  Output:
    fd		File descriptor.
    size	Size of file.
    iostat	I/O status.

------------------------------------------------------------------------*/
{
  int flags,is_scratch,pid;
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

  if((*fd = open(s,flags,0644)) < 0){*iostat = errno; return;}
  *size = Lseek(*fd,0,SEEK_END);

/* If its a scratch file, unlink it now, so that the file will disappear
   when it is closed (or this program crashes). */

  if(is_scratch)(void)unlink(s);
}
/************************************************************************/
void dclose_c(fd,iostat)
int fd,*iostat;
/*
  This subroutine does unbelievably complex stuff.
------------------------------------------------------------------------*/
{
  *iostat = ( close(fd) < 0 ? errno : 0 );
}
/************************************************************************/
void dread_c(fd,buffer,offset,length,iostat)
int fd,offset,length,*iostat;
char *buffer;
/*
  Read from a file.
------------------------------------------------------------------------*/
{
  int nread;

  if(Lseek(fd,offset,SEEK_SET) < 0) { *iostat = errno; return; }
  nread = read(fd,buffer,length);
  if(nread < 0) *iostat = errno; 
  else if(nread != length) *iostat = EIO;
}
/************************************************************************/
void dwrite_c(fd,buffer,offset,length,iostat)
int fd,offset,length,*iostat;
char *buffer;
/*
  Write to a file.
------------------------------------------------------------------------*/
{
  int nwrite;

  if(Lseek(fd,offset,SEEK_SET) < 0) { *iostat = errno; return; }
  nwrite = write(fd,buffer,length);
  if(nwrite < 0) *iostat = errno; 
  else if(nwrite != length) *iostat = EIO;
}
/************************************************************************/
/*ARGSUSED*/
void dwait_c(fd,iostat)
int fd,*iostat;
/*
  This nominally waits for i/o to a file to finish. Things work synchronously
  in UNIX.
------------------------------------------------------------------------*/
{
  *iostat = 0;
}
/************************************************************************/
int dexpand_c(template,output,length)
char *template,*output;
int length;
/*
  This expands wildcards, matching them with files.

  Input:
    template	The input character string, containing the wildcards.
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
  Strcat(line,template);
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
void dopendir_c(contxt,path)
char **contxt,*path;
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
void dclosedir_c(contxt)
char *contxt;
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
void dreaddir_c(contxt,path,length)
char *contxt,*path;
int length;
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
