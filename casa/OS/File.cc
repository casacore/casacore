//# File.cc: Class to define a File
//# Copyright (C) 1993,1994,1995,1996,1997,2001,2003
//# Associated Universities, Inc. Washington DC, USA.
//# 
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//# 
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//# 
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$


#include <casacore/casa/OS/Path.h>
#include <casacore/casa/OS/File.h>
#include <casacore/casa/OS/SymLink.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions.h>
#include <casacore/casa/Logging/LogIO.h>
#include <unistd.h>                 // needed for access, etc.
#include <sys/stat.h>               // needed for lstat or lstat64
#include <utime.h>                  // needed for utimbuf
#include <errno.h>                  // needed for errno
#include <casacore/casa/string.h>                 // needed for strerror
#include <casacore/casa/stdio.h>                  // needed for sprintf
#include <time.h>                   // needed for asctime/localtime on linux


namespace casacore { //# NAMESPACE CASACORE - BEGIN

uInt File::uniqueSeqnr_p = 0;      // Initialization
Mutex File::theirMutex;


File::File () 
{
    // Sets itsPath on the current working directory
}

File::File (const Path& path) 
: itsPath (path)
{}

File::File (const String& string)
: itsPath (string)
{}

File::File (const File& that)
: itsPath (that.itsPath)
{}

File::~File()
{}
    
File& File::operator= (const File& that)
{
    if (this != &that) {
	itsPath = that.itsPath;
    }
    return *this;
}

Bool File::isRegular (Bool followSymLink) const
{
    // The struct is filled in by mylstat, and S_ISREG checks buf
    // if the file is a regularfile.
    Path testPath = itsPath;
    if (isSymLink()) {
	if (! followSymLink) {
	    return False;
	}
	testPath = SymLink(itsPath).followSymLink();
    }
    struct fileSTAT buf;
    if (mylstat (testPath.expandedName().chars(), &buf) < 0) {
	return False;
    }
    return  (S_ISREG (buf.st_mode));
}

Bool File::isDirectory (Bool followSymLink) const
{
    // The struct is filled in by mylstat, and S_ISDIR checks buf
    // if the file is a directory.
    Path testPath = itsPath;
    if (isSymLink()) {
	if (! followSymLink) {
	    return False;
	}
	testPath = SymLink(itsPath).followSymLink();
    }
    struct fileSTAT buf;
    if (mylstat (testPath.expandedName().chars(), &buf) < 0) {
	return False;
    }
    return  (S_ISDIR (buf.st_mode));
}

Bool File::isSymLink() const
{
    // The struct is filled in by mylstat, and S_ISLNK checks buf
    // if the file is a symbolic link.
    struct fileSTAT buf;
    if (mylstat (itsPath.expandedName().chars(), &buf) < 0) {
	return False;
    }
    return  (S_ISLNK (buf.st_mode));
}

Bool File::isPipe() const
{
    // The struct is filled in by mylstat, and S_ISFIFO checks buf
    // if the file is a pipe.
    struct fileSTAT buf;
    getstat (&buf);
    return (S_ISFIFO (buf.st_mode)); 
}

Bool File::isCharacterSpecial() const
{
    // The struct is filled in by mylstat, and S_ISCHR checks buf
    // if the file is a characterspecialfile.
    struct fileSTAT buf;
    getstat (&buf);
    return (S_ISCHR (buf.st_mode));
}

Bool File::isBlockSpecial() const
{
    // The struct is filled in by mylstat, and S_ISBLK checks buf
    // if the file is a blokspecialfile.
    struct fileSTAT buf;
    getstat (&buf);
    return (S_ISBLK (buf.st_mode));
}

Bool File::isSocket() const
{
    // The struct is filled in by mylstat, and S_ISSOCK checks buf
    // if the file is a socket.
    struct fileSTAT buf;
    getstat (&buf);
    return (S_ISSOCK (buf.st_mode));
}

Bool File::exists() const
{
    // The function access always substitutes symlinks.
    // Therefore use lstat instead.
    struct fileSTAT buf;
    int status = mylstat((itsPath.expandedName()).chars(), &buf);
    if (status != 0 && errno != ENOENT){
        LogIO logIo (LogOrigin ("File", "exists"));
        logIo << LogIO::WARN;
        logIo << "lstat failed for " << itsPath.expandedName()
              << ": errno=" << errno << "'" << strerror (errno)
              << "'\n";
        logIo << LogIO::POST;
    }
    return status == 0;
}

Bool File::isReadable() const
{
    // The function access checks if the file is readable.
    return (access ((itsPath.expandedName()).chars(), R_OK)==0);
}

Bool File::isWritable() const
{
    // The function access checks if the file is writable.
    return (access ((itsPath.expandedName()).chars(), W_OK)==0);
}

Bool File::isExecutable() const
{
    // The function access checks if the file is executable.
    return (access ((itsPath.expandedName()).chars(), X_OK)==0);
}

Bool File::canCreate() const
{
    // Checks if the dirname of a file is a writable and executable
    // directory.
    File dir(itsPath.dirName());
    if (dir.isDirectory()  &&  dir.isWritable()  &&  dir.isExecutable()) {
	return True;
    }
    return False;
}

long File::userID() const
{
    // Returns the userid of a file which is extracted from the struct
    // buf.
    struct fileSTAT buf;
    getstat (&buf);
    return buf.st_uid;
}

long File::groupID() const
{
    // Returns the groupid of a file which is extracted from the struct
    // buf.
    struct fileSTAT buf;
    getstat (&buf);
    return buf.st_gid;
}

Int64 File::size() const
{
    // The struct buf is filled in by mylstat, and the size 
    // of the file is extracted from buf.
    struct fileSTAT buf;
    getstat (&buf);
    return buf.st_size;
}
    
uInt File::readPermissions() const
{
    // Returns the permissions as a decimal value. The value 
    // is extracted from buf.
    struct fileSTAT buf;
    getstat (&buf);
    return (uInt (buf.st_mode & 07) +
           (uInt (buf.st_mode &070) >> 3) * 10 +
           (uInt (buf.st_mode &0700) >> 6) * 100 );
}

void File::setPermissions(uInt permissions)
{
    // Changes the permissions by using chmod, the value must be 
    // an octal value.
    chmod ((itsPath.expandedName()).chars(),long (permissions));
}


Path File::newUniqueName (const String& directory, const String& prefix)
{
    // create an new unique name 
    char str[32];
    // fill str with the pid and the unique number
    uInt seqnr;
    {
      ScopedMutexLock lock(theirMutex); 
      seqnr = uniqueSeqnr_p++;
    }
    sprintf (str, "%i_%i", Int(getpid()), seqnr);
    if (directory.empty()  ||  directory.lastchar() == '/') {
	return Path (directory + prefix + str);
    }
    // a slash is added when a directory is given and the last
    // character is a slash.
    return Path (directory + "/" + prefix + str);
}

Path File::newUniqueName (const String& directory)
{
    // Uses the function newUniqueName with an empty prefix
    return newUniqueName (directory, "");
}

void File::touch(uInt time)
{
    // Uses the function utime to set the access time and the
    // modification time. 
    utimbuf times;
    times.actime = time;
    times.modtime = time;
    if (isWritable()) {
	utime ((itsPath.expandedName()).chars(), &times);
    }
}

void File::touch()
{
    // Uses the function utime to set the access time and the
    // modification time on the current time. 
    if (isWritable()) {
	utime ((itsPath.expandedName()).chars(), 0); 
    }
}

uInt File::accessTime () const
{
    // The struct is filled in by mylstat, and the accesstime 
    // is returned.
    struct fileSTAT buf;
    getstat (&buf);
    return buf.st_atime;
}

String File::accessTimeString () const
{
    // The struct is filled in by mylstat, and the accesstime 
    // is returned as a string.
    struct fileSTAT buf;
    getstat (&buf);
    return String (asctime (localtime (&buf.st_atime)));

}

uInt File::modifyTime () const
{
    // The struct is filled in by mylstat, and the modificationtime 
    // is returned.
    struct fileSTAT buf;
    getstat (&buf);
    return buf.st_mtime;
}


String File::modifyTimeString () const
{
    // The struct is filled in by mylstat, and the modificationtime 
    // is returned as a string.    
    struct fileSTAT buf;
    getstat (&buf);
    return String (asctime (localtime (&buf.st_mtime)));
}

File::FileWriteStatus File::getWriteStatus() const
{
  if (exists()) {
    return (isWritable()  ?  OVERWRITABLE : NOT_OVERWRITABLE);
  }
  return (canCreate()  ?  CREATABLE : NOT_CREATABLE);
}


uInt File::statusChangeTime () const
{
    // The struct is filled in by mylstat, and the statusChangetime 
    // is returned.
    struct fileSTAT buf;
    getstat (&buf);
    return buf.st_ctime;
}

String File::statusChangeTimeString() const
{
    // The struct is filled in by mylstat, and the statusChangetime 
    // is return from buf as a string.    
    struct fileSTAT buf;
    getstat (&buf);
    return String (asctime (localtime (&buf.st_ctime)));
}


void File::removeSymLinks ()
{
    // Remove the chain of symlinks starting with this one.
    File next(*this);
    while (next.isSymLink()) {
	SymLink symLink(next);
	next = symLink.readSymLink();
	symLink.remove();
    }
}

int File::mylstat(const char* path, void* buf) const
{
    return fileLSTAT (const_cast<char*>(path),
		      static_cast<struct fileSTAT*>(buf));
}

void File::getstat (const File& file, void* buf) const
{
    if (mylstat (file.path().expandedName().chars(), buf) < 0) {
	throw (AipsError ("File::getstat error on " +
			  file.path().expandedName() +
			  ": " + strerror(errno)));
    }
}

void File::checkTarget (Path& targetName, Bool overwrite,
			Bool forDirectory) const
{
    // Determine the target directory and file.
    // When the target is a directory, the basename is copied from the source.
    // Otherwise the target directory is the dirname of the target.
    Path targetDir;
    File target(targetName);
    if (!forDirectory  &&  target.isDirectory()) {
	targetDir = targetName;
	targetName.append (path().baseName());
    } else {
	targetDir = targetName.dirName();
    }
    // Check if the target directory is writable.
    target = targetDir;
    if (! target.isWritable()) {
	throw (AipsError ("RegularFile::copy/move: target directory " +
			  targetDir.originalName() + " is not writable"));
    }
    // Check if the target file exists, is writable and should be overwritten.
    target = targetName;
    if (target.exists()) {
	if (! overwrite) {
	    throw (AipsError ("RegularFile::copy/move: target file " +
			      targetName.originalName() + " already exists"));
	}
	if (! target.isWritable()) {
	    throw (AipsError ("RegularFile::copy/move: target file " +
			      targetName.originalName() +
			      " already exists and is not writable"));
	}
    }
}


#ifdef AIPS_DARWIN
#include <sys/param.h>
#include <sys/mount.h>
#else
#include <sys/vfs.h>
#endif


String File::getFSType() const
{
	String rstat("Normal");
	struct fileSTATFS  statbuf;
        fileSTATFS(itsPath.dirName().chars(), &statbuf);
#ifdef AIPS_DARWIN
	rstat = String(statbuf.f_fstypename);
#else
	if(statbuf.f_type == 0x0BD00BD0)
	   rstat = "Lustre";
#endif
	return rstat;
}

} //# NAMESPACE CASACORE - END

