//# RegularFile.cc: Manipulate and get information about regular files
//# Copyright (C) 1993,1994,1995,1996,1997,2001,2002,2003
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


#include <casacore/casa/Exceptions.h>
#include <casacore/casa/OS/RegularFile.h>
#include <casacore/casa/OS/SymLink.h>
#include <casacore/casa/IO/FiledesIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>

#include <fcntl.h>                // needed for creat
#include <unistd.h>               // needed for unlink, etc.
#include <errno.h>                // needed for errno
#include <casacore/casa/string.h>          // needed for strerror
#include <casacore/casa/stdlib.h>          // needed for system


namespace casacore { //# NAMESPACE CASACORE - BEGIN

RegularFile::RegularFile ()
: File()
{}

RegularFile::RegularFile (const Path& path)
: File(path)
{
    checkPath();
}

RegularFile::RegularFile (const String& path)
: File(path)
{
    checkPath();
}

RegularFile::RegularFile (const File& file)
: File(file)
{
    checkPath();
}
    
RegularFile::RegularFile (const RegularFile& that)
: File    (that),
  itsFile (that.itsFile)
{}

RegularFile::~RegularFile()
{}

RegularFile& RegularFile::operator= (const RegularFile& that)
{
    if (this != &that) {
	File::operator= (that);
	itsFile = that.itsFile;
    }
    return *this;
}

void RegularFile::checkPath()
{
    itsFile = *this;
    // If exists, check if it is a regular file.
    // If the file is a symlink, resolve the entire symlink chain.
    // Otherwise check if it can be created.
    if (exists()) {
	if (isSymLink()) {
	    itsFile = SymLink(*this).followSymLink();
	    // Error if no regular file and exists or cannot be created.
	    if (!itsFile.isRegular()) {
		if (itsFile.exists() || !itsFile.canCreate()) {
		    throw (AipsError ("RegularFile: " + path().expandedName()
				      + " is a symbolic link not"
				      " pointing to a valid regular file"));
		}
	    }
	} else if (!isRegular()) {
	    throw (AipsError ("RegularFile: " + path().expandedName() +
			      " exists, but is no regular file"));
	}
    } else {
	if (!canCreate()) {
	    throw (AipsError ("RegularFile: " + path().expandedName() +
			      " does not exist and cannot be created"));
	}
    }
}

void RegularFile::create (Bool overwrite) 
{
    // If overwrite is False the file will not be overwritten.
    if (exists()) {
	if (!itsFile.isRegular (False)) {
	    throw (AipsError ("RegularFile::create: " +
			      itsFile.path().expandedName() +
			      " already exists as a non-regular file"));
	}
	if (!overwrite) {
	    throw (AipsError ("RegularFile::create: " +
			      itsFile.path().expandedName() +
			      " already exists"));
	}
    }
    int fd = ::creat (itsFile.path().expandedName().chars(), 0666);
    if (fd < 0) {
	throw (AipsError ("RegularFile::create error on " +
			  itsFile.path().expandedName() +
			  ": " + strerror(errno)));
    }
    ::close (fd);
}

void RegularFile::remove() 
{
    if (isSymLink()) {
	removeSymLinks();
    }    
    unlink (itsFile.path().expandedName().chars());
}

void RegularFile::copy (const Path& target, Bool overwrite,
			Bool setUserWritePermission) const
{
    Path targetName(target);
    checkTarget (targetName, overwrite);
#if defined(AIPS_CRAY_PGI)
    manualCopy (itsFile.path().expandedName(), targetName.expandedName());
#else
    // This function uses the system function cp.	    
    String call("cp '");
    call += itsFile.path().expandedName() + "' '" +
            targetName.expandedName() + "'";
    AlwaysAssert (system(call.chars()) == 0, AipsError);
    if (setUserWritePermission) {
	File result(targetName.expandedName());
	if (! result.isWritable()) {
	    result.setPermissions (result.readPermissions() | 0200);
	}
    }
#endif
}

void RegularFile::manualCopy (const String& source, const String& target)
{
    int infd (FiledesIO::open (source.chars()));
    int outfd (FiledesIO::create (target.chars()));
    FiledesIO in (infd, source);
    FiledesIO out (outfd, target);
    char buf[32768];
    int nrc = in.read (sizeof(buf), buf, False);
    while (true) {
        AlwaysAssert (nrc >= 0, AipsError);
	out.write (nrc, buf);
	if (nrc != sizeof(buf)) {
	    break;
	}
	nrc = in.read (sizeof(buf), buf, False);
    }
    FiledesIO::close (infd);
    FiledesIO::close (outfd);
}

void RegularFile::move (const Path& target, Bool overwrite)
{
    Path targetPath(target);
    checkTarget (targetPath, overwrite);
    // Start trying to rename.
    // If source and target are the same directory, rename does nothing
    // and returns a success status.
    if (rename (path().expandedName().chars(),
		targetPath.expandedName().chars()) == 0) {
	return;
    
    }
    // The rename failed for one reason or another.
    // Remove the target if it already exists.
    Bool alrExist = False;
    if (errno == EEXIST) {
      alrExist = True;
    }
#if defined(EBUSY)
    if (errno == EBUSY) {
      alrExist = True;
    }
#endif
    if (alrExist) {
	unlink (targetPath.expandedName().chars());
    }
    // Try again.
    if (rename (path().expandedName().chars(),
		targetPath.expandedName().chars()) == 0) {
	return;
    }
    // Throw an exception if not "different file systems" error.
    if (errno != EXDEV) {
	throw (AipsError ("RegularFile::move error on " +
			  path().expandedName() + " to " +
			  targetPath.expandedName() +
			  ": " + strerror(errno)));
    }
    // Copy the file and remove it thereafter.
    copy (targetPath, overwrite, False);
    remove();
}

Int64 RegularFile::size() const
{
  return itsFile.size();
}

} //# NAMESPACE CASACORE - END

