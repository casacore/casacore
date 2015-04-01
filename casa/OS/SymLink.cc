//# SymLink.cc: Class to define a Symbolic Link
//# Copyright (C) 1996,2001,2002
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


#include <casacore/casa/OS/SymLink.h>
#include <casacore/casa/OS/RegularFile.h>
#include <casacore/casa/Exceptions.h>

#include <unistd.h>               // needed for unlink
#include <errno.h>                // needed for errno
#include <casacore/casa/string.h>          // needed for strerror

namespace casacore { //# NAMESPACE CASACORE - BEGIN

SymLink::SymLink()
: File()
{}

SymLink::SymLink (const Path& name)
: File(name)
{
    checkPath();
}

SymLink::SymLink (const String& name)
: File(name)
{
    checkPath();
}

SymLink::SymLink (const File& name)
: File(name)
{   
    checkPath();
}

SymLink::SymLink (const SymLink& that)
: File(that)
{}

SymLink::~SymLink()
{}

SymLink& SymLink::operator= (const SymLink& that)
{
    if (this != &that){
	 File::operator = (that);
    }
    return *this;	
}

void SymLink::checkPath() const
{
    // If exists, check if it is a symlink.
    // Otherwise check if it can be created.
    if (exists()) {
	if (!isSymLink()) {
	    throw (AipsError ("SymLink: " + path().expandedName() +
			      " exists, but is no symbolic link"));
	}
    } else {
	if (!canCreate()) {
	    throw (AipsError ("SymLink: " + path().expandedName() +
			      " does not exist and cannot be created"));
	}
    }
}

void SymLink::create (const Path& target, Bool overwrite)
{
    
    // If overwrite is False the file will not be overwritten.
    if (exists()) {
	if (!isSymLink()) {
	    throw (AipsError ("SymLink::create: " +
			      path().expandedName() +
			      " already exists as a non-symlink"));
	}
	if (!overwrite) {
	    throw (AipsError ("SymLink::create: " +
			      path().expandedName() +
			      " already exists"));
	}
	// Remove an existing symlink, otherwise symlink fails.
	remove();
    }
    if (symlink (target.expandedName().chars(),
		 path().expandedName().chars()) < 0) {
	throw (AipsError ("SymLink::create error on " + target.expandedName() +
			  ": " + strerror(errno)));
    }
}

void SymLink::remove()
{
    unlink (path().expandedName().chars());
}

void SymLink::copy (const Path& target, Bool overwrite) const
{
    Path targetName(target);
    checkTarget (targetName, overwrite);
    // This function cannot the system function cp, because
    // that copies the file the symlink is pointing to and
    // refuses to copy when it points to a directory.
    File targetFile(targetName);
    if (targetFile.isRegular (False)) {
	RegularFile(targetFile).remove();
    }
    SymLink newLink(targetFile);
    newLink.create (getSymLink());
}

void SymLink::move (const Path& target, Bool overwrite)
{
    Path targetName(target);
    checkTarget (targetName, overwrite);
    File targetFile(targetName);
    if (targetFile.isRegular (False)) {
	RegularFile(targetFile).remove();
    }
    SymLink newLink(targetFile);
    newLink.create (getSymLink());
    remove();
}

String SymLink::getSymLink() const
{
    // Create a buffer for readlink.
    char buf[2048];
    int length;
    // read the link, and place the result in buf, length is the number
    // of characters placed in buf by readlink
    length = readlink (path().expandedName().chars(), buf, 2048);
    if (length <= 0) {
	throw (AipsError ("SymLink: " + path().expandedName() +
			  " does not exist"));
    }
    return String (buf, length);
}

Path SymLink::readSymLink() const
{
    Path result (getSymLink());
    // Prepend with dirname if no absolute name.
    if (result.originalName().firstchar() != '/') {
	result = path().dirName() + "/" + result.originalName();
    }	
    return result; 
}

Path SymLink::followSymLink() const
{
    // Do it max. 25 times to avoid endless loops.
    Path result;
    File file(*this);
    Int count = 0;
    do {
	if (++count > 25) {
	    throw (AipsError ("SymLink: resolving " + path().expandedName() +
			      " results in an endless loop"));
	}
	SymLink link(file);
	result = link.readSymLink();
        file = File(result);
    } while (file.isSymLink());
    return result;
}

} //# NAMESPACE CASACORE - END

