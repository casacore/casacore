//# Path.cc: Class to define a pathname
//# Copyright (C) 1993,1994,1995,1996,1997,1998,1999,2000,2001,2002
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
#include <casacore/casa/OS/EnvVar.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions.h>

#include <pwd.h>                    // needed for getpwnam
#include <unistd.h>                 // needed for pathconf
#include <limits.h>                 // needed for PATH_MAX, etc.
#include <ctype.h>                  // needed for isprint
#include <stdlib.h>                 // needed for realpath
#include <errno.h>                  // needed for errno
#include <casacore/casa/string.h>            // needed for strerror


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// The maximum number of bytes in a pathname is 255 (_POSIX_PATH_MAX)
// Definition for POSIX systems
#if defined (_POSIX_PATH_MAX)
const uInt pathmax_posix = _POSIX_PATH_MAX;
#else
const uInt pathmax_posix = 255;
#endif


// The maximum number of bytes in a pathname is PATH_MAX 
#if defined (PATH_MAX)
static uInt pathMax = PATH_MAX;
#else
static uInt pathMax = 0;
#endif

const uInt PATH_MAX_GUESS = 1024; // if PATH_MAX is indeterminate
                                  // we're not guaranteed this is adequate. 

// The maximum number of bytes in a filename is 14 (_POSIX_NAME_MAX)
// Definition for POSIX systems
#if defined (_POSIX_NAME_MAX)
const uInt namemax_posix = _POSIX_NAME_MAX;
#else
const uInt namemax_posix = 14;
#endif


// The maximum number of bytes in a filename is NAME_MAX
#if defined (NAME_MAX)
static uInt nameMax = NAME_MAX;
#else
static uInt nameMax = 0;
#endif

const uInt NAME_MAX_GUESS = 255;  // if NAME_MAX is indeterminate
                                  // we're not guaranteed this is adequate. '


Path::Path() 
: itsOriginalPathName (".")
{}

Path::Path (const String& pathName)
: itsOriginalPathName (pathName)
{
    if (itsOriginalPathName.empty()) { 
	itsOriginalPathName = ".";    
    }
}    

Path::Path (const Path& that)
: itsOriginalPathName (that.itsOriginalPathName)
{}

Path::~Path()
{}   

Path& Path::operator= (const Path& that)
{
    if (this != &that) {   
	itsOriginalPathName = that.itsOriginalPathName;
	itsAbsolutePathName = that.itsAbsolutePathName;
	itsExpandedPathName = that.itsExpandedPathName;
    }
    return *this;
}

void Path::append (const String& string)
{
    if  (!string.empty()) {
	if (itsOriginalPathName.lastchar() != '/'
        &&  string.firstchar() != '/') {
	    itsOriginalPathName += "/";
	}
	itsOriginalPathName += string;
	itsAbsolutePathName = "";
	itsExpandedPathName = "";
    }
}

const String& Path::expandedName() const
{
    if (itsExpandedPathName.empty()) {
	itsExpandedPathName = expandName (itsOriginalPathName);
    }
    return itsExpandedPathName;
}

const String& Path::absoluteName() const
{
    if (itsAbsolutePathName.empty()) {
	itsAbsolutePathName = removeDots (makeAbsoluteName (expandedName()));
    }
    return itsAbsolutePathName;
}

String Path::resolvedName() const
{
    char name[PATH_MAX+1];
    char* ptr = realpath (absoluteName().c_str(), name);
    if (ptr == 0) {
        throw AipsError("resolvedName(" + absoluteName() + ") failed: " +
                        strerror(errno));
    }
    return String(name);
}

Bool Path::isValid() const
{
    // Check if the length of the pathname is not too long
    if (itsOriginalPathName.length() > getMaxPathNameSize()) {
	return False;
    }
    // Check if pathname contains double slashes
    if (itsOriginalPathName.contains ("//")) {
	return False;
    }
    // Check if pathname contains non-printables
    uInt i;
    for (i=0; i<itsOriginalPathName.length(); i++) {
	if (isprint (itsOriginalPathName[i]) == 0 ) {
	    return False;
	}
    }

    // Check if the length of the pathname is not too long
    if (itsOriginalPathName.length() > getMaxPathNameSize()) {
	return False;
    }

    // Check if filenames are not too long
    String subPathname[30];
    String sep = "/";
    uInt nw = split (itsOriginalPathName, subPathname, 15, sep);
    uInt nameSize = getMaxNameSize();
    for (i=0; i<nw; i++) {
	if (subPathname[i].length() > nameSize ) {
	    return False;
	}
    }
    return True;
}

Bool Path::isStrictlyPosix() const
{
    // Check if the length of the pathname is not too long, according POSIX 
    // standard
    if(itsOriginalPathName.length() > pathmax_posix ) {
	return False;
    }
    // Check if pathname contains double slashes
    if (itsOriginalPathName.contains ("//")) {
	return False;
    }
    // Check if pathname contains non-printables
    uInt i;
    for (i=0; i<itsOriginalPathName.length(); i++) {
	if (!isprint (itsOriginalPathName[i])) {
	    return False;
	}
    }

    // Check if filenames are not too long, according POSIX standard
    String subPathname[30];
    String sep = "/";
    uInt nw = split(itsOriginalPathName, subPathname, 15, sep);
    for (i=0; i<nw; ++i) {
	if (subPathname[i].length() > namemax_posix )
	    return False;
    }
    return True;
}

uInt Path::length() const
{
    return itsOriginalPathName.length();
}

uInt Path::maxLength() const
{
    return getMaxPathNameSize();
}

String Path::baseName() const
{
    // Determine from expanded path name.
    String name = expandedName();
    // Search last slash.
    // Get rid of trailing slash.
    Int len=name.length();
    if (len > 0  &&  name[len-1] == '/') {
	len--;
    }
    Int i=len;
    while (--i >= 0  &&  name[i] != '/') {}
    // The base name is the part from the slash till the end.
    return name(i+1, len-i-1);
}

String Path::dirName() const
{
    String name = expandedName();
    // Search last slash.
    // Get rid of trailing slash (except if name consists of a slash only).
    Int i=name.length();
    if (i > 1  &&  name[i-1] == '/') {
	i--;
    }
    while (--i >= 0  &&  name[i] != '/') {}
    // If no slash, the dirname is the current directory.
    if (i < 0) {
	return ".";
    }
    // If the slash found is not the first character, skip it.
    if (i > 0) {
	i--;
    }
    return name.through(i);
}


uInt Path::getMaxPathNameSize()
{
    // pathMax is not defined(<0) then pathconf sets pathMax, 
    // if this doesn't work pathMax will get the value of PATH_MAX_GUESS
    if (pathMax == 0) {
#if defined(AIPS_CRAY_PGI)
        pathMax = PATH_MAX_GUESS;
#else
	pathMax = pathconf ("/",_PC_PATH_MAX) < 0  ?  pathMax : PATH_MAX_GUESS;
#endif
    }
    return pathMax;
}

uInt Path::getMaxNameSize()
{
    // nameMax is not defined (<0) then pathconf sets nameMax, 
    // if this doesn't work nameMax will get the value of PATH_MAX_GUESS
    if (nameMax == 0) {
#if defined(AIPS_CRAY_PGI)
        pathMax = NAME_MAX_GUESS;
#else
	nameMax = pathconf ("/",_PC_NAME_MAX) < 0  ?  nameMax : NAME_MAX_GUESS;
#endif
    }
    return nameMax;
}


String Path::expandName (const String& inString) const
{
    String tempString (inString);
    uInt cursor = 0;
    uInt i = 0;
    Bool flag = True;
    uInt count = 0;
    // Flag is set True if an environment variable is detected. When this 
    // happens more then 25 times, there is probably a recursive variable set.
    // In that case an exception will be thrown.
    while (flag && count<25) {  
	// flag is False, if there is not an environment variable
	// the name will not be checked again
	flag = False;     
	count++;          // count is increased when the string is 
	cursor = 0;       // walked through 
	// Replace tilde with the name of the home directory
	if (tempString.firstchar() == '~') {
	    if (tempString.length() == 1  ||  tempString[1] == '/') {
	      // To get the home directory, environment variable HOME is used.
		String name (EnvironmentVariable::get("HOME"));
		if (! name.empty()) {
		    tempString.del ("~",0);
		    tempString.prepend (name);
		}
	    } else {
		tempString.del ("~",0);
		getNextName (tempString, cursor);
		String temp (tempString.before(Int(cursor)));
		// The password file is used to get the home directory 
		// of "~name"
		// This cannot be done on the CRAY XT3 CATAMOUNT as it
		// does not support sockets.
#ifdef AIPS_CRAY_PGI
		tempString.prepend ("~");
#else
		passwd* passWd = getpwnam(temp.chars());
		if (passWd != 0) {
		    tempString.del (tempString.before (Int(cursor)));
		    tempString.prepend (passWd->pw_dir);
		    cursor = 0;
		}else{
		    tempString.prepend ("~");
		}
#endif
	    }
	}
	i = 0;
        if (tempString.size() > 0  &&  tempString[0] == '/') {
            i = 1;
        }
	while (i < tempString.length()) {
            cursor = i;
	    getNextName (tempString, i);      // i is set on the next name   
            // See if an env.var is given
            String::size_type dpos = tempString.find ('$', cursor);
            if (dpos != String::npos) {
                String::size_type last = i;
		String dName (tempString.at (Int(dpos+1), Int((i-dpos)-1)));
                if (dName[0] == '{') {
                    String::size_type bracePos = dName.find ('}');
                    if (bracePos != std::string::npos) {
                        last = dpos+1+bracePos+1;
                        dName = dName.substr(1, bracePos-1);
                    } else {
                        dpos = String::npos;
                    }
                }
                if (dpos != String::npos) {
                    String name (EnvironmentVariable::get(dName));
                    if (! name.empty()) {
		        String res (name);
                        res.prepend (tempString.before(Int(dpos)));
                        res += tempString.after (Int(last-1));
                        // Update the index for the changed part.
                        i = last + Int(res.size()) - Int(tempString.size());
                        tempString = res;
                        // flag is set True, so the name will be checked again
                        // for environment variables
                        flag = True;
                    }
		}
	    }	
	    i++;    // go past slash
	}
    }
    if (flag) {
	// An exception is thrown if the string is checked more then 25 times
	throw (AipsError ("Path::expandName: recursive environment variable"));
    }
    return tempString;
}


String Path::makeAbsoluteName (const String& inString) const
{
    // If the first char is a slash the name is already absolute.
    if (inString.firstchar() == '/') {
	return inString;
    }
    // Otherwise we have a relative pathname.
    // Remove a possible leading . or ./
    String workString (inString);
    if (workString.length() > 0) {
	if (workString[0] == '.') {
	    Int from(1);
	    if (workString.length() > 1){
               switch(workString[1]) {
                case '/' :
		   from = 2;
                   break;
                case '.' :
                   from = 0;
                   break;
               }
	    } 
	    workString = workString.from (from);
	}
    }
    // Get the working directory and prepend it.
    // getcwd returns a null pointer if it fails.
    char temp[1024];
    AlwaysAssert (getcwd(temp, 1024), AipsError);
    String tempString (temp);
    // Return the working directory when no input string left.
    if (workString.empty()) {
	return tempString;
    }
    // Append a / if needed.
    if (tempString.lastchar() != '/') {
	tempString += '/';
    }
    tempString += workString;
    return tempString;
}

String Path::removeDots (const String& inString) const
{
    // Split the name at the slashes.
    Vector<String> parts (stringToVector (inString, '/'));
    Vector<uInt> validParts (parts.nelements());
    String dot(".");
    String dotdot("..");
    uInt nvalid = 0;
    uInt i;
    // Count the number of valid parts and keep their index.
    // Ignore blanks and . parts.
    // A .. part removes an entry from the valid list (if possible).
    for (i=0; i<parts.nelements(); i++) {
        if (! (parts(i).empty()  ||  parts(i) == dot)) {
	    if (parts(i) == dotdot) {
	        if (nvalid > 0) {
		    nvalid--;
		}
	    } else {
	        validParts(nvalid++) = i;
	    }
	}
    }
    // Combine the parts into the output string.
    // Start it with a slash if the input started with a slash.
    String outString;
    Bool doSlash = (parts(0).empty());
    for (i=0; i<nvalid; i++) {
        if (doSlash) {
	    outString += '/';
	}
	outString += parts(validParts(i));
	doSlash = True;
    }
    return outString;
}

void Path::getNextName (const String& inString, uInt& count) const
{
    // Sets count on the next slash or on the end of the string
    Int inx = inString.index ("/", count);
    if (inx < 0) {
	count = inString.length();
    } else {
	count = inx;
    }
}


String Path::stripDirectory (const String& name, const String& otherName)
{
    // Add trailing slash if not there.
    String dir (Path(otherName).absoluteName());
    if (dir.lastchar() != '/') {
	dir += '/';
    }
    Int leng = dir.length();
    // Convert name to an absolute path name.
    String aName (Path(name).absoluteName());
    // If directory is contained in this name, return name without it.
    // Prepend by ././ indicating full name is removed.
    if (leng > 0) {
        Int aleng = aName.length();
	if (aleng > leng  &&  aName.before(leng) == dir) {
	    return "././" + aName.from (leng);
	} else {
	    // No match; see if name matches start of otherName.
	    // If so, append /. to remainder of otherName.
	    if (aleng+1 < leng  &&  dir.before(aleng+1) == aName+'/') {
	        return dir.from(aleng+1) + '.';
	    }
	    // No match; now compare using the directory part only.
	    dir = Path(dir).dirName() + '/';
	    while (dir.length() >= 2  &&  dir[0] == '.'  &&  dir[1] == '/') {
		dir = dir.after(1);
	    }
	    leng = dir.length();
	    if (leng > 0) {
		if (aName.length() > uInt(leng)  && aName.before(leng) == dir) {
		    // The leading ./ indicates that directory is removed.
		    return "./" + aName.from (leng);
		}
	    }
	}
    }
    if (leng == 0) {
	// Also relative if the parent is this directory and the
	// subtable is relative.
	if (name[0] != '/'  &&  name[0] != '$'  &&  name[0] != '~') {
	    return "./" + name;
	}
    }
    // Nothing could be stripped.
    // Return original name after removing possible leading ./
    String tName(name);
    while (tName.length() >= 2  &&  tName[0] == '.'  &&  tName[1] == '/') {
	tName = tName.after(1);
    }
    return tName;
}

String Path::addDirectory (const String& name, const String& otherName)
{
    // If a directory was stripped off, add the directory of the parent.
    // Otherwise use the name as such.
    // Note that dirName will use the expanded name.
    // Start with removing possible leading ./
    String tName(name);
    while (tName.length() >= 2  &&  tName[0] == '.'  &&  tName[1] == '/') {
	tName = tName.after(1);
    }
    // If anything was removed, we have to add the directory.
    if (tName.length() < name.length()) {
	// If ./ was removed, add directory of otherName.
	// Otherwise add the full otherName.
	Path dir(otherName);
	if (tName.length() == name.length() - 2) {
	    dir = Path(dir.dirName());
	}
	dir.append (tName);
	return dir.originalName();
    } else {
        // If name ends in /. and if it matches the end of otherName,
        // we have to return the remainder of otherName.
        Int leng = tName.length();
        if (leng >= 3  &&  tName[leng-2] == '/'  &&  tName[leng-1] == '.') {
	    leng -= 2;
	    String oName(otherName);
	    Int oleng = oName.length();
	    if (oleng >= leng+2
            &&  '/'+tName.before(leng) == oName.from(oleng-leng-1)) {
	        return oName.before(oleng-leng-1);
	    }
	}
    }
    return tName;
}

} //# NAMESPACE CASACORE - END

