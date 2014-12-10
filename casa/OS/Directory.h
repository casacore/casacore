//# Directory.h: Get information about, and manipulate directories
//# Copyright (C) 1996,1997,1999
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

#ifndef CASA_DIRECTORY_H
#define CASA_DIRECTORY_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/OS/File.h>
   
namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T> class Vector;
class Regex;
class String;

// <summary>  
// Get information about, and manipulate directories
// </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// <use visibility=export>

// <prerequisite> 
//    <li> Basic knowledge of the UNIX file system
//    <li> <linkto class=File>File</linkto>
// </prerequisite>

// <synopsis> 
// Directory provides functions to manipulate and to get information about 
// directories. The functions for getting information (like ownership, dates)
// about directories are inherited from the <linkto class=File>File</linkto>
// class.
// Directory itself provides functions to create, copy, move, or remove
// a directory. The file name can be a symbolic link resolving
// (eventually) to a directory.
// <p>
// A separate class <linkto class=DirectoryIterator>DirectoryIterator</linkto>
// allows one to traverse a directory to get the file names in it.
// </synopsis>

// <example>
// <srcblock>
//    Directory dir("someDir");
//    // Create directory someDir in the working directory.
//    dir.create();
//    cout << dir.nEntries();        // #entries
//    // Assign to another directory.
//    dir = Directory("otherDir");
//    // Remove the directory and its contents.
//    dir.removeRecursive();
// </srcblock>
// </example>

// <motivation> 
// Provide functions for manipulating and getting information 
// about directories.
// </motivation>


class Directory: public File
{
public:

    // Sets the path on the current working directory
    Directory();

    // Create a directory object for a file with the given path name.
    // An exception is thrown if the directory is illegal, i.e. if it does
    // not exist as a directory or symbolic link or if cannot be created.
    // Note that the directory is not created if it does not exist yet.
    // This can be done using the function create.
    // <br>
    // When the given path name is a symbolic link, the symbolic link
    // is resolved (recursively) and the resulting directory name is used
    // instead.
    // <group>
    Directory (const Path& name);
    Directory (const String& name);
    Directory (const File& name);
    // </group>

    // Copy constructor (copy semantics).
    Directory (const Directory& that);

    ~Directory();

    // Assignment (copy semantics).
    Directory& operator= (const Directory& that);

    // Check if directory is empty.
    // If the directory does not exist, an exception will be thrown.
    Bool isEmpty() const;

    // Return the number of entries in the directory (not counting . and ..).
    // If the directory does not exist, an exception will be thrown.
    uInt nEntries() const;

    // Get the amount of free space (in bytes) on the file system this
    // directory is on. When the directory path is a symbolic link, that
    // link is resolved first.
    // <group>
    Double freeSpace() const;
    uInt freeSpaceInMB() const;
    // </group>

    // Create the directory.
    // <br>If the directory exists and overwrite=True, it will be removed
    // (recursively). Otherwise an exception is thrown.
    void create (Bool overwrite = True);

    // Remove a directory.
    // An exception is thrown if the directory is not empty.
    // If a symbolic link is given, the link chain pointing to the directory
    // will also be removed.
    void remove();

    // Remove all files in the directory except subdirectories.
    // The directory itself is not removed.
    void removeFiles();

    // Remove the directory and its contents (recursively in all
    // subdirectories).
    // If <src>keepDir==True</src>, the directory itself is kept
    //(to keep properties like placement on Lustre).
    void removeRecursive (Bool keepDir = False);

    // Copy the directory and its contents (recursively) to the target
    // path using the system command cp -r.
    // If the target already exists (as a file, directory or symlink),
    // and overwrite=True, it will first be removed.
    // The target directory is created and the data in the source
    // directory is copied to the new directory.
    // <br>An exception is thrown if:
    // <br>- the target directory is not writable
    // <br>- or the target already exists and overwrite!=True
    // <note role=caution>
    // 1. The behavior of this copy function is different from cp when the
    // target directory already exists. Cp copies the source to a
    // subdirectory of the target, while copy recreates the target.
    // <br>2. When a readonly file is copied, <src>cp</src> the resulting
    // file is also readonly. Therefore <src>chmod</src> is used to
    // set user write permission after the copy.
    // The flag <src>setUserWritePermission</src> can be set to False
    // when that should not be done.
    // </note>
    // <group>
    void copy (const Path& target, Bool overwrite = True,
	       Bool setUserWritePermission = True) const;
    void copy (const String& target, Bool overwrite = True,
	       Bool setUserWritePermission = True) const;
    // </group>

    // Copy a directory recursively in a manual way.
    // This is used in a copy using the system command is not possible
    // (like on the Cray XT3).
    void copyRecursive (const String& target) const;

    // Move the directory to the target path using the system command mv.
    // If the target already exists (as a file, directory or symlink),
    // and overwrite=True, it will first be removed.
    // The source directory is moved (thus renamed) to the target.
    // <br>An exception is thrown if:
    // <br>- the target directory is not writable
    // <br>- or the target already exists and overwrite!=True
    // <note role=caution>
    // The behavior of this move function is different from mv when the
    // target directory already exists. Mv moves the source to a
    // subdirectory of the target, while move recreates the target.
    // </note>
    // <group>
    void move (const Path& target, Bool overwrite = True);
    void move (const String& target, Bool overwrite = True);
    // </group>

    // Find all files which whose names match <src>regex</src>.  You
    // can do this recursively (default) or not.  Note that the 
    // matching is a regular expression match, not a shell file-expansion 
    // match. However, a shell file pattern can be converted to a regexp
    // using the function <linkto class=Regex>Regex::fromPattern</linkto>.
    // <src>Regex::fromString</src> allows one to convert a file name
    // to a regexp and to use this function for eact file name matching.
    // <br>To match the semantics of the unix <src>find</src> command,
    // symbolic links are not followed by default, but this behavior
    // can be over-ridden. 
    Vector<String> find (const Regex& regexp, Bool followSymLinks=False,
                         Bool recursive=True) const;


    // For each element of <src>files</src>, find all file names matching
    // it using shell file-expansion rules.  Return the list of all matched files
    // as absolute path + file names.  You may optionally drop the path and just return
    // the file names.   Note tha if <src>files(i)</src> contains a path as well as a file
    // name, no matching is done on the path, just the trailing file name.
    // Throws an AipsError if the shell pattern is illegal.
    static Vector<String> shellExpand (const Vector<String>& files, Bool stripPath=False);
    // Return the total size  of everything in the Directory. If the Directory
    // does not exist, an exception will be thrown.
    virtual Int64 size() const;

    //Check if a directory is mounted via NFS or not.
    Bool isNFSMounted() const;
    
private:
    // Check if the path defines a directory.
    // Also resolve possible symlinks.
    void checkPath();

    // This variable is used when a symbolic link is given to be
    // a directory.
    File itsFile;
};



inline void Directory::copy (const String& target, Bool overwrite,
			     Bool setUserWritePermission) const
{
    copy (Path(target), overwrite, setUserWritePermission);
}
inline void Directory::move (const String& target, Bool overwrite)
{
    move (Path(target), overwrite);
}
inline uInt Directory::freeSpaceInMB() const
{
    return uInt (0.5 + freeSpace() / (1024*1024));
}



} //# NAMESPACE CASACORE - END

#endif
