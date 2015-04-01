//# File.h: Class to get file information and a base for other file classes
//# Copyright (C) 1993,1994,1995,1996,2000,2003
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

#ifndef CASA_FILE_H
#define CASA_FILE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/OS/Mutex.h>
#include <casacore/casa/BasicSL/String.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary> 
// Class to get file information and a base for other file classes.
// </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// <use visibility=export>

// <prerequisite> 
//    <li> Basic knowledge of the UNIX file system 
//    <li> <linkto class=Path>Path</linkto> 
// </prerequisite>

// <etymology> 
// 'File' is used in a traditional sense.
// </etymology>

// <synopsis> 
// The File class provides the primary functions needed by all kinds of 
// files (directories, regular files, symbolic links, named pipes etc.).
// These shared functions serve mostly to return information about a 
// particular file -- for instance, its type, its ownership, read, write
// and execute permissions, date of latest access and the path on secundary 
// storage associated with this file. Every file object has, by definition, 
// a <linkto class=Path>Path</linkto> object associated with it which
// defines the file name.
// <p>
// See also the derived classes
// <linkto class=RegularFile>RegularFile</linkto>,
// <linkto class=Directory>Directory</linkto>, and
// <linkto class=SymLink>SymLink</linkto>.
// <br>
// This class does not contain virtual functions, because a lot of functions 
// have different parameters, e.g. 'create' for RegularFile has one parameter
// and 'create' for SymLink has two parameters. 
//
// It handles large files correctly.
// </synopsis>

// <example>
// <srcblock>
//    File myFile("someFileName");
//    if (myFile.exists()) {
//	myFile.setPermissions(0644);
//	if (myFile.isRegular()) {
//	    cout << "this file is a regular file" << endl;
//      }
//    }
//    else if (!myFile.exists()) {
//	  if (!myFile.canCreate()){
//	      cout << "cannot create this file" << endl;
//	  } 
//    }
// </srcblock>
// </example>

// <motivation> 
// File systems operations are a notorious source of porting problems.
// The file class provides a standard interface for programmers to use.
// </motivation>


class File
{
public: 

	enum FileWriteStatus {
		// file exists and can be overwritten
		OVERWRITABLE,
		// file exists but cannot be overwritten
		NOT_OVERWRITABLE,
		// file does not exist and is creatable
		CREATABLE,
		// file does not exist but cannot be created
		NOT_CREATABLE
	};


    // Construct a File object whose Path is set to the current working 
    // directory. 
    File();
    
    // Construct a File object whose Path is set to the given Path.
    // <group>
    File (const Path& path);
    File (const String& path);
    // </group>

    // Copy constructor (copy semantics).
    File (const File& that);

    virtual ~File();
    
    // Assignment (copy semantics).
    File& operator= (const File& that);

    // Returns the pathname of the file.
    const Path& path() const;

    // Check if the file is a regular file. If the boolean followSymLink is
    // False a symbolic link will not be followed.
    Bool isRegular (Bool followSymLink = True) const;

    // Check if the file is a directory. If the boolean followSymLink is
    // False a symbolic link will not be followed.
    Bool isDirectory (Bool followSymLink = True) const;

    // Check if the file is a symbolic link.
    Bool isSymLink() const;

    // Check if the file is a pipe.
    Bool isPipe() const;

    // Check if the file is a character special file.
    Bool isCharacterSpecial() const;

    // Check if the file is a block special file.
    Bool isBlockSpecial() const;

    // Check if the file is a socket.
    Bool isSocket() const;

    // Check if the file exists.
    Bool exists() const;

    // Check if the file is readable.
    Bool isReadable() const;

    // Check if the file is writable.
    Bool isWritable() const;

    // Check if the file is executable.
    Bool isExecutable() const;

    // Check if a file can be created.
    Bool canCreate() const;
    
    // Return the userID of the file.
    long userID() const; 

    // Return the groupID of the file.
    long groupID() const;
    
    // Return the size of the file. If the file
    // does not exist, an exception will be thrown.
    virtual Int64 size() const;

    // Return the permissions as a decimal value.
    uInt readPermissions() const;

    // Set permission with perm. Perm is an octal value.
    void setPermissions (uInt permissions);

    // Update access time and modification time of a file.
    void touch (uInt time);

    // Update access time and modification time of a file. This function
    // updates the file with the current time.
    void touch();

    // Time related fucnctions:
    // Return the time when the file was last accessed in seconds since
    // 00:00:00 GMT Jan 1, 1970.
    uInt accessTime() const;

    // Return the time when the file was last accessed
    // as a 26-characters String of the form:
    // Thu Feb  3 13:40:11 1994
    String accessTimeString() const;

    // Return the time when the file was last modified in seconds since
    // 00:00:00 GMT Jan 1, 1970.
    uInt modifyTime() const;

    // Return the time when the file was last modified
    // as a 26-characters String of the form:
    // Thu Feb  3 13:40:11 1994
    String modifyTimeString() const;

    // Return the time when the file status was last changed in seconds since
    // 00:00:00 GMT Jan 1, 1970.
    // It is set both by writing and changing the file status information,
    // such as changes of owner, group, link count, or mode.
    uInt statusChangeTime() const;

    // return the time when the file status was last changed
    // as a 26-characters String of the form:
    // Thu Feb  3 13:40:11 1994
    String statusChangeTimeString() const;

    // Create a new unique path name in the specified directory, with
    // the specified prefix and random trailing characters:
    // <srcblock>
    //    p.newUniqueName ("./", "temp")  -->  "./tempAAA00xx32"
    //    p.newUniqueName ("/home/me", "diary")  -->  "/home/me/diaryAAA00xxb0"
    // </srcblock>
    static Path newUniqueName (const String& directory, const String& prefix);

    // Create a new unique filename without a prefix.
    // As above, but all the characters in the filename are random:
    // <srcblock>
    //    p.newUniqueName ("./")  -->  "./AAA00xx32"
    //    p.newUniqueName ("/home/me")  -->  "/home/me/AAA00xxb0"
    // </srcblock>
    static Path newUniqueName (const String& directory); 


    // get write status of the file.
    // OVERWRITABLE - file exists and can be overwritten
    // NOT_OVERWRITABLE - file exists but cannot be overwritten
    // CREATABLE - File does not exist and can be created
    // NOT_CREATABLE - file does not exist and cannot be created.
    FileWriteStatus getWriteStatus() const;

    // Return the filesystem type.
    // If the file doesn't exsist crawl up the directory tree to
    // find one that does.
    String getFSType() const; 

protected:
    // This function is used by <linkto class=RegularFile>RegularFile</linkto> 
    // and <linkto class=Directory>Directory</linkto> to remove all the links
    // which, when followed, ultimately resolve to a Directory or a 
    // RegularFile.
    // For example, A->B, B->C, C->D and D points to a regular file.
    // When remove() is called for a regular file A,
    // that function uses removeLinks() to remove A, B, C and D.
    void removeSymLinks();

    // Check if the new path for a copy or move is valid.
    // An exception is thrown if:
    // <br>- the target directory is not writable
    // <br>- or the target file already exists and overwrite==False
    // <br>- or the target file already exists and is not writable
    // <br>When the targetName represents a directory, the basename
    // of the file is appended to it. This is done to cover the
    // case where the source is a symlink to a file. In that case
    // the target will get the basename of the symlink and not the
    // the basename of the file pointed to. This is not done when
    // forDirectory==True (which is used by class Directory).
    void checkTarget (Path& targetName, Bool overwrite,
		      Bool forDirectory = False) const;

private:
    // Define a function for lstat.
    // This is necessary since SunOS4.1.x prototypes lstat() with a first
    // argument of type (char*), while Solaris (and presumably all other
    // reasonable OS's) prototype it with a first argument of type
    // (const char*).  Since lstat() does not change its first argument,
    // it is safe to convert our const variable to a non-const one so that
    // we can call lstat() successfully.
    // <br>It is also useful to be able to pass the buffer as void*. In that
    // way the 32-bit or 64-bit file details are only needed in the cc file.
    int mylstat (const char* path, void* buf) const;

    // Get the lstat of this file.
    // Throw an exception when it fails.
    void getstat (void* buf) const;

    // Get the lstat of a file.
    // Throw an exception when it fails.
    void getstat (const File& file, void* buf) const;


    // Full pathname of the file.
    Path itsPath;
    // A sequence number to generate unique file names.
    static uInt uniqueSeqnr_p;
    static Mutex theirMutex;
};


inline const Path& File::path() const
{
    return itsPath;
}

inline void File::getstat (void* buf) const
{
    getstat (*this, buf);
}



//# The ifdef's below are similar to those in IO/LargeIOFuncDef.h.
#if !defined(AIPS_NOLARGEFILE)
# ifdef AIPS_LINUX
#  if !defined(_LARGEFILE64_SOURCE)
#   define _LARGEFILE64_SOURCE
#  endif
# endif
#if defined(AIPS_DARWIN) || defined(AIPS_BSD)
# define fileFSTAT fstat
# define fileLSTAT lstat
# define fileSTAT  stat
# define fileSTATFS  statfs
#else
# define fileFSTAT fstat64
# define fileLSTAT lstat64
# define fileSTAT  stat64
# define fileSTATFS  statfs64
#endif
#else
# define fileFSTAT fstat
# define fileLSTAT lstat
# define fileSTAT  stat
# define fileSTATFS  statfs
#endif



} //# NAMESPACE CASACORE - END

#endif
