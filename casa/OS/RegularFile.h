//# RegularFile.h: Manipulate and get information about regular files
//# Copyright (C) 1993,1994,1995,1996,1997,2003
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

#ifndef CASA_REGULARFILE_H
#define CASA_REGULARFILE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/OS/File.h>
#include <casacore/casa/BasicSL/String.h>
	

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary> 
// Manipulate and get information about regular files
// </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// <use visibility=export>

// <prerequisite> 
//    <li>Basic knowledge of the UNIX file system 
//    <li><linkto class=File>File</linkto>
// </prerequisite>

// <etymology> 
// The class RegularFile provides functions for manipulating and getting 
// information about regular files. Regular file are files which hold data.
// </etymology>

// <synopsis> 
// This class provides functions to manipulate and to get information about 
// regular files. The functions for getting information (like ownership, dates)
// about regular files are inherited from the <linkto class=File>File</linkto>
// class.
// <br>
// The class RegularFile itself provides functions to rename, remove, copy,
// and move regular files. The file name can be a symbolic link resolving
// (eventually) to a regular file.
// </synopsis>

// <example>
// <srcblock>
//    // Create the object rFile
//    RegularFile rFile ("isFile");
//
//    // Create file; if the file exists it will be overwritten
//    rFile.create (True);
//    rFile.copy (newPath);
//  
//    cout << rFile.size() << endl;     // Get the size of the file
//    cout << rFile.path() << endl;     // Show the relative pathname
//
//    rFile.remove();                   // remove the file
// </srcblock>
// </example>

// <motivation> 
// Provide functions for manipulating and getting information 
// about regular files.
// </motivation>


class RegularFile: public File
{
public:

    // Default constructor sets path to . (working directory).
    RegularFile();

    // Create a regular file object for a file with the given path name.
    // An exception is thrown if the file is illegal, i.e. if it does
    // not exist as a regular file or symbolic link or if cannot be created.
    // Note that the file is not created if it does not exist yet.
    // This can be done using the function create.
    // <br>
    // When the given path name is a symbolic link, the symbolic link
    // is resolved (recursively) and the resulting file name is used
    // instead.
    // <group>
    RegularFile (const Path& path);
    RegularFile (const String& string);
    RegularFile (const File& file);
    // </group>

    // Copy constructor (copy semantics).
    RegularFile (const RegularFile& regularFile);

    ~RegularFile();

    // Assignment (copy semantics).
    RegularFile& operator= (const RegularFile& regularFile);

    // Create the regular file.
    // <br>If the file exists and is not a regular file, an 
    // exception is thrown. Otherwise if overwrite is true the regular file
    // will be overwritten. If overwrite is false then nothing will be done.
    // If the file does not exist, it is created.
    void create (Bool overwrite = True);

    // Remove the file.
    // If it does not exist, an exception will be thrown.
    // If a symbolic link is given, the link chain pointing to the file
    // will also be removed.
    void remove();

    // Copy the file to the target path using the system command cp.
    // If the file is a symbolic link, the regular file pointed to
    // will be copied.
    // The target path can be a directory or a file (as in cp).
    // An exception is thrown if:
    // <br>- the target directory is not writable
    // <br>- or the target file already exists and overwrite==False
    // <br>- or the target file already exists and is not writable
    // <note role=caution>
    // When a readonly file is copied, the resulting
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

    // Copy the file manually in case the cp command cannot be used.
    // (like on the Cray XT3).
    static void manualCopy (const String& source, const String& target);

    // Move the file to the target path using the system command mv.
    // If the file is a symbolic link, the regular file pointed to
    // will be moved.
    // The target path can be a directory or a file (as in mv).
    // An exception is thrown if:
    // <br>- the target directory is not writable
    // <br>- or the target file already exists and overwrite==False
    // <br>- or the target file already exists and is not writable
    // <note role=tip> The system command mv is used instead of the
    // library function rename to be able to move across file systems.
    // </note>
    // <group>
    void move (const Path& target, Bool overwrite = True);
    void move (const String& target, Bool overwrite = True);
    // </group>

    // Return the size of the file. If the file
    // does not exist, an exception will be thrown.
    virtual Int64 size() const;

private:
    // Check if the path of the file is valid.
    // Also resolve possible symlinks.
    void checkPath();

    // This variable is used when a symbolic link points to the file.
    File itsFile;
};


inline void RegularFile::copy (const String& target, Bool overwrite,
			       Bool setUserWritePermission) const
{
    copy (Path(target), overwrite, setUserWritePermission);
}
inline void RegularFile::move (const String& target, Bool overwrite)
{
    move (Path(target), overwrite);
}



} //# NAMESPACE CASACORE - END

#endif
