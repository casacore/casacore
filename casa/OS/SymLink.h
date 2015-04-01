//# SymLink.h: Get information about, and manipulate symbolic links
//# Copyright (C) 1996
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


#ifndef CASA_SYMLINK_H
#define CASA_SYMLINK_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/OS/File.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>  
// Get information about, and manipulate symbolic links
// </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// <use visibility=export>

// <prerequisite> 
//    <li> Basic knowledge of the UNIX file system 
//    <li> <linkto class=File>File</linkto>
// </prerequisite>

// <etymology> 
// The class SymLink handles SYMbolic LINKs in the file system.
// </etymology>

// <synopsis> 
// SymLink provides functions to manipulate and to get information about 
// symbolic links. The functions for getting information (like ownership,
// dates) about symbolic links are inherited from the
// <linkto class=File>File</linkto> class.
// <br>
// The class SymLink itself provides functions to create, remove, copy, and
// move symbolic links. There is a function readSymLink which reads a link and 
// then returns a path and there is a function followSymLink which reads a
// link recursively. If the link eventually refers to itself (a loop),
// an exception will be thrown.  
// </synopsis>

// <example>
// <srcblock>
//    SymLink symLink1("isLink");
//    SymLink symLink2("isLink2");
//    SymLink symLinkA("A");
//    SymLink symLinkB("B");
//
//    symLink1.create("~", True);    // Create a symbolic link to the home
//                                   // directory. When it exists it will be
//                                   // overwritten.
//    symLink2.create("isLink", False); // Create a symbolic link to 
//                                      // isLink. When it exists it will not
//                                      // be overwritten.
//    symLinkA.create(Path("B"));    // Create a recursive link
//    symLinkB.create(Path("A"));    // Create a recursive link
//
//    cout << symLink1.readSymLink() << endl;  // The homedirectory is printed
//    cout << symLink2.readSymLink() << endl;  // isLink is printed
//    cout << symLink2.followSymLink() << endl;// The homedirectory is printed
//    cout << symLinkA.readSymLink() << endl;  // B is printed
//    cout << symLinkA.followSymLink() << endl;// An exception is thrown (loop)
// </srcblock>
// </example>

// <motivation> 
// Provide functions for manipulating and getting information 
// about symbolic links.
// </motivation>


class SymLink: public File
{
public:

    // The default constructor creates a SymLink with path ".".
    SymLink();

    // Create a SymLink with the given path.
    // An exception is thrown if the path exist and is no symbolic link
    // or if it does not exist, but cannot be created.
    // <group>
    SymLink (const Path& name);
    SymLink (const String& name);
    SymLink (const File& name);
    // </group>

    // Copy constructor (copy semantics).
    SymLink (const SymLink& that);

    ~SymLink();

    // Assignment (copy semantics).
    SymLink& operator= (const SymLink& that);
 
    // Make a symbolic link to a file given by target.
    // An exception will be thrown if:
    // <br>-target already exists and is no symlink
    // <br>-or target already exists and overwrite==False
    // <group>
    void create (const Path& target, Bool overwrite = True);
    void create (const String& target, Bool overwrite = True);
    // </group>

    // Copy the symlink to the target path using the system command cp.
    // The target path can be a directory or a file (as in cp).
    // An exception is thrown if:
    // <br>- the target directory is not writable
    // <br>- or the target file already exists and overwrite==False
    // <br>- or the target file already exists and is not writable
    // <group>
    void copy (const Path& target, Bool overwrite = True) const;
    void copy (const String& target, Bool overwrite = True) const;
    // </group>

    // Move the symlink to the target path using the system command mv.
    // The target path can be a directory or a file (as in mv).
    // An exception is thrown if:
    // <br>- the target directory is not writable
    // <br>- or the target file already exists and overwrite==False
    // <br>- or the target file already exists and is not writable
    // <group>
    void move (const Path& target, Bool overwrite = True);
    void move (const String& target, Bool overwrite = True);
    // </group>

    // Remove a symbolic link.
    void remove();

    // Read value of a symbolic link and return it as a Path. If 
    // the symlink does not exist, an exception will be thrown.
    // When the symlink points to a file with a relative name,
    // the resulting file name gets prepended by the dirname of the symlink,
    // which is similar to the way a shell handles symlinks.
    // E.g.
    // <srcblock>
    //  ls > subdir/a
    //  ln -s a subdir/b
    //  more subdir/b
    // </srcblock>
    // The more command shows the results of subdir/a. 
    Path readSymLink() const;

    // As readSymLink, but the entire symlink chain is followed
    // when the symlinks points to other symlinks.
    // An exception is thrown if this results in a loop (that is, if more
    // than 25 links are encountered).
    Path followSymLink() const;

private:
    // Check if the path of the file is valid.
    // Also resolve possible symlinks.
    void checkPath() const;

    // Get the value of the symlink.
    String getSymLink() const;
};


inline void SymLink::create (const String& target, Bool overwrite)
{
    create (Path(target), overwrite);
}
inline void SymLink::copy (const String& target, Bool overwrite) const
{
    copy (Path(target), overwrite);
}
inline void SymLink::move (const String& target, Bool overwrite)
{
    move (Path(target), overwrite);
}



} //# NAMESPACE CASACORE - END

#endif
