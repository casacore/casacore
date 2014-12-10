//# Path.h: Path name of a file
//# Copyright (C) 1993,1994,1995,1996,1997,1998,1999,2000
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


#ifndef CASA_PATH_H
#define CASA_PATH_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Path name of a file
// </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// <prerequisite> 
//    <li> Basic knowledge of the UNIX file system 
// </prerequisite>

// <etymology>
// The term 'path' is the standard term for describing the location of a file 
// in a hierarchy of possibly nested directories. In order to find a 
// particular file you must travel a specific path strating from a known 
// point. We use the term in its standard sense in this class. 
// </etymology>

// <synopsis>
// This class can be used to describe a pathname. One can also create,
// validate, parse (get base or directory names or original, expanded or
// absolute names), query and append strings. The client programmer can 
// give a string, at construction, which describes a path. This string can 
// be a relative or an absolute name. Environment variables and a tilde
// (with or without user name) can also be used in the string and will
// be expanded by the function expandedName.
// <br> The function 
// Once a Path has been constructed, you can query the object for its
// original name, expanded name, absolute name, the name of the directory
// where it is found or the name of only the file. Expanding the path name
// means that possible environment variables and tilde get expanded.
// There are also functions to get the length or maximum length of a path.
// Pathnames can also be checked on correctness and they can be checked
// if they conform the POSIX standard.
// </synopsis>

// <example>
// In this example a few pathnames are created.
// <srcblock>
//    Path test1("~/test/$TEST1/..");     // absolute path
//    Path test2("/$HOME/./analyse");     // absolute path
//    Path test3("myFile");               // relative path
// 
//    cout << test1.originalName() << endl;
//
//    // Test1 is according the POSIX standard
//    if (test1.isStrictlyPosix()){         
//       cout << "test1 is strictly POSIX << endl;  
//    }
// 
//    // Test1 is valid
//    if (test1.isValid()){                 
//       cout << test1.isValid() << endl;
//    }
//
//    // if "TEST1=$TEST2 and TEST2=$TEST1"(recursive environment variables) 
//    // an exception will be thrown. ~ is replaced by the homedirectory
//    cout << test1.expandedName() << endl;
//    // $HOME is expanded 
//    cout << test2.expandedName() << endl; 
//    cout << test1.absoluteName() << endl; 
//    cout << test2.absoluteName() << endl; 
//    cout << test2.baseName() << endl;
//    cout << test1.dirName() << endl;
//    cout << test3.originalName() << endl; // myFile is returned 
//    cout << test3.expandedName() << endl; // Nothing is changed
//    cout << test3.absoluteName() << endl; // The current working directory
//                                             is placed before 'myFile'  
//    cout << test3.baseName() << endl; // The current working directory 
//                                      // is returned
//    cout << test3.dirName() << endl;  // myFile is returned
// </srcblock>
// </example>

// <motivation>
// Programmer convenience and (eventually) OS independence.
// </motivation>

// <todo asof=$DATE$>
// <li> To make the class OS independent some functions should be rebuild.
//      These functions could be expandedName or absoluteName.
// <li> The function expandedName or absoluteName could map the filename to 
//      the native convention 
// <li> A (maybe static) function contractName(const String& pathName)
//      could be implemented to remove . and .. from the file name.
// </todo>


class Path
{
public:
    // Default constructor, the path is set to . (working directory).
    Path();

    // Construct a path with the given name.
    // When the name is empty, it is set to . (working directory).
    // It is not checked if the path name is valid.
    // Function isValid() can be used for that purpose.
    Path (const String& pathName);

    // Copy constructor, copy semantics.
    Path (const Path& that);

    // Destructor
    ~Path();

    // Assignment, copy semantics.
    Path& operator= (const Path& that);

    // Append a string to the path name.
    // When the current path does not end with a / and the string to append
    // does not start with a /, an intermediate / is also added.
    void append (const String& string);

    // Returns the string as given at construction. 
    const String& originalName () const;

    // Return a string giving the expanded pathname.
    // This means that the environment variables are expanded and the tilde 
    // is replaced by the home directory. An expanded name can still
    // be a relative path.
    // An exception is thrown when converting a recursive environment
    // variable results in an endless loop (that is, more than 25
    // substitutions).
    const String& expandedName () const;

    // Return the string which giving the absolute pathname.
    // It is generated from the expanded pathname by adding
    // the working directory when needed.
    const String& absoluteName () const;

    // Return the realpath which is the absolute pathname with possible
    // symlinks resolved. It also resolves //, /./, /../ and trailing /.
    // <br>The path must be an existing file or directory.
    // It uses the system's realpath function. In case it fails,
    // an exception is thrown.
    String resolvedName() const;

    // Check if pathname is valid. This function checks for: double slashes, 
    // non-printable characters, pathname length and filename lengths, this 
    // function is more OS-specific.
    Bool isValid() const;

    // Check if pathname is valid according the POSIX standard.
    // This function checks for
    // double slashes, non-printable characters,pathname length and filename 
    // lenghts, all according to the POSIX-standard.
    Bool isStrictlyPosix() const;

    // Return length of path name
    uInt length() const;

    // Return the maximum length a path name can have.
    uInt maxLength() const;

    // Return the basename of the path; this is only the name of the file.
    // It takes it from the expanded path name.
    String baseName() const;

    // Return the dirname of the path; this is the directory where the 
    // filename is found. It takes it from the expanded path name.
    // <br>To get the absolute dirname one could do:
    // <srcblock>
    //     Path tmpPath (myPath.dirName());
    //     String absDir (tmpPath.absoluteName());
    // </srcblock>
    // or
    // <srcblock>
    //     Path tmpPath (myPath.absoluteName());
    //     String absDir (tmpPath.dirName());
    // </srcblock>
    String dirName() const;

    // Strip otherName from this name. If stripped, the result gets a
    // leading ././
    // If not stripped, it is tried if name can be stripped from otherName.
    // If stripped, the result gets a trailing /.
    // If still not stripped, it is tried to strip the directory of otherName.
    // If that succeeds, the result gets a leading ./
    // This is used by RefTable and TableKeyword to ensure that the
    // name of a subtable or referenced table is always relative to
    // the main table.
    static String stripDirectory (const String& name, const String& otherName);

    // If the name starts with ././ add otherName to it.
    // If the name ends with /. strip name from otherName and return the
    // remainder.
    // If the name starts with ./ add the directory of otherName to it.
    // It is the opposite of stripDirectory.
    static String addDirectory (const String& name, const String& otherName);


private:
    // Strings to describe the pathname in three different ways.
    String itsOriginalPathName;
    // These variables are pointer to strings because the functions which use 
    // these variables are const functions. This means that they would not be 
    // able to modify the string, now they can.
    mutable String itsAbsolutePathName;
    mutable String itsExpandedPathName;

    // Define the maximum number of bytes in a pathname
    // This definition does not use Posix values.
    static uInt getMaxPathNameSize ();
    // Define the maximum number of bytes in a filename
    // This definition does not use Posix values.
    static uInt getMaxNameSize ();


    // This function is used by expandedName to replace the tilde and to
    // expand the environment variables
    String expandName (const String& inString) const;

    // This function is used by absoluteName to make a name absolute, 
    // this means that the name is described from the root
    String makeAbsoluteName (const String& inString) const;

    // Remove . and .. from the path name.
    // Also multiple slashes are replaced by a single.
    String removeDots (const String& inString) const;

    // This function is used by expandName and absoluteName. It sets the 
    // integer "count" on the next slash or on the end of a string
    void getNextName (const String& inString, uInt& count) const;
};


inline const String& Path::originalName() const
{
    return itsOriginalPathName;
}



} //# NAMESPACE CASACORE - END

#endif
