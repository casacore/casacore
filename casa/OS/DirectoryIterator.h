//# DirectoryIterator.h: Traverse the contents of a directory
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

#ifndef CASA_DIRECTORYITERATOR_H
#define CASA_DIRECTORYITERATOR_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/OS/File.h>
#include <casacore/casa/OS/Directory.h>
#include <casacore/casa/Utilities/Regex.h>

#include <dirent.h>                          // needed for DIR


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>  
// Traverse the contents of a directory
// </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// <use visibility=export>

// <prerequisite> 
//    <li> Basic knowledge of the UNIX file system
//    <li> <linkto class=Directory>Directory</linkto>
//    <li> possibly <linkto class=Regex>Regex</linkto>
// </prerequisite>

// <synopsis>
// DirectoryIterator allows to traverse a directory. In this way all
// file names in a directory can be gotten. Files . and .. will
// always be skipped.
// <p>
// By means of a regular expression it is possible to traverse the
// directory selectively. That is, only the file names matching the regular
// expression will be returned. Note that the regular expression is
// a true regular expression (as defined by class <linkto class=Regex>
// Regex</linkto> and not a file expression as used in shells.
// Thus to get all .cc files, one has to specify ".*\.cc" and not "*.cc".
// <p>
// The <linkto class=File>File</linkto> class can be used to determine if
// a file represents a symlink, directory or regular file.
// </synopsis>

// <example>
// <srcblock>
//    Directory dir("testdir");
//    // Get all .cc files.
//    // Note that Regex is a true regular expression and not a
//    // simplified file expression (like *.cc) as used in shells.
//    DirectoryIterator dirIter(dir, ".*.\cc");
//    while (!dirIter.pastEnd()){
//  	  cout << dirIter.name() << endl;
//	  dirIter++;
//    }
// </srcblock>
// </example>

// <motivation> 
// With this class it is easy to iterate through a directory.
// </motivation>

// <todo asof=$DATE$>
// <li> Allow file expressions like *.cc.
//      However, it's probably better to make that part of Regex.
// </todo>


class DirectoryIterator
{
public:

    // Construct the iterator for the working directory.
    // All entries (except . and ..) will be traversed.
    // It positions the iterator on the first entry.
    DirectoryIterator();

    // Construct the iterator for the given directory.
    // All entries (except . and ..) will be traversed.
    // It positions the iterator on the first entry.
    DirectoryIterator (const Directory& dir);

    // Construct the iterator for the given directory.
    // All entries matching the regular expression will be traversed.
    // It positions the iterator on the first entry.
    DirectoryIterator (const Directory& dir, const Regex& regExpression);

    // Copy constructor (copy semantics).
    // The iterator will be positioned at the beginning.
    DirectoryIterator (const DirectoryIterator& that);

    // Assignment (copy semantics).
    // The iterator will be positioned at the beginning.
    DirectoryIterator& operator= (const DirectoryIterator& that);

    ~DirectoryIterator();

    // Position on the next matching entry in the directory.
    // <br>An exception is thrown if the iterator is already past the end.
    // <group>
    void operator++();
    void operator++(int);
    // </group>

    // Returns the file name at the current position.
    // <br>An exception is thrown if the iterator is already past the end.
    String name() const;

    // Returns a File object for the file at the current position.
    // Note that this adds the path of the directory to get the
    // correct path for the file.
    // <br>An exception is thrown if the iterator is already past the end.
    File file() const;

    // Reposition the directory stream on the first entry.
    void reset();

    // Checks if the iterator is past the end.
    Bool pastEnd() const;

private:
    // Initialize the iterator.
    void init();

    // This variable is used for seeking in the directory.
    // The directory is opened and closed once during the lifetime 
    // of the class.
    DIR *itsDirectoryDescriptor;

    // This structure is used for information of the directory.
    dirent *itsDirectoryEntry;

    // Boolean to check if the directory stream has past the end
    Bool itsEnd;

    // class directory 
    Directory itsDirectory;
    
    // Regular expression if given, with this variable it is possible 
    // to compare files with regular expression.
    Regex itsExpression;

#if defined(AIPS_CRAY_PGI)
    // Cray XT3 does not support readdir on compute nodes.
    // Use scandir instead.
    dirent** itsNameList;
    int      itsNrNames;
    int      itsNameInx;
#endif
};



} //# NAMESPACE CASACORE - END

#endif
