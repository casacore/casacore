//# DOos.h: Functions used to implement the DO functionality
//# Copyright (C) 1999
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
//#
//# $Id$

#if !defined(AIPS_DOOS_H)
#define AIPS_DOOS_H

//# Includes
#include <aips/aips.h>
#include <trial/Tasking/ApplicationObject.h>

//# Forward Declarations
class String;


// <summary>
// DO for accessing os-specific functions
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto module=Tasking>Tasking</linkto>
//   <li> <linkto module=OS>OS</linkto>
// </prerequisite>

// <etymology>
// </etymology>

// <synopsis>
// This class is the connection between the Glish os server, and the
// OS module. It is meant for access to OS-specific functions, in
// particular file handling.
// </synopsis>

// <example>
// </example>

// <motivation>
// </motivation>

// <thrown>
//    <li> AipsError if AIPSPATH or HOME is not defined
// </thrown>

// <todo asof="1997/09/16">
//   <li> Check for feasable extensions
// </todo>


class DOos
{
public:
  // Is the given path name valid?
  static Bool isValidPathName (const String& pathName);

  // Give all file names in the directory matching the given pattern
  // and file types.
  // <br>The pattern can be a string like the filename pattern given in
  // a shell (e.g. '*.cc'). If the string is empty, all files are taken
  // into account.
  // <br>Filetypes is a string determining which file types will be selected.
  // Each character in the string determines a file type. They are:
  // <dl>
  // <dt>r<dd>regular file
  // <dt>d<dd>directory
  // <dt>s<dd>symbolic link
  // <dt>R<dd>readable file
  // <dt>W<dd>writable file
  // <dt>X<dd>executable file
  // </dl>
  // The all flag determines if file names starting with a . will also
  // be selected.
  static Vector<String> fileNames (const String& directoryName,
				   const String& fileNamePattern,
				   const String& fileTypes,
				   Bool all = False,
				   Bool follow = True);

  // Make a directory. It throws an exception if a file with that
  // name already exists.
  static void makeDirectory (const String& directoryName);

  // Return the full absolute name for the given name.
  static String fullName (const String& fileName);

  // Return the full direcotry name of the given name.
  static String dirName (const String& fileName);

  // Return the base name of the given file.
  static String baseName (const String& fileName);

  // Return the total size of the file or all files (recursively)
  // in the directory.
  // If follow is False, symbolic links are not followed.
  static Double totalSize (const String& fileName, Bool follow = True);

  // Return the total size on the device the given directory is on.
  // If follow is False, symbolic links are not followed.
  static Double freeSpace (const String& fileName, Bool follow = True);

  // Copy the file (or directory recursively).
  // If from is a symbolic link and follow is False, only the
  // symbolic link is copied.
  static void copy (const String& to, const String& from,
		    Bool overwrite = True, Bool follow = True);

  // Move the file or directory.
  // If from is a symbolic link and follow is False, only the
  // symbolic link is moved.
  static void move (const String& to, const String& from,
		    Bool overwrite = True, Bool follow = True);

  // Remove the file (or directory recursively).
  // If fileName is a symbolic link and follow is False, only the
  // symbolic link is removed.
  static void remove (const String& fileName, Bool recursive,
		      Bool mustExist = True, Bool follow = True);
};


#endif
