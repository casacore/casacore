//# DOos.h: Functions used to implement the DO functionality
//# Copyright (C) 1999,2000,2001
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

#ifndef CASA_DOOS_H
#define CASA_DOOS_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class String;


// <summary>
// DO for accessing os-specific functions
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto module=OS>OS</linkto>
// </prerequisite>

// <etymology>
// </etymology>

// <synopsis>
// This class serves as the connection between the OS module and a tasking
// interface in Glish or Python.
// It is meant for access to OS-specific functions, in
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
  // Are the given path names valid?
  // I.e. does a file with the given name exist or can it be created?
  static Vector<bool> isValidPathName (const Vector<String>& pathName);

  // Do the given files exist?
  // If follow is false, symbolic links are not followed.
  static Vector<bool> fileExists (const Vector<String>& fileName,
				  bool follow = true);

  // Give the type of the given files.
  static Vector<String> fileType (const Vector<String>& fileName,
				  bool follow = true);

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
				   bool all = false,
				   bool follow = true);

  // Make directories. It throws an exception if a file with that
  // name already exists.
  static void makeDirectory (const Vector<String>& directoryNames,
			     bool makeParent = false);

  // Return the full absolute names for the given names.
  static Vector<String> fullName (const Vector<String>& fileName);

  // Return the full directory names of the given files.
  static Vector<String> dirName (const Vector<String>& fileName);

  // Return the base names of the given files.
  static Vector<String> baseName (const Vector<String>& fileName);

  // Get the time of the given files.
  // <src>whichTime</src> determines which time to return:
  // <br>1 = time of last access
  // <br>2 = time of last modification
  // <br>3 = time of last status change
  static Vector<double> fileTime (const Vector<String>& fileName,
				  int32_t whichTime = 1, bool follow = true);

  // Return the total size (in bytes) for each file or directory given.
  // For a directory the size of all files (recursively) in it is given.
  // If follow is false, symbolic links are not followed.
  // <group>
  static Vector<double> totalSize (const Vector<String>& fileName,
				   bool follow = true);
  static double totalSize (const String& fileName, bool follow = true);
  // </group>

  // Return the total size on the devices the given directories are on.
  // If follow is false, symbolic links are not followed.
  static Vector<double> freeSpace (const Vector<String>& fileName,
				   bool follow = true);

  // Copy the file (or directory recursively).
  // If from is a symbolic link and follow is false, only the
  // symbolic link is copied.
  static void copy (const String& to, const String& from,
		    bool overwrite = true, bool follow = true);

  // Move the file or directory.
  // If from is a symbolic link and follow is false, only the
  // symbolic link is moved.
  static void move (const String& to, const String& from,
		    bool overwrite = true, bool follow = true);

  // Remove the files (or directories recursively).
  // If fileName is a symbolic link and follow is false, only the
  // symbolic link is removed.
  // <group>
  static void remove (const String& fileName, bool recursive,
		      bool mustExist = true, bool follow = true);
  static void remove (const Vector<String>& fileNames, bool recursive,
		      bool mustExist = true, bool follow = true);
  // </group>

  // Tell if a table is used or locked by another process.
  // It returns a vector containing 3 integers.
  // The first one tells if the table is in use or locked.
  // See <linkto class=LockFile>LockFile</linkto>\::showLock for details.
  // The second one gives the pid of the process using/locking the table.
  // The third one tells if the table is permanently locked (0 = not).
  static Vector<int32_t> lockInfo (const String& tableName);
};



} //# NAMESPACE CASACORE - END

#endif
