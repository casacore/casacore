//# DOos.cc: Functions used to implement the DO functionality
//# Copyright (C) 1999,2000,2001,2002
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


//# Includes
#include <casacore/casa/OS/DOos.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/OS/File.h>
#include <casacore/casa/OS/RegularFile.h>
#include <casacore/casa/OS/Directory.h>
#include <casacore/casa/OS/DirectoryIterator.h>
#include <casacore/casa/OS/SymLink.h>
#include <casacore/casa/IO/LockFile.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

Vector<Bool> DOos::isValidPathName (const Vector<String>& pathName)
{
  Vector<Bool> result(pathName.nelements());
  for (uInt i=0; i<pathName.nelements(); i++) {
    if (pathName(i).empty()) {
      result(i) = False;
    } else {
      File file(pathName(i));
      result(i) =  (file.exists() || file.canCreate());
    }
  }
  return result;
}

Vector<Bool> DOos::fileExists (const Vector<String>& pathName,
			       Bool follow)
{
  Vector<Bool> result(pathName.nelements());
  for (uInt i=0; i<pathName.nelements(); i++) {
    if (pathName(i).empty()) {
      result(i) = False;
    } else {
      File file(pathName(i));
      if (follow && file.isSymLink()) {
	file = File(SymLink(file).followSymLink());
      }
      result(i) = file.exists();
    }
  }
  return result;
}

Vector<String> DOos::fileType (const Vector<String>& pathName,
			       Bool follow)
{
  Vector<String> result(pathName.nelements());
  for (uInt i=0; i<pathName.nelements(); i++) {
    File file(pathName(i));
    if (file.isRegular (follow)) {
      result(i) = "Regular File";
    } else if (file.isDirectory (follow)) {
      File tab(pathName(i) + "/table.dat");
      if (tab.isRegular (follow)) {
	result(i) = "Table";
      } else {
	result(i) = "Directory";
      }
    } else if (file.isSymLink()) {
      result(i) = "SymLink";
    } else if (! file.exists()) {
      result(i) = "Invalid";
    } else {
      result(i) = "Unknown";
    }
  }
  return result;
}

Vector<String> DOos::fileNames (const String& directoryName,
				const String& fileNamePattern,
				const String& fileTypes,
				Bool all, Bool follow)
{
  // Determine if and how to select on file type.
  Bool takeRegular =  (fileTypes.contains ('r'));
  Bool takeDirectory =  (fileTypes.contains ('d'));
  Bool takeSymLink =  (fileTypes.contains ('s'));
  Bool takeReadable =  (fileTypes.contains ('R'));
  Bool takeWritable =  (fileTypes.contains ('W'));
  Bool takeExecutable =  (fileTypes.contains ('X'));
  Bool checkType =  (takeRegular || takeDirectory || takeSymLink);
  Bool checkAcc =  (takeReadable || takeWritable || takeExecutable);
  Bool check =  (checkType || checkAcc);
  // Set up the iterator. Default pattern is all.
  Vector<String> result;
  Directory dir (directoryName);
  uInt n = 0;
  DirectoryIterator iter (dir);
  if (! fileNamePattern.empty()) {
    iter = DirectoryIterator (dir, Regex::fromPattern (fileNamePattern));
  }
  // Iterate through the directory and add matching name to result.
  // Skip names starting with . if all is False.
  for (; !iter.pastEnd(); iter++) {
    String name = iter.name();
    if (name[0] != '.'  ||  all) {
      if (check) {
	File file(directoryName + '/' + name);
	if (checkType) {
	  if (!(   (takeRegular && file.isRegular (follow))
		|| (takeDirectory && file.isDirectory (follow))
		|| (takeSymLink && file.isSymLink()))) {
	    continue;
	  }
	}
	if (checkAcc) {
	  if (!(    (takeReadable && file.isReadable())
		 || (takeWritable && file.isWritable())
		 || (takeExecutable && file.isExecutable()))) {
	    continue;
	  }
	}
      }
      if (n >= result.nelements()) {
	result.resize (result.nelements() + 100, True);
      }
      result(n++) = name;
    }
  }
  result.resize (n, True);
  return result;
}

void DOos::makeDirectory (const Vector<String>& directoryName,
			  Bool makeParent)
{
  for (uInt i=0; i<directoryName.nelements(); i++) {
    File file(directoryName(i));
    if (file.exists()) {
      throw (AipsError ("DOos::makeDirectory - a file " + directoryName(i) +
			" already exists"));
    }
    // First make parent directory if allowed and if needed.
    if (makeParent) {
      String parent = Path(directoryName(i)).dirName();
      if (! File(parent).exists()) {
	Vector<String> parName(1);
	parName(0) = parent;
	makeDirectory (parName, makeParent);
      }
    }
    Directory dir(file);
    dir.create (False);
  }
}

Vector<String> DOos::fullName (const Vector<String>& fileName)
{
  Vector<String> result(fileName.nelements());
  for (uInt i=0; i<fileName.nelements(); i++) {
    result(i) = Path(fileName(i)).absoluteName();
  }
  return result;
}

Vector<String> DOos::dirName (const Vector<String>& fileName)
{
  Vector<String> result(fileName.nelements());
  for (uInt i=0; i<fileName.nelements(); i++) {
    result(i) = Path(Path(fileName(i)).absoluteName()).dirName();
  }
  return result;
}

Vector<String> DOos::baseName (const Vector<String>& fileName)
{
  Vector<String> result(fileName.nelements());
  for (uInt i=0; i<fileName.nelements(); i++) {
    result(i) = Path(Path(fileName(i)).absoluteName()).baseName();
  }
  return result;
}

Vector<Double> DOos::fileTime (const Vector<String>& fileName,
			       Int whichTime, Bool follow)
{
  Vector<Double> result(fileName.nelements());
  for (uInt i=0; i<fileName.nelements(); i++) {
    File file(fileName(i));
    if (!file.exists()) {
      throw (AipsError ("DOos::fileTime - file " + fileName(i) +
			" does not exist"));
    }
    if (follow && file.isSymLink()) {
      file = File(SymLink(file).followSymLink());
    }
    // Note that MJD 40587 is 1-1-1970 which is the starting time of
    // the file times.
    Double time;
    if (whichTime == 2) {
      time = file.modifyTime();
    } else if (whichTime == 3) {
      time = file.statusChangeTime();
    } else {
      time = file.accessTime();
    }
    result(i) = 40587 + time/(24*3600);
  }
  return result;
}

Vector<Double> DOos::totalSize (const Vector<String>& fileName, Bool follow)
{
  Vector<Double> result(fileName.nelements());
  for (uInt i=0; i<fileName.nelements(); i++) {
    File file(fileName(i));
    if (!file.exists()) {
      throw (AipsError ("DOos::totalSize - file " + fileName(i) +
			" does not exist"));
    }
    Double size = 0;
    if (! file.isDirectory (follow)) {
      if (file.isRegular (follow)) {
	size = RegularFile(file).size();
      }
    } else {
      DirectoryIterator iter (file);
      // Iterate through the directory.
      for (; !iter.pastEnd(); iter++) {
	size += totalSize(fileName(i) + '/' + iter.name(), follow);
      }
    }
    result(i) = size;
  }
  return result;
}

Double DOos::totalSize (const String& fileName, Bool follow)
{
  File file(fileName);
  Double size = 0;
  if (file.exists()) {
    if (! file.isDirectory (follow)) {
      if (file.isRegular (follow)) {
	size = RegularFile(file).size();
      }
    } else {
      DirectoryIterator iter (file);
      // Iterate through the directory.
      for (; !iter.pastEnd(); iter++) {
	size += totalSize(fileName + '/' + iter.name(), follow);
      }
    }
  }
  return size;
}

Vector<Double> DOos::freeSpace (const Vector<String>& fileName, Bool follow)
{
  Vector<Double> result(fileName.nelements());
  for (uInt i=0; i<fileName.nelements(); i++) {
    File file(fileName(i));
    if (file.isDirectory (follow)) {
      result(i) = Directory(file).freeSpace();
    } else if (file.isRegular (follow)  ||  file.isSymLink()) {
      String name = Path(Path(fileName(i)).absoluteName()).dirName();
      result(i) = Directory(name).freeSpace();
    } else {
      throw (AipsError ("DOos::freeSpace - file " + fileName(i) +
			" does not exist"));
    }
  }
  return result;
}

void DOos::copy (const String& to, const String& from, Bool overwrite,
		 Bool follow)
{
  File file(from);
  if (! file.exists()) {
    throw (AipsError ("DOos::copy - file " + from + " does not exist"));
  }
  if (file.isRegular (follow)) {
    RegularFile(file).copy (to, overwrite);
  } else if (file.isDirectory (follow)) {
    Directory(file).copy (to, overwrite);
  } else if (file.isSymLink()) {
    SymLink(file).copy (to, overwrite);
  } else {
    throw (AipsError ("DOos::copy - file " + from + " is not a regular file "
		      "directory, nor symlink"));
  }
}

void DOos::move (const String& to, const String& from, Bool overwrite,
		 Bool follow)
{
  File file(from);
  if (! file.exists()) {
    throw (AipsError ("DOos::move - file " + from + " does not exist"));
  }
  if (file.isRegular (follow)) {
    RegularFile(file).move (to, overwrite);
  } else if (file.isDirectory (follow)) {
    Directory(file).move (to, overwrite);
  } else if (file.isSymLink()) {
    SymLink(file).copy (to, overwrite);
  } else {
    throw (AipsError ("DOos::move - file " + from + " is not a regular file "
		      "directory, nor symlink"));
  }
}

void DOos::remove (const String& fileName, Bool recursive,
		   Bool mustExist, Bool follow)
{
  remove (Vector<String> (1, fileName), recursive, mustExist, follow);
}

void DOos::remove (const Vector<String>& fileNames, Bool recursive,
		   Bool mustExist, Bool follow)
{
  uInt i;
  if (mustExist) {
    for (i=0; i<fileNames.nelements(); i++) {
      File file(fileNames(i));
      if (! file.exists()) {
	throw (AipsError ("DOos::remove - file " + fileNames(i) +
			  " does not exist"));
      }
    }
  }
  for (i=0; i<fileNames.nelements(); i++) {
    File file(fileNames(i));
    if (file.exists()) {
      if (file.isRegular (follow)) {
	RegularFile rfile(file);
	rfile.remove();
      } else if (file.isDirectory (follow)) {
	Directory dir(file);
	if (recursive) {
	  dir.removeRecursive();
	} else {
	  dir.remove();
	}
      } else if (file.isSymLink()) {
	SymLink symlink(file);
	symlink.remove();
      } else {
	throw (AipsError ("DOos::remove - file " + fileNames(i) +
			  " is not a regular file, directory, nor symlink"));
      }
    }
  }
}

Vector<Int> DOos::lockInfo (const String& tableName)
{
  Vector<Int> result(3);
  uInt pid;
  Bool permLocked;
  result(0) = LockFile::showLock (pid, permLocked, tableName + "/table.lock");
  result(1) = pid;
  result(2) = (permLocked  ?  1 : 0);
  return result;
}

} //# NAMESPACE CASACORE - END

