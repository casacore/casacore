//# DOos.cc: Functions used to implement the DO functionality
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


//# Includes
#include <trial/OS/DOos.h>
#include <aips/OS/Path.h>
#include <aips/OS/File.h>
#include <aips/OS/RegularFile.h>
#include <aips/OS/Directory.h>
#include <aips/OS/DirectoryIterator.h>
#include <aips/OS/SymLink.h>
#include <aips/Arrays/Vector.h>
#include <aips/Utilities/String.h>
#include <aips/Exceptions/Error.h>


Bool DOos::isValidPathName (const String& pathName)
{
  File file(pathName);
  return ToBool (file.exists() || file.canCreate());
}

Vector<String> DOos::fileNames (const String& directoryName,
				const String& fileNamePattern,
				const String& fileTypes,
				Bool all, Bool follow)
{
  // Determine if and how to select on file type.
  Bool takeRegular = ToBool (fileTypes.contains ('r'));
  Bool takeDirectory = ToBool (fileTypes.contains ('d'));
  Bool takeSymLink = ToBool (fileTypes.contains ('s'));
  Bool takeReadable = ToBool (fileTypes.contains ('R'));
  Bool takeWritable = ToBool (fileTypes.contains ('W'));
  Bool takeExecutable = ToBool (fileTypes.contains ('X'));
  Bool checkType = ToBool (takeRegular || takeDirectory || takeSymLink);
  Bool checkAcc = ToBool (takeReadable || takeWritable || takeExecutable);
  Bool check = ToBool (checkType || checkAcc);
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

void DOos::makeDirectory (const String& directoryName)
{
  File file(directoryName);
  if (file.exists()) {
    throw (AipsError ("DOos::makeDirectory - a file " + directoryName +
		      " already exists"));
  }
  Directory dir(file);
  dir.create (False);
}

String DOos::fullName (const String& fileName)
{
  return Path(fileName).absoluteName();
}

String DOos::dirName (const String& fileName)
{
  return Path(fullName(fileName)).dirName();
}

String DOos::baseName (const String& fileName)
{
  return Path(fullName(fileName)).baseName();
}

Double DOos::totalSize (const String& fileName, Bool follow)
{
  File file(fileName);
  if (!file.exists()) {
    throw (AipsError ("DOos::totalSize - file " + fileName +
		      " does not exist"));
  }
  if (! file.isDirectory (follow)) {
    if (file.isRegular (follow)) {
      return RegularFile(file).size();
    }
    return 0;
  }
  Double size = 0;
  DirectoryIterator iter (file);
  // Iterate through the directory.
  for (; !iter.pastEnd(); iter++) {
    size += totalSize(fileName + '/' + iter.name(), follow);
  }
  return size;
}

Double DOos::freeSpace (const String& fileName, Bool follow)
{
  File file(fileName);
  if (file.isDirectory (follow)) {
    return Directory(file).freeSpace();
  }
  if (file.isRegular (follow)  ||  file.isSymLink()) {
    String name = Path(Path(fileName).absoluteName()).dirName();
    return Directory(name).freeSpace();
  }
  throw (AipsError ("DOos::freeSpace - file " + fileName +
		    " does not exist"));
  return 0;
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

void DOos::remove (const String& fileName, Bool recursive, Bool mustExist,
		   Bool follow)
{
  File file(fileName);
  if (! file.exists()) {
    if (mustExist) {
      throw (AipsError ("DOos::remove - file " + fileName +
			" does not exist"));
    }
  } else {
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
      throw (AipsError ("DOos::remove - file " + fileName +
			" is not a regular file, directory, nor symlink"));
    }
  }
}
