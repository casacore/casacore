//# HDF5File.h: An class representing an HDF5 file
//# Copyright (C) 2008
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

//# Includes
#include <casacore/casa/HDF5/HDF5File.h>
#include <casacore/casa/HDF5/HDF5HidMeta.h>
#include <casacore/casa/HDF5/HDF5Error.h>
#include <casacore/casa/OS/RegularFile.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

#ifdef HAVE_HDF5

  HDF5File::HDF5File (const String& name,
		      ByteIO::OpenOption option)
    : itsOption (option),
      itsDelete (False)
  {
    // Disable automatic printing of errors.
    H5Eset_auto2(H5E_DEFAULT, NULL, NULL);
    // Use absolute expanded path name.
    setName (Path(name).absoluteName());
    doOpen();
  }

  HDF5File::~HDF5File()
  {
    close();
    if (itsDelete) {
      RegularFile file(getName());
      file.remove();
    }
  }

  Bool HDF5File::isHDF5 (const String& name)
  {
    // Disable automatic printing of errors.
    H5Eset_auto2(H5E_DEFAULT, NULL, NULL);
    return H5Fis_hdf5 (name.chars());
  }

  void HDF5File::reopenRW()
  {
    if (itsOption != ByteIO::Update) {
      close();
      itsOption = ByteIO::Update;
    }
    reopen();
  }

  void HDF5File::close()
  {
    if (isValid()) {
      // Do not check for errors.
      // If the same file is opened twice, HDF5 will close the file on
      // the first occasion and gives an error for the second close.
      H5Fflush (getHid(), H5F_SCOPE_LOCAL);
      H5Fclose (getHid());
      clearHid();
    }
  }
    
  void HDF5File::reopen()
  {
    if (isClosed()) {
      doOpen();
    }
  }

  void HDF5File::flush()
  {
    if (isValid()) {
      if (H5Fflush (getHid(), H5F_SCOPE_LOCAL) < 0) {
	throw HDF5Error ("Failure when flushing file " + getName());
      }
    }
  }

  void HDF5File::doOpen()
  {
    // Use 8 byte offets and blocks of 32768 bytes.
    HDF5HidProperty create_plist (H5Pcreate(H5P_FILE_CREATE));
    H5Pset_sizes(create_plist, 8, 8);
    // Use unbuffered IO.
    HDF5HidProperty access_plist (H5Pcreate(H5P_FILE_ACCESS));
    H5Pset_fapl_sec2(access_plist);
    // Set a cache size of 16 MB.
    // First get old values and use them for the other parameters.
    int mdn;
    size_t ccn, ccb;
    double policy;
    H5Pget_cache(access_plist, &mdn, &ccn, &ccb, &policy);
    ccb = 16*1024*1024;
    H5Pset_cache(access_plist, mdn, ccn, ccb, policy);
    // Open or create the file.
    switch (itsOption) {
    case ByteIO::Old:
      setHid (H5Fopen (getName().chars(), H5F_ACC_RDONLY, access_plist));
      break;
    case ByteIO::Delete:
      itsDelete = True;
      // fall through
    case ByteIO::Update:
    case ByteIO::Append:
      setHid (H5Fopen (getName().chars(), H5F_ACC_RDWR, access_plist));
      itsOption = ByteIO::Update;
      break;
    case ByteIO::Scratch:
      itsDelete = True;
      // fall through
    case ByteIO::New:
      setHid (H5Fcreate (getName().chars(), H5F_ACC_TRUNC,
			 create_plist, access_plist));
      itsOption = ByteIO::Update;
      break;
    case ByteIO::NewNoReplace:
      setHid (H5Fcreate (getName().chars(), H5F_ACC_EXCL,
			 create_plist, access_plist));
      itsOption = ByteIO::Update;
      break;
    }
    if (!isValid()) {
      throw HDF5Error ("Could not open or create HDF5 file " + getName());
    }
  }

#else

  HDF5File::HDF5File (const String& name,
		      ByteIO::OpenOption option)
    : itsOption (option),
      itsDelete (False)
  {
    setName (name);
    doOpen();
  }

  HDF5File::~HDF5File()
  {}

  Bool HDF5File::isHDF5 (const String&)
  {
    return False;
  }

  void HDF5File::reopenRW()
  {}

  void HDF5File::close()
  {}
    
  void HDF5File::reopen()
  {}

  void HDF5File::flush()
  {}

  void HDF5File::doOpen()
  {
    throw HDF5Error("HDF5 support is not compiled into this casacore version");
  }

#endif

}
