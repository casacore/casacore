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

#ifndef CASA_HDF5FILE_H
#define CASA_HDF5FILE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/HDF5/HDF5Object.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/IO/ByteIO.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // <summary>
  // A class representing an HDF5 file.
  // </summary>

  // <use visibility=export>

  // <reviewed reviewer="" date="" tests="tHDF5File.cc">
  // </reviewed>

  // <prerequisite>
  //   <li> <a href="http://hdf.ncsa.uiuc.edu">HDF5 system</a>
  // </prerequisite>

  // <synopsis>
  // This class wraps the HDF5 functions to open, create, or close an
  // HDF5 file.
  // If the file is opened as readonly, it is possible to reopen it for
  // read/write (provided the user has the correct privileges).
  // It is also possible to temporarily close the file and reopen it later.
  // <note> It is ensured that the class and the static function <tt>isHDF5</tt>
  // are also defined if HDF5 is not compiled in. </note>
  // </synopsis> 

  // <motivation>
  // It was overkill to use the HDF5 C++ interface. Instead little wrappers
  // have been written. HDF5File can be embedded in a shared pointer making
  // it possible to share an HDF5 file amongst various HDF5Array objects.
  // </motivation>

  class HDF5File : public HDF5Object
  {
  public:
    // Create an HDF5 file object with the given file name (possible tilde
    // or environment variables in it will be expanded).
    // The ByteIO option determines if the file will be created,
    // opened for input and/or output, or possibly deleted by the destructor.
    explicit HDF5File (const String& name,
		       ByteIO::OpenOption = ByteIO::Old);

    // The destructor closes the file and deletes it when it was opened
    // using ByteIO::Scratch or ByteIO::Delete.
    ~HDF5File();

    // Test if the file with the given name is an HDF5 file.
    static Bool isHDF5 (const String& name);

    // Reopen the underlying file for read/write access.
    // Nothing will be done if the stream is writable already.
    // Otherwise it will be reopened and an exception will be thrown
    // if it is not possible to reopen it for read/write access.
    void reopenRW();

    // Is the file writable?
    Bool isWritable() const
      { return itsOption == ByteIO::Update; }

    // Is the file opened for delete?
    Bool isOpenedForDelete() const
      { return itsDelete; }

    // Is the file temporarily closed?
    Bool isClosed() const
      { return getHid()<0; }

    // Close the file (temporarily).
    // Note it will not delete the file; that is only done by the destructor.
    virtual void close();

    // Reopen the file if closed (which may change the HID).
    void reopen();

    // Flush the data to disk.
    void flush();

    // Get or set the chunk cache size (in bytes).
    // Note that all data sets in a file share the cache.
    // <group>
    size_t getChunkCacheSize() const;
    void setChunkCacheSize (size_t nbytes);
    // </group>

  private:
    // Open or create the file.
    void doOpen();


    //# Data members
    ByteIO::OpenOption itsOption;
    String             itsName;
    Bool               itsDelete;

  private:
    // Copy constructor cannot be used.
    HDF5File (const HDF5File& that);

    // Assignment cannot be used.
    HDF5File& operator= (const HDF5File& that);
  };

}

#endif
