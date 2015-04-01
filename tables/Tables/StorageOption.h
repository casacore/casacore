//# StorageOption.h: Options defining how table files are organized
//# Copyright (C) 2014
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
//# You should have receied a copy of the GNU Library General Public License
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
//# $Id: StorageOption.h 20859 2010-02-03 13:14:15Z gervandiepen $

#ifndef TABLES_STORAGEOPTION_H
#define TABLES_STORAGEOPTION_H


//# Includes
#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Options defining how table files are organized
// </summary>

// <use visibility=export>

// <reviewed reviewer="TPPR" date="08.11.94" tests="tTable.cc">
// </reviewed>

// <synopsis>
// This class can be used to define how the files of a table are organized.
// There are two ways:
// <ol>
//  <li> The old way where each storage manager has its own file(s).
//  <li> Using MultiFile that storage managers can use to combine about all
//       table files in a single file. This mode is particularly useful
//       for new file systems (like Lustre) requiring large block sizes.
//       <br>The block size to be used in a MultiFile can be defined in
//           this class. Default is 4 MByte.
//  <li> Using MultiHDF5 which behaves similar to MultiFile but uses an
//       HDF5 file instead of a regular file.
// </ol>
// It is possible to specify the storage type and block size using aipsrc.
// The aipsrc variables are:
// <ul>
//  <li> <src>tables.storage.type</src>. The (case-insensitive) value can be
//       'multifile' or 'multihdf5'.
//       Another value means the old way (separate files).
//  <li> <src>tables.storage.blocksize</src> gives the default blocksize to be
//       used for the multifile and multihdf5 option.
// </ul>
// </synopsis>


  class StorageOption
  {
  public:
    // Define the possible options how table files are organized.
    enum Option {
      // Let storage managers use a combined MultiFile.
      MultiFile,
      // Let storage managers use a combined MultiHDF5.
      MultiHDF5,
      // Let storage managers use separate files.
      SepFile,
      // Use default (currently MultiFile).
      Default,
      // Use as defined in the aipsrc file.
      Aipsrc
    };

    // Create an option object.
    // The parameter values are described in the synopsis.
    // A size value -2 means reading that size from the aipsrc file.
    StorageOption (Option option=Aipsrc, Int blockSize=-2);

    // Fill the option in case Aipsrc or Default was given.
    // It is done as explained in the synopsis.
    void fillOption();

    // Get the option.
    Option option() const
      { return itsOption; }

    // Get the block size.
    uInt blockSize() const
      { return itsBlockSize; }

  private:
    Option itsOption;
    Int    itsBlockSize;
  };

} //# NAMESPACE CASACORE - END

#endif
