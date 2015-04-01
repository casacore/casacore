//# StorageOption.cc: Options defining how table files are organized
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
//# $Id: StorageOption.cc 21154 2011-12-02 07:19:09Z gervandiepen $

#include <casacore/tables/Tables/StorageOption.h>
#include <casacore/casa/System/AipsrcValue.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  StorageOption::StorageOption (StorageOption::Option option, Int blockSize)
    : itsOption    (option),
      itsBlockSize (blockSize)
  {}

  void StorageOption::fillOption()
  {
    // Get variables from aipsrc if needed.
    if (itsOption == StorageOption::Aipsrc) {
      String opt;
      AipsrcValue<String>::find (opt, "table.storage.option", "default");
      opt.downcase();
      if (opt == "multifile") {
        itsOption = StorageOption::MultiFile;
      } else if (opt == "multihdf5") {
        itsOption = StorageOption::MultiHDF5;
      } else if (opt == "sepfile") {
        itsOption = StorageOption::SepFile;
      } else {
        itsOption = StorageOption::Default;
      }
    }
    // Default block size is 4MB.
    if (itsBlockSize <= -2) {
      AipsrcValue<Int>::find (itsBlockSize, "table.storage.blocksize", 0);
    }
    if (itsBlockSize <= 0) {
      itsBlockSize = 4*1024*1024;
    }
    // Default is to use separate files.
    if (itsOption == StorageOption::Default) {
      itsOption = StorageOption::SepFile;
    }
  }

} //# NAMESPACE CASACORE - END
