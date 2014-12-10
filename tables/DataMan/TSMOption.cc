//# TSMOption.cc: Options for the Tiled Storage Manager Access
//# Copyright (C) 2010
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
//# $Id$

#include <casacore/tables/DataMan/TSMOption.h>
#include <casacore/casa/System/AipsrcValue.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  TSMOption::TSMOption (TSMOption::Option option, Int bufferSize,
                        Int maxCacheSizeMB)
    : itsOption       (option),
      itsBufferSize   (bufferSize),
      itsMaxCacheSize (maxCacheSizeMB)
  {}

  void TSMOption::fillOption (Bool newTable)
  {
    // Get variables from aipsrc if needed.
    if (itsOption == TSMOption::Aipsrc) {
      String opt;
      AipsrcValue<String>::find (opt, "table.tsm.option", "cache");
      opt.downcase();
      if (opt == "map"  ||  opt == "mmap") {
        itsOption = TSMOption::MMap;
      } else if (opt == "cache") {
        itsOption = TSMOption::Cache;
        ///      } else if (opt == "buffer") {
        ///        itsOption = TSMOption::Buffer;
      } else if (opt == "default32") {
        itsOption = (newTable ? TSMOption::Cache : TSMOption::MMap);
      } else {
        itsOption = TSMOption::Default;
      }
    }
    // Default buffer size is 4096.
    if (itsBufferSize <= -2) {
      AipsrcValue<Int>::find (itsBufferSize, "table.tsm.buffersize", 0);
    }
    if (itsBufferSize <= 0) {
      itsBufferSize = 4096;
    }
    // Default is -1.
    if (itsMaxCacheSize <= -2) {
      AipsrcValue<Int>::find (itsMaxCacheSize, "table.tsm.maxcachesizemb", -1);
    }
    // Default is to use the old caching behaviour
    // Abandoned default to use mmap for existing files on 64 bit systems.
    if (itsOption == TSMOption::Default) {
      itsOption = TSMOption::Cache;
      ///#ifdef AIPS_64B
      ///      if (!newTable) {
      ///        itsOption = TSMOption::MMap;
      ///      }
      ///#endif
    }
  }

} //# NAMESPACE CASACORE - END
