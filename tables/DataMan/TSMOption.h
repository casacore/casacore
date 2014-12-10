//# TSMOption.h: Options for the Tiled Storage Manager Access
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

#ifndef TABLES_TSMOPTION_H
#define TABLES_TSMOPTION_H


//# Includes
#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Options for the Tiled Storage Manager Access
// </summary>

// <use visibility=export>

// <reviewed reviewer="TPPR" date="08.11.94" tests="tTiledShapeStMan.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TiledStMan>TiledStMan</linkto>
// </prerequisite>

// <synopsis>
// This class can be used to define how the Tiled Storage Manager accesses
// its data. There are three ways:
// <ol>
//  <li> Using a cache of its own. The cache size is derived using the hinted
//       access pattern. The cache can be (too) large when using large tables
//       with bad access patterns.
//       A maximum cache size can be defined to overcome this problem, but
//       that may result in poor caching behaviour.
//       Until January 2010 this was the only way to access the data.
//  <li> Use memory-mapped IO (mmap); the operating system take care of caching.
//       On 32-bit systems mmap cannot be used for larger tables due to the
//       4 GB address space limit.
//       When creating or extending files, mmap can be disadvantageous because
//       extending the file requires remapping it.
//  <li> Use buffered IO; the kernel's file cache should avoid unnecessary IO.
//       Its performance is less than mmap, but it works well on 32-bit systems.
//       The buffer size to be used can be defined.
// </ol>
//
// The constructor of the class can be used to define the options or
// to read options from the aipsrc file.
// <ul>
//  <li> <src>TSMOption::Cache</src>
//       Use unbuffered file IO with internal TSM caching. This is the old
//       behaviour.
//       The maximum cache size can be given as a constructor argument.
//  <li> <src>TSMOption::MMap</src>
//       Use memory-mapped IO.
//  <li> <src>TSMOption::Buffer</src>
//       Use buffered file IO without.
//       The buffer size can be given as a constructor argument.
//  <li> <src>TSMOption::Default</src>
//       Use default. This is MMap for existing files on 64-bit systems,
//       otherwise Buffer.
//  <li> <src>TSMOption::Aipsrc</src>
//       Use the option as defined in the aipsrc file.
// </ul>
// The aipsrc variables are:
// <ul>
//  <li> <src>tables.tsm.option</src> gives the option as the case-insensitive
//       string value:
//   <ul>
//    <li> <src>cache</src> means TSMCache.
//    <li> <src>mmap</src> (or <src>map</src>) means TSMMap.
//    <li> <src>mmapold</src> (or <src>mapold</src>) means TSMMap for existing
//         tables and TSMDefault for new tables.
//    <li> <src>buffer</src> means TSMBuffer.
//    <li> <src>default</src> means TSMDefault.
//   </ul>
//       It defaults to value <src>default</src>.
//       Note that <src>mmapold</src> is almost the same as <src>default</src>.
//       Only on 32-bit systems it is different.
//  <li> <src>tables.tsm.maxcachesizemb</src> gives the maximum cache size in MB
//       for option <src>TSMOption::Cache</src>. A value -1 means that
//       the system determines the maximum. A value 0 means unlimited.
//       It defaults to -1.
//       Note it can always be overridden using class ROTiledStManAccessor.
//  <li> <src>tables.tsm.buffersize</src> gives the buffer size for option
//       <src>TSMOption::Buffer</src>. A value <=0 means use the default 4096.
//       It defaults to 0.
// </ul>
// </synopsis>


  class TSMOption
  {
  public:
    // Define the possible options how the TiledStMan accesses its data.
    enum Option {
      // Use unbuffered file IO with internal TSM caching.
      Cache,
      // Use buffered file IO without internal TSM caching.
      Buffer,
      // Use memory-mapped IO.
      MMap,
      // Use default.
      Default,
      // Use as defined in the aipsrc file.
      Aipsrc
    };

    // Create an option object.
    // The parameter values are described in the synopsis.
    // A size value -2 means reading that size from the aipsrc file.
    TSMOption (Option option=Aipsrc, Int bufferSize=-2,
               Int maxCacheSizeMB=-2);

    // Fill the option in case Aipsrc or Default was given.
    // It is done as explained in the synopsis.
    void fillOption (Bool newFile);

    // Get the option.
    Option option() const
      { return itsOption; }

    // Get the buffer size.
    Int bufferSize() const
      { return itsBufferSize; }

    // Get the maximum cache size. -1 means undefined.
    Int maxCacheSizeMB() const
      { return itsMaxCacheSize; }

  private:
    Option itsOption;
    Int    itsBufferSize;
    Int    itsMaxCacheSize;
  };

} //# NAMESPACE CASACORE - END

#endif
