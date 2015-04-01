//# MSFreqOffIndex: index into a MeasurementSet FREQ_OFFSET subtable
//# Copyright (C) 2000,2002
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

#ifndef MS_MSFREQOFFINDEX_H
#define MS_MSFREQOFFINDEX_H

#include <casacore/casa/aips.h>
#include <casacore/ms/MSSel/MSTableIndex.h>

#include <casacore/casa/Containers/RecordField.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# forward declarations
class MSFreqOffset;

// <summary>
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MeasurementSet
//   <li> MSTableIndex
// </prerequisite>
//
// <etymology>
// </etymology>
//
// <synopsis>
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// </motivation>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//

class MSFreqOffIndex : public MSTableIndex
{
public:
    // no index attached, use the attach function or assignment operator to change that
  MSFreqOffIndex();

  // construct one using the indicated FREQOFF table
  MSFreqOffIndex(const MSFreqOffset &freqOffset);

  // construct one from another
  MSFreqOffIndex(const MSFreqOffIndex &other);

  virtual ~MSFreqOffIndex();

  MSFreqOffIndex &operator=(const MSFreqOffIndex &other);

  void attach(const MSFreqOffset &freqOffset);

  // access to the antenna1 ID key, throws an exception if isNull() is False
  Int &antenna1Id() {return *antenna1Id_p;}

  // access to the antenna2 ID key, throws an exception if isNull() is False
  Int &antenna2Id() {return *antenna2Id_p;}

  // access to the feed ID key, throws an exception if isNull() is False
  Int &feedId() {return *feedId_p;}

  // access to the spectral window ID key, throws an exception if isNull() is False
  Int &spectralWindowId() {return *spwId_p;}
private:
  RecordFieldPtr<Int> antenna1Id_p, antenna2Id_p, feedId_p, spwId_p;

  void attachIds();
};


} //# NAMESPACE CASACORE - END

#endif
    
