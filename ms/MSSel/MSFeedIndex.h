//# MSFeedIndex: index into a MeasurementSet FEED subtable
//# Copyright (C) 2000,2001,2002
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

#ifndef MS_MSFEEDINDEX_H
#define MS_MSFEEDINDEX_H

#include <casacore/casa/aips.h>
#include <casacore/ms/MSSel/MSTableIndex.h>

#include <casacore/ms/MeasurementSets/MSFeedColumns.h>
#include <casacore/casa/Containers/RecordField.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# forward declarations
class MSFeed;

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

class MSFeedIndex : public MSTableIndex
{
public:
    // no index attached, use the attach function or assignment operator to change that
  MSFeedIndex();

  // construct one using the indicated FEED table
  MSFeedIndex(const MSFeed &feed);

  // construct one from another
  MSFeedIndex(const MSFeedIndex &other);

  virtual ~MSFeedIndex();

  MSFeedIndex &operator=(const MSFeedIndex &other);

  void attach(const MSFeed &feed);

  // access to the antenna ID key, throws an exception if isNull() is False
  Int &antennaId() {return *antennaId_p;}

  // access to the feed ID key, throws an exception if isNull() is False
  Int &feedId() {return *feedId_p;}

  // access to the spectral window ID key, throws an exception if isNull() is False
  Int &spectralWindowId() {return *spwId_p;}

  // return feed id.'s (and associated row numbers) for a given antenna id., 
  // polzn type and receptor angle
  Vector<Int> matchFeedPolznAndAngle(const Int& antennaId, 
				     const Vector<String>& polznType,
				     const Vector<Float>& receptorAngle,
				     const Float& tol,
				     Vector<Int>& rowNumbers);

  // return feed id.'s (and associated row numbers) for a given antenna id.
  Vector<Int> matchAntennaId(const Int& antennaId, Vector<Int>& rowNumbers);

protected:
  // the specialized compare function to pass to the
  // <linkto class=ColumnsIndex>ColumnsIndex</linkto> object.  This supports -1
  // values for the SPECTRAL_WINDOW_ID
  static Int compare (const Block<void*>& fieldPtrs,
                      const Block<void*>& dataPtrs,
                      const Block<Int>& dataTypes,
                      Int index);
  
private:
  RecordFieldPtr<Int> antennaId_p, feedId_p, spwId_p;

  // Pointer to FEED columns accessor
  ROMSFeedColumns* msFeedCols_p;

  void attachIds();
};


} //# NAMESPACE CASACORE - END

#endif
    
