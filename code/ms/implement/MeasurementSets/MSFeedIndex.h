//# NewMSFeedIndex: index into a MeasurementSet FEED subtable
//# Copyright (C) 2000
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

#if !defined(AIPS_NEWMSFEEDINDEX_H)
#define AIPS_NEWMSFEEDINDEX_H

#include <trial/MeasurementSets/NewMSTableIndex.h>

#include <aips/Containers/RecordField.h>

//# forward declarations
class NewMSFeed;

// <summary>
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MeasurementSet
//   <li> NewMSTableIndex
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

class NewMSFeedIndex : public NewMSTableIndex
{
public:
    // no index attached, use the attach function or assignment operator to change that
  NewMSFeedIndex();

  // construct one using the indicated FEED table
  NewMSFeedIndex(const NewMSFeed &feed);

  // construct one from another
  NewMSFeedIndex(const NewMSFeedIndex &other);

  virtual ~NewMSFeedIndex();

  virtual NewMSFeedIndex &operator=(const NewMSFeedIndex &other);

  virtual void attach(const NewMSFeed &feed);

  // access to the antenna ID key, throws an exception if isNull() is False
  Int &antennaId() {return *antennaId_p;}

  // access to the feed ID key, throws an exception if isNull() is False
  Int &feedId() {return *feedId_p;}

  // access to the spectral window ID key, throws an exception if isNull() is False
  Int &spectralWindowId() {return *spwId_p;}
private:
  RecordFieldPtr<Int> antennaId_p, feedId_p, spwId_p;

  void attachIds();
};

#endif
    
