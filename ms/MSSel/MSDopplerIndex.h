//# MSDopplerIndex: index into a MeasurementSet DOPPLER subtable
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

#ifndef MS_MSDOPPLERINDEX_H
#define MS_MSDOPPLERINDEX_H

#include <casacore/casa/aips.h>
#include <casacore/ms/MSSel/MSTableIndex.h>

#include <casacore/casa/Containers/RecordField.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# forward declarations
class MSDoppler;

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

class MSDopplerIndex : public MSTableIndex
{
public:
    // no index attached, use the attach function or assignment operator to change that
  MSDopplerIndex();

  // construct one using the indicated DOPPLER table
  MSDopplerIndex(const MSDoppler &doppler);

  // construct one from another
  MSDopplerIndex(const MSDopplerIndex &other);

  virtual ~MSDopplerIndex();

  MSDopplerIndex &operator=(const MSDopplerIndex &other);

  void attach(const MSDoppler &doppler);

  // access to the doppler ID key, throws an exception if isNull() is False
  Int &dopplerId() {return *dopplerId_p;}

  // access to the source ID key, throws an exception if isNull() is False
  Int &sourceId() {return *sourceId_p;}
private:
  RecordFieldPtr<Int> dopplerId_p, sourceId_p;

  void attachIds();
};


} //# NAMESPACE CASACORE - END

#endif
    
