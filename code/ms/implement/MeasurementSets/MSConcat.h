//# MSConcat.h: A class for concatenating MeasurementSets.
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

#if !defined(AIPS_MSCONCAT_H)
#define AIPS_MSCONCAT_H

#include <aips/aips.h>
#include <aips/MeasurementSets/NewMSColumns.h>
#include <aips/MeasurementSets/NewMeasurementSet.h>
#include <aips/Arrays/IPosition.h>

class TableDesc;
class NewMSAntenna;
class NewMSFeed;
template <class T> class Block;

// <summary>A class with functions for concatenating MeasurementSets</summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> SomeClass
//   <li> SomeOtherClass
//   <li> some concept
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
// <templating arg=T>
//    <li>
//    <li>
// </templating>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//
// <todo asof="yyyy/mm/dd">
//   <li> add this feature
//   <li> fix this bug
//   <li> start discussion of this possible extension
// </todo>

class MSConcat: public NewMSColumns
{
public:
  MSConcat(NewMeasurementSet& ms);
  void concatenate(const NewMeasurementSet& otherMS);
private:
  MSConcat();
  static IPosition isFixedShape(const TableDesc& td);
  static IPosition getShape(const RONewMSColumns& msCols, uInt whichShape);
  void checkShape(const IPosition& otherShape) const;
  void checkCategories(const RONewMSColumns& otherCols) const;
  Block<uInt> copyAntennaAndFeed(const NewMSAntenna& otherAnt,
				 const NewMSFeed& otherFeed);

  NewMeasurementSet itsMS;
  IPosition itsFixedShape;
};
#endif


