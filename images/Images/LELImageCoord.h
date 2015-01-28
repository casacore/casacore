//# LELImageCoord.h: The letter class for image coordinates
//# Copyright (C) 1998,1999,2000,2001
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

#ifndef IMAGES_LELIMAGECOORD_H
#define IMAGES_LELIMAGECOORD_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/LEL/LELLattCoord.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/images/Images/ImageInfo.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Quanta/Unit.h>
#include <casacore/casa/Utilities/CountedPtr.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class LatticeExprNode;
class LattRegionHolder;


// <summary>
// The letter class for image coordinates.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=LELLattCoord>LELLattCoord</linkto>
// </prerequisite>

// <synopsis>
// This class is a letter class for the envelope class
// <linkto class=LELCoordinates>LELCoordinates</linkto>.
// It acts as the coordinates class for Lattice objects with
// proper coordinates (like PagedImage).
// </synopsis> 

// <motivation>
// It must be possible to handle image coordinates in a lattice.
// expression.
// </motivation>

//# <todo asof="1996/07/01">
//#   <li>
//# </todo>


class LELImageCoord : public LELLattCoord
{
public:
  LELImageCoord();

  LELImageCoord (const CoordinateSystem& coords, const ImageInfo& imageInfo,
		 const Unit& unit, const RecordInterface& miscInfo);

  virtual ~LELImageCoord();

  // Get the coordinates.
  const CoordinateSystem& coordinates() const;

  // Get the ImageInfo.
  const ImageInfo& imageInfo() const;

  // Get the brightness unit.
  const Unit& unit() const;

  // Get the MiscInfo.
  const TableRecord& miscInfo() const;

  // Create a SubLattice for an expression node.
  virtual LatticeExprNode makeSubLattice
                                    (const LatticeExprNode& expr,
				     const LattRegionHolder& region) const;

  // Create an extension for an expression node.
  virtual LatticeExprNode makeExtendLattice
                                    (const LatticeExprNode& expr,
				     const IPosition& newShape,
				     const LELLattCoordBase& newCoord) const;

  // Create a rebinning for an expression node.
  virtual LatticeExprNode makeRebinLattice
                                    (const LatticeExprNode& expr,
				     const IPosition& binning) const;

  // The class has true coordinates (thus returns True).
  virtual Bool hasCoordinates() const;

  // Get the coordinates of the spectral axis for the given shape.
  // It returns the pixel axis number of the spectral coordinates.
  // -1 indicates that there is no pixel spectral axis.
  // An exception is thrown if there are no world spectral coordinates.
  virtual uInt getSpectralInfo (Vector<Double>& worldCoordinates,
				const IPosition& shape) const;

  // The name of the class.
  virtual String classname() const;

  // Check how the coordinates of this and that compare.
  // The return value tells how they compare.
  // <br>-1: this is subset
  // <br>0: equal 
  // <br>1: this is superset
  // <br>9: invalid (mismatch)
  virtual Int compare (const LELLattCoordBase& other) const;

  // Check how the coordinates of this and that image compare.
  // This function is used by <src>conform</src> to make a
  // double virtual dispatch possible.
  virtual Int doCompare (const LELImageCoord& other) const;

private:
  CountedPtr<CoordinateSystem> coords_p;
  ImageInfo   imageInfo_p;
  Unit        unit_p;
  TableRecord miscInfo_p;
};


inline const CoordinateSystem& LELImageCoord::coordinates() const
{
  return *coords_p;
}

inline const ImageInfo& LELImageCoord::imageInfo() const
{
  return imageInfo_p;
}

inline const Unit& LELImageCoord::unit() const
{
  return unit_p;
}

inline const TableRecord& LELImageCoord::miscInfo() const
{
  return miscInfo_p;
}



} //# NAMESPACE CASACORE - END

#endif
