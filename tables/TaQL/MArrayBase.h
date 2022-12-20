//# MArrayBase.h: Base class for an array with an optional mask
//# Copyright (C) 2012
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

#ifndef CASA_MARRAYBASE_H
#define CASA_MARRAYBASE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayLogical.h>

//# Define the mask value indicating a valid value.
//# In this way it is easy to change it to another value (if ever needed).
//# The current setting is the same as used in numpy's masked_array and
//# in the MeasurementSet's FLAG column.
//# But the opposite value sounds somewhat better (same as MaskedArray)
//# because something like DATA[isnan(DATA)] = 0 is much more intuitive.
//#  #define MArrayValid False
//#  #define MArrayInvalid True


namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // <summary>
  // Base class for an array with an optional mask
  // </summary>

  // <use visibility=local>

  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
  // </reviewed>

  // <prerequisite>
  //# Classes you should understand before using this one.
  //   <li> <linkto class=Array>Array</linkto>
  // </prerequisite>

  // <synopsis> 
  // This class is the base class of the templated class MArray. It contains
  // the functions that are not template dependent.
  //
  // MArray is developed to make it easier to handle arrays with an
  // optional mask. The array is always present, but the mask is optional.
  // MArrayMath contains functions to operate on such arrays.
  //
  // Similar to numpy.masked_array and the MeasurementSet FLAG definition,
  // a mask value True means that the corresponding value is masked off,
  // thus is not taken into account in reduction functions like <src>sum</src>.
  // on a masked array. In operations like addition, masked off values are
  // processed because testing the mask value is more expensive than an
  // addition (even if the value is a NaN). For an operation with multiple
  // operands, the mask of the result is the OR of the operand masks.
  //
  // MArray can be null meaning that the array is a null value. It can be
  // used to indicate that a table cell does not contain an array.
  // A null MArray has an empty array and mask. Operations where an operand
  // is a null MArray, result in a null MArray.
  // </synopsis> 

  class MArrayBase
  {
  protected:
    // The default constructor creates an empty mask.
    explicit MArrayBase (Bool isNull)
      : itsSize   (0),
        itsNValid (0),
        itsNull   (isNull)
    {}

    // Construct from a given array shape and mask.
    MArrayBase (const ArrayBase& arr, const Array<Bool>& mask, Bool isNull);

    // Construct from a given array shape and mask from another MArray.
    MArrayBase (const ArrayBase& arr, const MArrayBase& marray);

    // Reference the mask and set the shape.
    void setBase (const ArrayBase& arr, const Array<Bool>& mask);

    // Reference another MArray.
    void referenceBase (const MArrayBase& other);

    // Set the array shape and resize the mask.
    void resizeBase (const ArrayBase& arr, Bool useMask);

  public:
    // Is the array null?
    Bool isNull() const
      { return itsNull; }

    // Remove the mask.
    void removeMask()
      { itsMask.resize(); itsNValid = itsSize; }

    // Is there a mask?
    Bool hasMask() const
      { return !itsMask.empty(); }

    // Set the mask. It checks if it matches the array shape.
    void setMask (const Array<Bool>& mask);

    // Get the mask. The returned array is empty if there is no mask.
    const Array<Bool>& mask() const
      { return itsMask; }
    Array<Bool>& wmask()
      { return itsMask; }

    // Return the number of valid array values, thus unflagged elements.
    Int64 nvalid() const
    {
      if (itsNValid < 0) fillNValid();
      return itsNValid;
    }

    // Is the array empty?
    Bool empty() const
      { return itsSize == 0; }

    // Get the dimensionality.
    uInt ndim() const
      { return itsShape.size(); }

    // Get the shape.
    const IPosition& shape() const
      { return itsShape; }

    // Get the size.
    // <group>
    size_t size() const
      { return itsSize; }
    size_t nelements() const
      { return itsSize; }
    // </group>

    // Combine this and the other mask.
    // One or both MArray-s can be unmasked.
    Array<Bool> combineMask (const MArrayBase& other) const;

  private:
    // Initialize and check.
    void init();

    // Fill the number of valid values.
    void fillNValid() const;

    //# Data members.
    Array<Bool>   itsMask;
    IPosition     itsShape;
    size_t        itsSize;
    mutable Int64 itsNValid;
    Bool          itsNull;   // True = array is null, thus undefined in a column
  };

} //# NAMESPACE CASACORE - END

#endif
