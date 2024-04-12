//# MSMIndColumn.h: Memory storage manager for variable shaped table arrays
//# Copyright (C) 2003
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#ifndef TABLES_MSMINDCOLUMN_H
#define TABLES_MSMINDCOLUMN_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/MSMColumn.h>
#include <casacore/tables/DataMan/MSMBase.h>
#include <casacore/casa/Arrays/IPosition.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Mmeory storage manager for variable shaped table arrays
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> MSMBase
//   <li> MSMColumn
// </prerequisite>

// <synopsis> 
// StManColumnArrayAipsIO handles indirect arrays in a table column.
//
// An array (or section of an array) is only read when needed.
// It, however, caches the array shape using the helper class
// StIndArray. Pointers to these objects
// are maintained using the standard StManColumnAipsIO facilities.
// When the column gets written, the offsets in the StManArrayFile file
// get written. Those will be read back when the column is read in.
//
// When a row gets deleted or when the array gets bigger, the file space
// is lost. This storage manager is a simple one and no attempts
// are done to make it smart.
// </synopsis> 

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class MSMIndColumn : public MSMColumn
{
public:

  // Create a column of the given type.
  MSMIndColumn (MSMBase*, int dataType);

  // Frees up the storage.
  ~MSMIndColumn();

  // Forbid copy constructor.
  MSMIndColumn (const MSMIndColumn&) = delete;

  // Forbid assignment.
  MSMIndColumn& operator= (const MSMIndColumn&) = delete;

  // Set the (fixed) shape of the arrays in the entire column.
  void setShapeColumn (const IPosition& shape);

  // Get the column shape.
  const IPosition& columnShape() const
    { return fixedShape_p; }

  // Set the shape of the array in the given row and allocate the array
  // in the file.
  void setShape (rownr_t rownr, const IPosition& shape);

  // Is the shape defined (i.e. is there an array) in this row?
  Bool isShapeDefined (rownr_t rownr);

  // Get the dimensionality of the item in the given row.
  // 0 is returned if there is no array.
  uInt ndim (rownr_t rownr);

  // Get the shape of the array in the given row.
  // An zero-length IPosition is returned if there is no array.
  IPosition shape (rownr_t rownr);

  // This storage manager can handle changing array shapes.
  Bool canChangeShape() const;

  // Get an array value in the given row.
  // The buffer given by <src>arr</src> has to have the correct length
  // (which is guaranteed by the ArrayColumn get function).
  void getArrayV (rownr_t rownr, ArrayBase& arr);

  // Put an array value into the given row.
  // The buffer given by <src>arr</src> has to have the correct length
  // (which is guaranteed by the ArrayColumn put function).
  void putArrayV (rownr_t rownr, const ArrayBase& arr);

  // Get a section of the array in the given row.
  // The buffer given by <src>arr</src> has to have the correct length
  // (which is guaranteed by the ArrayColumn getSlice function).
  void getSliceV (rownr_t rownr, const Slicer&, ArrayBase& arr);

  // Put into a section of the array in the given row.
  // The buffer given by <src>arr</src> has to have the correct length
  // (which is guaranteed by the ArrayColumn putSlice function).
  void putSliceV (rownr_t rownr, const Slicer&, const ArrayBase& arr);

  // Remove the value in the given row.
  // This will result in lost file space.
  void remove (rownr_t rownr);


private:
  class Data {
  public:
    Data (const IPosition& shape, int dtype, int elemSize);
    ~Data();
    Data (const Data&) = delete;
    Data& operator= (const Data&) = delete;
    const IPosition& shape() const {return shape_p;}
    void* data() {return data_p;}
  private:
    IPosition shape_p;
    void* data_p;
    bool data_is_string;
  };
  // The shape of all arrays in case it is fixed.
  IPosition fixedShape_p;
  // The size at the start of the data (for the IPosition).
  uInt startSize_p;


  // Delete the array in the given row.
  void deleteArray (rownr_t rownr);

  // Read the shape at the given row.
  // It throws an exception if undefined.
  Data* getShape (rownr_t rownr);

  // Get a pointer to the data array.
  void* getDataPtr (rownr_t rownr)
    { return (char*)(getShape(rownr)) + startSize_p; }
};



} //# NAMESPACE CASACORE - END

#endif
