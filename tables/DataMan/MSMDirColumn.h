//# MSMDirColumn.h: Memory storage manager for fixed shape table arrays
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
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id: MSMDirColumn.h 20551 2009-03-25 00:11:33Z Malte.Marquarding $

#ifndef TABLES_MSMDIRCOLUMN_H
#define TABLES_MSMDIRCOLUMN_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/MSMColumn.h>
#include <casacore/casa/Arrays/IPosition.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Memory storage manager for table arrays
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
// MSMDirColumn handles arrays in a table column.
// It only keeps them in memory, so they are not persistent.
// </synopsis> 

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class MSMDirColumn: public MSMColumn
{
public:
  // Create a column of the given type.
  MSMDirColumn (MSMBase* smptr, int dataType);

  // Frees up the storage.
  virtual ~MSMDirColumn();

  // Set the (fixed) shape of the arrays in the entire column.
  virtual void setShapeColumn (const IPosition& shape);

  // Add (newNrrow-oldNrrow) rows to the column.
  // Allocate the data arrays in these rows if the shape is fixed.
  virtual void addRow (uInt newNrrow, uInt oldNrrow);

  // Get the dimensionality of the item in the given row.
  // 0 is returned if there is no array.
  virtual uInt ndim (uInt rownr);

  // Get the shape of the array in the given row.
  // An zero-length IPosition is returned if there is no array.
  virtual IPosition shape (uInt rownr);

  // Get an array value in the given row.
  // The buffer pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ArrayColumn get function).
  virtual void getArrayV (uInt rownr, ArrayBase& dataPtr);
  
  // Put an array value into the given row.
  // The buffer pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ArrayColumn put function).
  virtual void putArrayV (uInt rownr, const ArrayBase& dataPtr);

  // Get a section of the array in the given row.
  // The buffer pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ArrayColumn getSlice function).
  ///virtual void getSliceV (uInt rownr, const Slicer&, ArrayBase& dataPtr);

  // Put into a section of the array in the given row.
  // The buffer pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ArrayColumn putSlice function).
  ///virtual void putSliceV (uInt rownr, const Slicer&, const ArrayBase& dataPtr);

  // Get all array values in the column.
  // The buffer pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ArrayColumn getColumn function).
  ///void getArrayColumnV (ArrayBase& dataPtr);

  // Put all arrays in the column.
  // The buffer pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ArrayColumn putColumn function).
  ///void putArrayColumnV (const ArrayBase& dataPtr);

  // Remove the value in the given row.
  void remove (uInt rownr);

  // Let the column create its arrays.
  void doCreate (uInt nrrow);

private:
  // The (unique) sequence number of the column.
  uInt seqnr_p;
  // The shape of the array.
  IPosition shape_p;
  // The nr of elements in the array.
  uInt nrelem_p;


  // Delete the array in the given row.
  void deleteArray (uInt rownr);

  // Forbid copy constructor.
  MSMDirColumn (const MSMDirColumn&);

  // Forbid assignment.
  MSMDirColumn& operator= (const MSMDirColumn&);
};


} //# NAMESPACE CASACORE - END

#endif
