//# StArrAipsIO.h: AipsIO storage manager for direct table arrays
//# Copyright (C) 1994,1995,1996,1999
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
//# $Id: StArrAipsIO.h 20551 2009-03-25 00:11:33Z Malte.Marquarding $

#ifndef TABLES_STARRAIPSIO_H
#define TABLES_STARRAIPSIO_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/StManAipsIO.h>
#include <casacore/tables/DataMan/MSMDirColumn.h>
#include <casacore/casa/Arrays/IPosition.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class AipsIO;


// <summary>
// AipsIO storage manager for direct table arrays
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> StManAipsIO
//   <li> StManColumnAipsIO
// </prerequisite>

// <etymology>
// StManColumnArrayAipsIO handles the access to a direct array in a table
// column using the AipsIO storage manager.
// </etymology>

// <synopsis> 
// StManColumnArrayAipsIO holds the direct arrays in memory and writes
// them into the AipsIO file when the table gets closed.
// It fully supports addition and removal of rows.
// When a row is added to the table, the direct array gets allocated.
// This is possible, because the shape of direct arrays is known.
//
// The class is derived from StManColumnAipsIO which is used to hold
// a pointer to the array.
// </synopsis> 

// <motivation>
// StManColumnArrayAipsIO handles the standard data types. The class
// is not templated, but a switch statement is used instead.
// Templates would cause too many instantiations.
// </motivation>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//   <li> Maybe TpArrayInt, etc. should be used instead of TpInt.
//   <li> get/putSlice use too many array operations.
//          To solve this requires an array constructor taking a
//          pointer to the data (which is foreseen in the new Array classes).
// </todo>


class StManColumnArrayAipsIO : public StManColumnAipsIO
{
public:

  // Create a column of the given data type.
  StManColumnArrayAipsIO (StManAipsIO*, int dataType);

  // Frees up the storage.
  virtual ~StManColumnArrayAipsIO();

  // Set the (fixed) shape of the arrays in the entire column.
  virtual void setShapeColumn (const IPosition& shape);

  // Add (newNrrow-oldNrrow) rows to the column.
  // Allocate the data arrays in these rows if the shape is fixed.
  virtual void addRow (rownr_t newNrrow, rownr_t oldNrrow);

  // Get the dimensionality of the item in the given row.
  // 0 is returned if there is no array.
  virtual uInt ndim (rownr_t rownr);

  // Get the shape of the array in the given row.
  // An zero-length IPosition is returned if there is no array.
  virtual IPosition shape (rownr_t rownr);

  // Get an array value in the given row.
  // The buffer pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ArrayColumn get function).
  virtual void getArrayV (rownr_t rownr, ArrayBase& dataPtr);
  
  // Put an array value into the given row.
  // The buffer pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ArrayColumn put function).
  virtual void putArrayV (rownr_t rownr, const ArrayBase& dataPtr);

  // Remove the value in the given row.
  virtual void remove (rownr_t rownr);

  // Let the column create its arrays.
  virtual void doCreate (rownr_t nrrow);

  // Write the data into AipsIO.
  // This will call StManColumnAipsIO::putFile which will in its turn
  // call putData in this class for each of its chunks of data.
  virtual void putFile (rownr_t nrval, AipsIO&);

  // Read the data from AipsIO.
  // This will call StManColumnAipsIO::getFile which will in its turn
  // call getData in this class for each of its chunks of data.
  virtual void getFile (rownr_t nrval, AipsIO&);

private:
  // The (unique) sequence number of the column.
  uInt seqnr_p;
  // The shape of the array.
  IPosition shape_p;
  // The nr of elements in the array.
  uInt nrelem_p;

  // Delete the array at the given index.
  void deleteArray (rownr_t index);

  // Put the data of a data block.
  // datap is an array of nrval pointers to arrays.
  virtual void putData (void* datap, uInt nrval, AipsIO&);

  // Get data arrays into a data block at the given index.
  // datap is an array of pointers to arrays. nrval arrays will
  // be allocated and read starting at datap[index].
  virtual void getData (void* datap, uInt index, uInt nrval,
                        AipsIO&, uInt version);

  // Forbid copy constructor.
  StManColumnArrayAipsIO (const StManColumnArrayAipsIO&);

  // Forbid assignment.
  StManColumnArrayAipsIO& operator= (const StManColumnArrayAipsIO&);
};


} //# NAMESPACE CASACORE - END

#endif
