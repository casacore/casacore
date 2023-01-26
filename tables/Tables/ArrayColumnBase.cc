//# ArrayColumnBase.cc: Base classfor access to an array table column
//# Copyright (C) 2013
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

#include <casacore/tables/Tables/ArrayColumnBase.h>
#include <casacore/tables/Tables/ArrayColumnFunc.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/RefRows.h>
#include <casacore/casa/Arrays/ArrayBase.h>
#include <casacore/casa/Arrays/ArrayPosIter.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/ValTypeId.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Utilities/Assert.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

ArrayColumnBase::ArrayColumnBase()
: TableColumn ()
{}

ArrayColumnBase::ArrayColumnBase (const Table& tab, const String& columnName)
: TableColumn (tab, columnName)
{}

ArrayColumnBase::ArrayColumnBase (const TableColumn& column)
: TableColumn (column)
{}

ArrayColumnBase::ArrayColumnBase (const ArrayColumnBase& that)
: TableColumn (that)
{}

void ArrayColumnBase::reference (const ArrayColumnBase& that)
{
  TableColumn::reference (that);
}

ArrayColumnBase::~ArrayColumnBase()
{}


bool ArrayColumnBase::checkShape (const IPosition& expShape,
                                  const IPosition& arrShape,
                                  bool noSlicing, int64_t rownr,
                                  const String& where) const
{
  if (! expShape.isEqual (arrShape)) {
    if (noSlicing  &&  canChangeShape_p) {
      return false;
    }
    String msg(where);
    if (rownr >= 0) {
      msg += " for row " + String::toString(rownr);
    }
    throw TableArrayConformanceError(msg + " in column " +
                                     baseColPtr_p->columnDesc().name(),
                                     arrShape, expShape);
  }
  return true;
}

void ArrayColumnBase::adaptShape (const IPosition& shp,
                                  ArrayBase& arr, bool resize,
                                  int64_t rownr,
                                  const String& where) const
{
  if (! shp.isEqual (arr.shape())) {
    if (resize  ||  arr.nelements() == 0) {
      arr.resize (shp);
    } else {
      checkShape (shp, arr.shape(), false, rownr, where);
    }
  }
}

void ArrayColumnBase::acbGet (rownr_t rownr, ArrayBase& arr, bool resize) const
{
  TABLECOLUMNCHECKROW(rownr);
  // Check array conformance and resize if needed and possible.
  adaptShape (shape(rownr), arr, resize, rownr, "ArrayColumn::get");
  baseColPtr_p->getArray (rownr, arr);
}

void ArrayColumnBase::acbGetSlice (rownr_t rownr, const Slicer& arraySection,
                                   ArrayBase& arr, bool resize) const
{
  TABLECOLUMNCHECKROW(rownr);
  // Check array conformance and resize if needed and possible.
  IPosition arrayShape (shape(rownr));
  IPosition blc,trc,inc;
  IPosition shp = arraySection.inferShapeFromSource (arrayShape,
                                                     blc, trc, inc);
  adaptShape (shp, arr, resize, rownr, "ArrayColumn::getSlice");
  // Get the slice.
  //# Creating a Slicer is somewhat expensive, so use the slicer
  //# itself if it contains no undefined values.
  if (arraySection.isFixed()) {
    baseColPtr_p->getSlice (rownr, arraySection, arr);
  } else {
    baseColPtr_p->getSlice (rownr,
                            Slicer(blc, trc, inc, Slicer::endIsLast),
                            arr);
  }
}

void ArrayColumnBase::acbGetSlice (rownr_t rownr,
                                   const Vector<Vector<Slice> >& arraySlices,
                                   ArrayBase& arr, bool resize) const
{
  TABLECOLUMNCHECKROW(rownr);
  // Use shape of row.
  IPosition colShp = shape(rownr);
  // Check the slices. Make a copy because a Slice is inserted if empty.
  Vector<Vector<Slice> > slices(arraySlices);
  Slicer slicer;
  IPosition shp = Slice::checkSlices (slices, slicer, colShp);
  // Check array conformance and resize if needed and possible.
  adaptShape (shp, arr, resize, rownr, "ArrayColumn::getSlice");
  // Now loop through all the slices and fill the array in parts.
  GetCellSlices functor(*this, rownr);
  handleSlices (slices, functor, slicer, arr);
}

void ArrayColumnBase::acbGetColumn (ArrayBase& arr, bool resize) const
{
  rownr_t nrrow = nrow();
  //# Take shape of array in first row.
  IPosition shp;
  if (nrrow > 0) {
    shp = shape(0);
  }
  //# Total shape is array shape plus nr of table rows.
  shp.append (IPosition(1,nrrow));
  // Check array conformance and resize if needed and possible.
  adaptShape (shp, arr, resize, -1, "ArrayColumn::getColumn");
  if (!arr.empty()) {
    //# Get the column.
    baseColPtr_p->getArrayColumn (arr);
  }
}

void ArrayColumnBase::acbGetColumn (const Slicer& arraySection,
                                    ArrayBase& arr, bool resize) const
{
  rownr_t nrrow = nrow();
  //# Use shape of array in first row.
  IPosition shp, blc,trc,inc;
  if (nrrow > 0) {
    shp = arraySection.inferShapeFromSource (shape(0), blc, trc, inc);
  }
  //# Total shape is slice shape plus nr of table rows.
  shp.append (IPosition(1,nrrow));
  // Check array conformance and resize if needed and possible.
  adaptShape (shp, arr, resize, -1, "ArrayColumn::getColumn");
  if (!arr.empty()) {
      //# Get the column slice.
    Slicer defSlicer (blc, trc, inc, Slicer::endIsLast);
    baseColPtr_p->getColumnSlice (defSlicer, arr);
  }
}


void ArrayColumnBase::acbGetColumn (const Vector<Vector<Slice> >& arraySlices,
                                    ArrayBase& arr, bool resize) const
{
  rownr_t nrrow = nrow();
  // Get total shape.
  // Use shape of first row (if there) as overall array shape.
  IPosition colShp;
  if (nrrow > 0) {
    colShp = shape(0);
  }
  Vector<Vector<Slice> > slices(arraySlices);
  Slicer slicer;
  IPosition shp = Slice::checkSlices (slices, slicer, colShp);
  // Total shape is slice shape plus nr of table rows.
  shp.append (IPosition(1,nrrow));
  // Check array conformance and resize if needed and possible.
  adaptShape (shp, arr, resize, -1, "ArrayColumn::getColumn");
  // Now loop through all the slices and fill the array in parts.
  GetColumnSlices functor(*this);
  handleSlices (slices, functor, slicer, arr);
}

void ArrayColumnBase::acbGetColumnRange (const Slicer& rowRange,
                                         ArrayBase& arr, bool resize) const
{
  rownr_t nrrow = nrow();
  IPosition shp, blc, trc, inc;
  shp = rowRange.inferShapeFromSource (IPosition(1,nrrow), blc, trc, inc);
  //# If the entire column is accessed, use that function.
  if (blc(0) == 0  &&  shp(0) == int32_t(nrrow)  &&  inc(0) == 1) {
    acbGetColumn (arr, resize);
  } else {
    acbGetColumnCells (RefRows(blc(0), trc(0), inc(0)), arr, resize);
  }
}

void ArrayColumnBase::acbGetColumnCells (const RefRows& rownrs,
                                         ArrayBase& arr, bool resize) const
{
  rownr_t nrrow = rownrs.nrow();
  //# Take shape of array in first row.
  IPosition arrshp;
  if (nrrow > 0) {
    arrshp = shape(rownrs.firstRow());
  }
  //# Total shape is array shape plus nr of table rows.
  arrshp.append (IPosition(1,nrrow));
  // Check array conformance and resize if needed and possible.
  adaptShape (arrshp, arr, resize, -1, "ArrayColumn::getColumnCells");
  baseColPtr_p->getArrayColumnCells (rownrs, arr);
}

void ArrayColumnBase::acbGetColumnRange (const Slicer& rowRange,
                                         const Slicer& arraySection,
                                         ArrayBase& arr, bool resize) const
{
  rownr_t nrrow = nrow();
  IPosition shp, blc, trc, inc;
  shp = rowRange.inferShapeFromSource (IPosition(1,nrrow), blc, trc, inc);
  //# If the entire column is accessed, use that function.
  if (blc(0) == 0  &&  shp(0) == int32_t(nrrow)  &&  inc(0) == 1) {
    acbGetColumn (arraySection, arr, resize);
  } else {
    acbGetColumnCells (RefRows(blc(0), trc(0), inc(0)),
                       arraySection, arr, resize);
  }
}

void ArrayColumnBase::acbGetColumnCells (const RefRows& rownrs,
                                         const Slicer& arraySection,
                                         ArrayBase& arr, bool resize) const
{
  rownr_t nrrow = rownrs.nrow();
  IPosition arrshp, arrblc, arrtrc, arrinc;
  if (nrrow > 0) {
    arrshp = arraySection.inferShapeFromSource (shape(rownrs.firstRow()),
                                                arrblc, arrtrc, arrinc);
  }
  //# Total shape is slice shape plus nr of table rows.
  arrshp.append (IPosition(1,nrrow));
  // Check array conformance and resize if needed and possible.
  adaptShape (arrshp, arr, resize, -1, "ArrayColumn::getColumnCells");
  if (!arr.empty()) {
    //# Get the column slice.
    Slicer defSlicer (arrblc, arrtrc, arrinc, Slicer::endIsLast);
    baseColPtr_p->getColumnSliceCells (rownrs, defSlicer, arr);
  }
}


void ArrayColumnBase::setShape (rownr_t rownr, const IPosition& shape)
{
  checkWritable();
  TABLECOLUMNCHECKROW(rownr); 
  //# Set shape if not defined yet or if changed (if possible).
  //# Throw exception if already defined with a different shape.
  if (canChangeShape_p  ||  !isDefined(rownr)) {
    baseColPtr_p->setShape (rownr, shape);
  } else {
    if (! shape.isEqual (baseColPtr_p->shape (rownr))) {
      throw TableInvOper
        ("ArrayColumn::setShape; shape cannot be changed for row " +
         String::toString(rownr) + " in column " +
         baseColPtr_p->columnDesc().name());
    }
  }
}

void ArrayColumnBase::setShape (rownr_t rownr, const IPosition& shape,
                                const IPosition& tileShape)
{
  checkWritable();
  TABLECOLUMNCHECKROW(rownr); 
  //# Only set shape if not defined yet.
  //# Throw exception if already defined with a different shape.
  if (canChangeShape_p  ||  !isDefined(rownr)) {
    baseColPtr_p->setShape (rownr, shape, tileShape);
  } else {
    if (! shape.isEqual (baseColPtr_p->shape (rownr))) {
      throw TableInvOper
        ("ArrayColumn::setShape; shape cannot be changed for row " +
         String::toString(rownr) + " column " +
         baseColPtr_p->columnDesc().name());	
    }
  }
}
	

void ArrayColumnBase::acbPut (rownr_t rownr, const ArrayBase& arr)
{
  checkWritable();
  TABLECOLUMNCHECKROW(rownr); 
  //# Define the shape if not defined yet.
  //# If defined, check if shape conforms.
  if (!isDefined(rownr)) {
    baseColPtr_p->setShape (rownr, arr.shape());
  }else{
    if (! checkShape (baseColPtr_p->shape(rownr), arr.shape(), true, rownr,
                      "ArrayColumn::put")) {
      baseColPtr_p->setShape (rownr, arr.shape());
    }
  }
  baseColPtr_p->putArray (rownr, arr);
}

void ArrayColumnBase::acbPutSlice (rownr_t rownr, const Slicer& arraySection,
                                   const ArrayBase& arr)
{
  checkWritable();
  TABLECOLUMNCHECKROW(rownr); 
  //# Check the array conformance.
  IPosition arrayShape (shape(rownr));
  IPosition blc,trc,inc;
  IPosition shp = arraySection.inferShapeFromSource (arrayShape, blc,trc,inc);
  checkShape (shp, arr.shape(), false, rownr,
              "ArrayColumn::putSlice");
  //# Put the slice.
  baseColPtr_p->putSlice (rownr, arraySection, arr);
}

void ArrayColumnBase::acbPutSlice (rownr_t rownr,
                                   const Vector<Vector<Slice> >& arraySlices,
                                   const ArrayBase& arr)
{
  checkWritable();
  TABLECOLUMNCHECKROW(rownr);
  // Use shape of the row.
  IPosition colShp = shape(rownr);
  Vector<Vector<Slice> > slices(arraySlices);
  Slicer slicer;
  IPosition shp = Slice::checkSlices (slices, slicer, colShp);
  checkShape (shp, arr.shape(), false, rownr,
              "ArrayColumn::putSlice(slices)");
  // Now loop through all the slices and fill the array in parts.
  PutCellSlices functor(*this, rownr);
  handleSlices (slices, functor, slicer, arr);
}

void ArrayColumnBase::acbPutColumn (const ArrayBase& arr)
{
  checkWritable();
  //# First check if number of rows matches.
  rownr_t nrrow = nrow();
  IPosition shp  = arr.shape();
  uint32_t last = shp.nelements() - 1;
  if (shp[last] != int32_t(nrrow)) {
    throw TableArrayConformanceError
      ("ArrayColumn::putColumn - column " +
       baseColPtr_p->columnDesc().name() + " has " +
       String::toString(nrrow) + ", array has " +
       String::toString(shp[last]) + " rows");
  }
  //# Remove #rows from shape to get the shape of each cell.
  shp.resize (last);
  //# If the array is fixed shape, check if the shape matches.
  if ((columnDesc().options() & ColumnDesc::FixedShape)
      == ColumnDesc::FixedShape) {
    checkShape (shapeColumn(), shp, false, -1,
                "ArrayColumn::putColumn");
  } else {
    //# Otherwise set the shape of each cell (as far as needed).
    for (rownr_t i=0; i<nrrow; i++) {
      setShape (i, shp);
    }
  }
  //# Put the column.
  baseColPtr_p->putArrayColumn (arr);
}

void ArrayColumnBase::acbPutColumn (const Slicer& arraySection,
                                    const ArrayBase& arr)
{
  checkWritable();
  rownr_t nrrow = nrow();
  //# First check if number of rows matches.
  IPosition arrshp = arr.shape();
  uint32_t last = arrshp.nelements() - 1;
  if (arrshp(last) != int32_t(nrrow)) {
    throw TableArrayConformanceError
      ("ArrayColumn::putColumn(slicer) - column " +
       baseColPtr_p->columnDesc().name() + " has " +
       String::toString(nrrow) + ", but array has " +
       String::toString(arrshp[last]) + " rows");
  }
  //# If the array is fixed shape, check if the shape matches.
  if ((columnDesc().options() & ColumnDesc::FixedShape)
      == ColumnDesc::FixedShape) {
    //# Remove #rows from shape to get the shape of each cell.
    arrshp.resize (last);
    IPosition blc,trc,inc;
    IPosition shp = arraySection.inferShapeFromSource (shapeColumn(),
                                                       blc,trc,inc);
    checkShape (shp, arrshp, false, -1,
                "ArrayColumn::putColumn(slicer)");
  }
  //# Put the column slice.
  baseColPtr_p->putColumnSlice (arraySection, arr);
}

void ArrayColumnBase::acbPutColumn (const Vector<Vector<Slice> >& arraySlices,
                                    const ArrayBase& arr)
{
  checkWritable();
  rownr_t nrrow = nrow();
  // Get total shape.
  // Use shape of first row (if there) as overall array shape.
  IPosition colShp;
  if (nrrow > 0) {
    colShp = shape(0);
  }
  Vector<Vector<Slice> > slices(arraySlices);
  Slicer slicer;
  IPosition shp = Slice::checkSlices (slices, slicer, colShp);
  // Total shape is slice shape plus nr of table rows.
  shp.append (IPosition(1,nrrow));
  // Check array conformance.
  if (! shp.isEqual (arr.shape())) {
    throw (TableArrayConformanceError
           ("ArrayColumn::putColumn for column " +
            baseColPtr_p->columnDesc().name()));
  }
  // Now loop through all the slices and fill the array in parts.
  PutColumnSlices functor(*this);
  handleSlices (slices, functor, slicer, arr);
}

void ArrayColumnBase::acbPutColumnRange (const Slicer& rowRange,
                                         const ArrayBase& arr)
{
  rownr_t nrrow = nrow();
  IPosition shp, blc, trc, inc;
  shp = rowRange.inferShapeFromSource (IPosition(1,nrrow), blc, trc, inc);
  //# If the entire column is accessed, use that function.
  if (blc(0) == 0  &&  shp(0) == int32_t(nrrow)  &&  inc(0) == 1) {
    acbPutColumn (arr);
  } else {
    acbPutColumnCells (RefRows(blc(0), trc(0), inc(0)), arr);
  }
}

void ArrayColumnBase::acbPutColumnCells (const RefRows& rownrs,
                                         const ArrayBase& arr)
{
  checkWritable();
  //# First check if number of rows matches.
  rownr_t nrrow = rownrs.nrow();
  IPosition arrshp  = arr.shape();
  uint32_t last = arrshp.nelements() - 1;
  if (arrshp(last) != int32_t(nrrow)) {
    throw (TableArrayConformanceError
           ("ArrayColumn::putColumnCells for column " +
            baseColPtr_p->columnDesc().name()));
  }
  //# Remove #rows from shape to get the shape of each cell.
  arrshp.resize (last);
  //# If the array is fixed shape, check if the shape matches.
  if ((columnDesc().options() & ColumnDesc::FixedShape)
      == ColumnDesc::FixedShape) {
    if (! arrshp.isEqual (shapeColumn())) {
      throw (TableArrayConformanceError
             ("ArrayColumn::putColumnCells for column " +
              baseColPtr_p->columnDesc().name()));
    }
  } else {
    //# Otherwise set the shape of each cell (as far as needed).
    RefRowsSliceIter iter(rownrs);
    while (! iter.pastEnd()) {
      rownr_t rownr = iter.sliceStart();
      rownr_t end = iter.sliceEnd();
      rownr_t incr = iter.sliceIncr();
      while (rownr <= end) {
        setShape (rownr, arrshp);
        rownr += incr;
      }
      iter++;
    }
  }
  //# Put the entire array.
  baseColPtr_p->putArrayColumnCells (rownrs, arr);
}

void ArrayColumnBase::acbPutColumnRange (const Slicer& rowRange,
                                         const Slicer& arraySection,
                                         const ArrayBase& arr)
{
  rownr_t nrrow = nrow();
  IPosition shp, blc, trc, inc;
  shp = rowRange.inferShapeFromSource (IPosition(1,nrrow), blc, trc, inc);
  //# If the entire column is accessed, use that function.
  if (blc(0) == 0  &&  shp(0) == int32_t(nrrow)  &&  inc(0) == 1) {
    acbPutColumn (arraySection, arr);
  } else {
    acbPutColumnCells (RefRows(blc(0), trc(0), inc(0)), arraySection, arr);
  }
}

void ArrayColumnBase::acbPutColumnCells (const RefRows& rownrs,
                                         const Slicer& arraySection,
                                         const ArrayBase& arr)
{
  checkWritable();
  //# First check if number of rows matches.
  rownr_t nrrow = rownrs.nrow();
  IPosition arrshp = arr.shape();
  uint32_t last = arrshp.nelements() - 1;
  if (arrshp(last) != int32_t(nrrow)) {
    throw (TableArrayConformanceError
           ("ArrayColumn::putColumnCells for column " +
            baseColPtr_p->columnDesc().name()));
  }
  //# If the array is fixed shape, check if the shape matches.
  if ((columnDesc().options() & ColumnDesc::FixedShape)
      == ColumnDesc::FixedShape) {
    //# Remove #rows from shape to get the shape of each cell.
    arrshp.resize (last);
    IPosition arrshp2,arrblc,arrtrc,arrinc;
    arrshp2 = arraySection.inferShapeFromSource (shapeColumn(),
                                                 arrblc, arrtrc, arrinc);
    if (! arrshp.isEqual(arrshp2)) {
      throw (TableArrayConformanceError
             ("ArrayColumn::putColumnCells for column " +
              baseColPtr_p->columnDesc().name()));
    }
  }
  //# Put the entire array.
  baseColPtr_p->putColumnSliceCells (rownrs, arraySection, arr);
}

void ArrayColumnBase::acbPutColumnCells (const RefRows& rows,
                                         const Vector<Vector<Slice> >& arraySlices,
                                         const ArrayBase& source)
{
  checkWritable();
  // Check if the nr of rows in the array matches.
  if (int64_t(rows.nrows()) != source.shape()[source.ndim()-1]) {
    throw TableArrayConformanceError("ArrayColumn::putColumnCells - number of "
                                     "rows in RefRows and Array mismatches");
  }
  // Put the destination array one row at a time.
  RefRowsSliceIter rowIter(rows);
  std::unique_ptr<ArrayPositionIterator> arrIter =
    source.makeIterator (source.ndim()-1);
  while (! rowIter.pastEnd()) {
    for (rownr_t rownr = rowIter.sliceStart(); rownr <= rowIter.sliceEnd();
         rownr += rowIter.sliceIncr()) {
      acbPutSlice (rownr, arraySlices, arrIter->getArray());
      arrIter->next();
    }
    // Go to next slice of rows.
    rowIter++;
  }
}

void ArrayColumnBase::acbGetColumnCells (const RefRows& rows,
                                         const ColumnSlicer& columnSlicer,
                                         ArrayBase& destination,
                                         bool resize) const
{
  // Calculate the shape of the destination data.  This will be
  // [s1, s2, ..., nR] where sI are the sum of the slice elements for
  // that axis as contained in arraySlices [i].  nR is the number of rows
  // in the RefRows object rows.  Resize the destination array to match.
  const Vector<Slicer*>& dataSlicers = columnSlicer.getDataSlicers();
  const Vector<Slicer*>& destSlicers = columnSlicer.getDestinationSlicers();
  IPosition destShape (columnSlicer.shape());
  destShape.append (IPosition (1, rows.nrows()));
  adaptShape (destShape, destination, resize, -1,
              "ArrayColumn::getColumnCells (rows, columnSlicer, ...)");
  // Fill the destination array one row at a time.
  RefRowsSliceIter rowIter(rows);
  std::unique_ptr<ArrayPositionIterator> arrIter =
    destination.makeIterator (destination.ndim()-1);
  while (! rowIter.pastEnd()) {
    for (rownr_t rownr = rowIter.sliceStart(); rownr <= rowIter.sliceEnd();
         rownr += rowIter.sliceIncr()) {
      ArrayBase& destArray = arrIter->getArray();
      // Iterate through the slicers.
      for (uint32_t j=0; j<destSlicers.size(); ++j){
        std::unique_ptr<ArrayBase> destPart = destArray.getSection (*destSlicers[j]);
        baseGetSlice (rownr, *dataSlicers[j], *destPart);
      }
      arrIter->next();
    }
    // Go to next slice or rows.
    rowIter++;
  }
}

void ArrayColumnBase::acbPutColumnCells (const RefRows& rows,
                                         const ColumnSlicer& columnSlicer,
                                         const ArrayBase& source)
{
  // Calculate the shape of the source data.  This will be
  // [s1, s2, ..., nR] where sI are the sum of the slice elements for
  // that axis as contained in arraySlices [i].  nR is the number of rows
  // in the RefRows object rows.
  const Vector<Slicer*>& dataSlicers = columnSlicer.getDataSlicers();
  const Vector<Slicer*>& destSlicers = columnSlicer.getDestinationSlicers();
  IPosition destShape (columnSlicer.shape());
  destShape.append (IPosition (1, rows.nrows()));
  checkShape (destShape, source.shape(), false, -1,
              "ArrayColumn::putColumnCells (rows, columnSlicer, ...)");
  // Fill the source array one row at a time.
  RefRowsSliceIter rowIter(rows);
  std::unique_ptr<ArrayPositionIterator> arrIter =
    source.makeIterator (source.ndim()-1);
  while (! rowIter.pastEnd()) {
    for (rownr_t rownr = rowIter.sliceStart(); rownr <= rowIter.sliceEnd();
         rownr += rowIter.sliceIncr()) {
      ArrayBase& destArray = arrIter->getArray();
      // Iterate through the slicers.
      for (uint32_t j=0; j<destSlicers.size(); ++j){
        std::unique_ptr<ArrayBase> destPart = destArray.getSection (*destSlicers[j]);
        basePutSlice (rownr, *dataSlicers[j], *destPart);
      }
      arrIter->next();
    }
    // Go to next slice or rows.
    rowIter++;
  }
}

//# This is a very simple implementation.
//# However, it does not need to be more fancy, since an array operation
//# is already much more expensive than the virtual function calls
//# involved in each loop iteration.
void ArrayColumnBase::acbFillColumn (const ArrayBase& value)
{
  rownr_t nrrow = nrow();
  for (rownr_t i=0; i<nrrow; i++) {
    acbPut (i, value);
  }
}

void ArrayColumnBase::acbPutColumn (const ArrayColumnBase& that)
{
  checkWritable();
  //# Check the column lengths.
  rownr_t nrrow = nrow();
  if (nrrow != that.nrow()) {
    throw TableConformanceError
      ("Nr of rows differs in ArrayColumn::putColumn for column " +
       baseColPtr_p->columnDesc().name() + " (from column " +
       that.baseColPtr_p->columnDesc().name() + ')');
  }
  for (rownr_t i=0; i<nrrow; i++) {
    put (i, that, i);
  }
}


void ArrayColumnBase::handleSlices (const Vector<Vector<Slice> >& slices,
                                    BaseSlicesFunctor& functor,
                                    const Slicer& slicer,
                                    const ArrayBase& arr) const
{
  // Set start and end position in Array for first Slice.
  IPosition arrStart (arr.ndim(), 0);
  IPosition arrEnd (slicer.length() - 1);
  if (arrStart.size() > arrEnd.size()) {
    arrEnd.append (IPosition(1, nrow()-1));   // for get/putColumn funcs
  }
  IPosition colStart (slicer.start());
  IPosition colLen   (slicer.length());
  IPosition colIncr  (slicer.stride());
  uint32_t nrdim = slicer.ndim();
  IPosition pos(nrdim, 0);
  while (true) {
    CountedPtr<ArrayBase> refArr
      (arr.getSection (Slicer(arrStart, arrEnd, Slicer::endIsLast)));
    functor.apply (Slicer(colStart, colLen, colIncr), *refArr);
    uint32_t i;
    for (i=0; i<nrdim; ++i) {
      pos[i]++;
      if (uint32_t(pos[i]) < slices[i].size()) {
        const Slice& slice = slices[i][pos[i]];
        colStart[i] = slice.start();
        colLen[i]   = slice.length();
        colIncr[i]  = slice.inc();
        arrStart[i] = arrEnd[i] + 1;
        arrEnd[i]   = arrStart[i] + slice.length() - 1;
        break;
      }
      pos[i] = 0;
      arrStart[i] = 0;
      arrEnd[i]   = slicer.length()[i] - 1;
      colStart[i] = slicer.start()[i];
      colLen[i]   = slicer.length()[i];
      colIncr[i]  = slicer.stride()[i];
    }
    if (i == nrdim) {
      break;
    }
  }
}



ColumnSlicer::ColumnSlicer (const IPosition& shape,
                            const Vector<Slicer*>& dataSlicers,
                            const Vector<Slicer*>& destinationSlicers)
  : dataSlicers_p (dataSlicers),
    destinationSlicers_p (destinationSlicers),
    shape_p (shape)
{
  String message = validateParameters ();
  if (! message.empty()){
    freeSlicers(); // Call gave them to us; set them free.
    throw AipsError ("ColumnSlicer (ctor):: " + message);
  }
}

ColumnSlicer::~ColumnSlicer ()
{
  freeSlicers();
}

void ColumnSlicer::freeSlicers ()
{
  // The two Vectors contain pointers to objects so they need to be freed.
  // They should have the same length normally, but during validation it's
  // possible that they have different lengths.
  for (uint32_t i = 0; i < dataSlicers_p.size(); i++){
    delete dataSlicers_p [i];
  }
  for (uint32_t i = 0; i < destinationSlicers_p.size(); i++){
    delete destinationSlicers_p [i];
  }
}

String ColumnSlicer::validateParameters() const
{
  // Validate the contruction parameters to see if they are consistent.
  if (dataSlicers_p.size() != destinationSlicers_p.size()){
    return String::format ("Number of data slicers (%d) and destination slicers (%d) "
                           "must match", dataSlicers_p.size(), destinationSlicers_p.size());
  }

  if (dataSlicers_p.size() == 0){
    return String::format ("At least one destination and one data slicer required.");
  }

  for (uint32_t i = 0; i < dataSlicers_p.size(); i++) {
    if (dataSlicers_p[i]->length() != destinationSlicers_p[i]->length()){
      return String::format ("Length of data slicer[%d] (%s) and "
                             "destination slicer [%d] (%s) must be equal", 
                             i, dataSlicers_p[i]->length().toString().c_str(),
                             i, destinationSlicers_p[i]->length().toString().c_str());
    }
  }
  return String();
}



} //# NAMESPACE CASACORE - END
