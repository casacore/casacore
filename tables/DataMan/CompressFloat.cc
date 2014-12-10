//# CompressFloat.cc: Virtual column engine to scale a table float array
//# Copyright (C) 2001,2002
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

//# Includes
#include <casacore/tables/DataMan/CompressFloat.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/casa/Arrays/ArrayIter.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/BasicSL/String.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

CompressFloat::CompressFloat (const String& virtualColumnName,
			      const String& storedColumnName,
			      Float scale, Float offset)
: BaseMappedArrayEngine<Float,Short> (virtualColumnName, storedColumnName),
  scale_p         (scale),
  offset_p        (offset),
  fixed_p         (True),
  autoScale_p     (False),
  scaleColumn_p   (0),
  offsetColumn_p  (0)
{}

CompressFloat::CompressFloat (const String& virtualColumnName,
			      const String& storedColumnName,
			      const String& scaleColumnName,
			      const String& offsetColumnName,
			      Bool autoScale)
: BaseMappedArrayEngine<Float,Short> (virtualColumnName, storedColumnName),
  scaleName_p     (scaleColumnName),
  offsetName_p    (offsetColumnName),
  scale_p         (0.0),
  offset_p        (0.0),
  fixed_p         (False),
  autoScale_p     (autoScale),
  scaleColumn_p   (0),
  offsetColumn_p  (0)
{}

CompressFloat::CompressFloat (const Record& spec)
: BaseMappedArrayEngine<Float,Short> (),
  scale_p         (1.0),
  offset_p        (0.0),
  fixed_p         (True),
  autoScale_p     (False),
  scaleColumn_p   (0),
  offsetColumn_p  (0)
{
  if (spec.isDefined("SOURCENAME")  &&  spec.isDefined("TARGETNAME")) {
    setNames (spec.asString("SOURCENAME"), spec.asString("TARGETNAME"));
    if (spec.isDefined("SCALE")  &&  spec.isDefined("OFFSET")) {
      spec.get ("SCALE", scale_p);
      spec.get ("OFFSET", offset_p);
    } else {
      spec.get ("SCALENAME", scaleName_p);
      spec.get ("OFFSETNAME", offsetName_p);
      fixed_p = False;
    }
    if (spec.isDefined("AUTOSCALE")) {
      spec.get ("AUTOSCALE", autoScale_p);
    }
  }
}

CompressFloat::CompressFloat (const CompressFloat& that)
: BaseMappedArrayEngine<Float,Short> (that),
  scaleName_p     (that.scaleName_p),
  offsetName_p    (that.offsetName_p),
  scale_p         (that.scale_p),
  offset_p        (that.offset_p),
  fixed_p         (that.fixed_p),
  autoScale_p     (that.autoScale_p),
  scaleColumn_p   (0),
  offsetColumn_p  (0)
{}

CompressFloat::~CompressFloat()
{
  delete scaleColumn_p;
  delete offsetColumn_p;
}

//# Clone the engine object.
DataManager* CompressFloat::clone() const
{
  return new CompressFloat (*this);
}


//# Return the type name of the engine (i.e. its class name).
String CompressFloat::dataManagerType() const
{
  return className();
}
//# Return the class name.
//# Get the data type names using class ValType.
String CompressFloat::className()
{
  return "CompressFloat";
}

String CompressFloat::dataManagerName() const
{
  return virtualName();
}

Record CompressFloat::dataManagerSpec() const
{
  Record spec;
  spec.define ("SOURCENAME", virtualName());
  spec.define ("TARGETNAME", storedName());
  if (fixed_p) {
    spec.define ("SCALE", scale_p);
    spec.define ("OFFSET", offset_p);
  } else {
    spec.define ("SCALENAME", scaleName_p);
    spec.define ("OFFSETNAME", offsetName_p);
  }
  spec.define ("AUTOSCALE", autoScale_p);
  return spec;
}

DataManager* CompressFloat::makeObject (const String&, const Record& spec)
{
  return new CompressFloat(spec);
}
void CompressFloat::registerClass()
{
  DataManager::registerCtor (className(), makeObject);
}


void CompressFloat::create (uInt initialNrrow)
{
  BaseMappedArrayEngine<Float,Short>::create (initialNrrow);
  // Store the various parameters as keywords in this column.
  TableColumn thisCol (table(), virtualName());
  thisCol.rwKeywordSet().define ("_CompressFloat_Scale",      scale_p);
  thisCol.rwKeywordSet().define ("_CompressFloat_Offset",     offset_p);
  thisCol.rwKeywordSet().define ("_CompressFloat_ScaleName",  scaleName_p);
  thisCol.rwKeywordSet().define ("_CompressFloat_OffsetName", offsetName_p);
  thisCol.rwKeywordSet().define ("_CompressFloat_Fixed",      fixed_p);
  thisCol.rwKeywordSet().define ("_CompressFloat_AutoScale",  autoScale_p);
}

void CompressFloat::prepare()
{
  BaseMappedArrayEngine<Float,Short>::prepare1();
  TableColumn thisCol (table(), virtualName());
  thisCol.keywordSet().get ("_CompressFloat_Scale",      scale_p);
  thisCol.keywordSet().get ("_CompressFloat_Offset",     offset_p);
  thisCol.keywordSet().get ("_CompressFloat_ScaleName",  scaleName_p);
  thisCol.keywordSet().get ("_CompressFloat_OffsetName", offsetName_p);
  thisCol.keywordSet().get ("_CompressFloat_Fixed",      fixed_p);
  thisCol.keywordSet().get ("_CompressFloat_AutoScale",  autoScale_p);
  //# Allocate column objects to get scale and offset.
  if (! fixed_p) {
    scaleColumn_p = new ScalarColumn<Float> (table(), scaleName_p);
    offsetColumn_p = new ScalarColumn<Float> (table(), offsetName_p);
  }
  // Do this at the end, because it might call addRow.
  BaseMappedArrayEngine<Float,Short>::prepare2();
}

void CompressFloat::reopenRW()
{}

void CompressFloat::addRowInit (uInt startRow, uInt nrrow)
{
  BaseMappedArrayEngine<Float,Short>::addRowInit (startRow, nrrow);
  if (autoScale_p) {
    for (uInt i=0; i<nrrow; i++) {
      scaleColumn_p->put (startRow++, 0.);
    }
  }
}

// Find minimum and maximum.
void CompressFloat::findMinMax (Float& minVal, Float& maxVal,
				const Array<Float>& array) const
{
  setNaN (minVal);
  setNaN (maxVal);
  Bool deleteIt;
  const Float* data = array.getStorage (deleteIt);
  const uInt nr = array.nelements();
  Bool firstTime = True;
  for (uInt i=0; i<nr; i++) {
    if (isFinite (data[i])) {
      if (firstTime) {
	minVal = data[i];
	maxVal = data[i];
	firstTime = False;
      } else {
	if (data[i] < minVal) {
	  minVal = data[i];
	} else if (data[i] > maxVal) {
	  maxVal = data[i];
	}
      }
    }
  }
  array.freeStorage (data, deleteIt);
}

// Find minimum and maximum.
void CompressFloat::makeScaleOffset (Float& scale, Float& offset,
				     Float minVal, Float maxVal) const
{
  if (isNaN (minVal)) {
    scale = 0;
    offset = 0;
  } else {
    if (minVal == maxVal) {
      scale = 1;
    } else {
      scale = (maxVal - minVal) / 65534;
    }
    offset = (maxVal + minVal) / 2;
  }
}

// Scale/offset an array for get.
void CompressFloat::scaleOnGet (Float scale, Float offset,
				Array<Float>& array,
				const Array<Short>& target)
{
  Bool deleteIn, deleteOut;
  Float* out = array.getStorage (deleteOut);
  const Short* in = target.getStorage (deleteIn);
  const uInt nr = array.nelements();
  for (uInt i=0; i<nr; i++) {
    if (in[i] == -32768) {
      setNaN (out[i]);
    } else {
      out[i] = in[i] * scale + offset;
    }
  }
  target.freeStorage (in, deleteIn);
  array.putStorage (out, deleteOut);
}

// Scale/offset an array for put.
void CompressFloat::scaleOnPut (Float scale, Float offset,
				const Array<Float>& array,
				Array<Short>& target)
{
  Bool deleteIn, deleteOut;
  const Float* in = array.getStorage (deleteIn);
  Short* out = target.getStorage (deleteOut);
  const uInt nr = array.nelements();
  for (uInt i=0; i<nr; i++) {
    if (isFinite (in[i])) {
      Float tmp = (in[i] - offset) / scale;
      if (tmp < 0) {
	out[i] = short (ceil(tmp - 0.5));
      } else {
	out[i] = short (floor(tmp + 0.5));
      }
    } else {
      out[i] = -32768;
    }
  }
  array.freeStorage (in, deleteIn);
  target.putStorage (out, deleteOut);
}


void CompressFloat::scaleColumnOnGet (Array<Float>& array,
				      const Array<Short>& target)
{
  if (fixed_p) {
    scaleOnGet (scale_p, offset_p, array, target);
  }else{
    ArrayIterator<Float> arrayIter (array, array.ndim() - 1);
    ReadOnlyArrayIterator<Short> targetIter (target, target.ndim() - 1);
    uInt rownr = 0;
    while (! arrayIter.pastEnd()) {
      scaleOnGet (getScale(rownr), getOffset(rownr),
		  arrayIter.array(), targetIter.array());
      rownr++;
      arrayIter.next();
      targetIter.next();
    }
  }
}

void CompressFloat::scaleColumnOnPut (const Array<Float>& array,
				      Array<Short>& target)
{
  if (fixed_p) {
    scaleOnPut (scale_p, offset_p, array, target);
  }else{
    ReadOnlyArrayIterator<Float> arrayIter (array, array.ndim() - 1);
    ArrayIterator<Short> targetIter (target, target.ndim() - 1);
    uInt rownr = 0;
    while (! arrayIter.pastEnd()) {
      scaleOnPut (getScale(rownr), getOffset(rownr),
		  arrayIter.array(), targetIter.array());
      rownr++;
      arrayIter.next();
      targetIter.next();
    }
  }
}


void CompressFloat::getArray (uInt rownr, Array<Float>& array)
{
  if (! array.shape().isEqual (buffer_p.shape())) {
    buffer_p.resize (array.shape());
  }
  column().baseGet (rownr, buffer_p);
  scaleOnGet (getScale(rownr), getOffset(rownr), array, buffer_p);
}

void CompressFloat::putArray (uInt rownr, const Array<Float>& array)
{
  if (! array.shape().isEqual (buffer_p.shape())) {
    buffer_p.resize (array.shape());
  }
  if (! autoScale_p) {
    scaleOnPut (getScale(rownr), getOffset(rownr), array, buffer_p);
  } else {
    Float minVal, maxVal;
    findMinMax (minVal, maxVal, array);
    Float scale, offset;
    makeScaleOffset (scale, offset, minVal, maxVal);
    scaleColumn_p->put (rownr, scale);
    offsetColumn_p->put (rownr, offset);
    scaleOnPut (scale, offset, array, buffer_p);
  }
  column().basePut (rownr, buffer_p);
}

void CompressFloat::getSlice (uInt rownr, const Slicer& slicer,
			      Array<Float>& array)
{
  if (! array.shape().isEqual (buffer_p.shape())) {
    buffer_p.resize (array.shape());
  }
  column().getSlice (rownr, slicer, buffer_p);
  scaleOnGet (getScale(rownr), getOffset(rownr), array, buffer_p);
}

void CompressFloat::putPart (uInt rownr, const Slicer& slicer,
			     const Array<Float>& array,
			     Float scale, Float offset)
{
  if (! array.shape().isEqual (buffer_p.shape())) {
    buffer_p.resize (array.shape());
  }
  scaleOnPut (scale, offset, array, buffer_p);
  column().putSlice (rownr, slicer, buffer_p);
}

void CompressFloat::putFullPart (uInt rownr, const Slicer& slicer,
				 Array<Float>& fullArray,
				 const Array<Float>& partArray,
				 Float minVal, Float maxVal)
{
  Array<Float> subarr = fullArray(slicer.start(), slicer.end(),
				  slicer.stride());
  subarr = partArray;
  Float scale, offset;
  makeScaleOffset (scale, offset, minVal, maxVal);
  scaleColumn_p->put (rownr, scale);
  offsetColumn_p->put (rownr, offset);
  if (! fullArray.shape().isEqual (buffer_p.shape())) {
    buffer_p.resize (fullArray.shape());
  }
  scaleOnPut (scale, offset, fullArray, buffer_p);
  column().basePut (rownr, buffer_p);
}

void CompressFloat::putSlice (uInt rownr, const Slicer& slicer,
			      const Array<Float>& array)
{
  // If the slice is the entire array, write it as such.
  IPosition shp = shape(rownr);
  if (shp.isEqual (array.shape())) {
    CompressFloat::putArray (rownr, array);
  } else {
    // Get current scale and offset.
    // If no autoscaling, write the part immediately.
    Float scale = getScale(rownr);
    Float offset = getOffset(rownr);
    if (! autoScale_p) {
      putPart (rownr, slicer, array, scale, offset);
    } else {
      // Determine min/max of new slice.
      // scale==0 means that no array data was written yet.
      // In that case initialize array to NaN if the slice has valid data.
      Float minValArr, maxValArr;
      findMinMax (minValArr, maxValArr, array);
      if (scale == 0) {
	if (! isNaN(minValArr)) {
	  Array<Float> arr(shp);
	  Float val;
	  setNaN (val);
	  arr = val;
	  putFullPart (rownr, slicer, arr, array, minValArr, maxValArr);
	}
      } else {
	// Valid data in row.
	// Writing the part will do if no valid data in it or if
	// its min/max is within the current min/max.
	// Otherwise we have to rescale using new min/max.
	Float maxValRow = offset + scale*65534/2;
	Float minValRow = offset - scale*65534/2;
	if (isNaN(minValArr)
	    ||  (minValArr >= minValRow  &&  maxValArr <= maxValRow)) {
	  putPart (rownr, slicer, array, scale, offset);
	} else {
	  Array<Float> arr(shp);
	  CompressFloat::getArray (rownr, arr);
	  putFullPart (rownr, slicer, arr, array,
		       min(minValRow, minValArr),
		       max(maxValRow, maxValArr));
	}
      }
    }
  }
}

void CompressFloat::getArrayColumn (Array<Float>& array)
{
  Array<Short> target(array.shape());
  column().getColumn (target);
  scaleColumnOnGet (array, target);
}
void CompressFloat::putArrayColumn (const Array<Float>& array)
{
  Array<Short> target(array.shape());
  if (! autoScale_p) {
    scaleColumnOnPut (array, target);
    column().putColumn (target);
  } else {
    ReadOnlyArrayIterator<Float> iter(array, array.ndim()-1);
    uInt nrrow = table().nrow();
    for (uInt rownr=0; rownr<nrrow; rownr++) {
      CompressFloat::putArray (rownr, iter.array());
      iter.next();
    }
  }
}

void CompressFloat::getArrayColumnCells (const RefRows& rownrs,
                                         Array<Float>& array)
{
  ArrayIterator<Float> arrIter(array, array.ndim()-1);
  RefRowsSliceIter rowsIter(rownrs);
  while (! rowsIter.pastEnd()) {
    uInt rownr = rowsIter.sliceStart();
    uInt end   = rowsIter.sliceEnd();
    uInt incr  = rowsIter.sliceIncr();
    while (rownr <= end) {
      CompressFloat::getArray (rownr, arrIter.array());
      arrIter.next();
      rownr += incr;
    }
    rowsIter++;
  }
}
void CompressFloat::putArrayColumnCells (const RefRows& rownrs,
                                         const Array<Float>& array)
{
  ReadOnlyArrayIterator<Float> arrIter(array, array.ndim()-1);
  RefRowsSliceIter rowsIter(rownrs);
  while (! rowsIter.pastEnd()) {
    uInt rownr = rowsIter.sliceStart();
    uInt end   = rowsIter.sliceEnd();
    uInt incr  = rowsIter.sliceIncr();
    while (rownr <= end) {
      CompressFloat::putArray (rownr, arrIter.array());
      arrIter.next();
      rownr += incr;
    }
    rowsIter++;
  }
}

void CompressFloat::getColumnSlice (const Slicer& slicer,
				    Array<Float>& array)
{
  Array<Short> target(array.shape());
  column().getColumn (slicer, target);
  scaleColumnOnGet (array, target);
}

void CompressFloat::putColumnSlice (const Slicer& slicer,
				    const Array<Float>& array)
{
  Array<Short> target(array.shape());
  if (! autoScale_p) {
    scaleColumnOnPut (array, target);
    column().putColumn (slicer, target);
  } else {
    ReadOnlyArrayIterator<Float> iter(array, array.ndim()-1);
    uInt nrrow = table().nrow();
    for (uInt rownr=0; rownr<nrrow; rownr++) {
      CompressFloat::putSlice (rownr, slicer, iter.array());
      iter.next();
    }
  }
}

void CompressFloat::getColumnSliceCells (const RefRows& rownrs,
                                         const Slicer& slicer,
                                         Array<Float>& array)
{
  ArrayIterator<Float> arrIter(array, array.ndim()-1);
  RefRowsSliceIter rowsIter(rownrs);
  while (! rowsIter.pastEnd()) {
    uInt rownr = rowsIter.sliceStart();
    uInt end   = rowsIter.sliceEnd();
    uInt incr  = rowsIter.sliceIncr();
    while (rownr <= end) {
      CompressFloat::getSlice (rownr, slicer, arrIter.array());
      arrIter.next();
      rownr += incr;
    }
    rowsIter++;
  }
}
void CompressFloat::putColumnSliceCells (const RefRows& rownrs,
                                         const Slicer& slicer,
                                         const Array<Float>& array)
{
  ReadOnlyArrayIterator<Float> arrIter(array, array.ndim()-1);
  RefRowsSliceIter rowsIter(rownrs);
  while (! rowsIter.pastEnd()) {
    uInt rownr = rowsIter.sliceStart();
    uInt end   = rowsIter.sliceEnd();
    uInt incr  = rowsIter.sliceIncr();
    while (rownr <= end) {
      CompressFloat::putSlice (rownr, slicer, arrIter.array());
      arrIter.next();
      rownr += incr;
    }
    rowsIter++;
  }
}

} //# NAMESPACE CASACORE - END

