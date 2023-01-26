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
			      float scale, float offset)
: BaseMappedArrayEngine<float,int16_t> (virtualColumnName, storedColumnName),
  scale_p         (scale),
  offset_p        (offset),
  fixed_p         (true),
  autoScale_p     (false),
  scaleColumn_p   (0),
  offsetColumn_p  (0)
{}

CompressFloat::CompressFloat (const String& virtualColumnName,
			      const String& storedColumnName,
			      const String& scaleColumnName,
			      const String& offsetColumnName,
			      bool autoScale)
: BaseMappedArrayEngine<float,int16_t> (virtualColumnName, storedColumnName),
  scaleName_p     (scaleColumnName),
  offsetName_p    (offsetColumnName),
  scale_p         (0.0),
  offset_p        (0.0),
  fixed_p         (false),
  autoScale_p     (autoScale),
  scaleColumn_p   (0),
  offsetColumn_p  (0)
{}

CompressFloat::CompressFloat (const Record& spec)
: BaseMappedArrayEngine<float,int16_t> (),
  scale_p         (1.0),
  offset_p        (0.0),
  fixed_p         (true),
  autoScale_p     (false),
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
      fixed_p = false;
    }
    if (spec.isDefined("AUTOSCALE")) {
      spec.get ("AUTOSCALE", autoScale_p);
    }
  }
}

CompressFloat::CompressFloat (const CompressFloat& that)
: BaseMappedArrayEngine<float,int16_t> (that),
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


void CompressFloat::create64 (rownr_t initialNrrow)
{
  BaseMappedArrayEngine<float,int16_t>::create64 (initialNrrow);
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
  BaseMappedArrayEngine<float,int16_t>::prepare1();
  TableColumn thisCol (table(), virtualName());
  thisCol.keywordSet().get ("_CompressFloat_Scale",      scale_p);
  thisCol.keywordSet().get ("_CompressFloat_Offset",     offset_p);
  thisCol.keywordSet().get ("_CompressFloat_ScaleName",  scaleName_p);
  thisCol.keywordSet().get ("_CompressFloat_OffsetName", offsetName_p);
  thisCol.keywordSet().get ("_CompressFloat_Fixed",      fixed_p);
  thisCol.keywordSet().get ("_CompressFloat_AutoScale",  autoScale_p);
  //# Allocate column objects to get scale and offset.
  if (! fixed_p) {
    scaleColumn_p = new ScalarColumn<float> (table(), scaleName_p);
    offsetColumn_p = new ScalarColumn<float> (table(), offsetName_p);
  }
  // Do this at the end, because it might call addRow.
  BaseMappedArrayEngine<float,int16_t>::prepare2();
}

void CompressFloat::reopenRW()
{}

void CompressFloat::addRowInit (rownr_t startRow, rownr_t nrrow)
{
  BaseMappedArrayEngine<float,int16_t>::addRowInit (startRow, nrrow);
  if (autoScale_p) {
    for (rownr_t i=0; i<nrrow; i++) {
      scaleColumn_p->put (startRow++, 0.);
    }
  }
}

// Find minimum and maximum.
void CompressFloat::findMinMax (float& minVal, float& maxVal,
				const Array<float>& array) const
{
  setNaN (minVal);
  setNaN (maxVal);
  bool deleteIt;
  const float* data = array.getStorage (deleteIt);
  const int64_t nr = array.nelements();
  bool firstTime = true;
  for (int64_t i=0; i<nr; i++) {
    if (isFinite (data[i])) {
      if (firstTime) {
	minVal = data[i];
	maxVal = data[i];
	firstTime = false;
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
void CompressFloat::makeScaleOffset (float& scale, float& offset,
				     float minVal, float maxVal) const
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
void CompressFloat::scaleOnGet (float scale, float offset,
				Array<float>& array,
				const Array<int16_t>& target)
{
  bool deleteIn, deleteOut;
  float* out = array.getStorage (deleteOut);
  const int16_t* in = target.getStorage (deleteIn);
  const int64_t nr = array.nelements();
  for (int64_t i=0; i<nr; i++) {
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
void CompressFloat::scaleOnPut (float scale, float offset,
				const Array<float>& array,
				Array<int16_t>& target)
{
  bool deleteIn, deleteOut;
  const float* in = array.getStorage (deleteIn);
  int16_t* out = target.getStorage (deleteOut);
  const int64_t nr = array.nelements();
  for (int64_t i=0; i<nr; i++) {
    if (isFinite (in[i])) {
      float tmp = (in[i] - offset) / scale;
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


void CompressFloat::scaleColumnOnGet (Array<float>& array,
				      const Array<int16_t>& target)
{
  if (fixed_p) {
    scaleOnGet (scale_p, offset_p, array, target);
  }else{
    ArrayIterator<float> arrayIter (array, array.ndim() - 1);
    ReadOnlyArrayIterator<int16_t> targetIter (target, target.ndim() - 1);
    rownr_t rownr = 0;
    while (! arrayIter.pastEnd()) {
      scaleOnGet (getScale(rownr), getOffset(rownr),
		  arrayIter.array(), targetIter.array());
      rownr++;
      arrayIter.next();
      targetIter.next();
    }
  }
}

void CompressFloat::scaleColumnOnPut (const Array<float>& array,
				      Array<int16_t>& target)
{
  if (fixed_p) {
    scaleOnPut (scale_p, offset_p, array, target);
  }else{
    ReadOnlyArrayIterator<float> arrayIter (array, array.ndim() - 1);
    ArrayIterator<int16_t> targetIter (target, target.ndim() - 1);
    rownr_t rownr = 0;
    while (! arrayIter.pastEnd()) {
      scaleOnPut (getScale(rownr), getOffset(rownr),
		  arrayIter.array(), targetIter.array());
      rownr++;
      arrayIter.next();
      targetIter.next();
    }
  }
}


void CompressFloat::getArray (rownr_t rownr, Array<float>& array)
{
  if (! array.shape().isEqual (buffer_p.shape())) {
    buffer_p.resize (array.shape());
  }
  column().baseGet (rownr, buffer_p);
  scaleOnGet (getScale(rownr), getOffset(rownr), array, buffer_p);
}

void CompressFloat::putArray (rownr_t rownr, const Array<float>& array)
{
  if (! array.shape().isEqual (buffer_p.shape())) {
    buffer_p.resize (array.shape());
  }
  if (! autoScale_p) {
    scaleOnPut (getScale(rownr), getOffset(rownr), array, buffer_p);
  } else {
    float minVal, maxVal;
    findMinMax (minVal, maxVal, array);
    float scale, offset;
    makeScaleOffset (scale, offset, minVal, maxVal);
    scaleColumn_p->put (rownr, scale);
    offsetColumn_p->put (rownr, offset);
    scaleOnPut (scale, offset, array, buffer_p);
  }
  column().basePut (rownr, buffer_p);
}

void CompressFloat::getSlice (rownr_t rownr, const Slicer& slicer,
			      Array<float>& array)
{
  if (! array.shape().isEqual (buffer_p.shape())) {
    buffer_p.resize (array.shape());
  }
  column().getSlice (rownr, slicer, buffer_p);
  scaleOnGet (getScale(rownr), getOffset(rownr), array, buffer_p);
}

void CompressFloat::putPart (rownr_t rownr, const Slicer& slicer,
			     const Array<float>& array,
			     float scale, float offset)
{
  if (! array.shape().isEqual (buffer_p.shape())) {
    buffer_p.resize (array.shape());
  }
  scaleOnPut (scale, offset, array, buffer_p);
  column().putSlice (rownr, slicer, buffer_p);
}

void CompressFloat::putFullPart (rownr_t rownr, const Slicer& slicer,
				 Array<float>& fullArray,
				 const Array<float>& partArray,
				 float minVal, float maxVal)
{
  Array<float> subarr = fullArray(slicer.start(), slicer.end(),
				  slicer.stride());
  subarr = partArray;
  float scale, offset;
  makeScaleOffset (scale, offset, minVal, maxVal);
  scaleColumn_p->put (rownr, scale);
  offsetColumn_p->put (rownr, offset);
  if (! fullArray.shape().isEqual (buffer_p.shape())) {
    buffer_p.resize (fullArray.shape());
  }
  scaleOnPut (scale, offset, fullArray, buffer_p);
  column().basePut (rownr, buffer_p);
}

void CompressFloat::putSlice (rownr_t rownr, const Slicer& slicer,
			      const Array<float>& array)
{
  // If the slice is the entire array, write it as such.
  IPosition shp = shape(rownr);
  if (shp.isEqual (array.shape())) {
    CompressFloat::putArray (rownr, array);
  } else {
    // Get current scale and offset.
    // If no autoscaling, write the part immediately.
    float scale = getScale(rownr);
    float offset = getOffset(rownr);
    if (! autoScale_p) {
      putPart (rownr, slicer, array, scale, offset);
    } else {
      // Determine min/max of new slice.
      // scale==0 means that no array data was written yet.
      // In that case initialize array to NaN if the slice has valid data.
      float minValArr, maxValArr;
      findMinMax (minValArr, maxValArr, array);
      if (scale == 0) {
	if (! isNaN(minValArr)) {
	  Array<float> arr(shp);
	  float val;
	  setNaN (val);
	  arr = val;
	  putFullPart (rownr, slicer, arr, array, minValArr, maxValArr);
	}
      } else {
	// Valid data in row.
	// Writing the part will do if no valid data in it or if
	// its min/max is within the current min/max.
	// Otherwise we have to rescale using new min/max.
	float maxValRow = offset + scale*65534/2;
	float minValRow = offset - scale*65534/2;
	if (isNaN(minValArr)
	    ||  (minValArr >= minValRow  &&  maxValArr <= maxValRow)) {
	  putPart (rownr, slicer, array, scale, offset);
	} else {
	  Array<float> arr(shp);
	  CompressFloat::getArray (rownr, arr);
	  putFullPart (rownr, slicer, arr, array,
		       min(minValRow, minValArr),
		       max(maxValRow, maxValArr));
	}
      }
    }
  }
}

void CompressFloat::getArrayColumn (Array<float>& array)
{
  Array<int16_t> target(array.shape());
  column().getColumn (target);
  scaleColumnOnGet (array, target);
}
void CompressFloat::putArrayColumn (const Array<float>& array)
{
  Array<int16_t> target(array.shape());
  if (! autoScale_p) {
    scaleColumnOnPut (array, target);
    column().putColumn (target);
  } else {
    ReadOnlyArrayIterator<float> iter(array, array.ndim()-1);
    rownr_t nrrow = table().nrow();
    for (rownr_t rownr=0; rownr<nrrow; rownr++) {
      CompressFloat::putArray (rownr, iter.array());
      iter.next();
    }
  }
}

void CompressFloat::getArrayColumnCells (const RefRows& rownrs,
                                         Array<float>& array)
{
  ArrayIterator<float> arrIter(array, array.ndim()-1);
  RefRowsSliceIter rowsIter(rownrs);
  while (! rowsIter.pastEnd()) {
    rownr_t rownr = rowsIter.sliceStart();
    rownr_t end   = rowsIter.sliceEnd();
    rownr_t incr  = rowsIter.sliceIncr();
    while (rownr <= end) {
      CompressFloat::getArray (rownr, arrIter.array());
      arrIter.next();
      rownr += incr;
    }
    rowsIter++;
  }
}
void CompressFloat::putArrayColumnCells (const RefRows& rownrs,
                                         const Array<float>& array)
{
  ReadOnlyArrayIterator<float> arrIter(array, array.ndim()-1);
  RefRowsSliceIter rowsIter(rownrs);
  while (! rowsIter.pastEnd()) {
    rownr_t rownr = rowsIter.sliceStart();
    rownr_t end   = rowsIter.sliceEnd();
    rownr_t incr  = rowsIter.sliceIncr();
    while (rownr <= end) {
      CompressFloat::putArray (rownr, arrIter.array());
      arrIter.next();
      rownr += incr;
    }
    rowsIter++;
  }
}

void CompressFloat::getColumnSlice (const Slicer& slicer,
				    Array<float>& array)
{
  Array<int16_t> target(array.shape());
  column().getColumn (slicer, target);
  scaleColumnOnGet (array, target);
}

void CompressFloat::putColumnSlice (const Slicer& slicer,
				    const Array<float>& array)
{
  Array<int16_t> target(array.shape());
  if (! autoScale_p) {
    scaleColumnOnPut (array, target);
    column().putColumn (slicer, target);
  } else {
    ReadOnlyArrayIterator<float> iter(array, array.ndim()-1);
    rownr_t nrrow = table().nrow();
    for (rownr_t rownr=0; rownr<nrrow; rownr++) {
      CompressFloat::putSlice (rownr, slicer, iter.array());
      iter.next();
    }
  }
}

void CompressFloat::getColumnSliceCells (const RefRows& rownrs,
                                         const Slicer& slicer,
                                         Array<float>& array)
{
  ArrayIterator<float> arrIter(array, array.ndim()-1);
  RefRowsSliceIter rowsIter(rownrs);
  while (! rowsIter.pastEnd()) {
    rownr_t rownr = rowsIter.sliceStart();
    rownr_t end   = rowsIter.sliceEnd();
    rownr_t incr  = rowsIter.sliceIncr();
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
                                         const Array<float>& array)
{
  ReadOnlyArrayIterator<float> arrIter(array, array.ndim()-1);
  RefRowsSliceIter rowsIter(rownrs);
  while (! rowsIter.pastEnd()) {
    rownr_t rownr = rowsIter.sliceStart();
    rownr_t end   = rowsIter.sliceEnd();
    rownr_t incr  = rowsIter.sliceIncr();
    while (rownr <= end) {
      CompressFloat::putSlice (rownr, slicer, arrIter.array());
      arrIter.next();
      rownr += incr;
    }
    rowsIter++;
  }
}

} //# NAMESPACE CASACORE - END

