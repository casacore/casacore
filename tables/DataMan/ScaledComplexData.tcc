//# ScaledComplexData.cc: Templated virtual column engine to scale a complex table array
//# Copyright (C) 1999,2000,2001
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

#ifndef TABLES_SCALEDCOMPLEXDATA_TCC
#define TABLES_SCALEDCOMPLEXDATA_TCC

//# Includes
#include <casacore/tables/DataMan/ScaledComplexData.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/RefRows.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayIter.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/Utilities/ValTypeId.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class S, class T>
ScaledComplexData<S,T>::ScaledComplexData (const String& virtualColumnName,
					   const String& storedColumnName,
					   S scale, S offset)
: BaseMappedArrayEngine<S,T> (virtualColumnName, storedColumnName),
  scale_p       (scale),
  offset_p      (offset),
  fixedScale_p  (True),
  fixedOffset_p (True),
  scaleColumn_p (0),
  offsetColumn_p(0)
{}

template<class S, class T>
ScaledComplexData<S,T>::ScaledComplexData (const String& virtualColumnName,
					   const String& storedColumnName,
					   const String& scaleColumnName,
					   S offset)
: BaseMappedArrayEngine<S,T> (virtualColumnName, storedColumnName),
  scaleName_p   (scaleColumnName),
  scale_p       (S(0.0, 0.0)),
  offset_p      (offset),
  fixedScale_p  (False),
  fixedOffset_p (True),
  scaleColumn_p (0),
  offsetColumn_p(0)
{}

template<class S, class T>
ScaledComplexData<S,T>::ScaledComplexData (const String& virtualColumnName,
					   const String& storedColumnName,
					   const String& scaleColumnName,
					   const String& offsetColumnName)
: BaseMappedArrayEngine<S,T> (virtualColumnName, storedColumnName),
  scaleName_p   (scaleColumnName),
  offsetName_p  (offsetColumnName),
  scale_p       (S(0.0, 0.0)),
  offset_p      (S(0.0, 0.0)),
  fixedScale_p  (False),
  fixedOffset_p (False),
  scaleColumn_p (0),
  offsetColumn_p(0)
{}

template<class S, class T>
ScaledComplexData<S,T>::ScaledComplexData (const ScaledComplexData<S,T>& that)
: BaseMappedArrayEngine<S,T> (that),
  scaleName_p   (that.scaleName_p),
  offsetName_p  (that.offsetName_p),
  scale_p       (that.scale_p),
  offset_p      (that.offset_p),
  fixedScale_p  (that.fixedScale_p),
  fixedOffset_p (that.fixedOffset_p),
  scaleColumn_p (0),
  offsetColumn_p(0)
{}

template<class S, class T>
ScaledComplexData<S,T>::ScaledComplexData (const Record& spec)
: BaseMappedArrayEngine<S,T> (),
  scale_p       (S(1.0, 1.0)),
  offset_p      (S(0.0, 0.0)),
  fixedScale_p  (True),
  fixedOffset_p (True),
  scaleColumn_p (0),
  offsetColumn_p(0)
{
    if (spec.isDefined("SOURCENAME")  &&  spec.isDefined("TARGETNAME")) {
        setNames (spec.asString("SOURCENAME"), spec.asString("TARGETNAME"));
	if (spec.isDefined("SCALE")) {
	    spec.get ("SCALE", scale_p);
	} else {
	  spec.get ("SCALENAME", scaleName_p);
	  fixedScale_p = False;
	}
	if (spec.isDefined("OFFSET")) {
	    spec.get ("OFFSET", offset_p);
	} else {
	  spec.get ("OFFSETNAME", offsetName_p);
	  fixedOffset_p = False;
	}
    }
}

template<class S, class T>
ScaledComplexData<S,T>::~ScaledComplexData()
{
    delete scaleColumn_p;
    delete offsetColumn_p;
}

//# Clone the engine object.
template<class S, class T>
DataManager* ScaledComplexData<S,T>::clone() const
{
    DataManager* dmPtr = new ScaledComplexData<S,T> (*this);
    return dmPtr;
}


//# Return the type name of the engine (i.e. its class name).
template<class S, class T>
String ScaledComplexData<S,T>::dataManagerType() const
{
    return className();
}
//# Return the class name.
//# Get the data type names using class ValType.
template<class S, class T>
String ScaledComplexData<S,T>::className()
{
    return "ScaledComplexData<" + valDataTypeId ((S*)0) + ","
	                        + valDataTypeId ((T*)0) + ">";
}

template<class S, class T>
Record ScaledComplexData<S,T>::dataManagerSpec() const
{
    Record spec;
    spec.define ("SOURCENAME", virtualName());
    spec.define ("TARGETNAME", storedName());
    if (fixedScale_p) {
        spec.define ("SCALE", scale_p);
    } else {
        spec.define ("SCALENAME", scaleName_p);
    }
    if (fixedOffset_p) {
        spec.define ("OFFSET", offset_p);
    } else {
        spec.define ("OFFSETNAME", offsetName_p);
    }
    return spec;
}

template<class S, class T>
DataManager* ScaledComplexData<S,T>::makeObject (const String&,
						 const Record& spec)
{
    return new ScaledComplexData<S,T> (spec);
}
template<class S, class T>
void ScaledComplexData<S,T>::registerClass()
{
    DataManager::registerCtor (className(), makeObject);
}


template<class S, class T>
void ScaledComplexData<S,T>::create (uInt initialNrrow)
{
    BaseMappedArrayEngine<S,T>::create (initialNrrow);
    // Store the various parameters as keywords in this column.
    TableColumn thisCol (table(), virtualName());
    thisCol.rwKeywordSet().define ("_ScaledComplexData_Scale",
				   scale_p);
    thisCol.rwKeywordSet().define ("_ScaledComplexData_Offset",
				   offset_p);
    thisCol.rwKeywordSet().define ("_ScaledComplexData_ScaleName",
				   scaleName_p);
    thisCol.rwKeywordSet().define ("_ScaledComplexData_OffsetName",
				   offsetName_p);
    thisCol.rwKeywordSet().define ("_ScaledComplexData_FixedScale",
				   fixedScale_p);
    thisCol.rwKeywordSet().define ("_ScaledComplexData_FixedOffset",
				   fixedOffset_p);
}

template<class S, class T>
void ScaledComplexData<S,T>::prepare()
{
    BaseMappedArrayEngine<S,T>::prepare();
    TableColumn thisCol (table(), virtualName());
    thisCol.keywordSet().get ("_ScaledComplexData_Scale",       scale_p);
    thisCol.keywordSet().get ("_ScaledComplexData_Offset",      offset_p);
    thisCol.keywordSet().get ("_ScaledComplexData_ScaleName",   scaleName_p);
    thisCol.keywordSet().get ("_ScaledComplexData_OffsetName",  offsetName_p);
    thisCol.keywordSet().get ("_ScaledComplexData_FixedScale",  fixedScale_p);
    thisCol.keywordSet().get ("_ScaledComplexData_FixedOffset", fixedOffset_p);
    //# Allocate column objects to get scale and offset.
    if (! fixedScale_p) {
	scaleColumn_p = new ScalarColumn<S> (table(), scaleName_p);
    }
    if (! fixedOffset_p) {
	offsetColumn_p = new ScalarColumn<S> (table(), offsetName_p);
    }
}


template<class S, class T>
Bool ScaledComplexData<S,T>::canAccessArrayColumnCells (Bool& reask) const
{
    reask = False;
    return True;
}

//# This function is called in case the virtual column has FixedShape arrays.
template<class S, class T>
void ScaledComplexData<S,T>::setShapeColumn (const IPosition& shape)
{
    BaseMappedArrayEngine<S,T>::setShapeColumn (storedShape(shape));
}

template<class S, class T>
void ScaledComplexData<S,T>::setShape (uInt rownr, const IPosition& shape)
{
    BaseMappedArrayEngine<S,T>::setShape (rownr, storedShape(shape));
}

template<class S, class T>
uInt ScaledComplexData<S,T>::ndim (uInt rownr)
{
    return column().ndim (rownr) - 1;
}

template<class S, class T>
IPosition ScaledComplexData<S,T>::shape (uInt rownr)
{
    // The virtual shape is the stored shape minus the first dimensions.
    IPosition storedShape = column().shape (rownr);
    return storedShape.getLast (storedShape.nelements() - 1);
}


template<class S, class T>
S ScaledComplexData<S,T>::getScale (uInt rownr)
{
    if (fixedScale_p) {
	return scale_p;
    }
    return (*scaleColumn_p)(rownr);
}
template<class S, class T>
S ScaledComplexData<S,T>::getOffset (uInt rownr)
{
    if (fixedOffset_p) {
	return offset_p;
    }
    return (*offsetColumn_p)(rownr);
}

// Scale/offset an array for get.
template<class S, class T>
void ScaledComplexData<S,T>::scaleOnGet (S scale, S offset,
					 Array<S>& array,
					 const Array<T>& target)
{
    Bool deleteIn, deleteOut;
    S* out = array.getStorage (deleteOut);
    S* op  = out;
    const T* in = target.getStorage (deleteIn);
    const T* ip = in;
    const T* last = ip + target.nelements();
    if (offset == S(0.0, 0.0)) {
	if (scale == S(1.0, 1.0)) {
	    while (ip < last) {
	      ///		op->real() = *ip++;
	      ///		op->imag() = *ip++;
	      *op = S(*ip, ip[1]); ip++; ip++;
	      op++;
	    }
	}else{
	    while (ip < last) {
	      ///		op->real() = *ip++ * scale.real();
	      ///		op->imag() = *ip++ * scale.imag();
	      *op = S(*ip * scale.real(), ip[1]  * scale.imag());
	      ip++; ip++; op++;
	    }
	}
    }else{
	if (scale == S(1.0, 1.0)) {
	    while (ip < last) {
	      ///		op->real() = *ip++ + offset.real();
	      ///		op->imag() = *ip++ + offset.imag();
	      *op = S(*ip + offset.real(), ip[1] + offset.imag());
	      ip++; ip++; op++;
	    }
	}else{
	    while (ip < last) {
///		op->real() = *ip++ * scale.real() + offset.real();
///		op->imag() = *ip++ * scale.imag() + offset.imag();
	      *op = S(*ip * scale.real() + offset.real(),
		      ip[1] * scale.imag() + offset.imag());
	      ip++; ip++; op++;
	    }
	}
    }
    target.freeStorage (in, deleteIn);
    array.putStorage (out, deleteOut);
}

// Scale/offset an array for put.
template<class S, class T>
void ScaledComplexData<S,T>::scaleOnPut (S scale, S offset,
					 const Array<S>& array,
					 Array<T>& target)
{
    Bool deleteIn, deleteOut;
    const S* in = array.getStorage (deleteIn);
    const S* ip = in;
    T* out = target.getStorage (deleteOut);
    T* op  = out;
    const T* last = op + target.nelements();
    if (offset == S(0.0, 0.0)) {
	if (scale == S(1.0, 1.0)) {
	    while (op < last) {
		*op++ = T(ip->real());
		*op++ = T(ip->imag());
		ip++;
	    }
	}else{
	    while (op < last) {
		*op++ = T(ip->real() / scale.real());
		*op++ = T(ip->imag() / scale.imag());
		ip++;
	    }
	}
    }else{
	if (scale == S(1.0, 1.0)) {
	    while (op < last) {
		*op++ = T(ip->real() - offset.real());
		*op++ = T(ip->imag() - offset.imag());
		ip++;
	    }
	}else{
	    while (op < last) {
		*op++ = T((ip->real() - offset.real()) / scale.real());
		*op++ = T((ip->imag() - offset.imag()) / scale.imag());
		ip++;
	    }
	}
    }
    array.freeStorage (in, deleteIn);
    target.putStorage (out, deleteOut);
}


template<class S, class T>
void ScaledComplexData<S,T>::scaleColumnOnGet (Array<S>& array,
					       const Array<T>& target)
{
    if (fixedScale_p && fixedOffset_p) {
	scaleOnGet (scale_p, offset_p, array, target);
    }else{
	ArrayIterator<S> arrayIter (array, array.ndim() - 1);
	ReadOnlyArrayIterator<T> targetIter (target, target.ndim() - 1);
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

template<class S, class T>
void ScaledComplexData<S,T>::scaleColumnOnPut (const Array<S>& array,
					       Array<T>& target)
{
    if (fixedScale_p && fixedOffset_p) {
	scaleOnPut (scale_p, offset_p, array, target);
    }else{
	ReadOnlyArrayIterator<S> arrayIter (array, array.ndim() - 1);
	ArrayIterator<T> targetIter (target, target.ndim() - 1);
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

template<class S, class T>
void ScaledComplexData<S,T>::scaleCellsOnGet (Array<S>& array,
					      const Array<T>& target,
					      const RefRows& rownrs)
{
    if (fixedScale_p && fixedOffset_p) {
	scaleOnGet (scale_p, offset_p, array, target);
    }else{
	ArrayIterator<S> arrayIter (array, array.ndim() - 1);
	ReadOnlyArrayIterator<T> targetIter (target, target.ndim() - 1);
	RefRowsSliceIter rowiter(rownrs);
	while (! rowiter.pastEnd()) {
	    uInt rownr = rowiter.sliceStart();
	    uInt end = rowiter.sliceEnd();
	    uInt incr = rowiter.sliceIncr();
	    // Iterate through the row numbers in the slice.
	    while (rownr <= end) {
	        scaleOnGet (getScale(rownr), getOffset(rownr),
			    arrayIter.array(), targetIter.array());
		rownr += incr;
		arrayIter.next();
		targetIter.next();
	    }
	    // Go to next slice.
	    rowiter++;
	}
    }
}

template<class S, class T>
void ScaledComplexData<S,T>::scaleCellsOnPut (const Array<S>& array,
					      Array<T>& target,
					      const RefRows& rownrs)
{
    if (fixedScale_p && fixedOffset_p) {
	scaleOnPut (scale_p, offset_p, array, target);
    }else{
	ReadOnlyArrayIterator<S> arrayIter (array, array.ndim() - 1);
	ArrayIterator<T> targetIter (target, target.ndim() - 1);
	RefRowsSliceIter rowiter(rownrs);
	while (! rowiter.pastEnd()) {
	    uInt rownr = rowiter.sliceStart();
	    uInt end = rowiter.sliceEnd();
	    uInt incr = rowiter.sliceIncr();
	    // Iterate through the row numbers in the slice.
	    while (rownr <= end) {
		scaleOnPut (getScale(rownr), getOffset(rownr),
			    arrayIter.array(), targetIter.array());
		rownr += incr;
		arrayIter.next();
		targetIter.next();
	    }
	    // Go to next slice.
	    rowiter++;
	}
    }
}


template<class S, class T>
void ScaledComplexData<S,T>::getArray (uInt rownr, Array<S>& array)
{
    Array<T> target(storedShape(array.shape()));
    column().get (rownr, target);
    scaleOnGet (getScale(rownr), getOffset(rownr), array, target);
}
template<class S, class T>
void ScaledComplexData<S,T>::putArray (uInt rownr, const Array<S>& array)
{
    Array<T> target(storedShape(array.shape()));
    scaleOnPut (getScale(rownr), getOffset(rownr), array, target);
    column().put (rownr, target);
}

template<class S, class T>
void ScaledComplexData<S,T>::getSlice (uInt rownr, const Slicer& slicer,
				       Array<S>& array)
{
    Array<T> target(storedShape(array.shape()));
    column().getSlice (rownr, storedSlicer(slicer), target);
    scaleOnGet (getScale(rownr), getOffset(rownr), array, target);
}
template<class S, class T>
void ScaledComplexData<S,T>::putSlice (uInt rownr, const Slicer& slicer,
				       const Array<S>& array)
{
    Array<T> target(storedShape(array.shape()));
    scaleOnPut (getScale(rownr), getOffset(rownr), array, target);
    column().putSlice (rownr, storedSlicer(slicer), target);
}

template<class S, class T>
void ScaledComplexData<S,T>::getArrayColumn (Array<S>& array)
{
    Array<T> target(storedShape(array.shape()));
    column().getColumn (target);
    scaleColumnOnGet (array, target);
}
template<class S, class T>
void ScaledComplexData<S,T>::putArrayColumn (const Array<S>& array)
{
    Array<T> target(storedShape(array.shape()));
    scaleColumnOnPut (array, target);
    column().putColumn (target);
}

template<class S, class T>
void ScaledComplexData<S,T>::getArrayColumnCells (const RefRows& rownrs,
						  Array<S>& array)
{
    Array<T> target(storedShape(array.shape()));
    column().getColumnCells (rownrs, target);
    scaleCellsOnGet (array, target, rownrs);
}
template<class S, class T>
void ScaledComplexData<S,T>::putArrayColumnCells (const RefRows& rownrs,
						  const Array<S>& array)
{
    Array<T> target(storedShape(array.shape()));
    scaleCellsOnPut (array, target, rownrs);
    column().putColumnCells (rownrs, target);
}

template<class S, class T>
void ScaledComplexData<S,T>::getColumnSlice (const Slicer& slicer,
					     Array<S>& array)
{
    Array<T> target(storedShape(array.shape()));
    column().getColumn (storedSlicer(slicer), target);
    scaleColumnOnGet (array, target);
}
template<class S, class T>
void ScaledComplexData<S,T>::putColumnSlice (const Slicer& slicer,
					     const Array<S>& array)
{
    Array<T> target(storedShape(array.shape()));
    scaleColumnOnPut (array, target);
    column().putColumn (storedSlicer(slicer), target);
}

template<class S, class T>
void ScaledComplexData<S,T>::getColumnSliceCells (const RefRows& rownrs,
						  const Slicer& slicer,
						  Array<S>& array)
{
    Array<T> target(storedShape(array.shape()));
    column().getColumnCells (rownrs, storedSlicer(slicer), target);
    scaleCellsOnGet (array, target, rownrs);
}
template<class S, class T>
void ScaledComplexData<S,T>::putColumnSliceCells (const RefRows& rownrs,
						  const Slicer& slicer,
						  const Array<S>& array)
{
    Array<T> target(storedShape(array.shape()));
    scaleCellsOnPut (array, target, rownrs);
    column().putColumnCells (rownrs, storedSlicer(slicer), target);
}


template<class S, class T>
Slicer ScaledComplexData<S,T>::storedSlicer (const Slicer& virtualSlicer) const
{
    return Slicer (IPosition(1,0).concatenate (virtualSlicer.start()),
		   IPosition(1,1).concatenate (virtualSlicer.end()),
		   IPosition(1,1).concatenate (virtualSlicer.stride()),
		   Slicer::endIsLast);
}

} //# NAMESPACE CASACORE - END


#endif
