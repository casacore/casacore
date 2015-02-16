//# ScaledArrayEngine.cc: Templated virtual column engine to scale a table array
//# Copyright (C) 1994,1995,1996,1997,1999,2001
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

#ifndef TABLES_SCALEDARRAYENGINE_TCC
#define TABLES_SCALEDARRAYENGINE_TCC

//# Includes
#include <casacore/tables/DataMan/ScaledArrayEngine.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayIter.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/ValTypeId.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class S, class T>
ScaledArrayEngine<S,T>::ScaledArrayEngine (const String& virtualColumnName,
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
ScaledArrayEngine<S,T>::ScaledArrayEngine (const String& virtualColumnName,
					   const String& storedColumnName,
					   const String& scaleColumnName,
					   S offset)
: BaseMappedArrayEngine<S,T> (virtualColumnName, storedColumnName),
  scaleName_p   (scaleColumnName),
  scale_p       (0.0),
  offset_p      (offset),
  fixedScale_p  (False),
  fixedOffset_p (True),
  scaleColumn_p (0),
  offsetColumn_p(0)
{}

template<class S, class T>
ScaledArrayEngine<S,T>::ScaledArrayEngine (const String& virtualColumnName,
					   const String& storedColumnName,
					   const String& scaleColumnName,
					   const String& offsetColumnName)
: BaseMappedArrayEngine<S,T> (virtualColumnName, storedColumnName),
  scaleName_p   (scaleColumnName),
  offsetName_p  (offsetColumnName),
  scale_p       (0.0),
  offset_p      (0.0),
  fixedScale_p  (False),
  fixedOffset_p (False),
  scaleColumn_p (0),
  offsetColumn_p(0)
{}

template<class S, class T>
ScaledArrayEngine<S,T>::ScaledArrayEngine (const Record& spec)
: BaseMappedArrayEngine<S,T> (),
  scale_p       (1.0),
  offset_p      (0.0),
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
ScaledArrayEngine<S,T>::ScaledArrayEngine (const ScaledArrayEngine<S,T>& that)
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
ScaledArrayEngine<S,T>::~ScaledArrayEngine()
{
    delete scaleColumn_p;
    delete offsetColumn_p;
}

//# Clone the engine object.
template<class S, class T>
DataManager* ScaledArrayEngine<S,T>::clone() const
{
    DataManager* dmPtr = new ScaledArrayEngine<S,T> (*this);
    return dmPtr;
}


//# Return the type name of the engine (i.e. its class name).
template<class S, class T>
String ScaledArrayEngine<S,T>::dataManagerType() const
{
    return className();
}
//# Return the class name.
//# Get the data type names using class ValType.
template<class S, class T>
String ScaledArrayEngine<S,T>::className()
{
    return "ScaledArrayEngine<" + valDataTypeId (static_cast<S*>(0)) + ","
	                        + valDataTypeId (static_cast<T*>(0)) + ">";
}

template<class S, class T>
String ScaledArrayEngine<S,T>::dataManagerName() const
{
    return virtualName();
}

template<class S, class T>
Record ScaledArrayEngine<S,T>::dataManagerSpec() const
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
        spec.define ("SCALE", scale_p);
    } else {
        spec.define ("SCALENAME", scaleName_p);
    }
    return spec;
}

template<class S, class T>
DataManager* ScaledArrayEngine<S,T>::makeObject (const String&,
						 const Record& spec)
{
    DataManager* dmPtr = new ScaledArrayEngine<S,T>(spec);
    return dmPtr;
}
template<class S, class T>
void ScaledArrayEngine<S,T>::registerClass()
{
    DataManager::registerCtor (className(), makeObject);
}


template<class S, class T>
void ScaledArrayEngine<S,T>::create (uInt initialNrrow)
{
    BaseMappedArrayEngine<S,T>::create (initialNrrow);
    // Store the various parameters as keywords in this column.
    TableColumn thisCol (table(), virtualName());
    thisCol.rwKeywordSet().define ("_ScaledArrayEngine_Scale",
				   scale_p);
    thisCol.rwKeywordSet().define ("_ScaledArrayEngine_Offset",
				   offset_p);
    thisCol.rwKeywordSet().define ("_ScaledArrayEngine_ScaleName",
				   scaleName_p);
    thisCol.rwKeywordSet().define ("_ScaledArrayEngine_OffsetName",
				   offsetName_p);
    thisCol.rwKeywordSet().define ("_ScaledArrayEngine_FixedScale",
				   fixedScale_p);
    thisCol.rwKeywordSet().define ("_ScaledArrayEngine_FixedOffset",
				   fixedOffset_p);
}

template<class S, class T>
void ScaledArrayEngine<S,T>::prepare()
{
    BaseMappedArrayEngine<S,T>::prepare();
    TableColumn thisCol (table(), virtualName());
    thisCol.keywordSet().get ("_ScaledArrayEngine_Scale",       scale_p);
    thisCol.keywordSet().get ("_ScaledArrayEngine_Offset",      offset_p);
    thisCol.keywordSet().get ("_ScaledArrayEngine_ScaleName",   scaleName_p);
    thisCol.keywordSet().get ("_ScaledArrayEngine_OffsetName",  offsetName_p);
    thisCol.keywordSet().get ("_ScaledArrayEngine_FixedScale",  fixedScale_p);
    thisCol.keywordSet().get ("_ScaledArrayEngine_FixedOffset", fixedOffset_p);
    //# Allocate column objects to get scale and offset.
    if (! fixedScale_p) {
	scaleColumn_p = new ScalarColumn<S> (table(), scaleName_p);
    }
    if (! fixedOffset_p) {
	offsetColumn_p = new ScalarColumn<S> (table(), offsetName_p);
    }
}


template<class S, class T>
S ScaledArrayEngine<S,T>::getScale (uInt rownr)
{
    if (fixedScale_p) {
	return scale_p;
    }
    return (*scaleColumn_p)(rownr);
}
template<class S, class T>
S ScaledArrayEngine<S,T>::getOffset (uInt rownr)
{
    if (fixedOffset_p) {
	return offset_p;
    }
    return (*offsetColumn_p)(rownr);
}

// Scale/offset an array for get.
template<class S, class T>
void ScaledArrayEngine<S,T>::scaleOnGet (S scale, S offset,
					 Array<S>& array,
					 const Array<T>& target)
{
    Bool deleteIn, deleteOut;
    S* out = array.getStorage (deleteOut);
    S* op  = out;
    const T* in = target.getStorage (deleteIn);
    const T* ip = in;
    const T* last = ip + array.nelements();
    if (offset == 0) {
	if (scale == 1) {
	    while (ip < last) {
		*op++ = *ip++;
	    }
	}else{
	    while (ip < last) {
		*op++ = *ip++ * scale;
	    }
	}
    }else{
	if (scale == 1) {
	    while (ip < last) {
		*op++ = *ip++ + offset;
	    }
	}else{
	    while (ip < last) {
		*op++ = *ip++ * scale + offset;
	    }
	}
    }
    target.freeStorage (in, deleteIn);
    array.putStorage (out, deleteOut);
}

// Scale/offset an array for put.
template<class S, class T>
void ScaledArrayEngine<S,T>::scaleOnPut (S scale, S offset,
					 const Array<S>& array,
					 Array<T>& target)
{
    Bool deleteIn, deleteOut;
    const S* in = array.getStorage (deleteIn);
    const S* ip = in;
    T* out = target.getStorage (deleteOut);
    T* op  = out;
    const T* last = op + array.nelements();
    if (offset == 0) {
	if (scale == 1) {
	    while (op < last) {
		*op++ = T(*ip++);
	    }
	}else{
	    while (op < last) {
		*op++ = T(*ip++ / scale);
	    }
	}
    }else{
	if (scale == 1) {
	    while (op < last) {
		*op++ = T(*ip++ - offset);
	    }
	}else{
	    while (op < last) {
		*op++ = T((*ip++ - offset) / scale);
	    }
	}
    }
    array.freeStorage (in, deleteIn);
    target.putStorage (out, deleteOut);
}


template<class S, class T>
void ScaledArrayEngine<S,T>::scaleColumnOnGet (Array<S>& array,
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
void ScaledArrayEngine<S,T>::scaleColumnOnPut (const Array<S>& array,
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
void ScaledArrayEngine<S,T>::getArray (uInt rownr, Array<S>& array)
{
    Array<T> target(array.shape());
    column().get (rownr, target);
    scaleOnGet (getScale(rownr), getOffset(rownr), array, target);
}
template<class S, class T>
void ScaledArrayEngine<S,T>::putArray (uInt rownr, const Array<S>& array)
{
    Array<T> target(array.shape());
    scaleOnPut (getScale(rownr), getOffset(rownr), array, target);
    column().put (rownr, target);
}

template<class S, class T>
void ScaledArrayEngine<S,T>::getSlice (uInt rownr, const Slicer& slicer,
				       Array<S>& array)
{
    Array<T> target(array.shape());
    column().getSlice (rownr, slicer, target);
    scaleOnGet (getScale(rownr), getOffset(rownr), array, target);
}
template<class S, class T>
void ScaledArrayEngine<S,T>::putSlice (uInt rownr, const Slicer& slicer,
				       const Array<S>& array)
{
    Array<T> target(array.shape());
    scaleOnPut (getScale(rownr), getOffset(rownr), array, target);
    column().putSlice (rownr, slicer, target);
}

template<class S, class T>
void ScaledArrayEngine<S,T>::getArrayColumn (Array<S>& array)
{
    Array<T> target(array.shape());
    column().getColumn (target);
    scaleColumnOnGet (array, target);
}
template<class S, class T>
void ScaledArrayEngine<S,T>::putArrayColumn (const Array<S>& array)
{
    Array<T> target(array.shape());
    scaleColumnOnPut (array, target);
    column().putColumn (target);
}

template<class S, class T>
void ScaledArrayEngine<S,T>::getColumnSlice (const Slicer& slicer,
					     Array<S>& array)
{
    Array<T> target(array.shape());
    column().getColumn (slicer, target);
    scaleColumnOnGet (array, target);
}
template<class S, class T>
void ScaledArrayEngine<S,T>::putColumnSlice (const Slicer& slicer,
					     const Array<S>& array)
{
    Array<T> target(array.shape());
    scaleColumnOnPut (array, target);
    column().putColumn (slicer, target);
}

} //# NAMESPACE CASACORE - END


#endif
