//# RetypedArrayEngine.cc: Virtual column engine to retype and reshape arrays
//# Copyright (C) 1995,1996,1999
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
#include <aips/Tables/RetypedArrayEngine.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/ColumnDesc.h>
#include <aips/Tables/DataManError.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayIter.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/ValTypeId.h>
#include <aips/Utilities/Copy.h>


template<class S, class T>
RetypedArrayEngine<S,T>::RetypedArrayEngine ()
: BaseMappedArrayEngine<S,T> (),
  isSourceFixedShape_p (False),
  copyInfo_p (0)
{}

template<class S, class T>
RetypedArrayEngine<S,T>::RetypedArrayEngine (const String& sourceColumnName,
					     const String& targetColumnName)
: BaseMappedArrayEngine<S,T> (sourceColumnName, targetColumnName),
  shape_p    (S::shape()),
  isSourceFixedShape_p (False),
  copyInfo_p (0)
{}

template<class S, class T>
RetypedArrayEngine<S,T>::RetypedArrayEngine (const String& sourceColumnName,
					   const String& targetColumnName,
					   const IPosition& sourceShape,
					   const TableRecord& extraInformation)
: BaseMappedArrayEngine<S,T> (sourceColumnName, targetColumnName),
  shape_p    (sourceShape),
  isSourceFixedShape_p (False),
  record_p   (extraInformation),
  copyInfo_p (0)
{}

template<class S, class T>
RetypedArrayEngine<S,T>::RetypedArrayEngine
                                        (const RetypedArrayEngine<S,T>& that)
: BaseMappedArrayEngine<S,T> (that),
  shape_p    (that.shape_p),
  isSourceFixedShape_p (False),
  record_p   (that.record_p),
  copyInfo_p (0)
{}


template<class S, class T>
RetypedArrayEngine<S,T>::~RetypedArrayEngine()
{
    S::deleteCopyInfo (copyInfo_p);
}


//# Clone the engine object.
template<class S, class T>
DataManager* RetypedArrayEngine<S,T>::clone() const
{
    DataManager* dmPtr = new RetypedArrayEngine<S,T> (*this);
    if (dmPtr == 0) {
	throw (AllocError ("RetypedArrayEngine::clone()", 1));
    }
    return dmPtr;
}


//# Return the type name of the engine (i.e. its class name).
template<class S, class T>
String RetypedArrayEngine<S,T>::dataManagerType() const
{
    return className();
}
//# Return the class name.
//# Get the data type names using class ValType.
template<class S, class T>
String RetypedArrayEngine<S,T>::className()
{
    return "RetypedArrayEngine<" + valDataTypeId((S*)0) + "," +
	                           valDataTypeId((T*)0) + ">";
}

template<class S, class T>
DataManager* RetypedArrayEngine<S,T>::makeObject (const String&)
{
    DataManager* dmPtr = new RetypedArrayEngine<S,T>();
    if (dmPtr == 0) {
	throw (AllocError ("RetypedArrayEngine::makeObject()", 1));
    }
    return dmPtr;
}
template<class S, class T>
void RetypedArrayEngine<S,T>::registerClass()
{
    DataManager::registerCtor (className(), makeObject);
}


template<class S, class T>
void RetypedArrayEngine<S,T>::prepare()
{
    // Get the various parameters from keywords in this column.
    ROTableColumn thisCol (table(), sourceName());
    Vector<Int> vector;
    thisCol.keywordSet().get ("_RetypedArrayEngine_Shape", vector);
    shape_p.resize (vector.nelements());
    shape_p = IPosition (vector);
    record_p = thisCol.keywordSet().subRecord ("_RetypedArrayEngine_Record");
    // Set the column shape in the base class (when needed).
    // This has to be dome before prepare in the base class is called.
    if (isSourceFixedShape_p) {
	BaseMappedArrayEngine<S,T>::setShapeColumn
                                (shape_p.concatenate (sourceFixedShape_p));
    }
    BaseMappedArrayEngine<S,T>::prepare();
    // Allocate and initialize a CopyInfo object for the source.
    copyInfo_p = S::newCopyInfo (record_p, shape_p);
}

template<class S, class T>
void RetypedArrayEngine<S,T>::create (uInt initialNrrow)
{
    BaseMappedArrayEngine<S,T>::create (initialNrrow);
    // Store the various parameters as keywords in this column.
    TableColumn thisCol (makeTableColumn (sourceName()));
    thisCol.rwKeywordSet().define ("_RetypedArrayEngine_Shape",
				   shape_p.asVector());
    thisCol.rwKeywordSet().defineRecord ("_RetypedArrayEngine_Record",
					 record_p);
}


//# This function is called in case the source column has FixedShape arrays.
//# Because the shape of the SourceType is not known yet (it is read
//# in prepare), the base class setShapeColumn is done in prepare().
template<class S, class T>
void RetypedArrayEngine<S,T>::setShapeColumn (const IPosition& shape)
{
    sourceFixedShape_p = shape;
    isSourceFixedShape_p = True;
}

template<class S, class T>
void RetypedArrayEngine<S,T>::setShape (uInt rownr, const IPosition& shape)
{
    //# Do not define the shape in the target column when it has
    //# already been defined and matches the source shape.
    if (rwColumn().isDefined (rownr)) {
	IPosition targetShape = rwColumn().shape (rownr);
	IPosition sourceShape = targetShape.getLast (shape.nelements());
	if (shape.isEqual (sourceShape)) {
	    return;
	}
    }
    //# Set the target shape to the default element shape plus source shape.
    rwColumn().setShape (rownr, shape_p.concatenate (shape));
}

template<class S, class T>
uInt RetypedArrayEngine<S,T>::ndim (uInt rownr)
{
    return roColumn().ndim (rownr) - shape_p.nelements();
}

template<class S, class T>
IPosition RetypedArrayEngine<S,T>::shape (uInt rownr)
{
    // The source shape is the target shape minus the first dimensions.
    IPosition targetShape = roColumn().shape (rownr);
    return targetShape.getLast (targetShape.nelements() - shape_p.nelements());
}


template<class S, class T>
IPosition RetypedArrayEngine<S,T>::targetShape (uInt rownr,
						const IPosition& sourceShape)
{
    //# Determine the element shape.
    //# If the target is defined, take it from there.
    IPosition elemShape(shape_p);
    if (rownr < table().nrow()  &&  roColumn().isDefined (rownr)) {
	elemShape = (roColumn().shape(rownr)).getFirst (elemShape.nelements());
    }
    //# The target shape is element shape plus source shape.
    return elemShape.concatenate (sourceShape);
}

template<class S, class T>
Slicer RetypedArrayEngine<S,T>::targetSlicer (const Slicer& sourceSlicer) const
{
    //# Determine the element dimensionality.
    //# Make the Slicer such that all values of the element are used.
    uInt ndim = shape_p.nelements();
    return Slicer (IPosition(ndim,0).concatenate (sourceSlicer.start()),
		   IPosition(ndim,Slicer::MimicSource).
                                     concatenate (sourceSlicer.end()),
		   IPosition(ndim,1).concatenate (sourceSlicer.stride()),
		   Slicer::endIsLast);
}


template<class S, class T>
IPosition RetypedArrayEngine<S,T>::checkShape (const Array<S>& source,
					       const Array<T>& target)
{
    IPosition tShape = target.shape();
    IPosition sShape = source.shape();
    //# Check if the dimensionalities match.
    //# Source + element shape must match target shape.
    if (tShape.nelements() != shape_p.nelements() + sShape.nelements()) {
	throw (DataManInvOper ("RetypedArrayEngine: target/source"
			       " dimensionalities are not appropriate"));
    }
    uInt i;
    //# Determine and check the shape of the source elements in the target
    //# which are formed by the first axes in the target.
    //# Their shape cannot be greater than the real source element shape.
    IPosition elemShape (shape_p.nelements());
    for (i=0; i<shape_p.nelements(); i++) {
	if (tShape(i) > shape_p(i)) {
	    throw (DataManInvOper
		               ("RetypedArrayEngine: target shape > source"));
	}
	elemShape(i) = tShape(i);
    }
    //# Check if remaining sizes in target shape match source shape.
    for (uInt j=0; j<sShape.nelements(); j++) {
	if (sShape(j) != tShape(i++)) {
	    throw (DataManInvOper ("RetypedArrayEngine: target/source shape"
				   " mismatch"));
	}
    }
    return elemShape;
}


// Copy an array for get.
template<class S, class T>
void RetypedArrayEngine<S,T>::copyOnGet (Array<S>& array,
					 const Array<T>& target)
{
    IPosition elemShape = checkShape (array, target);
    S::set (copyInfo_p, &array, target, elemShape);
}

// Copy an array for put.
template<class S, class T>
void RetypedArrayEngine<S,T>::copyOnPut (const Array<S>& array,
					 Array<T>& target)
{
    IPosition elemShape = checkShape (array, target);
    S::get (copyInfo_p, target, &array, elemShape);
}


template<class S, class T>
void RetypedArrayEngine<S,T>::getArray (uInt rownr, Array<S>& array)
{
    Array<T> target;
    roColumn().get (rownr, target);
    copyOnGet (array, target);
}
template<class S, class T>
void RetypedArrayEngine<S,T>::putArray (uInt rownr, const Array<S>& array)
{
    Array<T> target(targetShape (rownr, array.shape()));
    copyOnPut (array, target);
    rwColumn().put (rownr, target);
}

template<class S, class T>
void RetypedArrayEngine<S,T>::getSlice (uInt rownr, const Slicer& slicer,
					Array<S>& array)
{
    Array<T> target;
    roColumn().getSlice (rownr, targetSlicer(slicer), target);
    copyOnGet (array, target);
}
template<class S, class T>
void RetypedArrayEngine<S,T>::putSlice (uInt rownr, const Slicer& slicer,
					const Array<S>& array)
{
    Array<T> target(targetShape (rownr, array.shape()));
    copyOnPut (array, target);
    rwColumn().putSlice (rownr, targetSlicer(slicer), target);
}

template<class S, class T>
void RetypedArrayEngine<S,T>::getArrayColumn (Array<S>& array)
{
    Array<T> target;
    roColumn().getColumn (target);
    copyOnGet (array, target);
}
template<class S, class T>
void RetypedArrayEngine<S,T>::putArrayColumn (const Array<S>& array)
{
    Array<T> target(targetShape (0, array.shape()));
    copyOnPut (array, target);
    rwColumn().putColumn (target);
}

template<class S, class T>
void RetypedArrayEngine<S,T>::getColumnSlice (const Slicer& slicer,
					      Array<S>& array)
{
    Array<T> target;
    roColumn().getColumn (targetSlicer(slicer), target);
    copyOnGet (array, target);
}
template<class S, class T>
void RetypedArrayEngine<S,T>::putColumnSlice (const Slicer& slicer,
					      const Array<S>& array)
{
    Array<T> target(targetShape (0, array.shape()));
    copyOnPut (array, target);
    rwColumn().putColumn (targetSlicer(slicer), target);
}
