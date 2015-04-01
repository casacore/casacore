//# RetypedArrayEngine.cc: Virtual column engine to retype and reshape arrays
//# Copyright (C) 1995,1996,1999,2001
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

#ifndef TABLES_RETYPEDARRAYENGINE_TCC
#define TABLES_RETYPEDARRAYENGINE_TCC

//# Includes
#include <casacore/tables/DataMan/RetypedArrayEngine.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayIter.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/ValTypeId.h>
#include <casacore/casa/Utilities/Copy.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class S, class T>
RetypedArrayEngine<S,T>::RetypedArrayEngine (const String& virtualColumnName,
					     const String& storedColumnName)
: BaseMappedArrayEngine<S,T> (virtualColumnName, storedColumnName),
  shape_p    (S::shape()),
  isVirtualFixedShape_p (False),
  copyInfo_p (0)
{}

template<class S, class T>
RetypedArrayEngine<S,T>::RetypedArrayEngine (const String& virtualColumnName,
					   const String& storedColumnName,
					   const IPosition& virtualShape,
					   const TableRecord& extraInformation)
: BaseMappedArrayEngine<S,T> (virtualColumnName, storedColumnName),
  shape_p    (virtualShape),
  isVirtualFixedShape_p (False),
  record_p   (extraInformation),
  copyInfo_p (0)
{}

template<class S, class T>
RetypedArrayEngine<S,T>::RetypedArrayEngine (const Record& spec)
: BaseMappedArrayEngine<S,T> (),
  isVirtualFixedShape_p (False),
  copyInfo_p (0)
{
    if (spec.isDefined("SOURCENAME")  &&  spec.isDefined("TARGETNAME")) {
        setNames (spec.asString("SOURCENAME"), spec.asString("TARGETNAME"));
	if (spec.isDefined("SHAPE")) {
	    Vector<Int> shp;
	    spec.get ("SHAPE", shp);
	    shape_p.resize (shp.nelements());
	    shape_p = IPosition(shp);
	}
	if (spec.isDefined("RECORD")) {
	    record_p = spec.asRecord ("RECORD");
	}
    }
}

template<class S, class T>
RetypedArrayEngine<S,T>::RetypedArrayEngine
                                        (const RetypedArrayEngine<S,T>& that)
: BaseMappedArrayEngine<S,T> (that),
  shape_p    (that.shape_p),
  isVirtualFixedShape_p (False),
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
    return "RetypedArrayEngine<" + valDataTypeId(static_cast<S*>(0)) + "," +
	                           valDataTypeId(static_cast<T*>(0)) + ">";
}

template<class S, class T>
String RetypedArrayEngine<S,T>::dataManagerName() const
{
    return virtualName();
}

template<class S, class T>
Record RetypedArrayEngine<S,T>::dataManagerSpec() const
{
    Record spec;
    spec.define ("SOURCENAME", virtualName());
    spec.define ("TARGETNAME", storedName());
    spec.define ("SHAPE", shape_p.asVector());
    if (record_p.nfields() > 0) {
        spec.defineRecord ("RECORD", record_p);
    }
    return spec;
}

template<class S, class T>
DataManager* RetypedArrayEngine<S,T>::makeObject (const String&,
						  const Record& spec)
{
    DataManager* dmPtr = new RetypedArrayEngine<S,T>(spec);
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
    TableColumn thisCol (table(), virtualName());
    Vector<Int> vector;
    thisCol.keywordSet().get ("_RetypedArrayEngine_Shape", vector);
    shape_p.resize (vector.nelements());
    shape_p = IPosition (vector);
    record_p = thisCol.keywordSet().subRecord ("_RetypedArrayEngine_Record");
    // Set the column shape in the base class (when needed).
    // This has to be dome before prepare in the base class is called.
    if (isVirtualFixedShape_p) {
	BaseMappedArrayEngine<S,T>::setShapeColumn
                                (shape_p.concatenate (virtualFixedShape_p));
    }
    BaseMappedArrayEngine<S,T>::prepare();
    // Allocate and initialize a CopyInfo object for the virtual.
    copyInfo_p = S::newCopyInfo (record_p, shape_p);
}

template<class S, class T>
void RetypedArrayEngine<S,T>::create (uInt initialNrrow)
{
    BaseMappedArrayEngine<S,T>::create (initialNrrow);
    // Store the various parameters as keywords in this column.
    TableColumn thisCol (this->makeTableColumn (virtualName()));
    thisCol.rwKeywordSet().define ("_RetypedArrayEngine_Shape",
				   shape_p.asVector());
    thisCol.rwKeywordSet().defineRecord ("_RetypedArrayEngine_Record",
					 record_p);
}


//# This function is called in case the virtual column has FixedShape arrays.
//# Because the shape of the VirtualType is not known yet (it is read
//# in prepare), the base class setShapeColumn is done in prepare().
template<class S, class T>
void RetypedArrayEngine<S,T>::setShapeColumn (const IPosition& shape)
{
    virtualFixedShape_p = shape;
    isVirtualFixedShape_p = True;
}

template<class S, class T>
void RetypedArrayEngine<S,T>::setShape (uInt rownr, const IPosition& shape)
{
    //# Do not define the shape in the stored column when it has
    //# already been defined and matches the virtual shape.
    if (column().isDefined (rownr)) {
	IPosition storedShape = column().shape (rownr);
	IPosition virtualShape = storedShape.getLast (shape.nelements());
	if (shape.isEqual (virtualShape)) {
	    return;
	}
    }
    //# Set the stored shape to the default element shape plus virtual shape.
    column().setShape (rownr, shape_p.concatenate (shape));
}

template<class S, class T>
uInt RetypedArrayEngine<S,T>::ndim (uInt rownr)
{
    return column().ndim (rownr) - shape_p.nelements();
}

template<class S, class T>
IPosition RetypedArrayEngine<S,T>::shape (uInt rownr)
{
    // The virtual shape is the stored shape minus the first dimensions.
    IPosition storedShape = column().shape (rownr);
    return storedShape.getLast (storedShape.nelements() - shape_p.nelements());
}


template<class S, class T>
IPosition RetypedArrayEngine<S,T>::getStoredShape
(uInt rownr, const IPosition& virtualShape)
{
    //# Determine the element shape.
    //# If the stored is defined, take it from there.
    IPosition elemShape(shape_p);
    if (rownr < table().nrow()  &&  column().isDefined (rownr)) {
        elemShape = (column().shape(rownr)).getFirst (elemShape.nelements());
    }
    //# The stored shape is element shape plus virtual shape.
    return elemShape.concatenate (virtualShape);
}

template<class S, class T>
Slicer RetypedArrayEngine<S,T>::getStoredSlicer
(const Slicer& virtualSlicer) const
{
    //# Determine the element dimensionality.
    //# Make the Slicer such that all values of the element are used.
    uInt ndim = shape_p.nelements();
    return Slicer (IPosition(ndim,0).concatenate (virtualSlicer.start()),
		   IPosition(ndim,Slicer::MimicSource).
                                     concatenate (virtualSlicer.end()),
		   IPosition(ndim,1).concatenate (virtualSlicer.stride()),
		   Slicer::endIsLast);
}


template<class S, class T>
IPosition RetypedArrayEngine<S,T>::checkShape (const Array<S>& source,
					       const Array<T>& target)
{
    IPosition tShape = target.shape();
    IPosition sShape = source.shape();
    //# Check if the dimensionalities match.
    //# Source + element shape must match stored shape.
    if (tShape.nelements() != shape_p.nelements() + sShape.nelements()) {
	throw (DataManInvOper ("RetypedArrayEngine: stored/virtual"
			       " dimensionalities are not appropriate"));
    }
    uInt i;
    //# Determine and check the shape of the virtual elements in the target
    //# which are formed by the first axes in the stored.
    //# Their shape cannot be greater than the real virtual element shape.
    IPosition elemShape (shape_p.nelements());
    for (i=0; i<shape_p.nelements(); i++) {
	if (tShape(i) > shape_p(i)) {
	    throw (DataManInvOper
		               ("RetypedArrayEngine: stored shape > virtual"));
	}
	elemShape(i) = tShape(i);
    }
    //# Check if remaining sizes in stored shape match virtual shape.
    for (uInt j=0; j<sShape.nelements(); j++) {
	if (sShape(j) != tShape(i++)) {
	    throw (DataManInvOper ("RetypedArrayEngine: stored/virtual shape"
				   " mismatch"));
	}
    }
    return elemShape;
}


// Copy an array for get.
template<class S, class T>
void RetypedArrayEngine<S,T>::mapOnGet (Array<S>& array,
                                        const Array<T>& target)
{
    IPosition elemShape = checkShape (array, target);
    S::set (copyInfo_p, &array, target, elemShape);
}

// Copy an array for put.
template<class S, class T>
void RetypedArrayEngine<S,T>::mapOnPut (const Array<S>& array,
					 Array<T>& target)
{
    IPosition elemShape = checkShape (array, target);
    S::get (copyInfo_p, target, &array, elemShape);
}


} //# NAMESPACE CASACORE - END


#endif
