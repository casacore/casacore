//# TSMCoordColumn.cc: Tiled Hypercube Storage Manager for id columns
//# Copyright (C) 1995,1996,1997,1999,2001
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
#include <casacore/tables/DataMan/TSMCoordColumn.h>
#include <casacore/tables/DataMan/TiledStMan.h>
#include <casacore/tables/DataMan/TSMCube.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Containers/RecordField.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/BasicSL/String.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

TSMCoordColumn::TSMCoordColumn (const TSMColumn& column, uInt axisNr)
: TSMColumn (column),
  axisNr_p  (axisNr)
{}

TSMCoordColumn::~TSMCoordColumn()
{}


void TSMCoordColumn::setShape (uInt rownr, const IPosition& shape)
{
    if (shape.nelements() != 1) {
	throw (TSMError ("setShape of coordinate column " + columnName() +
			 " is not 1-dim"));
    }
    TSMCube* hypercube = stmanPtr_p->getHypercube (rownr);
    IPosition cubeShape = hypercube->cubeShape();
    if (cubeShape.nelements() != 0) {
	if (shape(0) != cubeShape(axisNr_p)) {
	    throw (TSMError ("setShape of coordinate column " + columnName() +
			     " does not match shape of hypercube"));
	}
	return;
    }
    const Record& rec = hypercube->valueRecord();
    if (rec.isDefined (columnName())) {
	if (shape != rec.shape (columnName())) {
	    throw (TSMError ("setShape of coordinate column " + columnName() +
			     " has already been done"));
	}
	return;
    }
    stmanPtr_p->setDataChanged();
    Record& record = hypercube->rwValueRecord();
    switch (dataType()) {
    case TpInt:
    case TpArrayInt:
	record.define (columnName(), Array<Int>(shape));
	break;
    case TpUInt:
    case TpArrayUInt:
	record.define (columnName(), Array<uInt>(shape));
	break;
    case TpFloat:
    case TpArrayFloat:
	record.define (columnName(), Array<float>(shape));
	break;
    case TpDouble:
    case TpArrayDouble:
	record.define (columnName(), Array<double>(shape));
	break;
    case TpComplex:
    case TpArrayComplex:
	record.define (columnName(), Array<Complex>(shape));
	break;
    case TpDComplex:
    case TpArrayDComplex:
	record.define (columnName(), Array<DComplex>(shape));
	break;
    default:
	throw (DataManInvDT ("Unknown data type for coordinate column " +
			     columnName()));
    }
}

Bool TSMCoordColumn::isShapeDefined (uInt rownr)
{
    //# The shape is defined when the shape is fixed, when
    //# a hypercube has been defined for this row or when the
    //# coordinate values have already been defined.
    if (shapeColumn().nelements() != 0) {
	return True;                             // FixedShape
    }
    TSMCube* hypercube = stmanPtr_p->getHypercube (rownr);
    if (hypercube->valueRecord().isDefined (columnName())) {
	return True;                             // already defined
    }
    return  (hypercube->cubeShape().nelements() != 0);
    
}

IPosition TSMCoordColumn::shape (uInt rownr)
{
    //# Return the shape when it is fixed.
    if (shapeColumn().nelements() != 0) {
	return shapeColumn();
    }
    //# Return coordinate shape if defined.
    TSMCube* hypercube = stmanPtr_p->getHypercube (rownr);
    const IPosition& cubeShape = hypercube->cubeShape();
    if (cubeShape.nelements() != 0) {
	return IPosition (1, cubeShape(axisNr_p));
    }
    if (! hypercube->valueRecord().isDefined (columnName())) {
        throw (DataManInvOper ("TSMCoord: no array in row " +
			       String::toString(rownr) +
			       " of coordinate column " + columnName()));
    }
    return hypercube->valueRecord().shape (columnName());
}


void TSMCoordColumn::getfloatV (uInt rownr, float* dataPtr)
{
    // Get the hypercube the row is in.
    // It also gives the position of the row in the hypercube.
    IPosition position;
    TSMCube* hypercube = stmanPtr_p->getHypercube (rownr, position);
    // Get vector of coordinate values.
    // Since a reference is used, it has to be treated as an Array.
    // Get the correct value.
    RORecordFieldPtr<Array<float> > field (hypercube->valueRecord(),
					   columnName());
    *dataPtr = (*field) (IPosition (1, position(axisNr_p)));
}

void TSMCoordColumn::putfloatV (uInt rownr, const float* dataPtr)
{
    // Get the hypercube the row is in.
    // It also gives the position of the row in the hypercube.
    IPosition position;
    TSMCube* hypercube = stmanPtr_p->getHypercube (rownr, position);
    // Get vector of coordinate values.
    // Since a reference is used, it has to be treated as an Array.
    // Update the correct value.
    RecordFieldPtr<Array<float> > field (hypercube->rwValueRecord(),
					 columnName());
    (*field) (IPosition (1, position(axisNr_p))) = *dataPtr;
    stmanPtr_p->setDataChanged();
}


void TSMCoordColumn::getArrayfloatV (uInt rownr, Array<float>* dataPtr)
{
    // Get the hypercube the row is in.
    // It also gives the position of the row in the hypercube.
    IPosition position;
    TSMCube* hypercube = stmanPtr_p->getHypercube (rownr, position);
    hypercube->valueRecord().get (columnName(), *dataPtr);
}

void TSMCoordColumn::putArrayfloatV (uInt rownr, const Array<float>* dataPtr)
{
    // Get the hypercube the row is in.
    // It also gives the position of the row in the hypercube.
    IPosition position;
    TSMCube* hypercube = stmanPtr_p->getHypercube (rownr, position);
    hypercube->rwValueRecord().define (columnName(), *dataPtr);
    stmanPtr_p->setDataChanged();
}



#define TSMCOORDCOLUMN_GETPUT(T,NM) \
void TSMCoordColumn::aips_name2(get,NM) (uInt rownr, T* dataPtr) \
{ \
    IPosition position; \
    TSMCube* hypercube = stmanPtr_p->getHypercube (rownr, position); \
    RORecordFieldPtr<Array<T> > field (hypercube->valueRecord(),columnName());\
    *dataPtr = (*field) (IPosition (1, position(axisNr_p))); \
} \
void TSMCoordColumn::aips_name2(put,NM) (uInt rownr, const T* dataPtr) \
{ \
    IPosition position; \
    TSMCube* hypercube = stmanPtr_p->getHypercube (rownr, position); \
    RecordFieldPtr<Array<T> > field (hypercube->rwValueRecord(),columnName()); \
    (*field) (IPosition (1, position(axisNr_p))) = *dataPtr; \
    stmanPtr_p->setDataChanged(); \
} \
void TSMCoordColumn::aips_name2(getArray,NM) (uInt rownr, Array<T>* dataPtr) \
{ \
    IPosition position; \
    TSMCube* hypercube = stmanPtr_p->getHypercube (rownr, position); \
    hypercube->valueRecord().get (columnName(), *dataPtr); \
} \
void TSMCoordColumn::aips_name2(putArray,NM) (uInt rownr, const Array<T>* dataPtr) \
{ \
    IPosition position; \
    TSMCube* hypercube = stmanPtr_p->getHypercube (rownr, position); \
    hypercube->rwValueRecord().define (columnName(), *dataPtr); \
    stmanPtr_p->setDataChanged(); \
}

TSMCOORDCOLUMN_GETPUT(Int,IntV)
TSMCOORDCOLUMN_GETPUT(uInt,uIntV)
//#TSMCOORDCOLUMN_GETPUT(float,floatV)
TSMCOORDCOLUMN_GETPUT(double,doubleV)
TSMCOORDCOLUMN_GETPUT(Complex,ComplexV)
TSMCOORDCOLUMN_GETPUT(DComplex,DComplexV)

} //# NAMESPACE CASACORE - END

