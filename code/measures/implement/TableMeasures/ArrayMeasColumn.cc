//# Copyright (C) 1997,1998
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
#include <iostream.h>
#include <string.h>
#include <trial/TableMeasures/ArrayMeasColumn.h>
#include <trial/TableMeasures/ScalarMeasColumn.h>
#include <trial/TableMeasures/TableMeasDesc.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/Vector.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Measures/MeasRef.h>
#include <aips/Measures/MeasValue.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MDoppler.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MRadialVelocity.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableError.h>
#include <aips/Utilities/Assert.h>

template<class M, class MV>
ROArrayMeasColumn<M, MV>::ROArrayMeasColumn()
: itsDataCol(0),
  itsRefCodeCol(0),
  itsArrRefCodeCol(0),
  itsOffsetCol(0),
  itsArrOffsetCol(0)
{}

template<class M, class MV>
ROArrayMeasColumn<M, MV>::ROArrayMeasColumn(const Table& tab,
					    const String& columnName)
: itsDataCol(0),
  itsRefCodeCol(0),
  itsArrRefCodeCol(0),
  itsOffsetCol(0),
  itsArrOffsetCol(0)
{
    TableMeasDesc<M>* tmDesc = (TableMeasDesc<M>*) 
	TableMeasDescBase::reconstruct(tab, columnName);
    
    AlwaysAssert(M::showMe() == tmDesc->type(), AipsError);
    itsDataCol = new ROArrayColumn<Double>(tab, columnName);
    
    // Set up the reference code component of the MeasRef
    if (tmDesc->isRefCodeVariable()) {
	const String varColName = tmDesc->refColumnName();
	if (tab.tableDesc().columnDesc(varColName).isScalar()) {
	    itsRefCodeCol = new ROScalarColumn<Int>(tab, varColName);	
	} else {
	    itsArrRefCodeCol = new ROArrayColumn<Int>(tab, varColName);
    	}
	itsVarRefFlag = True;
    } else {
	itsMeasRef.set(tmDesc->getRefCode());
	itsVarRefFlag = False;
    }
    
    // Set up the offset component of the MeasRef
    if (tmDesc->hasOffset()) {
	if (tmDesc->isOffsetVariable()) {
	    const String varColName = tmDesc->offsetColumnName();
	    if (tab.tableDesc().columnDesc(varColName).isScalar()) {
		itsOffsetCol = new ROScalarMeasColumn<M, MV>(tab, varColName);
	    } else {
		itsArrOffsetCol = 
		  new ROArrayMeasColumn<M, MV>(tab, varColName);
    	    }
	    itsVarOffsetFlag = True;
	} else {
	    itsMeasRef.set(tmDesc->getOffset());
	    itsVarOffsetFlag = False;
	}
    } 
    delete tmDesc;
}

template<class M, class MV>
ROArrayMeasColumn<M, MV>::ROArrayMeasColumn(
    const ROArrayMeasColumn<M, MV>& that)
: itsVarRefFlag(that.itsVarRefFlag),
  itsVarOffsetFlag(that.itsVarOffsetFlag),
  itsDataCol(that.itsDataCol),
  itsRefCodeCol(that.itsRefCodeCol),
  itsArrRefCodeCol(that.itsArrRefCodeCol),
  itsOffsetCol(that.itsOffsetCol),
  itsArrOffsetCol(that.itsArrOffsetCol),
  itsMeasRef(that.itsMeasRef)
{
    if (itsDataCol != 0) {
	itsDataCol = new ROArrayColumn<Double>(*itsDataCol);
    }
    if (itsRefCodeCol != 0) {
	itsRefCodeCol = new ROScalarColumn<Int>(*itsRefCodeCol);
    }
    if (itsArrRefCodeCol != 0) {
	itsArrRefCodeCol = new ROArrayColumn<Int>(*itsArrRefCodeCol);
    }
    if (itsArrOffsetCol != 0) {
	itsArrOffsetCol = new ROArrayMeasColumn<M, MV>(*itsArrOffsetCol);
    }
}

template<class M, class MV>
ROArrayMeasColumn<M, MV>::~ROArrayMeasColumn()
{
    cleanUp();
}

template<class M, class MV>
void ROArrayMeasColumn<M, MV>::cleanUp()
{
    delete itsDataCol;
    delete itsRefCodeCol;
    delete itsArrRefCodeCol;
    delete itsOffsetCol;
    delete itsArrOffsetCol;
}

template<class M, class MV>
void ROArrayMeasColumn<M, MV>::reference(const ROArrayMeasColumn<M, MV>& that)
{   
    cleanUp();
    itsDataCol = that.itsDataCol;
    itsVarRefFlag = that.itsVarRefFlag;
    itsRefCodeCol = that.itsRefCodeCol;
    itsArrRefCodeCol = that.itsArrRefCodeCol;
    itsOffsetCol = that.itsOffsetCol;
    itsArrOffsetCol = that.itsArrOffsetCol;
    itsMeasRef = that.itsMeasRef;
    if (itsDataCol != 0) {
	itsDataCol = new ROArrayColumn<Double>(*itsDataCol);
    }
    if (itsRefCodeCol != 0) {
	itsRefCodeCol = new ROScalarColumn<Int>(*itsRefCodeCol);
    }
    if (itsArrRefCodeCol != 0) {
	itsArrRefCodeCol = new ROArrayColumn<Int>(*itsArrRefCodeCol);
    }
    if (itsOffsetCol != 0) {
	itsOffsetCol = new ROScalarMeasColumn<M, MV>(*itsOffsetCol);
    }
    if (itsArrOffsetCol != 0) {
	itsArrOffsetCol = new ROArrayMeasColumn<M, MV>(*itsArrOffsetCol);
    }
}

template<class M, class MV>
void ROArrayMeasColumn<M, MV>::attach(const Table& tab, 
	    	    	    	      const String& columnName)
{
    reference(ROArrayMeasColumn<M, MV>(tab, columnName)); 
}
 
template<class M, class MV>
void ROArrayMeasColumn<M, MV>::get(uInt rownr, Array<M>& meas,
	    	    	    	   Bool resize) const
{
    // This will fail if array in rownr is undefined.
    Array<Double> tmpDataCol((*itsDataCol)(rownr));
    Bool deleteData;
    const Double *d_p = tmpDataCol.getStorage(deleteData);

    // The last component of the shape of tmpDataCol is the length of the 
    // MeasValue vector.
    IPosition shp(tmpDataCol.shape());
    const uInt mvlen = (shp.asVector())(shp.nelements() - 1);

    // The dimensionality of the resulting Array<Measure> is one less than the
    // dimensionality of the data column array.
    shp.resize(tmpDataCol.ndim() - 1);        
    if (!shp.isEqual(meas.shape())) {
	if (resize || meas.nelements() == 0) {
	    meas.resize(shp);
	} else {
	    throw(TableArrayConformanceError("ArrayQuantColumn::get"));
	}
    }
    
    Bool deleteMeas;
    M* meas_p = meas.getStorage(deleteMeas);

    MeasRef<M> locMRef = itsMeasRef;
    Bool refPerElem = (itsArrRefCodeCol != 0) ? True : False;
    Array<Int> refArr;
    const Int *r_p;
    Bool deleteRef;
    if (refPerElem) {
	refArr((*itsArrRefCodeCol)(rownr));
	r_p = refArr.getStorage(deleteRef);
    } else {
	if (itsRefCodeCol != 0) {
	    locMRef.set((*itsRefCodeCol)(rownr));
	}
    }
    
    // same for the offset component of the MeasRef
    Bool offsetPerElem = (itsArrOffsetCol != 0) ? True : False;
    Array<M> offsetArr;
    const M *os_p;
    Bool deleteOffset;
    if (offsetPerElem) {
//	offsetArr.reference((*itsArrOffsetCol)(rownr));
	itsArrOffsetCol->get(rownr, offsetArr, True);
	os_p = offsetArr.getStorage(deleteOffset);
    } else {
	if (itsOffsetCol != 0) {
	    locMRef.set((*itsOffsetCol)(rownr));
	}
    }
    
    uInt n = meas.nelements();
    Vector<Double> tmpVec(mvlen);
    MV measVal;
    for (uInt i=0; i<n; i++) {
	// first the data component of the measure
	for (uInt j=0; j<mvlen; j++) {
	    tmpVec(j) = *(d_p + j);
	}
	measVal.putVector(tmpVec);
	// now the reference and offset
	if (refPerElem) {
	    locMRef.set(*(r_p+i));
	}
	if (offsetPerElem) {
	    locMRef.set(*(os_p+i));
	}
	(meas_p+i)->set(measVal, locMRef);
	d_p += mvlen;
    }    
    d_p = d_p - (n * mvlen);

    tmpDataCol.freeStorage(d_p, deleteData);
    meas.putStorage(meas_p, deleteMeas);
    if (refPerElem) {
    	refArr.freeStorage(r_p, deleteRef);
    }
    if (offsetPerElem) {
    	offsetArr.freeStorage(os_p, deleteOffset);
    }
    
#ifdef COMMENT        
    String* u_p;
    Bool deleteUnits;
    Array<String> tmpUnitsCol;
    Unit localUnit;
    if (itsArrUnitsCol != 0) {
	tmpUnitsCol = (*itsArrUnitsCol)(rownr);
	u_p = tmpUnitsCol.getStorage(deleteUnits);
    } else if (itsScaUnitsCol != 0) {
	localUnit.setName((*itsScaUnitsCol)(rownr));
    } else {
	localUnit = itsUnit;
    }
    
    uInt n = tmpDataCol.nelements();
    for (uInt i=0; i<n; i++) {
    	(q_p+i)->setValue(*(d_p+i));
	
	if (itsArrUnitsCol != 0) {
	    (q_p+i)->setUnit(*(u_p+i));
	} else {
	    (q_p+i)->setUnit(localUnit);
	}
    }
#endif
}
    	
template<class M, class MV> 
Array<M> ROArrayMeasColumn<M, MV>::operator()(uInt rownr) const
{
    Array<M> meas;
    get(rownr, meas);
    return meas;
}

template<class M, class MV>
const MeasRef<M>& ROArrayMeasColumn<M, MV>::getRef() const
{
    return itsMeasRef;
}

template<class M, class MV>
void ROArrayMeasColumn<M, MV>::throwIfNull() const
{
    if (isNull()) {
        throw (TableInvOper("Measure table column is null"));
    }
}
 
template<class M, class MV>
ArrayMeasColumn<M, MV>::ArrayMeasColumn()
: ROArrayMeasColumn<M, MV>(),
  itsDataCol(0),
  itsRefCodeCol(0),
  itsArrRefCodeCol(0),
  itsOffsetCol(0),
  itsArrOffsetCol(0)
{}

template<class M, class MV>
ArrayMeasColumn<M, MV>::ArrayMeasColumn(const Table& tab,
					const String& columnName)
: ROArrayMeasColumn<M, MV>(tab, columnName),
  itsDataCol(0),
  itsRefCodeCol(0),
  itsArrRefCodeCol(0),
  itsOffsetCol(0),
  itsArrOffsetCol(0)
{
    TableMeasDesc<M>* tmDesc = (TableMeasDesc<M>*) 
	TableMeasDescBase::reconstruct(tab, columnName);
    
    AlwaysAssert(M::showMe() == tmDesc->type(), AipsError);
    itsDataCol = new ArrayColumn<Double>(tab, columnName);
    
    // Set up the reference code component of the MeasRef
    if (tmDesc->isRefCodeVariable()) {
	const String varColName = tmDesc->refColumnName();
	if (tab.tableDesc().columnDesc(varColName).isScalar()) {
	    itsRefCodeCol = new ScalarColumn<Int>(tab, varColName);	
	} else {
	    itsArrRefCodeCol = new ArrayColumn<Int>(tab, varColName);
    	}
    } else {
	itsMeasRef.set(tmDesc->getRefCode());
    }
    
    // Set up the offset component of the MeasRef
    if (tmDesc->hasOffset()) {
	if (tmDesc->isOffsetVariable()) {
	    const String varColName = tmDesc->offsetColumnName();
	    if (tab.tableDesc().columnDesc(varColName).isScalar()) {
		itsOffsetCol = new ScalarMeasColumn<M, MV>(tab, varColName);
	    } else {
		itsArrOffsetCol = 
		  new ArrayMeasColumn<M, MV>(tab, varColName);
    	    }
	} else {
	    itsMeasRef.set(tmDesc->getOffset());
	}
    } 
    delete tmDesc;
}

template<class M, class MV>
ArrayMeasColumn<M, MV>::ArrayMeasColumn(const ArrayMeasColumn<M, MV>& that)
: ROArrayMeasColumn<M, MV>(that),
  itsDataCol(that.itsDataCol),
  itsRefCodeCol(that.itsRefCodeCol),
  itsArrRefCodeCol(that.itsArrRefCodeCol),
  itsOffsetCol(that.itsOffsetCol),
  itsArrOffsetCol(that.itsArrOffsetCol),
  itsMeasRef(that.itsMeasRef)
{
    if (itsDataCol != 0) {
	itsDataCol = new ArrayColumn<Double>(*itsDataCol);
    }
    if (itsRefCodeCol != 0) {
	itsRefCodeCol = new ScalarColumn<Int>(*itsRefCodeCol);
    }
    if (itsArrRefCodeCol != 0) {
	itsArrRefCodeCol = new ArrayColumn<Int>(*itsArrRefCodeCol);
    }
    if (itsArrOffsetCol != 0) {
	itsArrOffsetCol = new ArrayMeasColumn<M, MV>(*itsArrOffsetCol);
    }
}

template<class M, class MV>
ArrayMeasColumn<M, MV>::~ArrayMeasColumn()
{}

template<class M, class MV>
void ArrayMeasColumn<M, MV>::cleanUp()
{
    delete itsDataCol;
    delete itsRefCodeCol;
    delete itsArrRefCodeCol;
    delete itsOffsetCol;
    delete itsArrOffsetCol;
}

template<class M, class MV>
void ArrayMeasColumn<M, MV>::reference(const ArrayMeasColumn<M, MV>& that)
{
    ROArrayMeasColumn<M, MV>::reference(that);
    itsDataCol = that.itsDataCol;
    itsRefCodeCol = that.itsRefCodeCol;
    itsArrRefCodeCol = that.itsArrRefCodeCol;
    itsOffsetCol = that.itsOffsetCol;
    itsArrOffsetCol = that.itsArrOffsetCol;
    itsMeasRef = that.itsMeasRef;
    if (itsDataCol != 0) {
	itsDataCol = new ArrayColumn<Double>(*itsDataCol);
    }
    if (itsRefCodeCol != 0) {
	itsRefCodeCol = new ScalarColumn<Int>(*itsRefCodeCol);
    }
    if (itsArrRefCodeCol != 0) {
	itsArrRefCodeCol = new ArrayColumn<Int>(*itsArrRefCodeCol);
    }
    if (itsArrOffsetCol != 0) {
	itsArrOffsetCol = new ArrayMeasColumn<M, MV>(*itsArrOffsetCol);
    }
}

template<class M, class MV>
void ArrayMeasColumn<M, MV>::attach(const Table& tab, 
				    const String& columnName)
{
    reference(ArrayMeasColumn<M, MV>(tab, columnName)); 
}
 
template<class M, class MV>
void ArrayMeasColumn<M, MV>::put(uInt rownr, const Array<M>& meas)
{
#ifdef COMMENT
    // probably need to check type etc for this measure
    // break the M up into its value and reference
    Vector<Double> val = meas.getValue().get();
    itsDataCol->put(rownr, val);
    cout << "Val to write is: " << val << endl; 
      
    // Each quantum in q is separated out into its Qtype component and
    // Unit component which are stored in itsDataCol and, if Units are
    // variable, itsArrUnitsCol repsectively.  If units are not variable
    // each quantum is first converted into local units before it is
    // saved.  
    // getStorage() is used to create pointers (to be used as iterators)
    // into the respective arrays.
    // if q is empty we don't want to do anything
#endif
    
    // If meas has entries then need to resize the dataColArr to conform
    // to meas.Shape() + one dimension for storing the measure's values.
    
    const uInt n = meas.nelements();
    cout << "elements: " << n << endl;
    if (n == 0) {
	return;
    }
    IPosition mIndx(meas.shape());
    mIndx = 0;
    cout << "ndim(): " << meas.ndim() << endl;
    const uInt mvlen = meas(mIndx).getValue().getVector().nelements();
    
    IPosition ms(meas.shape());
    ms.append(IPosition(1, mvlen));
    cout << "new ms shape: " << ms << endl;
    Bool deleteData;
    Array<Double> dataArr(ms);
    Double *d_p = dataArr.getStorage(deleteData);
    
    cout << "array.nelements(): " << dataArr.nelements() << endl;
    
    Bool deleteMeas;
    const M* meas_p = meas.getStorage(deleteMeas);

    // Set up put for reference component of measure.  Three possibilities:
    //     1. The reference is non-variable so ignore references.
    //     2. The reference varies per row so write reference once using
    //        ref from first meas in the array.
    //     3. Ref varies per element of array.  This requires a reference
    //        array which is eventually put into itsArrRefCodeCol.
    
    Bool refPerElem = (itsArrRefCodeCol != 0) ? True : False;
    Array<Int> refArr;
    Int *r_p;
    Bool deleteRef;
    if (refPerElem) {
	refArr(meas.shape());
	r_p = refArr.getStorage(deleteRef);
    } else {
	if (itsRefCodeCol != 0) {
	    // references are variable but only per row.
	    itsRefCodeCol->put(rownr, meas_p->getRef().getType());
	}
    }
    
    // Same again for offset.
    
    Bool offsetPerElem = (itsArrOffsetCol != 0) ? True : False;
    Array<M> offsetArr;
    M *os_p;
    Bool deleteOffset;
    if (offsetPerElem) {
	offsetArr(meas.shape());
	os_p = offsetArr.getStorage(deleteOffset);
    } else {
	if (itsOffsetCol != 0) {
	    // offsets are variable but only per row.
	    itsOffsetCol->put(rownr, meas_p->getRef().offset());
	}
    }
    
    Vector<Double> tmpVec;
    M offset;
    for (uInt i=0; i<n; i++) {
	tmpVec = (meas_p+i)->getValue().getVector();
	for (uInt j=0; j<mvlen; j++)
	    *(d_p+j) = tmpVec(j);
	d_p += mvlen;
	if (refPerElem) {
	    *(r_p+i) = (meas_p+i)->getRef().getType();
	}
	if (offsetPerElem) {
	    offset = (meas_p+i)->getRef().offset();
	    *(os_p+i) = offset;
	}
    }
    d_p = d_p - (n * mvlen);
    
    // update the real columns.
    dataArr.putStorage(d_p, deleteData);
    itsDataCol->put(rownr, dataArr);
    meas.freeStorage(meas_p, deleteMeas);
    if (refPerElem) {
    	refArr.putStorage(r_p, deleteRef);
    	itsArrRefCodeCol->put(rownr, refArr);
    }
    if (offsetPerElem) {
	offsetArr.putStorage(os_p, deleteOffset);
	itsArrOffsetCol->put(rownr, offsetArr);
    }
    
#ifdef COMMENT
    
    // If units are variable they could vary per element of the quantum
    // array (i.e., the units column is also an array) or they could vary
    // by row (i.e., the units column is a Scalar Column where each row 
    // contains a single unit entry).  When variable by row, the unit of the
    // first quantum in q is used.
    Bool deleteUnits;
    String* u_p;
    Array<String> unitsArr;
    Unit localUnit;
    if (itsArrUnitsCol != 0) {
	unitsArr.resize(meas.shape());
	u_p = unitsArr.getStorage(deleteUnits);
    } else if (itsScaUnitsCol != 0) {
	// just takes the value for unit from the first entry in q.  This
	// is safe because we know here that q contains at least 1 entry.
	localUnit = q_p->getFullUnit();
	itsScaUnitsCol->put(rownr, localUnit.getName());
    } else {
	localUnit = itsUnit;
    }
            
    for (uInt i=0; i<n; i++) {
//	if (itsArrUnitsCol != 0) {
//	    *(u_p+i) = (meas_p+i)->getFullUnit().getName();
	    *(d_p+i) = (meas_p+i)->getValue().getVector();
//	} else {
	    // ensure quantum's value is consistent with column's units
	    // before storing.
//	    itsQuantum = *(meas_p+i);
//	    itsQuantum.convert(localUnit);
//	    *(d_p+i) = itsQuantum.getValue();
//	}
    }
#endif

//#ifdef COMMENT    
//    if (itsArrUnitsCol != 0) {
//	unitsArr.putStorage(u_p, deleteUnits);
//	itsArrUnitsCol->put(rownr, unitsArr);
//    }
    
}
    	
