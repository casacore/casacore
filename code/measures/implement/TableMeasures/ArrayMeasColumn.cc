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
#include <aips/Measures/MBaseline.h>
#include <aips/Measures/Muvw.h>
#include <aips/Measures/MEarthMagnetic.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/ColumnDesc.h>
#include <aips/Tables/TableError.h>
#include <aips/Utilities/Assert.h>

template<class M, class MV>
ROArrayMeasColumn<M, MV>::ROArrayMeasColumn()
: itsDataCol(0),
  itsRefIntCol(0),
  itsArrRefIntCol(0),
  itsRefStrCol(0),
  itsArrRefStrCol(0),
  itsOffsetCol(0),
  itsArrOffsetCol(0)
{}

template<class M, class MV>
ROArrayMeasColumn<M, MV>::ROArrayMeasColumn(const Table& tab,
					    const String& columnName)
: itsDataCol(0),
  itsRefIntCol(0),
  itsArrRefIntCol(0),
  itsRefStrCol(0),
  itsArrRefStrCol(0),
  itsOffsetCol(0),
  itsArrOffsetCol(0)
{
    TableMeasDesc<M>* tmDesc = (TableMeasDesc<M>*) 
	TableMeasDescBase::reconstruct(tab, columnName);
    
    AlwaysAssert(M::showMe() == tmDesc->type(), AipsError);
    itsDataCol = new ROArrayColumn<Double>(tab, columnName);
    
    // Set up the reference code component of the MeasRef. It can be variable
    // and therefore stored in a column which may be either an array or scalar
    // column. Additionally, the references may be stored as strings or
    // refcodes.
    if (tmDesc->isRefCodeVariable()) {
	const String &rcName = tmDesc->refColumnName();
	const ColumnDesc &cd = tab.tableDesc().columnDesc(rcName);
	if (cd.isScalar()) {
	    if (cd.dataType() == TpString) {
		itsRefStrCol = new ROScalarColumn<String>(tab, rcName);
	    } else {
		itsRefIntCol = new ROScalarColumn<Int>(tab, rcName);
	    }
	} else {
	    if (cd.dataType() == TpString) {
		itsArrRefStrCol = new ROArrayColumn<String>(tab, rcName);
	    } else {
		itsArrRefIntCol = new ROArrayColumn<Int>(tab, rcName);
	    }
    	}
	itsVarRefFlag = True;
    } else {
	itsMeasRef.set(tmDesc->getRefCode());
	itsVarRefFlag = False;
    }
    
    // Set up the offset component of the MeasRef
    if (tmDesc->hasOffset()) {
	if (tmDesc->isOffsetVariable()) {
	    const String refColName = tmDesc->offsetColumnName();
	    if (tab.tableDesc().columnDesc(refColName).isScalar()) {
		itsOffsetCol = new ROScalarMeasColumn<M, MV>(tab, refColName);
	    } else {
		itsArrOffsetCol = 
		  new ROArrayMeasColumn<M, MV>(tab, refColName);
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
  itsRefIntCol(that.itsRefIntCol),
  itsArrRefIntCol(that.itsArrRefIntCol),
  itsRefStrCol(that.itsRefStrCol),
  itsArrRefStrCol(that.itsArrRefStrCol),
  itsOffsetCol(that.itsOffsetCol),
  itsArrOffsetCol(that.itsArrOffsetCol),
  itsMeasRef(that.itsMeasRef)
{
    if (itsDataCol != 0) {
	itsDataCol = new ROArrayColumn<Double>(*itsDataCol);
    }
    if (itsRefIntCol != 0) {
	itsRefIntCol = new ROScalarColumn<Int>(*itsRefIntCol);
    }
    if (itsArrRefIntCol != 0) {
	itsArrRefIntCol = new ROArrayColumn<Int>(*itsArrRefIntCol);
    }
    if (itsRefStrCol != 0) {
	itsRefStrCol = new ROScalarColumn<String>(*itsRefStrCol);
    }
    if (itsArrRefStrCol != 0) {
	itsArrRefStrCol = new ROArrayColumn<String>(*itsArrRefStrCol);
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
    delete itsRefIntCol;
    delete itsArrRefIntCol;
    delete itsRefStrCol;
    delete itsArrRefStrCol;
    delete itsOffsetCol;
    delete itsArrOffsetCol;
}

template<class M, class MV>
void ROArrayMeasColumn<M, MV>::reference(const ROArrayMeasColumn<M, MV>& that)
{   
    cleanUp();
    itsDataCol = that.itsDataCol;
    itsVarRefFlag = that.itsVarRefFlag;
    itsRefIntCol = that.itsRefIntCol;
    itsArrRefIntCol = that.itsArrRefIntCol;
    itsRefStrCol = that.itsRefStrCol;
    itsArrRefStrCol = that.itsArrRefStrCol;
    itsOffsetCol = that.itsOffsetCol;
    itsArrOffsetCol = that.itsArrOffsetCol;
    itsMeasRef = that.itsMeasRef;
    if (itsDataCol != 0) {
	itsDataCol = new ROArrayColumn<Double>(*itsDataCol);
    }
    if (itsRefIntCol != 0) {
	itsRefIntCol = new ROScalarColumn<Int>(*itsRefIntCol);
    }
    if (itsArrRefIntCol != 0) {
	itsArrRefIntCol = new ROArrayColumn<Int>(*itsArrRefIntCol);
    }
    if (itsRefStrCol != 0) {
	itsRefStrCol = new ROScalarColumn<String>(*itsRefStrCol);
    }
    if (itsArrRefStrCol != 0) {
	itsArrRefStrCol = new ROArrayColumn<String>(*itsArrRefStrCol);
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

    // Set up get for reference component of measure.  Three possibilities:
    //   1. A column reference is used.
    //   2. The reference varies per row and is stored in a ScalarColumn.
    //   3. Ref varies per element of array (hence stored in an ArrayColumn).
    // With options 2 and 3 the reference could be either stored as a string 
    // or int.
    
    MeasRef<M> locMRef = itsMeasRef;
    Bool refPerElem = (itsArrRefIntCol != 0) || (itsArrRefStrCol != 0)
	? True : False;
    Bool strRefs = (itsArrRefStrCol != 0) ? True : False;
    Array<Int> intRefArr;
    Array<String> strRefArr;
    const Int *r_p;
    const String *sr_p;
    Bool deleteRef;
    if (refPerElem) {
	if (strRefs) {
	    Array<String> tmp((*itsArrRefStrCol)(rownr));
	    strRefArr.reference(tmp);
	    sr_p = strRefArr.getStorage(deleteRef);
	} else {
	    Array<Int> tmp((*itsArrRefIntCol)(rownr));
	    intRefArr.reference(tmp);
	    r_p = intRefArr.getStorage(deleteRef);
	}
    } else {
	if (itsRefIntCol != 0) {
	    locMRef.set((*itsRefIntCol)(rownr));
	} else if (itsRefStrCol != 0) {
	    meas_p->giveMe(locMRef, (*itsRefStrCol)(rownr));
	}
    }
    
    // Setup for offset component of MeasRef.
    Bool offsetPerElem = (itsArrOffsetCol != 0) ? True : False;
    Array<M> offsetArr;
    const M *os_p;
    Bool deleteOffset;
    if (offsetPerElem) {
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
	// get the data component of the measure
	for (uInt j=0; j<mvlen; j++) {
	    tmpVec(j) = *(d_p + j);
	}
	measVal.putVector(tmpVec);
	// the reference
	if (refPerElem) {
	    if (strRefs) {
		(meas_p+1)->giveMe(locMRef, *(sr_p+i));
	    } else {
		locMRef.set(*(r_p+i));
	    }
	}
	// the offset
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
	if (strRefs) {
	    strRefArr.freeStorage(sr_p, deleteRef);
	} else {
	    intRefArr.freeStorage(r_p, deleteRef);
	}
    }
    if (offsetPerElem) {
    	offsetArr.freeStorage(os_p, deleteOffset);
    }
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
  itsRefIntCol(0),
  itsArrRefIntCol(0),
  itsRefStrCol(0),
  itsArrRefStrCol(0),
  itsOffsetCol(0),
  itsArrOffsetCol(0)
{}

template<class M, class MV>
ArrayMeasColumn<M, MV>::ArrayMeasColumn(const Table& tab,
					const String& columnName)
: ROArrayMeasColumn<M, MV>(tab, columnName),
  itsDataCol(0),
  itsRefIntCol(0),
  itsArrRefIntCol(0),
  itsRefStrCol(0),
  itsArrRefStrCol(0),
  itsOffsetCol(0),
  itsArrOffsetCol(0)
{
    TableMeasDesc<M>* tmDesc = (TableMeasDesc<M>*) 
	TableMeasDescBase::reconstruct(tab, columnName);
    
    AlwaysAssert(M::showMe() == tmDesc->type(), AipsError);
    itsDataCol = new ArrayColumn<Double>(tab, columnName);
    
    // Set up the reference code component of the MeasRef
    if (tmDesc->isRefCodeVariable()) {
	const String rcName = tmDesc->refColumnName();
	const ColumnDesc &cd = tab.tableDesc().columnDesc(rcName);
	if (cd.isScalar()) {
	    if (cd.dataType() == TpString) {
		itsRefStrCol = new ScalarColumn<String>(tab, rcName);
	    } else {
		itsRefIntCol = new ScalarColumn<Int>(tab, rcName);
	    }
	} else {
	    if (cd.dataType() == TpString) {
		itsArrRefStrCol = new ArrayColumn<String>(tab, rcName);
	    } else {
		itsArrRefIntCol = new ArrayColumn<Int>(tab, rcName);
	    }
    	}
    } 
    
    // Set up the offset component of the MeasRef
    if (tmDesc->hasOffset()) {
	if (tmDesc->isOffsetVariable()) {
	    const String refColName = tmDesc->offsetColumnName();
	    if (tab.tableDesc().columnDesc(refColName).isScalar()) {
		itsOffsetCol = new ScalarMeasColumn<M, MV>(tab, refColName);
	    } else {
		itsArrOffsetCol = 
		  new ArrayMeasColumn<M, MV>(tab, refColName);
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
  itsRefIntCol(that.itsRefIntCol),
  itsArrRefIntCol(that.itsArrRefIntCol),
  itsRefStrCol(that.itsRefStrCol),
  itsArrRefStrCol(that.itsArrRefStrCol),
  itsOffsetCol(that.itsOffsetCol),
  itsArrOffsetCol(that.itsArrOffsetCol),
  itsMeasRef(that.itsMeasRef)
{
    if (itsDataCol != 0) {
	itsDataCol = new ArrayColumn<Double>(*itsDataCol);
    }
    if (itsRefIntCol != 0) {
	itsRefIntCol = new ScalarColumn<Int>(*itsRefIntCol);
    }
    if (itsArrRefIntCol != 0) {
	itsArrRefIntCol = new ArrayColumn<Int>(*itsArrRefIntCol);
    }
    if (itsRefStrCol != 0) {
	itsRefStrCol = new ScalarColumn<String>(*itsRefStrCol);
    }
    if (itsArrRefStrCol != 0) {
	itsArrRefStrCol = new ArrayColumn<String>(*itsArrRefStrCol);
    }
    if (itsArrOffsetCol != 0) {
	itsArrOffsetCol = new ArrayMeasColumn<M, MV>(*itsArrOffsetCol);
    }
}

template<class M, class MV>
ArrayMeasColumn<M, MV>::~ArrayMeasColumn()
{
    cleanUp();
}

template<class M, class MV>
void ArrayMeasColumn<M, MV>::cleanUp()
{
    delete itsDataCol;
    delete itsRefIntCol;
    delete itsArrRefIntCol;
    delete itsRefStrCol;
    delete itsArrRefStrCol;
    delete itsOffsetCol;
    delete itsArrOffsetCol;
}

template<class M, class MV>
void ArrayMeasColumn<M, MV>::reference(const ArrayMeasColumn<M, MV>& that)
{
    ROArrayMeasColumn<M, MV>::reference(that);
    itsDataCol = that.itsDataCol;
    itsRefIntCol = that.itsRefIntCol;
    itsArrRefIntCol = that.itsArrRefIntCol;
    itsRefStrCol = that.itsRefStrCol;
    itsArrRefStrCol = that.itsArrRefStrCol;
    itsOffsetCol = that.itsOffsetCol;
    itsArrOffsetCol = that.itsArrOffsetCol;
    itsMeasRef = that.itsMeasRef;
    if (itsDataCol != 0) {
	itsDataCol = new ArrayColumn<Double>(*itsDataCol);
    }
    if (itsRefIntCol != 0) {
	itsRefIntCol = new ScalarColumn<Int>(*itsRefIntCol);
    }
    if (itsArrRefIntCol != 0) {
	itsArrRefIntCol = new ArrayColumn<Int>(*itsArrRefIntCol);
    }
    if (itsRefStrCol != 0) {
	itsRefStrCol = new ScalarColumn<String>(*itsRefStrCol);
    }
    if (itsArrRefStrCol != 0) {
	itsArrRefStrCol = new ArrayColumn<String>(*itsArrRefStrCol);
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
    // If meas has entries then need to resize the dataColArr to conform
    // to meas.Shape() + one dimension for storing the measure's values.
    
    const uInt n = meas.nelements();
    if (n == 0) {
	return;
    }
    IPosition mIndx(meas.shape());
    mIndx = 0;
    const uInt mvlen = meas(mIndx).getValue().getVector().nelements();
    
    IPosition ms(meas.shape());
    ms.append(IPosition(1, mvlen));
    Bool deleteData;
    Array<Double> dataArr(ms);
    Double *d_p = dataArr.getStorage(deleteData);
        
    Bool deleteMeas;
    const M* meas_p = meas.getStorage(deleteMeas);

    // Set up put for reference component of measure.  Three possibilities:
    //   1. The reference is non-variable so ignore reference component.
    //   2. The reference varies per row so write reference once using
    //      reference from first measure in the array.  No check is done
    //	    on the reference component of other measures.
    //   3. Ref varies per element of array. An array of references is written.
    // With 2 and 3 references are stored as either Strings or Ints.
    
    Bool refPerElem = (itsArrRefIntCol != 0) || (itsArrRefStrCol != 0)
	? True : False;
    Bool strRefs = (itsArrRefStrCol != 0) ? True : False;
    Array<Int> intRefArr;
    Array<String> strRefArr;
    Int *r_p;
    String *sr_p;
    Bool deleteRef;
    if (refPerElem) {
	if (strRefs) {
	    strRefArr(meas.shape());
	    sr_p = strRefArr.getStorage(deleteRef);
	} else {
	    intRefArr(meas.shape());
	    r_p = intRefArr.getStorage(deleteRef);
	}
    } else {
	// references are variable but only per row.
	if (itsRefIntCol != 0) {
	    itsRefIntCol->put(rownr, meas_p->getRef().getType());
	} else if (itsRefStrCol != 0) {
	    itsRefStrCol->put(rownr, M::showType(meas_p->getRef().getType()));
	}
    }
    
    // Setup for offset.   
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
	// put the value component
	tmpVec = (meas_p+i)->getValue().getVector();
	for (uInt j=0; j<mvlen; j++)
	    *(d_p+j) = tmpVec(j);
	d_p += mvlen;
	// put the reference
	if (refPerElem) {
	    if (strRefs) {
		*(sr_p+i) = M::showType((meas_p+i)->getRef().getType());
	    } else {
		*(r_p+i) = (meas_p+i)->getRef().getType();
	    }
	}
	// put the offset
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
	if (strRefs) {
	    strRefArr.putStorage(sr_p, deleteRef);
	    itsArrRefStrCol->put(rownr, strRefArr);
	} else {
	    intRefArr.putStorage(r_p, deleteRef);
	    itsArrRefIntCol->put(rownr, intRefArr);
	}
    }
    if (offsetPerElem) {
	offsetArr.putStorage(os_p, deleteOffset);
	itsArrOffsetCol->put(rownr, offsetArr);
    }    
}
    	
