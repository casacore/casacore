//# Copyright (C) 1997
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
#include <aips/Measures/MeasConvert.h>
#include <aips/Measures/MeasFrame.h>
#include <aips/Measures/MeasRef.h>
#include <aips/Measures/MeasValue.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MRadialVelocity.h>
#include <aips/Measures/MDoppler.h>
#include <aips/Measures/MFrequency.h>
#include <trial/TableMeasures/ScalarMeasColumn.h>
#include <trial/TableMeasures/TableMeasDesc.h>
#include <trial/TableMeasures/TableMeasRefDesc.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableError.h>
#include <aips/Tables/ColumnDesc.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>

template<class M, class MV>
ROScalarMeasColumn<M, MV>::ROScalarMeasColumn()
: itsDataCol(0),
  itsRefCodeCol(0),
  itsOffsetCol(0)
{}

template<class M, class MV>
ROScalarMeasColumn<M, MV>::ROScalarMeasColumn(const Table& tab,
					      const String& columnName)
: itsDataCol(0),
  itsRefCodeCol(0),
  itsOffsetCol(0)
{
    TableMeasDesc<M>* tmDesc = (TableMeasDesc<M>*) 
	TableMeasDescBase::reconstruct(tab, columnName);
	
    AlwaysAssert(M::showMe() == tmDesc->type(), AipsError);
        
    if (tab.tableDesc().columnDesc(columnName).isArray()) {
    	itsDataCol = new ROArrayColumn<Double>(tab, columnName);
    } else {
	throw(AipsError(String("The column " + columnName) 
		+ " is not an ArrayColumn."));
    }
    
    // Set up the reference code component of the MeasRef
    if (tmDesc->isRefCodeVariable()) {
	itsRefCodeCol = new ROScalarColumn<Int>(tab, tmDesc->refColumnName());
	itsVarRefFlag = True;
    } else {
	itsRefCodeCol = 0;
	itsMeasRef.set(tmDesc->getRefCode());
	itsVarRefFlag = False;
    }
    
    // Set up the offset component of the MeasRef
    if (tmDesc->hasOffset()) {
	if (tmDesc->isOffsetVariable()) {
	    itsOffsetCol = 
		new ROScalarMeasColumn<M, MV>(tab,tmDesc->offsetColumnName());
	} else {
	    itsMeasRef.set(tmDesc->getOffset());
	}
    } 
    delete tmDesc;
}

template<class M, class MV>
ROScalarMeasColumn<M, MV>::ROScalarMeasColumn(
    const ROScalarMeasColumn<M, MV>& that)
: itsVarRefFlag(that.itsVarRefFlag),
  itsMeasRef(that.itsMeasRef),
  itsDataCol(that.itsDataCol),
  itsRefCodeCol(that.itsRefCodeCol),
  itsOffsetCol(that.itsOffsetCol)
{
    if (itsDataCol != 0) {
	itsDataCol = new ROArrayColumn<Double>(*itsDataCol);
    }
    if (itsRefCodeCol != 0) {
	itsRefCodeCol = new ROScalarColumn<Int>(*itsRefCodeCol);
    }
    if (itsOffsetCol != 0) {
	itsOffsetCol = new ROScalarMeasColumn<M, MV>(*itsOffsetCol);
    }
}

template<class M, class MV>
ROScalarMeasColumn<M, MV>::~ROScalarMeasColumn()
{
    cleanUp();
}

template<class M, class MV>
void ROScalarMeasColumn<M, MV>::cleanUp()
{
    delete itsDataCol;
    delete itsRefCodeCol;
    delete itsOffsetCol;
}

template<class M, class MV>
void ROScalarMeasColumn<M, MV>::reference(
    const ROScalarMeasColumn<M, MV>& that)
{   
    cleanUp();
    itsDataCol = that.itsDataCol;
    itsVarRefFlag = that.itsVarRefFlag;
    itsRefCodeCol = that.itsRefCodeCol;
    itsOffsetCol = that.itsOffsetCol;
    itsMeasRef = that.itsMeasRef;
    if (itsDataCol != 0) {
	itsDataCol = new ROArrayColumn<Double>(*itsDataCol);
    }
    if (itsRefCodeCol != 0) {
	itsRefCodeCol = new ROScalarColumn<Int>(*itsRefCodeCol);
    }
    if (itsOffsetCol != 0) {
	itsOffsetCol = new ROScalarMeasColumn<M, MV>(*itsOffsetCol);
    }
}

template<class M, class MV>
void ROScalarMeasColumn<M, MV>::attach(const Table& tab, 
	    	    	    	       const String& columnName)
{
    reference(ROScalarMeasColumn<M, MV>(tab, columnName)); 
}
    
template<class M, class MV>
void ROScalarMeasColumn<M, MV>::get(uInt rownr, M& meas) const
{
    MV measVal;
    measVal.putVector((*itsDataCol)(rownr));
    if (itsVarRefFlag) {
	setMeasRef(rownr);
    }
    meas.set(measVal, itsMeasRef);
}
    	
template<class M, class MV> 
M ROScalarMeasColumn<M, MV>::operator()(uInt rownr) const
{
    M meas;
    get(rownr, meas);
    return meas;
}

template<class M, class MV>
const MeasRef<M>& ROScalarMeasColumn<M, MV>::getMeasRef() const
{
    return itsMeasRef;
}

template<class M, class MV>
void ROScalarMeasColumn<M, MV>::setMeasRef(uInt rownr) const
{
    if (itsRefCodeCol != 0) {
	itsMeasRef.set((*itsRefCodeCol)(rownr));
    }
    if (itsOffsetCol != 0) {
    	itsMeasRef.set((*itsOffsetCol)(rownr));
    }
}

template<class M, class MV>
void ROScalarMeasColumn<M, MV>::throwIfNull() const
{
    if (isNull()) {
        throw (TableInvOper("Measure table column is null"));
    }
}
 
template<class M, class MV>
ScalarMeasColumn<M, MV>::ScalarMeasColumn()
: ROScalarMeasColumn<M, MV>(),
  itsDataCol(0),
  itsRefCodeCol(0),
  itsOffsetCol(0)
{}

template<class M, class MV>
ScalarMeasColumn<M, MV>::ScalarMeasColumn(const Table& tab, 
	    	    	    	    	  const String& columnName)
: ROScalarMeasColumn<M, MV>(tab, columnName),
  itsDataCol(0),
  itsRefCodeCol(0),
  itsOffsetCol(0)
{
    TableMeasDesc<M>* tmDesc = (TableMeasDesc<M>*) 
	TableMeasDescBase::reconstruct(tab, columnName);
	
    AlwaysAssert(M::showMe() == tmDesc->type(), AipsError);
        
    if (tab.tableDesc().columnDesc(columnName).isArray()) {
    	itsDataCol = new ArrayColumn<Double>(tab, columnName);
    } else {
	throw(AipsError(String("The column " + columnName) 
		+ " is not an ArrayColumn."));
    }
    
    // Set up the reference code component of the MeasRef
    if (tmDesc->isRefCodeVariable()) {
	itsRefCodeCol = new ScalarColumn<Int>(tab, tmDesc->refColumnName());
    } else {
	itsRefCodeCol = 0;
    }
    
    // Set up the offset component of the MeasRef
    if (tmDesc->hasOffset()) {
	if (tmDesc->isOffsetVariable()) {
	    itsOffsetCol = 
		new ScalarMeasColumn<M, MV>(tab,tmDesc->offsetColumnName());
	}
    } 
    delete tmDesc;
}

template<class M, class MV>
ScalarMeasColumn<M, MV>::ScalarMeasColumn(const ScalarMeasColumn<M, MV>& that)
: ROScalarMeasColumn<M, MV>(that),
  itsDataCol(that.itsDataCol),
  itsRefCodeCol(that.itsRefCodeCol),
  itsOffsetCol(that.itsOffsetCol)
{
    if (itsDataCol != 0) {
	itsDataCol = new ArrayColumn<Double>(*itsDataCol);
    }
    if (itsRefCodeCol != 0) {
	itsRefCodeCol = new ScalarColumn<Int>(*itsRefCodeCol);
    }
    if (itsOffsetCol != 0) {
	itsOffsetCol = new ScalarMeasColumn<M, MV>(*itsOffsetCol);
    }
}

template<class M, class MV>
ScalarMeasColumn<M, MV>::~ScalarMeasColumn()
{
    cleanUp();
}

template<class M, class MV>
void ScalarMeasColumn<M, MV>::cleanUp()
{
    delete itsDataCol;
    delete itsRefCodeCol;
    delete itsOffsetCol;
}

template<class M, class MV>
void ScalarMeasColumn<M, MV>::reference(const ScalarMeasColumn<M, MV>& that)
{
    ROScalarMeasColumn<M, MV>::reference(that);
    itsDataCol = that.itsDataCol;
    itsRefCodeCol = that.itsRefCodeCol;
    itsOffsetCol = that.itsOffsetCol;
    if (itsDataCol != 0) {
	itsDataCol = new ArrayColumn<Double>(*itsDataCol);
    }
    if (itsRefCodeCol != 0) {
	itsRefCodeCol = new ScalarColumn<Int>(*itsRefCodeCol);
    }
    if (itsOffsetCol != 0) {
	itsOffsetCol = new ScalarMeasColumn<M, MV>(*itsOffsetCol);
    }
}

template<class M, class MV>
void ScalarMeasColumn<M, MV>::attach(const Table& tab, 
				     const String& columnName)
{
    reference(ScalarMeasColumn<M, MV>(tab, columnName)); 
}
 
template<class M, class MV>
void ScalarMeasColumn<M, MV>::put(uInt rownr, const M& meas)
{
    // When the reference is variable measure's reference and offset
    // are saved as well as the measure's value.
        
    if (itsRefCodeCol != 0) {
	itsRefCodeCol->put(rownr, meas.getRef().getType());
    }
    if (itsOffsetCol != 0) {
	if (meas.getRef().offset() != 0) {
    	    itsOffsetCol->put(rownr, meas.getRef().offset());
	} else {
    	    itsOffsetCol->put(rownr, M());
    	}
    }
    itsDataCol->put(rownr, meas.getValue().getVector());
}
    	
