//# Copyright (C) 1997,1998,1999
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
#include <aips/Measures/MBaseline.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MDoppler.h>
#include <aips/Measures/MEarthMagnetic.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MRadialVelocity.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Measures/MeasFrame.h>
#include <aips/Measures/MeasRef.h>
#include <aips/Measures/Muvw.h>
#include <aips/Quanta/MeasValue.h>
#include <trial/TableMeasures/ScalarMeasColumn.h>
#include <trial/TableMeasures/TableMeasDesc.h>
#include <trial/TableMeasures/TableMeasOffsetDesc.h>
#include <trial/TableMeasures/TableMeasRefDesc.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/ColumnDesc.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableError.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>

template<class M, class MV>
ROScalarMeasColumn<M, MV>::ROScalarMeasColumn()
: itsDataCol(0),
  itsRefIntCol(0),
  itsRefStrCol(0),
  itsOffsetCol(0)
{}

template<class M, class MV>
ROScalarMeasColumn<M, MV>::ROScalarMeasColumn(const Table& tab,
					      const String& columnName)
: itsDataCol(0),
  itsRefIntCol(0),
  itsRefStrCol(0),
  itsOffsetCol(0)
{
    TableMeasDesc<M>* tmDesc = (TableMeasDesc<M>*) 
	TableMeasDescBase::reconstruct(tab, columnName);
	
    AlwaysAssert(M::showMe() == tmDesc->type(), AipsError);

    // create the data column    
    itsDataCol = new ROArrayColumn<Double>(tab, columnName);
    
    // Set up the reference code component of the MeasRef
    if (tmDesc->isRefCodeVariable()) {
	const String &refColName = tmDesc->refColumnName();
	if ((tab.tableDesc().columnDesc(refColName).dataType() == TpString)) {
	    itsRefStrCol = new ROScalarColumn<String>(tab, refColName);
	} else {
	    itsRefIntCol = new ROScalarColumn<Int>(tab, refColName);
	}
	itsVarRefFlag = True;
    } else {
	itsMeasRef.set(tmDesc->getRefCode());
	itsVarRefFlag = False;
    }
    
    // Set up the offset component of the MeasRef
    if (tmDesc->hasOffset()) {
	if (tmDesc->isOffsetVariable()) {
	    if (tmDesc->isOffsetArray()) {
		throw(AipsError("ROScalarMeasColumn::ROScalarMeasColumn "
				"Offset column must be a ScalarMeasColumn."));
	    } else {
		itsOffsetCol = 
		    new ROScalarMeasColumn<M, MV>(tab,
						  tmDesc->offsetColumnName());
	    }
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
  itsRefIntCol(that.itsRefIntCol),
  itsRefStrCol(that.itsRefStrCol),
  itsOffsetCol(that.itsOffsetCol)
{
    if (itsDataCol != 0) {
	itsDataCol = new ROArrayColumn<Double>(*itsDataCol);
    }
    if (itsRefIntCol != 0) {
	itsRefIntCol = new ROScalarColumn<Int>(*itsRefIntCol);
    }
    if (itsRefStrCol != 0) {
	itsRefStrCol = new ROScalarColumn<String>(*itsRefStrCol);
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
    delete itsRefIntCol;
    delete itsRefStrCol;
    delete itsOffsetCol;
}

template<class M, class MV>
void ROScalarMeasColumn<M, MV>::reference(
    const ROScalarMeasColumn<M, MV>& that)
{   
    cleanUp();
    itsDataCol = that.itsDataCol;
    itsVarRefFlag = that.itsVarRefFlag;
    itsRefIntCol = that.itsRefIntCol;
    itsRefStrCol = that.itsRefStrCol;
    itsOffsetCol = that.itsOffsetCol;
    itsMeasRef = that.itsMeasRef;
    if (itsDataCol != 0) {
	itsDataCol = new ROArrayColumn<Double>(*itsDataCol);
    }
    if (itsRefIntCol != 0) {
	itsRefIntCol = new ROScalarColumn<Int>(*itsRefIntCol);
    }
    if (itsRefStrCol != 0) {
	itsRefStrCol = new ROScalarColumn<String>(*itsRefStrCol);
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
Bool ROScalarMeasColumn<M, MV>::isDefined(uInt rownr) const 
{ 
    return itsDataCol->isDefined(rownr); 
}

template<class M, class MV>
const MeasRef<M>& ROScalarMeasColumn<M, MV>::getMeasRef() const
{
    return itsMeasRef;
}

template<class M, class MV>
void ROScalarMeasColumn<M, MV>::setMeasRef(uInt rownr) const
{
    if (itsVarRefFlag) {
	if (itsRefStrCol != 0) {
	    M meas;
	    meas.giveMe(itsMeasRef, (*itsRefStrCol)(rownr));
	} else {
	    itsMeasRef.set((*itsRefIntCol)(rownr));
	}
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
  itsRefIntCol(0),
  itsRefStrCol(0),
  itsOffsetCol(0)
{}

template<class M, class MV>
ScalarMeasColumn<M, MV>::ScalarMeasColumn(const Table& tab, 
	    	    	    	    	  const String& columnName)
: ROScalarMeasColumn<M, MV>(tab, columnName),
  itsDataCol(0),
  itsRefIntCol(0),
  itsRefStrCol(0),
  itsOffsetCol(0)
{
    TableMeasDesc<M>* tmDesc = (TableMeasDesc<M>*) 
	TableMeasDescBase::reconstruct(tab, columnName);
	
    AlwaysAssert(M::showMe() == tmDesc->type(), AipsError);
        
    itsDataCol = new ArrayColumn<Double>(tab, columnName);
    
    // Set up the reference code component of the MeasRef
    if (tmDesc->isRefCodeVariable()) {
	const String &refColName = tmDesc->refColumnName();
	if ((tab.tableDesc().columnDesc(refColName).dataType() == TpString)) {
	    itsRefStrCol = new ScalarColumn<String>(tab, refColName);
	} else {
	    itsRefIntCol = new ScalarColumn<Int>(tab, refColName);
	}
    }
    
    // Set up the offset component of the MeasRef
    if (tmDesc->hasOffset()) {
	if (tmDesc->isOffsetVariable()) {
	    // don't need to test whether the offsetColumn isArray() as this
	    // this is done in the RO class.
	    itsOffsetCol = 
		new ScalarMeasColumn<M, MV>(tab, tmDesc->offsetColumnName());
	}
    } 
    delete tmDesc;
}

template<class M, class MV>
ScalarMeasColumn<M, MV>::ScalarMeasColumn(const ScalarMeasColumn<M, MV>& that)
: ROScalarMeasColumn<M, MV>(that),
  itsDataCol(that.itsDataCol),
  itsRefIntCol(that.itsRefIntCol),
  itsRefStrCol(that.itsRefStrCol),
  itsOffsetCol(that.itsOffsetCol)
{
    if (itsDataCol != 0) {
	itsDataCol = new ArrayColumn<Double>(*itsDataCol);
    }
    if (itsRefIntCol != 0) {
	itsRefIntCol = new ScalarColumn<Int>(*itsRefIntCol);
    }
    if (itsRefStrCol != 0) {
	itsRefStrCol = new ScalarColumn<String>(*itsRefStrCol);
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
    delete itsRefIntCol;
    delete itsRefStrCol;
    delete itsOffsetCol;
}

template<class M, class MV>
void ScalarMeasColumn<M, MV>::reference(const ScalarMeasColumn<M, MV>& that)
{
    ROScalarMeasColumn<M, MV>::reference(that);
    itsDataCol = that.itsDataCol;
    itsRefIntCol = that.itsRefIntCol;
    itsRefStrCol = that.itsRefStrCol;
    itsOffsetCol = that.itsOffsetCol;
    if (itsDataCol != 0) {
	itsDataCol = new ArrayColumn<Double>(*itsDataCol);
    }
    if (itsRefIntCol != 0) {
	itsRefIntCol = new ScalarColumn<Int>(*itsRefIntCol);
    }
    if (itsRefStrCol != 0) {
	itsRefStrCol = new ScalarColumn<String>(*itsRefStrCol);
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
        
    if (itsVarRefFlag) {
	if (itsRefStrCol != 0) {
	    itsRefStrCol->put(rownr, M::showType(meas.getRef().getType()));
	} else {
	    itsRefIntCol->put(rownr, meas.getRef().getType());
	}
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
    	
