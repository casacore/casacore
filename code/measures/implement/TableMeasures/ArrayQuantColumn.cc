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
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayIter.h>
#include <aips/Arrays/Vector.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Quanta/Unit.h>
#include <trial/TableMeasures/ArrayQuantColumn.h>
#include <trial/TableMeasures/TableQuantumDesc.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/ColumnDesc.h>
#include <aips/Tables/TableError.h>
#include <aips/Utilities/String.h>

template<class Qtype>
ROArrayQuantColumn<Qtype>::ROArrayQuantColumn()
: itsDataCol(0),
  itsArrUnitsCol(0),
  itsScaUnitsCol(0)
{}

template<class Qtype>
ROArrayQuantColumn<Qtype>::ROArrayQuantColumn(const Table& tab,
				              const String& columnName)
: itsDataCol(0),
  itsArrUnitsCol(0),
  itsScaUnitsCol(0)
{
    TableQuantumDesc* tqDesc =
    	TableQuantumDesc::reconstruct(tab.tableDesc(), columnName);
    if (tqDesc->isUnitVariable()) {
    	itsUnit = "";
	// the variable units column could be either an Array or a Scalar
	String varColName = tqDesc->unitColumnName();
	if (tab.tableDesc().columnDesc(varColName).isScalar()) {
	    itsScaUnitsCol = new ScalarColumn<String>(tab, varColName);	
	} else {
	    itsArrUnitsCol = new ArrayColumn<String>(tab, varColName);
    	}
    } else {
    	itsUnit = tqDesc->getUnits();
	itsArrUnitsCol = 0;
	itsScaUnitsCol = 0;
    }
    itsQuantum = Quantum<Qtype>((Qtype) 0, itsUnit);
    itsDataCol = new ArrayColumn<Qtype>(tab, columnName);
    delete tqDesc;
}

template<class Qtype>
ROArrayQuantColumn<Qtype>::ROArrayQuantColumn(const Table& tab,
				              const String& columnName,
	    	    	    	    	      const Unit& u)
: itsDataCol(0),
  itsArrUnitsCol(0),
  itsScaUnitsCol(0)
{
    TableQuantumDesc* tqDesc =
    	TableQuantumDesc::reconstruct(tab.tableDesc(), columnName);
    itsUnit = u;
    itsQuantum = Quantum<Qtype>((Qtype) 0, itsUnit);
    itsDataCol = new ArrayColumn<Qtype>(tab, columnName);
    delete tqDesc;
}

template<class Qtype>
ROArrayQuantColumn<Qtype>::ROArrayQuantColumn(
    const ROArrayQuantColumn<Qtype>& that)
: itsUnit(that.itsUnit),
  itsDataCol(that.itsDataCol),
  itsQuantum(that.itsQuantum),
  itsArrUnitsCol(that.itsArrUnitsCol),
  itsScaUnitsCol(that.itsScaUnitsCol)
{
    if (itsDataCol != 0) {
	itsDataCol = new ArrayColumn<Qtype>(*itsDataCol);
    }
    if (itsArrUnitsCol != 0) {
	itsArrUnitsCol = new ArrayColumn<String>(*itsArrUnitsCol);
    }
    if (itsScaUnitsCol != 0) {
	itsScaUnitsCol = new ScalarColumn<String>(*itsScaUnitsCol);
    }
}

template<class Qtype>
ROArrayQuantColumn<Qtype>::~ROArrayQuantColumn()
{
    cleanUp();
}

template<class Qtype>
void ROArrayQuantColumn<Qtype>::cleanUp()
{
    delete itsDataCol;
    delete itsArrUnitsCol;
    delete itsScaUnitsCol;
}

template<class Qtype>
void ROArrayQuantColumn<Qtype>::reference(
    const ROArrayQuantColumn<Qtype>& that)
{
    cleanUp();
    itsDataCol = that.itsDataCol;
    itsArrUnitsCol = that.itsArrUnitsCol;
    itsScaUnitsCol = that.itsScaUnitsCol;
    itsQuantum = that.itsQuantum;
    itsUnit = that.itsUnit;
    if (itsDataCol != 0) {
	itsDataCol = new ArrayColumn<Qtype>(*itsDataCol);
    }
    if (itsArrUnitsCol != 0) {
	itsArrUnitsCol = new ArrayColumn<String>(*itsArrUnitsCol);
    }
    if (itsScaUnitsCol != 0) {
	itsScaUnitsCol = new ScalarColumn<String>(*itsScaUnitsCol);
    }
}

template<class Qtype>
void ROArrayQuantColumn<Qtype>::attach(const Table& tab, 
	    	    	    	       const String& columnName)
{
    reference(ROArrayQuantColumn<Qtype>(tab, columnName)); 
}
 
template<class Qtype>
void ROArrayQuantColumn<Qtype>::attach(const Table& tab, 
	    	    	    	       const String& columnName, const Unit& u)
{
    reference(ROArrayQuantColumn<Qtype>(tab, columnName, u)); 
}
 
template<class Qtype>
void ROArrayQuantColumn<Qtype>::get(Array<Quantum<Qtype> >& q, 
				    uInt rownr,
				    Bool resize) const
{ 
    // Quantums are created and put into q by taking Qtype data from 
    // itsDataCol and Quantum units from one of itsArrUnitsCol (if units
    // are in a ArrayColumn) or itsScaUnitsCol (if units are in a
    // ScalarColumn) or from itsUnit (if units are static).
    // getStorage() is used on each array to return pointers which are 
    // used to iterate through the respective arrays.
    
    Bool deleteData;
    Array<Qtype> tmpDataCol = (*itsDataCol)(rownr);
    const Qtype* d_p = tmpDataCol.getStorage(deleteData);
    
    // ensure q is the correct size
    IPosition shp = tmpDataCol.shape();
    if (!shp.isEqual(q.shape())) {
	if (resize || q.nelements() == 0) {
	    q.resize(shp);
	} else {
	    throw(TableArrayConformanceError("ArrayQuantColumn::get"));
	}
    }
    Bool deleteQuant;
    Quantum<Qtype>* q_p = q.getStorage(deleteQuant);

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
    
    tmpDataCol.freeStorage(d_p, deleteData);
    q.putStorage(q_p, deleteQuant);
    if (itsArrUnitsCol != 0) {
	tmpUnitsCol.freeStorage(u_p, deleteUnits);
    }
}

template<class Qtype>
void ROArrayQuantColumn<Qtype>::get(Array<Quantum<Qtype> >& q, uInt rownr,
	    	    	    	 const Unit& s, Bool resize) const
{        
    get(q, rownr, resize);
    Bool deleteIt;
    Quantum<Qtype>* q_p = q.getStorage(deleteIt);
    uInt n = q.nelements();
    for (uInt i=0; i<n; i++) {
	(q_p+i)->convert(s);
    }
    q.putStorage(q_p, deleteIt);
}

template<class Qtype>
void ROArrayQuantColumn<Qtype>::get(Array<Quantum<Qtype> >& q, uInt rownr,
				 const Quantum<Qtype>& other, 
	    	    	    	 Bool resize) const
{
    get(q, rownr, other.getFullUnit(), resize);
}

template<class Qtype> 
Array<Quantum<Qtype> > ROArrayQuantColumn<Qtype>::operator()(uInt rownr) const
{
    Array<Quantum<Qtype> > q;
    get(q, rownr);
    return q;
}

template<class Qtype> 
Array<Quantum<Qtype> > ROArrayQuantColumn<Qtype>::operator()(uInt rownr,
						  	  const Unit& u) const
{
    Array<Quantum<Qtype> > q;
    get(q, rownr, u);
    return q;
}

template<class Qtype> 
Array<Quantum<Qtype> > ROArrayQuantColumn<Qtype>::operator()(uInt rownr,
						  const Quantum<Qtype>& other)
						  const
{
    Array<Quantum<Qtype> > q;
    get(q, rownr, other);
    return q;
}

template<class Qtype>
void ROArrayQuantColumn<Qtype>::throwIfNull() const
{
    if (isNull()) {
        throw (TableInvOper("Quantum table column is null"));
    }
}
 
template<class Qtype>
ArrayQuantColumn<Qtype>::ArrayQuantColumn()
: ROArrayQuantColumn<Qtype>()
{}

template<class Qtype>
ArrayQuantColumn<Qtype>::ArrayQuantColumn(const Table& tab,
				          const String& columnName)
: ROArrayQuantColumn<Qtype>(tab, columnName)
{}

template<class Qtype>
ArrayQuantColumn<Qtype>::ArrayQuantColumn(const Table& tab,
				          const String& columnName,
	    	    	    	    	  const Unit& u)
: ROArrayQuantColumn<Qtype>(tab, columnName, u)
{}

template<class Qtype>
ArrayQuantColumn<Qtype>::ArrayQuantColumn(const ArrayQuantColumn<Qtype>& that)
: ROArrayQuantColumn<Qtype>(that)
{}

template<class Qtype>
ArrayQuantColumn<Qtype>::~ArrayQuantColumn()
{}

template<class Qtype>
void ArrayQuantColumn<Qtype>::reference(const ArrayQuantColumn<Qtype>& that)
{
    ROArrayQuantColumn<Qtype>::reference(that);
}

template<class Qtype>
void ArrayQuantColumn<Qtype>::attach(const Table& tab, 
	    	    	    	     const String& columnName)
{
    reference(ArrayQuantColumn<Qtype>(tab, columnName)); 
}
 
template<class Qtype>
void ArrayQuantColumn<Qtype>::attach(const Table& tab, 
	    	    	    	     const String& columnName, const Unit& u)
{
    reference(ArrayQuantColumn<Qtype>(tab, columnName, u)); 
}
 
template<class Qtype>
void ArrayQuantColumn<Qtype>::put(uInt rownr, const Array<Quantum<Qtype> >& q)
{
    // Each quantum in q is separated out into its Qtype component and
    // Unit component which are stored in itsDataCol and, if Units are
    // variable, itsArrUnitsCol repsectively.  If units are not variable
    // each quantum is first converted into local units before it is
    // saved.  
    // getStorage() is used to create pointers (to be used as iterators)
    // into the respective arrays.
    
    // if q is empty we don't want to do anything
    const uInt n = q.nelements();
    if (n == 0) {
	return;
    }
    
    Bool deleteData;
    Array<Qtype> dataArr(q.shape());
    Qtype* d_p = dataArr.getStorage(deleteData);
    
    Bool deleteQuant;
    const Quantum<Qtype>* q_p = q.getStorage(deleteQuant);
    
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
	unitsArr.resize(q.shape());
	u_p = unitsArr.getStorage(deleteUnits);
    } else if (itsScaUnitsCol != 0) {
	// just takes the value for unit from the first entry in q.  This
	// is safe because we know here that q contains at least 1 entry.
	localUnit = q_p->getFullUnit();
	itsScaUnitsCol->put(rownr, localUnit.getName());
    } else {
	localUnit = itsUnit;
    }

    // copy the value component of quantum into the local data array.
    // If using an array to store units copy quantum unit to local unit array
    for (uInt i=0; i<n; i++) {
	if (itsArrUnitsCol != 0) {
	    *(u_p+i) = (q_p+i)->getFullUnit().getName();
	    *(d_p+i) = (q_p+i)->getValue();
	} else {
	    // ensure quantum's value is consistent with column's units
	    // before storing.
	    itsQuantum = *(q_p+i);
	    // the following is commented out.  At the moment no conversion
	    // is done if q's unit doesn't conform but this is possible
//	    itsQuantum.convert(localUnit);
	    *(d_p+i) = itsQuantum.getValue();
	}
    }

    // update the real columns.
    dataArr.putStorage(d_p, deleteData);
    itsDataCol->put(rownr, dataArr);
    if (itsArrUnitsCol != 0) {
	unitsArr.putStorage(u_p, deleteUnits);
	itsArrUnitsCol->put(rownr, unitsArr);
    }
    
    q.freeStorage(q_p, deleteQuant);
}

