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
#include <aips/Quanta/Quantum.h>
#include <aips/Quanta/Unit.h>
#include <trial/TableMeasures/ScalarQuantColumn.h>
#include <trial/TableMeasures/TableQuantumDesc.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableError.h>
#include <aips/Utilities/String.h>

template<class Qtype>
ROScalarQuantColumn<Qtype>::ROScalarQuantColumn()
: itsDataCol(0),
  itsUnitsCol(0)
{}

template<class Qtype>
ROScalarQuantColumn<Qtype>::ROScalarQuantColumn(const Table& tab,
				            	const String& columnName)
{
    TableQuantumDesc* tqDesc = 
    	TableQuantumDesc::reconstruct(tab.tableDesc(), columnName);
    if (tqDesc->isUnitVariable()) {
	itsUnitsCol = 
	    new ScalarColumn<String>(tab, tqDesc->unitColumnName());
    } else {
    	itsUnit = tqDesc->getUnits();
	itsUnitsCol = 0;
    }
    itsDataCol = new ScalarColumn<Qtype>(tab, columnName);
    delete tqDesc;
}

template<class Qtype>
ROScalarQuantColumn<Qtype>::ROScalarQuantColumn(const Table& tab,
				              	const String& columnName,
				            	const Unit& u)
: itsUnitsCol(0)
{
    TableQuantumDesc* tqDesc = 
    	TableQuantumDesc::reconstruct(tab.tableDesc(), columnName);
    itsUnit = u;
    itsDataCol = new ScalarColumn<Qtype>(tab, columnName);
    delete tqDesc;
}

template<class Qtype>
ROScalarQuantColumn<Qtype>::~ROScalarQuantColumn()
{
    cleanUp();
}

template<class Qtype>
void ROScalarQuantColumn<Qtype>::cleanUp()
{
    delete itsDataCol;
    delete itsUnitsCol;
}

template<class Qtype>
ROScalarQuantColumn<Qtype>::ROScalarQuantColumn(
    const ROScalarQuantColumn<Qtype>& that)
: itsDataCol(that.itsDataCol),
  itsUnit(that.itsUnit),
  itsUnitsCol(that.itsUnitsCol)
{
    if (itsDataCol != 0) {
	itsDataCol = new ScalarColumn<Qtype>(*itsDataCol);
    }
    if (itsUnitsCol != 0) {
	itsUnitsCol = new ScalarColumn<String>(*itsUnitsCol);
    }
}

template<class Qtype>
void ROScalarQuantColumn<Qtype>::reference(
    const ROScalarQuantColumn<Qtype>& that)
{   
    cleanUp();
    itsDataCol = that.itsDataCol;
    itsUnit = that.itsUnit;
    itsUnitsCol = that.itsUnitsCol;
    if (itsDataCol != 0) {
	itsDataCol = new ScalarColumn<Qtype>(*itsDataCol);
    }
    if (itsUnitsCol != 0) {
	itsUnitsCol = new ScalarColumn<String>(*itsUnitsCol);
    }
}

template<class Qtype>
void ROScalarQuantColumn<Qtype>::attach(const Table& tab, 
	    	    	    	    	const String& columnName)
{
    reference(ROScalarQuantColumn<Qtype>(tab, columnName)); 
}
 
template<class Qtype>
void ROScalarQuantColumn<Qtype>::attach(const Table& tab, 
	    	    	    	    	const String& columnName,
	    	    	    	    	const Unit& u)
{
    reference(ROScalarQuantColumn<Qtype>(tab, columnName, u)); 
}

template<class Qtype>
void ROScalarQuantColumn<Qtype>::throwIfNull() const
{
    if (isNull()) {
        throw (TableInvOper("Quantum table column is null"));
    }
}
 
template<class Qtype>
void ROScalarQuantColumn<Qtype>::get(Quantum<Qtype>& q, uInt rownr) const
{
    // Quantums are created from Qtypes stored in itsDataCol and Units
    // in itsUnitsCol, if units are variable, or itsUnit if non-variable.
        
    q.setValue((*itsDataCol)(rownr));
    if (itsUnitsCol != 0) {
	q.setUnit((*itsUnitsCol)(rownr));
    } else {
	q.setUnit(itsUnit);
    }
}

template<class Qtype>
void ROScalarQuantColumn<Qtype>::get(Quantum<Qtype>& q, uInt rownr,
	    	    	    	     const Unit& s) const
{
    get(q, rownr);
    q.convert(s);
}

template<class Qtype>
void ROScalarQuantColumn<Qtype>::get(Quantum<Qtype>& q, uInt rownr,
				     const Quantum<Qtype>& other) const
{
    get(q, rownr);
    q.convert(other);
}

template<class Qtype> 
Quantum<Qtype> ROScalarQuantColumn<Qtype>::operator()(uInt rownr) const
{
    Quantum<Qtype> q;
    get(q, rownr);
    return q;
}

template<class Qtype> 
Quantum<Qtype> ROScalarQuantColumn<Qtype>::operator()(uInt rownr,
						      const Unit& s) const
{
    Quantum<Qtype> q;
    get(q, rownr, s);
    return q;
}

template<class Qtype> 
Quantum<Qtype> ROScalarQuantColumn<Qtype>::operator()(uInt rownr,
						  const Quantum<Qtype>& other)
						  const
{
    Quantum<Qtype> q;
    get(q, rownr, other);
    return q;
}

template<class Qtype>
ScalarQuantColumn<Qtype>::ScalarQuantColumn()
: ROScalarQuantColumn<Qtype>()
{}

template<class Qtype>
ScalarQuantColumn<Qtype>::ScalarQuantColumn(const Table& tab,
				            const String& columnName)
: ROScalarQuantColumn<Qtype>(tab, columnName)
{}

template<class Qtype>
ScalarQuantColumn<Qtype>::ScalarQuantColumn(const Table& tab,
				            const String& columnName,
				            const Unit& u)
: ROScalarQuantColumn<Qtype>(tab, columnName, u)
{}
 
template<class Qtype>
ScalarQuantColumn<Qtype>::ScalarQuantColumn(
    const ScalarQuantColumn<Qtype>& that)
: ROScalarQuantColumn<Qtype>(that)
{}

template<class Qtype>
ScalarQuantColumn<Qtype>::~ScalarQuantColumn()
{}

template<class Qtype>
void ScalarQuantColumn<Qtype>::reference(const ScalarQuantColumn<Qtype>& that)
{
    ROScalarQuantColumn<Qtype>::reference(that);
}

template<class Qtype>
void ScalarQuantColumn<Qtype>::attach(const Table& tab, 
				      const String& columnName)
{
    reference(ScalarQuantColumn<Qtype>(tab, columnName)); 
}
 
template<class Qtype>
void ScalarQuantColumn<Qtype>::attach(const Table& tab, 
	    	    	    	      const String& columnName,
	    	    	    	      const Unit& u)
{
    reference(ScalarQuantColumn<Qtype>(tab, columnName, u)); 
}

template<class Qtype>
void ScalarQuantColumn<Qtype>::put(uInt rownr, const Quantum<Qtype>& q)
{
    // The value component of the quantum is stored in itsDataCol and the
    // unit component in itsUnitsCol unless Units are non-variable in
    // which case the Unit component is ignored (i.e., the Quantum's unit
    // is not checked against the Column's unit).
    
    if (itsUnitsCol != 0) {
	itsUnitsCol->put(rownr, q.getFullUnit().getName());
    }
    itsDataCol->put(rownr, q.getValue());
}

