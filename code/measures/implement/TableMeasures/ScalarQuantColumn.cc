//# ScalarQuantColumn.cc: Access to a Scalar Quantum Column in a table.
//# Copyright (C) 1997,1998,1999,2000
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
#include <trial/TableMeasures/ScalarQuantColumn.h>
#include <trial/TableMeasures/TableQuantumDesc.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Quanta/Unit.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/ColumnDesc.h>
#include <aips/Tables/TableError.h>
#include <aips/Utilities/String.h>


template<class T>
ROScalarQuantColumn<T>::ROScalarQuantColumn()
: itsDataCol (0),
  itsUnitsCol(0),
  itsConvOut (False)
{}

template<class T>
ROScalarQuantColumn<T>::ROScalarQuantColumn (const Table& tab,
					     const String& columnName)
: itsDataCol (0),
  itsUnitsCol(0),
  itsConvOut (False)
{
  init (tab, columnName);
  itsUnitOut = itsUnit;
}

template<class T>
ROScalarQuantColumn<T>::ROScalarQuantColumn (const Table& tab,
					     const String& columnName,
					     const Unit& u)
: itsDataCol (0),
  itsUnitsCol(0)
{
  init (tab, columnName);
  itsUnitOut = u;
  itsConvOut = ToBool(! itsUnitOut.getName().empty());
}

template<class T>
ROScalarQuantColumn<T>::~ROScalarQuantColumn()
{
  cleanUp();
}

template<class T>
void ROScalarQuantColumn<T>::cleanUp()
{
  delete itsDataCol;
  itsDataCol = 0;
  delete itsUnitsCol;
  itsUnitsCol = 0;
}

template<class T>
ROScalarQuantColumn<T>::ROScalarQuantColumn
                                        (const ROScalarQuantColumn<T>& that)
: itsDataCol (0),
  itsUnitsCol(0)
{
  reference (that);
}

template<class T>
void ROScalarQuantColumn<T>::init (const Table& tab, const String& columnName)
{
  TableQuantumDesc* tqDesc = 
                TableQuantumDesc::reconstruct (tab.tableDesc(), columnName);
  if (tqDesc->isUnitVariable()) {
    itsUnitsCol = new ROScalarColumn<String>(tab, tqDesc->unitColumnName());
  } else {
    Vector<String> units (tqDesc->getUnits());
    if (units.nelements() > 0) {
      if (units.nelements() > 1) {
	throw (AipsError ("ScalarQuantColumn is used for column " +
			  columnName + " but its description has >1 units"));
      }
      itsUnit = units(0);
    }
  }
  itsDataCol = new ROScalarColumn<T>(tab, columnName);
  delete tqDesc;
}

template<class T>
void ROScalarQuantColumn<T>::reference (const ROScalarQuantColumn<T>& that)
{   
  cleanUp();
  itsUnit    = that.itsUnit;
  itsUnitOut = that.itsUnitOut;
  itsConvOut = that.itsConvOut;
  if (that.itsDataCol != 0) {
    itsDataCol = new ROScalarColumn<T>(*that.itsDataCol);
  }
  if (that.itsUnitsCol != 0) {
    itsUnitsCol = new ROScalarColumn<String>(*that.itsUnitsCol);
  }
}

template<class T>
void ROScalarQuantColumn<T>::attach (const Table& tab, 
				     const String& columnName)
{
  reference (ROScalarQuantColumn<T>(tab, columnName)); 
}
 
template<class T>
void ROScalarQuantColumn<T>::attach (const Table& tab, 
				     const String& columnName,
				     const Unit& u)
{
  reference (ROScalarQuantColumn<T>(tab, columnName, u)); 
}

template<class T>
void ROScalarQuantColumn<T>::throwIfNull() const
{
  if (isNull()) {
    throw (TableInvOper("Quantum table column is null"));
  }
}
 
template<class T>
void ROScalarQuantColumn<T>::getData (uInt rownr, Quantum<T>& q) const
{
  // Quantums are created from Ts stored in itsDataCol and Units
  // in itsUnitsCol, if units are variable, or itsUnit if non-variable.
  q.setValue ((*itsDataCol)(rownr));
  if (itsUnitsCol != 0) {
    q.setUnit ((*itsUnitsCol)(rownr));
  } else {
    q.setUnit (itsUnit);
  }
}

template<class T>
void ROScalarQuantColumn<T>::get (uInt rownr, Quantum<T>& q) const
{
  getData (rownr, q);
  if (itsConvOut) {
    q.convert (itsUnitOut);
  }
}

template<class T>
void ROScalarQuantColumn<T>::get (uInt rownr, Quantum<T>& q,
				  const Unit& u) const
{
  getData (rownr, q);
  q.convert (u);
}

template<class T>
void ROScalarQuantColumn<T>::get (uInt rownr, Quantum<T>& q,
				  const Quantum<T>& other) const
{
  getData (rownr, q);
  q.convert (other);
}

template<class T> 
Quantum<T> ROScalarQuantColumn<T>::operator() (uInt rownr) const
{
  Quantum<T> q;
  get (rownr, q);
  return q;
}

template<class T> 
Quantum<T> ROScalarQuantColumn<T>::operator() (uInt rownr,
					       const Unit& u) const
{
  Quantum<T> q;
  get (rownr, q, u);
  return q;
}

template<class T> 
Quantum<T> ROScalarQuantColumn<T>::operator() (uInt rownr,
					       const Quantum<T>& other) const
{
  Quantum<T> q;
  get (rownr, q, other);
  return q;
}



template<class T>
ScalarQuantColumn<T>::ScalarQuantColumn()
: ROScalarQuantColumn<T>(),
  itsDataCol (0),
  itsUnitsCol(0)
{}

template<class T>
ScalarQuantColumn<T>::ScalarQuantColumn (const Table& tab,
					 const String& columnName)
: ROScalarQuantColumn<T>(tab, columnName),
  itsDataCol (0),
  itsUnitsCol(0)
{
  itsDataCol = new ScalarColumn<T> (tab, columnName);
  if (unitsCol() != 0) {
    itsUnitsCol = new ScalarColumn<String> (tab,
					    unitsCol()->columnDesc().name());
  }
}

template<class T>
ScalarQuantColumn<T>::ScalarQuantColumn (const ScalarQuantColumn<T>& that)
: ROScalarQuantColumn<T>(),
  itsDataCol (0),
  itsUnitsCol(0)
{
  reference (that);
}

template<class T>
ScalarQuantColumn<T>::~ScalarQuantColumn()
{
  cleanUp();
}

template<class T>
void ScalarQuantColumn<T>::cleanUp()
{
  delete itsDataCol;
  itsDataCol = 0;
  delete itsUnitsCol;
  itsUnitsCol = 0;
}

template<class T>
void ScalarQuantColumn<T>::reference (const ScalarQuantColumn<T>& that)
{
  cleanUp();
  ROScalarQuantColumn<T>::reference (that);
  if (that.itsDataCol != 0) {
    itsDataCol = new ScalarColumn<T>(*that.itsDataCol);
  }
  if (that.itsUnitsCol != 0) {
    itsUnitsCol = new ScalarColumn<String>(*that.itsUnitsCol);
  }
}

template<class T>
void ScalarQuantColumn<T>::attach (const Table& tab, 
				   const String& columnName)
{
  reference (ScalarQuantColumn<T>(tab, columnName)); 
}
 
template<class T>
void ScalarQuantColumn<T>::put (uInt rownr, const Quantum<T>& q)
{
  // The value component of the quantum is stored in itsDataCol and the
  // unit component in itsUnitsCol unless Units are non-variable in
  // which case the Unit component is ignored (i.e., the Quantum's unit
  // is not checked against the Column's unit).
  if (itsUnitsCol != 0) {
    itsUnitsCol->put (rownr, q.getUnit());
    itsDataCol->put (rownr, q.getValue());
  } else {
    itsDataCol->put (rownr, q.getValue(itsUnit));
  }
}
