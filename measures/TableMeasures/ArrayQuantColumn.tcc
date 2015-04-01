//# ArrayQuantColumn.cc: Access to an Array Quantum Column in a table.
//# Copyright (C) 1997,1998,1999,2000,2001
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

#ifndef MEASURES_ARRAYQUANTCOLUMN_TCC
#define MEASURES_ARRAYQUANTCOLUMN_TCC

//# Includes
#include <casacore/measures/TableMeasures/ArrayQuantColumn.h>
#include <casacore/measures/TableMeasures/TableQuantumDesc.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Quanta/Unit.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/BasicSL/String.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
ArrayQuantColumn<T>::ArrayQuantColumn()
: itsDataCol    (0),
  itsArrUnitsCol(0),
  itsScaUnitsCol(0),
  itsConvOut    (False)
{}

template<class T>
ArrayQuantColumn<T>::ArrayQuantColumn (const Table& tab,
                                       const String& columnName)
: itsDataCol    (0),
  itsArrUnitsCol(0),
  itsScaUnitsCol(0),
  itsConvOut    (False)
{
  init (tab, columnName);
  itsUnitOut = itsUnit;
}

template<class T>
ArrayQuantColumn<T>::ArrayQuantColumn (const Table& tab,
                                       const String& columnName,
                                       const Unit& u)
: itsDataCol    (0),
  itsArrUnitsCol(0),
  itsScaUnitsCol(0)
{
  init (tab, columnName);
  itsUnitOut.resize(1);
  itsUnitOut(0) = u;
  itsConvOut = (! itsUnitOut(0).getName().empty());
}

template<class T>
ArrayQuantColumn<T>::ArrayQuantColumn (const Table& tab,
                                       const String& columnName,
                                       const Vector<Unit>& u)
: itsDataCol    (0),
  itsArrUnitsCol(0),
  itsScaUnitsCol(0)
{
  init (tab, columnName);
  itsUnitOut.resize(u.nelements());
  itsUnitOut = u;
  itsConvOut = False;
  for (uInt i=0; i<itsUnitOut.nelements(); i++) {
    if (! itsUnitOut(i).getName().empty()) {
      itsConvOut = True;
      break;
    }
  }
}

template<class T>
ArrayQuantColumn<T>::ArrayQuantColumn (const ArrayQuantColumn<T>& that)
: itsDataCol    (0),
  itsArrUnitsCol(0),
  itsScaUnitsCol(0)
{
  reference (that);
}

template<class T>
ArrayQuantColumn<T>::~ArrayQuantColumn()
{
  cleanUp();
}

template<class T>
void ArrayQuantColumn<T>::cleanUp()
{
  delete itsDataCol;
  itsDataCol = 0;
  delete itsArrUnitsCol;
  itsArrUnitsCol = 0;
  delete itsScaUnitsCol;
  itsScaUnitsCol = 0;
}

template<class T>
void ArrayQuantColumn<T>::init (const Table& tab, const String& columnName)
{
  TableQuantumDesc* tqDesc =
    	TableQuantumDesc::reconstruct(tab.tableDesc(), columnName);
  if (tqDesc->isUnitVariable()) {
    // the variable units column could be either an Array or a Scalar
    String varColName = tqDesc->unitColumnName();
    if (tab.tableDesc().columnDesc(varColName).isScalar()) {
      itsScaUnitsCol = new ScalarColumn<String>(tab, varColName);	
    } else {
      itsArrUnitsCol = new ArrayColumn<String>(tab, varColName);
    }
  } else {
    Vector<String> units = tqDesc->getUnits();
    itsUnit.resize (units.nelements());
    for (uInt i=0; i<units.nelements(); i++) {
      itsUnit(i) = units(i);
    }
  }
  itsDataCol = new ArrayColumn<T>(tab, columnName);
  delete tqDesc;
}

template<class T>
void ArrayQuantColumn<T>::reference (const ArrayQuantColumn<T>& that)
{
  cleanUp();
  itsUnit.resize (that.itsUnit.nelements());
  itsUnitOut.resize (that.itsUnitOut.nelements());
  itsUnit    = that.itsUnit;
  itsUnitOut = that.itsUnitOut;
  itsConvOut = that.itsConvOut;
  if (that.itsDataCol != 0) {
    itsDataCol = new ArrayColumn<T>(*that.itsDataCol);
  }
  if (that.itsArrUnitsCol != 0) {
    itsArrUnitsCol = new ArrayColumn<String>(*that.itsArrUnitsCol);
  }
  if (that.itsScaUnitsCol != 0) {
    itsScaUnitsCol = new ScalarColumn<String>(*that.itsScaUnitsCol);
  }
}

template<class T>
void ArrayQuantColumn<T>::attach (const Table& tab, 
                                  const String& columnName)
{
  reference(ArrayQuantColumn<T> (tab, columnName)); 
}
 
template<class T>
void ArrayQuantColumn<T>::attach (const Table& tab, 
                                  const String& columnName,
                                  const Unit& u)
{
  reference(ArrayQuantColumn<T> (tab, columnName, u)); 
}
 
template<class T>
void ArrayQuantColumn<T>::attach (const Table& tab, 
                                  const String& columnName,
                                  const Vector<Unit>& u)
{
  reference(ArrayQuantColumn<T> (tab, columnName, u)); 
}
 
template<class T>
Vector<String> ArrayQuantColumn<T>::getUnits() const
{
  Vector<String> names(itsUnit.nelements());
  for (uInt i=0; i<itsUnit.nelements(); i++) {
    names(i) = itsUnit(i).getName();
  }
  return names;
}

template<class T>
void ArrayQuantColumn<T>::getData (uInt rownr, Array<Quantum<T> >& q, 
                                   Bool resize) const
{ 
  // Quantums are created and put into q by taking T data from 
  // itsDataCol and Quantum units from one of itsArrUnitsCol (if units
  // are in a ArrayColumn) or itsScaUnitsCol (if units are in a
  // ScalarColumn) or from itsUnit (if units are static).
  // getStorage() is used on each array to return pointers which are 
  // used to iterate through the respective arrays.
  Bool deleteData;
  Array<T> tmpDataCol = (*itsDataCol)(rownr);
  const T* d_p = tmpDataCol.getStorage(deleteData);
  // Ensure q is the correct size. Resize if needed.
  IPosition shp = tmpDataCol.shape();
  if (!shp.isEqual(q.shape())) {
    if (resize || q.nelements() == 0) {
      q.resize(shp);
    } else {
      throw(TableArrayConformanceError("ArrayQuantColumn::get"));
    }
  }
  Bool deleteQuant;
  Quantum<T>* q_p = q.getStorage(deleteQuant);
  
  const String* u_p=0;
  Bool deleteUnits;
  Array<String> tmpUnitsCol;
  Vector<Unit> localUnit(itsUnit);
  if (itsArrUnitsCol != 0) {
    Array<String> tmp = (*itsArrUnitsCol)(rownr);
    tmpUnitsCol.reference (tmp);
    u_p = tmpUnitsCol.getStorage(deleteUnits);
  } else if (itsScaUnitsCol != 0) {
    localUnit.resize(1);
    localUnit(0) = (*itsScaUnitsCol)(rownr);
  }
  uInt nrun = localUnit.nelements();
  
  uInt n = tmpDataCol.nelements();
  for (uInt i=0; i<n; i++) {
    q_p[i].setValue (d_p[i]);
    if (itsArrUnitsCol != 0) {
      q_p[i].setUnit (u_p[i]);
    } else {
      q_p[i].setUnit (localUnit(i%nrun));
    }
  }
  
  tmpDataCol.freeStorage (d_p, deleteData);
  q.putStorage (q_p, deleteQuant);
  if (itsArrUnitsCol != 0) {
    tmpUnitsCol.freeStorage (u_p, deleteUnits);
  }
}

template<class T>
void ArrayQuantColumn<T>::get (uInt rownr, Array<Quantum<T> >& q,
                               Bool resize) const
{        
  if (itsConvOut) {
    get (rownr, q, itsUnitOut, resize);
  } else {
    getData (rownr, q, resize);
  }
}

template<class T>
void ArrayQuantColumn<T>::get (uInt rownr, Array<Quantum<T> >& q,
                               const Unit& u, Bool resize) const
{        
  getData (rownr, q, resize);
  if (! u.getName().empty()) {
    Bool deleteIt;
    Quantum<T>* q_p = q.getStorage(deleteIt);
    uInt n = q.nelements();
    for (uInt i=0; i<n; i++) {
      q_p[i].convert (u);
    }
    q.putStorage(q_p, deleteIt);
  }
}

template<class T>
void ArrayQuantColumn<T>::get (uInt rownr, Array<Quantum<T> >& q,
                               const Vector<Unit>& u, Bool resize) const
{        
  getData (rownr, q, resize);
  Bool hasUnits = False;
  uInt nrun = u.nelements();
  Vector<Bool> hasUnit(nrun, False);
  for (uInt i=0; i<nrun; i++) {
    if (!u(i).getName().empty()) {
      hasUnits = True;
      hasUnit(i) = True;
    }
  }
  if (hasUnits) {
    Bool deleteIt;
    Quantum<T>* q_p = q.getStorage(deleteIt);
    uInt n = q.nelements();
    for (uInt i=0; i<n; i++) {
      uInt inx = i%nrun;
      if (hasUnit(inx)) {
	q_p[i].convert (u(inx));
      }
    }
    q.putStorage(q_p, deleteIt);
  }
}

template<class T>
void ArrayQuantColumn<T>::get (uInt rownr, Array<Quantum<T> >& q,
                               const Quantum<T>& other, 
                               Bool resize) const
{
  get (rownr, q, other.getFullUnit(), resize);
}

template<class T> 
Array<Quantum<T> > ArrayQuantColumn<T>::operator() (uInt rownr) const
{
  Array<Quantum<T> > q;
  get (rownr, q);
  return q;
}

template<class T> 
Array<Quantum<T> > ArrayQuantColumn<T>::operator() (uInt rownr,
                                                    const Unit& u) const
{
  Array<Quantum<T> > q;
  get (rownr, q, u);
  return q;
}

template<class T> 
Array<Quantum<T> > ArrayQuantColumn<T>::operator() (uInt rownr,
                                                    const Vector<Unit>& u) const
{
  Array<Quantum<T> > q;
  get (rownr, q, u);
  return q;
}

template<class T> 
Array<Quantum<T> > ArrayQuantColumn<T>::operator()
                               (uInt rownr, const Quantum<T>& other) const
{
  Array<Quantum<T> > q;
  get (rownr, q, other);
  return q;
}

template<class T>
void ArrayQuantColumn<T>::throwIfNull() const
{
  if (isNull()) {
    throw (TableInvOper("Quantum table column is null"));
  }
}
 
template<class T>
void ArrayQuantColumn<T>::put (uInt rownr, const Array<Quantum<T> >& q)
{
  // Each quantum in q is separated out into its T component and
  // Unit component which are stored in itsDataCol and, if Units are
  // variable, itsArrUnitsCol repsectively.  If units are not variable
  // each quantum is first converted into local units before it is
  // saved.  
  
  // If q is empty, write empty arrays.
  const uInt n = q.nelements();
  if (n == 0) {
    Array<T> arr;
    itsDataCol->put (rownr, arr);
    if (itsArrUnitsCol != 0) {
      Array<String> arru;
      itsArrUnitsCol->put (rownr, arru);
    } else if (itsScaUnitsCol != 0) {
      itsScaUnitsCol->put (rownr, String());
    }
    return;
  }
  
  Array<T> dataArr(q.shape());
  Bool deleteData;
  T* d_p = dataArr.getStorage(deleteData);
  
  Bool deleteQuant;
  const Quantum<T>* q_p = q.getStorage(deleteQuant);
  
  // If units are variable they could vary per element of the quantum
  // array (i.e., the units column is also an array) or they could vary
  // by row (i.e., the units column is a Scalar Column where each row 
  // contains a single unit entry).  When variable by row, the unit of the
  // first quantum in q is used.
  Bool deleteUnits;
  String* u_p;
  Array<String> unitsArr;
  Vector<Unit> localUnit(itsUnit);
  if (itsArrUnitsCol != 0) {
    unitsArr.resize(q.shape());
    u_p = unitsArr.getStorage(deleteUnits);
  } else if (itsScaUnitsCol != 0) {
    // Take the value for unit from the first entry in q.  This
    // is safe because we know here that q contains at least 1 entry.
    localUnit.resize(1);
    localUnit(0) = q_p[0].getFullUnit();
    itsScaUnitsCol->put (rownr, localUnit(0).getName());
  }
  uInt nrun = localUnit.nelements();
  
  // Copy the value component of each quantum into the local data array.
  // If using an array to store units, copy quantum unit to local unit array
  for (uInt i=0; i<n; i++) {
    if (itsArrUnitsCol != 0) {
      u_p[i] = q_p[i].getFullUnit().getName();
      d_p[i] = q_p[i].getValue();
    } else {
      // Convert to fixed unit.
      d_p[i] = q_p[i].getValue (localUnit(i%nrun));
    }
  }
  
  // update the real columns.
  dataArr.putStorage (d_p, deleteData);
  itsDataCol->put (rownr, dataArr);
  if (itsArrUnitsCol != 0) {
    unitsArr.putStorage (u_p, deleteUnits);
    itsArrUnitsCol->put (rownr, unitsArr);
  }
  
  q.freeStorage(q_p, deleteQuant);
}

} //# NAMESPACE CASACORE - END


#endif
