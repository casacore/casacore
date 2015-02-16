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

#ifndef MEASURES_SCALARMEASCOLUMN_TCC
#define MEASURES_SCALARMEASCOLUMN_TCC

//# Includes
#include <casacore/measures/TableMeasures/ScalarMeasColumn.h>
#include <casacore/measures/TableMeasures/TableMeasDescBase.h>
#include <casacore/measures/TableMeasures/TableMeasOffsetDesc.h>
#include <casacore/measures/TableMeasures/TableMeasRefDesc.h>
#include <casacore/measures/Measures/MeasConvert.h>
#include <casacore/casa/Quanta/MeasValue.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class M>
ScalarMeasColumn<M>::ScalarMeasColumn()
  :  itsConvFlag  (False),
     itsArrDataCol(0),
     itsScaDataCol(0),
     itsRefIntCol (0),
     itsRefStrCol (0),
     itsOffsetCol (0)
{}

template<class M>
ScalarMeasColumn<M>::ScalarMeasColumn (const Table& tab,
                                       const String& columnName)
  : TableMeasColumn (tab, columnName),
    itsConvFlag  (False),
    itsArrDataCol(0),
    itsScaDataCol(0),
    itsRefIntCol (0),
    itsRefStrCol (0),
    itsOffsetCol (0)
{
  TableMeasDescBase& tmDesc = measDesc();
  AlwaysAssert(M::showMe() == tmDesc.type(), AipsError);

  // Create the data column. If the underlying measure can handle a
  // single value for its data then use a ScalarColumn otherwise an
  // ArrayColumn is needed to store the data component of the Measures.
  M tMeas;
  itsNvals = tMeas.getValue().getTMRecordValue().nelements();
  AlwaysAssert (itsNvals <= tmDesc.getUnits().size(), AipsError);
  if (itsNvals == 1) {
    itsScaDataCol = new ScalarColumn<Double>(tab, columnName);
  } else {
    itsArrDataCol = new ArrayColumn<Double>(tab, columnName);
  }

  // Set up the reference code component of the MeasRef
  if (tmDesc.isRefCodeVariable()) {
    const String& rcName = tmDesc.refColumnName();
    if ((tab.tableDesc().columnDesc(rcName).dataType() == TpString)) {
      itsRefStrCol = new ScalarColumn<String>(tab, rcName);
    } else {
      itsRefIntCol = new ScalarColumn<Int>(tab, rcName);
    }
  } else {
    itsMeasRef.set (tmDesc.getRefCode());
  }

  // Set up the offset component of the MeasRef
  if (tmDesc.hasOffset()) {
    if (tmDesc.isOffsetVariable()) {
      if (tmDesc.isOffsetArray()) {
	throw(AipsError("ScalarMeasColumn::ScalarMeasColumn "
			"Offset column must be a ScalarMeasColumn."));
      }
      itsOffsetCol = new ScalarMeasColumn<M>(tab, tmDesc.offsetColumnName());
    } else {
      itsMeasRef.set (tmDesc.getOffset());
    }
  } 

  // Only need to convert (during a put) if some component of the reference
  // for the column is fixed.
  itsConvFlag = (itsVarRefFlag == False) || (itsOffsetCol == 0);
  // For an old table write the reference codes and types.
  if (tab.isWritable()) {
    tmDesc.writeIfOld (tab);
  }
}

template<class M>
ScalarMeasColumn<M>::ScalarMeasColumn (const ScalarMeasColumn<M>& that)
: TableMeasColumn(),
  itsArrDataCol(0),
  itsScaDataCol(0),
  itsRefIntCol (0),
  itsRefStrCol (0),
  itsOffsetCol (0)
{
  reference (that);
}

template<class M>
ScalarMeasColumn<M>::~ScalarMeasColumn()
{
  cleanUp();
}

template<class M>
void ScalarMeasColumn<M>::cleanUp()
{
  delete itsArrDataCol;
  delete itsScaDataCol;
  delete itsRefIntCol;
  delete itsRefStrCol;
  delete itsOffsetCol;
}

template<class M>
void ScalarMeasColumn<M>::reference (const ScalarMeasColumn<M>& that)
{   
  cleanUp();
  TableMeasColumn::reference (that);
  itsConvFlag   = that.itsConvFlag;
  itsArrDataCol = that.itsArrDataCol;
  itsScaDataCol = that.itsScaDataCol;
  itsRefIntCol  = that.itsRefIntCol;
  itsRefStrCol  = that.itsRefStrCol;
  itsOffsetCol  = that.itsOffsetCol;
  itsMeasRef = that.itsMeasRef;
  if (itsArrDataCol != 0) {
    itsArrDataCol = new ArrayColumn<Double>(*itsArrDataCol);
  }
  if (itsScaDataCol != 0) {
    itsScaDataCol = new ScalarColumn<Double>(*itsScaDataCol);
  }
  if (itsRefIntCol != 0) {
    itsRefIntCol = new ScalarColumn<Int>(*itsRefIntCol);
  }
  if (itsRefStrCol != 0) {
    itsRefStrCol = new ScalarColumn<String>(*itsRefStrCol);
  }
  if (itsOffsetCol != 0) {
    itsOffsetCol = new ScalarMeasColumn<M>(*itsOffsetCol);
  }
}

template<class M>
void ScalarMeasColumn<M>::attach (const Table& tab, 
                                  const String& columnName)
{
  reference (ScalarMeasColumn<M>(tab, columnName)); 
}
    
template<class M>
void ScalarMeasColumn<M>::get (uInt rownr, M& meas) const
{
  Vector<Quantum<Double> > qvec(itsNvals);
  const Vector<Unit>& units = measDesc().getUnits();
  if (itsScaDataCol != 0) {
    qvec(0).setValue ((*itsScaDataCol)(rownr));
    qvec(0).setUnit (units(0));
  } else {
    Array<Double> tmpArr((*itsArrDataCol)(rownr));
    Bool deleteData;
    const Double* d_p = tmpArr.getStorage (deleteData);
    for (uInt i=0; i<itsNvals; i++) {
      qvec(i).setValue (d_p[i]);
      qvec(i).setUnit (units(i));
    }
    tmpArr.freeStorage (d_p, deleteData);
  }
  typename M::MVType measVal (qvec);
  meas.set (measVal, makeMeasRef(rownr));
}
    	
template<class M> 
M ScalarMeasColumn<M>::convert (uInt rownr, const MeasRef<M>& measRef) const
{
  M tmp;
  get (rownr, tmp);
  return typename M::Convert(tmp, measRef)();
}

template<class M> 
M ScalarMeasColumn<M>::convert (uInt rownr, uInt refCode) const
{
  M tmp;
  get (rownr, tmp);
  return typename M::Convert(tmp, typename M::Types(refCode))();
}

template<class M> 
M ScalarMeasColumn<M>::operator() (uInt rownr) const
{
  M meas;
  get (rownr, meas);
  return meas;
}

template<class M>
MeasRef<M> ScalarMeasColumn<M>::makeMeasRef (uInt rownr) const
{
  // Fixed reference can be returned immediately.
  if (!itsVarRefFlag  &&  itsOffsetCol == 0) {
    return itsMeasRef;
  }
  MeasRef<M> locMRef (itsMeasRef);
  if (itsVarRefFlag) {
    // Get reference type as int (from a string or int column).
    if (itsRefStrCol != 0) {
      typename M::Types tp;
      M::getType (tp, (*itsRefStrCol)(rownr));
      locMRef.set (tp);
    } else {
      locMRef.set (measDesc().getRefDesc().tab2cur((*itsRefIntCol)(rownr)));
    }
  }
  if (itsOffsetCol != 0) {
    locMRef.set ((*itsOffsetCol)(rownr));
  }
  return locMRef;
}


template<class M>
void ScalarMeasColumn<M>::setDescRefCode (uInt refCode,
					  Bool tableMustBeEmpty)
{
  Table tab = table();
  if (tableMustBeEmpty  &&  tab.nrow() != 0) {
    throw (AipsError ("ScalarMeasColumn::setDescRefCode cannot be done; "
		      "the table is not empty"));
  }
  itsDescPtr->resetRefCode (refCode);
  itsDescPtr->write (tab);
  itsMeasRef.set (refCode);
}

template<class M>
void ScalarMeasColumn<M>::setDescOffset (const Measure& offset,
					 Bool tableMustBeEmpty)
{
  Table tab = table();
  if (tableMustBeEmpty  &&  tab.nrow() != 0) {
    throw (AipsError ("ScalarMeasColumn::setDescOffset cannot be done; "
		      "the table is not empty"));
  }
  itsDescPtr->resetOffset (offset);
  itsDescPtr->write (tab);
  itsMeasRef.set (offset);
}

template<class M>
void ScalarMeasColumn<M>::setDescUnits (const Vector<Unit>& units,
					Bool tableMustBeEmpty)
{
  Table tab = table();
  if (tableMustBeEmpty  &&  tab.nrow() != 0) {
    throw (AipsError ("ScalarMeasColumn::setDescUnits cannot be done; "
		      "the table is not empty"));
  }
  itsDescPtr->resetUnits (units);
  itsDescPtr->write (tab);
}
 
template<class M>
void ScalarMeasColumn<M>::put (uInt rownr, const M& meas)
{
  // A few things about put:
  // 1. No support for storage of frames so if the meas has a frame and
  //    this column has variable references throw an exception.
  // 2. Convert meas if reference and/or offset are different and
  //    not variable for the column.
  // 3. Convert units if different.

  // When the reference is variable, measure's reference and offset
  // are saved as well as the measure's value.

  // check if the entered measure is "legal"
  if (itsVarRefFlag) {
    if (! meas.getRefPtr()->getFrame().empty()) {
      throw(AipsError("ScalarMeasColumn::put() measure has a frame."
		      " Illegal for variable reference column."));
    }
  }
  M locMeas = meas;

  // Conversion is needed if the reference for the incoming measure is
  // not equal to that of the column (and itsConvFlag is true)
  if (itsConvFlag  &&  !equalRefs(itsMeasRef, locMeas.getRef())) {
    MeasRef<M> refConv = itsMeasRef;
    if (itsVarRefFlag) {
      refConv.set (locMeas.getRef().getType());
    }

    //        cerr << "\nDOING CONVERT!!!!\n";
    //        cerr << "itsMeasRef: " << refConv << endl;
    //        cerr << "locMeasRef: " << locMeas.getRef() << endl;
    //        cerr << "itsMeasRef: " << refConv.offset() << ' ';
    //	if (refConv.offset()) {
    //	  cerr << *refConv.offset() << ' ';
    //	  refConv.offset()->print(cerr);
    //	  refConv.offset()->getRefPtr()->print(cerr);
    //	  cerr << refConv.offset()->getRefPtr()->offset();
    //	}
    //	cerr << endl;
    //        cerr << "locMeasRef: " << locMeas.getRef().offset() << endl;
    //	if (locMeas.getRef().offset()) {
    //	  cerr << *locMeas.getRef().offset() << ' ';
    //	  locMeas.getRef().offset()->print(cerr);
    //	  locMeas.getRef().offset()->getRefPtr()->print(cerr);
    //	  cerr << locMeas.getRef().offset()->getRefPtr()->offset();
    //	}
    //	cerr << endl;

    //    cerr << "Before convert: " << locMeas << locMeas.getRef() << endl;
    typename M::Convert conv(locMeas, refConv);
    locMeas = conv();
    //    cerr << " After convert: " << locMeas << locMeas.getRef() << endl;
  }
    
  if (itsVarRefFlag) {
    if (itsRefStrCol != 0) {
      itsRefStrCol->put(rownr, M::showType(locMeas.getRef().getType()));
    } else {
      uInt tp = locMeas.getRef().getType();
      itsRefIntCol->put(rownr, measDesc().getRefDesc().cur2tab (tp));
    }
  }
  if (itsOffsetCol != 0) {
    if (locMeas.getRef().offset() != 0) {
      itsOffsetCol->put(rownr, M(locMeas.getRef().offset()));
    } else {
      itsOffsetCol->put(rownr, M());
    }
  }

  const Vector<Unit>& units = measDesc().getUnits();
  Vector<Quantum<Double> > qvec = locMeas.getValue().getTMRecordValue();
  if (itsScaDataCol != 0) {
    itsScaDataCol->put (rownr, qvec(0).getValue(units(0)));
  } else {
    Vector<Double> d_vec(itsNvals);
    for (uInt i=0; i<itsNvals; i++) {
      d_vec(i) = qvec(i).getValue(units(i));
    }
    itsArrDataCol->put (rownr, d_vec);
  }
}

template<class M>
Bool ScalarMeasColumn<M>::equalRefs (const MRBase& r1, const MRBase& r2) const
{
  return ((r1.getType() == r2.getType()) && (r1.offset() == r2.offset()));
}

} //# NAMESPACE CASACORE - END

#endif
