//# TableMeasDef.cc: Definition of a Measure in a Table.
//# Copyright (C) 1997,1999,2000
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
#include <aips/TableMeasures/TableMeasDesc.h>
#include <aips/Measures/MeasRef.h>
#include <aips/Quanta/Unit.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Arrays/Vector.h>
#include <aips/Exceptions/Error.h>


template<class M>
TableMeasDesc<M>::TableMeasDesc (const TableMeasValueDesc& value)
: TableMeasDescBase(value, TableMeasRefDesc(M::DEFAULT))
{
  M meas;
  Vector<Quantum<Double> > val;
  val = meas.getValue().getTMRecordValue();
  Vector<Unit> u;
  setMeasUnits (meas, val, u);
}

template<class M>
TableMeasDesc<M>::TableMeasDesc (const TableMeasValueDesc& value,
				 const Vector<Unit>& u)
: TableMeasDescBase(value, TableMeasRefDesc(M::DEFAULT))
{ 
  M meas;
  Vector<Quantum<Double> > val;
  val = meas.getValue().getTMRecordValue();
  setMeasUnits (meas, val, u);
}

template<class M>
TableMeasDesc<M>::TableMeasDesc (const TableMeasValueDesc& value,
				 const TableMeasRefDesc& ref)
: TableMeasDescBase(value, ref)
{
  M meas;
  Vector<Quantum<Double> > val;
  val = meas.getValue().getTMRecordValue();
  Vector<Unit> u;
  setMeasUnits (meas, val, u);
}

template<class M>
TableMeasDesc<M>::TableMeasDesc (const TableMeasValueDesc& value,
				 const TableMeasRefDesc& ref,
				 const Vector<Unit>& u)
: TableMeasDescBase(value, ref)
{
  M meas;
  Vector<Quantum<Double> > val;
  val = meas.getValue().getTMRecordValue();
  setMeasUnits (meas, val, u);
}

template<class M>
TableMeasDescBase* TableMeasDesc<M>::clone() const
{
  return new TableMeasDesc<M>(*this);
}

template<class M>
TableMeasDesc<M>::TableMeasDesc (const TableMeasDesc<M>& that)
: TableMeasDescBase(that)
{}

template<class M>
TableMeasDesc<M>::~TableMeasDesc()
{}

template<class M>
TableMeasDesc<M>& TableMeasDesc<M>::operator= (const TableMeasDesc<M>& that)
{
  TableMeasDescBase::operator= (that);
  return *this;
}
