//# TableMeasDef.cc: Definition of a Measure in a Table.
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
#include <aips/Exceptions/Error.h>
#include <aips/Measures/MeasRef.h>
#include <aips/Tables/TableDesc.h>
#include <trial/TableMeasures/TableMeasDesc.h>
#include <trial/TableMeasures/TableMeasDescBase.h>

template<class M>
TableMeasDesc<M>::TableMeasDesc()
: TableMeasDescBase()
{}

template<class M>
TableMeasDesc<M>::TableMeasDesc(const TableMeasValueDesc& value)
: TableMeasDescBase(value)
{}

template<class M>
TableMeasDesc<M>::TableMeasDesc(const TableMeasValueDesc& value,
				const TableMeasRefDesc& ref)
: TableMeasDescBase(value, ref)
{}

template<class M>
TableMeasDescBase* TableMeasDesc<M>::clone() const
{
    return new TableMeasDesc<M>(*this);
}

template<class M>
TableMeasDesc<M>::TableMeasDesc(const TableMeasDesc<M>& that)
: TableMeasDescBase(that)
{}

template<class M>
TableMeasDesc<M>::~TableMeasDesc()
{}

template<class M>
TableMeasDesc<M>& TableMeasDesc<M>::operator=(const TableMeasDesc<M>& that)
{
    TableMeasDescBase::operator=(that);
    return *this;
}

template<class M>
const String& TableMeasDesc<M>::type() const 
{ 
    return M::showMe();
}

template<class M>
const uInt TableMeasDesc<M>::refCode(const String& refString) const 
{
    MeasRef<M> refType;
    M measure;
    Bool succ = measure.giveMe(refType, refString);
    if (!succ) {
	throw(AipsError("TableMeasDesc: refType " + refString +
			" unknown for measType " + type()));
    }
    
    return refType.getType();
}


template<class M>
const String& TableMeasDesc<M>::refType(const uInt refCode) const 
{ 
    return M::showType(refCode);
}
