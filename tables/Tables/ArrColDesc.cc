//# ArrColDesc.cc: Templated class to describe columns of arrays in tables
//# Copyright (C) 1994,1995,1996,1997,1999,2001
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ArrColData.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/iostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

ArrayColumnDescBase::ArrayColumnDescBase (const String& name,
                                          const String& comment,
                                          const String& dataManagerType,
                                          const String& dataManagerGroup,
                                          DataType dt, const String& dataTypeId,
                                          Int options,
                                          uInt ndim, const IPosition& shape)
  : BaseColumnDesc (name, comment, dataManagerType, dataManagerGroup,
                    dt, dataTypeId, options, ndim, shape,
                    False, True, False)
{
    if (nrdim_p <= 0) {
	nrdim_p = -1;
    }
}

ArrayColumnDescBase::ArrayColumnDescBase (const ArrayColumnDescBase& that)
: BaseColumnDesc (that)
{}

ArrayColumnDescBase::~ArrayColumnDescBase()
{}

ArrayColumnDescBase& ArrayColumnDescBase::operator=
                                       (const ArrayColumnDescBase& that)
{
    BaseColumnDesc::operator= (that);
    return *this;
}

//# Return the class name.
String ArrayColumnDescBase::className() const
    { return "ArrayColumnDesc<" + dataTypeId(); }

//# Put the object.
//# The data is read by the ctor taking AipsIO.
//# It was felt that putstart takes too much space, so therefore
//# the version is put "manually".
void ArrayColumnDescBase::putDesc (AipsIO& ios) const
{
    ios << (uInt)1;                  // class version 1
    // Formerly a switch was written to determine if a default existed.
    // This switch was always false.
    // Keep writing this switch (which is not used anymore).
    ios << False;
}

void ArrayColumnDescBase::getDesc (AipsIO& ios)
{
    uInt version;
    ios >> version;
    // Formerly a switch was written to determine if a default existed.
    // This switch was always false.
    // So read it in and do not do anything with it.
    Bool sw;
    ios >> sw;
}

//# Show the column.
void ArrayColumnDescBase::show (ostream& os) const
{
    os << "   Name=" << name();
    os << "   DataType=" << dataType();
    if (dataType() == TpOther) {
	os << ", " << dataTypeId();
    }
    if (maxLength() > 0) {
	os << "   MaxLength=" << maxLength();
    }
    os << "   Nrdim=" << ndim();
    os << "   Shape=" << shape();
    os << endl;
    os << "   DataManager=" << dataManagerType();
    os << "/" << dataManagerGroup();
    os << endl;
    os << "   Comment = " << comment() << endl;
}


//# Create a column object from the description.
PlainColumn* ArrayColumnDescBase::makeColumn (ColumnSet* csp) const
{
    PlainColumn* bcp = new ArrayColumnData (this, csp);
    return bcp;
}


} //# NAMESPACE CASACORE - END
