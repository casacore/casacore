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
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#ifndef TABLES_ARRCOLDESC_TCC
#define TABLES_ARRCOLDESC_TCC

#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ArrColData.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Utilities/ValTypeId.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/iostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
ArrayColumnDesc<T>::ArrayColumnDesc (const String& name,
				     Int ndim, int opt)
: BaseColumnDesc (name, "", "", "",
		  ValType::getType(static_cast<T*>(0)),
		  valDataTypeId(static_cast<T*>(0)),
		  opt, ndim, IPosition(),
		  False, True, False)
{
    if (nrdim_p <= 0) {
	nrdim_p = -1;
    }
}

template<class T>
ArrayColumnDesc<T>::ArrayColumnDesc (const String& name,
				     const String& comment,
				     Int ndim, int opt)
: BaseColumnDesc (name, comment, "", "",
		  ValType::getType(static_cast<T*>(0)),
		  valDataTypeId(static_cast<T*>(0)),
		  opt, ndim, IPosition(),
		  False, True, False)
{
    if (nrdim_p == 0) {
	nrdim_p = -1;
    }
}
  
template<class T>
ArrayColumnDesc<T>::ArrayColumnDesc (const String& name,
				     const String& comment,
				     const String& dataManName,
				     const String& dataManGroup,
				     Int ndim, int opt)
: BaseColumnDesc (name, comment, dataManName, dataManGroup,
		  ValType::getType(static_cast<T*>(0)),
		  valDataTypeId(static_cast<T*>(0)),
		  opt, ndim, IPosition(),
		  False, True, False)
{
    if (nrdim_p == 0) {
	nrdim_p = -1;
    }
}
  
template<class T>
ArrayColumnDesc<T>::ArrayColumnDesc (const String& name,
				     const IPosition& shp,
				     int opt)
: BaseColumnDesc (name, "", "", "",
		  ValType::getType(static_cast<T*>(0)),
		  valDataTypeId(static_cast<T*>(0)),
		  opt, shp.nelements(), shp,
		  False, True, False)
{
    if (nrdim_p == 0) {
	nrdim_p = -1;
    }
}
  
template<class T>
ArrayColumnDesc<T>::ArrayColumnDesc (const String& name,
				     const String& comment,
				     const IPosition& shp,
				     int opt)
: BaseColumnDesc (name, comment, "", "",
		  ValType::getType(static_cast<T*>(0)),
		  valDataTypeId(static_cast<T*>(0)),
		  opt, shp.nelements(), shp,
		  False, True, False)
{
    if (nrdim_p == 0) {
	nrdim_p = -1;
    }
}
  
template<class T>
ArrayColumnDesc<T>::ArrayColumnDesc (const String& name,
				     const String& comment,
				     const String& dataManName,
				     const String& dataManGroup,
				     const IPosition& shp,
				     int opt,
				     int ndim)
: BaseColumnDesc (name, comment, dataManName, dataManGroup,
		  ValType::getType(static_cast<T*>(0)),
		  valDataTypeId(static_cast<T*>(0)),
		  opt, shp.nelements(), shp,
		  False, True, False)
{
    if (nrdim_p == 0) {
	nrdim_p = -1;
    }
    if (ndim > 0) {
        if (nrdim_p > 0  &&  ndim != nrdim_p) {
	    throw (TableInvColumnDesc (name, "Shape length mismatches ndim"));
	} else {
	    nrdim_p = ndim;
	}
    }
}

template<class T>
ArrayColumnDesc<T>::ArrayColumnDesc (const ArrayColumnDesc<T>& that)
: BaseColumnDesc (that)
{}


//# Make a new object.
template<class T>
BaseColumnDesc* ArrayColumnDesc<T>::makeDesc(const String&)
{
    BaseColumnDesc* ptr = new ArrayColumnDesc<T>(String());
    return ptr;
}

template<class T>
ArrayColumnDesc<T>::~ArrayColumnDesc()
{}


template<class T>
ArrayColumnDesc<T>& ArrayColumnDesc<T>::operator=
                                       (const ArrayColumnDesc<T>& that)
{
    BaseColumnDesc::operator= (that);
    return *this;
}

//# Clone this column description to another.
template<class T>
BaseColumnDesc* ArrayColumnDesc<T>::clone() const
{
    BaseColumnDesc* ptr = new ArrayColumnDesc<T>(*this);
    return ptr;
}


//# Return the class name.
template<class T>
String ArrayColumnDesc<T>::className() const
    { return "ArrayColumnDesc<" + dataTypeId(); }

//# Register the makeDesc function.
template<class T>
void ArrayColumnDesc<T>::registerClass() const
{
    ColumnDesc::registerCtor (className(), makeDesc);
}


//# Put the object.
//# The data is read by the ctor taking AipsIO.
//# It was felt that putstart takes too much space, so therefore
//# the version is put "manually".
template<class T>
void ArrayColumnDesc<T>::putDesc (AipsIO& ios) const
{
    ios << (uInt)1;                  // class version 1
    // Formerly a switch was written to determine if a default existed.
    // This switch was always false.
    // Keep writing this switch (which is not used anymore).
    ios << False;
}

template<class T>
void ArrayColumnDesc<T>::getDesc (AipsIO& ios)
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
template<class T>
void ArrayColumnDesc<T>::show (ostream& os) const
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
template<class T>
PlainColumn* ArrayColumnDesc<T>::makeColumn (ColumnSet* csp) const
{
    PlainColumn* bcp = new ArrayColumnData<T> (this, csp);
    return bcp;
}


} //# NAMESPACE CASACORE - END


#endif
