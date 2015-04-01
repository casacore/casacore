//# ScaColDesc.cc: Templated class for description of table scalar columns
//# Copyright (C) 1994,1995,1996,1997,1998,2001
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

#ifndef TABLES_SCACOLDESC_TCC
#define TABLES_SCACOLDESC_TCC

#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ScaColData.h>
#include <casacore/tables/Tables/ConcatScalarColumn.h>
#include <casacore/casa/Utilities/ValTypeId.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/iostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
ScalarColumnDesc<T>::ScalarColumnDesc (const String& name,
				       int opt)
: BaseColumnDesc (name, "", "", "",
		  ValType::getType(&defaultVal_p),
		  valDataTypeId(&defaultVal_p),
		  opt, 0, IPosition(), True, False, False)
{
    defaultVal_p = T();
}

template<class T>
ScalarColumnDesc<T>::ScalarColumnDesc (const String& name,
				       const String& comment,
				       int opt)
: BaseColumnDesc (name, comment, "", "",
		  ValType::getType(&defaultVal_p),
		  valDataTypeId(&defaultVal_p),
		  opt, 0, IPosition(), True, False, False)
{
    defaultVal_p = T();
}

template<class T>
ScalarColumnDesc<T>::ScalarColumnDesc (const String& name,
				       const String& comment,
				       const String& dataManName,
				       const String& dataManGroup,
				       int opt)
: BaseColumnDesc (name, comment, dataManName, dataManGroup,
		  ValType::getType(&defaultVal_p),
		  valDataTypeId(&defaultVal_p),
		  opt, 0, IPosition(), True, False, False)
{
    defaultVal_p = T();
}
  
template<class T>
ScalarColumnDesc<T>::ScalarColumnDesc (const String& name,
				       const String& comment,
				       const String& dataManName,
				       const String& dataManGroup,
				       const T& defaultVal,
				       int opt)
: BaseColumnDesc (name, comment, dataManName, dataManGroup,
		  ValType::getType(&defaultVal_p),
		  valDataTypeId(&defaultVal_p),
		  opt, 0, IPosition(), True, False, False),
  defaultVal_p   (defaultVal)
{}
  
template<class T>
ScalarColumnDesc<T>::ScalarColumnDesc (const ScalarColumnDesc<T>& that)
: BaseColumnDesc (that),
  defaultVal_p   (that.defaultVal_p)
{}

// Make a new object.
template<class T>
BaseColumnDesc* ScalarColumnDesc<T>::makeDesc (const String&)
{
  return new ScalarColumnDesc<T>(String());
}

template<class T>
ScalarColumnDesc<T>::~ScalarColumnDesc()
{}


template<class T>
ScalarColumnDesc<T>& ScalarColumnDesc<T>::operator= (
           const ScalarColumnDesc<T>& that)
{
    BaseColumnDesc::operator= (that);
    defaultVal_p = that.defaultVal_p;
    return *this;
}

// Clone this column description to another.
template<class T>
BaseColumnDesc* ScalarColumnDesc<T>::clone() const
{
    BaseColumnDesc* ptr = new ScalarColumnDesc<T>(*this);
    return ptr;
}


// Return the class name.
template<class T>
String ScalarColumnDesc<T>::className() const
    { return "ScalarColumnDesc<" + dataTypeId(); }

//# Register the makeDesc function.
template<class T>
void ScalarColumnDesc<T>::registerClass() const
{
  ColumnDesc::registerCtor (className(), makeDesc);
}


// Put the object.
// The data is read by the ctor taking AipsIO.
// It was felt that putstart takes too much space, so therefore
// the version is put "manually".
template<class T>
void ScalarColumnDesc<T>::putDesc (AipsIO& ios) const
{
    ios << (uInt)1;                  // class version 1
    ValType::put (ios, &defaultVal_p);
}

template<class T>
void ScalarColumnDesc<T>::getDesc (AipsIO& ios)
{
    uInt version;
    ios >> version;
    ValType::get (ios, &defaultVal_p);
}


// Show the column.
template<class T>
void ScalarColumnDesc<T>::show (ostream& os) const
{
    os << "   Name=" << name();
    os << "   DataType=" << dataType();
    if (dataType() == TpOther) {
	os << ", " << dataTypeId();
    }
    if (maxLength() > 0) {
	os << "   MaxLength=" << maxLength();
    }
    os << endl;
    os << "   DataManager=" << dataManagerType();
    os << "/" << dataManagerGroup();
    os << "   Default=";
    ValType::put (os, &defaultVal_p);
    os << endl;
    os << "   Comment = " << comment() << endl;
}


// Create a column object from the description.
template<class T>
PlainColumn* ScalarColumnDesc<T>::makeColumn (ColumnSet* csp) const
{
    PlainColumn* bcp = new ScalarColumnData<T> (this, csp);
    return bcp;
}

template<class T>
ConcatColumn* ScalarColumnDesc<T>::makeConcatColumn (ConcatTable* ct) const
{
    return new ConcatScalarColumn<T> (this, ct);
}

} //# NAMESPACE CASACORE - END


#endif
