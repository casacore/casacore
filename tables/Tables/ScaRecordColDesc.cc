//# ScaRecordColDesc.cc: Class for description of table scalar record columns
//# Copyright (C) 1998,2001
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

#include <casacore/tables/Tables/ScaRecordColDesc.h>
#include <casacore/tables/Tables/ScaRecordColData.h>
#include <casacore/casa/Utilities/ValTypeId.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/iostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

ScalarRecordColumnDesc::ScalarRecordColumnDesc (const String& name)
: BaseColumnDesc (name, "", "", "",
		  TpRecord, "TableRecord", 0,
		  0, IPosition(), True, False, False)
{}

ScalarRecordColumnDesc::ScalarRecordColumnDesc (const String& name,
						const String& comment)
: BaseColumnDesc (name, comment, "", "",
		  TpRecord, "TableRecord", 0,
		  0, IPosition(), True, False, False)
{}

ScalarRecordColumnDesc::ScalarRecordColumnDesc (const String& name,
						const String& comment,
						const String& dataManName,
						const String& dataManGroup)
: BaseColumnDesc (name, comment, dataManName, dataManGroup,
		  TpRecord, "TableRecord", 0,
		  0, IPosition(), True, False, False)
{}
  
ScalarRecordColumnDesc::ScalarRecordColumnDesc
                                          (const ScalarRecordColumnDesc& that)
: BaseColumnDesc (that)
{}

// Make a new object.
BaseColumnDesc* ScalarRecordColumnDesc::makeDesc (const String&)
{
    return new ScalarRecordColumnDesc("");
}

ScalarRecordColumnDesc::~ScalarRecordColumnDesc()
{}


ScalarRecordColumnDesc& ScalarRecordColumnDesc::operator=
                                          (const ScalarRecordColumnDesc& that)
{
    BaseColumnDesc::operator= (that);
    return *this;
}

// Clone this column description to another.
BaseColumnDesc* ScalarRecordColumnDesc::clone() const
{
    return new ScalarRecordColumnDesc(*this);
}


// Return the class name.
String ScalarRecordColumnDesc::className() const
{
    return "ScalarRecordColumnDesc";
}


// Put the object.
// The data is read by the ctor taking AipsIO.
// It was felt that putstart takes too much space, so therefore
// the version is put "manually".
void ScalarRecordColumnDesc::putDesc (AipsIO& ios) const
{
    ios << (uInt)1;                  // class version 1
}

void ScalarRecordColumnDesc::getDesc (AipsIO& ios)
{
    uInt version;
    ios >> version;
}


// Show the column.
void ScalarRecordColumnDesc::show (ostream& os) const
{
    os << "   Name=" << name();
    os << "   DataType=" << dataType();
    os << endl;
    os << "   DataManager=" << dataManagerType();
    os << "/" << dataManagerGroup();
    os << endl;
    os << "   Comment = " << comment() << endl;
}


// Create a column object from the description.
PlainColumn* ScalarRecordColumnDesc::makeColumn (ColumnSet* csp) const
{
    return new ScalarRecordColumnData (this, csp);
}

} //# NAMESPACE CASACORE - END

