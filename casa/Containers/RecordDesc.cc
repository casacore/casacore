//# RecordDesc.cc: Description of the fields in a Record object
//# Copyright (C) 1995,1996,1998,2001
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

#include <casacore/casa/Containers/RecordDesc.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/IO/ArrayIO.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

ostream& RecordDesc::put (ostream &os) const
{
    int32_t i;
    // There must be a better way to handle indentation, e.g. a manipulator.
    // If control leaves abnormally the indentation might be wrong.
    static int32_t indentLevel = -1;
    indentLevel++;
    String indentation;
    for (i=0; i < indentLevel*4; i++) {
	indentation += " ";
    }

    int32_t n = nfields();
    for (i=0; i < n; i++) {
	os << indentation << i << "  "  << name(i) << " : ";
	if (isSubRecord(i)) {
	    os << "SUBRECORD" << endl;
	    os << subRecord(i);
	} else if (isTable(i)) {
	    os << "TableDesc " << tableDescName(i) << endl;
	} else {
	    os << type(i);
	    if (isArray(i)) {
		os << shape(i);
	    }
	    os << endl;
	}
	if (! comment(i).empty()) {
	    os << indentation << "    " << comment(i) << endl;
	}
    }
    indentLevel--;
    return os;
}

AipsIO& RecordDesc::put (AipsIO& os) const
{
    os.putstart ("RecordDesc", 2);              // version 2
    int32_t n = nfields();
    os << n;
    for (int32_t i=0; i<n; i++) {
	os << name(i);
	os << int32_t(type(i));
	if (isSubRecord(i)) {
	    os << subRecord(i);
	} else if (isArray(i)) {
	    os << shape(i);
	} else if (isTable(i)) {
	    os << tableDescName(i);
	}
	os << comment(i);
    }
    os.putend();
    return os;
}

AipsIO& RecordDesc::get (AipsIO& os)
{
    uint32_t version = os.getstart ("RecordDesc");
    // Clear the description.
    *this = RecordDesc();
    int32_t n;
    String name, descName, comment;
    int32_t type;
    IPosition shape;
    RecordDesc sub;
    os >> n;
    for (int32_t i=0; i<n; i++) {
	os >> name;
	os >> type;
	switch (type) {
	case TpRecord:
	    os >> sub;
	    addField (name, sub);
	    break;
	case TpTable:
	    os >> descName;
	    addTable (name, descName);
	    break;
	case TpArrayBool:
	case TpArrayChar:
	case TpArrayUChar:
	case TpArrayShort:
	case TpArrayUShort:
	case TpArrayInt:
	case TpArrayUInt:
	case TpArrayInt64:
	case TpArrayFloat:
	case TpArrayDouble:
	case TpArrayComplex:
	case TpArrayDComplex:
	case TpArrayString:
	    os >> shape;
	    addField (name, DataType(type), shape);
	    break;
	default:
	    addField (name, DataType(type));
	}
	if (version > 1) {
	    os >> comment;
	    setComment (i, comment);
	}
    }
    os.getend();
    return os;
}

} //# NAMESPACE CASACORE - END

