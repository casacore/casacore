//# TableMeasRefDef.cc: Definition of a MeasRef in a Table.
//# Copyright (C) 1997,1999
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
#include <aips/Measures/MRBase.h>
#include <trial/TableMeasures/TableMeasDescBase.h>
#include <trial/TableMeasures/TableMeasOffsetDesc.h>
#include <trial/TableMeasures/TableMeasRefDesc.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Exceptions/Error.h>

TableMeasRefDesc::TableMeasRefDesc(uInt referenceCode)
: itsRefCode(referenceCode),
  itsOffset(0)
{}

TableMeasRefDesc::TableMeasRefDesc(uInt referenceCode, 
	    	    	    	   const TableMeasOffsetDesc& offset)
: itsRefCode(referenceCode),
  itsOffset(new TableMeasOffsetDesc(offset))
{}

TableMeasRefDesc::TableMeasRefDesc(const TableDesc &td, const String& column)
: itsRefCode(0),
  itsColumn(column),
  itsOffset(0)
{
    checkColumn(td);
}

TableMeasRefDesc::TableMeasRefDesc(const TableDesc &td, const String& column,
	    	    	    	   const TableMeasOffsetDesc& offset)
: itsRefCode(0),
  itsColumn(column),
  itsOffset(new TableMeasOffsetDesc(offset))
{
    checkColumn(td);
}

TableMeasRefDesc::TableMeasRefDesc(const TableMeasRefDesc& that)
: itsRefCode(that.itsRefCode),
  itsColumn(that.itsColumn),
  itsOffset(that.itsOffset)
{
    if (itsOffset != 0) {
	itsOffset = new TableMeasOffsetDesc(*itsOffset);
    }
}
    
TableMeasRefDesc& TableMeasRefDesc::operator= (const TableMeasRefDesc& that)
{
    if (this != &that) {
	delete itsOffset;
	itsRefCode = that.itsRefCode;
	itsColumn = that.itsColumn;
	itsOffset  = that.itsOffset;
	if (itsOffset != 0) {
	    itsOffset = new TableMeasOffsetDesc(*itsOffset);
	}
    }
    return *this;
}

TableMeasRefDesc::~TableMeasRefDesc()
{
    delete itsOffset;
}

TableMeasRefDesc::TableMeasRefDesc(const TableRecord& measInfo,
    	    	    	    	   const Table& tab,
				   const TableMeasDescBase& mDesc,
    	    	    	    	   const String& refString)
: itsOffset(0)
{
    itsOffset = TableMeasOffsetDesc::reconstruct(measInfo, "RefOff", tab);
    if (refString == "variable") {
    	Int fnr;
	fnr = measInfo.fieldNumber("VarRefCol");
	if (fnr >= 0) {
	    itsColumn = measInfo.asString(fnr);
	}
    } else {
	itsRefCode = mDesc.refCode(refString);
    }

}

TableMeasRefDesc* TableMeasRefDesc::reconstruct(const TableRecord& measInfo,
				    	        const Table& tab,
				    	      	const TableMeasDescBase& mDesc)
{
    TableMeasRefDesc* P = 0;
    String refString;    
    Int fnr;
    fnr = measInfo.fieldNumber("Ref");
    if (fnr >= 0) {
	refString = measInfo.asString(fnr);
    	P = new TableMeasRefDesc(measInfo, tab, mDesc, refString);
    } else {
	// set up TableMeasRefDesc with a default non-variable reference
	P = new TableMeasRefDesc(0);
    }
    
    return P;
}

void TableMeasRefDesc::write(TableDesc& td, TableRecord& measInfo, 
			     const TableMeasDescBase& measDesc)
{
    String refString;
    if (isVariable()) {
    	refString = "variable";
	measInfo.define("Ref", "variable");
	measInfo.define("VarRefCol", itsColumn);
    } else {
    	refString = measDesc.refType(itsRefCode);
    	measInfo.define("Ref", refString);
    }
      
    if (itsOffset != 0) {
	itsOffset->write(td, measInfo, "RefOff");
    }
}

void TableMeasRefDesc::checkColumn(const TableDesc& td) const
{
    if (td.isColumn(itsColumn) == False) {
    	throw(AipsError("TableMeasRefDesc::checkColumn; No such column: "
            	    	 + itsColumn));
    } else if ((td.columnDesc(itsColumn).dataType() != TpInt) &&
	       (td.columnDesc(itsColumn).dataType() != TpString)) {
    	throw(AipsError("TableMeasRefDesc::checkColumn; Reference column's "
			"type must be Int or String: " + itsColumn));
    }

}

