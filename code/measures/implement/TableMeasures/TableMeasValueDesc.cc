//# TableMeasValueDesc.cc: Definition of a MeasValue in a Table.
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
#include <aips/Exceptions/Error.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/ColumnDesc.h>
#include <aips/Tables/TableRecord.h>
#include <trial/TableMeasures/TableMeasValueDesc.h>

TableMeasValueDesc::TableMeasValueDesc()
{}

TableMeasValueDesc::TableMeasValueDesc(const TableDesc& td, 
    	    	    	    	       const String& colName)
: itsColumn(colName)
{
    checkColumn(td);
}

TableMeasValueDesc::TableMeasValueDesc(const TableMeasValueDesc& that)
: itsColumn(that.itsColumn)
{}

TableMeasValueDesc::~TableMeasValueDesc()
{}

TableMeasValueDesc& TableMeasValueDesc::operator=(const TableMeasValueDesc& 
    	    	    	    	    	    	  that)
{
    if (this != &that) {
	itsColumn = that.itsColumn;
    }
    return *this;
}

    
void TableMeasValueDesc::write(TableDesc& td, const TableRecord& measInfo)
{
    TableRecord& columnKeyset = td.rwColumnDesc(itsColumn).rwKeywordSet();

    if (measInfo.nfields() > 0) {
	columnKeyset.defineRecord("MEASINFO", measInfo);
    }
}

void TableMeasValueDesc::checkColumn(const TableDesc& td) const
{
    if (td.isColumn(itsColumn) == False) {
    	throw(AipsError("TableMeasValueDesc::checkColumn; No such column: "
            	    	 + itsColumn));
    } else if (!td.columnDesc(itsColumn).isArray()) {
    	throw(AipsError("TableMeasValueDesc::checkColumn; MeasValue column "
		    	"must be an array: " + itsColumn));
    } else if (td.columnDesc(itsColumn).dataType() != TpDouble) {
    	throw(AipsError("TableMeasValueDesc::checkColumn; Column's type "
		    	"must be Double: " + itsColumn));
    }

}

