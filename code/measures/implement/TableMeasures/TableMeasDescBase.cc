//# TableMeasDefBase.cc: Definition of a Measure in a Table.
//# Copyright (C) 1997,1998
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
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MDoppler.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MRadialVelocity.h>
#include <aips/Measures/MBaseline.h>
#include <aips/Measures/Muvw.h>
#include <aips/Measures/MEarthMagnetic.h>
#include <trial/TableMeasures/TableMeasDesc.h>
#include <trial/TableMeasures/TableMeasDescBase.h>
#include <trial/TableMeasures/TableMeasRefDesc.h>
#include <aips/Tables/ColumnDesc.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableColumn.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableRecord.h>

TableMeasDescBase::TableMeasDescBase()
: itsRef(0)
{}

TableMeasDescBase::TableMeasDescBase(const TableMeasValueDesc& value)
: itsValue(value),
  itsRef(0)
{}

TableMeasDescBase::TableMeasDescBase(const TableMeasValueDesc& value,
				     const TableMeasRefDesc& ref)
: itsValue(value),
  itsRef(new TableMeasRefDesc(ref))
{}

TableMeasDescBase::TableMeasDescBase(const TableMeasDescBase& that)
: itsValue(that.itsValue),
  itsRef(that.itsRef)
{
    if (itsRef != 0) {
    	itsRef = new TableMeasRefDesc(*itsRef);
    }
}

TableMeasDescBase::~TableMeasDescBase()
{
    delete itsRef;
}

TableMeasDescBase* TableMeasDescBase::reconstruct(const Table& tab, 
    	    	    	    	    	    	  const String& columnName)
{
    Int fnr;
    String mtype;
    TableMeasDescBase* P=0;
    TableRecord measInfo;
    const TableRecord& columnKeyset = tab.tableDesc()[columnName].keywordSet();
    
    fnr = columnKeyset.fieldNumber("MEASINFO");
    if (fnr >= 0) {
        measInfo = columnKeyset.asRecord(fnr);
    	mtype = measInfo.asString("Type");    	
    } else {
        throw(AipsError("TableMeasDescBase::reconstruct; MEASINFO record not "
            	        "found for column " + columnName));
    }
    
    if (mtype == "Epoch") {
    	P = new TableMeasDesc<MEpoch>();
    } else if (mtype == "Position") {
    	P = new TableMeasDesc<MPosition>();
    } else if (mtype == "Direction") {
    	P = new TableMeasDesc<MDirection>();
    } else if (mtype == "Radialvelocity") {
    	P = new TableMeasDesc<MRadialVelocity>();
    } else if (mtype == "Doppler") {
    	P = new TableMeasDesc<MDoppler>();
    } else if (mtype == "Frequency") {
    	P = new TableMeasDesc<MFrequency>();
    } else if (mtype == "Baseline") {
    	P = new TableMeasDesc<MBaseline>();
    } else if (mtype == "uvw") {
    	P = new TableMeasDesc<Muvw>();
    } else if (mtype == "EarthMagnetic") {
    	P = new TableMeasDesc<MEarthMagnetic>();
    } else {
        throw(AipsError("TableMeasDescBase::reconstruct; unknown Measure type " 
            	    	+ mtype));
    }

    // create its TableMeasRefDesc if it exists 
    P->itsRef = TableMeasRefDesc::reconstruct(measInfo, tab, *P);
    
    return(P);
}

TableMeasDescBase& TableMeasDescBase::operator= (const TableMeasDescBase& that)
{
    if (this != &that) {
	itsValue = that.itsValue;
	delete itsRef;
	itsRef = that.itsRef;
	if (itsRef != 0) {
	    itsRef = new TableMeasRefDesc (*itsRef);
	}
    }
    return *this;
}
    
void TableMeasDescBase::write(TableDesc& td)
{
    TableRecord measInfo;
    
    measInfo.define("Type", type());
    if (itsRef != 0) {
	itsRef->write(td, measInfo, *this);
    }
    itsValue.write(td, measInfo);
}

