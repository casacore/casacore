//# TableMeasOffsetDesc.cc: Definition of a offset measure in a Table.
//# Copyright (C) 1997,1998,1999
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
#include <aips/Exceptions.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MDoppler.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MRadialVelocity.h>
#include <aips/Measures/MBaseline.h>
#include <aips/Measures/Muvw.h>
#include <aips/Measures/MEarthMagnetic.h>
#include <aips/Measures/Measure.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableRecord.h>
#include <trial/TableMeasures/TableMeasOffsetDesc.h>
#include <trial/TableMeasures/TableMeasDescBase.h>
#include <trial/TableMeasures/TableMeasDesc.h>

TableMeasOffsetDesc::TableMeasOffsetDesc(const TableMeasDescBase& column,
					 const Bool asArray)
: itsTMDesc(column.clone()),
  itsMeasure(0),
  itsVarPerArr(asArray)
{}

TableMeasOffsetDesc::TableMeasOffsetDesc(const Measure& measure)
: itsTMDesc(0),
  itsMeasure(measure.clone()),
  itsVarPerArr(False)
{}

TableMeasOffsetDesc::TableMeasOffsetDesc(const TableMeasOffsetDesc& that)
: itsTMDesc(0),
  itsMeasure(0)
{
    *this = that;
}
    
TableMeasOffsetDesc::~TableMeasOffsetDesc()
{
    delete itsTMDesc;
    delete itsMeasure;
}

TableMeasOffsetDesc* TableMeasOffsetDesc::reconstruct(
	    	    	    	    	    const TableRecord& measInfo,
				    	    const String& prefix,
				    	    const Table& tab)
{
    TableMeasOffsetDesc* P = 0;
    
    if ((measInfo.fieldNumber(prefix + "Msr") >= 0) || 
    	(measInfo.fieldNumber(prefix + "Col") >= 0)) {
   	P = new TableMeasOffsetDesc(measInfo, prefix, tab);
    }
    
    return P;
}

TableMeasOffsetDesc::TableMeasOffsetDesc(const TableRecord& measInfo,
				     	 const String& prefix,
				     	 const Table& tab)
: itsTMDesc(0),
  itsMeasure(0)
{
    Int fnr;
    fnr = measInfo.fieldNumber(prefix + "Msr");
    if (fnr >= 0) {
	// this is a non-variable offset.  The offset is fully defined in the
	// column keywords.
    	const TableRecord& measRec = measInfo.subRecord(fnr);
	fnr = measRec.fieldNumber("mRef");
	uInt measRef = 0;
	if (fnr >= 0) {
	    measRef = measRec.asuInt(fnr);
	} else {
    	    throw(AipsError("TableMeasOffsetDesc::TableMeasOffsetDesc() "
    	    	"measure reference type unknown: " +
    	    	measRec.fieldNumber("mRef")));
	}
	fnr = measRec.fieldNumber("mVal");
    	Vector<Double> measVal;
	if (fnr >= 0) {
	    measVal = measRec.asArrayDouble(fnr);
	} else {
    	    throw(AipsError("TableMeasOffsetDesc::TableMeasOffsetDesc() "
    	    	"measure value not found: " +
    	    	measRec.fieldNumber("mVal")));
	}
    	fnr = measRec.fieldNumber("mType");
    	if (measRec.asString(fnr) == "Epoch") {
	    MVEpoch meval;
	    meval.putVector(measVal);
    	    itsMeasure = new MEpoch(meval, measRef);
    	} else if (measRec.asString(fnr) == "Position") {
	    MVPosition meval;
	    meval.putVector(measVal);
    	    itsMeasure = new MPosition(meval, measRef);
    	} else if (measRec.asString(fnr) == "Direction") {
	    MVDirection meval;
	    meval.putVector(measVal);
    	    itsMeasure = new MDirection(meval, measRef);
    	} else if (measRec.asString(fnr) == "Frequency") {
	    MVFrequency meval;
	    meval.putVector(measVal);
    	    itsMeasure = new MFrequency(meval, measRef);
    	} else if (measRec.asString(fnr) == "Doppler") {
	    MVDoppler meval;
	    meval.putVector(measVal);
    	    itsMeasure = new MDoppler(meval, measRef);
    	} else if (measRec.asString(fnr) == "RadialVelocity") {
	    MVRadialVelocity meval;
	    meval.putVector(measVal);
    	    itsMeasure = new MRadialVelocity(meval, measRef);
    	} else if (measRec.asString(fnr) == "Baseline") {
	    MVBaseline meval;
	    meval.putVector(measVal);
    	    itsMeasure = new MBaseline(meval, measRef);
    	} else if (measRec.asString(fnr) == "uvw") {
	    MVuvw meval;
	    meval.putVector(measVal);
    	    itsMeasure = new Muvw(meval, measRef);
    	} else if (measRec.asString(fnr) == "EarthMagnetic") {
	    MVEarthMagnetic meval;
	    meval.putVector(measVal);
    	    itsMeasure = new MEarthMagnetic(meval, measRef);
    	} else {
    	    throw(AipsError("TableMeasOffsetDesc::TableMeasOffsetDesc() "
    	    	"measure type unknown: " + measRec.fieldNumber("mType")));
    	}
    }

    // offset stored in a a measure column
    fnr = measInfo.fieldNumber(prefix + "Col");
    if (fnr >= 0) {
	itsTMDesc = TableMeasDescBase::reconstruct(tab, 
	    	    	    	    	    	   measInfo.asString(fnr));
	itsVarColName = measInfo.asString(fnr);
    }

    // offset stored per array element
    fnr = measInfo.fieldNumber(prefix + "varPerArr");
    if (fnr >= 0) {
	itsVarPerArr = measInfo.asBool(fnr);
    }
}

TableMeasOffsetDesc& TableMeasOffsetDesc::operator=(
    const TableMeasOffsetDesc& that)
{
    if (this != &that) {
	delete itsTMDesc;
	delete itsMeasure;
	itsTMDesc  = that.itsTMDesc;
	itsMeasure = that.itsMeasure;
	itsVarPerArr = that.itsVarPerArr;
	if (itsTMDesc != 0) {
	    itsTMDesc = itsTMDesc->clone();
	}
	if (itsMeasure != 0) {
	    itsMeasure = itsMeasure->clone();
	}
    }
    return *this;
}

const Measure& TableMeasOffsetDesc::getOffset() const
{
    if (isVariable()) {
    	throw(AipsError("TableMeasOffsetDesc::getOffset() "
    	    "attempt to reference undefined measure offset."));
    }
    return *itsMeasure;
}

const String& TableMeasOffsetDesc::columnName() const 
{
    return itsVarColName;
}

void TableMeasOffsetDesc::write(TableDesc& td, TableRecord& measInfo, 
	    	    	    	const String& prefix)
{
    if (itsMeasure != 0) {
    	TableRecord measRec;
    	measRec.define("mType", itsMeasure->tellMe());
	measRec.define("mRef", itsMeasure->getRefPtr()->getType());
	measRec.define("mVal", itsMeasure->getData()->getVector());
        measInfo.defineRecord(prefix + "Msr", measRec);
    }
    if (itsTMDesc != 0) {
	measInfo.define(prefix + "Col", itsTMDesc->columnName());
	measInfo.define(prefix + "varPerArr", itsVarPerArr);
	itsTMDesc->write(td);
    }
}
