//# SDFITSHandler.cc: fills all otherwise unhandled columns for the SDFITS filler
//# Copyright (C) 2000,2001
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
#include <casacore/msfits/MSFits/SDFITSHandler.h>

#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/DataMan/StandardStMan.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/DataMan/StandardStMan.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/fits/FITS/CopyRecord.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/measures/Measures/MCEpoch.h>
#include <casacore/measures/TableMeasures/TableMeasDesc.h>
#include <casacore/measures/TableMeasures/TableQuantumDesc.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

SDFITSHandler::SDFITSHandler() 
    : tab_p(0), copier_p(0)
{;}

SDFITSHandler::SDFITSHandler(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row)
    : tab_p(0), copier_p(0)
{
    initAll(ms, handledCols, row);
}

SDFITSHandler::SDFITSHandler(const SDFITSHandler &other) 
    : tab_p(0), copier_p(0)
{
    *this = other;
}

SDFITSHandler &SDFITSHandler::operator=(const SDFITSHandler &other)
{
    if (this != &other) {
	clearAll();
	tab_p = new Table(*(other.tab_p));
	AlwaysAssert(tab_p, AipsError);
	timeMeas_p.attach(*tab_p, "TIME");
	intervalQuant_p.attach(*tab_p, "INTERVAL");
	copier_p = new CopyRecordToTable(*other.copier_p);
    }
    return *this;
}

void SDFITSHandler::attach(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row)
{
    clearAll();
    initAll(ms, handledCols, row);
}

void SDFITSHandler::fill(const Record &, const MEpoch &time, const Double &interval)
{
    // don't bother unless there is something there
    if (tab_p) {
	// fill it
	Int rownr = tab_p->nrow();
	tab_p->addRow();
	timeMeas_p.put(rownr, time);
	intervalQuant_p.put(rownr, Quantity(interval,"s"));
	copier_p->copy(rownr);
    }
}

void SDFITSHandler::clearAll()
{
    delete tab_p;
    tab_p = 0;
    
    clearRow();
}

void SDFITSHandler::clearRow()
{
    delete copier_p;
    copier_p = 0;
}

void SDFITSHandler::initAll(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row)
{
    // static_cast is a workaround for an SGI compiler bug! wky 2000/11/02
    // don't bother unless there are some unhandled columns
    if (anyEQ(static_cast<Vector<Bool> >(handledCols), False)) {
	Vector<String> colNames;
	TableDesc td = requiredTableDesc(handledCols, colNames, row);
	// is there already an SDFITS table, or is one needed
	if (ms.keywordSet().fieldNumber("NS_SDFITS") >= 0 &&
	    ms.keywordSet().dataType("NS_SDFITS") == TpTable) {
	    tab_p = new Table(ms.keywordSet().asTable("NS_SDFITS"));
	    AlwaysAssert(tab_p, AipsError);
	    // only add columns not already in tab_p
	    Vector<String> colNames(td.columnNames());
	    for (uInt i=0;i<colNames.nelements();i++) {
		if (!tab_p->tableDesc().isColumn(colNames(i))) {
		    td.addColumn(td[i]);
		}
	    }
	} else {
	    // create a new table and attach it to the MAIN ms table
	    SetupNewTable newtab(ms.tableName() + "/NS_SDFITS",
				  td, Table::New);
	    StandardStMan stman;
	    newtab.bindAll(stman);
	    tab_p = new Table(newtab);
	    AlwaysAssert(tab_p, AipsError);
	    ms.rwKeywordSet().defineTable("NS_SDFITS", *tab_p);
	}
	// attach the TIME and INTERVAL measure columns
	timeMeas_p.attach(*tab_p, "TIME");
	intervalQuant_p.attach(*tab_p, "INTERVAL");
    
	initRow(handledCols, colNames, row);
    }
}

void SDFITSHandler::initRow(Vector<Bool> &handledCols, const Vector<String> &colNames, const Record &row)
{
    // need to get the mapping between row fields and tab columns
    // there are always the same number of elements in handledCols 
    // as there are fields in row
    Vector<Int> fieldMap(handledCols.nelements(),-1);
    for (uInt i=0;i<colNames.nelements();i++) {
	Int field = row.fieldNumber(colNames(i));
	if (field >= 0) {
	    fieldMap(field) = i;
	    handledCols(field) = True;
	}
    }
    copier_p = new CopyRecordToTable(*tab_p, row, fieldMap);
    AlwaysAssert(copier_p, AipsError);
}

TableDesc SDFITSHandler::requiredTableDesc(Vector<Bool> &handledCols, Vector<String> &colNames, 
					   const Record &row)
{
    // build a TableDesc using row and any un-handled columns
    TableDesc td;
    Regex sdfPrefix("^NS_SDFITS_");
    Regex sdfPrefixMatch("^NS_SDFITS_.*");
    colNames.resize(handledCols.nelements());
    colNames = "";
    uInt colCount = 0;
    for (uInt i=0;i<handledCols.nelements();i++) {
	if (!handledCols(i)) {
	    // construct the output name
	    // its the input name unless it starts with NS_SDFITS_ in which case its
	    // everything after the NS_SDFITS_
	    String colName = row.name(i);
	    if (colName.matches(sdfPrefixMatch)) {
		colName = colName.after(sdfPrefix);
	    } 
	    // ignore any TIME and INTERVAL here
	    if (colName == "TIME" || colName == "INTERVAL") {
		handledCols(i) = True;
	    } else {
		colNames(colCount++) = row.name(i);
		switch (row.type(i)) {
		case TpBool:
		    td.addColumn(ScalarColumnDesc<Bool>(colName));
		    break;
		case TpUChar:
		    td.addColumn(ScalarColumnDesc<uChar>(colName));
		    break;
		case TpShort:
		    td.addColumn(ScalarColumnDesc<Short>(colName));
		    break;
		case TpInt:
		    td.addColumn(ScalarColumnDesc<Int>(colName));
		    break;
		case TpFloat:
		    td.addColumn(ScalarColumnDesc<Float>(colName));
		    break;
		case TpDouble:
		    td.addColumn(ScalarColumnDesc<Double>(colName));
		    break;
		case TpComplex:
		    td.addColumn(ScalarColumnDesc<Complex>(colName));
		    break;
		case TpDComplex:
		    td.addColumn(ScalarColumnDesc<DComplex>(colName));
		    break;
		case TpString:
		    td.addColumn(ScalarColumnDesc<String>(colName));
		    break;
		case TpArrayBool:
		    td.addColumn(ArrayColumnDesc<Bool>(colName));
		    break;
		case TpArrayUChar:
		    td.addColumn(ArrayColumnDesc<uChar>(colName));
		    break;
		case TpArrayShort:
		    td.addColumn(ArrayColumnDesc<Short>(colName));
		    break;
		case TpArrayInt:
		    td.addColumn(ArrayColumnDesc<Int>(colName));
		    break;
		case TpArrayFloat:
		    td.addColumn(ArrayColumnDesc<Float>(colName));
		    break;
		case TpArrayDouble:
		    td.addColumn(ArrayColumnDesc<Double>(colName));
		    break;
		case TpArrayComplex:
		    td.addColumn(ArrayColumnDesc<Complex>(colName));
		    break;
		case TpArrayDComplex:
		    td.addColumn(ArrayColumnDesc<DComplex>(colName));
		    break;
		case TpArrayString:
		    td.addColumn(ArrayColumnDesc<String>(colName));
		    break;
		default:
		    LogIO os;
		    os << LogIO::SEVERE << WHERE 
		       << "Field " << colName 
		       << " has an invalid type, " 
		       << Int(row.type(i))
		       << " -- this should never happen."
		       << LogIO::EXCEPTION;
		    break;
		}
	    }
	}
    }
    colNames.resize(colCount, True);
    // add the TIME and INTERVAL columns
    // TIME is an MEpoch column
    td.addColumn(ScalarColumnDesc<Double>("TIME"));
    TableMeasDesc<MEpoch> measCol(TableMeasValueDesc(td,"TIME"),
				  TableMeasRefDesc(MEpoch::DEFAULT));
    measCol.write(td);
    // and change the units to "s" from the default of "d"
    TableQuantumDesc timeqd(td,"TIME",Unit("s"));
    timeqd.write(td);
    // INTERVAL is a Quantity column
    td.addColumn(ScalarColumnDesc<Double>("INTERVAL"));
    TableQuantumDesc quantCol(td, "INTERVAL", Unit("s"));
    quantCol.write(td);

    return td;
}

} //# NAMESPACE CASACORE - END

