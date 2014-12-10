//# TableMeasOffsetDesc.cc: Definition of a offset measure in a Table.
//# Copyright (C) 1997,1998,1999,2000,2001
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
#include <casacore/measures/TableMeasures/TableMeasOffsetDesc.h>
#include <casacore/measures/TableMeasures/TableMeasDescBase.h>
#include <casacore/measures/TableMeasures/TableMeasDesc.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Exceptions.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

TableMeasOffsetDesc::TableMeasOffsetDesc (const TableMeasDescBase& column,
					  Bool asArray)
: itsTMDesc(column.clone()),
  itsVarPerArr(asArray)
{}

TableMeasOffsetDesc::TableMeasOffsetDesc (const Measure& measure)
: itsTMDesc(0),
  itsMeasure(measure),
  itsVarPerArr(False)
{}

TableMeasOffsetDesc::TableMeasOffsetDesc (const TableMeasOffsetDesc& that)
: itsTMDesc(0)
{
  *this = that;
}
    
TableMeasOffsetDesc::~TableMeasOffsetDesc()
{
  delete itsTMDesc;
}

TableMeasOffsetDesc* TableMeasOffsetDesc::reconstruct(
	    	    	    	    	    const TableRecord& measInfo,
				    	    const String& prefix,
				    	    const Table& tab)
{
  TableMeasOffsetDesc* p = 0;
  if ((measInfo.fieldNumber(prefix + "Msr") >= 0)
  ||  (measInfo.fieldNumber(prefix + "Col") >= 0)) {
    p = new TableMeasOffsetDesc(measInfo, prefix, tab);
  }
  return p;
}

TableMeasOffsetDesc::TableMeasOffsetDesc (const TableRecord& measInfo,
					  const String& prefix,
					  const Table& tab)
: itsTMDesc(0)
{
  Int fnr;
  fnr = measInfo.fieldNumber(prefix + "Msr");
  if (fnr >= 0) {
    // this is a non-variable offset.  The offset is fully defined in the
    // column keywords.
    String error;
    const TableRecord& measRec = measInfo.subRecord(fnr);
    if (! itsMeasure.fromRecord (error, measRec)) {
      throw(AipsError("TableMeasOffsetDesc::TableMeasOffsetDesc() " + error));
    }
  }
  // offset stored in a a measure column
  fnr = measInfo.fieldNumber(prefix + "Col");
  if (fnr >= 0) {
    itsVarColName = measInfo.asString(fnr);
    itsTMDesc = TableMeasDescBase::reconstruct(tab, itsVarColName);
  }
  // offset stored per array element
  fnr = measInfo.fieldNumber(prefix + "varPerArr");
  if (fnr >= 0) {
    itsVarPerArr = measInfo.asBool(fnr);
  }
}

TableMeasOffsetDesc& TableMeasOffsetDesc::operator=
                                           (const TableMeasOffsetDesc& that)
{
  if (this != &that) {
    delete itsTMDesc;
    itsTMDesc  = that.itsTMDesc;
    itsMeasure = that.itsMeasure;
    itsVarColName = that.itsVarColName;
    itsVarPerArr  = that.itsVarPerArr;
     if (itsTMDesc != 0) {
      itsTMDesc = itsTMDesc->clone();
    }
  }
  return *this;
}

const Measure& TableMeasOffsetDesc::getOffset() const
{
  if (isVariable()) {
    throw (AipsError ("TableMeasOffsetDesc::getOffset() "
		      "attempt to reference undefined measure offset."));
  }
  return itsMeasure.asMeasure();
}

void TableMeasOffsetDesc::write (TableDesc& td, TableRecord& measInfo, 
				 const String& prefix)
{
  writeKeys (measInfo, prefix);
  if (itsTMDesc != 0) {
    itsTMDesc->write (td);
  }
}

void TableMeasOffsetDesc::write (Table& tab, TableRecord& measInfo, 
				 const String& prefix)
{
  writeKeys (measInfo, prefix);
  if (itsTMDesc != 0) {
    itsTMDesc->write (tab);
  }
}

void TableMeasOffsetDesc::writeKeys (TableRecord& measInfo, 
				     const String& prefix)
{
  if (! itsMeasure.isEmpty()) {
    String error;
    TableRecord measRec;
    itsMeasure.toRecord (error, measRec);
    measInfo.defineRecord (prefix + "Msr", measRec);
  }
  if (itsTMDesc != 0) {
    measInfo.define (prefix + "Col", itsTMDesc->columnName());
    measInfo.define (prefix + "varPerArr", itsVarPerArr);
  }
}

void TableMeasOffsetDesc::resetOffset (const Measure& offset)
{
  if (isVariable()) {
    throw (AipsError ("tableMeasOffsetDesc::resetOffset cannot be done;"
		      "the offset is not fixed for the entire column"));
  }
  itsMeasure = MeasureHolder(offset);
}

} //# NAMESPACE CASACORE - END

