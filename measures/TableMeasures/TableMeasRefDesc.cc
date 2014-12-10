//# TableMeasRefDef.cc: Definition of a MeasRef in a Table.
//# Copyright (C) 1997,1999,2000,2001
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
#include <casacore/measures/TableMeasures/TableMeasRefDesc.h>
#include <casacore/measures/TableMeasures/TableMeasDescBase.h>
#include <casacore/measures/Measures/MeasureHolder.h>
#include <casacore/measures/Measures/Measure.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Utilities/LinearSearch.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Set the default type getting function.
TableMeasRefDesc::TypesFunc* TableMeasRefDesc::theirTypesFunc =
                                        TableMeasRefDesc::defaultTypesFunc;


TableMeasRefDesc::TableMeasRefDesc (uInt referenceCode)
: itsRefCode       (referenceCode),
  itsRefCodeColInt (False),
  itsHasRefTab     (True),
  itsOffset        (0)
{}

TableMeasRefDesc::TableMeasRefDesc (uInt referenceCode, 
				    const TableMeasOffsetDesc& offset)
: itsRefCode       (referenceCode),
  itsRefCodeColInt (False),
  itsHasRefTab     (True),
  itsOffset        (new TableMeasOffsetDesc(offset))
{}

TableMeasRefDesc::TableMeasRefDesc (const TableDesc &td, const String& column)
: itsRefCode       (0),
  itsColumn        (column),
  itsRefCodeColInt (False),
  itsHasRefTab     (True),
  itsOffset        (0)
{
  checkColumn (td);
}

TableMeasRefDesc::TableMeasRefDesc (const TableDesc &td, const String& column,
				    const TableMeasOffsetDesc& offset)
: itsRefCode       (0),
  itsColumn        (column),
  itsRefCodeColInt (False),
  itsHasRefTab     (True),
  itsOffset        (new TableMeasOffsetDesc(offset))
{
  checkColumn (td);
}

TableMeasRefDesc::TableMeasRefDesc (const TableMeasRefDesc& that)
: itsOffset(0)
{
  operator= (that);
}
    
TableMeasRefDesc& TableMeasRefDesc::operator= (const TableMeasRefDesc& that)
{
  if (this != &that) {
    delete itsOffset;
    itsRefCode       = that.itsRefCode;
    itsColumn        = that.itsColumn;
    itsRefCodeColInt = that.itsRefCodeColInt;
    itsHasRefTab     = that.itsHasRefTab;
    itsOffset        = that.itsOffset;
    if (itsOffset != 0) {
      itsOffset = new TableMeasOffsetDesc(*itsOffset);
    }
    itsTabRefTypes = that.itsTabRefTypes;
    itsTabRefCodes = that.itsTabRefCodes;
    itsTab2Cur     = that.itsTab2Cur;
    itsCur2Tab     = that.itsCur2Tab;
  }
  return *this;
}

TableMeasRefDesc::~TableMeasRefDesc()
{
  delete itsOffset;
}

TableMeasRefDesc::TableMeasRefDesc (const TableRecord& measInfo,
				    const Table& tab,
				    const MeasureHolder& measHolder,
				    const TableMeasDescBase& mDesc)
: itsRefCode       (0),
  itsRefCodeColInt (False),
  itsHasRefTab     (True),
  itsOffset        (0)
{
  Int fnr;
  fnr = measInfo.fieldNumber("Ref");
  // Read back. The refcode is fixed or variable.
  if (fnr >= 0) {
    itsRefCode = mDesc.refCode (measInfo.asString(fnr));
  }
  fnr = measInfo.fieldNumber("VarRefCol");
  if (fnr >= 0) {
    // Variable refcode.
    itsColumn = measInfo.asString(fnr);
    // See if the refcodes/types are defined in the table.
    // If so, read back. Otherwise initialize with default.
    if (tab.tableDesc().columnDesc(itsColumn).dataType() == TpInt) {
      itsRefCodeColInt = True;
      fnr = measInfo.fieldNumber("TabRefTypes");
      if (fnr >= 0) {
	itsTabRefTypes = measInfo.asArrayString ("TabRefTypes");
	itsTabRefCodes = measInfo.asArrayuInt ("TabRefCodes");
	fillTabRefMap (measHolder);
      } else {
	itsHasRefTab = False;
	initTabRef (measHolder);
      }
    }
  }
  itsOffset = TableMeasOffsetDesc::reconstruct (measInfo, "RefOff", tab);
}

void TableMeasRefDesc::defaultTypesFunc (Vector<String>& curTypes,
					 Vector<uInt>& curCodes,
					 const MeasureHolder& measHolder)
{
  Int nall, nexact;
  const uInt* codes;
  const String* types = measHolder.asMeasure().allTypes (nall, nexact, codes);
  // Remove the duplicates which are at the end of the arrays.
  Bool found;
  while (nall > 0) {
    if (linearSearchBrackets (found, codes, codes[nall-1], nall-1) < 0) {
      break;
    }
    nall--;
  }
  IPosition shp(1, nall);
  curTypes = Vector<String> (shp, types);
  curCodes = Vector<uInt> (shp, codes);
}

void TableMeasRefDesc::initTabRef (const MeasureHolder& measHolder)
{
  itsTabRefTypes.resize (0);
  itsTabRefCodes.resize (0);
  theirTypesFunc (itsTabRefTypes, itsTabRefCodes, measHolder);
  initTabRefMap();
}

void TableMeasRefDesc::initTabRefMap()
{
  uInt maxcod = max(itsTabRefCodes);
  itsTab2Cur.resize (maxcod+1);
  itsTab2Cur = -1;
  for (uInt i=0; i<itsTabRefCodes.nelements(); ++i) {
    uInt tp = itsTabRefCodes[i];
    itsTab2Cur[tp] = tp;
  }
  itsCur2Tab = itsTab2Cur;
}

void TableMeasRefDesc::fillTabRefMap (const MeasureHolder& measHolder)
{
  Vector<String> curtyp;
  Vector<uInt>   curcod;
  theirTypesFunc (curtyp, curcod, measHolder);
  if (curtyp.nelements() == itsTabRefTypes.nelements()
  &&  allEQ (curtyp, itsTabRefTypes)
  &&  allEQ (curcod, itsTabRefCodes)) {
    initTabRefMap();
  } else {
    uInt maxtab = max(itsTabRefCodes);
    uInt maxcur = max(curcod);
    itsCur2Tab.resize (maxcur+1);
    // First map current codes to table codes; this may add table code entries.
    maxtab = fillMap (itsCur2Tab, curcod, curtyp,
		      itsTabRefCodes, itsTabRefTypes, maxtab);
    itsTab2Cur.resize (maxtab+1);
    fillMap (itsTab2Cur, itsTabRefCodes, itsTabRefTypes, curcod, curtyp, -1);
  }
}

uInt TableMeasRefDesc::fillMap (Block<Int>& f2t,
				const Vector<uInt>& codesf,
				const Vector<String>& typesf,
				Vector<uInt>& codest,
				Vector<String>& typest,
				Int maxnr)
{
  f2t = -1;
  uInt nt = typest.nelements();
  for (uInt i=0; i<typesf.size(); i++) {
    Int inx = linearSearch1 (typest, typesf[i]);
    if (inx >= 0) {
      f2t[codesf[i]] = codest[inx];
    } else {
      if (maxnr < 0) {
	throw AipsError ("TableMeasRefDesc error: old refcode " + typesf[i] +
			 " does not exist anymore");
      }
      codest.resize (nt+1, True);
      typest.resize (nt+1, True);
      maxnr++;
      codest[nt] = maxnr;
      typest[nt] = typesf[i];
      f2t[codesf[i]] = codest[nt];
      nt++;
    }
  }
  return maxnr;
}

uInt TableMeasRefDesc::tab2cur (uInt tabRefCode) const
{
  AlwaysAssert (tabRefCode < itsTab2Cur.nelements()
		&&  itsTab2Cur[tabRefCode] >= 0, AipsError);
  return itsTab2Cur[tabRefCode];
}

uInt TableMeasRefDesc::cur2tab (uInt curRefCode) const
{
  AlwaysAssert (curRefCode < itsCur2Tab.nelements()
		&&  itsCur2Tab[curRefCode] >= 0, AipsError);
  return itsCur2Tab[curRefCode];
}

void TableMeasRefDesc::write (TableDesc& td, TableRecord& measInfo, 
			      const TableMeasDescBase& measDesc)
{
  writeKeys (measInfo, measDesc);
  if (itsOffset != 0) {
    itsOffset->write (td, measInfo, "RefOff");
  }
}

void TableMeasRefDesc::write (Table& tab, TableRecord& measInfo, 
			      const TableMeasDescBase& measDesc)
{
  writeKeys (measInfo, measDesc);
  if (itsOffset != 0) {
    itsOffset->write (tab, measInfo, "RefOff");
  }
}

void TableMeasRefDesc::writeKeys (TableRecord& measInfo, 
				  const TableMeasDescBase& measDesc)
{
  if (isRefCodeVariable()) {
    measInfo.define ("VarRefCol", itsColumn);
    if (itsRefCodeColInt) {
      measInfo.define ("TabRefTypes", itsTabRefTypes);
      measInfo.define ("TabRefCodes", itsTabRefCodes);
    }
  } else {
    measInfo.define ("Ref", measDesc.refType (itsRefCode));
  } 
}

void TableMeasRefDesc::checkColumn (const TableDesc& td)
{
  if (! td.isColumn(itsColumn)) {
    throw (AipsError ("TableMeasRefDesc::checkColumn; No such column: "
		      + itsColumn));
  } else {
    if (td.columnDesc(itsColumn).dataType() != TpString) {
      if (td.columnDesc(itsColumn).dataType() != TpInt) {
	throw AipsError ("TableMeasRefDesc::checkColumn; Reference column's "
			 "type must be Int or String: " + itsColumn);
      }
      itsRefCodeColInt = True;
    }
  }
}

void TableMeasRefDesc::resetRefCode (uInt refCode)
{
  if (isRefCodeVariable()) {
    throw (AipsError ("tableMeasRefDesc::resetRefCode cannot be done;"
		      "the refcode is not fixed for the entire column"));
  }
  itsRefCode = refCode;
}

void TableMeasRefDesc::resetOffset (const Measure& offset)
{
  if (itsOffset == 0) {
    itsOffset = new TableMeasOffsetDesc (offset);
  } else {
    itsOffset->resetOffset (offset);
  }
    if (isOffsetVariable()) {
      throw (AipsError ("tableMeasRefDesc::resetOffset cannot be done;"
			"the offset is not fixed for the entire column"));
    }
}

} //# NAMESPACE CASACORE - END

