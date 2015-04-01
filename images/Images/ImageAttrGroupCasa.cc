//# ImageAttrGroupCasa.cc: Attribute group for a CASA image
//# Copyright (C) 2012
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
#include <casacore/images/Images/ImageAttrGroupCasa.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/TableRow.h>
#include <casacore/tables/Tables/TableColumn.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore {

  ImageAttrGroupCasa::ImageAttrGroupCasa (const Table& image,
                                          const String& attrName)
    : itsTable (image.keywordSet().subRecord("ATTRGROUPS").asTable(attrName))
  {}

  ImageAttrGroupCasa::~ImageAttrGroupCasa()
  {
    flush();
  }

  void ImageAttrGroupCasa::flush()
  {
    if (! itsTable.table().isNull()) {
      itsTable.flush (True);
    }
  }
  
  uInt ImageAttrGroupCasa::nrows() const
  {
    return itsTable.table().nrow();
  }

  Bool ImageAttrGroupCasa::hasAttr (const String& attrName) const
  {
    return itsTable.table().tableDesc().isColumn(attrName);
  }

  Vector<String> ImageAttrGroupCasa::attrNames() const
  {
    return itsTable.table().tableDesc().columnNames();
  }

  DataType ImageAttrGroupCasa::dataType (const String& attrName) const
  {
    const TableDesc& tdesc = itsTable.table().tableDesc();
    if (tdesc.isColumn(attrName)) {
      return tdesc[attrName].dataType();
    }
    return TpOther;
  }

  ValueHolder ImageAttrGroupCasa::getData (const String& attrName, uInt rownr)
  {
    ValueHolder value (itsTable.getCell (attrName, rownr));
    if (value.isNull()) {
      value = ValueHolder (Array<Int>());
    }
    return value;
  }

  Record ImageAttrGroupCasa::getDataRow (uInt rownr)
  {
    ROTableRow tabrow (itsTable.table());
    // Transform TableRecord to Record.
    return ValueHolder(tabrow.get(rownr)).asRecord();
  }

  Vector<String> ImageAttrGroupCasa::getUnit (const String& attrName)
  {
    ROTableColumn col(itsTable.table(), attrName);
    if (col.keywordSet().isDefined("QuantumUnits")) {
      return col.keywordSet().asArrayString("QuantumUnits");
    }
    return Vector<String>();
  }

  Vector<String> ImageAttrGroupCasa::getMeasInfo (const String& attrName)
  {
    ROTableColumn col(itsTable.table(), attrName);
    if (col.keywordSet().isDefined("MEASINFO")) {
      Vector<String> info(2);
      const TableRecord& rec = col.keywordSet().subRecord("MEASINFO");
      info[0] = rec.asString("type");
      info[1] = rec.asString("Ref");
      return info;
    }
    return Vector<String>();
  }

  void ImageAttrGroupCasa::putData (const String& attrName,
                                    uInt rownr,
                                    const ValueHolder& data,
                                    const Vector<String>& units,
                                    const Vector<String>& measInfo)
  {
    itsTable.reopenRW();
    // If needed, add the column for the attribute.
    if (addNewColumn (attrName, data)) {
    // Units and MEASINFO are supposed to be the same for all rows,
    // so only put them for the first time, thus if the column has been added.
      TableColumn col(itsTable.table(), attrName);
      if (!units.empty()) {
        itsTable.putKeyword (attrName, "QuantumUnits", -1, False,
                             ValueHolder(units));
      }
      if (!measInfo.empty()) {
        AlwaysAssert (measInfo.size() == 2, AipsError);
        // Define MEASINFO if not defined yet.
        if (! col.rwKeywordSet().isDefined("MEASINFO")) {
          TableRecord rec;
          col.rwKeywordSet().defineRecord ("MEASINFO", rec);
        }
        itsTable.putKeyword (attrName, "MEASINFO.type", -1, False,
                             ValueHolder(measInfo[0]));
        itsTable.putKeyword (attrName, "MEASINFO.Ref",  -1, False,
                             ValueHolder(measInfo[1]));
      }
    }
    checkRows (attrName, rownr);
    itsTable.putCell (attrName, Vector<Int>(1,rownr), data);
  }

  void ImageAttrGroupCasa::checkRows (const String& attrName, uInt rownr)
  {
    uInt nrow = itsTable.nrows();
    // A new row can only be added right after the last row.
    if (rownr > nrow) {
      throw AipsError("ImageAttrGroupCasa: row " + String::toString(rownr) +
                      " of attribute " + attrName +
                      " cannot be added; beyond current #rows " +
                      String::toString(nrow));
    }
    if (rownr == nrow) {
      itsTable.addRow(1);
    }
  }

  Bool ImageAttrGroupCasa::addNewColumn (const String& attrName,
                                         const ValueHolder& data)
  {
    Table& tab = itsTable.table();
    if (tab.tableDesc().isColumn(attrName)) {
      // Column already exists.
      return False;
    }
    // Add the column with the correct type.
    // Assume arrays can have varying shapes.
    IPosition colShape(1,1);
    switch (data.dataType()) {
    case TpBool:
      tab.addColumn (ScalarColumnDesc<Bool>(attrName));
      break;
    case TpArrayBool:
      tab.addColumn (ArrayColumnDesc<Bool> (attrName));
      break;
    case TpChar:
    case TpUChar:
    case TpShort:
    case TpUShort:
    case TpInt:
    case TpUInt:
      tab.addColumn (ScalarColumnDesc<Int>(attrName));
      break;
    case TpArrayInt:
      tab.addColumn (ArrayColumnDesc<Int> (attrName));
      break;
    case TpFloat:
      tab.addColumn (ScalarColumnDesc<Float>(attrName));
      break;
    case TpArrayFloat:
      tab.addColumn (ArrayColumnDesc<Float> (attrName));
      break;
    case TpDouble:
      tab.addColumn (ScalarColumnDesc<Double>(attrName));
      break;
    case TpArrayDouble:
      tab.addColumn (ArrayColumnDesc<Double> (attrName));
      break;
    case TpComplex:
      tab.addColumn (ScalarColumnDesc<Complex>(attrName));
      break;
    case TpArrayComplex:
      tab.addColumn (ArrayColumnDesc<Complex> (attrName));
      break;
    case TpDComplex:
      tab.addColumn (ScalarColumnDesc<DComplex>(attrName));
      break;
    case TpArrayDComplex:
      tab.addColumn (ArrayColumnDesc<DComplex> (attrName));
      break;
    case TpString:
      tab.addColumn (ScalarColumnDesc<String>(attrName));
      break;
    case TpArrayString:
      tab.addColumn (ArrayColumnDesc<String> (attrName));
      break;
    default:
      throw AipsError("ImageAttrGroupCasa::addNewColumn: Unknown datatype " +
                      String::toString(data.dataType()));
    }
    return True;
  }

} //# NAMESPACE CASACORE - END
