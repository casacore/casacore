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
#include <images/Images/ImageAttrGroupCasa.h>
#include <tables/Tables/TableRecord.h>
#include <tables/Tables/TableColumn.h>
#include <tables/Tables/ArrColDesc.h>
#include <tables/Tables/ScaColDesc.h>
#include <casa/Exceptions/Error.h>

namespace casa {

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
  
  uInt ImageAttrGroupCasa::nvalues() const
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

  ValueHolder ImageAttrGroupCasa::getData (const String& attrName)
  {
    ValueHolder value (itsTable.getColumn (attrName, 0, -1, 1));
    if (value.isNull()) {
      value = ValueHolder (Array<Int>());
    }
    return value;
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
                                    const ValueHolder& data,
                                    const Vector<String>& units,
                                    const Vector<String>& measInfo)
  {
    itsTable.reopenRW();
    addColumn (attrName, data);
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
    itsTable.putColumn (attrName, 0, -1 , 1, data);
  }

  void ImageAttrGroupCasa::checkRows (const String& attrName, uInt size)
  {
    uInt nrow = itsTable.nrows();
    if (nrow == 0) {
      itsTable.addRow (size);
    } else if (size != nrow) {
      throw AipsError("ImageAttrGroupCasa: cannot put " +
                      String::toString(size) +
                      " elements of attr " + attrName +
                      " into table containing " +
                      String::toString(nrow) + " rows");
    }
  }

  void ImageAttrGroupCasa::addColumn (const String& attrName,
                                      const ValueHolder& data)
  {
    Table& tab = itsTable.table();
    Bool doAdd = !tab.tableDesc().isColumn(attrName);
    IPosition colShape(1,1);
    switch (data.dataType()) {
    case TpArrayBool:
      colShape.resize(0);
      colShape = data.asArrayBool().shape();   // fall through
    case TpBool:
      if (doAdd) {
        if (colShape.size() == 1) {
          tab.addColumn (ScalarColumnDesc<Bool>(attrName));
        } else{
          tab.addColumn (ArrayColumnDesc<Bool>
                         (attrName, colShape.getFirst(colShape.size()-1)));
        }
      }
      break;
    case TpArrayInt:
      colShape.resize(0);
      colShape = data.asArrayInt().shape();   // fall through
    case TpInt:
      if (doAdd) {
        if (colShape.size() == 1) {
          tab.addColumn (ScalarColumnDesc<Int>(attrName));
        } else{
          tab.addColumn (ArrayColumnDesc<Int>
                         (attrName, colShape.getFirst(colShape.size()-1)));
        }
      }
      break;
    case TpArrayFloat:
      colShape.resize(0);
      colShape = data.asArrayFloat().shape();   // fall through
    case TpFloat:
      if (doAdd) {
        if (colShape.size() == 1) {
          tab.addColumn (ScalarColumnDesc<Float>(attrName));
        } else{
          tab.addColumn (ArrayColumnDesc<Float>
                         (attrName, colShape.getFirst(colShape.size()-1)));
        }
      }
      break;
    case TpArrayDouble:
      colShape.resize(0);
      colShape = data.asArrayDouble().shape();   // fall through
    case TpDouble:
      if (doAdd) {
        if (colShape.size() == 1) {
          tab.addColumn (ScalarColumnDesc<Double>(attrName));
        } else{
          tab.addColumn (ArrayColumnDesc<Double>
                         (attrName, colShape.getFirst(colShape.size()-1)));
        }
      }
      break;
    case TpArrayComplex:
      colShape.resize(0);
      colShape = data.asArrayComplex().shape();   // fall through
    case TpComplex:
      if (doAdd) {
        if (colShape.size() == 1) {
          tab.addColumn (ScalarColumnDesc<Complex>(attrName));
        } else{
          tab.addColumn (ArrayColumnDesc<Complex>
                         (attrName, colShape.getFirst(colShape.size()-1)));
        }
      }
      break;
    case TpArrayDComplex:
      colShape.resize(0);
      colShape = data.asArrayDComplex().shape();   // fall through
    case TpDComplex:
      if (doAdd) {
        if (colShape.size() == 1) {
          tab.addColumn (ScalarColumnDesc<DComplex>(attrName));
        } else{
          tab.addColumn (ArrayColumnDesc<DComplex>
                         (attrName, colShape.getFirst(colShape.size()-1)));
        }
      }
      break;
    case TpArrayString:
      colShape.resize(0);
      colShape = data.asArrayString().shape();   // fall through
    case TpString:
      if (doAdd) {
        if (colShape.size() == 1) {
          tab.addColumn (ScalarColumnDesc<String>(attrName));
        } else{
          tab.addColumn (ArrayColumnDesc<String>
                         (attrName, colShape.getFirst(colShape.size()-1)));
        }
      }
      break;
    default:
      throw AipsError("ImageAttrGroupCasa::addColumn: Unknown datatype " +
                      String::toString(data.dataType()));
    }
    checkRows (attrName, colShape[colShape.size()-1]);
  }

} //# NAMESPACE CASA - END
