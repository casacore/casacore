//# TableParseSortKey.cc: Class to manage a key in a TaQL SORT command
//# Copyright (C) 1994-2022
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

#include <casacore/tables/TaQL/TableParseSortKey.h>
#include <casacore/tables/TaQL/TableParseGroupby.h>
#include <casacore/tables/Tables/TableError.h>
#include <memory>


namespace casacore { //# NAMESPACE CASACORE - BEGIN


  TableParseSortKey::TableParseSortKey()
    : order_p (Sort::Ascending),
      given_p (False)
  {}

  TableParseSortKey::TableParseSortKey (const TableExprNode& node)
    : node_p  (node),
      order_p (Sort::Ascending),
      given_p (False)
  {
    checkNode();
  }
  
  TableParseSortKey::TableParseSortKey (const TableExprNode& node,
                                        Sort::Order order)
    : node_p  (node),
      order_p (order),
      given_p (True)
  {
    checkNode();
  }
  
  void TableParseSortKey::checkNode() const
  {
    if (! node_p.isScalar()) {
      throw TableInvExpr("ORDERBY column/expression must be a scalar");
    }
    TableParseGroupby::checkAggrFuncs (node_p);
  }

  
  std::shared_ptr<ArrayBase> TableParseSortKey::addSortValues
  (Sort& sort, Sort::Order mainOrder, const Vector<rownr_t>& rownrs) const
  {
    // First check if the sort key has a valid data type.
    // This throws an exception for unknown data types (datetime, regex).
    node_p.getColumnDataType();
    // If an order is given for this key, use it. Otherwise the main order.
    Sort::Order order = (given_p  ?  order_p : mainOrder);
    std::shared_ptr<ArrayBase> arrPtr;
    switch (node_p.getColumnDataType()) {
    case TpBool:
      {
        auto array = std::make_shared<Array<Bool>>(node_p.getColumnBool(rownrs));
        if (! array->contiguousStorage()) {
          array = std::make_shared<Array<Bool>>(array->copy());
        }
        arrPtr = array;
        sort.sortKey (array->data(), TpBool, 0, order);
      }
      break;
    case TpUChar:
      {
        auto array = std::make_shared<Array<uChar>>(node_p.getColumnuChar(rownrs));
        if (! array->contiguousStorage()) {
          array = std::make_shared<Array<uChar>>(array->copy());
        }
        arrPtr = array;
        sort.sortKey (array->data(), TpUChar, 0, order);
      }
      break;
    case TpShort:
      {
        auto array = std::make_shared<Array<Short>>(node_p.getColumnShort(rownrs));
        if (! array->contiguousStorage()) {
          array = std::make_shared<Array<Short>>(array->copy());
        }
        arrPtr = array;
        sort.sortKey (array->data(), TpShort, 0, order);
      }
      break;
    case TpUShort:
      {
        auto array = std::make_shared<Array<uShort>>(node_p.getColumnuShort(rownrs));
        if (! array->contiguousStorage()) {
          array = std::make_shared<Array<uShort>>(array->copy());
        }
        arrPtr = array;
        sort.sortKey (array->data(), TpUShort, 0, order);
      }
      break;
    case TpInt:
      {
        auto array = std::make_shared<Array<Int>>(node_p.getColumnInt(rownrs));
        if (! array->contiguousStorage()) {
          array = std::make_shared<Array<Int>>(array->copy());
        }
        arrPtr = array;
        sort.sortKey (array->data(), TpInt, 0, order);
      }
      break;
    case TpUInt:
      {
        auto array = std::make_shared<Array<uInt>>(node_p.getColumnuInt(rownrs));
        if (! array->contiguousStorage()) {
          array = std::make_shared<Array<uInt>>(array->copy());
        }
        arrPtr = array;
        sort.sortKey (array->data(), TpUInt, 0, order);
      }
      break;
    case TpInt64:
      {
        auto array = std::make_shared<Array<Int64>>(node_p.getColumnInt64(rownrs));
        if (! array->contiguousStorage()) {
          array = std::make_shared<Array<Int64>>(array->copy());
        }
        arrPtr = array;
        sort.sortKey (array->data(), TpInt64, 0, order);
      }
      break;
    case TpFloat:
      {
        auto array = std::make_shared<Array<Float>>(node_p.getColumnFloat(rownrs));
        if (! array->contiguousStorage()) {
          array = std::make_shared<Array<Float>>(array->copy());
        }
        arrPtr = array;
        sort.sortKey (array->data(), TpFloat, 0, order);
      }
      break;
    case TpDouble:
      {
        auto array = std::make_shared<Array<Double>>(node_p.getColumnDouble(rownrs));
        if (! array->contiguousStorage()) {
          array = std::make_shared<Array<Double>>(array->copy());
        }
        arrPtr = array;
        sort.sortKey (array->data(), TpDouble, 0, order);
      }
      break;
    case TpComplex:
      {
        auto array = std::make_shared<Array<Complex>>(node_p.getColumnComplex(rownrs));
        if (! array->contiguousStorage()) {
          array = std::make_shared<Array<Complex>>(array->copy());
        }
        arrPtr = array;
        sort.sortKey (array->data(), TpComplex, 0, order);
      }
      break;
    case TpDComplex:
      {
        auto array = std::make_shared<Array<DComplex>>(node_p.getColumnDComplex(rownrs));
        if (! array->contiguousStorage()) {
          array = std::make_shared<Array<DComplex>>(array->copy());
        }
        arrPtr = array;
        sort.sortKey (array->data(), TpDComplex, 0, order);
      }
      break;
    case TpString:
      {
        auto array = std::make_shared<Array<String>>(node_p.getColumnString(rownrs));
        if (! array->contiguousStorage()) {
          array = std::make_shared<Array<String>>(array->copy());
        }
        arrPtr = array;
        sort.sortKey (array->data(), TpString, 0, order);
      }
      break;
    default:
      AlwaysAssert (False, AipsError);
    }
    return arrPtr;
  }
  
} //# NAMESPACE CASACORE - END
