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
//#
//# $Id: TableParseSortKey.cc 21399 2013-11-12 07:55:35Z gervandiepen $

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
    Bool deleteIt;
    switch (node_p.getColumnDataType()) {
    case TpBool:
      {
        Array<Bool>* array = new Array<Bool>
          (node_p.getColumnBool(rownrs));
        arrPtr.reset (array);
        Bool* data = array->getStorage (deleteIt);
        if (deleteIt) {
          // A copy has been made, so replace the array.
          arrPtr.reset (new Array<Bool> (array->shape(), data, TAKE_OVER));
        }
        sort.sortKey (data, TpBool, 0, order);
      }
      break;
    case TpUChar:
      {
        Array<uChar>* array = new Array<uChar>
          (node_p.getColumnuChar(rownrs));
        arrPtr.reset (array);
        uChar* data = array->getStorage (deleteIt);
        if (deleteIt) {
          // A copy has been made, so replace the array.
          arrPtr.reset (new Array<uChar> (array->shape(), data, TAKE_OVER));
        }
        sort.sortKey (data, TpUChar, 0, order);
      }
      break;
    case TpShort:
      {
        Array<Short>* array = new Array<Short>
          (node_p.getColumnShort(rownrs));
        arrPtr.reset (array);
        Short* data = array->getStorage (deleteIt);
        if (deleteIt) {
          // A copy has been made, so replace the array.
          arrPtr.reset (new Array<Short> (array->shape(), data, TAKE_OVER));
        }
        sort.sortKey (data, TpShort, 0, order);
      }
      break;
    case TpUShort:
      {
        Array<uShort>* array = new Array<uShort>
          (node_p.getColumnuShort(rownrs));
        arrPtr.reset (array);
        uShort* data = array->getStorage (deleteIt);
        if (deleteIt) {
          // A copy has been made, so replace the array.
          arrPtr.reset (new Array<uShort> (array->shape(), data, TAKE_OVER));
        }
        sort.sortKey (data, TpUShort, 0, order);
      }
      break;
    case TpInt:
      {
        Array<Int>* array = new Array<Int>
          (node_p.getColumnInt(rownrs));
        arrPtr.reset (array);
        Int* data = array->getStorage (deleteIt);
        if (deleteIt) {
          // A copy has been made, so replace the array.
          arrPtr.reset (new Array<Int> (array->shape(), data, TAKE_OVER));
        }
        sort.sortKey (data, TpInt, 0, order);
      }
      break;
    case TpUInt:
      {
        Array<uInt>* array = new Array<uInt>
          (node_p.getColumnuInt(rownrs));
        arrPtr.reset (array);
        uInt* data = array->getStorage (deleteIt);
        if (deleteIt) {
          // A copy has been made, so replace the array.
          arrPtr.reset (new Array<uInt> (array->shape(), data, TAKE_OVER));
        }
        sort.sortKey (data, TpUInt, 0, order);
      }
      break;
    case TpInt64:
      {
        Array<Int64>* array = new Array<Int64>
          (node_p.getColumnInt64(rownrs));
        arrPtr.reset (array);
        Int64* data = array->getStorage (deleteIt);
        if (deleteIt) {
          // A copy has been made, so replace the array.
          arrPtr.reset (new Array<Int64> (array->shape(), data, TAKE_OVER));
        }
        sort.sortKey (data, TpInt64, 0, order);
      }
      break;
    case TpFloat:
      {
        Array<Float>* array = new Array<Float>
          (node_p.getColumnFloat(rownrs));
        arrPtr.reset (array);
        Float* data = array->getStorage (deleteIt);
        if (deleteIt) {
          // A copy has been made, so replace the array.
          arrPtr.reset (new Array<Float> (array->shape(), data, TAKE_OVER));
        }
        sort.sortKey (data, TpFloat, 0, order);
      }
      break;
    case TpDouble:
      {
        Array<Double>* array = new Array<Double>
          (node_p.getColumnDouble(rownrs));
        arrPtr.reset (array);
        Double* data = array->getStorage (deleteIt);
        if (deleteIt) {
          // A copy has been made, so replace the array.
          arrPtr.reset (new Array<Double> (array->shape(), data, TAKE_OVER));
        }
        sort.sortKey (data, TpDouble, 0, order);
      }
      break;
    case TpComplex:
      {
        Array<Complex>* array = new Array<Complex>
          (node_p.getColumnComplex(rownrs));
        arrPtr.reset (array);
        Complex* data = array->getStorage (deleteIt);
        if (deleteIt) {
          // A copy has been made, so replace the array.
          arrPtr.reset (new Array<Complex> (array->shape(), data, TAKE_OVER));
        }
        sort.sortKey (data, TpComplex, 0, order);
      }
      break;
    case TpDComplex:
      {
        Array<DComplex>* array = new Array<DComplex>
          (node_p.getColumnDComplex(rownrs));
        arrPtr.reset (array);
        DComplex* data = array->getStorage (deleteIt);
        if (deleteIt) {
          // A copy has been made, so replace the array.
          arrPtr.reset (new Array<DComplex> (array->shape(), data, TAKE_OVER));
        }
        sort.sortKey (data, TpDComplex, 0, order);
      }
      break;
    case TpString:
      {
        Array<String>* array = new Array<String>
          (node_p.getColumnString(rownrs));
        arrPtr.reset (array);
        String* data = array->getStorage (deleteIt);
        if (deleteIt) {
          // A copy has been made, so replace the array.
          arrPtr.reset (new Array<String> (array->shape(), data, TAKE_OVER));
        }
        sort.sortKey (data, TpString, 0, order);
      }
      break;
    default:
      AlwaysAssert (False, AipsError);
    }
    return arrPtr;
  }
  
} //# NAMESPACE CASACORE - END
