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
      given_p (false)
  {}

  TableParseSortKey::TableParseSortKey (const TableExprNode& node)
    : node_p  (node),
      order_p (Sort::Ascending),
      given_p (false)
  {
    checkNode();
  }
  
  TableParseSortKey::TableParseSortKey (const TableExprNode& node,
                                        Sort::Order order)
    : node_p  (node),
      order_p (order),
      given_p (true)
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
    bool deleteIt;
    switch (node_p.getColumnDataType()) {
    case TpBool:
      {
        Array<bool>* array = new Array<bool>
          (node_p.getColumnBool(rownrs));
        arrPtr.reset (array);
        bool* data = array->getStorage (deleteIt);
        if (deleteIt) {
          // A copy has been made, so replace the array.
          arrPtr.reset (new Array<bool> (array->shape(), data, TAKE_OVER));
        }
        sort.sortKey (data, TpBool, 0, order);
      }
      break;
    case TpUChar:
      {
        Array<unsigned char>* array = new Array<unsigned char>
          (node_p.getColumnuChar(rownrs));
        arrPtr.reset (array);
        unsigned char* data = array->getStorage (deleteIt);
        if (deleteIt) {
          // A copy has been made, so replace the array.
          arrPtr.reset (new Array<unsigned char> (array->shape(), data, TAKE_OVER));
        }
        sort.sortKey (data, TpUChar, 0, order);
      }
      break;
    case TpShort:
      {
        Array<int16_t>* array = new Array<int16_t>
          (node_p.getColumnShort(rownrs));
        arrPtr.reset (array);
        int16_t* data = array->getStorage (deleteIt);
        if (deleteIt) {
          // A copy has been made, so replace the array.
          arrPtr.reset (new Array<int16_t> (array->shape(), data, TAKE_OVER));
        }
        sort.sortKey (data, TpShort, 0, order);
      }
      break;
    case TpUShort:
      {
        Array<uint16_t>* array = new Array<uint16_t>
          (node_p.getColumnuShort(rownrs));
        arrPtr.reset (array);
        uint16_t* data = array->getStorage (deleteIt);
        if (deleteIt) {
          // A copy has been made, so replace the array.
          arrPtr.reset (new Array<uint16_t> (array->shape(), data, TAKE_OVER));
        }
        sort.sortKey (data, TpUShort, 0, order);
      }
      break;
    case TpInt:
      {
        Array<int32_t>* array = new Array<int32_t>
          (node_p.getColumnInt(rownrs));
        arrPtr.reset (array);
        int32_t* data = array->getStorage (deleteIt);
        if (deleteIt) {
          // A copy has been made, so replace the array.
          arrPtr.reset (new Array<int32_t> (array->shape(), data, TAKE_OVER));
        }
        sort.sortKey (data, TpInt, 0, order);
      }
      break;
    case TpUInt:
      {
        Array<uint32_t>* array = new Array<uint32_t>
          (node_p.getColumnuInt(rownrs));
        arrPtr.reset (array);
        uint32_t* data = array->getStorage (deleteIt);
        if (deleteIt) {
          // A copy has been made, so replace the array.
          arrPtr.reset (new Array<uint32_t> (array->shape(), data, TAKE_OVER));
        }
        sort.sortKey (data, TpUInt, 0, order);
      }
      break;
    case TpInt64:
      {
        Array<int64_t>* array = new Array<int64_t>
          (node_p.getColumnInt64(rownrs));
        arrPtr.reset (array);
        int64_t* data = array->getStorage (deleteIt);
        if (deleteIt) {
          // A copy has been made, so replace the array.
          arrPtr.reset (new Array<int64_t> (array->shape(), data, TAKE_OVER));
        }
        sort.sortKey (data, TpInt64, 0, order);
      }
      break;
    case TpFloat:
      {
        Array<float>* array = new Array<float>
          (node_p.getColumnFloat(rownrs));
        arrPtr.reset (array);
        float* data = array->getStorage (deleteIt);
        if (deleteIt) {
          // A copy has been made, so replace the array.
          arrPtr.reset (new Array<float> (array->shape(), data, TAKE_OVER));
        }
        sort.sortKey (data, TpFloat, 0, order);
      }
      break;
    case TpDouble:
      {
        Array<double>* array = new Array<double>
          (node_p.getColumnDouble(rownrs));
        arrPtr.reset (array);
        double* data = array->getStorage (deleteIt);
        if (deleteIt) {
          // A copy has been made, so replace the array.
          arrPtr.reset (new Array<double> (array->shape(), data, TAKE_OVER));
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
      AlwaysAssert (false, AipsError);
    }
    return arrPtr;
  }
  
} //# NAMESPACE CASACORE - END
