//# TableParseUpdate.cc: Class to manage TaQL UPDATE command handling
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

#include <casacore/tables/TaQL/TableParseUpdate.h>
#include <casacore/tables/TaQL/TableParseGroupby.h>
#include <casacore/tables/TaQL/TaQLStyle.h>
#include <casacore/tables/TaQL/TableExprIdAggr.h>
#include <casacore/tables/TaQL/ExprNodeArray.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/TableError.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN


  TableParseUpdate::TableParseUpdate (const String& columnName,
                                      const String& columnNameMask,
                                      const TableExprNode& node,
                                      Bool checkAggr)
    : columnName_p     (columnName),
      columnNameMask_p (columnNameMask),
      maskFirst_p      (False),
      indexPtr_p       (0),
      node_p           (node)
  {
    if (checkAggr) {
      TableParseGroupby::checkAggrFuncs (node);
    }
  }

  TableParseUpdate::TableParseUpdate (const String& columnName,
                                      const String& columnNameMask,
                                      const TableExprNodeSet& indices,
                                      const TableExprNode& node,
                                      const TaQLStyle& style)
    : columnName_p     (columnName),
      columnNameMask_p (columnNameMask),
      maskFirst_p      (False),
      indexPtr_p       (0),
      node_p           (node)
  {
    TableParseGroupby::checkAggrFuncs (node);
    handleIndices (indices, style);
    if (indexPtr_p == 0) {
      if (! columnNameMask_p.empty()) {
        throw TableInvExpr ("No mask column name can be given if the update "
                            "data column is masked");
      }
      maskFirst_p = True;
    }
  }

  TableParseUpdate::TableParseUpdate (const String& columnName,
                                      const String& columnNameMask,
                                      const TableExprNodeSet& indices1,
                                      const TableExprNodeSet& indices2,
                                      const TableExprNode& node,
                                      const TaQLStyle& style)
    : columnName_p     (columnName),
      columnNameMask_p (columnNameMask),
      maskFirst_p      (False),
      indexPtr_p       (0),
      node_p           (node)
  {
    // The grammar does not allow a column mask name, but you can never tell.
    AlwaysAssert (columnNameMask.empty(), AipsError);
    TableParseGroupby::checkAggrFuncs (node);
    handleIndices (indices1, style);
    maskFirst_p = indexPtr_p==0;
    handleIndices (indices2, style);
  }

  void TableParseUpdate::handleIndices (const TableExprNodeSet& indices,
                                        const TaQLStyle& style)
  {
    // Create a mask if a single bool element is given.
    if (indices.isSingle()  &&  indices.size() == 1  &&
        indices.dataType() == TableExprNodeRep::NTBool) {
      if (! mask_p.isNull()) {
        throw TableInvExpr ("A double indexed update array cannot contain "
                            "two masks");
      }
      if (! indices.hasArrays()) {
        throw TableInvExpr ("A mask in an update must be an array");
      }
      mask_p = TableExprNode(indices[0]->start());
    } else {
      if (indexPtr_p) {
        throw TableInvExpr ("A double indexed update array cannot contain "
                            "two index ranges");
      }
      indexPtr_p  = new TableExprNodeIndex (indices, style);
      indexNode_p = TableExprNode(indexPtr_p);
    }
  }


  //# The following are helper functions for updateColumn.
  //# Due to their templated nature, they have to be properly ordered
  //# (thus the implementation before being used).
  template<typename TCOL, typename TNODE>
  void TableParseUpdate::updateScalar (rownr_t row, const TableExprId& rowid,
                                       const TableExprNode& node,
                                       TableColumn& col)
  {
    AlwaysAssert (node.isScalar(), AipsError);
    TNODE val;
    node.get (rowid, val);
    TCOL value(static_cast<TCOL>(val));
    col.putScalar (row, value);
  }

  template<typename TCOL, typename TNODE>
  void TableParseUpdate::updateArray (rownr_t row, const TableExprId& rowid,
                                      const TableExprNode& node,
                                      const Array<TNODE>& res,
                                      ArrayColumn<TCOL>& col)
  {
    if (node.isScalar()  &&  col.isDefined (row)) {
      TNODE val;
      node.get (rowid, val);
      Array<TCOL> arr(col.shape(row));
      arr = static_cast<TCOL>(val);
      col.put (row, arr);
    } else {
      Array<TCOL> arr(res.shape());
      convertArray (arr, res);
      col.put (row, arr);
    }
  }
  
  template<typename TCOL, typename TNODE>
  void TableParseUpdate::updateSlice (rownr_t row, const TableExprId& rowid,
                                      const TableExprNode& node,
                                      const Array<TNODE>& res,
                                      const Slicer& slice,
                                      ArrayColumn<TCOL>& col)
  {
    if (col.isDefined(row)) {
      if (node.isScalar()) {
        TNODE val;
        node.get (rowid, val);
        Array<TCOL> arr;
        if (slice.isFixed()) {
          arr.resize (slice.length());
        } else {
          // Unbound slicer, so derive from array shape.
          IPosition blc, trc, inc;
          arr.resize (slice.inferShapeFromSource
                      (col.shape(row), blc, trc, inc));
        }
        arr = static_cast<TCOL>(val);
        col.putSlice (row, slice, arr);
      } else {
        // Note that the calling function tests if the MArray is null.
        Array<TCOL> arr(res.shape());
        convertArray (arr, res);
        col.putSlice (row, slice, arr);
      }
    }
  }

  template<typename TCOL, typename TNODE>
  void TableParseUpdate::copyMaskedValue (rownr_t row, ArrayColumn<TCOL>& acol,
                                          const Slicer* slicerPtr,
                                          const TNODE* val,
                                          size_t incr, const Array<Bool>& mask)
  {
    // Get the array from the table.
    Array<TCOL> res(mask.shape());
    if (slicerPtr) {
      acol.getSlice (row, *slicerPtr, res);
    } else {
      acol.get (row, res);
    }
    // Copy values where masked.
    typename Array<TCOL>::iterator ito = res.begin();
    Array<Bool>::const_iterator imask = mask.begin();
    size_t n = res.size();
    for (size_t i=0; i<n; ++i) {
      if (*imask) {
        *ito = static_cast<TCOL>(*val);
      }
      ++ito;
      ++imask;
      val += incr;
    }
    // Put the array (slice).
    if (slicerPtr) {
      acol.putSlice (row, *slicerPtr, res);
    } else {
      acol.put (row, res);
    }
  }

  template<typename TCOL, typename TNODE>
  void TableParseUpdate::updateValue (rownr_t row, const TableExprId& rowid,
                                      Bool isScalarCol,
                                      const TableExprNode& node,
                                      const Array<Bool>& mask,
                                      TableColumn& col,
                                      const Slicer* slicerPtr,
                                      ArrayColumn<Bool>& maskCol)
  {
    if (isScalarCol) {
      updateScalar<TCOL,TNODE> (row, rowid, node, col);
    } else {
      MArray<TNODE> aval;
      if (! node.isScalar()) {
        node.get (rowid, aval);
        if (aval.isNull()) {
          return;
        }
      }
      checkMaskColumn (aval.hasMask(), maskCol, col);
      ArrayColumn<TCOL> acol(col);
      if (mask.empty()) {
        if (slicerPtr) {
          updateSlice<TCOL,TNODE> (row, rowid, node, aval.array(),
                                   *slicerPtr, acol);
          if (! maskCol.isNull()) {
            updateSlice<Bool,Bool> (row, rowid, node, aval.mask(),
                                    *slicerPtr, maskCol);
          }
        } else {
          updateArray<TCOL,TNODE> (row, rowid, node, aval.array(), acol);
          if (! maskCol.isNull()) {
            updateArray<Bool,Bool> (row, rowid, node, aval.mask(), maskCol);
          }
        }
      } else {
        // A mask is used; can only be done if the column cell
        // contains an array.
        if (acol.isDefined(row)) {
          IPosition shapeCol = acol.shape (row);
          // Check shapes, get possible slice from mask.
          Array<Bool> smask(makeMaskSlice (mask, shapeCol, slicerPtr));
          // Get the expression data (scalar or array).
          TNODE sval;
          const TNODE* ptr = &sval;
          size_t incr = 0;
          Bool deleteIt = False;
          if (node.isScalar()) {
            node.get (rowid, sval);
          } else {
            if (! aval.shape().isEqual (smask.shape())) {
              throw TableInvExpr ("Array shapes in update of column " +
                                  col.columnDesc().name() + " mismatch");
            }
            ptr = aval.array().getStorage (deleteIt);
            incr = 1;
          }
          // Put the array into the column (slice).
          // Copy values where masked.
          copyMaskedValue (row, acol, slicerPtr, ptr, incr, smask);
          if (! node.isScalar()) {
            aval.array().freeStorage (ptr, deleteIt);
            if (! maskCol.isNull()) {
              const Bool* bptr = aval.mask().getStorage (deleteIt);
              copyMaskedValue (row, maskCol, slicerPtr, bptr, 1, smask);
              aval.mask().freeStorage (bptr, deleteIt);
            }
          }
        }
      }
    }
  }

  Array<Bool> TableParseUpdate::makeMaskSlice (const Array<Bool>& mask,
                                               const IPosition& shapeCol,
                                               const Slicer* slicerPtr)
  {
    if (! slicerPtr  ||  maskFirst_p) {
      if (! mask.shape().isEqual (shapeCol)) {
        throw TableInvExpr ("Update mask must conform the column's array shape");
      }
    }
    if (slicerPtr) {
      IPosition length;
      if (slicerPtr->isFixed()) {
        length = slicerPtr->length();
      } else {
        IPosition blc, trc, inc;
        length = slicerPtr->inferShapeFromSource (shapeCol, blc, trc, inc);
      }
      if (maskFirst_p) {
        // Mask before section, so apply the section to the mask.
        return mask(*slicerPtr);
      } else {
        if (! mask.shape().isEqual (length)) {
          throw TableInvExpr ("Update mask must conform the column's array section");
        }
      }
    }
    return mask;
  }

  void TableParseUpdate::checkMaskColumn (Bool hasMask,
                                          const ArrayColumn<Bool>& maskCol,
                                          const TableColumn& col)
  {
    // If a mask column is given, the expression must have a mask.
    // But if the expression has a mask, a mask column is not needed.
    // In that case the mask is ignored. This is necessary for a mask in
    // a select expression, otherwise function ARRAYDATA is always needed.
    if (! maskCol.isNull()) {
      if (! hasMask) {
        throw TableInvExpr ("No update mask column can be given for an "
                            "unmasked expression in update of column " +
                            col.columnDesc().name());
      }
    }
  }

  void TableParseUpdate::updateColumn (TableColumn& col,
                                       ArrayColumn<Bool>& maskCol,
                                       rownr_t row,
                                       const TableExprId& rowid)
  {
    // Get possible subscripts.
    const Slicer* slicerPtr = 0;
    if (indexPtr_p != 0) {
      slicerPtr = &(indexPtr_p->getSlicer(rowid));
    }
    // Evaluate a possible mask.
    MArray<Bool> mask;
    if (! mask_p.isNull()) {
      mask_p.get (rowid, mask);
    }
    // The expression node type determines how to get the data.
    // The column data type determines how to put it.
    // The node data type should be convertible to the column data type.
    // The updateValue function does the actual work.
    // We simply switch on the types.
    Bool isScalarCol = col.columnDesc().isScalar();
    switch (node_p.getNodeRep()->dataType()) {
    case TableExprNodeRep::NTBool:
      switch (col.columnDesc().dataType()) {
      case TpBool:
        updateValue<Bool,Bool> (row, rowid, isScalarCol, node_p,
                                mask.array(), col, slicerPtr, maskCol);
        break;
      default:
        throw TableInvExpr ("Column " + columnName_p +
                            " has an invalid data type for an"
                            " UPDATE with a bool value");
      }
      break;

    case TableExprNodeRep::NTString:
      switch (col.columnDesc().dataType()) {
      case TpString:
        updateValue<String,String> (row, rowid, isScalarCol, node_p,
                                    mask.array(), col, slicerPtr, maskCol);
        break;
      default:
        throw TableInvExpr ("Column " + columnName_p +
                            " has an invalid data type for an"
                            " UPDATE with a string value");
      }
      break;

    case TableExprNodeRep::NTInt:
      switch (col.columnDesc().dataType()) {
      case TpUChar:
        updateValue<uChar,Int64> (row, rowid, isScalarCol, node_p,
                                  mask.array(), col, slicerPtr, maskCol);
        break;
      case TpShort:
        updateValue<Short,Int64> (row, rowid, isScalarCol, node_p,
                                  mask.array(), col, slicerPtr, maskCol);
        break;
      case TpUShort:
        updateValue<uShort,Int64> (row, rowid, isScalarCol, node_p,
                                   mask.array(), col, slicerPtr, maskCol);
        break;
      case TpInt:
        updateValue<Int,Int64> (row, rowid, isScalarCol, node_p,
                                mask.array(), col, slicerPtr, maskCol);
        break;
      case TpUInt:
        updateValue<uInt,Int64> (row, rowid, isScalarCol, node_p,
                                 mask.array(), col, slicerPtr, maskCol);
        break;
      case TpInt64:
        updateValue<Int64,Int64> (row, rowid, isScalarCol, node_p,
                                  mask.array(), col, slicerPtr, maskCol);
        break;
      case TpFloat:
        updateValue<Float,Int64> (row, rowid, isScalarCol, node_p,
                                  mask.array(), col, slicerPtr, maskCol);
        break;
      case TpDouble:
        updateValue<Double,Int64> (row, rowid, isScalarCol, node_p,
                                   mask.array(), col, slicerPtr, maskCol);
        break;
      case TpComplex:
        updateValue<Complex,Int64> (row, rowid, isScalarCol, node_p,
                                    mask.array(), col, slicerPtr, maskCol);
        break;
      case TpDComplex:
        updateValue<DComplex,Int64> (row, rowid, isScalarCol, node_p,
                                     mask.array(), col, slicerPtr, maskCol);
        break;
      default:
        throw TableInvExpr ("Column " + columnName_p +
                            " has an invalid data type for an"
                            " UPDATE with an integer value");
      }
      break;

    case TableExprNodeRep::NTDouble:
    case TableExprNodeRep::NTDate:
      switch (col.columnDesc().dataType()) {
      case TpUChar:
        updateValue<uChar,Double> (row, rowid, isScalarCol, node_p,
                                   mask.array(), col, slicerPtr, maskCol);
        break;
      case TpShort:
        updateValue<Short,Double> (row, rowid, isScalarCol, node_p,
                                   mask.array(), col, slicerPtr, maskCol);
        break;
      case TpUShort:
        updateValue<uShort,Double> (row, rowid, isScalarCol, node_p,
                                    mask.array(), col, slicerPtr, maskCol);
        break;
      case TpInt:
        updateValue<Int,Double> (row, rowid, isScalarCol, node_p,
                                 mask.array(), col, slicerPtr, maskCol);
        break;
      case TpUInt:
        updateValue<uInt,Double> (row, rowid, isScalarCol, node_p,
                                  mask.array(), col, slicerPtr, maskCol);
        break;
      case TpInt64:
        updateValue<Int64,Double> (row, rowid, isScalarCol, node_p,
                                   mask.array(), col, slicerPtr, maskCol);
        break;
      case TpFloat:
        updateValue<Float,Double> (row, rowid, isScalarCol, node_p,
                                   mask.array(), col, slicerPtr, maskCol);
        break;
      case TpDouble:
        updateValue<Double,Double> (row, rowid, isScalarCol, node_p,
                                    mask.array(), col, slicerPtr, maskCol);
        break;
      case TpComplex:
        updateValue<Complex,Double> (row, rowid, isScalarCol, node_p,
                                     mask.array(), col, slicerPtr, maskCol);
        break;
      case TpDComplex:
        updateValue<DComplex,Double> (row, rowid, isScalarCol, node_p,
                                      mask.array(), col, slicerPtr, maskCol);
        break;
      default:
        throw TableInvExpr ("Column " + columnName_p +
                            " has an invalid data type for an"
                            " UPDATE with a double value");
      }
      break;

    case TableExprNodeRep::NTComplex:
      switch (col.columnDesc().dataType()) {
      case TpComplex:
        updateValue<Complex,DComplex> (row, rowid, isScalarCol, node_p,
                                       mask.array(), col, slicerPtr, maskCol);
        break;
      case TpDComplex:
        updateValue<DComplex,DComplex> (row, rowid, isScalarCol, node_p,
                                        mask.array(), col, slicerPtr, maskCol);
        break;
      default:
        throw TableInvExpr ("Column " + columnName() +
                            " has an invalid data type for an"
                            " UPDATE with a complex value");
      }
      break;
          
    default:
      throw TableInvExpr ("Unknown UPDATE expression data type");
    }
  }

  void TableParseUpdate::check (const Table& origTable,
                                const Table& updTable) const
  {
    // Check if the correct table is used in the update and index expression.
    // A constant expression can be given.
    if (! node_p.checkTableSize (origTable, True)) {
      throw TableInvExpr ("Table(s) with incorrect size used in the "
                          "UPDATE expr of column " + columnName_p +
                          " (mismatches first table)");
    }
    if (indexPtr_p != 0) {
      if (! indexNode_p.checkTableSize (updTable, True)) {
        throw TableInvExpr ("Table(s) with incorrect size used in the "
                            "index expr in UPDATE of column " + columnName_p +
                            " (mismatches first table)");
      }
    }
    // This throws an exception for unknown data types (datetime, regex).
    node_p.getColumnDataType();
    // Check if the column exists and is writable.
    const TableDesc& tabdesc = updTable.tableDesc();
    if (! tabdesc.isColumn (columnName_p)) {
      throw TableInvExpr ("Update column " + columnName_p +
                          " does not exist in table " +
                          updTable.tableName());
    }
    if (! updTable.isColumnWritable (columnName_p)) {
      throw TableInvExpr ("Update column " + columnName_p +
                          " is not writable in table " +
                          updTable.tableName());
    }
    const ColumnDesc& coldesc = tabdesc[columnName_p];
    Bool isScalar = coldesc.isScalar();
    if (! columnNameMask_p.empty()) {
      if (! tabdesc.isColumn (columnNameMask_p)) {
        throw TableInvExpr ("Update column " + columnNameMask_p +
                            " does not exist in table " +
                            updTable.tableName());
      }
      if (! updTable.isColumnWritable (columnNameMask_p)) {
        throw TableInvExpr ("Update column " + columnNameMask_p +
                            " is not writable in table " +
                            updTable.tableName());
      }
      const ColumnDesc& coldescMask = tabdesc[columnNameMask_p];
      if (node_p.isScalar()) {
        throw TableInvExpr ("Update mask column " + columnNameMask_p +
                            " cannot be given for a scalar expression");
      }
      if (coldescMask.dataType() != TpBool) {
        throw TableInvExpr ("Update mask column " + columnNameMask_p +
                            " must have data type Bool");
      }
    }
    // An index expression can only be given for an array column.
    if (indexPtr_p != 0) {
      if (isScalar) {
        throw TableInvExpr ("Index value cannot be given in UPDATE of "
                            " scalar column " + columnName_p);
      }
      if (indexPtr_p->isSingle()) {
        isScalar = True;
      }
    }
    // Check if the value type matches.
    if (isScalar  &&  !node_p.isScalar()) {
      throw TableInvExpr ("An array value cannot be used in UPDATE of "
                          " scalar element of column " +
                          columnName_p + " in table " +
                          updTable.tableName());
    }
  }

} //# NAMESPACE CASACORE - END
