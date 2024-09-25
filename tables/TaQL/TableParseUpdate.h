//# TableParseUpdate.h: Class to manage TaQL UPDATE command handling
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#ifndef TABLES_TABLEPARSEUPDATE_H
#define TABLES_TABLEPARSEUPDATE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/casa/BasicSL/String.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

  //# Forward Declarations
  class TableExprNodeSet;
  class TableExprNodeIndex;
  class TaQLStyle;
  class TableExprId;
  class TableColumn;
  template<class T> class ArrayColumn;


  // <summary>
  // Manage TaQL UPDATE command handling
  // </summary>

  // <use visibility=local>

  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
  // </reviewed>

  // <synopsis> 
  // TableParseUpdate holds the column name, optional indices, optional mask,
  // and new value expression of a column to be updated.
  // Furthermore, it has functions to perform updates in the column and possibly
  // mask column using the expression and possible slice.
  // </synopsis> 

  class TableParseUpdate
  {
  public:
    TableParseUpdate()
      : indexPtr_p(0)
    {}

    // Construct from a column name and expression.
    // By default it checks if no aggregate functions are used.
    TableParseUpdate (const String& columnName,
                      const String& columnNameMask,
                      const TableExprNode&,
                      Bool checkAggr=True);

    // Construct from a column name, subscripts or mask, and expression.
    // It checks if no aggregate functions are used.
    TableParseUpdate (const String& columnName,
                      const String& columnNameMask,
                      const TableExprNodeSet& indices,
                      const TableExprNode&,
                      const TaQLStyle&);

    // Construct from a column name, subscripts and mask, and expression.
    // It checks if no aggregate functions are used.
    // It checks if one of the indices represents subscripts, the other a mask.
    TableParseUpdate (const String& columnName,
                      const String& columnNameMask,
                      const TableExprNodeSet& indices1,
                      const TableExprNodeSet& indices2,
                      const TableExprNode&,
                      const TaQLStyle&);

    // Handle the subscripts or mask.
    // It checks if subscripts or mask is not already used.
    void handleIndices (const TableExprNodeSet& indices,
                        const TaQLStyle& style);

    // Set the node expression (used by TableParseQuery::doInsert).
    void setNode (const TableExprNode& node)
      { node_p = node; }
    
    // Set the column name.
    void setColumnName (const String& name)
      { columnName_p = name; }

    // Set the column name for the mask.
    void setColumnNameMask (const String& name)
      { columnNameMask_p = name; }

    // Get the column name.
    const String& columnName() const
      { return columnName_p; }

    // Get the possible column name for the mask.
    const String& columnNameMask() const
      { return columnNameMask_p; }

    // Adapt the possible unit of the expression to the possible unit
    // of the column.
    void adaptUnit (const Unit& columnUnit)
      { node_p.adaptUnit (columnUnit); }

    // Check if the update column and expression specifications match.
    // Also check if the number of rows is correct.
    void check (const Table& origTable, const Table& updTable) const;
    
    // Update the values in the column with the values of the node_p expression.
    // The column can contain scalars of arrays. Possible only array slices
    // are updated.
    // The mask column values are also updated if a mask is used.
    void updateColumn (TableColumn& col, ArrayColumn<Bool>& maskCol,
                       rownr_t row, const TableExprId& rowid);

  private:
    // Update the values in the columns (helpers of updateColumn).
    // It converts the data type of the expression to that opf the column.
    // <group>
    template<typename TCOL, typename TNODE>
    void updateValue (rownr_t row, const TableExprId& rowid,
                      Bool isScalarCol, const TableExprNode& node,
                      const Array<Bool>& mask,
                      TableColumn& col, const Slicer* slicerPtr,
                      ArrayColumn<Bool>& maskCol);
    template<typename TCOL, typename TNODE>
    void updateScalar (rownr_t row, const TableExprId& rowid,
                       const TableExprNode& node,
                       TableColumn& col);
    template<typename TCOL, typename TNODE>
    void updateArray (rownr_t row, const TableExprId& rowid,
                      const TableExprNode& node,
                      const Array<TNODE>& res,
                      ArrayColumn<TCOL>& col);
    template<typename TCOL, typename TNODE>
    void updateSlice (rownr_t row, const TableExprId& rowid,
                      const TableExprNode& node,
                      const Array<TNODE>& res,
                      const Slicer& slice,
                      ArrayColumn<TCOL>& col);
    template<typename TCOL, typename TNODE>
    void copyMaskedValue (rownr_t row, ArrayColumn<TCOL>& acol,
                          const Slicer* slicerPtr,
                          const TNODE* val,
                          size_t incr, const Array<Bool>& mask);
    Array<Bool> makeMaskSlice (const Array<Bool>& mask,
                               const IPosition& shapeCol,
                               const Slicer* slicerPtr);
    void checkMaskColumn (Bool hasMask,
                          const ArrayColumn<Bool>& maskCol,
                          const TableColumn& col);
    // </group>

    //# Data members
    String              columnName_p;
    String              columnNameMask_p;
    Bool                maskFirst_p; //# True = mask is given before slice
    TableExprNodeIndex* indexPtr_p;  //# copy of pointer in indexNode_p; no need to delete
    TableExprNode       indexNode_p;
    TableExprNode       mask_p;
    TableExprNode       node_p;
  };


} //# NAMESPACE CASACORE - END

#endif
