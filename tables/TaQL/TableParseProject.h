//# TableParseProject.h: Class holding the info of a TaQL projection command
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
//# $Id$

#ifndef TABLES_TABLEPARSEPROJECT_H
#define TABLES_TABLEPARSEPROJECT_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/TableParseTableList.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/casa/Arrays/ArrayFwd.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Containers/Record.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  //# Forward Declarations
  class TableParseUpdate;
  class TableExprGroupResult;


  // <summary>
  // Class holding the info of a TaQL projection command
  // </summary>

  // <use visibility=local>

  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
  // </reviewed>

  // <synopsis>
  // Table projection is the selection of columns in a TaQL command.
  // This class gets the results of the parser of all columns in the projection.
  // Those results are column name, new name, data type and expression.
  // Furthermore, it holds info which projected columns are used in other
  // parts of a TaQL command (HAVING and ORDERBY). 
  // Once the TaQL is fully parsed and all parameters are stored, it is executed.
  // This class creates the table description and data manager info and finally
  // creates the resulting table. First projected columns used in other parts
  // are filled. After a possible HAVING clause and ORDERBY clause is executed,
  // the remaining columns are filled.
  // </synopsis> 

  class TableParseProject
  {
  public:
    // Constructor fills a reference to the tableList objkect.
    TableParseProject (const TableParseTableList& tableList);

    // Get access to the datamanager info.
    Record& dminfo()
      { return dminfo_p; }

    // Get the table description.
    const TableDesc& tableDesc() const
      { return *tableDesc_p; }

    // Get the projected column names.
    const Block<String>& getColumnNames() const
      { return columnNames_p; }

    // Get the projected column expressions.
    const Block<TableExprNode>& getColumnExpr() const
      { return columnExpr_p; }

    // Are expressions used in the column projection?
    Bool hasExpressions() const
      { return nrSelExprUsed_p > 0; }

    // Return the number of projected columns used in other clauses such as HAVING
    // which need to be precalculated.
    uInt nColumnsPreCalc() const
      { return projectExprSubset_p.size(); }

    // Set the column names to the ones to be updated.
    void setColumnNames (const std::vector<CountedPtr<TableParseUpdate>>&);

    // Put the column name and mask name into the update objects.
    void setUpdateNames (std::vector<CountedPtr<TableParseUpdate>>&);
    
    // Set the names to the stored columns of the first table.
    // Resize columnNameMasks_p accordingly.
    void setStoredColumns();

    // Add a column to the list of column names.
    void handleColumn (Int stringType,
                       const String& name,
                       const TableExprNode& expr,
                       const String& newName,
                       const String& newNameMask,
                       const String& newDtype,
                       TableParseQuery&);

    // Handle the selection of a wildcarded column name.
    void handleWildColumn (Int stringType, const String& name);

    // Finish the additions to the block of column names
    // by removing the deleted empty names and creating Expr objects as needed.
    // An exception is thrown if there is a resultset and if columns are selected.
    Table handleColumnFinish (Bool distinct, Bool hasResultSet, TableParseQuery&);

    // Keep the column specification in a create table command.
    void handleColSpec (const String& columnName, const String& likeColName,
                        const String& dataType,
                        const Record& spec, Bool isCOrder);

    // Add columns to the table of ALTER TABLE.
    // The column descriptions have already been added to tableDesc_p.
    void handleAddCol (const Record& dmInfo, Table&);
    
    // Initialize the table and data manager descriptions.
    void initDescriptions (const TableDesc& desc, const Record& dminfo);

    // Set the DataManager info for a new table.
    void setDMInfo (const Record& dminfo)
      { dminfo_p = dminfo;}

    // Find the keyword or column name and create a TableExprNode from it.
    // If <src>tryProj=True</src> it is first tried if the column is a coluymn
    // in the projected table (i.e., result from the SELECT part).
    TableExprNode handleKeyCol (const String& name, Bool tryProj,
                                TableParseQuery&);

    // Make the table projection using the selected columns.
    // The columns in the resulting table are renamed if a new name was given.
    Table project (const Table& tab);

    // Create TableParseUpdate objects for the selected column expressions.
    void makeUpdate (Bool useSel, TableParseQuery& tpq);
    
    // Fill projectExprSelColumn_p telling the columns to be projected
    // at the first stage.
    void makeProjectExprSel();

    // Check if the tables used in selection columns have the same
    // size as the first table given in FROM.
    void checkTableProjSizes() const;

    // Add possible aggregation functions used in projection to the vector.
    void getAggrNodes (std::vector<TableExprNodeRep*>& aggr) const;

    // Check if the COUNT columns are given correctly.
    void checkCountColumns() const;
    
  private:
    // Make the (empty) table for the expression in the SELECT clause.
    Table makeProjectExprTable (TableParseQuery&);

    // Make a data type from the string.
    // It checks if it is compatible with the given (expression) data type.
    DataType makeDataType (DataType dtype, const String& dtstr,
                           const String& colName);

    // Add the description of a column to the table description.
    // ndim < 0 means a scalar column.
    void addColumnDesc (TableDesc& td, DataType dtype,
                        const String& colName, Int options,
                        Int ndim, const IPosition& shape,
                        const String& dmType, const String& dmGroup,
                        const String& comment,
                        const TableRecord& keywordSet,
                        const Vector<String>& unitName,
                        const Record& attributes);

    // Find the ColumnDesc and data manager info of another column (a LIKE column).
    // The LIKE column name can be qualified to use another table.
    // It sets the new column name in the data manager info.
    // An exception is thrown if colName is invalid or unknown.
    std::pair<ColumnDesc,Record> findColumnInfo (const String& colName,
                                                 const String& newColName) const;
  
    //# Data members.
    const TableParseTableList& tableList_p;
    //# Table and data manager description for a series of column descriptions.
    //# TableDesc has no copy ctor, so use a shared_ptr.
    std::shared_ptr<TableDesc> tableDesc_p;
    Record dminfo_p;
    //# Block of selected column names (new name in case of select).
    Block<String> columnNames_p;
    //# Block of selected mask column names (for masked arrays).
    Block<String> columnNameMasks_p;
    //# Block of selected column expressions.
    Block<TableExprNode> columnExpr_p;
    //# The old name for a selected column.
    Block<String> columnOldNames_p;
    //# The new data type for a column.
    Block<String> columnDtypes_p;
    //# The keywords used in a column.
    Block<TableRecord> columnKeywords_p;
    //# Number of real expressions used in selected columns.
    uInt nrSelExprUsed_p;
    //# The projected columns used in the HAVING and ORDERBY clauses.
    Block<uInt>  projectExprSubset_p;
    Block<Bool>  projectExprSelColumn_p;
    //# The first table used when creating a column object.
    //# All other tables used for them should have the same size.
    Table  firstColTable_p;
    String firstColName_p;
  };


} //# NAMESPACE CASACORE - END

#endif
