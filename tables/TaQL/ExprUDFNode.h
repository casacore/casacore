//# ExprUDFNode.h: Class representing a scalar UDF in select expression
//# Copyright (C) 2010
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

#ifndef TABLES_EXPRUDFNODE_H
#define TABLES_EXPRUDFNODE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/ExprNodeRep.h>
#include <casacore/tables/TaQL/UDFBase.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  //# Forward Declarations
  class TableExprNodeSet;

  // <summary>
  // Class representing a scalar UDF in select expression
  // </summary>

  // <use visibility=local>

  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
  // </reviewed>
  // <prerequisite>
  //# Classes you should understand before using this one.
  //   <li> <linkto class=TableExprNodeMulti>TableExprNodeMulti</linkto>
  // </prerequisite>

  // <synopsis>
  // This class represents a function in a table select tree.
  // The <src>rownumber</src> function is represented by class
  // <linkto class=TableExprNodeRownr>TableExprNodeRownr</linkto>.
  // The <src>rowid</src> function is represented by class
  // <linkto class=TableExprNodeRowid>TableExprNodeRowid</linkto>.
  // The <src>rand</src> function is represented by class
  // <linkto class=TableExprNodeRandom>TableExprNodeRandom</linkto>.
  // <p>
  // When one wants to add a function to the table selection grammar,
  // the following has to be done:
  // <ul>
  //  <li> Add the function to the enum below.
  //  <li> Implement the function in the get functions in ExprFuncNode(Array).cc.
  //  <li> Implement the function in the checkOperands in ExprFuncNode.cc.
  //  <li> Declare and define the function in ExprNode.h (for C++ binding).
  //  <li> Add the function to findFunc in TableParse.cc (for TaQL).
  // </ul>
  // </synopsis> 


  class TableExprUDFNode: public TableExprNodeMulti
  {
  public:

    // Constructor
    TableExprUDFNode (const std::shared_ptr<UDFBase>& udf,
                      const TableExprInfo&,
                      const TableExprNodeSet& source);

    // Destructor
    ~TableExprUDFNode() override = default;

    // Is the UDF an aggregate function?
    Bool isAggregate() const override
      { return itsUDF->isAggregate(); }

    // Get the table info.
    TableExprInfo getTableInfo() const override;
  
    // Flatten the node tree by adding the node and its children to the vector.
    void flattenTree (std::vector<TableExprNodeRep*>&) override;
  
    // Do not apply the selection.
    void disableApplySelection() override;

    // If needed, let the UDF re-create column objects for a selection of rows.
    // It calls the function recreateColumnObjects.
    void applySelection (const Vector<rownr_t>& rownrs) override;

    // UDFs do not need a TableExprGroupFuncBase,
    // so TableExprGroupNull is returned.
    std::shared_ptr<TableExprGroupFuncBase> makeGroupAggrFunc() override;

    // Functions to get the desired result of a function
    // <group>
    Bool      getBool     (const TableExprId& id) override;
    Int64     getInt      (const TableExprId& id) override;
    Double    getDouble   (const TableExprId& id) override;
    DComplex  getDComplex (const TableExprId& id) override;
    String    getString   (const TableExprId& id) override;
    TaqlRegex getRegex    (const TableExprId& id) override;
    MVTime    getDate     (const TableExprId& id) override;
    // </group>

  private:
    TableExprInfo            itsTableInfo;
    std::shared_ptr<UDFBase> itsUDF;
  };


} //# NAMESPACE CASACORE - END

#endif
