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
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

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
    TableExprUDFNode (UDFBase* udf, const Table&,
                      const TableExprNodeSet& source);

    // Destructor
    virtual ~TableExprUDFNode();

    // Is the UDF an aggregate function?
    Bool isAggregate() const
      { return itsUDF->isAggregate(); }

    // Get the nodes representing an aggregate function.
    virtual void getAggrNodes (vector<TableExprNodeRep*>& aggr);

    // Get the nodes representing a table column.
    virtual void getColumnNodes (vector<TableExprNodeRep*>& cols);
  
    // UDFs do not need a TableExprGroupFuncBase,
    // so TableExprGroupNull is returned.
    CountedPtr<TableExprGroupFuncBase> makeGroupAggrFunc();

    // Functions to get the desired result of a function
    // <group>
    virtual Bool      getBool     (const TableExprId& id);
    virtual Int64     getInt      (const TableExprId& id);
    virtual Double    getDouble   (const TableExprId& id);
    virtual DComplex  getDComplex (const TableExprId& id);
    virtual String    getString   (const TableExprId& id);
    virtual TaqlRegex getRegex    (const TableExprId& id);
    virtual MVTime    getDate     (const TableExprId& id);
    // </group>

  private:
    UDFBase* itsUDF;
  };


} //# NAMESPACE CASACORE - END

#endif
