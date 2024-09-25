//# TaQLJoin.h: Class handling the condition of the join clause
//# Copyright (C) 2022
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

#ifndef TABLES_TAQLJOIN_H
#define TABLES_TAQLJOIN_H

#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/ExprDerNode.h>
#include <casacore/tables/TaQL/ExprNodeSetOpt.h>
#include <vector>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  //# Forward declarations.
  class TableExprNode;
  class TableExprNodeSetElemCont;
  class TableParseJoin;
  
  // <summary>
  // Base class for handling a comparison in the join clause in a TaQL command
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="Tammo Jan Dijkema" date="2022/12/15" tests="tTableGramJoin">
  // </reviewed>
  // <synopsis>
  // </synopsis> 

  class TaQLJoinBase
  {
  public:
    virtual ~TaQLJoinBase() = default;

    // Find the row number. <0 means not found.
    virtual Int64 findRow (const TableExprId&) = 0;
  };


  // <summary>
  // Class holding the row number as the final level in the comparison tree
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="Tammo Jan Dijkema" date="2022/12/15" tests="tTableGramJoin">
  // </reviewed>
  // <synopsis>
  // Objects of this class form the lowest level in a TaQLJoin tree. It only
  // contains a row number telling which row in the join table contains the
  // values at the higher levels in the tree.
  // </synopsis> 

  class TaQLJoinRow : public TaQLJoinBase
  {
  public:
    TaQLJoinRow (Int64 row)
      : itsRow (row)
    {}
    ~TaQLJoinRow() override = default;

    // Return the row number.
    Int64 findRow (const TableExprId&) override;
    
  private:
    Int64 itsRow;
  };


  // <summary>
  // Class holding a comparison part of a join condition
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="Tammo Jan Dijkema" date="2022/12/15" tests="tTableGramJoin">
  // </reviewed>
  // <synopsis>
  // TaQLJoin holds a vector of nested TaQLJoinBase objects, one for each
  // unique value or interval in the join table. It uses TableExprNodeSetOptBase
  // to hold the values and the index in the vector of TaQLJoinBase objects.
  // The data types that can be used in a join condition are the same as those
  // supported by TableExprNodeSetOptBase; that is: integer and string for a
  // discrete value and int, double, datetime and string for an interval.
  // Note that in the interval case int and datetime are handled as double.
  //
  // Note that at the lowest level the vector of TaQLJoinBase objects contains
  // TaQLJoinRow objects holding the join table row number. The TaQLJoin objects
  // at the higher levels contain the index in the vector at the next level.
  //
  // The class contains static functions to build the tree given the left and
  // right part of a comparison, where left is an expression using the main table
  // and right using the join table. The right expression gives the values to
  // build the tree as sketched above.
  //
  // <example>
  // <srcblock>
  //   SELECT t1.COL1, t2.NAME FROM maintab t1 JOIN jointab t2 ON
  //                             t1.COL1 = t2.IDCOL
  //                         AND t1.TIME AROUND t2.TIME IN t2.INTERVAL
  // </srcblock>
  // In this example the main and join table are joined on 2 condition parts.
  // First the equality match of COL1 and IDCOL, second the interval match on TIME.
  // Note that the NAME column from the join table are selected, thus
  // the join is used to find the name for each row in the main table.
  // <br>The join table columns may contain something like below.
  // Thus 4 unique IDCOL values, each having 2 time intervals.
  // <srcblock>
  //   IDCOL     TIME     INTERVAL      NAME
  //     0        tm1        d1        name01
  //     1        tm1        d1        name11
  //     2        tm1        d1        name21
  //     3        tm1        d1        name31
  //     0        tm2        d2        name02
  //     1        tm2        d2        name12
  //     2        tm2        d2        name22
  //     3        tm2        d2        name32
  // </srcblock>
  // The TaQLJoin tree will consist of 3 levels. The first level handles the
  // equality match. It consists of a single TaQLJoin object containing a vector
  // of second level TaQLJoin objects, one for each unique IDCOL value. 
  // The second level handles the TIME match. The third level contains the row
  // number for each IDCOL/TIME pair. Schematically:
  // <srcblock>
  // level 1 TaQLJoin, IDCOL:    0          1          2         3
  // level 2 TaQLJoin, TIME:  tm1 tm2    tm1 tm2    tm1 tm2    tm1 tm2
  // level 3 TaQLJoinRow:      0   1      2   3      4   5      6   7
  // </srcblock>
  // To find the matching join table row means that first the matching
  // IDCOL is found at level 1, thereafter at level 2 the matching interval
  // for that IDCOL.Level 3 gives the correct row number in the join table.
  // </example>
  // </synopsis> 

  class TaQLJoin : public TaQLJoinBase
  {
  public:
    TaQLJoin (const TENShPtr& mainNode, const TENShPtr& joinNode,
              const std::vector<std::shared_ptr<TaQLJoinBase>>& children);
    
    ~TaQLJoin() override = default;

    // Find the row number in the join table for the given row in the main table.
    Int64 findRow (const TableExprId&) override;

    // From the given level on create nested TaQLJoin nodes.
    // It use makeOptDiscrete or makeOptInterval to create the appropriate
    // TableExprNodeSetOptBase object.
    static std::shared_ptr<TaQLJoinBase> createRecursive
    (const std::vector<TableExprNode>& mainNodes,
     const std::vector<TableExprNode>& joinNodes,
     const std::vector<rownr_t>& rows,
     size_t level);

    // Create nested TaQLJoin nodes for a join expression part at the given level
    // using discrete values.
    // Thereafter createRecursive is called for the next level.
    template<typename T>
    static std::shared_ptr<TaQLJoinBase> makeOptDiscrete
    (TableExprNodeRep& node,
     const std::vector<TableExprNode>& mainNodes,
     const std::vector<TableExprNode>& joinNodes,
     const std::vector<rownr_t>& rows,
     size_t level);

    // Create nested TaQLJoin nodes for a join expression part at the given level
    // using intervals.
    // Thereafter createRecursive is called for the next level.
    template<typename T>
    static std::shared_ptr<TaQLJoinBase> makeOptInterval
    (const TableExprNodeSet& set,
     const std::vector<TableExprNode>& mainNodes,
     const std::vector<TableExprNode>& joinNodes,
     const std::vector<rownr_t>& rows,
     size_t level);

  private:
    TENShPtr itsMainNode;
    TENShPtr itsJoinNode;                  // only used for automatic deletion
    TableExprNodeSetOptBase* itsOptSet;    // same ptr as itsJoinNode
    std::vector<std::shared_ptr<TaQLJoinBase>> itsChildren;
  };


  // <summary>
  // A column in a join table
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="Tammo Jan Dijkema" date="2022/12/15" tests="tTableGramJoin">
  // </reviewed>
  // <synopsis>
  // TaQLJoinColumn contains the TableExprNodeColumn object for a
  // column in a join table. It is used to find a value in the join column
  // for a row in the main table. It uses the TableParseJoin object to find
  // the row in the join table given the row in the main table.
  // </synopsis> 

  class TaQLJoinColumn : public TableExprNodeRep
  {
  public:
    TaQLJoinColumn (const TENShPtr& columnNode, const TableParseJoin&);
    
    ~TaQLJoinColumn() override = default;

    // Get the table info for this column.
    TableExprInfo getTableInfo() const override;

    // Get the data for the given id.
    // Using the Join object it maps the row number in the main table
    // to the row number in the join table.
    // <group>
    MArray<Bool> getArrayBool         (const TableExprId& id) override;
    MArray<Int64> getArrayInt         (const TableExprId& id) override;
    MArray<Double> getArrayDouble     (const TableExprId& id) override;
    MArray<DComplex> getArrayDComplex (const TableExprId& id) override;
    MArray<String> getArrayString     (const TableExprId& id) override;
    MArray<MVTime> getArrayDate       (const TableExprId& id) override;
    // </group>

    // Clear the internal data vector (in derived classes).
    virtual void clear();

    // Make the appropriate TaQLJoinColumn object.
    static TableExprNode makeColumnNode (const TENShPtr& columnNode,
                                         const TableParseJoin&);
    
  protected:
    TENShPtr              itsColumn;
    const TableParseJoin& itsJoin;
  };


  // <summary>
  // A scalar column of the given type in a join table
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="Tammo Jan Dijkema" date="2022/12/15" tests="tTableGramJoin">
  // </reviewed>
  // <synopsis>
  // TaQLJoinArrayColumn contains the TableExprNodeColumn object for an array
  // column in a join table. It is used to find a value in the join column
  // for a row in the main table. It uses the TableParseJoin object to find
  // the row in the join table given the row in the main table.
  // </synopsis> 

  class TaQLJoinColumnBool : public TaQLJoinColumn
  {
  public:
    TaQLJoinColumnBool (const TENShPtr& columnNode, const TableParseJoin&);
    ~TaQLJoinColumnBool() override = default;
    Bool getBool (const TableExprId& id) override;
    void clear() override;
  private:
    Vector<Bool> itsData;
  };

  class TaQLJoinColumnInt : public TaQLJoinColumn
  {
  public:
    TaQLJoinColumnInt (const TENShPtr& columnNode, const TableParseJoin&);
    ~TaQLJoinColumnInt() override = default;
    Int64 getInt (const TableExprId& id) override;
    void clear() override;
  private:
    Vector<Int64> itsData;
  };

  class TaQLJoinColumnDouble : public TaQLJoinColumn
  {
  public:
    TaQLJoinColumnDouble (const TENShPtr& columnNode, const TableParseJoin&);
    ~TaQLJoinColumnDouble() override = default;
    Double getDouble (const TableExprId& id) override;
    void clear() override;
  private:
    Vector<Double> itsData;
  };

  class TaQLJoinColumnDComplex : public TaQLJoinColumn
  {
  public:
    TaQLJoinColumnDComplex (const TENShPtr& columnNode, const TableParseJoin&);
    ~TaQLJoinColumnDComplex() override = default;
    DComplex getDComplex (const TableExprId& id) override;
    void clear() override;
  private:
    Vector<DComplex> itsData;
  };

  class TaQLJoinColumnString : public TaQLJoinColumn
  {
  public:
    TaQLJoinColumnString (const TENShPtr& columnNode, const TableParseJoin&);
    ~TaQLJoinColumnString() override = default;
    String getString (const TableExprId& id) override;
    void clear() override;
  private:
    Vector<String> itsData;
  };

  class TaQLJoinColumnDate : public TaQLJoinColumn
  {
  public:
    TaQLJoinColumnDate (const TENShPtr& columnNode, const TableParseJoin&);
    ~TaQLJoinColumnDate() override = default;
    MVTime getDate (const TableExprId& id) override;
    void clear() override;
  private:
    Vector<MVTime> itsData;
  };



  // <summary>
  // The rowid in a join table
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="Tammo Jan Dijkema" date="2022/12/15" tests="tTableGramJoin">
  // </reviewed>
  // <synopsis>
  // TaQLJoinRowid contains row sequence numbers of the matching rows
  // in the join table.
  // </synopsis> 

  class TaQLJoinRowid : public TableExprNodeRep
  {
  public:
    TaQLJoinRowid (const TableExprInfo&, const TableParseJoin&);
    ~TaQLJoinRowid() override = default;
    TableExprInfo getTableInfo() const override;
    // Get the data (rowid in join table) for the given id.
    // Using the Join object it maps the row number in the main table
    // to the row number in the join table.
    Int64 getInt (const TableExprId& id) override;
  private:
    TableExprInfo         itsTabInfo;
    const TableParseJoin& itsJoin;
  };


} //# NAMESPACE CASACORE - END

#endif
