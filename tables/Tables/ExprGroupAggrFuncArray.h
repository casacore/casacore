//# ExprGroupAggrFuncArray.h: The various array aggregation functions
//# Copyright (C) 2013
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
//# $Id: TaQLNode.h 21051 2011-04-20 11:46:29Z gervandiepen $

#ifndef TABLES_EXPRGROUPAGGRFUNCARRAY_H
#define TABLES_EXPRGROUPAGGRFUNCARRAY_H

//# Includes
#include <tables/Tables/ExprGroup.h>
#include <vector>


namespace casa { //# NAMESPACE CASA - BEGIN


// <summary>
// Envelope class for a node in the raw TaQL parse tree.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto group=TableGram.h#TableGramFunctions>TableGram</linkto>
//   <li> Note 199 describing
//        <a href="../notes/199.html">
//        TaQL</a>
// </prerequisite>

// <synopsis>
// The result of parsing a TaQL command is stored in TaQLNode objects.
// Each part of the command can have its own specialized
// <linkto class=TaQLNodeRep>TaQLNodeRep</linkto> object, which forms
// the letter in the TaQLNode envelope.
// <br>The actual scanning/parsing of the command is done using flex/bison
// as defined in the TableGram files.
// </synopsis> 

// <motivation>
// The letter-envelope idiom (counted pointer) makes if much easier
// to keep track of memory, especially in the case of exceptions.
// </motivation>


  class TableExprGroupArrAny: public TableExprGroupFuncBool
  {
  public:
    TableExprGroupArrAny();
    virtual ~TableExprGroupArrAny();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };

  class TableExprGroupArrAll: public TableExprGroupFuncBool
  {
  public:
    TableExprGroupArrAll();
    virtual ~TableExprGroupArrAll();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };

  class TableExprGroupArrNTrue: public TableExprGroupFuncInt
  {
  public:
    TableExprGroupArrNTrue();
    virtual ~TableExprGroupArrNTrue();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };

  class TableExprGroupArrNFalse: public TableExprGroupFuncInt
  {
  public:
    TableExprGroupArrNFalse();
    virtual ~TableExprGroupArrNFalse();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };

  class TableExprGroupMinArrInt: public TableExprGroupFuncInt
  {
  public:
    TableExprGroupMinArrInt();
    virtual ~TableExprGroupMinArrInt();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };

  class TableExprGroupMaxArrInt: public TableExprGroupFuncInt
  {
  public:
    TableExprGroupMaxArrInt();
    virtual ~TableExprGroupMaxArrInt();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };

  class TableExprGroupSumArrInt: public TableExprGroupFuncInt
  {
  public:
    TableExprGroupSumArrInt();
    virtual ~TableExprGroupSumArrInt();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };

  class TableExprGroupProductArrInt: public TableExprGroupFuncInt
  {
  public:
    TableExprGroupProductArrInt();
    virtual ~TableExprGroupProductArrInt();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };

  class TableExprGroupSumSqrArrInt: public TableExprGroupFuncInt
  {
  public:
    TableExprGroupSumSqrArrInt();
    virtual ~TableExprGroupSumSqrArrInt();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };


  class TableExprGroupMinArrDouble: public TableExprGroupFuncDouble
  {
  public:
    TableExprGroupMinArrDouble();
    virtual ~TableExprGroupMinArrDouble();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };

  class TableExprGroupMaxArrDouble: public TableExprGroupFuncDouble
  {
  public:
    TableExprGroupMaxArrDouble();
    virtual ~TableExprGroupMaxArrDouble();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };

  class TableExprGroupSumArrDouble: public TableExprGroupFuncDouble
  {
  public:
    TableExprGroupSumArrDouble();
    virtual ~TableExprGroupSumArrDouble();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };

  class TableExprGroupProductArrDouble: public TableExprGroupFuncDouble
  {
  public:
    TableExprGroupProductArrDouble();
    virtual ~TableExprGroupProductArrDouble();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };

  class TableExprGroupSumSqrArrDouble: public TableExprGroupFuncDouble
  {
  public:
    TableExprGroupSumSqrArrDouble();
    virtual ~TableExprGroupSumSqrArrDouble();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };

  class TableExprGroupMeanArrDouble: public TableExprGroupFuncDouble
  {
  public:
    TableExprGroupMeanArrDouble();
    virtual ~TableExprGroupMeanArrDouble();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
    virtual void finish();
  private:
    Int64 itsNr;
  };

  class TableExprGroupVarianceArrDouble: public TableExprGroupFuncDouble
  {
  public:
    TableExprGroupVarianceArrDouble();
    virtual ~TableExprGroupVarianceArrDouble();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
    virtual void finish();
  protected:
    Int64  itsNr;
    Double itsM2;
  };

  class TableExprGroupStdDevArrDouble: public TableExprGroupVarianceArrDouble
  {
  public:
    TableExprGroupStdDevArrDouble();
    virtual ~TableExprGroupStdDevArrDouble();
    virtual void finish();
  };

  class TableExprGroupRmsArrDouble: public TableExprGroupFuncDouble
  {
  public:
    TableExprGroupRmsArrDouble();
    virtual ~TableExprGroupRmsArrDouble();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
    virtual void finish();
  private:
    Int64 itsNr;
  };

  class TableExprGroupFractileArrDouble: public TableExprGroupFuncDouble
  {
  public:
    explicit TableExprGroupFractileArrDouble (Double fractile);
    virtual ~TableExprGroupFractileArrDouble();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
    virtual void finish();
  private:
    Double                 itsFrac;
    vector<Array<Double> > itsValues;
  };


  class TableExprGroupSumArrDComplex: public TableExprGroupFuncDComplex
  {
  public:
    TableExprGroupSumArrDComplex();
    virtual ~TableExprGroupSumArrDComplex();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };

  class TableExprGroupProductArrDComplex: public TableExprGroupFuncDComplex
  {
  public:
    TableExprGroupProductArrDComplex();
    virtual ~TableExprGroupProductArrDComplex();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };

  class TableExprGroupSumSqrArrDComplex: public TableExprGroupFuncDComplex
  {
  public:
    TableExprGroupSumSqrArrDComplex();
    virtual ~TableExprGroupSumSqrArrDComplex();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };

  class TableExprGroupMeanArrDComplex: public TableExprGroupFuncDComplex
  {
  public:
    TableExprGroupMeanArrDComplex();
    virtual ~TableExprGroupMeanArrDComplex();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
    virtual void finish();
  private:
    Int64 itsNr;
  };


} //# NAMESPACE CASA - END

#endif
