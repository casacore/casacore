//# ExprGroupAggrFunc.h: The various aggregation functions
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

#ifndef TABLES_EXPRGROUPAGGRFUNC_H
#define TABLES_EXPRGROUPAGGRFUNC_H

//# Includes
#include <tables/Tables/ExprGroup.h>
#include <vector>


namespace casa { //# NAMESPACE CASA - BEGIN

//# Forward Declaration
class TableExprNodeArrayColumn;


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


  // min,max,sum,sumsqr,product,count,mean,variance,stddev,rms,median,fractile,any,all
  class TableExprGroupCountAll: public TableExprGroupFuncInt
  {
  public:
    TableExprGroupCountAll();
    virtual ~TableExprGroupCountAll();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
    // Set result in case it is known directly.
    void setResult (Int64 cnt)
      { itsValue = cnt; }
  };

  class TableExprGroupCount: public TableExprGroupFuncInt
  {
  public:
    explicit TableExprGroupCount (TableExprAggrNode& node);
    virtual ~TableExprGroupCount();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  private:
    TableExprNodeArrayColumn* itsColumn;
  };

  class TableExprGroupAny: public TableExprGroupFuncBool
  {
  public:
    TableExprGroupAny();
    virtual ~TableExprGroupAny();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };

  class TableExprGroupAll: public TableExprGroupFuncBool
  {
  public:
    TableExprGroupAll();
    virtual ~TableExprGroupAll();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };

  class TableExprGroupNTrue: public TableExprGroupFuncInt
  {
  public:
    TableExprGroupNTrue();
    virtual ~TableExprGroupNTrue();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };

  class TableExprGroupNFalse: public TableExprGroupFuncInt
  {
  public:
    TableExprGroupNFalse();
    virtual ~TableExprGroupNFalse();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };

  class TableExprGroupMinInt: public TableExprGroupFuncInt
  {
  public:
    TableExprGroupMinInt();
    virtual ~TableExprGroupMinInt();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };

  class TableExprGroupMaxInt: public TableExprGroupFuncInt
  {
  public:
    TableExprGroupMaxInt();
    virtual ~TableExprGroupMaxInt();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };

  class TableExprGroupSumInt: public TableExprGroupFuncInt
  {
  public:
    TableExprGroupSumInt();
    virtual ~TableExprGroupSumInt();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };

  class TableExprGroupProductInt: public TableExprGroupFuncInt
  {
  public:
    TableExprGroupProductInt();
    virtual ~TableExprGroupProductInt();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };

  class TableExprGroupSumSqrInt: public TableExprGroupFuncInt
  {
  public:
    TableExprGroupSumSqrInt();
    virtual ~TableExprGroupSumSqrInt();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };


  class TableExprGroupMinDouble: public TableExprGroupFuncDouble
  {
  public:
    TableExprGroupMinDouble();
    virtual ~TableExprGroupMinDouble();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };

  class TableExprGroupMaxDouble: public TableExprGroupFuncDouble
  {
  public:
    TableExprGroupMaxDouble();
    virtual ~TableExprGroupMaxDouble();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };

  class TableExprGroupSumDouble: public TableExprGroupFuncDouble
  {
  public:
    TableExprGroupSumDouble();
    virtual ~TableExprGroupSumDouble();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };

  class TableExprGroupProductDouble: public TableExprGroupFuncDouble
  {
  public:
    TableExprGroupProductDouble();
    virtual ~TableExprGroupProductDouble();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };

  class TableExprGroupSumSqrDouble: public TableExprGroupFuncDouble
  {
  public:
    TableExprGroupSumSqrDouble();
    virtual ~TableExprGroupSumSqrDouble();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };

  class TableExprGroupMeanDouble: public TableExprGroupFuncDouble
  {
  public:
    TableExprGroupMeanDouble();
    virtual ~TableExprGroupMeanDouble();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
    virtual void finish();
  private:
    Int64 itsNr;
  };

  class TableExprGroupVarianceDouble: public TableExprGroupFuncDouble
  {
  public:
    TableExprGroupVarianceDouble();
    virtual ~TableExprGroupVarianceDouble();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
    virtual void finish();
  protected:
    Int64  itsNr;
    Double itsM2;
  };

  class TableExprGroupStdDevDouble: public TableExprGroupVarianceDouble
  {
  public:
    TableExprGroupStdDevDouble();
    virtual ~TableExprGroupStdDevDouble();
    virtual void finish();
  };

  class TableExprGroupRmsDouble: public TableExprGroupFuncDouble
  {
  public:
    TableExprGroupRmsDouble();
    virtual ~TableExprGroupRmsDouble();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
    virtual void finish();
  private:
    Int64 itsNr;
  };

  class TableExprGroupFractileDouble: public TableExprGroupFuncDouble
  {
  public:
    explicit TableExprGroupFractileDouble (Double fractile);
    virtual ~TableExprGroupFractileDouble();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
    virtual void finish();
  private:
    Double         itsFrac;
    vector<Double> itsValues;
  };


  class TableExprGroupSumDComplex: public TableExprGroupFuncDComplex
  {
  public:
    TableExprGroupSumDComplex();
    virtual ~TableExprGroupSumDComplex();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };

  class TableExprGroupProductDComplex: public TableExprGroupFuncDComplex
  {
  public:
    TableExprGroupProductDComplex();
    virtual ~TableExprGroupProductDComplex();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };

  class TableExprGroupSumSqrDComplex: public TableExprGroupFuncDComplex
  {
  public:
    TableExprGroupSumSqrDComplex();
    virtual ~TableExprGroupSumSqrDComplex();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
  };

  class TableExprGroupMeanDComplex: public TableExprGroupFuncDComplex
  {
  public:
    TableExprGroupMeanDComplex();
    virtual ~TableExprGroupMeanDComplex();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id);
    virtual void finish();
  private:
    Int64 itsNr;
  };


} //# NAMESPACE CASA - END

#endif
