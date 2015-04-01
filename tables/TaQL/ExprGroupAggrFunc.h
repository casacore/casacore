//# ExprGroupAggrFunc.h: The various scalar aggregation functions
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
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/ExprGroup.h>
#include <vector>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

  //# Forward Declaration
  class TableExprNodeArrayColumn;


  // <summary>
  // Aggregate class counting number of rows in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class counting number of rows in a group.
  // </synopsis>
  class TableExprGroupCountAll: public TableExprGroupFuncInt
  {
  public:
    explicit TableExprGroupCountAll (TableExprNodeRep* node);
    virtual ~TableExprGroupCountAll();
    virtual void apply (const TableExprId& id);
    // Set result in case it is known directly.
    void setResult (Int64 cnt)
      { itsValue = cnt; }
  };

  // <summary>
  // Aggregate class counting number of rows in a group containing a value
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class counting number of rows in a group containing a value.
  // </synopsis>
  class TableExprGroupCount: public TableExprGroupFuncInt
  {
  public:
    explicit TableExprGroupCount (TableExprNodeRep* node);
    virtual ~TableExprGroupCount();
    virtual void apply (const TableExprId& id);
  private:
    TableExprNodeArrayColumn* itsColumn;
  };

  // <summary>
  // Aggregate class counting if any value in a group is true
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class counting if any value in a group is true.
  // </synopsis>
  class TableExprGroupAny: public TableExprGroupFuncBool
  {
  public:
    explicit TableExprGroupAny (TableExprNodeRep* node);
    virtual ~TableExprGroupAny();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class counting if all values in a group are true
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class counting if all values in a group are true.
  // </synopsis>
  class TableExprGroupAll: public TableExprGroupFuncBool
  {
  public:
    explicit TableExprGroupAll (TableExprNodeRep* node);
    virtual ~TableExprGroupAll();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class counting the number of true values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class counting the number of true values in a group.
  // </synopsis>
  class TableExprGroupNTrue: public TableExprGroupFuncInt
  {
  public:
    explicit TableExprGroupNTrue (TableExprNodeRep* node);
    virtual ~TableExprGroupNTrue();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class counting the number of false values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class counting the number of false values in a group.
  // </synopsis>
  class TableExprGroupNFalse: public TableExprGroupFuncInt
  {
  public:
    explicit TableExprGroupNFalse (TableExprNodeRep* node);
    virtual ~TableExprGroupNFalse();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining the minimum integer value in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the minimum integer value in a group.
  // </synopsis>
  class TableExprGroupMinInt: public TableExprGroupFuncInt
  {
  public:
    explicit TableExprGroupMinInt (TableExprNodeRep* node);
    virtual ~TableExprGroupMinInt();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining the maximum integer value in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the maximum integer value in a group.
  // </synopsis>
  class TableExprGroupMaxInt: public TableExprGroupFuncInt
  {
  public:
    explicit TableExprGroupMaxInt (TableExprNodeRep* node);
    virtual ~TableExprGroupMaxInt();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining the sum of integer values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the sum of integer values in a group.
  // </synopsis>
  class TableExprGroupSumInt: public TableExprGroupFuncInt
  {
  public:
    explicit TableExprGroupSumInt (TableExprNodeRep* node);
    virtual ~TableExprGroupSumInt();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining the product of integer values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the product of integer values in a group.
  // </synopsis>
  class TableExprGroupProductInt: public TableExprGroupFuncInt
  {
  public:
    explicit TableExprGroupProductInt (TableExprNodeRep* node);
    virtual ~TableExprGroupProductInt();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining the sum of squares of integer values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the sum of squares of integer values in a group.
  // </synopsis>
  class TableExprGroupSumSqrInt: public TableExprGroupFuncInt
  {
  public:
    explicit TableExprGroupSumSqrInt (TableExprNodeRep* node);
    virtual ~TableExprGroupSumSqrInt();
    virtual void apply (const TableExprId& id);
  };


  // <summary>
  // Aggregate class determining the minimum double value in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the minimum double value in a group.
  // </synopsis>
  class TableExprGroupMinDouble: public TableExprGroupFuncDouble
  {
  public:
    explicit TableExprGroupMinDouble (TableExprNodeRep* node);
    virtual ~TableExprGroupMinDouble();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining the maximum double value in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the maximum double value in a group.
  // </synopsis>
  class TableExprGroupMaxDouble: public TableExprGroupFuncDouble
  {
  public:
    explicit TableExprGroupMaxDouble (TableExprNodeRep* node);
    virtual ~TableExprGroupMaxDouble();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining the sum of double values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the sum of double values in a group.
  // </synopsis>
  class TableExprGroupSumDouble: public TableExprGroupFuncDouble
  {
  public:
    explicit TableExprGroupSumDouble (TableExprNodeRep* node);
    virtual ~TableExprGroupSumDouble();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining the product of double values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the product of double values in a group.
  // </synopsis>
  class TableExprGroupProductDouble: public TableExprGroupFuncDouble
  {
  public:
    explicit TableExprGroupProductDouble (TableExprNodeRep* node);
    virtual ~TableExprGroupProductDouble();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining the sum of squares of double values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the sum of squares of double values in a group.
  // </synopsis>
  class TableExprGroupSumSqrDouble: public TableExprGroupFuncDouble
  {
  public:
    explicit TableExprGroupSumSqrDouble (TableExprNodeRep* node);
    virtual ~TableExprGroupSumSqrDouble();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining the mean of values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the mean of values in a group.
  // </synopsis>
  class TableExprGroupMeanDouble: public TableExprGroupFuncDouble
  {
  public:
    explicit TableExprGroupMeanDouble (TableExprNodeRep* node);
    virtual ~TableExprGroupMeanDouble();
    virtual void apply (const TableExprId& id);
    virtual void finish();
  private:
    Int64 itsNr;
  };

  // <summary>
  // Aggregate class determining the variance of values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the variance of values in a group.
  // It uses a running algorithm
  // (see en.wikipedia.org/wiki/Algorithms_for_calculating_variance)
  // </synopsis>
  class TableExprGroupVarianceDouble: public TableExprGroupFuncDouble
  {
  public:
    explicit TableExprGroupVarianceDouble (TableExprNodeRep* node);
    virtual ~TableExprGroupVarianceDouble();
    virtual void apply (const TableExprId& id);
    virtual void finish();
  protected:
    Int64  itsNr;
    Double itsM2;
  };

  // <summary>
  // Aggregate class determining the standard deviation of values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the standard deviation of values in a group.
  // It uses a running algorithm
  // (see en.wikipedia.org/wiki/Algorithms_for_calculating_variance)
  // </synopsis>
  class TableExprGroupStdDevDouble: public TableExprGroupVarianceDouble
  {
  public:
    explicit TableExprGroupStdDevDouble (TableExprNodeRep* node);
    virtual ~TableExprGroupStdDevDouble();
    virtual void finish();
  };

  // <summary>
  // Aggregate class determining the RMS of values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the RMS of values in a group.
  // </synopsis>
  class TableExprGroupRmsDouble: public TableExprGroupFuncDouble
  {
  public:
    explicit TableExprGroupRmsDouble (TableExprNodeRep* node);
    virtual ~TableExprGroupRmsDouble();
    virtual void apply (const TableExprId& id);
    virtual void finish();
  private:
    Int64 itsNr;
  };

  // <summary>
  // Aggregate class determining the fractile of values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the fractile of values in a group.
  // <br>It is a lazy aggregate class, thus <src>apply</src> does nothing.
  // Instead, <src>getDouble</src> assembles the values and determines the
  // fractile.
  // </synopsis>
  class TableExprGroupFractileDouble: public TableExprGroupFuncDouble
  {
  public:
    explicit TableExprGroupFractileDouble (TableExprNodeRep* node,
                                           Double fractile);
    virtual ~TableExprGroupFractileDouble();
    virtual Bool isLazy() const;
    virtual void apply (const TableExprId& id);
    virtual Double getDouble (const vector<TableExprId>& ids);
  private:
    Double itsFrac;
  };


  // <summary>
  // Aggregate class determining the sum of complex values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the sum of complex values in a group.
  // </synopsis>
  class TableExprGroupSumDComplex: public TableExprGroupFuncDComplex
  {
  public:
    explicit TableExprGroupSumDComplex (TableExprNodeRep* node);
    virtual ~TableExprGroupSumDComplex();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining the product of complex values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the product of complex values in a group.
  // </synopsis>
  class TableExprGroupProductDComplex: public TableExprGroupFuncDComplex
  {
  public:
    explicit TableExprGroupProductDComplex (TableExprNodeRep* node);
    virtual ~TableExprGroupProductDComplex();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining the sum of squares of complex values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the sum of squares of complex values in a group.
  // </synopsis>
  class TableExprGroupSumSqrDComplex: public TableExprGroupFuncDComplex
  {
  public:
    explicit TableExprGroupSumSqrDComplex (TableExprNodeRep* node);
    virtual ~TableExprGroupSumSqrDComplex();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining the mean of complex values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the mean of complex values in a group.
  // </synopsis>
  class TableExprGroupMeanDComplex: public TableExprGroupFuncDComplex
  {
  public:
    explicit TableExprGroupMeanDComplex (TableExprNodeRep* node);
    virtual ~TableExprGroupMeanDComplex();
    virtual void apply (const TableExprId& id);
    virtual void finish();
  private:
    Int64 itsNr;
  };


} //# NAMESPACE CASACORE - END

#endif
