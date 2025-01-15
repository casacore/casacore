//# ExprGroupAggrFuncArray.h: The various array reduction aggregation functions
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#ifndef TABLES_EXPRGROUPAGGRFUNCARRAY_H
#define TABLES_EXPRGROUPAGGRFUNCARRAY_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/ExprGroup.h>
#include <vector>


namespace casacore { //# NAMESPACE CASACORE - BEGIN


  // <summary>
  // Aggregate class counting if any array value in a group is true
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class counting if any array value in a group is true.
  // </synopsis>
  class TableExprGroupArrayAny: public TableExprGroupFuncBool
  {
  public:
    TableExprGroupArrayAny (TableExprNodeRep* node);
    virtual ~TableExprGroupArrayAny();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class counting if all array values in a group are true
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class counting if all array values in a group are true.
  // </synopsis>
  class TableExprGroupArrayAll: public TableExprGroupFuncBool
  {
  public:
    TableExprGroupArrayAll (TableExprNodeRep* node);
    virtual ~TableExprGroupArrayAll();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class counting the number of true array values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class counting the number of true array values in a group.
  // </synopsis>
  class TableExprGroupArrayNTrue: public TableExprGroupFuncInt
  {
  public:
    TableExprGroupArrayNTrue (TableExprNodeRep* node);
    virtual ~TableExprGroupArrayNTrue();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class counting the number of false array values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class counting the number of false array values in a group.
  // </synopsis>
  class TableExprGroupArrayNFalse: public TableExprGroupFuncInt
  {
  public:
    TableExprGroupArrayNFalse (TableExprNodeRep* node);
    virtual ~TableExprGroupArrayNFalse();
    virtual void apply (const TableExprId& id);
  };


  // <summary>
  // Aggregate class determining the minimum integer array value in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the minimum integer array value in a group.
  // </synopsis>
  class TableExprGroupMinArrayInt: public TableExprGroupFuncInt
  {
  public:
    TableExprGroupMinArrayInt (TableExprNodeRep* node);
    virtual ~TableExprGroupMinArrayInt();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining the maximum integer array value in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the maximum integer array value in a group.
  // </synopsis>
  class TableExprGroupMaxArrayInt: public TableExprGroupFuncInt
  {
  public:
    TableExprGroupMaxArrayInt (TableExprNodeRep* node);
    virtual ~TableExprGroupMaxArrayInt();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining the sum of integer array values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the sum of integer array values in a group.
  // </synopsis>
  class TableExprGroupSumArrayInt: public TableExprGroupFuncInt
  {
  public:
    TableExprGroupSumArrayInt (TableExprNodeRep* node);
    virtual ~TableExprGroupSumArrayInt();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining the product of integer array values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the product of integer array values in a group.
  // </synopsis>
  class TableExprGroupProductArrayInt: public TableExprGroupFuncInt
  {
  public:
    TableExprGroupProductArrayInt (TableExprNodeRep* node);
    virtual ~TableExprGroupProductArrayInt();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining the sum of squares of integer array values
  // in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the sum of squares of integer array values
  // in a group.
  // </synopsis>
  class TableExprGroupSumSqrArrayInt: public TableExprGroupFuncInt
  {
  public:
    TableExprGroupSumSqrArrayInt (TableExprNodeRep* node);
    virtual ~TableExprGroupSumSqrArrayInt();
    virtual void apply (const TableExprId& id);
  };


  // <summary>
  // Aggregate class determining the minimum double array value in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the minimum double array value in a group.
  // </synopsis>
  class TableExprGroupMinArrayDouble: public TableExprGroupFuncDouble
  {
  public:
    TableExprGroupMinArrayDouble (TableExprNodeRep* node);
    virtual ~TableExprGroupMinArrayDouble();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining the maximum double array value in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the maximum double array value in a group.
  // </synopsis>
  class TableExprGroupMaxArrayDouble: public TableExprGroupFuncDouble
  {
  public:
    TableExprGroupMaxArrayDouble (TableExprNodeRep* node);
    virtual ~TableExprGroupMaxArrayDouble();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining the sum of double array values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the sum of double array values in a group.
  // </synopsis>
  class TableExprGroupSumArrayDouble: public TableExprGroupFuncDouble
  {
  public:
    TableExprGroupSumArrayDouble (TableExprNodeRep* node);
    virtual ~TableExprGroupSumArrayDouble();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining the product of double array values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the product of double array values in a group.
  // </synopsis>
  class TableExprGroupProductArrayDouble: public TableExprGroupFuncDouble
  {
  public:
    TableExprGroupProductArrayDouble (TableExprNodeRep* node);
    virtual ~TableExprGroupProductArrayDouble();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining the sum of squares of double array values
  // in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the sum of squares of double array values
  // in a group.
  // </synopsis>
  class TableExprGroupSumSqrArrayDouble: public TableExprGroupFuncDouble
  {
  public:
    TableExprGroupSumSqrArrayDouble (TableExprNodeRep* node);
    virtual ~TableExprGroupSumSqrArrayDouble();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining the mean of array values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the mean of array values in a group.
  // </synopsis>
  class TableExprGroupMeanArrayDouble: public TableExprGroupFuncDouble
  {
  public:
    TableExprGroupMeanArrayDouble (TableExprNodeRep* node);
    virtual ~TableExprGroupMeanArrayDouble();
    virtual void apply (const TableExprId& id);
    virtual void finish();
  private:
    Int64 itsNr;
  };

  // <summary>
  // Aggregate class determining the variance of array values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the variance of array values in a group.
  // It uses a running algorithm
  // (see en.wikipedia.org/wiki/Algorithms_for_calculating_variance)
  // </synopsis>
  class TableExprGroupVarianceArrayDouble: public TableExprGroupFuncDouble
  {
  public:
    TableExprGroupVarianceArrayDouble (TableExprNodeRep* node, uInt ddof);
    virtual ~TableExprGroupVarianceArrayDouble();
    virtual void apply (const TableExprId& id);
    virtual void finish();
  protected:
    uInt   itsDdof;
    Int64  itsNr;
    Double itsCurMean;
  };

  // <summary>
  // Aggregate class determining the standard devation of array values
  // in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the standard deviation of array values
  // in a group. It uses a running algorithm
  // (see en.wikipedia.org/wiki/Algorithms_for_calculating_variance)
  // </synopsis>
  class TableExprGroupStdDevArrayDouble: public TableExprGroupVarianceArrayDouble
  {
  public:
    TableExprGroupStdDevArrayDouble (TableExprNodeRep* node, uInt ddof);
    virtual ~TableExprGroupStdDevArrayDouble();
    virtual void finish();
  };

  // <summary>
  // Aggregate class determining the RMS of array values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the RMS of array values in a group.
  // </synopsis>
  class TableExprGroupRmsArrayDouble: public TableExprGroupFuncDouble
  {
  public:
    TableExprGroupRmsArrayDouble (TableExprNodeRep* node);
    virtual ~TableExprGroupRmsArrayDouble();
    virtual void apply (const TableExprId& id);
    virtual void finish();
  private:
    Int64 itsNr;
  };

  // <summary>
  // Aggregate class determining the fractile of array values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the fractile of array values in a group.
  // <br>It is a lazy aggregate class, thus <src>apply</src> does nothing.
  // Instead, <src>getDouble</src> assembles the values and determines the
  // fractile.
  // </synopsis>
  class TableExprGroupFractileArrayDouble: public TableExprGroupFuncDouble
  {
  public:
    explicit TableExprGroupFractileArrayDouble (TableExprNodeRep* node,
                                                Double fractile);
    virtual ~TableExprGroupFractileArrayDouble();
    virtual Bool isLazy() const;
    virtual void apply (const TableExprId& id);
    virtual Double getDouble (const vector<TableExprId>& ids);
  private:
    Double itsFrac;
  };


  // <summary>
  // Aggregate class determining the sum of complex array values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the sum of complex array values in a group.
  // </synopsis>
  class TableExprGroupSumArrayDComplex: public TableExprGroupFuncDComplex
  {
  public:
    TableExprGroupSumArrayDComplex (TableExprNodeRep* node);
    virtual ~TableExprGroupSumArrayDComplex();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining the product of complex array values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the product of complex array values in a group.
  // </synopsis>
  class TableExprGroupProductArrayDComplex: public TableExprGroupFuncDComplex
  {
  public:
    TableExprGroupProductArrayDComplex (TableExprNodeRep* node);
    virtual ~TableExprGroupProductArrayDComplex();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining the sum of squares of complex array values
  // in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the sum of squares of complex array values
  // in a group.
  // </synopsis>
  class TableExprGroupSumSqrArrayDComplex: public TableExprGroupFuncDComplex
  {
  public:
    TableExprGroupSumSqrArrayDComplex (TableExprNodeRep* node);
    virtual ~TableExprGroupSumSqrArrayDComplex();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining the mean of complex array values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the mean of complex array values in a group.
  // </synopsis>
  class TableExprGroupMeanArrayDComplex: public TableExprGroupFuncDComplex
  {
  public:
    TableExprGroupMeanArrayDComplex (TableExprNodeRep* node);
    virtual ~TableExprGroupMeanArrayDComplex();
    virtual void apply (const TableExprId& id);
    virtual void finish();
  private:
    Int64 itsNr;
  };

  // <summary>
  // Aggregate class determining the variance of array values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the variance of array values in a group.
  // It uses a running algorithm
  // (see en.wikipedia.org/wiki/Algorithms_for_calculating_variance)
  // Note that the result is a Double value (not DComplex).
  // </synopsis>
  class TableExprGroupVarianceArrayDComplex: public TableExprGroupFuncDouble
  {
  public:
    TableExprGroupVarianceArrayDComplex (TableExprNodeRep* node, uInt ddof);
    virtual ~TableExprGroupVarianceArrayDComplex();
    virtual void apply (const TableExprId& id);
    virtual void finish();
  protected:
    uInt     itsDdof;
    Int64    itsNr;
    DComplex itsCurMean;
  };

  // <summary>
  // Aggregate class determining the standard devation of array values
  // in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the standard deviation of array values
  // in a group. It uses a running algorithm
  // (see en.wikipedia.org/wiki/Algorithms_for_calculating_variance)
  // </synopsis>
  class TableExprGroupStdDevArrayDComplex: public TableExprGroupVarianceArrayDComplex
  {
  public:
    TableExprGroupStdDevArrayDComplex (TableExprNodeRep* node, uInt ddof);
    virtual ~TableExprGroupStdDevArrayDComplex();
    virtual void finish();
  };

  // <summary>
  // Aggregate class counting per array index in a group if any is true
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class counting per array index in a group if any is true.
  // </synopsis>
  class TableExprGroupArrayAnys: public TableExprGroupFuncArrayBool
  {
  public:
    TableExprGroupArrayAnys (TableExprNodeRep* node);
    virtual ~TableExprGroupArrayAnys();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class counting per array index in a group if all are true
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class counting per array index in a group if all are true.
  // </synopsis>
  class TableExprGroupArrayAlls: public TableExprGroupFuncArrayBool
  {
  public:
    TableExprGroupArrayAlls (TableExprNodeRep* node);
    virtual ~TableExprGroupArrayAlls();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class counting per array index in a group the nr of true values
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class counting per array index in a group the nr of true values.
  // </synopsis>
  class TableExprGroupArrayNTrues: public TableExprGroupFuncArrayInt
  {
  public:
    TableExprGroupArrayNTrues (TableExprNodeRep* node);
    virtual ~TableExprGroupArrayNTrues();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class counting per array index in a group the nr of false values
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class counting per array index in a group the nr of false values.
  // </synopsis>
  class TableExprGroupArrayNFalses: public TableExprGroupFuncArrayInt
  {
  public:
    TableExprGroupArrayNFalses (TableExprNodeRep* node);
    virtual ~TableExprGroupArrayNFalses();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining per array index in a group the minimum value
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining per array index in a group the minimum value.
  // </synopsis>
  class TableExprGroupMinsArrayInt: public TableExprGroupFuncArrayInt
  {
  public:
    TableExprGroupMinsArrayInt (TableExprNodeRep* node);
    virtual ~TableExprGroupMinsArrayInt();
    virtual void apply (const TableExprId& id);
    virtual void finish();
  };

  // <summary>
  // Aggregate class determining per array index in a group the maximum value
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining per array index in a group the maximum value.
  // </synopsis>
  class TableExprGroupMaxsArrayInt: public TableExprGroupFuncArrayInt
  {
  public:
    TableExprGroupMaxsArrayInt (TableExprNodeRep* node);
    virtual ~TableExprGroupMaxsArrayInt();
    virtual void apply (const TableExprId& id);
    virtual void finish();
  };

  // <summary>
  // Aggregate class determining per array index in a group the sum of values
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining per array index in a group the sum of values.
  // </synopsis>
  class TableExprGroupSumsArrayInt: public TableExprGroupFuncArrayInt
  {
  public:
    TableExprGroupSumsArrayInt (TableExprNodeRep* node);
    virtual ~TableExprGroupSumsArrayInt();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining per array index in a group the product of values
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining per array index in a group the product of values.
  // </synopsis>
  class TableExprGroupProductsArrayInt: public TableExprGroupFuncArrayInt
  {
  public:
    TableExprGroupProductsArrayInt (TableExprNodeRep* node);
    virtual ~TableExprGroupProductsArrayInt();
    virtual void apply (const TableExprId& id);
    virtual void finish();
  };

  // <summary>
  // Aggregate class determining per array index in a group the sum of value squares
  // in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining per array index in a group the sum of value squares.
  // </synopsis>
  class TableExprGroupSumSqrsArrayInt: public TableExprGroupFuncArrayInt
  {
  public:
    TableExprGroupSumSqrsArrayInt (TableExprNodeRep* node);
    virtual ~TableExprGroupSumSqrsArrayInt();
    virtual void apply (const TableExprId& id);
  };


  // <summary>
  // Aggregate class determining the minimum double array value in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the minimum double array value in a group.
  // </synopsis>
  class TableExprGroupMinsArrayDouble: public TableExprGroupFuncArrayDouble
  {
  public:
    TableExprGroupMinsArrayDouble (TableExprNodeRep* node);
    virtual ~TableExprGroupMinsArrayDouble();
    virtual void apply (const TableExprId& id);
    virtual void finish();
  };

  // <summary>
  // Aggregate class determining the maximum double array value in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the maximum double array value in a group.
  // </synopsis>
  class TableExprGroupMaxsArrayDouble: public TableExprGroupFuncArrayDouble
  {
  public:
    TableExprGroupMaxsArrayDouble (TableExprNodeRep* node);
    virtual ~TableExprGroupMaxsArrayDouble();
    virtual void apply (const TableExprId& id);
    virtual void finish();
  };

  // <summary>
  // Aggregate class determining the sum of double array values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the sum of double array values in a group.
  // </synopsis>
  class TableExprGroupSumsArrayDouble: public TableExprGroupFuncArrayDouble
  {
  public:
    TableExprGroupSumsArrayDouble (TableExprNodeRep* node);
    virtual ~TableExprGroupSumsArrayDouble();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining the product of double array values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the product of double array values in a group.
  // </synopsis>
  class TableExprGroupProductsArrayDouble: public TableExprGroupFuncArrayDouble
  {
  public:
    TableExprGroupProductsArrayDouble (TableExprNodeRep* node);
    virtual ~TableExprGroupProductsArrayDouble();
    virtual void apply (const TableExprId& id);
    virtual void finish();
  };

  // <summary>
  // Aggregate class determining the sum of squares of double array values
  // in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the sum of squares of double array values
  // in a group.
  // </synopsis>
  class TableExprGroupSumSqrsArrayDouble: public TableExprGroupFuncArrayDouble
  {
  public:
    TableExprGroupSumSqrsArrayDouble (TableExprNodeRep* node);
    virtual ~TableExprGroupSumSqrsArrayDouble();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining the mean of array values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the mean of array values in a group.
  // </synopsis>
  class TableExprGroupMeansArrayDouble: public TableExprGroupFuncArrayDouble
  {
  public:
    TableExprGroupMeansArrayDouble (TableExprNodeRep* node);
    virtual ~TableExprGroupMeansArrayDouble();
    virtual void apply (const TableExprId& id);
    virtual void finish();
  private:
    Array<Int64> itsNr;
  };

  // <summary>
  // Aggregate class determining the variance of array values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the variance of array values in a group.
  // It uses a running algorithm
  // (see en.wikipedia.org/wiki/Algorithms_for_calculating_variance)
  // </synopsis>
  class TableExprGroupVariancesArrayDouble: public TableExprGroupFuncArrayDouble
  {
  public:
    TableExprGroupVariancesArrayDouble (TableExprNodeRep* node, uInt ddof);
    virtual ~TableExprGroupVariancesArrayDouble();
    virtual void apply (const TableExprId& id);
    virtual void finish();
  protected:
    uInt          itsDdof;
    Array<Int64>  itsNr;
    Array<Double> itsCurMean;
  };

  // <summary>
  // Aggregate class determining the standard devation of array values
  // in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the standard deviation of array values
  // in a group. It uses a running algorithm
  // (see en.wikipedia.org/wiki/Algorithms_for_calculating_variance)
  // </synopsis>
  class TableExprGroupStdDevsArrayDouble: public TableExprGroupVariancesArrayDouble
  {
  public:
    TableExprGroupStdDevsArrayDouble (TableExprNodeRep* node, uInt ddof);
    virtual ~TableExprGroupStdDevsArrayDouble();
    virtual void finish();
  };

  // <summary>
  // Aggregate class determining the RMS of array values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the RMS of array values in a group.
  // </synopsis>
  class TableExprGroupRmssArrayDouble: public TableExprGroupFuncArrayDouble
  {
  public:
    TableExprGroupRmssArrayDouble (TableExprNodeRep* node);
    virtual ~TableExprGroupRmssArrayDouble();
    virtual void apply (const TableExprId& id);
    virtual void finish();
  private:
    Array<Int64> itsNr;
  };


  // <summary>
  // Aggregate class determining the sum of complex array values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the sum of complex array values in a group.
  // </synopsis>
  class TableExprGroupSumsArrayDComplex: public TableExprGroupFuncArrayDComplex
  {
  public:
    TableExprGroupSumsArrayDComplex (TableExprNodeRep* node);
    virtual ~TableExprGroupSumsArrayDComplex();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining the product of complex array values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the product of complex array values in a group.
  // </synopsis>
  class TableExprGroupProductsArrayDComplex: public TableExprGroupFuncArrayDComplex
  {
  public:
    TableExprGroupProductsArrayDComplex (TableExprNodeRep* node);
    virtual ~TableExprGroupProductsArrayDComplex();
    virtual void apply (const TableExprId& id);
    virtual void finish();
  };

  // <summary>
  // Aggregate class determining the sum of squares of complex array values
  // in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the sum of squares of complex array values
  // in a group.
  // </synopsis>
  class TableExprGroupSumSqrsArrayDComplex: public TableExprGroupFuncArrayDComplex
  {
  public:
    TableExprGroupSumSqrsArrayDComplex (TableExprNodeRep* node);
    virtual ~TableExprGroupSumSqrsArrayDComplex();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining the mean of complex array values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the mean of complex array values in a group.
  // </synopsis>
  class TableExprGroupMeansArrayDComplex: public TableExprGroupFuncArrayDComplex
  {
  public:
    TableExprGroupMeansArrayDComplex (TableExprNodeRep* node);
    virtual ~TableExprGroupMeansArrayDComplex();
    virtual void apply (const TableExprId& id);
    virtual void finish();
  private:
    Array<Int64> itsNr;
  };

  // <summary>
  // Aggregate class determining the variance of array values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the variance of array values in a group.
  // It uses a running algorithm
  // (see en.wikipedia.org/wiki/Algorithms_for_calculating_variance)
  // Note that result is a Double value (npot DComplex).
  // </synopsis>
  class TableExprGroupVariancesArrayDComplex: public TableExprGroupFuncArrayDouble
  {
  public:
    TableExprGroupVariancesArrayDComplex (TableExprNodeRep* node, uInt ddof);
    virtual ~TableExprGroupVariancesArrayDComplex();
    virtual void apply (const TableExprId& id);
    virtual void finish();
  protected:
    uInt            itsDdof;
    Array<Int64>    itsNr;
    Array<DComplex> itsCurMean;
  };

  // <summary>
  // Aggregate class determining the standard devation of array values
  // in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the standard deviation of array values
  // in a group. It uses a running algorithm
  // (see en.wikipedia.org/wiki/Algorithms_for_calculating_variance)
  // </synopsis>
  class TableExprGroupStdDevsArrayDComplex: public TableExprGroupVariancesArrayDComplex
  {
  public:
    TableExprGroupStdDevsArrayDComplex (TableExprNodeRep* node, uInt ddof);
    virtual ~TableExprGroupStdDevsArrayDComplex();
    virtual void finish();
  };


  // <summary>
  // Base aggregate class determining the histogram of values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Base aggregate class determining the histogram of values in a group
  // </synopsis>
  class TableExprGroupHistBase: public TableExprGroupFuncBase
  {
  public:
    explicit TableExprGroupHistBase (TableExprNodeRep* node,
                                     Int64 nbin, Double start, Double end);
    virtual ~TableExprGroupHistBase();
    virtual MArray<Int64> getArrayInt (const vector<TableExprId>&);
  protected:
    // Add the value to the histogram.
    void add (Double value);
  private:
    Vector<Int64> itsHist;
    Double itsStart;
    Double itsWidth;
  };

  // <summary>
  // Aggregate class determining the histogram of scalar values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the histogram of scalar values in a group
  // </synopsis>
  class TableExprGroupHistScalar: public TableExprGroupHistBase
  {
  public:
    explicit TableExprGroupHistScalar (TableExprNodeRep* node,
                                       Int64 nbin, Double start, Double end);
    virtual ~TableExprGroupHistScalar();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining the histogram of integer array values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the histogram of integer array values in a group
  // </synopsis>
  class TableExprGroupHistInt: public TableExprGroupHistBase
  {
  public:
    explicit TableExprGroupHistInt (TableExprNodeRep* node,
                                    Int64 nbin, Double start, Double end);
    virtual ~TableExprGroupHistInt();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Aggregate class determining the histogram of double array values in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // Aggregate class determining the histogram of double array values in a group
  // </synopsis>
  class TableExprGroupHistDouble: public TableExprGroupHistBase
  {
  public:
    explicit TableExprGroupHistDouble (TableExprNodeRep* node,
                                       Int64 nbin, Double start, Double end);
    virtual ~TableExprGroupHistDouble();
    virtual void apply (const TableExprId& id);
  };


} //# NAMESPACE CASACORE - END

#endif
