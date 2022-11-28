//# ExprNodeArray.h: Classes representing an array in table select expression
//# Copyright (C) 1997,1999,2000,2001
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
//# $Id: ExprNodeArray.h 21262 2012-09-07 12:38:36Z gervandiepen $

#ifndef TABLES_EXPRNODEARRAY_H
#define TABLES_EXPRNODEARRAY_H

//# Includes
#include <casacore/tables/TaQL/ExprNodeRep.h>
#include <casacore/tables/TaQL/TaQLStyle.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/casa/Arrays/Slicer.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class TableExprNodeSet;


// <summary>
// Base class for arrays in table select expression
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
//   <li> TableExprNodeRep
//   <li> TableExprNodeBinary
// </prerequisite>

// <synopsis> 
// This class is the base class to represent an array.
// The actual storing of the array column is done by its derivations.
// </synopsis> 

class TableExprNodeArray : public TableExprNodeBinary
{
public:
    // Create the object.
    // <group>
    TableExprNodeArray (NodeDataType, OperType);
    TableExprNodeArray (const TableExprNodeRep& node, NodeDataType, OperType);
    TableExprNodeArray (NodeDataType, OperType, const IPosition& shape);
    // </group>

    ~TableExprNodeArray() override = default;

    // Turn a constant array with one element into a scalar.
    // It returns a zero pointer if not possible.
    // The default implementation returns 0.
    virtual TENShPtr makeConstantScalar();

    // Validate the given index against the array's shape.
    // Treat a negative as an index from the end (a la python) and replace it.
    IPosition validateIndex (const IPosition& index,
                             const ArrayBase& arr) const;

    // Get the shape of the array in the given row.
    // This default implementation evaluates the value and returns its shape.
    const IPosition& getShape (const TableExprId& id) override;

    // The default implementation of getArrayDouble does
    // getArrayInt and converts the result.
    MArray<Double> getArrayDouble (const TableExprId& id) override;

    // The default implementation of getArrayDComplex does
    // getArrayDouble and converts the result.
    MArray<DComplex> getArrayDComplex (const TableExprId& id) override;

    // Does a value occur in the set?
    // <group>
    Bool contains (const TableExprId& id, Bool value) override;
    Bool contains (const TableExprId& id, Int64 value) override;
    Bool contains (const TableExprId& id, Double value) override;
    Bool contains (const TableExprId& id, DComplex value) override;
    Bool contains (const TableExprId& id, String value) override;
    Bool contains (const TableExprId& id, MVTime value) override;
    MArray<Bool> contains (const TableExprId& id,
                           const MArray<Bool>& value) override;
    MArray<Bool> contains (const TableExprId& id,
                           const MArray<Int64>& value) override;
    MArray<Bool> contains (const TableExprId& id,
                           const MArray<Double>& value) override;
    MArray<Bool> contains (const TableExprId& id,
                           const MArray<DComplex>& value) override;
    MArray<Bool> contains (const TableExprId& id,
                           const MArray<String>& value) override;
    MArray<Bool> contains (const TableExprId& id,
                           const MArray<MVTime>& value) override;
    // </group>

    // Get a single element from the array in the given row.
    // <group>
    virtual Bool     getElemBool     (const TableExprId& id,
                                      const Slicer& index);
    virtual Int64    getElemInt      (const TableExprId& id,
                                      const Slicer& index);
    virtual Double   getElemDouble   (const TableExprId& id,
                                      const Slicer& index);
    virtual DComplex getElemDComplex (const TableExprId& id,
                                      const Slicer& index);
    virtual String   getElemString   (const TableExprId& id,
                                      const Slicer& index);
    virtual MVTime   getElemDate     (const TableExprId& id,
                                      const Slicer& index);
    // </group>

    // Get a slice of the array in the given row.
    // <group>
    virtual MArray<Bool>     getSliceBool     (const TableExprId& id,
                                                  const Slicer&);
    virtual MArray<Int64>    getSliceInt      (const TableExprId& id,
                                                  const Slicer&);
    virtual MArray<Double>   getSliceDouble   (const TableExprId& id,
                                                  const Slicer&);
    virtual MArray<DComplex> getSliceDComplex (const TableExprId& id,
                                                  const Slicer&);
    virtual MArray<String>   getSliceString   (const TableExprId& id,
                                                  const Slicer&);
    virtual MArray<MVTime>   getSliceDate     (const TableExprId& id,
                                                  const Slicer&);
    // </group>

    // Get a single element for the entire column (used by sort).
    // <group>
    virtual Array<Bool>     getElemColumnBool     (const Vector<rownr_t>& rownrs,
                                                   const Slicer&);
    virtual Array<uChar>    getElemColumnuChar    (const Vector<rownr_t>& rownrs,
                                                   const Slicer&);
    virtual Array<Short>    getElemColumnShort    (const Vector<rownr_t>& rownrs,
                                                   const Slicer&);
    virtual Array<uShort>   getElemColumnuShort   (const Vector<rownr_t>& rownrs,
                                                   const Slicer&);
    virtual Array<Int>      getElemColumnInt      (const Vector<rownr_t>& rownrs,
                                                   const Slicer&);
    virtual Array<uInt>     getElemColumnuInt     (const Vector<rownr_t>& rownrs,
                                                   const Slicer&);
    virtual Array<Int64>    getElemColumnInt64    (const Vector<rownr_t>& rownrs,
                                                   const Slicer&);
    virtual Array<Float>    getElemColumnFloat    (const Vector<rownr_t>& rownrs,
                                                   const Slicer&);
    virtual Array<Double>   getElemColumnDouble   (const Vector<rownr_t>& rownrs,
                                                   const Slicer&);
    virtual Array<Complex>  getElemColumnComplex  (const Vector<rownr_t>& rownrs,
                                                   const Slicer&);
    virtual Array<DComplex> getElemColumnDComplex (const Vector<rownr_t>& rownrs,
                                                   const Slicer&);
    virtual Array<String>   getElemColumnString   (const Vector<rownr_t>& rownrs,
                                                   const Slicer&);
    // </group>

    // Make an array with the given shape and fill it with the value.
    static MArray<Int64>    makeArray (const IPosition& shape, Int64 value);
    static MArray<Double>   makeArray (const IPosition& shape, Double value);
    static MArray<DComplex> makeArray (const IPosition& shape,
                                      const DComplex& value);

protected:
    IPosition varShape_p;
};



// <summary>
// Base class for Array column in table select expression
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNodeArray
// </prerequisite>

// <synopsis> 
// This class is the base class to store an array column.
// The actual storing of the array column is done by its derivations.
// </synopsis> 

class TableExprNodeArrayColumn : public TableExprNodeArray
{
public:
    // Create the object for the given column and table.
    TableExprNodeArrayColumn (const TableColumn& tablecol,
                              const TableExprInfo&);

    ~TableExprNodeArrayColumn() override = default;

    // Get the table info for this column.
    TableExprInfo getTableInfo() const override;

    // Do not apply the selection.
    void disableApplySelection() override;

    // Re-create the column object for a selection of rows.
    void applySelection (const Vector<rownr_t>& rownrs) override;

    // Get the TableColumn object.
    const TableColumn& getColumn() const;

    // Get the shape of the array in the given row.
    const IPosition& getShape (const TableExprId& id) override;

    // Is the value in the given row defined?
    Bool isDefined (const TableExprId& id) override;

    // Get the data type of this column.
    // It returns with a True status.
    Bool getColumnDataType (DataType&) const override;

protected:
    TableExprInfo tableInfo_p;
    TableColumn   tabCol_p;
    Bool          applySelection_p;
};



// <summary>
// Bool array column in table select expression
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNodeArrayColumn
// </prerequisite>

// <synopsis> 
// These classes store an array column of type X.
// </synopsis> 

class TableExprNodeArrayColumnBool : public TableExprNodeArrayColumn
{
public:
    TableExprNodeArrayColumnBool (const TableColumn&,
                                  const TableExprInfo&);
    ~TableExprNodeArrayColumnBool() override = default;

    // Re-create the column object for a selection of rows.
    void applySelection (const Vector<rownr_t>& rownrs) override;

    Bool getElemBool (const TableExprId& id, const Slicer& index) override;
    MArray<Bool> getArrayBool (const TableExprId& id) override;
    MArray<Bool> getSliceBool (const TableExprId& id, const Slicer&) override;
    Array<Bool>  getElemColumnBool (const Vector<rownr_t>& rownrs,
                                    const Slicer&) override;
protected:
    ArrayColumn<Bool> col_p;
};


// <summary>
// uChar array column in table select expression
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNodeArrayColumn
// </prerequisite>

// <synopsis> 
// These classes store an array column of type X.
// </synopsis> 

class TableExprNodeArrayColumnuChar : public TableExprNodeArrayColumn
{
public:
    TableExprNodeArrayColumnuChar (const TableColumn&,
                                   const TableExprInfo&);
    ~TableExprNodeArrayColumnuChar() override = default;

    // Re-create the column object for a selection of rows.
    void applySelection (const Vector<rownr_t>& rownrs) override;

    Int64 getElemInt (const TableExprId& id, const Slicer& index) override;
    MArray<Int64> getArrayInt (const TableExprId& id) override;
    MArray<Int64> getSliceInt (const TableExprId& id,
                               const Slicer&) override;
    Array<uChar>  getElemColumnuChar (const Vector<rownr_t>& rownrs,
                                      const Slicer&) override;
protected:
    ArrayColumn<uChar> col_p;
};


// <summary>
// Short array column in table select expression
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNodeArrayColumn
// </prerequisite>

// <synopsis> 
// These classes store an array column of type X.
// </synopsis> 

class TableExprNodeArrayColumnShort : public TableExprNodeArrayColumn
{
public:
    TableExprNodeArrayColumnShort (const TableColumn&,
                                   const TableExprInfo&);
    ~TableExprNodeArrayColumnShort() override = default;

    // Re-create the column object for a selection of rows.
    void applySelection (const Vector<rownr_t>& rownrs) override;

    Int64 getElemInt (const TableExprId& id, const Slicer& index) override;
    MArray<Int64> getArrayInt (const TableExprId& id) override;
    MArray<Int64> getSliceInt (const TableExprId& id,
                               const Slicer&) override;
    Array<Short>  getElemColumnShort (const Vector<rownr_t>& rownrs,
                                      const Slicer&) override;
protected:
    ArrayColumn<Short> col_p;
};


// <summary>
// uShort array column in table select expression
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNodeArrayColumn
// </prerequisite>

// <synopsis> 
// These classes store an array column of type X.
// </synopsis> 

class TableExprNodeArrayColumnuShort : public TableExprNodeArrayColumn
{
public:
    TableExprNodeArrayColumnuShort (const TableColumn&,
                                    const TableExprInfo&);
    ~TableExprNodeArrayColumnuShort() override = default;

    // Re-create the column object for a selection of rows.
    void applySelection (const Vector<rownr_t>& rownrs) override;

    Int64 getElemInt (const TableExprId& id, const Slicer& index) override;
    MArray<Int64> getArrayInt (const TableExprId& id) override;
    MArray<Int64> getSliceInt (const TableExprId& id,
                               const Slicer&) override;
    Array<uShort> getElemColumnuShort (const Vector<rownr_t>& rownrs,
                                       const Slicer&) override;
protected:
    ArrayColumn<uShort> col_p;
};


// <summary>
// Int array column in table select expression
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNodeArrayColumn
// </prerequisite>

// <synopsis> 
// These classes store an array column of type X.
// </synopsis> 

class TableExprNodeArrayColumnInt : public TableExprNodeArrayColumn
{
public:
    TableExprNodeArrayColumnInt (const TableColumn&,
                                 const TableExprInfo&);
    ~TableExprNodeArrayColumnInt() override = default;

    // Re-create the column object for a selection of rows.
    void applySelection (const Vector<rownr_t>& rownrs) override;

    Int64 getElemInt (const TableExprId& id, const Slicer& index) override;
    MArray<Int64> getArrayInt (const TableExprId& id) override;
    MArray<Int64> getSliceInt (const TableExprId& id,
                               const Slicer&) override;
    Array<Int>    getElemColumnInt (const Vector<rownr_t>& rownrs,
                                    const Slicer&) override;
protected:
    ArrayColumn<Int> col_p;
};


// <summary>
// uInt array column in table select expression
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNodeArrayColumn
// </prerequisite>

// <synopsis> 
// These classes store an array column of type X.
// </synopsis> 

class TableExprNodeArrayColumnuInt : public TableExprNodeArrayColumn
{
public:
    TableExprNodeArrayColumnuInt (const TableColumn&,
                                  const TableExprInfo&);
    ~TableExprNodeArrayColumnuInt() override = default;

    // Re-create the column object for a selection of rows.
    void applySelection (const Vector<rownr_t>& rownrs) override;

    Int64 getElemInt (const TableExprId& id, const Slicer& index) override;
    MArray<Int64> getArrayInt (const TableExprId& id) override;
    MArray<Int64> getSliceInt (const TableExprId& id,
                               const Slicer&) override;
    Array<uInt>   getElemColumnuInt (const Vector<rownr_t>& rownrs,
                                     const Slicer&) override;
protected:
    ArrayColumn<uInt> col_p;
};


// <summary>
// Int64 array column in table select expression
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNodeArrayColumn
// </prerequisite>

// <synopsis> 
// These classes store an array column of type X.
// </synopsis> 

class TableExprNodeArrayColumnInt64 : public TableExprNodeArrayColumn
{
public:
    TableExprNodeArrayColumnInt64 (const TableColumn&,
                                   const TableExprInfo&);
    ~TableExprNodeArrayColumnInt64() override = default;

    // Re-create the column object for a selection of rows.
    void applySelection (const Vector<rownr_t>& rownrs) override;

    Int64 getElemInt (const TableExprId& id, const Slicer& index) override;
    MArray<Int64> getArrayInt (const TableExprId& id) override;
    MArray<Int64> getSliceInt (const TableExprId& id,
                               const Slicer&) override;
    Array<Int64>  getElemColumnInt64 (const Vector<rownr_t>& rownrs,
                                      const Slicer&) override;
protected:
    ArrayColumn<Int64> col_p;
};


// <summary>
// Float array column in table select expression
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNodeArrayColumn
// </prerequisite>

// <synopsis> 
// These classes store an array column of type X.
// </synopsis> 

class TableExprNodeArrayColumnFloat : public TableExprNodeArrayColumn
{
public:
    TableExprNodeArrayColumnFloat (const TableColumn&,
                                   const TableExprInfo&);
    ~TableExprNodeArrayColumnFloat() override = default;

    // Re-create the column object for a selection of rows.
    void applySelection (const Vector<rownr_t>& rownrs) override;

    Double getElemDouble (const TableExprId& id, const Slicer& index) override;
    MArray<Double> getArrayDouble (const TableExprId& id) override;
    MArray<Double> getSliceDouble (const TableExprId& id,
                                   const Slicer&) override;
    Array<Float>  getElemColumnFloat (const Vector<rownr_t>& rownrs,
                                      const Slicer&) override;
protected:
    ArrayColumn<Float> col_p;
};


// <summary>
// Double array column in table select expression
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNodeArrayColumn
// </prerequisite>

// <synopsis> 
// These classes store an array column of type X.
// </synopsis> 

class TableExprNodeArrayColumnDouble : public TableExprNodeArrayColumn
{
public:
    TableExprNodeArrayColumnDouble (const TableColumn&,
                                    const TableExprInfo&);
    ~TableExprNodeArrayColumnDouble() override = default;

    // Re-create the column object for a selection of rows.
    void applySelection (const Vector<rownr_t>& rownrs) override;

    Double getElemDouble (const TableExprId& id, const Slicer& index) override;
    MArray<Double> getArrayDouble (const TableExprId& id) override;
    MArray<Double> getSliceDouble (const TableExprId& id,
                                   const Slicer&) override;
    Array<Double> getElemColumnDouble (const Vector<rownr_t>& rownrs,
                                       const Slicer&) override;
protected:
    ArrayColumn<Double> col_p;
};


// <summary>
// Complex array column in table select expression
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNodeArrayColumn
// </prerequisite>

// <synopsis> 
// These classes store an array column of type X.
// </synopsis> 

class TableExprNodeArrayColumnComplex : public TableExprNodeArrayColumn
{
public:
    TableExprNodeArrayColumnComplex (const TableColumn&,
                                     const TableExprInfo&);
    ~TableExprNodeArrayColumnComplex() override = default;

    // Re-create the column object for a selection of rows.
    void applySelection (const Vector<rownr_t>& rownrs) override;

    DComplex getElemDComplex (const TableExprId& id, const Slicer& index) override;
    MArray<DComplex> getArrayDComplex (const TableExprId& id) override;
    MArray<DComplex> getSliceDComplex (const TableExprId& id,
                                       const Slicer&) override;
    Array<Complex>  getElemColumnComplex (const Vector<rownr_t>& rownrs,
                                          const Slicer&) override;
protected:
    ArrayColumn<Complex> col_p;
};


// <summary>
// DComplex array column in table select expression
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNodeArrayColumn
// </prerequisite>

// <synopsis> 
// These classes store an array column of type X.
// </synopsis> 

class TableExprNodeArrayColumnDComplex : public TableExprNodeArrayColumn
{
public:
    TableExprNodeArrayColumnDComplex (const TableColumn&,
                                      const TableExprInfo&);
    ~TableExprNodeArrayColumnDComplex() override = default;

    // Re-create the column object for a selection of rows.
    void applySelection (const Vector<rownr_t>& rownrs) override;

    DComplex getElemDComplex (const TableExprId& id, const Slicer& index) override;
    MArray<DComplex> getArrayDComplex (const TableExprId& id) override;
    MArray<DComplex> getSliceDComplex (const TableExprId& id,
                                       const Slicer&) override;
    Array<DComplex> getElemColumnDComplex (const Vector<rownr_t>& rownrs,
                                           const Slicer&) override;
protected:
    ArrayColumn<DComplex> col_p;
};


// <summary>
// String array column in table select expression
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNodeArrayColumn
// </prerequisite>

// <synopsis> 
// These classes store an array column of type X.
// </synopsis> 

class TableExprNodeArrayColumnString : public TableExprNodeArrayColumn
{
public:
    TableExprNodeArrayColumnString (const TableColumn&,
                                    const TableExprInfo&);
    ~TableExprNodeArrayColumnString() override = default;

    // Re-create the column object for a selection of rows.
    void applySelection (const Vector<rownr_t>& rownrs) override;

    String getElemString (const TableExprId& id, const Slicer& index) override;
    MArray<String> getArrayString (const TableExprId& id) override;
    MArray<String> getSliceString (const TableExprId& id,
                                   const Slicer&) override;
    Array<String> getElemColumnString (const Vector<rownr_t>& rownrs,
                                       const Slicer&) override;
protected:
    ArrayColumn<String> col_p;
};




// <summary>
// The index of an array element in a table select expression
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNodeMulti
// </prerequisite>

// <etymology>
// TableExprNodeIndex is used to store an index.
// All the operands must be Int.
// </etymology>

// <synopsis> 
// TableExprNodeIndex is a derivation of TableExprNodeMulti
// expression tree that represents an index.
// </synopsis> 

// <motivation>
// All operands of TableExprNodeIndex must be Int,
// therefore it is a derivation of TableExprNodeMulti.
// </motivation>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//   <li> to be filled in
// </todo>

class TableExprNodeIndex : public TableExprNodeMulti
{
public:
    // Constructor
    explicit TableExprNodeIndex (const TableExprNodeSet& indices,
                                 const TaQLStyle& = TaQLStyle(0));

    // Destructor
    ~TableExprNodeIndex() override = default;

    // Link all the operands and check datatype.
    // Calculate the IPosition values for the const operands.
    void fillIndex (const TableExprNodeSet& indices);

    // Check if the index values match the dimensionality and shape
    // of fixed-shaped array.
    void checkIndexValues (const TENShPtr& arrayNode);

    // Get the Slicer value for a constant index.
    const Slicer& getConstantSlicer() const;

    // Get the Slicer value for the slice.
    const Slicer& getSlicer (const TableExprId& id);

    // Does it index a single element?
    Bool isSingle() const;

protected:
    Int         origin_p;        //# origin 0 for C++/Python; 1 for Glish
    Int         endMinus_p;      //# subtract from end (origin and endExcl)
    Bool        isCOrder_p;      //# True for Python
    IPosition   start_p;         //# precalculated start values
    IPosition   end_p;           //# precalculated end values (<0 = till end)
    IPosition   incr_p;          //# precalculated increment values
    Slicer      slicer_p;        //# combined start, end, and incr
    Block<Bool> varIndex_p;      //# is the start for the axes variable?
    Bool        isSingle_p;      //# Index a single value?

    // Precalculate the constant indices and store them.
    void convertConstIndex();

    // Fill the slicer for this row.
    void fillSlicer (const TableExprId& id);

    // Get the shape of the node involved. Reverse axes if needed.
    IPosition getNodeShape (const TENShPtr& arrayNode) const;
};




// <summary>
// Array column part in table select expression
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
//   <li> TableExprNodeRep
//   <li> TableExprNodeBinary
// </prerequisite>

// <synopsis> 
// This class handles a part of an array.
// It uses a TableExprNodeArray to handle the array
// and a TableExprNodeIndex to store the index.
// </synopsis> 

class TableExprNodeArrayPart : public TableExprNodeArray
{
public:
    TableExprNodeArrayPart (const TENShPtr& arrayNode,
                            const TENShPtr& indexNode);
    ~TableExprNodeArrayPart() override = default;

    // Show the node.
    void show (ostream& os, uInt indent) const override;

    Bool     getBool     (const TableExprId& id) override;
    Int64    getInt      (const TableExprId& id) override;
    Double   getDouble   (const TableExprId& id) override;
    DComplex getDComplex (const TableExprId& id) override;
    String   getString   (const TableExprId& id) override;
    MVTime   getDate     (const TableExprId& id) override;

    MArray<Bool>     getArrayBool     (const TableExprId& id) override;
    MArray<Int64>    getArrayInt      (const TableExprId& id) override;
    MArray<Double>   getArrayDouble   (const TableExprId& id) override;
    MArray<DComplex> getArrayDComplex (const TableExprId& id) override;
    MArray<String>   getArrayString   (const TableExprId& id) override;
    MArray<MVTime>   getArrayDate     (const TableExprId& id) override;

    // Get the data type of this column (if possible).
    // It returns with a False status when the index is not constant
    // (that means that the index can vary with row number).
    Bool getColumnDataType (DataType&) const override;

    Array<Bool>     getColumnBool (const Vector<rownr_t>& rownrs) override;
    Array<uChar>    getColumnuChar (const Vector<rownr_t>& rownrs) override;
    Array<Short>    getColumnShort (const Vector<rownr_t>& rownrs) override;
    Array<uShort>   getColumnuShort (const Vector<rownr_t>& rownrs) override;
    Array<Int>      getColumnInt (const Vector<rownr_t>& rownrs) override;
    Array<uInt>     getColumnuInt (const Vector<rownr_t>& rownrs) override;
    Array<Int64>    getColumnInt64 (const Vector<rownr_t>& rownrs) override;
    Array<Float>    getColumnFloat (const Vector<rownr_t>& rownrs) override;
    Array<Double>   getColumnDouble (const Vector<rownr_t>& rownrs) override;
    Array<Complex>  getColumnComplex (const Vector<rownr_t>& rownrs) override;
    Array<DComplex> getColumnDComplex (const Vector<rownr_t>& rownrs) override;
    Array<String>   getColumnString (const Vector<rownr_t>& rownrs) override;

    // Get the index node.
    const TableExprNodeIndex* getIndexNode() const;

    // Get the array column node.
    // It returns 0 if the parent object is no array column.
    const TableExprNodeArrayColumn* getColumnNode() const;

private:
    TableExprNodeIndex*       inxNode_p;
    TableExprNodeArray*       arrNode_p;
    TableExprNodeArrayColumn* colNode_p;   //# 0 if arrNode is no arraycolumn
};




inline Bool TableExprNodeIndex::isSingle() const
{
    return isSingle_p;
}
inline const Slicer& TableExprNodeIndex::getConstantSlicer() const
{
    return slicer_p;
}
inline const Slicer& TableExprNodeIndex::getSlicer (const TableExprId& id)
{
    if (!isConstant()) {
        fillSlicer (id);
    }
    return slicer_p;
}

inline const TableColumn& TableExprNodeArrayColumn::getColumn() const
{
    return tabCol_p;
}

inline const TableExprNodeIndex* TableExprNodeArrayPart::getIndexNode() const
{
    return inxNode_p;
}

inline const TableExprNodeArrayColumn*
TableExprNodeArrayPart::getColumnNode() const
{
    return colNode_p;
}



} //# NAMESPACE CASACORE - END

#endif

