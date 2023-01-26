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
    MArray<double> getArrayDouble (const TableExprId& id) override;

    // The default implementation of getArrayDComplex does
    // getArrayDouble and converts the result.
    MArray<DComplex> getArrayDComplex (const TableExprId& id) override;

    // Does a value occur in the set?
    // <group>
    bool contains (const TableExprId& id, bool value) override;
    bool contains (const TableExprId& id, int64_t value) override;
    bool contains (const TableExprId& id, double value) override;
    bool contains (const TableExprId& id, DComplex value) override;
    bool contains (const TableExprId& id, String value) override;
    bool contains (const TableExprId& id, MVTime value) override;
    MArray<bool> contains (const TableExprId& id,
                           const MArray<bool>& value) override;
    MArray<bool> contains (const TableExprId& id,
                           const MArray<int64_t>& value) override;
    MArray<bool> contains (const TableExprId& id,
                           const MArray<double>& value) override;
    MArray<bool> contains (const TableExprId& id,
                           const MArray<DComplex>& value) override;
    MArray<bool> contains (const TableExprId& id,
                           const MArray<String>& value) override;
    MArray<bool> contains (const TableExprId& id,
                           const MArray<MVTime>& value) override;
    // </group>

    // Get a single element from the array in the given row.
    // <group>
    virtual bool     getElemBool     (const TableExprId& id,
                                      const Slicer& index);
    virtual int64_t    getElemInt      (const TableExprId& id,
                                      const Slicer& index);
    virtual double   getElemDouble   (const TableExprId& id,
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
    virtual MArray<bool>     getSliceBool     (const TableExprId& id,
                                                  const Slicer&);
    virtual MArray<int64_t>    getSliceInt      (const TableExprId& id,
                                                  const Slicer&);
    virtual MArray<double>   getSliceDouble   (const TableExprId& id,
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
    virtual Array<bool>     getElemColumnBool     (const Vector<rownr_t>& rownrs,
                                                   const Slicer&);
    virtual Array<unsigned char>    getElemColumnuChar    (const Vector<rownr_t>& rownrs,
                                                   const Slicer&);
    virtual Array<int16_t>    getElemColumnShort    (const Vector<rownr_t>& rownrs,
                                                   const Slicer&);
    virtual Array<uint16_t>   getElemColumnuShort   (const Vector<rownr_t>& rownrs,
                                                   const Slicer&);
    virtual Array<int32_t>      getElemColumnInt      (const Vector<rownr_t>& rownrs,
                                                   const Slicer&);
    virtual Array<uint32_t>     getElemColumnuInt     (const Vector<rownr_t>& rownrs,
                                                   const Slicer&);
    virtual Array<int64_t>    getElemColumnInt64    (const Vector<rownr_t>& rownrs,
                                                   const Slicer&);
    virtual Array<float>    getElemColumnFloat    (const Vector<rownr_t>& rownrs,
                                                   const Slicer&);
    virtual Array<double>   getElemColumnDouble   (const Vector<rownr_t>& rownrs,
                                                   const Slicer&);
    virtual Array<Complex>  getElemColumnComplex  (const Vector<rownr_t>& rownrs,
                                                   const Slicer&);
    virtual Array<DComplex> getElemColumnDComplex (const Vector<rownr_t>& rownrs,
                                                   const Slicer&);
    virtual Array<String>   getElemColumnString   (const Vector<rownr_t>& rownrs,
                                                   const Slicer&);
    // </group>

    // Make an array with the given shape and fill it with the value.
    static MArray<int64_t>    makeArray (const IPosition& shape, int64_t value);
    static MArray<double>   makeArray (const IPosition& shape, double value);
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
    bool isDefined (const TableExprId& id) override;

    // Get the data type of this column.
    // It returns with a true status.
    bool getColumnDataType (DataType&) const override;

protected:
    TableExprInfo tableInfo_p;
    TableColumn   tabCol_p;
    bool          applySelection_p;
};



// <summary>
// bool array column in table select expression
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

    bool getElemBool (const TableExprId& id, const Slicer& index) override;
    MArray<bool> getArrayBool (const TableExprId& id) override;
    MArray<bool> getSliceBool (const TableExprId& id, const Slicer&) override;
    Array<bool>  getElemColumnBool (const Vector<rownr_t>& rownrs,
                                    const Slicer&) override;
protected:
    ArrayColumn<bool> col_p;
};


// <summary>
// unsigned char array column in table select expression
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

    int64_t getElemInt (const TableExprId& id, const Slicer& index) override;
    MArray<int64_t> getArrayInt (const TableExprId& id) override;
    MArray<int64_t> getSliceInt (const TableExprId& id,
                               const Slicer&) override;
    Array<unsigned char>  getElemColumnuChar (const Vector<rownr_t>& rownrs,
                                      const Slicer&) override;
protected:
    ArrayColumn<unsigned char> col_p;
};


// <summary>
// int16_t array column in table select expression
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

    int64_t getElemInt (const TableExprId& id, const Slicer& index) override;
    MArray<int64_t> getArrayInt (const TableExprId& id) override;
    MArray<int64_t> getSliceInt (const TableExprId& id,
                               const Slicer&) override;
    Array<int16_t>  getElemColumnShort (const Vector<rownr_t>& rownrs,
                                      const Slicer&) override;
protected:
    ArrayColumn<int16_t> col_p;
};


// <summary>
// uint16_t array column in table select expression
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

    int64_t getElemInt (const TableExprId& id, const Slicer& index) override;
    MArray<int64_t> getArrayInt (const TableExprId& id) override;
    MArray<int64_t> getSliceInt (const TableExprId& id,
                               const Slicer&) override;
    Array<uint16_t> getElemColumnuShort (const Vector<rownr_t>& rownrs,
                                       const Slicer&) override;
protected:
    ArrayColumn<uint16_t> col_p;
};


// <summary>
// int32_t array column in table select expression
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

    int64_t getElemInt (const TableExprId& id, const Slicer& index) override;
    MArray<int64_t> getArrayInt (const TableExprId& id) override;
    MArray<int64_t> getSliceInt (const TableExprId& id,
                               const Slicer&) override;
    Array<int32_t>    getElemColumnInt (const Vector<rownr_t>& rownrs,
                                    const Slicer&) override;
protected:
    ArrayColumn<int32_t> col_p;
};


// <summary>
// uint32_t array column in table select expression
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

    int64_t getElemInt (const TableExprId& id, const Slicer& index) override;
    MArray<int64_t> getArrayInt (const TableExprId& id) override;
    MArray<int64_t> getSliceInt (const TableExprId& id,
                               const Slicer&) override;
    Array<uint32_t>   getElemColumnuInt (const Vector<rownr_t>& rownrs,
                                     const Slicer&) override;
protected:
    ArrayColumn<uint32_t> col_p;
};


// <summary>
// int64_t array column in table select expression
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

    int64_t getElemInt (const TableExprId& id, const Slicer& index) override;
    MArray<int64_t> getArrayInt (const TableExprId& id) override;
    MArray<int64_t> getSliceInt (const TableExprId& id,
                               const Slicer&) override;
    Array<int64_t>  getElemColumnInt64 (const Vector<rownr_t>& rownrs,
                                      const Slicer&) override;
protected:
    ArrayColumn<int64_t> col_p;
};


// <summary>
// float array column in table select expression
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

    double getElemDouble (const TableExprId& id, const Slicer& index) override;
    MArray<double> getArrayDouble (const TableExprId& id) override;
    MArray<double> getSliceDouble (const TableExprId& id,
                                   const Slicer&) override;
    Array<float>  getElemColumnFloat (const Vector<rownr_t>& rownrs,
                                      const Slicer&) override;
protected:
    ArrayColumn<float> col_p;
};


// <summary>
// double array column in table select expression
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

    double getElemDouble (const TableExprId& id, const Slicer& index) override;
    MArray<double> getArrayDouble (const TableExprId& id) override;
    MArray<double> getSliceDouble (const TableExprId& id,
                                   const Slicer&) override;
    Array<double> getElemColumnDouble (const Vector<rownr_t>& rownrs,
                                       const Slicer&) override;
protected:
    ArrayColumn<double> col_p;
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
// All the operands must be int32_t.
// </etymology>

// <synopsis> 
// TableExprNodeIndex is a derivation of TableExprNodeMulti
// expression tree that represents an index.
// </synopsis> 

// <motivation>
// All operands of TableExprNodeIndex must be int32_t,
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
    bool isSingle() const;

protected:
    int32_t         origin_p;        //# origin 0 for C++/Python; 1 for Glish
    int32_t         endMinus_p;      //# subtract from end (origin and endExcl)
    bool        isCOrder_p;      //# true for Python
    IPosition   start_p;         //# precalculated start values
    IPosition   end_p;           //# precalculated end values (<0 = till end)
    IPosition   incr_p;          //# precalculated increment values
    Slicer      slicer_p;        //# combined start, end, and incr
    Block<bool> varIndex_p;      //# is the start for the axes variable?
    bool        isSingle_p;      //# Index a single value?

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
    void show (ostream& os, uint32_t indent) const override;

    bool     getBool     (const TableExprId& id) override;
    int64_t    getInt      (const TableExprId& id) override;
    double   getDouble   (const TableExprId& id) override;
    DComplex getDComplex (const TableExprId& id) override;
    String   getString   (const TableExprId& id) override;
    MVTime   getDate     (const TableExprId& id) override;

    MArray<bool>     getArrayBool     (const TableExprId& id) override;
    MArray<int64_t>    getArrayInt      (const TableExprId& id) override;
    MArray<double>   getArrayDouble   (const TableExprId& id) override;
    MArray<DComplex> getArrayDComplex (const TableExprId& id) override;
    MArray<String>   getArrayString   (const TableExprId& id) override;
    MArray<MVTime>   getArrayDate     (const TableExprId& id) override;

    // Get the data type of this column (if possible).
    // It returns with a false status when the index is not constant
    // (that means that the index can vary with row number).
    bool getColumnDataType (DataType&) const override;

    Array<bool>     getColumnBool (const Vector<rownr_t>& rownrs) override;
    Array<unsigned char>    getColumnuChar (const Vector<rownr_t>& rownrs) override;
    Array<int16_t>    getColumnShort (const Vector<rownr_t>& rownrs) override;
    Array<uint16_t>   getColumnuShort (const Vector<rownr_t>& rownrs) override;
    Array<int32_t>      getColumnInt (const Vector<rownr_t>& rownrs) override;
    Array<uint32_t>     getColumnuInt (const Vector<rownr_t>& rownrs) override;
    Array<int64_t>    getColumnInt64 (const Vector<rownr_t>& rownrs) override;
    Array<float>    getColumnFloat (const Vector<rownr_t>& rownrs) override;
    Array<double>   getColumnDouble (const Vector<rownr_t>& rownrs) override;
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




inline bool TableExprNodeIndex::isSingle() const
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

