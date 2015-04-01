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
//# $Id$

#ifndef TABLES_EXPRNODEARRAY_H
#define TABLES_EXPRNODEARRAY_H

//# Includes
#include <casacore/casa/aips.h>
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

    ~TableExprNodeArray();

    // Turn a constant array with one element into a scalar.
    // It returns a zero pointer if not possible.
    // The default implementation returns 0.
    virtual TableExprNodeRep* makeConstantScalar();

    // Get the shape of the array in the given row.
    // This default implementation evaluates the value and returns its shape.
    virtual const IPosition& getShape (const TableExprId& id);

    // The default implementation of getArrayDouble does
    // getArrayInt and converts the result.
    virtual Array<Double> getArrayDouble (const TableExprId& id);

    // The default implementation of getArrayDComplex does
    // getArrayDouble and converts the result.
    virtual Array<DComplex> getArrayDComplex (const TableExprId& id);

    // Does a value occur in the set?
    // <group>
    virtual Bool hasBool     (const TableExprId& id, Bool value);
    virtual Bool hasInt      (const TableExprId& id, Int64 value);
    virtual Bool hasDouble   (const TableExprId& id, Double value);
    virtual Bool hasDComplex (const TableExprId& id, const DComplex& value);
    virtual Bool hasString   (const TableExprId& id, const String& value);
    virtual Bool hasDate     (const TableExprId& id, const MVTime& value);
    virtual Array<Bool> hasArrayBool     (const TableExprId& id,
					  const Array<Bool>& value);
    virtual Array<Bool> hasArrayInt      (const TableExprId& id,
					  const Array<Int64>& value);
    virtual Array<Bool> hasArrayDouble   (const TableExprId& id,
					  const Array<Double>& value);
    virtual Array<Bool> hasArrayDComplex (const TableExprId& id,
					  const Array<DComplex>& value);
    virtual Array<Bool> hasArrayString   (const TableExprId& id,
					  const Array<String>& value);
    virtual Array<Bool> hasArrayDate     (const TableExprId& id,
					  const Array<MVTime>& value);
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
    virtual Array<Bool>     getSliceBool     (const TableExprId& id,
					      const Slicer&);
    virtual Array<Int64>    getSliceInt      (const TableExprId& id,
					      const Slicer&);
    virtual Array<Double>   getSliceDouble   (const TableExprId& id,
					      const Slicer&);
    virtual Array<DComplex> getSliceDComplex (const TableExprId& id,
					      const Slicer&);
    virtual Array<String>   getSliceString   (const TableExprId& id,
					      const Slicer&);
    virtual Array<MVTime>   getSliceDate     (const TableExprId& id,
					      const Slicer&);
    // </group>

    // Get a single element for the entire column (used by sort).
    // <group>
    virtual Array<Bool>     getElemColumnBool     (const Vector<uInt>& rownrs,
                                                   const Slicer&);
    virtual Array<uChar>    getElemColumnuChar    (const Vector<uInt>& rownrs,
                                                   const Slicer&);
    virtual Array<Short>    getElemColumnShort    (const Vector<uInt>& rownrs,
                                                   const Slicer&);
    virtual Array<uShort>   getElemColumnuShort   (const Vector<uInt>& rownrs,
                                                   const Slicer&);
    virtual Array<Int>      getElemColumnInt      (const Vector<uInt>& rownrs,
                                                   const Slicer&);
    virtual Array<uInt>     getElemColumnuInt     (const Vector<uInt>& rownrs,
                                                   const Slicer&);
    virtual Array<Float>    getElemColumnFloat    (const Vector<uInt>& rownrs,
                                                   const Slicer&);
    virtual Array<Double>   getElemColumnDouble   (const Vector<uInt>& rownrs,
                                                   const Slicer&);
    virtual Array<Complex>  getElemColumnComplex  (const Vector<uInt>& rownrs,
                                                   const Slicer&);
    virtual Array<DComplex> getElemColumnDComplex (const Vector<uInt>& rownrs,
                                                   const Slicer&);
    virtual Array<String>   getElemColumnString   (const Vector<uInt>& rownrs,
                                                   const Slicer&);
    // </group>

    // Make an array with the given shape and fill it with the value.
    static Array<Int64>    makeArray (const IPosition& shape, Int64 value);
    static Array<Double>   makeArray (const IPosition& shape, Double value);
    static Array<DComplex> makeArray (const IPosition& shape,
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
			      const Table& table);

    ~TableExprNodeArrayColumn();

    // This node represents a table column.
    virtual void getColumnNodes (vector<TableExprNodeRep*>& cols);
  
    // Do not apply the selection.
    virtual void disableApplySelection();

    // Re-create the column object for a selection of rows.
    virtual void applySelection (const Vector<uInt>& rownrs);

    // Get the TableColumn object.
    const TableColumn& getColumn() const;

    // Get the shape of the array in the given row.
    virtual const IPosition& getShape (const TableExprId& id);

    // Is the value in the given row defined?
    virtual Bool isDefined (const TableExprId& id);

    // Get the data type of this column.
    // It returns with a True status.
    virtual Bool getColumnDataType (DataType&) const;

protected:
    Table       selTable_p;
    TableColumn tabCol_p;
    Bool        applySelection_p;
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
				  const Table&);
    ~TableExprNodeArrayColumnBool();

    // Re-create the column object for a selection of rows.
    virtual void applySelection (const Vector<uInt>& rownrs);

    virtual Bool getElemBool (const TableExprId& id, const Slicer& index);
    virtual Array<Bool>  getArrayBool (const TableExprId& id);
    virtual Array<Bool>  getSliceBool (const TableExprId& id, const Slicer&);
    virtual Array<Bool>  getElemColumnBool (const Vector<uInt>& rownrs,
                                            const Slicer&);
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
				   const Table&);
    ~TableExprNodeArrayColumnuChar();

    // Re-create the column object for a selection of rows.
    virtual void applySelection (const Vector<uInt>& rownrs);

    virtual Int64 getElemInt (const TableExprId& id, const Slicer& index);
    virtual Array<Int64> getArrayInt (const TableExprId& id);
    virtual Array<Int64> getSliceInt (const TableExprId& id,
					  const Slicer&);
    virtual Array<uChar>  getElemColumnuChar (const Vector<uInt>& rownrs,
                                              const Slicer&);
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
				   const Table&);
    ~TableExprNodeArrayColumnShort();

    // Re-create the column object for a selection of rows.
    virtual void applySelection (const Vector<uInt>& rownrs);

    virtual Int64 getElemInt (const TableExprId& id, const Slicer& index);
    virtual Array<Int64> getArrayInt (const TableExprId& id);
    virtual Array<Int64> getSliceInt (const TableExprId& id,
					  const Slicer&);
    virtual Array<Short>  getElemColumnShort (const Vector<uInt>& rownrs,
                                              const Slicer&);
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
				    const Table&);
    ~TableExprNodeArrayColumnuShort();

    // Re-create the column object for a selection of rows.
    virtual void applySelection (const Vector<uInt>& rownrs);

    virtual Int64 getElemInt (const TableExprId& id, const Slicer& index);
    virtual Array<Int64> getArrayInt (const TableExprId& id);
    virtual Array<Int64> getSliceInt (const TableExprId& id,
					  const Slicer&);
    virtual Array<uShort> getElemColumnuShort (const Vector<uInt>& rownrs,
                                               const Slicer&);
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
				 const Table&);
    ~TableExprNodeArrayColumnInt();

    // Re-create the column object for a selection of rows.
    virtual void applySelection (const Vector<uInt>& rownrs);

    virtual Int64 getElemInt (const TableExprId& id, const Slicer& index);
    virtual Array<Int64> getArrayInt (const TableExprId& id);
    virtual Array<Int64> getSliceInt (const TableExprId& id,
					  const Slicer&);
    virtual Array<Int>    getElemColumnInt (const Vector<uInt>& rownrs,
                                            const Slicer&);
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
				  const Table&);
    ~TableExprNodeArrayColumnuInt();

    // Re-create the column object for a selection of rows.
    virtual void applySelection (const Vector<uInt>& rownrs);

    virtual Int64 getElemInt (const TableExprId& id, const Slicer& index);
    virtual Array<Int64> getArrayInt (const TableExprId& id);
    virtual Array<Int64> getSliceInt (const TableExprId& id,
					  const Slicer&);
    virtual Array<uInt>   getElemColumnuInt (const Vector<uInt>& rownrs,
                                             const Slicer&);
protected:
    ArrayColumn<uInt> col_p;
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
				   const Table&);
    ~TableExprNodeArrayColumnFloat();

    // Re-create the column object for a selection of rows.
    virtual void applySelection (const Vector<uInt>& rownrs);

    virtual Double getElemDouble (const TableExprId& id, const Slicer& index);
    virtual Array<Double> getArrayDouble (const TableExprId& id);
    virtual Array<Double> getSliceDouble (const TableExprId& id,
					  const Slicer&);
    virtual Array<Float>  getElemColumnFloat (const Vector<uInt>& rownrs,
                                              const Slicer&);
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
				    const Table&);
    ~TableExprNodeArrayColumnDouble();

    // Re-create the column object for a selection of rows.
    virtual void applySelection (const Vector<uInt>& rownrs);

    virtual Double getElemDouble (const TableExprId& id, const Slicer& index);
    virtual Array<Double> getArrayDouble (const TableExprId& id);
    virtual Array<Double> getSliceDouble (const TableExprId& id,
					  const Slicer&);
    virtual Array<Double> getElemColumnDouble (const Vector<uInt>& rownrs,
                                               const Slicer&);
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
				     const Table&);
    ~TableExprNodeArrayColumnComplex();

    // Re-create the column object for a selection of rows.
    virtual void applySelection (const Vector<uInt>& rownrs);

    virtual DComplex getElemDComplex (const TableExprId& id, const Slicer& index);
    virtual Array<DComplex> getArrayDComplex (const TableExprId& id);
    virtual Array<DComplex> getSliceDComplex (const TableExprId& id,
					      const Slicer&);
    virtual Array<Complex>  getElemColumnComplex (const Vector<uInt>& rownrs,
                                                  const Slicer&);
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
				      const Table&);
    ~TableExprNodeArrayColumnDComplex();

    // Re-create the column object for a selection of rows.
    virtual void applySelection (const Vector<uInt>& rownrs);

    virtual DComplex getElemDComplex (const TableExprId& id, const Slicer& index);
    virtual Array<DComplex> getArrayDComplex (const TableExprId& id);
    virtual Array<DComplex> getSliceDComplex (const TableExprId& id,
					      const Slicer&);
    virtual Array<DComplex> getElemColumnDComplex (const Vector<uInt>& rownrs,
                                                   const Slicer&);
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
				    const Table&);
    ~TableExprNodeArrayColumnString();

    // Re-create the column object for a selection of rows.
    virtual void applySelection (const Vector<uInt>& rownrs);

    virtual String getElemString (const TableExprId& id, const Slicer& index);
    virtual Array<String> getArrayString (const TableExprId& id);
    virtual Array<String> getSliceString (const TableExprId& id,
					  const Slicer&);
    virtual Array<String> getElemColumnString (const Vector<uInt>& rownrs,
                                               const Slicer&);
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
    virtual ~TableExprNodeIndex();

    // Link all the operands and check datatype.
    // Calculate the IPosition values for the const operands.
    void fillIndex (const TableExprNodeSet& indices);

    // Check if the index values match the dimensionality and shape
    // of fixed-shaped array.
    void checkIndexValues (const TableExprNodeRep* arrayNode);

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
    IPosition getNodeShape (const TableExprNodeRep* arrayNode) const;
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
    TableExprNodeArrayPart (TableExprNodeRep* arrayNode, TableExprNodeIndex*);
    ~TableExprNodeArrayPart();

    // Show the node.
    void show (ostream& os, uInt indent) const;

    Bool     getBool     (const TableExprId& id);
    Int64    getInt      (const TableExprId& id);
    Double   getDouble   (const TableExprId& id);
    DComplex getDComplex (const TableExprId& id);
    String   getString   (const TableExprId& id);
    MVTime   getDate     (const TableExprId& id);

    Array<Bool>     getArrayBool     (const TableExprId& id);
    Array<Int64>    getArrayInt      (const TableExprId& id);
    Array<Double>   getArrayDouble   (const TableExprId& id);
    Array<DComplex> getArrayDComplex (const TableExprId& id);
    Array<String>   getArrayString   (const TableExprId& id);
    Array<MVTime>   getArrayDate     (const TableExprId& id);

    // Get the data type of this column (if possible).
    // It returns with a False status when the index is not constant
    // (that means that the index can vary with row number).
    Bool getColumnDataType (DataType&) const;

    Array<Bool>     getColumnBool (const Vector<uInt>& rownrs);
    Array<uChar>    getColumnuChar (const Vector<uInt>& rownrs);
    Array<Short>    getColumnShort (const Vector<uInt>& rownrs);
    Array<uShort>   getColumnuShort (const Vector<uInt>& rownrs);
    Array<Int>      getColumnInt (const Vector<uInt>& rownrs);
    Array<uInt>     getColumnuInt (const Vector<uInt>& rownrs);
    Array<Float>    getColumnFloat (const Vector<uInt>& rownrs);
    Array<Double>   getColumnDouble (const Vector<uInt>& rownrs);
    Array<Complex>  getColumnComplex (const Vector<uInt>& rownrs);
    Array<DComplex> getColumnDComplex (const Vector<uInt>& rownrs);
    Array<String>   getColumnString (const Vector<uInt>& rownrs);

    // Get the index node.
    const TableExprNodeIndex* getIndexNode() const;

    // Get the array column node.
    // It returns 0 if the parent object is no array column.
    const TableExprNodeArrayColumn* getColumnNode() const;

private:
    TableExprNodeIndex*       indexNode_p;
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
    return indexNode_p;
}

inline const TableExprNodeArrayColumn*
TableExprNodeArrayPart::getColumnNode() const
{ 
    return colNode_p;
}



} //# NAMESPACE CASACORE - END

#endif

