//# ExprNodeArray.h: Classes representing an array in table select expression
//# Copyright (C) 1997
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

#if !defined(AIPS_EXPRNODEARRAY_H)
#define AIPS_EXPRNODEARRAY_H

#if defined(_AIX)
#pragma implementation ("ExprNodeArray.cc")
#endif

#include <aips/Tables/ExprNodeRep.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/ArrayColumn.h>


// <summary>
// Base class for Array column in table select expression
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
//   <li> TableExprNodeRep
//   <li> TableExprNodeBinary
// </prerequisite>

// <synopsis> 
// This class is the base class to store an array column.
// The actual storing of the array column is done by it's derivations.
// </synopsis> 

class TableExprNodeArray : public TableExprNodeBinary
{
public:
    // Create the object for the given column and table.
    TableExprNodeArray (const ROTableColumn& tablecol,
			const BaseTable* tabptr);

    ~TableExprNodeArray();

    // Get the ROTableColumn object.
    const ROTableColumn& getColumn() const;

    // Get the dimensionality and shape of arrays in a column.
    // <group>
    virtual uInt ndim() const;
    virtual IPosition shape() const;
    // </group>

    // Get the data type of this column.
    // It returns with a True status.
    virtual Bool getColumnDataType (DataType&) const;

    virtual Bool     getElemBool     (uInt rownr, const IPosition& index);
    virtual Double   getElemDouble   (uInt rownr, const IPosition& index);
    virtual DComplex getElemDComplex (uInt rownr, const IPosition& index);
    virtual String   getElemString   (uInt rownr, const IPosition& index);

    virtual Array<Bool>     getElemColumnBool     (const IPosition&);
    virtual Array<uChar>    getElemColumnuChar    (const IPosition&);
    virtual Array<Short>    getElemColumnShort    (const IPosition&);
    virtual Array<uShort>   getElemColumnuShort   (const IPosition&);
    virtual Array<Int>      getElemColumnInt      (const IPosition&);
    virtual Array<uInt>     getElemColumnuInt     (const IPosition&);
    virtual Array<Float>    getElemColumnFloat    (const IPosition&);
    virtual Array<Double>   getElemColumnDouble   (const IPosition&);
    virtual Array<Complex>  getElemColumnComplex  (const IPosition&);
    virtual Array<DComplex> getElemColumnDComplex (const IPosition&);
    virtual Array<String>   getElemColumnString   (const IPosition&);

private:
    ROTableColumn tabCol_p;
};



// <summary>
// Typed array column in table select expression
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNodeArray
// </prerequisite>

// <synopsis> 
// These classes store an array column of type X.
// </synopsis> 

class TableExprNodeArrayBool : public TableExprNodeArray
{
public:
    TableExprNodeArrayBool (const ROTableColumn&,
			    const BaseTable*);
    ~TableExprNodeArrayBool();
    Bool getElemBool (uInt rownr, const IPosition& index);
    Array<Bool>     getElemColumnBool (const IPosition&);
protected:
    ROArrayColumn<Bool> col_p;
};

class TableExprNodeArrayuChar : public TableExprNodeArray
{
public:
    TableExprNodeArrayuChar (const ROTableColumn&,
			     const BaseTable*);
    ~TableExprNodeArrayuChar();
    double getElemDouble (uInt rownr, const IPosition& index);
    Array<uChar>    getElemColumnuChar (const IPosition&);
protected:
    ROArrayColumn<uChar> col_p;
};

class TableExprNodeArrayShort : public TableExprNodeArray
{
public:
    TableExprNodeArrayShort (const ROTableColumn&,
			     const BaseTable*);
    ~TableExprNodeArrayShort();
    double getElemDouble (uInt rownr, const IPosition& index);
    Array<Short>    getElemColumnShort (const IPosition&);
protected:
    ROArrayColumn<Short> col_p;
};

class TableExprNodeArrayuShort : public TableExprNodeArray
{
public:
    TableExprNodeArrayuShort (const ROTableColumn&,
			      const BaseTable*);
    ~TableExprNodeArrayuShort();
    double getElemDouble (uInt rownr, const IPosition& index);
    Array<uShort>   getElemColumnuShort (const IPosition&);
protected:
    ROArrayColumn<uShort> col_p;
};

class TableExprNodeArrayInt : public TableExprNodeArray
{
public:
    TableExprNodeArrayInt (const ROTableColumn&,
			   const BaseTable*);
    ~TableExprNodeArrayInt();
    double getElemDouble (uInt rownr, const IPosition& index);
    Array<Int>      getElemColumnInt (const IPosition&);
protected:
    ROArrayColumn<Int> col_p;
};

class TableExprNodeArrayuInt : public TableExprNodeArray
{
public:
    TableExprNodeArrayuInt (const ROTableColumn&,
			    const BaseTable*);
    ~TableExprNodeArrayuInt();
    double getElemDouble (uInt rownr, const IPosition& index);
    Array<uInt>     getElemColumnuInt (const IPosition&);
protected:
    ROArrayColumn<uInt> col_p;
};

class TableExprNodeArrayFloat : public TableExprNodeArray
{
public:
    TableExprNodeArrayFloat (const ROTableColumn&,
			     const BaseTable*);
    ~TableExprNodeArrayFloat();
    double getElemDouble (uInt rownr, const IPosition& index);
    Array<Float>    getElemColumnFloat (const IPosition&);
protected:
    ROArrayColumn<Float> col_p;
};

class TableExprNodeArrayDouble : public TableExprNodeArray
{
public:
    TableExprNodeArrayDouble (const ROTableColumn&,
			      const BaseTable*);
    ~TableExprNodeArrayDouble();
    double getElemDouble (uInt rownr, const IPosition& index);
    Array<Double>   getElemColumnDouble (const IPosition&);
protected:
    ROArrayColumn<Double> col_p;
};

class TableExprNodeArrayComplex : public TableExprNodeArray
{
public:
    TableExprNodeArrayComplex (const ROTableColumn&,
			       const BaseTable*);
    ~TableExprNodeArrayComplex();
    DComplex getElemDComplex (uInt rownr, const IPosition& index);
    Array<Complex>  getElemColumnComplex (const IPosition&);
protected:
    ROArrayColumn<Complex> col_p;
};

class TableExprNodeArrayDComplex : public TableExprNodeArray
{
public:
    TableExprNodeArrayDComplex (const ROTableColumn&,
				const BaseTable*);
    ~TableExprNodeArrayDComplex();
    DComplex getElemDComplex (uInt rownr, const IPosition& index);
    Array<DComplex> getElemColumnDComplex (const IPosition&);
protected:
    ROArrayColumn<DComplex> col_p;
};

class TableExprNodeArrayString : public TableExprNodeArray
{
public:
    TableExprNodeArrayString (const ROTableColumn&,
			      const BaseTable*);
    ~TableExprNodeArrayString();
    String getElemString (uInt rownr, const IPosition& index);
    Array<String>   getElemColumnString (const IPosition&);
protected:
    ROArrayColumn<String> col_p;
};




// <summary>
// The index of an array element in a table select expression
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNodeMulti
// </prerequisite>

// <etymology>
// TableExprNodeIndex is used to store an index.
// All the operands must be double.
// </etymology>

// <synopsis> 
// TableExprNodeIndex is a derivation of TableExprNodeMulti
// expression tree that represents an index.
// </synopsis> 

// <motivation>
// All operands of TableExprNodeIndex must be double,
// therefore it is a derivation of TableExprNodeMulti
// </motivation>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//   <li> to be filled in
// </todo>

class TableExprNodeIndex : public TableExprNodeMulti
{
public:
    // Constructor
    explicit TableExprNodeIndex (uInt origin = 0);

    // Destructor
    virtual ~TableExprNodeIndex();

    // Link all the operands and check datatype.
    // Calculate the IPosition value for the const operands.
    void fillNode (const PtrBlock<TableExprNodeRep*>& indices);

    // Check indices, all indices must be double
    // If not, an exception is thrown.
    static void checkIndices (const PtrBlock<TableExprNodeRep*>& indices);

    // Check if the index values match the dimensionality and shape
    // of fixed-shaped array.
    void checkIndexValues (const TableExprNodeRep* arrayNode);

    // Get the IPosition value of the index
    const IPosition& getIndex (uInt rownr);

protected:
    uInt        origin_p;    //# origin 0 for C++; 1 for TaQL
    IPosition   index_p;     //# precalculated constant IPosition values
    Block<Bool> varAxes_p;   //# if the index for an axis variable?
    Bool        isConst_p;   //# are all axes constant?

    // Precalculate the constant indices and store them in index_p.
    void convertConst();

    // Fill the index for this row.
    void fillIndex (uInt rownr);
};




// <summary>
// Array column element in table select expression
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
//   <li> TableExprNodeRep
//   <li> TableExprNodeBinary
// </prerequisite>

// <synopsis> 
// This class stores one element of an array column.
// It uses a TableExprNodeArray to store the array column
// and a TableExprNodeIndex to store the index.
// </synopsis> 

class TableExprNodeArrayElement : public TableExprNodeBinary
{
public:
    TableExprNodeArrayElement (TableExprNodeIndex*, NodeDataType tp);
    ~TableExprNodeArrayElement();
    Bool     getBool     (uInt rownr);
    double   getDouble   (uInt rownr);
    DComplex getDComplex (uInt rownr);
    String   getString   (uInt rownr);

    // Get the data type of this column (if possible).
    // It returns with a False status when the index is not constant
    // (that means that the index can vary with row number).
    Bool getColumnDataType (DataType&) const;

    Array<Bool>     getColumnBool();
    Array<uChar>    getColumnuChar();
    Array<Short>    getColumnShort();
    Array<uShort>   getColumnuShort();
    Array<Int>      getColumnInt();
    Array<uInt>     getColumnuInt();
    Array<Float>    getColumnFloat();
    Array<Double>   getColumnDouble();
    Array<Complex>  getColumnComplex();
    Array<DComplex> getColumnDComplex();
    Array<String>   getColumnString();

private:
    TableExprNodeIndex* indexNode_p;
}; 



inline const IPosition& TableExprNodeIndex::getIndex (uInt rownr)
{
    if (!isConst_p) {
	fillIndex (rownr);
    }
    return index_p;
}
inline const ROTableColumn& TableExprNodeArray::getColumn() const
    { return tabCol_p; }



#endif
