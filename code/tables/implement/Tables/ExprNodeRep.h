//# ExprNodeRep.h: Abstract base class for a node in a table column expression tree
//# Copyright (C) 1994,1995,1996,1997,1998
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

#if !defined(AIPS_EXPRNODEREP_H)
#define AIPS_EXPRNODEREP_H

//# Includes
#include <aips/aips.h>
#include <aips/Tables/ExprRange.h>
#include <aips/Mathematics/Complex.h>
#include <aips/Quanta/MVTime.h>
#include <aips/Utilities/DataType.h>

//# Forward Declarations
class TableExprNode;
class BaseTable;
class TableExprNodeColumn;
template<class T> class Block;
#if defined(AIPS_STDLIB)
#include <iosfwd.h>
#else
imported class ostream;
#endif


// <summary>
// Abstract base class for a node in a table column expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TableExprNode>TableExprNode</linkto>
// </prerequisite>

// <etymology>
// TableExprNodeRep is the (abstract) REPresentation of a node in a table
// expression tree.
// </etymology>

// <synopsis> 
// TableExprNodeRep is the base class for all nodes in a table
// expression tree. It is used by the handle class TableExprNode.
// <p>
// The objects of this class are reference-counted to make it possible
// that the same object is reused.
// </synopsis> 

// <motivation>
// TableExprNodeRep and its derivations store a table select expression
// before actually evaluating it. It is also possible that the classes
// are used by the table expression parser defined in TableParse and
// TableGram.
// <br>
// For each operator a special derived class is implemented.
// Another approach could have been to store the operator as
// a flag and switch on that. However, that causes extra overhead
// and the C++ virtual function mechanism is designed for
// these purposes.
// </motivation>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//   <li> add selection by comparing with a set of values
// </todo>


class TableExprNodeRep
{
public:
    // Define the data types of a node.
    enum NodeDataType {
	NTBool,
	NTDouble,
	NTComplex,
	NTString,
	NTRegex,
	NTDate,
	NTNumeric,             //# NTDouble or NTComplex
	NTAny                  //# Any data type
    };

    // Define the value types.
    enum ValueType {
	VTScalar,
	VTArray,
	VTRecord,
	VTSetElem,
	VTSet,
	VTIndex
    };

    // Define the operator types.
    // LE and LT are handled as GE and GT with swapped operands.
    enum OperType {OtPlus, OtMinus, OtTimes, OtDivide, OtModulo,
		   OtEQ, OtGE, OtGT, OtNE, OtIN,
		   OtAND, OtOR, OtNOT, OtMIN,
		   OtColumn, OtLiteral, OtFunc, OtSlice, OtUndef,
	           OtRownr, OtRandom
    };

    // Define the value types of the 2 arguments when arrays are involved.
    enum ArgType {
	NoArr, ArrArr, ArrSca, ScaArr
    };

    // Define (sub-)expression type
    enum ExprType {
	// A constant subexpression which can be evaluated immediately.
	Constant,
	// A variable (i.e. row dependent) subexpression which
	// has to be evaluated for each table row.
	Variable
	// An expensive constant subexpression which should only be
	// evaluated when needed (e.g. a subquery).
//	Lazy
    };

    // Construct a node.
    TableExprNodeRep (NodeDataType, ValueType, OperType, ArgType, ExprType,
		      Int ndim, const IPosition& shape,
		      const BaseTable* baseTablePtr);

    // This constructor is called from the derived TableExprNodeRep.
    TableExprNodeRep (NodeDataType, ValueType, OperType, const BaseTable*);

    // Copy constructor.
    TableExprNodeRep (const TableExprNodeRep&);

    // The destructor deletes all the underlying TableExprNode objects.
    virtual ~TableExprNodeRep();

    // Link to this object, i.e. increase its reference count.
    TableExprNodeRep* link();

    // Unlink from the given object.
    // If its reference count is zero, delete it.
    static void unlink (TableExprNodeRep*);

    // Get a scalar value for this node in the given row.
    // The appropriate functions are implemented in the derived classes and
    // will usually invoke the get in their children and apply the
    // operator on the resulting values.
    // <group>
    virtual Bool getBool         (uInt rownr);
    virtual Double getDouble     (uInt rownr);
    virtual DComplex getDComplex (uInt rownr);
    virtual String getString     (uInt rownr);
    virtual Regex getRegex       (uInt rownr);
    virtual MVTime getDate       (uInt rownr);
    // </group>

    // Get an array value for this node in the given row.
    // The appropriate functions are implemented in the derived classes and
    // will usually invoke the get in their children and apply the
    // operator on the resulting values.
    // <group>
    virtual Array<Bool> getArrayBool         (uInt rownr);
    virtual Array<Double> getArrayDouble     (uInt rownr);
    virtual Array<DComplex> getArrayDComplex (uInt rownr);
    virtual Array<String> getArrayString     (uInt rownr);
    virtual Array<MVTime> getArrayDate       (uInt rownr);
    // </group>

    // Does a value occur in an array or set?
    // <group>
    virtual Bool hasBool     (uInt rownr, Bool value);
    virtual Bool hasDouble   (uInt rownr, Double value);
    virtual Bool hasDComplex (uInt rownr, const DComplex& value);
    virtual Bool hasString   (uInt rownr, const String& value);
    virtual Bool hasDate     (uInt rownr, const MVTime& value);
    virtual Array<Bool> hasArrayBool     (uInt rownr,
					  const Array<Bool>& value);
    virtual Array<Bool> hasArrayDouble   (uInt rownr,
					  const Array<Double>& value);
    virtual Array<Bool> hasArrayDComplex (uInt rownr,
					  const Array<DComplex>& value);
    virtual Array<Bool> hasArrayString   (uInt rownr,
					  const Array<String>& value);
    virtual Array<Bool> hasArrayDate     (uInt rownr,
					  const Array<MVTime>& value);
    // </group>

    // Get the number of rows in the table associated with this expression.
    // Zero is returned when no table is associated with it.
    uInt nrow() const;

    // Get the data type of the column.
    // It returns True when it could set the data type (which it can
    // if the expression is a scalar column or a constant array column pixel).
    // Otherwise it returns False.
    virtual Bool getColumnDataType (DataType&) const;

    // Get the value of the expression evaluated for the entire column.
    // The data of function called should match the data type as
    // returned by function <src>getColumnDataType</src>.
    // <group>
    virtual Array<Bool>     getColumnBool();
    virtual Array<uChar>    getColumnuChar();
    virtual Array<Short>    getColumnShort();
    virtual Array<uShort>   getColumnuShort();
    virtual Array<Int>      getColumnInt();
    virtual Array<uInt>     getColumnuInt();
    virtual Array<Float>    getColumnFloat();
    virtual Array<Double>   getColumnDouble();
    virtual Array<Complex>  getColumnComplex();
    virtual Array<DComplex> getColumnDComplex();
    virtual Array<String>   getColumnString();
    // </group>

    // Convert the tree to a number of range vectors which at least
    // select the same things.
    // This function is very useful to convert the expression to
    // some intervals covering the select expression. This can
    // be used to do a rough fast selection via an index and do the
    // the slower final selection on that much smaller subset.
    // The function can only convert direct comparisons of columns
    // with constants (via ==, !=, >, >=, < or <=) and their combinations
    // using && or ||.
    virtual void ranges (Block<TableExprRange>&);

    // Get the data type of the derived TableExprNode object.
    // This is the data type of the resulting value. E.g. a compare
    // of 2 numeric values results in a Bool, thus the data type
    // of, say, TableExprNodeEQ<T> is always Bool.
    // Function getInternalDT gives the internal data type, thus in
    // the example above the data type of T.
    NodeDataType dataType() const;

    // Get the value type.
    ValueType valueType() const;

    // Get the operator type.
    OperType operType() const;

    // Get the expression type.
    ExprType exprType() const;

    // Is the exprerssion a constant?
    Bool isConstant() const;

    // Get the fixed dimensionality (same for all rows).
    Int ndim() const;

    // Get the fixed shape (same for all rows).
    const IPosition& shape() const;

    // Get the shape for the given row.
    // It returns the fixed shape if defined, otherwise getShape(rownr).
    const IPosition& shape (uInt rownr);

    // Is the value in the given row defined?
    // The default implementation returns True.
    virtual Bool isDefined (uInt rownr);

    // Show the expression tree.
    virtual void show (ostream&, uInt indent) const;

    // Get basetable. This gets a pointer to the BaseTable to which a
    // TableExprNode belongs. A TableExprNode belongs to the BaseTable to
    // which the column(s) used in an expression belong. Note that
    // all columns in an expression have to belong to the same table.
    const BaseTable* baseTablePtr() const;

    // Replace the Table pointer in this node and all its children.
    virtual void replaceTablePtr (const Table&, const BaseTable*);

    // Create a range object from a column and an interval.
    static void createRange (Block<TableExprRange>&,
			     TableExprNodeColumn*, Double start, Double end);

    // Create a empty range object.
    static void createRange (Block<TableExprRange>&);

protected:
    uInt              count_p;       //# Reference count
    const BaseTable*  baseTabPtr_p;  //# Table from which node is "derived"
    NodeDataType      dtype_p;       //# data type of the operation
    ValueType         vtype_p;       //# value type of the result
    OperType          optype_p;      //# operator type
    ArgType           argtype_p;     //# argument types
    ExprType          exprtype_p;    //# Constant or Variable
    Int               ndim_p;        //# Fixed dimensionality of node values
                                     //# -1 = variable dimensionality
    IPosition         shape_p;       //# Fixed shape of node values

    // Get the shape for the given row.
    virtual const IPosition& getShape (uInt rownr);

    // Get pointer to REPresentation object.
    // This is used by derived classes.
    static TableExprNodeRep* getRep (TableExprNode&);

    // When one of the children is a constant, convert its data type
    // to that of the other operand. This avoids that conversions are
    // done for each get.
    // The default implementation does nothing.
    virtual void convertConstChild();

    // Check if this node uses the same table pointer.
    // Fill the pointer if it is still zero.
    // <group>
    void checkTablePtr (const TableExprNodeRep* node);
    static void checkTablePtr (const BaseTable*& baseTablePtr,
			       const TableExprNodeRep* node);
    // </group>

    // Set expression type to Variable if node is Variable.
    // <group>
    void fillExprType (const TableExprNodeRep* node);
    static void fillExprType (ExprType&, const TableExprNodeRep* node);
    // </group>

    // When the node is constant, it is evaluated and replaced by
    // the appropriate TableExprNodeConst object.
    // If not constant, it calls the virtual ConvertConstChild function
    // which can convert a constant child when appropriate.
    static TableExprNodeRep* convertNode (TableExprNodeRep* thisNode,
					  Bool convertConstType);

private:
    // A copy of a TableExprNodeRep cannot be made.
    TableExprNodeRep& operator= (const TableExprNodeRep&);
};




// <summary>
// Abstract base class for a node having 0, 1, or 2 child nodes.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TableExprNodeRep>TableExprNodeRep</linkto>
// </prerequisite>

// <etymology>
// TableExprNodeBinary is a node in the table expression tree
// representing a binary node (i.e. having 2 operands).
// </etymology>

// <synopsis> 
// TableExprNodeBinary is the abstract base class for all nodes in a table
// expression tree using up to 2 operands.
// It is used as the base class for the node classes representing
// operator +, -, etc..
// </synopsis> 

// <motivation>
// This class contains the common functionality for the classes
// representing a binary (or unary) operator.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//#   <li> to be filled in
//# </todo>


class TableExprNodeBinary : public TableExprNodeRep
{
public:
    // Constructor
    TableExprNodeBinary (NodeDataType, ValueType, OperType, const BaseTable*);
    TableExprNodeBinary (NodeDataType, const TableExprNodeRep&, OperType);

    // Destructor
    virtual ~TableExprNodeBinary();
    
    // Show the expression tree.
    virtual void show (ostream&, uInt indent) const;

    // Check the data types and get the common one.
    static NodeDataType getDT (NodeDataType leftDtype,
			       NodeDataType rightDype,
			       OperType operType);

    // Check the data and value types and get the common one.
    static TableExprNodeRep getTypes (const TableExprNodeRep& left,
				      const TableExprNodeRep& right,
				      OperType operType);

    // Link the children to the node and convert the children
    // to constants if needed and possible. Also convert the node to
    // constant if possible.
    static TableExprNodeRep* fillNode (TableExprNodeBinary* thisNode,
				       TableExprNodeRep* left,
				       TableExprNodeRep* right,
				       Bool convertConstType);

    // When one of the children is a constant, convert its data type
    // to that of the other operand. This avoids that conversions are
    // done for each get.
    void convertConstChild();

    // Replace the Table pointer in this node and all its children.
    virtual void replaceTablePtr (const Table&, const BaseTable*);

protected:
    TableExprNodeRep* lnode_p;     //# left operand
    TableExprNodeRep* rnode_p;     //# right operand
};




// <summary>
// Abstract base class for a node having multiple child nodes.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TableExprNodeRep>TableExprNodeRep</linkto>
// </prerequisite>

// <etymology>
// TableExprNodeMulti is a node in the table expression tree
// which can have MULTIple child nodes.
// </etymology>

// <synopsis> 
// TableExprNodeMulti is the abstract base class for all nodes in a table
// expression tree using multiple operands.
// It is used as the base class for the node classes representing
// functions, sets, indices, etc..
// </synopsis> 

// <motivation>
// This class contains the common functionality for the classes
// representing a node with multiple operands.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//#   <li> to be filled in
//# </todo>


class TableExprNodeMulti : public TableExprNodeRep
{
public:
    // Constructor
    TableExprNodeMulti (NodeDataType, ValueType, OperType,
			const TableExprNodeRep& source);

    // Destructor
    virtual ~TableExprNodeMulti();

    // Show the expression tree.
    virtual void show (ostream&, uInt indent) const;

    // Check number of arguments
    // low <= number_of_args <= high
    // It throws an exception if wrong number of arguments.
    static uInt checkNumOfArg (uInt low, uInt high,
			       const PtrBlock<TableExprNodeRep*>& nodes);
    
    // Replace the Table pointer in this node and all its children.
    virtual void replaceTablePtr (const Table&, const BaseTable*);

protected:
    PtrBlock<TableExprNodeRep*> operands_p;


    // Check datatype of nodes and return output type.
    // It also sets the expected data type of the operands (from dtIn).
    static NodeDataType checkDT (Block<Int>& dtypeOper,
				 NodeDataType dtIn, NodeDataType dtOut,
				 const PtrBlock<TableExprNodeRep*>& nodes);
};



//# Get the data type of the node.
inline TableExprNodeRep::NodeDataType TableExprNodeRep::dataType() const
    { return dtype_p; }

//# Get the data type of the node.
inline TableExprNodeRep::ValueType TableExprNodeRep::valueType() const
    { return vtype_p; }

//# Get the operator type of the node.
inline TableExprNodeRep::OperType TableExprNodeRep::operType() const
    { return optype_p; }

//# Get the expression type of the node.
inline TableExprNodeRep::ExprType TableExprNodeRep::exprType() const
    { return exprtype_p; }

//# Is the expression a constant?
inline Bool TableExprNodeRep::isConstant() const
    { return ToBool (exprtype_p == Constant); }

//# Get the fixed dimensionality of the node.
inline Int TableExprNodeRep::ndim() const
    { return ndim_p; }

//# Get the fixed shape of the node.
inline const IPosition& TableExprNodeRep::shape() const
    { return shape_p; }

//# Get the table from which the node is derived.
inline const BaseTable* TableExprNodeRep::baseTablePtr() const
    { return baseTabPtr_p; }

inline void TableExprNodeRep::checkTablePtr (const TableExprNodeRep* node)
    { checkTablePtr (baseTabPtr_p, node); }
inline void TableExprNodeRep::fillExprType (const TableExprNodeRep* node)
    { fillExprType (exprtype_p, node); }

//# Link to the node.
inline TableExprNodeRep* TableExprNodeRep::link()
{
    count_p++;
    return this;
}

#endif
