//# ExprNode.h: Handle class for a table column expression tree
//# Copyright (C) 1994,1995,1996,1997
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

#if !defined(AIPS_EXPRNODE_H)
#define AIPS_EXPRNODE_H

#if defined(_AIX)
#pragma implementation ("ExprNode.cc")
#endif

//# Includes
#include <aips/aips.h>
#include <aips/Tables/ExprNodeRep.h>
#include <aips/Tables/ExprRange.h>
#include <aips/Utilities/DataType.h>
#include <aips/Mathematics/Complex.h>
#include <aips/Tables/ExprFuncNode.h>

//# Forward Declarations
class BaseTable;
class Table;
class String;
class TableRecord;
template<class T> class Block;
template<class T> class Array;


// <summary>
// Handle class for a table column expression tree
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=Table>Table</linkto>
// </prerequisite>

// <etymology>
// TableExprNode represents a node in the tree reflecting a
// table select expression.
// </etymology>

// <synopsis> 
// TableExprNode is the class to store a table select expression,
// which allows to select rows from the table. The selected rows form
// a table which is a view of the original table.
// <p>
// TableExprNode is a handle class for the counted referenced class
// TableExprNodeRep.
// Classes (like TableExprNodePlusXX) derived from TableExprNodeRep
// hold the individual
// nodes in the expression, i.e. the operators and operands. The nodes
// form a binary tree reflecting the expression.
// E.g. the expression 2*COLUMN results in the node TableExprNodeTimes
// with its children TableExprNodeConst and TableExprNodeColumn.
// Constant subexpressions (like 2*3) are evaluated immediately and
// only the result is stored as a node.
// <p>
// There are a few TableExprNode constructors taking a constant.
// In this way constant value are automatically converted to the
// appropriate TableExprNodeConst object.
// <p>
// The derived classes also reflect the data type of the node.
// Data types Bool, double, DComplex and String are used.
// Char, uChar, Short, uShort, Int, uInt and float are converted 
// to double and Complex to DComplex.
// Binary operators +, -, *, /, ==, >=, >, <, <= and != are recognized.
// Also &&, ||, parentheses and unary +, - and ! are recognized.
// For strings the binary operator + can also be used.
// The operators have the normal C++ precedence.
// Furthermore functions (like sin, max, ceil) can be used in an expression.
// <p>
// The Table function col has to be used to create a TableExprNode
// object for a column in the table. The Table
// <linkto file="Table.h#keycol">operator()</linkto> can be used
// the do the actual selection from the top TableExprNode object.
// </synopsis> 

// <example>
// <srcblock>
//   // Select from table X all rows where column RA<5 and where column
//   // SWITCH is true.
//   Table table("X");
//   Table subtable = table(table.col("RA") < 5 && table.col("SWITCH"));
//
//   // Select from that result all rows where the concatenation of
//   // the strings in columns STR1 and STR2 is equal to the string
//   // in keyword STRKEY.
//   Table subsub = subtable(subtable.col("STR1") + subtable.col("STR2")
//                           == subtable.key("STRKEY"));
// </srcblock>
// </example>

// <motivation>
// Having TableExprNode as a handle class makes it possible to
// handle temporary objects created by the compiler in a smooth way.
// TableExprNode and its derivations allow to store an expression
// before actually evaluating it. This also allows the classes to
// be used by the table expression parser defined in TableParse and
// TableGram.
//
// For each operator a special derived class is implemented.
// Another approach could have been to store the operator as
// a flag and switch on that. However, that causes extra overhead
// and the C++ virtual function mechanism is the designed for
// these purposes.
// </motivation>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//   <li> add operations on arrays
//   <li> add selection by comparing with a set of values
// </todo>


class TableExprNode
{
    friend class TableExprNodeRep;

    //# Define the operations we allow.
    //# Note that the arguments are defined as const. This is necessary
    //# because the compiler generates temporaries when converting a constant
    //# to a TableExprNode using the constructors. Temporaries has to be const.
    //# However, we have to delete created nodes, so lnode_p and rnode_p
    //# cannot be const. The const arguments are casted to a non-const in
    //# the function fill which calls the non-const function simplify.

    // Arithmetic operators for numeric TableExprNode's.
    // <group>
    // + is also defined for strings (means concatenation).
    friend TableExprNode operator+ (const TableExprNode& left,
				    const TableExprNode& right);
    friend TableExprNode operator- (const TableExprNode& left,
				    const TableExprNode& right);
    friend TableExprNode operator* (const TableExprNode& left,
				    const TableExprNode& right);
    friend TableExprNode operator/ (const TableExprNode& left,
				    const TableExprNode& right);
    friend TableExprNode operator% (const TableExprNode& left,
				    const TableExprNode& right);
    // </group>

    // Comparison operators.
    // <group>
    friend TableExprNode operator== (const TableExprNode& left,
				     const TableExprNode& right);
    friend TableExprNode operator!= (const TableExprNode& left,
				     const TableExprNode& right);
    // Not defined for Bool.
    // <group>
    friend TableExprNode operator>= (const TableExprNode& left,
				     const TableExprNode& right);
    friend TableExprNode operator>  (const TableExprNode& left,
				     const TableExprNode& right);
    friend TableExprNode operator<= (const TableExprNode& left, 
				     const TableExprNode& right);
    friend TableExprNode operator<  (const TableExprNode& left,
				     const TableExprNode& right);
    // </group>
    // </group>

    // Logical operators to combine boolean TableExprNode's.
    // <group>
    friend TableExprNode operator&& (const TableExprNode& left,
				     const TableExprNode& right);
    friend TableExprNode operator|| (const TableExprNode& left,
				     const TableExprNode& right);
    // </group>

    // Transcendental functions that can be applied to essentially all numeric
    // nodes.
    // <group>
    friend TableExprNode sin    (const TableExprNode& node);
    friend TableExprNode sinh   (const TableExprNode& node);
    friend TableExprNode cos    (const TableExprNode& node);
    friend TableExprNode cosh   (const TableExprNode& node);
    friend TableExprNode exp    (const TableExprNode& node);
    friend TableExprNode log    (const TableExprNode& node);
    friend TableExprNode log10  (const TableExprNode& node);
    friend TableExprNode pow    (const TableExprNode& x,
				 const TableExprNode& y);
    friend TableExprNode square (const TableExprNode& node);
    friend TableExprNode sqrt   (const TableExprNode& node);
    friend TableExprNode norm   (const TableExprNode& node);
    // </group>

    // Transcendental functions applied to the array on an element-by-element
    // basis. They do not make sense for Complex nodes.
    // <group>
    friend TableExprNode asin  (const TableExprNode& node);
    friend TableExprNode acos  (const TableExprNode& node);
    friend TableExprNode atan  (const TableExprNode& node);
    friend TableExprNode atan2 (const TableExprNode& y,
				const TableExprNode& x);
    friend TableExprNode tan   (const TableExprNode& node);
    friend TableExprNode tanh  (const TableExprNode& node);
    friend TableExprNode sign  (const TableExprNode& node);
    friend TableExprNode round (const TableExprNode& node);
    friend TableExprNode ceil  (const TableExprNode& node);
    friend TableExprNode abs   (const TableExprNode& node);
    friend TableExprNode floor (const TableExprNode& node);
    friend TableExprNode fmod  (const TableExprNode& x,
				const TableExprNode& y);
    // </group>

    // String functions.
    // <group>
    friend TableExprNode strlength (const TableExprNode& node);
    friend TableExprNode upcase    (const TableExprNode& node);
    friend TableExprNode downcase  (const TableExprNode& node);
    friend TableExprNode trim      (const TableExprNode& node);
    // </group>

    // Functions for regular expression matching and 
    // pattern matching.
    // <group>
    friend TableExprNode regex   (const TableExprNode& node);
    friend TableExprNode pattern (const TableExprNode& node);
    // </group>

    // Functions for date-values
    // <group>
    friend TableExprNode datetime  (const TableExprNode& node);
    friend TableExprNode mjdtodate (const TableExprNode& node);
    friend TableExprNode mjd       (const TableExprNode& node);
    friend TableExprNode date      (const TableExprNode& node);
    friend TableExprNode year      (const TableExprNode& node);
    friend TableExprNode month     (const TableExprNode& node);
    friend TableExprNode day       (const TableExprNode& node);
    friend TableExprNode cmonth    (const TableExprNode& node);
    friend TableExprNode weekday   (const TableExprNode& node);
    friend TableExprNode cdow      (const TableExprNode& node);
    friend TableExprNode week	   (const TableExprNode& node);
    friend TableExprNode time      (const TableExprNode& node);
    // </group>

    // Minimum or maximum of 2 nodes.
    // Makes sense for numeric and String values. For Complex values
    // the norm is compared.
    // <group>
    friend TableExprNode min (const TableExprNode& a, const TableExprNode& b);
    friend TableExprNode max (const TableExprNode& a, const TableExprNode& b);
    // </group>

    // The complex conjugate of a complex node.
    friend TableExprNode conj (const TableExprNode& node);

    // The real part of a complex node.
    friend TableExprNode real (const TableExprNode& node);

    // The imaginary part of a complex node.
    friend TableExprNode imag (const TableExprNode& node);

    // The amplitude (i.e. sqrt(re*re + im*im)) of a complex node.
    // This is a synonym for function abs.
    friend TableExprNode amplitude (const TableExprNode& node);

    // The phase (i.e. atan2(im, re)) of a complex node.
    // This is a synonym for function arg.
    friend TableExprNode phase (const TableExprNode& node);

    // The arg (i.e. atan2(im, re)) of a complex node.
    friend TableExprNode arg (const TableExprNode& node);

    // Form a complex number from two doubles.
    friend TableExprNode complex (const TableExprNode& real,
				  const TableExprNode& imag);

public:
				    TableExprNode ();

    // Unary operators on numeric TableExprNode's.
    // <group>
    TableExprNode operator+ ();
    TableExprNode operator- ();
    // </group>
    // Unary NOT-operator on boolean TableExprNode's.
    TableExprNode operator! ();

    // Constructors to convert a constant value to a TableExprNode.
    // <group>
    TableExprNode (const Bool& value);
    TableExprNode (const Int& value);
    TableExprNode (const double& value);
    TableExprNode (const DComplex& value);
    TableExprNode (const String& value);
    TableExprNode (const Regex& value);
    TableExprNode (const MVTime& value);

    // The constructor for char* is also supported to convert a
    // character-array to a string, since a two step conversion
    // is not done automatically.
    TableExprNode (const char*);
    // </group>

    // Construct a node from a node representation.
    TableExprNode (TableExprNodeRep*);

    // copy constructor (reference semantics).
    TableExprNode (const TableExprNode&);

    // Assignment (reference semantics).
    TableExprNode& operator= (const TableExprNode&);

    // The destructor deletes all the underlying TableExprNode objects,
    ~TableExprNode ();

    // Get the data type of the expression.
    // Currently the only possible values are TpBool, TpDouble,
    // TpDComplex and TpString.
    DataType dataType() const;

    // Get a value for this node in the given row.
    // These functions are implemented in the derived classes and
    // will usually invoke the get in their children and apply the
    // operator on the resulting values.
    // <group>
    void get (uInt rownr, Bool& value) const;
    void get (uInt rownr, double& value) const;
    void get (uInt rownr, DComplex& value) const;
    void get (uInt rownr, String& value) const;
    void get (uInt rownr, Regex& value) const;
    void get (uInt rownr, MVTime& value) const;
    // </group>

    // Get the data type for doing a getColumn on the expression.
    // This is the data type of the column when the expression
    // consists of a single column only.
    // Otherwise it is the expression data type as returned by
    // function <src>dataType</src>.
    DataType getColumnDataType() const;

    // Get the value of the expression evaluated for the entire column.
    // The data of function called should match the data type as
    // returned by function <src>getColumnDataType</src>.
    // <group>
    Array<Bool>     getColumnBool() const;
    Array<uChar>    getColumnuChar() const;
    Array<Short>    getColumnShort() const;
    Array<uShort>   getColumnuShort() const;
    Array<Int>      getColumnInt() const;
    Array<uInt>     getColumnuInt() const;
    Array<Float>    getColumnFloat() const;
    Array<Double>   getColumnDouble() const;
    Array<Complex>  getColumnComplex() const;
    Array<DComplex> getColumnDComplex() const;
    Array<String>   getColumnString() const;
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
    void ranges (Block<TableExprRange>&);

    // Check if the Table used in the expression matches the given
    // Table. This is used by the Table selection and sorting to
    // check if it is correct.
    Bool checkTable (const Table& table) const;

    // Get basetable. This gets a pointer to the BaseTable to which a
    // TableExprNode belongs. A TableExprNode belongs to the BaseTable to
    // which the column(s) used in an expression belong. Note that
    // all columns in an expression have to belong to the same table.
    const BaseTable* baseTablePtr() const;

    // Create a column node on behalf of the Table class.
    // For builtin data types another type of node is created than
    // for other data types.
    // isArray indicates if the column should be an array column.
    static TableExprNode newColumnNode (const Table& tab,
					const BaseTable* tabptr,
					const String& name,
					Bool isArray);

    // Create a TableExprNodeConst for a table keyword
    // (which is handled as a constant).
    static TableExprNode newKeyConst (const TableRecord&, const String& name,
				      Bool isArray);

    // Throw invalid data type exception.
    // <group>
    static void throwInvDT();
    static void throwInvDT (const String& message);
    // </group>

    // Create function node of the given type with the given arguments.
    // <group>
    static TableExprNode newFunctionNode (TableExprFuncNode::FunctionType,
					  Block<TableExprNode>& nodes,
					  const Table& table);
    static TableExprNode newFunctionNode (TableExprFuncNode::FunctionType,
					  const TableExprNode& node);
    static TableExprNode newFunctionNode (TableExprFuncNode::FunctionType,
					  const TableExprNode& node1,
					  const TableExprNode& node2);
    // </group>

    // Create rownumber() function node.
    // Origin indicates whether the first row should be zero (for C++ binding)
    // or an other value (one for TaQL binding).
    static TableExprNode newRownrNode (const BaseTable* tabptr, uInt origin);

    // Create rand() function node.
    static TableExprNode newRandomNode (const BaseTable* tabptr);

    // Create ArrayElement node for the given array column
    // with the given index
    static TableExprNode newArrayElementNode (TableExprNode& arrayColumn,
					      Block<TableExprNode>& indices);
 

private:
    // returns pointer to the representation-object of it
    TableExprNodeRep* getRep();

    // convert Block of TableExprNode to PtrBlock of TableExprNodeRep*.
    static PtrBlock<TableExprNodeRep*> convertBlockTEN
                                             (Block<TableExprNode>& nodes);

    // Construct a new node for the given operation.
    // <group>
    TableExprNodeRep* newPlus   (TableExprNodeRep* right) const;
    TableExprNodeRep* newMinus  (TableExprNodeRep* right) const;
    TableExprNodeRep* newTimes  (TableExprNodeRep* right) const;
    TableExprNodeRep* newDivide (TableExprNodeRep* right) const;
    TableExprNodeRep* newModulo (TableExprNodeRep* right) const;
    TableExprNodeRep* newEQ     (TableExprNodeRep* right) const;
    TableExprNodeRep* newNE     (TableExprNodeRep* right) const;
    TableExprNodeRep* newGE     (TableExprNodeRep* right) const;
    TableExprNodeRep* newGT     (TableExprNodeRep* right) const;
    TableExprNodeRep* newOR     (TableExprNodeRep* right) const;
    TableExprNodeRep* newAND    (TableExprNodeRep* right) const;
    // </group>

    // The actual (counted referenced) representation of a node.
    TableExprNodeRep* node_p;
};




inline void TableExprNode::ranges (Block<TableExprRange>& blrange)
    { node_p->ranges (blrange); }

//# Get the table from which the node is derived.
inline const BaseTable* TableExprNode::baseTablePtr() const
    { return node_p->baseTablePtr(); }

//# Get the value of an expression.
inline void TableExprNode::get (uInt rownr, Bool& value) const
    { value = node_p->getBool (rownr); }
inline void TableExprNode::get (uInt rownr, double& value) const
    { value = node_p->getDouble (rownr); }
inline void TableExprNode::get (uInt rownr, DComplex& value) const
    { value = node_p->getDComplex (rownr); }
inline void TableExprNode::get (uInt rownr, String& value) const
    { value = node_p->getString (rownr); }
inline void TableExprNode::get (uInt rownr, Regex& value) const
    { value = node_p->getRegex (rownr); }
inline void TableExprNode::get (uInt rownr, MVTime& value) const
    { value = node_p->getDate (rownr); }

inline Array<Bool>      TableExprNode::getColumnBool() const
    { return node_p->getColumnBool(); }
inline Array<uChar>     TableExprNode::getColumnuChar() const
    { return node_p->getColumnuChar(); }
inline Array<Short>     TableExprNode::getColumnShort() const
    { return node_p->getColumnShort(); }
inline Array<uShort>    TableExprNode::getColumnuShort() const
    { return node_p->getColumnuShort(); }
inline Array<Int>       TableExprNode::getColumnInt() const
    { return node_p->getColumnInt(); }
inline Array<uInt>      TableExprNode::getColumnuInt() const
    { return node_p->getColumnuInt(); }
inline Array<Float>     TableExprNode::getColumnFloat() const
    { return node_p->getColumnFloat(); }
inline Array<Double>    TableExprNode::getColumnDouble() const
    { return node_p->getColumnDouble(); }
inline Array<Complex>   TableExprNode::getColumnComplex() const
    { return node_p->getColumnComplex(); }
inline Array<DComplex>  TableExprNode::getColumnDComplex() const
    { return node_p->getColumnDComplex(); }
inline Array<String>    TableExprNode::getColumnString() const
    { return node_p->getColumnString(); }


inline TableExprNode operator+ (const TableExprNode& left,
				const TableExprNode& right)
{
    return left.newPlus (right.node_p);
}
inline TableExprNode operator- (const TableExprNode& left,
				const TableExprNode& right)
{
    return left.newMinus (right.node_p);
}
inline TableExprNode operator* (const TableExprNode& left,
				const TableExprNode& right)
{
    return left.newTimes (right.node_p);
}
inline TableExprNode operator/ (const TableExprNode& left,
				const TableExprNode& right)
{
    return left.newDivide (right.node_p);
}
inline TableExprNode operator% (const TableExprNode& left,
				const TableExprNode& right)
{
    return left.newModulo (right.node_p);
}
inline TableExprNode operator== (const TableExprNode& left,
				 const TableExprNode& right)
{
    return left.newEQ (right.node_p);
}
inline TableExprNode operator!= (const TableExprNode& left,
				 const TableExprNode& right)
{
    return left.newNE (right.node_p);
}
inline TableExprNode operator> (const TableExprNode& left,
				const TableExprNode& right)
{
    return left.newGT (right.node_p);
}
inline TableExprNode operator>= (const TableExprNode& left,
				 const TableExprNode& right)
{
    return left.newGE (right.node_p);
}
inline TableExprNode operator<= (const TableExprNode& left,
				 const TableExprNode& right)
{
    return right.newGE (left.node_p);
}
inline TableExprNode operator< (const TableExprNode& left,
				const TableExprNode& right)
{
    return right.newGT (left.node_p);
}
inline TableExprNode operator&& (const TableExprNode& left,
	const TableExprNode& right)
{
    return left.newAND (right.node_p);
}
inline TableExprNode operator|| (const TableExprNode& left,
				const TableExprNode& right)
{
    return left.newOR (right.node_p);
}

inline TableExprNode cos (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::cosFUNC, node);
}
inline TableExprNode cosh (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::coshFUNC, node);
}
inline TableExprNode exp (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::expFUNC, node);
}
inline TableExprNode log (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::logFUNC, node);
}
inline TableExprNode log10 (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::log10FUNC, node);
}
inline TableExprNode pow (const TableExprNode& x, const TableExprNode& y)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::powFUNC, x, y);
}
inline TableExprNode sin (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::sinFUNC, node);
}
inline TableExprNode sinh (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::sinhFUNC, node);
}
inline TableExprNode square (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::squareFUNC,
					   node);
}
inline TableExprNode sqrt (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::sqrtFUNC, node);
}
inline TableExprNode norm (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::normFUNC, node);
}
inline TableExprNode acos (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::acosFUNC, node);
}
inline TableExprNode asin (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::asinFUNC, node);
}
inline TableExprNode atan (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::atanFUNC, node);
}
inline TableExprNode atan2 (const TableExprNode& y, const TableExprNode& x)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::atan2FUNC, y, x);
}
inline TableExprNode sign (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::signFUNC, node);
}
inline TableExprNode round (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::roundFUNC, node);
}
inline TableExprNode ceil (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::ceilFUNC, node);
}
inline TableExprNode abs (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::absFUNC, node);
}
inline TableExprNode floor (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::floorFUNC, node);
}
inline TableExprNode fmod (const TableExprNode& x, const TableExprNode& y)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::fmodFUNC, x, y);
}
inline TableExprNode tan (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::tanFUNC, node);
}
inline TableExprNode tanh (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::tanhFUNC, node);
}
inline TableExprNode min (const TableExprNode& a, const TableExprNode& b)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::minFUNC, a, b);
}
inline TableExprNode max (const TableExprNode& a, const TableExprNode& b)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::maxFUNC, a, b);
}
inline TableExprNode real (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::realFUNC, node);
}
inline TableExprNode imag (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::imagFUNC, node);
}
inline TableExprNode conj (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::conjFUNC, node);
}
inline TableExprNode amplitude (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::absFUNC, node);
}
inline TableExprNode arg (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::argFUNC, node);
}
inline TableExprNode phase (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::argFUNC, node);
}
inline TableExprNode complex (const TableExprNode& real,
			      const TableExprNode& imag)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::complexFUNC,
					   real, imag);
}
inline TableExprNode strlength (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::strlengthFUNC,
					   node);
}
inline TableExprNode upcase (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::upcaseFUNC,
					   node);
}
inline TableExprNode downcase (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::downcaseFUNC,
					   node);
}
inline TableExprNode regex (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::regexFUNC, node);
}
inline TableExprNode pattern (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::patternFUNC,
					   node);
}
inline TableExprNode datetime (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::datetimeFUNC,
					   node);
}
inline TableExprNode mjdtodate (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::mjdtodateFUNC,
					   node);
}
inline TableExprNode mjd (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::mjdFUNC, node);
}
inline TableExprNode date (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::dateFUNC, node);
}
inline TableExprNode year (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::yearFUNC, node);
}
inline TableExprNode month (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::monthFUNC, node);
}
inline TableExprNode day (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::dayFUNC, node);
}
inline TableExprNode cmonth (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::cmonthFUNC,
					   node);
}
inline TableExprNode weekday (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::weekdayFUNC,
					   node);
}
inline TableExprNode cdow (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::cdowFUNC, node);
}
inline TableExprNode week (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::weekFUNC, node);
}
inline TableExprNode time (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::timeFUNC, node);
}
inline TableExprNode trim (const TableExprNode& node)
{
    return TableExprNode::newFunctionNode (TableExprFuncNode::trimFUNC, node);
}

inline TableExprNodeRep* TableExprNode::getRep()
{
    return node_p;
}


#endif
