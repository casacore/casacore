//# LatticeParse.h: Classes to hold results from lattice expression parser
//# Copyright (C) 1998
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

#if !defined(AIPS_LATTICEPARSE_H)
#define AIPS_LATTICEPARSE_H


//# Includes
#include <trial/Lattices/LatticeExpr.h>
#include <aips/Mathematics/Complex.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/DataType.h>

//# Forward Declarations


// <summary>
// Class to hold values from lattice expression parser
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//  <li> <linkto class=LatticeExpr>LatticeExpr</linkto>
// </prerequisite>

// <etymology>
// LatticeParse is the class used to parse a lattice command.
// </etymology>

// <synopsis> 
// LatticeParse is used by the parser of lattice expression statements.
// The parser is written in Bison and Flex in files LatticeGram.y and .l.
// The statements in there use the routines in this file to act
// upon a reduced rule.
// <p>
// The class contains some values used by the parsing process.
// The only function to be used by a user is the static
// function LatticeParse::command which parses an expression.
// It returns a <linkto class=LatticeExprNode>LatticeExprNode</linkto>
// object containing the expression represented as a tree.
// The object can be used as a <src>Lattice(Expr)</src> in other operations.
// </synopsis> 

// <example>
// <srcblock>
//    LatticeExpr<Double> expr ("a + sin(b)");
//    ArrayLattice<Double> arr(expr.shape());
//    arr.copyData (expr);
// </srcblock>
// Line 1 creates a LatticeExpr object for the given expression. Note that
// <src>a</src> and <src>b</src> are names of lattice files (e.g. PagedImage).
// <br> Line 2 creates an ArrayLattice with the same shape as the expression
// (which is the shape of lattice a (and b)).
// <br> Line 3 copies the result of the expression to the ArrayLattice.
// </example>

// <motivation>
// It is necessary to be able to give a lattice expression command in ASCII.
// This can be used in glish to operate on lattices/images.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class LatticeParse
{
public:

    // Parse the given command.
    // It will open all lattices needed.
    // It returns the resulting lattice expression.
    static LatticeExprNode command (const String& str);

    // Construct a literal object for the given type.
    // <group>
    LatticeParse (Int value);
    LatticeParse (Float value);
    LatticeParse (Double value);
    LatticeParse (const Complex& value);
    LatticeParse (const DComplex& value);
    LatticeParse (const String& value);
    // </group>

    // Make a LatticeExprNode for a function.
    // <group>
    LatticeExprNode makeFuncNode () const;
    LatticeExprNode makeFuncNode (const LatticeExprNode& arg1) const;
    LatticeExprNode makeFuncNode (const LatticeExprNode& arg1,
				  const LatticeExprNode& arg2) const;
    // </group>

    // Make a LatticeExprNode object for the lattice.
    LatticeExprNode makeLatticeNode() const;

    // Make a LatticeExprNode object for the literal.
    LatticeExprNode makeLiteralNode() const;

    // Set the static node object (used by the .y file).
    static void setNode (const LatticeExprNode& node)
        { theirNode = node; }

private:
    //# A 'global' node object to hold the resulting expression.
    static LatticeExprNode theirNode;

    DataType itsType;
    Int      itsIval;              //# integer literal
    Float    itsFval;              //# Float literal
    Double   itsDval;              //# Double literal
    Complex  itsCval;              //# Complex literal
    DComplex itsDCval;             //# DComplex literal
    String   itsSval;              //# lattice name; function name
};


#endif
