//# LatticeParse.cc: Classes to hold results from lattice expression parser
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

#include <trial/Lattices/LatticeParse.h>
#include <trial/Lattices/LatticeGram.h>
#include <trial/Lattices/LatticeExprNode.h>
#include <trial/Lattices/PagedArray.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/ColumnDesc.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>


//# Initialize static members.
LatticeExprNode LatticeParse::theirNode;



LatticeParse::LatticeParse (Int value)
: itsType (TpInt),
  itsIval (value)
{}

LatticeParse::LatticeParse (Float value)
: itsType (TpFloat),
  itsFval (value)
{}

LatticeParse::LatticeParse (Double value)
: itsType (TpDouble),
  itsDval (value)
{}

LatticeParse::LatticeParse (const Complex& value)
: itsType (TpComplex),
  itsCval (value)
{}

LatticeParse::LatticeParse (const DComplex& value)
: itsType  (TpDComplex),
  itsDCval (value)
{}

LatticeParse::LatticeParse (const String& value)
: itsType (TpString),
  itsSval (value)
{}


LatticeExprNode LatticeParse::command (const String& str)
{
    String message;
    String command = str + '\n';
    Bool error = False;
    try {
	// Parse and execute the command.
	if (latticeGramParseCommand(command) != 0) {
	    throw (AipsError("Parse error in lattice expression " + str));
	}
    } catch (AipsError x) {
	message = x.getMesg();
	error = True;
    } end_try;
    //# Save the resulting expression and clear the common node object.
    LatticeExprNode node = theirNode;
    theirNode = LatticeExprNode();
    //# If an exception was thrown; throw it again with the message.
    if (error) {
	throw (AipsError(message + '\n' + "Scanned so far: " +
	                 command.before(latticeGramPosition())));
    }
    return node;
}


LatticeExprNode LatticeParse::makeFuncNode() const
{
    AlwaysAssert (itsType == TpString, AipsError);
    if (itsSval == "pi") {
	return LatticeExprNode (C::pi);
    } else if (itsSval == "e") {
	return LatticeExprNode (C::e);
    } else {
	throw (AipsError ("0-argument function " + itsSval + " is unknown"));
    }
    return LatticeExprNode();
}

LatticeExprNode LatticeParse::makeFuncNode (const LatticeExprNode& arg1) const
{
    AlwaysAssert (itsType == TpString, AipsError);
    if (itsSval == "sin") {
	return sin(arg1);
    } else if (itsSval == "sinh") {
	return sinh(arg1);
    } else if (itsSval == "asin") {
	return asin(arg1);
    } else if (itsSval == "cos") {
	return cos(arg1);
    } else if (itsSval == "cosh") {
	return cosh(arg1);
    } else if (itsSval == "acos") {
	return acos(arg1);
    } else if (itsSval == "tan") {
	return tan(arg1);
    } else if (itsSval == "tanh") {
	return tanh(arg1);
    } else if (itsSval == "atan") {
	return atan(arg1);
    } else if (itsSval == "exp") {
	return exp(arg1);
    } else if (itsSval == "log") {
	return log(arg1);
    } else if (itsSval == "log10") {
	return log10(arg1);
    } else if (itsSval == "sqrt") {
	return sqrt(arg1);
    } else if (itsSval == "ceil") {
	return ceil(arg1);
    } else if (itsSval == "floor") {
	return floor(arg1);
    } else if (itsSval == "conj") {
	return conj(arg1);
    } else if (itsSval == "abs"  ||  itsSval == "amplitude") {
	return abs(arg1);
    } else if (itsSval == "arg"  ||  itsSval == "phase") {
	return arg(arg1);
    } else if (itsSval == "real") {
	return real(arg1);
    } else if (itsSval == "imag") {
	return imag(arg1);
    } else if (itsSval == "min") {
	return min(arg1);
    } else if (itsSval == "max") {
	return max(arg1);
    } else if (itsSval == "mean") {
	return mean(arg1);
    } else if (itsSval == "sum") {
	return sum(arg1);
    } else if (itsSval == "nelements"  ||  itsSval == "count") {
	return nelements(arg1);
    } else if (itsSval == "any") {
	return any(arg1);
    } else if (itsSval == "all") {
	return all(arg1);
    } else if (itsSval == "ntrue") {
	return ntrue(arg1);
    } else if (itsSval == "nfalse") {
	return nfalse(arg1);
    } else if (itsSval == "float") {
	return toFloat(arg1);
    } else if (itsSval == "double") {
	return toDouble(arg1);
    } else if (itsSval == "complex") {
	return toComplex(arg1);
    } else if (itsSval == "dcomplex") {
	return toDComplex(arg1);
    } else {
	throw (AipsError ("1-argument function " + itsSval + " is unknown"));
    }
    return LatticeExprNode();
}

LatticeExprNode LatticeParse::makeFuncNode (const LatticeExprNode& arg1,
					    const LatticeExprNode& arg2) const
{
    AlwaysAssert (itsType == TpString, AipsError);
    if (itsSval == "atan2") {
	return atan2(arg1, arg2);
    } else if (itsSval == "pow") {
	return pow(arg1, arg2);
    } else if (itsSval == "fmod") {
	return fmod(arg1, arg2);
    } else if (itsSval == "min") {
	return min(arg1, arg2);
    } else if (itsSval == "max") {
	return max(arg1, arg2);
    } else if (itsSval == "amp") {
	return amp(arg1, arg2);
    } else {
	throw (AipsError ("2-argument function " + itsSval + " is unknown"));
    }
    return LatticeExprNode();
}


LatticeExprNode LatticeParse::makeLiteralNode() const
{
    switch (itsType) {
    case TpInt:
	return LatticeExprNode (itsIval);
    case TpFloat:
	return LatticeExprNode (itsFval);
    case TpDouble:
	return LatticeExprNode (itsDval);
    case TpComplex:
	return LatticeExprNode (itsCval);
    case TpDComplex:
	return LatticeExprNode (itsDCval);
    default:
	throw (AipsError ("LatticeParse: unknown data type for literal"));
    }
    return LatticeExprNode();
}

LatticeExprNode LatticeParse::makeLatticeNode() const
{
    DataType dtype = TpOther;
    String colName;
    if (Table::isReadable(itsSval)) {
	try {
	    TableDesc desc;
	    uInt nrow = Table::getLayout (desc, itsSval);
	    if (nrow != 1) {
		throw (AipsError ("LatticeParse can only handle Lattices/"
				  "Images with 1 row"));
	    }
	    ColumnDesc cdesc = desc[0];
	    if (cdesc.isArray()) {
		dtype = cdesc.dataType();
		colName = cdesc.name();
	    }
	} catch (AipsError x) {
	    // Nothing
	} end_try;
    }
    Table table(itsSval);
    switch (dtype) {
    case TpBool:
	return LatticeExprNode (PagedArray<Bool> (table, colName, 0));
    case TpFloat:
	return LatticeExprNode (PagedArray<Float> (table, colName, 0));
    case TpDouble:
	return LatticeExprNode (PagedArray<Double> (table, colName, 0));
    case TpComplex:
	return LatticeExprNode (PagedArray<Complex> (table, colName, 0));
    case TpDComplex:
	return LatticeExprNode (PagedArray<DComplex> (table, colName, 0));
    default:
	throw (AipsError ("LatticeParse: " + itsSval +
			  " is an unknown Lattice/Image name"));
    }
    return LatticeExprNode();
}
