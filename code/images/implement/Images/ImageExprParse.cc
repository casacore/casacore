//# ImageExprParse.cc: Classes to hold results from image expression parser
//# Copyright (C) 1998,1999
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

#include <trial/Images/ImageExprParse.h>
#include <trial/Images/ImageExprGram.h>
#include <trial/Images/PagedImage.h>
#include <trial/Lattices/LatticeExprNode.h>
#include <trial/Lattices/PagedArray.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/ColumnDesc.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>


//# Define a block to hold temporary lattices.
static const Block<LatticeExprNode>* theTempLattices;


//# Initialize static members.
LatticeExprNode ImageExprParse::theirNode;



ImageExprParse::ImageExprParse (Bool value)
: itsType (TpBool),
  itsBval (value)
{}

ImageExprParse::ImageExprParse (Int value)
: itsType (TpInt),
  itsIval (value)
{}

ImageExprParse::ImageExprParse (Float value)
: itsType (TpFloat),
  itsFval (value)
{}

ImageExprParse::ImageExprParse (Double value)
: itsType (TpDouble),
  itsDval (value)
{}

ImageExprParse::ImageExprParse (const Complex& value)
: itsType (TpComplex),
  itsCval (value)
{}

ImageExprParse::ImageExprParse (const DComplex& value)
: itsType  (TpDComplex),
  itsDCval (value)
{}

ImageExprParse::ImageExprParse (const Char* value)
: itsType (TpString),
  itsSval (String(value))
{}

ImageExprParse::ImageExprParse (const String& value)
: itsType (TpString),
  itsSval (value)
{}


LatticeExprNode ImageExprParse::command (const String& str)
{
    Block<LatticeExprNode> dummy;
    return command (str, dummy);
}
LatticeExprNode ImageExprParse::command
                           (const String& str,
			    const Block<LatticeExprNode>& tempLattices)
{
    theTempLattices = &tempLattices;
    String message;
    String command = str + '\n';
    Bool error = False;
    try {
	// Parse and execute the command.
	if (imageExprGramParseCommand(command) != 0) {
	    throw (AipsError("Parse error in image expression " + str));
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
	                 command.before(imageExprGramPosition())));
    }
    return node;
}


LatticeExprNode ImageExprParse::makeFuncNode() const
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

LatticeExprNode ImageExprParse::makeFuncNode (const LatticeExprNode& arg1) const
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

LatticeExprNode ImageExprParse::makeFuncNode (const LatticeExprNode& arg1,
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
    } else if (itsSval == "complex") {
	return complex(arg1, arg2);
    } else if (itsSval == "length") {
	return length(arg1, arg2);
    } else if (itsSval == "amp") {
	return amp(arg1, arg2);
    } else if (itsSval == "pa") {
	return pa(arg1, arg2);
    } else {
	throw (AipsError ("2-argument function " + itsSval + " is unknown"));
    }
    return LatticeExprNode();
}

LatticeExprNode ImageExprParse::makeFuncNode (const LatticeExprNode& arg1,
					      const LatticeExprNode& arg2,
					      const LatticeExprNode& arg3) const
{
    AlwaysAssert (itsType == TpString, AipsError);
    if (itsSval == "iif") {
	return iif(arg1, arg2, arg3);
    } else {
	throw (AipsError ("3-argument function " + itsSval + " is unknown"));
    }
    return LatticeExprNode();
}


LatticeExprNode ImageExprParse::makeLiteralNode() const
{
    switch (itsType) {
    case TpBool:
	return LatticeExprNode (itsBval);
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
	throw (AipsError ("ImageExprParse: unknown data type for literal"));
    }
    return LatticeExprNode();
}

LatticeExprNode ImageExprParse::makeLatticeNode() const
{
    // When the name is numeric, we have a temporary lattice number.
    // Find it in the block of temporary lattices.
    if (itsType == TpInt) {
        Int latnr = itsIval-1;
	if (latnr < 0  ||  latnr >= Int(theTempLattices->nelements())) {
	    throw (AipsError ("ImageExprParse: invalid temporary image "
			      "number given"));
	}
	return ((*theTempLattices)[latnr]);
    }
    // A true name has been given.
    if (! Table::isReadable(itsSval)) {
	throw (AipsError ("ImageExprParse: '" + itsSval +
			  "' is not a table or is not readable"));
    }
    Table table(itsSval);
    Bool isImage = True;
    String type = table.tableInfo().type();
    if (type == TableInfo::type(TableInfo::PAGEDARRAY)) {
	isImage = False;
    } else if (type != TableInfo::type(TableInfo::PAGEDIMAGE)) {
	throw (AipsError ("ImageExprParse: '" + itsSval +
			  "' is not a PagedArray or PagedImage"));
    }
    if (table.nrow() != 1) {
	throw (AipsError ("ImageExprParse can only handle Lattices/"
			  "Images with 1 row"));
    }
    DataType dtype = TpOther;
    String colName;
    ColumnDesc cdesc = table.tableDesc()[0];
    if (cdesc.isArray()) {
	dtype = cdesc.dataType();
	colName = cdesc.name();
    }
    if (isImage) {
	switch (dtype) {
///	case TpBool:
///	    return LatticeExprNode (PagedImage<Bool> (table, 0));
	case TpFloat:
	    return LatticeExprNode (PagedImage<Float> (table, 0));
///	case TpDouble:
///	    return LatticeExprNode (PagedImage<Double> (table, 0));
	case TpComplex:
	    return LatticeExprNode (PagedImage<Complex> (table, 0));
///	case TpDComplex:
///	    return LatticeExprNode (PagedImage<DComplex> (table, 0));
	default:
	    throw (AipsError ("ImageExprParse: " + itsSval + " is a PagedImage "
			      "with an unsupported data type"));
	}
    } else {
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
	    throw (AipsError ("ImageExprParse: " + itsSval + " is a PagedArray "
			      "with an unsupported data type"));
	}
    }
    return LatticeExprNode();
}

LatticeExprNode ImageExprParse::makeLitLattNode() const
{
    // The following outcommented code makes it possible to specify
    // a constant without ().
    // E.g.            image.file * pi
    // instead of      image.file * pi()
    // However, it forbids the use of pi, e, etc. as a lattice name and
    // may make things unclear. Therefore it is not supported (yet?).
///    if (itsSval == "pi") {
///	return LatticeExprNode (C::pi);
///    } else if (itsSval == "e") {
///	return LatticeExprNode (C::e);
///    }
    // It is a the name of a constant, so it must be a lattice name.
    return makeLatticeNode();
}
