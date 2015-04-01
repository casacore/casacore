//# ImageExprParse.h: Classes to hold results from image expression parser
//# Copyright (C) 1998,1999,2000,2003
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

#ifndef IMAGES_IMAGEEXPRPARSE_H
#define IMAGES_IMAGEEXPRPARSE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/LEL/LatticeExpr.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/stdvector.h>
#include <casacore/casa/Utilities/CountedPtr.h>
#include <casacore/casa/HDF5/HDF5File.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template<class T> class Block;
template<class T> class PtrBlock;
class ImageRegion;
class Table;
class Slice;


// <summary>
// Class to hold values from image expression parser
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//  <li> <linkto class=LatticeExpr>LatticeExpr</linkto>
// </prerequisite>

// <etymology>
// ImageExprParse is the class used to parse an image expression command.
// </etymology>

// <synopsis> 
// ImageExprParse is used by the parser of image expression statements.
// The parser is written in Bison and Flex in files ImageExprGram.y and .l.
// The statements in there use the routines in this file to act
// upon a reduced rule.
// <p>
// The main function (and the only function to be used by a user) is the
// static function ImageExprParse::command which parses an expression command.
// It returns a <linkto class=LatticeExprNode>LatticeExprNode</linkto>
// object containing the expression represented as a tree.
// The object can be used as a <src>Lattice(Expr)<T></src> in other operations.
// <p>
// The syntax of the command is similar to that of expressions in C++.
// E.g.
// <srcblock>
//   min(img1, img2) + sin(img3)
// </srcblock>
// The following items can be used in an expression:
// <ul>
//  <li> Binary operators +, -, *, /, % (modulo), and ^ (power).
//  <li> Unary operators + and -.
//  <li> Comparison operators ==, >, >=, <, <=, and !=.
//  <li> Logical operators &&, ||, and !.
//  <li> Constant single and double precision values.
//       <br>No exponent or exponent "e" results in single precision (Float),
//           while "d" results in double precision (Double).
//  <li> The imaginary part of a complex value can be given by the suffix "i".
//       A full complex number can be given by addition. E.g. "3+4i".
//       The complex is single (Complex) or double (DComplex) precision
//       depending on the constituting parts.
//  <li> The special constants pi and e can be given as a double precision
//       value by means of the functions pi() and e().
//  <li> Boolean constants T and F can be given.
//  <li> A lot of functions are available.
//       They are the same as the ones supported by class
//       <linkto class=LatticeExprNode>LatticeExprNode</linkto>.
//  <li> Explicit conversion functions float, double, complex and dcomplex
//       are available. Conversions are automatically done where needed,
//       but for performance reasons it may sometimes be better to do
//       explicit conversions. See also below in the first example.
//  <li> An image can to be given using its file name. The file name
//       can contain environment variables and user home directories
//       using the standard UNIX syntax $ENVVAR and ~username.
//       There are 3 ways to specify a file name:
//       <ol>
//        <li> When the name contains no other special characters than
//             $, ~, and . it can be given as such.
//        <li> Backslashes can be used to escape individual special characters.
//        <li> The full name can be enclosed in quotes (single or double)
//             to escape the entire name. Adjacent quoted parts
//             are combined to one name, which can be used to use quotes
//             in the file name.
//       </ol>
//       Note that escaping has to be used too for the file name
//       T or F (otherwise it is the boolean constant).
//       E.g.
//       <srcblock>
//          image.data
//          "~noordam/data/image.data"
//          "~/image.data"
//          "$HOME/image.data"
//          $HOME\/image.data
//          "ab'c"'d"e'          results in  ab'cd"e
//       </srcblock>
//       Only input images with data type Float and Complex are supported,
//       because those data types are the only ones used so far.
//       Support of Bool, Double, and DComplex is very simple to build in.
//       The resulting lattice can be of type Bool, Float, Double,
//       Complex, and DComplex.
//  <li> An image can also be given by means of the <src>$n</src> notation,
//       where <src>n</src> is the sequence number in the
//       <src>tempLattices</src> argument given to the <src>command</src>
//       function. Note that the sequence numbers start counting at 1
//       (to be compliant with glish indexing).
//       <br>It can, for instance, be used to use non-persistent lattices
//       in an expression.
// </ul>
// When the expression is parsed, it is checked if the images and lattices
// involved have conforming shapes and coordinates. Note, however, that
// some functions (e.g. mean) reduce an image to a scalar. Such an image
// can have a different shape and coordinates.
// <p>
// The data types of the images and constants involved can be different.
// The data type of a subexpression is the common data type (e.g.
// Float and Double result in Double; Complex and Double result in DComplex).
// Automatic implicit conversions are done where needed. However, for
// performance reasons it may sometimes be better to convert explicitly.
// See below in the first example.
// <p>
// The expression evaluator (which is not part of the parser) evaluates
// the expression in chunks to avoid having to keep large temporary
// results. A scalar subexpression is evaluated only once to avoid
// unnecessary (possibly expensive) calculations.
// <p>
// Some examples:
// <dl>
//  <dt> <src> img1 + min(float(pi()), mean(img2)) </src>
//  <dd> Suppose img1 and img2 are images with single precision data.
//       They do not need to have conforming shapes and coordinates,
//       because only the mean of img2 is used.
//       <br>Note that pi is explicitly converted to single precision,
//       because pi() results in a Double. If that was not done,
//       the expression result would be a Double with the effect that
//       all data of img1 had to be converted to Double.
//  <dt> <src> min(img1, (min(img1)+max(img1))/2) </src>
//  <dd> This example shows that there are 2 min functions. One with a
//       single argument returning the minimum value of that image.
//       The other with 2 arguments returning a lattice containing
//       img1 data clipped at the value of the 2nd argument.
// </dl>
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
// It is necessary to be able to give an image expression command in ASCII.
// This can be used in glish to operate on lattices/images.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class ImageExprParse
{
public:

    // Parse the given command.
    // It will open all lattices needed.
    // It returns the resulting image expression.
    // <br>The <src>tempLattices/tempRegions</src> arguments make it possible
    // to use temporary lattices/images and regions in the expression by means
    // of the <src>$n</src> notation.
    // <br> If a directory name is given, it is used instead of the working
    // directory for relative file names.
    // <group>
    static LatticeExprNode command (const String& str,
				    const String& dirName = String());
    static LatticeExprNode command (const String& str,
				    const Block<LatticeExprNode>& tempLattices,
				    const PtrBlock<const ImageRegion*>& tempRegions,
				    const String& dirName = String());
    // </group>

    // Construct a literal object for the given type.
    // <group>
    ImageExprParse (Bool value);
    ImageExprParse (Int value);
    ImageExprParse (Float value);
    ImageExprParse (Double value);
    ImageExprParse (const Complex& value);
    ImageExprParse (const DComplex& value);
    ImageExprParse (const Char* value);
    ImageExprParse (const String& value);
    // </group>

    // Make a LatticeExprNode for a function.
    // <group>
    LatticeExprNode makeFuncNode () const;
    LatticeExprNode makeFuncNode (const LatticeExprNode& arg1) const;
    LatticeExprNode makeFuncNode (const LatticeExprNode& arg1,
				  const LatticeExprNode& arg2) const;
    LatticeExprNode makeFuncNode (const LatticeExprNode& arg1,
				  const LatticeExprNode& arg2,
				  const LatticeExprNode& arg3) const;
    // </group>

    // Make a LatticeExprNode object for the lattice or region name.
    LatticeExprNode makeLRNode() const;

    // Make a LatticeExprNode object for the name of constant, lattice,
    // or region.
    LatticeExprNode makeLitLRNode() const;

    // Make a LatticeExprNode object for the temporary region number.
    LatticeExprNode makeRegionNode() const;

    // Make a LatticeExprNode object for the literal value.
    LatticeExprNode makeLiteralNode() const;

    // Make a Slice object from 1-3 literals.
    // <group>
    static Slice* makeSlice (const ImageExprParse& start);
    static Slice* makeSlice (const ImageExprParse& start,
			     const ImageExprParse& end);
    static Slice* makeSlice (const ImageExprParse& start,
			     const ImageExprParse& end,
			     const ImageExprParse& incr);
    // </group>

    // Make a node for the INDEXIN function.
    static LatticeExprNode makeIndexinNode (const LatticeExprNode& axis,
					    const vector<Slice>& slices);

    // Make an array from a value list.
    static LatticeExprNode makeValueList
                                  (const Block<LatticeExprNode>& values);

    // Make an IPosition containing the binning values.
    static IPosition makeBinning (const LatticeExprNode& values);

    // Set the static node object (used by the .y file).
    static void setNode (const LatticeExprNode& node)
        { theirNode = node; }

    // Keep track of the nodes allocated while parsing the expression.
    // <group>
    static void addNode (LatticeExprNode* node);
    static void addNode (ImageExprParse* node);
    static void deleteNodes();
    // </group>

    // A function to test addDir. It first sets the directory.
    static String setAddDir (const String& dirName, const String& fileName);

private:
    // If a directory was given, prepend it to the file name if relative.
    static String addDir (const String& fileName);

    // Try if the name represent a lattice or image.
    // Return False if not.
    Bool tryLatticeNode (LatticeExprNode& node, const String& name) const;

    // Make the node from the image name and a mask name.
    // The mask name can be NOMASK (case insensitive) meaning that no mask
    // is applied to the image.
    LatticeExprNode makeImageNode (const String& name,
				   const String& mask) const;

    // Callback function for RegionHandlerTable to get the table to be used.
    static Table& getRegionTable (void*, Bool);

    // Callback function for RegionHandlerHDF5 to get the file to be used.
    static const CountedPtr<HDF5File>& getRegionHDF5 (void*);

    //# A 'global' node object to hold the resulting expression.
    static LatticeExprNode theirNode;

    DataType itsType;
    Bool     itsBval;              //# boolean literal
    Int      itsIval;              //# integer literal
    Float    itsFval;              //# Float literal
    Double   itsDval;              //# Double literal
    Complex  itsCval;              //# Complex literal
    DComplex itsDCval;             //# DComplex literal
    String   itsSval;              //# lattice name; function name
};


} //# NAMESPACE CASACORE - END

#endif
