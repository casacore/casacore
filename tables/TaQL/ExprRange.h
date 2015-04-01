//# ExprRange.h: Select range of a column in an select expression
//# Copyright (C) 1994,1995,1999
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

#ifndef TABLES_EXPRRANGE_H
#define TABLES_EXPRRANGE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class TableColumn;


// <summary>
// Select range of a column in an select expression
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
// </prerequisite>

// <etymology>
// TableExprRange represents the ranges of a column as specified in
// a table select expression.
// </etymology>

// <synopsis> 
// TableExprRange holds the ranges of values for a column as specified
// in a table select expression.
// It traverses the expression tree and composes the hull of the values.
// Only double values are taken into account.
// It can handle operators &&, ||, ==, >, >=, <, <=, !.
// It can handle a comparison operator only for a column with a constant.
// Other operators and expressions are non-convertable.
//
// The ranges function in class TableExprNode returns a Block
// of TableExprRange objects which contains the ranges for each
// (applicable) column used in the expression.
// </synopsis> 

// <motivation>
// TableExprRange gives great possibilities in optimizing a table
// selection. It allows to get a rough estimate of the values needed
// for a column which can be used to do a fast preselect using an index.
// </motivation>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//   <li> Support other data types than double
//   <li> Recognize that 2*COL<3 is equal to COL<3/2
// </todo>


class TableExprRange
{
public:
    // Default constructor (needed for Block<TableExprRange>).
    TableExprRange();

    // Construct from a column and a single constant range.
    TableExprRange (const TableColumn&, double stval, double endval);

    // Copy constructor.
    TableExprRange (const TableExprRange&);

    ~TableExprRange ();

    // Assignment operator (copy semantics).
    TableExprRange& operator= (const TableExprRange&);

    // Return the vector of start values.
    // Together with the equally sized vector of end values, this forms
    // the ranges for the column (which can be acquired using getColumn).
    const Vector<double>& start() const;

    // Return the vector of end values.
    // Together with the equally sized vector of start values, this forms
    // the ranges for the column (which can be acquired using getColumn).
    const Vector<double>& end() const;

    // Return the column object.
    const TableColumn& getColumn() const;

    //*display 4
    // Mix with another range for an AND expression.
    void mixAnd (const TableExprRange&);

    //*display 4
    // Mix with another range for an OR expression.
    void mixOr (const TableExprRange&);

private:
    Vector<double>    sval_p;                 //# start values
    Vector<double>    eval_p;                 //# end values
    TableColumn*      tabColPtr_p;            //# pointer to column
};


inline const Vector<double>& TableExprRange::start() const
    { return sval_p; }
inline const Vector<double>& TableExprRange::end() const
    { return eval_p; }



} //# NAMESPACE CASACORE - END

#endif
