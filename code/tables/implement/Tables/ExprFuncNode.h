//# ExprFuncNode.h: Class representing a function in table select expression
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

#if !defined(AIPS_EXPRFUNCNODE_H)
#define AIPS_EXPRFUNCNODE_H

#if defined(_AIX)
#pragma implementation ("ExprFuncNode.cc")
#endif

//# Includes
#include <aips/Tables/ExprNodeRep.h>


// <summary>
// Class representing a function in table select expression
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TableExprNodeMulti>TableExprNodeMulti</linkto>
// </prerequisite>

// <synopsis> 
// This class represents a function in a table select tree.
// The <src>rownumber</src> function is represented by class
// <linkto class=TableExprNodeRownr>TableExprNodeRownr</linkto>.
// <p>
// When one wants to add a function to the table selection grammar,
// the following has to be done:
// <ul>
//  <li> Add the function to the enum below.
//  <li> Implement the function in the get functions in ExprFuncNode.cc.
//  <li> Implement the function in the checkOperands in ExprFuncNode.cc.
//  <li> Declare and define the function in ExprNode.h (for C++ binding).
//  <li> Add the function to tableParseFunc in TableParse.cc (for TaQL).
// </ul>
// </synopsis> 


class TableExprFuncNode : public TableExprNodeMulti
{
public:
    //# Define the function types.
    enum FunctionType {
	piFUNC,
	eFUNC,
            // for double and DComplex returning same data type
	sinFUNC,
	sinhFUNC,
	cosFUNC,
	coshFUNC,
	expFUNC,
	logFUNC,
	log10FUNC,
	powFUNC,
	squareFUNC,
	sqrtFUNC,
	conjFUNC,
	minFUNC,
	maxFUNC,
            // for double and DComplex returning double
	normFUNC,
	absFUNC,
	argFUNC,
	realFUNC,
	imagFUNC,
            // for double returning double
	asinFUNC,
	acosFUNC,
	atanFUNC,
	atan2FUNC,
	tanFUNC,
	tanhFUNC,
	signFUNC,
	roundFUNC,
	floorFUNC,
	ceilFUNC,
	fmodFUNC,
            // for double returning DComplex
	complexFUNC,
            // for String
	strlengthFUNC,         //# returning double
	upcaseFUNC,            //# returning String
	downcaseFUNC,          //# returning String
	trimFUNC,              //# returning String
	regexFUNC,             //# returning Regex
	patternFUNC,           //# returning Regex
            // for Date
	datetimeFUNC,          //# returning Date
	mjdtodateFUNC,         //# returning Date
	mjdFUNC,               //# returning double
	dateFUNC,              //# returning Date
	timeFUNC,              //# returning double (in radians)
	yearFUNC,              //# returning double
	monthFUNC,             //# returning double
	dayFUNC,               //# returning double
	cmonthFUNC,            //# returning String
	weekdayFUNC,           //# returning double
	cdowFUNC,              //# returning String
	weekFUNC,              //# returning double
	    // special function to select on a random number
	randFUNC,
            // special function to select on row number
	rownrFUNC,
	NRFUNC      //# should be last
	};

    // Constructor
    TableExprFuncNode (FunctionType, NodeDataType);

    // Destructor
    ~TableExprFuncNode ();

    // 'get' Functions to get te desired result of a function
    // <group>
    Bool     getBool     (uInt rownr);
    double   getDouble   (uInt rownr);
    DComplex getDComplex (uInt rownr);
    String   getString   (uInt rownr);
    Regex    getRegex    (uInt rownr);
    MVTime   getDate     (uInt rownr);
    // </group>

    // Check the datatypes of the operands.
    // It sets the exptected data types of the operands.
    // Returns the resulting datatype of the function.
    static NodeDataType checkOperands (Block<Int>& dtypeOper,
				       FunctionType,
				       PtrBlock<TableExprNodeRep*>&);

private:
    FunctionType funcType_p;    // which function
};



#endif
