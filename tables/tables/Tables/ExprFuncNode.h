//# ExprFuncNode.h: Class representing a function in table select expression
//# Copyright (C) 1994,1995,1996,1997,1998,2000,2001,2003
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

#ifndef TABLES_EXPRFUNCNODE_H
#define TABLES_EXPRFUNCNODE_H

//# Includes
#include <tables/Tables/ExprNodeRep.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Forward Declarations
class TableExprNodeSet;


// <summary>
// Class representing a function in table select expression
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TableExprNodeMulti>TableExprNodeMulti</linkto>
// </prerequisite>

// <synopsis> 
// This class represents a function in a table select tree.
// The <src>rownumber</src> function is represented by class
// <linkto class=TableExprNodeRownr>TableExprNodeRownr</linkto>.
// The <src>rowid</src> function is represented by class
// <linkto class=TableExprNodeRowid>TableExprNodeRowid</linkto>.
// The <src>rand</src> function is represented by class
// <linkto class=TableExprNodeRandom>TableExprNodeRandom</linkto>.
// <p>
// When one wants to add a function to the table selection grammar,
// the following has to be done:
// <ul>
//  <li> Add the function to the enum below.
//  <li> Implement the function in the get functions in ExprFuncNode(Array).cc.
//  <li> Implement the function in the checkOperands in ExprFuncNode.cc.
//  <li> Declare and define the function in ExprNode.h (for C++ binding).
//  <li> Add the function to findFunc in TableParse.cc (for TaQL).
// </ul>
// </synopsis> 


class TableExprFuncNode : public TableExprNodeMulti
{
public:
    //# Define the function types.
    enum FunctionType {
	piFUNC,
	eFUNC,
	    // for Double or Complex returning Bool
	    // (2 is with default tolerance)
	near2FUNC,
	near3FUNC,
	nearabs2FUNC,
	nearabs3FUNC,
            // for Double or DComplex returning same data type
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
            // for Double or DComplex returning Double
	normFUNC,
	absFUNC,
	argFUNC,
	realFUNC,
	imagFUNC,
            // for Double returning Double
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
            // for Double returning DComplex
	complexFUNC,
	    // for Double or Complex array returning scalar
	arrsumFUNC,
	arrsumsFUNC,
	arrproductFUNC,
	arrproductsFUNC,
	arrsumsqrFUNC,
	arrsumsqrsFUNC,
	    // for Double array returning Double
	arrminFUNC,
	arrminsFUNC,
	runminFUNC,
	boxminFUNC,
	arrmaxFUNC,
	arrmaxsFUNC,
	runmaxFUNC,
	boxmaxFUNC,
	arrmeanFUNC,
	arrmeansFUNC,
	runmeanFUNC,
	boxmeanFUNC,
	arrvarianceFUNC,
	arrvariancesFUNC,
	runvarianceFUNC,
	boxvarianceFUNC,
	arrstddevFUNC,
	arrstddevsFUNC,
	runstddevFUNC,
	boxstddevFUNC,
	arravdevFUNC,
	arravdevsFUNC,
	runavdevFUNC,
	boxavdevFUNC,
	arrrmsFUNC,
	arrrmssFUNC,
	runrmsFUNC,
	boxrmsFUNC,
	arrmedianFUNC,
	arrmediansFUNC,
	runmedianFUNC,
	boxmedianFUNC,
	arrfractileFUNC,
	arrfractilesFUNC,
	    // for Bool array returning Bool
        anyFUNC,
        anysFUNC,
        runanyFUNC,
        boxanyFUNC,
	allFUNC,
	allsFUNC,
	runallFUNC,
	boxallFUNC,
	    // for Bool array returning Double
	ntrueFUNC,
	ntruesFUNC,
	nfalseFUNC,
	nfalsesFUNC,
	    // for any type returning array of that type
	arrayFUNC,
	    // for Double or DComplex array returning Bool
	isnanFUNC,
	    // for any array returning Bool scalar
	isdefFUNC,
	    // for any array returning Double scalar
	ndimFUNC,
	nelemFUNC,
	    // for any array returning Double array
	shapeFUNC,
            // for String
	strlengthFUNC,         //# returning Double
	upcaseFUNC,            //# returning String
	downcaseFUNC,          //# returning String
	trimFUNC,              //# returning String
	regexFUNC,             //# returning Regex
	patternFUNC,           //# returning Regex
	sqlpatternFUNC,        //# returning Regex
            // for Date
	datetimeFUNC,          //# returning Date
	mjdtodateFUNC,         //# returning Date
	mjdFUNC,               //# returning Double
	dateFUNC,              //# returning Date
	timeFUNC,              //# returning Double (in radians)
	yearFUNC,              //# returning Double
	monthFUNC,             //# returning Double
	dayFUNC,               //# returning Double
	cmonthFUNC,            //# returning String
	weekdayFUNC,           //# returning Double
	cdowFUNC,              //# returning String
	weekFUNC,              //# returning Double
	    // special function to select on a random number
	randFUNC,
            // special function to select on row number
	rownrFUNC,
            // special function to return row id (meant for GIVING)
	rowidFUNC,
            // special function resembling if statement
	iifFUNC,
	    // other functions, implemented in derived class
	conesFUNC,
	cones3FUNC,
	anyconeFUNC,
	anycone3FUNC,
	findconeFUNC,
	findcone3FUNC,
	NRFUNC      //# should be last
	};

    // Constructor
    TableExprFuncNode (FunctionType, NodeDataType, ValueType,
		       const TableExprNodeSet& source);

    // Destructor
    ~TableExprFuncNode ();

    // 'get' Functions to get the desired result of a function
    // <group>
    Bool     getBool     (const TableExprId& id);
    Double   getDouble   (const TableExprId& id);
    DComplex getDComplex (const TableExprId& id);
    String   getString   (const TableExprId& id);
    Regex    getRegex    (const TableExprId& id);
    MVTime   getDate     (const TableExprId& id);
    // </group>

    // Check the data and value types of the operands.
    // It sets the exptected data and value types of the operands.
    // Set the value type of the function result and returns
    // the data type of the function result.
    static NodeDataType checkOperands (Block<Int>& dtypeOper,
				       ValueType& resVT,
				       Block<Int>& vtypeOper,
				       FunctionType,
				       PtrBlock<TableExprNodeRep*>&);

    // Fill the result unit in the node.
    // Adapt the children nodes if their units need to be converted.
    static void fillUnits (TableExprNodeRep* node,
			   PtrBlock<TableExprNodeRep*>& nodes,
			   FunctionType func);

    // Link the children to the node and convert the children
    // to constants if possible. Also convert the node to
    // constant if possible.
    static TableExprNodeRep* fillNode (TableExprFuncNode* thisNode,
				       PtrBlock<TableExprNodeRep*>& nodes,
				       const Block<Int>& dtypeOper);

    // Link the children to the node and convert the children
    // to constants if possible.
    static void fillChildNodes (TableExprFuncNode* thisNode,
				PtrBlock<TableExprNodeRep*>& nodes,
				const Block<Int>& dtypeOper);

    // Fill the unit of the node for the functions with a predefined unit
    // (like asin).
    static void fillUnit (TableExprNodeRep& node, FunctionType func);

    // Some functions to be used by TableExprNodeFuncArray.
    // <group>
    PtrBlock<TableExprNodeRep*> operands()
        { return operands_p; }
    FunctionType funcType() const
        { return funcType_p; }
    NodeDataType argDataType() const
        { return argDataType_p; }
    // </group>

private:
    // Try if the function gives a constant result.
    // If so, set the expression type to Constant.
    void tryToConst();

    // Make the units of nodes from <src>starg</src> till <src>endarg</src>
    // equal. Return the unit found.
    static const Unit& makeEqualUnits (PtrBlock<TableExprNodeRep*>& nodes,
				       uInt starg, uInt endarg);


    FunctionType funcType_p;        // which function
    NodeDataType argDataType_p;     // common argument data type
};




} //# NAMESPACE CASA - END

#endif
