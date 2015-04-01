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
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/ExprNodeRep.h>
#include <casacore/casa/Quanta/MVAngle.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

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
        piFUNC,           //# 0
	eFUNC,            //# 1
        cFUNC,            //# 2
	    // for Int, or Double or Complex returning Bool
	    // (2 is with default tolerance)
	near2FUNC,        //# 3
	near3FUNC,        //# 4
	nearabs2FUNC,     //# 5
	nearabs3FUNC,     //# 6
            // for Int, Double or DComplex returning Double or Complex
	sinFUNC,          //# 7
	sinhFUNC,         //# 8
	cosFUNC,          //# 9
	coshFUNC,         //# 10
	expFUNC,          //# 11
	logFUNC,          //# 12
	log10FUNC,        //# 13
	sqrtFUNC,         //# 14
	powFUNC,          //# 15
	conjFUNC,         //# 16
            // for Int, Double or DComplex returning Int, Double or Complex
	squareFUNC,       //# 17
	cubeFUNC,         //# 18
	minFUNC,          //# 19
	maxFUNC,          //# 20
            // for Int, Double or DComplex returning Int or Double
	normFUNC,         //# 21
	absFUNC,          //# 22
	argFUNC,          //# 23
            // for Int, Double or DComplex returning Double
	realFUNC,         //# 24
	imagFUNC,         //# 25
            // for Int or Double returning Int (using floor)
        intFUNC,          //# 26
            // for Int or Double returning Double
	asinFUNC,         //# 27
	acosFUNC,         //# 28
	atanFUNC,         //# 29
	atan2FUNC,        //# 30
	tanFUNC,          //# 31
	tanhFUNC,         //# 32
            // for Int or Double returning Int or Double
	signFUNC,         //# 33
	roundFUNC,        //# 34
	floorFUNC,        //# 35
	ceilFUNC,         //# 36
	fmodFUNC,         //# 37
            // for Int, Double or DComplex returning DComplex
	complexFUNC,      //# 38
	    // for Int, Double or Complex array returning the same
	arrsumFUNC,       //# 39
	arrsumsFUNC,      //# 40
	arrproductFUNC,   //# 41
	arrproductsFUNC,  //# 42
	arrsumsqrFUNC,    //# 43
	arrsumsqrsFUNC,   //# 44
	    // for Int or Double array returning Int or Double
	arrminFUNC,       //# 45
	arrminsFUNC,      //# 46
	runminFUNC,       //# 47
	boxminFUNC,       //# 48
	arrmaxFUNC,       //# 49
	arrmaxsFUNC,      //# 50
	runmaxFUNC,       //# 51
	boxmaxFUNC,       //# 52
	    // for Int or Double array returning Double
	arrmeanFUNC,      //# 53
	arrmeansFUNC,     //# 54
	runmeanFUNC,      //# 55
	boxmeanFUNC,      //# 56
	arrvarianceFUNC,  //# 57
	arrvariancesFUNC, //# 58
	runvarianceFUNC,  //# 59
	boxvarianceFUNC,  //# 60
	arrstddevFUNC,    //# 61
	arrstddevsFUNC,   //# 62
	runstddevFUNC,    //# 63
	boxstddevFUNC,    //# 64
	arravdevFUNC,     //# 65
	arravdevsFUNC,    //# 66
	runavdevFUNC,     //# 67
	boxavdevFUNC,     //# 68
	arrrmsFUNC,       //# 69
	arrrmssFUNC,      //# 70
	runrmsFUNC,       //# 71
	boxrmsFUNC,       //# 72
	arrmedianFUNC,    //# 73
	arrmediansFUNC,   //# 74
	runmedianFUNC,    //# 75
	boxmedianFUNC,    //# 76
	arrfractileFUNC,  //# 77
	arrfractilesFUNC, //# 78
	    // for Bool array returning Bool
        anyFUNC,          //# 79
        anysFUNC,         //# 80
        runanyFUNC,       //# 81
        boxanyFUNC,       //# 82
	allFUNC,          //# 83
	allsFUNC,         //# 84
	runallFUNC,       //# 85
	boxallFUNC,       //# 86
	    // for Bool array returning Int scalar
	ntrueFUNC,        //# 87
	ntruesFUNC,       //# 88
	nfalseFUNC,       //# 89
	nfalsesFUNC,      //# 90
	    // for any type returning array of that type
	arrayFUNC,        //# 91
	transposeFUNC,    //# 92
	    // for Int, Double or DComplex array returning Bool
	isnanFUNC,        //# 93
	isinfFUNC,        //# 94
        isfiniteFUNC,     //# 95
	    // for any array returning Bool scalar
	isdefFUNC,        //# 96
	    // for any array returning Int scalar
	ndimFUNC,         //# 97
	nelemFUNC,        //# 98
	    // for any array returning Int array
	shapeFUNC,        //# 99
            // for String
	strlengthFUNC,    //# 100          returning Int
	upcaseFUNC,       //# 101          returning String
	downcaseFUNC,     //# 102          returning String
	capitalizeFUNC,   //# 103          returning String
	trimFUNC,         //# 104          returning String
	ltrimFUNC,        //# 105          returning String
	rtrimFUNC,        //# 106          returning String
	substrFUNC,       //# 107          returning String
        replaceFUNC,      //# 108          returning String
	regexFUNC,        //# 109          returning TaqlRegex
	patternFUNC,      //# 110          returning TaqlRegex
	sqlpatternFUNC,   //# 111          returning TaqlRegex
            // for Date
	datetimeFUNC,     //# 112          returning Date
	mjdtodateFUNC,    //# 113          returning Date
	mjdFUNC,          //# 114          returning Double
	dateFUNC,         //# 115          returning Date
	timeFUNC,         //# 116          returning Double (in radians)
	yearFUNC,         //# 117          returning Int
	monthFUNC,        //# 118          returning Int
	dayFUNC,          //# 119          returning Int
	cmonthFUNC,       //# 120          returning String
	weekdayFUNC,      //# 121          returning Int
	cdowFUNC,         //# 122          returning String
	weekFUNC,         //# 123          returning Int
        ctodFUNC,         //# 124          returning String
        cdateFUNC,        //# 125          returning String
        ctimeFUNC,        //# 126          returning String
            // return values as strings
        stringFUNC,       //# 127
            // return angles as hms strings
        hmsFUNC,          //# 128
            // return angles as dms strings
        dmsFUNC,          //# 129
            // return angles as hms/dms strings
        hdmsFUNC,         //# 130
	    // special function returning a random Double number
	randFUNC,         //# 131
            // special function returning Int row number
	rownrFUNC,        //# 132
            // special function returning Int row id (meant for GIVING)
	rowidFUNC,        //# 133
            // special function resembling if statement
	iifFUNC,          //# 134
            // angular distance returning radians
        angdistFUNC,      //# 135
        angdistxFUNC,     //# 136
	    // other functions, implemented in derived class
	conesFUNC,        //# 137
	cones3FUNC,       //# 138
	anyconeFUNC,      //# 139
	anycone3FUNC,     //# 140
	findconeFUNC,     //# 141
	findcone3FUNC,    //# 142
        //# AGGREGATE functions must be the last ones.
        FirstAggrFunc,    //# 143
        countallFUNC = FirstAggrFunc,
        gcountFUNC,
        gfirstFUNC,
        glastFUNC,
        //# Grouping doing aggregation on the fly; reducing to a scalar per group
        gminFUNC,         //# 147
        gmaxFUNC,
        gsumFUNC,
        gproductFUNC,
        gsumsqrFUNC,
        gmeanFUNC,
        gvarianceFUNC,
        gstddevFUNC,
        grmsFUNC,
        ganyFUNC,
        gallFUNC,
        gntrueFUNC,
        gnfalseFUNC,
        ghistFUNC,
        //# Grouping requiring aggregation of rows when getting result
        gaggrFUNC,        //# 160
        growidFUNC,
        gmedianFUNC,
        gfractileFUNC,
        gexpridFUNC,      //# special function (can be inserted by TableParse)
	NRFUNC            //# should be last
	};

    // Constructor
    TableExprFuncNode (FunctionType, NodeDataType, ValueType,
		       const TableExprNodeSet& source);

    // Destructor
    ~TableExprFuncNode ();

    // 'get' Functions to get the desired result of a function
    // <group>
    Bool      getBool     (const TableExprId& id);
    Int64     getInt      (const TableExprId& id);
    Double    getDouble   (const TableExprId& id);
    DComplex  getDComplex (const TableExprId& id);
    String    getString   (const TableExprId& id);
    TaqlRegex getRegex    (const TableExprId& id);
    MVTime    getDate     (const TableExprId& id);
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
    // It returns a possible scale factor in case result unit is SI (for sqrt).
    static Double fillUnits (TableExprNodeRep* node,
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

    // Set unit scale factor (needed for sqrt).
    void setScale (Double scale)
        { scale_p = scale; }

    // Get possible unit scale factor (needed for sqrt).
    Double getScale() const
        { return scale_p; }

    // Some functions to be used by TableExprNodeFuncArray.
    // <group>
    const PtrBlock<TableExprNodeRep*>& operands() const
        { return operands_p; }
    PtrBlock<TableExprNodeRep*>& rwOperands()
        { return operands_p; }
    FunctionType funcType() const
        { return funcType_p; }
    NodeDataType argDataType() const
        { return argDataType_p; }
    // </group>

    // Get the possible print format, width, and/or precision.
    static void getPrintFormat (String& fmt, Int& width, Int& prec,
                                const PtrBlock<TableExprNodeRep*>& operands,
                                const TableExprId& id);

    // Convert the date and/or time to a string.
    // <group>
    static String stringDT (const MVTime& dt, Int prec, MVTime::formatTypes);
    static String stringDateTime (const MVTime& dt, Int prec);
    static String stringDate (const MVTime& dt);
    static String stringTime (const MVTime& dt, Int prec);
    // </group>

    // Convert a value to a string.
    // If <src>fmt</src> is empty, ostringstream is used.
    // Otherwise the printf-like format is used.
    // If possible, a double value is converted to radians if formatted as angle.
    // <group>
    static String stringValue (Bool val, const String& fmt, Int width);
    static String stringValue (Int64 val, const String& fmt, Int width);
    static String stringValue (Double val, const String& fmt,
                               Int width, Int prec,
                               const std::pair<int,int>& mvFormat,
                               const Unit& unit);
    static String stringValue (const DComplex& val, const String& fmt,
                               Int width, Int prec);
    static String stringValue (const String& val, const String& fmt,
                               Int width);
    static String stringValue (const MVTime& val, const String& fmt,
                               Int width,
                               const std::pair<int,int>& mvFormat);

    // Convert angle to a string (hms or dms).
    // <group>
    static String stringAngle (double val, Int prec,
                               MVAngle::formatTypes type);
    static String stringHMS (double val, Int prec);
    static String stringDMS (double val, Int prec);
    // </group>

    // Get the MVTime/Angle format and optional precision.
    // 0,0 is returned if empty or unknown format.
    static std::pair<int,int> getMVFormat (const String& fmt);

    // Get the angular distance between two positions on a sphere.
    static double angdist (double ra1, double dec1, double ra2, double dec2)
      { return acos (sin(dec1)*sin(dec2) + cos(dec1)*cos(dec2)*cos(ra1-ra2)); }

private:
    // Try if the function gives a constant result.
    // If so, set the expression type to Constant.
    void tryToConst();

    // Make the units of nodes from <src>starg</src> till <src>endarg</src>
    // equal. Return the unit found.
    static const Unit& makeEqualUnits (PtrBlock<TableExprNodeRep*>& nodes,
				       uInt starg, uInt endarg);

    //# Data members.
    FunctionType funcType_p;        // which function
    NodeDataType argDataType_p;     // common argument data type
    Double       scale_p;           // possible scaling for unit conversion
                                    // (needed for sqrt)
};


} //# NAMESPACE CASACORE - END

#endif
