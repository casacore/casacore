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
//# $Id: ExprFuncNode.h 21277 2012-10-31 16:07:31Z gervandiepen $

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
            // for Int, Double or DComplex returning Double
	argFUNC,          //# 23
            // for Int, Double, DComplex, Bool or String returning Double
	realFUNC,         //# 24
            // for Double or DComplex returning Double
	imagFUNC,         //# 25
            // for Int, Double, Bool or String returning Int (using floor)
        intFUNC,          //# 26
            // for Int, Double or Complex returning Double or Complex
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
            // for DComplex or String returning DComplex
	complexFUNC,      //# 38
	    // for Int, Double or Complex array returning the same
	arrsumFUNC,       //# 39
	arrsumsFUNC,      //# 40
        runsumFUNC,       //# 41
        boxsumFUNC,       //# 42
	arrproductFUNC,   //# 43
	arrproductsFUNC,  //# 44
        runproductFUNC,   //# 45
        boxproductFUNC,   //# 46
	arrsumsqrFUNC,    //# 47
	arrsumsqrsFUNC,   //# 48
        runsumsqrFUNC,    //# 49
        boxsumsqrFUNC,    //# 50
	    // for Int or Double array returning Int or Double
	arrminFUNC,       //# 51
	arrminsFUNC,      //# 52
	runminFUNC,       //# 53
	boxminFUNC,       //# 54
	arrmaxFUNC,       //# 55
	arrmaxsFUNC,      //# 56
	runmaxFUNC,       //# 57
	boxmaxFUNC,       //# 58
	    // for Int or Double array returning Double
	arrmeanFUNC,      //# 59
	arrmeansFUNC,     //# 60
	runmeanFUNC,      //# 61
	boxmeanFUNC,      //# 62
	arrvariance0FUNC, //# 63
	arrvariances0FUNC,//# 64
	runvariance0FUNC, //# 65
	boxvariance0FUNC, //# 66
	arrvariance1FUNC, //# 67
	arrvariances1FUNC,//# 68
	runvariance1FUNC, //# 69
	boxvariance1FUNC, //# 70
	arrstddev0FUNC,   //# 71
	arrstddevs0FUNC,  //# 72
	runstddev0FUNC,   //# 73
	boxstddev0FUNC,   //# 74
	arrstddev1FUNC,   //# 75
	arrstddevs1FUNC,  //# 76
	runstddev1FUNC,   //# 77
	boxstddev1FUNC,   //# 78
	arravdevFUNC,     //# 79
	arravdevsFUNC,    //# 80
	runavdevFUNC,     //# 81
	boxavdevFUNC,     //# 82
	arrrmsFUNC,       //# 83
	arrrmssFUNC,      //# 84
	runrmsFUNC,       //# 85
	boxrmsFUNC,       //# 86
	arrmedianFUNC,    //# 87
	arrmediansFUNC,   //# 88
	runmedianFUNC,    //# 89
	boxmedianFUNC,    //# 90
	arrfractileFUNC,  //# 91
	arrfractilesFUNC, //# 92
	runfractileFUNC,  //# 93
	boxfractileFUNC,  //# 94
	    // for Bool array returning Bool
        arranyFUNC,       //# 95
        arranysFUNC,      //# 96
        runanyFUNC,       //# 97
        boxanyFUNC,       //# 98
	arrallFUNC,       //# 99
	arrallsFUNC,      //# 100
	runallFUNC,       //# 101
	boxallFUNC,       //# 102
	    // for Bool array returning Int scalar
	arrntrueFUNC,     //# 103
	arrntruesFUNC,    //# 104
	runntrueFUNC,     //# 105
	boxntrueFUNC,     //# 106
	arrnfalseFUNC,    //# 107
	arrnfalsesFUNC,   //# 108
	runnfalseFUNC,    //# 109
	boxnfalseFUNC,    //# 110
	    // for any type returning array of that type
	arrayFUNC,        //# 111
	transposeFUNC,    //# 112
        areverseFUNC,     //# 113
        resizeFUNC,       //# 114
	diagonalFUNC,     //# 115
	    // for Int, Double or DComplex array returning Bool
	isnanFUNC,        //# 116
	isinfFUNC,        //# 117
        isfiniteFUNC,     //# 118
	    // for any array returning Bool scalar
	isdefFUNC,        //# 119
        isnullFUNC,       //# 120
        iscolFUNC,        //# 121
        iskeyFUNC,        //# 122
	    // for any array returning Int scalar
	ndimFUNC,         //# 123
	nelemFUNC,        //# 124
	    // for any array returning Int array
	shapeFUNC,        //# 125
            // for String
	strlengthFUNC,    //# 126          returning Int
	upcaseFUNC,       //# 127          returning String
	downcaseFUNC,     //# 128          returning String
	capitalizeFUNC,   //# 129          returning String
        sreverseFUNC,     //# 130
	trimFUNC,         //# 131          returning String
	ltrimFUNC,        //# 132          returning String
	rtrimFUNC,        //# 133          returning String
	substrFUNC,       //# 134          returning String
        replaceFUNC,      //# 135          returning String
	regexFUNC,        //# 136          returning TaqlRegex
	patternFUNC,      //# 137          returning TaqlRegex
	sqlpatternFUNC,   //# 138          returning TaqlRegex
            // for Date
	datetimeFUNC,     //# 139          returning Date
	mjdtodateFUNC,    //# 140          returning Date
	mjdFUNC,          //# 141          returning Double
	dateFUNC,         //# 142          returning Date
	timeFUNC,         //# 143          returning Double (in radians)
	yearFUNC,         //# 144          returning Int
	monthFUNC,        //# 145          returning Int
	dayFUNC,          //# 146          returning Int
	cmonthFUNC,       //# 147          returning String
	weekdayFUNC,      //# 148          returning Int
	cdowFUNC,         //# 149          returning String
	weekFUNC,         //# 150          returning Int
        ctodFUNC,         //# 151          returning String
        cdateFUNC,        //# 152          returning String
        ctimeFUNC,        //# 153          returning String
            // return values as strings
        stringFUNC,       //# 154
            // return angles as hms strings
        hmsFUNC,          //# 155
            // return angles as dms strings
        dmsFUNC,          //# 156
            // return angles as hms/dms strings
        hdmsFUNC,         //# 157
	    // special function returning a random Double number
	randFUNC,         //# 158
            // special function returning Int row number
	rownrFUNC,        //# 159
            // special function returning Int row id (meant for GIVING)
	rowidFUNC,        //# 160
            // special function resembling if statement
	iifFUNC,          //# 161
            // angular distance returning radians
        angdistFUNC,      //# 162
        angdistxFUNC,     //# 163
	    // cone search functions, implemented in derived class
	conesFUNC,        //# 164
	cones3FUNC,       //# 165
	anyconeFUNC,      //# 166
	anycone3FUNC,     //# 167
	findconeFUNC,     //# 168
	findcone3FUNC,    //# 169
            // for Int, Double, Complex or String returning Bool
        boolFUNC,         //# 170
            // masked array functions
        nullarrayFUNC,    //# 171
        marrayFUNC,       //# 172
        arrdataFUNC,      //# 173
        arrmaskFUNC,      //# 174
        negatemaskFUNC,   //# 175
        replmaskedFUNC,   //# 176
        replunmaskedFUNC, //# 178
        arrflatFUNC,      //# 179
        //# AGGREGATE functions must be the last ones.
        FirstAggrFunc,    //# 180
        countallFUNC = FirstAggrFunc,
        gcountFUNC,
        gfirstFUNC,
        glastFUNC,
        //# Grouping doing aggregation on the fly; reducing to a scalar per group
        gminFUNC,         //# 184
        gmaxFUNC,
        gsumFUNC,
        gproductFUNC,
        gsumsqrFUNC,
        gmeanFUNC,
        gvariance0FUNC,
        gvariance1FUNC,
        gstddev0FUNC,
        gstddev1FUNC,
        grmsFUNC,
        ganyFUNC,
        gallFUNC,
        gntrueFUNC,
        gnfalseFUNC,
        //# Grouping doing aggregation on the fly; reducing to an array per group
        FirstAggrArrayFunc,//# 199
        gminsFUNC = FirstAggrArrayFunc,
        gmaxsFUNC,
        gsumsFUNC,
        gproductsFUNC,
        gsumsqrsFUNC,
        gmeansFUNC,
        gvariances0FUNC,
        gvariances1FUNC,
        gstddevs0FUNC,
        gstddevs1FUNC,
        grmssFUNC,
        ganysFUNC,
        gallsFUNC,
        gntruesFUNC,
        gnfalsesFUNC,
        LastAggrArrayFunc, //# 214
        ghistFUNC = LastAggrArrayFunc,
        //# Grouping requiring aggregation of rows when getting result
        gaggrFUNC,         //# 215
        growidFUNC,
        gmedianFUNC,
        gfractileFUNC,
        gexpridFUNC,       //# special function (can be inserted by TableParse)
	NRFUNC             //# 220  should be last
	};

    // Constructor
    TableExprFuncNode (FunctionType, NodeDataType, ValueType,
		       const TableExprNodeSet& source,
                       const vector<TENShPtr>& nodes,
                       const Block<Int>& dtypeOper,
                       const Table& = Table());

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
				       std::vector<TENShPtr>&);

    // Fill the result unit in the node.
    // Adapt the children nodes if their units need to be converted.
    // It returns a possible scale factor in case result unit is SI (for sqrt).
    void fillUnits();

    // Link the children to the node and convert the children
    // to constants if possible.
    void fillChildNodes (const vector<TENShPtr>& nodes,
                         const Block<Int>& dtypeOper);

    // Get possible unit scale factor (needed for sqrt).
    Double getScale() const
        { return scale_p; }

    // Some functions to be used by TableExprNodeFuncArray.
    // <group>
    const std::vector<TENShPtr>& operands() const
        { return operands_p; }
    std::vector<TENShPtr>& rwOperands()
        { return operands_p; }
    FunctionType funcType() const
        { return funcType_p; }
    NodeDataType argDataType() const
        { return argDataType_p; }
    // </group>

    // Get the possible print format, width, and/or precision.
    static void getPrintFormat (String& fmt, Int& width, Int& prec,
                                const std::vector<TENShPtr>& operands,
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

    // Read a string as an integer, double, complex or bool.
    static Int64 string2Int (const String&);
    static Double string2Real (const String&);
    static DComplex string2Complex (const String&);
    static Bool string2Bool (const String&);

private:
    // Try if the function gives a constant result.
    // If so, set the expression type to Constant.
    void tryToConst();

    // Make the units of nodes from <src>starg</src> till <src>endarg</src>
    // equal. Return the unit found.
    static const Unit& makeEqualUnits (std::vector<TENShPtr>& nodes,
				       uInt starg, uInt endarg);

    //# Data members.
    FunctionType funcType_p;        // which function
    NodeDataType argDataType_p;     // common argument data type
    Double       scale_p;           // possible scaling for unit conversion
                                    // (needed for sqrt)
    Table        table_p;           // table (for iscolumn and iskeyword)
};


} //# NAMESPACE CASACORE - END

#endif
