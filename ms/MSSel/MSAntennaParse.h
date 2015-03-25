//# MSAntennaParse.h: Classes to hold results from antenna grammar parser
//# Copyright (C) 1994,1995,1997,1998,1999,2000,2001,2003
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

#ifndef MS_MSANTENNAPARSE_H
#define MS_MSANTENNAPARSE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/ms/MSSel/MSParse.h>
#include <casacore/ms/MSSel/MSSelectionErrorHandler.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <bitset>
namespace casacore { //# NAMESPACE CASACORE - BEGIN
  
  //# Forward Declarations
  
  // <summary>
  // Class to hold values from antenna grammar parser
  // </summary>
  
  // <use visibility=local>
  
  // <reviewed reviewer="" date="" tests="">
  // </reviewed>
  
  // <prerequisite>
  //# Classes you should understand before using this one.
  // </prerequisite>
  
  // <etymology>
  // MSAntennaParse is the class used to parse a antenna command.
  // </etymology>
  
  // <synopsis>
  // MSAntennaParse is used by the parser of antenna sub-expression statements.
  // The parser is written in Bison and Flex in files MSAntennaGram.y and .l.
  // The statements in there use the routines in this file to act
  // upon a reduced rule.
  // Since multiple tables can be given (with a shorthand), the table
  // names are stored in a list. The variable names can be qualified
  // by the table name and will be looked up in the appropriate table.
  //
  // The class MSAntennaParse only contains information about a table
  // used in the table command. Global variables (like a list and a vector)
  // are used in MSAntennaParse.cc to hold further information.
  //
  // Global functions are used to operate on the information.
  // The main function is the global function msAntennaCommand.
  // It executes the given STaQL command and returns the resulting ms.
  // This is, in fact, the only function to be used by a user.
  // </synopsis>
  
  // <motivation>
  // It is necessary to be able to give a ms command in ASCII.
  // This can be used in a CLI or in the table browser to get a subset
  // of a table or to sort a table.
  // </motivation>
  
  //# <todo asof="$DATE:$">
  //# A List of bugs, limitations, extensions or planned refinements.
  //# </todo>
  
  
  class MSAntennaParse : public MSParse
  {
    
  public:
    // Define the operator types (&&&, &&, and &).
    enum BaselineListType {AutoCorrOnly=0, AutoCorrAlso, CrossOnly};
    enum ComplexityLevels {RESET=0,ANTREGEX,ANTLIST,STATIONREGEX, STATIONLIST, ANTATSTATIONLIST, BASELINELIST,HIGHESTLEVEL};

    // Default constructor
    MSAntennaParse();
    
    // Associate the ms.
    MSAntennaParse (const MeasurementSet* ms);

    MSAntennaParse (const MSAntenna& antSubTable, 
		    const TableExprNode& ant1AsTEN, const TableExprNode& ant2AsTEN);

    ~MSAntennaParse() {column1AsTEN_p=TableExprNode();column2AsTEN_p=TableExprNode();}

    // Add the given antennae selection.
    const TableExprNode* selectAntennaIds(const Vector<Int>& antennaIds, 
					  BaselineListType baselineType=CrossOnly,
                                          Bool negate=False);

    // Add the given baseline selection.
    const TableExprNode* selectAntennaIds(const Vector<Int>& antennaIds1,
                                          const Vector<Int>& antennaIds2, 
					  BaselineListType baselineType=CrossOnly,
                                          Bool negate=False);

    // Select by name or station number.
    const TableExprNode* selectNameOrStation(const Vector<String>& antenna,
 					     BaselineListType baselineType=CrossOnly,
                                             Bool negate=False);
    const TableExprNode* selectNameOrStation(const Vector<String>& antenna1,
                                             const Vector<String>& antenna2, 
 					     BaselineListType baselineType=CrossOnly,
                                             Bool negate=False);

    const TableExprNode* selectNameOrStation(const String& antenna1,
                                             const String& antenna2, 
 					     BaselineListType baselineType=CrossOnly,
                                             Bool negate=False);
    
    // Selection on baseline length
    const TableExprNode* selectLength(const std::vector<double>& lengths,
                                      Bool negate=False);

    // Get a pointer to the table expression node object.
    TableExprNode node() const
      { return node_p; }
    const Vector<Int>& selectedAnt1() const
      { return ant1List; }
    const Vector<Int>& selectedAnt2() const
      { return ant2List; }
    const Matrix<Int>& selectedBaselines() const
      { return baselineList; }

    // Get the factor to convert the given unit to m.
    static double getUnitFactor (const char* unit);
    
    void setComplexity(const ComplexityLevels& level=RESET) 
    {if (level==RESET) complexity.reset(); else complexity.set(level,True);}
    std::bitset<HIGHESTLEVEL> getComplexity() {return complexity;}
    MSAntenna& subTable() {return msSubTable_p;}
  private:
    const TableExprNode* setTEN(TableExprNode& condition, 
                                BaselineListType baselineType=CrossOnly,
                                Bool negate=False);
    Matrix<double> getBaselineLengths();
    void makeBaselineList(const Vector<Int>&a1, const Vector<Int>&a2, Matrix<Int>&b, 
			  BaselineListType baselineType=CrossOnly,
			  Bool negate=False);
    void makeAntennaList(Vector<Int>& antList,const Vector<Int>& thisList,
                         Bool negate=False);
    Bool addBaseline(const Matrix<Int>& baselist,
                     const Int ant1, const Int ant2, 
 		     BaselineListType baselineType=CrossOnly);

    //# Data members.
  public:
    static MSAntennaParse* thisMSAParser;
    static MSSelectionErrorHandler* thisMSAErrorHandler;
    std::bitset<HIGHESTLEVEL> complexity;
  private:
    TableExprNode node_p;
    const String colName1, colName2;
    Vector<Int> ant1List, ant2List;
    Matrix<Int> baselineList;
    MSAntenna msSubTable_p;
    static TableExprNode column1AsTEN_p,column2AsTEN_p;
  };
  
} //# NAMESPACE CASACORE - END

#endif
