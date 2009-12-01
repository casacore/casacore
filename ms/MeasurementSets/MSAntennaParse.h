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
#include <ms/MeasurementSets/MSParse.h>
#include <casa/Arrays/Matrix.h>
namespace casa { //# NAMESPACE CASA - BEGIN
  
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
    enum BaselineListType {AutoCorrOnly=0, AutoCorrAlso, CrossOnly};
    // Default constructor
    MSAntennaParse ();
    
    // Associate the ms and the shorthand.
    MSAntennaParse (const MeasurementSet* ms);
    
    //    ~MSAntennaParse() {if (node_p) delete node_p;node_p=0x0;};
    const TableExprNode* selectAntennaIds(const Vector<Int>& antennaIds, 
					  BaselineListType autoCorr=CrossOnly, Bool negate=False);
    const TableExprNode* selectAntennaIds(const Vector<Int>& antennaIds1,
                                          const Vector<Int>& antennaIds2, 
					  //  Bool autoCorr=False, Bool negate=False);
					  BaselineListType autoCorr=CrossOnly, Bool negate=False);
    
    const TableExprNode* selectNameOrStation(const Vector<String>& antenna,
// 					     Bool autoCorr=False, Bool negate=False);
 					     BaselineListType autoCorr=CrossOnly, Bool negate=False);
    const TableExprNode* selectNameOrStation(const Vector<String>& antenna1,
                                             const Vector<String>& antenna2, 
 					     BaselineListType autoCorr=CrossOnly, Bool negate=False);
// 					     Bool autoCorr=False, Bool negate=False);
    const TableExprNode* selectNameOrStation(const String& antenna1,
                                             const String& antenna2, 
 					     BaselineListType autoCorr=CrossOnly, Bool negate=False);
// 					     Bool autoCorr=False, Bool negate=False);
    
    // selection from antenna and CP
    const TableExprNode* selectFromIdsAndCPs(const Int index, const String& cp);
    const TableExprNode* selectFromIdsAndCPs(const Int firstIndex, const String& firstcp, 
					     const Int secondIndex, const String& secondcp);
    // Get table expression node object.
    static const TableExprNode* node();
    static MSAntennaParse* thisMSAParser;
    static Vector<Int> selectedAnt1() {return ant1List;};
    static Vector<Int> selectedAnt2() {return ant2List;};
    static Matrix<Int> selectedBaselines() {return baselineList;};
    static void reset(){ant1List.resize(0);ant2List.resize(0);baselineList.resize(0,2);};
    static void cleanup() {if (node_p) delete node_p;node_p=0x0;};
  private:
    
    static TableExprNode* node_p;
    const String colName1, colName2;
//     void setTEN(TableExprNode& condition, Bool autoCorr=False, Bool negate=False);
     void setTEN(TableExprNode& condition, 
		 BaselineListType autoCorr=CrossOnly,
		 Bool negate=False);
    void makeBaselineList(const Vector<Int>&a1, const Vector<Int>&a2, Matrix<Int>&b, 
			  //			  Bool autoCorr=False, 
			  BaselineListType autoCorr=CrossOnly,
			  Bool negate=False);
    void makeAntennaList(Vector<Int>& antList,const Vector<Int>& thisList, Bool negate=False);
    Bool addBaseline(const Matrix<Int>& baselist, const Int ant1, const Int ant2, 
 		     BaselineListType autoCorr=CrossOnly);
// 		     Bool autoCorr=False);
    static  Vector<Int> ant1List, ant2List;
    static Matrix<Int> baselineList;
  };
  
} //# NAMESPACE CASA - END

#endif
