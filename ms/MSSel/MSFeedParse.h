//# MSFeedParse.h: Classes to hold results from feed grammar parser
//# Copyright (C) 2015
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

#ifndef MS_MSFEEDPARSE_H
#define MS_MSFEEDPARSE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/ms/MSSel/MSParse.h>
#include <casacore/ms/MSSel/MSSelectionErrorHandler.h>
#include <casacore/casa/Arrays/Matrix.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN
  
  //# Forward Declarations
  
  // <summary>
  // Class to hold values from feed grammar parser
  // </summary>
  
  // <use visibility=local>
  
  // <reviewed reviewer="" date="" tests="">
  // </reviewed>
  
  // <prerequisite>
  //# Classes you should understand before using this one.
  // </prerequisite>
  
  // <etymology>
  // MSFeedParse is the class used to parse a feed command.
  // </etymology>
  
  // <synopsis>
  // MSFeedParse is used by the parser of feed sub-expression statements.
  // The parser is written in Bison and Flex in files MSFeedGram.yy and .ll.
  // The statements there use the routines in this file to act
  // upon a reduced rule.
  // Since multiple tables can be given (with a shorthand), the table
  // names are stored in a list. The variable names can be qualified
  // by the table name and will be looked up in the appropriate table.
  //
  // The class MSFeedParse only contains information about a table
  // used in the table command. Global variables (like a list and a vector)
  // are used in MSFeedParse.cc to hold further information.
  //
  // Global functions are used to operate on the information.
  // The main function is the global function msFeedCommand.
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
  
  
  class MSFeedParse : public MSParse
  {
    
  public:
    // Define the operator types (&&&, &&, and &).
    // NB: Keeping the same notation as Antenna parser, even tho not a baseline here!
    enum BaselineListType {AutoCorrOnly=0, AutoCorrAlso, CrossOnly};

    // Default constructor
    MSFeedParse();
    
    // Associate the ms.
    MSFeedParse (const MeasurementSet* ms);

    MSFeedParse (const MSFeed& feedSubTable, 
		    const TableExprNode& feed1AsTEN, const TableExprNode& feed2AsTEN);

    ~MSFeedParse() {column1AsTEN_p=TableExprNode();column2AsTEN_p=TableExprNode();}

    // Add the given feed selection.
    const TableExprNode* selectFeedIds(const Vector<int32_t>& feedIds, 
					  BaselineListType baselineType=CrossOnly,
                                          bool negate=false);

    // Add the given "baseline" selection.
    const TableExprNode* selectFeedIds(const Vector<int32_t>& feedIds1,
                      const Vector<int32_t>& feedIds2, 
					  BaselineListType baselineType=CrossOnly,
                      bool negate=false);

    // Get a pointer to the table expression node object.
    TableExprNode node() const
      { return node_p; }
    const Vector<int32_t>& selectedFeed1() const
      { return feed1List; }
    const Vector<int32_t>& selectedFeed2() const
      { return feed2List; }
    const Matrix<int32_t>& selectedFeedPairs() const
      { return feedPairList; }

    MSFeed& subTable() {return msSubTable_p;}

  private:

    const TableExprNode* setTEN(TableExprNode& condition, 
                                BaselineListType baselineType=CrossOnly,
                                bool negate=false);
    void makeFeedPairList(const Vector<int32_t>&f1, const Vector<int32_t>&f2, Matrix<int32_t>&fp, 
			  BaselineListType baselineType=CrossOnly,
			  bool negate=false);
    void makeFeedList(Vector<int32_t>& feedList,const Vector<int32_t>& thisList,
                         bool negate=false);
    bool addFeedPair(const Matrix<int32_t>& feedpairlist,
                     const int32_t feed1, const int32_t feed2, 
 		     BaselineListType baselineType=CrossOnly);

    //# Data members.
  public:
    static MSFeedParse* thisMSFParser;
    static CountedPtr<MSSelectionErrorHandler> thisMSFErrorHandler;
    static void cleanupErrorHandler() {thisMSFErrorHandler.reset();}
  private:
    TableExprNode node_p;
    const String colName1, colName2;
    Vector<int32_t> feed1List, feed2List;
    Matrix<int32_t> feedPairList;
    MSFeed msSubTable_p;
    static TableExprNode column1AsTEN_p,column2AsTEN_p;
  };
  
} //# NAMESPACE CASACORE - END

#endif
