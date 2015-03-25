//# MSScanParse.h: Classes to hold results from scan grammar parser
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

#ifndef MS_MSSCANPARSE_H
#define MS_MSSCANPARSE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/ms/MSSel/MSParse.h>
#include <casacore/measures/Measures/MEpoch.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations

// <summary>
// Class to hold values from scan grammar parser
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
// </prerequisite>

// <etymology>
// MSScanParse is the class used to parse a scan command.
// </etymology>

// <synopsis>
// MSScanParse is used by the parser of scan sub-expression statements.
// The parser is written in Bison and Flex in files MSScanGram.y and .l.
// The statements in there use the routines in this file to act
// upon a reduced rule.
// Since multiple tables can be given (with a shorthand), the table
// names are stored in a list. The variable names can be qualified
// by the table name and will be looked up in the appropriate table.
//
// The class MSScanParse only contains information about a table
// used in the table command. Global variables (like a list and a vector)
// are used in MSScanParse.cc to hold further information.
//
// Global functions are used to operate on the information.
// The main function is the global function msScanCommand.
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


class MSScanParse : public MSParse
{

public:
  // Default constructor
  MSScanParse ();
  
  // Associate the ms and the shorthand.
  MSScanParse (const MeasurementSet* ms);
  MSScanParse (const MeasurementSet* ms, const TableExprNode& colAsTEN);
  ~MSScanParse() {columnAsTEN_p=TableExprNode();}

  const TableExprNode *selectRangeGTAndLT(const Int& n0, const Int& n1);
  const TableExprNode *selectRangeGEAndLE(const Int& n0, const Int& n1);
  const TableExprNode *selectScanIds(const Vector<Int>& scanids);
  inline const TableExprNode *selectScanIds() {return selectScanIds(parsedIDList_p);}
  const TableExprNode *selectScanIdsGT(const Vector<Int>& scanids);
  const TableExprNode *selectScanIdsLT(const Vector<Int>& scanids);
  const TableExprNode *selectScanIdsGTEQ(const Vector<Int>& scanids);
  const TableExprNode *selectScanIdsLTEQ(const Vector<Int>& scanids);
  std::vector<Int>& accumulateIDs(const Int id0, const Int id1=-1);

    // Get table expression node object.
  const TableExprNode node();

  Vector<Int> selectedIDs() {return idList;}
  void reset(){idList.resize(0);parsedIDList_p.resize(0);}
  void cleanup() {}

  void setMaxScan(const Int& n) {maxScans_p=n;}

  static MSScanParse* thisMSSParser;

private:
  TableExprNode node_p;
  Vector<Int> idList;
  std::vector<Int> parsedIDList_p;
  const String colName;
  void appendToIDList(const Vector<Int>& v);
  Int maxScans_p;
  static TableExprNode columnAsTEN_p;
};

} //# NAMESPACE CASACORE - END

#endif
