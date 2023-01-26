//# MSTimeParse.h: Classes to hold results from time grammar parser
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

#ifndef MS_MSTIMEPARSE_H
#define MS_MSTIMEPARSE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/ms/MSSel/MSParse.h>
#include <casacore/ms/MSSel/MSSelectableMainColumn.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/ms/MSSel/MSTimeDefinitions.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Arrays/Matrix.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations

// <summary>
// Class to hold values from time grammar parser
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
// </prerequisite>

// <etymology>
// MSTimeParse is the class used to parse a time command.
// </etymology>

// <synopsis>
// MSTimeParse is used by the parser of time sub-expression statements.
// The parser is written in Bison and Flex in files MSTimeGram.y and .l.
// The statements in there use the routines in this file to act
// upon a reduced rule.
// Since multiple tables can be given (with a shorthand), the table
// names are stored in a list. The variable names can be qualified
// by the table name and will be looked up in the appropriate table.
//
// The class MSTimeParse only contains information about a table
// used in the table command. Global variables (like a list and a vector)
// are used in MSTimeParse.cc to hold further information.
//
// Global functions are used to operate on the information.
// The main function is the global function msTimeCommand.
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

class MSTimeParse : public MSParse
{

public:
  // Default constructor
  MSTimeParse ();

  // Associate the ms and the shorthand.
  MSTimeParse (const MeasurementSet* ms,const TableExprNode& otherTens,const bool honourRowFlags=true);
  MSTimeParse (const MeasurementSet* ms,const TableExprNode& colAsTEN,
	       MSSelectableMainColumn& msMainColInterface,
	       const TableExprNode& otherTEN,
	       const bool honourRowFlags=true);
  ~MSTimeParse() {columnAsTEN_p=TableExprNode();}

//   ~MSTimeParse() 
//   {
//     if (node_p) delete node_p;node_p=0x0;
//     if (otherTens_p) delete otherTens_p;otherTens_p=0x0;
//   }

  const TableExprNode *selectTime(const MEpoch& time,
				  bool daytime = false);
  const TableExprNode *selectTimeGT(const MEpoch& lowboundTime,
				    bool daytime = false);
  const TableExprNode *selectTimeLT(const MEpoch& upboundTime,
				    bool daytime = false);
  const TableExprNode *selectTimeRange(const MEpoch& lowboundTime, 
				       const MEpoch& upboundTime,
				       bool daytime = false,
                                       float edgeWidth=-1.0);
  Matrix<double> selectedTimes() {return timeList;}
  const TableExprNode *addCondition(TableExprNode& condition);

  /*
  static const MEpoch *dayTimeConvert(int32_t day=-1, int32_t hour = -1,
				      int32_t minute = -1, int32_t second = -1,
				      int32_t millisec = -1);
  */

  static void setDefaults(TimeFields& tf, bool dataOrigin=true);
  void getDefaults();
  static void copyDefaults(TimeFields& target, TimeFields& source);
  static const MEpoch *yearTimeConvert(int32_t year=-1, int32_t month=-1, int32_t day=-1,
				       int32_t hour = -1, int32_t minute = -1,
				       int32_t second = -1, int32_t millisec = -1);
  static const MEpoch *yearTimeConvert(const TimeFields& tf);

  // Get table expression node object.
  static const TableExprNode* node();

  int32_t year0() {return defaultYear;}
  int32_t month0() {return defaultMonth;}
  int32_t day0() {return defaultDay;}
  int32_t hour0() {return defaultHour;}
  int32_t minute0() {return defaultMinute;}
  int32_t second0() {return defaultSeconds;}
  int32_t fractionalsec0() {return defaultFractionalSec;}
  double defaultInteg() {return defaultExposure;}

  static void validate(const TimeFields& tf);
  static void reset(){timeList.resize(3,0);}
  static void cleanup() {if (node_p) delete node_p;node_p=0x0;}

  static TableExprNode* node_p;
  //private:
  
  static TableExprNode *otherTens_p;
  static bool defaultTimeComputed;
  MVTime firstRowTime;
  static MeasurementSet *ms_p;
  static double toTAIInSec(const MEpoch& time);
  static MEpoch* yeartime;
  static MEpoch* daytime;
  int32_t defaultYear, defaultMonth, defaultDay,
    defaultHour, defaultMinute, defaultSeconds, defaultFractionalSec;
  double defaultExposure;
  const String colName;
  bool honourRowFlags_p;
  static Matrix<double> timeList;
  void accumulateTimeList(const double t0, const double t1,const double dT=-1);
  static MSTimeParse *thisMSTParser;
  static TableExprNode columnAsTEN_p;
  static MSSelectableMainColumn *mainColumn_p;
};

} //# NAMESPACE CASACORE - END

#endif
