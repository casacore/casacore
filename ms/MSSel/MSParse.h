//# MSParse.h: Classes to hold results from an ms grammar parser
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

#ifndef MS_MSPARSE_H
#define MS_MSPARSE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>

#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MSSel/MSSelectableTable.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class AipsIO;


// <summary>
// Class to hold values from an ms grammar parser
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
// </prerequisite>

// <etymology>
// MSParse is the class used to parse an ms command.
// </etymology>

// <synopsis>
// MSParse is used by the parser of an ms sub-expression statements.
// The parser is written in Bison and Flex in files MSXXXGram.y and .l.
// The statements in there use the routines in this file to act
// upon a reduced rule.
// Since multiple tables can be given (with a shorthand), the table
// names are stored in a list. The variable names can be qualified
// by the table name and will be looked up in the appropriate table.
//
// The class MSParse only contains information about an ms
// used in the ms command. Global variables (like a list and a vector)
// are used in MSParse.cc to hold further information.
//
// Global functions are used to operate on the information.
// The main function is the global function msXXXCommand.
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


class MSParse
{
// Dummy AipsIO routines; they are needed for the List container.
// <group>
friend AipsIO& operator<< (AipsIO&, const MSParse&);
friend AipsIO& operator>> (AipsIO&, MSParse&);
// </group>

public:
    // Default constructor for List container class.
    MSParse ();

    // Copy constructor (copy semantics).
    MSParse (const MSParse&);

    ~MSParse ();

    // Assignment (copy semantics).
    MSParse& operator= (const MSParse&);

    // Associate the ms and the shorthand.
    MSParse (const MeasurementSet* ms, const String& shorthand);
    // Associate the ms and the shorthand.
    MSParse (const MSSelectableTable* ms, const String& shorthand);

    // Test if shorthand matches.
    Bool test (const String& shortHand) const;

    // Get the shorthand.
    String& shorthand();

    // Get ms object.
    MeasurementSet* ms();
    // Get ms object.
    MSSelectableTable* msInterface();

  void setMS(MeasurementSet* ms) {ms_p=ms;}
  void setMSInterface(MSSelectableTable* msI) {msInterface_p = msI;}
  static MeasurementSet *ms_p;
  static MSSelectableTable *msInterface_p;
  void addCondition(TableExprNode& target, TableExprNode& source);

private:
  String shorthand_p;
  // The following exists for the period we make the transition from
  // using MS to using MSSelectableTable.  Till then, both interfaces
  // have to be supported.
  MSSelectableTable *tempMSInterface_p;
};

} //# NAMESPACE CASACORE - END

#endif
