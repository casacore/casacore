//# LogFilterExpr.h: Class to deal with a TaQL expression to filter messages
//# Copyright (C) 2000
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#ifndef TABLES_LOGFILTEREXPR_H
#define TABLES_LOGFILTEREXPR_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/TableExprData.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class TableExprNode;
class LogMessage;


// <summary>
// Class to deal with a TaQL expression to filter messages.
// </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// This program tests the class TableExprData.
// This example shows how a data set consisting of two vectors
// of scalars can be used.


class LogFilterExpr : public TableExprData
{
public:
  // Construct it from an expression which gets parsed.
  LogFilterExpr (const String& expr);

  // Copy constructor (copy semantics).
  LogFilterExpr (const LogFilterExpr&);

  virtual ~LogFilterExpr();

  // Assignment (copy semantics).
  LogFilterExpr& operator= (const LogFilterExpr&);

  // Does this message match the expression?
  Bool matches (const LogMessage& message);

  // Get the data.
  // <group>
  virtual Double getDouble (const Block<Int>& fieldNrs) const;
  virtual String getString (const Block<Int>& fieldNrs) const;
  // </group>

  // Get the data type of the various values.
  virtual DataType dataType (const Block<Int>& fieldNrs) const;

private:
  TableExprNode*    itsExpr;
  const LogMessage* itsMessage;
};



} //# NAMESPACE CASACORE - END

#endif
