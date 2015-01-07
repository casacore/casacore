//# TaQL.h: The TaQL module - Casacore data querying
//# Copyright (C) 1994-2010
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
//# $Id: Tables.h 21434 2014-05-07 13:07:20Z gervandiepen $

#ifndef TABLES_TAQL_H
#define TABLES_TAQL_H

//# Includes
//#   table expressions (for selection of rows)
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/tables/TaQL/TableParse.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <module>

// <summary>
// TaQL is the query language for Casacore tables
// </summary>

// <use visibility=export>

// <reviewed reviewer="jhorstko" date="1994/08/30" tests="" demos="">
// </reviewed>

// <prerequisite>
//    <li> <linkto module="Tables:description">Tables</linkto> module
// </prerequisite>

// <etymology>
// "TaQL" is the Table Query Language. Its pronounciation rhymes with bagel.
// </etymology>

// <synopsis> 
// TaQL is an SQL-like language to query a Casacore table.
// Amongst its options are row select, sort, update, and delete.
// <br>Some more information is given in the description of the
// <linkto module="Tables:select and sort">Tables module</linkto>.
// A detailed description is given in <A HREF="../notes/199.html">note 199</A>.
// </synopsis>
// </module>


} //# NAMESPACE CASACORE - END

#endif
