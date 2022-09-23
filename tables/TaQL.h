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
  //
  // The high-level interface is using a TaQL command as described in
  // <A HREF="../notes/199.html">note 199</A>. Such a command can be given
  // in C++ (using TableParse.h), Python or the shell program 'taql'.
  // The code for parsing and executing TaQL commands is quite complex.
  // Processing a command consists of two steps.
  // <ol>
  //  <li>
  //  First a command is parsed using 'flex' and 'bison'. The file TableGram.ll
  //  is used by 'flex' to recognize the tokens in the command. The file
  //  TableGram.yy defines the grammar which is used by bison to invoke
  //  actions on the recognized parts of the command. These actions
  //  consist of building a parse tree by means of class TaQLNode and
  //  associated classes. In this way the command is syntactically checked.
  //  <li>
  //  If the parsing is done successfully, the command is executed
  //  by walking through the parse tree using class TaQLNodeHandler.
  //  In its turn that class invokes functions in class TableParseQuery
  //  to check (semantically) and execute commands such as SELECT, UPDATE, etc..
  //  Note that subqueries are executed before the entire parse tree has
  //  been walked through, thus before possible later semantic errors are
  //  detected.
  //  <br>Expressions in the parse tree are converted to expression trees
  //  using the various TableExprNode classes. Expression trees are evaluated
  //  for each row in a table. Note that expressions can be used in many
  //  parts of a TaQL command.
  //  Functions in a command are handled by TableParseFunc.
  // </ol>
  //
  // Expression trees can also be generated directly in C++ using class
  // TableExprNode which is overloaded for many operators and functions
  // (such as sin, max, etc.). In fact, TaQLNodeHandler uses this code.
  // For example:
  // <example>
  // <srcblock>
  //    Table tab("my.ms");
  //    Table selection (tab(tab.col("ANTENNA1") == tab.col("ANTENNA2")  &&
  //                         tab.col("SPECTRAL_WINDOW_ID") == 0));
  // </srcblock>
  //  creates a (reference) table containing the autocorrelations of the
  //  first spectral window in "my.ms".
  // </example>
  // </synopsis>
  // </module>

} //# NAMESPACE CASACORE - END

#endif
