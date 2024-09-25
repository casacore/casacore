//# tTableGramError.cc: This program tests erroneous table commands
//# Copyright (C) 2022
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include <casacore/tables/TaQL/TableParse.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

// Run a table command which should fail.
// Return 1 if not failing.
int errCmd (const String& cmd)
{
  cout << cmd << endl;
  try {
    tableCommand (cmd);
    cout << "  Above command succeeded, but was expected to fail" << endl;
    return 1;
  } catch (const AipsError& x) {
    cout << "  " << x.what() << endl;
  }
  return 0;
}


int main()
{
  // Start with creating the table to use in the tests.
  tableCommand ("CREATE TABLE tTableGramError_tmp.tab1 [cb bool, ci int, cd double [unit='Hz'], cs string, cc complex, ct date, cai int [shape=[3,4]]] limit 1");
  tableCommand ("CREATE TABLE tTableGramError_tmp.tab2 [cb bool, ci int, cd double, cs string, cc complex, ct date] limit 2");
  int n = 0;
  // Incomplete commands
  n += errCmd ("select from");
  n += errCmd ("select from tTableGramError_tmp.tab1 where");
  n += errCmd ("select from tTableGramError_tmp.tab1 where ci>1 groupby");
  n += errCmd ("select from tTableGramError_tmp.tab1 where ci>1 orderby");
  // Invalid clause
  n += errCmd ("select from tTableGramError_tmp.tab1 where ci>1 group ci");
  n += errCmd ("select from tTableGramError_tmp.tab1 where ci>1 order ci");
  n += errCmd ("select from tTableGramError_tmp.tab1 where ci>1 order ci groupby ci");
  // Shorthand already used or not defined
  n += errCmd ("select from tTableGramError_tmp.tab1 t1, tTableGramError_tmp.tab2 t1");
  n += errCmd ("select t2.ci from tTableGramError_tmp.tab1 t1");
  // Table does not exist
  n += errCmd ("select from tTableGramError_tmp.tabx");
  n += errCmd ("select from tTableGramError_tmp.tabx::SUBTAB");
  n += errCmd ("select from tTableGramError_tmp.tab1::SUBTAB");
  n += errCmd ("select from ['tTableGramError_tmp.tabx*']");
  n += errCmd ("select from $1");
  // Column does not exist
  n += errCmd ("select cx from tTableGramError_tmp.tab1");
  n += errCmd ("select from tTableGramError_tmp.tab1 where cx>1");
  n += errCmd ("select from tTableGramError_tmp.tab1 orderby cx");
  n += errCmd ("select from tTableGramError_tmp.tab1 groupby cx");
  n += errCmd ("create table a.b like tTableGramError_tmp.tab1 drop column cx");
  // Mismatching unit
  n += errCmd ("select cd kg from tTableGramError_tmp.tab1");
  n += errCmd ("select from tTableGramError_tmp.tab1 where cd>1kg");
  n += errCmd ("select from tTableGramError_tmp.tab1 orderby cd kg");
  n += errCmd ("select from tTableGramError_tmp.tab1 groupby cd kg");
  // Mismatching nr of rows
  n += errCmd ("select t1.cb, t2.ci from tTableGramError_tmp.tab1 t1, tTableGramError_tmp.tab2 t2");
  n += errCmd ("select from tTableGramError_tmp.tab1 t1, tTableGramError_tmp.tab2 t2 where t1.ci!=t2.ci");
  // Unknown shorthand or column
  n += errCmd ("select from tTableGramError_tmp.tab1 t1 where t2.ci=0");
  n += errCmd ("select from tTableGramError_tmp.tab1 t1 where t1.cx=0");
  // Invalid axes specification
  n += errCmd ("select means(cai,0,[1]) from tTableGramError_tmp.tab1");
  // Invalid function name
  n += errCmd ("select sqrtt(cai) from tTableGramError_tmp.tab1");
  cout << ">>>" << endl;   // error messages are system dependent
  n += errCmd ("select m.sqrtt(cai) from tTableGramError_tmp.tab1");  // libm exists
  n += errCmd ("select mxy.sqrtt(cai) from tTableGramError_tmp.tab1");
  cout << "<<<" << endl;
  // Invalid aggregation
  n += errCmd ("select gsum(gmin(ci)) from tTableGramError_tmp.tab1");
  n += errCmd ("select from tTableGramError_tmp.tab1 groupby ci having gsum(gmin(ci)>0");
  n += errCmd ("select from tTableGramError_tmp.tab1 where gsum(ci) > 0");
  n += errCmd ("select from tTableGramError_tmp.tab1 orderby gsum(ci)");
  n += errCmd ("select from tTableGramError_tmp.tab1 groupby gsum(ci)");
  // Mismatching data types
  n += errCmd ("select from tTableGramError_tmp.tab1 where ci>cb");
  n += errCmd ("select from tTableGramError_tmp.tab1 where ci+cs>0");
  n += errCmd ("select from tTableGramError_tmp.tab1 where cc+cs>0");
  n += errCmd ("select ct+cs from tTableGramError_tmp.tab1");
  n += errCmd ("calc date() + date()");
  // Invalid INSERT.
  n+= errCmd ("INSERT INTO tTableGramError_tmp.tab1 (ci,cd) VALUES (1,2),(1,2,3)");
  n+= errCmd ("INSERT INTO tTableGramError_tmp.tab1 (ci,cd) VALUES (1,2,3)");
  n+= errCmd ("INSERT INTO tTableGramError_tmp.tab1 (ci,cs) VALUES (1,2)");
  // Invalid multi-dim array or mask
  n += errCmd ("calc sum([[1,2],[3,4,5]])");
  n += errCmd ("calc sum([1,2][T,F])");
  cout << ">>>" << endl;
  n += errCmd ("calc sum([1,2][[T,F,F]])");  // gives system-dependent message
  cout << "<<<" << endl;
  // Invalid LIMIT
  n += errCmd ("select ci from tTableGramError_tmp.tab1 limit ::0");
  n += errCmd ("select ci from tTableGramError_tmp.tab1 limit ci");
  n += errCmd ("select ci limit -1");
  n += errCmd ("select ci offset -1");
  // Invalid GIVING (cannot use array).
  n += errCmd ("select ci from tTableGramError_tmp.tab1 where ci>max([select from tTableGramError_tmp.tab1 giving [cai]])");

  if (n > 0) {
    cerr << "***Error*** " << n
         << " commands succeeded, but were expected to fail." << endl;
    return 1;
  }
  return 0;
}
