//# TaQLShow.cc: Class to get various TaQL-related info
//# Copyright (C) 2016
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


//# Includes
#include <casacore/tables/TaQL/TaQLShow.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/casa/Quanta/UnitMap.h>
#include <casacore/casa/Exceptions/Error.h>
#include <ostream>
#include <sstream>

using namespace std;

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // This function concatenates the help info below to a single string.
  String concHelp (const char* str[], size_t n)
  {
    std::string s;
    for (size_t i=0; i<n; ++i) {
      s.append (str[i]);
      s.append ("\n");
    }
    return s;
  }

// Macro to get the full help string.
#define getHelp(arg) concHelp(arg, sizeof(arg) / sizeof(arg[0]))


  const char* infoHelp[] = {
    "Possible show/help commands:",
    "  show table tablename              table information (a la showtableinfo)",
    "  show command(s) [command]         TaQL commands and their syntax",
    "  show expr(essions)                how to form an expression",
    "  show oper(ators)                  available operators",
    "  show sets                         how to specify sets and intervals",
    "  show const(ants)                  how to specify constant scalars and arrays",
    "  show datatypes     (or dtype)     supported data types and their names",
    "  show tableoptions  (or tabopt)    possible options on table creation",
    "  show dminfo                       how to specify datamanager info",
    "  show functions [type] [subtype]   available functions",
    "       possible types: math, logical, conv(ersion), datetime, string",
    "                       array, reduce, astro, misc, aggr(egate)",
    "                name of UDF libraries (e.g., show functions mscal)",
    "  show meastypes [type]             available measure types",
    "       possible types: pos(ition), epoch, dir(ection), earthmagnetic (em),",
    "                       freq(uency), radialvelocity (radvel), doppler",
    "  show units [kind]                 available units and/or prefixes",
    "       possible kinds: prefix, length, time, angle, temperature,",
    "                       current, intensity, molar, mass, solidangle",
    "       If a unit is given as kind, all corresponding units are shown.",
    "       Note that TaQL can convert between ANGLE and TIME.",
    "",
    "A command can be followed by ; and/or by #anycomment (that are ignored).",
    "See http://casacore.github.io/casacore-notes/199.html for a full",
    "description of TaQL."
  };

  const char* tableHelp[] = {
    "Usage:   show table tablename [opt1 opt2 ...]",
    "    Options   Default",
    "         dm      nodm   show data managers?",
    "        col       col   show column descriptions?",
    "       sort    nosort   show columns alphabetically?",
    "        key     nokey   show table and column keywords?",
    "     tabkey  notabkey   show table keywords?",
    "     colkey  nocolkey   show column keywords?",
    "      recur   norecur   show subtables recursively?"
  };

  const char* commandHelp[] = {
    "Select a subset from a table, possibly calculating new values.",
    "  SELECT [[DISTINCT] expression_list] [INTO table [AS options]]",
    "    [FROM table_list]",
    "    [JOIN table_list ON expression [JOIN table_list ON expression ...]]",
    "    [WHERE expression]",
    "    [GROUPBY expression_list] [HAVING expression]",
    "    [ORDERBY [DISTINCT] sort_list] [LIMIT expression] [OFFSET expression]",
    "    [GIVING table [AS options] | set] [DMINFO datamanagers]",
    "",
    "Calculate an expression, possibly using table columns.",
    "  CALC expression [FROM table_list]",
    "",
    "Update (part of) the contents of a table.",
    "  UPDATE table_list SET update_list [FROM table_list]",
    "    [WHERE ...] [ORDERBY ...] [LIMIT ...] [OFFSET ...]",
    "",
    "Add rows with new values to a table.",
    "  INSERT INTO table_list SET column=expr, column=expr, ...",
    "  INSERT INTO table_list [(column_list)] VALUES (expr_list)",
    "  INSERT INTO table_list [(column_list)] SELECT_command",
    "",
    "Remove rows from a table.",
    "  DELETE FROM table_list",
    "    [WHERE ...] [ORDERBY ...] [LIMIT ...] [OFFSET ...]",
    "",
    "Create a new table, possibly adding rows.",
    "  CREATE TABLE table [AS options] [LIKE table [DROP COLUMN col, col, ...]]",
    "    [ADD COLUMN] [(column_specs)] [LIMIT ...] [DMINFO datamanagers]",
    "",
    "Alter a table (add/copy/rename/remove columns/keywords; add rows).",
    "  ALTER TABLE table [FROM table_list]",
    "    [ADD COLUMN [column_specs] [DMINFO datamanagers]]",
    "    [COPY COLUMN newcol=col, newcol=col, ... [DMINFO datamanagers]]",
    "    [RENAME COLUMN old TO new, old TO new, ...]",
    "    [DROP COLUMN col, col, ...]",
    "    [SET KEYWORD key=value, key=value, ...]",
    "    [COPY KEYWORD key=other, key=other, ...]",
    "    [RENAME KEYWORD old TO new, old TO new, ...]",
    "    [DROP KEYWORD key, key, ...]",
    "    [ADD ROW nrow]",
    "",
    "Count number of rows per group (subset of SELECT/GROUPBY).",
    "  COUNT [column_list] FROM table_list [WHERE ...]",
    "",
    "All commands can be preceded by 'WITH table-list' having temporary tables.",
    "Use 'show command <command>' for more information about a command.",
    "    'show expr(essions)'     for more information about forming expressions.",
    "See http://casacore.github.io/casacore-notes/199.html for full info.",
  };

  const char* selectHelp[] = {
    " [WITH table_list]",
    "SELECT",
    "  [[DISTINCT] expression_list]",
    "  [INTO table [AS options]]",
    "  [FROM table_list]",
    "  [JOIN table_list ON expression [JOIN table_list ON expression ...]]",
    "  [WHERE expression]",
    "  [GROUPBY expression_list]",
    "  [HAVING expression]",
    "  [ORDERBY [DISTINCT] [ASC|DESC] sort_list]",
    "  [LIMIT expression] [OFFSET expression]",
    "  [GIVING table [AS options] | set]",
    "  [DMINFO datamanagers]",
    "",
    "WITH table_list",
    "  Optional list of temporary tables to factor out multiply used subqueries.",
    "  E.g.,    WITH [select from some.tab WHERE expr] AS t1",
    "  creates a temporary table that can be used as table 't1' further down.",
    "",
    "[DISTINCT] expression-list",
    "  Optional list of expressions (possibly containing aggregate functions).",
    "  An expression can be followed by 'AS newname [datatype]' to define a name for",
    "  the resulting column and its data type (defaults to the expression type).",
    "   Possible data types: B, U1, I2, U2, I4, U4, I8, R4, R8, C4, C8, S, EPOCH",
    "   Use 'show datatypes' to get more information about the possible data types.",
    "  A masked array expression can be stored in 2 columns like",
    "      expression AS (datacolumn,maskcolumn) [datatype]",
    "  where datatype applies to the datacolumn (the maskcolumn is always Bool).",
    "  A regex (see 'show constants') can be used at any place in the expression-list",
    "  to include or exclude columns.",
    "    For example:   !~p/*DATA/    to exclude all columns ending in DATA",
    "  DISTINCT (or UNIQUE) removes output rows with equal values.",
    "  If no expression-list is given, all columns of the first table are selected.",
    "",
    "INTO table [AS options]",
    "  Name of resulting table; if not given, no output table is created.",
    "  Possible options:",
    "    TYPE      = PLAIN,SCRATCH,MEMORY",
    "      If TYPE is not given, a reference table is made if no expressions",
    "      are given in the SELECT clause, otherwise a plain table is made.",
    "    ENDIAN    = BIG,LITTLE,LOCAL,AIPSRC                     default AIPSRC",
    "    STORAGE   = SEPFILE,MULTIFILE,MULTIHDF5,DEFAULT,AIPSRC  default AIPSRC",
    "    BLOCKSIZE = <n>",
    "    OVERWRITE = T|F                                         default T",
    "  Use 'show tableoptions' to get more information about the possible options.",
    "",
    "FROM table_list",
    "  One or more input tables to use. Each table can be followed by a",
    "  shorthand to be used to qualify a column or keyword in an expression.",
    "  A table can be one of the following:",
    "  - a tablename proper, possibly containing ~ and $",
    "    Use :: (not /) to name a subtable (e.g., my.ms::ANTENNA)",
    "  - a subquery enclosed in parentheses or brackets",
    "  - concatenated tables; separated by commas enclosed in brackets",
    "  - backreference to an earlier created table using its shorthand",
    "  If FROM is not given, an empty table with LIMIT (default 1) rows is used.",
    "",
    "JOIN table_list ON expression ...",
    "  Zero or more joins can be done to connect tables. For instance, in a MeasurementSet",
    "  the main table can be joined with the ANTENNA subtable to find antenna names.",
    "  The tables in the list are the join tables, while the tables in the FROM table_list",
    "  or in a previous join are the main tables. The join tables can be specified in the",
    "  same way as described in the FROM clause above.",
    "  The expression must be a boolean scalar expression doing one or more comparisons",
    "  separated by AND. Each comparison matches a column in the main and join table.",
    "  A comparison can be the equality operator = or the interval operator IN,",
    "  where the interval must be in the join table. For example:",
    "    SELECT t1.ANTENNA1, t2.NAME FROM my.ms t1",
    "        JOIN ::ANTENNA t2 on t1.ANTENNA1=t2.rownumber()",
    "  selects the antenna number and its name by joining the antenna number with the row",
    "  number in the ANTENNA subtable (where the row number is the implicit key).",
    "  A more complicated join is given below. It finds information in the SYSCAL table",
    "  based on antenna, spectral window and time.",
    "    JOIN ::DATA_DESC t2 ON t1.DATA_DESC_ID==t2.rownumber() JOIN ::SYSCAL t3 ON",
    "    t1.ANTENNA1==t3.ANTENNA_ID and t2.SPECTRAL_WINDOW_ID==t3.SPECTRAL_WINDOW_ID and",
    "    t1.TIME AROUND t3.TIME IN t3.INTERVAL",
    "",
    "WHERE expression",
    "  An expression resulting in a boolean scalar telling if a row is selected.",
    "  If not given, all rows will be selected.",
    "",
    "GROUPBY expression_list",
    "  It can be used to group rows with equal values for the given",
    "  expressions (which must result in scalar values).",
    "  Often aggregate functions (such as GSUM) are used in the SELECT and/or",
    "  HAVING clause to calculate an aggregate value, but in something like",
    "      select from my.ms groupby ANTENNA1,ANTENNA2",
    "  GROUPBY is used to get the number of unique baselines in the MS.",
    "",
    "HAVING expression",
    "  Similar to WHERE, but is a filter on the result of GROUPBY.",
    "  A column in the expression can be a column created in the SELECT clause.",
    "",
    "ORDERBY [DISTINCT] [ASC|DESC] sort_list",
    "  It specifies how the output of the SELECT clause has to be ordered.",
    "  The sort list is a list of expressions (resulting in scalar values),",
    "  optionally followed by ASC or DESC to define the order for that sort key.",
    "  A column in an expression can be a column created in the SELECT clause.",
    "  ASC (default) or DESC given before the sort list defines the default order.",
    "  DISTINCT (or UNIQUE) removes all rows with equal sort keys.",
    "",
    "LIMIT expression OFFSET expression",
    "LIMIT start:end:step",
    "  can be used to limit the number of output rows.",
    "  The first form is in fact the same as 'offset:offset+limit:1'",
    "    start   first row to use; defaults to 0",
    "    end     last row (exclusive); defaults to all rows",
    "    step    take every step-th row; defaults to 1",
    "  start and end can be negative to count from the end (a la python).",
    "  This is also true for the expressions in the first form.",
    "",
    "GIVING table [AS options] | set",
    "  Defines the output table, which is an alternative for the INTO clause.",
    "  It is also possible to specify a set containing a column or interval,",
    "  which can be used to define the result of a nested query.",
    "",
    "DMINFO datamanagers",
    "  can be used by expert users to define data managers for the output columns.",
    "  Use 'show dminfo' for more information."
  };

  const char* calcHelp[] = {
    " [WITH table_list]",
    "CALC expression [FROM table_list]",
    "",
    "This command evaluates the given expression, which can contain columns from",
    "the given tables. It is basically the same as",
    "  SELECT expression [FROM table_list]",
    "but does not create an output table as SELECT always does."
  };

  const char* updateHelp[] = {
    " [WITH table_list]",
    "UPDATE",
    "  table_list",
    "  SET update_list",
    "  [FROM table_list]",
    "  [WHERE expression]",
    "  [ORDERBY [DISTINCT] [ASC|DESC] sort_list]",
    "  [LIMIT expression] [OFFSET expression]",
    "",
    "table_list",
    "  The first table in the table_list is updated. More tables can be given to be",
    "  used in expressions, but preferably these tables are given in the FROM part.",
    "",
    "SET update_list",
    "  The update_list looks like:",
    "    COLUMN=expression, COLUMN=expression, ...",
    "  COLUMN is the name of the column to be updated with the expression value.",
    "  If the column contains scalars, the expression must be a scalar.",
    "  Arrays in columns can be set in various ways:",
    "  - If set to a scalar, all array elements in the column are set to the scalar.",
    "  - If set to an array, each array is replaced by the new one, possibly with a",
    "    different shape.",
    "  It is possible to update part of an array by slicing and/or masking it like:",
    "    COLUMN[start:end:step]   or   COLUMN[mask]",
    "  If a mask is used, only array values with a True mask value will be updated.",
    "  It is possible to apply slicing and masking in succession (in any order).",
    "  Array and mask can be updated jointly like",
    "    (DATACOLUMN,MASKCOLUMN)=expression",
    "  if expression is a masked array.",
    "  Slicing and masking can only be applied to both of them like:",
    "    (COLUMN,MASKCOLUMN)[start:end:step]   or   (COLUMN,MASKCOLUMN)[mask]",
    "",
    "FROM table_list",
    "  Extra tables used in expressions (can also be given in the first table_list).",
    "  A table can be any type (as in the FROM clause of the SELECT command).",
    "",
    "WHERE, ORDERBY, LIMIT, OFFSET",
    "  Using these clauses (similar to SELECT) a subset of the table can be updated."
  };

  const char* insertHelp[] = {
    " [WITH table_list]",
    "INSERT INTO table_list SET column=expr, column=expr, ...",
    "  Add one row.",
    "INSERT INTO table_list [(column_list)] VALUES (expr_list),(expr_list),... [LIMIT n]",
    "  Add n rows.",
    "INSERT INTO table_list [(column_list)] SELECT_command",
    "  Add selected rows from another (or same) table.",
    "",
    "table_list",
    "  Rows are added to the first table in the list. More tables can be given",
    "  to be used in expressions. Any table form can be used as in SELECT FROM.",
    "",
    "SET column=expr, column=expr, ...",
    "  Specify the column values in the new row in the same way as for UPDATE.",
    "",
    "(column_list)",
    "  Specify the column names (enclosed in parentheses).",
    "  The form (DATACOLUMN,MASKCOLUMN) can be used to store a masked array.",
    "  If (column_list) is omitted, it defaults to all stored columns in the table.",
    "",
    "(expr_list)",
    "  List of expressions separated by commas enclosed in parentheses.",
    "  Each expr_list is a row to be added.",
    "",
    "LIMIT expression",
    "  defines the number of rows to add. It defaults to the number of (expr_list)",
    "  given. If different, it iterates through the expr_lists as needed.",
    "",
    "SELECT_command",
    "  The rows resulting from the SELECT command are added.",
    "  The resulting column names and data types have to match the column_list,",
    "  but their order can differ."
  };

  const char* deleteHelp[] = {
    " [WITH table_list]",
    "DELETE FROM table_list",
    "  [WHERE ...] [ORDERBY ...] [LIMIT ...] [OFFSET ...]",
    "",
    "table_list",
    "  Rows matching the selection criteria are removed from the first table",
    "  in the table_list. More tables can be given to be used in expressions.",
    "",
    "WHERE, ORDERBY, LIMIT, OFFSET",
    "  Only rows matching these selection criteria are removed.",
    "  See 'help command select' for a brief description of these clauses."
  };

  const char* createHelp[] = {
    " [WITH table_list]",
    "CREATE TABLE table [AS options]",
    "  [LIKE other_table [DROP COLUMN column_list]]",
    "  [[ADD COLUMN] (column_specs)]",
    "  [LIMIT expression]",
    "  [DMINFO datamanagers]",
    "",
    "table [AS options]",
    "  The name of the table to create followed by optional table options (enclosed in",
    "  square brackets) controlling how the table is created.",
    "  Possible options:",
    "    ENDIAN    = BIG,LITTLE,LOCAL,AIPSRC                     default AIPSRC",
    "    STORAGE   = SEPFILE,MULTIFILE,MULTIHDF5,DEFAULT,AIPSRC  default AIPSRC",
    "    BLOCKSIZE = <n>",
    "    OVERWRITE = T|F                                         default T",
    "  Use 'show tableoptions' to get more information about the possible options.",
    "",
    "LIKE other_table DROP COLUMN column_list",
    "  The new table is created with the same description and dminfo as other_table.",
    "  other_table can be followed by a shorthand to be used in other command parts.",
    "  Optionally DROP COLUMN column_list can be given to omit the given columns.",
    "  It is possible to add columns in the column_specs.",
    "",
    "[ADD COLUMN] (column_specs)",
    "  A list of column specifications separated by commas and enclosed in parentheses (or",
    "  square brackets) and preceded by ADD COLUMN. Both can be left out if LIKE",
    "  is not used. Each spec looks like:",
    "      name [LIKE other_name] [datatype] [prop_list]",
    "  name       Name of the column.",
    "  other_name Name of the column whose description is used for the new column.",
    "             It can be from another table using the 't.column' syntax.",
    "             The description can be modified using the datatype and prop_list arguments.",
    "  datatype   Data type (B, U1, I2, U2, I4, U4, I8, R4, R8, C4, C8, S, EPOCH).",
    "             Use 'show datatypes' for more information about data types.",
    "             It must be given if 'LIKE other_name' is not given.",
    "  prop_list  Optional key=value list of other properties. Enclose in square",
    "             brackets if multiple key=value pairs are given.",
    "      NDIM=n              dimensionality",
    "      SHAPE=[n1,n2,...]   fixed shape; NDIM must match if also given",
    "      UNIT='string'       unit of values in the column",
    "      COMMENT='string'    comment string describing the column",
    "",
    "LIMIT expression",
    "  Number of rows in the new table. No rows if omitted.",
    "",
    "DMINFO datamanagers",
    "  can be used by expert users to define data managers for the columns.",
    "  Use 'show dminfo' for more information."
  };

  const char* alterHelp[] = {
    " [WITH table_list]",
    "ALTER TABLE table [FROM table_list] subcommand1 subcommand2 ...",
    "  Alter the table with the given name.",
    "  The FROM part can be given to use other tables in the subcommands.",
    "  One or more of the following subcommands can be given.",
    "",
    " ADD COLUMN [column_specs] [DMINFO datamanagers]",
    "  Add one or more columns to the table.",
    "  Use 'show command create' to see the syntax for the arguments.",
    "",
    " COPY COLUMN newcol=other, newcol=other, ... [DMINFO datamanagers]",
    "  Copy the data of a column to a new column, which gets the same description.",
    "  By default it gets the same data manager, but can be overwritten using DMINFO.",
    "  Use 'show dminfo' for the syntax of the DMINFO argument.",
    "",
    " RENAME COLUMN old TO new, old TO new, ...",
    "  Rename one or more columns.",
    "",
    " DROP COLUMN col, col, ...",
    "  Remove one or more columns.",
    "",
    " SET KEYWORD key=value [AS dtype], key=value [AS dtype], ...",
    "  Add or reset one or more table and/or column keyword values.",
    "  A column keyword name must be preceded by the column name like col::key.",
    "  Nested keyword names can be used (e.g., col::key.subkey.fld)",
    "  The value can be any expression, also an empty array given as [].",
    "  A data type can be given, which defaults to the expression data type.",
    "  Use 'show datatypes' to see the possible data types.",
    "  The value cannot be a record, but an empty record can be given as [=].",
    "",
    " COPY KEYWORD key=other, key=other, ...",
    "  Copy the value of a table and/or column keyword to a new keyword.",
    "  The keyword can be a nested one, thus a field in a record value.",
    "  The value can be anything, also a record.",
    "",
    " RENAME KEYWORD old TO new, old TO new, ...",
    "  Rename one or more table and/or column keywords, possibly nested ones.",
    "  The new name cannot be qualified with a column or nested keyword name,",
    "  thus renaming cannot be done across columns or so.",
    "",
    " DROP KEYWORD key, key, ...",
    "  Remove one or more table and/or column keywords, possibly nested ones.",
    "",
    " ADD ROW expression",
    "  The expression gives the number of rows to be added to the table."
  };

  const char* countHelp[] = {
    " [WITH table_list]",
    "COUNT [column_list] FROM table_list [WHERE expression]",
    "",
    "After having done the WHERE selection, it counts the number of rows",
    "in the first table for each group formed by the columns in the list.",
    "It creates a table with the columns in the column list and the column",
    "_COUNT_ containing the number of rows in each group.",
    "",
    "  COUNT col1,col2 FROM my.ms WHERE expression",
    "is the same as",
    "  SELECT col1,col2,gcount() as _COUNT_ FROM my.ms",
    "  WHERE expression GROUPBY col1,col2",
    "",
    "SELECT/GROUPBY is much more powerful, but the COUNT command can still be used."
  };

  const char* exprHelp[] = {
    "A TaQL expression can use scalar and/or arrays (as in numpy).",
    "The following elements can be used:",
    "  operators         see 'show oper(ators)'",
    "  functions         see 'show func(tions)'",
    "  constants         see 'show const(ants)'",
    "                    (scalar and arrays of various data types)",
    "  sets/intervals    see 'show sets'",
    "  units             see 'show units'",
    "  columns           see 'show table <tablename>'",
    "  keywords          see 'show table <tablename> tabkey colkey'",
    "",
    "- A unit can be given after each (sub)expression; if needed conversion is done.",
    "  For example:   (1e9Hz + 1GHz)MHz     results in 2000 MHz",
    "- Columns and keywords can be taken from any table given in a TaQL command",
    "  by preceding their names with a table shorthand (e.g., t0.DATA)",
    "- A keyword can be a table keyword or a column keyword (e.g., col::key)",
    "  Nested keywords can be used (e.g., col::key.subkey.fld)",
    "- In the HAVING and ORDERBY clause an expression can use a column",
    "  created in the SELECT clause.",
    "- Aggregate functions are only possible in the SELECT and HAVING clause."
  };

  const char* operHelp[] = {
    "Available TaQL operators in order of precedence (high to low):",
    "    **",
    "    !  ~  +  -       (unary operators)",
    "    *  /  // %",
    "    +  -",
    "    &",
    "    ^",
    "    |",
    "    == != >  >= <  <=  ~= !~= IN INCONE BETWEEN AROUND EXISTS LIKE  ~  !~",
    "    &&",
    "    ||",
    "",
    "Some operators have a synonym:",
    "    ==     =",
    "    !=     <>",
    "    &&     AND",
    "    ||     OR",
    "    !      NOT",
    "    ^      XOR",
    "",
    "Description of some operators:",
    "    **            power, right-associative, thus 2**1**2 = 2",
    "    ~             if unary, bit-wise complement",
    "    /             real division, thus 1/2 = 0.5",
    "    //            integer division, thus 1//2 = 0",
    "    %             modulo as in Python; sign of divisor",
    "    +             also string concat; also add days to datetime",
    "    -             also for datetime (results in unit d)",
    "    &             bit-wise and",
    "    ^             bit-wise xor",
    "    |             bit-wise or",
    "    ~= !~=        (not) about equal (relative to 1e-5)",
    "    ~ !~ (I)LIKE  pattern match",
    "    IN            is left hand an element in the right hand?",
    "    INCONE        cone searching",
    "    EXISTS        does subquery have at least 1 match?",
    "    x BETWEEN a AND b         is x in bounded interval <a,b>?",
    "    x AROUND m IN w           is x in bounded interval <m-w/2,m+w/2>?"
  };

  const char* constHelp[] = {
    "Scalar constants of following data types:",
    "  bool       TRUE or FALSE (case-insensitive), T or F",
    "  int        integer; also hexadecimal like 0xffff",
    "  double     12.  12e5  3.2e-5  etc.",
    "             value followed by a unit like 10m or 10.5sec",
    "             position/time in HMS or DMS like 2h13m44.5 or 30d13m44.4",
    "               results in double with unit rad",
    "  complex    add imaginary part like  1+2i  or  3 - 5j",
    "  string     enclose in single or double quotes; concatenation like 'ab'\"cd\"",
    "  datetime   date/time like 3Mar16/12:14:23.3 or 2016-03-02/1:4:23",
    "               - or space can be used instead of /",
    "  regex      p/globpattern/ or f/regex/ or m/regex/ (same as f/.*regex.*/)",
    "               used with operator ~ or !~",
    "               % or @ can also be used as regex delimiter",
    "",
    "N-dim array of those data types (except regex) like:",
    "             [1,2,3] (1-dim)  or  [[1,2,3],[4,5e3,6]] (2-dim)",
    "          or using function ARRAY",
    "",
    "Masked array (True value means bad (as in numpy)):",
    "             array[mask]  such as  [1,2,3][[T,F,T]]",
    "          or using function MARRAY"
  };

  const char* dtypeHelp[] = {
    "Internally TaQL supports the data types bool, int64, double, dcomplex,",
    "string, regex and datetime.",
    "'show constants' shows how to define constants for those types.",
    "",
    "There is more choice for the data type of a table column.",
    "When creating a table (using e.g. the SELECT or CREATE TABLE command), the",
    "data types of the columns can be defined using one of the following words.",
    "",
    "      B          BOOL       BOOLEAN",
    "      U1         UCHAR      BYTE",
    "      I2         SHORT      SMALLINT",
    "      U2   UI2   USHORT     USMALLINT",
    "      I4         INT        INTEGER",
    "      U4   UI4   UINT       UINTEGER",
    "      I8         LONG       BIGINT",
    "      R4   FLT   FLOAT",
    "      R8   DBL   DOUBLE",
    "      C4   FC    FCOMPLEX   COMPLEX",
    "      C8   DC    DCOMPLEX",
    "      S          STRING",
    "      EPOCH",
    "",
    "EPOCH can be used for a datetime value. It uses a column with data type",
    "double and sets the column keywords defining the Measure type MEpoch."
  };

  const char* taboptHelp[] = {
    "One or more of the following key=value options can be used to specify",
    "how a table has to be created. If multiple options are given, they have",
    "to be enclosed in square brackets separated by commas.",
    "",
    "  TYPE='value' specifies the table type.",
    "    PLAIN   = make a persistent table, thus a true copy of all",
    "              selected rows/columns.",
    "    SCRATCH = as plain, but only as a temporary table.",
    "    MEMORY  = as plain, but keep everything in memory.",
    "",
    "  ENDIAN='value' specifies the endianness.",
    "    BIG    = big endian",
    "    LITTLE = little endian",
    "    LOCAL  = native endianness of the machine being used",
    "    AIPSRC = as defined in the .casarc file (usually defaults to LOCAL)",
    "  If ENDIAN is not given, it defaults to AIPSRC.",
    "",
    "  STORAGE='value' specifies the storage type.",
    "    SEPFILE   = store as separate files (the old Casacore table format)",
    "    MULTIFILE = combine all storage manager files into a single file",
    "    MULTIHDF5 = as MULTIFILE, but use HDF5 instead of a regular file",
    "    DEFAULT   = use SEPFILE (might change in a future Casacore version)",
    "    AIPSRC    = as defined in the .casarc file (usually defaults to DEFAULT)",
    "  If STORAGE is not given, it defaults to AIPSRC.",
    "",
    "  BLOCKSIZE=n   the blocksize (in bytes) to use for MULTIFILE or MULTIHDF5",
    "",
    "  OVERWRITE=T|F overwrite an existing table? Default is T"
      };

  const char* dminfoHelp[] = {
    "DMINFO [NAME=name, TYPE=type, SPEC=[...] COLUMNS=[col1, col2, ...]], ...",
    "  defines the data managers to be used by columns. It is a comma separated",
    "  list of key=value definitions enclosed in square brackets.",
    "    NAME=name      defines the unique data manager name.",
    "    TYPE=type      defines the type; StandardStMan, TiledShapeStMan, etc.",
    "    COLUMNS=[...]  is a comma separated list of the names of the columns to",
    "                   be stored with this data manager.",
    "    SPEC=[...]     is a datamanager-specific key=value list defining the",
    "                   data manager parameters. If not given, defaults are used.",
    "        Tiled storage managers:",
    "            DEFAULTTILESHAPE   default tile shape (in Fortran order!!)",
    "            MAXCACHESIZE       maximum tile cache size (0=no limit)",
    "        IncrementalStMan:",
    "            BUCKETSIZE         size of bucket (in bytes)",
    "        StandardStMan:",
    "            BUCKETSIZE         size of bucket in bytes",
    "         or BUCKETROWS         size of bucket in rows",
    "        VirtualTaQLColumn:",
    "            TAQLCALCEXPR       expression (using other columns)",
    "        BitFlagsEngine:",
    "            SOURCENAME         name of the boolean source column",
    "            TARGETNAME         name of the integer target column",
    "            ReadMask           integer defining flags to use on read",
    "         or ReadMaskKeys       vector with flag names to use on read",
    "            WriteMask          integer defining flags to use on write",
    "         or WriteMaskKeys      vector with flag names to use on write",
    "  There are several more data managers, also external ones such as LofarStMan.",
    "  For example:",
    "    dminfo [NAME='ISM1',TYPE='IncrementalStMan',COLUMNS=['col1']],",
    "           [NAME='SSM1',TYPE='StandardStMan',",
    "            SPEC=[BUCKETSIZE=1000],COLUMNS=['col2','col3']]",
    "  defines 2 data managers (one for col1 and the other for col2 and col3)."
  };

  const char* setHelp[] = {
    "A set is a series of values, ranges and/or intervals enclosed in brackets.",
    "Often the IN operator is used on a set, but a set can also be used as an array.",
    "",
    "A value can be of type integer, double, complex, datetime or string.",
    "Numeric data types can be mixed; the 'highest' type is used.",
    "",
    "A range is a series of values written as start:end:step",
    "  'start' can be omitted and defaults to 0",
    "  'end'   can be omitted making it unbounded (till infinity)",
    "  ':step' can be omitted and defaults to 1",
    "start and end can be integer, double or datetime",
    "step must be integer or double and can be negative (as in Python)",
    "",
    "An interval is a continuous set of real values with optional bounds.",
    "If a bound is given, it can be open or closed.",
    "An interval can be given in various ways:",
    "  as start-end using curly braces (closed side) and angle brackets (open side)",
    "    bounded:     {1,2}   <1,2>   {1,2>   <1,2}",
    "    unbounded:   {1,}    <1,>    {,2>    <,2}",
    "  as start-end using  a=:=b (closed sides)  and  a<:<b (open sides)",
    "    bounded:     1=:=2   1<:<2   1=:<2   1<:=2",
    "    unbounded:   1=:     1<:     :<2     :=2",
    "  as start-end (closed sides) using  BETWEEN start AND end",
    "  as mid-width (closed sides) using  mid<:>width  or  AROUND mid IN width",
    "",
    "A set consisting of values and/or bounded ranges is a 1-dim array.",
    "  For example:   [1,2,3,4,5]   [1:6]   [1,2:5,5]   are all the same",
    "Multi-dimensional arrays can be given as nested sets.",
    "  For example:   [[1,2,3],[4:7]]   defines an array with shape [2,3]",
    "",
    "A set can be created from a subquery as used in:",
    "  ANTENNA1 IN [select rowid() from ::ANTENNA where NAME~p/CS*/]",
  };

  const char* allFuncHelp[] = {
    "About all TaQL functions operate on scalars and arrays (and mixed)",
    "math functions:",
    "  pi          e           c           rand",
    "  sin         sinh        asin        cos         cosh        acos",
    "  tan         tanh        atan        atan2",
    "  exp         log         log10",
    "  pow         sqrt        sqr         cube",
    "  norm        abs         arg         fmod",
    "  sign        round       floor       ceil",
    "logical functions:",
    "  near        nearabs     isnan       isinf       isfinite",
    "  isdef       isnull      iscol       iskey",
    "  min         max         iif",
    "conversion functions:",
    "  bool        int         real        imag        complex     conj",
    "  string      hms         dms         hdms",
    "datetime functions:",
    "  datetime    mjdtodate   mjd         date        time",
    "  year        month       day         weekday     week",
    "  cmonth      cweekday    cdatetime   cdate       ctime",
    "string functions:",
    "  strlength   upcase      downcase    capitalize  sreverse",
    "  trim        ltrim       rtrim       substr      replace",
    "  regex       pattern     sqlpattern",
    "array functions:",
    "  array       ndim        nelem       shape",
    "  transpose   areverse    resize      diagonal",
    "  nullarray   marray      arraydata   mask",
    "  flatten     negatemask  replacemasked           replaceunmasked",
    "reduce functions:",
    "  sum         product     sumsqr      min         max            mean",
    "  variance    samplevariance          stddev      samplestddev   avdev       rms",
    "  median      fractile    any         all         ntrue          nfalse",
    "    plural, running and boxed forms of above reduce functions",
    "astro functions:",
    "  angdist     angdistx    normangle   cones       anycone     findcone",
    "    see also 'show func meas' and 'show func mscal'",
    "misc functions:",
    "  msid        rownr       rowid",
    "aggregate functions:",
    "  gmin        gmax        gsum        gproduct    gsumsqr        gmean",
    "  gvariance   gsamplevariance         gstddev     gsamplestddev  grms",
    "  gany        gall        gntrue      gnfalse",
    "    plural forms of above aggregate functions (e.g., gmins)",
    "  gmedian     gfractile   ghist       gstack",
    "  countall    gcount      gfirst      glast "
  };

  const char* mathFuncHelp[] = {
    "Mathematical functions",
    "",
    "  double  PI    ()",
    "  double  E     ()",
    "  double  C     ()                      m/s",
    "  double  RAND  ()",
    "  numeric SIN   (numeric)",
    "  numeric SINH  (numeric)",
    "  double  ASIN  (real)                  rad",
    "  numeric COS   (numeric)",
    "  numeric COSH  (numeric)",
    "  double  ACOS  (real)                  rad",
    "  double  TAN   (real)",
    "  double  TANH  (real)",
    "  double  ATAN  (real)                  rad",
    "  double  ATAN2 (real y, real x)        rad",
    "  numeric EXP   (numeric)",
    "  numeric LOG   (numeric)",
    "  numeric LOG10 (numeric)",
    "  numeric POW   (numeric, numeric exp)",
    "  numeric SQRT  (numeric)",
    "  numeric SQR   (numeric)      aka SQUARE",
    "  numeric CUBE  (numeric)",
    "  real    NORM  (numeric)",
    "  real    ABS   (numeric)      aka AMPLITUDE",
    "  double  ARG   (numeric)      aka PHASE",
    "  real    FMOD  (real, real)   modulo as in C; sign of dividend",
    "  real    SIGN  (real)",
    "  real    ROUND (real)         round(-1.6) = -2",
    "  real    FLOOR (real)         floor(-2.2) = -3",
    "  real    CEIL  (real)         ceil (-2.2) = -2"
  };

  const char* convFuncHelp[] = {
    "Conversion functions",
    "",
    "  string HMS       (real RAD)    convert angles to e.g. 12h34m56.789",
    "  string DMS       (real RAD)    convert angles to e.g. 12d34m56.789",
    "  string HDMS      (realarray)   convert angles alternately to HMS and DMS",
    "  string STR       (string, int WIDTH)    make string WIDTH long",
    "  string STR       (numeric, )    make string WIDTH long",
    "  string STR       (string, int WIDTH)    make string WIDTH long"
  };

  const char* logicalFuncHelp[] = {
    "Logical functions",
    "",
    "  bool    NEAR      (numeric, numeric, double tol)    relative near",
    "  bool    NEARABS   (numeric, numeric, double tol)    absolute near",
    "",
    "  bool    ISNAN     (numeric val)       is value Not-a-Number?",
    "  bool    ISINF     (numeric val)       is value infinite?",
    "  bool    ISFINITE  (numeric val)       is value finite?",
    "  bool    ISNULL    (anytype)           is array a null array?",
    "  bool    ISDEFINED (anytype)           contains row a value?",
    "  bool  t.ISCOLUMN  (string)            does keyword exist in table?",
    "  bool  t.ISKEYWORD (string)            does keyword exist in table?",
    "",
    "  numeric MIN (numeric, numeric)",
    "  numeric MAX (numeric, numeric)",
    "  anytype IIF (bool cond, arg1, arg2)   arg1 if cond is True, else arg2"
  };

  const char* dateTimeFuncHelp[] = {
    "Date/time functions",
    "Functions taking a datetime, use current UTC date/time if not given",
    "",
    "  datetime DATETIME  (string)       convert string to datetime",
    "  datetime MJDTODATE (real)         convert MJD to datetime",
    "  double   MJD       (datetime)     convert datetime to MJD",
    "  datetime DATE      (datetime)     get date, thus remove time part",
    "  double   TIME      (datetime)     get time (in rad), thus remove date part",
    "  int      YEAR      (datetime)     get year",
    "  int      MONTH     (datetime)     get month (1..12)",
    "  int      DAY       (datetime)     get day (1..31)",
    "  int      WEEK      (datetime)     get week number (0..53)",
    "  int      WEEKDAY   (datetime)   aka DOW     weekday (1=Monday .. 7=Sunday)",
    "  string   CDATETIME (datetime)   aka CTOD    YYYY/MM/DD/HH:MM:SS.SSS",
    "  string   CDATE     (datetime)               DD-MMM-YYYY",
    "  string   CTIME     (datetime)               HH:MM:SS.SSS",
    "  string   CMONTH    (datetime)               Jan..Dec",
    "  string   CWEEKDAY  (datetime)   aka CDOW    Mon..Sun"
  };

  const char* stringFuncHelp[] = {
    "String functions",
    "",
    "  int    LEN        (string)      aka STRLENGTH",
    "  string UPCASE     (string)      aka UPPER",
    "  string DOWNCASE   (string)      aka LOWER",
    "  string CAPITALIZE (string)",
    "  string SREVERSE   (string)      reverse the string   (aka REVERSESTRING)",
    "  string TRIM       (string)      remove leading/trailing whitespace",
    "  string LTRIM      (string)      remove leading whitespace",
    "  string RTRIM      (string)      remove trailing whitespace",
    "  string SUBSTR     (string, int START, int N)    START<0 means from the end",
    "  string REPLACE    (string SRC, string TOREPLACE[, string REPLACEMENT])",
    "  string REPLACE    (string SRC, regex  TOREPLACE[, string REPLACEMENT])",
    "",
    "  regex  REGEX      (string)      make regex from regular expression string",
    "  regex  PATTERN    (string)      make regex from glob pattern string",
    "  regex  SQLPATTERN (string)      make regex from SQL-style pattern string",
    "",
    "'show func conversion'  for functions converting values to string",
    "'show func datetime'    for functions converting date/time to string"
  };

  const char* arrayFuncHelp[] = {
    "Array creation/manipulation functions",
    "",
    "  array ARRAY     (value, shape)          create array and fill with value",
    "  int   NELEMENTS (any)     aka COUNT     size of array (1 for scalar)",
    "  int   NDIM      (any)                   dimensionality of array (0 for scalar)",
    "  array SHAPE     (any)                   shape of array (empty for scalar)",
    "  array TRANSPOSE (array [,axes])         transpose array",
    "    if axes are given, only those axes are transposed",
    "  array AREVERSE  (array [,axes])         reverse array elements of given axes",
    "    if no axes are given, elements of all axes are reversed",
    "  array RESIZE    (array, shape [,mode])  resize an array",
    "    no mode: copy corresponding elements",
    "    mode=0 : upsampling; copy values evenly if axis gets larger: 1,2 -> 1,1,2,2",
    "    mode=1 : repeat values if axis gets larger: 1,2 -> 1,2,1,2",
    "  array DIAGONAL  (array [,firstaxis [,diag]])",
    "    diagonal of each 2-dim subarray (at axis firstaxis)",
    "    diag=0 main diagonal; <0 below main diagonal; >0 above",
    "",
    "  array NULLARRAY (value)                 create null array",
    "  array MARRAY    (array, boolarray)      create masked array",
    "    same as 'array[boolarray]'",
    "  array ARRAYDATA (array)                 array without possible mask",
    "  bool  ARRAYMASK (array)   aka MASK      mask of masked array",
    "  array FLATTEN   (array)                 remove masked elements",
    "  array NEGATEMASK(array)                 negate mask in masked array",
    "  array REPLACEMASKED   (arr1, arr2)",
    "    replace masked elements in arr1 by corresponding value in arr2",
    "  array REPLACEUNMASKED (arr1, arr2)",
    "    replace unmasked elements in arr1 by corresponding value in arr2"
  };

  const char* reduceFuncHelp[] = {
    "Array reduce functions (use unmasked elements only)",
    " XXX        (array)               reduces to a scalar",
    " XXXS       (array, reduceAxes)   reduces to a (N-M)-dim array",
    " RUNNINGXXX (array, windowSize)   calculates XXX in sliding window",
    " BOXEDXXX   (array, boxSize)      calculates XXX for each box",
    "",
    "XXX can be one of the following functions:",
    "  bool    ANY      (bool)      is any element true?",
    "  bool    ALL      (bool)      are all elements true?",
    "  int     NTRUE    (bool)      number of true elements",
    "  int     NFALSE   (bool)      number of false elements",
    "  numeric SUM      (numeric)   sum of all elements",
    "  numeric SUMSQR   (numeric)   sum of all squared elements   aka SUMSQUARE",
    "  numeric PRODUCT  (numeric)   product of all elements",
    "  real    MIN      (real)      minimum of all elements",
    "  real    MAX      (real)      maximum of all elements",
    "  numeric MEAN     (numeric)   mean of all elements          aka AVG",
    "  double  VARIANCE (numeric)   population variance              complex variance",
    "  double  SAMPLEVARIANCE(numeric)  sample variance              uses absolute value",
    "  double  STDDEV   (numeric)   population standard deviation    (is sum of real and",
    "  double  SAMPLESTDDEV(numeric)    sample standard deviation     imaginary variances)",
    "  double  AVDEV    (numeric)   average deviation",
    "  double  RMS      (real)      root-mean-square",
    "  double  MEDIAN   (real)      median (the middle element)",
    "  double  FRACTILE (real, fraction)   element at given fraction"
  };

  const char* astroFuncHelp[] = {
    "Astronomical functions",
    "",
    "  double ANGDIST    (arg1,arg2)     aka ANGULARDISTANCE",
    "    angular distance (in rad) between corrersponding positions in arg1 and arg2",
    "    arg1 and arg2 must be arrays containing ra/dec or lon/lat pairs",
    "  double ANGDISTX   (arg1,arg2)     aka ANGULARDISTANCEX",
    "    same as ANGDIST, but between all positions in arg1 and arg2",
    "  double NORMANGLE  (arg1)",
    "    normalize an angle between -pi and pi radians",
    "  bool   ANYCONE    (source, cones)",
    "    True if source in at least one of the cones",
    "    synonym for operator INCONE",
    "  bool   ANYCONE    (source, conepos, radii)",
    "    same as above, but cone centers and radii are given separately",
    "    each radius is applied to each cone",
    "  int    FINDCONE   (sources, cones)",
    "    index of the first cone containing a source",
    "    if a single source is given, the result is a scalar, otherwise an array",
    "  int    FINDCONE   (sources, conepos, radii)",
    "    same as above, but cone centers and radii are given separately",
    "    each radius is applied to each cone",
    "  bool   CONES      (sources, cones)",
    "    2-dim bool array telling for each source if in each cone",
    "  bool   CONES      (sources, conepos, radii)",
    "    3-dim bool array telling for each source if in each cone and radius",
    "",
    "'show func meas'   for measures functions converting between reference frames",
    "'show func mscal'  for mscal functions handling measures in MeasurementSets",
  };

  const char* miscFuncHelp[] = {
    "Miscellaneous functions",
    "",
    "  int ROWNR()   aka ROWNUMBER    return row number in current table",
    "  int ROWID()                    return row number in input table",
    "      MSID(column)               use column if existing, otherwise ROWID()",
  };

  const char* aggrFuncHelp[] = {
    "Aggregate functions operating per group (using GROUPBY)",
    "",
    "The following functions result in a scalar value",
    "  int     GCOUNT()              number of rows                aka GCOUNT(*)",
    "  int     GCOUNT (columnname)   number of rows for which the column has a value",
    "  anytype GFIRST    (anytype)   first value in the group",
    "  anytype GLAST     (anytype)   last value of the group",
    "  bool    GANY      (bool)      is any element true?",
    "  bool    GALL      (bool)      are all elements true?",
    "  bool    GNTRUE    (bool)      number of true elements",
    "  int     GNFALSE   (bool)      number of false elements",
    "  numeric GSUM      (numeric)   sum of all elements",
    "  numeric GSUMSQR   (numeric)   sum of all squared elements   aka GSUMSQUARE",
    "  numeric GPRODUCT  (numeric)   product of all elements",
    "  real    GMIN      (real)      minimum of all elements",
    "  real    GMAX      (real)      maximum of all elements",
    "  numeric GMEAN     (numeric)   mean of all elements          aka GAVG",
    "  double  GVARIANCE (numeric)   population variance              complex variance",
    "  double  GSAMPLEVARIANCE(numeric)  sample variance              uses absolute value",
    "  double  GSTDDEV   (numeric)   population standard deviation    (is sum of real and",
    "  double  GSAMPLESTDDEV(numeric)    sample standard deviation     imaginary variances)",
    "  double  GRMS      (real)      root-mean-square",
    "  double  GMEDIAN   (real)      median (the middle element)",
    "  double  GFRACTILE (real, fraction)   element at given fraction",
    "",
    "The following functions result in an array and operate element by element",
    "  GANYS       GALLS       GNTRUES     GNFALSES",
    "  GSUMS       GSUMSQRS    GPRODUCTS   GMINS      GMAXS           GMEANS",
    "  GVARIANCES  GSAMPLEVARIANCES        GSTDDEVS   GSAMPLESTDDEVS  GRMSS",
    "",
    "The following functions result in an array",
    "  double  GHIST  (data, nbin, start, end)   histogram of the data",
    "  anytype GSTACK (anytype)    stack the data to an array      aka GAGGR"
  };

  const char* positionHelp[] = {
      "Position types:",
      "    ITRF",
      "    WGS84"
  };

  const char* epochHelp[] = {
      "Epoch types:",
      "    LAST           Local Apparent Sidereal Time",
      "    LMST           Local Mean Sidereal Time",
      "    GMST1, GMST    Greenwich Mean ST1",
      "    GAST           Greenwich Apparent ST",
      "    UT1, UT        Universal Time",
      "    UT2            Universal Time",
      "    UTC            Coordinated Universal Time",
      "    TAI, IAT       International Atomic Time",
      "    TDT, TT, ET    Terrestrial Dynamical Time",
      "    TCG            Geocentric Coordinate Time",
      "    TDB            Barycentric Dynamical Time",
      "    TCB            Barycentric Coordinate Time"
  };

  const char* directionHelp[] = {
    "Direction types:",
    "    J2000       mean equator and equinox at J2000.0 (FK5)",
    "    JNAT        geocentric natural frame",
    "    JMEAN       mean equator and equinox at frame epoch",
    "    JTRUE       true equator and equinox at frame epoch",
    "    APP         apparent geocentric position",
    "    B1950       mean epoch and ecliptic at B1950.0",
    "    B1950_VLA   mean epoch(1979.9)) and ecliptic at B1950.0",
    "    BMEAN       mean equator and equinox at frame epoch",
    "    BTRUE       true equator and equinox at frame epoch",
    "    HADEC       topocentric hourangle and declination",
    "    AZEL        topocentric Azimuth and Elevation (N through E)",
    "    AZELNE      topocentric Azimuth and Elevation (N through E)",
    "    AZELSW      topocentric Azimuth and Elevation (S through W)",
    "    AZELGEO     geodetic Azimuth and Elevation (N through E)",
    "    AZELNEGEO   geodetic Azimuth and Elevation (N through E)",
    "    AZELSWGEO   geodetic Azimuth and Elevation (S through W)",
    "    ECLIPTIC    ecliptic for J2000 equator and equinox",
    "    MECLIPTIC   ecliptic for mean equator of date",
    "    TECLIPTIC   ecliptic for true equator of date",
    "    GALACTIC    galactic coordinates",
    "    SUPERGAL    supergalactic coordinates",
    "    ITRF        coordinates wrt ITRF Earth frame",
    "    TOPO        apparent topocentric position",
    "    ICRS        International Celestial Reference System"
  };

  const char* earthMagneticHelp[] = {
    "EarthMagnetic types:",
    "    IGRF        IGRF model",
    "    J2000       mean equator and equinox at J2000.0 (FK5)",
    "    JNAT        geocentric natural frame",
    "    JMEAN       mean equator and equinox at frame epoch",
    "    JTRUE       true equator and equinox at frame epoch",
    "    APP         apparent geocentric position",
    "    B1950       mean epoch and ecliptic at B1950.0",
    "    B1950_VLA   mean epoch(1979.9)) and ecliptic at B1950.0",
    "    BMEAN       mean equator and equinox at frame epoch",
    "    BTRUE       true equator and equinox at frame epoch",
    "    HADEC       topocentric hourangle and declination",
    "    AZEL        topocentric Azimuth and Elevation (N through E)",
    "    AZELNE      topocentric Azimuth and Elevation (N through E)",
    "    AZELSW      topocentric Azimuth and Elevation (S through W)",
    "    AZELGEO     geodetic Azimuth and Elevation (N through E)",
    "    AZELNEGEO   geodetic Azimuth and Elevation (N through E)",
    "    AZELSWGEO   geodetic Azimuth and Elevation (S through W)",
    "    ECLIPTIC    ecliptic for J2000 equator and equinox",
    "    MECLIPTIC   ecliptic for mean equator of date",
    "    TECLIPTIC   ecliptic for true equator of date",
    "    GALACTIC    galactic coordinates",
    "    SUPERGAL    supergalactic coordinates",
    "    ITRF        coordinates wrt ITRF Earth frame",
    "    TOPO        apparent topocentric position",
    "    ICRS        International Celestial Reference System"
  };

  const char* frequencyHelp[] = {
    "Frequency types",
    "    REST     Rest frequency",
    "    LSRD     Local Standard of Rest (J2000) - dynamical definition (IAU, [9,12,7] km/s in galactic coordinates)",
    "    LSRK     LSR as kinematical (radio) definition - 20.0 km/s in direction ra,dec = [270,+30] deg (B1900.0)",
    "    BARY     Barycentric (J2000)",
    "    GEO      Geocentric",
    "    TOPO     Topocentric",
    "    GALACTO  Galacto centric (with rotation of 220 km/s in direction l,b = [90,0] deg",
    "    LGROUP   Local group velocity -- 308km/s towards l,b = [105,-7] deg (F. Ghigo)",
    "    CMB      CMB velocity -- 369.5km/s towards l,b = [264.4, 48.4] deg (F. Ghigo)"
  };

  const char* radialVelocityHelp[] = {
    "RadialVelocity types",
    "    LSRD     Local Standard of Rest (J2000) - dynamical definition (IAU, [9,12,7] km/s in galactic coordinates)",
    "    LSRK     LSR as kinematical (radio) definition - 20.0 km/s in direction ra,dec = [270,+30] deg (B1900.0)",
    "    BARY     Barycentric (J2000)",
    "    GEO      Geocentric",
    "    TOPO     Topocentric",
    "    GALACTO  Galacto centric (with rotation of 220 km/s in direction l,b = [90,0] deg",
    "    LGROUP   Local group velocity -- 308km/s towards l,b = [105,-7] deg (F. Ghigo)",
    "    CMB      CMB velocity -- 369.5km/s towards l,b = [264.4, 48.4] deg (F. Ghigo)"
  };

  const char* dopplerHelp[] = {
    "Doppler types (with F = f/f0, the frequency ratio)",
    "    Z, OPTICAL      -1 + 1/F",
    "    RATIO           F",
    "    RADIO           1  - F",
    "    BETA, TRUE      (1 - F**2)/(1 + F**2)",
    "    RELATIVISTIC = BETA (= v/c)",
    "    GAMMA           (1 + F**2)/2F"
  };


  String TaQLShow::getInfo (const Vector<String>& parts,
                            const TaQLStyle& style)
  {
    // parts contains the possible command, type and subtypes.
    if (parts.empty()) {
      return getHelp (infoHelp);
    }
    String cmd(parts[0]);
    cmd.downcase();
    String type;
    if (parts.size() > 1) type = parts[1];
    String origType(type);
    type.downcase();
    if (cmd == "table") {
      return showTable (parts);
    } else if (cmd == "command"  ||  cmd == "commands") {
      return showCommand (type);
    } else if (cmd == "expr"  ||  cmd == "expression") {
      return getHelp (exprHelp);
    } else if (cmd == "oper"  ||  cmd == "operator"  ||
               cmd == "operators") {
      return getHelp (operHelp);
    } else if (cmd == "const"  ||  cmd == "constant"  ||
               cmd == "constants") {
      return getHelp (constHelp);
    } else if (cmd == "dtype"  ||  cmd == "datatype"  ||
               cmd == "datatypes") {
      return getHelp (dtypeHelp);
    } else if (cmd == "tabopt"  ||  cmd == "tableoption"  ||
               cmd == "tableoptions") {
      return getHelp (taboptHelp);
    } else if (cmd == "dminfo") {
      return getHelp (dminfoHelp);
    } else if (cmd == "set"  ||  cmd == "interval"  ||
               cmd == "sets"  ||  cmd == "intervals") {
      return getHelp (setHelp);
    } else if (cmd == "func"  ||  cmd == "function"  ||
               cmd == "functions") {
      return showFuncs (type, parts, style);
    } else if (cmd == "meastype"  ||  cmd == "meastypes") {
      return showMeasTypes (type);
    } else if (cmd == "unit"  ||  cmd == "units") {
      return showUnits (origType);
    }
    throw TableInvExpr (cmd + " is an unknown SHOW command");
  }

  String TaQLShow::showTable (const Vector<String>& parts)
  {
    AlwaysAssert (parts.size() < 2, AipsError);
    return getHelp (tableHelp);
  }

  String TaQLShow::showCommand (const String& cmd)
  {
    if (cmd.empty()) {
      return getHelp (commandHelp);
    } else if (cmd == "select") {
      return getHelp (selectHelp);
    } else if (cmd == "calc") {
      return getHelp (calcHelp);
    } else if (cmd == "update") {
      return getHelp (updateHelp);
    } else if (cmd == "insert") {
      return getHelp (insertHelp);
    } else if (cmd == "delete") {
      return getHelp (deleteHelp);
    } else if (cmd == "create") {
      return getHelp (createHelp);
    } else if (cmd == "alter") {
      return getHelp (alterHelp);
    } else if (cmd == "count") {
      return getHelp (countHelp);
    }
    throw TableInvExpr (cmd +
                        " is an unknown command for 'show command <command>'\n"
                        "   use select, calc, update, insert, delete, create,"
                        " alter or count\n");
  }

  String TaQLShow::showFuncs (const String& type,
                              const Vector<String>& parts,
                              const TaQLStyle& style)
  {
    if (type.empty()  ||  type == "all") {
      return getHelp (allFuncHelp);
    } else if (type == "math") {
      return getHelp (mathFuncHelp);
    } else if (type == "conversion"  ||  type == "conv") {
      return getHelp (convFuncHelp);
    } else if (type == "logical") {
      return getHelp (logicalFuncHelp);
    } else if (type == "datetime") {
      return getHelp (dateTimeFuncHelp);
    } else if (type == "string") {
      return getHelp (stringFuncHelp);
    } else if (type == "array") {
      return getHelp (arrayFuncHelp);
    } else if (type == "reduce") {
      return getHelp (reduceFuncHelp);
    } else if (type == "astro") {
      return getHelp (astroFuncHelp);
    } else if (type == "misc") {
      return getHelp (miscFuncHelp);
    } else if (type == "aggr") {
      return getHelp (aggrFuncHelp);
    }
    try {
      TableExprNodeSet operands;
      String ftype;
      if (parts.size() > 2) ftype = parts[2];
      operands.add (TableExprNodeSetElem(ftype));
      // Make a node for the UDF library given by type to use its help function.
      // It takes an argument (which can be empty).
      TableExprNode node = TableExprNode::newUDFNode (type+".help",
                                                      operands,
                                                      TableExprInfo(), style);
      return node.getString(0);   // get the UDF help info as a string
    } catch (const std::exception&) {
      return type + " is an unknown type in 'show functions <type>'\n"
        "  (maybe an unknown UDF library)\n";
    }
  }


  void TaQLShow::showUnitKind (ostream& os, const UnitVal& kind,
                               const map<String, UnitName>& units)
  {
    for (map<String,UnitName>::const_iterator iter = units.begin();
         iter != units.end(); ++iter) {
      if (Unit(iter->first).getValue() == kind) {
        os << "    " << iter->second << endl;
      }
    }
  }

  String TaQLShow::showUnits (const String& type)
  {
    ostringstream os;
    if (type.empty()) {
      UnitMap::list (os);
    } else if (type == "prefix") {
      UnitMap::listPref (os);
    } else {
      UnitVal kind;
      if (type == "length") {
        kind = UnitVal::LENGTH;
      } else if (type == "mass") {
        kind = UnitVal::MASS;
      } else if (type == "time") {
        kind = UnitVal::TIME;
      } else if (type == "current") {
        kind = UnitVal::CURRENT;
      } else if (type == "temperature") {
        kind = UnitVal::TEMPERATURE;
      } else if (type == "intensity") {
        kind = UnitVal::INTENSITY;
      } else if (type == "molar") {
        kind = UnitVal::MOLAR;
      } else if (type == "angle") {
        kind = UnitVal::ANGLE;
      } else if (type == "solidangle") {
        kind = UnitVal::SOLIDANGLE;
      } else {
        try {
          Unit unit(type);
          kind = unit.getValue();
        } catch (const AipsError&) {
          throw TableInvExpr ("Unknown kind or unit given in command "
                              "'show units " + type + "'\nUse 'show' to "
                              "show the valid kinds");
        }
      }
      showUnitKind (os, kind, UnitMap::giveDef());
      showUnitKind (os, kind, UnitMap::giveSI());
      showUnitKind (os, kind, UnitMap::giveCust());
      showUnitKind (os, kind, UnitMap::giveUser());
    }
    return os.str();
  }

  String TaQLShow::showMeasTypes (const String& type)
  {
    // Because libtables cannot be dependent on libmeasures,
    // no Measures functions can be used to show the types.
    if (type.empty()) {
      return getHelp (positionHelp) +
        getHelp (epochHelp) +
        getHelp (directionHelp) +
        getHelp (earthMagneticHelp) +
        getHelp (frequencyHelp) +
        getHelp (radialVelocityHelp) +
        getHelp (dopplerHelp) +
        "\nSee also 'show functions meas "
        "pos|epoch|dir|em|freq|radvel|doppler\n";
    } else if (type == "pos"  ||  type == "position") {
      return getHelp (positionHelp);
    } else if (type == "epoch") {
      return getHelp (epochHelp);
    } else if (type == "dir"  ||  type == "direction") {
      return getHelp (directionHelp);
    } else if (type == "em"  ||  type == "earthmagnetic") {
      return getHelp (earthMagneticHelp);
    } else if (type == "freq"  ||  type == "frequency") {
      return getHelp (frequencyHelp);
    } else if (type == "rv"  ||  type == "radvel"  ||  type == "radialvelocity") {
      return getHelp (radialVelocityHelp);
    } else if (type == "doppler") {
      return getHelp (dopplerHelp);
    }
    throw TableInvExpr (type +
                        " is an unknown type for command "
                        "'show meastypes <type>'");
  }

} //# NAMESPACE CASACORE - END
