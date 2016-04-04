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
    "  show command(s) [command]         syntax of TaQL commands",
    "  show expr(essions)                how to form an expression",
    "  show oper(ators)                  available operators",
    "  show sets                         how to specify sets and intervals",
    "  show const(ants)                  how to specify constant scalars and arrays",
    "  show functions [type] [subtype]   available functions",
    "       possible types: math, logical, conv(ersion), datetime, string",
    "                       array, reduce, astro, misc, aggr(egate)",
    "                name of UDF libraries (e.g., show functions mscal)",
    "  show meastypes [type]             available measure types",
    "       possible types: epoch, position, direction",
    "  show units [kind]                 available units and/or prefixes",
    "       possible kinds: prefix, length, time, angle, temperature,",
    "                       current, intensity, molar, mass, solidangle",
    "       If a unit is given as kind, all corresponding units are shown.",
    "       Note that TaQL can convert between ANGLE and TIME.",
    "",
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
    "    [FROM table_list] [WHERE expression]",
    "    [GROUPBY expression_list] [HAVING expression]",
    "    [ORDERBY [DISTINCT] sort_list] [LIMIT expression] [OFFSET expression]",
    "    [GIVING table [AS options] | set] [DMINFO datamanagers]",
    "",
    "Calculate an expression, possibly using table columns.",
    "  CALC expression [FROM table_list]",
    "",
    "Update part of the contents of a table.",
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
    "  CREATE TABLE table [AS options]",
    "    [column_specs]",
    "    [LIMIT ...]",
    "    [DMINFO datamanagers]",
    "",
    "Alter a table (add/rename/remove columns/keywords; add rows).",
    "  ALTER TABLE table",
    "    [ADD COLUMN [column_specs] [DMINFO datamanagers]",
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
    "Use 'show command <command>' for more information about a command.",
    "    'show expr(essions)'     for more information about forming expressions.",
    "See http://casacore.github.io/casacore-notes/199.html for full info.",
  };

  const char* selectHelp[] = {
    "SELECT",
    "  [[DISTINCT] expression_list]",
    "  [INTO table [AS options]]",
    "  [FROM table_list]",
    "  [WHERE expression]",
    "  [GROUPBY expression_list]",
    "  [HAVING expression]",
    "  [ORDERBY [DISTINCT] [ASC|DESC] sort_list]",
    "  [LIMIT expression] [OFFSET expression]",
    "  [GIVING table [AS options] | set]",
    "  [DMINFO datamanagers]",
    "",
    "[DISTINCT] expression-list",
    "  Optional list of expressions (possibly containing aggregate functions).",
    "  If not given, all columns of the first table are selected.",
    "  It is possible to use pattern matching to include or exclude columns.",
    "    For example:   !~p/*DATA/    to exclude all columns ending in DATA",
    "  An expression can be followed by 'AS newname [datatype]' to define a name for",
    "  the resulting column and its data type (defaults to the expression type.",
    "  Possible column data types:",
    "      B          BOOL       BOOLEAN",
    "      U1         UCHAR      BYTE",
    "      I2         SHORT      SMALLINT",
    "      U2   UI2   USHORT     USMALLINT",
    "      I4         INT        INTEGER",
    "      U4   UI4   UINT       UINTEGER",
    "      R4   FLT   FLOAT",
    "      R8   DBL   DOUBLE",
    "      C4   FC    FCOMPLEX   COMPLEX",
    "      C8   DC    DCOMPLEX", 
    "      S          STRING",
    "  DISTINCT (or UNIQUE) removes output rows with equal values.",
    "",
    "INTO table [AS options]",
    "  Name of resulting table; if not given, no output table is created.",
    "  Possible options:",
    "    TYPE='value' specifies the table type.",
    "      PLAIN   = make a persistent table, thus a true copy of all",
    "                selected rows/columns.",
    "      SCRATCH = as plain, but only as a temporary table.",
    "      MEMORY  = as plain, but keep everything in memory.",
    "      If TYPE is not given, a reference table is made if no expressions",
    "      are given in the SELECT clause, otherwise a plain table is made.",
    "    ENDIAN='value' specifies the endianness.",
    "      BIG    = big endian",
    "      LITTLE = little endian",
    "      LOCAL  = native endianness of the machine being used",
    "      AIPSRC = as defined in the .casarc file (usually defaults to LOCAL)",
    "      If ENDIAN is not given, it defaults to AIPSRC.",
    "    STORAGE='value' specifies the storage type.",
    "      SEPFILE   = store as separate files (the old Casacore table format)",
    "      MULTIFILE = combine all storage manager files into a single file",
    "      MULTIHDF5 = as MULTIFILE, but use HDF5 instead of a regular file",
    "      DEFAULT   = use SEPFILE (might change in a future Casacore version)",
    "      AIPSRC    = as defined in the .casarc file (usually defaults to DEFAULT)",
    "      If STORAGE is not given, it defaults to AIPSRC.",
    "    BLOCKSIZE=n   specifies the blocksize to use for MULTIFILE or MULTIHDF5",
    "    OVERWRITE=T|F overwrite an existing table? Default is T",
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
    "WHERE expression",
    "  An expression resulting in a boolean scalar telling if a row is selected.",
    "  If not given, all rows will be selected.",
    "",
    "GROUPBY expression_list",
    "  It can be used to group rows with equal values for the given",
    "  expressions (which must result in scalar values).",
    "  Often aggregate functions (like gsum) are used in the SELECT and/or",
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
    "  can be used by expert users to define data managers for the output columns."
  };

  const char* calcHelp[] = {
    "CALC expression [FROM table_list]",
    "",
    "This command evaluates the given expression, which can contain columns from",
    "the given tables. It is basically the same as",
    "  SELECT expression [FROM table_list]",
    "but does not create an output table as SELECT always does."
  };

  const char* updateHelp[] = {
    "UPDATE table_list",
    "  SET update_list",
    "  [FROM table_list]",
    "  [WHERE expression]",
    "  [ORDERBY [DISTINCT] [ASC|DESC] sort_list]",
    "  [LIMIT expression] [OFFSET expression]",
    "",
    "This command makes it possible to update values in selected rows and columns",
    "of the first table in the table list.",
    ""
  };

  const char* insertHelp[] = {
    "INSERT INTO table_list SET column=expr, column=expr, ...",
    "INSERT INTO table_list [(column_list)] VALUES (expr_list)",
    "INSERT INTO table_list [(column_list)] SELECT_command",
    "'show command insert' not implemented yet"
  };

  const char* deleteHelp[] = {
    "DELETE FROM table_list",
    "  [WHERE ...] [ORDERBY ...] [LIMIT ...] [OFFSET ...]",
    "'show command' delete' not implemented yet"
  };

  const char* createHelp[] = {
    "CREATE TABLE table [AS options]",
    "  [column_specs]",
    "  [LIMIT ...]",
    "  [DMINFO datamanagers]",
    "'show command create' not implemented yet"
  };

  const char* alterHelp[] = {
    "ALTER TABLE table",
    "  [ADD COLUMN [column_specs] [DMINFO datamanagers]",
    "  [RENAME COLUMN old TO new, old TO new, ...]",
    "  [DROP COLUMN col, col, ...]",
    "  [SET KEYWORD key=value, key=value, ...]",
    "  [COPY KEYWORD key=other, key=other, ...]",
    "  [RENAME KEYWORD old TO new, old TO new, ...]",
    "  [DROP KEYWORD key, key, ...]",
    "  [ADD ROW nrow]",
    "'show command alter' not implemented yet"
  };

  const char* countHelp[] = {
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
    "A TaQL expression can use scalar and/or arrays (like numpy).",
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
    "    == != >  >= <  <=  ~= !~= IN INCONE BETWEEN EXISTS LIKE  ~  !~",
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
    "    **          power, right-associative, thus 2**1**2 = 2",
    "    ~           if unary, bit-wise complement",
    "    /           real division, thus 1/2 = 0.5",
    "    //          integer division, thus 1//2 = 0",
    "    %           modulo",
    "    +           also string concat; also add days to datetime",
    "    -           also for datetime (results in unit d)",
    "    &           bit-wise and",
    "    ^           bit-wise xor",
    "    |           bit-wise or",
    "    ~= !~=      (not) about equal (relative to 1e-5)",
    "    ~ !~ LIKE   pattern match",
    "    IN          is left hand an element in the right hand?",
    "    INCONE      cone searching",
    "    EXISTS      does subquery have at least 1 match?"
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
    "",
    "N-dim array of those data types (except regex) like:",
    "             [1,2,3] (1-dim)  or  [[1,2,3],[4,5e3,6]] (2-dim)",
    "          or using function ARRAY",
    "",
    "Masked array (True value means bad (as in numpy)):",
    "             array[mask]  like  [1,2,3][[T,F,T]]",
    "          or using function MARRAY"
  };

  const char* setHelp[] = {
    "A set is a series of values, ranges and/or intervals enclosed in brackets.",
    "Often the IN operator is used on a set, but a set can also be used as an array.",
    "",
    "A value can be of type integer, double, complex, datetime or string.",
    "Numeric data types can be mixed; the 'highest' type is used.",
    "",
    "A range is a series of values written as start:end:step",
    "  'start' can be left out making it unbounded (from -infinity)",
    "  'end'   can be left out making it unbounded (till +infinity)",
    "  ':step' can be left out and defaults to 1",
    "start and end can be integer, double, or datetime",
    "step must be integer or double",
    "",
    "An interval is a continuous set of real values with optional bounds.",
    "If a bound is given, it can be open or closed.",
    "An interval can be given in two ways:",
    "  Using curly braces (closed bound) and arrows (open bound)",
    "    bounded:     {1,2}   <1,2>   {1,2>   <1,2}",
    "    unbounded:   {1,}    <1,>    {,2>    <,2}",
    "  Using  a=:=b (closed bounds)  and  a<:<b (open bounds)",
    "    bounded:     1=:=2   1<:<2   1=:<2   1<:=2",
    "    unbounded:   1=:     1<:     :<2     :=2",
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
    "  strlength   upcase      downcase    capitalize",
    "  trim        ltrim       rtrim       substr      replace",
    "  regex       pattern     sqlpattern",
    "array functions:",
    "  array       ndim        nelem       shape",
    "  transpose   resize      diagonal",
    "  nullarray   marray      arraydata   mask",
    "  flatten     negatemask  replacemasked           replaceunmasked",
    "reduce functions:",
    "  sum         product     sumsqr      min         max",
    "  mean        variance    stddev      avdev       rms",
    "  median      fractile    any         all         ntrue       nfalse",
    "    plural, running and boxed forms of above reduce functions",
    "astro functions:",
    "  angdist     angdistx    cones       anycone     findcone",
    "    see also 'show func meas' and 'show func mscal'",
    "misc functions:",
    "  rownr       rowid",
    "aggregate functions:",
    "  gmin        gmax        gsum        gproduct    gsumsqr",
    "  gmean       gvariance   gstddev     grms",
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
    "  real    FMOD  (real, real)",
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
    "  string LTRIM      (string)      remove leading whitespace",
    "  string RTRIM      (string)      remove trailing whitespace",
    "  string TRIM       (string)      remove leading/trailing whitespace",
    "  string SUBSTR     (string, int START, int N)    ST<0 means from the end",
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
    " RUNNINGXXX (array, windowSize)   calculates XXX in sliding window over pixel",
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
    "  double  VARIANCE (real)      variance",
    "  double  STDDEV   (real)      standard deviation",
    "  double  AVDEV    (real)      average deviation",
    "  double  RMS      (real)      root-mean-squares",
    "  double  MEDIAN   (real)      median (the middle element)",
    "  double  FRACTILE (real, fraction)   element at given fraction"
  };

  const char* astroFuncHelp[] = {
    "Astronomical functions",
    "",
    "  double ANGDIST  (arg1,arg2)     aka ANGULARDISTANCE",
    "    angular distance (in rad) between corrersponding positions in arg1 and arg2",
    "    arg1 and arg2 must be arrays containing ra/dec or lon/lat pairs",
    "  double ANGDISTX (arg1,arg2)     aka ANGULARDISTANCEX",
    "    same as ANGDIST, but between all positions in arg1 and arg2",
    "  bool   ANYCONE    (source, cones)",
    "    True if source in at least one of the cones",
    "    synonym for operator INCONE",
    "  bool   ANYCONE    (source, conepos, radii)",
    "    same as above, but cone centers and radii are given separately",
    "    each radius is applied to each cone",
    "  int    FINDCONE    (sources, cones)",
    "    index of the first cone containing a source",
    "    if a single source is given, the result is a scalar, otherwise an array",
    "  int    FINDCONE    (sources, conepos, radii)",
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
    "  int ROWID()                    return row number in input table"
  };

  const char* aggrFuncHelp[] = {
    "Aggregate functions operating per group (using GROUPBY)",
    "",
    "The following functions results in a scalar value",
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
    "  double  GVARIANCE (real)      variance",
    "  double  GSTDDEV   (real)      standard deviation",
    "  double  GAVDEV    (real)      average deviation",
    "  double  GRMS      (real)      root-mean-squares",
    "  double  GMEDIAN   (real)      median (the middle element)",
    "  double  GFRACTILE (real, fraction)   element at given fraction",
    "",
    "The following functions results in an array and operate element by element",
    "  GANYS       GALLS       GNTRUES     GNFALSES",
    "  GSUMS       GSUMSQRS    GPRODUCTS   GMINS       GMAXS",
    "  GMEANS      GVARIANCES  GSTDDEVS    GAVDEVS     GRMSS",
    "",
    "The following functions results in a scalar value",
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
    "    ICRS        International Celestial Reference System",
  };

  const char* frequencyHelp[] = {
    "'show meastype frequency' not implemented yet"
  };

  const char* dopplerHelp[] = {
    "'show meastype doppler' not implemented yet"
  };

  const char* radVelHelp[] = {
    "'show meastype radialvelocity' not implemented yet"
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
    throw AipsError (cmd + " is an unknown SHOW command");
  }

  String TaQLShow::showTable (const Vector<String>& parts)
  {
    if (parts.size() < 2  ||  parts[1].empty()) {
      return getHelp (tableHelp);
    }
    Table table(Table::openTable(parts[1]));
    Bool showdm = False;
    Bool showcol = True;
    Bool showsub = False;
    Bool sortcol = False;
    Bool tabkey = False;
    Bool colkey = False;
    for (uInt i=2; i<parts.size(); ++i) {
      String opt(parts[i]);
      opt.downcase();
      Bool fop = True;
      if (opt.size() > 2   &&  opt.substr(0,2) == "no") {
        fop = False;
        opt = opt.substr(2);
      }
      if (opt == "dm") {
        showdm = fop;
      } else if (opt == "col") {
        showcol = fop;
      } else if (opt == "sort") {
        sortcol = fop;
      } else if (opt == "key") {
        tabkey = fop;
        colkey = fop;
      } else if (opt == "tabkey") {
        tabkey = fop;
      } else if (opt == "colkey") {
        colkey = fop;
      } else if (opt == "recur") {
        showsub = fop;
      } else {
        throw AipsError (parts[i] + " is an unknown show table option; use: "
                         "dm col sort key colkey recur");
      }
    }
    std::ostringstream os;
    table.showStructure (os, showdm, showcol, showsub, sortcol);
    table.showKeywords (os, showsub, tabkey, colkey);
    return os.str();
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
    throw AipsError (cmd +
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
      TableExprNode node = TableExprNode::newUDFNode (type+".help",
                                                      operands,
                                                      Table(), style);
      return node.getString(0);
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
          throw AipsError ("Unknown kind or unit given in command "
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
        ///getHelp (frequencyHelp) +
        ///getHelp (dopplerHelp) +
        ///getHelp (radVelHelp) +
        "\nSee also 'show functions meas pos|epoch|dir\n";
      /// "\nSee also 'show functions meas "
      /// "pos|epoch|dir|freq|doppler|radvel\n";
    } else if (type == "pos"  ||  type == "position") {
      return getHelp (positionHelp);
    } else if (type == "epoch") {
      return getHelp (epochHelp);
    } else if (type == "dir"  ||  type == "direction") {
      return getHelp (directionHelp);
    } else if (type == "freq"  ||  type == "frequency") {
      return getHelp (frequencyHelp);
    } else if (type == "doppler") {
      return getHelp (dopplerHelp);
    } else if (type == "radvel"  ||  type == "radialvelocity") {
      return getHelp (radVelHelp);
    }
    throw AipsError (type +
                     " is an unknown type for command "
                     "'show meastypes <type>'");
  }

} //# NAMESPACE CASACORE - END
