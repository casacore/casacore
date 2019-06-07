/*
    TableGram.l: Lexical analyzer for table commands
    Copyright (C) 1994,1995,1996,1997,1998,2001,2003
    Associated Universities, Inc. Washington DC,a USA.

    This library is free software; you can redistribute it and/or modify it
    under the terms of the GNU Library General Public License as published by
    the Free Software Foundation; either version 2 of the License, or (at your
    option) any later version.

    This library is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
    License for more details.

    You should have received a copy of the GNU Library General Public License
    along with this library; if not, write to the Free Software Foundation,
    Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.

    Correspondence concerning AIPS++ should be addressed as follows:
           Internet email: aips2-request@nrao.edu.
           Postal address: AIPS++ Project Office
                           National Radio Astronomy Observatory
                           520 Edgemont Road
                           Charlottesville, VA 22903-2475 USA

    $Id$
*/

/* yy_unput is not used, so let flex not generate it, otherwise picky
   compilers will issue warnings. */
%option nounput

%{
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) result=tableGramInput(buf,max_size)

#undef YY_DECL
#define YY_DECL int TableGramlex (YYSTYPE* lvalp)
%}

/* States to distinguish how some tokens are recognized */
%s STYLEstate
%s EXPRstate
%s GIVINGstate
%s FROMstate
%s CRETABstate
/* Exclusive state meaning that tokens are only recognized in that state */
%x SHOWstate

/* Define all tokens recognized by flex.
   Imaginary part of a Complex value can be given as:   FLOATi
          where i is the letter i or j (in lowercase only).
   In a NAME the backslash can be used to escape special characters like -.
   In that way a name like DATE-OBS can be given as DATE\-OBS.
   Comment is from # till the end of the command.
*/
WHITE1    [ \t\n]
WHITE     {WHITE1}*
NONWHITE  [^ \t\n]+
COMMENT   "#".*
DIGIT     [0-9]
INT       {DIGIT}+
INT2      {DIGIT}{DIGIT}
INT4      {INT2}{INT2}
HEXINT    0[xX][0-9a-fA-F]+
EXP       [Ee][+-]?{INT}
FLOAT     {INT}{EXP}|{INT}"."{DIGIT}*({EXP})?|{DIGIT}*"."{INT}({EXP})?
FLINT     {FLOAT}|{INT}
COMPLEX   {FLINT}[ij]
TRUE      T|([Tt][Rr][Uu][Ee])
FALSE     F|([Ff][Aa][Ll][Ss][Ee])
FLINTUNIT {FLINT}[a-zA-Z]+

MONTH     [A-Za-z]+
/* Date with numeric month must have 2 digits in day and month and 4 in year.
   Can be DMY or YMD with - or / as delimiter. */
DATEH     ({INT2}"-"{INT2}"-"{INT4})|({INT4}"-"{INT2}"-"{INT2})
DATES     {INT4}"/"{INT2}"/"{INT2}
/* Date with alphabetic month has more freedom in day and year */
DATEA     {INT}{MONTH}{INT}|{INT}"-"{MONTH}"-"{INT}
DATE      {DATEA}|{DATEH}|{DATES}
/* Time part of a datetime can use h/m or : */
DTIMEH    {INT}[hH]({INT}?([mM]({FLINT})?)?)?
DTIMEC    {INT}":"({INT}?(":"({FLINT})?)?)?
DTIME     {DTIMEH}|{DTIMEC}
/* - / space or T can be used as separator between date and time; optional ISO Z */
DATETIME  {DATE}([-/ T]{DTIME}(Z?))?

/* Sky position as HMS or DMS; dots can be used instead of DMS (as MVAngle allows).
   Positions/times with colons cannot be allowed, because they interfere
   with the interval syntax. It is only possible when preceeded by a date. */
POSHM     {INT}[hH]{INT}[mM]{FLINT}?
POSDM     {INT}[dD]{INT}[mM]{FLINT}?
POSD      {INT}"."{INT}"."{FLINT}
TIME      {POSHM}|{POSDM}|{POSD}

/* Strings can be quoted using single or double quotes */
QSTRING   \"[^\"\n]*\"
ASTRING   \'[^\'\n]*\'
/* Recognize a quoted string without an end-quote (to give an error message) */
UQSTRING   \"[^\"\n]*\n
UASTRING   \'[^\'\n]*\n
STRING    ({QSTRING}|{ASTRING})+
USTRING   ({UQSTRING}|{UASTRING})+
/* Recognize all reserved words (can be a mix of lowercase and uppercase) */
UNION     [Uu][Nn][Ii][Oo][Nn]
INTERSECT [Ii][Nn][Tt][Ee][Rr][Ss][Ee][Cc][Tt]
EXCEPT    ([Ee][Xx][Cc][Ee][Pp][Tt])|([Mm][Ii][Nn][Uu][Ss])
STYLE     [Uu][Ss][Ii][Nn][Gg]{WHITE}[Ss][Tt][Yy][Ll][Ee]{WHITE1}
TIMEWORD  [Tt][Ii][Mm][Ee]
SHOW      ([Ss][Hh][Oo][Ww])|([Hh][Ee][Ll][Pp])
WITH      [Ww][Ii][Tt][Hh]
SELECT    [Ss][Ee][Ll][Ee][Cc][Tt]
UPDATE    [Uu][Pp][Dd][Aa][Tt][Ee]
INSERT    [Ii][Nn][Ss][Ee][Rr][Tt]
DELETE    [Dd][Ee][Ll][Ee][Tt][Ee]
DROP      ([Dd][Rr][Oo][Pp])|{DELETE}
ADD       [Aa][Dd][Dd]
RENAME    [Rr][Ee][Nn][Aa][Mm][Ee]
SET       [Ss][Ee][Tt]
COPY      [Cc][Oo][Pp][Yy]
COLUMN    [Cc][Oo][Ll][Uu][Mm][Nn]([Ss])?
KEYWORD   [Kk][Ee][Yy][Ww][Oo][Rr][Dd]([Ss])?
ROW       [Rr][Oo][Ww]([Ss])?
COUNT     [Cc][Oo][Uu][Nn][Tt]
COUNTALL  [Gg]{COUNT}{WHITE}"("{WHITE}"*"?{WHITE}")"
CALC      [Cc][Aa][Ll][Cc]
CREATETAB [Cc][Rr][Ee][Aa][Tt][Ee]{WHITE}[Tt][Aa][Bb][Ll][Ee]{WHITE1}
ALTERTAB  [Aa][Ll][Tt][Ee][Rr]{WHITE}[Tt][Aa][Bb][Ll][Ee]{WHITE1}
/* Optionally the ALTER TABLE subcommands can be separated by commas;
   they need a space after the subcommand name. */
ADDCOL    ,?{WHITE}{ADD}{WHITE}{COLUMN}{WHITE1}
RENAMECOL ,?{WHITE}{RENAME}{WHITE}{COLUMN}{WHITE1}
DROPCOL   ,?{WHITE}{DROP}{WHITE}{COLUMN}{WHITE1}
SETKEY    ,?{WHITE}{SET}{WHITE}{KEYWORD}{WHITE1}
COPYKEY   ,?{WHITE}{COPY}{WHITE}{KEYWORD}{WHITE1}
RENAMEKEY ,?{WHITE}{RENAME}{WHITE}{KEYWORD}{WHITE1}
DROPKEY   ,?{WHITE}{DROP}{WHITE}{KEYWORD}{WHITE1}
ADDROW    ,?{WHITE}{ADD}{WHITE}{ROW}{WHITE1}
DMINFO    [Dd][Mm][Ii][Nn][Ff][Oo]
VALUES    [Vv][Aa][Ll][Uu][Ee][Ss]
FROM      [Ff][Rr][Oo][Mm]
WHERE     [Ww][Hh][Ee][Rr][Ee]
ORDERBY   [Oo][Rr][Dd][Ee][Rr]{WHITE}[Bb][Yy]{WHITE1}
NODUPL1   [Nn][Oo][Dd][Uu][Pp][Ll][Ii][Cc][Aa][Tt][Ee][Ss] 
DISTINCT  [Dd][Ii][Ss][Tt][Ii][Nn][Cc][Tt]
UNIQUE    [Uu][Nn][Ii][Qq][Uu][Ee]
NODUPL    {NODUPL1}|{DISTINCT}|{UNIQUE}
GIVING1   [Gg][Ii][Vv][Ii][Nn][Gg]
SAVETO    [Ss][Aa][Vv][Ee]{WHITE}[Tt][Oo]{WHITE1}
GIVING    {GIVING1}|{SAVETO}
INTO      [Ii][Nn][Tt][Oo]
SUBTABLES [Ss][Uu][Bb][Tt][Aa][Bb][Ll][Ee][Ss]
GROUPBY   [Gg][Rr][Oo][Uu][Pp]{WHITE}[Bb][Yy]{WHITE1}
GROUPROLL {GROUPBY}{WHITE}[Rr][Oo][Ll][Ll][Uu][Pp]{WHITE1}
HAVING    [Hh][Aa][Vv][Ii][Nn][Gg]
JOIN      [Jj][Oo][Ii][Nn]
ON        [Oo][Nn]
ASC       [Aa][Ss][Cc]
DESC      [Dd][Ee][Ss][Cc]
LIMIT     ([Ll][Ii][Mm][Ii][Tt])|([Tt][Oo][Pp])
OFFSET    [Oo][Ff][Ff][Ss][Ee][Tt]
BETWEEN   [Bb][Ee][Tt][Ww][Ee][Ee][Nn]
EXISTS    [Ee][Xx][Ii][Ss][Tt][Ss]
LIKE      [Ll][Ii][Kk][Ee]
ILIKE     [Ii][Ll][Ii][Kk][Ee]
IN        [Ii][Nn]
INCONE    [Ii][Nn]{WHITE}[Cc][Oo][Nn][Ee]{WHITE1}
AS        [Aa][Ss]
TO        [Tt][Oo]
AND       [Aa][Nn][Dd]
OR        [Oo][Rr]
XOR       [Xx][Oo][Rr]
NOT       [Nn][Oo][Tt]
ALL       [Aa][Ll][Ll]
/* To distinguish keyword ALL from function ALL, the latter has a parenthesis */
ALLFUNC   {ALL}{WHITE}"("
/* A basic name is alphanumeric (and underscores) or is escaped with a backslash */
NAME      \\?[A-Za-z_]([A-Za-z_0-9]|(\\.))*
/* A field name is a name with dots or double colons */
NAMEFLD   ({NAME}".")?{NAME}?("::")?{NAME}("."{NAME})*
/* A temporary table name can be followed by field names */
TEMPTAB   [$]{INT}(("."{NAME})?"::"{NAME}("."{NAME})*)?
/* A table name can contain about every character
   (but is recognized in specific states only).
   It can be a mix of quoted and unquoted strings (with escaped characters).
   NOTE: when changing NAMETABC, also change TaQLNodeRep::addEscape. */
NAMETABC  ([A-Za-z0-9_./+\-~$@:]|(\\.))+
NAMETAB   {NAMETABC}|(({STRING}|{NAMETABC})+)
/* A UDFlib synonym */
UDFLIBSYN {NAME}{WHITE}"="{WHITE}{NAME}
/* A regular expression can be delimited by / % or @ optionall=y followed by i
   to indicate case-insensitive matching.
     m is a partial match (match if part of string matches the regex)
     f is a full match
     p is a pattern match (glob-style pattern)
*/
REGEX1    m"/"[^/]+"/"
REGEX2    m%[^%]+%
REGEX3    m@[^@]+@
REGEX     {REGEX1}|{REGEX2}|{REGEX3}
FREGEX1   f"/"[^/]+"/"
FREGEX2   f%[^%]+%
FREGEX3   f@[^@]+@
FREGEX    {FREGEX1}|{FREGEX2}|{FREGEX3}
PATT1     p\/[^/]+\/
PATT2     p%[^%]+%
PATT3     p@[^@]+@
PATT      {PATT1}|{PATT2}|{PATT3}
PATTEX    ({REGEX}|{FREGEX}|{PATT})i?
/* String distance is similar; has options b i and nn (distance) in any order */
DIST1     d\/[^/]+\/
DIST2     d%[^%]+%
DIST3     d@[^@]+@
DISTOPT   [bi]*{INT}?[bi]*
DISTEX    ({DIST1}|{DIST2}|{DIST3}){DISTOPT}
/* Part of the pattern is the operator */
OPERREX   "!"?"~"
PATTREX   {OPERREX}{WHITE}({PATTEX}|{DISTEX})

%%
 /* The command to be analyzed is something like:
        SELECT column-list FROM table-list WHERE expression
                          ORDER BY column-list GIVING table
    The WHERE, ORDER BY, and GIVING parts are optional.
    Elements in a list are separated by commas.
    A table-name can be only a table file name or a table file name
    followed by whitespace and an alphanumeric name. That 2nd name
    serves as a shorthand for possible later use in the field name.
    A table name can be given in the FROM part and in the giving PART.
    These are indicated by the FROM/GIVING/CRETABstate, because a table name
    can contain special characters like -.
    A table name can also be $nnn indicating a temporary table. It can optionally
    be followed by :: and the name of a subtable of that temporary table.
    In a subquery care must be taken that the state is switched back to
    EXPRstate, because a FROM can be the last part in a subquery and
    because a set can be specified in the GIVING part.
    This is done by setting the state when parentheses or square brackets
    are found. ( and [ indicate the beginning of a set(subquery).
    ) and ] indicate the end of subquery.

    The order in the following list is important, since, for example,
    the word "giving" must be recognized as GIVING and not as NAME.
    Similarly, an alphanumeric string must be recognized as NAME
    and not as NAMETAB or NAMEFLD.

    TableGramText is the char* pointer giving the start of the token recognized.
    yyleng gives the length of the token recognized by flex.
    tableGramPosition() is an Int& keeping track of the position in the command
    string for error reporting in TableGram.cc.
    Note that lvalp is defined at the beginning of this file as the argument
    to TableGramlex (in the YY_DECL definition). The possible lvalp fields
    (such as lvalp->val) are defined in the union in TableGram.yy.
 */

 /* In the SHOW command any word (such as SELECT) is allowed */
<SHOWstate>{NAME} {
            tableGramPosition() += yyleng;
            lvalp->val = new TaQLConstNode(
                new TaQLConstNodeRep (tableGramRemoveEscapes (TableGramtext)));
            TaQLNode::theirNodesCreated.push_back (lvalp->val);
	    return NAME;
	  }

{UNION}  {
            tableGramPosition() += yyleng;
            throw (TableInvExpr ("UNION is not supported yet"));
	  }
{INTERSECT}  {
            tableGramPosition() += yyleng;
            throw (TableInvExpr ("INTERSECT is not supported yet"));
	  }
{EXCEPT}  {
            tableGramPosition() += yyleng;
	    return EXCEPT;
	  }
{STYLE}   {
            tableGramPosition() += yyleng;
            BEGIN(STYLEstate);
	    return STYLE;
	  }
{SELECT}  {
            tableGramPosition() += yyleng;
	    BEGIN(EXPRstate);
	    return SELECT;
	  }
{UPDATE}  {
            tableGramPosition() += yyleng;
	    BEGIN(FROMstate);
	    return UPDATE;
	  }
{SET}  {
            tableGramPosition() += yyleng;
	    BEGIN(EXPRstate);
	    return UPDSET;
	  }
{INSERT}  {
            tableGramPosition() += yyleng;
	    BEGIN(FROMstate);
	    return INSERT;
	  }
{VALUES}  {
            tableGramPosition() += yyleng;
	    BEGIN(EXPRstate);
	    return VALUES;
          }
{DELETE}  {
            tableGramPosition() += yyleng;
	    return DELETE;
	  }
{COUNT}   {
            tableGramPosition() += yyleng;
	    BEGIN(EXPRstate);
	    return COUNT;
	  }
{COUNTALL} {
            tableGramPosition() += yyleng;
	    BEGIN(EXPRstate);
	    return COUNTALL;
	  }
{CALC}  {
            tableGramPosition() += yyleng;
	    BEGIN(EXPRstate);
	    return CALC;
	  }
{CREATETAB} {
            tableGramPosition() += yyleng;
	    BEGIN(CRETABstate);
	    return CREATETAB;
	  }
{ALTERTAB} {
            tableGramPosition() += yyleng;
	    BEGIN(CRETABstate);
	    return ALTERTAB;
	  }
{ADDCOL}  {
            tableGramPosition() += yyleng;
	    BEGIN(EXPRstate);
	    return ADDCOL;
          } 
{RENAMECOL} {
            tableGramPosition() += yyleng;
	    BEGIN(EXPRstate);
	    return RENAMECOL;
          } 
{DROPCOL} {
            tableGramPosition() += yyleng;
	    BEGIN(EXPRstate);
	    return DROPCOL;
          } 
{SETKEY}  {
            tableGramPosition() += yyleng;
	    BEGIN(EXPRstate);
	    return SETKEY;
          } 
{COPYKEY} {
            tableGramPosition() += yyleng;
	    BEGIN(EXPRstate);
	    return COPYKEY;
          } 
{RENAMEKEY} {
            tableGramPosition() += yyleng;
	    BEGIN(EXPRstate);
	    return RENAMEKEY;
          } 
{DROPKEY} {
            tableGramPosition() += yyleng;
	    BEGIN(EXPRstate);
	    return DROPKEY;
          } 
{ADDROW}  {
	    BEGIN(EXPRstate);
            tableGramPosition() += yyleng;
	    return ADDROW;
          } 
{DMINFO}  {
            tableGramPosition() += yyleng;
	    BEGIN(EXPRstate);
	    return DMINFO;
	  }
{WITH}    {
            tableGramPosition() += yyleng;
            BEGIN(FROMstate);
	    return WITH;
	  }
{FROM}    {
            tableGramPosition() += yyleng;
	    BEGIN(FROMstate);
	    return FROM;
	  }
{WHERE}   {
            tableGramPosition() += yyleng;
	    BEGIN(EXPRstate);
            theFromQueryDone = False;
	    return WHERE;
	  }
{ORDERBY} {
            tableGramPosition() += yyleng;
	    BEGIN(EXPRstate);
	    return ORDERBY;
          }
{NODUPL}  {
            tableGramPosition() += yyleng;
	    return NODUPL;
          }
{DESC}    {
            tableGramPosition() += yyleng;
	    return SORTDESC;
          }
{ASC}     {
            tableGramPosition() += yyleng;
	    return SORTASC;
          }
{GIVING}  {
            tableGramPosition() += yyleng;
	    BEGIN(GIVINGstate);
	    return GIVING;
          }
{INTO}    {
            tableGramPosition() += yyleng;
	    return INTO;
          }
{SUBTABLES}    {
            tableGramPosition() += yyleng;
	    return SUBTABLES;
          }
{LIMIT}   {
            tableGramPosition() += yyleng;
	    BEGIN(EXPRstate);
	    return LIMIT;
          }
{OFFSET}  {
            tableGramPosition() += yyleng;
	    BEGIN(EXPRstate);
	    return OFFSET;
          }
{GROUPBY} {
            tableGramPosition() += yyleng;
	    BEGIN(EXPRstate);
	    return GROUPBY;
          }
{GROUPROLL} {
            tableGramPosition() += yyleng;
	    BEGIN(EXPRstate);
	    return GROUPROLL;
          }
{HAVING}  {
            tableGramPosition() += yyleng;
	    BEGIN(EXPRstate);
	    return HAVING;
          }
{JOIN} {
            tableGramPosition() += yyleng;
            throw (TableInvExpr ("JOIN ON is not supported yet"));
          }
{ON}  {
            tableGramPosition() += yyleng;
            throw (TableInvExpr ("JOIN ON is not supported yet"));
          }

{AS}      {
            tableGramPosition() += yyleng;
            return AS;
          }
{TO}      {
            tableGramPosition() += yyleng;
            return TO;
          }
{IN}      {
            tableGramPosition() += yyleng;
            return IN;
          }
{INCONE}  {
            tableGramPosition() += yyleng;
            return INCONE;
          }
"["       {
            tableGramPosition() += yyleng;
            BEGIN(EXPRstate);
            return LBRACKET;
          }
"("       {
            tableGramPosition() += yyleng;
            BEGIN(EXPRstate);
            return LPAREN;
          }
"]"       {
            tableGramPosition() += yyleng;
            BEGIN(EXPRstate);
            return RBRACKET;
          }
")"       {
            tableGramPosition() += yyleng;
            BEGIN(EXPRstate);
            return RPAREN;
          }
";"       {
            tableGramPosition() += yyleng;
            return SEMICOL;
          }

 /* UDF libname synonym definition */
<STYLEstate>{UDFLIBSYN} {
            tableGramPosition() += yyleng;
            lvalp->val = new TaQLConstNode(
                new TaQLConstNodeRep (tableGramRemoveEscapes (TableGramtext)));
            TaQLNode::theirNodesCreated.push_back (lvalp->val);
	    return UDFLIBSYN;
	  }

 /* regular expression and pattern handling */
<EXPRstate>{PATTREX} {
            tableGramPosition() += yyleng;
            lvalp->valre = new TaQLRegexNode(
                new TaQLRegexNodeRep (String(TableGramtext,yyleng)));
            TaQLNode::theirNodesCreated.push_back (lvalp->valre);
	    return REGEX;
	  }

 /* operators */
"<:<"     { tableGramPosition() += yyleng; return OPENOPEN; }
"<:="     { tableGramPosition() += yyleng; return OPENCLOSED; }
"=:<"     { tableGramPosition() += yyleng; return CLOSEDOPEN; }
"=:="     { tableGramPosition() += yyleng; return CLOSEDCLOSED; }
"<:"      { tableGramPosition() += yyleng; return OPENEMPTY; }
":<"      { tableGramPosition() += yyleng; return EMPTYOPEN; }
"=:"      { tableGramPosition() += yyleng; return CLOSEDEMPTY; }
":="      { tableGramPosition() += yyleng; return EMPTYCLOSED; }
":"       { tableGramPosition() += yyleng; return COLON; }
"=="      { tableGramPosition() += yyleng; return EQ; }
"="       { tableGramPosition() += yyleng; return EQASS; }
"!="      { tableGramPosition() += yyleng; return NE; }
"<>"      { tableGramPosition() += yyleng; return NE; }
">="      { tableGramPosition() += yyleng; return GE; }
">"       { tableGramPosition() += yyleng; return GT; }
"<="      { tableGramPosition() += yyleng; return LE; }
"<"       { tableGramPosition() += yyleng; return LT; }
"~="      { tableGramPosition() += yyleng; return EQNEAR; }
"!~="     { tableGramPosition() += yyleng; return NENEAR; }
{BETWEEN} { tableGramPosition() += yyleng; return BETWEEN; }
{EXISTS}  { tableGramPosition() += yyleng; return EXISTS; }
{LIKE}    { tableGramPosition() += yyleng; return LIKE; }
{ILIKE}   { tableGramPosition() += yyleng; return ILIKE; }
"&&"      { tableGramPosition() += yyleng; return AND; }
{AND}     { tableGramPosition() += yyleng; return AND; }
"||"      { tableGramPosition() += yyleng; return OR; }
{OR}      { tableGramPosition() += yyleng; return OR; }
"!"       { tableGramPosition() += yyleng; return NOT; }
{NOT}     { tableGramPosition() += yyleng; return NOT; }
"^"       { tableGramPosition() += yyleng; return BITXOR; }
{XOR}     { tableGramPosition() += yyleng; return BITXOR; }
"**"      { tableGramPosition() += yyleng; return POWER; }
"*"       { tableGramPosition() += yyleng; return TIMES; }
"//"      { tableGramPosition() += yyleng; return DIVIDETRUNC; }
"/"       { tableGramPosition() += yyleng; return DIVIDE; }
"%"       { tableGramPosition() += yyleng; return MODULO; }
"+"       { tableGramPosition() += yyleng; return PLUS; }
"-"       { tableGramPosition() += yyleng; return MINUS; }
"|"       { tableGramPosition() += yyleng; return BITOR; }
"&"       { tableGramPosition() += yyleng; return BITAND; }
"~"       { tableGramPosition() += yyleng; return BITNOT; }
"{"       { tableGramPosition() += yyleng; return LBRACE; }
"}"       { tableGramPosition() += yyleng; return RBRACE; }
","       {
            tableGramPosition() += yyleng;
            if (theFromQueryDone) {
              BEGIN(FROMstate);
              theFromQueryDone=False;
            }
            return COMMA;
          }


 /* Literals */
 /* TIME must be done before FLINTUNIT, otherwise something like 2d1m is
    recognized as FLINTUNIT instead of TIME.
    Similarly COMPLEX must be done before FLINTUNIT. */
{COMPLEX} {
            tableGramPosition() += yyleng;
            double v;
	    sscanf (TableGramtext, "%lf%*c", &v);
            lvalp->val = new TaQLConstNode(
                new TaQLConstNodeRep (DComplex(0, v)));
            TaQLNode::theirNodesCreated.push_back (lvalp->val);
	    return LITERAL;
	  }
{FLOAT}   {
            tableGramPosition() += yyleng;
	    double v = atof(TableGramtext);
            lvalp->val = new TaQLConstNode(new TaQLConstNodeRep (v));
            TaQLNode::theirNodesCreated.push_back (lvalp->val);
	    return LITERAL;
	  }
{INT}     {
            tableGramPosition() += yyleng;
            char* endPtr;
            Int64 v = strtoll(TableGramtext, &endPtr, 10);
            if (endPtr != TableGramtext+yyleng) {
                throw TableInvExpr ("Integer number not fully parsed");
            }
            lvalp->val = new TaQLConstNode(new TaQLConstNodeRep (v));
            TaQLNode::theirNodesCreated.push_back (lvalp->val);
            return LITERAL;
	  }
{HEXINT}  {
            tableGramPosition() += yyleng;
            char* endPtr;
            Int64 v = strtoll(TableGramtext, &endPtr, 0);
            if (endPtr != TableGramtext+yyleng) {
                throw TableInvExpr ("Hex number not fully parsed");
            }
            lvalp->val = new TaQLConstNode(new TaQLConstNodeRep (v));
            TaQLNode::theirNodesCreated.push_back (lvalp->val);
            return LITERAL;
	  }
{TRUE}    {
            tableGramPosition() += yyleng;
            lvalp->val = new TaQLConstNode(new TaQLConstNodeRep (True));
            TaQLNode::theirNodesCreated.push_back (lvalp->val);
	    return LITERAL;
	  }
{FALSE}   {
            tableGramPosition() += yyleng;
            lvalp->val = new TaQLConstNode(new TaQLConstNodeRep (False));
            TaQLNode::theirNodesCreated.push_back (lvalp->val);
	    return LITERAL;
	  }
{STRING}  {
            tableGramPosition() += yyleng;
            lvalp->val = new TaQLConstNode(
                new TaQLConstNodeRep (tableGramRemoveQuotes (TableGramtext)));
            TaQLNode::theirNodesCreated.push_back (lvalp->val);
	    return STRINGLITERAL;
	  }
{DATETIME} {
            tableGramPosition() += yyleng;
            lvalp->val = new TaQLConstNode(
                new TaQLConstNodeRep (tableGramParseDateTime (TableGramtext)));
            TaQLNode::theirNodesCreated.push_back (lvalp->val);
	    return LITERAL;
	  }
<EXPRstate>{TIME} {
            tableGramPosition() += yyleng;
            double v = tableGramParseTime (TableGramtext);
            lvalp->val = new TaQLConstNode(
                new TaQLConstNodeRep (v, String("rad")));
            TaQLNode::theirNodesCreated.push_back (lvalp->val);
	    return LITERAL;
	  }
<EXPRstate>{FLINTUNIT} {
            tableGramPosition() += yyleng;
            double v;
            char unit[32];
	    sscanf (TableGramtext, "%lf%31s", &v, unit);
            lvalp->val = new TaQLConstNode(
                new TaQLConstNodeRep (v, String(unit)));
            TaQLNode::theirNodesCreated.push_back (lvalp->val);
	    return LITERAL;
	  }

 /* In the Exprstate the word TIME is a normal column or function name.
    Otherwise it is the TIME keyword (to show timings).
    The same for SHOW.
 */
<EXPRstate,FROMstate,CRETABstate,GIVINGstate>{TIMEWORD} { 
            tableGramPosition() += yyleng;
            lvalp->val = new TaQLConstNode(
                new TaQLConstNodeRep (tableGramRemoveEscapes (TableGramtext)));
            TaQLNode::theirNodesCreated.push_back (lvalp->val);
	    return NAME;
	  }
{TIMEWORD} {
            tableGramPosition() += yyleng;
	    return TIMING;
	  }
<EXPRstate,FROMstate,CRETABstate,GIVINGstate>{SHOW} { 
            tableGramPosition() += yyleng;
            lvalp->val = new TaQLConstNode(
                new TaQLConstNodeRep (tableGramRemoveEscapes (TableGramtext)));
            TaQLNode::theirNodesCreated.push_back (lvalp->val);
	    return NAME;
	  }
{SHOW}    {
            tableGramPosition() += yyleng;
            BEGIN(SHOWstate);
	    return SHOW;
	  }
            
 /* In the FROM clause a shorthand (for a table) can be given.
    In the WHERE and ORDERBY clause a function name can be given.
    Note that this rule could also be done by NAMEFLD. However, in the
    future :: and . might be be operators instead of parts of the name.
    ALL is a special name, because it can also be used instead of DISTINCT
    in the SELECT clause (note that ALL is also a function name).
    So recognize ALL followed by a parenthesis as a function name.
 */
{ALLFUNC} {
  /* will not work for e.g. select all (1+2)*3, but nothing to do about it */
            yyless(3);     /* unput everything but ALL */
            tableGramPosition() += yyleng;
            lvalp->val = new TaQLConstNode(
                new TaQLConstNodeRep (String("ALL")));
            TaQLNode::theirNodesCreated.push_back (lvalp->val);
	    return NAME;
	  }
{ALL}     {
            tableGramPosition() += yyleng;
	    return ALL;
	  }
{NAME}    {
            tableGramPosition() += yyleng;
            lvalp->val = new TaQLConstNode(
                new TaQLConstNodeRep (tableGramRemoveEscapes (TableGramtext)));
            TaQLNode::theirNodesCreated.push_back (lvalp->val);
	    return NAME;
	  }
 /* Field names can be used in the SELECT, FROM, WHERE, and ORDERBY clause */
{NAMEFLD} {
            tableGramPosition() += yyleng;
            lvalp->val = new TaQLConstNode(
                new TaQLConstNodeRep (tableGramRemoveEscapes (TableGramtext)));
            TaQLNode::theirNodesCreated.push_back (lvalp->val);
	    return FLDNAME;
	  }

 /* A temporary table number possibly followed by a subtable name*/
{TEMPTAB} {
            tableGramPosition() += yyleng;
            Int64 ival = atoi(TableGramtext+1);
            lvalp->val = new TaQLConstNode(
                new TaQLConstNodeRep (ival, tableGramRemoveEscapes (TableGramtext)));
            TaQLNode::theirNodesCreated.push_back (lvalp->val);
	    return TABNAME;
	  }
<SHOWstate>{TEMPTAB} {
            tableGramPosition() += yyleng;
            Int64 ival = atoi(TableGramtext+1);
            lvalp->val = new TaQLConstNode(
                new TaQLConstNodeRep (ival, tableGramRemoveEscapes (TableGramtext)));
            TaQLNode::theirNodesCreated.push_back (lvalp->val);
	    return TABNAME;
	  }

 /* A table file name can be given in the UPDATE, FROM, GIVING, CRETAB clause */
<FROMstate,CRETABstate,GIVINGstate,SHOWstate>{NAMETAB} {
            tableGramPosition() += yyleng;
            lvalp->val = new TaQLConstNode(
                         new TaQLConstNodeRep (tableGramRemoveEscapesQuotes (TableGramtext)));
            TaQLNode::theirNodesCreated.push_back (lvalp->val);
	    return TABNAME;
	  }

 /* Whitespace is skipped */
{WHITE}   { tableGramPosition() += yyleng; }
<SHOWstate>{WHITE}   { tableGramPosition() += yyleng; }

 /* Comment is skipped */
{COMMENT} { tableGramPosition() += yyleng; }
<SHOWstate>{COMMENT} { tableGramPosition() += yyleng; }

 /* Any other non-white character is an error for SHOW */
<SHOWstate>{NONWHITE} { throw TableInvExpr ("Invalid character used in SHOW command"); }

 /* An unterminated string is an error */
{USTRING} { throw (TableInvExpr ("Unterminated string")); }

 /* terminate on EOF */
<<EOF>>   { yyterminate(); }

 /* Any other character is invalid */
.         { return YYERRCODE; }

%%
