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

/* states */
%s STYLEstate
%s EXPRstate
%s GIVINGstate
%s FROMstate
%s CRETABstate

/* The order in the following list is important, since, for example,
   the word "giving" must be recognized as GIVING and not as NAME.
   Similarly, an alphanumeric string must be recognized as NAME
   and not as NAMETAB or NAMEFLD.
   Complex values can be given as:   FLOATi
          where i is the letter i or j (in lowercase only).
   In a NAME the backslash can be used to escape special characters like -.
   In that way a name like DATE-OBS can be given as DATE\-OBS.
*/
WHITE1    [ \t\n]
WHITE     {WHITE1}*
DIGIT     [0-9]
INT       {DIGIT}+
INT2      {DIGIT}{DIGIT}
INT4      {INT2}{INT2}
HEXINT    0[xX][0-9a-fA-F]+
EXP       [DdEe][+-]?{INT}
FLOAT     {INT}{EXP}|{INT}"."{DIGIT}*({EXP})?|{DIGIT}*"."{INT}({EXP})?
FLINT     {FLOAT}|{INT}
COMPLEX   {FLINT}[ij]
TRUE      T|([Tt][Rr][Uu][Ee])
FALSE     F|([Ff][Aa][Ll][Ss][Ee])
FLINTUNIT {FLINT}[a-zA-Z]+

MONTH     [A-Za-z]+
DATEA     {INT}{MONTH}{INT}|{INT}"-"{MONTH}"-"{INT}
DATEH     ({INT2}"-"{INT2}"-"{INT4})|({INT4}"-"{INT2}"-"{INT2})
DATES     {INT4}"/"{INT2}"/"{INT2}
DATE      {DATEA}|{DATEH}|{DATES}
DTIMEHM   {INT}[hH]({INT}?([mM]({FLINT})?)?)?
DTIMEC    {INT}":"({INT}?(":"({FLINT})?)?)?
DTIME     {DTIMEHM}|{DTIMEC}
DATETIME  {DATE}([-/ ]{DTIME})?

POSDM     {INT}[dD]{INT}[mM]{FLINT}?
POSD      {INT}"."{INT}?"."{FLINT}?
TIME      {DTIMEHM}|{POSDM}|{POSD}
/*
     positions/times with colons cannot be allowed, because they interfere
     with the interval syntax. It is only possible when preceeded by a date.
     Furthermore, a colon is sometimes also used for degrees (in declinations),
     so it's better to stick to hms and dms.
*/


QSTRING   \"[^\"\n]*\"
ASTRING   \'[^\'\n]*\'
UQSTRING   \"[^\"\n]*\n
UASTRING   \'[^\'\n]*\n
STRING    ({QSTRING}|{ASTRING})+
USTRING   ({UQSTRING}|{UASTRING})+
UNION     [Uu][Nn][Ii][Oo][Nn]
INTERSECT [Ii][Nn][Tt][Ee][Rr][Ss][Ee][Cc][Tt]
EXCEPT    ([Ee][Xx][Cc][Ee][Pp][Tt])|([Mm][Ii][Nn][Uu][Ss])
STYLE     [Uu][Ss][Ii][Nn][Gg]{WHITE}[Ss][Tt][Yy][Ll][Ee]{WHITE1}
TIMEWORD  [Tt][Ii][Mm][Ee]
SELECT    [Ss][Ee][Ll][Ee][Cc][Tt]
UPDATE    [Uu][Pp][Dd][Aa][Tt][Ee]
INSERT    [Ii][Nn][Ss][Ee][Rr][Tt]
DELETE    [Dd][Ee][Ll][Ee][Tt][Ee]
COUNT     [Cc][Oo][Uu][Nn][Tt]
COUNTALL  [Gg]{COUNT}{WHITE}"("{WHITE}"*"?{WHITE}")"
CALC      [Cc][Aa][Ll][Cc]
CREATETAB [Cc][Rr][Ee][Aa][Tt][Ee]{WHITE}[Tt][Aa][Bb][Ll][Ee]{WHITE1}
DMINFO    [Dd][Mm][Ii][Nn][Ff][Oo]
SET       [Ss][Ee][Tt]
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
GROUPBY   [Gg][Rr][Oo][Uu][Pp]{WHITE}[Bb][Yy]{WHITE1}
GROUPROLL {GROUPBY}{WHITE}[Rr][Oo][Ll][Ll][Uu][Pp]{WHITE1}
HAVING    [Hh][Aa][Vv][Ii][Nn][Gg]
JOIN      [Jj][Oo][Ii][Nn]
ON        [Oo][Nn]
ASC       [Aa][Ss][Cc]
DESC      [Dd][Ee][Ss][Cc]
LIMIT     [Ll][Ii][Mm][Ii][Tt]
OFFSET    [Oo][Ff][Ff][Ss][Ee][Tt]
BETWEEN   [Bb][Ee][Tt][Ww][Ee][Ee][Nn]
EXISTS    [Ee][Xx][Ii][Ss][Tt][Ss]
LIKE      [Ll][Ii][Kk][Ee]
IN        [Ii][Nn]
INCONE    [Ii][Nn]{WHITE}[Cc][Oo][Nn][Ee]{WHITE1}
AS        [Aa][Ss]
AND       [Aa][Nn][Dd]
OR        [Oo][Rr]
XOR       [Xx][Oo][Rr]
NOT       [Nn][Oo][Tt]
ALL       [Aa][Ll][Ll]
ALLFUNC   {ALL}{WHITE}"("
NAME      \\?[A-Za-z_]([A-Za-z_0-9]|(\\.))*
NAMEFLD   {NAME}?"."?{NAME}?("::")?{NAME}("."{NAME})*
TEMPTAB   [$]{INT}
NAMETAB   ([A-Za-z0-9_./+\-~$@:]|(\\.))+
UDFLIBSYN {NAME}{WHITE}"="{WHITE}{NAME}
REGEX1    m"/"[^/]+"/"
REGEX2    m%[^%]+%
REGEX3    m#[^#]+#
REGEX     {REGEX1}|{REGEX2}|{REGEX3}
FREGEX1   f"/"[^/]+"/"
FREGEX2   f%[^%]+%
FREGEX3   f#[^#]+#
FREGEX    {FREGEX1}|{FREGEX2}|{FREGEX3}
PATT1     p\/[^/]+\/
PATT2     p%[^%]+%
PATT3     p#[^#]+#
PATT      {PATT1}|{PATT2}|{PATT3}
PATTEX    ({REGEX}|{FREGEX}|{PATT})i?
DIST1     d\/[^/]+\/
DIST2     d%[^%]+%
DIST3     d#[^#]+#
DISTOPT   [bi]*{INT}?[bi]*
DISTEX    ({DIST1}|{DIST2}|{DIST3}){DISTOPT}
OPERREX   "!"?"~"
PATTREX   {OPERREX}{WHITE}({PATTEX}|{DISTEX})

%%
 /* The command to be analyzed is:
        SELECT column-list FROM table-list WHERE expression
                          ORDER BY column-list GIVING table
    The WHERE, ORDER BY, and GIVING parts are optional.
    Elements in a list are separated by commas.
    A table-name can be only a table file name or a table file name
    followed by whitespace and an alphanumeric name. That 2nd name
    serves as a shorthand for possible later use in the field name.
    A table name can be given in the FROM part and in the giving PART.
    These are indicated by the FROM/GIVING/CRETABstate, because a table name
    can contain special characters like -. In the FROMstate a table name
    can also be $nnn indicating a temporary table.
    In a subquery care must be taken that the state is switched back to
    EXPRstate, because a FROM can be the last part in a subquery and
    because a set can be specified in the GIVING part.
    This is done by setting the state when parentheses or square brackets
    are found. ( and [ indicate the beginning of a set(subquery).
    ) and ] indicate the end of subquery.
 */
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
{DMINFO}  {
            tableGramPosition() += yyleng;
	    BEGIN(EXPRstate);
	    return DMINFO;
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
 */
<EXPRstate>{TIMEWORD} { 
            tableGramPosition() += yyleng;
            lvalp->val = new TaQLConstNode(
                new TaQLConstNodeRep (tableGramRemoveEscapes (TableGramtext)));
            TaQLNode::theirNodesCreated.push_back (lvalp->val);
	    return NAME;
	  }
{TIMEWORD}    {
            tableGramPosition() += yyleng;
	    return TIMING;
	  }
            
 /* In the FROM clause a shorthand (for a table) can be given.
    In the WHERE and ORDERBY clause a function name can be given.
    Note that this rule could also be done by NAMEFLD. However, in the
    future :: and . will be operators instead of parts of the name.
    ALL is a special name, because it can also be used instead of DISTINCT
    in the SELECT clause (note that ALL is also a function name).
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

 /* A temporary table number can be given in the FROM clause */
<FROMstate>{TEMPTAB} {
            tableGramPosition() += yyleng;
            Int64 ival = atoi(TableGramtext+1);
            lvalp->val = new TaQLConstNode(new TaQLConstNodeRep (ival));
            TaQLNode::theirNodesCreated.push_back (lvalp->val);
	    return TABNAME;
	  }

 /* A table file name can be given in the UPDATE, FROM, GIVING, CRETAB clause */
<FROMstate,CRETABstate,GIVINGstate>{NAMETAB} {
            tableGramPosition() += yyleng;
            lvalp->val = new TaQLConstNode(
                new TaQLConstNodeRep (tableGramRemoveEscapes (TableGramtext)));
            TaQLNode::theirNodesCreated.push_back (lvalp->val);
	    return TABNAME;
	  }

 /* Whitespace is skipped */
{WHITE}   { tableGramPosition() += yyleng; }

 /* An unterminated string is an error */
{USTRING} { throw (TableInvExpr ("Unterminated string")); }

 /* terminate on EOF */
<<EOF>>   { yyterminate(); }

 /* Any other character is invalid */
.         { return YYERRCODE; }

%%
