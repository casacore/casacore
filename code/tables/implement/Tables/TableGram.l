/*
    TableGram.l: Lexical analyzer for table commands
    Copyright (C) 1994,1995,1996,1997,1998
    Associated Universities, Inc. Washington DC, USA.

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

%{
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) result=tableGramInput(buf,max_size)

#undef YY_DECL
#define YY_DECL int TableGramlex (YYSTYPE* lvalp)
%}

/* states */
%s EXPRstate
%s GIVINGstate
%s FROMstate

/* The order in the following list is important, since, for example,
   the word "giving" must be recognized as GIVING and not as NAME.
   Similarly, an alphanumeric string must be recognized as NAME
   and not as NAMETAB or NAMEFLD.
   Complex values can be given as:   FLOATi
          where i is the letter i (in lowercase only).
   In a NAME the backslash can be used to escape special characters like -.
   In that way a name like DATE-OBS can be given as DATE\-OBS.
*/
WHITE     [ \t\n]*
DIGIT     [0-9]
INT       {DIGIT}+
EXP       [DdEe][+-]?{INT}
FLOAT     {INT}{EXP}|{INT}"."{DIGIT}*({EXP})?|{DIGIT}*"."{INT}({EXP})?
FLINT     {FLOAT}|{INT}
COMPLEX   {FLINT}"i"

MONTH     ("-"{INT}?"-")|("-"?[A-Za-z]+"-"?)
DATEH     {INT}{MONTH}{INT}
DATES     {INT}"/"{INT}?"/"{INT}
DATE      {DATEH}|{DATES}
DTIMEHM   {INT}[hH]({INT}?([mM]({FLINT})?)?)?
DTIMEC    {INT}":"({INT}?(":"({FLINT})?)?)?
DTIME     {DTIMEHM}|{DTIMEC}
DATETIME  {DATE}([-/]{DTIME})?
TIMESL    "/"{DTIME}

TIMEU     {FLINT}[a-zA-Z]+
POSDM     {INT}[dD]({INT}?([mM]({FLINT})?)?)?
POSD      {INT}"."{INT}?"."{FLINT}?
TIME      {DTIMEHM}|{TIMEU}|{POSDM}|{POSD}
/*
     positions with colons cannot be allowed, because they interfere
     with the interval syntax (and a starting slash is rather ambiguous).
TIME      {DTIMEHM}|{TIMESL}|{TIMEU}|{POSDM}|{POSD}
*/

QSTRING   \"[^\"\n]*\"
ASTRING   \'[^\'\n]*\'
UQSTRING   \"[^\"\n]*\n
UASTRING   \'[^\'\n]*\n
STRING    ({QSTRING}|{ASTRING})+
USTRING   ({UQSTRING}|{UASTRING})+
SELECT    [Ss][Ee][Ll][Ee][Cc][Tt]
FROM      [Ff][Rr][Oo][Mm]
WHERE     [Ww][Hh][Ee][Rr][Ee]
ORDERBY   [Oo][Rr][Dd][Ee][Rr]{WHITE}[Bb][Yy]
NODUPL    [Nn][Oo][Dd][Uu][Pp][Ll][Ii][Cc][Aa][Tt][Ee][Ss]
GIVING    [Gg][Ii][Vv][Ii][Nn][Gg]
ASC       [Aa][Ss][Cc]
DESC      [Dd][Ee][Ss][Cc]
IN        [Ii][Nn]
AS        [Aa][Ss]
AND       [Aa][Nn][Dd]
OR        [Oo][Rr]
NOT       [Nn][Oo][Tt]
NAME      \\?[A-Za-z_]([A-Za-z_0-9]|(\\.))*
NAMEFLD   ({NAME}".")?{NAME}?("::")?{NAME}
TEMPTAB   [$]{INT}
NAMETAB   ([A-Za-z0-9_./+\-~$]|(\\.))+


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
    These are indicated by the FROM/GIVINGstate, because a table name
    can contain special characters like -. In the FROMstate a table name
    can also be $nnn indicating a temporary table.
    In a subquery care must be taken that the state is switched back to
    EXPRstate, because a FROM can be the last part in a subquery and
    because a set can be specified in the GIVING part.
    This is done by setting the state when square brackets are found.
    [ indicates the beginning of a set. ] indicates end of subquery.
 */
{SELECT}  {
            tableGramPosition() += yyleng;
	    BEGIN(EXPRstate);
	    return SELECT;
	  }
{FROM}    {
            tableGramPosition() += yyleng;
	    BEGIN(FROMstate);
	    return FROM;
	  }
{WHERE}   {
            tableGramPosition() += yyleng;
	    BEGIN(EXPRstate);
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

{AS}      {
            tableGramPosition() += yyleng;
            return AS;
          }
{IN}      {
            tableGramPosition() += yyleng;
            return IN;
          }
"["       {
            tableGramPosition() += yyleng;
            BEGIN(EXPRstate);
            return LBRACKET;
          }
"]"       {
            tableGramPosition() += yyleng;
            BEGIN(EXPRstate);
            return RBRACKET;
          }

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
"="       { tableGramPosition() += yyleng; return EQ; }
"!="      { tableGramPosition() += yyleng; return NE; }
"<>"      { tableGramPosition() += yyleng; return NE; }
">="      { tableGramPosition() += yyleng; return GE; }
">"       { tableGramPosition() += yyleng; return GT; }
"<="      { tableGramPosition() += yyleng; return LE; }
"<"       { tableGramPosition() += yyleng; return LT; }
"&&"      { tableGramPosition() += yyleng; return AND; }
{AND}     { tableGramPosition() += yyleng; return AND; }
"||"      { tableGramPosition() += yyleng; return OR; }
{OR}      { tableGramPosition() += yyleng; return OR; }
"!"       { tableGramPosition() += yyleng; return NOT; }
{NOT}     { tableGramPosition() += yyleng; return NOT; }
"^"       { tableGramPosition() += yyleng; return POWER; }
"*"       { tableGramPosition() += yyleng; return TIMES; }
"/"       { tableGramPosition() += yyleng; return DIVIDE; }
"%"       { tableGramPosition() += yyleng; return MODULO; }
"+"       { tableGramPosition() += yyleng; return PLUS; }
"-"       { tableGramPosition() += yyleng; return MINUS; }
"("       { tableGramPosition() += yyleng; return LPAREN; }
")"       { tableGramPosition() += yyleng; return RPAREN; }
","       { tableGramPosition() += yyleng; return COMMA; }


 /* Literals */
{COMPLEX} {
            tableGramPosition() += yyleng;
            lvalp->val = TableParseVal::makeValue();
	    lvalp->val->type = 'c';
	    sscanf (TableGramtext, "%lf%*c", &(lvalp->val->dval[1]));
	    lvalp->val->dval[0] = 0;
	    return LITERAL;
	  }
{FLOAT}   {
            tableGramPosition() += yyleng;
            lvalp->val = TableParseVal::makeValue();
	    lvalp->val->type = 'f';
	    lvalp->val->dval[0] = atof(TableGramtext);
	    return LITERAL;
	  }
{INT}     {
            tableGramPosition() += yyleng;
            lvalp->val = TableParseVal::makeValue();
            Int ival = atoi(TableGramtext);
            Double dval = atof(TableGramtext);
            if (ival < dval-0.1  ||  ival > dval+0.1) {
                lvalp->val->type = 'f';
                lvalp->val->dval[0] = dval;
            } else {
                lvalp->val->type = 'i';
                lvalp->val->ival = ival;
            }
            return LITERAL;
	  }
{STRING}  {
            tableGramPosition() += yyleng;
            lvalp->val = TableParseVal::makeValue();
	    lvalp->val->type = 's';
	    lvalp->val->str = tableGramRemoveQuotes (TableGramtext);
	    return STRINGLITERAL;
	  }
{DATETIME} {
            tableGramPosition() += yyleng;
            lvalp->val = TableParseVal::makeValue();
	    lvalp->val->type = 'd';
	    lvalp->val->str = TableGramtext;
	    return LITERAL;
	  }
{TIME}    {
            tableGramPosition() += yyleng;
            lvalp->val = TableParseVal::makeValue();
	    lvalp->val->type = 't';
	    lvalp->val->str = TableGramtext;
	    return LITERAL;
	  }

 /* In the FROM clause a shorthand (for a table) can be given.
    In the WHERE and ORDERBY clause a function name can be given.
    Note that this rule could also be done by NAMEFLD. However, in the
    future :: and . will be operators instead of parts of the name.
 */
{NAME}    {
            tableGramPosition() += yyleng;
            lvalp->val = TableParseVal::makeValue();
	    lvalp->val->type = 's';
	    lvalp->val->str = tableGramRemoveEscapes (TableGramtext);
	    return NAME;
	  }
 /* Field names can be used in the SELECT, FROM, WHERE, and ORDERBY clause */
{NAMEFLD} {
            tableGramPosition() += yyleng;
            lvalp->val = TableParseVal::makeValue();
	    lvalp->val->type = 's';
	    lvalp->val->str = tableGramRemoveEscapes (TableGramtext);
	    return FLDNAME;
	  }

 /* A temporary table number can be given in the FROM clause */
<FROMstate>{TEMPTAB} {
            tableGramPosition() += yyleng;
            lvalp->val = TableParseVal::makeValue();
	    lvalp->val->type = 'i';
            lvalp->val->ival = atoi(TableGramtext+1);
	    return TABNAME;
	  }

 /* A table file name can be given in the FROM or GIVING clause */
<FROMstate,GIVINGstate>{NAMETAB} {
            tableGramPosition() += yyleng;
            lvalp->val = TableParseVal::makeValue();
	    lvalp->val->type = 's';
	    lvalp->val->str = tableGramRemoveEscapes (TableGramtext);
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
