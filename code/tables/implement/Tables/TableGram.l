/*
    TableGram.l: Lexical analyzer for table commands
    Copyright (C) 1994,1995,1996,1997
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

/* start conditions */
%s SELECTpart
%s FROMpart
%s WHEREpart
%s ORDERpart
%s GIVINGpart

/* The order in the following list is important, since, for example,
   the word "giving" must be recognized as GIVING and not as NAME.
   Similarly, an alphanumeric string must be recognized as NAME
   and not as NAMETAB or NAMEFLD.
   Note that ORDER BY and ORDERED BY are both allowed; the blank is optional. 
   Complex values can be given as:
    :     float s floati
          where s is + or -  and  i is the letter i (in lowercase only).
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
     do not allow positions with colons yet
TIME      {DTIMEHM}|{TIMESL}|{TIMEU}|{POSDM}|{POSD}
*/

QSTRING   \"[^\"]*\"
ASTRING   \'[^\']*\'
STRING    ({QSTRING}|{ASTRING})+
SELECT    [Ss][Ee][Ll][Ee][Cc][Tt]
FROM      [Ff][Rr][Oo][Mm]
WHERE     [Ww][Hh][Ee][Rr][Ee]
ORDERBY   [Oo][Rr][Dd][Ee][Rr][Bb][Yy]
GIVING    [Gg][Ii][Vv][Ii][Nn][Gg]
ASC       [Aa][Ss][Cc]
DESC      [Dd][Ee][Ss][Cc]
NAME      \\?[A-Za-z_]([A-Za-z_0-9]|(\\.))*
NAMEFLD   ({NAME}".")?{NAME}
NAMETAB   [A-Za-z0-9./_+-~$]+


%%
 /* The command to be analyzed is:
        SELECT column-list FROM table-list WHERE expression
                          ORDER BY column-list GIVING table
    The WHERE, ORDER BY, and GIVING parts are optional.
    Elements in a list are separated by commas.
    A table-name can be only a table file name or a table file name
    followed by whitespace and an alphanumeric name. That 2nd name
    serves as a shorthand for possible later use in the field name.
 */
<INITIAL>{SELECT} {
	    BEGIN(SELECTpart);
	    return SELECT;
	  }
<SELECTpart>{FROM} {
	    BEGIN(FROMpart);
	    return FROM;
	  }
<FROMpart>{WHERE} {
	    BEGIN(WHEREpart);
	    return WHERE;
	  }
<FROMpart,WHEREpart>{ORDERBY} {
	    BEGIN(ORDERpart);
	    return ORDERBY;
          }
 /* Note that the following line assures that GIVING can only be given
    when a FROM, WHERE or ORDERBY clause has been given.
 */
<FROMpart,WHEREpart,ORDERpart>{GIVING} {
	    BEGIN(GIVINGpart);
	    return GIVING;

          }

 /* In the ORDERBY clause the sort qualifier ASC or DESC can be given.
 */
<ORDERpart>{DESC} {
	    return SORTDESC;
          }
<ORDERpart>{ASC} {
	    return SORTASC;
          }

 /* Literals can only be used in the WHERE and ORDERBY expression */
<WHEREpart,ORDERpart>{COMPLEX} {
            lvalp->val = TableParseVal::makeValue();
	    lvalp->val->type = 'c';
	    sscanf (TableGramtext, "%lf%*c", &(lvalp->val->dval[1]));
	    lvalp->val->dval[0] = 0;
	    return LITERAL;
	  }
<WHEREpart,ORDERpart>{FLOAT} {
            lvalp->val = TableParseVal::makeValue();
	    lvalp->val->type = 'f';
	    lvalp->val->dval[0] = atof(TableGramtext);
	    return LITERAL;
	  }
<WHEREpart,ORDERpart>{INT} {
            lvalp->val = TableParseVal::makeValue();
            lvalp->val->type = 'i';
            lvalp->val->ival = atoi(TableGramtext);
            return LITERAL;
	  }
<WHEREpart,ORDERpart>{STRING} {
            lvalp->val = TableParseVal::makeValue();
	    lvalp->val->type = 's';
	    lvalp->val->str = TableGramtext;
	    return LITERAL;
	  }
<WHEREpart,ORDERpart>{DATETIME} {
            lvalp->val = TableParseVal::makeValue();
	    lvalp->val->type = 'd';
	    lvalp->val->str = TableGramtext;
	    return LITERAL;
	  }
<WHEREpart,ORDERpart>{TIME} {
            lvalp->val = TableParseVal::makeValue();
	    lvalp->val->type = 't';
	    lvalp->val->str = TableGramtext;
	    return LITERAL;
	  }

 /* In the FROM clause a shorthand (for a table) can be given.
    In the WHERE and ORDERBY clause a function name can be given.
 */
<FROMpart,WHEREpart,ORDERpart>{NAME} {
            lvalp->val = TableParseVal::makeValue();
	    lvalp->val->str = tableGramRemoveEscapes (TableGramtext);
	    return NAME;
	  }
 /* Field names can be used in the SELECT, WHERE and ORDERBY clause.
 */
<SELECTpart,WHEREpart,ORDERpart>{NAMEFLD} {
            lvalp->val = TableParseVal::makeValue();
	    lvalp->val->str = tableGramRemoveEscapes (TableGramtext);
	    return NAME;
	  }
 /* A NAMETAB is always a table file name */
<FROMpart,GIVINGpart>{NAMETAB} {
            lvalp->val = TableParseVal::makeValue();
	    lvalp->val->str = TableGramtext;
	    return TABNAME;
	  }

"=="      { return EQ; }
">="      { return GE; }
">"       { return GT; }
"<="      { return LE; }
"<"       { return LT; }
"!="      { return NE; }
"&&"      { return AND; }
"||"      { return OR; }
"!"       { return NOT; }
"^"       { return POWER; }
"*"       { return TIMES; }
"/"       { return DIVIDE; }
"%"       { return MODULO; }
"+"       { return PLUS; }
"-"       { return MINUS; }
"("       { return LPAREN; }
")"       { return RPAREN; }
","       { return COMMA; }
"["       { return LBRACKET; }
"]"       { return RBRACKET; }

 /* Whitespace is skipped */
{WHITE}

 /* terminate on EOF */
<<EOF>>   { yyterminate(); }

 /* Any other character is invalid */
.         { return YYERRCODE; }

%%

