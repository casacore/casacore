/*
    RecordGram.l: Lexical analyzer for table commands
    Copyright (C) 2000
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
#define YY_INPUT(buf,result,max_size) result=recordGramInput(buf,max_size)

#undef YY_DECL
#define YY_DECL int RecordGramlex (YYSTYPE* lvalp)
%}


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
IN        [Ii][Nn]
AND       [Aa][Nn][Dd]
OR        [Oo][Rr]
NOT       [Nn][Oo][Tt]
NAME      \\?[A-Za-z_]([A-Za-z_0-9]|(\\.))*
NAMEFLD   {NAME}("."{NAME})*


%%
 /* This grammar is used for selection of records to be used in C++.
    It is the same as the WHERE part of TableGram.
 */
{IN}      {
            recordGramPosition() += yyleng;
            return IN;
          }
"["       {
            recordGramPosition() += yyleng;
            return LBRACKET;
          }
"]"       {
            recordGramPosition() += yyleng;
            return RBRACKET;
          }

"<:<"     { recordGramPosition() += yyleng; return OPENOPEN; }
"<:="     { recordGramPosition() += yyleng; return OPENCLOSED; }
"=:<"     { recordGramPosition() += yyleng; return CLOSEDOPEN; }
"=:="     { recordGramPosition() += yyleng; return CLOSEDCLOSED; }
"<:"      { recordGramPosition() += yyleng; return OPENEMPTY; }
":<"      { recordGramPosition() += yyleng; return EMPTYOPEN; }
"=:"      { recordGramPosition() += yyleng; return CLOSEDEMPTY; }
":="      { recordGramPosition() += yyleng; return EMPTYCLOSED; }
":"       { recordGramPosition() += yyleng; return COLON; }
"=="      { recordGramPosition() += yyleng; return EQ; }
"="       { recordGramPosition() += yyleng; return EQ; }
"!="      { recordGramPosition() += yyleng; return NE; }
"<>"      { recordGramPosition() += yyleng; return NE; }
">="      { recordGramPosition() += yyleng; return GE; }
">"       { recordGramPosition() += yyleng; return GT; }
"<="      { recordGramPosition() += yyleng; return LE; }
"<"       { recordGramPosition() += yyleng; return LT; }
"&&"      { recordGramPosition() += yyleng; return AND; }
{AND}     { recordGramPosition() += yyleng; return AND; }
"||"      { recordGramPosition() += yyleng; return OR; }
{OR}      { recordGramPosition() += yyleng; return OR; }
"!"       { recordGramPosition() += yyleng; return NOT; }
{NOT}     { recordGramPosition() += yyleng; return NOT; }
"^"       { recordGramPosition() += yyleng; return POWER; }
"*"       { recordGramPosition() += yyleng; return TIMES; }
"/"       { recordGramPosition() += yyleng; return DIVIDE; }
"%"       { recordGramPosition() += yyleng; return MODULO; }
"+"       { recordGramPosition() += yyleng; return PLUS; }
"-"       { recordGramPosition() += yyleng; return MINUS; }
"("       { recordGramPosition() += yyleng; return LPAREN; }
")"       { recordGramPosition() += yyleng; return RPAREN; }
"{"       { recordGramPosition() += yyleng; return LBRACE; }
"}"       { recordGramPosition() += yyleng; return RBRACE; }
","       { recordGramPosition() += yyleng; return COMMA; }


 /* Literals */
{COMPLEX} {
            recordGramPosition() += yyleng;
            lvalp->val = TableParseVal::makeValue();
	    lvalp->val->type = 'c';
	    sscanf (RecordGramtext, "%lf%*c", &(lvalp->val->dval[1]));
	    lvalp->val->dval[0] = 0;
	    return LITERAL;
	  }
{FLOAT}   {
            recordGramPosition() += yyleng;
            lvalp->val = TableParseVal::makeValue();
	    lvalp->val->type = 'f';
	    lvalp->val->dval[0] = atof(RecordGramtext);
	    return LITERAL;
	  }
{INT}     {
            recordGramPosition() += yyleng;
            lvalp->val = TableParseVal::makeValue();
            Int ival = atoi(RecordGramtext);
            Double dval = atof(RecordGramtext);
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
            recordGramPosition() += yyleng;
            lvalp->val = TableParseVal::makeValue();
	    lvalp->val->type = 's';
	    lvalp->val->str = recordGramRemoveQuotes (RecordGramtext);
	    return STRINGLITERAL;
	  }
{DATETIME} {
            recordGramPosition() += yyleng;
            lvalp->val = TableParseVal::makeValue();
	    lvalp->val->type = 'd';
	    lvalp->val->str = RecordGramtext;
	    return LITERAL;
	  }
{TIME}    {
            recordGramPosition() += yyleng;
            lvalp->val = TableParseVal::makeValue();
	    lvalp->val->type = 't';
	    lvalp->val->str = RecordGramtext;
	    return LITERAL;
	  }

{NAME}    {
            recordGramPosition() += yyleng;
            lvalp->val = TableParseVal::makeValue();
	    lvalp->val->type = 's';
	    lvalp->val->str = recordGramRemoveEscapes (RecordGramtext);
	    return NAME;
	  }

{NAMEFLD} {
            recordGramPosition() += yyleng;
            lvalp->val = TableParseVal::makeValue();
	    lvalp->val->type = 's';
	    lvalp->val->str = recordGramRemoveEscapes (RecordGramtext);
	    return FLDNAME;
	  }

 /* Whitespace is skipped */
{WHITE}   { recordGramPosition() += yyleng; }

 /* An unterminated string is an error */
{USTRING} { throw (TableInvExpr ("Unterminated string")); }

 /* terminate on EOF */
<<EOF>>   { yyterminate(); }

 /* Any other character is invalid */
.         { return YYERRCODE; }

%%
