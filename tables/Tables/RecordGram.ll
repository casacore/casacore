/*
    RecordGram.l: Lexical analyzer for table commands
    Copyright (C) 2000,2003
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

/* yy_unput is not used, so let flex not generate it, otherwise picky
   compilers will issue warnings. */
%option nounput

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
WHITE1    [ \t\n]
WHITE     {WHITE1}*
DIGIT     [0-9]
INT       {DIGIT}+
HEXINT    0[xX][0-9a-fA-F]+
EXP       [DdEe][+-]?{INT}
FLOAT     {INT}{EXP}|{INT}"."{DIGIT}*({EXP})?|{DIGIT}*"."{INT}({EXP})?
FLINT     {FLOAT}|{INT}
COMPLEX   {FLINT}[ij]
TRUE      T|([Tt][Rr][Uu][Ee])
FALSE     F|([Ff][Aa][Ll][Ss][Ee])
FLINTUNIT {FLINT}[a-zA-Z]+

MONTH     ("-"{INT}?"-")|("-"?[A-Za-z]+"-"?)
DATEH     {INT}{MONTH}{INT}
DATES     {INT}"/"{INT}?"/"{INT}
DATE      {DATEH}|{DATES}
DTIMEHM   {INT}[hH]({INT}?([mM]({FLINT})?)?)?
DTIMEC    {INT}":"({INT}?(":"({FLINT})?)?)?
DTIME     {DTIMEHM}|{DTIMEC}
DATETIME  {DATE}([-/]{DTIME})?

POSDM     {INT}[dD]{INT}[mM]{FLINT}?
POSD      {INT}"."{INT}?"."{FLINT}?
TIME      {DTIMEHM}|{POSDM}|{POSD}
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
STYLE     [Uu][Ss][Ii][Nn][Gg]{WHITE}[Ss][Tt][Yy][Ll][Ee]{WHITE1}
BETWEEN   [Bb][Ee][Tt][Ww][Ee][Ee][Nn]
LIKE      [Ll][Ii][Kk][Ee]
IN        [Ii][Nn]
INCONE    [Ii][Nn]{WHITE}[Cc][Oo][Nn][Ee]{WHITE1}
AND       [Aa][Nn][Dd]
OR        [Oo][Rr]
NOT       [Nn][Oo][Tt]
NAME      \\?[A-Za-z_]([A-Za-z_0-9]|(\\.))*
NAMEFLD   {NAME}("."{NAME})*
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
 /* This grammar is used for selection of records to be used in C++.
    It is the same as the WHERE part of TableGram.
 */
{IN}      {
            recordGramPosition() += yyleng;
            return IN;
          }
{INCONE}  {
            recordGramPosition() += yyleng;
            return INCONE;
          }
"["       {
            recordGramPosition() += yyleng;
            return LBRACKET;
          }
"]"       {
            recordGramPosition() += yyleng;
            return RBRACKET;
          }

 /* regular expression and pattern handling */
{PATTREX} {
            recordGramPosition() += yyleng;
            lvalp->val = new RecordGramVal();
            RecordGram::addToken (lvalp->val);
            lvalp->val->type = 'r';
            lvalp->val->str = String(RecordGramtext,yyleng);
	    return REGEX;
	  }

 /* operators */
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
{STYLE}   { recordGramPosition() += yyleng; return STYLE; }
{BETWEEN} { recordGramPosition() += yyleng; return BETWEEN; }
{LIKE}    { recordGramPosition() += yyleng; return LIKE; }
"&&"      { recordGramPosition() += yyleng; return AND; }
{AND}     { recordGramPosition() += yyleng; return AND; }
"||"      { recordGramPosition() += yyleng; return OR; }
{OR}      { recordGramPosition() += yyleng; return OR; }
"!"       { recordGramPosition() += yyleng; return NOT; }
{NOT}     { recordGramPosition() += yyleng; return NOT; }
"^"       { recordGramPosition() += yyleng; return BITXOR; }
"**"      { recordGramPosition() += yyleng; return POWER; }
"*"       { recordGramPosition() += yyleng; return TIMES; }
"/"       { recordGramPosition() += yyleng; return DIVIDE; }
"//"      { recordGramPosition() += yyleng; return DIVIDETRUNC; }
"%"       { recordGramPosition() += yyleng; return MODULO; }
"+"       { recordGramPosition() += yyleng; return PLUS; }
"-"       { recordGramPosition() += yyleng; return MINUS; }
"|"       { tableGramPosition() += yyleng; return BITOR; }
"&"       { tableGramPosition() += yyleng; return BITAND; }
"~"       { tableGramPosition() += yyleng; return BITNOT; }
"("       { recordGramPosition() += yyleng; return LPAREN; }
")"       { recordGramPosition() += yyleng; return RPAREN; }
"{"       { recordGramPosition() += yyleng; return LBRACE; }
"}"       { recordGramPosition() += yyleng; return RBRACE; }
","       { recordGramPosition() += yyleng; return COMMA; }


 /* Literals */
 /* TIME must be done before FLINTUNIT, otherwise something like 2d1m is
    recognized as FLINTUNIT instead of TIME.
    Similarly COMPLEX must be done before FLINTUNIT. */
{COMPLEX} {
            recordGramPosition() += yyleng;
            lvalp->val = new RecordGramVal();
            RecordGram::addToken (lvalp->val);
	    lvalp->val->type = 'c';
	    sscanf (RecordGramtext, "%lf%*c", &(lvalp->val->dval[1]));
	    lvalp->val->dval[0] = 0;
	    return LITERAL;
	  }
{FLOAT}   {
            recordGramPosition() += yyleng;
            lvalp->val = new RecordGramVal();
            RecordGram::addToken (lvalp->val);
	    lvalp->val->type = 'f';
	    lvalp->val->dval[0] = atof(RecordGramtext);
	    return LITERAL;
	  }
{INT}     {
            recordGramPosition() += yyleng;
            char* endPtr;
            Int64 v = strtol(RecordGramtext, &endPtr, 10);
            if (endPtr != RecordGramtext+yyleng) {
                throw TableInvExpr ("Integer number not fully parsed");
            }
            lvalp->val = new RecordGramVal();
            RecordGram::addToken (lvalp->val);
            lvalp->val->type = 'i';
            lvalp->val->ival = v;
            return LITERAL;
	  }
{HEXINT}  {
            recordGramPosition() += yyleng;
            char* endPtr;
            Int64 v = strtol(RecordGramtext, &endPtr, 0);
            if (endPtr != RecordGramtext+yyleng) {
                throw TableInvExpr ("Hex number not fully parsed");
            }
            lvalp->val = new RecordGramVal();
            RecordGram::addToken (lvalp->val);
            lvalp->val->type = 'i';
            lvalp->val->ival = v;
            return LITERAL;
	  }
{TRUE}    {
            recordGramPosition() += yyleng;
            lvalp->val = new RecordGramVal();
            RecordGram::addToken (lvalp->val);
	    lvalp->val->type = 'b';
	    lvalp->val->bval = True;
	    return LITERAL;
	  }
{FALSE}   {
            recordGramPosition() += yyleng;
            lvalp->val = new RecordGramVal();
            RecordGram::addToken (lvalp->val);
	    lvalp->val->type = 'b';
	    lvalp->val->bval = False;
	    return LITERAL;
	  }
{STRING}  {
            recordGramPosition() += yyleng;
            lvalp->val = new RecordGramVal();
            RecordGram::addToken (lvalp->val);
	    lvalp->val->type = 's';
	    lvalp->val->str = recordGramRemoveQuotes (RecordGramtext);
	    return STRINGLITERAL;
	  }
{DATETIME} {
            recordGramPosition() += yyleng;
            lvalp->val = new RecordGramVal();
            RecordGram::addToken (lvalp->val);
	    lvalp->val->type = 'd';
	    lvalp->val->str = RecordGramtext;
	    return LITERAL;
	  }
{TIME}    {
            recordGramPosition() += yyleng;
            lvalp->val = new RecordGramVal();
            RecordGram::addToken (lvalp->val);
	    lvalp->val->type = 't';
	    lvalp->val->str = RecordGramtext;
	    return LITERAL;
	  }
{FLINTUNIT} {
            recordGramPosition() += yyleng;
            double v;
            char unit[32];
	    sscanf (RecordGramtext, "%lf%31s", &v, unit);
            lvalp->val = new RecordGramVal();
            RecordGram::addToken (lvalp->val);
	    lvalp->val->type = 'f';
	    lvalp->val->str = unit;
	    return LITERAL;
	  }


{NAME}    {
            recordGramPosition() += yyleng;
            lvalp->val = new RecordGramVal();
            RecordGram::addToken (lvalp->val);
	    lvalp->val->type = 's';
	    lvalp->val->str = recordGramRemoveEscapes (RecordGramtext);
	    return NAME;
	  }

{NAMEFLD} {
            recordGramPosition() += yyleng;
            lvalp->val = new RecordGramVal();
            RecordGram::addToken (lvalp->val);
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
