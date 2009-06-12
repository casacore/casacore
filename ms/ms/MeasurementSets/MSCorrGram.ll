/*
    MSCorrGram.l: Lexical analyzer for ms selection commands
    Copyright (C) 1994,1995,1996,1997,1998,2001,2003
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
#define YY_INPUT(buf,result,max_size) result=msCorrGramInput(buf,max_size)

#undef YY_DECL
#define YY_DECL int MSCorrGramlex (YYSTYPE* lvalp)
%}

WHITE     [ \t\n]*
DIGIT     [0-9]
INT       {DIGIT}+
EXP       [DdEe][+-]?{INT}
FNUMBER   {INT}"."{DIGIT}*
TRUE      T
FALSE     F

QSTRING   \"[^\"\n]*\"
ASTRING   \'[^\'\n]*\'
UQSTRING   \"[^\"\n]*\n
UASTRING   \'[^\'\n]*\n
STRING    ({QSTRING}|{ASTRING})+
USTRING   ({UQSTRING}|{UASTRING})+

CORRTYPE  [IQUV]{1}|[RL]{2}|[XY]{2}

REGEX1    m"/"[^/]+"/"
REGEX2    m%[^%]+%
REGEX3    m#[^#]+#
REGEX     {REGEX1}|{REGEX2}|{REGEX3}

  /* rules */

%%

"["       { msCorrGramPosition() += yyleng;
            return LBRACKET;
          }
"("       { msCorrGramPosition() += yyleng;
            return LPAREN;
          }
"]"       { msCorrGramPosition() += yyleng;
            return RBRACKET;
          }
")"       { msCorrGramPosition() += yyleng;
            return RPAREN;
          }

":"       { msCorrGramPosition() += yyleng;
            return COLON; }
"=="      { msCorrGramPosition() += yyleng;
            return EQ; }
"="       { msCorrGramPosition() += yyleng;
            return EQASS; }
"!="      { msCorrGramPosition() += yyleng;
            return NE; }
"<>"      { msCorrGramPosition() += yyleng;
            return NE; }
">="      { msCorrGramPosition() += yyleng;
            return GE; }
">"       { msCorrGramPosition() += yyleng;
            return GT; }
"<="      { msCorrGramPosition() += yyleng;
            return LE; }
"<"       { msCorrGramPosition() += yyleng;
            return LT; }
"&&"      { msCorrGramPosition() += yyleng;
            return AND; }
"||"      { msCorrGramPosition() += yyleng;
            return OR; }
"!"       { msCorrGramPosition() += yyleng;
            return NOT; }
"^"       { msCorrGramPosition() += yyleng;
            return POWER; }
"*"       { msCorrGramPosition() += yyleng;
            return TIMES; }
"/"       { msCorrGramPosition() += yyleng;
            return DIVIDE; }
"%"       { msCorrGramPosition() += yyleng;
            return PERCENT; }
"+"       { msCorrGramPosition() += yyleng;
            return PLUS; }
"~"       { msCorrGramPosition() += yyleng;
            return DASH; }
"{"       { msCorrGramPosition() += yyleng;
            return LBRACE; }
"}"       { msCorrGramPosition() += yyleng;
            return RBRACE; }
"'"       { msCorrGramPosition() += yyleng;
            return SQUOTE; }
","       { msCorrGramPosition() += yyleng;
            return COMMA;
          }

 /* Literals */

{CORRTYPE} {
             msCorrGramPosition() += yyleng;
             lvalp->str = MSCorrGramtext;
	     return CORRTYPE;
           }

%%

