/*
    MSUvDistGram.l: Lexical analyzer for ms selection commands
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

%{
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) result=msUvDistGramInput(buf,max_size)

#undef YY_DECL
#define YY_DECL int MSUvDistGramlex (YYSTYPE* lvalp)
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

DISTANCEUNIT [Kk][Mm]
ML           [Mm][Ll]
KL           [Kk][Ll]
L            [Ll]
WAVELENTHUNIT {ML}|{KL}|{L}

REGEX1    m"/"[^/]+"/"
REGEX2    m%[^%]+%
REGEX3    m#[^#]+#
REGEX     {REGEX1}|{REGEX2}|{REGEX3}

  /* rules */

%%

"["       { msUvDistGramPosition() += yyleng;
            return LBRACKET;
          }
"("       { msUvDistGramPosition() += yyleng;
            return LPAREN;
          }
"]"       { msUvDistGramPosition() += yyleng;
            return RBRACKET;
          }
")"       { msUvDistGramPosition() += yyleng;
            return RPAREN;
          }

":"       { msUvDistGramPosition() += yyleng;
            return COLON; }
"=="      { msUvDistGramPosition() += yyleng;
            return EQ; }
"="       { msUvDistGramPosition() += yyleng;
            return EQASS; }
"!="      { msUvDistGramPosition() += yyleng;
            return NE; }
"<>"      { msUvDistGramPosition() += yyleng;
            return NE; }
">="      { msUvDistGramPosition() += yyleng;
            return GE; }
">"       { msUvDistGramPosition() += yyleng;
            return GT; }
"<="      { msUvDistGramPosition() += yyleng;
            return LE; }
"<"       { msUvDistGramPosition() += yyleng;
            return LT; }
"&&"      { msUvDistGramPosition() += yyleng;
            return AND; }
"||"      { msUvDistGramPosition() += yyleng;
            return OR; }
"!"       { msUvDistGramPosition() += yyleng;
            return NOT; }
"^"       { msUvDistGramPosition() += yyleng;
            return POWER; }
"*"       { msUvDistGramPosition() += yyleng;
            return TIMES; }
"/"       { msUvDistGramPosition() += yyleng;
            return DIVIDE; }
"%"       { msUvDistGramPosition() += yyleng;
            return PERCENT; }
"+"       { msUvDistGramPosition() += yyleng;
            return PLUS; }
"-"       { msUvDistGramPosition() += yyleng;
            return DASH; }
"{"       { msUvDistGramPosition() += yyleng;
            return LBRACE; }
"}"       { msUvDistGramPosition() += yyleng;
            return RBRACE; }
"'"       { msUvDistGramPosition() += yyleng;
            return SQUOTE; }
","       { msUvDistGramPosition() += yyleng;
            return COMMA;
          }

 /* Literals */
{INT}     { msUvDistGramPosition() += yyleng;
            lvalp->ival = atoi(MSUvDistGramtext);
            return NUMBER;
          }
{DISTANCEUNIT}  { msUvDistGramPosition() += yyleng;
                  return DISTANCEUNIT;
                }
{WAVELENTHUNIT} { msUvDistGramPosition() += yyleng;
                  return WAVELENTHUNIT;
                }

%%

