/*
    MSSpwGram.l: Lexical analyzer for ms selection commands
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
#define YY_INPUT(buf,result,max_size) result=msSPWGramInput(buf,max_size)

#undef YY_DECL
#define YY_DECL int MSSpwGramlex (YYSTYPE* lvalp)
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

UVDIST     [Uu][Vv][Dd][Ii][Ss][Tt]
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

"["       { msSPWGramPosition() += yyleng;
            return LBRACKET;
          }
"("       { msSPWGramPosition() += yyleng;
            return LPAREN;
          }
"]"       { msSPWGramPosition() += yyleng;
            return RBRACKET;
          }
")"       { msSPWGramPosition() += yyleng;
            return RPAREN;
          }

":"       { msSPWGramPosition() += yyleng;
            return COLON; }
"=="      { msSPWGramPosition() += yyleng;
            return EQ; }
"="       { msSPWGramPosition() += yyleng;
            return EQASS; }
"!="      { msSPWGramPosition() += yyleng;
            return NE; }
"<>"      { msSPWGramPosition() += yyleng;
            return NE; }
">="      { msSPWGramPosition() += yyleng;
            return GE; }
">"       { msSPWGramPosition() += yyleng;
            return GT; }
"<="      { msSPWGramPosition() += yyleng;
            return LE; }
"<"       { msSPWGramPosition() += yyleng;
            return LT; }
"&&"      { msSPWGramPosition() += yyleng;
            return AND; }
"||"      { msSPWGramPosition() += yyleng;
            return OR; }
"!"       { msSPWGramPosition() += yyleng;
            return NOT; }
"^"       { msSPWGramPosition() += yyleng;
            return POWER; }
"*"       { msSPWGramPosition() += yyleng;
            return TIMES; }
"/"       { msSPWGramPosition() += yyleng;
            return DIVIDE; }
"%"       { msSPWGramPosition() += yyleng;
            return PERCENT; }
"+"       { msSPWGramPosition() += yyleng;
            return PLUS; }
"-"       { msSPWGramPosition() += yyleng;
            return DASH; }
"{"       { msSPWGramPosition() += yyleng;
            return LBRACE; }
"}"       { msSPWGramPosition() += yyleng;
            return RBRACE; }
"'"       { msSPWGramPosition() += yyleng;
            return SQUOTE; }
","       { msSPWGramPosition() += yyleng;
            return COMMA;
          }

 /* Literals */

%%

