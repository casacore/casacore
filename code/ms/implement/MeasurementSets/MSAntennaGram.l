/*
    MSAntennaGram.l: Lexical analyzer for ms selection commands
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
#define YY_INPUT(buf,result,max_size) result=msAntennaGramInput(buf,max_size)

#undef YY_DECL
#define YY_DECL int MSAntennaGramlex (YYSTYPE* lvalp)
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

IDENTIFIER  [A-Za-z_:0-9]*

REGEX1    m"/"[^/]+"/"
REGEX2    m%[^%]+%
REGEX3    m#[^#]+#
REGEX     {REGEX1}|{REGEX2}|{REGEX3}

  /* rules */
%%

"["       { msAntennaGramPosition() += yyleng;
            return LBRACKET;
          }
"("       { msAntennaGramPosition() += yyleng;
            return LPAREN;
          }
"]"       { msAntennaGramPosition() += yyleng;
            return RBRACKET;
          }
")"       { msAntennaGramPosition() += yyleng;
            return RPAREN;
          }
":"       { msAntennaGramPosition() += yyleng;
            return COLON; }
"=="      { msAntennaGramPosition() += yyleng;
            return EQ; }
"="       { msAntennaGramPosition() += yyleng;
            return EQASS; }
"!="      { msAntennaGramPosition() += yyleng;
            return NE; }
"<>"      { msAntennaGramPosition() += yyleng;
            return NE; }
">="      { msAntennaGramPosition() += yyleng;
            return GE; }
">"       { msAntennaGramPosition() += yyleng;
            return GT; }
"<="      { msAntennaGramPosition() += yyleng;
            return LE; }
"<"       { msAntennaGramPosition() += yyleng;
            return LT; }
"&"       { msAntennaGramPosition() += yyleng;
            return AMPERSAND; }
"&&"      { msAntennaGramPosition() += yyleng;
            return AND; }
"||"      { msAntennaGramPosition() += yyleng;
            return OR; }
"!"       { msAntennaGramPosition() += yyleng;
            return NOT; }
"^"       { msAntennaGramPosition() += yyleng;
            return POWER; }
"*"       { msAntennaGramPosition() += yyleng;
            return STAR; }
"/"       { msAntennaGramPosition() += yyleng;
            return DIVIDE; }
"%"       { msAntennaGramPosition() += yyleng;
            return MODULO; }
"+"       { msAntennaGramPosition() += yyleng;
            return PLUS; }
"-"       { msAntennaGramPosition() += yyleng;
            return DASH; }
"{"       { msAntennaGramPosition() += yyleng;
            return LBRACE; }
"}"       { msAntennaGramPosition() += yyleng;
            return RBRACE; }
"'"       { msAntennaGramPosition() += yyleng;
            return SQUOTE; }
","       { msAntennaGramPosition() += yyleng;
            return COMMA;
          }

  /* Literals */

{IDENTIFIER} { msAntennaGramPosition() += yyleng;
               lvalp->str = MSAntennaGramtext;
               return IDENTIFIER;
             }
%%

