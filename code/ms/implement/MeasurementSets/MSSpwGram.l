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
#define YY_INPUT(buf,result,max_size) result=msSpwGramInput(buf,max_size)

#undef YY_DECL
#define YY_DECL int MSSpwGramlex (YYSTYPE* lvalp)
%}

WHITE     [ \t\n]*
DIGIT     [0-9]
INT       {DIGIT}+
EXP       [DdEe][+-]?{INT}
FNUMBER   {INT}|({INT}"."{DIGIT}*)
TRUE      T
FALSE     F

QSTRING   \"[^\"\n]*\"
ASTRING   \'[^\'\n]*\'
UQSTRING   \"[^\"\n]*\n
UASTRING   \'[^\'\n]*\n
STRING    ({QSTRING}|{ASTRING})+
USTRING   ({UQSTRING}|{UASTRING})+

FREQUENCYUNIT    [Mm][Hh][Zz]
VELOCITYUNIT     [Kk][Mm]"/"[Ss]
SPWNAME    [A-Za-z]([A-Za-z0-9])*

  /* rules */

%%

"["       { msSpwGramPosition() += yyleng;
            return LBRACKET;
          }
"("       { msSpwGramPosition() += yyleng;
            return LPAREN;
          }
"]"       { msSpwGramPosition() += yyleng;
            return RBRACKET;
          }
")"       { msSpwGramPosition() += yyleng;
            return RPAREN;
          }

":"       { msSpwGramPosition() += yyleng;
            return COLON; }
"=="      { msSpwGramPosition() += yyleng;
            return EQ; }
"="       { msSpwGramPosition() += yyleng;
            return EQASS; }
"!="      { msSpwGramPosition() += yyleng;
            return NE; }
"<>"      { msSpwGramPosition() += yyleng;
            return NE; }
">="      { msSpwGramPosition() += yyleng;
            return GE; }
">"       { msSpwGramPosition() += yyleng;
            return GT; }
"<="      { msSpwGramPosition() += yyleng;
            return LE; }
"<"       { msSpwGramPosition() += yyleng;
            return LT; }
"||"      { msSpwGramPosition() += yyleng;
            return OR; }
"!"       { msSpwGramPosition() += yyleng;
            return NOT; }
"^"       { msSpwGramPosition() += yyleng;
            return POWER; }
"*"       { msSpwGramPosition() += yyleng;
            return TIMES; }
"/"       { msSpwGramPosition() += yyleng;
            return SLASH; }
"%"       { msSpwGramPosition() += yyleng;
            return MODULO; }
"+"       { msSpwGramPosition() += yyleng;
            return PLUS; }
"-"       { msSpwGramPosition() += yyleng;
            return DASH; }
"{"       { msSpwGramPosition() += yyleng;
            return LBRACE; }
"}"       { msSpwGramPosition() += yyleng;
            return RBRACE; }
"'"       { msSpwGramPosition() += yyleng;
            return SQUOTE; }
","       { msSpwGramPosition() += yyleng;
            return COMMA;
          }

  /* Literals */
{FREQUENCYUNIT}  { msSpwGramPosition() += yyleng;
                   return FREQUENCYUNIT;
                 }
{VELOCITYUNIT}  { msSpwGramPosition() += yyleng;
                  return VELOCITYUNIT;
                }

{FNUMBER} { msSpwGramPosition() += yyleng;
            lvalp->dval = atof(MSSpwGramtext);
            return NUMBER;
	  }

{SPWNAME} { msSpwGramPosition() += yyleng;
            lvalp->str = MSSpwGramtext;
            return SPWNAME;
	  }

%%

