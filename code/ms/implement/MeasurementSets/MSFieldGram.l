/*
    MSFieldGram.l: Lexical analyzer for ms commands
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
#define YY_INPUT(buf,result,max_size) result=msFieldGramInput(buf,max_size)

#undef YY_DECL
#define YY_DECL int MSFieldGramlex (YYSTYPE* lvalp)
%}


WHITE     [ \t\n]*
DIGIT     [0-9]
INT       {DIGIT}+
EXP       [DdEe][+-]?{INT}
FLOAT     {INT}{EXP}|{INT}"."{DIGIT}*({EXP})?|{DIGIT}*"."{INT}({EXP})?
FLINT     {FLOAT}|{INT}
TRUE      T
FALSE     F

QSTRING   \"[^\"\n]*\"
ASTRING   \'[^\'\n]*\'
UQSTRING   \"[^\"\n]*\n
UASTRING   \'[^\'\n]*\n
STRING    ({QSTRING}|{ASTRING})+
USTRING   ({UQSTRING}|{UASTRING})+

NAMEORCODE [A-Za-z0-9]*[+-]*[A-Za-z_0-9]*|[A-Za-z0-9]*"*"

REGEX1    m"/"[^/]+"/"
REGEX2    m%[^%]+%
REGEX3    m#[^#]+#
REGEX     {REGEX1}|{REGEX2}|{REGEX3}

  /* rules */

%%

"["       { msFieldGramPosition() += yyleng;
            return LBRACKET;
          }
"("       { msFieldGramPosition() += yyleng;
            return LPAREN;
          }
"]"       { msFieldGramPosition() += yyleng;
            return RBRACKET;
          }
")"       { msFieldGramPosition() += yyleng;
            return RPAREN;
          }
"-"       { msFieldGramPosition() += yyleng;
            return DASH; 
          }
":"       { msFieldGramPosition() += yyleng;
            return COLON; }
"=="      { msFieldGramPosition() += yyleng; 
            return EQ; }
"="       { msFieldGramPosition() += yyleng;
            return EQASS; }
"!="      { msFieldGramPosition() += yyleng;
            return NE; }
"<>"      { msFieldGramPosition() += yyleng;
            return NE; }
">="      { msFieldGramPosition() += yyleng;
            return GE; }
">"       { msFieldGramPosition() += yyleng;
            return GT; }
"<="      { msFieldGramPosition() += yyleng;
            return LE; }
"<"       { msFieldGramPosition() += yyleng;
            return LT; }
"&&"      { msFieldGramPosition() += yyleng;
            return AND; }
"||"      { msFieldGramPosition() += yyleng;
            return OR; }
"!"       { msFieldGramPosition() += yyleng;
            return NOT; }
"^"       { msFieldGramPosition() += yyleng;
            return POWER; }
"*"       { msFieldGramPosition() += yyleng;
            return TIMES; }
"/"       { msFieldGramPosition() += yyleng;
            return DIVIDE; }
"%"       { msFieldGramPosition() += yyleng;
            return MODULO; }
"+"       { msFieldGramPosition() += yyleng;
            return PLUS; }
"{"       { msFieldGramPosition() += yyleng;
            return LBRACE; }
"}"       { msFieldGramPosition() += yyleng;
            return RBRACE; }
"'"       { msFieldGramPosition() += yyleng;
            return SQUOTE; }
","       { msFieldGramPosition() += yyleng;
            return COMMA;
          }

 /* Literals */

{INT}     {
            msFieldGramPosition() += yyleng;
            lvalp->ival = atoi(MSFieldGramtext);
            return INDEX;
	  }
{NAMEORCODE}  {
                msFieldGramPosition() += yyleng;
                lvalp->str = MSFieldGramtext;
	        return NAMEORCODE;
              }

%%

