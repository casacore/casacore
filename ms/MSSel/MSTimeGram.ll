/* -*- C++ -*-
    MSTimeGram.l: Lexical analyzer for ms selection commands
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
#define YY_INPUT(buf,result,max_size) result=msTimeGramInput((char*) buf,max_size)

#undef YY_DECL
#define YY_DECL int MSTimeGramlex (YYSTYPE* lvalp)
%}

WHITE     [ \t\n]*
DIGIT     [0-9]
INT       {DIGIT}+
EXP       [DdEe][+-]?{INT}
POINT     \.
FNUMBER    ({INT}|{INT}{POINT}|{POINT}{INT}|{INT}{POINT}{INT})
NUMBER    {INT}
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

{WHITE}   {msTimeGramPosition() += yyleng;;} /* Just eat the white spaces */
":"       { msTimeGramPosition() += yyleng;
            return COLON; }
">"       { msTimeGramPosition() += yyleng;
            return GT; }
"<="      { msTimeGramPosition() += yyleng;
            return LE; }
"<"       { msTimeGramPosition() += yyleng;
            return LT; }
"&&"      { msTimeGramPosition() += yyleng;
            return AND; }
"||"      { msTimeGramPosition() += yyleng;
            return OR; }
"!"       { msTimeGramPosition() += yyleng;
            return NOT; }
"."       { msTimeGramPosition() += yyleng;
            return DOT; }
"/"       { msTimeGramPosition() += yyleng;
            return SLASH; }
"%"       { msTimeGramPosition() += yyleng;
            return PERCENT; }
"+"       { msTimeGramPosition() += yyleng;
            return PLUS; }
"~"       { msTimeGramPosition() += yyleng;
            return DASH; }
"'"       { msTimeGramPosition() += yyleng;
            return SQUOTE; }
","       { msTimeGramPosition() += yyleng;
            return COMMA;
          }
"*"       { msTimeGramPosition() += yyleng;
            return STAR;
          }
"["       { msTimeGramPosition() += yyleng; return LSQBRACKET;}
"]"       { msTimeGramPosition() += yyleng; return RSQBRACKET;}

 /* Literals */
{NUMBER}        { msTimeGramPosition() += yyleng;
                  lvalp->ival = atoi((const char *) MSTimeGramtext);
                  return NUMBER;
                }
{FNUMBER}        { msTimeGramPosition() += yyleng;
                  lvalp->dval = atof((const char *) MSTimeGramtext);
                  return FNUMBER;
                }
. {return UNKNOWN;}
%%
