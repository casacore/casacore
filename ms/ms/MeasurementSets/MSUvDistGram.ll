/* -*- C -*-
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
NUMBER    {INT}|{INT}"."{DIGIT}*
FNUMBER   {NUMBER}?{EXP}?

KILO       ([Kk]?)
MEGA       (M?)
METER      (m?)
LAMBDA     ((("lambda")|("LAMBDA"))?)
WAVELENGTHUNIT   (({KILO}|{MEGA})?{LAMBDA})
DISTANCEUNIT     (({KILO})?{METER})
UNIT      ({DISTANCEUNIT}|{WAVELENGTHUNIT})

  /* rules */
%%

{FNUMBER}       { msUvDistGramPosition() += yyleng;
                  lvalp->dval = atof(MSUvDistGramtext);

                  return FNUMBER;
                }

 /* Literals */
{UNIT}          { msUvDistGramPosition() += yyleng;
                  lvalp->str = MSUvDistGramtext;

                  return UNIT;
                }
"~"       { msUvDistGramPosition() += yyleng;
            return DASH; }
":"       { msUvDistGramPosition() += yyleng;
            return COLON; }
","       { msUvDistGramPosition() += yyleng;
            return COMMA;
          }
">"       { msUvDistGramPosition() += yyleng;
            return GT;
          }
"<"       { msUvDistGramPosition() += yyleng;
            return LT;
          }
"%"       { msUvDistGramPosition() += yyleng;
            return PERCENT;
          }
{WHITE}   { msUvDistGramPosition() += yyleng;} /* Eat white spaces */
.         { msUvDistGramPosition() += yyleng;return MSUvDistGramtext[0];}
%%
