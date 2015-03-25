/* -*- C -*-
    MSObservationGram.l: Lexical analyzer for ms selection commands
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
#define YY_INPUT(buf,result,max_size) result=msObservationGramInput(buf,max_size)

#undef YY_DECL
#define YY_DECL int MSObservationGramlex (YYSTYPE* lvalp)
static string                qstr;
#include <casacore/ms/MSSel/MSSelectionTools.h>
%}

WHITE     [ \t\n]*
DIGIT     [0-9]
INT       ({WHITE}{DIGIT}+{WHITE})

/* rules */
%%
{INT}     { msObservationGramPosition() += yyleng;
            lvalp->str = (char *)malloc((strlen(MSObservationGramtext) + 1) * sizeof(char));
            strcpy(lvalp->str, stripWhite(MSObservationGramtext).c_str());
	    //	    cout << "INT  = \"" << MSObservationGramtext << "\" \"" << lvalp->str << "\"" << endl;

            return INT;
          }

"~"       { msObservationGramPosition() += yyleng;
            return DASH; }
","       { msObservationGramPosition() += yyleng;
            return COMMA;
          }
"<"       { msObservationGramPosition() += yyleng;
            return LT;
          }
">"       { msObservationGramPosition() += yyleng;
            return GT;
          }
"<="       { msObservationGramPosition() += yyleng;
            return LE;
          }
">="       { msObservationGramPosition() += yyleng;
            return GE;
          }
"&"       { msObservationGramPosition() += yyleng;
            return AMPERSAND;
          }
  /* Literals */

"("       { msObservationGramPosition() += yyleng; return LPAREN; }
")"       { msObservationGramPosition() += yyleng; return RPAREN;}
{WHITE}   { msObservationGramPosition() += yyleng;} /* Eat white spaces */
.         { msObservationGramPosition() += yyleng;return MSObservationGramtext[0];}
%%
