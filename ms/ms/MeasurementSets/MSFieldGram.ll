/* -*- C -*-
    MSFieldGram.l: Lexical analyzer for ms selection commands
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
static string                qstr;
#include <ms/MeasurementSets/MSSelectionTools.h>
%}

WHITE     [ \t\n]*
DIGIT     [0-9]
INT       ({WHITE}{DIGIT}+{WHITE})

QSTRING   \"[^\"\n]*\"
STRING    ({QSTRING})+

QUOTE     (\")
RQUOTE    (\/)
NQ        [^\\\n\"]+
NRQ       [^\\\n\/]+

/*
NAME        ([A-za-z0-0_'{''}''+''-'])
*/
IDENTIFIER  ([A-Za-z0-9_\{\}\+\-\.]+|STRING)
SIDENTIFIER ({WHITE}[A-Za-z0-9_'{''}''+''-''*''?' ]+{WHITE})

%x QS RS ESC
/* rules */
%%
{QUOTE}   { // Start of a quoted string
            qstr.resize(0);
            BEGIN(QS);
          }

"\\"      {printf("%s\n",MSFieldGramtext);BEGIN(ESC);}
<ESC>.    {BEGIN(INITIAL);}

<QS>{NQ}  {(qstr)+= MSFieldGramtext;}

<QS>{QUOTE} { /* saw closing quote - all done */
               BEGIN(INITIAL);
               lvalp->str = (char *)malloc((qstr.length() + 1)*sizeof(char));
               strcpy(lvalp->str,qstr.c_str());
               qstr.resize(0);

               return QSTRING;
             }

{RQUOTE}  { // Start of a regex string
            qstr.resize(0);
            BEGIN(RS);
          }

<RS>{NRQ} {(qstr)+= MSFieldGramtext;}

<RS>{RQUOTE} { /* saw closing quote - all done */
               BEGIN(INITIAL);
               lvalp->str = (char *)malloc((qstr.length() + 1)*sizeof(char));
               strcpy(lvalp->str,qstr.c_str());
               qstr.resize(0);

               return REGEX;
             }

{INT}     { msFieldGramPosition() += yyleng;
            lvalp->str = (char *)malloc((strlen(MSFieldGramtext) + 1) * sizeof(char));
            strcpy(lvalp->str, stripWhite(MSFieldGramtext).c_str());
	    //cout << "INT  = \"" << MSFieldGramtext << "\" \"" << lvalp->str << "\"" << endl;

            return INT;
          }

"~"       { msFieldGramPosition() += yyleng; return DASH;}
","       { msFieldGramPosition() += yyleng; return COMMA;}
"<"       { msFieldGramPosition() += yyleng; return LT;}
">"       { msFieldGramPosition() += yyleng; return GT;}
"&"       { msFieldGramPosition() += yyleng; return AMPERSAND;}
  /* Literals */


{IDENTIFIER} { msFieldGramPosition() += yyleng;
               lvalp->str = (char *)malloc((strlen(MSFieldGramtext) + 1) * sizeof(char));
               strcpy(lvalp->str, MSFieldGramtext);
 	       //cout << "ID = \"" << MSFieldGramtext << "\" \"" << lvalp->str << "\"" << endl;

               return IDENTIFIER;
             }
{SIDENTIFIER} { msFieldGramPosition() += yyleng;
                lvalp->str = (char *)malloc((strlen(MSFieldGramtext) + 1) * sizeof(char));
                strcpy(lvalp->str, stripWhite(MSFieldGramtext).c_str());
 		//cout << "SID = \"" << MSFieldGramtext << "\" \"" << lvalp->str << "\"" << endl;
                return QSTRING;
              }
"("       { msFieldGramPosition() += yyleng; return LPAREN;}
")"       { msFieldGramPosition() += yyleng; return RPAREN;}
.         { msFieldGramPosition() += yyleng; return MSFieldGramtext[0];}
%%
