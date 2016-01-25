/* -*- C -*-
    MSFeedGram.l: Lexical analyzer for ms feed selection commands
    Copyright (C) 2015
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
#define YY_INPUT(buf,result,max_size) result=msFeedGramInput(buf,max_size)

#undef YY_DECL
#define YY_DECL int MSFeedGramlex (YYSTYPE* lvalp)
%}

WHITE     [ \t\n]*
DIGIT     [0-9]
INT       {DIGIT}+

/* rules */
%%

{INT}     {
             msFeedGramPosition() += yyleng;
             lvalp->str = (char*)malloc(strlen(MSFeedGramtext) + 1);
             strcpy(lvalp->str, MSFeedGramtext);
             return INT;
          }


";"       { msFeedGramPosition() += yyleng; return SEMICOLON; }
"&"       { msFeedGramPosition() += yyleng; return AMPERSAND; }
"~"       { msFeedGramPosition() += yyleng; return DASH; }
","       { msFeedGramPosition() += yyleng; return COMMA;}
"!"       { msFeedGramPosition() += yyleng; return NOT;}


{WHITE}   { msFeedGramPosition() += yyleng;} /* Eat whitespace */

 /* An unterminated string is an error */
\'|\"     { throw MSSelectionFeedError ("Unterminated string"); }

 /* terminate on EOF */
<<EOF>>   { yyterminate(); }

 /* Any other character is invalid */
.         { return YYERRCODE; }

%%
