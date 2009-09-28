/* -*- C -*-
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

/* yy_unput is not used, so let flex not generate it, otherwise picky
   compilers will issue warnings. */
%option nounput

%{
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) result=msAntennaGramInput(buf,max_size)

#undef YY_DECL
#define YY_DECL int MSAntennaGramlex (YYSTYPE* lvalp)
static string                qstr;
%}

WHITE     [ \t\n]*
DIGIT     [0-9]
INT       {DIGIT}+

QSTRING   \"[^\"\n]*\"
STRING    ({QSTRING})+

QUOTE     (\")
RQUOTE    (\/)
NQ        [^\\\n\"]+
NRQ       [^\\\n\/]+

/* 
NAMES       ([a-zA-Z_{}]+[a-zA-Z:0-9_{}]*) 
IDENTIFIER  ({NAMES}+|STRING)
SIDENTIFIER  ([A-Za-z_'{''}''*''-''+''*']+[A-Za-z:0-9_'{''}''*''+''-']+)
*/

NAME ([A-za-z0-9_'{''}''+''-':])
		 /*IDENTIFIER  ([A-Za-z0-9_{}+-:]+|STRING)*/
IDENTIFIER  ({NAME}+|STRING)
SIDENTIFIER  ([A-Za-z0-9_'{''}''+''-''*''?':]+)
		 /* SIDENTIFIER  ({NAMES}+"*") */
		 /* IDENTIFIER  ([A-Za-z:0-9_{}]+|STRING)*/
		 /* SIDENTIFIER  ([A-Za-z:0-9_{}]+"*")   */
%x QS RS
/* rules */
%%
{QUOTE}   { // Start of a quoted string
            qstr.resize(0);
            BEGIN(QS);
          }

<QS>{NQ}  { 
            (qstr)+= MSAntennaGramtext;
          }

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

<RS>{NRQ} {
            (qstr)+= MSAntennaGramtext;
          }

<RS>{RQUOTE} { /* saw closing quote - all done */
               BEGIN(INITIAL);
               lvalp->str = (char *)malloc((qstr.length() + 1)*sizeof(char));
               strcpy(lvalp->str,qstr.c_str());
               qstr.resize(0);

               return REGEX;
             }


{INT}     { msAntennaGramPosition() += yyleng;
            lvalp->str = (char *)malloc((strlen(MSAntennaGramtext) + 1) * sizeof(char));
            strcpy(lvalp->str, MSAntennaGramtext);
            
            return INT;
          }

";"       { msAntennaGramPosition() += yyleng;  return SEMICOLON; }
"&"       { msAntennaGramPosition() += yyleng;  return AMPERSAND; }
"~"       { msAntennaGramPosition() += yyleng;  return DASH; }
","       { msAntennaGramPosition() += yyleng;  return COMMA;}
"!"       { msAntennaGramPosition() += yyleng;  return NOT;}
  /* Literals */

{IDENTIFIER} { msAntennaGramPosition() += yyleng;
               lvalp->str = (char *)malloc((strlen(MSAntennaGramtext) + 1) * sizeof(char));
               strcpy(lvalp->str, MSAntennaGramtext);

               return IDENTIFIER;
             }
{SIDENTIFIER} { msAntennaGramPosition() += yyleng;
                lvalp->str = (char *)malloc((strlen(MSAntennaGramtext) + 1) * sizeof(char));
                strcpy(lvalp->str, MSAntennaGramtext);

                return QSTRING;
              }
"("       { msAntennaGramPosition() += yyleng; return LPAREN; }
")"       { msAntennaGramPosition() += yyleng; return RPAREN;}
{WHITE}   { msAntennaGramPosition() += yyleng;} /* Eat white spaces */
.         { msAntennaGramPosition() += yyleng;return MSAntennaGramtext[0];}
%%
