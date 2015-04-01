/* -*- C -*-
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

/* yy_unput is not used, so let flex not generate it, otherwise picky
   compilers will issue warnings. */
%option nounput

%{
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) result=msSpwGramInput(buf,max_size)

#undef YY_DECL
#define YY_DECL int MSSpwGramlex (YYSTYPE* lvalp)
static string                qstr;
%}

WHITE     [ \t\n]*
DIGIT     [0-9]
SIGN      [+-]
INT       {DIGIT}+
EXP       [DdEe]{SIGN}?{INT}
DOT       \.?
NUMBER    ({INT}|{INT}?{DOT}{INT}*)
FNUMBER   {SIGN}?{NUMBER}?{EXP}?

KILO       ([Kk])
MEGA       ([Mm])
GIGA       ([Gg])
TERA       ([Tt])
HERTZ      (([Hh][Zz]))
METER      (([m]))
SECOND     ([Ss]|([Ss][Ee][Cc]))
VELOCITY   (({KILO}|{MEGA})?({METER}[/]{SECOND}))
FREQ       (({KILO}|{MEGA}|{GIGA}|{TERA})?{HERTZ})
UNIT       (({FREQ}|{VELOCITY}))

QSTRING   \"[^\"\n]*\"
STRING    ({QSTRING})+

QUOTE     (\")
RQUOTE    (\/)
NQ        [^\\\n\"]+
NRQ       [^\\\n\/]+

NAME       ([a-zA-Z_'{''}''+''-'#]+[a-zA-Z0-9_{}#]*) 
/*NAME ([A-za-z0-9_'{''}''+''-'])*/
/*IDENTIFIER  ({NAME}+|STRING)*/
IDENTIFIER  ({NAME}+)
SIDENTIFIER  ([A-Za-z_*?{}'+''-'#][A-Za-z0-9_'{''}''+''-''*''?'#]*)
/*
SIDENTIFIER  ([A-Za-z_*?{}'+''-']+[A-Za-z0-9_'{''}''+''-''*''?':]*)
SIDENTIFIER  ({NAME}+['{''}''+''-''*''?']+)
NAMES       ([a-zA-Z_{}]+[a-zA-Z0-9_{}]*)
IDENTIFIER  ({NAMES}+|STRING)
SIDENTIFIER  ({NAMES}+"*")
*/
%x QS RS
/* rules */
%%
{QUOTE}   { // Start of a quoted string
            qstr.resize(0);
            BEGIN(QS);
          }

<QS>{NQ}  { 
            (qstr)+= MSSpwGramtext;
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
            (qstr)+= MSSpwGramtext;
          }

<RS>{RQUOTE} { /* saw closing quote - all done */
               BEGIN(INITIAL);
               lvalp->str = (char *)malloc((qstr.length() + 1)*sizeof(char));
               strcpy(lvalp->str,qstr.c_str());
               qstr.resize(0);
               return REGEX;
             }

{FNUMBER} {msSpwGramPosition() += yyleng;
            lvalp->str = (char *)malloc((strlen(MSSpwGramtext) + 1) * sizeof(char));
            strcpy(lvalp->str, MSSpwGramtext);
	    //	    cout << "FN = " << MSSpwGramtext << endl;
            return FNUMBER;
          } 
{UNIT}    { msSpwGramPosition() += yyleng;
            lvalp->str = (char *)malloc((strlen(MSSpwGramtext) + 1) * sizeof(char));
            strcpy(lvalp->str, MSSpwGramtext);
            return UNIT;
          }
"~"       { msSpwGramPosition() += yyleng;
            return DASH; }
","       { msSpwGramPosition() += yyleng;
            return COMMA;
          }
"<"       { msSpwGramPosition() += yyleng;
            return LT;
          }
">"       { msSpwGramPosition() += yyleng;
            return GT;
          }
"&"       { msSpwGramPosition() += yyleng;
            return AMPERSAND;
          }
";"       { msSpwGramPosition() += yyleng;
            return SEMICOLON;
          }
":"       { msSpwGramPosition() += yyleng;
            return COLON;
          }
"^"       { msSpwGramPosition() += yyleng;
            return CARET;
          }
"<>"      { msSpwGramPosition() += yyleng;
            return GTNLT;
          }
  /* Literals */

{IDENTIFIER} { msSpwGramPosition() += yyleng;
               lvalp->str = (char *)malloc((strlen(MSSpwGramtext) + 1) * sizeof(char));
               strcpy(lvalp->str, MSSpwGramtext);
	       //	       cout << "ID.l = " << MSSpwGramtext << endl;

               return IDENTIFIER;
             }
{SIDENTIFIER} { msSpwGramPosition() += yyleng;
                lvalp->str = (char *)malloc((strlen(MSSpwGramtext) + 1) * sizeof(char));
                strcpy(lvalp->str, MSSpwGramtext);
		//		cout << "QS.l = " << MSSpwGramtext << endl;

                return QSTRING;
              }
"("       { msSpwGramPosition() += yyleng; return LPAREN; }
")"       { msSpwGramPosition() += yyleng; return RPAREN;}
{WHITE}   { msSpwGramPosition() += yyleng;} /* Eat white spaces */
.         { msSpwGramPosition() += yyleng;return MSSpwGramtext[0];}
%%
