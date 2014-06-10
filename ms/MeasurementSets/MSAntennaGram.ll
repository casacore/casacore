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
%}

WHITE     [ \t\n]*
DIGIT     [0-9]
INT       {DIGIT}+
EXP       [DdEe][+-]?{INT}
FLOAT     {INT}{EXP}|{INT}"."{DIGIT}*({EXP})?|{DIGIT}*"."{INT}({EXP})?

QSTRING   \"[^\"\n]*\"
ASTRING   \'[^\'\n]*\'
STRING    ({QSTRING}|{ASTRING})+
REGEX     "^"?"/"[^/\n]*"/"

ALPHA     [a-zA-Z]
UNIT      {ALPHA}+
NAMECHAR  [-+a-zA-Z0-9_.]
NAMENNUM  [a-zA-Z_]
COLON     ":"
NAME1     {NAMENNUM}{NAMECHAR}*
NAME2     {NAMECHAR}*{COLON}({NAMECHAR}|{COLON})*
NAME      {NAME1}|{NAME2}
ESCNAME   \\[^ \t\n]+
PATTCHAR  [][*?^]
PATT1     ({NAMENNUM}|{PATTCHAR})({NAMECHAR}|{PATTCHAR})*
PATT2     ({NAMECHAR}|{PATTCHAR})*{COLON}({NAMECHAR}|{PATTCHAR}|{COLON})*
PATTERN   {PATT1}|{PATT2}


/* rules */
%%

{STRING}  {
             int lenstr = strlen(MSAntennaGramtext) - 2;
             lvalp->str = (char*)malloc(lenstr+1);
             strncpy(lvalp->str, MSAntennaGramtext+1, lenstr);
             lvalp->str[lenstr] = '\0';
             return QSTRING;
          }

{REGEX}   {
             int lenstr = strlen(MSAntennaGramtext) - 2;
             lvalp->str = (char*)malloc(lenstr+1);
             strncpy(lvalp->str, MSAntennaGramtext+1, lenstr);
             if (MSAntennaGramtext[0] == '^') lvalp->str[0] = '^';
             lvalp->str[lenstr] = '\0';
             return REGEX;
          }


{UNIT}    {
             msAntennaGramPosition() += yyleng;
             lvalp->str = (char*)malloc(strlen(MSAntennaGramtext) + 1);
             strcpy(lvalp->str, MSAntennaGramtext);
             return UNIT;
          }

{NAME}    {
             msAntennaGramPosition() += yyleng;
             lvalp->str = (char *)malloc((strlen(MSAntennaGramtext) + 1));
             strcpy(lvalp->str, MSAntennaGramtext);
             return IDENTIFIER;
          }

{ESCNAME} {
             msAntennaGramPosition() += yyleng;
             lvalp->str = (char *)malloc((strlen(MSAntennaGramtext)));
             strcpy(lvalp->str, MSAntennaGramtext+1);
             return IDENTIFIER;
          }

{PATTERN} {
             msAntennaGramPosition() += yyleng;
             lvalp->str = (char *)malloc((strlen(MSAntennaGramtext) + 1));
             strcpy(lvalp->str, MSAntennaGramtext);
             return QSTRING;
          }

{INT}     {
             msAntennaGramPosition() += yyleng;
             lvalp->str = (char*)malloc(strlen(MSAntennaGramtext) + 1);
             strcpy(lvalp->str, MSAntennaGramtext);
             return INT;
          }

{FLOAT}   {
             msAntennaGramPosition() += yyleng;
             lvalp->str = (char*)malloc(strlen(MSAntennaGramtext) + 1);
             strcpy(lvalp->str, MSAntennaGramtext);
             return FLOAT;
          }

";"       { msAntennaGramPosition() += yyleng; return SEMICOLON; }
"@"       { msAntennaGramPosition() += yyleng; return AT; }
"&"       { msAntennaGramPosition() += yyleng; return AMPERSAND; }
"~"       { msAntennaGramPosition() += yyleng; return DASH; }
","       { msAntennaGramPosition() += yyleng; return COMMA;}
"!"       { msAntennaGramPosition() += yyleng; return NOT;}
"("       { msAntennaGramPosition() += yyleng; return LPAREN; }
")"       { msAntennaGramPosition() += yyleng; return RPAREN;}
"<"       { msAntennaGramPosition() += yyleng; return LT;}
"<="      { msAntennaGramPosition() += yyleng; return LE;}
">"       { msAntennaGramPosition() += yyleng; return GT;}
">="      { msAntennaGramPosition() += yyleng; return GE;}

{WHITE}   { msAntennaGramPosition() += yyleng;} /* Eat whitespace */

 /* An unterminated string is an error */
\'|\"     { throw MSSelectionAntennaError ("Unterminated string"); }

 /* An unterminated regex is an error */
"/"       { throw MSSelectionAntennaError ("Unterminated regex"); }

 /* terminate on EOF */
<<EOF>>   { yyterminate(); }

 /* Any other character is invalid */
.         { return YYERRCODE; }

%%
