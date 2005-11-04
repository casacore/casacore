/*
    MSTimeGram.y: Parser for time expressions
    Copyright (C) 2004
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
using namespace casa;
%}

%pure_parser                /* make parser re-entrant */

%union {
  const TableExprNode* node;
  Block<TableExprNode>* exprb;
  TableExprNodeSetElem* elem;
  TableExprNodeSet* settp;
  const MEpoch* tval;
  Int ival;
  Double dval;
}

%token EQASS
%token <ival> NUMBER
%token SQUOTE

%token DASH
%token LT
%token GT
%token COLON
%token COMMA
%token SLASH
%token DOT
%token PERCENT

%token LBRACKET
%token LPAREN
%token RBRACKET
%token RPAREN
%token LBRACE
%token RBRACE

%type <node> timestatement
%type <node> timeexpr
%type <node> singletimeexpr
%type <node> rangetimeexpr
%type <node> upboundtimeexpr
%type <node> lowboundtimeexpr
%type <tval> daytimeexpr
%type <tval> yeartimeexpr

%left OR
%left AND
%nonassoc EQ EQASS GT GE LT LE NE COLON SLASH
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%nonassoc UNARY
%nonassoc NOT
%right POWER

%{
int MSTimeGramlex (YYSTYPE*);
%}

%%
timestatement: SQUOTE timeexpr SQUOTE {
                 $$ = $2;
               }
             ;

timeexpr: singletimeexpr
        | rangetimeexpr
        | lowboundtimeexpr
        | upboundtimeexpr
        ;

singletimeexpr: daytimeexpr {
                  $$ = MSTimeParse().selectTime(MEpoch(*($1)), true);
                }
              | yeartimeexpr {
                  $$ = MSTimeParse().selectTime(MEpoch(*($1)), false);
                }
              ;

rangetimeexpr: daytimeexpr DASH daytimeexpr {
                 $$ = MSTimeParse().selectTimeRange(MEpoch(*($1)),
                                                    MEpoch(*($3)), true);
               }
             | yeartimeexpr DASH yeartimeexpr {
                 $$ = MSTimeParse().selectTimeRange(MEpoch(*($1)),
                                                    MEpoch(*($3)), false);
               }
             ;

lowboundtimeexpr: GT daytimeexpr {
                    $$ = MSTimeParse().selectTimeGT(MEpoch(*($2)), true);
                  }
                |
                  GT yeartimeexpr {
                    $$ = MSTimeParse().selectTimeGT(MEpoch(*($2)), false);
                  }
                ;

upboundtimeexpr: LT daytimeexpr {
                   $$ = MSTimeParse().selectTimeLT(MEpoch(*($2)), true);
                 }
               |
                 LT yeartimeexpr {
                   $$ = MSTimeParse().selectTimeLT(MEpoch(*($2)), false);
                 }
               ;

daytimeexpr: NUMBER SLASH NUMBER COLON NUMBER COLON NUMBER DOT NUMBER {
               $$ = MSTimeParse::dayTimeConvert($1, $3, $5, $7, $9);
             }
           | NUMBER SLASH NUMBER COLON NUMBER COLON NUMBER {
               $$ = MSTimeParse::dayTimeConvert($1, $3, $5, $7);
             }
           | NUMBER SLASH NUMBER COLON NUMBER {
               $$ = MSTimeParse::dayTimeConvert($1, $3, $5);
             }
           | NUMBER SLASH NUMBER {
               $$ = MSTimeParse::dayTimeConvert($1, $3);
             }
           ;

yeartimeexpr: NUMBER SLASH NUMBER SLASH NUMBER SLASH NUMBER
              COLON NUMBER COLON NUMBER DOT NUMBER {
                $$ = MSTimeParse::yearTimeConvert($1, $3, $5, $7, $9, $11, $13);
              }
            | NUMBER SLASH NUMBER SLASH NUMBER SLASH NUMBER
              COLON NUMBER COLON NUMBER {
                $$ = MSTimeParse::yearTimeConvert($1, $3, $5, $7, $9, $11);
              }
            | NUMBER SLASH NUMBER SLASH NUMBER SLASH NUMBER
              COLON NUMBER {
                $$ = MSTimeParse::yearTimeConvert($1, $3, $5, $7, $9);
              }
            | NUMBER SLASH NUMBER SLASH NUMBER SLASH NUMBER {
                $$ = MSTimeParse::yearTimeConvert($1, $3, $5, $7);
              }
            | NUMBER SLASH NUMBER SLASH NUMBER {
                $$ = MSTimeParse::yearTimeConvert($1, $3, $5);
              }
            ;

%%

