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
  Double dval[2];
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
%nonassoc EQ EQASS GT GE LT LE NE
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
                 cout << "timestatement:" << endl;
                 $$ = $2;
               }
             ;

timeexpr: singletimeexpr
        | rangetimeexpr
        | lowboundtimeexpr
        | upboundtimeexpr
        ;

singletimeexpr: daytimeexpr {
                 cout << "singletimeexpr: daytimeexpr" << endl;
                  $$ = MSTimeParse().selectTime(*($1), true);
                }
              | yeartimeexpr {
                 cout << "singletimeexpr: yeartimeexpr" << endl;
                  $$ = MSTimeParse().selectTime(*($1), false);
                }
              ;

rangetimeexpr: daytimeexpr DASH daytimeexpr {
                 cout << "rangetimeexpr: daytimeexpr" << endl;
                 $$ = MSTimeParse().selectTimeRange(*($1), *($3), true);
               }
             | yeartimeexpr DASH yeartimeexpr {
                 cout << "rangetimeexpr: yeartimeexpr" << endl;
                 $$ = MSTimeParse().selectTimeRange(*($1), *($3), false);
               }
             ;

lowboundtimeexpr: GT daytimeexpr {
                 cout << "lowboundtimeexpr: daytimeexpr" << endl;
                    $$ = MSTimeParse().selectTimeGT(*($2), true);
                  }
                |
                  GT yeartimeexpr {
                 cout << "lowboundtimeexpr: yeartimeexpr" << endl;
                    $$ = MSTimeParse().selectTimeGT(*($2), false);
                  }
                ;

upboundtimeexpr: LT daytimeexpr {
                 cout << "upboundtimeexpr: daytimeexpr" << endl;
                   $$ = MSTimeParse().selectTimeLT(*($2), true);
                 }
               |
                 LT yeartimeexpr {
                 cout << "upboundtimeexpr: yeartimeexpr" << endl;
                   $$ = MSTimeParse().selectTimeLT(*($2), false);
                 }
               ;

daytimeexpr: NUMBER SLASH NUMBER {
                 cout << "daytimeexpr: 2" << endl;
               $$ = MSTimeParse::dayTimeConvert($1, $3);
             }
           | NUMBER SLASH NUMBER COLON NUMBER {
                 cout << "daytimeexpr: 3" << endl;
               $$ = MSTimeParse::dayTimeConvert($1, $3, $5);
             }
           | NUMBER SLASH NUMBER COLON NUMBER COLON NUMBER {
                 cout << "daytimeexpr: 4" << endl;
               $$ = MSTimeParse::dayTimeConvert($1, $3, $5, $7);
             }
           | NUMBER SLASH NUMBER COLON NUMBER COLON NUMBER DOT NUMBER {
                 cout << "daytimeexpr: 5" << endl;
               $$ = MSTimeParse::dayTimeConvert($1, $3, $5, $7, $9);
             }
           ;

yeartimeexpr: NUMBER SLASH NUMBER SLASH NUMBER {
                 cout << "yeartimeexpr: 3" << endl;
                $$ = MSTimeParse::yearTimeConvert($1, $3, $5);
              }
            | NUMBER SLASH NUMBER SLASH NUMBER SLASH NUMBER {
                 cout << "yeartimeexpr: 4" << endl;
                $$ = MSTimeParse::yearTimeConvert($1, $3, $5, $7);
              }
            | NUMBER SLASH NUMBER SLASH NUMBER SLASH NUMBER
              COLON NUMBER {
                 cout << "yeartimeexpr: 5" << endl;
                $$ = MSTimeParse::yearTimeConvert($1, $3, $5, $7, $9);
              }
            | NUMBER SLASH NUMBER SLASH NUMBER SLASH NUMBER
              COLON NUMBER COLON NUMBER {
                 cout << "yeartimeexpr: 6" << endl;
                $$ = MSTimeParse::yearTimeConvert($1, $3, $5, $7, $9, $11);
              }
            | NUMBER SLASH NUMBER SLASH NUMBER SLASH NUMBER
              COLON NUMBER COLON NUMBER DOT NUMBER {
                 cout << "yeartimeexpr: 7" << endl;
                $$ = MSTimeParse::yearTimeConvert($1, $3, $5, $7, $9, $11, $13);
              }
            ;

%%

