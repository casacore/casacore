/*
    MSUvDistGram.y: Parser for UV distribution expressions
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
  Int ival;
  char * str;
  Double dval;
}

%token <str> UNIT
%token EQASS
%token <dval> FNUMBER
%token SQUOTE
%token DASH
%token LT
%token GT
%token COLON
%token COMMA
%token PERCENT

%token LBRACKET
%token LPAREN
%token RBRACKET
%token RPAREN
%token LBRACE
%token RBRACE

%type <node> uvdiststatement
%type <node> uvdistexpr
%type <node> rangeexprlist
%type <node> rangeexpr
%type <node> upuvbound
%type <node> lowuvbound
%type <node> uvdistwithfract

%left OR
%left AND 
%left UNIT
%right PERCENT
%nonassoc EQ EQASS GT GE LT LE NE COLON 
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%nonassoc UNARY
%nonassoc NOT
%right POWER

%{
int MSUvDistGramlex (YYSTYPE*);
%}

%%
uvdiststatement: SQUOTE uvdistexpr SQUOTE {
                   $$ = $2 ;
                 }
               ;

uvdistexpr: rangeexprlist
          | upuvbound 
          | lowuvbound 
          | uvdistwithfract {
              $$ = $1;}
          ;

rangeexprlist: rangeexpr {
                 $$ = $1;}
             | rangeexprlist COMMA rangeexpr {
       	         $$ = $3;
	       }
             ;

rangeexpr: FNUMBER DASH FNUMBER UNIT {
             $$ = MSUvDistParse().selectUVRange($1, $3, String($4));
           }
         ;

upuvbound: LT FNUMBER UNIT {
             $$ = MSUvDistParse().selectUVRange(0, $2, String($3));
           }
         ;
        
lowuvbound: GT FNUMBER UNIT {
              $$ = MSUvDistParse().selectUVRange($2, 1000000, String($3));
            }
          ;

uvdistwithfract: FNUMBER UNIT COLON FNUMBER PERCENT {
                   $$ = MSUvDistParse().selectUVRange($1-$4*0.01, $1+$4*0.01, String($2));
                 }
               ;
%%

