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
  TableExprNode* node;
  Block<TableExprNode>* exprb;
  TableExprNodeSetElem* elem;
  TableExprNodeSet* settp;
  Int ival;
  Double dval[2];
}

%token UVDIST
%token EQASS
%token SQUOTE
%token <ival> NUMBER
%token <dval> FNUMBER
%token DASH
%token DISTANCEUNIT
%token WAVELENTHUNIT
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
%type <node> upuvbound
%type <node> lowuvbound
%type <node> uvdistwithfract
%type <node> rangeexpr

%left OR
%left AND
%nonassoc EQ EQASS GT GE LT LE NE
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%nonassoc UNARY
%nonassoc NOT
%right POWER

%{
int MSUvDistGramlex (YYSTYPE*);
%}

%%
uvdiststatement : UVDIST EQASS SQUOTE uvdistexpr SQUOTE {
                    $$ = $4 ;
                    cout << "Start statement" << endl;}
                ;

uvdistexpr : rangeexprlist
            |upuvbound
            |lowuvbound
            |uvdistwithfract {
              $$ = $1;
              /* delete $1;} */
             }
           ;

rangeexprlist :   rangeexpr
                | rangeexprlist COMMA rangeexpr {
                  $$ = $1;
                  delete $1;}
           ;

rangeexpr : NUMBER DASH NUMBER unit {
              MSSelection * thissel = new MSSelection();
	      Double sUV = $1;
	      Double eUV = $3;
	      cout << "uvdist between " << $1 << " and " << $3 << endl;
              thissel->setUVRange(sUV, eUV);
	      cout << "after set UVrange " << endl;
	      cout << "ms has rows " << msUvDistGramMS().nrow() << endl;
              MSSelection::msTableExprNode = new TableExprNode(						       thissel->toTableExprNode(msUvDistGramMS()));
	      cout << "TableExprNode is constructed " << endl;}
           ;
unit :  DISTANCEUNIT
      | WAVELENTHUNIT
      ;

upuvbound : LT NUMBER unit {
              MSSelection * thissel = new MSSelection();
	      Double sUV = 0;
	      Double eUV = $2;
              thissel->setUVRange(sUV, eUV);
              MSSelection::msTableExprNode = new TableExprNode(						       thissel->toTableExprNode(msUvDistGramMS()));
	      cout << "uvdist < " << $2 << endl;}
           ;
        
lowuvbound : GT NUMBER unit {
              MSSelection * thissel = new MSSelection();
	      Double sUV = $2;
	      Double eUV = 1000000;
              thissel->setUVRange(sUV, eUV);
              MSSelection::msTableExprNode = new TableExprNode(						       thissel->toTableExprNode(msUvDistGramMS()));
	      cout << "uvdist > " << $2 << endl;}
           ;
uvdistwithfract : NUMBER unit COLON NUMBER PERCENT {
              MSSelection * thissel = new MSSelection();
	      Double sUV = $1-$4*0.01;
	      Double eUV = $1+$4*0.01;
              thissel->setUVRange(sUV, eUV);
              MSSelection::msTableExprNode = new TableExprNode(thissel->toTableExprNode(msUvDistGramMS()));
	      cout << "uvdist around " << $1 << endl;
              delete thissel;}
             ;
%%

