/*
    MSCorrGram.y: Parser for corr expressions
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
using namespace casacore;
%}

%pure-parser                /* make parser re-entrant */

%union {
  const TableExprNode* node;
  Block<TableExprNode>* exprb;
  TableExprNodeSetElem* elem;
  TableExprNodeSet* settp;
  Int ival;
  char * str;
  Double dval[2];
}

%token EQASS
%token SQUOTE
%token <ival> NUMBER
%token <str>  CORRTYPE
%token <dval> FNUMBER
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

%type <node> corrstatement
%type <node> stdstokes

%left OR
%left AND
%nonassoc EQ EQASS GT GE LT LE NE
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%nonassoc UNARY
%nonassoc NOT
%right POWER

%{
int MSCorrGramlex (YYSTYPE*);
%}

%%
corrstatement: SQUOTE stdstokes SQUOTE {
                 $$ = $2;
             }
             ;

stdstokes: CORRTYPE {
                String identifier = String($1);
                $$ = MSCorrParse().selectCorrType(identifier);
              }
          | stdstokes COMMA CORRTYPE {
                String identifier = String($3);		
                $$ = MSCorrParse().selectCorrType(identifier);
              }
         ;
%%

