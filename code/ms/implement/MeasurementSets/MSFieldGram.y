
/*
    MSFieldGram.y: Parser for Field expressions
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
#define YYDEBUG 1
#include <stdio.h>
%}

%pure_parser                /* make parser re-entrant */

%union {
  TableExprNode *node;
  Block<TableExprNode>* exprb;
  TableExprNodeSetElem* elem;
  TableExprNodeSet* settp;
  Int ival;
  char * str;
  Double dval[2];
}

%token <str> NAME
%token CODE
%token FIELD
%token EQASS
%token <ival> INDEX
%token <dval> FNUMBER
%token SQUOTE

%token LBRACKET
%token LPAREN
%token RBRACKET
%token RPAREN
%token LBRACE
%token RBRACE
%token COLON
%token COMMA
%token DASH

%type <node> fieldstatement
%type <node> fieldexpr
%type <node> namelist
%type <node> indexrangeexpr
%type <node> lowindexboundexpr
%type <node> upindexboundexpr

%left OR
%left AND
%nonassoc EQ EQASS GT GE LT LE NE
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%nonassoc UNARY
%nonassoc NOT
%right POWER

%{
int MSFieldGramlex (YYSTYPE*);
%}

%%
fieldstatement: FIELD EQASS SQUOTE fieldexpr SQUOTE {
                  $$ = $4 ;
                  cout << "field selection" << endl;}
               ;
fieldexpr:  namelist
           |CODE {
		 printf("field or source code\n");}
           |indexrangeexpr
           |lowindexboundexpr
           |upindexboundexpr
            ;
namelist : NAME {
		 Vector<String> fieldnames(1);
                 fieldnames[0] = String($1);		
		 $$ = MSFieldParse().selectFieldNames(fieldnames);}
         | namelist COMMA NAME { 
	         printf("For list case, this one match first\n");}
         ;
            
indexrangeexpr : INDEX DASH INDEX {
                   Int len = $<ival>3-$<ival>1+1;
		   Vector<Int> fieldids(len);
                   for(Int i = 0; i < len; i++) {
                     fieldids[i] = $<ival>1 + i;
		     cout << "field ids" << fieldids[i] << endl;
                   }
                   $$ = MSFieldParse().selectFieldIds(fieldids);}
               ;

lowindexboundexpr : GT INDEX {
                   cout << "> index " << $2 << endl;
		   ROMSFieldColumns msFieldCols_p(MSFieldParse::ms().field());
		   Int startID = $2;
		   Int len = msFieldCols_p.nrow();
		   Vector<Int> fieldids(len- startID -1);
		   for(Int i = 0; i < (Int)fieldids.nelements(); i++) {
		     fieldids[i] = startID + i + 1;
		   }
		   $$ = MSFieldParse().selectFieldIds(fieldids);}
               ;

upindexboundexpr : LT INDEX {
		   Int len = $2;
		   Vector<Int> fieldids(len);
                   for(Int i = 0; i < len; i++) {
                     fieldids[i] = i;
		     cout << "field ids" << fieldids[i] << endl;
                   }
                   $$ = MSFieldParse().selectFieldIds(fieldids);}
              ;

%%

