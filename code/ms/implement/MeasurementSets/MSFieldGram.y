
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
  const TableExprNode *node;
  Block<TableExprNode>* exprb;
  TableExprNodeSetElem* elem;
  TableExprNodeSet* settp;
  Int ival;
  char * str;
  Double dval[2];
}

%token <str> NAMEORCODE
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
%type <node> fieldorsourceexpr
%type <node> indexexpr
%type <node> compoundexpr
%type <node> singlerange
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
fieldstatement: indexexpr {
                  $$ = $1 ;
                }
              | SQUOTE fieldorsourceexpr SQUOTE {
                  $$ = $2 ;
                }
              ;

fieldorsourceexpr: NAMEORCODE {
                     String fieldname = String($1);		
                     $$ = MSFieldParse().selectFieldOrSource(fieldname);
                   }
                 | fieldorsourceexpr COMMA NAMEORCODE {
                     String fieldname = String($3);		
                     $$ = MSFieldParse().selectFieldOrSource(fieldname);
                   }
                 ;
            
indexexpr: compoundexpr
         | lowindexboundexpr
         | upindexboundexpr
         ;

compoundexpr: INDEX {
                Vector<Int> fieldids(1);
                fieldids[0] = $1;
                $$ = MSFieldParse().selectFieldIds(fieldids);
              }
            | singlerange {
                $$ = $1;}
            | compoundexpr COMMA INDEX {
                $$ = new TableExprNode ($1 || $3);} 
            | compoundexpr COMMA singlerange {
		  $$ = new TableExprNode ($1 || $3);}
            ;
 
singlerange:  INDEX DASH INDEX {
                  Int len = $<ival>3-$<ival>1+1;
                  Vector<Int> fieldids(len);
                  for(Int i = 0; i < len; i++) {
                    fieldids[i] = $<ival>1 + i;
                  }
                  $$ = MSFieldParse().selectFieldIds(fieldids);
              }
           ;

lowindexboundexpr: GT INDEX {
		     ROMSFieldColumns msFieldCols_p(MSFieldParse::ms()->field());
		     Int startID = $2;
		     Int len = msFieldCols_p.nrow();
		     if(len- startID -1 <= 0) {
		       cout << "Your selection is out of range " << endl;
		       exit(0);
		     } else {
		       Vector<Int> fieldids(len- startID -1);
		       for(Int i = 0; i < (Int)fieldids.nelements(); i++) {
			 fieldids[i] = startID + i + 1;
		       }
		       $$ = MSFieldParse().selectFieldIds(fieldids);
		     }
                   }
                 ;

upindexboundexpr: LT INDEX {
		    Int len = $2;
		    Vector<Int> fieldids(len);
                    for(Int i = 0; i < len; i++) {
                      fieldids[i] = i;
                    }
                    $$ = MSFieldParse().selectFieldIds(fieldids);
                  }
                ;

%%

