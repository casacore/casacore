/*
    MSAntennaGram.y: Parser for antenna expressions
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
  Double dval;
  char* str;
}

%token <str> IDENTIFIER
%token <ival> INDEX
%token <dval> FNUMBER
%token DASH
%token STAR

%token EQASS
%token SQUOTE

%token LBRACKET
%token LPAREN
%token RBRACKET
%token RPAREN
%token LBRACE
%token RBRACE
%token COMMA
%token COLON

%type <node> antennastatement
%type <node> antennaexpr
%type <node> indexcombexprlist
%type <node> indexcombexpr
%type <node> indexlist
%type <node> antennalistexpr

%left OR AND
%nonassoc EQ EQASS GT GE LT LE NE COMMA AMPERSAND
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%nonassoc UNARY
%nonassoc NOT
%right POWER

%{
int MSAntennaGramlex (YYSTYPE*);
%}

%%
antennastatement: SQUOTE antennaexpr SQUOTE {
                    $$ = $2;
                  }
                | antennalistexpr {
                    $$ = $1;
                  }
                ;

antennaexpr: IDENTIFIER {
                  Vector<String> name(1);
                  name[0] = String($1);
		  $$ = MSAntennaParse().selectAntennaName(name);
             }
	   | indexcombexprlist 
	   ;
		   
indexcombexprlist: indexcombexpr
                 | indexcombexprlist COMMA indexcombexpr 
                 ;

indexcombexpr: indexlist AMPERSAND indexlist
             | indexlist AMPERSAND STAR
             ;

indexlist: LPAREN antennalistexpr RPAREN {
             $$ = $2;
           }
         | antennalistexpr {
             $$ = $1;
           }
         ;
antennalistexpr: IDENTIFIER {
                   Vector<Int> ind(1);
                   ind[0] = atoi($1);
                   $$ = MSAntennaParse().selectAntennaIds(ind);
                 }
               | antennalistexpr COMMA IDENTIFIER
               | IDENTIFIER DASH IDENTIFIER {
                   Int len = atoi($3)-atoi($1)+1;
                   Vector<Int> antennaidx(len);
                   for(Int i = 0; i < len; i++) {
                     antennaidx[i] = atoi($1) + i;
                   }
                   $$ = MSAntennaParse().selectAntennaIds(antennaidx);
		 }
               ;
%%

