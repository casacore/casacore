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
  char* str;
  Double dval;
}


%token EQASS
%token SQUOTE
%token <str> IDENTIFIER
%token <dval> FNUMBER
%token COMMA

%token LBRACKET
%token LPAREN
%token RBRACKET
%token RPAREN
%token LBRACE
%token RBRACE

%token DASH
%token STAR
%token COLON

%type <node> antennastatement
%type <node> antennaexpr
%type <node> combnameorstation
%type <node> namesorstations
%type <node> indexcombexpr
%type <node> idandcp

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
antennastatement: indexcombexpr 
                | SQUOTE antennaexpr SQUOTE {
                    $$ = $2;
                  }
                ;

antennaexpr: combnameorstation 
           | combnameorstation AMPERSAND combnameorstation {
                  $$ = new TableExprNode ($1 || $3) ;}
           | combnameorstation AMPERSAND STAR {
                  $$ = $1;}
           ;

combnameorstation: namesorstations 
                 | LPAREN namesorstations RPAREN {
		     $$ = $2;}
                 ;
namesorstations: IDENTIFIER {
               	   $$ = MSAntennaParse().selectNameOrStation(String($1));
                 }
               | namesorstations COMMA IDENTIFIER {
		   $$ = MSAntennaParse().selectNameOrStation(String($3));
	         }
               | IDENTIFIER DASH IDENTIFIER {
                   if(isdigit(atoi($1))) {
		     Int len = atoi($3) - atoi($1) + 1;
		     String antennanms;
		     for(Int i = 0; i < len; i++) {
		       antennanms = String(atoi($1) + i);
		       $$ = MSAntennaParse().selectNameOrStation(antennanms);
		     }
		   }else {
		     cout << " Incorrect range expresssion " << endl;
		   }
		 }
               ;
		  
indexcombexpr  : IDENTIFIER {
                   Vector<Int> ind(1);
                   ind[0] = atoi($1);
                   $$ = MSAntennaParse().selectAntennaIds(ind);
                 }

               | idandcp
	       | idandcp AMPERSAND idandcp {
                   $$ = new TableExprNode ($1 || $3); 
   	         }
	       ;
idandcp: IDENTIFIER COLON IDENTIFIER {
           $$ = MSAntennaParse().selectFromIdsAndCPs(atoi($1), String($3));
	   }
       ;

%%

