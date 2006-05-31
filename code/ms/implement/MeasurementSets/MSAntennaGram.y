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
  Int ival[2];
  char * str;
  Double dval;
  Vector<Int>* iv;
  Vector<String>* is;
}


%token EQASS
%token SQUOTE
%token <str> IDENTIFIER
%token COMMA

%token LBRACKET
%token LPAREN
%token RBRACKET
%token RPAREN
%token LBRACE
%token RBRACE

%token COLON
%token SEMICOLON

%type <node> antennastatement
%type <node> antennaexpr
%type <node> subantennaexpr
%type <node> namesorstations
%type <node> indexcombexpr
%type <is> namelist
%type <iv> indexlist

%left OR AND
%nonassoc EQ EQASS GT GE LT LE NE COMMA DASH AMPERSAND
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

antennaexpr: subantennaexpr
           | antennaexpr SEMICOLON subantennaexpr {
               $$ = $3;
             }
           ;

subantennaexpr: namesorstations 
              | LPAREN namesorstations RPAREN {
		  $$ = $2;
                }
              ;

namesorstations: IDENTIFIER DASH IDENTIFIER {
                   $$ = MSAntennaParse().selectNameOrStation(String($1), String($3));
	         }
               | namelist AMPERSAND namelist {
                 $$ = MSAntennaParse().selectNameOrStation(*($1),*($3));
	         }
               | namelist AMPERSAND {
                 $$ = MSAntennaParse().selectNameOrStation(*($1),*($1));
	         }
               | namelist {
                   $$ = MSAntennaParse().selectNameOrStation(*($1));
                 }
               ;

namelist : IDENTIFIER {
             Vector<String> antennanms(1);
             antennanms[0] = String($1);
             $$ = new Vector<String>(antennanms);
           }
         | namelist COMMA IDENTIFIER {
             Vector<String> antennanms(*($1));
             antennanms = String($3);
             $$ = new Vector<String>(antennanms);
           }
         ;

indexcombexpr  : IDENTIFIER COLON IDENTIFIER {
                   $$ = MSAntennaParse().selectFromIdsAndCPs(atoi($1), String($3));
	         }
	       | indexlist AMPERSAND indexlist {
                   $$ = MSAntennaParse().selectAntennaIds(*($1), *($3));
                 } 
               | indexlist AMPERSAND {
                   $$ = MSAntennaParse().selectAntennaIds(*($1), *($1));
   	         }
               | indexlist {
                   $$ = MSAntennaParse().selectAntennaIds(*($1));
                 }
	       ;

indexlist : IDENTIFIER {
              Vector<Int> antennaids(1);
              antennaids[0] = atoi($1);
              $$ = new Vector<Int>(antennaids);
            }
          | indexlist COMMA IDENTIFIER {
              Vector<Int> antennaids(*($1));
              antennaids = atoi($3);
              $$ = new Vector<Int>(antennaids);
            }
          | IDENTIFIER DASH IDENTIFIER {
              Int start = atoi($1);
              Int end   = atoi($3);
              Int len = end - start + 1;
              Vector<Int> antennaids(len);
              for(Int i = 0; i < len; i++) {
                antennaids[i] = start + i;
              }
              $$ = new Vector<Int>(antennaids);	   
            }
	  ;

%%

