/*
    MSScanGram.y: Parser for scan expressions
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
#include <errno.h>
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
%token WHITE

%token <str> INT
%token <str> QSTRING
%token <str> REGEX

%token COLON
%token SEMICOLON

%type <node> scanstatement
%type <node> indexcombexpr
%type <node> scanidrange
%type <node> scanidbounds

%nonassoc EQ EQASS GT GE LT LE NE COMMA DASH AMPERSAND

%{
  int MSScanGramlex (YYSTYPE*);
%}

%%
scanstatement: indexcombexpr 
                  {
                    $$ = $1;
                  }
                 | LPAREN indexcombexpr RPAREN //Parenthesis are syntactically 
		                               // not useful here
                  {
		    $$ = $2;
		  }
                ;
indexcombexpr  : scanidrange                       {$$=$1;}
                | scanidbounds                     {$$=$1;}
                | scanidrange COMMA indexcombexpr  {$$=$1;}
                | scanidbounds COMMA indexcombexpr {$$=$1;}
	       ;


scanidbounds: LT INT // <ID
                {
		  const Vector<Int> idv(1,atoi($2));
		  $$ = MSScanParse().selectScanIdsLT(idv);
		  free($2);
		}
              | GT INT // >ID
                {
		  const Vector<Int> idv(1,atoi($2));
		  $$ = MSScanParse().selectScanIdsGT(idv);
		  free($2);
		}
              | LE INT // <=ID
                {
		  const Vector<Int> idv(1,atoi($2));
		  $$ = MSScanParse().selectScanIdsLTEQ(idv);
		  free($2);
		}
              | GE INT // >=ID
                {
		  const Vector<Int> idv(1,atoi($2));
		  $$ = MSScanParse().selectScanIdsGTEQ(idv);
		  free($2);
		}
              | GE INT AMPERSAND LE INT // >=ID & <=ID
                {
		  Int n0=atoi($2), n1=atoi($5);
		  $$ = MSScanParse().selectRangeGEAndLE(n0,n1);

		  free($2); free($5);
		}
              | GT INT AMPERSAND LT INT // >ID & <ID
                {
		  Int n0=atoi($2), n1=atoi($5);
		  $$ = MSScanParse().selectRangeGTAndLT(n0,n1);

		  free($2); free($5);
		}
             ;

scanidrange: INT // A single scan index
            {
	      const Vector<Int> idv(1,atoi($1));
	      $$ = MSScanParse().selectScanIds(idv);
	      free($1);
	    }
           | INT DASH INT // A range of integer scan indices
            {
              Int start = atoi($1);
              Int end   = atoi($3);
              Int len = end - start + 1;
              Vector<Int> scanids(len);
              for(Int i = 0; i < len; i++) {
                scanids[i] = start + i;
              }
	      $$ = MSScanParse().selectScanIds(scanids);
	      free($1); free($3);
            }
          ;
%%

