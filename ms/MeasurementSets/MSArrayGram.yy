/* -*- C++ -*-
    MSArrayGram.y: Parser for scan expressions
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
  using namespace casacore;
%}

%pure-parser                /* make parser re-entrant */

%union {
  const TableExprNode* node;
  Block<TableExprNode>* exprb;
  TableExprNodeSetElem* elem;
  TableExprNodeSet* settp;
  Int ival[2];
  char * str;
  Double dval;
  std::vector<Int>* iv; // std::vectors have push_back, insert, etc.
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
%type <node> compoundexpr
%type <node> scanboundsexpr
%type <node> scanidbounds
%type <iv> scanids

%nonassoc EQ EQASS GT GE LT LE NE COMMA DASH AMPERSAND

%{
  int MSArrayGramlex (YYSTYPE*);
%}

%%
scanstatement: compoundexpr                {$$ = MSArrayParse::thisMSAParser->selectArrayIds();}
             ;
// Here, for ID-list expressions (INT and INT DASH INT), we only
// collect the list of IDs generated (accumulated internally in
// MSArrayPrase).  The accumulated IDs are used for selection in the
// terminal node above.  Bounds expressions are however used for
// selection as they are parsed.
compoundexpr: scanids                           {/*$$ = MSArrayParse::thisMSAParser->node();*/}
            | scanboundsexpr                    {$$=$1;}
            | compoundexpr COMMA scanids        {$$=$1;}
            | compoundexpr COMMA scanboundsexpr {$$=$1;}
            ;

scanidbounds: LT INT // <ID
                {
		  const Vector<Int> idv(1,atoi($2));
		  $$ = MSArrayParse::thisMSAParser->selectArrayIdsLT(idv);
		  free($2);
		}
            | GT INT // >ID
                {
		  const Vector<Int> idv(1,atoi($2));
		  $$ = MSArrayParse::thisMSAParser->selectArrayIdsGT(idv);
		  free($2);
		}
            | LE INT // <=ID
                {
		  const Vector<Int> idv(1,atoi($2));
		  $$ = MSArrayParse::thisMSAParser->selectArrayIdsLTEQ(idv);
		  free($2);
		}
            | GE INT // >=ID
                {
		  const Vector<Int> idv(1,atoi($2));
		  $$ = MSArrayParse::thisMSAParser->selectArrayIdsGTEQ(idv);
		  free($2);
		}
            | GE INT AMPERSAND LE INT // >=ID & <=ID
                {
		  Int n0=atoi($2), n1=atoi($5);
		  $$ = MSArrayParse::thisMSAParser->selectRangeGEAndLE(n0,n1);

		  free($2); free($5);
		}
            | GT INT AMPERSAND LT INT // >ID & <ID
                {
		  Int n0=atoi($2), n1=atoi($5);
		  $$ = MSArrayParse::thisMSAParser->selectRangeGTAndLT(n0,n1);

		  free($2); free($5);
		}
            ;
scanboundsexpr: scanidbounds {$$=$1;}
//
// Build a list of scan IDs.  This can be a single ID or a range of
// IDs converted to a list.  Actual selection is done at the end of
// parsing cycle (at the terminal node above).
//
scanids: INT
           {
	     $$=&MSArrayParse::thisMSAParser->accumulateIDs(atoi($1));
	     free($1);
	   }
       | INT DASH INT
           {
	     $$=&MSArrayParse::thisMSAParser->accumulateIDs(atoi($1),atoi($3));
	     free($1); free($3);
	   }
        ;
%%

