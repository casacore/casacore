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
using namespace casacore;
%}

%pure-parser                /* make parser re-entrant */

%union {
  const TableExprNode* node;
  char * str;
  Double dval;
}

%token <str> UNIT
%token <dval> FNUMBER
%token COLON
%token COMMA
%token PERCENT

%type <dval> fnumwithunits
%type <node> uvwdiststatement
%type <node> uvwdistexprlist
%type <node> uvwdistexpr
%left UNIT
%right PERCENT
%nonassoc EQ EQASS GT GE LT LE NE DASH COLON 

%{
  #include <limits.h>
  int MSUvDistGramlex (YYSTYPE*);
  String MSUvDistGramlexGlobalUnits="m"; // Its a global - make the name crazy to minimize
                                         // namespace conflicts.
#define EPS 1E-10
%}

%%
uvwdiststatement:uvwdistexprlist
                 {
                   $$ = $1;
                 }
               ;

uvwdistexprlist: uvwdistexpr 
                 {
		   $$ = $1;
		 }
               | uvwdistexprlist COMMA uvwdistexpr 
                 {
		   $$ = $3;
		 }
               ;

fnumwithunits:   FNUMBER
                 {
		   MSUvDistGramlexGlobalUnits = "m";
		   $$ = $1; // The default units are meters ("m")
		 }
               | FNUMBER UNIT
                 {
		   MSUvDistGramlexGlobalUnits = String($2);
		   $$ = $1;
		 }
               ;

uvwdistexpr:     fnumwithunits
                 {
		   //		   $$ = MSUvDistParse().selectUVRange($1, $1, MSUvDistGramlexGlobalUnits);
		   $$ = MSUvDistParse::thisMSUParser->selectUVRange($1, $1, MSUvDistGramlexGlobalUnits);
		 }
               | FNUMBER DASH fnumwithunits
                 {
		   $$ = MSUvDistParse::thisMSUParser->selectUVRange($1, $3, MSUvDistGramlexGlobalUnits);
		 }
               | LT fnumwithunits
                 {
		   //		   $$ = MSUvDistParse().selectUVRange(0, $2, MSUvDistGramlexGlobalUnits);
		   $$ = MSUvDistParse::thisMSUParser->selectUVRange(0, $2, MSUvDistGramlexGlobalUnits);
		 }
               | GT fnumwithunits
                 {
/* 		   $$ = MSUvDistParse().selectUVRange($2+EPS, std::numeric_limits<Float>::max(),  */
/* 						      MSUvDistGramlexGlobalUnits); */
		   $$ = MSUvDistParse::thisMSUParser->selectUVRange($2+EPS, std::numeric_limits<Float>::max(), 
								    MSUvDistGramlexGlobalUnits);
		 }
               | fnumwithunits COLON FNUMBER PERCENT 
                 {
/*                    $$ = MSUvDistParse().selectUVRange($1*(1-$3*0.01), $1*(1+$3*0.01),  */
/* 						      MSUvDistGramlexGlobalUnits); */
                   $$ = MSUvDistParse::thisMSUParser->selectUVRange($1*(1-$3*0.01), $1*(1+$3*0.01), 
								    MSUvDistGramlexGlobalUnits);
                 }
               ;

%%
