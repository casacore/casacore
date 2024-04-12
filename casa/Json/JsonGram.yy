/*
//# JsonGram.yy: Parser for Json-style key:value lines
//# Copyright (C) 2016
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
*/

%{
using namespace casacore;
%}

%pure-parser                /* make parser re-entrant */

%union {
  JsonValue* val;
  JsonKVMap* block;
  std::vector<JsonValue>* vec;
}

%{
int JsonGramlex (YYSTYPE*);
%}


%token <val> LITERAL
%token <val> STRING
%token COLON
%token COMMA
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token TOKENERROR
%type <block> keyvals
%type <val> value
%type <val> valuesl
%type <vec> values

%% /* Grammar rules and actions follow */

json:      LBRACE keyvals RBRACE {
               JsonParser::setMap ($2);
           }
       ;

keyvals:   keyvals COMMA STRING COLON value {
               $$ = $1;
	       (*$$)[$3->getString()] = *$5;
	       delete $3;
	       delete $5;
	   }
       |   STRING COLON value {
               $$ = new JsonKVMap;
	       (*$$)[$1->getString()] = *$3;
	       delete $1;
	       delete $3;
	   }
       |   {
	       /* empty 'record' */
	       $$ = new JsonKVMap;
           }
       ;

value:     valuesl
	   {   $$ = $1; }
       |   LBRACKET values RBRACKET {
	       $$ = new JsonValue (*$2);
	       delete $2;
           }
       |   LBRACE keyvals RBRACE {
	       $$ = new JsonValue (*$2);
	       delete $2;
           }
       ;

values:    values COMMA value {
               $$ = $1;
	       $$->push_back (*$3);
	       delete $3;
	   }
       |   value {
               $$ = new std::vector<JsonValue>();
	       $$->push_back (*$1);
	       delete $1;
	   }
       ;

valuesl:   LITERAL
	   {   $$ = $1; }
       |   STRING
	   {   $$ = $1; }
  

%%
