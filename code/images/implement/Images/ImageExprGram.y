/*
    ImageExprGram.y: Parser for image expressions
    Copyright (C) 1998,1999
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

%pure_parser                /* make parser re-entrant */

%union {
LatticeExprNode* node;
ImageExprParse* val;
}

%token <val> NAME           /* name of constant, function, or lattice */
%token <val> LATNAME        /* lattice name */
%token <val> LITERAL
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token COMMA
%type <node> orexpr
%type <node> andexpr
%type <node> relexpr
%type <node> arithexpr
%type <node> unaryexpr
%type <node> simexpr


%left OR
%left AND
%nonassoc EQ GT GE LT LE NE
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%right POWER
%right NOT

%{
int ImageExprGramlex (YYSTYPE*);
%}

%%
command:   orexpr {
               ImageExprParse::setNode (*$1);
	       delete $1;
           }
         ;

orexpr:    andexpr
               { $$ = $1; }
         | orexpr OR andexpr {
	       $$ = new LatticeExprNode (*$1 || *$3);
	       delete $1;
	       delete $3;
	   }
         ;

andexpr:   relexpr
               { $$ = $1; }
         | andexpr AND relexpr {
	       $$ = new LatticeExprNode (*$1 && *$3);
	       delete $1;
	       delete $3;
	   }
         ;

relexpr:   arithexpr
               { $$ = $1; }
         | arithexpr EQ arithexpr {
	       $$ = new LatticeExprNode (*$1 == *$3);
	       delete $1;
	       delete $3;
	   }
         | arithexpr GT arithexpr {
	       $$ = new LatticeExprNode (*$1 > *$3);
	       delete $1;
	       delete $3;
	   }
         | arithexpr GE arithexpr {
	       $$ = new LatticeExprNode (*$1 >= *$3);
	       delete $1;
	       delete $3;
	   }
         | arithexpr LT arithexpr {
	       $$ = new LatticeExprNode (*$1 < *$3);
	       delete $1;
	       delete $3;
	   }
         | arithexpr LE arithexpr {
	       $$ = new LatticeExprNode (*$1 <= *$3);
	       delete $1;
	       delete $3;
	   }
         | arithexpr NE arithexpr {
	       $$ = new LatticeExprNode (*$1 != *$3);
	       delete $1;
	       delete $3;
	   }
         ;

arithexpr: unaryexpr
               { $$ = $1; }
         | arithexpr PLUS  arithexpr {
	       $$ = new LatticeExprNode (*$1 + *$3);
	       delete $1;
	       delete $3;
	   }
         | arithexpr MINUS arithexpr {
	       $$ = new LatticeExprNode (*$1 - *$3);
	       delete $1;
	       delete $3;
	   }
         | arithexpr TIMES  arithexpr {
	       $$ = new LatticeExprNode (*$1 * *$3);
	       delete $1;
	       delete $3;
	   }
         | arithexpr DIVIDE arithexpr {
	       $$ = new LatticeExprNode (*$1 / *$3);
	       delete $1;
	       delete $3;
	   }
         | arithexpr MODULO arithexpr {
	       $$ = new LatticeExprNode (fmod (*$1, *$3));
	       delete $1;
	       delete $3;
	   }
         | arithexpr POWER  arithexpr {
	       $$ = new LatticeExprNode (pow (*$1, *$3));
	       delete $1;
	       delete $3;
	   }
         ;

unaryexpr: simexpr
               { $$ = $1; }
         | MINUS unaryexpr {
	       $$ = new LatticeExprNode (-*$2);
	       delete $2;
	   }
         | PLUS  unaryexpr
               { $$ = $2; }
         | NOT   unaryexpr {
	       $$ = new LatticeExprNode (!*$2);
	       delete $2;
	   }
         ;

simexpr:   LPAREN orexpr RPAREN
               { $$ = $2; }
         | simexpr LBRACKET orexpr RBRACKET {
               $$ = new LatticeExprNode ((*$1)[*$3]);
	       delete $1;
	       delete $3;
	   }
         | NAME LPAREN RPAREN {
               $$ = new LatticeExprNode ($1->makeFuncNode());
	       delete $1;
	   }
         | NAME LPAREN orexpr RPAREN {
               $$ = new LatticeExprNode ($1->makeFuncNode (*$3));
	       delete $1;
	       delete $3;
	   }
         | NAME LPAREN orexpr COMMA orexpr RPAREN {
               $$ = new LatticeExprNode ($1->makeFuncNode (*$3, *$5));
	       delete $1;
	       delete $3;
	       delete $5;
	   }
         | NAME LPAREN orexpr COMMA orexpr COMMA orexpr RPAREN {
               $$ = new LatticeExprNode ($1->makeFuncNode (*$3, *$5, *$7));
	       delete $1;
	       delete $3;
	       delete $5;
	       delete $7;
	   }
         | LATNAME {
	       $$ = new LatticeExprNode ($1->makeLatticeNode());
	       delete $1;
	   }
         | NAME {
	       $$ = new LatticeExprNode ($1->makeLitLattNode());
	       delete $1;
	   }
         | LITERAL {
	       $$ = new LatticeExprNode ($1->makeLiteralNode());
	       delete $1;
	   }
         ;

%%
