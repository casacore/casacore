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
%token <val> TMPREG         /* temporary region name */
%token <val> LITERAL
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token COMMA
%token COLON
%type <node> orexpr
%type <node> andexpr
%type <node> relexpr
%type <node> arithexpr
%type <node> simexpr


%left OR
%left AND
%nonassoc EQ GT GE LT LE NE
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%nonassoc UNARY
%nonassoc NOT
%right POWER

%{
int ImageExprGramlex (YYSTYPE*);
%}

%%
command:   orexpr {
               ImageExprParse::setNode (*$1);
           }
         ;

orexpr:    andexpr
               { $$ = $1; }
         | orexpr OR andexpr {
	       $$ = new LatticeExprNode (*$1 || *$3);
	       ImageExprParse::addNode ($$);
	   }
         ;

andexpr:   relexpr
               { $$ = $1; }
         | andexpr AND relexpr {
	       $$ = new LatticeExprNode (*$1 && *$3);
	       ImageExprParse::addNode ($$);
	   }
         ;

relexpr:   arithexpr
               { $$ = $1; }
         | arithexpr EQ arithexpr {
	       $$ = new LatticeExprNode (*$1 == *$3);
	       ImageExprParse::addNode ($$);
	   }
         | arithexpr GT arithexpr {
	       $$ = new LatticeExprNode (*$1 > *$3);
	       ImageExprParse::addNode ($$);
	   }
         | arithexpr GE arithexpr {
	       $$ = new LatticeExprNode (*$1 >= *$3);
	       ImageExprParse::addNode ($$);
	   }
         | arithexpr LT arithexpr {
	       $$ = new LatticeExprNode (*$1 < *$3);
	       ImageExprParse::addNode ($$);
	   }
         | arithexpr LE arithexpr {
	       $$ = new LatticeExprNode (*$1 <= *$3);
	       ImageExprParse::addNode ($$);
	   }
         | arithexpr NE arithexpr {
	       $$ = new LatticeExprNode (*$1 != *$3);
	       ImageExprParse::addNode ($$);
	   }
         ;

arithexpr: simexpr
               { $$ = $1; }
         | arithexpr PLUS  arithexpr {
	       $$ = new LatticeExprNode (*$1 + *$3);
	       ImageExprParse::addNode ($$);
	   }
         | arithexpr MINUS arithexpr {
	       $$ = new LatticeExprNode (*$1 - *$3);
	       ImageExprParse::addNode ($$);
	   }
         | arithexpr TIMES  arithexpr {
	       $$ = new LatticeExprNode (*$1 * *$3);
	       ImageExprParse::addNode ($$);
	   }
         | arithexpr DIVIDE arithexpr {
	       $$ = new LatticeExprNode (*$1 / *$3);
	       ImageExprParse::addNode ($$);
	   }
         | arithexpr MODULO arithexpr {
	       $$ = new LatticeExprNode (fmod (*$1, *$3));
	       ImageExprParse::addNode ($$);
	   }
         | MINUS arithexpr %prec UNARY {
	       $$ = new LatticeExprNode (-*$2);
	       ImageExprParse::addNode ($$);
	   }
         | PLUS  arithexpr %prec UNARY
               { $$ = $2; }
         | NOT   arithexpr {
	       $$ = new LatticeExprNode (!*$2);
	       ImageExprParse::addNode ($$);
	   }
         | arithexpr POWER arithexpr {
	       $$ = new LatticeExprNode (pow (*$1, *$3));
	       ImageExprParse::addNode ($$);
	   }
         ;

simexpr:   LPAREN orexpr RPAREN
               { $$ = $2; }
         | simexpr LBRACKET orexpr RBRACKET {
               $$ = new LatticeExprNode ((*$1)[*$3]);
	       ImageExprParse::addNode ($$);
	   }
         | NAME LPAREN RPAREN {
               $$ = new LatticeExprNode ($1->makeFuncNode());
	       ImageExprParse::addNode ($$);
	   }
         | NAME LPAREN orexpr RPAREN {
               $$ = new LatticeExprNode ($1->makeFuncNode (*$3));
	       ImageExprParse::addNode ($$);
	   }
         | NAME LPAREN orexpr COMMA orexpr RPAREN {
               $$ = new LatticeExprNode ($1->makeFuncNode (*$3, *$5));
	       ImageExprParse::addNode ($$);
	   }
         | NAME LPAREN orexpr COMMA orexpr COMMA orexpr RPAREN {
               $$ = new LatticeExprNode ($1->makeFuncNode (*$3, *$5, *$7));
	       ImageExprParse::addNode ($$);
	   }
         | LATNAME {
	       $$ = new LatticeExprNode ($1->makeLRNode());
	       ImageExprParse::addNode ($$);
	   }
         | NAME {
	       $$ = new LatticeExprNode ($1->makeLitLRNode());
	       ImageExprParse::addNode ($$);
	   }
         | TMPREG {
	       $$ = new LatticeExprNode ($1->makeRegionNode());
	       ImageExprParse::addNode ($$);
	   }
         | LITERAL {
	       $$ = new LatticeExprNode ($1->makeLiteralNode());
	       ImageExprParse::addNode ($$);
	   }
         ;

%%
