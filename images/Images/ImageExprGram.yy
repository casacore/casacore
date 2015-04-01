/*
    ImageExprGram.y: Parser for image expressions
    Copyright (C) 1998,1999,2003
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
LatticeExprNode* node;
ImageExprParse*  val;
Block<LatticeExprNode>* scalarvector;
vector<Slice>*   slicelist;
Slice*           slice;
}

%token <val> NAME           /* name of constant, function, or lattice */
%token <val> LATNAME        /* lattice name */
%token <val> TMPREG         /* temporary region name */
%token <val> LITERAL
%token <val> INDEXN
%token INDEXIN
%token INDEXNOTIN
%token IN
%token NOT
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
%type <scalarvector> scalarlist;
%type <slicelist> rangelist;
%type <slice> frange;


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
         | INDEXIN LPAREN orexpr COMMA LBRACKET rangelist RBRACKET RPAREN {
               $$ = new LatticeExprNode (ImageExprParse::makeIndexinNode (*$3, *$6));
	       ImageExprParse::addNode ($$);
               delete $6;
	   }
         | INDEXNOTIN LPAREN orexpr COMMA LBRACKET rangelist RBRACKET RPAREN {
               LatticeExprNode node (ImageExprParse::makeIndexinNode (*$3, *$6));
	       $$ = new LatticeExprNode (!node);
	       ImageExprParse::addNode ($$);
               delete $6;
	   }
         | INDEXN IN LBRACKET rangelist RBRACKET {
	       LatticeExprNode axis($1->makeLiteralNode());
               $$ = new LatticeExprNode (ImageExprParse::makeIndexinNode (axis, *$4));
	       ImageExprParse::addNode ($$);
               delete $4;
	   }
         | INDEXN NOT IN LBRACKET rangelist RBRACKET {
	       LatticeExprNode axis($1->makeLiteralNode());
               LatticeExprNode node(ImageExprParse::makeIndexinNode (axis, *$5));
	       $$ = new LatticeExprNode (!node);
	       ImageExprParse::addNode ($$);
               delete $5;
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
         | LBRACKET scalarlist RBRACKET {
	       $$ = new LatticeExprNode (ImageExprParse::makeValueList(*$2));
	       delete $2;
	       ImageExprParse::addNode ($$);
	   }
         ;

scalarlist: scalarlist COMMA orexpr {
               $$ = $1;
	       uInt nr = $$->nelements();
	       $$->resize (nr+1);
	       (*$$)[nr] = *$3;
	    }
         | orexpr {
               $$ = new Block<LatticeExprNode>(1, *$1);
	   }
         ;

rangelist: rangelist COMMA frange {
               $$ = $1;
	       $$->push_back (*$3);
	       delete $3;
	   }
         | frange {
	       $$ = new vector<Slice>;
	       $$->push_back (*$1);
	       delete $1;
	   }
         ;

frange:    LITERAL {
               $$ = ImageExprParse::makeSlice (*$1);
           }
         | LITERAL COLON LITERAL {
               $$ = ImageExprParse::makeSlice (*$1, *$3);
           }
         | LITERAL COLON LITERAL COLON LITERAL {
               $$ = ImageExprParse::makeSlice (*$1, *$3, *$5);
           }
         ;

%%
