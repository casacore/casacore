/*
    RecordGram.y: Parser for table commands
    Copyright (C) 2000
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
TableExprNode* node;
TableParseVal* val;
Block<TableExprNode>* exprb;
TableExprNodeSetElem* elem;
TableExprNodeSet* settp;
}

%token <val> NAME           /* name of function or shorthand for table */
%token <val> FLDNAME        /* name of field or shorthand for table */
%token <val> LITERAL
%token <val> STRINGLITERAL
%token IN
%token LPAREN
%token RPAREN
%token COMMA
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token COLON
%token OPENOPEN
%token OPENCLOSED
%token CLOSEDOPEN
%token CLOSEDCLOSED
%token OPENEMPTY
%token EMPTYOPEN
%token CLOSEDEMPTY
%token EMPTYCLOSED
%type <node> whexpr
%type <node> orexpr
%type <node> andexpr
%type <node> relexpr
%type <node> arithexpr
%type <node> inxexpr
%type <node> simexpr
%type <node> set
%type <settp> subscripts
%type <settp> elemlist
%type <settp> elems
%type <elem> elem
%type <elem> subsrange
%type <elem> colonrange


%left OR
%left AND
%nonassoc EQ GT GE LT LE NE
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%nonassoc UNARY
%nonassoc NOT
%right POWER


%{
int RecordGramlex (YYSTYPE*);
%}

%%
whexpr:    orexpr {
               RecordGram::setNodePtr ($1);
	       $$ = $1;
	   }
         ;

orexpr:    andexpr
         | orexpr OR andexpr {
	       $$ = new TableExprNode (*$1 || *$3);
	       delete $1;
	       delete $3;
	   }
         ;

andexpr:   relexpr
         | andexpr AND relexpr {
	       $$ = new TableExprNode (*$1 && *$3);
	       delete $1;
	       delete $3;
	   }
         ;

relexpr:   arithexpr
         | arithexpr EQ arithexpr {
	       $$ = new TableExprNode (*$1 == *$3);
	       delete $1;
	       delete $3;
	   }
         | arithexpr GT arithexpr {
	       $$ = new TableExprNode (*$1 > *$3);
	       delete $1;
	       delete $3;
	   }
         | arithexpr GE arithexpr {
	       $$ = new TableExprNode (*$1 >= *$3);
	       delete $1;
	       delete $3;
	   }
         | arithexpr LT arithexpr {
	       $$ = new TableExprNode (*$1 < *$3);
	       delete $1;
	       delete $3;
	   }
         | arithexpr LE arithexpr {
	       $$ = new TableExprNode (*$1 <= *$3);
	       delete $1;
	       delete $3;
	   }
         | arithexpr NE arithexpr {
	       $$ = new TableExprNode (*$1 != *$3);
	       delete $1;
	       delete $3;
	   }
         | arithexpr IN arithexpr {
               $$ = new TableExprNode ($1->in (*$3));
               delete $1;
               delete $3;
           }
         ;

arithexpr: inxexpr
         | arithexpr PLUS  arithexpr {
	       $$ = new TableExprNode (*$1 + *$3);
	       delete $1;
	       delete $3;
	   }
         | arithexpr MINUS arithexpr {
	       $$ = new TableExprNode (*$1 - *$3);
	       delete $1;
	       delete $3;
	   }
         | arithexpr TIMES  arithexpr {
	       $$ = new TableExprNode (*$1 * *$3);
	       delete $1;
	       delete $3;
	   }
         | arithexpr DIVIDE arithexpr {
	       $$ = new TableExprNode (*$1 / *$3);
	       delete $1;
	       delete $3;
	   }
         | arithexpr MODULO arithexpr {
	       $$ = new TableExprNode (*$1 % *$3);
	       delete $1;
	       delete $3;
	   }
         | MINUS arithexpr %prec UNARY {
	       $$ = new TableExprNode (-*$2);
	       delete $2;
	   }
         | PLUS  arithexpr %prec UNARY
               { $$ = $2; }
         | NOT   arithexpr {
	       $$ = new TableExprNode (!*$2);
	       delete $2;
	   }
         | arithexpr POWER arithexpr {
	       $$ = new TableExprNode (pow (*$1, *$3));
	       delete $1;
	       delete $3;
	   }
         ;

inxexpr:   simexpr
         | simexpr LBRACKET subscripts RBRACKET {
	       $$ = new TableExprNode (TableParseSelect::currentSelect()->
                                                 handleSlice (*$1, *$3));
	       delete $1;
	       delete $3;
	   }
         ;

simexpr:   LPAREN orexpr RPAREN
               { $$ = $2; }
         | NAME LPAREN elemlist RPAREN {
               $$ = new TableExprNode (RecordGram::handleFunc ($1->str, *$3));
	       delete $1;
	       delete $3;
	   }
         | NAME {
	       $$ = new TableExprNode (RecordGram::handleField ($1->str));
	       delete $1;
	   }
         | FLDNAME {
	       $$ = new TableExprNode (RecordGram::handleField ($1->str));
	       delete $1;
	   }
         | LITERAL {
	       $$ = new TableExprNode (TableParseSelect::handleLiteral ($1));
	       delete $1;
	   }
         | STRINGLITERAL {
	       $$ = new TableExprNode (TableParseSelect::handleLiteral ($1));
	       delete $1;
	   }
         | set {
	       $$ = $1;
	   }
         ;

set:       LBRACKET elems RBRACKET {
               $$ = new TableExprNode ($2->setOrArray());
               delete $2;
           }
         ;

elemlist:  elems
               { $$ = $1; }
         |
               { $$ = new TableExprNodeSet; }       /* no elements */
         ;

elems:     elems COMMA elem {
               $$ = $1;
	       $$->add (*$3);
	       delete $3;
	   }
         | elem {
	       $$ = new TableExprNodeSet;
	       $$->add (*$1);
	       delete $1;
	   }
         ;

elem:      orexpr {
               $$ = new TableExprNodeSetElem(*$1);
	       delete $1;
	   }
         | colonrange {
               $$ = $1;
           }
         | LT arithexpr COMMA arithexpr GT {
               $$ = new TableExprNodeSetElem (False, *$2, *$4, False);
	       delete $2;
	       delete $4;
           }
         | LT arithexpr COMMA arithexpr RBRACE {
               $$ = new TableExprNodeSetElem (False, *$2, *$4, True);
	       delete $2;
	       delete $4;
           }
         | LBRACE arithexpr COMMA arithexpr GT {
               $$ = new TableExprNodeSetElem (True, *$2, *$4, False);
	       delete $2;
	       delete $4;
           }
         | LBRACE arithexpr COMMA arithexpr RBRACE {
               $$ = new TableExprNodeSetElem (True, *$2, *$4, True);
	       delete $2;
	       delete $4;
           }
         | LBRACE COMMA arithexpr GT {
                $$ = new TableExprNodeSetElem (*$3, False);
	       delete $3;
          }
         | LT COMMA arithexpr GT {
                $$ = new TableExprNodeSetElem (*$3, False);
	       delete $3;
          }
         | LBRACE COMMA arithexpr RBRACE {
                $$ = new TableExprNodeSetElem (*$3, True);
	       delete $3;
           }
         | LT COMMA arithexpr RBRACE {
                $$ = new TableExprNodeSetElem (*$3, True);
	       delete $3;
           }
         | LT arithexpr COMMA RBRACE {
               $$ = new TableExprNodeSetElem (False, *$2);
	       delete $2;
           }
         | LT arithexpr COMMA GT {
               $$ = new TableExprNodeSetElem (False, *$2);
	       delete $2;
           }
         | LBRACE arithexpr COMMA RBRACE {
               $$ = new TableExprNodeSetElem (True, *$2);
	       delete $2;
           }
         | LBRACE arithexpr COMMA GT {
               $$ = new TableExprNodeSetElem (True, *$2);
	       delete $2;
           }
         | arithexpr OPENOPEN arithexpr {
               $$ = new TableExprNodeSetElem (False, *$1, *$3, False);
	       delete $1;
	       delete $3;
           }
         | arithexpr OPENCLOSED arithexpr {
               $$ = new TableExprNodeSetElem (False, *$1, *$3, True);
	       delete $1;
	       delete $3;
           }
         | arithexpr CLOSEDOPEN arithexpr {
               $$ = new TableExprNodeSetElem (True, *$1, *$3, False);
	       delete $1;
	       delete $3;
           }
         | arithexpr CLOSEDCLOSED arithexpr {
               $$ = new TableExprNodeSetElem (True, *$1, *$3, True);
	       delete $1;
	       delete $3;
           }
	 | EMPTYOPEN arithexpr {
               $$ = new TableExprNodeSetElem (*$2, False);
	       delete $2;
           }
	 | EMPTYCLOSED arithexpr {
               $$ = new TableExprNodeSetElem (*$2, True);
	       delete $2;
           }
	 | arithexpr OPENEMPTY {
               $$ = new TableExprNodeSetElem (False, *$1);
	       delete $1;
           }
	 | arithexpr CLOSEDEMPTY {
               $$ = new TableExprNodeSetElem (True, *$1);
	       delete $1;
           }
         ;

subscripts: subscripts COMMA subsrange {
               $$ = $1;
	       $$->add (*$3);
	       delete $3;
	   }
         | subscripts COMMA {
               $$ = $1;
	       $$->add (TableExprNodeSetElem (0, 0, 0));
	   }
         | COMMA {
	       $$ = new TableExprNodeSet;
	       $$->add (TableExprNodeSetElem (0, 0, 0));
	       $$->add (TableExprNodeSetElem (0, 0, 0));
	   }
         | COMMA subsrange {
	       $$ = new TableExprNodeSet;
	       $$->add (TableExprNodeSetElem (0, 0, 0));
	       $$->add (*$2);
	       delete $2;
	   }
         | subsrange {
	       $$ = new TableExprNodeSet;
	       $$->add (*$1);
	       delete $1;
	   }
         ;

subsrange: arithexpr {
               $$ = new TableExprNodeSetElem (*$1);
	       delete $1;
            }
         | colonrange {
               $$ = $1;
	 }

colonrange: arithexpr COLON arithexpr {
               $$ = new TableExprNodeSetElem ($1, $3, 0);
	       delete $1;
	       delete $3;
            }
         |  arithexpr COLON arithexpr COLON arithexpr {
               $$ = new TableExprNodeSetElem ($1, $3, $5);
	       delete $1;
	       delete $3;
	       delete $5;
            }
         |  arithexpr COLON {
	       TableExprNode incr(1);
               $$ = new TableExprNodeSetElem ($1, 0, &incr);
	       delete $1;
            }
         |  arithexpr COLON COLON arithexpr {
               $$ = new TableExprNodeSetElem ($1, 0, $4);
	       delete $1;
	       delete $4;
            }
         |  COLON arithexpr {
               $$ = new TableExprNodeSetElem (0, $2, 0);
	       delete $2;
            }
         |  COLON arithexpr COLON arithexpr {
               $$ = new TableExprNodeSetElem (0, $2, $4);
	       delete $2;
	       delete $4;
            }
         |  COLON COLON arithexpr {
               $$ = new TableExprNodeSetElem (0, 0, $3);
	       delete $3;
            }
         ;
%%
