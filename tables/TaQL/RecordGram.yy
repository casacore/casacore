/*
    RecordGram.y: Parser for table commands
    Copyright (C) 2000-2002,2003
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

/*
 The grammar has 1 shift/reduce conflict which is resolved in a correct way.
*/


%{
using namespace casacore;
%}

%pure-parser                /* make parser re-entrant */

%expect 1                   /* do not report 1 shift/reduce conflict */

%union {
TableExprNode* node;
RecordGramVal* val;
TableExprNodeSetElem* elem;
TableExprNodeSet* settp;
}

%token <val> NAME           /* name of function or shorthand for table */
%token <val> FLDNAME        /* name of field or shorthand for table */
%token <val> LITERAL
%token <val> STRINGLITERAL
%token <val> REGEX
%token STYLE
%token IN
%token INCONE
%token BETWEEN
%token LIKE
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
%type <val> unit
%type <node> orexpr
%type <node> andexpr
%type <node> relexpr
%type <node> arithexpr
%type <node> inxexpr
%type <node> simexpr
%type <node> simbexpr
%type <node> set
%type <node> singlerange
%type <settp> subscripts
%type <settp> elemlist
%type <settp> elems
%type <elem> elem
%type <elem> subsrange
%type <elem> colonrange
%type <elem> range


%left OR
%left AND
%nonassoc EQ GT GE LT LE NE
%left BITOR
%left BITXOR
%left BITAND
%left PLUS MINUS
%left TIMES DIVIDE DIVIDETRUNC MODULO
%nonassoc UNARY BITNOT
%nonassoc NOT
%right POWER


%{
namespace casacore { //# NAMESPACE CASACORE - BEGIN
} //# NAMESPACE CASACORE - END
int RecordGramlex (YYSTYPE*);
%}

%%
topcomm:   whexpr
         | stylecoms whexpr
         ;

stylecoms: stylecoms stylecomm
         | stylecomm
         ;

stylecomm: STYLE stylelist
         ;

stylelist: stylelist COMMA NAME {
               RecordGram::theirTaQLStyle.set ($3->str);
               RecordGram::deleteToken ($3);
           }
         | NAME {
               RecordGram::theirTaQLStyle.set ($1->str);
               RecordGram::deleteToken ($1);
           }
         ;

whexpr:    orexpr
                 /* set the final result */
             { RecordGram::setNodePtr ($1); }
         ;

orexpr:    andexpr
         | orexpr OR andexpr {
	       $$ = new TableExprNode (*$1 || *$3);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	       RecordGram::deleteToken ($3);
	   }
         ;

andexpr:   relexpr
         | andexpr AND relexpr {
	       $$ = new TableExprNode (*$1 && *$3);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	       RecordGram::deleteToken ($3);
	   }
         ;

relexpr:   arithexpr
         | arithexpr EQ arithexpr {
	       $$ = new TableExprNode (*$1 == *$3);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	       RecordGram::deleteToken ($3);
	   }
         | arithexpr GT arithexpr {
	       $$ = new TableExprNode (*$1 > *$3);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	       RecordGram::deleteToken ($3);
	   }
         | arithexpr GE arithexpr {
	       $$ = new TableExprNode (*$1 >= *$3);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	       RecordGram::deleteToken ($3);
	   }
         | arithexpr LT arithexpr {
	       $$ = new TableExprNode (*$1 < *$3);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	       RecordGram::deleteToken ($3);
	   }
         | arithexpr LE arithexpr {
	       $$ = new TableExprNode (*$1 <= *$3);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	       RecordGram::deleteToken ($3);
	   }
         | arithexpr NE arithexpr {
	       $$ = new TableExprNode (*$1 != *$3);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	       RecordGram::deleteToken ($3);
	   }
         | arithexpr REGEX {
           $$ = new TableExprNode (RecordGram::handleRegex (*$1, $2->str));
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	       RecordGram::deleteToken ($2);
	   }
         | arithexpr LIKE arithexpr {
	       $$ = new TableExprNode (*$1 == sqlpattern(*$3));
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	       RecordGram::deleteToken ($3);
	   }
         | arithexpr NOT LIKE arithexpr {
	       $$ = new TableExprNode (*$1 != sqlpattern(*$4));
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	       RecordGram::deleteToken ($4);
	   }
         | arithexpr IN arithexpr {
               $$ = new TableExprNode ($1->in (*$3));
               RecordGram::addToken ($$);
               RecordGram::deleteToken ($1);
               RecordGram::deleteToken ($3);
           }
         | arithexpr NOT IN arithexpr {
               TableExprNode node ($1->in (*$4));
               $$ = new TableExprNode (!node);
               RecordGram::addToken ($$);
               RecordGram::deleteToken ($1);
               RecordGram::deleteToken ($4);
           }
         | arithexpr IN singlerange {
               $$ = new TableExprNode ($1->in (*$3));
               RecordGram::addToken ($$);
               RecordGram::deleteToken ($1);
               RecordGram::deleteToken ($3);
           }
         | arithexpr NOT IN singlerange {
               TableExprNode node ($1->in (*$4));
               $$ = new TableExprNode (!node);
               RecordGram::addToken ($$);
               RecordGram::deleteToken ($1);
               RecordGram::deleteToken ($4);
           }
         | arithexpr BETWEEN arithexpr AND arithexpr {
 	       TableExprNodeSet set;
	       set.add (TableExprNodeSetElem(True, *$3, *$5, True));
               $$ = new TableExprNode ($1->in (set));
               RecordGram::addToken ($$);
               RecordGram::deleteToken ($1);
               RecordGram::deleteToken ($3);
	       RecordGram::deleteToken ($5);
           }
         | arithexpr NOT BETWEEN arithexpr AND arithexpr {
 	       TableExprNodeSet set;
	       set.add (TableExprNodeSetElem(True, *$4, *$6, True));
               TableExprNode node ($1->in (set));
               $$ = new TableExprNode (!node);
               RecordGram::addToken ($$);
               RecordGram::deleteToken ($1);
               RecordGram::deleteToken ($4);
	       RecordGram::deleteToken ($6);
           }
         | arithexpr INCONE arithexpr {
	       $$ = new TableExprNode (anyCone (*$1, *$3));
               RecordGram::addToken ($$);
               RecordGram::deleteToken ($1);
               RecordGram::deleteToken ($3);
           }
         | arithexpr NOT INCONE arithexpr {
	       TableExprNode node(anyCone (*$1, *$4));
	       $$ = new TableExprNode (!node);
               RecordGram::addToken ($$);
               RecordGram::deleteToken ($1);
               RecordGram::deleteToken ($4);
           }
         ;

arithexpr: inxexpr
         | arithexpr PLUS  arithexpr {
	       $$ = new TableExprNode (*$1 + *$3);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	       RecordGram::deleteToken ($3);
	   }
         | arithexpr MINUS arithexpr {
	       $$ = new TableExprNode (*$1 - *$3);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	       RecordGram::deleteToken ($3);
	   }
         | arithexpr TIMES  arithexpr {
	       $$ = new TableExprNode (*$1 * *$3);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	       RecordGram::deleteToken ($3);
	   }
         | arithexpr DIVIDE arithexpr {
	       $$ = new TableExprNode (*$1 / *$3);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	       RecordGram::deleteToken ($3);
	   }
         | arithexpr DIVIDETRUNC arithexpr {
               $$ = new TableExprNode (floor(*$1 / *$3));
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	       RecordGram::deleteToken ($3);
	   }
         | arithexpr MODULO arithexpr {
	       $$ = new TableExprNode (*$1 % *$3);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	       RecordGram::deleteToken ($3);
	   }
         | arithexpr BITAND arithexpr {
	       $$ = new TableExprNode (*$1 & *$3);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	       RecordGram::deleteToken ($3);
	   }
         | arithexpr BITXOR arithexpr {
	       $$ = new TableExprNode (*$1 ^ *$3);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	       RecordGram::deleteToken ($3);
	   }
         | arithexpr BITOR arithexpr {
               $$ = new TableExprNode (*$1 | *$3);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	       RecordGram::deleteToken ($3);
	   }
         | MINUS arithexpr %prec UNARY {
	       $$ = new TableExprNode (-*$2);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($2);
	   }
         | PLUS  arithexpr %prec UNARY
               { $$ = $2; }
         | BITNOT arithexpr {
	       $$ = new TableExprNode (~*$2);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($2);
	   }
         | NOT   arithexpr {
	       $$ = new TableExprNode (!*$2);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($2);
	   }
         | arithexpr POWER arithexpr {
	       $$ = new TableExprNode (pow (*$1, *$3));
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	       RecordGram::deleteToken ($3);
	   }
         ;

inxexpr:   simexpr
         | simexpr LBRACKET subscripts RBRACKET {
               $$ = new TableExprNode (TableParseSelect::handleSlice
				       (*$1, *$3, RecordGram::theirTaQLStyle));
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	       RecordGram::deleteToken ($3);
	   }
         ;

simexpr:   simbexpr
               { $$ = $1; }
         | simbexpr unit {
	       $$ = new TableExprNode ($1->useUnit ($2->str));
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	       RecordGram::deleteToken ($2);
           }
         ;

simbexpr:  LPAREN orexpr RPAREN
               { $$ = $2; }
         | NAME LPAREN elemlist RPAREN {
               $$ = new TableExprNode (RecordGram::handleFunc ($1->str, *$3));
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	       RecordGram::deleteToken ($3);
	   }
         | FLDNAME LPAREN elemlist RPAREN {
               $$ = new TableExprNode (RecordGram::handleFunc ($1->str, *$3));
               RecordGram::addToken ($$);
               RecordGram::deleteToken ($1);
               RecordGram::deleteToken ($3);
	   }
         | NAME {
	       $$ = new TableExprNode (RecordGram::handleField ($1->str));
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	   }
         | FLDNAME {
	       $$ = new TableExprNode (RecordGram::handleField ($1->str));
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	   }
         | LITERAL {
	       $$ = new TableExprNode (RecordGram::handleLiteral ($1));
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	   }
         | STRINGLITERAL {
	       $$ = new TableExprNode (RecordGram::handleLiteral ($1));
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	   }
         | set {
	       $$ = $1;
	   }
         ;

unit:      NAME            /* simple unit */
           { $$ = $1; }
         | FLDNAME         /* unit with . */
           { $$ = $1; }
         | STRINGLITERAL   /* compound unit (with special characters) */
           { $$ = $1; }
         ;

set:       LBRACKET elems RBRACKET {
               $$ = new TableExprNode ($2->setOrArray());
               RecordGram::addToken ($$);
               RecordGram::deleteToken ($2);
           }
         | LPAREN elems RPAREN {
               $$ = new TableExprNode ($2->setOrArray());
               RecordGram::addToken ($$);
               RecordGram::deleteToken ($2);
           }
         ;

elemlist:  elems
               { $$ = $1; }
         | {
               $$ = new TableExprNodeSet;         /* no elements */
               RecordGram::addToken ($$);
           }
         ;

elems:     elems COMMA elem {
               $$ = $1;
	       $$->add (*$3);
	       RecordGram::deleteToken ($3);
	   }
         | elem {
	       $$ = new TableExprNodeSet;
               RecordGram::addToken ($$);
	       $$->add (*$1);
	       RecordGram::deleteToken ($1);
	   }
         ;

elem:      orexpr {
               $$ = new TableExprNodeSetElem(*$1);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	   }
         | range {
               $$ = $1;
           }
         ;

singlerange: range {
	       TableExprNodeSet set;
	       set.add (*$1);
	       RecordGram::deleteToken ($1);
               $$ = new TableExprNode (set.setOrArray());
               RecordGram::addToken ($$);
           }
         ;

range:     colonrange {
               $$ = $1;
           }
         | LT arithexpr COMMA arithexpr GT {
               $$ = new TableExprNodeSetElem (False, *$2, *$4, False);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($2);
	       RecordGram::deleteToken ($4);
           }
         | LT arithexpr COMMA arithexpr RBRACE {
               $$ = new TableExprNodeSetElem (False, *$2, *$4, True);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($2);
	       RecordGram::deleteToken ($4);
           }
         | LBRACE arithexpr COMMA arithexpr GT {
               $$ = new TableExprNodeSetElem (True, *$2, *$4, False);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($2);
	       RecordGram::deleteToken ($4);
           }
         | LBRACE arithexpr COMMA arithexpr RBRACE {
               $$ = new TableExprNodeSetElem (True, *$2, *$4, True);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($2);
	       RecordGram::deleteToken ($4);
           }
         | LBRACE COMMA arithexpr GT {
               $$ = new TableExprNodeSetElem (*$3, False);
               RecordGram::addToken ($$);
               RecordGram::deleteToken ($3);
          }
         | LT COMMA arithexpr GT {
               $$ = new TableExprNodeSetElem (*$3, False);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($3);
          }
         | LBRACE COMMA arithexpr RBRACE {
               $$ = new TableExprNodeSetElem (*$3, True);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($3);
           }
         | LT COMMA arithexpr RBRACE {
               $$ = new TableExprNodeSetElem (*$3, True);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($3);
           }
         | LT arithexpr COMMA RBRACE {
               $$ = new TableExprNodeSetElem (False, *$2);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($2);
           }
         | LT arithexpr COMMA GT {
               $$ = new TableExprNodeSetElem (False, *$2);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($2);
           }
         | LBRACE arithexpr COMMA RBRACE {
               $$ = new TableExprNodeSetElem (True, *$2);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($2);
           }
         | LBRACE arithexpr COMMA GT {
               $$ = new TableExprNodeSetElem (True, *$2);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($2);
           }
         | arithexpr OPENOPEN arithexpr {
               $$ = new TableExprNodeSetElem (False, *$1, *$3, False);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	       RecordGram::deleteToken ($3);
           }
         | arithexpr OPENCLOSED arithexpr {
               $$ = new TableExprNodeSetElem (False, *$1, *$3, True);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	       RecordGram::deleteToken ($3);
           }
         | arithexpr CLOSEDOPEN arithexpr {
               $$ = new TableExprNodeSetElem (True, *$1, *$3, False);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	       RecordGram::deleteToken ($3);
           }
         | arithexpr CLOSEDCLOSED arithexpr {
               $$ = new TableExprNodeSetElem (True, *$1, *$3, True);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	       RecordGram::deleteToken ($3);
           }
	 | EMPTYOPEN arithexpr {
               $$ = new TableExprNodeSetElem (*$2, False);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($2);
           }
	 | EMPTYCLOSED arithexpr {
               $$ = new TableExprNodeSetElem (*$2, True);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($2);
           }
	 | arithexpr OPENEMPTY {
               $$ = new TableExprNodeSetElem (False, *$1);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
           }
	 | arithexpr CLOSEDEMPTY {
               $$ = new TableExprNodeSetElem (True, *$1);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
           }
         ;

subscripts: subscripts COMMA subsrange {
               $$ = $1;
	       $$->add (*$3);
	       RecordGram::deleteToken ($3);
	   }
         | subscripts COMMA {
               $$ = $1;
	       $$->add (TableExprNodeSetElem (0, 0, 0));
	   }
         | COMMA {
	       $$ = new TableExprNodeSet;
               RecordGram::addToken ($$);
	       $$->add (TableExprNodeSetElem (0, 0, 0));
	       $$->add (TableExprNodeSetElem (0, 0, 0));
	   }
         | COMMA subsrange {
	       $$ = new TableExprNodeSet;
               RecordGram::addToken ($$);
	       $$->add (TableExprNodeSetElem (0, 0, 0));
	       $$->add (*$2);
	       RecordGram::deleteToken ($2);
	   }
         | subsrange {
	       $$ = new TableExprNodeSet;
               RecordGram::addToken ($$);
	       $$->add (*$1);
	       RecordGram::deleteToken ($1);
	   }
         ;

subsrange: arithexpr {
               $$ = new TableExprNodeSetElem (*$1);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
           }
         | colonrange {
               $$ = $1;
	   }
         ;

colonrange: arithexpr COLON arithexpr {
               $$ = new TableExprNodeSetElem ($1, $3, 0);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	       RecordGram::deleteToken ($3);
            }
         |  arithexpr COLON arithexpr COLON arithexpr {
               $$ = new TableExprNodeSetElem ($1, $3, $5);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	       RecordGram::deleteToken ($3);
	       RecordGram::deleteToken ($5);
            }
         |  arithexpr COLON {
	       TableExprNode incr(1);
               $$ = new TableExprNodeSetElem ($1, 0, &incr);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
            }
         |  arithexpr COLON COLON arithexpr {
               $$ = new TableExprNodeSetElem ($1, 0, $4);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($1);
	       RecordGram::deleteToken ($4);
            }
         |  COLON arithexpr {
               $$ = new TableExprNodeSetElem (0, $2, 0);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($2);
            }
         |  COLON arithexpr COLON arithexpr {
               $$ = new TableExprNodeSetElem (0, $2, $4);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($2);
	       RecordGram::deleteToken ($4);
            }
         |  COLON COLON arithexpr {
               $$ = new TableExprNodeSetElem (0, 0, $3);
               RecordGram::addToken ($$);
	       RecordGram::deleteToken ($3);
            }
         ;
%%
