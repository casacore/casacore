/*
    TableGram.y: Parser for table commands
    Copyright (C) 1994,1995,1997,1998
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
PtrBlock<TableParseSort*>* sortb;
TableParseSort* sort;
TableParseSelect* select;
}

%token SELECT
%token FROM
%token WHERE
%token ORDERBY
%token NODUPL
%token GIVING
%token SORTASC
%token SORTDESC
%token <val> NAME           /* name of function or shorthand for table */
%token <val> FLDNAME        /* name of field or shorthand for table */
%token <val> TABNAME        /* table name */
%token <val> LITERAL
%token <val> STRINGLITERAL
%token AS
%token IN
%token LPAREN
%token RPAREN
%token COMMA
%token LBRACKET
%token RBRACKET
%token COLON
%token OPENOPEN
%token OPENCLOSED
%token CLOSEDOPEN
%token CLOSEDCLOSED
%token OPENEMPTY
%token EMPTYOPEN
%token CLOSEDEMPTY
%token EMPTYCLOSED
%type <val> tfname
%type <val> tabname
%type <node> whexpr
%type <node> orexpr
%type <node> andexpr
%type <node> relexpr
%type <node> arithexpr
%type <node> unaryexpr
%type <node> inxexpr
%type <node> simexpr
%type <node> set
%type <settp> subscripts
%type <settp> elemlist
%type <settp> elems
%type <elem> elem
%type <elem> colonrange
%type <sort>  sortexpr
%type <sortb> sortlist


%left OR
%left AND
%nonassoc EQ GT GE LT LE NE
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%right POWER
%right NOT

%{
int TableGramlex (YYSTYPE*);
%}

%%
command:   select selrow
         ;

select:    SELECT {
               TableParseSelect::newSelect();
	   }
         ;
selrow:    selwh order given
         ;

selwh:     columns FROM table whexpr {
	       TableParseSelect::currentSelect()->handleSelect ($4);
	       delete $4;
	   }
         ;

order:               /* no sort */
         | ORDERBY sortlist {
	       TableParseSelect::currentSelect()->handleSort ($2, False);
	   }
         | ORDERBY NODUPL sortlist {
	       TableParseSelect::currentSelect()->handleSort ($3, True);
	   }
         ;

given:               /* no result */
         | GIVING tabname {
               TableParseSelect::currentSelect()->handleGiving ($2->str);
	       delete $2;
	   }
         | GIVING LBRACKET elems RBRACKET {
               TableParseSelect::currentSelect()->handleGiving (*$3);
	       delete $3;
	   }
         ;

columns:             /* no column names given (thus take all) */
         | NAME {
	       TableParseSelect::currentSelect()->handleSelectColumn ($1->str);
	       delete $1;
	   }
         | columns COMMA NAME {
	       TableParseSelect::currentSelect()->handleSelectColumn ($3->str);
	       delete $3;
	   }
         ;

table:     NAME {                            /* table is shorthand */
	       TableParseSelect::currentSelect()->addTable ($1, $1->str);
	       delete $1;
	   }
         | tfname {                          /* no shorthand */
	       TableParseSelect::currentSelect()->addTable ($1, "");
	       delete $1;
	   }
	 | tabname NAME {
	       TableParseSelect::currentSelect()->addTable ($1, $2->str);
	       delete $1;
	       delete $2;
	   }
	 | tabname AS NAME {
	       TableParseSelect::currentSelect()->addTable ($1, $3->str);
	       delete $1;
	       delete $3;
	   }
	 | NAME IN tabname {
	       TableParseSelect::currentSelect()->addTable ($3, $1->str);
	       delete $1;
	       delete $3;
	   }
         | table COMMA NAME {
	       TableParseSelect::currentSelect()->addTable ($3, $3->str);
	       delete $3;
	   }
         | table COMMA tfname {
	       TableParseSelect::currentSelect()->addTable ($3, "");
	       delete $3;
	   }
	 | table COMMA tabname NAME {
	       TableParseSelect::currentSelect()->addTable ($3, $4->str);
	       delete $3;
	       delete $4;
	   }
	 | table COMMA tabname AS NAME {
	       TableParseSelect::currentSelect()->addTable ($3, $5->str);
	       delete $3;
	       delete $5;
	   }
	 | table COMMA NAME IN tabname {
	       TableParseSelect::currentSelect()->addTable ($5, $3->str);
	       delete $3;
	       delete $5;
	   }
         ;

tfname:    TABNAME
               { $$ = $1; }
         | FLDNAME
               { $$ = $1; }
         | STRINGLITERAL
               { $$ = $1; }
         ;

tabname:   NAME
               { $$ = $1; }
         | tfname
               { $$ = $1; }
         ;   

whexpr:
	       { $$ = 0; }                      /* no selection */
	 | WHERE orexpr
	       { $$ = $2; }
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

arithexpr: unaryexpr
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
         | arithexpr POWER  arithexpr {
	       $$ = new TableExprNode (pow (*$1, *$3));
	       delete $1;
	       delete $3;
	   }
         ;

unaryexpr: inxexpr
               { $$ = $1; }
         | MINUS unaryexpr {
	       $$ = new TableExprNode (-*$2);
	       delete $2;
	   }
         | PLUS  unaryexpr
               { $$ = $2; }
         | NOT   unaryexpr {
	       $$ = new TableExprNode (!*$2);
	       delete $2;
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
               $$ = new TableExprNode (TableParseSelect::currentSelect()->
                                                 handleFunc ($1->str, *$3));
	       delete $1;
	       delete $3;
	   }
         | NAME {
	       $$ = new TableExprNode (TableParseSelect::currentSelect()->
                                                 handleKeyCol($1->str));
	       delete $1;
	   }
         | FLDNAME {
	       $$ = new TableExprNode (TableParseSelect::currentSelect()->
                                                 handleKeyCol($1->str));
	       delete $1;
	   }
         | LITERAL {
	       $$ = new TableExprNode (TableParseSelect::currentSelect()->
                                                 handleLiteral ($1));
	       delete $1;
	   }
         | STRINGLITERAL {
	       $$ = new TableExprNode (TableParseSelect::currentSelect()->
                                                 handleLiteral ($1));
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
         | LBRACKET command RBRACKET {
	       TableParseSelect* p = TableParseSelect::popSelect();
               $$ = new TableExprNode (p->doSubQuery());
	       delete p;
           }
         ;

elemlist: elems
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

elem:    colonrange {
               $$ = $1;
           }
         | orexpr OPENOPEN orexpr {
               $$ = new TableExprNodeSetElem (False, *$1, *$3, False);
	       delete $1;
	       delete $3;
           }
         | orexpr OPENCLOSED orexpr {
               $$ = new TableExprNodeSetElem (False, *$1, *$3, True);
	       delete $1;
	       delete $3;
           }
         | orexpr CLOSEDOPEN orexpr {
               $$ = new TableExprNodeSetElem (True, *$1, *$3, False);
	       delete $1;
	       delete $3;
           }
         | orexpr CLOSEDCLOSED orexpr {
               $$ = new TableExprNodeSetElem (True, *$1, *$3, True);
	       delete $1;
	       delete $3;
           }
	 | EMPTYOPEN orexpr {
               $$ = new TableExprNodeSetElem (*$2, False);
	       delete $2;
           }
	 | EMPTYCLOSED orexpr {
               $$ = new TableExprNodeSetElem (*$2, True);
	       delete $2;
           }
	 | orexpr OPENEMPTY {
               $$ = new TableExprNodeSetElem (False, *$1);
	       delete $1;
           }
	 | orexpr CLOSEDEMPTY {
               $$ = new TableExprNodeSetElem (True, *$1);
	       delete $1;
           }
         ;

subscripts: subscripts COMMA colonrange {
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
         | COMMA colonrange {
	       $$ = new TableExprNodeSet;
	       $$->add (TableExprNodeSetElem (0, 0, 0));
	       $$->add (*$2);
	       delete $2;
	   }
         | colonrange {
	       $$ = new TableExprNodeSet;
	       $$->add (*$1);
	       delete $1;
	   }
         ;

colonrange: orexpr {
               $$ = new TableExprNodeSetElem (*$1);
	       delete $1;
            }
         |  orexpr COLON orexpr {
               $$ = new TableExprNodeSetElem ($1, $3, 0);
	       delete $1;
	       delete $3;
            }
         |  orexpr COLON orexpr COLON orexpr {
               $$ = new TableExprNodeSetElem ($1, $3, $5);
	       delete $1;
	       delete $3;
	       delete $5;
            }
         |  orexpr COLON {
	       TableExprNode incr(1);
               $$ = new TableExprNodeSetElem ($1, 0, &incr);
	       delete $1;
            }
         |  orexpr COLON COLON orexpr {
               $$ = new TableExprNodeSetElem ($1, 0, $4);
	       delete $1;
	       delete $4;
            }
         |  COLON orexpr {
               $$ = new TableExprNodeSetElem (0, $2, 0);
	       delete $2;
            }
         |  COLON orexpr COLON orexpr {
               $$ = new TableExprNodeSetElem (0, $2, $4);
	       delete $2;
	       delete $4;
            }
         |  COLON COLON orexpr {
               $$ = new TableExprNodeSetElem (0, 0, $3);
	       delete $3;
            }
         ;

sortlist : sortlist COMMA sortexpr {
               $$ = $1;
               $$->resize($$->nelements() + 1);
	       (*$$)[$$->nelements() - 1] = $3;
	   }
         | sortexpr {
	       $$ = new PtrBlock<TableParseSort*>(1);
	       (*$$)[0] = $1;
	   }
         ;

sortexpr : orexpr {
               $$ = new TableParseSort (*$1, Sort::Ascending);
	       delete $1;
           }
         | orexpr SORTASC {
               $$ = new TableParseSort (*$1, Sort::Ascending);
	       delete $1;
           }
         | orexpr SORTDESC {
               $$ = new TableParseSort (*$1, Sort::Descending);
	       delete $1;
           }
         ;
%%
