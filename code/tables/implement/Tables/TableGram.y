/*
    TableGram.y: Parser for table commands
    Copyright (C) 1994,1995,1997
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
// Forward declaration of YYSTYPE.
// It gets defined by Bison at the %union statement.
void TableGramerror (char* s)
    { printf ("%s\n", s); }
%}

%pure_parser                /* make parser re-entrant */

%union {
TableExprNode* node;
TableParseVal* val;
Block<TableExprNode>* exprb;
PtrBlock<TableParseSort*>* sortb;
TableParseSort* sort;
}

%token SELECT
%token FROM
%token WHERE
%token ORDERBY
%token GIVING
%token SORTASC
%token SORTDESC
%token <val> NAME           /* name of field or shorthand for table */
%token <val> TABNAME        /* table name */
%token <val> LITERAL
%token LPAREN
%token RPAREN
%token COMMA
%token LBRACKET
%token RBRACKET
%type <node> whexpr
%type <node> orexpr
%type <node> andexpr
%type <node> relexpr
%type <node> arithexpr
%type <node> simexpr
%type <exprb> paramlist
%type <exprb> params
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
command:   select selrow {
               TableParseSelect::deleteSelect();
	   }
         ;

select:    SELECT {
               TableParseSelect::newSelect();
	   }
         ;
selrow:    selwh order given
         ;

selwh:     columns FROM table whexpr {
	       TableParseSelect::currentSelect()->doSelect ($4);
	       delete $4;
	   }
         ;

order:               /* no sort */
         | ORDERBY sortlist {
	       TableParseSelect::currentSelect()->doSort ($2);
	   }
         ;

given:               /* no result */
         | GIVING TABNAME {
               TableParseSelect::currentSelect()->handleGiving ($2->str);
	       delete $2;
	   }
         ;

columns:             /* no column names given (thus take all) */
         | NAME {
	       TableParseSelect::currentSelect()->handleColumn ($1->str);
	       delete $1;
	   }
         | columns COMMA NAME {
	       TableParseSelect::currentSelect()->handleColumn ($3->str);
	       delete $3;
	   }
         ;

table:     NAME {                            /* table is shorthand */
	       TableParseSelect::currentSelect()->addTable ($1->str, $1->str);
	       delete $1;
	   }
         | TABNAME {                         /* no shorthand */
	       TableParseSelect::currentSelect()->addTable ($1->str, "");
	       delete $1;
	   }
	 | NAME NAME {
	       TableParseSelect::currentSelect()->addTable ($1->str, $2->str);
	       delete $1;
	       delete $2;
	   }
	 | TABNAME NAME {
	       TableParseSelect::currentSelect()->addTable ($1->str, $2->str);
	       delete $1;
	       delete $2;
	   }
         | table COMMA NAME {
	       TableParseSelect::currentSelect()->addTable ($3->str, $3->str);
	       delete $3;
	   }
         | table COMMA TABNAME {
	       TableParseSelect::currentSelect()->addTable ($3->str, "");
	       delete $3;
	   }
	 | table COMMA NAME NAME {
	       TableParseSelect::currentSelect()->addTable ($3->str, $4->str);
	       delete $3;
	       delete $4;
	   }
	 | table COMMA TABNAME NAME {
	       TableParseSelect::currentSelect()->addTable ($3->str, $4->str);
	       delete $3;
	       delete $4;
	   }
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
         ;

arithexpr: simexpr
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

simexpr:   LPAREN orexpr RPAREN
               { $$ = $2; }
         | NAME LPAREN paramlist RPAREN {
               $$ = new TableExprNode (TableParseSelect::currentSelect()->
                                                   handleFunc ($1->str, *$3));
	       delete $1;
	       delete $3;
	   }
         | NAME LBRACKET paramlist RBRACKET {
	       $$ = new TableExprNode (TableParseSelect::currentSelect()->
                                                   handleArray ($1->str, *$3));
	       delete $1;
	       delete $3;
	   }
         | NAME {
	       $$ = new TableExprNode (TableParseSelect::currentSelect()->
                                                   handleName($1->str, False));
	       delete $1;
	   }
         | LITERAL {
	       $$ = new TableExprNode (TableParseSelect::currentSelect()->
                                                   handleLiteral ($1));
	       delete $1;
	   }
         | MINUS simexpr {
	       $$ = new TableExprNode (-*$2);
	       delete $2;
	   }
         | PLUS  simexpr
               { $$ = $2; }
         | NOT   simexpr {
	       $$ = new TableExprNode (!*$2);
	       delete $2;
	   }
         ;

paramlist: params {
	       $$ = $1;
	   }
         |
               { $$ = new Block<TableExprNode>; }      /* no parameters */
         ;

params:    params COMMA orexpr {
               $$ = $1;
               $$->resize($$->nelements() + 1);
	       (*$$)[$$->nelements() - 1] = *$3;
	       delete $3;
	   }
         | orexpr {
	       $$ = new Block<TableExprNode>(1);
	       (*$$)[0] = *$1;
	       delete $1;
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








