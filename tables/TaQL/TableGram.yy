/*
    TableGram.yy: Parser for table commands
    Copyright (C) 1994,1995,1997,1998,1999,2001,2002,2003
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

/*
The grammar has 2 shift/reduce conflicts which are resolved in a correct way.
- '(orexpr' can be the start of a set or be a subexpression.
- '[name'   can be the start of a set or subquery or be a record value.
*/
%expect 2

%token STYLE
%token TIMING
%token SHOW
%token SELECT
%token UPDATE
%token UPDSET
%token INSERT
%token VALUES
%token DELETE
%token COUNT
%token COUNTALL
%token CALC
%token CREATETAB
%token ALTERTAB
%token FROM
%token WHERE
%token GROUPBY
%token GROUPROLL
%token HAVING
%token ORDERBY
%token NODUPL
%token GIVING
%token INTO
%token SUBTABLES
%token EXCEPT
%token SORTASC
%token SORTDESC
%token LIMIT
%token OFFSET
%token ADDCOL
%token RENAMECOL
%token DROPCOL
%token SETKEY
%token COPYKEY
%token RENAMEKEY
%token DROPKEY
%token ADDROW
%token DMINFO
%token ALL                  /* ALL (in SELECT ALL) */
%token <val> NAME           /* name of function, field, table, or alias */
%token <val> UDFLIBSYN      /* UDF library name synonym definition */
%token <val> FLDNAME        /* name of field or table */
%token <val> TABNAME        /* table name */
%token <val> LITERAL
%token <val> STRINGLITERAL
%token <valre> REGEX
%token AS
%token TO
%token IN
%token INCONE
%token BETWEEN
%token EXISTS
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
%type <val> literal
%type <nodename> tabname
%type <nodename> stabname
%type <nodename> namefld
%type <nodename> unit
%type <node> tabalias
%type <node> tfnamen
%type <node> tfname
%type <node> showcomm
%type <nodeselect> selcomm
%type <node> updcomm
%type <node> inscomm
%type <node> delcomm
%type <node> calccomm
%type <nodeselect> nestedcomm
%type <nodeselect> countcomm
%type <nodeselect> cretabcomm
%type <nodeselect> alttabcomm
%type <nodeselect> tfcommand
%type <nodeselect> subquery
%type <nodeselect> selrow
%type <node> selcol
%type <node> normcol
%type <nodelist> tables
%type <nodelist> tabconc
%type <nodelist> concsub
%type <nodelist> concslist
%type <nodename> concinto
%type <node> whexpr
%type <node> groupby
%type <nodelist> exprlist
%type <node> having
%type <node> order
%type <node> limitoff
%type <nodelist> tabnmtyps
%type <node> tabnmtyp
%type <node> given
%type <node> into
%type <node> colexpr
%type <node> wildcol
%type <node> nrowspec
%type <node> colspec
%type <nodelist> columns
%type <nodelist> collist
%type <nodelist> nmcolumns
%type <nodelist> colspecs
%type <nodelist> colspecl
%type <nodelist> showlist
%type <nodelist> showflds
%type <node> updrow
%type <nodelist> updlist
%type <node> updexpr
%type <node> insrow
%type <nodelist> insclist
%type <node> insvalue
%type <nodelist> insparts
%type <nodelist> inspart
%type <nodelist> insvlist
%type <node> altcomm
%type <nodelist> altlist
%type <nodelist> rencols
%type <nodelist> dropcols
%type <nodelist> renkeys
%type <nodelist> dropkeys
%type <nodelist> setkeys
%type <nodelist> copykeys
%type <node> setkey
%type <node> copykey
%type <node> orexpr
%type <node> andexpr
%type <node> relexpr
%type <node> arithexpr
%type <node> inxexpr
%type <node> simexpr
%type <node> simbexpr
%type <node> set
%type <nodelist> singlerange
%type <nodelist> subscripts
%type <nodelist> elemlist
%type <nodelist> elems
%type <node> elem
%type <node> subsingle
%type <node> subsrange
%type <node> colonrange
%type <node> colonrangeinterval
%type <node> colonrangeindex
%type <node> range
%type <node> sortexpr
%type <nodelist> sortlist
%type <nodelist> dminfo
%type <nodelist> dmlist
%type <node> dmelem
%type <nodelist> recexpr
%type <node> recfield
%type <node> srecfield
%type <node> rrecfield
%type <noderecfldrep> brackval
%type <noderecfldrep> keyval
%type <noderecfldrep> srecval

/* This defines the precedence order of the operators (low to high) */
%left OR
%left AND
%nonassoc EQ EQASS GT GE LT LE NE EQNEAR NENEAR
%left BITOR
%left BITXOR
%left BITAND
%left PLUS MINUS
%left TIMES DIVIDE DIVIDETRUNC MODULO
%nonassoc UNARY BITNOT
%nonassoc NOT
%right POWER

/* Alas you cannot use objects in a union, so pointers have to be used.
   This is causing problems in cleaning up in case of a parse error.
   Hence a vector (in TaQLNode) is used to keep track of the nodes created.
   They are deleted at the end of the parsing.
*/
%union {
TaQLConstNode* val;
TaQLRegexNode* valre;
TaQLNode* node;
TaQLConstNode* nodename;
TaQLMultiNode* nodelist;
TaQLQueryNode* nodeselect;
TaQLRecFldNodeRep* noderecfldrep;
}

%{
namespace casacore { //# NAMESPACE CASACORE - BEGIN
Bool theFromQueryDone;           /* for flex for knowing how to handle a , */
} //# NAMESPACE CASACORE - END
int TableGramlex (YYSTYPE*);
%}

%%
topcomm:   command
         | sttimcoms command
         ;

sttimcoms: TIMING
             { TaQLNode::theirStyle.setTiming (True); }
         | stylecoms
         | stylecoms TIMING
             { TaQLNode::theirStyle.setTiming (True); }
         | TIMING stylecoms
             { TaQLNode::theirStyle.setTiming (True); }
         | stylecoms TIMING stylecoms
             { TaQLNode::theirStyle.setTiming (True); }
         ;

stylecoms: stylecoms stylecomm
         | stylecomm
         ;

stylecomm: STYLE stylelist
         ;

stylelist: stylelist COMMA NAME
             { TaQLNode::theirStyle.set ($3->getString()); }
         | NAME
             { TaQLNode::theirStyle.set ($1->getString()); }
         | stylelist COMMA UDFLIBSYN
             { TaQLNode::theirStyle.defineSynonym ($3->getString()); }
         | UDFLIBSYN
             { TaQLNode::theirStyle.defineSynonym ($1->getString()); }
         ;

command:   selcomm
             { TaQLNode::theirNode = *$1; }
         | updcomm
             { TaQLNode::theirNode = *$1; }
         | inscomm
             { TaQLNode::theirNode = *$1; }
         | delcomm
             { TaQLNode::theirNode = *$1; }
         | calccomm
             { TaQLNode::theirNode = *$1; }
         | nestedcomm
             { TaQLNode::theirNode = *$1; }
         | showcomm
             { TaQLNode::theirNode = *$1; }
         ;

showcomm:   SHOW showlist {
               $$ = new TaQLNode(new TaQLShowNodeRep (*$2));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

showlist:  {      /* no list */
               $$ = new TaQLMultiNode();
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | showflds
             { $$ = $1; }

showflds:  showflds tabname {
               $$ = $1;
               $$->add (*$2);
           }
         | tabname {
	       $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
               $$->add (*$1);
           }
         ;

/* The commands (besides SELECT) that can be used in a nested FROM */
nestedcomm: countcomm
             { $$ = $1; }
         | cretabcomm
             { $$ = $1; }
         | alttabcomm
             { $$ = $1; }
         ;

tfcommand: subquery {
               $$ = $1;
	   }
         | LPAREN nestedcomm RPAREN {
               $$ = $2;
	       $$->setBrackets();
	   }
         | LBRACKET nestedcomm RBRACKET {
               $$ = $2;
	       $$->setBrackets();
	   }
         ;

subquery:  LPAREN selcomm RPAREN {
               $$ = $2;
	       $$->setBrackets();
	   }
         | LBRACKET selcomm RBRACKET {
               $$ = $2;
	       $$->setBrackets();
	   }
         ;

selcomm:   SELECT selrow {
               $$ = $2;
           }

selrow:    selcol FROM tables whexpr groupby having order limitoff given dminfo {
               $$ = new TaQLQueryNode(
                    new TaQLSelectNodeRep (*$1, *$3, 0, *$4, *$5, *$6,
					   *$7, *$8, *$9, *$10));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | selcol into FROM tables whexpr groupby having order limitoff dminfo {
               $$ = new TaQLQueryNode(
		    new TaQLSelectNodeRep (*$1, *$4, 0, *$5, *$6, *$7,
					   *$8, *$9, *$2, *$10));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | selcol whexpr groupby having order limitoff given dminfo {
               $$ = new TaQLQueryNode(
                    new TaQLSelectNodeRep (*$1, *$2, *$3, *$4,
					   *$5, *$6, *$7, *$8));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | selcol into whexpr groupby having order limitoff dminfo {
               $$ = new TaQLQueryNode(
		    new TaQLSelectNodeRep (*$1, *$3, *$4, *$5,
					   *$6, *$7, *$2, *$8));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

selcol:    normcol {
               $$ = $1;
           }
         | ALL columns {
               $$ = new TaQLNode(
                    new TaQLColumnsNodeRep (False, *$2));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | NODUPL columns {
               $$ = new TaQLNode(
                    new TaQLColumnsNodeRep (True, *$2));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

normcol:   columns {
               $$ = new TaQLNode(
                    new TaQLColumnsNodeRep (False, *$1));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }

countcomm: COUNT normcol FROM tables whexpr {
	       $$ = new TaQLQueryNode(
                    new TaQLCountNodeRep (*$2, *$4, *$5));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

updcomm:   UPDATE updrow {
               $$ = $2;
           }
         ;

updrow:    tables UPDSET updlist FROM tables whexpr order limitoff {
               $$ = new TaQLNode(
                    new TaQLUpdateNodeRep (*$1, *$3, *$5, *$6, *$7, *$8));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | tables UPDSET updlist whexpr order limitoff {
               $$ = new TaQLNode(
		    new TaQLUpdateNodeRep (*$1, *$3, 0, *$4, *$5, *$6));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

updlist:   updlist COMMA updexpr {
               $$ = $1;
               $$->add (*$3);
           }
         | updexpr {
	       $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
               $$->add (*$1);
           }
         ;

updexpr:   NAME EQASS orexpr {
	       $$ = new TaQLNode(
                    new TaQLUpdExprNodeRep ($1->getString(), "", *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | NAME LBRACKET subscripts RBRACKET EQASS orexpr {
               /* array slice or mask */
	       $$ = new TaQLNode(
                    new TaQLUpdExprNodeRep ($1->getString(), "", *$3, *$6));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | NAME LBRACKET subscripts RBRACKET LBRACKET subscripts RBRACKET EQASS orexpr {
               /* array slice and mask (in any order) */
	       $$ = new TaQLNode(
                    new TaQLUpdExprNodeRep ($1->getString(), "", *$3, *$6, *$9));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | LPAREN NAME COMMA NAME RPAREN EQASS orexpr {
	       $$ = new TaQLNode(
                    new TaQLUpdExprNodeRep ($2->getString(),
                                            $4->getString(), *$7));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;
         | LPAREN NAME COMMA NAME RPAREN LBRACKET subscripts RBRACKET EQASS orexpr {
	       $$ = new TaQLNode(
                    new TaQLUpdExprNodeRep ($2->getString(),
                                            $4->getString(), *$7, *$10));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

inscomm:   INSERT insrow {
               $$ = $2;
           }
         ;

insrow:    INTO tables insclist selcomm {
               /* insert with SELECT command */
	       $4->setNoExecute();
	       $$ = new TaQLNode(
                    new TaQLInsertNodeRep (*$2, *$3, *$4, TaQLNode()));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | INTO tables insclist insvalue {
               /* insert in SQL style */
	       $$ = new TaQLNode(
                    new TaQLInsertNodeRep (*$2, *$3, *$4, TaQLNode()));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | LIMIT orexpr INTO tables insclist insvalue {
	       $$ = new TaQLNode(
                    new TaQLInsertNodeRep (*$4, *$5, *$6, *$2));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | INTO tables insclist insvalue LIMIT orexpr {
	       $$ = new TaQLNode(
                    new TaQLInsertNodeRep (*$2, *$3, *$4, *$6));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | INTO tables UPDSET updlist {
               /* insert in update style */
	       $$ = new TaQLNode(
                    new TaQLInsertNodeRep (*$2, *$4));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

insclist:  {         /* no insert column-list */   
               $$ = new TaQLMultiNode();
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | LBRACKET nmcolumns RBRACKET {
               $$ = $2;
           }
         | LPAREN nmcolumns RPAREN {
               $$ = $2;
           }
         ;

insvalue:  VALUES insparts {
	       $2->setPPFix ("VALUES ", "");
               $$ = $2;
           }
         ;

insparts:  insparts COMMA inspart {
               $$ = $1;
	       $$->add (*$3);
           }
           | inspart {
               $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
	       $$->add (*$1);
           }

inspart:   LBRACKET insvlist RBRACKET {
               $$ = $2;
           }
         | LPAREN insvlist RPAREN {
               $$ = $2;
           }
         ;

insvlist:  insvlist COMMA orexpr {
               $$ = $1;
	       $$->add (*$3);
           }
         | orexpr {
               $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
	       $$->setPPFix ("[", "]");
	       $$->add (*$1);
           }
         ;

delcomm:   DELETE FROM tables whexpr order limitoff {
	       $$ = new TaQLNode(
                    new TaQLDeleteNodeRep (*$3, *$4, *$5, *$6));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

calccomm:  CALC FROM tables CALC orexpr {
	       $$ = new TaQLNode(
                    new TaQLCalcNodeRep (*$3, *$5,
                                         TaQLNode(), TaQLNode(), TaQLNode()));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | CALC orexpr {
               TaQLMultiNode tabNode((TaQLMultiNodeRep*)0);
	       $$ = new TaQLNode(
               new TaQLCalcNodeRep (tabNode, *$2,
                                    TaQLNode(), TaQLNode(), TaQLNode()));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | CALC orexpr FROM tables whexpr order limitoff {
	       $$ = new TaQLNode(
                    new TaQLCalcNodeRep (*$4, *$2, *$5, *$6, *$7));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }

cretabcomm: CREATETAB tabnmtyp colspecs nrowspec dminfo {
	       $$ = new TaQLQueryNode(
                    new TaQLCreTabNodeRep (*$2, *$3, *$4, *$5));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
          | CREATETAB tabnmtyp LPAREN colspecs RPAREN nrowspec dminfo {
	       $$ = new TaQLQueryNode(
                    new TaQLCreTabNodeRep (*$2, *$4, *$6, *$7));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
          | CREATETAB tabnmtyp LBRACKET colspecs RBRACKET nrowspec dminfo {
	       $$ = new TaQLQueryNode(
                    new TaQLCreTabNodeRep (*$2, *$4, *$6, *$7));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

alttabcomm: ALTERTAB tabalias altlist {
               $$ = new TaQLQueryNode(
                    new TaQLAltTabNodeRep (*$2, TaQLMultiNode(), *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
           | ALTERTAB tabalias FROM tables altlist {
               $$ = new TaQLQueryNode(
                    new TaQLAltTabNodeRep (*$2, *$4, *$5));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

altlist:   altlist altcomm {
               $$ = $1;
	       $$->add (*$2);
           }
         | altcomm {
               $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
               $$->setSeparator (" ");
	       $$->add (*$1);
           }
         ;

altcomm:   ADDCOL colspecs dminfo {
               $$ = new TaQLNode (
                    new TaQLAddColNodeRep(*$2, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | RENAMECOL rencols {
	       $$ = new TaQLNode(
                    new TaQLRenDropNodeRep(0, *$2));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | DROPCOL dropcols {
	       $$ = new TaQLNode(
                    new TaQLRenDropNodeRep(1, *$2));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | SETKEY setkeys {
	       $$ = new TaQLNode(
                    new TaQLSetKeyNodeRep (*$2));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | COPYKEY copykeys {
	       $$ = new TaQLNode(
                    new TaQLSetKeyNodeRep (*$2));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | RENAMEKEY renkeys {
	       $$ = new TaQLNode(
                    new TaQLRenDropNodeRep(2, *$2));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | DROPKEY dropkeys {
	       $$ = new TaQLNode(
                    new TaQLRenDropNodeRep(3, *$2));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | ADDROW orexpr {
	       $$ = new TaQLNode(
                    new TaQLAddRowNodeRep(*$2));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

rencols:   rencols COMMA NAME TO NAME {
               $$ = $1;
               $$->add (new TaQLKeyColNodeRep ($3->getString()));
               $$->add (new TaQLKeyColNodeRep ($5->getString()));
           }
         | NAME TO NAME {
               $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
               $$->setSeparator (2, " TO ");
               $$->add (new TaQLKeyColNodeRep ($1->getString()));
               $$->add (new TaQLKeyColNodeRep ($3->getString()));
           }
         ;

dropcols:  dropcols COMMA NAME {
               $$ = $1;
               $$->add (new TaQLKeyColNodeRep ($3->getString()));
           }
         | NAME {
               $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
               $$->add (new TaQLKeyColNodeRep ($1->getString()));
           }
         ;

renkeys:   renkeys COMMA namefld TO NAME {
               $$ = $1;
               $$->add (new TaQLKeyColNodeRep ($3->getString()));
               $$->add (new TaQLKeyColNodeRep ($5->getString()));
           }
         | namefld TO NAME {
               $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
               $$->setSeparator (2, " TO ");
               $$->add (new TaQLKeyColNodeRep ($1->getString()));
               $$->add (new TaQLKeyColNodeRep ($3->getString()));
           }
         ;

dropkeys:  dropkeys COMMA namefld {
               $$ = $1;
               $$->add (new TaQLKeyColNodeRep ($3->getString()));
           }
         | namefld {
               $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
               $$->add (new TaQLKeyColNodeRep ($1->getString()));
           }
         ;

setkeys:   setkeys COMMA setkey {
               $$ = $1;
               $$->add (*$3);
           }
         | setkey {
               $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
               $$->add (*$1);
           }

setkey:   namefld EQASS keyval {
	       $$ = new TaQLNode(
                    new TaQLRecFldNodeRep($1->getString(), *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
               delete $3;
           }

copykeys:  copykeys COMMA copykey {
               $$ = $1;
               $$->add (*$3);
           }
         | copykey {
               $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
               $$->add (*$1);
           }

copykey:  namefld EQASS namefld {
	       $$ = new TaQLNode(
                    new TaQLRecFldNodeRep($1->getString(),
                                          $3->getString(), ""));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
        | namefld EQASS namefld AS NAME {
	       $$ = new TaQLNode(
                    new TaQLRecFldNodeRep($1->getString(),
                                          $3->getString(), $5->getString()));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }

keyval:    srecval {
               $$ = $1;
           }
         | brackval {
               $$ = $1;
           }

brackval:  LBRACKET recexpr RBRACKET {
	       $$ = new TaQLRecFldNodeRep ("", *$2, "");
           }
         | LBRACKET EQASS RBRACKET {
	       /* Like in glish [=] is the syntax for an empty 'record' */
               TaQLMultiNode empty(False);
               empty.setPPFix ("[", "]");
               $$ = new TaQLRecFldNodeRep ("", empty, "");
           }
         | LBRACKET RBRACKET AS NAME {
               /* empty vector of the given type */
               $$ = new TaQLRecFldNodeRep ("", TaQLNode(), $4->getString());
           }

dminfo:    {      /* no datamans */
               $$ = new TaQLMultiNode();
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | DMINFO dmlist {
               $$ = $2;
           }
         ;

exprlist:  exprlist COMMA orexpr {
               $$ = $1;
	       $$->add (*$3);
           }
         | orexpr {
               $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
	       $$->add (*$1);
           }
         ;

groupby:   {          /* no groupby */
	       $$ = new TaQLNode();
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | GROUPBY exprlist {
	       $$ = new TaQLNode(
                    new TaQLGroupNodeRep (TaQLGroupNodeRep::Normal, *$2));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | GROUPROLL exprlist {
	       $$ = new TaQLNode(
                    new TaQLGroupNodeRep (TaQLGroupNodeRep::Rollup, *$2));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         ;

having:    {          /* no having */
	       $$ = new TaQLNode();
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | HAVING orexpr {
               $$ = $2;
	   }
         ;

order:     {          /* no sort */
	       $$ = new TaQLNode();
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | ORDERBY sortlist {
	       $$ = new TaQLNode(
	            new TaQLSortNodeRep (False, TaQLSortNodeRep::Ascending, *$2));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | ORDERBY SORTASC sortlist {
	       $$ = new TaQLNode(
	            new TaQLSortNodeRep (False, TaQLSortNodeRep::Ascending, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | ORDERBY SORTDESC sortlist {
	       $$ = new TaQLNode(
	            new TaQLSortNodeRep (False, TaQLSortNodeRep::Descending, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | ORDERBY NODUPL sortlist {
	       $$ = new TaQLNode(
	            new TaQLSortNodeRep (True, TaQLSortNodeRep::Ascending, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | ORDERBY NODUPL SORTASC sortlist {
	       $$ = new TaQLNode(
	            new TaQLSortNodeRep (True, TaQLSortNodeRep::Ascending, *$4));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | ORDERBY NODUPL SORTDESC sortlist {
	       $$ = new TaQLNode(
	            new TaQLSortNodeRep (True, TaQLSortNodeRep::Descending, *$4));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | ORDERBY SORTASC NODUPL sortlist {
	       $$ = new TaQLNode(
	            new TaQLSortNodeRep (True, TaQLSortNodeRep::Ascending, *$4));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | ORDERBY SORTDESC NODUPL sortlist {
	       $$ = new TaQLNode(
	            new TaQLSortNodeRep (True, TaQLSortNodeRep::Descending, *$4));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         ;

limitoff:  {         /* no limit,offset */
	       $$ = new TaQLNode();
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | LIMIT colonrangeinterval {
	       $$ = new TaQLNode(
	            new TaQLLimitOffNodeRep (*$2, 0));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | LIMIT orexpr {
	       $$ = new TaQLNode(
	            new TaQLLimitOffNodeRep (*$2, 0));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | OFFSET orexpr {
	       $$ = new TaQLNode(
	            new TaQLLimitOffNodeRep (0, *$2));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | LIMIT orexpr OFFSET orexpr {
	       $$ = new TaQLNode(
	            new TaQLLimitOffNodeRep (*$2, *$4));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | OFFSET orexpr LIMIT orexpr {
	       $$ = new TaQLNode(
	            new TaQLLimitOffNodeRep (*$4, *$2));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         ;

tabnmtyp:  tabname {
	       $$ = new TaQLNode(
                    new TaQLGivingNodeRep ($1->getString(), TaQLMultiNode()));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | tabname AS tabnmtyps {
	       $$ = new TaQLNode(
                    new TaQLGivingNodeRep ($1->getString(), *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | AS tabnmtyps {
	       $$ = new TaQLNode(
                    new TaQLGivingNodeRep ("", *$2));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
/*
         | tabname LIKE tabname {
	       $$ = new TaQLNode(
                    new TaQLGivingNodeRep ($1->getString(), TaQLMultiNode()));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | tabname LIKE tabname AS tabnmtyps {
	       $$ = new TaQLNode(
                    new TaQLGivingNodeRep ($1->getString(), *$5));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | tabname FROM tabname {
	       $$ = new TaQLNode(
                    new TaQLGivingNodeRep ($1->getString(), TaQLMultiNode()));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | tabname FROM tabname AS tabnmtyps {
	       $$ = new TaQLNode(
                    new TaQLGivingNodeRep ($1->getString(), *$5));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
*/
         ;

tabnmtyps: NAME {  /* PLAIN_BIG, etc. for backward compatibility */
               TaQLNode val(new TaQLConstNodeRep (True));
               $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
               $$->setPPFix ("[", "]");
               $$->add (new TaQLRecFldNodeRep ($1->getString(), val, ""));
	   }
         | LBRACKET recexpr RBRACKET {
               $$ = $2;
	   }
         ;
  
given:     {          /* no result */
	       $$ = new TaQLNode();
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | GIVING tabnmtyp {
               $$ = $2;
	   }
         | GIVING LBRACKET elems RBRACKET {
	       $$ = new TaQLNode(
                    new TaQLGivingNodeRep (*$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         ;

into:      INTO tabnmtyp {
               $$ = $2;
	   }
         ;

columns:   {          /* no column names given (thus take all) */
               $$ = new TaQLMultiNode();
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | collist {
               $$ = $1;
           }

collist:   colexpr {
               $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
               $$->add (*$1);
	   }
         | collist COMMA colexpr {
	       $$ = $1;
               $$->add (*$3);
	   }
         ;

colexpr:   orexpr {
	       $$ = new TaQLNode(
                    new TaQLColNodeRep (*$1, "", "", ""));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | orexpr AS NAME {
	       $$ = new TaQLNode(
                    new TaQLColNodeRep (*$1, $3->getString(), "", ""));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | orexpr AS LPAREN NAME COMMA NAME RPAREN {
	       $$ = new TaQLNode(
                    new TaQLColNodeRep (*$1, $4->getString(),
                                        $6->getString(), ""));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | orexpr AS NAME NAME {
	       $$ = new TaQLNode(
	            new TaQLColNodeRep (*$1, $3->getString(),
                                        "", $4->getString()));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | orexpr AS LPAREN NAME COMMA NAME RPAREN NAME {
	       $$ = new TaQLNode(
                    new TaQLColNodeRep (*$1, $4->getString(),
                                        $6->getString(), $8->getString()));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
	 | wildcol {
               $$= $1;
           }
         ;

wildcol:   TIMES {          /* SELECT * FROM ... */
               TaQLRegexNode p (new TaQLRegexNodeRep ("~p/*/"));
               $$ = new TaQLNode (new TaQLColNodeRep (p, "", "", ""));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
	 | REGEX {
               $$ = new TaQLNode (new TaQLColNodeRep (*$1, "", "", ""));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

nmcolumns: NAME {
               $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
               $$->add (new TaQLKeyColNodeRep ($1->getString()));
	   }
         | LPAREN NAME COMMA NAME RPAREN {
               $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
               $$->add (new TaQLKeyColNodeRep ($2->getString(),
                                               $4->getString()));
	   }
         | nmcolumns COMMA NAME {
	       $$ = $1;
               $$->add (new TaQLKeyColNodeRep ($3->getString()));
	   }
         | nmcolumns COMMA LPAREN NAME COMMA NAME RPAREN {
	       $$ = $1;
               $$->add (new TaQLKeyColNodeRep ($4->getString(),
                                               $6->getString()));
	   }
         ;

nrowspec:  {          /* no nrows given */
               $$ = new TaQLNode();
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | LIMIT orexpr {
               $$ = $2;
	   }
         ;

colspecs:  {          /* no column specifications given */
               $$ = new TaQLMultiNode();
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | colspecl {
               $$ = $1;
           }

colspecl: colspec {
               $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
               $$->add (*$1);
	   }
         | colspecl COMMA colspec {
	       $$ = $1;
               $$->add (*$3);
	   }
         ;

colspec:   NAME NAME {
	       $$ = new TaQLNode(
		    new TaQLColSpecNodeRep($1->getString(), $2->getString(),
		                           TaQLMultiNode()));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | NAME NAME srecfield {	
               TaQLMultiNode re(False);
	       re.add (*$3);
	       $$ = new TaQLNode(
                    new TaQLColSpecNodeRep($1->getString(), $2->getString(),
		                           re));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | NAME NAME LBRACKET recexpr RBRACKET {
	       $$ = new TaQLNode(
                    new TaQLColSpecNodeRep($1->getString(), $2->getString(),
		                           *$4));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
/*
         | NAME LIKE namefld {
	       $$ = new TaQLNode(
		    new TaQLColSpecNodeRep($1->getString(), $3->getString(),
		                           TaQLMultiNode()));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | NAME LIKE namefld srecfield {	
               TaQLMultiNode re(False);
	       re.add (*$4);
	       $$ = new TaQLNode(
                    new TaQLColSpecNodeRep($1->getString(), $3->getString(),
		                           re));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | NAME LIKE namefld LBRACKET recexpr RBRACKET {
	       $$ = new TaQLNode(
                    new TaQLColSpecNodeRep($1->getString(), $3->getString(),
		                           *$5));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
*/
         ;

tables:    tabalias {
               $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
               $$->add (*$1);
	   }
         | tables COMMA tabalias {
	       $$ = $1;
               $$->add (*$3);
	   }
         ;

/* If NAME is given, it is purely alphanumeric, so it can be used as alias.
   This is not the case if another type of name is given, so in that case
   there is no alias.
   Hence the 2 cases have to be handled differently.
*/
tabalias:  NAME {                          /* table name is also alias */
	       $1->setIsTableName();
	       $$ = new TaQLNode(
                    new TaQLTableNodeRep(*$1, $1->getString()));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | tfname {                        /* no alias */
	       $$ = new TaQLNode(
	            new TaQLTableNodeRep(*$1, ""));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | tfnamen NAME {
	       $2->setIsTableName();
	       $$ = new TaQLNode(
	            new TaQLTableNodeRep(*$1, $2->getString()));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | tfnamen AS NAME {
	       $$ = new TaQLNode(
	            new TaQLTableNodeRep(*$1, $3->getString()));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | NAME IN tfnamen {
	       $$ = new TaQLNode(
	            new TaQLTableNodeRep(*$3, $1->getString()));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

tfnamen:   tfname {
               $$ = $1;
           }
         | NAME {
	       $1->setIsTableName();
               $$ = $1;
           }
         ;

tfname:    tfcommand {
	       theFromQueryDone = True;
	       $1->setFromExecute();
               $$ = $1;
           }
         | stabname {
	       $$ = $1;
           }
         | LBRACKET tabconc concsub concinto RBRACKET {
	       $$ = new TaQLNode(
                    new TaQLConcTabNodeRep($4->getString(), *$2, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

concsub:   {    /* no SUBTABLES */
                $$ = new TaQLMultiNode();
                TaQLNode::theirNodesCreated.push_back ($$);
           }
         | SUBTABLES concslist {
                $$ = $2;
           }
         ;

concslist: NAME {
               $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
	       $1->setIsTableName();
               $$->add (*$1);
	   }
         | concslist COMMA NAME {
	       $$ = $1;
	       $3->setIsTableName();
               $$->add (*$3);
	   }
         ;

concinto:  {    /* no GIVING */
                $$ = new TaQLConstNode(new TaQLConstNodeRep(String()));
                TaQLNode::theirNodesCreated.push_back ($$);
           }
         | GIVING tabname {
                $$ = $2;
           }
         | INTO tabname {
                $$ = $2;
          }
         ;

stabname:  TABNAME {
	       $1->setIsTableName();
               $$ = $1;
           }
         | FLDNAME {
	       $1->setIsTableName();
               $$ = $1;
           }
         | STRINGLITERAL {
	       $1->setIsTableName();
               $$ = $1;
           }
         ;

tabname:   NAME {
	       $1->setIsTableName();
               $$ = $1;
           }
         | stabname {
               $$ = $1;
           }
         ;

tabconc:   tabalias {
               $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
               $$->add (*$1);
	   }
         | tabconc COMMA tabalias {
	       $$ = $1;
               $$->add (*$3);
           }
	 ;

whexpr:    {                   /* no selection */
	       $$ = new TaQLNode();
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | WHERE orexpr {
	       $$ = $2;
	   }
	 ;

orexpr:    andexpr {
	       $$ = $1;
           }
	 | orexpr OR andexpr {
	       $$ = new TaQLNode(
	            new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_OR, *$1, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         ;

andexpr:   relexpr {
	       $$ = $1;
           }
         | andexpr AND relexpr {
	       $$ = new TaQLNode(
	            new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_AND, *$1, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         ;

relexpr:   arithexpr {
	       $$ = $1;
           }
         | arithexpr EQ arithexpr {
	       $$ = new TaQLNode(
	            new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_EQ, *$1, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | arithexpr EQASS arithexpr {
	       $$ = new TaQLNode(
	            new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_EQ, *$1, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | arithexpr GT arithexpr {
	       $$ = new TaQLNode(
	            new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_GT, *$1, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | arithexpr GE arithexpr {
	       $$ = new TaQLNode(
	            new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_GE, *$1, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | arithexpr LT arithexpr {
	       $$ = new TaQLNode(
	            new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_LT, *$1, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | arithexpr LE arithexpr {
	       $$ = new TaQLNode(
	            new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_LE, *$1, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | arithexpr NE arithexpr {
	       $$ = new TaQLNode(
	            new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_NE, *$1, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | arithexpr EQNEAR arithexpr {
   	       TaQLMultiNode set(False);
               set.add (*$1);
               set.add (*$3);
               set.add (TaQLConstNode(new TaQLConstNodeRep(1e-5)));
               $$ = new TaQLNode (new TaQLFuncNodeRep("NEAR", set));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | arithexpr NENEAR arithexpr {
   	       TaQLMultiNode set(False);
               set.add (*$1);
               set.add (*$3);
               set.add (TaQLConstNode(new TaQLConstNodeRep(1e-5)));
               TaQLNode ref (new TaQLFuncNodeRep("NEAR", set));
	       $$ = new TaQLNode(
                    new TaQLUnaryNodeRep (TaQLUnaryNodeRep::U_NOT, ref));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | arithexpr REGEX {
	       $$ = new TaQLNode(TaQLBinaryNodeRep::handleRegex (*$1, *$2));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | arithexpr LIKE arithexpr {
   	       TaQLMultiNode re(False);
               re.add (*$3);
               TaQLNode ref (new TaQLFuncNodeRep("SQLPATTERN", re));
	       $$ = new TaQLNode(
	            new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_EQ, *$1, ref));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | arithexpr NOT LIKE arithexpr {
   	       TaQLMultiNode re(False);
               re.add (*$4);
               TaQLNode ref (new TaQLFuncNodeRep("SQLPATTERN", re));
	       $$ = new TaQLNode(
	            new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_NE, *$1, ref));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | EXISTS subquery {
	       $2->setNoExecute();
	       $$ = new TaQLNode(
	            new TaQLUnaryNodeRep (TaQLUnaryNodeRep::U_EXISTS, *$2));
	       TaQLNode::theirNodesCreated.push_back ($$);
	    }
         | NOT EXISTS subquery {
	       $3->setNoExecute();
	       $$ = new TaQLNode(
	            new TaQLUnaryNodeRep (TaQLUnaryNodeRep::U_NOTEXISTS, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
            }
         | arithexpr IN arithexpr {
	       $$ = new TaQLNode(
	            new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_IN, *$1, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | arithexpr NOT IN arithexpr {
	       TaQLNode p(
                    new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_IN, *$1, *$4));
	       $$ = new TaQLNode(
                    new TaQLUnaryNodeRep (TaQLUnaryNodeRep::U_NOT, p));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | arithexpr IN singlerange {
	       $$ = new TaQLNode(
	            new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_IN, *$1, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | arithexpr NOT IN singlerange {
	       TaQLNode p (new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_IN, *$1, *$4));
	       $$ = new TaQLNode(
                    new TaQLUnaryNodeRep (TaQLUnaryNodeRep::U_NOT, p));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | arithexpr BETWEEN arithexpr AND arithexpr {
	       TaQLMultiNode pr(False);
	       pr.add (new TaQLRangeNodeRep (True, *$3, *$5, True));
	       $$ = new TaQLNode(
	            new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_IN, *$1, pr));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | arithexpr NOT BETWEEN arithexpr AND arithexpr {
	       TaQLMultiNode pr(False);
	       pr.add (new TaQLRangeNodeRep (True, *$4, *$6, True));
	       TaQLNode p (new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_IN, *$1, pr));
	       $$ = new TaQLNode(
                    new TaQLUnaryNodeRep (TaQLUnaryNodeRep::U_NOT, p));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | arithexpr INCONE arithexpr {
	       TaQLMultiNode pr(False);
	       pr.add (*$1);
	       pr.add (*$3);
	       $$ = new TaQLNode(
                    new TaQLFuncNodeRep ("anyCone", pr));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | arithexpr NOT INCONE arithexpr {
	       TaQLMultiNode pr(False);
	       pr.add (*$1);
	       pr.add (*$4);
               TaQLNode p (new TaQLFuncNodeRep ("anyCone", pr));
	       $$ = new TaQLNode(
                    new TaQLUnaryNodeRep (TaQLUnaryNodeRep::U_NOT, p));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

arithexpr: simexpr {
	       $$= $1;
           }
         | arithexpr PLUS  arithexpr {
	       $$ = new TaQLNode(
	            new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_PLUS, *$1, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | arithexpr MINUS arithexpr {
	       $$ = new TaQLNode(
	            new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_MINUS, *$1, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | arithexpr TIMES  arithexpr {
	       $$ = new TaQLNode(
	            new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_TIMES, *$1, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | arithexpr DIVIDE arithexpr {
	       $$ = new TaQLNode(
	            new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_DIVIDE, *$1, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | arithexpr DIVIDETRUNC arithexpr {
	       $$ = new TaQLNode(
	            new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_DIVIDETRUNC, *$1, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | arithexpr MODULO arithexpr {
	       $$ = new TaQLNode(
	            new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_MODULO, *$1, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | arithexpr BITAND arithexpr {
	       $$ = new TaQLNode(
	            new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_BITAND, *$1, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | arithexpr BITXOR arithexpr {
	       $$ = new TaQLNode(
	            new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_BITXOR, *$1, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | arithexpr BITOR arithexpr {
	       $$ = new TaQLNode(
	            new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_BITOR, *$1, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | MINUS arithexpr %prec UNARY {
	       $$ = new TaQLNode(
	            new TaQLUnaryNodeRep (TaQLUnaryNodeRep::U_MINUS, *$2));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | PLUS  arithexpr %prec UNARY
               { $$ = $2; }
         | BITNOT arithexpr {
	       $$ = new TaQLNode(
	            new TaQLUnaryNodeRep (TaQLUnaryNodeRep::U_BITNOT, *$2));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | NOT   arithexpr {
	       $$ = new TaQLNode(
	            new TaQLUnaryNodeRep (TaQLUnaryNodeRep::U_NOT, *$2));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | arithexpr POWER arithexpr {
	       $$ = new TaQLNode(
	            new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_POWER, *$1, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         ;

simexpr:   inxexpr
               { $$ = $1; }
         | inxexpr unit {
	       $$ = new TaQLNode(
                    new TaQLUnitNodeRep ($2->getString(), *$1));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

inxexpr:   simbexpr {
               $$ = $1;
           }
         | simbexpr LBRACKET subscripts RBRACKET {
	       $$ = new TaQLNode(
	            new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_INDEX, *$1, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         ;

simbexpr:  LPAREN orexpr RPAREN
               { $$ = $2; }
         | namefld LPAREN elemlist RPAREN {
	       $$ = new TaQLNode(
                    new TaQLFuncNodeRep ($1->getString(), *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | COUNT LPAREN elemlist RPAREN {
	       $$ = new TaQLNode(
                    new TaQLFuncNodeRep ("COUNT", *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | COUNTALL {
	       $$ = new TaQLNode(
                    new TaQLFuncNodeRep ("COUNTALL"));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | namefld {
	       $$ = new TaQLNode(
                    new TaQLKeyColNodeRep ($1->getString()));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | literal {
	       $$ = $1;
	   }
         | set {
	       $$ = $1;
	   }
         ;

namefld:   NAME {            /* simple name */
               $$ = $1;
           }
         | FLDNAME {         /* name with . or :: */
               $$ = $1;
           }
         ;

unit:      namefld {
               $$ = $1;
           }
         | STRINGLITERAL {   /* compound unit (with special characters) */
               $$ = $1;
           }
         ;

literal:   LITERAL {
	       $$ = $1;
	   }
         | STRINGLITERAL {
	       $$ = $1;
	   }
         ;

set:       LBRACKET elems RBRACKET {
               $2->setIsSetOrArray();
               $$ = $2;
           }
         | LPAREN elems RPAREN {
               $2->setIsSetOrArray();
               $$ = $2;
           }
         | subquery {
               $$ = $1;
           }
         ;

elemlist:  elems {
               $$ = $1;
	       $$->setPPFix("", "");
           }
         | {
               $$ = new TaQLMultiNode(False);       /* no elements */
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

elems:     elems COMMA elem {
               $$ = $1;
	       $$->add (*$3);
	   }
         | elem {
	       $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
	       $$->setPPFix ("[", "]");
	       $$->add (*$1);
	   }
         ;

elem:      orexpr {
               $$ = $1;
	   }
         | range {
               $$ = $1;
           }
         ;

singlerange: range {
	       $$ = new TaQLMultiNode(True);
	       TaQLNode::theirNodesCreated.push_back ($$);
	       $$->add (*$1);
           }
         ;

range:     colonrangeinterval {
               $$ = $1;
           }
         | LT arithexpr COMMA arithexpr GT {
	       $$ = new TaQLNode(
                    new TaQLRangeNodeRep (False, *$2, *$4, False));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | LT arithexpr COMMA arithexpr RBRACE {
	       $$ = new TaQLNode(
                    new TaQLRangeNodeRep (False, *$2, *$4, True));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | LBRACE arithexpr COMMA arithexpr GT {
	       $$ = new TaQLNode(
                    new TaQLRangeNodeRep (True, *$2, *$4, False));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | LBRACE arithexpr COMMA arithexpr RBRACE {
	       $$ = new TaQLNode(
                    new TaQLRangeNodeRep (True, *$2, *$4, True));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | LBRACE COMMA arithexpr GT {
	       $$ = new TaQLNode(
                    new TaQLRangeNodeRep (*$3, False));
	       TaQLNode::theirNodesCreated.push_back ($$);
          }
         | LT COMMA arithexpr GT {
	       $$ = new TaQLNode(
                    new TaQLRangeNodeRep (*$3, False));
	       TaQLNode::theirNodesCreated.push_back ($$);
          }
         | LBRACE COMMA arithexpr RBRACE {
	       $$ = new TaQLNode(
                    new TaQLRangeNodeRep (*$3, True));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | LT COMMA arithexpr RBRACE {
	       $$ = new TaQLNode(
                    new TaQLRangeNodeRep (*$3, True));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | LT arithexpr COMMA RBRACE {
	       $$ = new TaQLNode(
                    new TaQLRangeNodeRep (False, *$2));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | LT arithexpr COMMA GT {
	       $$ = new TaQLNode(
                    new TaQLRangeNodeRep (False, *$2));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | LBRACE arithexpr COMMA RBRACE {
	       $$ = new TaQLNode(
                    new TaQLRangeNodeRep (True, *$2));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | LBRACE arithexpr COMMA GT {
	       $$ = new TaQLNode(
                    new TaQLRangeNodeRep (True, *$2));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | arithexpr OPENOPEN arithexpr {
	       $$ = new TaQLNode(
                    new TaQLRangeNodeRep (False, *$1, *$3, False));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | arithexpr OPENCLOSED arithexpr {
	       $$ = new TaQLNode(
                    new TaQLRangeNodeRep (False, *$1, *$3, True));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | arithexpr CLOSEDOPEN arithexpr {
	       $$ = new TaQLNode(
                    new TaQLRangeNodeRep (True, *$1, *$3, False));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | arithexpr CLOSEDCLOSED arithexpr {
	       $$ = new TaQLNode(
                    new TaQLRangeNodeRep (True, *$1, *$3, True));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
	 | EMPTYOPEN arithexpr {
	       $$ = new TaQLNode(
                    new TaQLRangeNodeRep (*$2, False));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
	 | EMPTYCLOSED arithexpr {
	       $$ = new TaQLNode(
                    new TaQLRangeNodeRep (*$2, True));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
	 | arithexpr OPENEMPTY {
	       $$ = new TaQLNode(
                    new TaQLRangeNodeRep (False, *$1));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
	 | arithexpr CLOSEDEMPTY {
	       $$ = new TaQLNode(
                    new TaQLRangeNodeRep (True, *$1));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

subscripts: subscripts COMMA subsrange {
               $$ = $1;
	       $$->add (*$3);
	   }
         | subscripts COMMA {
               $$ = $1;
	       $$->add (new TaQLIndexNodeRep(0, 0, 0));
	   }
         | COMMA {
	       $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
	       $$->setPPFix ("[", "]");
	       $$->add (new TaQLIndexNodeRep(0, 0, 0));
	       $$->add (new TaQLIndexNodeRep(0, 0, 0));
	   }
         | COMMA subsrange {
	       $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
	       $$->setPPFix ("[", "]");
	       $$->add (new TaQLIndexNodeRep(0, 0, 0));
	       $$->add (*$2);
	   }
         | subsingle {
	       $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
	       $$->setPPFix ("[", "]");
	       $$->add (*$1);
	   }
         ;

subsingle: orexpr {
	       $$ = new TaQLNode(
                    new TaQLIndexNodeRep (*$1, 0, 0));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | colonrangeindex {
               $$ = $1;
	   }
         ;

subsrange: arithexpr {
	       $$ = new TaQLNode(
                    new TaQLIndexNodeRep (*$1, 0, 0));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | colonrangeindex {
               $$ = $1;
	   }
         ;

colonrangeinterval: colonrange {
               $$ = $1;
           }
         | arithexpr COLON {
	       $$ = new TaQLNode(
                    new TaQLIndexNodeRep (*$1, 0, 0));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | arithexpr COLON COLON {
	       $$ = new TaQLNode(
                    new TaQLIndexNodeRep (*$1, 0, 0));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | arithexpr COLON COLON arithexpr {
	       $$ = new TaQLNode(
                    new TaQLIndexNodeRep (*$1, 0, *$4));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }

colonrangeindex: colonrange {
               $$ = $1;
           }
         | arithexpr COLON {
	       $$ = new TaQLNode (new TaQLIndexNodeRep
                    (*$1, TaQLConstNode(new TaQLConstNodeRep(Int64(Slicer::MimicSource))), 0));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | arithexpr COLON COLON {
	       $$ = new TaQLNode (new TaQLIndexNodeRep
                    (*$1, TaQLConstNode(new TaQLConstNodeRep(Int64(Slicer::MimicSource))), 0));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | arithexpr COLON COLON arithexpr {
	       $$ = new TaQLNode (new TaQLIndexNodeRep
                    (*$1, TaQLConstNode(new TaQLConstNodeRep(Int64(Slicer::MimicSource))), *$4));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }

colonrange: arithexpr COLON arithexpr {
	       $$ = new TaQLNode(
                    new TaQLIndexNodeRep (*$1, *$3, 0));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | arithexpr COLON arithexpr COLON arithexpr {
	       $$ = new TaQLNode(
                    new TaQLIndexNodeRep (*$1, *$3, *$5));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | COLON arithexpr {
	       $$ = new TaQLNode(
                    new TaQLIndexNodeRep (0, *$2, 0));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | COLON arithexpr COLON arithexpr {
	       $$ = new TaQLNode(
                    new TaQLIndexNodeRep (0, *$2, *$4));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | COLON COLON arithexpr {
	       $$ = new TaQLNode(
                    new TaQLIndexNodeRep (0, 0, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

sortlist : sortlist COMMA sortexpr {
               $$ = $1;
               $$->add (*$3);
	   }
         | sortexpr {
	       $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
               $$->add (*$1);
	   }
         ;

sortexpr : orexpr {
	       $$ = new TaQLNode(
                    new TaQLSortKeyNodeRep (TaQLSortKeyNodeRep::None, *$1));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | orexpr SORTASC {
	       $$ = new TaQLNode(
                    new TaQLSortKeyNodeRep (TaQLSortKeyNodeRep::Ascending, *$1));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | orexpr SORTDESC {
	       $$ = new TaQLNode(
                    new TaQLSortKeyNodeRep (TaQLSortKeyNodeRep::Descending, *$1));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

dmlist:   dmelem {
               $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
               $$->add (*$1);
	   }
         | dmlist COMMA dmelem {
	       $$ = $1;
               $$->add (*$3);
	   }
         ;
           
dmelem:    LBRACKET recexpr RBRACKET {
	       $$ = new TaQLNode(
                    new TaQLRecFldNodeRep ("", *$2, ""));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         ;
           
recexpr:   recexpr COMMA recfield {
               $$->add (*$3);
	   }
         | recfield {
	       $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
	       $$->setPPFix ("[", "]");
               $$->add (*$1);
	   }
       ;

recfield:  srecfield {
               $$ = $1;
           }
         | rrecfield {
               $$ = $1;
           }
       ;

srecfield: NAME EQASS srecval {
               $$ = new TaQLNode(
                    new TaQLRecFldNodeRep ($1->getString(), *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
               delete $3;
           }
      ;

srecval:   orexpr {
               $$ = new TaQLRecFldNodeRep ("", *$1, "");
           }
         | orexpr AS NAME {     /* unit */
	       $$ = new TaQLRecFldNodeRep ("", *$1, $3->getString());
           }

rrecfield: NAME EQASS brackval {
	       $$ = new TaQLNode(
                    new TaQLRecFldNodeRep ($1->getString(), *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
               delete $3;
           }
       ;

%%
