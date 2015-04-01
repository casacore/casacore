/*
    TableGram.y: Parser for table commands
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

/*
 The grammar has 1 shift/reduce conflict which is resolved in a correct way.
*/


%{
using namespace casacore;
%}

%pure-parser                /* make parser re-entrant */

%expect 1                   /* do not report 1 shift/reduce conflict */

%token STYLE
%token TIMING
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
%token FROM
%token WHERE
%token GROUPBY
%token GROUPROLL
%token HAVING
%token ORDERBY
%token NODUPL
%token GIVING
%token INTO
%token EXCEPT
%token SORTASC
%token SORTDESC
%token LIMIT
%token OFFSET
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
%type <nodename> unit
%type <node> tabalias
%type <node> tfnamen
%type <node> tfname
%type <nodeselect> selcomm
%type <nodeselect> countcomm
%type <node> updcomm
%type <node> inscomm
%type <node> delcomm
%type <node> calccomm
%type <node> cretabcomm
%type <nodeselect> subselcnt
%type <nodeselect> subquery
%type <nodeselect> selrow
%type <node> selcol
%type <node> normcol
%type <nodelist> tables
%type <node> whexpr
%type <node> groupby
%type <nodelist> exprlist
%type <node> having
%type <node> order
%type <node> limitoff
%type <node> given
%type <node> into
%type <node> colexpr
%type <node> wildcol
%type <node> colspec
%type <nodelist> columns
%type <nodelist> nmcolumns
%type <nodelist> colspecs
%type <node> updrow
%type <nodelist> updlist
%type <node> updexpr
%type <node> insrow
%type <nodelist> insclist
%type <node> inspart
%type <nodelist> insvlist
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
%type <node> subsrange
%type <node> colonrange
%type <node> colonrangeinterval
%type <node> colonrangeindex
%type <node> range
%type <node> sortexpr
%type <nodelist> sortlist
%type <nodelist> dminfo
%type <nodelist> reclist
%type <node> recelem
%type <nodelist> recexpr
%type <nodelist> recvalues
%type <node> recfield
%type <node> srecfield
%type <node> rrecfield

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
TaQLColNodeRep* nodecolrep;
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
         | countcomm
             { TaQLNode::theirNode = *$1; }
         | calccomm
             { TaQLNode::theirNode = *$1; }
         | cretabcomm
             { TaQLNode::theirNode = *$1; }
         ;

subselcnt: subquery {
               $$ = $1;
	   }
         | LPAREN countcomm RPAREN {
               $$ = $2;
	       $$->setBrackets();
	   }
         | LBRACKET countcomm RBRACKET {
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
         ;

selrow:    selcol FROM tables whexpr groupby having order limitoff given {
               $$ = new TaQLQueryNode(
                    new TaQLSelectNodeRep (*$1, *$3, 0, *$4, *$5, *$6,
					   *$7, *$8, *$9));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | selcol into FROM tables whexpr groupby having order limitoff {
               $$ = new TaQLQueryNode(
		    new TaQLSelectNodeRep (*$1, *$4, 0, *$5, *$6, *$7,
					   *$8, *$9, *$2));
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
                    new TaQLUpdExprNodeRep ($1->getString(), 0, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | NAME LBRACKET subscripts RBRACKET EQASS orexpr {
	       $$ = new TaQLNode(
                    new TaQLUpdExprNodeRep ($1->getString(), *$3, *$6));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

inscomm:   INSERT insrow {
               $$ = $2;
           }
         ;

insrow:    INTO tables insclist inspart {
	       $$ = new TaQLNode(
                    new TaQLInsertNodeRep (*$2, *$3, *$4));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | INTO tables UPDSET updlist {
	       $$ = new TaQLNode(
                    new TaQLInsertNodeRep (*$2, *$4));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

insclist:  {         /* no column-list */   
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

inspart:   VALUES LBRACKET insvlist RBRACKET {
               $$ = $3;
           }
         | VALUES LPAREN insvlist RPAREN {
               $$ = $3;
           }
         | selcomm {
	       $1->setNoExecute();
               $$ = $1;
	   }
         ;

insvlist:  insvlist COMMA orexpr {
               $$ = $1;
	       $$->add (*$3);
           }
         | orexpr {
               $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
	       $$->setPPFix ("VALUES [", "]");
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

cretabcomm: CREATETAB tabname colspecs dminfo {
	       $$ = new TaQLNode(
                    new TaQLCreTabNodeRep ($2->getString(), *$3, *$4));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
          | CREATETAB tabname LPAREN colspecs RPAREN dminfo {
	       $$ = new TaQLNode(
                    new TaQLCreTabNodeRep ($2->getString(), *$4, *$6));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
          | CREATETAB tabname LBRACKET colspecs RBRACKET dminfo {
	       $$ = new TaQLNode(
                    new TaQLCreTabNodeRep ($2->getString(), *$4, *$6));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

dminfo:    {      /* no datamans */
               $$ = new TaQLMultiNode();
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | DMINFO reclist {
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

given:     {          /* no result */
	       $$ = new TaQLNode();
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | GIVING tabname {
	       $$ = new TaQLNode(
                    new TaQLGivingNodeRep ($2->getString(), ""));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | GIVING tabname AS NAME {
	       $$ = new TaQLNode(
                    new TaQLGivingNodeRep ($2->getString(), $4->getString()));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | GIVING AS NAME {
	       $$ = new TaQLNode(
                    new TaQLGivingNodeRep ("", $3->getString()));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | GIVING LBRACKET elems RBRACKET {
	       $$ = new TaQLNode(
                    new TaQLGivingNodeRep (*$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         ;

into:      INTO tabname {
	       $$ = new TaQLNode(
                    new TaQLGivingNodeRep ($2->getString(), ""));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | INTO tabname AS NAME {
	       $$ = new TaQLNode(
                    new TaQLGivingNodeRep ($2->getString(), $4->getString()));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | INTO AS NAME {
	       $$ = new TaQLNode(
                    new TaQLGivingNodeRep ("", $3->getString()));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         ;

columns:   {          /* no column names given (thus take all) */
               $$ = new TaQLMultiNode();
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | colexpr {
               $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
               $$->add (*$1);
	   }
         | columns COMMA colexpr {
	       $$ = $1;
               $$->add (*$3);
	   }
         ;

colexpr:   orexpr {
	       $$ = new TaQLNode(
	            new TaQLColNodeRep (*$1, "", ""));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | orexpr AS NAME {
	       $$ = new TaQLNode(
	            new TaQLColNodeRep (*$1, $3->getString(), ""));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | orexpr AS NAME NAME {
	       $$ = new TaQLNode(
	            new TaQLColNodeRep (*$1, $3->getString(), $4->getString()));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
	 | wildcol {
               $$= $1;
           }
         ;

wildcol:   TIMES {          /* SELECT * FROM ... */
               TaQLRegexNode p (new TaQLRegexNodeRep ("~p/*/"));
               $$ = new TaQLNode (new TaQLColNodeRep (p, "", ""));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
	 | REGEX {
               $$ = new TaQLNode (new TaQLColNodeRep (*$1, "", ""));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

nmcolumns: NAME {
               $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
               $$->add (new TaQLKeyColNodeRep ($1->getString()));
	   }
         | nmcolumns COMMA NAME {
	       $$ = $1;
               $$->add (new TaQLKeyColNodeRep ($3->getString()));
	   }
         ;

colspecs:  {          /* no column specifications given */
               $$ = new TaQLMultiNode();
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | colspec {
               $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
               $$->add (*$1);
	   }
         | colspecs COMMA colspec {
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

tfname:    subselcnt {
	       theFromQueryDone = True;
	       $1->setFromExecute();
               $$ = $1;
           }
         | stabname {
	       $$ = $1;
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
         | NAME LPAREN elemlist RPAREN {
	       $$ = new TaQLNode(
                    new TaQLFuncNodeRep ($1->getString(), *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | FLDNAME LPAREN elemlist RPAREN {
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
         | NAME {
	       $$ = new TaQLNode(
                    new TaQLKeyColNodeRep ($1->getString()));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | FLDNAME {
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

unit:      NAME            /* simple unit */
           { $$ = $1; }
         | FLDNAME         /* unit with . */
           { $$ = $1; }
         | STRINGLITERAL   /* compound unit (with special characters) */
           { $$ = $1; }
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
         | subsrange {
	       $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
	       $$->setPPFix ("[", "]");
	       $$->add (*$1);
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
         |  arithexpr COLON {
	       $$ = new TaQLNode(
                    new TaQLIndexNodeRep (*$1, 0, 0));
	       TaQLNode::theirNodesCreated.push_back ($$);
            }
         |  arithexpr COLON COLON {
	       $$ = new TaQLNode(
                    new TaQLIndexNodeRep (*$1, 0, 0));
	       TaQLNode::theirNodesCreated.push_back ($$);
            }
         |  arithexpr COLON COLON arithexpr {
	       $$ = new TaQLNode(
                    new TaQLIndexNodeRep (*$1, 0, *$4));
	       TaQLNode::theirNodesCreated.push_back ($$);
            }

colonrangeindex: colonrange {
               $$ = $1;
            }
         |  arithexpr COLON {
	       $$ = new TaQLNode (new TaQLIndexNodeRep
                    (*$1, TaQLConstNode(new TaQLConstNodeRep(Int64(-1))), 0));
	       TaQLNode::theirNodesCreated.push_back ($$);
            }
         |  arithexpr COLON COLON {
	       $$ = new TaQLNode (new TaQLIndexNodeRep
                    (*$1, TaQLConstNode(new TaQLConstNodeRep(Int64(-1))), 0));
	       TaQLNode::theirNodesCreated.push_back ($$);
            }
         |  arithexpr COLON COLON arithexpr {
	       $$ = new TaQLNode (new TaQLIndexNodeRep
                    (*$1, TaQLConstNode(new TaQLConstNodeRep(Int64(-1))), *$4));
	       TaQLNode::theirNodesCreated.push_back ($$);
            }

colonrange: arithexpr COLON arithexpr {
	       $$ = new TaQLNode(
                    new TaQLIndexNodeRep (*$1, *$3, 0));
	       TaQLNode::theirNodesCreated.push_back ($$);
            }
         |  arithexpr COLON arithexpr COLON arithexpr {
	       $$ = new TaQLNode(
                    new TaQLIndexNodeRep (*$1, *$3, *$5));
	       TaQLNode::theirNodesCreated.push_back ($$);
            }
         |  COLON arithexpr {
	       $$ = new TaQLNode(
                    new TaQLIndexNodeRep (0, *$2, 0));
	       TaQLNode::theirNodesCreated.push_back ($$);
            }
         |  COLON arithexpr COLON arithexpr {
	       $$ = new TaQLNode(
                    new TaQLIndexNodeRep (0, *$2, *$4));
	       TaQLNode::theirNodesCreated.push_back ($$);
            }
         |  COLON COLON arithexpr {
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

reclist:  recelem {
               $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
               $$->add (*$1);
	   }
         | reclist COMMA recelem {
	       $$ = $1;
               $$->add (*$3);
	   }
         ;
           
recelem:   LBRACKET recexpr RBRACKET {
	       $$ = new TaQLNode(
                    new TaQLRecFldNodeRep ("", *$2));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         ;
           
recexpr:   recexpr COMMA recfield {
               $$->add (*$3);
	   }
       |   recfield {
	       $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
	       $$->setPPFix ("[", "]");
               $$->add (*$1);
	   }
       ;

recfield:  srecfield {
               $$ = $1;
           }
       |   rrecfield {
               $$ = $1;
           }
       |   NAME EQASS LBRACKET EQASS RBRACKET {
	       /* Like in glish [=] is the syntax for an empty 'record' */
	       $$ = new TaQLNode(
                    new TaQLRecFldNodeRep ($1->getString(), TaQLNode()));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
       ;

srecfield: NAME EQASS literal {
	       $$ = new TaQLNode(
                    new TaQLRecFldNodeRep ($1->getString(), *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
       |   NAME EQASS LBRACKET recvalues RBRACKET {
	       $$ = new TaQLNode(
                    new TaQLRecFldNodeRep ($1->getString(), *$4));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
       ;

rrecfield: NAME EQASS LBRACKET recexpr RBRACKET {
	       $$ = new TaQLNode(
                    new TaQLRecFldNodeRep ($1->getString(), *$4));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
       ;

recvalues: recvalues COMMA literal {
               $$->add (*$3);
	   }
       |   literal {
	       $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
	       $$->setPPFix ("[", "]");
               $$->add (*$1);
	   }
       |   {      /* empty vector */
	       $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
	       $$->setPPFix ("[", "]");
           }
       ;
%%
