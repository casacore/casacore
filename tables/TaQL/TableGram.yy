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
Expect them, so bison does not generate an error message.
*/
%expect 2

/* Define the terminals (tokens returned by flex), if needed with their type */
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
%token WITH
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
%token ILIKE
%token LPAREN
%token RPAREN
%token COMMA
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token COLON
%token SEMICOL
%token OPENOPEN
%token OPENCLOSED
%token CLOSEDOPEN
%token CLOSEDCLOSED
%token OPENEMPTY
%token EMPTYOPEN
%token CLOSEDEMPTY
%token EMPTYCLOSED

/* Define all non-terminals with their '$$ return type' */
%type <val> literal
%type <val> asdtype
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
%type <node> selcol
%type <node> normcol
%type <nodelist> withpart
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
%type <nodelist> tabnmopts
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
%type <nodelist> updlist
%type <node> updexpr
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

/*
Define the possible $$ value types.
Alas you cannot use objects in a union, so pointers have to be used.
They are not deleted automatically. Hence a vector (in TaQLNode) is used to keep
track of the nodes created. They are deleted at the end of the parsing,
also in case everything goes well since copies are used in the parse tree.
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
Bool theFromQueryDone;           /* for flex to know how to handle a , */
} //# NAMESPACE CASACORE - END
int TableGramlex (YYSTYPE*);
%}


/*
Now define the parser by defining all non-terminal rules.
The code belonging to a rule is executed if the entire rule is reconized
by bison. The code builds a parse tree using the classes defined in 
TaQLNodeDer.h. That tree is traversed and execuited by TaQLNodeHandler.
Note that $$ is the 'return value'. $1, $2, etc. are the rule arguments.
*/
%%
/* A command can optionally be ended with a semicolon */
topcomm:   topcomm1
         | topcomm1 SEMICOL
         ;

/* A command can be preceeded by the TIME keyword and style arguments */
topcomm1:  command
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

/* Multiple STYLE commands can be given */
stylecoms: stylecoms stylecomm
         | stylecomm
         ;

stylecomm: STYLE stylelist
         ;

/* A style can consist of multiple keywords and UDFLIB synonyms */
stylelist: stylelist COMMA NAME
             { TaQLNode::theirStyle.set ($3->getString()); }
         | NAME
             { TaQLNode::theirStyle.set ($1->getString()); }
         | stylelist COMMA UDFLIBSYN
             { TaQLNode::theirStyle.defineSynonym ($3->getString()); }
         | UDFLIBSYN
             { TaQLNode::theirStyle.defineSynonym ($1->getString()); }
         ;

/* The possible TaQL commands; nestedcomm can be used in a nested FROM */
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

/* The SHOW (or HELP) can have a list of names */
showcomm:  SHOW showlist {
               $$ = new TaQLNode(new TaQLShowNodeRep (*$2));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

showlist:  {   /* no list */
               $$ = new TaQLMultiNode();
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | showflds
             { $$ = $1; }
         ;

/* This is the standard Bison way to define a list.
   First the addition to a list; secondly the initial list.
 */
showflds:  showflds tabname {
               $$ = $1;
               $$->add (*$2);
           }
         | tabname {
	       $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
               $$->setSeparator (" ");
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

/* A nested FROM command is a subquery or one of the other commands
   enclosed in parentheses or square brackets */
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

/* A subquery must be enclosed in parentheses or square brackets */
subquery:  LPAREN selcomm RPAREN {
               $$ = $2;
	       $$->setBrackets();
	   }
         | LBRACKET selcomm RBRACKET {
               $$ = $2;
	       $$->setBrackets();
	   }
         ;

/* WITH table-list is optional */
withpart:  /* no WITH part */
           { $$ = new TaQLMultiNode(); }
         | WITH tables
           { $$ = $2; }
         ;

/* The SELECT command; note that many parts are optional which is handled
   in the rule of that part. The FROM part being optional is handled here. */
selcomm:   withpart SELECT selcol FROM tables whexpr groupby having order limitoff given dminfo {
               $$ = new TaQLQueryNode(
                    new TaQLSelectNodeRep (*$3, *$1, *$5, 0, *$6, *$7, *$8,
					   *$9, *$10, *$11, *$12));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | withpart SELECT selcol into FROM tables whexpr groupby having order limitoff dminfo {
               $$ = new TaQLQueryNode(
		    new TaQLSelectNodeRep (*$3, *$1, *$6, 0, *$7, *$8, *$9,
					   *$10, *$11, *$4, *$12));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | withpart SELECT selcol whexpr groupby having order limitoff given dminfo {
               $$ = new TaQLQueryNode(
                    new TaQLSelectNodeRep (*$3, *$1, *$4, *$5, *$6,
					   *$7, *$8, *$9, *$10));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | withpart SELECT selcol into whexpr groupby having order limitoff dminfo {
               $$ = new TaQLQueryNode(
		    new TaQLSelectNodeRep (*$3, *$1, *$5, *$6, *$7,
					   *$8, *$9, *$4, *$10));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

/* The column list can be preceeded by ALL or DISTINCT */
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
         ;

/* The COUNT command */
countcomm: withpart COUNT normcol FROM tables whexpr {
	       $$ = new TaQLQueryNode(
                    new TaQLCountNodeRep (*$1, *$3, *$5, *$6));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

/* The UPDATE command */
updcomm:   withpart UPDATE tables UPDSET updlist FROM tables whexpr order limitoff {
               $$ = new TaQLNode(
                    new TaQLUpdateNodeRep (*$1, *$3, *$5, *$7, *$8, *$9, *$10));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | withpart UPDATE tables UPDSET updlist whexpr order limitoff {
               $$ = new TaQLNode(
                    new TaQLUpdateNodeRep (*$1, *$3, *$5, 0, *$6, *$7, *$8));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

/* The list of columns to be updated with their value expressions */
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

/* An array column to be updated can have a range and mask.
   Furthermore, a value and mask column can be assigned (from a masked array) */
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
         | LPAREN NAME COMMA NAME RPAREN LBRACKET subscripts RBRACKET EQASS orexpr {
	       $$ = new TaQLNode(
                    new TaQLUpdExprNodeRep ($2->getString(),
                                            $4->getString(), *$7, *$10));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

/* INSERT can be used in multiple ways */
inscomm:   withpart INSERT INTO tables insclist selcomm {
               /* insert with SELECT command */
	       $6->setNoExecute();
	       $$ = new TaQLNode(
                    new TaQLInsertNodeRep (*$1, *$4, *$5, *$6, TaQLNode()));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | withpart INSERT INTO tables insclist insvalue {
               /* insert in SQL style */
	       $$ = new TaQLNode(
                    new TaQLInsertNodeRep (*$1, *$4, *$5, *$6, TaQLNode()));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | withpart INSERT LIMIT orexpr INTO tables insclist insvalue {
	       $$ = new TaQLNode(
                    new TaQLInsertNodeRep (*$1, *$6, *$7, *$8, *$4));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | withpart INSERT INTO tables insclist insvalue LIMIT orexpr {
	       $$ = new TaQLNode(
                    new TaQLInsertNodeRep (*$1, *$4, *$5, *$6, *$8));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | withpart INSERT INTO tables UPDSET updlist {
               /* insert in update style */
	       $$ = new TaQLNode(
                    new TaQLInsertNodeRep (*$1, *$4, *$6));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

/* The optional INSERT column list must be enclosed in parentheses or
   square brackets */
insclist:  {   /* no insert column-list */   
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

/* The value list for the columns to be inserted can have multiple values,
   one for each row to be added */
insparts:  insparts COMMA inspart {
               $$ = $1;
	       $$->add (*$3);
           }
           | inspart {
               $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
	       $$->add (*$1);
           }
         ;

/* Each row to be inserted has a list of values whose size should match
   the columns to be inserted */
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

/* The DELETE command */
delcomm:   withpart DELETE FROM tables whexpr order limitoff {
	       $$ = new TaQLNode(
                    new TaQLDeleteNodeRep (*$1, *$4, *$5, *$6, *$7));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

/* The CALC command can calculate a single expression */
calccomm:  withpart CALC FROM tables CALC orexpr {
	       $$ = new TaQLNode(
                    new TaQLCalcNodeRep (*$1, *$4, *$6,
                                         TaQLNode(), TaQLNode(), TaQLNode()));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | withpart CALC orexpr {
               TaQLMultiNode tabNode((TaQLMultiNodeRep*)0);
	       $$ = new TaQLNode(
               new TaQLCalcNodeRep (*$1, tabNode, *$3,
                                    TaQLNode(), TaQLNode(), TaQLNode()));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | withpart CALC orexpr FROM tables whexpr order limitoff {
	       $$ = new TaQLNode(
                    new TaQLCalcNodeRep (*$1, *$5, *$3, *$6, *$7, *$8));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

/* The CREATE TABLE command has a few flavours */
cretabcomm: withpart CREATETAB tabnmtyp colspecs nrowspec dminfo {
	       $$ = new TaQLQueryNode(
                    new TaQLCreTabNodeRep (*$1, *$3, *$4, *$5, *$6));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
          | withpart CREATETAB tabnmtyp LPAREN colspecs RPAREN nrowspec dminfo {
	       $$ = new TaQLQueryNode(
                    new TaQLCreTabNodeRep (*$1, *$3, *$5, *$7, *$8));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
          | withpart CREATETAB tabnmtyp LBRACKET colspecs RBRACKET nrowspec dminfo {
	       $$ = new TaQLQueryNode(
                    new TaQLCreTabNodeRep (*$1, *$3, *$5, *$7, *$8));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

/* The ALTER TABLE command */
alttabcomm: withpart ALTERTAB tabalias altlist {
               $$ = new TaQLQueryNode(
                    new TaQLAltTabNodeRep (*$1, *$3, TaQLMultiNode(), *$4));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
           | withpart ALTERTAB tabalias FROM tables altlist {
               $$ = new TaQLQueryNode(
                    new TaQLAltTabNodeRep (*$1, *$3, *$5, *$6));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

/* The ALTER TABLE commands consists of one or more subcommands */
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

/* The ALTER TABLE subcommands */
altcomm:   ADDCOL colspecl dminfo {
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

/* RENAME COLUMN subcommand can rename multiple columns */
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

/* DROP COLUMN subcommand can remove multiple columns */
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

/* RENAME COLUMN subcommand can rename multiple columns */
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

/* DROP KEYWORD subcommand can remove multiple keywords */
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

/* SET KEYWORD subcommand can set multiple keywords */
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
         ;

/* COPY KEYWORD subcommand can copy multiple keywords */
copykeys:  copykeys COMMA copykey {
               $$ = $1;
               $$->add (*$3);
           }
         | copykey {
               $$ = new TaQLMultiNode(False);
	       TaQLNode::theirNodesCreated.push_back ($$);
               $$->add (*$1);
           }
         ;

/* A copied keyword can get another data type */
copykey:   namefld EQASS namefld asdtype {
	       $$ = new TaQLNode(
                    new TaQLRecFldNodeRep($1->getString(),
                                          $3->getString(), $4->getString()));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

keyval:    srecval {
               $$ = $1;
           }
         | brackval {
               $$ = $1;
           }
         ;

/* Keyword values using square brackets */
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
               /* empty vector of the datatype given by NAME */
               $$ = new TaQLRecFldNodeRep ("", TaQLNode(), $4->getString());
           }
         ;

/* The DataManager info (used by various commands) */
dminfo:    {   /* no datamans */
               $$ = new TaQLMultiNode();
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | DMINFO dmlist {
               $$ = $2;
           }
         ;

/* A (non-empty) list of expressions */
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

/* There does not need to be a GROUPBY clause.
   GROUPBY ROLLUP is not implemented (yet) */
groupby:   {   /* no groupby */
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

/* There does not need to be a HAVING clause. */
having:    {   /* no having */
	       $$ = new TaQLNode();
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | HAVING orexpr {
               $$ = $2;
	   }
         ;

/* There does not need to be an ORDERBY clause.
   If there, the default sort order can be given first, which can
   be given per expression as well.
   ASCENDING (or DESCENDING) and DISTINCT can be given in either order.
*/
order:     {   /* no sort */
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

/* There does not need to be a LIMIT/OFFSET clause.
   LIMIT can be given as a start:end:step range in which case OFFSET
   cannot be given. Otherwise LIMIT and/or OFFSET take a single value
   and can be given in either order.
*/
limitoff:  {   /* no limit,offset */
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

/* The optional table name and options when creating a table */
tabnmtyp:  tabname {
	       $$ = new TaQLNode(
                    new TaQLGivingNodeRep ($1->getString(), TaQLMultiNode()));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | tabname AS tabnmopts {
	       $$ = new TaQLNode(
                    new TaQLGivingNodeRep ($1->getString(), *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | AS tabnmopts {
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
         | tabname LIKE tabname AS tabnmopts {
	       $$ = new TaQLNode(
                    new TaQLGivingNodeRep ($1->getString(), *$5));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | tabname FROM tabname {
	       $$ = new TaQLNode(
                    new TaQLGivingNodeRep ($1->getString(), TaQLMultiNode()));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | tabname FROM tabname AS tabnmopts {
	       $$ = new TaQLNode(
                    new TaQLGivingNodeRep ($1->getString(), *$5));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
*/
         ;

/* The table creation options specified as a bracketed keyword=value list */
tabnmopts: NAME {  /* PLAIN_BIG, etc. for backward compatibility */
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

/* The optional GIVING clause can result in a table or a set */
given:     {   /* no result */
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

/* If INTO instead of GIVING is used, the only result can be a table */
into:      INTO tabnmtyp {
               $$ = $2;
	   }
         ;

/* The optional list of columns in the SELECT clause */
columns:   {   /* no columns given (thus take all) */
               $$ = new TaQLMultiNode();
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | collist {
               $$ = $1;
           }
         ;

/* List of columns */
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

/* A selected column is an expression optionally followed by its name
   and possibly data type. To handle a masked array, two column names
   can be given (one for the value and one for the mask). Note that the
   data type applies to the value column (since the mask is Bool).
   It is also possible to use wildcards in the column list.
*/
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
         | orexpr AS NAME NAME {   /* name and data type */
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

/* A wildcard in a SELECT clause can be * or a regex.
   * means all columns of the input table. A regex (with includes ~ or !~)
   can be used to exclude (or include) columns.
*/
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

/* The column list for an INSERT command. Similar to the SELECT clause,
   two names can be given for a masked array.
   Note that a data type cannot be given, since the specified columns
   must exist, thus already have a data type.
*/
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

/* In CREATE TABLE the LIMIT clause can be used to specify #rows */
nrowspec:  {   /* no nrows given */
               $$ = new TaQLNode();
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | LIMIT orexpr {
               $$ = $2;
	   }
         ;

/* Optional column specifications can be given in CREATE TABLE */
colspecs:  {   /* no column specifications given */
               $$ = new TaQLMultiNode();
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | colspecl {
               $$ = $1;
           }
         ;

/* A non-empty list of column specifications (also for ADD COLUMN) */
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

/* The specification of a column consists of a name, data type and
   possibly a bracketed key=value list for properties such as NDIM, etc.
   A single property can be given as a non-bracketed key=value.
*/
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

/* A list of tables with optional aliases. */
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
   Note that the alias can be given with or without AS. It can also be
   given in a reversed way using IN (which is OQL syntax).
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
         | tfnamen NAME {                  /* table name and alias */
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

/* General table specification */
tfnamen:   tfname {
               $$ = $1;
           }
         | NAME {
	       $1->setIsTableName();
               $$ = $1;
           }
         ;

/* Slightly more specific table specification.
   The tabalias rule above needs a separate line for NAME, therefore this rule
   does not include it.
   The table can be a subquery, table name, or table concatenation.
   With table concatenation it is possible to specify the subtables to
   concatenate and the GIVING/INTO to make the concat table persistent.
*/
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

/* Subtable concatenation is optional */
concsub:   {    /* no SUBTABLES */
                $$ = new TaQLMultiNode();
                TaQLNode::theirNodesCreated.push_back ($$);
           }
         | SUBTABLES concslist {
                $$ = $2;
           }
         ;

/* A list of subtables to concatenate */
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

/* Concat table persistency using GIVING or INTO is optional */
concinto:  {   /* no GIVING */
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

/* A table name can contain various characters, possibly using a quoted literal */
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

/* A general table name also includes an aplhanumeric name */
tabname:   NAME {
	       $1->setIsTableName();
               $$ = $1;
           }
         | stabname {
               $$ = $1;
           }
         ;

/* A list of table names and possible aliases for concatenation */
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

/* WHERE is optional */
whexpr:    {   /* no selection */
	       $$ = new TaQLNode();
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | WHERE orexpr {
	       $$ = $2;
	   }
	 ;

/* Multiple ORs can be used (OR has lowest precedence) */
orexpr:    andexpr {
	       $$ = $1;
           }
	 | orexpr OR andexpr {
	       $$ = new TaQLNode(
	            new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_OR, *$1, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         ;

/* Multiple ANDs can be used (AND has higher precedence) */
andexpr:   relexpr {
	       $$ = $1;
           }
         | andexpr AND relexpr {
	       $$ = new TaQLNode(
	            new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_AND, *$1, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         ;

/* All possible logical expressions */
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
         | arithexpr EQNEAR arithexpr {     /* ~= means function NEAR */
   	       TaQLMultiNode set(False);
               set.add (*$1);
               set.add (*$3);
               set.add (TaQLConstNode(new TaQLConstNodeRep(1e-5)));
               $$ = new TaQLNode (new TaQLFuncNodeRep("NEAR", set));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | arithexpr NENEAR arithexpr {     /* !~= means NOT function NEAR */
   	       TaQLMultiNode set(False);
               set.add (*$1);
               set.add (*$3);
               set.add (TaQLConstNode(new TaQLConstNodeRep(1e-5)));
               TaQLNode ref (new TaQLFuncNodeRep("NEAR", set));
	       $$ = new TaQLNode(
                    new TaQLUnaryNodeRep (TaQLUnaryNodeRep::U_NOT, ref));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | arithexpr REGEX {     /* REGEX also contains operator ~ or !~ */
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
         | arithexpr ILIKE arithexpr {     /* case-insensitive LIKE */
   	       TaQLMultiNode mn1(False);
               mn1.add (*$1);
               TaQLNode tn1 (new TaQLFuncNodeRep("LOWER", mn1));
   	       TaQLMultiNode mn2(False);
               mn2.add (*$3);
               TaQLNode tn2 (new TaQLFuncNodeRep("LOWER", mn2));
   	       TaQLMultiNode re(False);
               re.add (tn2);
               TaQLNode ref (new TaQLFuncNodeRep("SQLPATTERN", re));
	       $$ = new TaQLNode(
	            new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_EQ, tn1, ref));
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
         | arithexpr NOT ILIKE arithexpr {
   	       TaQLMultiNode mn1(False);
               mn1.add (*$1);
               TaQLNode tn1 (new TaQLFuncNodeRep("LOWER", mn1));
   	       TaQLMultiNode mn2(False);
               mn2.add (*$4);
               TaQLNode tn2 (new TaQLFuncNodeRep("LOWER", mn2));
   	       TaQLMultiNode re(False);
               re.add (tn2);
               TaQLNode ref (new TaQLFuncNodeRep("SQLPATTERN", re));
	       $$ = new TaQLNode(
	            new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_NE, tn1, ref));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | EXISTS subquery {     /* is subquery result non-empty */
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

/* All possible numeric expressions */
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

/* A (sub)expression can be followed by a unit */
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
         | inxexpr LBRACKET subscripts RBRACKET {
	       $$ = new TaQLNode(
	            new TaQLBinaryNodeRep (TaQLBinaryNodeRep::B_INDEX, *$1, *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         ;

/* A subexpression, function, column, literal or set.
   Note that the COUNT function has a separate line because COUNT is also a keyword.
   COUNTALL has a somewhat special syntax which is recognized in flex.
*/
simbexpr:  LPAREN orexpr RPAREN     /* subexpression in parentheses */
               { $$ = $2; }
         | namefld LPAREN elemlist RPAREN {     /* function */
	       $$ = new TaQLNode(
                    new TaQLFuncNodeRep ($1->getString(), *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | COUNT LPAREN elemlist RPAREN {     /* COUNT function */
	       $$ = new TaQLNode(
                    new TaQLFuncNodeRep ("COUNT", *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | COUNTALL {
	       $$ = new TaQLNode(     /* COUNT(*) function */
                    new TaQLFuncNodeRep ("COUNTALL"));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         | namefld {
	       $$ = new TaQLNode(     /* column name */
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

/* Column name or keyword name (possibly with alias) */
namefld:   NAME {            /* simple name */
               $$ = $1;
           }
         | FLDNAME {         /* name with . or :: */
               $$ = $1;
           }
         ;

/* Simple unit or compund unit enclosed in quotes */
unit:      namefld {
               $$ = $1;
           }
         | STRINGLITERAL {   /* compound unit (with special characters) */
               $$ = $1;
           }
         ;

/* A numeric or boolean literal or a string literal (in quotes) */
literal:   LITERAL {
	       $$ = $1;
	   }
         | STRINGLITERAL {
	       $$ = $1;
	   }
         ;

/* A set is is a series of values enclosed in brackets or parentheses.
   It can also be the result of a subquery. */
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

/* A possibly empty list of values */
elemlist:  elems {
               $$ = $1;
	       $$->setPPFix("", "");
           }
         | {
               $$ = new TaQLMultiNode(False);       /* no elements */
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         ;

/* A non-empty list of values */
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

/* A value in a set can be an expression or a range specification */
elem:      orexpr {
               $$ = $1;
	   }
         | range {
               $$ = $1;
           }
         ;

/* A range in a set requires an extra MultiNode */
singlerange: range {
	       $$ = new TaQLMultiNode(True);
	       TaQLNode::theirNodesCreated.push_back ($$);
	       $$->add (*$1);
           }
         ;

/* A range can be a discrete strt:end:step range or a continuous interval.
   The latter can be specified in two ways: using angle brackets
   and braces or using the =:= notation (where = can also be <).
   Angle brackets indicate an open side, others a closed side.
   It is possible to leave out the start or end value (meaning - or +infinity).
*/
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

/* Array subscripts indicate a single value or a range of values per axis.
   An array subscript can be left out (indicating entire axis), but the
   comma has to be present.
   A single subscript can be a mask for a masked array. */
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

/* A single subscript can be a single element in a vector, but also an array
   giving a boolean mask. Hence it accepts an orexpr instead of arithexpr. */
subsingle: orexpr {
	       $$ = new TaQLNode(
                    new TaQLIndexNodeRep (*$1, 0, 0));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | colonrangeindex {
               $$ = $1;
	   }
         ;

/* An array axis subscript is a single value or a range */
subsrange: arithexpr {
	       $$ = new TaQLNode(
                    new TaQLIndexNodeRep (*$1, 0, 0));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | colonrangeindex {
               $$ = $1;
	   }
         ;

/* A range interval is a start:end:step specification where all parts are
   optional. An array index slice is similar, but requires a different
   representation of an unspecified end. Hence they share the range
   specifications except those with an unspefied end. */
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
         ;

/* An array axis slice is the colonrange below extended with the possibility.
   of an unspecified end which is represented as Slicer::MimicSource. */
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
         ;

/* Each part in a start:end:step range is optional as well as
   the last colon if end and/or step is not given. */
colonrange: arithexpr COLON arithexpr {
	       $$ = new TaQLNode(
                    new TaQLIndexNodeRep (*$1, *$3, 0));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | arithexpr COLON arithexpr COLON {
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
         | COLON arithexpr COLON {
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

/* A list of expressions to sort on (each with optional ASC or DESC) */
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

/* A sort expression can have ASCENDING or DESCENDING */
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

/* A list of DataManager specifications */
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

/* A DataManager specification is a record (list of key=value) */
dmelem:    LBRACKET recexpr RBRACKET {
	       $$ = new TaQLNode(
                    new TaQLRecFldNodeRep ("", *$2, ""));
	       TaQLNode::theirNodesCreated.push_back ($$);
	   }
         ;

/* A list of general record field definitions (key=value) */
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

/* A general key=value can have a simple or (nested) record value */
recfield:  srecfield {
               $$ = $1;
           }
         | rrecfield {
               $$ = $1;
           }
         ;

/* A simple key=value (having an optional data type specification) */
srecfield: NAME EQASS srecval {
               $$ = new TaQLNode(
                    new TaQLRecFldNodeRep ($1->getString(), *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
               delete $3;
           }
         ;

/* A record value has an optional data type */
srecval:   orexpr asdtype {
	       $$ = new TaQLRecFldNodeRep ("", *$1, $2->getString());
           }

/* A record value being a record in itself */
rrecfield: NAME EQASS brackval {
	       $$ = new TaQLNode(
                    new TaQLRecFldNodeRep ($1->getString(), *$3));
	       TaQLNode::theirNodesCreated.push_back ($$);
               delete $3;
           }
         ;

/* An optional data type */
asdtype:   {   /* no datatype */
               $$ = new TaQLConstNode(new TaQLConstNodeRep(String()));
	       TaQLNode::theirNodesCreated.push_back ($$);
           }
         | AS NAME {
               $$ = $2;
           }
         ;

%%
