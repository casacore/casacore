/*
    MSAntennaGram.y: Parser for antenna expressions
    Copyright (C) 2004
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
  using namespace casa;
%}

%pure_parser                /* make parser re-entrant */

%union {
  const TableExprNode* node;
  Block<TableExprNode>* exprb;
  TableExprNodeSetElem* elem;
  TableExprNodeSet* settp;
  Int ival[2];
  char * str;
  Double dval;
  Vector<Int>* iv;
  Vector<String>* is;
}


%token EQASS
%token SQUOTE
%token <str> IDENTIFIER
%token COMMA

%token LBRACKET
%token LPAREN
%token RBRACKET
%token RPAREN
%token LBRACE
%token RBRACE
%token WHITE

%token <str> INT
%token <str> QSTRING
%token <str> REGEX

%token COLON
%token SEMICOLON

%type <node> antennastatement
%type <node> indexcombexpr
%type <iv> indexlist
%type <iv> antidrange
%type <iv> antidlist
%type <iv> antid

%left OR AND
%nonassoc EQ EQASS GT GE LT LE NE COMMA DASH AMPERSAND
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%nonassoc UNARY
%nonassoc NOT
%right POWER

%{
  int MSAntennaGramlex (YYSTYPE*);
  void reportError(char *str)
  {
    String errorMesg;
    ostringstream Mesg;
    Mesg << "Antenna Expression: No match found for \"" << str << "\"";
    errorMesg = String(Mesg.str().c_str());
    throw(MSSelectionAntennaParseError(errorMesg));
  }
%}

%%
antennastatement: indexcombexpr 
                  {
                    $$ = $1;
                  }
                 | indexcombexpr SEMICOLON indexcombexpr
                  { 
		    $$ = $1;
                  }
                 | LPAREN indexcombexpr RPAREN //Parenthesis are not
					       //syntactically useful
					       //here
                  {
		    $$ = $2;
		  }
                ;
indexcombexpr  : IDENTIFIER COLON IDENTIFIER 
                 {
                   $$ = MSAntennaParse().selectFromIdsAndCPs(atoi($1), String($3));
	         }
	       | indexlist AMPERSAND indexlist 
                 {
                   $$ = MSAntennaParse().selectAntennaIds(*($1), *($3));
		   cout << "ID & ID = " << *($1) << "&" << *($3) << endl;
		   delete $1; delete $3;
                 } 
               | indexlist AMPERSAND 
                 {
                   $$ = MSAntennaParse().selectAntennaIds(*($1), *($1));
		   cout << "ID & =" << *($1) << endl;
		   delete $1;
   	         }
               | indexlist 
                 {
                   $$ = MSAntennaParse().selectAntennaIds(*($1));
		   cout << "ID = " << *($1) << endl;
		   delete $1;
                 }
	       ;

antid: IDENTIFIER  // A single antenna name (this could be a regex and
		   // hence produce a list inf indices)
                   // 
        { // Use the string as-is.  This cannot include patterns/regex
	  // which has characters that are part of range or list
	  // syntax (',', '-') (that's all I think).
	  //
	  // Convert name to index
	  //
	  MSAntennaIndex myMSAI(MSAntennaParse::thisMSAParser->ms()->antenna());
	  if (!($$)) delete $$;
	  $$=new Vector<Int>(myMSAI.matchAntennaName($1));
	  //$$=new Vector<Int>(myMSAI.matchAntennaRegexOrPattern($1));
	  if ((*($$)).nelements() == 0) reportError($1);
	  free($1);
	}
       | QSTRING 
        { // Quoted string: This is a pattern which will be converted
	  // to regex internally.  E.g. "VLA{20,21}*" becomes
	  // "VLA((20)|(21)).*" regex.  This can include any character
	  // string.
	  //
	  // Convert name to index
	  //
	  MSAntennaIndex myMSAI(MSAntennaParse::thisMSAParser->ms()->antenna());
	  if (!$$) delete $$;
	  $$ = new Vector<Int>(myMSAI.matchAntennaRegexOrPattern($1));
	  if ((*($$)).nelements() == 0) reportError($1);
	  free($1);
	}
       | REGEX
        { // A string delimited by a pair of '/': This will be treated
	  // as a regular expression internally.
	  //
	  // Convert name to index
	  //
	  MSAntennaIndex myMSAI(MSAntennaParse::thisMSAParser->ms()->antenna());
	  if (!$$) delete $$;

	  $$ = new Vector<Int>(myMSAI.matchAntennaRegexOrPattern($1,True));
	  if ((*($$)).nelements() == 0) reportError($1);

	  free($1);
	}
       ;

antidrange: INT // A single antenna index
            {
	      if (!($$)) delete $$;
	      $$ = new Vector<Int>(1);
	      (*($$))(0) = atoi($1);
	      free($1);
	    }
           | INT DASH INT // A range of integer antenna indices
            {
              Int start = atoi($1);
              Int end   = atoi($3);
              Int len = end - start + 1;
              Vector<Int> antennaids(len);
              for(Int i = 0; i < len; i++) {
                antennaids[i] = start + i;
              }
	      if (!($$)) delete $$;
              $$ = new Vector<Int>(antennaids);	   
	      free($1); free($3);
            }
          ;

antidlist: antid // A singe antenna ID
            {
	      $$ = $1;
            }
          | antidrange
            {
	      $$ = $1;
	    }
          ;
indexlist : antidlist
            {
	      if (!($$)) delete $$;
	      $$ = new Vector<Int>(*$1);
	      delete $1;
	    }
          | indexlist COMMA antidlist  // AntnnaID, AntnnaID,...
            {
	      Int N0=(*($1)).nelements(), 
		N1 = (*($3)).nelements();
	      (*($$)).resize(N0+N1,True);  // Resize the existing list
	      for(Int i=N0;i<N0+N1;i++)
		(*($$))(i) = (*($3))(i-N0);
	      delete $3;
            }
          | LPAREN indexlist RPAREN //Parenthesis are not
				    //syntactically useful here
            {
	      $$ = $2;
	    }
          ;
%%

