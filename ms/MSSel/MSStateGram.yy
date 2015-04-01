/* -*- C++ -*-
    MSStateGram.y: Parser for field expressions
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
#include <errno.h>
  using namespace casacore;
%}

%pure-parser                /* make parser re-entrant */

%union {
  const TableExprNode* node;
  char * str;
  Vector<Int>* iv;
  // Block<TableExprNode>* exprb;
  // TableExprNodeSetElem* elem;
  // TableExprNodeSet* settp;
  // Int ival[2];
  // Double dval;
  // Vector<String>* is;
}


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

%type <node> statestatement
%type <node> indexcombexpr
%type <iv> indexlist
%type <iv> stateidrange
%type <iv> stateidlist
%type <iv> stateid
%type <iv> stateidbounds
%type <iv> logicallist

%nonassoc EQ EQASS GT GE LT LE NE COMMA DASH AMPERSAND

%{
#include <casacore/ms/MSSel/MSSelectionTools.h>
  int MSStateGramlex (YYSTYPE*);
  void checkStateError(Vector<Int>& list, ostringstream& msg, Bool force=False, char* = NULL)
  {
    if ((list.nelements() == 0) || force)
      {
	String errorMesg;
	ostringstream Mesg;
	Mesg << "State Expression: " << msg.str().c_str();
	
	errorMesg = String(Mesg.str().c_str());
	
	MSStateParse::thisMSSErrorHandler->reportError(NULL,Mesg.str());
	//throw(MSSelectionStateParseError(errorMesg));
      }
  }
%}

%%
statestatement: indexcombexpr 
                  {
                    $$ = $1;
                  }
                 | LPAREN indexcombexpr RPAREN //Parenthesis are syntactically 
		                               // not useful here
                  {
		    $$ = $2;
		  }
                ;
indexcombexpr  : indexlist 
                 {
		   ostringstream m;
	           MSStateIndex myMSSI(MSStateParse::thisMSSIParser->ms()->state());
		   Vector<Int> selectedIDs(myMSSI.maskStateIDs(*($1)));
		   if (selectedIDs.nelements() != set_intersection(selectedIDs,(*($1))).nelements())
		     {
		       m << "Possible out of range index in the list " << *($1)
			 << " [TIP: Double-quoted strings forces name matching]";
		       checkStateError(selectedIDs, m , True);
		     }
                   $$ = MSStateParse().selectStateIds(selectedIDs);
		   m << "Partial or no match for State ID list " << (*($1));
                   checkStateError(selectedIDs, m);

		   delete $1;
                 }
	       ;
//
// Ampersand separated list of stateid.  The result is the logical AND
// of list of indices in stateid.
//
logicallist: stateid AMPERSAND stateid
          {
	    if (!$$) delete $$;
	    $$ = new Vector<Int>(set_intersection(*$1,*$3));
	  };
        | logicallist AMPERSAND stateid
	  {
	    if (!$$) delete $$;
	    $$ = new Vector<Int>(set_intersection(*$1,*$3));
	  };
//
// A single state name (this could be a regex and
// hence produce a list of indices)
//
stateid: IDENTIFIER   
          { //
	    // Use the string as-is.  This cannot include patterns/regex
	    // which has characters that are part of range or list
	    // syntax (',', '-') (that's all I think).
	    //
	    // Convert name to index
	    //
	  MSStateIndex myMSSI(MSStateParse::thisMSSIParser->ms()->state());
	  if (!($$)) delete $$;
	  $$=new Vector<Int>(myMSSI.matchStateObsMode($1));
	  //$$=new Vector<Int>(myMSAI.matchStateRegexOrPattern($1));

	  ostringstream m; m << "No match found for \"" << $1 << "\"";
	  checkStateError(*($$), m);

	  free($1);
	}
       | QSTRING 
        { //
	  // Quoted string: This is a pattern which will be converted
	  // to regex internally.  E.g. "VLA{20,21}*" becomes
	  // "VLA((20)|(21)).*" regex.  This can include any character
	  // string.
	  //
	  // Convert name to index
	  //
	  MSStateIndex myMSSI(MSStateParse::thisMSSIParser->ms()->state());
	  if (!$$) delete $$;
	  $$ = new Vector<Int>(myMSSI.matchStateRegexOrPattern($1));

	  ostringstream m; m << "No match found for \"" << $1 << "\"";
	  checkStateError(*($$), m);
	  //	  String s(m.str());

	  free($1);
	}
       | REGEX
        { //
	  // A string delimited by a pair of '/': This will be treated
	  // as a regular expression internally.
	  //
	  // Convert name to index
	  //
	  MSStateIndex myMSSI(MSStateParse::thisMSSIParser->ms()->state());
	  if (!$$) delete $$;
	  $$ = new Vector<Int>(myMSSI.matchStateRegexOrPattern($1,True));

	  ostringstream m; m << "No match found for \"" << $1 << "\"";
	  checkStateError(*($$), m);

	  free($1);
	}
       ;

stateidrange: INT // A single state index
            {
	      if (!($$)) delete $$;
	      $$ = new Vector<Int>(1);
	      (*($$))(0) = atoi($1);
	      free($1);
	    }
           | INT DASH INT // A range of integer state indices
            {
              Int start = atoi($1);
              Int end   = atoi($3);
              Int len = end - start + 1;
              Vector<Int> stateids(len);
              for(Int i = 0; i < len; i++) {
                stateids[i] = start + i;
              }
	      if (!($$)) delete $$;
              $$ = new Vector<Int>(stateids);	   
	      free($1); free($3);
            }
          ;

stateidbounds: LT INT // <ID
                {
		  MSStateIndex myMSSI(MSStateParse::thisMSSIParser->ms()->state());
		  if (!($$)) delete $$;
		  Int n=atoi($2);
		  $$ = new Vector<Int>(myMSSI.matchStateIDLT(n));

		  ostringstream m; m << "No state ID found <" << n;
		  checkStateError(*($$), m);


		  free($2);
		}
              | GT INT // >ID
                {
		  MSStateIndex myMSSI(MSStateParse::thisMSSIParser->ms()->state());
		  if (!($$)) delete $$;
		  Int n=atoi($2);
		  $$ = new Vector<Int>(myMSSI.matchStateIDGT(n));

		  ostringstream m; m << "No state ID found >" << n;
		  checkStateError(*($$), m);

		  free($2);
		}
              | GT INT AMPERSAND LT INT // >ID & <ID
                {
		  MSStateIndex myMSSI(MSStateParse::thisMSSIParser->ms()->state());
		  if (!($$)) delete $$;
		  Int n0=atoi($2), n1=atoi($5);
		  $$ = new Vector<Int>(myMSSI.matchStateIDGTAndLT(n0,n1));

		  ostringstream m; 
		  m << "No state found in the range [" << n0 << "," << n1 << "]";
		  checkStateError(*($$), m);

		  free($2); free($5);
		}
             ;
stateidlist: stateid // A singe state ID
            {
	      $$ = $1;
            }
          | logicallist // Ampersand seperated stateid list
	    {
	      $$ = $1;
	    }
          | stateidrange // ID range ( n0-n1 )
            {
	      $$ = $1;
	    }
          | stateidbounds  // >ID, <ID, >ID & <ID
            {
	      $$ = $1;
            }
          ;
indexlist : stateidlist
            {
	      if (!($$)) delete $$;
	      $$ = new Vector<Int>(*$1);
	      delete $1;
	    }
          | indexlist COMMA stateidlist  
            {
	      Int N0=(*($1)).nelements(), 
		N1 = (*($3)).nelements();
	      (*($$)).resize(N0+N1,True);  // Resize the existing list
	      for(Int i=N0;i<N0+N1;i++)
		(*($$))(i) = (*($3))(i-N0);
	      delete $3;
            }
          ;
%%

