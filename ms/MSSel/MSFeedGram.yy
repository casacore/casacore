/*-*- C++ -*-
    MSFeedGram.y: Parser for feed expressions based on antenna parser
    Copyright (C) 2015
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
           Internet email: casa-feedback@nrao.edu.
           Postal address: AIPS++ Project Office
                           National Radio Astronomy Observatory
                           520 Edgemont Road
                           Charlottesville, VA 22903-2475 USA
*/

%{
  using namespace casacore;
%}

%define api.pure                /* make parser re-entrant */

%union {
  const TableExprNode* node;
  char* str;
  Vector<Int>* iv;
  std::vector<String>* sv;
}


%token COMMA
%token SEMICOLON
%token AMPERSAND
%token DASH
%token NOT

%token <str> INT

%type <node> feedstatement
%type <node> indexcombexpr
%type <node> feedpairs
%type <node> gfeedpairs
%type <iv> feedlist
%type <iv> feedids


%{
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/ms/MSSel/MSSelectionTools.h>

  int MSFeedGramlex (YYSTYPE*);
  Bool MSFeedGramNegate=False;
%}

%%
feedstatement: indexcombexpr                
                   {
		     $$ = $1; 
		   }

indexcombexpr: gfeedpairs                         {$$=$1;}
             | indexcombexpr SEMICOLON gfeedpairs
                {
		  $$ = $1;
                }

gfeedpairs: NOT {MSFeedGramNegate=True;}  feedpairs {$$=$3;}
         |     {MSFeedGramNegate=False;} feedpairs {$$=$2;}

feedpairs: feedlist AMPERSAND feedlist  // Two non-identical lists for the '&' operator
           {
	     MSFeedIndex myMSFI(MSFeedParse::thisMSFParser->subTable());
	     Vector<Int> f1 = myMSFI.matchFeedId(*($1)); 
	     Vector<Int> f2 = myMSFI.matchFeedId(*($3)); 
	     $$ = MSFeedParse::thisMSFParser->selectFeedIds
	       (f1,f2, MSFeedParse::CrossOnly, MSFeedGramNegate); 
	     delete $1;
	     delete $3;
	   } 
        | feedlist AMPERSAND  // Implicit same list on the RHS of '&' operator
           {
		 MSFeedIndex myMSFI(MSFeedParse::thisMSFParser->subTable());
		 Vector<Int> f1 = myMSFI.matchFeedId(*($1)); 
		 $$ = MSFeedParse::thisMSFParser->selectFeedIds
		   (f1,f1, MSFeedParse::CrossOnly, MSFeedGramNegate); 
	     delete $1;
	   }
        | feedlist           //Match FEEDLIST & ALLFEEDS (implicit "&*")
           {
	     MSFeedIndex myMSFI(MSFeedParse::thisMSFParser->subTable());
	     Vector<Int> f1 = myMSFI.matchFeedId(*($1)); 
	     $$ = MSFeedParse::thisMSFParser->selectFeedIds
	       (f1, MSFeedParse::CrossOnly, MSFeedGramNegate); 
	     delete $1;
	   }
        | feedlist AMPERSAND AMPERSAND feedlist //Include self-correlations
           {
	     MSFeedIndex myMSFI(MSFeedParse::thisMSFParser->subTable());
	     Vector<Int> f1 = myMSFI.matchFeedId(*($1)); 
	     Vector<Int> f2 = myMSFI.matchFeedId(*($4)); 
	     $$ = MSFeedParse::thisMSFParser->selectFeedIds
	       (f1, f2, MSFeedParse::AutoCorrAlso, MSFeedGramNegate); 
	     delete $1;
	     delete $4;
	   } 
        | feedlist AMPERSAND AMPERSAND // Include self-correlations 
           {
	     MSFeedIndex myMSFI(MSFeedParse::thisMSFParser->subTable());
	     Vector<Int> f1 = myMSFI.matchFeedId(*($1)); 
	     $$ = MSFeedParse::thisMSFParser->selectFeedIds
	       (f1, f1, MSFeedParse::AutoCorrAlso, MSFeedGramNegate); 
	     delete $1;
	   }
        | feedlist AMPERSAND AMPERSAND AMPERSAND // Only self-correlations
           {
	     MSFeedIndex myMSFI(MSFeedParse::thisMSFParser->subTable());
	     Vector<Int> f1 = myMSFI.matchFeedId(*($1)); 
	     $$ = MSFeedParse::thisMSFParser->selectFeedIds
	       (f1, MSFeedParse::AutoCorrOnly, MSFeedGramNegate); 
	     delete $1;
	   }


feedlist: feedids  // single feed or range
          {
	    $$ = new Vector<Int>(*$1);
	    delete $1;
	  }
       | feedlist COMMA feedids  // feedID, feedID,...
          {
            $$ = $1;
	    Int N0=(*($1)).nelements(), N1 = (*($3)).nelements();
	    (*($$)).resize(N0+N1,True);  // Resize the existing list
	    for(Int i=N0;i<N0+N1;i++) (*($$))(i) = (*($3))(i-N0);
	    delete $3;
	  }

feedids: INT // A single feed index
             {
	       $$ = new Vector<Int>(1);
	       (*($$))(0) = atoi($1);
	       free($1);
	     }
           | INT DASH INT // A range of integer feed indices feedID~feedID
              {
		Int start = atoi($1);
		Int end   = atoi($3);
		Int len = end - start + 1;
		Vector<Int> feedids(len);
		for(Int i = 0; i < len; i++) feedids[i] = start + i;

		$$ = new Vector<Int>(len);

		for (Int i=0; i<len; i++) 
		  {
		    ((*$$))[i] = feedids[i];
		  }
		free($1);
		free($3);
	      }

%%
