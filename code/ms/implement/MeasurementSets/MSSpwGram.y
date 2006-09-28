/*
    MSSpwGram.y: Parser for field expressions
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
  Float fval[2];
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

%token <str> UNIT
%token <str> INT
%token <str> FNUMBER
%token <str> QSTRING
%token <str> REGEX

%token COLON
%token SEMICOLON

%type <node> spwstatement
%type <node> indexcombexpr
%type <iv> indexlist
%type <iv> spwidrange
%type <iv> spwidlist
%type <iv> spwid
%type <iv> spwidbounds
%type <iv> spwfreq
%type <fval> freqrange
%type <fval> freq

%nonassoc GT GE LT LE NE COMMA DASH AMPERSAND
%right TILDA
%{
  #include <values.h>
  int MSSpwGramlex (YYSTYPE*);
  void checkSpwError(Vector<Int>& list, ostringstream& msg)
  {
    if (list.nelements() == 0)
      {
	String errorMesg;
	ostringstream Mesg;
	Mesg << "Spw Expression: " << msg.str().c_str();
	
	errorMesg = String(Mesg.str().c_str());
	throw(MSSelectionSpwParseError(errorMesg));
      }
  }
%}

%%
spwstatement: indexcombexpr 
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
indexcombexpr  : indexlist 
                 {
                   $$ = MSSpwParse().selectSpwIdsFromIDList(*($1));
		   delete $1;
                 }
	       ;
//
// A single spw name (this could be a regex and
// hence produce a list inf indices)
//
spwid: IDENTIFIER   
          { //
	    // Use the string as-is.  This cannot include patterns/regex
	    // which has characters that are part of range or list
	    // syntax (',', '-') (that's all I think).
	    //
	    // Convert name to index
	    //
	    MSSpwIndex myMSSI(MSSpwParse::thisMSSParser->ms()->spectralWindow());
	    if (!($$)) delete $$;
	    $$=new Vector<Int>(myMSSI.matchName($1));

	    ostringstream m; m << "No match found for \"" << $1 << "\"";
	    checkSpwError(*($$), m);
	    
	    free($1);
	}
       | QSTRING 
        { //
	  // Quoted string: This is a pattern which will be converted
	  // to regex internally.  E.g. "LBAN*" becomes
	  // "LBAN.*" regex.  This can include any character
	  // string.
	  //
	  // Convert name to index
	  //
	  MSSpwIndex myMSSI(MSSpwParse::thisMSSParser->ms()->spectralWindow());
	  if (!$$) delete $$;
	  $$ = new Vector<Int>(myMSSI.matchRegexOrPattern($1));

	  ostringstream m; m << "No match found for \"" << $1 << "\"";
	  checkSpwError(*($$), m);

	  free($1);
	}
       | REGEX
        { //
	  // A string delimited by a pair of '/': This will be treated
	  // as a regular expression internally.
	  //
	  // Convert name to index
	  //
	  MSSpwIndex myMSSI(MSSpwParse::thisMSSParser->ms()->spectralWindow());
	  if (!$$) delete $$;

	  $$ = new Vector<Int>(myMSSI.matchRegexOrPattern($1,True));

	  ostringstream m; m << "No match found for \"" << $1 << "\"";
	  checkSpwError(*($$), m);

	  free($1);
	}
       ;

spwidrange: FNUMBER // A single spw index
            {
	      if (!($$)) delete $$;
	      $$ = new Vector<Int>(1);
	      (*($$))(0) = atoi($1);
	      free($1);
	    }
           | FNUMBER DASH FNUMBER // A range of integer spw indices
            {
              Int start = atoi($1);
              Int end   = atoi($3);
              Int len = end - start + 1;
	      if (len < 0) 
		throw(MSSelectionSpwParseError(String("Spw expression: Start of "
						      "range greater than end of "
						      "the range")));
              Vector<Int> spwids(len);
              for(Int i = 0; i < len; i++) {
                spwids[i] = start + i;
              }
	      if (!($$)) delete $$;
              $$ = new Vector<Int>(spwids);	   

	      free($1); free($3);
            }
          ;

freq: FNUMBER UNIT
            {
	      MSSpwIndex myMSSI(MSSpwParse::thisMSSParser->ms()->spectralWindow());
	      String unit($2);
	      Float f0,f1;
	      f0=atof($1);
	      f1 = -1;
	      Vector<Float> Freq = myMSSI.convertToHz(f0,f1,unit);
	      Freq(1) = -MAXFLOAT;
	      delete $1;delete $2;
	      $$[0] = Freq(0);
	      $$[1] = Freq(1);
	    }
         ;
freqrange: FNUMBER DASH FNUMBER UNIT
            {
	      MSSpwIndex myMSSI(MSSpwParse::thisMSSParser->ms()->spectralWindow());
	      String unit($4);
	      Float f0,f1;
	      f0=atof($1);
	      f1=atof($3);

	      delete $1;delete $3;delete $4;
	      if (f0 > f1)
		throw(MSSelectionSpwParseError(String("Spw expression: Start of "
						      "range greater than end of "
						      "the range")));
	      Vector<Float> Freq = myMSSI.convertToHz(f0,f1,unit);
	      $$[0] = Freq[0];
	      $$[1] = Freq[1];
	    }
         ;
spwfreq: freq
             {
	       MSSpwIndex myMSSI(MSSpwParse::thisMSSParser->ms()->spectralWindow());
	       if (!($$)) delete $$;
	       $$ = new Vector<Int>(myMSSI.matchFrequencyRange($1[0],$1[1],False));
	     }
          | TILDA freq 
             {
	       MSSpwIndex myMSSI(MSSpwParse::thisMSSParser->ms()->spectralWindow());
	       if (!($$)) delete $$;
	       $$ = new Vector<Int>(myMSSI.matchFrequencyRange($2[0],$2[1],True));
	     }
         | freqrange
            {
	      MSSpwIndex myMSSI(MSSpwParse::thisMSSParser->ms()->spectralWindow());;
	      if (!($$)) delete $$;
	      $$ = new Vector<Int>(myMSSI.matchFrequencyRange($1[0],$1[1],False));
	    }
         ;
spwidbounds: LT FNUMBER // <ID
                {
		  MSSpwIndex myMSSI(MSSpwParse::thisMSSParser->ms()->spectralWindow());
		  if (!($$)) delete $$;
		  Int n=atoi($2);
		  $$ = new Vector<Int>(myMSSI.matchIDLT(n));

		  ostringstream m; m << "No spw ID found <=" << n;
		  checkSpwError(*($$), m);

		  free($2);
		}
              | GT FNUMBER // >ID
                {
		  MSSpwIndex myMSSI(MSSpwParse::thisMSSParser->ms()->spectralWindow());
		  if (!($$)) delete $$;
		  Int n=atoi($2);
		  $$ = new Vector<Int>(myMSSI.matchIDGT(n));

		  ostringstream m; m << "No spw ID found >= " << n;
		  checkSpwError(*($$), m);

		  free($2);
		}
              | GT FNUMBER AMPERSAND LT FNUMBER // >ID & <ID
                {
		  MSSpwIndex myMSSI(MSSpwParse::thisMSSParser->ms()->spectralWindow());
		  if (!($$)) delete $$;
		  Int n0=atoi($2), n1=atoi($5);
		  $$ = new Vector<Int>(myMSSI.matchIDGTAndLT(n0,n1));

		  ostringstream m; 
		  m << "No spw found in the range [" << n0 << "," << n1 << "]";
		  checkSpwError(*($$), m);

		  free($2); free($5);
		}
             ;
spwidlist: spwid // A singe spw ID
            {
	      $$ = $1;
            }
          | spwidrange // ID range ( n0-n1 )
            {
	      $$ = $1;
	    }
          | spwidbounds  // >ID, <ID, >ID & <ID
            {
	      $$ = $1;
            }
          | spwfreq
            {
	      $$ = $1;
	    }
          ;
indexlist : spwidlist
            {
	      if (!($$)) delete $$;
	      $$ = new Vector<Int>(*$1);
	      delete $1;
	    }
          | indexlist COMMA spwidlist  
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

