/*
    MSSpwGram.y: Parser for Spw expressions
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
  Float fval2[2], fval4[4],fval;
  char * str;
  Int ival;
  Vector<Float>* fv;
  Vector<Int>* iv;
}


%token SQUOTE
%token <str> IDENTIFIER
%token COMMA

%token LPAREN
%token RPAREN
%token WHITE
%token COLON
%token CARET
%token SEMICOLON

%token <str> UNIT
%token <str> INT
%token <str> FNUMBER
%token <str> QSTRING
%token <str> REGEX

%type <node> SpwStatement
%type <node> FullSpec
%type <node> FullExpr
%type <fval2> OneFreq
%type <fval4> FreqRange
%type <fval2> Physical
%type <fval4> IndexRange
%type <fval4> PhyRange
%type <iv>    Spw
%type <fval4> FListElements
%type <fv>   FreqList
%type <fval> PhyVal
%type <fval2> UnitCode

%nonassoc GT GE LT LE NE COMMA DASH AMPERSAND SEMICOLON COLON CARET
%right TILDA
%{
  #include <limits.h>
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
SpwStatement: FullExpr
                  {
                    $$ = $1;
                  }
                 | LPAREN FullExpr RPAREN //Parenthesis are not
                                          //syntactically useful
                                          //here
                  {
		    $$ = $2;
		  }
                ;

PhyVal: FNUMBER
           { 
	     Float f;
	     sscanf($1,"%f",&f);
	     $$ = f;
	     free($1);
	   }
           ;

UnitCode: UNIT 
          {
	    String str($1);
	    str.downcase();
	    if (str.contains("hz")) $$[0]=MSSpwIndex::MSSPW_UNITHZ;
	    else // Only Frequency and velocity units will make to the parser.
	      {
		$$[0] = MSSpwIndex::MSSPW_UNITVELOCITY;
		throw(MSSelectionSpwParseError(String("Spw expression: Velocity units "
						      "support temporarily disabled.")));
	      }

	    MSSpwIndex myMSSI(MSSpwParse::thisMSSParser->ms()->spectralWindow());
	    $$[1] = (Int)myMSSI.convertToMKS(1.0,1.0,$1)(0);
	    free($1);
          }
          ;
Physical: PhyVal UnitCode
           {
	     $$[0] = $1*$2[1]; // UnitCode[1] has the factor to get to MKS.
	     $$[1] = $2[0];    // UnitCode[0] has the code for the unit.
	   }
;
PhyRange: Physical DASH Physical 
           {
	     if ($1[0] > $3[0])
	       throw(MSSelectionSpwParseError(String("Spw expression: Start of "
						     "range greater than end of range")));
	     $$[0] = $1[0];
	     $$[1] = $3[0];
	     $$[2] = 0;     // The Step
	     if ($1[1] != $3[1])
	       throw(MSSelectionSpwParseError(String("Spw expression: Start and stop specification"
						     " not in the same units.")));
	       
	     $$[3] = $3[1]; // The Unit
	   }
        | PhyVal DASH PhyVal UnitCode
           {
	     if ($1 > $3)
	       throw(MSSelectionSpwParseError(String("Spw expression: Start of "
						     "range greater than end of range")));
	     MSSpwIndex myMSSI(MSSpwParse::thisMSSParser->ms()->spectralWindow());

	     //	     cout << $1 << " " << $3 << " " << $4[0] << " " << $4[1] << endl;
	     $$[0]=$1*$4[1];
	     $$[1]=$3*$4[1];
	     $$[2] = 1;       // The Step
	     $$[3] = $4[0];
    	   }
        | PhyRange CARET Physical
           {
	     if ($1[3] != $3[1])
	       throw(MSSelectionSpwParseError(String("Spw expression: Range and step specifications"
						     " not in the same units.")));
	       
	     $$[2] = $3[0];  // Load the step
	   } 
        | CARET Physical
           {
	       throw(MSSelectionSpwParseError(String("Spw expression: A lone \"^PhyUnits\""
						     " not yet supported.")));
	   }
;
IndexRange: PhyVal DASH PhyVal
             { 
	       if ($1 > $3)
		 throw(MSSelectionSpwParseError(String("Spw expression: Start of "
						       "range greater than end of range")));
	       $$[0] = (Int)$1;
	       $$[1] = (Int)$3;
	       $$[2] = 1;
	       $$[3] = MSSpwIndex::MSSPW_INDEX;
	     }
          | IndexRange CARET PhyVal
             {
	       $$[2] = (Int)$3;
             }
          | CARET PhyVal
             {
	       $$[0] = -1;
	       $$[1] = -1;
	       $$[2] = $2;
	       $$[3] = MSSpwIndex::MSSPW_INDEX;
             }
;
FreqRange: IndexRange 
            {
	      $$[0] = $1[0];//Start index
	      $$[1] = $1[1];//End index
	      $$[2] = $1[2];//Step
	      $$[3] = MSSpwIndex::MSSPW_INDEXRANGE;//Code
	    }; 
         | PhyRange 
            {
	      $$[0] = $1[0];//Start value
	      $$[1] = $1[1];//End value
	      $$[2] = $1[2];//Step
	      $$[3] = $1[3];//Unit code //MSSpwIndex::MSSPW_UNITHZ;
	    }
;
OneFreq:  PhyVal
           {
	     //	     cout << "Index = " << (Int)$1 << endl;
	     $$[0] = (Int)$1; // The Index
	     $$[1] = MSSpwIndex::MSSPW_INDEX; // The index code
	   } 
        | Physical 
           {
	     $$[0] = $1[0]; // The value
	     $$[1] = $1[1]; // The UnitCode
	     //	     $$[2] = $1[1];
	   }
;
FListElements: FreqRange
                 {
		   $$[0]=$1[0];  //Start of the range value
		   $$[1]=$1[1];  //End of the range value
		   $$[2]=$1[2];  //Step of the range
		   $$[3]=$1[3];  //Unit code
		 }
             | OneFreq
                 {
		   $$[0]=$1[0];  //Start of the range value
		   $$[1]=$1[0];  //End of the range value
		   $$[2]=0;      //Step (set to zero to indicate that this is not a range)
		   $$[3]=$1[1];  //Unit code
		 }
;
FreqList: FListElements
           {
	     if (!($$)) delete $$;
	     $$ = new Vector<Float>(0);
	     Int N0=(*($$)).nelements(),N1=4; 
	     (*($$)).resize(N0+N1,True);  // Resize the existing list
	      for(Int i=N0;i<N0+N1;i++)
		(*($$))(i) = (Float)($1[i-N0]);
	   } 
        | FreqList SEMICOLON FListElements
           {
	     Int N0=(*($$)).nelements(), N1=4;
	      (*($$)).resize(N0+N1,True);  // Resize the existing list
	      for(Int i=N0;i<N0+N1;i++)
		(*($$))(i) = $3[i-N0];
	   }
;
Spw: IDENTIFIER   
      { 
	//
	// Use the string as-is.  This cannot include patterns/regex
	// which has characters that are part of range or list
	// syntax (',', '~', ';',':') (that's all I think).
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
      { 
	//
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
      { 
	//
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
   | GT OneFreq
      {
	MSSpwIndex myMSSI(MSSpwParse::thisMSSParser->ms()->spectralWindow());
	if (!($$)) delete $$;
	if ($2[1] == MSSpwIndex::MSSPW_INDEX)
	  $$ = new Vector<Int>(myMSSI.matchIDGT((Int)$2[0]));
	else
	  throw(MSSelectionSpwParseError(String(">NUMBER UNIT not yet implemented")));
	  
	
	ostringstream m; m << "No spw ID found >= " << (Int)$2[0];
	checkSpwError(*($$), m);
      }
   | LT OneFreq
      {
	MSSpwIndex myMSSI(MSSpwParse::thisMSSParser->ms()->spectralWindow());
	if (!($$)) delete $$;
	if ($2[1] == MSSpwIndex::MSSPW_INDEX)
	  $$ = new Vector<Int>(myMSSI.matchIDLT((Int)$2[0]));
	else
	  throw(MSSelectionSpwParseError(String("<NUMBER UNIT not yet implemented")));
	  
	
	ostringstream m; m << "No spw ID found >= " << (Int)$2[0];
	checkSpwError(*($$), m);
      }
   | DASH OneFreq
      {
	MSSpwIndex myMSSI(MSSpwParse::thisMSSParser->ms()->spectralWindow());
	if (!($$)) delete $$;
	$$ = new Vector<Int>(myMSSI.matchFrequencyRange($2[0],$2[0],True));
	  
	
	ostringstream m; m << "No spw ID found ~= " << (Int)$2[0];
	checkSpwError(*($$), m);
      }
   | FreqList 
      {
	MSSpwIndex myMSSI(MSSpwParse::thisMSSParser->ms()->spectralWindow());
	if (!($$)) delete $$;
	Int nSpec;
/* 	   cout << (*($1)) << "  " << endl; */
/* 	   cout << "FreqList "; */
/* 	   if ((*($1))[3] == MSSpwIndex::MSSPW_INDEX) cout << "Index "; */
/* 	   if ((*($1))[3] == MSSpwIndex::MSSPW_INDEXRANGE) cout << "IndexRange "; */
/* 	   if ((*($1))[3] == MSSpwIndex::MSSPW_UNITHZ) cout << "FreqRange "; */
	$$ = new Vector<Int>(myMSSI.convertToSpwIndex($1[0],nSpec)); 
	/*   cout << (*($$)) << endl; */
      }
;
FullSpec: Spw
            {
	      MSSpwIndex myMSSI(MSSpwParse::thisMSSParser->ms()->spectralWindow());
	      //
	      // Convert Spw to a TEN
	      //
	      Vector<Int> varifiedSpwList=myMSSI.matchId(*($1));
	      //	      $$ = MSSpwParse().selectSpwIdsFromIDList(varifiedSpwList);
	      $$ = MSSpwParse::thisMSSParser->selectSpwIdsFromIDList(varifiedSpwList);
	      Int nFSpec;
	      Vector<Float> dummy(0);
	      Vector<Int> chanList = myMSSI.convertToChannelIndex(varifiedSpwList,dummy, nFSpec);
	      //	      MSSpwParse().selectChannelsFromIDList(varifiedSpwList, chanList, nFSpec);
	      MSSpwParse::thisMSSParser->selectChannelsFromIDList(varifiedSpwList, chanList, nFSpec);
	      delete $1;
            }
        | Spw COLON FreqList 
            {
	      MSSpwIndex myMSSI(MSSpwParse::thisMSSParser->ms()->spectralWindow());
	      //
	      // Convert Spw to a TEN and FreqList to a list of channel indexes
	      //
	      Vector<Int> varifiedSpwList=myMSSI.matchId(*($1));
	      //	      $$ = MSSpwParse().selectSpwIdsFromIDList(varifiedSpwList);
	      $$ = MSSpwParse::thisMSSParser->selectSpwIdsFromIDList(varifiedSpwList);
	      Int nFSpecs;
	      Vector<Int> chanList = myMSSI.convertToChannelIndex(varifiedSpwList, (*($3)), nFSpecs);
	      //
	      // This just fills in the chan. list structure (to be
	      // returned for MSSelection::getChanList()).  This is a
	      // statement of intent (i.e. whenever we can figure out
	      // a way to select channels in the VisBuffer, this
	      // method is where we will do it).
	      //
	      //	      MSSpwParse::thisMSSParser->selectChannelsFromIDList((*($1)), chanList);
	      //	      MSSpwParse().selectChannelsFromIDList(varifiedSpwList, chanList, nFSpecs);
	      MSSpwParse::thisMSSParser->selectChannelsFromIDList(varifiedSpwList, chanList, nFSpecs);
	      delete $1;
	    }
;
FullExpr: FullSpec        {} 
        | FullExpr COMMA FullSpec   {}
;
%%
