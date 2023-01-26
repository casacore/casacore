/* -*- C++ -*-
    MSFieldGram.y: Parser for field expressions
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
*/

%{
#include <errno.h>
  using namespace casacore;
%}

%pure-parser                /* make parser re-entrant */

%union {
  const TableExprNode* node;
  Block<TableExprNode>* exprb;
  TableExprNodeSetElem* elem;
  TableExprNodeSet* settp;
  int32_t ival[2];
  char * str;
  double dval;
  Vector<int32_t>* iv;
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

%type <node> fieldstatement
%type <node> indexcombexpr
%type <iv> indexlist
%type <iv> fieldidrange
%type <iv> fieldidlist
%type <iv> fieldid
%type <iv> fieldidbounds

%nonassoc EQ EQASS GT GE LT LE NE COMMA DASH AMPERSAND

%{
#include <casacore/ms/MSSel/MSSelectionTools.h>
  int MSFieldGramlex (YYSTYPE*);
  void checkFieldError(Vector<int32_t>& list, ostringstream& msg, bool force=false, char* = NULL)
  {
    if ((list.nelements() == 0) || force)
      {
	String errorMesg;
	ostringstream Mesg;
	Mesg << "Field Expression: " << msg.str().c_str();
	
	errorMesg = String(Mesg.str().c_str());
	throw(MSSelectionFieldParseError(errorMesg));
      }
  }
%}

%%
fieldstatement: indexcombexpr 
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
	           //MSFieldIndex myMSFI(MSFieldParse::thisMSFParser->msInterface()->field());
	           //MSFieldIndex myMSFI(MSFieldParse::thisMSFParser->ms()->field());
	           MSFieldIndex myMSFI(MSFieldParse::thisMSFParser->subTable());
		   myMSFI.matchIdAgainstNames(*($1));
		   Vector<int32_t> selectedIDs(myMSFI.maskFieldIDs(myMSFI.validateIndices(*($1))));
                   $$ = MSFieldParse().selectFieldIds(selectedIDs);
		   m << "Partial or no match for Field ID list " << (*($1));
                   /* Do delete before check to avoid leak when exception */
		   delete $1;
                   checkFieldError(selectedIDs, m);
                 }
	       ;
//
// A single field name (this could be a regex and
// hence produce a list of indices)
//
fieldid: IDENTIFIER   
          { //
	    // Use the string as-is.  This cannot include patterns/regex
	    // which has characters that are part of range or list
	    // syntax (',', '-') (that's all I think).
	    //
	    // Convert name to index
	    //
	    //MSFieldIndex myMSFI(MSFieldParse::thisMSFParser->msInterface()->field());
	    //MSFieldIndex myMSFI(MSFieldParse::thisMSFParser->ms()->field());
	    MSFieldIndex myMSFI(MSFieldParse::thisMSFParser->subTable());
	    //	    cerr << "ID: " << $1 << endl;
	    $$=new Vector<int32_t>(myMSFI.matchFieldNameOrCode($1));
	    //$$=new Vector<int32_t>(myMSAI.matchFieldRegexOrPattern($1));
	    
	    ostringstream m; m << "No match found for name \"" << $1 << "\"";
	    free($1);
	    checkFieldError(*($$), m);
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
	    //MSFieldIndex myMSFI(MSFieldParse::thisMSFParser->msInterface()->field());
	    //MSFieldIndex myMSFI(MSFieldParse::thisMSFParser->ms()->field());
	    MSFieldIndex myMSFI(MSFieldParse::thisMSFParser->subTable());
	    //	    cerr << "QS: " << $1 << endl;
	    $$ = new Vector<int32_t>(myMSFI.matchFieldRegexOrPattern($1));
	    
	    ostringstream m; m << "No match found for name \"" << $1 << "\"";
	    free($1);
	    checkFieldError(*($$), m);
	    String s(m.str());
	  }
       | REGEX
          { //
	    // A string delimited by a pair of '/': This will be treated
	    // as a regular expression internally.
	    //
	    // Convert name to index
	    //
	    //MSFieldIndex myMSFI(MSFieldParse::thisMSFParser->msInterface()->field());
	    //MSFieldIndex myMSFI(MSFieldParse::thisMSFParser->ms()->field());
	    MSFieldIndex myMSFI(MSFieldParse::thisMSFParser->subTable());
	    $$ = new Vector<int32_t>(myMSFI.matchFieldRegexOrPattern($1,true));
	    
	    ostringstream m; m << "No match found for \"" << $1 << "\"";
	    free($1);
	    checkFieldError(*($$), m);
          }
       ;

fieldidrange: INT // A single field index
               {
		 $$ = new Vector<int32_t>(1);
		 (*($$))(0) = atoi($1);
		 free($1);
	       }
            | INT DASH INT // A range of integer field indices
               {
		 int32_t start = atoi($1);
		 int32_t end   = atoi($3);
		 int32_t len = end - start + 1;
		 Vector<int32_t> fieldids(len);
		 for(int32_t i = 0; i < len; i++) 
		   fieldids[i] = start + i;

		 $$ = new Vector<int32_t>(fieldids);	   
		 free($1); free($3);
	       }
            ;

fieldidbounds: LT INT // <ID
                {
		  //MSFieldIndex myMSFI(MSFieldParse::thisMSFParser->msInterface()->field());
		  //MSFieldIndex myMSFI(MSFieldParse::thisMSFParser->ms()->field());
		  MSFieldIndex myMSFI(MSFieldParse::thisMSFParser->subTable());
		  int32_t n=atoi($2);
		  $$ = new Vector<int32_t>(myMSFI.matchFieldIDLT(n));

		  ostringstream m; m << "No field ID found <" << n;
		  free($2);
		  checkFieldError(*($$), m);
		}
             | GT INT // >ID
                {
		  //MSFieldIndex myMSFI(MSFieldParse::thisMSFParser->msInterface()->field());
		  //MSFieldIndex myMSFI(MSFieldParse::thisMSFParser->ms()->field());
		  MSFieldIndex myMSFI(MSFieldParse::thisMSFParser->subTable());
		  int32_t n=atoi($2);
		  $$ = new Vector<int32_t>(myMSFI.matchFieldIDGT(n));

		  ostringstream m; m << "No field ID found >" << n;
		  free($2);
		  checkFieldError(*($$), m);
		}
             | GT INT AMPERSAND LT INT // >ID & <ID
                {
		  //MSFieldIndex myMSFI(MSFieldParse::thisMSFParser->msInterface()->field());
		  //MSFieldIndex myMSFI(MSFieldParse::thisMSFParser->ms()->field());
		  MSFieldIndex myMSFI(MSFieldParse::thisMSFParser->subTable());
		  int32_t n0=atoi($2), n1=atoi($5);
		  $$ = new Vector<int32_t>(myMSFI.matchFieldIDGTAndLT(n0,n1));

		  ostringstream m; 
		  m << "No field found in the range [" << n0 << "," << n1 << "]";
		  free($2); free($5);
		  checkFieldError(*($$), m);
		}
             ;
fieldidlist: fieldid // A singe field ID
               {
		 $$ = $1;
	       }
           | fieldidrange // ID range ( n0-n1 )
              {
		$$ = $1;
	      }
           | fieldidbounds  // >ID, <ID, >ID & <ID
              {
		$$ = $1;
	      }
           ;
indexlist: fieldidlist
            {
	      $$ = new Vector<int32_t>(*$1);
	      delete $1;
	    }
         | indexlist COMMA fieldidlist  
            {
              $$ = $1;
	      int32_t N0=(*($1)).nelements(), 
		N1 = (*($3)).nelements();
	      (*($$)).resize(N0+N1,true);  // Resize the existing list
	      for(int32_t i=N0;i<N0+N1;i++)
		(*($$))(i) = (*($3))(i-N0);
	      delete $3;
            }
/*
          | LPAREN indexlist RPAREN //Parenthesis are not
				    //syntactically useful here
            {
	      $$ = $2;
	    }
*/
          ;
%%

