/*-*- C++ -*-
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
  char* str;
  double dval;
  Vector<Int>* iv;
  std::vector<double>* dv;
  Vector<String>* is;
}


%token COMMA
%token SEMICOLON
%token AMPERSAND
%token DASH
%token NOT
%token LPAREN
%token RPAREN
%token LT
%token LE
%token GT
%token GE

%token <str> INT
%token <str> FLOAT
%token <str> UNIT
%token <str> QSTRING
%token <str> REGEX
%token <str> IDENTIFIER

%type <node> antennastatement
%type <node> indexcombexpr
%type <node> baseline
%type <node> gbaseline
%type <str> ident
%type <dval> flt
%type <dval> flint
%type <dval> unit
%type <dval> flintunit
%type <dval> flunit
%type <iv> indexlist
%type <iv> antidrange
%type <iv> antidlist
%type <iv> antid
%type <dv> blength
%type <dv> blengthlist

%{
  int MSAntennaGramlex (YYSTYPE*);
  Bool MSAntennaGramNegate=False;
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
antennastatement: indexcombexpr                {$$ = $1;}
                 | LPAREN indexcombexpr RPAREN {$$ = $2;}
                ;


indexcombexpr: gbaseline                         {$$=$1;}
             | indexcombexpr SEMICOLON gbaseline {$$ = $1;}
             ;

gbaseline: NOT {MSAntennaGramNegate=True;}  baseline {$$=$3;}
         |     {MSAntennaGramNegate=False;} baseline {$$=$2;}

baseline: indexlist AMPERSAND indexlist 
            {
	      MSAntennaIndex myMSAI(MSAntennaParse::thisMSAParser->ms()->antenna()); 
	      Vector<Int> a1 = myMSAI.matchId(*($1)); 
	      Vector<Int> a2 = myMSAI.matchId(*($3)); 
	      $$ = MSAntennaParse::thisMSAParser->selectAntennaIds
                (a1,a2,MSAntennaParse::CrossOnly, MSAntennaGramNegate); 
	      delete $1;
              delete $3;
	    } 
        | indexlist AMPERSAND  //Match INDEXLIST & INDEXLIST
            {
	      MSAntennaIndex myMSAI(MSAntennaParse::thisMSAParser->ms()->antenna()); 
	      Vector<Int> a1 = myMSAI.matchId(*($1)); 
	      $$ = MSAntennaParse::thisMSAParser->selectAntennaIds
                (a1,a1,MSAntennaParse::CrossOnly, MSAntennaGramNegate); 
	      delete $1;
	    }
        | indexlist           //Match INDEXLIST & ALLANTENNAS
            {
	      MSAntennaIndex myMSAI(MSAntennaParse::thisMSAParser->ms()->antenna()); 
	      Vector<Int> a1 = myMSAI.matchId(*($1)); 
 	      $$ = MSAntennaParse::thisMSAParser->selectAntennaIds
                (a1,MSAntennaParse::CrossOnly, MSAntennaGramNegate); 
	      delete $1;
	    }
        | indexlist AMPERSAND AMPERSAND indexlist // Include self-correlations
            {
	      MSAntennaIndex myMSAI(MSAntennaParse::thisMSAParser->ms()->antenna()); 
	      Vector<Int> a1 = myMSAI.matchId(*($1)); 
	      Vector<Int> a2 = myMSAI.matchId(*($4)); 
 	      $$ = MSAntennaParse::thisMSAParser->selectAntennaIds
                (a1,a2,MSAntennaParse::AutoCorrAlso, MSAntennaGramNegate); 
	      delete $1;
              delete $4;
	    } 
        | indexlist AMPERSAND AMPERSAND // Include self-correlations 
            {
	      MSAntennaIndex myMSAI(MSAntennaParse::thisMSAParser->ms()->antenna()); 
	      Vector<Int> a1 = myMSAI.matchId(*($1)); 
 	      $$ = MSAntennaParse::thisMSAParser->selectAntennaIds
                (a1,a1,MSAntennaParse::AutoCorrAlso, MSAntennaGramNegate); 
	      delete $1;
	    }
        | indexlist AMPERSAND AMPERSAND AMPERSAND // Only self-correlations :-)
            {
	      MSAntennaIndex myMSAI(MSAntennaParse::thisMSAParser->ms()->antenna()); 
	      Vector<Int> a1 = myMSAI.matchId(*($1)); 
 	      $$ = MSAntennaParse::thisMSAParser->selectAntennaIds
                (a1,MSAntennaParse::AutoCorrOnly, MSAntennaGramNegate); 
	      delete $1;
	    }
        | blengthlist  // baseline length list
            {
              $$ = MSAntennaParse::thisMSAParser->selectLength
                (*$1, MSAntennaGramNegate);
              delete $1;
            }
        ;

ident:   IDENTIFIER
           { $$ = $1; }
       | UNIT             // a unit is an aphabetic name, so here it is a name
           { $$ = $1; }

antid: ident  // A single antenna name (this could be a regex and
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
	      //
	      // This code is due to VLA specienfic complication
	      // arising due to the fact that VLA antennam "NAMES" are
	      // strings that can be parsed as valid integers! Believe
	      // it or not, VLA antenna NAMES are "1", "2", "3" and so
	      // on.....  So (phew).  Just for antenna selection (and
	      // this *just* because of silly convention for VLA
	      // antenna naming!), if we get an INT, treat it as name
	      // still and first attempt a match with the NAME column.
	      // If that fails, treat it as an integer index and do the
	      // right thing.
	      //
	      MSAntennaIndex myMSAI(MSAntennaParse::thisMSAParser->ms()->antenna());
	      Vector<Int> tmp(myMSAI.matchAntennaName($1));
	      $$ = new Vector<Int>(1);
	      if (tmp.nelements() > 0)
		(*($$))(0) = tmp[0];
	      else
		(*($$))(0) = atoi($1);
	      free($1);
	    }
           | INT DASH INT // A range of integer antenna indices
            {
              Int start = atoi($1);
              Int end   = atoi($3);
              Int len = end - start + 1;
              Vector<Int> antennaids(len);
              for(Int i = 0; i < len; i++) antennaids[i] = start + i;

	      if (!($$)) delete $$;
	      //              $$ = new Vector<Int>(antennaids);	   
              $$ = new Vector<Int>(len);

	      MSAntennaIndex myMSAI(MSAntennaParse::thisMSAParser->ms()->antenna());
	      for (Int i=0; i<len; i++) {
		  ostringstream vlaName;
		  vlaName << antennaids[i];
		  Vector<Int> tmp(myMSAI.matchAntennaName(vlaName));
		  if (tmp.nelements() > 0) ((*$$))[i] = tmp[0];
		  else ((*$$))[i] = antennaids[i];
		}
	      free($1);
              free($3);
            }
          ;

antidlist: antid       {$$ = $1;}// A singe antenna ID
          | antidrange {$$ = $1;}
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

blengthlist: blength
             {
               $$ = $1;
             }
           | blengthlist COMMA blength
             {
               $$ = $1;
               $$->push_back ((*$3)[0]);
               $$->push_back ((*$3)[1]);
               delete $3;
             }

blength:     LT flunit
             {
               $$ = new std::vector<double>();
               $$->push_back (-1e30);
               $$->push_back ($2 - 0.000000001);
             }
           | LE flunit
             {
               $$ = new std::vector<double>();
               $$->push_back (-1e30);
               $$->push_back ($2);
             }
           | GT flunit
             {
               $$ = new std::vector<double>();
               $$->push_back ($2 + 0.000000001);
               $$->push_back (1e30);
             }
           | GE flunit
             {
               $$ = new std::vector<double>();
               $$->push_back ($2);
               $$->push_back (1e30);
             }
           | flt DASH flt
             {
               $$ = new std::vector<double>();
               $$->push_back ($1);
               $$->push_back ($3);
             }
           | flt DASH flt unit
             {
               $$ = new std::vector<double>();
               $$->push_back ($1 / $4);
               $$->push_back ($3 / $4);
             }
           | flintunit DASH flintunit
             {
               $$ = new std::vector<double>();
               $$->push_back ($1);
               $$->push_back ($3);
             }

flunit:    flint
             { $$ = $1; }
         | flintunit
             { $$ = $1; }

flintunit: flint unit
             { $$ = $1/$2; }

unit:      UNIT
           { 
             $$ = MSAntennaParse::getUnitFactor ($1);
             free($1);
           }

flint:     flt
             { $$ = $1; }
         | INT
           {
             $$ = atoi($1);
             free ($1);
           }

flt:       FLOAT
           {
             $$ = atof($1);
             free ($1);
           }

%%
