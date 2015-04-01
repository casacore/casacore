/* -*-C++-*-
    MSTimeGram.y: Parser for time expressions
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
using namespace casacore;
%}

%pure-parser                /* make parser re-entrant */

%union {
  const TableExprNode* node;
  Block<TableExprNode>* exprb;
  TableExprNodeSetElem* elem;
  TableExprNodeSet* settp;
  const MEpoch* tval;
  Int ival;
  Double dval;
  Double dval3[3];
  Int ival3[3];
  TimeFields timeFields;
}

%token <ival> NUMBER
%token <dval> FNUMBER
%token SQUOTE

%token DASH
%token LT
%token GT
%token COLON
%token COMMA
%token SLASH
%token DOT
%token PERCENT
%token STAR
%token LSQBRACKET
%token RSQBRACKET

%token UNKNOWN

%type <node> timestatement
%type <node> timeexpr
%type <node> singletimeexpr
%type <node> rangetimeexpr
%type <node> brangetimeexpr
%type <node> upboundtimeexpr
%type <node> lowboundtimeexpr
%type <timeFields> yeartimeexpr
%type <dval> wildFloat
%type <ival> wildNumber
%type <ival> tFields
%type <dval3> timeObj
%type <ival> yFields
%type <ival3> yearObj

%left OR
%left AND
%nonassoc GT LT LE COLON SLASH
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%nonassoc UNARY
%nonassoc NOT
%right POWER

%{
#include <casacore/ms/MSSel/MSSelectionError.h>
  //  extern MSTimeParse *thisMSTParser;
  Bool MSTimeEdgeInclusiveRange=False;
  int MSTimeGramlex (YYSTYPE*);
  inline void MSTGgarbageCollector(const MEpoch* tval){if (tval) delete tval;}
  void splitSec(const Double& fsec, Int &sec, Int &milliSec) 
  {
    sec = (Int)(fsec);
    if (sec < 0)
      milliSec = -1;
    else
      milliSec = (Int)((fsec - sec)*1000);
  }
%}

%%
timestatement: timeexpr {$$ = $1;}
             | timestatement COMMA timeexpr 
                {
		  //
		  // The various MSTimeParse::select* methods return the
		  // static member node_p.  Each visit to these methods
		  // produces a TEN which is ORed with the existing node_p.
		  // Effectively node_p is a global store of the TENs tree
		  // generated during parsing. Hence here $1 and $3 both
		  // refer to the same TEN tree (node_p) which anyway has
		  // all the ORed TENs.  So just return either $1 or $3; We
		  // choose the first-come-first-served principle and
		  // return $1 :-)
		  $$=$1;
		}
             ;

timeexpr: singletimeexpr
        | brangetimeexpr
        | lowboundtimeexpr
        | upboundtimeexpr
        ;

wildNumber: STAR {$$ = -1;}
          | NUMBER {$$ = $1;}
          ;
wildFloat: wildNumber {$$ = $1;}
         | FNUMBER {$$ = $1;}
         ;
singletimeexpr: yeartimeexpr 
                  {
		    MSTimeParse::thisMSTParser->setDefaults($1);
		    const MEpoch *t0=MSTimeParse::thisMSTParser->yearTimeConvert($1);
		    //		    $$ = MSTimeParse().selectTime(t0, false); 
		    $$ = MSTimeParse::thisMSTParser->selectTime(t0, false); 

		    MSTGgarbageCollector(t0);
		  }
              ;

brangetimeexpr: LSQBRACKET {MSTimeEdgeInclusiveRange=True;}  rangetimeexpr RSQBRACKET {$$=$3;MSTimeEdgeInclusiveRange=False;}
              |            rangetimeexpr            {MSTimeEdgeInclusiveRange=False;$$=$1;}
rangetimeexpr: yeartimeexpr DASH yeartimeexpr 
                 {
		   MSTimeParse::thisMSTParser->setDefaults($1);
		   MSTimeParse::thisMSTParser->copyDefaults($3,$1);
		   const MEpoch *t0=MSTimeParse::thisMSTParser->yearTimeConvert($1);
		   const MEpoch *t1=MSTimeParse::thisMSTParser->yearTimeConvert($3);

		   $$ = MSTimeParse::thisMSTParser->selectTimeRange(t0,t1,MSTimeEdgeInclusiveRange);
		   MSTGgarbageCollector(t0);
		   MSTGgarbageCollector(t1);
		 }
             | yeartimeexpr PLUS yeartimeexpr
                 {
		   MSTimeParse::thisMSTParser->setDefaults($1);
		   MSTimeParse::thisMSTParser->setDefaults($3,false);
		   MSTimeParse::thisMSTParser->validate($1);
		   MSTimeParse::thisMSTParser->validate($3);
		   Double s;
		   s=$3.sec;

		   Time time0($1.year,$1.month,$1.day,$1.hour,$1.minute,(Double)$1.sec);
		   Time time1($3.year,$3.month,$3.day,$3.hour,$3.minute,s);
		   Double mjd=time1.modifiedJulianDay()*86400.0;

		   time1 = time0 + mjd;
		   const MEpoch *t0=new MEpoch(MVEpoch(time0.modifiedJulianDay()));
		   const MEpoch *t1=new MEpoch(MVEpoch(time1.modifiedJulianDay()));

		   $$ = MSTimeParse::thisMSTParser->selectTimeRange(t0,t1,MSTimeEdgeInclusiveRange);
		   MSTGgarbageCollector(t0);
		   MSTGgarbageCollector(t1);
		 }
             ;

lowboundtimeexpr: GT yeartimeexpr 
                    {
		      MSTimeParse::thisMSTParser->setDefaults($2);
		      const MEpoch *t0=MSTimeParse::thisMSTParser->yearTimeConvert($2);
		      //		      $$ = MSTimeParse().selectTimeGT(t0,false);
		      $$ = MSTimeParse::thisMSTParser->selectTimeGT(t0,false);
		      MSTGgarbageCollector(t0);
		    }
                    ;

upboundtimeexpr: LT yeartimeexpr 
                   {
		     MSTimeParse::thisMSTParser->setDefaults($2);
		     const MEpoch *t0=MSTimeParse::yearTimeConvert($2);
		     //		     $$ = MSTimeParse().selectTimeLT(t0,false);
		     $$ = MSTimeParse::thisMSTParser->selectTimeLT(t0,false);
		     MSTGgarbageCollector(t0);
		   }
                   ;
tFields: wildNumber COLON {$$ = $1;}
       | COLON            {$$ = 0;}
       ;

yFields: wildNumber SLASH {$$ = $1;}
       | SLASH            {$$ = 0;}
       ;

timeObj: tFields tFields wildFloat {/* HH:MM:SS.FF */ $$[0] = $1; $$[1] = $2; $$[2] = $3;}
       | tFields tFields           {/* HH:MM */       $$[0] = $1; $$[1] = $2; $$[2] = 0;}
       | tFields wildFloat         {/* MM:SS.FF */    $$[0] = 0;  $$[1] = $1; $$[2] = $2;}
       | tFields                   {/* HH: */         $$[0] = 0;  $$[1] = 0;  $$[2] = $1;}
       | wildNumber                {/* HH */          $$[0] = -1;  $$[1] = -1;  $$[2] = $1;}
;
yearObj: yFields yFields wildNumber {/* YYYY/MM/DD */ $$[0]=$1; $$[1]=$2; $$[2]=$3;}
       | yFields yFields            {/* MM/DD/ */     $$[0]=0;  $$[1]=$1; $$[2]=$2;}
/*       | yFields wildNumber { // MM/DD
	     $$[0]=0; $$[1]=$1; $$[2]=$2;
	   }
*/
;
yeartimeexpr: yearObj SLASH timeObj
                 {
		   // YY/MM/DD/HH:MM:SS.FF
		   Int sec,milliSec;
		   splitSec($3[2],sec,milliSec);
		   msTimeGramSetTimeFields($$,$1[0],$1[1],$1[2],(Int)$3[0],(Int)$3[1],sec,milliSec);

/* 		   cout << $1[0] << " "  */
/* 			<< $1[1] << " " */
/* 			<< $1[2] << " " */
/* 			<< $3[0] << " " */
/* 			<< $3[1] << " " */
/* 			<< sec << " " << milliSec << endl; */
		 }
            | yearObj 
                 {
                   msTimeGramSetTimeFields($$,$1[0],$1[1],$1[2],-1,-1,-1,-1);
/* 		   cout << $1[0] << " " */
/* 			<< $1[1] << " " */
/* 			<< $1[2] << " " */
/* 			<< "-1 -1 -1 -1"  */
/* 			<< endl; */
		 }
            | timeObj 
                 {
		   Int sec,milliSec;
		   splitSec($1[2],sec,milliSec);
		   msTimeGramSetTimeFields($$,-1,-1,-1,(Int)$1[0],(Int)$1[1],sec,milliSec);
/* 		   cout << -1 << " " */
/* 			<< -1 << " " */
/* 			<< -1 << " " */
/* 			<< $1[0] << " " << $1[1] << " " << sec << " " << milliSec */
/* 			<< endl; */
		 }
                 ;
%%
/*
yeartimeexpr: WNUMBER SLASH WNUMBER SLASH WNUMBER SLASH WNUMBER
              COLON WNUMBER COLON FLOAT
                {
		  // YY/MM/DD/HH:MM:SS.FF
		  Int sec,milliSec;
		  splitSec($11,sec,milliSec);
		  msTimeGramSetTimeFields($$,$1,$3,$5,$7,$9,sec,milliSec);
		}
              | WNUMBER SLASH WNUMBER SLASH WNUMBER SLASH WNUMBER
                COLON WNUMBER 
                {
		  // YY/MM/DD/HH:MM
		  msTimeGramSetTimeFields($$,$1,$3,$5,$7,$9,-1,-1);
		}
              | WNUMBER SLASH WNUMBER SLASH WNUMBER SLASH WNUMBER 
                {
		  // YY/MM/DD/HH
		  msTimeGramSetTimeFields($$,$1,$3,$5,$7,-1,-1,-1);
		}
              | WNUMBER SLASH WNUMBER SLASH WNUMBER 
                {
		  // YY/MM/DD
		  msTimeGramSetTimeFields($$,$1,$3,$5,-1,-1,-1,-1);
		}
              | WNUMBER SLASH WNUMBER SLASH WNUMBER 
                COLON WNUMBER COLON FLOAT
                {
		  // MM/DD/HH:MM:SS.FF
		  Int sec,milliSec;
		  splitSec($9,sec,milliSec);
		  msTimeGramSetTimeFields($$,-1,$1,$3,$5,$7,sec,milliSec);
		}
              | WNUMBER COLON WNUMBER COLON FLOAT
                {
		  // HH:MM:SS.FF
		  Int sec,milliSec;
		  splitSec($5,sec,milliSec);
		  msTimeGramSetTimeFields($$,-1,-1,-1,$1,$3,sec,milliSec);
		}
              | WNUMBER COLON FLOAT
                {
		  // MM:SS.FF
		  Int sec,milliSec;
		  splitSec($3,sec,milliSec);
		  msTimeGramSetTimeFields($$,-1,-1,-1,-1,$1,sec,milliSec);
		}
              | FLOAT
                {
		  Int sec,milliSec;
		  splitSec($1,sec,milliSec);
		  msTimeGramSetTimeFields($$,-1,-1,-1,-1,-1,sec,milliSec);
		}
             | WNUMBER SLASH WNUMBER COLON WNUMBER COLON FLOAT 
               {
		 // DD/HH:MM:SS
		 Int sec,milliSec;
		 splitSec($7,sec,milliSec);
		 msTimeGramSetTimeFields($$,-1,-1,$1,$3,$5,sec,milliSec);
	       }
             | WNUMBER SLASH WNUMBER COLON WNUMBER 
               {
		 // DD/HH:MM
		 msTimeGramSetTimeFields($$,-1,-1,$1,$3,$5,-1,-1);
	       }
             | WNUMBER SLASH WNUMBER 
               {
		 // DD/HH
		  msTimeGramSetTimeFields($$,$1,$3,-1,-1,-1,-1,-1);
	       }            ;
*/
/*
daytimeexpr: WNUMBER SLASH WNUMBER COLON WNUMBER COLON WNUMBER DOT WNUMBER 
               {
		 // DD/HH:MM:SS.FF
		  msTimeGramSetTimeFields($$,$1,$3,$5,$7,$9,-1,-1);
		  //                 $$ = MSTimeParse::dayTimeConvert($1, $3, $5, $7, $9);
	       }
             | WNUMBER SLASH WNUMBER COLON WNUMBER COLON WNUMBER 
               {
		 // DD/HH:MM:SS
		 msTimeGramSetTimeFields($$,-1,-1,$1,$3,$5,$7,-1);
		  //	         $$ = MSTimeParse::dayTimeConvert($1, $3, $5, $7, 0);
	       }
             | WNUMBER SLASH WNUMBER COLON WNUMBER 
               {
		 // DD/HH:MM
		 msTimeGramSetTimeFields($$,-1,-1,$1,$3,$5,-1,-1);
		  //	         $$ = MSTimeParse::dayTimeConvert($1, $3, $5, 0, 0);
	       }
             | WNUMBER SLASH WNUMBER 
               {
		 // DD/HH
		  msTimeGramSetTimeFields($$,$1,$3,-1,-1,-1,-1,-1);
		  //	         $$ = MSTimeParse::dayTimeConvert($1, $3, 0, 0, 0);
	       }
           ;
*/
