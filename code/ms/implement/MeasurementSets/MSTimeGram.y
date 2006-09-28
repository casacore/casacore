/*
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
using namespace casa;
%}

%pure_parser                /* make parser re-entrant */

%union {
  const TableExprNode* node;
  Block<TableExprNode>* exprb;
  TableExprNodeSetElem* elem;
  TableExprNodeSet* settp;
  const MEpoch* tval;
  Int ival;
  Double dval;
  TimeFields timeFields;
}

%token EQASS
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

%token LBRACKET
%token LPAREN
%token RBRACKET
%token RPAREN
%token LBRACE
%token RBRACE
%token UNKNOWN

%type <node> timestatement
%type <node> timeexpr
%type <node> singletimeexpr
%type <node> rangetimeexpr
%type <node> upboundtimeexpr
%type <node> lowboundtimeexpr
%type <timeFields> yeartimeexpr
%type <dval> FLOAT
%type <ival> WNUMBER

%left OR
%left AND
%nonassoc EQ EQASS GT GE LT LE NE COLON SLASH
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%nonassoc UNARY
%nonassoc NOT
%right POWER

%{
  extern MSTimeParse *thisMSTParser;
  int MSTimeGramlex (YYSTYPE*);
  inline void MSTGgarbageCollector(const MEpoch* tval){if (tval) delete tval;}
  void splitSec(const Double& fsec, Int &sec, Int &milliSec) 
  {
    sec = (Int)(fsec);
    milliSec = (Int)((fsec - sec)*1000);
  }
%}

%%
timestatement: timeexpr {$$ = $1;}
             ;

timeexpr: singletimeexpr
        | rangetimeexpr
        | lowboundtimeexpr
        | upboundtimeexpr
        | timeexpr COMMA timeexpr 
           {
	     //
	     // The various MSTimeParse::select* methods return the
	     // static member node_p.  Each visit to these methods
	     // produces a TEN which is ORed with the existing node_p.
	     // Effectively node_p is a global store of the TENs tree
	     // generated during parsing. Hence here $1 and $3 both
	     // refer to the same TEN tree (node_p) which anyway has
	     // all the ORed TENs.  So just return either $1 or $2;
	     // 
	     $$=$1;
	   }
        ;

WNUMBER: STAR {$$ = -1}
          | NUMBER {$$ = $1;}
          ;
FLOAT: WNUMBER {$$ = $1;}
        | FNUMBER {$$ = $1;}
          ;

singletimeexpr: yeartimeexpr 
                  {
		    thisMSTParser->setDefaults($1);
		    const MEpoch *t0=MSTimeParse::yearTimeConvert($1);
		    $$ = MSTimeParse().selectTime(t0, false); 

		    MSTGgarbageCollector(t0);
		  }
              ;

rangetimeexpr: yeartimeexpr DASH yeartimeexpr 
                 {
		   thisMSTParser->setDefaults($1);
		   thisMSTParser->copyDefaults($3,$1);
		   const MEpoch *t0=MSTimeParse::yearTimeConvert($1);
		   const MEpoch *t1=MSTimeParse::yearTimeConvert($3);

		   $$ = MSTimeParse().selectTimeRange(t0,t1,false);
		   MSTGgarbageCollector(t0);
		   MSTGgarbageCollector(t1);
		 }
               | yeartimeexpr PLUS yeartimeexpr
                 {
		   thisMSTParser->setDefaults($1);
		   thisMSTParser->setDefaults($3,false);
		   thisMSTParser->validate($1);
		   thisMSTParser->validate($3);
		   Double s;
		   s=$3.sec;

		   Time time0($1.year,$1.month,$1.day,$1.hour,$1.minute,(Double)$1.sec);
		   Time time1($3.year,$3.month,$3.day,$3.hour,$3.minute,s);
		   Double mjd=time1.modifiedJulianDay()*86400.0;

		   time1 = time0 + mjd;
		   const MEpoch *t0=new MEpoch(MVEpoch(time0.modifiedJulianDay()));
		   const MEpoch *t1=new MEpoch(MVEpoch(time1.modifiedJulianDay()));

		   $$ = MSTimeParse().selectTimeRange(t0,t1,true);
		   MSTGgarbageCollector(t0);
		   MSTGgarbageCollector(t1);
		 }
             ;

lowboundtimeexpr: GT yeartimeexpr 
                    {
		      thisMSTParser->setDefaults($2);
		      const MEpoch *t0=MSTimeParse::yearTimeConvert($2);
		      $$ = MSTimeParse().selectTimeGT(t0,false);
		      MSTGgarbageCollector(t0);
		    }
                ;

upboundtimeexpr: LT yeartimeexpr 
                   {
		     thisMSTParser->setDefaults($2);
		     const MEpoch *t0=MSTimeParse::yearTimeConvert($2);
		     $$ = MSTimeParse().selectTimeLT(t0,false);
		     MSTGgarbageCollector(t0);
		   }
               ;
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
%%
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
