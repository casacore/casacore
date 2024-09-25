/*
//# JsonGram.ll: Scanner for Json-style key=value lines
//# Copyright (C) 2016
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
*/

/* yy_unput is not used, so let flex not generate it, otherwise picky
   compilers will issue warnings. */
%option nounput noinput

%{
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) result=JsonParser::input(buf,max_size)

#undef YY_DECL
#define YY_DECL int JsonGramlex (YYSTYPE* lvalp)
%}

/* Json is very strict on number representation; see json.org */
WHITE     [ \t\n\r\f]*
DIGIT19   [1-9]
DIGIT     [0-9]
DIGITS    {DIGIT}+
INT       -?({DIGIT}|{DIGIT19}{DIGITS})
DEXP      [Ee][+-]?{DIGITS}
FRAC      "."{DIGITS}
DOUBLE    {INT}({FRAC}|{DEXP}|{FRAC}{DEXP})
DBINT     {DOUBLE}|{INT}
REAL      \"r\"{WHITE}":"{WHITE}{DBINT}
IMAG      \"i\"{WHITE}":"{WHITE}{DBINT}
COMPLEX   "{"{WHITE}{REAL}{WHITE}","{WHITE}{IMAG}{WHITE}"}"
TRUE      true
FALSE     false
NULL      null

STRING    \"(([^\"\n\\])|(\\.))*\"
USTRING   \"(([^\"\n\\])|(\\.))*\n
COMMENT1  "#".*\n
COMMENT2  "//".*\n
COMMENT3  "/*".*"*/"
COMMENT   {COMMENT1}|{COMMENT2}|{COMMENT3}


%%

 /* quit on EOF */
<<EOF>>   {
            yyterminate();
          }

 /* Literals */
{COMPLEX} {
            JsonParser::position() += yyleng;
            double valr,vali;
	    sscanf(JsonGramtext, "{ \"r\" : %lf , \"i\" : %lf }", &valr, &vali);
            lvalp->val = new JsonValue (DComplex(valr, vali));
	    return LITERAL;
	  }
{DOUBLE}  {
            JsonParser::position() += yyleng;
            double val;
	    sscanf(JsonGramtext, "%lf", &val);
            lvalp->val = new JsonValue (val);
	    return LITERAL;
	  }
{INT}     {
            JsonParser::position() += yyleng;
            Int64 ival = atol(JsonGramtext);
            double dval = atof(JsonGramtext);
            /* Handle integers exceeding integer precision as doubles */
            if (ival < dval-0.1  ||  ival > dval+0.1) {
                lvalp->val = new JsonValue (dval);
            } else {
                lvalp->val = new JsonValue (ival);
            }
            return LITERAL;
	  }
{TRUE}    {
            JsonParser::position() += yyleng;
            lvalp->val = new JsonValue (True);
	    return LITERAL;
	  }
{FALSE}   {
            JsonParser::position() += yyleng;
            lvalp->val = new JsonValue (False);
	    return LITERAL;
	  }
{NULL}   {
            JsonParser::position() += yyleng;
            lvalp->val = new JsonValue();
	    return LITERAL;
	  }

 /*
 Strings can have quotes which have to be removed.
 Names can have escape characters to be removed.
 */
{STRING}  {
            JsonParser::position() += yyleng;
            lvalp->val = new JsonValue
              (JsonParser::removeEscapes (String(JsonGramtext+1, yyleng-2)));
	    return STRING;
	  }

":"       { JsonParser::position() += yyleng; return COLON; }

","       { JsonParser::position() += yyleng; return COMMA; }

"["       { JsonParser::position() += yyleng; return LBRACKET; }

"]"       { JsonParser::position() += yyleng; return RBRACKET; }

"\{"      { JsonParser::position() += yyleng; return LBRACE; }

"\}"      { JsonParser::position() += yyleng; return RBRACE; }

 /* Whitespace is skipped */
{WHITE}   { JsonParser::position() += yyleng; }

 /* Comments are skipped */
{COMMENT} { JsonParser::position() += yyleng; }

 /* An unterminated string is an error */
{USTRING} { throw JsonError("JsonGram: Unterminated string"); }

 /* Any other character is invalid */
.         { return TOKENERROR; }

%%
