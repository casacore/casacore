/*
    LatticeGram.l: Lexical analyzer for lattice expressions
    Copyright (C) 1998
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
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) result=latticeGramInput(buf,max_size)

#undef YY_DECL
#define YY_DECL int LatticeGramlex (YYSTYPE* lvalp)
%}

/*
   Complex values can be given as:   FLOATi
          where i is the letter i (in lowercase only).
   In a NAME the backslash can be used to escape special characters like -.
   In that way a name like DIR/NAME can be given as DIR\/NAME.
   Alternatively the name can be enclosed in single or double quotes.
*/
WHITE     [ \t\n]*
DIGIT     [0-9]
INT       {DIGIT}+
FEXP      [Ee][+-]?{INT}
DEXP      [Dd][+-]?{INT}
FLOAT     {INT}{FEXP}|{INT}"."{DIGIT}*({FEXP})?|{DIGIT}*"."{INT}({FEXP})?
DOUBLE    {INT}{DEXP}|{INT}"."{DIGIT}*({DEXP})?|{DIGIT}*"."{INT}({DEXP})?
FLINT     {FLOAT}|{INT}
DBINT     {DOUBLE}|{INT}
COMPLEX   {FLINT}"i"
DCOMPLEX  {DBINT}"i"

QSTRING   \"[^\"\n]*\"
ASTRING   \'[^\'\n]*\'
UQSTRING   \"[^\"\n]*\n
UASTRING   \'[^\'\n]*\n
STRING    ({QSTRING}|{ASTRING})+
USTRING   ({UQSTRING}|{UASTRING})+
NAME      \\?[A-Za-z_]([A-Za-z_0-9]|(\\.))*
NAMELAT   [A-Za-z0-9._\~$]+


%%

 /* Literals */
{DCOMPLEX} {
            latticeGramPosition() += yyleng;
            Double value;
	    sscanf (LatticeGramtext, "%lf%*c", &value);
            lvalp->val = new LatticeParse (DComplex (0, value));
	    return LITERAL;
	  }
{COMPLEX} {
            latticeGramPosition() += yyleng;
            Float value;
	    sscanf (LatticeGramtext, "%f%*c", &value);
            lvalp->val = new LatticeParse (Complex (0, value));
	    return LITERAL;
	  }
{DOUBLE}  {
            latticeGramPosition() += yyleng;
            lvalp->val = new LatticeParse (atof(LatticeGramtext));
	    return LITERAL;
	  }
{FLOAT}   {
            latticeGramPosition() += yyleng;
            lvalp->val = new LatticeParse (Float(atof(LatticeGramtext)));
	    return LITERAL;
	  }
{INT}     {
            latticeGramPosition() += yyleng;
            lvalp->val = new LatticeParse (atoi(LatticeGramtext));
            return LITERAL;
	  }

 /* A simple name can be a lattice name or a function name */
{NAME}    {
            latticeGramPosition() += yyleng;
            lvalp->val = new LatticeParse
                                (latticeGramRemoveEscapes (LatticeGramtext));
	    return NAME;
	  }

 /*
 A lattice name can have more characters than a simple name
 Note that the name of a lattice file can be given in 3 ways:
 - When it contains only alphanumerical characters and ._~$
   it can be given as such. E.g.
       a.img
 - When other characters are used, they have to be escaped.
   This can be done in 2 ways:
   - Enclose the string in single or double quotes (concatenation is
     possible). E.g.
       "a/b/c"      results in a/b/c
       "a'b"'c"d'   results in a'bc"d
   - Use the backslash escape character. E.g.
       a\/b\/c      results in a/b/c
 */
{NAMELAT} {
            latticeGramPosition() += yyleng;
            lvalp->val = new LatticeParse (LatticeGramtext);
	    return LATNAME;
	  }
{STRING}  {
            latticeGramPosition() += yyleng;
            lvalp->val = new LatticeParse (LatticeGramtext);
	    return LATNAME;
	  }

"=="      { latticeGramPosition() += yyleng; return EQ; }
">="      { latticeGramPosition() += yyleng; return GE; }
">"       { latticeGramPosition() += yyleng; return GT; }
"<="      { latticeGramPosition() += yyleng; return LE; }
"<"       { latticeGramPosition() += yyleng; return LT; }
"!="      { latticeGramPosition() += yyleng; return NE; }
"&&"      { latticeGramPosition() += yyleng; return AND; }
"||"      { latticeGramPosition() += yyleng; return OR; }
"!"       { latticeGramPosition() += yyleng; return NOT; }
"^"       { latticeGramPosition() += yyleng; return POWER; }
"*"       { latticeGramPosition() += yyleng; return TIMES; }
"/"       { latticeGramPosition() += yyleng; return DIVIDE; }
"%"       { latticeGramPosition() += yyleng; return MODULO; }
"+"       { latticeGramPosition() += yyleng; return PLUS; }
"-"       { latticeGramPosition() += yyleng; return MINUS; }
"("       { latticeGramPosition() += yyleng; return LPAREN; }
")"       { latticeGramPosition() += yyleng; return RPAREN; }
","       { latticeGramPosition() += yyleng; return COMMA; }

 /* Whitespace is skipped */
{WHITE}   { latticeGramPosition() += yyleng; }

 /* An unterminated string is an error */
{USTRING} { throw (AipsError ("LatticeParse: Unterminated string")); }

 /* terminate on EOF */
<<EOF>>   { yyterminate(); }

 /* Any other character is invalid */
.         { return YYERRCODE; }

%%
