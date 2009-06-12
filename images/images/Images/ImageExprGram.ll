/*
    ImageExprGram.l: Lexical analyzer for image expressions
    Copyright (C) 1998,1999,2000,2003
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

/* yy_unput is not used, so let flex not generate it, otherwise picky
   compilers will issue warnings. */
%option nounput

%{
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) result=imageExprGramInput(buf,max_size)

#undef YY_DECL
#define YY_DECL int ImageExprGramlex (YYSTYPE* lvalp)
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
TRUE      T
FALSE     F

INDEXIN   [Ii][Nn][Dd][Ee][Xx][Ii][Nn]
INDEXNOTIN [Ii][Nn][Dd][Ee][Xx][Nn][Oo][Tt][Ii][Nn]
INDEXN    [Ii][Nn][Dd][Ee][Xx]{INT}
IN        [Ii][Nn]
NOT       [Nn][Oo][Tt]

QSTRING   \"[^\"\n]*\"
ASTRING   \'[^\'\n]*\'
UQSTRING   \"[^\"\n]*\n
UASTRING   \'[^\'\n]*\n
STRING    ({QSTRING}|{ASTRING})+
USTRING   ({UQSTRING}|{UASTRING})+
NAME      [A-Za-z_]([A-Za-z_0-9])*
TMPNAME   "$"{INT}
TMPREGION "$"[rR]{INT}
ESCNAME   ([A-Za-z_~$]|(\\.))([A-Za-z0-9._~$]|(\\.))*
COLONNAME ({NAME}|{ESCNAME})?":"":"?({NAME}|{ESCNAME})


%%

 /* Literals */
{DCOMPLEX} {
            imageExprGramPosition() += yyleng;
            Double value;
	    sscanf (ImageExprGramtext, "%lf%*c", &value);
            lvalp->val = new ImageExprParse (DComplex (0, value));
	    ImageExprParse::addNode (lvalp->val);
	    return LITERAL;
	  }
{COMPLEX} {
            imageExprGramPosition() += yyleng;
            Float value;
	    sscanf (ImageExprGramtext, "%f%*c", &value);
            lvalp->val = new ImageExprParse (Complex (0, value));
	    ImageExprParse::addNode (lvalp->val);
	    return LITERAL;
	  }
{DOUBLE}  {
            imageExprGramPosition() += yyleng;
            lvalp->val = new ImageExprParse (atof(ImageExprGramtext));
	    ImageExprParse::addNode (lvalp->val);
	    return LITERAL;
	  }
{FLOAT}   {
            imageExprGramPosition() += yyleng;
            lvalp->val = new ImageExprParse (Float(atof(ImageExprGramtext)));
	    ImageExprParse::addNode (lvalp->val);
	    return LITERAL;
	  }
{INT}     {
            imageExprGramPosition() += yyleng;
            Int ival = atoi(ImageExprGramtext);
            Double dval = atof(ImageExprGramtext);
            if (ival < dval-0.1  ||  ival > dval+0.1) {
                lvalp->val = new ImageExprParse (dval);
            } else {
                lvalp->val = new ImageExprParse (ival);
            }
	    ImageExprParse::addNode (lvalp->val);
            return LITERAL;
	  }
{TRUE}    {
            imageExprGramPosition() += yyleng;
            lvalp->val = new ImageExprParse (True);
	    ImageExprParse::addNode (lvalp->val);
	    return LITERAL;
	  }
{FALSE}   {
            imageExprGramPosition() += yyleng;
            lvalp->val = new ImageExprParse (False);
	    ImageExprParse::addNode (lvalp->val);
	    return LITERAL;
	  }

{INDEXIN} {
            imageExprGramPosition() += yyleng;
            return INDEXIN;
	  }
{INDEXNOTIN} {
            imageExprGramPosition() += yyleng;
            return INDEXNOTIN;
	  }
{INDEXN}  {
            imageExprGramPosition() += yyleng;
            Int ival = atoi(ImageExprGramtext+5);
            lvalp->val = new ImageExprParse (ival);
	    ImageExprParse::addNode (lvalp->val);
            return INDEXN;
	  }
{IN}      {
            imageExprGramPosition() += yyleng;
            return IN;
	  }
{NOT}     {
            imageExprGramPosition() += yyleng;
            return NOT;
	  }

 /* A simple name can be name of constant, function, or lattice */
{NAME}    {
            imageExprGramPosition() += yyleng;
            lvalp->val = new ImageExprParse (ImageExprGramtext);
	    ImageExprParse::addNode (lvalp->val);
	    return NAME;
	  }

 /* A temporary image number can be given */
{TMPNAME} {
            imageExprGramPosition() += yyleng;
            lvalp->val = new ImageExprParse (atoi(ImageExprGramtext+1));
	    ImageExprParse::addNode (lvalp->val);
	    return NAME;
	  }

 /* A temporary region number can be given */
{TMPREGION} {
            imageExprGramPosition() += yyleng;
            lvalp->val = new ImageExprParse (atoi(ImageExprGramtext+2));
	    ImageExprParse::addNode (lvalp->val);
	    return TMPREG;
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
 Furthermore a name can look like ::region or lattice::region indicating
 that a region is given.
 */
{ESCNAME} {
            imageExprGramPosition() += yyleng;
            lvalp->val = new ImageExprParse
                             (imageExprGramRemoveEscapes (ImageExprGramtext));
	    ImageExprParse::addNode (lvalp->val);
	    return LATNAME;
	  }
{COLONNAME} {
            imageExprGramPosition() += yyleng;
            lvalp->val = new ImageExprParse
                             (imageExprGramRemoveEscapes (ImageExprGramtext));
	    ImageExprParse::addNode (lvalp->val);
	    return LATNAME;
	  }
{STRING}  {
            imageExprGramPosition() += yyleng;
            lvalp->val = new ImageExprParse
                             (imageExprGramRemoveQuotes (ImageExprGramtext));
	    ImageExprParse::addNode (lvalp->val);
	    return LATNAME;
	  }

"=="      { imageExprGramPosition() += yyleng; return EQ; }
">="      { imageExprGramPosition() += yyleng; return GE; }
">"       { imageExprGramPosition() += yyleng; return GT; }
"<="      { imageExprGramPosition() += yyleng; return LE; }
"<"       { imageExprGramPosition() += yyleng; return LT; }
"!="      { imageExprGramPosition() += yyleng; return NE; }
"&&"      { imageExprGramPosition() += yyleng; return AND; }
"||"      { imageExprGramPosition() += yyleng; return OR; }
"!"       { imageExprGramPosition() += yyleng; return NOT; }
"^"       { imageExprGramPosition() += yyleng; return POWER; }
"*"       { imageExprGramPosition() += yyleng; return TIMES; }
"/"       { imageExprGramPosition() += yyleng; return DIVIDE; }
"%"       { imageExprGramPosition() += yyleng; return MODULO; }
"+"       { imageExprGramPosition() += yyleng; return PLUS; }
"-"       { imageExprGramPosition() += yyleng; return MINUS; }
"("       { imageExprGramPosition() += yyleng; return LPAREN; }
")"       { imageExprGramPosition() += yyleng; return RPAREN; }
"["       { imageExprGramPosition() += yyleng; return LBRACKET; }
"]"       { imageExprGramPosition() += yyleng; return RBRACKET; }
","       { imageExprGramPosition() += yyleng; return COMMA; }
":"       { imageExprGramPosition() += yyleng; return COLON; }

 /* Whitespace is skipped */
{WHITE}   { imageExprGramPosition() += yyleng; }

 /* An unterminated string is an error */
{USTRING} { throw (AipsError ("ImageExprParse: Unterminated string")); }

 /* terminate on EOF */
<<EOF>>   { yyterminate(); }

 /* Any other character is invalid */
.         { return YYERRCODE; }

%%
