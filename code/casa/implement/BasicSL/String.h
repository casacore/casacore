//# String.h: String classes
//# Copyright (C) 1992,1993,1994,1995,1996,1997,1998
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
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#if !defined (AIPS_STRING_H)
#define AIPS_STRING_H

//# String classes, adapted from gnu string classes.

//# This is the gnu string implementation,
//# modified by AIPS++ to use aips++ style exceptions, move some things out
//# of line, etc.

#include <aips/aips_exit.h>
#include <iostream.h>
#include <strstream.h>
#include <aips/aips.h>
#include <aips/Utilities/Regex.h>

extern void stringThrowError(const char *);

// <summary> 
// StrRep: The internal String representation which handles most operations.
// </summary>

// <use visibility=local>

// <reviewed reviewer="Timothy P. P. Roberts, troberts@NRAO.edu" date="Mon 1995/03/27 15:57:01 GMT" tests="tString.cc" demos="">
// </reviewed>

// <prerequisite>
//   <li> none
// </prerequisite>
//
// <etymology>
// The StrRep struct name is a concatination of String Representation.  This
// is to indicate that StrReps handle most of the internal functionality of
// the String class.
// </etymology>
//
// <synopsis> 
// The user never uses StrRep directly - all processing is done at the String
// or SubString class level.
// </synopsis> 
//
// <motivation>
// By isolating the String functionality into a set of simple processes on a 
// small set of data elements the String class is broken into levels that may
// be easily coded, optimized and debugged.  Thus, StrReps may be changed 
// without changing the users interface.
// </motivation>
//
// <todo asof="Mon 1995/03/27 15:57:01 GMT">
//   <li> none noted.
// </todo>

struct StrRep
{

  // string length 
  unsigned int len;

  // allocated space
  unsigned int sz;

  // the string starts here 
  // (at least 1 char for trailing null)
  // allocated & expanded via non-public fcts
  char s[1];
};

//# primitive ops on StrReps -- nearly all String fns go through these.
StrRep *Salloc(StrRep*, const char*, int, int);
StrRep *Scopy(StrRep*, StrRep*);
StrRep *Sresize(StrRep*, int);
StrRep *Scat(StrRep*, const char*, int, const char*, int);
StrRep *Scat(StrRep*, const char*, int,const char*,int, const char*,int);
StrRep *Sprepend(StrRep*, const char*, int);
StrRep *Sreverse(StrRep*, StrRep*);
StrRep *Supcase(StrRep*, StrRep*);
StrRep *Sdowncase(StrRep*, StrRep*);
StrRep *Scapitalize(StrRep*, StrRep*);


//# These classes need to be defined in the order given

class String;
class SubString;

// <summary> 
// SubString: A reference to all or part of an existing String.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Timothy P. P. Roberts, troberts@NRAO.edu" date="Mon 1995/03/27 15:57:01 GMT" tests="tString.cc" demos="">
// </reviewed>

// <prerequisite>
//   <li> String
//   <li> Regex - the regular expressions class
// </prerequisite>
//
// <etymology>
// The SubString class name is an indication of its role as a reference into
// a small part of a whole String.  
// </etymology>
//
// <synopsis> 
// There isn't a public SubString constructor - SubStrings are always 
// created by a String member function.  Since SubStrings are a reference to 
// part of an already existing String, changing the value of the SubString 
// will change the value of the parent String.  This was the intent behind 
// their creation.  SubStrings allow you to change the rapidly varying part
// of a String without needing to concatinate the String back together.
// <srcblock> 
//     String myOutput = "Error: class foo has thrown an exception.";
//     myOutput.at("foo") = "Oinker";
//     cout << myOutput << endl;
//     myOutput.at("Oinker") = "Swine";
//     cout << myOutput << endl;
// </srcblock> Would print the following:
//
// Error: class Oinker has thrown an exception.
// Error: class Swine has thrown an exception.
//
// The SubString class itself has many member functions which check if a set
// of characters is "contain"ed within a SubString or allow "match"ing of a 
// regular expression.  Length of the SubString, a check to see if the 
// SubString is empty and conversion to "const char *" are provided, as well.
// </synopsis> 
//
// <example>
// <srcblock>
// String MailingLabel("Dr. Richard Cranium, 1003 Lopezville Road, Socorro");
// MailingLabel += ", NM  87801";
// // we will extract some SubString "fields"
// // the part which holds "Richard Cranium" is attached
// SubString name = MailingLabel.at("Richard Cranium");
// // the part before ", Soc" and after skipping 20 characters of name
// SubString street = MailingLabel.before(", Soc", 20);
// // the through to ", NM" and after skipping 43 previous characters.
// SubString city = MailingLabel.through(", NM, 43);
// // the part from "NM" to the end
// SubString zip = MailingLabel.from("NM");
// // the possible extra part after the zip code (e.g. zip+4)
// SubString plusFour = MailingLabel.after("801");
// // Now we replace as needed - assume an Array of Strings is our data source
// for(int i = 0; i<5, i++){
//    name = creditCardData(i,0);
//    street = creditCardData(i,1);
//    city = creditCardData(i,2);
//    zip = creditCardData(i,3);
//    plusFour = creditCardData(i,4);
//    fprintf(Harass, "%s/n", MailingLabel.chars());
// }
// </srcblock>
// </example>
//
// <motivation>
// SubStrings were created to allow the user to access parts of a String 
// without changing the String's coded appearence.  This is part of the GNU
// C++ library and has been converted to use AIPS++ exception handling.
// </motivation>
//
// <todo asof="Mon 1995/03/27 15:57:01 GMT">
//   <li> none noted
// </todo>

class SubString 
{
public:

  friend class String;

  // destructor
  ~SubString();

  // assignment operator
  // <group>
  void operator = (const String &y);
  void operator = (const SubString &y);
  void operator = (const char *t);
  void operator = (char c);
  // </group>

  // ostream operator for output.
  friend ostream &operator<<(ostream &s, const SubString &x);

  // Return 1 if the target appears anywhere in SubString; else 0.
  //<group>
  int contains(char c) const;
  int contains(const String &y) const;
  int contains(const SubString &y) const;
  int contains(const char *t) const;
  int contains(const Regex &r) const;
  //</group>

  // Return 1 if the target matches entire SubString.
  int matches(const Regex &r) const;

  // length of the SubString
  unsigned int length() const;

  // return 1 if empty, 0 if filled.
  int empty() const;

  // convert to char* for use in "C" type character arguments
  const char *chars() const;

  // indexing ok
  int OK() const; 

protected:

  // The String I'm a substring of
  String &S;

  // starting position in S's rep
  unsigned int pos;

  // length of substring
  unsigned int len;

  void assign(StrRep *, const char *, int = -1);

  // <note> There are no public constructors for SubString.
  //        SubStrings are always created via String operations.</note>
  //<group> 
  SubString(String &x, unsigned int p, unsigned int l);
  SubString(const SubString &x);
  //</group>

};


// <summary> 
// String: the storage and methods of handling collections of characters.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Timothy P. P. Roberts, troberts@NRAO.edu" date="Mon 1995/03/27 15:57:01 GMT" tests="tString.cc" demos="">
// </reviewed>

// <prerequisite>
//   <li> Regex - the regular expressions class
// </prerequisite>
//
// <etymology>
// The String class name is a continuation of the "C" language custom of
// refering to collections of characters as "strings of characters".
// </etymology>
//
// <synopsis> 
// The String class may be instantiated in many ways:
// <ol><li> A single character - <src>String myChar("C");</src>
// <li> A char* argument - <src>String myWord("Yowza");</src>
// <li> The first n chararcters of a pre-existing string - 
// <src>String myFoo("fooey", 3);</src>
// </ol> As well as the copy, default and SubString constructors.
//
// A String may be concatinated with another object (String, SubString, or 
// char*) with either prepending or postpending.  A search for the position
// of a character within a String may return its position, a Bool that it
// is contained within or a Bool confirming your guess at the charater's 
// position is correct.  A check of the frequency of occurance of a character
// within a String will return the number of occurances.  
// 
// SubStrings may be extracted from Strings at, before, through, from and 
// after a starting position within the String.  Deletion of characters is
// possible after a given position within the String. Global substitution
// of characters within a String is provided, as well.  Splitting of Strings 
// into a carray of Strings is possible, based upon a given separator 
// character, with a return value of the number of elements split.  The joining
// together of the elements of a carray of Strings into one String is possible.
// 
// Finally, transformations of case and conversions of type are provided. 
// </synopsis> 
//
// <example>
// <srcblock>
// // Let's start with a simple string.
// String myString("the time");
// // add some more on the end...
// myString += " for all good men";
// // prepend some on the front...
// myString.prepend("Now is ");
// // do some concatination...
// String evenMore;
// cat(myString, " to come to", evenMore);
// // do some three way concatination
// String allKeys, finishIt(" their country.");
// cat(evenMore, "the aid of", finishIt, allKeys);
// // find the spot where we put something earlier
// int position = allKeys.index(finishIt);
// // find if the word is in the String...
// int query = myString.contains("good men");
// // ask if the position we think is true is correct...
// int answer = allKeys.matches(finishIt, position);
// // How many spaces are in our phrase?
// int spacesCount = allKeys.freq(" ");
// </scrblock>
// SubStrings are references to all or part of a String.  An obvious advantage
// of this would be attaching the small, rapidly changing part (i.e. a name) 
// within a lengthy String to a SubString.  Thus, the SubString may be changed 
// without affecting the larger String's handling.  
// <srcblock>
// String MailingLabel("Dr. Richard Cranium, 1003 Lopezville Road, Socorro");
// MailingLabel += ", NM  87801";
// // we will extract some SubString "fields"
// // the part which holds "Richard Cranium" is attached
// SubString name = MailingLabel.at("Richard Cranium");
// // the part before ", Soc" and after skipping 20 characters of name
// SubString street = MailingLabel.before(", Soc", 20);
// // the through to ", NM" and after skipping 43 previous characters.
// SubString city = MailingLabel.through(", NM, 43);
// // the part from "NM" to the end
// SubString zip = MailingLabel.from("NM");
// // the possible extra part after the zip code (e.g. zip+4)
// SubString plusFour = MailingLabel.after("801");
// // Now we replace as needed - assume an Array of Strings is our data source
// for(int i = 0; i<5, i++){
//    name = creditCardData(i,0);
//    street = creditCardData(i,1);
//    city = creditCardData(i,2);
//    zip = creditCardData(i,3);
//    plusFour = creditCardData(i,4);
//    fprintf(Harass, "%s/n", MailingLabel.chars());
// }
// </srcblock>
// </example>
//
// <motivation>
// The String class eases the handling of characters within the AIPS++ 
// environment.  The class was written as part of the GNU C++ library and 
// adapted to use the AIPS++ exception handling.
// </motivation>
//
// <todo asof="Mon 1995/03/27 15:57:01 GMT">
//   <li> none noted
// </todo>

class String
{
public:

  friend class SubString;

  // default constructors
  String();

  // copy constructor
  String(const String &x);

  // SubString constructor
  String(const SubString &x);

  // const char * constructor
  String(const char *t);

  // construct String of len number of characters from beginning of char *t
  String(const char *t, int len);

  // single char constructor
  String(char c);

  // Create a string from the characters in an ostrstream. The ostrstream must be
  // dynamic, that is created with its default constructor. After this call
  // <src>os</src> should not be used any more as its internal buffer has been
  // deleted.
  String(ostrstream &os);

  // destructor
  ~String();

  // Assignment operator
  // <group name=assign>
  void operator=(const String &y);
  void operator=(const char *y);
  void operator=(char  c);
  void operator=(const SubString &y);
  // </group>

  // This is a concatenation operator.
  //<group name=concate_ops>
  void operator+=(const String &y); 
  void operator+=(const SubString &y);
  void operator+=(const char *t);
  void operator+=(char c);
  //</group> 

  // This is a synonym for at (int pos, int len).
  SubString operator()(int pos, int len);

  // Stream operator
  // <group name=streams>
  friend ostream &operator<<(ostream &s, const String &x);
  friend ostream &operator<<(ostream &s, const SubString &x);
  friend istream &operator>>(istream &s, String &x);
  // </group>

  // Concatinate by prepending the argument onto String
  //<group name=concatenation_method>
  void prepend(const String &y); 
  void prepend(const SubString &y);
  void prepend(const char *t);
  void prepend(char c);
  //</group> 


  // Return the position of the target in the string or -1 for failure.
  //<group name=index>
  int index(char c, int startpos = 0) const; 
  int index(const String &y, int startpos = 0) const; 
  int index(const SubString &y, int startpos = 0) const; 
  int index(const char *t, int startpos = 0) const; 
  int index(const Regex &r, int startpos = 0) const; 
  //</group>

  // Return 1 if the target appears anyhere in the String; else 0.
  //<group name=contains>
  int contains(char c) const;
  int contains(const String &y) const;
  int contains(const SubString &y) const;
  int contains(const char *t) const;
  int contains(const Regex &r) const;
  //</group> 

  // Return 1 if the target appears anywhere after position pos 
  // (or before, if pos is negative) in String; else 0.
  //<group name=contains_pos>
  int contains(char c, int pos) const;
  int contains(const String &y, int pos) const;
  int contains(const SubString &y, int pos) const;
  int contains(const char *t, int pos) const;
  int contains(const Regex &r, int pos) const;
  //</group>

  // Return 1 if the target appears at position pos in String; else 0.
  //<group name=matches>
  int matches(char c, int pos = 0) const;
  int matches(const String &y, int pos = 0) const;
  int matches(const SubString &y, int pos = 0) const;
  int matches(const char *t, int pos = 0) const;
  int matches(const Regex &r, int pos = 0) const;
  //</group>

  //  Return the number of occurences of target in String.
  //<group name=freq>
  int freq(char c) const; 
  int freq(const String &y) const;
  int freq(const SubString &y) const;
  int freq(const char *t) const;
  //</group>

  // <note> You can't take a substring of a const String, since this leaves 
  // open the possiblility of indirectly modifying the String through the 
  // SubString </note>
  //<group name=SubString>
  // extract the SubString "at" the argument's position.
  // <group name=at>
  SubString at(int pos, int len);
  SubString at(const String &x, int startpos = 0); 
  SubString at(const SubString &x, int startpos = 0); 
  SubString at(const char *t, int startpos = 0);
  SubString at(char c, int startpos = 0);
  SubString at(const Regex &r, int startpos = 0); 
  //</group>

  // Start at startpos and extract the SubString "before" the argument's 
  // position, exclusive.
  // <group name=before>
  SubString before(int pos);
  SubString before(const String &x, int startpos = 0);
  SubString before(const SubString &x, int startpos = 0);
  SubString before(const char *t, int startpos = 0);
  SubString before(char c, int startpos = 0);
  SubString before(const Regex &r, int startpos = 0);
  //</group>

  // Start at startpos and extract the SubString "through" to the argument's 
  // position, inclusive.
  // <group name=through>
  SubString through(int pos);
  SubString through(const String &x, int startpos = 0);
  SubString through(const SubString &x, int startpos = 0);
  SubString through(const char *t, int startpos = 0);
  SubString through(char c, int startpos = 0);
  SubString through(const Regex &r, int startpos = 0);
  //</group>

  // Start at startpos and extract the SubString "from" the argument's 
  // position, inclusive, to the String's end.
  // <group name=from>
  SubString from(int pos);
  SubString from(const String &x, int startpos = 0);
  SubString from(const SubString &x, int startpos = 0);
  SubString from(const char *t, int startpos = 0);
  SubString from(char c, int startpos = 0);
  SubString from(const Regex &r, int startpos = 0);
  //</group>

  // Start at startpos and extract the SubString "after" the argument's 
  // position, exclusive, to the String's end.
  // <group name=after>
  SubString after(int pos);
  SubString after(const String &x, int startpos = 0);
  SubString after(const SubString &x, int startpos = 0);
  SubString after(const char *t, int startpos = 0);
  SubString after(char c, int startpos = 0);
  SubString after(const Regex &r, int startpos = 0);
  //</group>
  //</group> 

  // Delete len chars starting at pos.
  void del(int pos, int len);

  // Delete the first occurrence of target after startpos.
  //<group name=del_after>
  void del(const String &y, int startpos = 0);
  void del(const SubString &y, int startpos = 0);
  void del(const char *t, int startpos = 0);
  void del(char c, int startpos = 0);
  void del(const Regex &r, int startpos = 0);
  //</group> 

  // Global substitution: substitute all occurrences of pat with repl.
  //<group name=gsub>
  int gsub(const String &pat, const String &repl);
  int gsub(const SubString &pat, const String &repl);
  int gsub(const char *pat, const String &repl);
  int gsub(const char *pat, const char *repl);
  int gsub(const Regex &pat, const String &repl);
  //</group>

  // return a reference to the char element at position "i".
  // <group>
  char &operator[](int i);
  char operator[](int i) const;
  // </group>

  // return the char element at position "i".
  char elem(unsigned int i) const;

  // return the first char of the String
  char firstchar() const;

  // return the last char of the String
  char lastchar() const;

  // convert to const char * with bogus cast e.g. (const char*)myString.
  operator const char*() const;

  // return const char *
  const char *chars() const;

  // return the length of the String
  unsigned int length() const;

  // return 0 if the String is filled, 1 if empty
  int empty() const;

  // Status
  int OK() const;

  // Preallocate some space for String.
  void alloc(int newsize);

  // Report the current allocation (not length!).
  int allocation() const;

  // hash function from IV strings (pjt - march 1992).
  unsigned long	hash() const;

  // internal transformation to reverse order of String.
  void reverse();

  // internal transformation to uppercase of String.
  void upcase();

  // internal transformation to lowercase of String.
  void downcase();

  // internal transformation to capitalization of String.
  void capitalize();

  // global function which returns a transformation to reverse order of String.
  friend String reverse(const String &x);

  // global function  which returns a transformation to uppercase of String.
  friend String upcase(const String &x);

  // global function  which returns a transformation to lowercase of String.
  friend String downcase(const String &x);

  // global function  which returns a transformation to capitalization of 
  // String.
  friend String capitalize(const String &x);

  // Global function which concatenates the first 2 arguments,
  // and stores the result in the last argument.
  //<group name=concatenation_single>
  friend void cat(const String&, const String&, String&);
  friend void cat(const String&, const SubString&, String&);
  friend void cat(const String&, const char*, String&);
  friend void cat(const String&, char, String&);

  friend void cat(const SubString&, const String&, String&);
  friend void cat(const SubString&, const SubString&, String&);
  friend void cat(const SubString&, const char*, String&);
  friend void cat(const SubString&, char, String&);

  friend void cat(const char*, const String&, String&);
  friend void cat(const char*, const SubString&, String&);
  friend void cat(const char*, const char*, String&);
  friend void cat(const char*, char, String&);
  //</group>

  // Global function which concatenates the first 3 arguments, and stores 
  // the result in the last argument.
  //<group name=concatenation_double>
  friend void cat(const String&, const String&, const String&, String&);
  friend void cat(const String&, const String&, const SubString&, String&);
  friend void cat(const String&, const String&, const char*, String&);
  friend void cat(const String&, const String&, char, String&);
  friend void cat(const String&, const SubString&, const String&, String&);
  friend void cat(const String&, const SubString&, const SubString&, String&);
  friend void cat(const String&, const SubString&, const char*, String&);
  friend void cat(const String&, const SubString&, char, String&);
  friend void cat(const String&, const char*, const String&, String&);
  friend void cat(const String&, const char*, const SubString&, String&);
  friend void cat(const String&, const char*, const char*, String&);
  friend void cat(const String&, const char*, char, String&);

  friend void cat(const char*, const String&, const String&, String&);
  friend void cat(const char*, const String&, const SubString&, String&);
  friend void cat(const char*, const String&, const char*, String&);
  friend void cat(const char*, const String&, char, String&);
  friend void cat(const char*, const SubString&, const String&, String&);
  friend void cat(const char*, const SubString&, const SubString&, String&);
  friend void cat(const char*, const SubString&, const char*, String&);
  friend void cat(const char*, const SubString&, char, String&);
  friend void cat(const char*, const char*, const String&, String&);
  friend void cat(const char*, const char*, const SubString&, String&);
  friend void cat(const char*, const char*, const char*, String&);
  friend void cat(const char*, const char*, char, String&);
  //</group>

  // Global function which splits the String into carray res at separators
  // and returns the number of elements.
  //<group name=split>
  friend int split(const String &x, String res[], int maxn, const String &sep);
  friend int split(const String &x, String res[], int maxn, const Regex & sep);
  //</group> 

  friend String common_prefix(const String &x, const String &y, 
			      int startpos = 0);
  friend String common_suffix(const String &x, const String &y, 
			      int startpos = -1);
  friend String replicate(char c, int n);
  friend String replicate(const String &y, int n);
  friend String join(String src[], int n, const String &sep);


  // IO
  friend int readline(istream &s, String &x, char terminator = '\n',
		      int discard_terminator = 1);

protected:

  // Strings are pointers to their representations
  StrRep *rep;

  // This is a helper function.
  //<group name=helperfunc>
  int search(int, int, const char*, int = -1) const;
  int search(int, int, char) const;
  int match(int, int, int, const char*, int = -1) const;
  int _gsub(const char*, int, const char *,int);
  int _gsub(const Regex&, const char*, int);
  SubString _substr(unsigned int, unsigned int);
  //</group>

};

// Initialize the String type() rtti functions.
rtti_dcl_init(String);

// This is here for backward compatibility.
typedef String StrTmp;

//<summary>
// Global Functions which compare Strings
//</summary>

// The global function "compare" returns 1 if the arguments are equal,
// 0 otherwise.
// <group name=compare>
int compare(const String &x, const String &y);
int compare(const String &x, const SubString &y);
int compare(const String &x, const char *y);
int compare(const SubString &x, const String &y);
int compare(const SubString &x, const SubString &y);
int compare(const SubString &x, const char *y);

// this version ignores case
int fcompare(const String &x, const String &y);
//</group>

extern StrRep _nilStrRep;
extern String _nilString;

//<summary>
// Global Functions which concatinate Strings with the +operator.
//</summary>

// The global "+" operator is a method of concatination
// <group name=operat>
String operator+(const String &x, const String &y);
String operator+(const String &x, const SubString &y);
String operator+(const String &x, const char *y);
String operator+(const String &x, char y);
String operator+(const SubString &x, const String &y);
String operator+(const SubString &x, const SubString &y);
String operator+(const SubString &x, const char *y);
String operator+(const SubString &x, char y);
String operator+(const char *x, const String &y);
String operator+(const char *x, const SubString &y);
// </group>

//<summary>
// Global Functions which compare Strings with the boolean math comparitor
// operators.
//</summary>

// The global "less than, greater than, equal to and not equal to" operators
// return 1 if fulfilled, 0 otherwise.
// <group name=comparitor>

inline int operator==(const String &x, const String &y) 
{
  return compare(x, y) == 0; 
}

inline int operator!=(const String &x, const String &y)
{
  return compare(x, y) != 0; 
}

inline int operator>(const String &x, const String &y)
{
  return compare(x, y) > 0; 
}

inline int operator>=(const String &x, const String &y)
{
  return compare(x, y) >= 0; 
}

inline int operator<(const String &x, const String &y)
{
  return compare(x, y) < 0; 
}

inline int operator<=(const String &x, const String &y)
{
  return compare(x, y) <= 0; 
}

inline int operator==(const String &x, const SubString &y) 
{
  return compare(x, y) == 0; 
}

inline int operator!=(const String &x, const SubString &y)
{
  return compare(x, y) != 0; 
}

inline int operator>(const String &x, const SubString &y) 
{
  return compare(x, y) > 0; 
}

inline int operator>=(const String &x, const SubString &y)
{
  return compare(x, y) >= 0; 
}

inline int operator<(const String &x, const SubString &y) 
{
  return compare(x, y) < 0; 
}

inline int operator<=(const String &x, const SubString &y)
{
  return compare(x, y) <= 0; 
}

inline int operator==(const String &x, const char *t) 
{
  return compare(x, t) == 0; 
}

inline int operator!=(const String &x, const char *t) 
{
  return compare(x, t) != 0; 
}

inline int operator>(const String &x, const char *t) 
{
  return compare(x, t) > 0; 
}

inline int operator>=(const String &x, const char *t) 
{
  return compare(x, t) >= 0; 
}

inline int operator<(const String &x, const char *t) 
{
  return compare(x, t) < 0; 
}

inline int operator<=(const String &x, const char *t) 
{
  return compare(x, t) <= 0; 
}

inline int operator==(const SubString &x, const String &y) 
{
  return compare(y, x) == 0; 
}

inline int operator!=(const SubString &x, const String &y)
{
  return compare(y, x) != 0;
}

inline int operator>(const SubString &x, const String &y) 
{
  return compare(y, x) < 0;
}

inline int operator>=(const SubString &x, const String &y) 
{
  return compare(y, x) <= 0;
}

inline int operator<(const SubString &x, const String &y) 
{
  return compare(y, x) > 0;
}

inline int operator<=(const SubString &x, const String &y) 
{
  return compare(y, x) >= 0;
}

inline int operator==(const SubString &x, const SubString &y) 
{
  return compare(x, y) == 0; 
}

inline int operator!=(const SubString &x, const SubString &y)
{
  return compare(x, y) != 0;
}

inline int operator>(const SubString &x, const SubString &y) 
{
  return compare(x, y) > 0;
}

inline int operator>=(const SubString &x, const SubString &y)
{
  return compare(x, y) >= 0;
}

inline int operator<(const SubString &x, const SubString &y) 
{
  return compare(x, y) < 0;
}

inline int operator<=(const SubString &x, const SubString &y)
{
  return compare(x, y) <= 0;
}

inline int operator==(const SubString &x, const char *t) 
{
  return compare(x, t) == 0; 
}

inline int operator!=(const SubString &x, const char *t) 
{
  return compare(x, t) != 0;
}

inline int operator>(const SubString &x, const char *t) 
{
  return compare(x, t) > 0; 
}

inline int operator>=(const SubString &x, const char *t) 
{
  return compare(x, t) >= 0; 
}

inline int operator<(const SubString &x, const char *t) 
{
  return compare(x, t) < 0; 
}

inline int operator<=(const SubString &x, const char *t) 
{
  return compare(x, t) <= 0; 
}
//</group>


//#-----------------begin inlining------------------------------------

//#
//# status reports, needed before defining other things
//#

inline unsigned int String::length() const { return rep->len; }
inline int String::empty() const { return rep->len == 0; }
inline const char *String::chars() const { return &(rep->s[0]); }
inline int String::allocation() const { return rep->sz; }
inline void String::alloc(int newsize) { rep = Sresize(rep, newsize); }

inline unsigned int SubString::length() const { return len; }
inline int SubString::empty() const { return len == 0; }
inline const char *SubString::chars() const { return &(S.rep->s[pos]); }


//#
//# constructors
//#

inline String::String(const String &x) 
  : rep(Scopy(0, x.rep)) {}
inline String::String(const char *t) 
  : rep(Salloc(0, t, -1, -1)) {}
inline String::String(const char *t, int tlen)
  : rep(Salloc(0, t, tlen, tlen)) {}
inline String::String(const SubString &y)
  : rep(Salloc(0, y.chars(), y.length(), y.length())) {}
inline String::String(char c) 
  : rep(Salloc(0, &c, 1, 1)) {}

inline SubString::SubString(const SubString &x)
  :S(x.S), pos(x.pos), len(x.len) {}
inline SubString::SubString(String &x, unsigned int first, unsigned int l)
  :S(x), pos(first), len(first+l > x.length()  ?  x.length()-first : l) {}

inline SubString::~SubString() {}

//#
//# assignment
//#

inline void String::operator = (const String &y)
{ 
  rep = Scopy(rep, y.rep);
}

inline void String::operator=(const char *t)
{
  rep = Salloc(rep, t, -1, -1); 
}

inline void String::operator=(const SubString &y)
{
  rep = Salloc(rep, y.chars(), y.length(), y.length());
}

inline void String::operator=(char c)
{
  rep = Salloc(rep, &c, 1, 1); 
}


inline void SubString::operator = (const char *ys)
{
  assign(0, ys);
}

inline void SubString::operator = (char ch)
{
  assign(0, &ch, 1);
}

inline void SubString::operator = (const String &y)
{
  assign(y.rep, y.chars(), y.length());
}

inline void SubString::operator = (const SubString &y)
{
  assign(y.S.rep, y.chars(), y.length());
}


//#
//# Zillions of cats...
//#

inline void cat(const String &x, const String &y, String &r)
{
  r.rep = Scat(r.rep, x.chars(), x.length(), y.chars(), y.length());
}

inline void cat(const String &x, const SubString &y, String &r)
{
  r.rep = Scat(r.rep, x.chars(), x.length(), y.chars(), y.length());
}

inline void cat(const String &x, const char *y, String &r)
{
  r.rep = Scat(r.rep, x.chars(), x.length(), y, -1);
}

inline void cat(const String &x, char y, String &r)
{
  r.rep = Scat(r.rep, x.chars(), x.length(), &y, 1);
}

inline void cat(const SubString &x, const String &y, String &r)
{
  r.rep = Scat(r.rep, x.chars(), x.length(), y.chars(), y.length());
}

inline void cat(const SubString &x, const SubString &y, String &r)
{
  r.rep = Scat(r.rep, x.chars(), x.length(), y.chars(), y.length());
}

inline void cat(const SubString &x, const char *y, String &r)
{
  r.rep = Scat(r.rep, x.chars(), x.length(), y, -1);
}

inline void cat(const SubString &x, char y, String &r)
{
  r.rep = Scat(r.rep, x.chars(), x.length(), &y, 1);
}

inline void cat(const char *x, const String &y, String &r)
{
  r.rep = Scat(r.rep, x, -1, y.chars(), y.length());
}

inline void cat(const char *x, const SubString &y, String &r)
{
  r.rep = Scat(r.rep, x, -1, y.chars(), y.length());
}

inline void cat(const char *x, const char *y, String &r)
{
  r.rep = Scat(r.rep, x, -1, y, -1);
}

inline void cat(const char *x, char y, String &r)
{
  r.rep = Scat(r.rep, x, -1, &y, 1);
}

inline void cat(const String &a, const String &x, const String &y, String &r)
{
  r.rep = Scat(r.rep, a.chars(), a.length(), x.chars(), x.length(), y.chars(), y.length());
}

inline void cat(const String &a, const String &x, const SubString &y, String &r)
{
  r.rep = Scat(r.rep, a.chars(), a.length(), x.chars(), x.length(), y.chars(), y.length());
}

inline void cat(const String &a, const String &x, const char *y, String &r)
{
  r.rep = Scat(r.rep, a.chars(), a.length(), x.chars(), x.length(), y, -1);
}

inline void cat(const String &a, const String &x, char y, String &r)
{
  r.rep = Scat(r.rep, a.chars(), a.length(), x.chars(), x.length(), &y, 1);
}

inline void cat(const String &a, const SubString &x, const String &y, String &r)
{
  r.rep = Scat(r.rep, a.chars(), a.length(), x.chars(), x.length(), y.chars(), y.length());
}

inline void cat(const String &a, const SubString &x, const SubString &y, String &r)
{
  r.rep = Scat(r.rep, a.chars(), a.length(), x.chars(), x.length(), y.chars(), y.length());
}

inline void cat(const String &a, const SubString &x, const char *y, String &r)
{
  r.rep = Scat(r.rep, a.chars(), a.length(), x.chars(), x.length(), y, -1);
}

inline void cat(const String &a, const SubString &x, char y, String &r)
{
  r.rep = Scat(r.rep, a.chars(), a.length(), x.chars(), x.length(), &y, 1);
}

inline void cat(const String &a, const char *x, const String &y, String &r)
{
  r.rep = Scat(r.rep, a.chars(), a.length(), x, -1, y.chars(), y.length());
}

inline void cat(const String &a, const char *x, const SubString &y, String &r)
{
  r.rep = Scat(r.rep, a.chars(), a.length(), x, -1, y.chars(), y.length());
}

inline void cat(const String &a, const char *x, const char *y, String &r)
{
  r.rep = Scat(r.rep, a.chars(), a.length(), x, -1, y, -1);
}

inline void cat(const String &a, const char *x, char y, String &r)
{
  r.rep = Scat(r.rep, a.chars(), a.length(), x, -1, &y, 1);
}


inline void cat(const char *a, const String &x, const String &y, String &r)
{
  r.rep = Scat(r.rep, a, -1, x.chars(), x.length(), y.chars(), y.length());
}

inline void cat(const char *a, const String &x, const SubString &y, String &r)
{
  r.rep = Scat(r.rep, a, -1, x.chars(), x.length(), y.chars(), y.length());
}

inline void cat(const char *a, const String &x, const char *y, String &r)
{
  r.rep = Scat(r.rep, a, -1, x.chars(), x.length(), y, -1);
}

inline void cat(const char *a, const String &x, char y, String &r)
{
  r.rep = Scat(r.rep, a, -1, x.chars(), x.length(), &y, 1);
}

inline void cat(const char *a, const SubString &x, const String &y, String &r)
{
  r.rep = Scat(r.rep, a, -1, x.chars(), x.length(), y.chars(), y.length());
}

inline void cat(const char *a, const SubString &x, const SubString &y, String &r)
{
  r.rep = Scat(r.rep, a, -1, x.chars(), x.length(), y.chars(), y.length());
}

inline void cat(const char *a, const SubString &x, const char *y, String &r)
{
  r.rep = Scat(r.rep, a, -1, x.chars(), x.length(), y, -1);
}

inline void cat(const char *a, const SubString &x, char y, String &r)
{
  r.rep = Scat(r.rep, a, -1, x.chars(), x.length(), &y, 1);
}

inline void cat(const char *a, const char *x, const String &y, String &r)
{
  r.rep = Scat(r.rep, a, -1, x, -1, y.chars(), y.length());
}

inline void cat(const char *a, const char *x, const SubString &y, String &r)
{
  r.rep = Scat(r.rep, a, -1, x, -1, y.chars(), y.length());
}

inline void cat(const char *a, const char *x, const char *y, String &r)
{
  r.rep = Scat(r.rep, a, -1, x, -1, y, -1);
}

inline void cat(const char *a, const char *x, char y, String &r)
{
  r.rep = Scat(r.rep, a, -1, x, -1, &y, 1);
}


//#
//# operator versions of cats
//#

inline void String::operator +=(const String &y)
{
  cat(*this, y, *this);
}

inline void String::operator +=(const SubString &y)
{
  cat(*this, y, *this);
}

inline void String::operator += (const char *y)
{
  cat(*this, y, *this);
}

inline void String:: operator +=(char y)
{
  cat(*this, y, *this);
}

//#
//# element extraction
//#

inline char &String::operator [] (int i) 
{ 
  if (((uInt)i) >= length()) {
    stringThrowError("invalid index");
  }
  return rep->s[i];
}

inline char String::operator [] (int i) const
{ 
  if (((uInt)i) >= length()) {
    stringThrowError("invalid index");
  }
  return rep->s[i];
}

inline char String::elem (unsigned int i) const
{ 
  if (i >= length()) {
    stringThrowError("invalid index");
  }
  return rep->s[i];
}

inline char String::firstchar() const
{ 
  return elem(0);
}

inline char String::lastchar() const
{ 
  return elem(length() - 1);
}


//#
//# searching
//#

inline int String::index(char c, int startpos) const
{
  return search(startpos, length(), c);
}

inline int String::index(const char *t, int startpos) const
{ 
  return search(startpos, length(), t);
}

inline int String::index(const String &y, int startpos) const
{ 
  return search(startpos, length(), y.chars(), y.length());
}

inline int String::index(const SubString &y, int startpos) const
{ 
  return search(startpos, length(), y.chars(), y.length());
}

inline int String::index(const Regex &r, int startpos) const
{
  int unused;  return r.search(chars(), length(), unused, startpos);
}



inline int String::contains(char c) const
{
  return search(0, length(), c) >= 0;
}

inline int String::contains(const char *t) const
{ 
  return search(0, length(), t) >= 0;
}

inline int String::contains(const String &y) const
{ 
  return search(0, length(), y.chars(), y.length()) >= 0;
}

inline int String::contains(const SubString &y) const
{ 
  return search(0, length(), y.chars(), y.length()) >= 0;
}

inline int String::contains(char c, int p) const
{
  return match(p, length(), 0, &c, 1) >= 0;
}

inline int String::contains(const char *t, int p) const
{
  return match(p, length(), 0, t) >= 0;
}

inline int String::contains(const String &y, int p) const
{
  return match(p, length(), 0, y.chars(), y.length()) >= 0;
}

inline int String::contains(const SubString &y, int p) const
{
  return match(p, length(), 0, y.chars(), y.length()) >= 0;
}

inline int String::contains(const Regex &r) const
{
  int unused;  return r.search(chars(), length(), unused, 0) >= 0;
}

inline int String::contains(const Regex &r, int p) const
{
  return r.match(chars(), length(), p) >= 0;
}


inline int String::matches(const SubString &y, int p) const
{
  return match(p, length(), 1, y.chars(), y.length()) >= 0;
}

inline int String::matches(const String &y, int p) const
{
  return match(p, length(), 1, y.chars(), y.length()) >= 0;
}

inline int String::matches(const char *t, int p) const
{
  return match(p, length(), 1, t) >= 0;
}

inline int String::matches(char c, int p) const
{
  return match(p, length(), 1, &c, 1) >= 0;
}

inline int String::matches(const Regex &r, int p) const
{
  int l = (p < 0)? -p : length() - p;
  return r.match(chars(), length(), p) == l;
}


inline int SubString::contains(const char *t) const
{ 
  return S.search(pos, pos+len, t) >= 0;
}

inline int SubString::contains(const String &y) const
{ 
  return S.search(pos, pos+len, y.chars(), y.length()) >= 0;
}

inline int SubString::contains(const SubString &y) const
{ 
  return S.search(pos, pos+len, y.chars(), y.length()) >= 0;
}

inline int SubString::contains(char c) const
{
  return S.search(pos, pos+len, c) >= 0;
}

inline int SubString::contains(const Regex &r) const
{
  int unused;  return r.search(chars(), len, unused, 0) >= 0;
}

inline int SubString::matches(const Regex &r) const
{
  return r.match(chars(), len, 0) == int(len);
}


inline int String::gsub(const String &pat, const String &r)
{
  return _gsub(pat.chars(), pat.length(), r.chars(), r.length());
}

inline int String::gsub(const SubString &pat, const String &r)
{
  return _gsub(pat.chars(), pat.length(), r.chars(), r.length());
}

inline int String::gsub(const Regex &pat, const String &r)
{
  return _gsub(pat, r.chars(), r.length());
}

inline int String::gsub(const char *pat, const String &r)
{
  return _gsub(pat, -1, r.chars(), r.length());
}

inline int String::gsub(const char *pat, const char *r)
{
  return _gsub(pat, -1, r, -1);
}



inline ostream &operator<<(ostream &s, const String &x)
{
   s << x.chars(); return s;
}



//
// This is a helper needed by at, before, etc.
//

inline SubString String::_substr(unsigned int first, unsigned int l)
{
  if (first >= length() )
    return SubString(_nilString, 0, 0) ;
  else 
    return SubString(*this, first, l);
}

#if defined(__GNUG__)
inline String &at_c(String &val) { return(val); };

inline const String &at_cc(const String &val) { return(val); };
#endif

#endif  /* AIPS_STRING_H */
