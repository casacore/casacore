//# Aipsrc.h: Class to read the aipsrc general resource files
//# Copyright (C) 1995,1996,1997
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

#if !defined(AIPS_AIPSRC)
#define AIPS_AIPSRC_H

#if defined(_AIX)
#pragma implementation ("Aipsrc.cc")
#endif

#include <aips/aips.h>
#include <aips/Utilities/String.h>
#include <aips/Containers/Block.h>

// <summary> Class to read the aipsrc general resource files </summary>

// <use visibility=export>

// <reviewed reviewer="wyoung" date="1996/11/25" tests="tAipsrc" demos="">
// </reviewed>

// <prerequisite>
//  <li> Cannot be empty
// </prerequisite>
//
// <etymology>
// A class for getting values from the aipsrc files
// </etymology>
//
// <synopsis>
// The static Aipsrc class can get information from the aipsrc resource files.
// It has the same functionality as getrc (in use for aips++ scripts).<br>
// The format of a line in a resource file is:
// <srcblock>
//	# Line starting with an # is considered a comment (as is an empty line)
//	keyword:   value
//	keyword:   value
// </srcblock>
// The keyword consists in general of keywords separated by periods:  
//<srcblock>
//	printer.ps.page
//	measures.precession.d_interval
// 	measures.nutation.d_interval
// </srcblock>
// and, by preference, in lower case (but
// search is case sensitive) with an <src>_</src> as word-parts separator. <br>
// The keyword and value are separated by a <src>:</src>. The value is the string
// between the first non-whitespace character after the separator and the end of
// the line. Interpretation of the string is the programs responsibility.<br>
// Any part of the keyword string can be replaced with a wildcard <src>*</src>
// to indicate all values with that structure (e.g.
// <src>*.d_interval</src> would indicate in the example above both the
// precession and the nutation <src>d_interval</src>.<br>
// A match between a keyword to be found and a keyword in the resource files
// will be the first match (taking wildcards into account) encountered in the
// search through the resource files. The resource files searched are (in the
// given order):
// <srcblock>
//   ~/.aipsrc
//   $AIPSROOT/.aipsrc
//   $AIPSHOST/aipsrc
//   $AIPSSITE/aipsrc
//   $AIPSARCH/aipsrc
// </srcblock> 
// It is not an error for any of the aipsrc files to be absent or empty.<br>
// The basic interaction with the class is with the static keyword match function
// <src>Aipsrc::find(String &result, const String &keyword)</src>.
// <note role=caution> The search keyword (unlike the file keyword) has no
// wildcards. The real name should, of course, be looked for.</note>
// </synopsis>
//
// <example>
// <srcblock>
//  String printerPage;		// result of keyword find
//  if(!Aipsrc::find(printerPage, "printer.ps.page")) {	// look for keyword match
//    printerPage = "notSet";
//  };
// </srcblock>
// A more convenient way of accomplishing the same result is:
// <srcblock>
//    Aipsrc::find(printerPage, "printer.ps.page", "notSet");
// </srcblock>
// Here the final argument is the default to use if the keyword is not found
// at all.
// </example>
//
// <motivation>
// Programs need a way to interact with the aipsrc files.
// </motivation>
//
// <thrown>
//    <li> Nothing thrown by Aipsrc but used classes may.
// </thrown>
//
// <todo asof="1996/10/24">
//   <li> Find a better way to do the keyword matching, not a real high priority
// </todo>

class Aipsrc {

public:
//# Constructors

//# Destructor

//# Copy assignment

  //# Member functions
// <group>
// The <src>find()</src> functions will, given a keyword, return the value
// with a matched keyword found in the files. If no match found the
// function will be False. The <src>findNoHome()<src> emulates the <src>-i</src>
// switch of getrc by bypassing the <src>~/.aipsrc</src> file.
   static Bool find(String &value, const String &keyword);
   static Bool findNoHome(String &value, const String &keyword);
// </group>

// This find usually saves you some lines of code, since you can supply the
// default you want to use when no such keywords is defined. These functions
// If the return value is False, the keyword was not found and the default
// was used.
// <group>
   static Bool find(String &value, const String &keyword, 
		    const String &deflt);
   static Bool findNoHome(String &value, const String &keyword,
			  const String &deflt);
// </group>

// A find() which returns the file the match was
// found in, and the complete line in that file.
// <group>
   static Bool find(String &value, String &fileFound,
		    String &lineFound, const String &keyword);
   static Bool findNoHome(String &value, String &fileFound,
		    String &lineFound, const String &keyword);
// </group>

// The <src>reRead()</src> function, will reinitialise the static maps and read the
// aipsrc files again. It could be useful in some interactive or multi-processor 
// circumstances.
   static void reRead();

// The following functions return the full lists of available data. They could
// be useful for debugging purposes.
// <group>
  static const Block<String> &keywords();
  static const Block<String> &values();
  static const Block<String> &lines();
  static const Block<String> &files();
  static const Block<uInt> &fileEnds();
// </group>

// The following <src>show()</src> function, useful for debugging, outputs all information
  static void show(ostream &oStream);
// Prints all info on cout
  static void show();

private:
//# Data
  // Indicate files read
  static Bool          doInit;
  // List of keywords found
  static Block<String> keywordName;
  // List of values belonging to keywords found
  static Block<String> keywordValue;
  // List of patterns deducted from names
  static Block<String> keywordPattern;
  // Full line content of keywords found
  static Block<String> keywordLine;
  // The names of the files found
  static Block<String> keywordFile;
  // The end in the keyword list for each file found
  static Block<uInt>   fileEnd;

//# General member functions
// Read in the aipsrc files, returning the number of lines found
// <group>
  static uInt parse();
  static uInt parse(String &fileList);
// </group>

// Locate the right keyword in the static maps
   static Bool matchKeyword(uInt &where, const String &keyword,
			    uInt start);

// Find a match
   static Bool find(String &value, String &fileFound,
		    String &lineFound, const String &keyword,
		    uInt start);
};


#endif


