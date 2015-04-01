//# Aipsrc.h: Class to read the casa general resource files
//# Copyright (C) 1995,1996,1997,1998,1999,2002,2004
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

#ifndef CASA_AIPSRC_H
#define CASA_AIPSRC_H

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/OS/Mutex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations
template <class T> class AipsrcValue;
template <class T> class AipsrcVector;
class Aipsrc;

//# Typedefs
typedef AipsrcValue<Double> AipsrcDouble;
typedef AipsrcValue<Int> AipsrcInt;
typedef AipsrcValue<Bool> AipsrcBool;
typedef Aipsrc AipsrcString;
typedef AipsrcVector<Double> AipsrcVDouble;
typedef AipsrcVector<Int> AipsrcVInt;
typedef AipsrcVector<Bool> AipsrcVBool;
typedef AipsrcVector<String> AipsrcVString;


// <summary> Class to read the casa general resource files </summary>

// <use visibility=export>

// <reviewed reviewer="wyoung" date="1996/11/25" tests="tAipsrc" demos="">
// </reviewed>

// <prerequisite>
//  <li> None
// </prerequisite>
//
// <etymology>
// A class for getting values from the casa resource files
// </etymology>
//
// <synopsis>
// The static Aipsrc class can get information from the casa resource files.
// It has the same functionality as getrc (c program used for Casacore 
// installation scripts).<br>
// In addition it acts as a central clearing house between system and
// software by providing functionality to obtain Casacore system parameters
// (like AIPSPATH elements), and the possibility of storing system wide
// information provided by a class for reference by other classes. <br>
// The format of a line in a resource file is:
// <srcblock>
//	# Line starting with an # in column 1 is a comment (as is an empty line)
//	keyword:   value
//	keyword:   value
// </srcblock>
// The keyword (starting at first non-blank) 
// consists in general of keyword fields separated by periods:  
//<srcblock>
//	printer.ps.page
//	measures.precession.d_interval
// 	measures.nutation.d_interval
// </srcblock>
// and, by preference, in lower case (but
// search is case sensitive) with an <src>_</src> as word-parts separator. <br>
// The keyword and value are separated by a <src>:</src>. The value is the string
// from the first non-whitespace character after the separator to the end of
// the line. Interpretation of the string is in general the program's 
// responsibility, but special <src>find()</src> calls (see below) exist to 
// aid.<br>
// Any part of the keyword string can be replaced by a wildcard <src>*</src>
// to indicate all values with that structure (e.g.
// <src>*.d_interval</src> would indicate in the example above both the
// precession and the nutation <src>d_interval</src>.<br>
// A match between a keyword to be found and a keyword in the resource files
// will be the first match (taking wildcards into account) encountered in the
// search through the resource files.
// The resource files to be looked at can be defined in the environment
// variable CASARCFILES. If undefined, the resource files searched are (in the
// given order):
// <srcblock>
//   ~/.casarc
//   ~/.casa/rc
//   ~/.aipsrc
//   $AIPSROOT/.aipsrc
//   $AIPSHOST/aipsrc
//   $AIPSSITE/aipsrc
//   $AIPSARCH/aipsrc
// </srcblock> 
// It is not an error for any of the aipsrc files to be absent or empty.
// However, it is an error if <em>HOME</em> has not been set:
// an exception will occur. AIPSPATH will in general be
// read from the global environment variables, but can, before any other
// <src>Aipsrc</src> related call, be set with the
// <src>setAipsPath()</src> call.<br>
// If AIPSPATH is not set in either way, it is set to the home directory.
// <p>
// The basic interaction with the class is with the static keyword match function
// <srcblock>Bool Aipsrc::find(String &result, const String &keyword)
// </srcblock>
// A set of 
// <srcblock>Bool AipsrcValue::find(Type &result, const String &keyword, ...)
// </srcblock>
// are available to interpret the string value found.
// (see <linkto class="AipsrcValue">AipsrcValue</linkto>).<br>
// All the <src>find</src>
// functions have the ability to set a default if there is no match,
// while also unit conversion is possible.<br>
// The Bool return indicates if the keyword was found, and, in the case of the
// interpretative finds, if an 'important' format error was found (e.g.
// '+12a' will be accepted as a Double, with a result of '12', since the
// standard double conversion in <src>>></src> will produce this result.) 
// <note role=caution> The search keyword (unlike the file keyword) has no
// wildcards. The real name should, of course, be looked for.</note>
// To aid in other places, the following (static) methods are available
// to get the requested information (derived from <src>HOME</src> and
// <src>AIPSPATH</src>, computer system information and/or aipsrc keywords):
//  <ul>
//   <li> const String &Aipsrc::aipsRoot()
//   <li> const String &Aipsrc::aipsArch()
//   <li> const String &Aipsrc::aipsSite()
//   <li> const String &Aipsrc::aipsHost()
//   <li> const String &Aipsrc::aipsHome()
//  </ul>
// Other, numeric, system information can be found in
// <linkto class=AipsrcValue>AipsrcValue</linkto>.<br>
//
// Given an AIPSPATH of 
// <srcblock>/epp/aips++ sun4sol_gnu epping norma</srcblock>
// aipsSite will return
// <srcblock>/epp/aips++/sun4sol_gnu/epping</srcblock>.
//
// The basic find above reacts with the aipsrc files available. If regular 
// access is necessary (e.g. a lot of routines have to check independently a
// certain integration time limit), keywords can be <em>registered</em> to
// enable:
// <ul>
//   <li> fast access with integer code, rather than string
//   <li> ability to set values from programs if no aipsrc information given
//		(a dynamic default)
//   <li> update the <src>$HOME/.aipsrc</src> keyword/value list with save() 
// </ul>
// <note role=tip> The registered value is never equal to zero, hence a zero
// value can be used to check if registration is done. Also, registering the
// same keyword twice is safe, and will produce the same value.</note>
// When saving a keyword/value pair in <src>$HOME/.aipsrc</src>, the old
// version is saved in <src>$HOME/.aipsrc.old</src>, before the keyword/value
// pair is prepended to the file. A limited number of edits of the same keyword
// is preserved only (default 5, changeable with the
// <src>user.aipsrc.edit.keep</src> keyword.
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
// at all.<br>
// If you often want to know, dynamically, the current 'printer.ps.page'
// value, you could do something like:
// <srcblock>
//	static uInt pp = Aipsrc::registerRC("printer.ps.page", "noSet");
//	String printerPage = Aipsrc::get(pp);
// // Processing, and maybe somewhere else:
//	Aipsrc::set(pp, "nowSet");
// // ...
//	printerPage = Aipsrc::get(pp);
// // and save it to the <src>$HOME/.aipsrc</src> list
//	Aipsrc::save(pp);
// </srcblock>
// </example>
//
// <motivation>
// Programs need a way to interact with the aipsrc files.
// </motivation>
//
// <thrown>
//    <li>AipsError if the environment variables HOME and/or AIPSPATH not set.
// </thrown>
//
// <todo asof="1997/08/07">
// </todo>

class Aipsrc {

public:
  //# Constructors

  //# Destructor

  //# Copy assignment

  //# Member functions
  // <thrown>
  // <li> AipsError if HOME environment variable not set
  // </thrown> 
  // The <src>find()</src> functions will, given a keyword, return the value
  // with a matched keyword found in the files. If no match found the
  // function will be False. The <src>findNoHome()</src> emulates the <src>-i</src>
  // switch of getrc by bypassing the <src>~/.aipsrc</src> file.
  // <group>
  static Bool find(String &value, const String &keyword);
  static Bool findNoHome(String &value, const String &keyword);
  // </group>

  // These finds check a (possible) value of the keyword against a list
  // of coded values provided, and return an index into the list (N if not
  // found). Matching is minimax, case insensitive. Always better to use
  // the one with default. return is False if no keyword or no match.
  // <group>
  static Bool find(uInt &value, const String &keyword,
		   Int Nname, const String tname[]);
  static Bool find(uInt &value, const String &keyword,
		   const Vector<String> &tname);
  // </group>
  // This find usually saves you some lines of code, since you can supply the
  // default you want to use when no such keyword is defined.
  // If the return value is False, the keyword was not found and the default
  // was used.
  // <group>
  static Bool find(String &value, const String &keyword, 
		   const String &deflt);
  static Bool findNoHome(String &value, const String &keyword,
			 const String &deflt);
  static Bool find(uInt &value, const String &keyword,
		   Int Nname, const String tname[], const String &deflt);
  static Bool find(uInt &value, const String &keyword,
		   const Vector<String> &tname, const String &deflt);
  // </group>

  // Sets foundDir to the first /firstPart/lastPart path that it finds
  // present on the system, where /firstPart comes from, in order,
  // this list:
  //   contents of prepends
  //   + useStd ? (., aipsHome(), aipsRoot()) : ()
  //   + contents of appends
  static Bool findDir(String& foundDir, const String& lastPart="",
                      const Vector<String>& prepends=Vector<String>(),
                      const Vector<String>& appends=Vector<String>(),
                      Bool useStds=True);

  // Functions to register keywords for later use in get() and set(). The
  // returned value is the index for get() and set().
  // <group>
  static uInt registerRC(const String &keyword,
			 const String &deflt);
  static uInt registerRC(const String &keyword,
			 Int Nname, const String tname[], const String &deflt);
  static uInt registerRC(const String &keyword,
			 const Vector<String> &tname, const String &deflt);
  // </group>

  // Gets are like find, but using registered integers rather than names.
  // <group>
  static const String &get(uInt keyword);
  // get for code
  static const uInt &get(uInt &code, uInt keyword);
  // </group>

  // Sets allow registered values to be set
  // <group>
  static void set(uInt keyword, const String &deflt);
  static void set(uInt keyword,
		  Int Nname, const String tname[], const String &deflt);
  static void set(uInt keyword,
		  const Vector<String> &tname, const String &deflt);
  // </group>

  // Save a registered keyword value to <src>$HOME/.aipsrc</src>
  // <group>
  static void save(uInt keyword);
  static void save(uInt keyword, const String tname[]);
  static void save(uInt keyword, const Vector<String> &tname);
  // </group>

  // Set an AIPSPATH that should be used in stead of a global AIPSPATH.
  // This call should be made before any Aipsrc related call. The AIPSPATH
  // will have up to 4 fields (which can all be empty) giving the root, host,
  // site and arch directory that will be searched for possible
  // <src>[.]aipsrc</src> files.
  static void setAipsPath(const String &path = String());

  // Returns the appropriate Casacore or system variable values
  // <group>
  static const String &aipsRoot();
  static const String &aipsArch();
  static const String &aipsSite();
  static const String &aipsHost();
  // Returns: <src>~/aips++</src>
  static const String &aipsHome();
  // </group>
  
  // The <src>reRead()</src> function, will reinitialise the static maps and read the
  // aipsrc files again. It could be useful in some interactive or multi-processor 
  // circumstances. <src>lastRead()</src> returns the time last reRead.
  // <group>
  static void reRead();
  static Double lastRead();
  // </group>
  

  // The following functions return the full lists of available data. They could
  // be useful for debugging purposes.
  // <group>
  static const Block<String> &values();
  static const Block<String> &patterns();
  // </group>
  
  // The following <src>show()</src> function, useful for debugging, outputs 
  // all keyword/value pairs found
  static void show(ostream &oStream);
  // Prints all info on cout
  static void show();
  // The following set is a general set of functions
  // <group>
  // Read aipsrc type files (without wildcards), and return the unique names
  // and values in the Vector arguments. The return value is number of names.
  static uInt genRestore(Vector<String> &namlst, Vector<String> &vallst,
		    const String &fileList);
  // Save the names/values in file
  static void genSave(Vector<String> &namlst, Vector<String> &vallst,
		      const String &fnam);
  // Set (new or overwrite) keyword/value pair
  static void genSet(Vector<String> &namlst, Vector<String> &vallst,
		     const String &nam, const String &val);
  // Remove a keyword from list (False if not in list)
  static Bool genUnSet(Vector<String> &namlst, Vector<String> &vallst,
		       const String &nam);
  // Get the value of a keyword
  static Bool genGet(String &val, Vector<String> &namlst, Vector<String> &vallst,
		     const String &nam);
  // </group>

protected:
  // Actual find function
  static Bool find(String &value, const String &keyword,
		   uInt start);
  // The registration function
  static uInt registerRC(const String &keyword, Block<String> &nlst);
  // Actual saving
  static void save(const String keyword, const String val);
  
private:
  //# Data
  static Mutex theirMutex;
  // Indicate files read
  static volatile Bool doInit;
  // Last time data was (re)read
  static Double lastParse; 
  // List of values belonging to keywords found
  static Block<String> keywordValue;
  // List of patterns deducted from names
  static Block<String> keywordPattern;
  // The start of the non-home values
  static uInt fileEnd;
  // The possibly set external AIPSPATH
  static String extAipsPath;
  // AIPSROOT
  static String root;
  // AIPSARCH
  static String arch;
  // AIPSSITE
  static String site;
  // AIPSHOST
  static String host;
  // AIPSHOME
  static String home;
  // HOME
  static String uhome;
  // Indicate above filled
  static Bool filled;
  // String register list
  // <group>
  static Block<String> strlst;
  static Block<String> nstrlst;
  static Block<uInt> codlst;
  static Block<String> ncodlst;
  // </group>

  //# General member functions
  // Read in the aipsrc files, returning the number of lines found
  // <group>
  static void parse(Bool force=False);
  static void doParse(String &fileList);
  // </group>
  
  // The following parse function can be used for any list of files. It will
  // return the list of Patterns and values found, and the last keyword number
  // of first file in list.
  static uInt genParse(Block<String> &keywordPattern, 
		       Block<String> &keywordValue,
		       uInt &fileEnd, const String &fileList);

  // Locate the right keyword in the static maps
  static Bool matchKeyword(uInt &where, const String &keyword,
			   uInt start);
  // Fill in root, arch, site, host and home, and return requested nam
  static const String &fillAips(const String &nam);
};


} //# NAMESPACE CASACORE - END

#endif


