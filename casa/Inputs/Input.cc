//# Input.cc: A linked list of user input parameters
//# Copyright (C) 1993,1994,1995,1996,1999,2000,2001,2002
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

//  Class Input: the user interface

#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/OS/EnvVar.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/iostream.h>

#if defined(TESTBED)
#define DEBUG 1
#endif

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//
// The constructor does nothing more but read the users environment
// and add appropriate things to the linked list of Param`s
// This can be turned off by overriding the default `init' argument.
//

Input::Input (Int createEnv) 
: is_closed(False),
  do_prompt(False),
  debug_level(0),
  p_count(0)
{
  if (createEnv){
    envCreate ("DEBUG", "debug", "0");
    envCreate ("HELP" , "help", "0");
    
    debug_level = getInt("debug");
    if (debug(5)) {
      cout << "Input::Input: (debug=" << debug_level << ")\n";
    }
  } else {
    create ("debug","0","Debug Level");
    create ("help","0");
  }
}

// Destructor
Input::~Input()
{
  if (debug(5)) {
    cout << "INPUT> Destructing " << count() << " parameters\n";
  }
}

void Input::create (const String& key)
{
  createPar (0, key, "", "", "", "", "");
}

void Input::create (const String& key, const String& value)
{
  createPar(0, key, value, "", "", "", "");
}

void Input::create (const String& key, const String& value, const String& help)
{
  createPar(0, key, value, help, "", "", "");
}

void Input::create (const String& key, const String& value,
		    const String& help, const String& type)
{
  createPar(0, key, value, help, type, "", "");
}

void Input::create (const String& key, const String& value,
		    const String& help, const String& type,
		    const String& range)
{
  createPar(0, key, value, help, type, range, "");
}

void Input::create (const String& key, const String& value,
		    const String& help, const String& type,
		    const String& range, const String& unit)
{
  createPar(0, key, value, help, type, range, unit);
}

// CreatePar() is the workhorse function, it is a private member function

void Input::createPar (Int system, const String& key, const String& value,
		       const String& help, const String& type,
		       const String& range, const String& unit)
{
  if (is_closed) {
    String msg = "Input::createPar: " + key +
      ": Cannot create any more Parameters";
    throw(AipsError(msg));
  }
  
  int i = getParam(key);
  if (i>=0) {
    String msg = "Input:cCreatePar: " + key + ": Parameters already exists.";
    throw (AipsError(msg));
  } 
  
  if (key=="help") {
    if (value=="prompt") {
      do_prompt = True;
    }
    help_mode = value;
  }

  if (debug(5)) {
    cout << "Input::CreatePar: Creating new keyword " << key << "=" 
	 << value << "\n" << flush;
  }
  Param tmp(key,value,help,type,range,unit);
  if (system) {
    tmp.setSystem(True);
  } else {
    tmp.setIndex(++p_count);
  }
  parList_p.push_back (tmp);
}

void Input::close() 
{ 
  if (is_closed) {
    String msg = "Input::Close: parameter creation is already closed.";
    throw(AipsError(msg));
  } else {
    is_closed = True; 
  }
  if (debug(1)) {       // Display Param as: 'key=val: help'
    cout << "INPUT> Closing parameter creation: \n";
    cout << "INPUT> ----------------------------------------------------\n";
    for (const auto& x : parList_p) {
      cout << "INPUT> " << x.keyVal() << ": " << x.getHelp() << "\n";
    }
    cout << "INPUT>-----------------------------------------------------\n"
	 << flush;
  }
}

// Query functions
Double Input::getDouble (const String& key)
{
  Int i = getParam(key);
  if (i<0) {
    String msg ="Input::GetDouble: Parameter " + key + " is unknown.";
    throw (AipsError(msg));
  }
  Param& x = parList_p[i];
  if (do_prompt && !x.isSystem()) {
    prompt(x);
  }
  return x.getDouble();
}

Block<Double> Input::getDoubleArray (const String& key)
{
  Int i = getParam(key);
  if (i<0) {
    String msg ="Input::GetDoubleArray: Parameter " + key + " is unknown.";
    throw (AipsError(msg));
  }
  Param& x = parList_p[i];
  if (do_prompt && !x.isSystem()) {
    prompt(x);
  }
  return x.getDoubleArray();
}

int Input::getInt (const String& key)
{
  Int i = getParam(key);
  if (i<0) {
    String msg ="Input::GetInt: Parameter " + key + " is unknown.";
    throw (AipsError(msg));
  }
  Param& x = parList_p[i];
  if (do_prompt && !x.isSystem()) {
    prompt(x);
  }
  return x.getInt();
}

Block<Int> Input::getIntArray (const String& key)
{
  Int i = getParam(key);
  if (i<0) {
    String msg ="Input::GetIntArray: Parameter " + key + " is unknown.";
    throw (AipsError(msg));
  }
  Param& x = parList_p[i];
  if (do_prompt && !x.isSystem()) {
    prompt(x);
  }
  return x.getIntArray();
}

String Input::getString (const String& key)
{
  Int i = getParam(key);
  if (i<0) {
    String msg ="Input::GetString: Parameter " + key + " is unknown.";
    throw (AipsError(msg));
  }
  Param& x = parList_p[i];
  if (do_prompt && !x.isSystem()) {
    prompt(x);
  }
  return x.getString();
}

Bool Input::getBool (const String& key)
{
  Int i = getParam(key);
  if (i<0) {
    String msg ="Input::GetBool: Parameter " + key + " is unknown.";
    throw (AipsError(msg));
  }
  Param& x = parList_p[i];
  if (do_prompt && !x.isSystem()) {
    prompt(x);
  }
  return x.getBool();
}

// Modifiers
Bool Input::put (const String& key, const String& value)
{
  String akey, avalue;
  
  if (debug(5)) {
    cout << "PUT> " << key << "=" << value << "\n";
  }
  Int i = getParam(key);
  if (i<0) {
    String msg = "Input::Put: parameter " + key + " is unknown.";
    throw (AipsError(msg));
  }
  Param& x = parList_p[i];
  x.put(value);
  return True;
}

Bool Input::put (const String& key)
{
  String k = key;                  // Need non-const string
  String::size_type inx = key.index("=");
  if (inx == String::npos) {
    String msg = "Input::Put: " + key + " is not a valid parameter.";
    throw (AipsError(msg));
  }
  return put (k.before(inx), k.after(inx));
}

Int Input::count() const
{
  return parList_p.size();
}

void Input::version (const String& a_version)
{
  version_id = a_version;
}

void Input::announce()
{
  if (debug(5)) {
    cout << getString("argv0") << " ";
    for (const auto& x : parList_p) {
      if (x.getIndex() > 0) {
	cout << x << " ";
      }
    }
    cout << "\n";
  }
  
  if (help_mode.contains("prompt")) {
    do_prompt = True;
  }
  if (help_mode.contains("keys")) {
    keys();
  }
  if (help_mode.contains("exit")) {
    exit(0);
  }
  if (version_id.length() > 0) {               // Always announce ???
    cout << getString("argv0") << ": Version " << version_id << "\n";
  }
}


// Some private member functions we need

//
// Move to the current named Parameter; return 0 if none found
// A value >= 1 is the index (not used in K_ stuff)
//
Int Input::getParam (const String& name) const
{
  for (uInt i=0; i<parList_p.size(); ++i) {
    if (parList_p[i].getKey() == name) {
      return i;
    }
  }
  return -1;
}

void Input::prompt (Param& x) const
{
  if (x.getHelp().length() == 0) {
    String msg = "Input::Prompt: keyword=" + x.getKey() +
      " doesn't have an associated help field.";
    throw(AipsError(msg));
  }
  Bool ok = False;
  Char input[1024];
  while (ok==False) { 
    cout << x.getHelp() << " [" << x.getString() << "]: " 
	 << x.getKey() << "=";
    cin.getline(input,80);
    if (input[0] != 0) {
      String s = input;
      ok = x.put(s);
    } else {
      ok = True;
    }
  }
}

void Input::envCreate (const Char *env, const String& key, const String& def)
{
  String s (EnvironmentVariable::get(env));
  if (s.empty()) {
    s = def;
  }
  createPar (1, key, s, "-", "", "", "");
}

//      Show current key=val pairs, with program name
void Input::keys()
{
  String p = getString("argv0");
  cout << p;

  for (const auto& x : parList_p) {
    if (! x.isSystem()) {
      cout << " " << x;
    }
  }
  cout << "\n";
}

// Swallow arguments, setting keyword=value pairs appropriately from command
// line arguments in either "keyword=value" or "-keyword value" format. Argv[0]
// is assumed to be the program name.
void Input::readArguments (int ac, char const* const* av)
{
  Int i;
  createPar (1, "argv0", av[0], "Program name", "", "", "");

  if (debug(5)) {
    cout << "MAIN> " << p_count << " program parameters\n";
  }
  close();             // close creation 

  // Show version on screen if -v or --version is given.
  if (av[1] && (String(av[1]) == "-v"  ||  String(av[1]) == "--version")) {
    cerr << av[0] << "  version " << version_id << endl;
    exit (1);
  }
  // Show parameters on screen if -h or --help given.
  if (av[1] && (String(av[1]) == "-h"  ||  String(av[1]) == "--help")) {
    cerr << av[0] << "  version " << version_id << endl;
    for (const auto& x : parList_p) {
      if (x.getIndex() > 0) {
	cerr << x.getKey() << ", " << x.getType() << ", ";
	if (String(x.get()) != "") {
	  cerr << '(' << x.get() << ')';
	}
	cerr << endl;
        if (String(x.getHelp()) != "") {
	  cerr << "    " << x.getHelp() << endl;
	}
      }
    }
    exit (1);
  }

  // Turn command line style inputs into keyword=value inputs
  //     ' -channels 64'  becomes  'channels=64'
  String thisarg;
  String keyandval;
  // Pass through keyword=value and turn -keyword value into keyword=value
  for (i=1; i<ac; i++) {
    thisarg = av[i];
    // The next statement uses elem() rather than [] to cater for sgi-32b
    // problem
    if (thisarg.elem(0) == '-') {
      if (i >= ac - 1) {
	throw(AipsError("Input::ReadArguments:"
			"-keyword not followed by value"));
      }
      i++;  // Advance
      keyandval = thisarg.after(0) + "=" + av[i];
    } else {
      keyandval = thisarg;
    }
    //	cout << "putting " << keyandval << endl;
    put (keyandval);   // Insert command line parameters
    if (keyandval.size() > 5  &&  keyandval.before(6) == "debug=") {
      debug_level = atoi(keyandval.after(6).chars());
    } else if (keyandval.size() > 4  &&  keyandval.before(5) == "help=") {
      help_mode = keyandval.after(4);
    }
  }
  announce();          // Announce and possibly die here
}

Vector<Bool> Input::makeMaskFromRanges(const String& ranges, uInt length,
				       Bool oneRelative)
{
  Regex single("^[ \t]*[0-9]+[ \t]*$", 1);
  Regex range("^[ \t]*[0-9]+[ \t]*-[ \t]*[0-9]+[ \t]*$", 1);
  
  Vector<Bool> mask(length);
  mask = False;
  
  // Step through the string, comma separated expression by comma separated
  // expression.
  Int numberOfCommas = ranges.freq(",");
  Block<String> expressions(numberOfCommas + 1);
  split (ranges, expressions.storage(), numberOfCommas + 1, ",");
  
  for (uInt i=0; i < expressions.nelements(); i++) {
    // Validate
    if (expressions[i].contains(single) == False &&
	expressions[i].contains(range) == False) {
      throw (AipsError(String("Input::makeMaskFromRanges - "
			      "invalid range:") + expressions[i]));
    }
    Int left, right;
    String::size_type inx = expressions[i].index('-');
    if (inx != String::npos) {
      left = atoi(expressions[i].before(inx).chars());
      right = atoi(expressions[i].after(inx).chars());
    } else {
      left = right = atoi(expressions[i].chars());
    }
    if (oneRelative) {
      left -= 1;
      right -= 1;
    }
    if (left + 1 > Int(mask.nelements()) ||
	right + 1 > Int(mask.nelements()) ||
	left > right) {
      throw (AipsError(String("Input::makeMaskFromRanges - "
			      "out of range or end<start ") + ranges));
    }
    mask(Slice(Int(left), Int(right-left+1))) = True;
  }
  
  return mask;
}

} //# NAMESPACE CASACORE - END

