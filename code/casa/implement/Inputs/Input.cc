//# Input.cc: A linked list of user input parameters
//# Copyright (C) 1993,1994,1995,1996
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

//  Class Input: the user interface

#include <aips/Inputs/Input.h>
#include <aips/Arrays/Vector.h>
#include <aips/Exceptions/Error.h>

#if defined(TESTBED)
#define DEBUG 1
#endif

//
// The constructor does nothing more but read the users environment
// and add appropriate things to the linked list of Param`s
// This can be turned off by overriding the default `init' argument.
//

Input::Input(Int createEnv) 
: version("1995/04/06 BEG/TPPR/PJT"),
  is_closed(False), do_prompt(False), debug_level(0), p_count(0)
{
  if (createEnv){
    EnvCreate("DEBUG","debug","0");
    EnvCreate("HELP" ,"help","0");
    
    debug_level = GetInt("debug");
    if(Debug(5)) 
      cout << "Input::Input: (debug=" << debug_level << ")\n";
  } else {
    Create("debug","0","Debug Level");
    Create("help","0");
  };
};

// Destructor
Input::~Input()
{
  do_prompt = False;
  if(Debug(5)) cout << "INPUT> Destructing " << Count() << " parameters\n";
};

void Input::Create(String key)
{
  CreatePar(0, key, "", "", "", "", "");
};

void Input::Create(String key, String value)
{
  CreatePar(0, key, value, "", "", "", "");
};

void Input::Create(String key, String value, String help)
{
  CreatePar(0, key, value, help, "", "", "");
};

void Input::Create(String key, String value, String help, String type)
{
  CreatePar(0, key, value, help, type, "", "");
};

void Input::Create(String key, String value, String help, String type, 
		   String range)
{
  CreatePar(0, key, value, help, type, range, "");
};

void Input::Create(String key, String value, String help, String type, 
		   String range, String unit)
{
  CreatePar(0, key, value, help, type, range, unit);
};

// CreatePar() is the workhorse function, it is a private member function

void Input::CreatePar(int system, String key, String value, String help,
		      String type, String range, String unit)
{
  if(is_closed) {
    String msg = "Input::CreatePar: " + key +
      ": Cannot create any more Parameters";
    throw(AipsError(msg));
    return;
  }
  
  int i = GetParam(key);
  if (i!=0) {
    String msg = "Input::CreatPar: " + key + ": Parameters already exists.";
    throw(AipsError(msg));
    return;
  } 
  
  if (key=="help"){
    if (value=="prompt") do_prompt = True;
    help_mode = value;
  }

  if(Debug(5)) 
    cout << "Input::CreatePar: Creating new keyword " << key << "=" 
      << value << "\n" << flush;
  
  Param tmp(key,value,help,type,range,unit);
  if (system) 
    tmp.SetSystem(True);
  else
    tmp.SetIndex(++p_count);

  ListIter<Param> parlist(ParList);           
  parlist.toStart();
  while (!parlist.atEnd())
    ++parlist;
  
  parlist.addRight(tmp);
};

void Input::Close() 
{ 
  if(is_closed) {
    String msg = "Input::Close: parameter creation is already closed.";
    throw(AipsError(msg));
    return;
  } else
    is_closed = True; 
  
  if(Debug(1)) {       // Display Param as: 'key=val: help'
    cout << "INPUT> Closing parameter creation: \n";
    cout << "INPUT> ----------------------------------------------------\n"; 
    ListIter<Param> parlist(ParList);       
    parlist.toStart();
    for(int i=0; i<Count(); i++, ++parlist) { 
      Param &x = parlist.getRight();
      cout << "INPUT> " << x.KeyVal() << ": " << x.GetHelp() << "\n";
    }
    cout << "INPUT>-----------------------------------------------------\n"
      << flush;
  }
};

// Query functions
Double Input::GetDouble(String key)
{
  int i = GetParam(key);
  
  if(i==0) {
    String msg ="Input::GetDouble: Parameter " + key + " is unknown.";
    throw(AipsError(msg));
    return 0.0;
  }
  ListIter<Param> parlist(ParList);       
  parlist.toStart();
  parlist.step(i-1);
  Param &x = parlist.getRight();
  
  if(do_prompt && !x.IsSystem()) Prompt(x);
  
  return x.GetDouble();
};

Block<Double> Input::GetDoubleArray(String key)
{
  int i = GetParam(key);
  
  if(i==0) {
    String msg ="Input::GetDoubleArray: Parameter " + key + " is unknown.";
    throw(AipsError(msg));
  }
  ListIter<Param> parlist(ParList);       
  parlist.toStart();
  parlist.step(i-1);
  Param &x = parlist.getRight();
  
  if(do_prompt && !x.IsSystem()) Prompt(x);
  
  return x.GetDoubleArray();
};

int Input::GetInt(String key)
{
  int i = GetParam(key);
  
  if(i==0) {
    String msg ="Input::GetInt: Parameter " + key + " is unknown.";
    throw(AipsError(msg));
    return 0;
  }
  ListIter<Param> parlist(ParList);       
  parlist.toStart();
  parlist.step(i-1);
  Param &x = parlist.getRight();

  if(do_prompt && !x.IsSystem()) Prompt(x);
  
  return x.GetInt();
};

Block<Int> Input::GetIntArray(String key)
{
  int i = GetParam(key);
  
  if(i==0) {
    String msg ="Input::GetIntArray: Parameter " + key + " is unknown.";
    throw(AipsError(msg));
  }
  ListIter<Param> parlist(ParList);       
  parlist.toStart();
  parlist.step(i-1);
  Param &x = parlist.getRight();
  
  if(do_prompt && !x.IsSystem()) Prompt(x);
  
  return x.GetIntArray();
};

String Input::GetString(String key)
{
  int i = GetParam(key);
  
  if(i==0) {
    String msg ="Input::GetString: Parameter " + key + " is unknown.";
    throw(AipsError(msg));
    return "";
  }
  ListIter<Param> parlist(ParList);       
  parlist.toStart();
  parlist.step(i-1);
  Param &x = parlist.getRight();
  
  if(do_prompt && !x.IsSystem()) Prompt(x);
  
  return x.GetString();
};

Bool Input::GetBool(String key)
{
  int i = GetParam(key);
  
  if(i==0) {
    String msg ="Input::GetBool: Parameter " + key + " is unknown.";
    throw(AipsError(msg));
    return False;
  }
  ListIter<Param> parlist(ParList);       
  parlist.toStart();
  parlist.step(i-1);
  Param &x = parlist.getRight();
  
  if(do_prompt && !x.IsSystem()) Prompt(x);
  
  return x.GetBool();
};

// Modifyers
Bool Input::Put(String key, String value)
{
  String akey, avalue;
  
  if(Debug(5)) cout << "PUT> " << key << "=" << value << "\n";
  
  int i = GetParam(key);
  if(i==0) {
    String msg = "Input::Put: parameter " + key + " is unknown.";
    throw(AipsError(msg));
    return False;
  }
  ListIter<Param> parlist(ParList);       
  parlist.toStart();
  parlist.step(i-1);
  Param &x = parlist.getRight();
  x.Put(value);
  return True;
};

Bool Input::Put(String key)
{
  if(!key.index("=")) {
    String msg = "Input::Put: " + key + " is not a valid parameter.";
    throw(AipsError(msg));
    return False;
  }
  return Put(key.before("="), key.after("="));
};

int Input::Count()
{
  ListIter<Param> parlist(ParList);       
  return parlist.len();
};

Bool Input::Debug(int l)
{
  return (debug_level >= l) ? True : False;        // make it inline
};

void Input::Usage(String)
{
  cout << "Inputs::Usage() is deprecated, please delete it from your code."
    << endl;
};

void Input::Version(String a_version)
{
  version = a_version;
};

void Input::Announce()
{
  if (Debug(5)) {
    cout << GetString("argv0") << " ";
    ListIter<Param> parlist(ParList);       
    parlist.toStart();
    for(int i=0; i<Count(); i++, ++parlist) { 
      Param &x = parlist.getRight();
      if(x.GetIndex() > 0) cout << x << " ";
    }
    cout << "\n";
    
  }
  
  if (help_mode.contains("prompt")) {
    do_prompt = True;
  }

  if (help_mode.contains("pane")) {
    Pane();
    exit(0);
  }
  if (help_mode.contains("keys")) {
    Keys();
  }
  if (help_mode.contains("exit")) {
    exit(0);
  }
  if (version.length() > 0)               // Always announce ???
    cout << GetString("argv0") << ": Version " << version << "\n";
};


// Some private member functions we need

//
// Move to the current named Parameter; return 0 if none found
// A value >= 1 is the index (not used in K_ stuff)
//
int Input::GetParam(String name)
{
  int n = Count();
  
  if (n<=0) return n;
  
  ListIter<Param> parlist(ParList);       
  parlist.toStart();
  for(int i=0; i<n; i++, ++parlist) {            // loop over all parameters
    Param &x = parlist.getRight();
    if(x.GetKey() == name) return i+1;         // Force full match here
  }
  return 0;
};

void Input::Prompt(Param& x)
{
  if(x.GetHelp().length() == 0){
    String msg = "Input::Prompt: keyword=" + x.GetKey() +
      " doesn't have an associated help field.";
    throw(AipsError(msg));
  }

  Bool ok = False;
  char input[80];
  
  while (ok==False) { 
    cout << x.GetHelp() << " [" << x.GetString() << "]: " 
      << x.GetKey() << "=";
    cin.getline(input,80);
#if defined(__GNUC__)
    if(input[0] != 0) {
#else
    if(input[0] != NULL) {
#endif
      String s = input;
      ok = x.Put(s);
    } else
      ok = True;
  }
};

void Input::EnvCreate(char *env, String key, String def)
{
  char *cp=getenv(env);
  String s;
  if (cp)
    s = cp;
  else
    s = def;
  CreatePar(1,key,s,"-","","","");
};

//      Create a pane file for Khoros
void Input::Pane()
{
  int n = Count(), opt, vcount=2;
  String p = GetString("argv0");
  String value, type;

  cout << "-F 4.2 1 0 170x7+10+20 +35+1 'CANTATA for KHOROS' cantata\n";
  cout << "-M 1 0 100x40+10+20 +23+1 'An AIPS++ program' aips++\n";
  
  ListIter<Param> parlist(ParList);       
  parlist.toStart();
  for(int i=0; i<n; i++, ++parlist) {       // loop over all Params
    Param &x = parlist.getRight();
    if (x.IsSystem()) continue;
    
    if(x.Get() == "???") {
      opt = 0;
      value = " ";
    } else {
      opt = 1;
      value = x.Get();
    }
    value = x.Get();
    if (value.length()==0) value=" ";
    
    type = x.GetType();
    if (type.contains("File")) {
      if (type == "InFile") 
	cout << "-I ";
      else if (type == "OutFile")
	cout << "-O ";
      else 
	cout << "-I ";
      cout << "1 0 " << opt << " 1 0 1 50x1+2+" << vcount
	<< " +0+0 '" << value << "' '"
	  << x.GetKey() << "' '"
	    << x.GetHelp() << "' "
	      << x.GetKey() << "\n";
    } else { 
      cout << "-s 1 0 " << opt << " 1 0 50x1+2+" << vcount
	<< " +0+0 '" << value << "' '"
	  << x.GetKey() << "' '"
	    << x.GetHelp() << "' "
	      << x.GetKey() << "\n";
    }
    vcount += 2;
  }
  cout << "-H 1 13x2+1+" << vcount
    << " 'Help' 'Help for " << p << "' aips.help\n";
  cout << "-R 1 0 1 13x2+39+" << vcount
    << " 'Run' 'RunMe' " << p << "\n";
  cout << "-E\n-E\n-E\n";
  
};

//      Show current key=val pairs, with program name
void Input::Keys()
{
  int n = Count();
  String p = GetString("argv0");
  
  cout << p;

  ListIter<Param> parlist(ParList);       
  parlist.toStart();
  for(int i=0; i<n; i++, ++parlist) {       // loop over all Params
    Param &x = parlist.getRight();
    if (x.IsSystem()) continue;
    cout << " " << x;
  }
  cout << "\n";
};

// Swallow arguments, setting keyword=value pairs appropriately from command
// line arguments in either "keyword=value" or "-keyword value" format. Argv[0]
// is assumed to be the program name.
void Input::ReadArguments(int ac, char *av[])
{
  int i;
  
  CreatePar(1,"argv0",av[0],"Program name", "", "", "");

  if(Debug(5))  cout << "MAIN> " << p_count << " program parameters\n";
  Close();             // close creation 
  
  // turn khoros style command inputs into keyword=value inputs
  //     ' -channels 64'  becomes  'channels=64'
  String thisarg;
  String keyandval;
  // Pass through keyword=value and turn -keyword value into keyword=value
  for(i=1; i<ac; i++) {
    thisarg = av[i];
    if (thisarg[0] == '-') {
      if (i >= ac - 1) throw(AipsError("Input::ReadArguments:"
				       "-keyword not followed by value"));
      i++;  // Advance
      keyandval = thisarg.after("-") + "=" + av[i];
    } else {
      keyandval = thisarg;
    }
    //	cout << "putting " << keyandval << endl;
    Put(keyandval);   // Insert command line parameters
    if(keyandval.before("=")=="debug") 
      debug_level = atoi(keyandval.after("=").chars());
    else if (keyandval.before("=")=="help")
      help_mode = keyandval.after("=");
  }
  Announce();          // Announce and possibly die here
};

Vector<Bool> Input::makeMaskFromRanges(const String &ranges, uInt length,
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
  split(ranges, expressions.storage(), numberOfCommas + 1, ",");
  
  
  for (uInt i=0; i < expressions.nelements(); i++) {
    // Validate
    if (expressions[i].contains(single) == False &&
	expressions[i].contains(range) == False) {
      throw(AipsError(String("Input::makeMaskFromRanges - "
			     "invalid range:") + expressions[i]));
    }
    int left, right;
    if (expressions[i].contains("-")) {
      left = atoi(expressions[i].before("-").chars());
      right = atoi(expressions[i].after("-").chars());
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
      throw(AipsError(String("Input::makeMaskFromRanges - "
			     "out of range or end<start ") + ranges));
    }
    mask(Slice(Int(left), Int(right-left+1))) = True;
  }
  
  return mask;
};







