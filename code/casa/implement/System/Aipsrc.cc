//# Aipsrc.cc: Class to read the aipsrc general resource files 
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

//# Includes

#include <aips/Tasking/Aipsrc.h>
#include <aips/Exceptions.h>
#include <aips/OS/EnvVar.h>
#include <aips/OS/File.h>
#include <aips/OS/Time.h>
#include <aips/Arrays/Vector.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/Regex.h>
#include <aips/Measures/MUString.h>
#include <iostream.h>
#include <fstream.h>
#include <strstream.h>

// This is the function that does most of the work. It is pretty slow for
// large maps, but no real problem.

Bool Aipsrc::matchKeyword(uInt &where,  const String &keyword,
			  uInt start) {
  if (doInit) parse();
 
  for (Int i=start; i<keywordPattern.nelements(); i++) {
     if (keyword.contains(Regex(keywordPattern[i]))) {
       where = i;
       return True;
     };
   };
   return False;
} 

Bool Aipsrc::find(String &value,	
		  const String &keyword,
		  uInt start) {
  uInt keyInMap;
  if (matchKeyword(keyInMap, keyword, start)) {
    value = keywordValue[keyInMap];
    return True;
  };
  return False; 
}

Bool Aipsrc::find(String &value,
		  const String &keyword) {
  return find(value, keyword, 0);
}

Bool Aipsrc::findNoHome(String &value,
			const String &keyword) {
  return find(value, fileEnd);
}

Bool Aipsrc::find(String &value, const String &keyword, 
		  const String &deflt)
{
  return (find(value, keyword) ? True : (value = deflt, False));
}

Bool Aipsrc::findNoHome(String &value, const String &keyword,
			  const String &deflt) {
  return (findNoHome(value, keyword) ? True : (value = deflt, False));
}

Bool Aipsrc::find(uInt &value, const String &keyword,
		  Int Nname, const String tname[]) {
  String res;
  if (find(res, keyword)) {
    value = MUString::minimaxNC(res, Nname, tname);
    return (value < Nname ? True : False);
  };
  return False;
}

Bool Aipsrc::find(uInt &value, const String &keyword,
		  const Vector<String> &tname) {
  String res;
  if (find(res, keyword)) {
    value = MUString::minimaxNC(res, tname);
    return (value < tname.nelements() ? True : False);
  };
  return False;
}

Bool Aipsrc::find(uInt &value, const String &keyword,
		  Int Nname, const String tname[], const String &deflt) {
  if (!find(value, keyword, Nname, tname)) {
    value = MUString::minimaxNC(deflt, Nname, tname);
    return False;
  };
  return True;
}

Bool Aipsrc::find(uInt &value, const String &keyword,
		  const Vector<String> &tname, const String &deflt) {
  if (!find(value, keyword, tname)) {
    value = MUString::minimaxNC(deflt, tname);
    return False;
  };
  return True;
}

void Aipsrc::reRead() {
  parse();
}

Double Aipsrc::lastRead() {
  return lastParse;
}

const Block<String> &Aipsrc::values() {
  if (doInit) parse();
  return keywordValue;
}

const Block<String> &Aipsrc::patterns() {
  if (doInit) parse();
  return keywordPattern;
}

const String &Aipsrc::fillAips(const String &nam) {
  if (!filled) {
    uhome = EnvironmentVariables::value("HOME");
    if (uhome.empty())
      throw(AipsError(String("The HOME environment variable has not been set") +
		      "\n\t(see system administrator)"));
    String aipsPath = EnvironmentVariables::value("AIPSPATH");
    if (aipsPath.empty())
      throw(AipsError(String("The AIPSPATH environment variable has not been set") +
		      "\n\t(see system administrator)"));
    Int n = aipsPath.freq(' ') + aipsPath.freq('	') + 1;
    String *newdir = new String[n];
    n = split(aipsPath, newdir, n, Regex("[ 	]"));
    root = newdir[0];
    arch = root + "/" + newdir[1];
    site = arch + "/" + newdir[2];
    host = site + "/" + newdir[3];
    delete [] newdir;
    filled = True;
  };
  return nam;
}

const String &Aipsrc::aipsRoot() {
  return fillAips(root);
}

const String &Aipsrc::aipsArch() {
  return fillAips(arch);
}

const String &Aipsrc::aipsSite() {
  return fillAips(site);
}

const String &Aipsrc::aipsHost() {
  return fillAips(host);
}

const String &Aipsrc::aipsHome() {
  if (doInit) parse();
  return fillAips(home);
}

uInt Aipsrc::registerRC(const String &keyword, Block<String> &nlst) {
  uInt n;
  for (n=0; n<nlst.nelements(); n++) {
    if (nlst[n] == keyword) break;
  };
  n++;
  if (n>nlst.nelements()) {
    nlst.resize(n);
  };
  nlst[n-1] = keyword;
  return n;
}

uInt Aipsrc::registerRC(const String &keyword,
			const String &deflt) {
  uInt n = Aipsrc::registerRC(keyword, nstrlst);
  strlst.resize(n);
  find (strlst[n-1], keyword, deflt);
  return n;
}

uInt Aipsrc::registerRC(const String &keyword,
			Int Nname, const String tname[], 
			const String &deflt) {
  uInt n = Aipsrc::registerRC(keyword, ncodlst);
  codlst.resize(n);
  find (codlst[n-1], keyword, Nname, tname, deflt);
  return n;
}

uInt Aipsrc::registerRC(const String &keyword,
			const Vector<String> &tname, const String &deflt) {
  uInt n = Aipsrc::registerRC(keyword, ncodlst);
  codlst.resize(n);
  find (codlst[n-1], keyword, tname, deflt);
  return n;
}

const String &Aipsrc::get(uInt keyword) {
  AlwaysAssert(keyword>0 && keyword<=strlst.nelements(), AipsError);
  return strlst[keyword-1];
}

const uInt &Aipsrc::get(uInt &code, uInt keyword) {
  AlwaysAssert(keyword>0 && keyword<=codlst.nelements(), AipsError);
  code = codlst[keyword-1];
  return codlst[keyword-1];
}

void Aipsrc::set(uInt keyword, const String &deflt) {
  AlwaysAssert(keyword>0 && keyword<=strlst.nelements(), AipsError);
  strlst[keyword-1] = deflt;
}
	       
void Aipsrc::set(uInt keyword,
		 Int Nname, const String tname[], const String &deflt) {
  AlwaysAssert(keyword>0 && keyword<=codlst.nelements(), AipsError);
  find (codlst[keyword-1], keyword, Nname, tname, deflt);
}

void Aipsrc::set(uInt keyword,
		 const Vector<String> &tname, const String &deflt) {
  AlwaysAssert(keyword>0 && keyword<=codlst.nelements(), AipsError);
  find (codlst[keyword-1], keyword, tname, deflt);
}
  
uInt Aipsrc::parse() {
  // Refill basic data
  filled = False;
  // This parse based on order HOME, AIPSROOT, AIPSHOST, AIPSSITE, AIPSARCH
  String filelist = fillAips(uhome) + String("/.aipsrc:");
  filelist += (root + String("/.aipsrc:"));
  filelist += (host + String("/aipsrc:"));
  filelist += (site + String("/aipsrc:"));
  filelist += (arch + String("/aipsrc:"));
  uInt i = parse(filelist);
  String x;
  if (find(x, String("user.aipsdir"), String("/aips++")))
    home = x;
  else
    home = uhome + x;
  return i;
}

uInt Aipsrc::parse(String &fileList) {
  doInit = False;		 // Indicate parse done
  Time x;
  lastParse = x.modifiedJulianDay();	// Save time of parse
  keywordValue.resize(0, True);  // Clear the old values if any
  keywordPattern.resize(0, True);
  Block<String> keywordFile;
  fileEnd = 0;
  Int nkw = 0;			// # of keywords found
  Int nfile = 0;		// # of files found
  
  // This here be the parse function. It looks through all the directories
  // looking for files to parse.
  
  Int dirCount(fileList.freq(':') + 1);
  String *directories = new String[dirCount];
  dirCount = split(fileList, directories, dirCount, ":");
  keywordFile.resize(dirCount);
  for (Int i=0; i<dirCount; i++) {
    keywordFile[nfile] = directories[i];
    if (i == 0) fileEnd = nkw;

    //   Ok now if we have a filename let's see if we can open it
    File fil(keywordFile[nfile]);
    if (fil.exists()) {
      ifstream fileAipsrc(keywordFile[nfile], ios::in | ios::nocreate);
      String buffer;
      Char buf[8192];	// Single lines must fit in this
      String keyword;
      String value;
      const Regex comm("^[ 	]*#");	// Comment line
      const Regex defin(":[ 	]*");	// Line with value
      const Regex lspace("^[ 	]*");	// Leading spaces
      const String gs00(".");	// make correct patterns
      const String gs01("\\.");
      const String gs10("*");
      const String gs11(".*");
      while (fileAipsrc.getline(&buf[0], sizeof(buf))) {
	buffer = buf;
	if (buffer.empty() || buffer.contains(comm))	// Ignore comments
	  continue;
	buffer = buffer.after(lspace);
	if (buffer.contains(defin)) {		// value defined
	  keyword = buffer.before(defin);
	  value = buffer.after(defin);
	  if (keyword.length() < 1)
	    continue;
	  while (nkw >= keywordPattern.nelements()) {
            keywordPattern.resize(2*keywordPattern.nelements() + 1);
	    keywordValue.resize(keywordPattern.nelements());
	  };
	  keywordValue[nkw] = value;
	  keyword.gsub(gs00, gs01);
	  keyword.gsub(gs10, gs11);
	  keywordPattern[nkw] = String("^") + keyword + String("$");
	  nkw++;
	  if (i == 0) fileEnd = nkw;
	};            
      };
    };
    nfile++;
  };
  delete [] directories;

  // Resize static lists
  keywordValue.resize(nkw, True);
  keywordPattern.resize(nkw, True);

  return keywordValue.nelements();
}

void Aipsrc::show() {
  show(cout);
}

void Aipsrc::show(ostream &oStream) {
  if (doInit) parse();
  String nam;
  const String gs00(".*");
  const String gs01("*");
  const String gs10("\\.");
  const String gs11(".");
  oStream << keywordValue.nelements() <<
    " keyword/value pairs found:" << endl;
  for (Int j = 0; j<keywordValue.nelements(); j++) {
    nam = keywordPattern[j];
    nam.gsub(gs00, gs01);
    nam.gsub(gs10, gs11);
    oStream << j << ":	" << 
      nam << ":	" <<
      keywordValue[j] << endl;
  };
}

// Static Initializations -- Only really want to read the files once

Bool Aipsrc::doInit = True;
Double Aipsrc::lastParse = 0;
Block<String> Aipsrc::keywordPattern(0);
Block<String> Aipsrc::keywordValue(0);
uInt Aipsrc::fileEnd = 0;
String Aipsrc::root = String();
String Aipsrc::arch = String();
String Aipsrc::site = String();
String Aipsrc::host = String();
String Aipsrc::home = String();
String Aipsrc::uhome= String();
Bool Aipsrc::filled = False;
Block<String> Aipsrc::strlst(0);
Block<String> Aipsrc::nstrlst(0);
Block<uInt> Aipsrc::codlst(0);
Block<String> Aipsrc::ncodlst(0);
