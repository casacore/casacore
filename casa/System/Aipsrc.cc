//# Aipsrc.cc: Class to read the aipsrc general resource files 
//# Copyright (C) 1995,1996,1997,1998,2000,2001,2002,2003,2004,2016
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

//# Includes

#include <casacore/casa/System/Aipsrc.h>
#include <casacore/casa/Exceptions.h>
#include <casacore/casa/OS/EnvVar.h>
#include <casacore/casa/OS/RegularFile.h>
#include <casacore/casa/OS/Time.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/Utilities/MUString.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/fstream.h>
#include <casacore/casa/sstream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// This is the function that does most of the work. It is pretty slow for
// large maps, but no real problem.

bool Aipsrc::matchKeyword(uint32_t &where,  const String &keyword,
			  uint32_t start) {
  for (uint32_t i=start; i<keywordPattern.nelements(); i++) {
     if (keyword.contains(Regex(keywordPattern[i]))) {
       where = i;
       return true;
     }
   }
   return false;
} 

bool Aipsrc::find(String &value,	
		  const String &keyword,
		  uint32_t start) {
  std::call_once(theirCallOnceFlag, parse);
  return findNoParse(value, keyword, start);
}

bool Aipsrc::findNoParse(String &value,
                         const String &keyword,
                         uint32_t start) {
  uint32_t keyInMap;
  if (matchKeyword(keyInMap, keyword, start)) {
    value = keywordValue[keyInMap];
    return true;
  }
  return false; 
}

bool Aipsrc::find(String &value,
		  const String &keyword) {
  return find(value, keyword, 0);
}

bool Aipsrc::findNoHome(String &value,
			const String &keyword) {
  return find(value, keyword, fileEnd);
}

bool Aipsrc::find(String &value, const String &keyword, 
		  const String &deflt)
{
  return (find(value, keyword) ? true : (value = deflt, false));
}

bool Aipsrc::findNoHome(String &value, const String &keyword,
			  const String &deflt) {
  return (findNoHome(value, keyword) ? true : (value = deflt, false));
}

bool Aipsrc::find(uint32_t &value, const String &keyword,
		  int32_t Nname, const String tname[]) {
  String res;
  if (find(res, keyword)) {
    value = MUString::minimaxNC(res, Nname, tname);
    return ((int32_t)value < Nname ? true : false);
  }
  return false;
}

bool Aipsrc::find(uint32_t &value, const String &keyword,
		  const Vector<String> &tname) {
  String res;
  if (find(res, keyword)) {
    value = MUString::minimaxNC(res, tname);
    return (value < tname.nelements() ? true : false);
  }
  return false;
}

bool Aipsrc::find(uint32_t &value, const String &keyword,
		  int32_t Nname, const String tname[], const String &deflt) {
  if (!find(value, keyword, Nname, tname)) {
    value = MUString::minimaxNC(deflt, Nname, tname);
    return false;
  }
  return true;
}

bool Aipsrc::find(uint32_t &value, const String &keyword,
		  const Vector<String> &tname, const String &deflt) {
  if (!find(value, keyword, tname)) {
    value = MUString::minimaxNC(deflt, tname);
    return false;
  }
  return true;
}

bool Aipsrc::findDir(String& foundDir, const String& lastPart,
                     const Vector<String>& prepends,
                     const Vector<String>& appends,
                     bool useStds)
{
  // Setup a string that is either "/" + lastPart or blank.
  String myLastPart("");
  if (lastPart != "") {
    myLastPart += "/" + lastPart;
  }
  // Note that this function returns as soon as possible, i.e. it goes until it
  // matches or runs out of possibilities.
  for (uint32_t i = 0; i < prepends.nelements(); ++i) {
    foundDir = prepends[i] + myLastPart;
    File testPath(foundDir);
    if (testPath.isDirectory()) {
      return true;
    }
  }
  if (useStds) {
    // Test . using lastPart or ., not "".
    if (lastPart!= "") {
      foundDir = lastPart;
    } else {
      foundDir = ".";
    }
    File testDot(foundDir);
    if (testDot.isDirectory()) {
      return true;
    }
    foundDir = aipsHome() + myLastPart;
    File testAipsHome(foundDir);
    if (testAipsHome.isDirectory()) {
      return true;
    }
    foundDir = aipsRoot() + myLastPart;
    File testAipsRoot(foundDir);
    if (testAipsRoot.isDirectory()) {
      return true;
    }
  }
  for (uint32_t i = 0; i < appends.nelements(); ++i) {
    foundDir = appends[i] + myLastPart;
    File testPath(foundDir);
    if (testPath.isDirectory()) {
      return true;
    }
  }
  return false;
}

void Aipsrc::reRead() {
  parse(); // i.e. bypass theirCallOnce(parse)
}

double Aipsrc::lastRead() {
  return lastParse;
}

const Block<String> &Aipsrc::values() {
  std::call_once(theirCallOnceFlag, parse);
  return keywordValue;
}

const Block<String> &Aipsrc::patterns() {
  std::call_once(theirCallOnceFlag, parse);
  return keywordPattern;
}

void Aipsrc::fillAips() {
  uhome = EnvironmentVariable::get("HOME");
  if (uhome.empty()) {
    throw AipsError(String("The HOME environment variable has not been set"
                           "\n\t(see system administrator)"));
  }
 
  String aipsPath;
  if (extAipsPath.empty()) {
    aipsPath = EnvironmentVariable::get("CASAPATH");
    if (aipsPath.empty()) {
      aipsPath = EnvironmentVariable::get("AIPSPATH");
    }
  } else {
    aipsPath = extAipsPath;
  }
  // Set the path to home if not defined in any way.
  if (aipsPath.empty()) {
    setAipsPath(uhome);
    aipsPath = extAipsPath;
  }
  int32_t n = aipsPath.freq(' ') + aipsPath.freq('	') + 4;
  String *newdir = new String[n];
  n = split(aipsPath, newdir, n, Regex("[ 	]"));
  // Cater for non-existing fields
  for (int32_t i=n; i<4; i++) {
    newdir[i] = "UnKnOwN";
  }
  root = newdir[0];
  arch = root + "/" + newdir[1];
  site = arch + "/" + newdir[2];
  host = site + "/" + newdir[3];
  delete [] newdir;
}
  
void Aipsrc::setAipsPath(const String &path) {
  if (extAipsPath.empty()) {
    extAipsPath = path + " ";
  }
}

const String &Aipsrc::aipsRoot() {
  std::call_once(theirCallOnceFlag, parse);
  return root;
}

const String &Aipsrc::aipsArch() {
  std::call_once(theirCallOnceFlag, parse);
  return arch;
}

const String &Aipsrc::aipsSite() {
  std::call_once(theirCallOnceFlag, parse);
  return site;
}

const String &Aipsrc::aipsHost() {
  std::call_once(theirCallOnceFlag, parse);
  return host;
}

const String &Aipsrc::aipsHome() {
  std::call_once(theirCallOnceFlag, parse);
  return home;
}

uint32_t Aipsrc::registerRC(const String &keyword, Block<String> &nlst) {
  uint32_t n;
  for (n=0; n<nlst.nelements(); n++) {
    if (nlst[n] == keyword) break;
  }
  n++;
  if (n>nlst.nelements()) {
    nlst.resize(n);
  }
  nlst[n-1] = keyword;
  return n;
}

uint32_t Aipsrc::registerRC(const String &keyword,
			const String &deflt) {
  uint32_t n = Aipsrc::registerRC(keyword, nstrlst);
  strlst.resize(n);
  find (strlst[n-1], keyword, deflt);
  return n;
}

uint32_t Aipsrc::registerRC(const String &keyword,
			int32_t Nname, const String tname[], 
			const String &deflt) {
  uint32_t n = Aipsrc::registerRC(keyword, ncodlst);
  codlst.resize(n);
  find (codlst[n-1], keyword, Nname, tname, deflt);
  return n;
}

uint32_t Aipsrc::registerRC(const String &keyword,
			const Vector<String> &tname, const String &deflt) {
  uint32_t n = Aipsrc::registerRC(keyword, ncodlst);
  codlst.resize(n);
  find (codlst[n-1], keyword, tname, deflt);
  return n;
}

const String &Aipsrc::get(uint32_t keyword) {
  AlwaysAssert(keyword>0 && keyword<=strlst.nelements(), AipsError);
  return strlst[keyword-1];
}

const uint32_t &Aipsrc::get(uint32_t &code, uint32_t keyword) {
  AlwaysAssert(keyword>0 && keyword<=codlst.nelements(), AipsError);
  code = codlst[keyword-1];
  return codlst[keyword-1];
}

void Aipsrc::set(uint32_t keyword, const String &deflt) {
  AlwaysAssert(keyword>0 && keyword<=strlst.nelements(), AipsError);
  strlst[keyword-1] = deflt;
}
	       
void Aipsrc::set(uint32_t keyword,
		 int32_t Nname, const String tname[], const String &deflt) {
  AlwaysAssert(keyword>0 && keyword<=codlst.nelements(), AipsError);
  find (codlst[keyword-1], String::toString(keyword), Nname, tname, deflt);
}

void Aipsrc::set(uint32_t keyword,
		 const Vector<String> &tname, const String &deflt) {
  AlwaysAssert(keyword>0 && keyword<=codlst.nelements(), AipsError);
  find (codlst[keyword-1], String::toString(keyword), tname, deflt);
}

void Aipsrc::save(uint32_t keyword) {
  AlwaysAssert(keyword>0 && keyword<=strlst.nelements(), AipsError);
  Aipsrc::save(nstrlst[keyword-1], strlst[keyword-1]);
}

void Aipsrc::save(uint32_t keyword, const String tname[]) {
  AlwaysAssert(keyword>0 && keyword<=codlst.nelements(), AipsError);
  Aipsrc::save(ncodlst[keyword-1], tname[codlst[keyword-1]]);
}

void Aipsrc::save(uint32_t keyword, const Vector<String> &tname) {
  AlwaysAssert(keyword>0 && keyword<=codlst.nelements(), AipsError);
  Aipsrc::save(ncodlst[keyword-1], tname(codlst[keyword-1]));
}

// Note that the parameters should not be references!
void Aipsrc::save(const String keyword, const String val) {
  static uint32_t nv_r = Aipsrc::registerRC("user.aipsrc.edit.keep", "5");
  static String editTxt = "# Edited at ";
  std::call_once(theirCallOnceFlag, parse);
  String filn(uhome + "/.aipsrc");
  String filno(filn + ".old");
  RegularFile fil(filn);
  RegularFile filo(filno);
  if (fil.exists()) {
    fil.move(filno, true);
  } else if (filo.exists()) {
    filo.remove();
  }
  ofstream ostr(filn.chars(), ios::out);
  ostr << editTxt << 
    MVTime(Time()).string(MVTime::YMD | MVTime::LOCAL, 0) << endl;
  ostr << keyword << ":	" << val << endl;
  fil = RegularFile(filno);
  char *buf = new char[8192];	// Single lines must fit in this
  if (fil.exists()) {
    String buffer;
    int32_t nv = atoi(Aipsrc::get(nv_r).chars());	// number to keep
    bool editSeen = false;	// if edit line seen
    String editBuf;		// edit line buffer
    int32_t editCnt = 0;		// count for edits
    String kwt = keyword + ":";  // keyword test
    ifstream istr(filno.chars(), ios::in );
    while (istr.getline(buf, 8192)) {
      buffer = buf;
      if (editSeen) {
	if (buffer.index(kwt) == 0) {
	  editCnt++;
	  if (editCnt < nv) { // copy
	    ostr << editBuf << endl;
	    ostr << buffer << endl;
	  }
	  editSeen = false;
	  continue;
	} else {
	  ostr << editBuf << endl;
	}
      }
      editSeen = (buffer.index(editTxt) == 0);
      if (editSeen) {
	editBuf = buffer;
      } else {
	ostr << buffer << endl;
      }
    }
  }
  delete [] buf;
}
  
void Aipsrc::parse() {
  // Refill basic data
  fillAips();

  // If defined, use setting of CASARCFILES. Make sure it ends with a colon.
  String filelist = EnvironmentVariable::get("CASARCFILES");
  if (! filelist.empty()) {
    filelist += ':';
  } else {
    // Otherwise use CASAPATH.
    // This parse based on order HOME, AIPSROOT, AIPSHOST, AIPSSITE, AIPSARCH
    filelist  = uhome + String("/.casarc:");
    filelist += uhome + String("/.casa/rc:");
    filelist += uhome + String("/.aipsrc:");
    filelist += (root + String("/.aipsrc:"));
    filelist += (host + String("/aipsrc:"));
    filelist += (site + String("/aipsrc:"));
    filelist += (arch + String("/aipsrc:"));
  }
  doParse(filelist);

  String x;
  // Use findNoParse() variant to avoid recursion back into parse().
  if (findNoParse(x, String("user.aipsdir"), 0)) {
    home = x;
  } else {
    home = uhome + "/aips++";
  }
}

void Aipsrc::doParse(String &fileList) {
  Time x;
  lastParse = x.modifiedJulianDay();	// Save time of parse
  int32_t nkw = Aipsrc::genParse(Aipsrc::keywordPattern,
                             Aipsrc::keywordValue, 
                             Aipsrc::fileEnd, fileList);
  const String gs00(".");	// make correct patterns
  const String gs01("\\.");
  const String gs10("*");
  const String gs11(".*");
  String keyword;
  for (int32_t i=0; i<nkw; i++) {
    keyword = keywordPattern[i];
    keyword.gsub(gs00, gs01);
    keyword.gsub(gs10, gs11);
    keywordPattern[i] = String("^") + keyword + String("$");
  }
}

uint32_t Aipsrc::genParse(Block<String> &keywordPattern, 
		      Block<String> &keywordValue,
		      uint32_t &fileEnd, const String &fileList) {
  keywordValue.resize(0, true);  // Clear the old values if any
  keywordPattern.resize(0, true);
  Block<String> keywordFile;
  fileEnd = 0;
  uint32_t nkw = 0;			// # of keywords found
  int32_t nfile = 0;		// # of files found
  
  // This here be the parse function. It looks through all the directories
  // looking for files to parse.
  
  int32_t dirCount(fileList.freq(':') + 1);
  String *directories = new String[dirCount];
  dirCount = split(fileList, directories, dirCount, ":");
  keywordFile.resize(dirCount);
  char *buf = new char[8192];
  for (int32_t i=0; i<dirCount; i++) {
    keywordFile[nfile] = directories[i];
    if (i == 0) fileEnd = nkw;

    //   Ok now if we have a filename let's see if we can open it
    if (! keywordFile[nfile].empty()) {
      File fil(keywordFile[nfile]);
      if (fil.exists()) {
	ifstream fileAipsrc(keywordFile[nfile].chars(), ios::in);
	String buffer;
	String keyword;
	String value;
	const Regex comm("^[ 	]*#");	// Comment line
	while (fileAipsrc.getline(buf, 8192)) {
	  buffer = buf;
	  if (buffer.empty() || buffer.contains(comm))	// Ignore comments
	    continue;
          String::size_type inx = buffer.find(':');
          if (inx != String::npos) {
	    keyword = buffer.before(inx);
	    value = buffer.after(inx);
            keyword.trim();
            value.trim();
	    if (keyword.length() < 1)
	      continue;
	    while (nkw >= keywordPattern.nelements()) {
	      keywordPattern.resize(2*keywordPattern.nelements() + 1);
	      keywordValue.resize(keywordPattern.nelements());
	    }
	    keywordValue[nkw] = value;
	    keywordPattern[nkw] = keyword;
	    nkw++;
	    if (i == 0) fileEnd = nkw;
	  }            
	}
      }
    }
    nfile++;
  }
  delete [] buf;
  delete [] directories;

  // Resize static lists
  keywordValue.resize(nkw, true);
  keywordPattern.resize(nkw, true);

  return keywordValue.nelements();
}

void Aipsrc::show() {
  show(cout);
}

void Aipsrc::show(ostream &oStream) {
  std::call_once(theirCallOnceFlag, parse);
  String nam;
  const String gs00(".*");
  const String gs01("*");
  const String gs10("\\.");
  const String gs11(".");
  oStream << keywordValue.nelements() <<
    " keyword/value pairs found:" << endl;
  for (uint32_t j = 0; j<keywordValue.nelements(); j++) {
    nam = keywordPattern[j];
    nam.gsub(gs00, gs01);
    nam.gsub(gs10, gs11);
    oStream << j << ":	" << 
      nam << ":	" <<
      keywordValue[j] << endl;
  }
}

// General usage routines
uint32_t Aipsrc::genRestore(Vector<String> &namlst, Vector<String> &vallst,
			const String &fileList) {
  uint32_t ef;
  Block<String> nl;
  Block<String> vl;
  int32_t nkw = Aipsrc::genParse(nl, vl, ef, fileList);
  Block<String> nla;
  Block<String> vla;
  nla.resize(0);
  vla.resize(0);
  uint32_t n;
  for (int32_t i=nkw-1; i>=0; i--) {	// reverse order to do aipsrc like
    if (!nl[i].contains('*')) {		// no wild cards
      n = Aipsrc::registerRC(nl[i], nla);
      vla.resize(n);
      vla[n-1] = vl[i];
    }
  }
  namlst = Vector<String>(nla.begin(), nla.end());
  vallst = Vector<String>(vla.begin(), vla.end());
  return namlst.nelements();
}

void Aipsrc::genSave(Vector<String> &namlst, Vector<String> &vallst,
		     const String &fnam) {
  static String editTxt = "# Saved at ";
  String filno(fnam + ".old");
  RegularFile fil(fnam);
  RegularFile filo(filno);
  if (fil.exists()) {
    fil.move(filno, true);
  } else if (filo.exists()) {
    filo.remove();
  }
  ofstream ostr(fnam.chars(), ios::out);
  ostr << editTxt << 
    MVTime(Time()).string(MVTime::YMD | MVTime::LOCAL, 0) << endl;
  for (int32_t i=namlst.nelements()-1; i>=0; i--) {
    ostr << namlst(i) << ":	" << vallst(i) << endl;
  }
}

void Aipsrc::genSet(Vector<String> &namlst, Vector<String> &vallst,
		    const String &nam, const String &val) {
  Block<String> nl = makeBlock(namlst);
  uint32_t n = Aipsrc::registerRC(nam, nl);
  if (n > vallst.nelements()) vallst.resize(n, true);
  vallst(n-1) = val;
//   if (n > namlst.nelements()) namlst.resize(n, true);
  namlst.resize(0);
  namlst = Vector<String>(nl.begin(), nl.end());
}

bool Aipsrc::genUnSet(Vector<String> &namlst, Vector<String> &vallst,
		      const String &nam) {
  uint32_t n;
  uint32_t N = namlst.nelements();
  for (n=0; n<N; n++) {
    if (namlst(n) == nam) break;
  }
  n++;
  if (n>N) return false;
  for (uint32_t i=n; i<N; i++) {
    namlst(i-1) = namlst(i);
    vallst(i-1) = vallst(i);
  }
  namlst.resize(N-1, true);
  vallst.resize(N-1, true);
  return true;
}

bool Aipsrc::genGet(String &val, Vector<String> &namlst, Vector<String> &vallst,
		    const String &nam) {
  uint32_t n;
  for (n=0; n<namlst.nelements(); n++) {
    if (namlst(n) == nam) break;
  }
  n++;
  if (n>vallst.nelements()) return false;
  val = vallst(n-1);
  return true;
}

  // Static Initializations -- Only really want to read the files once

  std::once_flag Aipsrc::theirCallOnceFlag;
  double Aipsrc::lastParse = 0;
  Block<String> Aipsrc::keywordPattern(0);
  Block<String> Aipsrc::keywordValue(0);
  uint32_t Aipsrc::fileEnd = 0;
  String Aipsrc::extAipsPath  = String();
  String Aipsrc::root = String();
  String Aipsrc::arch = String();
  String Aipsrc::site = String();
  String Aipsrc::host = String();
  String Aipsrc::home = String();
  String Aipsrc::uhome= String();
  bool Aipsrc::filled = false;
  Block<String> Aipsrc::strlst(0);
  Block<String> Aipsrc::nstrlst(0);
  Block<uint32_t> Aipsrc::codlst(0);
  Block<String> Aipsrc::ncodlst(0);

} //# NAMESPACE CASACORE - END

