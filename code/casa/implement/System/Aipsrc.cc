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
#include <aips/Utilities/String.h>
#include <fstream.h>
#include <strstream.h>
#include <aips/OS/EnvVar.h>

// This is the function that does most of the work. It is pretty kludgy.
// If the Maps become too large we will need to do something else, but
// for now this should work just fine.

Bool Aipsrc::matchKeyword(uInt &where,
			  const String &keyword,
			  uInt start) {
   if (doInit) parse();
 
   for (Int i=start; i<keywordName.nelements(); i++) {
     //cerr << "*" << keyword << "*" << keyInMap << "*" <<  pattern << "*" << endl;
     if (keywordPattern[i].length() > 1 && 
	 keyword.contains(Regex(keywordPattern[i]))) {
       where = i;
       return True;
     };
   };
   return False;
} 

Bool Aipsrc::find(String &value,	
		  String &fileFound,
		  String &lineFound,
		  const String &keyword,
		  uInt start) {
  uInt keyInMap;
  if (matchKeyword(keyInMap, keyword, start)) {
    value = keywordValue[keyInMap];
    for (Int i=0; i<keywordFile.nelements(); i++) {
      if (keyInMap < fileEnd[i]) {
	fileFound = keywordFile[i];
	break;
      };
    };
    lineFound = keywordLine[keyInMap];
    return True;
  };
  return False; 
}

Bool Aipsrc::find(String &value,
		  const String &keyword) {
  String file, line;
  return find(value, file, line, keyword, 0);
}

Bool Aipsrc::findNoHome(String &value,
			const String &keyword) {
  Int n = (keywordFile.nelements()<=0 ? 0 : fileEnd[0]);
  String file, line;
  return find(value, file, line, keyword, n);
}

Bool Aipsrc::find(String &value, const String &keyword, 
		  const String &deflt)
{
    Bool found = find(value, keyword);
    if (!found) {
	value = deflt;
    }
    return found;
}

Bool Aipsrc::findNoHome(String &value, const String &keyword,
			  const String &deflt)
{
    Bool found = findNoHome(value, keyword);
    if (!found) {
	value = deflt;
    }
    return found;
}


Bool Aipsrc::find(String &value,	
		  String &fileFound,
		  String &lineFound,
		  const String &keyword) {
  return find(value, fileFound, lineFound, keyword, 0);
}

Bool Aipsrc::findNoHome(String &value,
			String &fileFound,
			String &lineFound,
			const String &keyword) {
  Int n = (keywordFile.nelements()<=0 ? 0 : fileEnd[0]);
  return find(value, fileFound, lineFound, keyword, n);
}

void Aipsrc::reRead() {
  parse();
};

const Block<String> &Aipsrc::keywords() {
  if (doInit) parse();
  return keywordName;
}

const Block<String> &Aipsrc::values() {
  if (doInit) parse();
  return keywordValue;
}

const Block<String> &Aipsrc::lines() {
  if (doInit) parse();
  return keywordLine;
}

const Block<String> &Aipsrc::files() {
  if (doInit) parse();
  return keywordFile;
}

const Block<uInt> &Aipsrc::fileEnds() {
  if (doInit) parse();
  return fileEnd;
}

void Aipsrc::fillAips() {
  if (!filled) {
    home = EnvironmentVariables::value("HOME");
    if (home.empty())
      throw(AipsError(String("The HOME environment variable has not been set") +
		      "\n\t(see system administrator)"));
    home += "/aips++";
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
}

const String &Aipsrc:: aipsRoot() {
  fillAips();
  return root;
}

const String &Aipsrc:: aipsArch() {
  fillAips();
  return arch;
}

const String &Aipsrc:: aipsSite() {
  fillAips();
  return site;
}

const String &Aipsrc:: aipsHost() {
  fillAips();
  return host;
}

const String &Aipsrc:: aipsHome() {
  fillAips();
  return home;
}

uInt Aipsrc::parse() {
  // This parse based on order HOME, AIPSROOT, AIPSHOST, AIPSSITE, AIPSARCH
  fillAips();
  String filelist (EnvironmentVariables::value("HOME") + String("/.aipsrc:"));
  filelist += (root + String("/.aipsrc:"));
  filelist += (host + String("/aipsrc:"));
  filelist += (site + String("/aipsrc:"));
  filelist += (arch + String("/aipsrc:"));
  return parse(filelist);
}

uInt Aipsrc::parse(String &fileList) {
  doInit = False;		 // Indicate parse done
  keywordValue.resize(0, True);  // Clear the old values if any
  keywordLine.resize(0, True);
  keywordName.resize(0, True);
  keywordPattern.resize(0, True);
  keywordFile.resize(0, True);
  fileEnd.resize(0, True);
  Int nkw = 0;			// # of keywords found
  Int nfile = 0;		// # of files found
  
  // This here be the parse function. It looks through all the directories
  // looking for files to parse.  We can probably make it more sophisticated
  // but it should work fine.
  
  Int dirCount(fileList.freq(':') + 1);
  String *directories = new String[dirCount];
  dirCount = split(fileList, directories, dirCount, ":");
  for (Int i=0; i<dirCount; i++) {
    while (i>=keywordFile.nelements()) {
      keywordFile.resize(2*keywordFile.nelements() + 1);
      fileEnd.resize(keywordFile.nelements());
    };
    keywordFile[nfile] = directories[i];
    fileEnd[nfile]= nkw;

//   Ok now if we have a filename let's see if we can open it

    ifstream fileAipsrc((char *)keywordFile[nfile].chars());
    while (fileAipsrc.good()) {
      String buffer; 
      readline(fileAipsrc, buffer);
      if (!buffer.empty() && buffer[0] != '#') {    // Ignore comment lines
	String splitbuffer[2];
	String keyword, value;
	split(buffer, splitbuffer, 2, RXwhite);
	keyword = splitbuffer[0];
	value = splitbuffer[1];
	// cerr << keyword << "=" << value << endl;
	keyword.del(":");             // Strip that :
	while (nkw >= keywordName.nelements()) {
	  keywordName.resize(2*keywordName.nelements() + 1);
	  keywordValue.resize(keywordName.nelements());
	  keywordLine.resize(keywordName.nelements());
	  keywordPattern.resize(keywordName.nelements());
	};
	keywordValue[nkw] = value;
	keywordLine[nkw]  = buffer;
	keywordName[nkw]  = keyword;
	keywordPattern[nkw] = String("^");
	for (Int j=0; j<keyword.length(); j++) { // Build the pattern cause I
	                                         // cant figure out how to gsub it.
	  if (keyword[j] == '*') {
	    keywordPattern[nkw] += '.';
	  } else if (keyword[j] == '.') {
	    keywordPattern[nkw] += '\\';      // Work around since String/Regex
	  };      	                 // doesn't use extended regex
	  keywordPattern[nkw] += keyword[j];
	};
	nkw++;
	fileEnd[nfile] = nkw;
      };            
    };
    nfile++;
  };
  delete [] directories;

  // Resize static lists
  keywordValue.resize(nkw, True);  // Clear the old values if any
  keywordLine.resize(nkw, True);
  keywordName.resize(nkw, True);
  keywordPattern.resize(nkw, True);
  keywordFile.resize(nfile, True);
  fileEnd.resize(nfile, True);

  // cerr << *this << endl;
  return keywordValue.nelements();
}

//ostream & operator <<(ostream &oStream,
//		      const Aipsrc &keysValuesEtc) {
//  oStream << "Keyword-Value Pairs" << endl;
//  {
//    for(uInt i= 0; i < keysValuesEtc.keywordValue.ndefined(); i++) {
//      oStream << keysValuesEtc.keywordValue.getKey(i) << "\t" 
//	      << keysValuesEtc.keywordValue.getVal(i) << endl;
//    };
//  }
//  oStream << endl << "Keyword-File Pairs" << endl;
//  {
//   / for (uInt i= 0; i < keysValuesEtc.keywordFile.ndefined(); i++) {
//  //      oStream << keysValuesEtc.keywordFile.getKey(i) << "\t" 
//	      << keysValuesEtc.keywordFile.getVal(i) << endl;
//    };
//  }
//  oStream << endl << "Keyword-Line Pairs" << endl;
//  {
//    for (uInt i= 0; i < keysValuesEtc.keywordLine.ndefined(); i++) {
//      oStream << keysValuesEtc.keywordLine.getKey(i) << "\t" 
//	      << keysValuesEtc.keywordLine.getVal(i) << endl;
//    };
//  }
//  return oStream;
//}

void Aipsrc::show() {
  show(cout);
}

void Aipsrc::show(ostream &oStream) {
  if (doInit) parse();
  {
   Int start = 0;
   for (Int i = 0; i<keywordFile.nelements(); i++) {
     oStream << "Lines analysed on file: " << keywordFile[i] << endl;
     for (Int j = start; j<fileEnd[i]; j++) {
       oStream << j << ":	" << 
	 keywordName[j] << ":	" <<
	 keywordValue[j] << endl;
     };
     start = fileEnd[i];
   };
  }
  {
   Int start = 0;
   for (Int i = 0; i<keywordFile.nelements(); i++) {
     oStream << "Lines read on file: " << keywordFile[i] << endl;
     for (Int j = start; j<fileEnd[i]; j++) {
       oStream << j << ":	" << keywordLine[j] << endl;
     };
     start = fileEnd[i];
   };
  }
}

// Static Initializations -- Only really want to read the files once

Bool Aipsrc::doInit = True;
Block<String> Aipsrc::keywordName = Block<String>(0);
Block<String> Aipsrc::keywordPattern = Block<String>(0);
Block<String> Aipsrc::keywordValue = Block<String>(0);
Block<String> Aipsrc::keywordLine = Block<String>(0);
Block<String> Aipsrc::keywordFile = Block<String>(0);
Block<uInt> Aipsrc::fileEnd = Block<uInt>(0);
String Aipsrc::root = String();
String Aipsrc::arch = String();
String Aipsrc::site = String();
String Aipsrc::host = String();
String Aipsrc::home = String();
Bool Aipsrc::filled = False;
