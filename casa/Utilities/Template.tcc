//# Template.cc: Canonicalise, format etc. Casacore template definitions
//# Copyright (C) 2001-2005
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

#ifndef CASA_TEMPLATE_TCC
#define CASA_TEMPLATE_TCC

//# Includes
#include <casacore/casa/Utilities/Template.h>

#include <casacore/casa/ostream.h>
#include <casacore/casa/fstream.h>
#include <casacore/casa/typeinfo.h>
#include <casacore/casa/stdlib.h>
#include <casacore/casa/sstream.h>

#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/Utilities/Sort.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN
  
  //# Static constants
  // Patterns to analyse an input line
  const Regex Template::spaces	   = String("^[[:space:]]*$");
  const Regex Template::comment    = String("^[[:space:]]*#");
  const Regex Template::ifRE 	   = String("^[[:space:]]*#if");
  const Regex Template::endifRE    = String("^[[:space:]]*#endif[[:space:]]*$");
  const Regex Template::elseRE 	   = String("^[[:space:]]*#else[[:space:]]*$");
  const Regex Template::templateRE = String("^[[:space:]]*template[[:space:]<]");
  const Regex Template::contRE 	   = String("^[[:space:]]*=");
  const Regex Template::fileRE 	   = String("^[[:space:]]*[[:digit:]]*"
					    "[[:space:]]*"
					    "[^.[:space:]]*[.](cc|h)");
  const Regex Template::typedefRE  = String("^[[:space:]]*typedef[[:space:]]");
  const Regex Template::auxtemplRE = String("^[[:space:]]*"
					    "AIPS_[A-Z0-9]*_AUX_TEMPLATES"
					    "[[:space:](]");
  const Regex Template::namespaceRE= String("^[[:space:]]*#namespace");

// Simple pattern and replacements to make canonical templates files
const Regex Template::PATcanon[Ncanon] = {
  String("[[:space:]]"),		// 00
  String(" ="),
  String("   *"),
  String("^ "),
  String(" $"),
  String(" [(]"),
  String(" ,"),
  String(","),
  String("&"),
  String("[*][*]"),
  String(" [*] [*]"),			// 10
  String("[(] [*][)]"),
  String("[(] *"),
  String("[*]const"),
  String(" operator "),
  String(" operator[ ]*&[ ]*&[(]"),
  String(" <"),
  String("< "),
  String(" >"),
  String(">>"),
  String(">>"),				// 20
  String("operator> >[(]"),
  String(" template *< *>"),
  String(" template *<"),
  String("> *class "),
  String("short unsigned int"),
  String("unsigned short int"),
  String("short signed int"),
  String("signed short int"),
  String("long unsigned int"),
  String("unsigned long int"),		// 30
  String("long signed int"),
  String("signed long int"),
  String("unsigned char"),
  String("signed char"),
  String("unsigned short"),
  String("short unsigned"),
  String("signed short"),
  String("short signed"),
  String("short int"),
  String("unsigned long"),		// 40
  String("long unsigned"),
  String("signed long"),
  String("long signed"),
  String("long int"),
  String("long float"),
  String("long double"),
  String("unsigned int"),
  String("signed int"),
  String(" *;+$"),
  String("  *"),			// 50
  String(">const")
};
const String Template::REPcanon[Ncanon] = {
  " ",					// 00
  " = ",
  " ",
  "",
  "",
  "(",
  ",",
  ", ",
  " &",
  "* *",
  " **",				// 10
  "(*)",
  "(",
  "* const",
  " operator",
  " operator&&(",
  "<",
  "<",
  ">",
  "> >",
  "> >",				// 20
  "operator>>(",
  " template <> ",
  " template <",
  "> class ",
  "uShort",
  "uShort",
  "Short",
  "Short",
  "uLong",
  "uLong",				// 30
  "Long",
  "Long",
  "uChar",
  "Char",
  "uShort",
  "uShort",
  "Short",
  "Short",
  "Short",
  "uLong",				// 40
  "uLong",
  "Long",
  "Long",
  "Long",
  "Double",
  "lDouble",
  "uInt",
  "Int",
  "",
  " ",					// 50
  "> const"
};

const Regex Template::PATcanon20[Ncanon2] = {
  String("[^[:alnum:]_]char[^[:alnum:]_]"),		// 00
  String("[^[:alnum:]_]short[^[:alnum:]_]"),
  String("[^[:alnum:]_]unsigned[^[:alnum:]_]"),
  String("[^[:alnum:]_]signed[^[:alnum:]_]"),
  String("[^[:alnum:]_]int[^[:alnum:]_]"),
  String("[^[:alnum:]_]long[^[:alnum:]_]"),
  String("[^[:alnum:]_]float[^[:alnum:]_]"),
  String("[^[:alnum:]_]double[^[:alnum:]_]"),
  String("[^[:alnum:]_]complex<Float>"),
  String("[^[:alnum:]_]complex<Double>"),
  String("<Complex >"),					// 10
  String("<DComplex >"),
  String("[^[:alnum:]_]bool[^[:alnum:]_]"),
  String("std::Complex"),
  String("std::DComplex")
};
const Regex Template::PATcanon21[Ncanon2] = {
  String("char"),					// 00
  String("short"),
  String("unsigned"),
  String("signed"),
  String("int"),
  String("long"),
  String("float"),
  String("double"),
  String("complex<Float>"),
  String("complex<Double>"),
  String("<Complex >"),					// 10
  String("<DComplex >"),
  String("bool"),
  String("std::Complex"),
  String("std::DComplex")
};
const String Template::REPcanon2[Ncanon2] = {
  "Char",						// 00
  "Short",
  "uInt",
  "Int",
  "Int",
  "Long",
  "Float",
  "Double",
  "Complex",
  "DComplex",
  "<Complex>",						// 10
  "<DComplex>",
  "Bool",
  "std::complex<Float>",
  "std::complex<Double>"
};
  
// Make canonical numbers of 4 digits minimum
const Regex Template::PATnmin[Nnmin] = {
  String("^[^[:digit:]]"),				// 00
  String("^[[:digit:]] "),
  String("^[[:digit:]][[:digit:]] "),
  String("^[[:digit:]][[:digit:]][[:digit:]] ")
};
const String Template::REPnmin[Nnmin] = {
  "0000 ",						// 00
  "000",
  "00",
  "0"
};
  
// Make canonical numbers of 4 digits maximum
const Regex Template::PATnmax[Nnmax] = {
  String("^[[:digit:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]]+ ")
};
const Regex Template::REPnmax[Nnmax] = {
  String("[[:digit:]][[:digit:]][[:digit:]][[:digit:]] ")
};

// Patterns to split off number and name
const Regex Template::splitnum = String("^[[:digit:]]+ ");
const Regex Template::splitnam = String("^[^ ]+ ");

// Patterns to check the saved template line
const Regex Template::sifRE("^#if");
const Regex Template::stemRE("^template");
const Regex Template::sconstRE("([(] *const|, *const|< *const)");
const Regex Template::sretRE1("^template class ");
const Regex Template::sretRE2("^template [^ ]*[(]");
const Regex Template::sretRE3("^template <");
const Regex Template::sretRE4("[^t][^o][^r>]>[(]");
const Regex Template::stypedefRE("^typedef");
const Regex Template::sauxtemplRE("^AIPS_[A-Z0-9]*_AUX_TEMPLATES");
const Regex Template::snamespaceRE("^#namespace");

// Replacement patterns for ifs in saved line
const String Template::PATinif[Ninif] = {
  String("& &"),				// 00
  String("= ="),
  String("<"),
  String("< ="),
  String(">")
};
const String Template::REPinif[Ninif] = {
  "&&",						// 00
  "==",
  " < ",
  "<=",
  " >"
};

// Tests for finding real templates for duplicate tests
const Regex Template::classprelude("^.*template[[:space:]]*class[[:space:]]*");
const Regex Template::functionprelude("^.*template[[:space:]]*");
const Regex Template::forwardprelude("^.*template[[:space:]]*<");
const Regex Template::funcnameprelude("[^[:space:]]*[:(]");
const Regex Template::mylistprelude("^[[:space:]]*[[:digit:]]+: ");

// Data to remove spaces at begin, end, make single, count/remove const
const Regex Template::leadsp("^[[:space:]]+");
const Regex Template::endsp("[[:space:]]+$");
const Regex Template::mulsp("[[:space:]]+");
const Regex Template::constsp("const");
const String Template::nullsp;
const String Template::singlesp(" ");

// Patterns to make all typedefs comparisons for duplicates possible
// Note that the first three should be in that position for run-time
// change on some systems.
const Regex Template::PATtypedef0[Ntypedef] = {
  String("[^[:alnum:]_]FitsLong[^[:alnum:]_]"),			// 00
  String("[^[:alnum:]_]lDouble[^[:alnum:]_]"),
  String("[^[[:alnum:]_]Long[^[:alnum:]_]"),
  String("[[:alnum:]_]DataStatus[[:alnum:]_]"),
  String("[[:alnum:]_]LogicalRecord[[:alnum:]_]"),
  String("[[:alnum:]_]TapeHeader[[:alnum:]_]"),
  String("[[:alnum:]_]Convolver<Double>[[:alnum:]_]"),
  String("[[:alnum:]_]Convolver<Float>][[:alnum:]_]"),
  String("[[:alnum:]_]AipsrcValue<Bool>[[:alnum:]_]"),
  String("[[:alnum:]_]AipsrcValue<Double>[[:alnum:]_]"),
  String("[[:alnum:]_]AipsrcValue<Int>[[:alnum:]_]"),		// 10
  String("[[:alnum:]_]AipsrcVector<Bool>[[:alnum:]_]"),
  String("[[:alnum:]_]AipsrcVector<Double>[[:alnum:]_]"),
  String("[[:alnum:]_]AipsrcVector<Int>[[:alnum:]_]"),
  String("[[:alnum:]_]AipsrcVector<String>[[:alnum:]_]"),
  String("[[:alnum:]_]LogicalArrayElem[[:alnum:]_]"),
  String("[[:alnum:]_]Array<Bool>[[:alnum:]_]"),
  String("[[:alnum:]_]MaskedArray<Bool>[[:alnum:]_]"),
  String("[[:alnum:]_]Cube<Bool>[[:alnum:]_]"),
  String("[[:alnum:]_]Matrix<Bool>[[:alnum:]_]"),
  String("[[:alnum:]_]Vector<Bool>[[:alnum:]_]"),		// 20
  String("[[:alnum:]_]MeasurementSet[[:alnum:]_]"),
  String("[[:alnum:]_]Quantum<Double>[[:alnum:]_]")
};
const Regex Template::PATtypedef1[Ntypedef] = {
  String("FitsLong"),						// 00
  String("lDouble"),
  String("Long"),
  String("DataStatus"),
  String("LogicalRecord"),
  String("TapeHeader"),
  String("Convolver<Double>"),
  String("Convolver<Float>"),
  String("AipsrcValue<Bool>"),
  String("AipsrcValue<Double>"),
  String("AipsrcValue<Int>"),					// 10
  String("AipsrcVector<Bool>"),
  String("AipsrcVector<Double>"),
  String("AipsrcVector<Int>"),
  String("AipsrcVector<String>"),
  String("LogicalArrayElem"),
  String("Array<Bool>"),
  String("MaskedArray<Bool>"),
  String("Cube<Bool>"),
  String("Matrix<Bool>"),
  String("Vector<Bool>"),					// 20
  String("MeasurementSet"),
  String("Quantum<Double>")
};
String Template::REPtypedef[Ntypedef] = {
  "Long",							// 00
  "lDouble",
  "Long",
  "DataStatusStructure",
  "LogicalRecordStructure",
  "TapeHeader",
  "DoubleConvolver",
  "FloatConvolver",
  "AipsrcBool",
  "AipsrcDouble",
  "AipsrcInt",							// 10
  "AipsrcVBool",
  "AipsrcVDouble",
  "AipsrcVInt",
  "AipsrcVString",
  "Bool",
  "LogicalArray",
  "MaskedArrayLogical",
  "LogicalCube",
  "LogicalMatrix",
  "LogicalVector",						//20
  "MS",
  "Quantity"
};

  // Name of repository files
const String Template::reposName = "/_ReposFiller/templates";

//# Constructors
Template::Template() :
  output_p(100), count_p(0), tcount_p(0),
  comout_p(100), comptr_p(100), ccount_p(0),
  isSplit_p(False), dcount_p(0),
  nstring_p(0), allstring_p(0), namstring_p(0), nval_p(0),
  tdflist_p(0),
  tdcount_p(0), tdlist_p(100), tdfile_p(100), tdline_p(100), tdname_p(0) {
  reset();
}

Template::Template(const Vector<String> &files) :
  output_p(100), count_p(0), tcount_p(0),
  comout_p(100), comptr_p(100), ccount_p(0),
  isSplit_p(False), dcount_p(0),
  nstring_p(0), allstring_p(0), namstring_p(0), nval_p(0),
  tdflist_p(0),
  tdcount_p(0), tdlist_p(100), tdfile_p(100), tdline_p(100), tdname_p(0) {
  reset();
  read(files);
}

Template::Template(const String &filename) :
  output_p(100), count_p(0), tcount_p(0),
  comout_p(100), comptr_p(100), ccount_p(0),
  isSplit_p(False), dcount_p(0),
  nstring_p(0), allstring_p(0), namstring_p(0), nval_p(0),
  tdflist_p(0),
  tdcount_p(0), tdlist_p(100), tdfile_p(100), tdline_p(100), tdname_p(0) {
  reset();
  read(filename);
}

//# Destructor
Template::~Template() {}

//# Member functions
void Template::reset() {
  count_p = 0;
  tcount_p = 0;
  ccount_p = 0;
  isSplit_p = False;
  dcount_p = 0;
  tdcount_p = 0;
  tdflist_p.resize(0);
  // Make sure all (known) variable typedefs are catered for
  if (typeid(FitsLong) == typeid(Int)) REPtypedef[0] = "Int";
  if (typeid(lDouble) == typeid(Double)) REPtypedef[1] = "Double";
  if (typeid(Long) == typeid(Int)) REPtypedef[2] = "Int";
}

void Template::read(const Vector<String> &files) {
  for (uInt i=0; i < files.nelements(); i++) { // for each file...
    read(files(i));
  }
}

void Template::read(const String &filename) {
  // Open and read file
  ifstream file(filename.chars(), ios::in);
  if (!file) {
    cerr << "Cannot open input file " << filename << endl;
    return;
  }
  // Save filename in list
  tdflist_p.resize(tdflist_p.nelements()+1);
  tdflist_p[tdflist_p.nelements()-1] = filename;
  String extracted;			// a single input line
  String combine;			// a full combined line
  uInt c1 = 0;				// the input line count
  Bool ok(True);
  while (ok && (((extracted = ""), (ok = getline(file, extracted))) ||
		!combine.empty())) {
    c1++;				// Count input lines
    Bool err = False;
    // Skip empty lines
    if ((extracted.empty() || extracted.contains(spaces)) && ok) continue;
    // Check if correct first line
    if (combine.empty() && ok) {
      // Comment allowed
      if (extracted.contains(comment)) setComment(extracted, combine.empty());
      // Start of entry allowed
      else if (extracted.contains(fileRE)) combine = extracted;
      else err = True;
      if (!err) continue;
    }
    // Handle regular extension lines
    if ((extracted.contains(ifRE) ||
	 extracted.contains(endifRE) ||
	 extracted.contains(elseRE) ||
	 extracted.contains(templateRE) ||
	 extracted.contains(contRE) ||
	 extracted.contains(typedefRE) ||
	 extracted.contains(auxtemplRE) ||
	 extracted.contains(namespaceRE)) && ok && !err) {
      // Replace a continuation include line with /=/ pattern  
      if (extracted.contains(contRE)) extracted.gsub(contRE, String("/=/"));
      combine += String(" ") + extracted; // make one line
      // Find proper templates for list
      if (extracted.contains(templateRE)) {
	if (extracted.contains(forwardprelude)) continue; // skip forward declarations
	if (extracted.contains(mylistprelude)) {	  // special nnnn: list format
	  extracted = extracted.after(mylistprelude);
	} else if (extracted.contains(classprelude)) {
	  extracted = extracted.after(classprelude); 	  // template class
	} else if (extracted.contains(functionprelude)) {
	  extracted = extracted.after(functionprelude);   // template global
	  if (extracted.contains(funcnameprelude)) {
	    extracted = extracted.from(funcnameprelude);
	  }
	} else continue;		// unknown entry
	if (!extracted.empty()) {	// save the entry
	  if (tdcount_p >= tdlist_p.nelements()) {
	    tdlist_p.resize(tdcount_p+100);
	    tdfile_p.resize(tdcount_p+100);
	    tdline_p.resize(tdcount_p+100);
	  }
	  tdlist_p[tdcount_p]   = extracted;
	  tdfile_p[tdcount_p]   = tdflist_p.nelements()-1;
	  tdline_p[tdcount_p++] = c1;
	}
      }
      continue;
    }
    // Handle comment lines
    if (ok && !err && extracted.contains(comment)) {
      setComment(extracted, combine.empty());
      continue;
    }
    // Handle an initial line
    if ((ok && !err && extracted.contains(fileRE)) || !ok) {
      if (!combine.empty()) setOutput(combine);
      combine = extracted;
    } else err = True;
    if (err) {
      cerr << "Warning: illegal entry commented out near line " <<
	c1 << " in " << filename << ":\n\t" << 
	extracted(0,
		  ((extracted.length() <= 60) ? extracted.length() : 60)) <<
	" ..." << endl;
      for (uInt j=0; j<extracted.length(); j+=60) {
	ostringstream text;
	text << "#." << j/60 << ".\t" <<
	  extracted(j,
		    ((extracted.length()-j <= 60) ? extracted.length() : 60));
	setComment(String(text), combine.empty());
      }
    }
    if (!ok) break;
  }
}

void Template::canonical(const Bool tmplonly) {
  // Reformat all entries using the replacement patterns
  String combine;
  String lpat;			// A run-time pattern
  String lrep;			// A run-time replacement
  if (!tmplonly) {
    for (uInt i=0; i<count_p; i++) {
      combine = output_p[i];
      for (uInt j=0; j<Ncanon; j++) combine.gsub(PATcanon[j], REPcanon[j]);
      for (uInt j=0; j<Ncanon2; j++) {
	while (combine.contains(PATcanon20[j])) {
	  lpat = combine.at(PATcanon20[j]);
	  lpat = lpat.through(PATcanon20[j]);
	  lrep = lpat;
	  lrep.gsub(PATcanon21[j], REPcanon2[j]);
	  combine.gsub(lpat, lrep);
	}
      }
      for (uInt j=0; j<Nnmin; j++) {
	if (combine.contains(PATnmin[j])) combine = REPnmin[j] + combine;
      }
      for (uInt j=0; j<Nnmax; j++) {
	if (combine.contains(PATnmax[j])) combine = combine.from(REPnmax[j]);
      }
      output_p[i] = combine;
    }
  }
  // Do all template entries
  tdname_p.resize(tdcount_p);
  for (uInt i=0; i<tdcount_p; i++) {
    combine = tdlist_p[i];
    for (uInt j=0; j<Ncanon; j++) combine.gsub(PATcanon[j], REPcanon[j]);
    for (uInt j=0; j<Ncanon2; j++) {
      while (combine.contains(PATcanon20[j])) {
	lpat = combine.at(PATcanon20[j]);
	lpat = lpat.through(PATcanon20[j]);
	lrep = lpat;
	lrep.gsub(PATcanon21[j], REPcanon2[j]);
	combine.gsub(lpat, lrep);
      }
    }
    // Remove leading/trailing spaces and singlefy spaces
    combine.gsub(leadsp, nullsp); 
    combine.gsub(endsp, nullsp);
    combine.gsub(mulsp, singlesp);
    tdlist_p[i] = combine;

    // Cater for typedef usage
    for (uInt j=0; j<Ntypedef; j++) {
      // Make sure no infinite loops
      if (REPtypedef[j] != PATtypedef1[j].regexp()) {
	while (combine.contains(PATtypedef0[j])) {
	  lpat = combine.at(PATtypedef0[j]);
	  lpat = lpat.through(PATtypedef0[j]);
	  lrep = lpat;
	  lrep.gsub(PATtypedef1[j], REPtypedef[j]);
	  combine.gsub(lpat, lrep);
	}
      }
    }
    // Remove all spaces, and just count 'const' since they can be at
    // different places.
    combine.gsub(mulsp, nullsp);
    combine += Char('0' + combine.gsub(constsp, nullsp));
    tdname_p[i] = combine;
  }
}

void Template::splitName() {
  // Only split if necessary
  if (isSplit_p) return;
  // Make correct sizes available
  nstring_p.resize(count_p);
  allstring_p.resize(count_p);
  namstring_p.resize(count_p);
  nval_p.resize(count_p);
  // Split
  for (uInt i=0; i<count_p; i++) {
    nstring_p[i] = output_p[i].through(splitnum);
    allstring_p[i] = output_p[i].after(splitnum);
    namstring_p[i] = allstring_p[i].through(splitnam);
    nval_p[i] = atoi(nstring_p[i].chars());
  }
  isSplit_p = True;
}

void Template::sortName(const Bool renumber) {
  // Split first if necessary
  splitName();
  // Indexes and specify sort
  Vector<uInt> inx;
  Sort sort;
  sort.sortKey(allstring_p.storage(), TpString);
  sort.sortKey(nstring_p.storage(), TpString);
  // Sort and fill missing numbers
  sort.sort(inx, count_p);
  // Make numbers
  if (renumber) {
    String prev;
    uInt ident(0);
    for (uInt j=0; j<count_p; j++) {
      if (namstring_p[inx(j)] == prev) {
	ident += 10;
      } else {
	ident = 1000;
	prev = namstring_p[inx(j)];
      }
      ostringstream text;
      text << ident;
      nstring_p[inx(j)] = String(text) + " ";
    }
  } else {
    String prev;
    Int pid(0);
    uInt mid(0);
    for (uInt k=0; k<count_p; k++) {
      if (prev.empty()) {
	prev = namstring_p[inx(k)];
	pid = k;
	mid = 990;
	uInt j = k;
	while (j<count_p && namstring_p[inx(j)] == prev) {
	  mid = (nval_p[inx(j)] > mid) ? nval_p[inx(j)] : mid;
	  j++;
	}
	mid = (mid/10)*10 + 10;
      }
      if (prev == namstring_p[inx(k)]) {
	if (nval_p[inx(k)] < 1000) {
	  ostringstream text;
	  text << mid;
	  nstring_p[inx(k)] = String(text) + " ";
	  nval_p[inx(k)] = mid;
	  mid += 10;
	} else {
	  for (Int j=k-1; j>=pid; j--) {
	    if (nval_p[inx(k)] == nval_p[inx(j)]) {
	      ostringstream text;
	      text << mid;
	      namstring_p[inx(k)] = String(text) + " ";
	      nval_p[inx(k)] = mid;
	      mid += 10;
	      break;
	    }
	  }
	}
      } else {
	prev = "";
	k--;
      }
    }
  }
  // Make new full line
  for (uInt j=0; j<count_p; j++) {
    output_p[j] = nstring_p[inx(j)] + allstring_p[inx(j)];
  }
  // Re-sort comments
  for (uInt j=0; j<ccount_p; j++) {
    if (comptr_p[j] >= 0 && comptr_p[j] < Int(count_p)) {
      for (uInt j3=0; j3<count_p; j3++) {
      	if (comptr_p[j] == Int(inx(j3))) {
      	  comptr_p[j] = j3;
      	  break;
      	}
      }
    }
  }
}

void Template::writeOut(ostream &os, const Bool warn) {
  // Constants
  static const String sp = " ";
  const Int Nsplit = 2000;	// # of fields in one entry must fit in here

  // Local data
  String spf[Nsplit];		// Fields in full line
  Int c1 = 0;			// Output line count
  Bool cwarn = False;           // Do not give a compressed warning
  // Write initial comments
  for (uInt j=0; j<ccount_p; j++) {	// initial comments
    if (comptr_p[j] < 0) {
      os << comout_p[j] << endl;
      c1++;
    }
  }
  for (uInt i=0; i<count_p; i++) {
    // Split output at spaces
    uInt nsp = split(output_p[i], spf, Nsplit, sp);
    uInt k = 0;
    uInt p = 0;
    Int c = 0;			// Level of indentation for #if
    Bool pr = True;
    String w;			// Line indentation
    String v;			// Line start pattern
    for (uInt j=0; j<=nsp; j++) {
      // If not the first element of a line; or it is the last
      if (j == nsp ||
	  spf[j] == "template" || spf[j] == "#endif" ||
	  spf[j] == "#else" || spf[j] == "/=/" ||
	  spf[j].contains(sifRE) ||
	  spf[j].contains(stypedefRE) ||
	  spf[j].contains(sauxtemplRE) ||
	  spf[j].contains(snamespaceRE)) {
	if (k != 0) {
	  v = "";
	  for (uInt m=p; m < p+k; m++) {	// all fields found till now
	    if (pr && v.length()>40) {
	      os << w << v << endl;
	      v = "= ";		// Indicate follow-on include
	      w = "     ";	// Indent
	      for (Int i1=0; i1<c; i1++) w += "  ";
	    }
	    v += spf[m] + sp;
	  }
	  c1++;
	  // Format the fields after #if
	  if (v.contains(sifRE)) {
	    for (uInt j3=0; j3<Ninif; j3++) v.gsub(PATinif[j3], REPinif[j3]);
	  }
	  // Format fields in template and count them
	  if (v.contains(stemRE)) {
	    if (!v.contains(sretRE3)) tcount_p++;
	    if (v.contains(sconstRE)) {
	      cerr << "Error:   non-canonical position of const "
		"at line " << c1 << endl;
	    }
	    if (!(v.contains(sretRE1) || v.contains(sretRE3))) {
	      if (v.contains(sretRE2)) {
		cerr << "Error:   missing return type "
		  "at line " << c1 << endl;
	      }
	      if (v.contains(sretRE4)) {
		if (warn) {
		  cerr << "Warning: superfluous template argument given -- "
		    "remove at line " << c1 << endl;
		} else cwarn = True;
	      }
	    }
	  }
	  os << w << v << endl;
	  if (j == nsp) break;		// ready with element
	}
	k = 1; p = j; w = "     ";
	if (spf[j] == "#endif" || spf[j] == "#else") c--;
	for (Int i1=0; i1<c; i1++) w += "  ";
	if (spf[j] == "#else" && c < 0) cerr << "SEVERE: #else without #if "
					  "near line " << c1+1 << endl;
	if (spf[j].contains(sifRE) || spf[j] == "#else") c++;
	if (spf[j] == "/=/") {
	  spf[j] = "=";
	  pr = True;
	} else pr = False;
	continue;
      }
      k++;
    }
    if (c<0) cerr << "SEVERE: too many #endif "
	       "near line " << c1 << endl;
    while (c>0) {
      c--; 
      w = "     ";
      for (Int i1=0; i1<c; i1++) w += "  ";
      c1++;
      cerr << "Warning: included missing #endif "
	"at line " << c1 << endl;
      os << w << "#endif" << endl;
    }
    for (uInt j1=0; j1<ccount_p; j1++) {	// comments
      if (comptr_p[j1] == Int(i)) {
	c1++;
	os << comout_p[j1] << endl;
      }
    }
  }
  for (uInt j2=0; j2<ccount_p; j2++) {		// comments
    if (comptr_p[j2] >= Int(count_p)) {
      c1++;
      os << comout_p[j2] << endl;
    }
  }
  if (cwarn) {
    cerr << "Warning: One or more possibly superfluous template arguments "
      "given.\n         Run reident with the -v (verbose) switch to learn more" << endl;
  }
}

void Template::writeDup(ostream &os, const String &userFile, Bool isSys) {
  // Sort the name list
  Vector<uInt> inx;
  Sort sort;
  sort.sortKey(tdname_p.storage(), TpString);
  sort.sort(inx, tdcount_p);
  uInt i(0);			// Count the entries
  // Scan all entries for groups
  dcount_p = 0;
  while (i<tdcount_p) {
    uInt n=0;			// Length of group
    for (uInt j=i; j<tdcount_p; j++) {
      if (tdname_p[inx(j)] == tdname_p[inx(i)]) n++;
      else break;
    }
    // Found duplicates
    if (n>1) {
      // Check if -s switch given
      Bool doit = True;
      if (isSys) {
	doit = False;
	// Check if _ReposFiller mentioned
	for (uInt j=i; j<i+n; j++) {
	  if (tdflist_p[tdfile_p[inx(j)]].contains(reposName)) doit = True;
	  // Check for same file duplicates
	  for (uInt k=j+1; k<i+n; k++) {
	    if (tdflist_p[tdfile_p[inx(j)]] ==
		tdflist_p[tdfile_p[inx(k)]]) doit = True;
	  }
	  if (doit) break;
	}
      }
      if (doit) {
	os << "---------------------------------------------" << endl;
	for (uInt j=i; j<i+n; j++) {
	  os << tdlist_p[inx(j)] << "   " <<
	    tdflist_p[tdfile_p[inx(j)]] << " line " << tdline_p[inx(j)];
	  if (tdflist_p[tdfile_p[inx(j)]] == userFile) os << " ***" << endl;
	  else os << " -" << endl;;
	}
	dcount_p += n;
      }
    }
    i += n;			// next group
  }
}

void Template::setComment(const String &txt, const Bool atstart) {
  // Resize
  if (ccount_p >= comout_p.nelements()) {
    comout_p.resize(ccount_p+100);
    comptr_p.resize(ccount_p+100);
  }
  comout_p[ccount_p] = txt;
  comptr_p[ccount_p] = count_p;
  if (atstart && count_p == 0) comptr_p[ccount_p] = -1;
  ccount_p++;
}

void Template::setOutput(const String &txt) {
  if (count_p >= output_p.nelements()) output_p.resize(count_p+100);
  output_p[count_p++] = txt;
}

} //# NAMESPACE CASACORE - END


#endif
