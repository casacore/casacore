//# Template.h: Canonicalise, format etc. Casacore template definitions
//# Copyright (C) 2001,2002,2004,2005
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#ifndef CASA_TEMPLATE_H
#define CASA_TEMPLATE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations
class Regex;
template <class T> class Vector;

// <summary>
// Canonicalise, format and other actions on Casacore template definitions
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tSpectralFit" demos="">
// </reviewed>

// <prerequisite>
//  <li> Knowledge about the Casacore DYO template system
// </prerequisite>

// <synopsis>
// A set of methods on template repository files and on template definitions to be
// used in the reident, used, unused and duplicates programs (see 
// <a href="../../../reference/System?System.html">Sytem manual</a> 
// for details. <br /br>
// Methods exist to read templates, to canonicalise them for comparison and
// search functions and to format them for output.
// </synopsis>
//
// <motivation>
// To make template formatting identical across formatting/testing programs.
// </motivation>
//
// <todo asof="2001/03/20">
//   <li> nothing I know
// </todo>

class Template {
 public:

  //# Constructors
  // Default constructor. Need to read data into it
  Template();
  // Create from the file names given
  explicit Template(const Vector<String> &files);
  // Create from the file name given
  explicit Template(const String &filename);

  // Destructor
  ~Template();

  // Operators
  const String &operator[](uInt n) { return output_p[n]; }

  //# Member functions
  // Clear the object for a re-use.
  void reset();
  // Read the templates file or files into the class. Multiple reading is additive.
  // Errors are reported to cerr, and commented out in the file.
  // <group>
  void read(const Vector<String> &files);
  void read(const String &filename);
  // </group>
  // Get the number of template entries
  uInt getCount() const { return count_p; };
  // Get the number of template definition lines found
  uInt getTDCount() const { return tdcount_p; };
  // Get the number of templates found after all processing
  uInt getTCount() const { return tcount_p; };
  // Get the number of duplicates found
  uInt getDCount() { return dcount_p; };
  // Get the various template definition information fields.
  // Meant for testing and special projects only.
  // <group>
  const String &getTDFlist(uInt n) { return tdflist_p[n]; };
  const String &getTDlist(uInt n) { return tdlist_p[n]; };
  const uInt &getTDfile(uInt n) { return tdfile_p[n]; };
  const uInt &getTDline(uInt n) { return tdline_p[n]; };
  const String &getTDname(uInt n) { return tdname_p[n]; };
  // </group>

  // Canonicalise the template entries in the object. If switch True, do only
  // the templates entry for duplication
  void canonical(const Bool tmplonly=False);
  // Split the entries in number, name id, rest
  void splitName();
  // Sort the data on name and number and fill in missing number. If switch
  // is True, renumber all template entries in sequence.
  void sortName(const Bool renumber=False);
  // Write the data formatted to the specified file. Notify errors and warnings
  // by writing to <src>cerr</src>. If <src>warn</src> is False, some warnings will be
  // compressed into a general warning.
  void writeOut(ostream &os, const Bool warn=False);
  // Write the duplicate list; the userFile gets ***; isSys gives the system switch
  void writeDup(ostream &os, const String &userFile, Bool isSys=False);

 private:
  //# Data
  // Each element is a template entry on a single line
  Block<String> output_p;
  // Count the lines
  uInt count_p;
  // Count the templates
  uInt tcount_p;
  // Record comment lines
  Block<String> comout_p;
  // And where they originated
  Block<Int> comptr_p;
  // And count the comment lines
  uInt ccount_p;
  // Indicate data split
  Bool isSplit_p;
  // Count the duplicates
  uInt dcount_p;
  // Data split of number string (or empty/spaces)
  Block<String> nstring_p;
  // Data split all text
  Block<String> allstring_p;
  // Data split name string (first include file)
  Block<String> namstring_p;
  // Data split numeric number
  Block<uInt> nval_p;

  // List of files used
  Block<String> tdflist_p;
  // Number of template definitions extracted from input
  uInt tdcount_p;
  // List of template definitions
  Block<String> tdlist_p;
  // Pointers to in which file in list
  Block<uInt> tdfile_p;
  // Line number in file at which template found
  Block<uInt> tdline_p;
  // List of comparison names
  Block<String> tdname_p;

  //# Constructors
  // Copy constructor (not implemented)
  Template(const Template &other);
  //# Operators
  // Assignment (not implemented)
  Template &operator=(const Template &other);
  //# Member functions
  // Save comment
  void setComment(const String &txt, const Bool atstart=False);
  // Save a line
  void setOutput(const String &txt);

  //# Static conversion data
  // Patterns to analyse an input line
  static const Regex spaces;
  static const Regex comment;
  static const Regex ifRE;
  static const Regex endifRE;
  static const Regex elseRE;
  static const Regex templateRE;
  static const Regex contRE;
  static const Regex fileRE;
  static const Regex typedefRE;
  static const Regex auxtemplRE;
  static const Regex namespaceRE;

  // Simple pattern and replacements to make canonical templates files
  static const uInt Ncanon = 52;
  static const Regex PATcanon[Ncanon];
  static const String REPcanon[Ncanon];

  // For canonical change: replacement of pattern with pattern
  static const uInt Ncanon2 = 15;
  static const Regex PATcanon20[Ncanon2];
  static const Regex PATcanon21[Ncanon2];
  static const String REPcanon2[Ncanon2];

  // Make canonical numbers of 4 digits minimum
  static const uInt Nnmin = 4;
  static const Regex PATnmin[Nnmin];
  static const String REPnmin[Nnmin];
  // Make canonical numbers of 4 digits maximum
  static const uInt Nnmax = 1;
  static const Regex PATnmax[Nnmax];
  static const Regex REPnmax[Nnmax];
 
  // Patterns to split off number and name
  // Patterns to split off number and name
  static const Regex splitnum;
  static const Regex splitnam;

  // Patterns to check the template line
  static const Regex sifRE;
  static const Regex stemRE;
  static const Regex sconstRE;
  static const Regex sretRE1;
  static const Regex sretRE2;
  static const Regex sretRE3;
  static const Regex sretRE4;
  static const Regex stypedefRE;
  static const Regex sauxtemplRE;
  static const Regex snamespaceRE;

  // Replacement patterns for ifs in saved line
  static const uInt Ninif = 5;
  static const String PATinif[Ninif];
  static const String REPinif[Ninif];

  // Tests for finding real templates for duplicate tests
  static const Regex classprelude;
  static const Regex functionprelude;
  static const Regex forwardprelude;
  static const Regex funcnameprelude;
  static const Regex mylistprelude;

  // Data to remove spaces at begin, end, make single, count/remove const
  static const Regex leadsp;
  static const Regex endsp;
  static const Regex mulsp;
  static const Regex constsp;
  static const String nullsp;
  static const String singlesp;

  // Patterns to make all typedefs comparisons for duplicates possible
  // Note that the first three should be in that position for run-time
  // change on some systems.
  static const uInt Ntypedef = 23;
  static const Regex PATtypedef0[Ntypedef];
  static const Regex PATtypedef1[Ntypedef];
  static String REPtypedef[Ntypedef];

  // Name of repository files
  static const String reposName;

};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Utilities/Template.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
