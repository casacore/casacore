//# Param.cc: Helper class for key=value user interface
//# Copyright (C) 1993,1994,1995,1999,2001,2002
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

// Param.C: implementation of a parameter (key,value,help,type,range,....)
//          users typically do not use this class, it is merely
//          a helper class for Input.

#include <casacore/casa/Inputs/Param.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/iostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

Param::Param()			// default constructor; doesn't do anything
{}

				// standard constructors
Param::Param (const String& a_key, const String& a_value, const String& a_help,
             const String& a_type, const String& a_range, const String& a_unit)
: key      (a_key),
  value    (a_value),
  help     (a_help),
  type     (a_type),
  range    (a_range),
  unit     (a_unit),
  hasvalue ((value.length() > 0) ? true : false),
  system   (false),
  index    (0)
{
#if defined(DEBUG)
    cout << "Creating parameter " << key      << "\n"
         << "         value:    " << value    << "\n"
         << "         help:     " << help     << "\n"
         << "         hasvalue: " << hasvalue << "\n"
         << "         system:   " << system   << "\n"
         << "         index:    " << index    << "\n";
#endif
}


Param::~Param()
{
#if defined(DEBUG)
    cout << "Destructing parameter " << key << "; but not really\n";
#endif
}


Param::Param (const Param& other)		// copy
: key      (other.key),
  value    (other.value),
  help     (other.help),
  type     (other.type),
  range    (other.range),
  unit     (other.unit),
  hasvalue (other.hasvalue),
  system   (other.system),
  index    (other.index)
{
#if defined(DEBUG)
    cout << "Copying  parameter " << key      << "\n"
         << "         value:    " << value    << "\n"
         << "         help:     " << help     << "\n"
         << "         hasvalue: " << hasvalue << "\n"
         << "         system:   " << system   << "\n"
         << "         index:    " << index    << "\n";
#endif
}

				// operator functions
Param& Param::operator= (const Param& other)          // assignment
{
    // Make sure we don't assign ourselves
    if (this != &other ) {
      key   = other.key;
      value = other.value;
      help  = other.help;
      type  = other.type;
      range = other.range;
      unit  = other.unit;
      hasvalue = other.hasvalue;    
      system   = other.system;
      index    = other.index;
    }
    return *this;
}

bool						// comparison, don't allow
Param::operator== (const Param&) const
{
    return false;
}


double 
Param::getDouble (bool prompt) const		// double value
{
#if defined(EVAL)
    double d;
    int32_t n = eval_double((const char *)value, &d, 1, &iret);
    if (n==1) {
      return d;
    } else {
      return 0.0;
    }
#else
    if (prompt) {
      cerr << "No prompting implemented yet" << endl;
    }
    return atof(value.chars());
#endif
}

Block<double>
Param::getDoubleArray (bool prompt) const	// double value
{
    int32_t i;
    int32_t idx=0;
    int32_t n = value.freq(",")+1;
    String z;
    String val(value);            // need a non-const String
    Block<double> x(n);

    if (prompt) {
      cerr << "No prompting implemented yet" << endl;
    }
    for (i=0; i<n; i++) {
        if (i==0) {
            z = val;
            idx = z.index(",");
        } else {
            z = val.after(idx);
            idx += z.index(",") + 1;
        }
        x[i] = atof(z.chars());
    }
    return x;
}

int32_t 
Param::getInt (bool prompt) const		// int32_t value
{
    if (prompt) {
      cerr << "No prompting implemented yet" << endl;
    }
    return atoi(value.chars());
}

Block<int32_t>
Param::getIntArray (bool prompt) const
{
    int32_t i;
    int32_t idx=0;
    int32_t n = value.freq(",")+1;
    String z;
    String val(value);            // need a non-const String
    Block<int32_t> x(n);

    if (prompt) {
      cerr << "No prompting implemented yet" << endl;
    }
    for (i=0; i<n; i++) {
        if (i==0) {
            z = val;
            idx = z.index(",");
        } else {
            z = val.after(idx);
            idx += z.index(",") + 1;
        }
        x[i] = atoi(z.chars());
    }
    return x;
}

const String&
Param::getString (bool prompt) const		// string value
{
    if (prompt) {
      cerr << "No prompting implemented yet" << endl;
    }
    return value;
}

Block<String>
Param::getStringArray (bool prompt) const
{
    int32_t i;
    int32_t idx=0;
    int32_t n = value.freq(",")+1;
    String z;
    String val(value);            // need a non-const String
    Block<String> x(n);

    if (prompt) {
      cerr << "No prompting implemented yet" << endl;
    }
    for (i=0; i<n; i++) {
        if (i==0) {
            z = val;
            idx = z.index(",");
        } else {
            z = val.after(idx);
            idx += z.index(",") + 1;
        }
        x[i] = z;
    }
    return x;
}

bool
Param::getBool(bool prompt) const		// bool value
{
    if (prompt) {
      cerr << "No prompting implemented yet" << endl;
    }
    return  (value.contains(Regex("[TtYy1Jj]")));
}

#if 0
Block<bool>
Param::getBoolArray(bool prompt) const
{
    int32_t i;
    int32_t idx;
    int32_t n = value.freq(",")+1;
    String z;
    Block<bool> x(n);

    if (prompt) {
      cerr << "No prompting implemented yet" << endl;
    }
    for (i=0; i<n; i++) {
        if (i==0) {
            z=value;
            idx =  z.index(",");
        } else {
            z=value.after(idx);
            idx += z.index(",") + 1;
        }
        x[i] =  (z.contains(Regex("[TtYy1Jj]")));
    }
    return x;
}
#endif


// modify and other misc function

bool
Param::put (const String& other)			// set new value
{ 
// value checking will be done here too?
//        cout << "Param::Put> " << key << "=" << value << "\n";
    value = other;
//        cout << "Param::Put> " << key << "=" << value << "\n";
    return true;
}


ostream & operator<< (ostream &os, const Param& p) {
    os << p.key << "=" << p.value;
    return os;
}

istream & operator>> (istream &os, Param& p) {
    // needed because of Slist<Param>
    cerr << "==>Got to implement >> operator for Param now; " << p << endl;
    return os;
}

AipsIO & operator<< (AipsIO &os, const Param& p) {
    //
    cerr << "==>Got to implement << operator for Param now; " << p << endl;
    return os;
}

AipsIO & operator>> (AipsIO &os, Param& p) {
    // needed because of Slist<Param>
    cerr << "==>Got to implement >> operator for Param now; " << p << endl;
    return os;
}


} //# NAMESPACE CASACORE - END

