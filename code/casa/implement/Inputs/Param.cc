//# Param.cc: Helper class for key=value user interface
//# Copyright (C) 1993,1994,1995
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

// Param.C: implementation of a parameter (key,value,help,type,range,....)
//          users typically do not use this class, it is merely
//          a helper class for Input.

#include <aips/Inputs/Param.h>

Param::Param()			// default constructor; doesn't do anything
{
}

				// standard constructors
Param::Param(String a_key, String a_value, String a_help,
             String a_type, String a_range, String a_unit)
{
    key   = a_key;
    value = a_value;
    help  = a_help;
    type  = a_type;
    range = a_range;
    unit  = a_unit;

    hasvalue = (value.length() > 0) ? True : False;
    system = False;
    index = 0;

#if defined(DEBUG)
    cout << "Creating parameter " << key      << "\n"
         << "         value:    " << value    << "\n"
         << "         help:     " << help     << "\n"
         << "         hasvalue: " << hasvalue << "\n"
         << "         system:   " << system   << "\n"
         << "         index:    " << index    << "\n";
#endif
}


				// destructor
Param::~Param()
{
#if defined(DEBUG)
    cout << "Destructing parameter " << key << "; but not really\n";
#endif
}


Param::Param(const Param& other)		// copy
{

    key   = other.key;
    value = other.value;
    help  = other.help;
    type  = other.type;
    range = other.range;
    unit  = other.unit;

    hasvalue = other.hasvalue;
    system   = other.system;
    index    = other.index;
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
Param& 
Param::operator= (const Param& other)          // assignment
{
    // Make sure we don't assign ourselves
    if (this == &other ) return *this;

    // No deletion of old ones yet..
    key   = other.key;
    value = other.value;
    help  = other.help;
    type  = other.type;
    range = other.range;
    unit  = other.unit;

    hasvalue = other.hasvalue;    
    system   = other.system;
    index    = other.index;

    return *this;
}

Bool						// comparison, don't allow
Param::operator== (const Param&) const
{
    return False;
}

// query functions

double 
Param::GetDouble(Bool prompt)			// double value
{
#if defined(EVAL)
    double d;
    int n;
    n = eval_double((const char *)value, &d, 1, &iret);
    if (n==1) return d;
    else return 0.0;
#else
    if (prompt) cerr << "No prompting implemented yet" << endl;
    return atof((const char *)value);
#endif
}

Block<double>
Param::GetDoubleArray(Bool prompt)			// double value
{
    int i, idx=0, n = value.freq(",")+1;
    String z;
    Block<double> x(n);

    if (prompt) cerr << "No prompting implemented yet" << endl;
    for (i=0; i<n; i++) {
        if (i==0) {
            z=value;
            idx =  z.index(",");
        } else {
            z=value.after(idx);
            idx += z.index(",") + 1;
        }
        x[i] = atof(z);
    }
    return x;
}

int 
Param::GetInt(Bool prompt)			// int value
{
    if (prompt) cerr << "No prompting implemented yet" << endl;
    return atoi((const char *)value);
}

Block<Int>
Param::GetIntArray(Bool prompt)
{
    int i, idx=0, n = value.freq(",")+1;
    String z;
    Block<Int> x(n);

    if (prompt) cerr << "No prompting implemented yet" << endl;
    for (i=0; i<n; i++) {
        if (i==0) {
            z=value;
            idx =  z.index(",");
        } else {
            z=value.after(idx);
            idx += z.index(",") + 1;
        }
        x[i] = atoi(z);
    }
    return x;
}

String
Param::GetString(Bool prompt)			// string value
{
    if (prompt) cerr << "No prompting implemented yet" << endl;
    return value;
}

Block<String>
Param::GetStringArray(Bool prompt)
{
    int i, idx=0, n = value.freq(",")+1;
    String z;
    Block<String> x(n);

    if (prompt) cerr << "No prompting implemented yet" << endl;
    for (i=0; i<n; i++) {
        if (i==0) {
            z=value;
            idx =  z.index(",");
        } else {
            z=value.after(idx);
            idx += z.index(",") + 1;
        }
        x[i] = z;
    }
    return x;
}

Bool
Param::GetBool(Bool prompt)			// Bool value
{
    if (prompt) cerr << "No prompting implemented yet" << endl;
    if (value.contains(Regex("[TtYy1Jj]")))
        return True;
    else
        return False;
}

#if 0
Block<Bool>
Param::GetBoolArray(Bool prompt)
{
    int i, idx, n = value.freq(",")+1;
    String z;
    Block<Bool> x(n);

    if (prompt) cerr << "No prompting implemented yet" << endl;
    for (i=0; i<n; i++) {
        if (i==0) {
            z=value;
            idx =  z.index(",");
        } else {
            z=value.after(idx);
            idx += z.index(",") + 1;
        }
        if (z.contains(Regex("[TtYy1Jj]")))
            x[i] = True;
        else
            x[i] = False;
    }
    return x;
}
#endif



// modify and other misc function

Bool
Param::Put(const String other)			// set new value
{ 
// value checking will be done here too?
//        cout << "Param::Put> " << key << "=" << value << "\n";
    value = other;
//        cout << "Param::Put> " << key << "=" << value << "\n";
    return True;
}

void
Param::SetSystem(Bool val)                      // set system level
{
    system = val;
}

Bool
Param::IsSystem()                               // get system level
{
    return system;
}

void
Param::SetIndex(int val)
{
    index = val;
}

int
Param::GetIndex()
{
    return index;
}


ostream & operator<<(ostream &os, const Param& p) {
    os << p.key << "=" << p.value;
    return os;
}

istream & operator>>(istream &os, Param& p) {
    // needed because of Slist<Param>
    cerr << "==>Got to implement >> operator for Param now; " << p << endl;
    return os;
}

AipsIO & operator<<(AipsIO &os, const Param& p) {
    //
    cerr << "==>Got to implement << operator for Param now; " << p << endl;
    return os;
}

AipsIO & operator>>(AipsIO &os, Param& p) {
    // needed because of Slist<Param>
    cerr << "==>Got to implement >> operator for Param now; " << p << endl;
    return os;
}

