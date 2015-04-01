//# StringDistance.h: Class to deal with Levensthein distance of strings
//# Copyright (C) 2009
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

#ifndef CASA_STRINGDISTANCE_H
#define CASA_STRINGDISTANCE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Arrays/Matrix.h>

namespace casacore {

// <summary>
// Class to deal with Levensthein distance of strings.
// </summary>

// <synopsis>
// The Levenshtein Distance is a metric telling how similar strings are.
// It is also known as the Edit Distance.
//
// The distance tells how many operations (i.e., character substitutions,
// insertions, and deletions are needed to transform one string into another.
// <br>There are several extensions to the basic definition:
// <ul>
//  <li> Treat a swap of adjacent characters as one operation.
//  <li> Give a weight to operations (e.g., insertions and deletions have
//       half the weight of the other operations).
//  <li> Take locality into account. For example, successive substitutions
//       weigh more than non-successive.
//  <li> Extend to wildcarded strings.
// </ul> 
// This class optionally uses the swap extension. Furthermore one can
// optionally ignore blanks. By default both options are used.
//
// The code is based on code written by Anders Sewerin Johansen.
// Calculating the distance is an expensive O(N^2) operation, thus
// should be used with care.
//
// The class is constructed with the source string to compare against.
// Thereafter its <code>match</code> or <code>distance</code>
// function can be used for each target string.
// </synopsis>

class StringDistance
{
public:
  // Default constructor sets maxDistance to 0.
  StringDistance();

  // Construct from the source string and maximum distance.
  // If the maximum distance is negative, it defaults to 1+strlength/3.
  // Note that maximum distance 0 means that the strings must match exactly.
  explicit StringDistance (const String& source, Int maxDistance=-1,
                           Bool countSwaps=True, Bool ignoreBlanks=True,
                           Bool caseInsensitive=False);

  // Get data members.
  // <group>
  const string& source() const
    { return itsSource; }
  Int maxDistance() const
    { return itsMaxDistance; }
  const Matrix<Int>& matrix() const
    { return itsMatrix; }
  // </group>
  
  // Test if the given target string is within the maximum distance.
  Bool match (const String& target) const;

  // Calculate the distance from the string to the string given in the constructor.
  // If the length of target exceeds source length + maxDistance,
  // the difference in lengths is returned.
  Int distance (const String& target) const;

  // Calculate the distance between the two strings.
  // This is slower than the <src>distance</src> member function, because
  // it has to allocate the underlying Matrix for each invocation.
  static Int distance (const String& source, const String& target,
                       Bool countSwaps=True);

  // Remove blanks from the given string.
  static String removeBlanks (const String& source);

private:
  // Calculate the distance.
  static Int doDistance (const String& source, const String& target,
                         Bool countSwaps, Matrix<Int>& matrix);


private:
  String              itsSource;
  mutable Matrix<Int> itsMatrix;
  Int                 itsMaxDistance;
  Bool                itsCountSwaps;
  Bool                itsIgnoreBlanks;
  Bool                itsCaseInsensitive;
};

} //# end namespace

#endif
