//# StringDistance.cc: Class to deal with Levensthein distance of strings
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

#include <casacore/casa/Utilities/StringDistance.h>

using namespace std;

namespace casacore {

StringDistance::StringDistance()
  : itsMaxDistance     (0),
    itsCountSwaps      (False),
    itsIgnoreBlanks    (False),
    itsCaseInsensitive (False)
{}

StringDistance::StringDistance (const String& source, int maxDistance,
                                Bool countSwaps, Bool ignoreBlanks,
                                Bool caseInsensitive)
  : itsSource          (source),
    itsMaxDistance     (maxDistance),
    itsCountSwaps      (countSwaps),
    itsIgnoreBlanks    (ignoreBlanks),
    itsCaseInsensitive (caseInsensitive)
{
  if (ignoreBlanks) {
    itsSource = removeBlanks (itsSource);
  }
  if (caseInsensitive) {
    itsSource.downcase();
  }
  if (itsMaxDistance < 0) {
    itsMaxDistance = 1 + itsSource.size() / 3;
  }
  // Size the matrix such that it suffices for all possible strings.
  itsMatrix.resize (itsSource.size() + 1, itsSource.size() + itsMaxDistance + 1);
  itsMatrix = -1;
}

Bool StringDistance::match (const String& target) const
{
  String t(target);
  if (itsIgnoreBlanks) {
    t = removeBlanks (target);
  }
  int diff = t.size();
  diff -= itsSource.size();
  if (std::abs(diff) > itsMaxDistance) {
    return false;
  }
  if (itsCaseInsensitive) {
    t.downcase();
  }
  if (itsMaxDistance == 0) {
    return t == itsSource;
  }
  return doDistance(itsSource, t, itsCountSwaps, itsMatrix) <= itsMaxDistance;
}

Int StringDistance::distance (const String& target) const
{
  String t(target);
  if (itsIgnoreBlanks) {
    t = removeBlanks (target);
  }
  if (t.size() > itsSource.size() + itsMaxDistance) {
    return t.size() - itsSource.size();
  }
  if (itsCaseInsensitive) {
    t.downcase();
  }
  return doDistance (itsSource, t, itsCountSwaps, itsMatrix);
}

Int StringDistance::distance (const String& source, 
                              const String& target,
                              Bool countSwaps)
{
  Matrix<Int> matrix (source.size() + 1, target.size() + 1);
  return doDistance (source, target, countSwaps, matrix);
}

Int StringDistance::doDistance (const String& source,
                                const String& target,
                                Bool countSwaps,
                                Matrix<Int>& matrix)
{
  int n = source.size();
  int m = target.size();
  if (n == 0) {
    return m;
  }
  if (m == 0) {
    return n;
  }
  // Initialize the first row and column.
  for (int i=0; i<=n; ++i) {
    matrix(i,0) = i;
  }
  for (int j=0; j<=m; ++j) {
    matrix(0,j) = j;
  }
  // Loop over all characters in source and target.
  for (int j=0; j<m; ++j) {
    char t_j = target[j];
    for (int i=0; i<n; ++i) {
      char s_i = source[i];
      // If equal, no cost involved.
      int cost = (s_i == t_j  ?  0 : 1);
      // Step 6
      int above = matrix(i,j+1);
      int left  = matrix(i+1,j);
      int diag  = matrix(i,j);
      int cell  = std::min(above + 1, std::min(left + 1, diag + cost));
      // Step 6A: Cover transposition, in addition to deletion,
      // insertion and substitution. This step is taken from:
      // Berghel, Hal ; Roach, David : "An Extension of Ukkonen's 
      // Enhanced Dynamic Programming ASM Algorithm"
      // (http://www.acm.org/~hlb/publications/asm/asm.html)
      if (countSwaps && i>0 && j>0) {
        int trans = matrix(i-1,j-1) + 1;
        if (source[i-1] != t_j) trans++;
        if (s_i != target[j-1]) trans++;
        if (cell > trans) cell=trans;
      }
      matrix(i+1,j+1) = cell;
    }
  }
  // Now the last element contains the distance.
  return matrix(n,m);
}

String StringDistance::removeBlanks (const String& source)
{
  String dest;
  int n = source.size();
  dest.reserve (n);
  for (int i=0; i<n; ++i) {
    if (source[i] != ' ') {
      int sz = dest.size();
      dest.resize (sz+1);
      dest[sz] = source[i];
    }
  }
  return dest;
}

} //# end namespace


/*
http://www.berghel.net/publications/asm/asm.php
p := k;
   repeat
       inc := p;
       for temp_p := 0 to p-1
           if |(n-m) - inc| <= temp_p
               f((n-m)-inc,temp_p);
         endif
         if |(n-m) + inc| <= temp_p
             f((n-m)+inc,temp_p);
         endif
         inc := inc - 1;
       endfor
       f(n-m,p);
       p := p + 1;
   until FKP[(n-m)+ZERO_K,p-1] = m;
   s := p-1;
   procedure f(k, p)
   begin
       t := FKP[k+ZERO_K,p-1] + 1;
       t2 := t;
       if atat+1 = bk+t+1bk+t then
           t2 := t+1;
       t := max(t, FKP[k-1+ZERO_K,p-1],
           FKP[k+1+ZERO_K,p-1] + 1,t2);
       while at+1 = bt+1+k and t < min(m,n-k) do
           t := t+1;
       FKP[k+ZERO_K,p] := t;
   end
*/
