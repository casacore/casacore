//# LatticeConvolver.cc:
//# Copyright (C) 1997
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

#include <trial/Lattices/LatticeConvolver.h>
#include <trial/Lattices/LatticeFFT.h>
#include <aips/Lattices/IPosition.h>

template<class T> LatticeConvolver<T>::
LatticeConvolver()
  :itsXfr(IPosition(1,1))
{
  itsXfr.putAt(NumericTraits<T>::ConjugateType(1), IPosition(1,1));
} 

template<class T> LatticeConvolver<T>::
LatticeConvolver(const Lattice<T> & psf)
{
  IPosition shape = psf.shape();
  shape(0) = (shape(0) + 2)/2;
  itsXfr = TempLattice<NumericTraits<T>::ConjugateType>(shape);
  LatticeFFT::rcfft(itsXfr, psf);
} 

template<class T> LatticeConvolver<T>::
~LatticeConvolver()
{
}

template<class T> void LatticeConvolver<T>::
getPsf(Lattice<T> & psf) const {
  LatticeFFT::crfft(psf, itsXfr);
}

// Local Variables: 
// compile-command: "cd test; gmake OPTLIB=1 inst"
// End: 
