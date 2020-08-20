//# LatticeFFT.h: Definitions for Lattice FFT functions
//# Copyright (C) 1996,1997,1998,2003
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
//#
//# $Id$

#ifndef LATTICES_LATTICEFFT_H
#define LATTICES_LATTICEFFT_H

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/scimath/Mathematics/NumericTraits.h>
#include <casacore/casa/Arrays/ArrayFwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T> class Lattice;

// <summary>Functions for Fourier transforming Lattices</summary>

// <reviewed reviewer="" date="" tests="" demos="">
// </reviewed>

// <prerequisite>
// </prerequisite>

// <etymology>
// </etymology>

// <synopsis> 
// </synopsis> 

// <example>
// <srcblock>
// </srcblock>
// </example>

// <motivation>
// </motivation>

// <todo asof="">
// </todo>

class LatticeFFT
{

public: 
  // 2-D in-place complex->complex FFT. Transforms over the first two
  // dimensions and iterates over all the others. The Lattice must have two or
  // more dimensions otherwise an AipsError is thrown.
    template <class ComplexType> static void cfft2d(
        Lattice<ComplexType> & cLattice, const Bool toFrequency=True
    );
  
  // N-D in-place complex->complex FFT. Only transform over selected
  // dimensions. Iterate over the others. whichAxes must be the same length as
  // the number of dimensions in the Lattice otherwise an AipsError is thrown.
    template <class ComplexType> static void cfft(Lattice<ComplexType> & cLattice,
  		  const Vector<Bool> & whichAxes, const Bool toFrequency=True);

  // Non-folded version
    template <class ComplexType> static void cfft0(Lattice<ComplexType> & cLattice,
  		  const Vector<Bool> & whichAxes, const Bool toFrequency=True);

  // N-D in-place complex->complex FFT. Transform over all axes.
    template <class ComplexType> static void cfft(
        Lattice<ComplexType> & cLattice, const Bool toFrequency=True
    );

  // N-D real->complex FFT. Only one half of the Hermition result is
  // returned. Transforms are only done on selected dimensions. The origin of
  // the transform is the center of the Lattice ie., [nx/2,ny/2,...] if
  // doShift is True. Otherwise it is the first element ie., [0,0,...]
    template <class ComplexType> static void rcfft(
        Lattice<ComplexType> & out,
        const Lattice<typename NumericTraits<ComplexType>::ConjugateType> & in,
        const Vector<Bool> & whichAxes, const Bool doShift=True,
        Bool doFast=False
    );

    template <class ComplexType> static void myrcfft(
        Lattice<ComplexType> & out,
        const Lattice<typename NumericTraits<ComplexType>::ConjugateType> & in,
        const Vector<Bool> & whichAxes, const Bool doShift=True
    );

  // N-D real->complex FFT. Only one half of the Hermition result is
  // returned. Transform over all dimensions. The origin of
  // the transform is the center of the Lattice ie., [nx/2,ny/2,...] if
  // doShift is True. Otherwise it is the first element ie., [0,0,...]
    template <class ComplexType> static void rcfft(
        Lattice<ComplexType> & out,
        const Lattice<typename NumericTraits<ComplexType>::ConjugateType> & in,
        const Bool doShift=True, Bool doFast=False
    );
    template <class ComplexType> static void myrcfft(
        Lattice<ComplexType> & out,
        const Lattice<typename NumericTraits<ComplexType>::ConjugateType> & in,
        const Bool doShift=True
    );

  // N-D complex->real FFT. Only one half of the Hermition input is
  // required. If whichAxis is specified Transforms are only done on selected
  // dimensions otherwise they are done on all axes. The origin of the
  // transform is the center of the Lattice ie., [nx/2,ny/2,...] if doShift is
  // True, otherwise it is the first element ie., [0,0,...]  

  // These functions will <b>scramble the input Lattice</b> unless the versions
  // with const inputs are used. The const input versions are less efficient as
  // they create a temporary Lattice and copy the input data into it.
  // <group>
    template <class ComplexType> static void crfft(
        Lattice<typename NumericTraits<ComplexType>::ConjugateType> & out,
        Lattice<ComplexType> & in, const Vector<Bool> & whichAxes,
		const Bool doShift=True, Bool doFast=False
    );
    template <class ComplexType> static void crfft(
        Lattice<typename NumericTraits<ComplexType>::ConjugateType> & out,
        Lattice<ComplexType> & in, const Bool doShift=True, Bool doFast=False
    );
    template <class ComplexType> static void crfft(
        Lattice<typename NumericTraits<ComplexType>::ConjugateType> & out,
        const Lattice<ComplexType> & in,
        const Bool doShift=True, Bool doFast=False
    );
  // </group>
};

// implement template specializations to throw exceptions in the relevant cases.

template <> inline void LatticeFFT::cfft2d(Lattice<Float>&, const Bool) {
    ThrowCc(
        String(__func__) +": This method does not support real-valued lattices"
    );
}

template <> inline void LatticeFFT::cfft2d(Lattice<Double>&, const Bool) {
    ThrowCc(
        String(__func__) + ": This method does not support real-valued lattices"
    );
}

template <> inline void LatticeFFT::cfft(
    Lattice<Float>&, const Vector<Bool>&, const Bool
) {
    ThrowCc(
        String(__func__) + ": This method does not support real-valued lattices"
    );
}

template <> inline void LatticeFFT::cfft(
    Lattice<Double>&, const Vector<Bool>&, const Bool
) {
    ThrowCc(
        String(__func__) + ": This method does not support real-valued lattices"
    );
}

template <> inline void LatticeFFT::rcfft(
    Lattice<Float> &, const Lattice<Complex> &, const Vector<Bool> & ,
    const Bool, Bool
) {
    ThrowCc(
        String(__func__) + ": This is the real -> complex version, you've "
        "called it with the wrong parameters"
    );
}

template <> inline void LatticeFFT::rcfft(
    Lattice<Double> &, const Lattice<DComplex> & , const Vector<Bool> &,
    const Bool, Bool
) {
    ThrowCc(
        String(__func__) + ": This is the real -> complex version, you've "
        "called it with the wrong parameters"
    );
}

template <> inline void LatticeFFT::myrcfft(
    Lattice<Float> &, const Lattice<Complex> &,
    const Vector<Bool> &, const Bool
) {
    ThrowCc(
        String(__func__) + ": This is the real -> complex version, you've "
        "called it with the wrong parameters"
    );
}

template <> inline void LatticeFFT::myrcfft(
    Lattice<Double> &, const Lattice<DComplex> &,
    const Vector<Bool> &, const Bool
) {
    ThrowCc(
        String(__func__) + ": This is the real -> complex version, you've "
        "called it with the wrong parameters"
    );
}

template <> inline void LatticeFFT::rcfft(
    Lattice<Float> &, const Lattice<Complex> &, const Bool, Bool
) {
    ThrowCc(
        String(__func__) + ": This is the real -> complex version, you've "
        "called it with the wrong parameters"
    );
}

template <> inline void LatticeFFT::rcfft(
    Lattice<Double> &, const Lattice<DComplex> &, const Bool, Bool
) {
    ThrowCc(
        String(__func__) + ": This is the real -> complex version, you've "
        "called it with the wrong parameters"
    );
}

template <> inline void LatticeFFT::myrcfft(
    Lattice<Float> &, const Lattice<Complex> &, const Bool
) {
    ThrowCc(
        String(__func__) + ": This is the real -> complex version, you've "
        "called it with the wrong parameters"
    );
}

template <> inline void LatticeFFT::myrcfft(
    Lattice<Double> &, const Lattice<DComplex> &, const Bool
) {
    ThrowCc(
        String(__func__) + ": This is the real -> complex version, you've "
        "called it with the wrong parameters"
    );
}

template <> inline void LatticeFFT::crfft(
    Lattice<Complex>&, Lattice<Float> &,
    const Vector<Bool> &, const Bool, Bool
) {
    ThrowCc(
        String(__func__) + ": This is the complex -> real version, you've "
        "called it with the wrong parameters"
    );
}

template <> inline void LatticeFFT::crfft(
    Lattice<DComplex> &, Lattice<Double> &,
    const Vector<Bool> &, const Bool, Bool
) {
    ThrowCc(
        String(__func__) + ": This is the complex -> real version, you've "
        "called it with the wrong parameters"
    );
}

template <> inline void LatticeFFT::crfft(
    Lattice<Complex> &, Lattice<Float> &, const Bool, Bool
) {
    ThrowCc(
        String(__func__) + ": This is the complex -> real version, you've "
        "called it with the wrong parameters"
    );
}

template <> inline void LatticeFFT::crfft(
    Lattice<DComplex> &, Lattice<Double> &, const Bool, Bool
) {
    ThrowCc(
        String(__func__) + ": This is the complex -> real version, you've "
        "called it with the wrong parameters"
    );
}

template <> inline void LatticeFFT::crfft(
    Lattice<Complex> &, const Lattice<Float> &, const Bool, Bool
) {
    ThrowCc(
        String(__func__) + ": This is the complex -> real version, you've "
        "called it with the wrong parameters"
    );
}

template <> inline void LatticeFFT::crfft(
    Lattice<DComplex> &, const Lattice<Double> &, const Bool, Bool
) {
    ThrowCc(
        String(__func__) + ": This is the complex -> real version, you've "
        "called it with the wrong parameters"
    );
}

} //# NAMESPACE CASACORE - END
#ifndef CASACORE_NO_AUTO_TEMPLATES
#include "LatticeFFT.tcc"
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
