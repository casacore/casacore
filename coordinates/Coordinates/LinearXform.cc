//# LinearXForm.cc: this defines LinearXForm
//# Copyright (C) 1997-2003
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
//#---------------------------------------------------------------------------

#include <casacore/coordinates/Coordinates/LinearXform.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/MatrixMath.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/LinearSearch.h>
#include <casacore/casa/stdlib.h>
#include <casacore/casa/sstream.h>

#include <wcslib/lin.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

LinearXform::LinearXform(uInt naxis)
  : isPCDiagonal_p(True)
{
    linprm_p.flag = -1;
    linini(1, naxis, &linprm_p);
    set_linprm();
}


LinearXform::LinearXform(const Vector<Double> &crpixIn,
                         const Vector<Double> &cdeltIn)
  : isPCDiagonal_p(True)
{
    const uInt naxis = crpixIn.nelements();
    AlwaysAssert(cdeltIn.nelements() == naxis, AipsError);
//
    int n = naxis;
    linprm_p.flag = -1;
    linini(1, n, &linprm_p);
//
    for (uInt i = 0; i < naxis; i++) {
        linprm_p.crpix[i] = crpixIn[i];
        linprm_p.cdelt[i] = cdeltIn[i];
    }
    set_linprm();
}


LinearXform::LinearXform(const Vector<Double>& crpixIn,
                         const Vector<Double>& cdeltIn,
                         const Matrix<Double>& pcIn)
{
    const uInt naxis = crpixIn.nelements();
    AlwaysAssert(cdeltIn.nelements() == naxis && pcIn.nrow() == naxis &&
                 pcIn.ncolumn() == naxis, AipsError);
//
    int n = naxis;
    linprm_p.flag = -1;
    linini(1, n, &linprm_p);
//
    Double zero = 0.0;
    Double tol = 1e-12;
    isPCDiagonal_p = True;
//
    uInt ij = 0;
    for (uInt i = 0; i < naxis; i++) {
        linprm_p.crpix[i] = crpixIn[i];
        linprm_p.cdelt[i] = cdeltIn[i];
//
        for (uInt j = 0; j < naxis; j++) {

// Is pc is diagonal?  Done purely for use in the Fourier
// inversion stuff.  Urk.

            if (i != j && !casacore::near(pcIn(j,i),zero,tol)) {
                isPCDiagonal_p = False;
            }
            linprm_p.pc[ij++] = pcIn(j,i);
        }
    }
//
    set_linprm();
}


LinearXform::LinearXform(const LinearXform &other)
  : isPCDiagonal_p(other.isPCDiagonal_p)
{
    linprm_p.flag = -1;
    lincpy(1, &(other.linprm_p), &linprm_p);
    set_linprm();
}


LinearXform &LinearXform::operator=(const LinearXform &other)
{
    if (this != &other) {
        lincpy(1, &(other.linprm_p), &linprm_p);
        isPCDiagonal_p = other.isPCDiagonal_p;
        set_linprm();
    }
    return *this;
}


LinearXform::~LinearXform()
{
    linfree(&linprm_p);
}


uInt LinearXform::nWorldAxes() const
{
    return linprm_p.naxis;
}


Bool LinearXform::forward(Vector<Double>& pixel,
                          const Vector<Double>& world,
                          String& errorMsg) const
{
    uInt naxis = world.nelements();
    pixel.resize(naxis);
//
    Bool delPixel, delWorld;
    double* pixelStor = pixel.getStorage(delPixel);
    const double* worldStor = world.getStorage(delWorld);
//
    int n = naxis;
    if (int err = linx2p(&linprm_p, 1, n, worldStor, pixelStor)) {
        errorMsg = "wcs linx2p error: ";
        errorMsg += linx2p_errmsg[err];
        return False;
    }
//
    pixel.putStorage(pixelStor, delPixel);
    world.freeStorage(worldStor, delWorld);
//
    return True;
}

Bool LinearXform::reverse(Vector<Double> &world,
                          const Vector<Double> &pixel,
                          String &errorMsg) const
{
    uInt naxis = pixel.nelements();
    world.resize(naxis);
//
    Bool delPixel, delWorld;
    const double* pixelStor = pixel.getStorage(delPixel);
    double* worldStor = world.getStorage(delWorld);
//
    int n = naxis;
    if (int err = linp2x(&linprm_p, 1, n, pixelStor, worldStor)) {
        errorMsg = "wcs linp2x error: ";
        errorMsg += linp2x_errmsg[err];
        return False;
    }
//
    pixel.freeStorage(pixelStor, delPixel);
    world.putStorage(worldStor, delWorld);
//
    return True;
}


Vector<Double> LinearXform::crpix() const
{
    uInt naxis = linprm_p.naxis;
    Vector<Double> tmp(naxis);
//
    const double* dp = linprm_p.crpix;
    for (uInt i = 0; i < naxis; i++) {
        tmp[i] = *(dp++);
    }
    return tmp;
}


Vector<Double> LinearXform::cdelt() const
{
    uInt naxis = linprm_p.naxis;
    Vector<Double> tmp(naxis);
//
    const double* dp = linprm_p.cdelt;
    for (uInt i = 0; i < naxis; i++) {
        tmp[i] = *(dp++);
    }
    return tmp;
}


Matrix<Double> LinearXform::pc() const
{
    uInt naxis = linprm_p.naxis;
    Matrix<Double> tmp(naxis,naxis);
//
    const double* dp = linprm_p.pc;
    for (uInt i = 0; i < naxis; i++) {
        for (uInt j = 0; j < naxis; j++) {
          tmp(j,i) = *(dp++);
        }
    }

    return tmp;
}

void LinearXform::crpix(const Vector<Double> &newvals)
{
    AlwaysAssert(newvals.nelements() == nWorldAxes(), AipsError);
//
    const Vector<Double>& cdlt = this->cdelt();
    const Matrix<Double>& pcm = this->pc();
//
    *this = LinearXform(newvals, cdlt, pcm);
}

void LinearXform::cdelt(const Vector<Double> &newvals)
{
    AlwaysAssert(newvals.nelements() == nWorldAxes(), AipsError);
//
    const Vector<Double>& crp = this->crpix();
    const Matrix<Double>& pcm = this->pc();
//
    *this = LinearXform(crp, newvals, pcm);
}


void LinearXform::pc(const Matrix<Double>& newvals)
{
    AlwaysAssert(newvals.nrow()    == nWorldAxes() &&
                 newvals.ncolumn() == nWorldAxes(), AipsError);
//
    const Vector<Double>& crp = this->crpix();
    const Vector<Double>& cdlt = this->cdelt();

    *this = LinearXform(crp, cdlt, newvals);
}


Bool LinearXform::near(const LinearXform& other,
                       Double tol) const
{
   Vector<Int> excludeAxes;
   return near(other, excludeAxes, tol);
}



Bool LinearXform::near(const LinearXform& other,
                       const Vector<Int>& excludeAxes,
                       Double tol) const
{
// Number of pixel and world axes is the same for a LinearXform.

    uInt naxes = excludeAxes.nelements();
    Vector<Bool> exclude(linprm_p.naxis);
    Bool found;
    for (uInt i = 0; i < nWorldAxes(); i++) {
        exclude[i] = (linearSearch(found, excludeAxes, Int(i), naxes) >= 0);
    }

// Compare reference pixels and increments.

    {
       const Vector<Double>& d1 = this->crpix();
       const Vector<Double>& d2 = other.crpix();
       if (d1.nelements() != d2.nelements()) return False;
       for (uInt i = 0; i < d1.nelements(); i++) {
           if (!exclude[i]) {
               if (!casacore::near(d1(i),d2(i),tol)) return False;
           }
       }
    }

    {
       const Vector<Double>& d1 = this->cdelt();
       const Vector<Double>& d2 = other.cdelt();
       if (d1.nelements() != d2.nelements()) return False;
       for (uInt i = 0; i < d1.nelements(); i++) {
           if (!exclude(i)) {
               if (!casacore::near(d1[i],d2[i],tol)) return False;
           }
       }
    }

// Check the matrix.

    Matrix<Double> pc1 = this->pc();
    Matrix<Double> pc2 = other.pc();
    if (pc1.nrow()    != pc2.nrow())    return False;
    if (pc1.ncolumn() != pc2.ncolumn()) return False;

// Compare row by row.  An axis will turn up in the PC matrix in any row
// or column with that number.  E.g., values pertaining to axis "i" will
// be found in all entries of row "i" and all entries of column "i".

    for (uInt j = 0; j < pc1.nrow(); j++) {
        Vector<Double> row1 = pc1.row(j);
        Vector<Double> row2 = pc2.row(j);
        if (!exclude(j)) {
            for (uInt i = 0; i < row1.nelements(); i++) {
                if (!exclude(i)) {
                    if (!casacore::near(row1(i),row2(i),tol)) return False;
                }
            }
        }
    }

    return True;
}


void LinearXform::set_linprm(void)
{
    if (int err = linset(&linprm_p)) {
        String errmsg = "wcs linset error: ";
        errmsg += linset_errmsg[err];
        throw(AipsError(errmsg));
    }
}

} //# NAMESPACE CASACORE - END

