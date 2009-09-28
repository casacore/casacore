//# ImagePolProxy.h: a casa namespace class for imagepol tool
//# Copyright (C) 2007
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

#ifndef IMAGES_IMAGEPOL_H
#define IMAGES_IMAGEPOL_H

#include <images/Images/ImagePolarimetry.h>

namespace casa {

//# Forward declarations
class LogIO;
class String;
class Record;
class Normal;
template<class T> class ImageInterface;

// Implementation of the image polarimetry functionality
// available from casapy.

class ImagePol
{

 public:
  // "imagepol" constructors 
  ImagePol();
  ImagePol(ImageInterface<Float>& im);
  virtual ~ImagePol();

  // Make test image
  Bool imagepoltestimage(const String& outFile = "imagepol.iquv",
			 const Vector<Double>& rm = Vector<Double>(1, 0.0),
			 Bool rmDefault = True,
			 Double pa0 = 0.0, Double sigma = 0.01,
			 Int nx = 32, Int ny = 32, Int nf = 32,
			 Double f0 = 1.4e9, Double df = 128.0e6);

  Bool open(ImageInterface<Float>& im);
  Bool open(const String& infile);

  // Depolarization ratio
  // The image containing the delpolratio is in the returnrec 
  // Can be recovered using ImageInterface::fromRecord
  Bool depolratio(ImageInterface<Float>*& rtnim, const String& infile, 
		  Bool debias = False,
		  Double clip = 10.0, Double sigma = -1, 
		  const String& oufile="");  

  //Complex linear polarization image is stored in outfile
  Bool complexlinpol(const String& outfile);


  // Summary
  void summary() const;

  // sigma
  Float sigma(Float clip = 10.0) const;

  // Stokes I
  Bool stokesI(ImageInterface<Float>*& rtnim, const String& outfile="");
  Float sigmaStokesI(Float clip = 10.0) const;

  // Stokes Q
  Bool stokesQ(ImageInterface<Float>*& rtnim, const String& outfile="");
  Float sigmaStokesQ(Float clip = 10.0) const;

  // Stokes U
  Bool stokesU(ImageInterface<Float>*& rtnim, const String& outfile="");
  Float sigmaStokesU(Float clip = 10.0) const;

  // Stokes V
  Bool stokesV(ImageInterface<Float>*& rtnim, const String& outfile="");
  Float sigmaStokesV(Float clip = 10.0) const;

  // Linearly polarized intensity
  Bool linPolInt(ImageInterface<Float>*& rtnim, Bool debias = False,
		 Float clip = 10.0,
		 Float sigma = -1, const String& outfile = "");
  Float sigmaLinPolInt(Float clip = 10.0, Float sigma = -1) const;

  // Total polarized intensity.
  Bool totPolInt(ImageInterface<Float>*& rtnim, Bool debias = False,
		 Float clip = 10.0,
		 Float sigma = -1, const String& outfile = "");
  Float sigmaTotPolInt(Float clip = 10.0, Float sigma = -1) const;

  // Complex linear polarization
  void complexLinearPolarization (const String& outfile);

  // Complex linear polarization
  void complexFractionalLinearPolarization (const String& outfile);

  // Linearly polarized position angle
  Bool linPolPosAng(ImageInterface<Float>*& rtnim,
		    const String& outfile = "");
  Bool sigmaLinPolPosAng(ImageInterface<Float>*& rtnim, Float clip = 10.0,
			 Float sigma = -1, const String& outfile = "");

  // Fractional linearly polarized intensity
  Bool fracLinPol(ImageInterface<Float>*& rtnim, Bool debias = False,
		  Float clip = 10.0,
		  Float sigma = -1, const String& outfile = "");
  Bool sigmaFracLinPol(ImageInterface<Float>*& rtnim, Float clip = 10.0,
		       Float sigma = -1, const String& outfile = "");

  // Fractional total polarized intensity
  Bool fracTotPol(ImageInterface<Float>*& rtnim, Bool debias = False,
		  Float clip = 10.0,
		  Float sigma = -1, const String& outfile = "");
  Bool sigmaFracTotPol(ImageInterface<Float>*& rtnim, Float clip = 10.0,
		       Float sigma = -1, const String& outfile = "");

  // Depolarization ratio
  Bool depolarizationRatio (ImageInterface<Float>*& rtnim, 
			    const String& infile,
			    Bool debias = False, Float clip = 10.0,
			    Float sigma = -1, const String& outfile = "");
  Bool sigmaDepolarizationRatio (ImageInterface<Float>*& rtnim,
				 const String& infile,
				 Bool debias = False, Float clip = 10.0,
				 Float sigma = -1, const String& outfile = "");

  // Find Rotation Measure from Fourier method
  void fourierRotationMeasure(const String& outfile = "",
			      const String& outfileAmp = "",
			      const String& outfilePA = "",
			      const String& outfileReal = "",
			      const String& outfileImag = "",
			      Bool zeroZeroLag = False);

  // Find Rotation Measure from traditional method
  void rotationMeasure(const String& outRM = "", const String& outRMErr = "",
		       const String& outPA0 = "", const String& outPA0Err = "",
		       const String& outNTurns = "",
		       const String& outChiSq = "",
		       Int axis = -1, Float varQU = -1, Float rmFg = 0.0,
		       Float rmMax = 0.0, Float maxPaErr = 1e30,
		       const String& plotter = "",
		       Int nx = 5, Int ny = 5);

  // Make a complex image
  void makeComplex (const String& complex, const String& real = "",
		    const String& imag = "", const String& amp = "",
		    const String& phase = "");

 private:
  LogIO *itsLog;
  ImagePolarimetry *itsImPol;
  
  Bool copyImage(ImageInterface<Float>*& out, const ImageInterface<Float>&in, 
	    const String& outfile="", Bool overwrite=true);
  // Copy miscellaneous (MiscInfo, ImageInfo, history, units)
  void copyMiscellaneous (ImageInterface<Complex>& out,
			  const ImageInterface<Float>& in);
  void copyMiscellaneous (ImageInterface<Float>& out,
			 const ImageInterface<Float>& in);
  void fiddleStokesCoordinate(ImageInterface<Float>& ie,
			      Stokes::StokesTypes type);
  void fiddleStokesCoordinate(ImageInterface<Complex>& ie,
			      Stokes::StokesTypes type);
  // Make a PagedImage or TempImage output
  Bool makeImage (ImageInterface<Complex>*& out, 
		  const String& outfile, const CoordinateSystem& cSys,
		  const IPosition& shape, Bool isMasked=False,
		  Bool tempAllowed=True);
  Bool makeImage (ImageInterface<Float>*& out, 
		  const String& outfile, const CoordinateSystem& cSys,
		  const IPosition& shape, Bool isMasked=False,
		  Bool tempAllowed=True);
  // Make an IQUV image with some dummy RM data
  Bool  makeIQUVImage (ImageInterface<Float>*& pImOut, const String& outfile, 
		       Double sigma, 
                       Double pa0, const Vector<Float>& rm, 
		       const IPosition& shape,
                       Double f0, Double dF);
  // Fill IQUV image with Stokes values from RM data
  Bool fillIQUV (ImageInterface<Float>& im, uInt stokesAxis,
		 uInt spectralAxis, const Vector<Float>& rm, 
		 Float pa0);
  // Add noise to Array
  void addNoise (Array<Float>& slice, Normal& noiseGen);
  // Centre reference pixelin image
  void centreRefPix (CoordinateSystem& cSys, const IPosition& shape);
  // Make and define a mask
  Bool makeMask(ImageInterface<Float>& out, Bool init=False);
  Bool makeMask(ImageInterface<Complex>& out, Bool init=False);
  // What Stokes type?  Exception if more than one.
  Stokes::StokesTypes stokesType(const CoordinateSystem& cSys);


};

} // casa namespace

#endif
