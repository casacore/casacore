//# ImageAnalysis.h: Image analysis and handling tool
//# Copyright (C) 2007
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

#ifndef _IMAGEANALYSIS__H__
#define _IMAGEANALYSIS__H__


//# put includes here
#include <coordinates/Coordinates/CoordinateSystem.h>
#include <lattices/LatticeMath/Fit2D.h>
#include <casa/Quanta.h>
#include <measures/Measures/Stokes.h>
#include <images/Images/ImageFit1D.h>
#include <images/Images/ImageInfo.h>
#include <images/Images/ImageInterface.h>
#include <components/ComponentModels/ComponentType.h>
#include <casa/Arrays/AxesSpecifier.h>
#include <casa/Utilities/PtrHolder.h>
#include <measures/Measures/Stokes.h>

namespace casa {

class DirectionCoordinate;
class LogIO;
class SkyComponent;
class Record;
class Fit2D;
class ImageRegion;
class ComponentList;
template<class T> class Array;
template<class T> class Block;
template<class T> class PtrBlock; 
template<class T> class Flux;
template<class T> class ImageStatistics;
template<class T> class ImageHistograms;
template<class T> class MaskedArray;
template<class T> class Quantum;
template<class T> class SubLattice;
template<class T> class SubImage;
template<class T> class Vector;

// <summary>
// Image analysis and handling tool
// </summary>

// <synopsis>
// This the casapy image tool.
// One time it should be merged with pyrap's image tool ImageProxy.
// </synopsis>

class ImageAnalysis
{
  public:

    ImageAnalysis();

    //ImageInterface constructor
    ImageAnalysis(const ImageInterface<Float>* inImage);
    
    //Use this constructor with cloneInputPointer=False if you want this object
    // to take over management of the input pointer. The input pointer will be deleted
    // when this object is destroyed.
    ImageAnalysis(ImageInterface<Float>* inImage, const Bool cloneInputPointer);

    virtual ~ImageAnalysis();

    Bool addnoise(const String& type, const Vector<Double>& pars,
                  Record& region, const Bool zero = False);

    ImageInterface<Float> * imagecalc(const String& outfile, 
                                      const String& pixels, 
                                      const Bool overwrite = False);

    ImageInterface<Float> * imageconcat(const String& outfile, 
                                        const Vector<String>& infiles, 
                                        const Int axis, 
                                        const Bool relax = False, 
                                        const Bool tempclose = True, 
                                        const Bool overwrite = False);

    Bool imagefromarray(const String& outfile, Array<Float>& pixels, 
                        const Record& csys, const Bool linear = False, 
                        const Bool overwrite = False, const Bool log = True);

    Bool imagefromascii(const String& outfile, const String& infile, 
                        const Vector<Int>& shape, const String& sep, 
                        const Record& csys, const Bool linear = False, 
                        const Bool overwrite = False);

    Bool imagefromfits(const String& outfile, const String& infile, 
                       const Int whichrep = 0, const Int whichhdu = 0, 
                       const Bool zeroblanks = False, 
                       const Bool overwrite = False);

    Bool imagefromimage(const String& outfile, const String& infile, 
                        Record& region, const String& mask, 
                        const Bool dropdeg = False, 
                        const Bool overwrite = False);

    Bool imagefromshape(const String& outfile, const Vector<Int>& shape, 
                        const Record& csys, const Bool linear = True, 
                        const Bool overwrite = False, const Bool log = True);

    Bool adddegaxes(const String& outfile, 
                    PtrHolder<ImageInterface<Float> >& ph, 
                    const Bool direction, 
                    const Bool spectral, 
                    const String& stokes, 
                    const Bool linear = False, 
                    const Bool tabular = False, 
                    const Bool overwrite = False);

    ImageInterface<Float> * convolve(const String& outfile, 
                                     Array<Float>& kernel, 
                                     const String& kernImage, 
                                     const Double scale, 
                                     Record& region, String& mask, 
                                     const Bool overwrite = False, 
                                     const Bool async = False);

    Record* boundingbox(const Record& region);

    String brightnessunit();

    Bool calc(const String& pixels);

    // regions should be a Record of Records having different regions

    Bool calcmask(const String& mask, Record& regions, const String& name, 
                  const Bool asdefault = True);


    ImageInterface<Float> * continuumsub(const String& outline, 
                                         const String& outcont, Record& region,
                                         const Vector<int>& channels, 
                                         const String& pol = "", 
                                         const Int fitorder = 0, 
                                         const Bool overwrite = false);

    // the output <src>fakeBeam</src> indicates if there was no beam in the header and a fake one
    // was assumed to do the conversion.
    Quantity convertflux(Bool& fakeBeam, const Quantity& value, const Quantity& major,
                         const Quantity& minor, 
                         const String& type = "Gaussian", 
                         const Bool topeak = True,
                         Bool supressNoBeamWarnings = False);

    ImageInterface<Float>* convolve2d(
    		const String& outfile, const Vector<Int>& axes,
            const String& type, const Quantity& major,
            const Quantity& minor, const Quantity& pa,
            Double scale, Record& region, const String& mask,
            const Bool overwrite = False
    );

    CoordinateSystem coordsys(const Vector<int>& axes);

    CoordinateSystem csys(const Vector<int>& axes);

    Record* coordmeasures(Quantity& intensity, Record& direction, 
                          Record& frequency, Record& velocity, 
                          const Vector<double>& pixel);

   Matrix<Float> decompose(Record& region, const String& mask, 
                            const Bool simple = false, 
                            const Double threshold = -1, 
                            const Int ncontour = 11, 
                            const Int minrange = 1, 
                            const Int naxis = 2, 
                            const Bool fit = True, 
                            const Double maxrms = -1, 
                            const Int maxretry = -1, 
                            const Int maxiter = 256, 
                            const Double convcriteria = 0.0001);

   Matrix<Float> decompose(Matrix<Int>& blcs, Matrix<Int>& trcs, Record& region, const String& mask, 
			    const Bool simple = false, 
			    const Double threshold = -1, 
			    const Int ncontour = 11, 
			    const Int minrange = 1, 
			    const Int naxis = 2, 
			    const Bool fit = True, 
			    const Double maxrms = -1, 
			    const Int maxretry = -1, 
			    const Int maxiter = 256, 
			    const Double convcriteria = 0.0001);

    Record deconvolvecomponentlist(Record& complist);

    Bool remove(Bool verbose=true);

    Bool fft(const String& real, const String& imag, const String& amp, 
             const String& phase, const Vector<Int>& axes, Record& region, 
             const String& mask);

    Record findsources(const Int nmax, const Double cutoff, Record& region, 
                        const String& mask, const Bool point = True, 
                        const Int width = 5, const Bool negfind = False);

    // <src>subImage</src> will contain the subimage of the original image
    // on which the fit is performed.
    Vector<ImageFit1D<Float> >  fitallprofiles(
    	Record& region, SubImage<Float>& subImage, String& xUnit,
    	const Int axis, const String& mask,
        const Int ngauss, const Int poly,
        const String& weightsImageName = "", const String& fit = "",
        const String& resid = ""
    ) const;

    // <src>subImage</src> will contain the subimage of the original image
    // on which the fit is performed.
    ImageFit1D<Float> fitprofile(
        Record& regionRecord, SubImage<Float>& subImage,
        String& xUnit,
        const uInt axis, const String& mask,
        const Record& estimate, const uInt ngauss = 0,
        const Int poly = -1, const String& modelName = "",
        const String& residName = "", const Bool fit = True,
        const String weightsImageName = ""
    );

    ImageInterface<Float>* fitpolynomial(const String& residfile, 
                                         const String& fitfile, 
                                         const String& sigmafile, 
                                         const Int axis, const Int order, 
                                         Record& region, const String& mask, 
                                         const bool overwrite = false);

    // adding residImage parameter. If set to blank, do not write residual iamge,
    // else write to the file given.
    ComponentList fitsky(
    	Array<Float>& pixels, Array<Bool>& pixelmask,
        Bool& converged, Record& inputStats, Record& residStats,
        Double& chiSquared, Record& region, const uInt& chan,
        const String& stokesString, const String& mask,
        const Vector<String>& models, Record& inputEstimate,
        const Vector<String>& fixedparams,
        const Vector<Float>& includepix,
        const Vector<Float>& excludepix,
        const Bool fit=True,
        const Bool deconvolve=False, const Bool list=True,
        const String& residImage="", const String& modelImageName=""
    );

    Bool getchunk(Array<Float>& pixel, Array<Bool>& pixmask, 
                  const Vector<Int>& blc, const Vector<Int>& trc, 
                  const Vector<Int>& inc, const Vector<Int>& axes, 
                  const Bool list = False, const Bool dropdeg = False, 
                  const bool getmask = False);

    Bool getregion(Array<Float>& pixels, Array<Bool>& pixmask, Record& region, 
                   const Vector<Int>& axes, const String& mask, 
                   const Bool list = False, const Bool dropdeg = False, 
                   const bool getmask = False);

    Record* getslice(const Vector<Double>& x, const Vector<Double>& y, 
                     const Vector<Int>& axes, const Vector<Int>& coord, 
                     const Int npts = 0, const String& method = "linear");

    ImageInterface<Float> * hanning(const String& outfile, Record& region, 
                                    const String& mask, const Int axis = -10, 
                                    const Bool drop = True, 
                                    const bool overwrite = False);

    Vector<Bool> haslock();

    Bool histograms(Record& histout, const Vector<Int>& axes, Record& region, 
                    const String& mask, const Int nbins, 
                    const Vector<Double>& includepix, const Bool gauss, 
                    const Bool cumu, const Bool log, const Bool list, 
                    const String& plotter, const Int nx, const Int ny, 
                    const Vector<Int>& size, const Bool force = False, 
                    const Bool disk = False);

    Vector<String> history(const Bool list = False, const Bool browse = True);

    ImageInterface<Float> * insert(const String& infile, Record& region, 
                                   const Vector<double>& locate);

    //    Bool isopen();

    Bool ispersistent();

    Bool lock(const Bool writelock = False, const Int nattempts = 0);

    Bool makecomplex(const String& outfile, const String& imag, Record& region,
                     const Bool overwrite = False);

    Vector<String> maskhandler(const String& op,const Vector<String>& nam);

    Record miscinfo();

    Bool modify(Record& model, Record& region , const String& mask, 
                const Bool subtract = True, const Bool list = True);

    Record maxfit(Record& region, const Bool point, const Int width = 5, 
                   const Bool negfind = False, const Bool list = True);

    ImageInterface<Float> * moments(const Vector<Int>& moments, const Int axis,
                                    Record& region, const String& mask, 
                                    const Vector<String>& method, 
                                    const Vector<Int>& smoothaxes, 
                                    const Vector<String>& smoothtypes, 
                                    const Vector<Quantity>& smoothwidths, 
                                    const Vector<Float>& includepix, 
                                    const Vector<Float>& excludepix, 
                                    const Double peaksnr, const Double stddev, 
                                    const String& doppler = "RADIO", 
                                    const String& outfile = "", 
                                    const String& smoothout = "", 
                                    const String& plotter = "/NULL", 
                                    const Int nx = 1, const Int ny = 1, 
                                    const Bool yind = False, 
                                    const Bool overwrite = False, 
                                    const Bool drop = True);

    String name(const Bool strippath = False);

    Bool open(const String& infile);

    Record* pixelvalue(const Vector<Int>& pixel);
    void pixelValue (Bool& offImage, Quantum<Double>& value, Bool& mask,
                     Vector<Int>& pos) const;

    Bool putchunk(const Array<Float>& pixels, const Vector<Int>& blc, 
                  const Vector<Int>& inc, const Bool list = False, 
                  const Bool locking = True, const Bool replicate = False);

    Bool putregion(const Array<Float>& pixels, const Array<Bool>& pixelmask, 
                   Record& region, const Bool list = False, 
                   const Bool usemask = True, 
                   const Bool locking = True, const Bool replicate = False);

    ImageInterface<Float> * rebin(const String& outfile, 
                                  const Vector<Int>& bin, Record& region, 
                                  const String& mask, const Bool dropdeg, 
                                  const Bool overwrite = False);

    //regrids to a given coordinate system...one uses a record that is 
    //converted to a CoordinateSytem 

    ImageInterface<Float> * regrid(const String& outfile, 
                                   const Vector<Int>& shape, 
                                   const Record& csys, const Vector<Int>& axes,
                                   Record& region, const String& mask, 
                                   const String& method = "linear", 
                                   const Int decimate = 10, 
                                   const Bool replicate = False, 
                                   const Bool doref = True, 
                                   const Bool dropdeg = False, 
                                   const Bool overwrite = False, 
                                   const Bool force = False);
    
    ImageInterface<Float> * regrid(const String& outfile, 
                                   const Vector<Int>& shape, 
                                   const CoordinateSystem& csys, 
                                   const Vector<Int>& axes,
                                   Record& region, const String& mask, 
                                   const String& method = "linear", 
                                   const Int decimate = 10, 
                                   const Bool replicate = False, 
                                   const Bool doref = True, 
                                   const Bool dropdeg = False, 
                                   const Bool overwrite = False, 
                                   const Bool force = False);

    ImageInterface<Float> * rotate(const String& outfile, 
                                   const Vector<int>& shape, 
                                   const Quantity& pa, Record& region, 
                                   const String& mask, 
                                   const String& method = "cubic", 
                                   const Int decimate = 0, 
                                   const Bool replicate = False, 
                                   const Bool dropdeg = False,
                                   const Bool overwrite = False);

    Bool rename(const String& name, const Bool overwrite = False);

    Bool replacemaskedpixels(const String& pixels, Record& region,
                             const String& mask, const Bool update = False, 
                             const Bool list = False);

    Record restoringbeam();

    ImageInterface<Float> * sepconvolve(const String& outfile, 
                                        const Vector<Int>& axes, 
                                        const Vector<String>& types, 
                                        const Vector<Quantity>& widths, 
                                        Double scale, 
                                        Record& region, 
                                        const String& mask, 
                                        const bool overwrite = False);

    Bool set(const String& pixels, const Int pixelmask, 
             Record& region, const Bool list = false);

    Bool setbrightnessunit(const String& unit);

    bool setcoordsys(const Record& csys);

    bool sethistory(const String& origin, const Vector<String>& history);

    bool setmiscinfo(const Record& info);

    Vector<Int> shape();

    Bool setrestoringbeam(const Quantity& major, const Quantity& minor, 
                          const Quantity& pa, const Record& beam, 
                          const Bool remove = False, const Bool log = True);

    Bool statistics(
    	Record& statsout, const Vector<Int>& axes, Record& region,
        const String& mask, const Vector<String>& plotstats,
        const Vector<Float>& includepix, const Vector<Float>& excludepix,
        const String& plotter = "/NULL", const Int nx = 1,
        const Int ny = 1, const Bool list = True,
        const Bool force = False, const Bool disk = False,
        const Bool robust = False, const Bool verbose = True
    );

    bool twopointcorrelation(const String& outfile, Record& region, 
                             const String& mask, const Vector<Int>& axes, 
                             const String& method = "structurefunction", 
                             const Bool overwrite = False);

    ImageInterface<Float> * subimage(const String& outfile, Record& region, 
                                     const String& mask, 
                                     const Bool dropdeg = False, 
                                     const Bool overwrite = False, 
                                     const Bool list = True);

    Vector<String> summary(Record& header, const String& doppler = "RADIO", 
                            const Bool list = True, 
                            const Bool pixelorder = True);

    Bool tofits(const String& outfile, const Bool velocity, const Bool optical,
                const Int bitpix, const Double minpix, const Double maxpix, 
                Record& region, const String& mask, 
                const Bool overwrite = False, 
                const Bool dropdeg = False, const Bool deglast = False, 
                const Bool dropstokes = False, const Bool stokeslast = False,
		const Bool wavelength = False);

    Bool toASCII(const String& outfile, Record& region, const String& mask,
                 const String& sep = " ", const String& format = "%e", 
                 const Double maskvalue = -999, const Bool overwrite = False);


    Vector<Double> topixel(Record& value);

    Record toworld(const Vector<double>& value, const String& format = "n");

    Bool unlock();

    Bool detached();

    Record setregion(const Vector<Int>& blc, const Vector<Int>& trc, 
                      const String& infile = "");

    Record setboxregion(const Vector<Double>& blc, const Vector<Double>& trc,
                        const Bool frac = False, const String& infile = "");

    //make test image...cube or 2d (default)
    bool maketestimage(const String& outfile="", const Bool overwrite=False, 
                       const String& imagetype="2d");

    ImageInterface<Float> * newimage(const String& infile,
                                     const String& outfile,
                                     Record& region,
                                     const String& Mask,
                                     const bool dropdeg = False,
                                     const bool overwrite = False);

    ImageInterface<Float> * newimagefromfile(const String& fileName);

    ImageInterface<Float> * newimagefromarray(const String& outfile,
                                              Array<Float> & pixelsArray,
                                              const Record& csys,
                                              const Bool linear = False,
                                              const Bool overwrite = False,
                                              const Bool log = True);

    ImageInterface<Float> * newimagefromshape(const String& outfile,
                                              const Vector<Int>& shape, 
                                              const Record& csys,
                                              const Bool linear = True, 
                                              const Bool overwrite = False,
                                              const Bool log = True);

    ImageInterface<Float> * newimagefromfits(const String& outfile,
                                             const String& infile, 
                                             const Int whichrep = 0,
                                             const Int whichhdu = 0, 
                                             const Bool zeroblanks = False, 
                                             const Bool overwrite = False);

    Record* echo(Record& v, const Bool godeep = False);

    //Functions to get you back a spectral profile at direction position x, y.
    //x, y are to be in the world coord value or pixel value...user specifies
    //by parameter xytype ("world" or "pixel").
    //On success returns true
    //return value of profile is in zyaxisval, zxaxisval contains the spectral 
    //values at which zyaxisval is evaluated its in the spectral type
    //specified by specaxis...possibilities are "pixel", "freq", "radiovel", or "opticalvel"
    //(the code looks for the keywords "pixel", "freq", "vel", "optical", and "radio"
    // in the string)
    // if "vel" is found but no "radio" or "optical", the full relativistic velocity
    // is generated (MFrequency::RELATIVISTIC)
    // xunits determines the units of the x-axis values...default is "GHz" for 
    // freq and "km/s" for vel
    //PLEASE note that the returned value of zyaxisval are the units of the image
    //specframe can be a valid frame from MFrequency...i.e LSRK, LSRD etc...
    Bool getFreqProfile(const Vector<Double>& xy,  
                        Vector<Float>& zxaxisval, Vector<Float>& zyaxisval,
                        const String& xytype="world", 
                        const String& specaxis="freq",
                        const Int& whichStokes=0,
                        const Int& whichTabular=0,
                        const Int& whichLinear=0,
                        const String& xunits="",
			const String& specframe="");

    //how about using this ? 
    //for x.shape(xn) & y shape(yn)
    //if xn == yn == 1, single point
    //if xn == yn == 2, rectangle
    //if (xn == yn) > 2, polygon
    Bool getFreqProfile(const Vector<Double>& x,  
                        const Vector<Double>& y,  
                        Vector<Float>& zxaxisval, Vector<Float>& zyaxisval,
                        const String& xytype="world", 
                        const String& specaxis="freq",
                        const Int& whichStokes=0,
                        const Int& whichTabular=0,
                        const Int& whichLinear=0,
                        const String& xunits="",
			const String& specframe="");

    // Return a record of the associates ImageInterface 
    Bool toRecord(RecordInterface& rec);
    // Create a pagedimage if imagename is not "" else create a tempimage
    Bool fromRecord(const RecordInterface& rec, const String& imagename="");

    // Deconvolve from beam
    // return True if the deconvolved source is a point source, False otherwise
    // The returned value of successFit is True if the deconvolution could be performed, False otherwise.
    casa::Bool
      deconvolveFromBeam(Quantity& majorFit,
                         Quantity& minorFit,
                         Quantity& paFit,
                         Bool& successFit,
                         const Vector<Quantity>& beam);

    // get the associated ImageInterface object
    const ImageInterface<Float>* getImage() const;

 private:
    
    ImageInterface<Float>* pImage_p;
    LogIO * itsLog;

  

    // Having private version of IS and IH means that they will
    // only recreate storage images if they have to

    ImageStatistics<casa::Float>* pStatistics_p;
    ImageHistograms<casa::Float>* pHistograms_p;
    //
    IPosition last_chunk_shape_p;
    ImageRegion* pOldStatsRegionRegion_p;
    casa::ImageRegion* pOldStatsMaskRegion_p;
    casa::ImageRegion* pOldHistRegionRegion_p;
    casa::ImageRegion* pOldHistMaskRegion_p;
    casa::Bool oldStatsStorageForce_p, oldHistStorageForce_p;


   
    // Center refpix apart from STokes
    void centreRefPix (casa::CoordinateSystem& cSys,
                       const casa::IPosition& shape) const;
    
    // Convert types
    casa::ComponentType::Shape convertModelType (casa::Fit2D::Types typeIn) const;
   
    // Delete private ImageStatistics and ImageHistograms objects
    bool deleteHistAndStats();
   
    // Convert error parameters from pixel to world and insert in SkyComponent
    void encodeSkyComponentError (casa::LogIO& os,
                                  casa::SkyComponent& sky,
                                  casa::Double fluxRatio,
                                  const casa::ImageInterface<casa::Float>& subIm,
                                  const casa::Vector<casa::Double>& parameters,
                                  const casa::Vector<casa::Double>& errors,
                                  casa::Stokes::StokesTypes stokes,
                                  casa::Bool xIsLong) const;
    
    // Hanning smooth a vector
    void hanning_smooth (casa::Array<casa::Float>& out,
                         casa::Array<casa::Bool>& maskOut,
                         const casa::Vector<casa::Float>& in,
                         const casa::Array<casa::Bool>& maskIn,
                         casa::Bool isMasked) const;
    
    
// Make a new image with given CS
    casa::Bool make_image(casa::String &error, const casa::String &image,
                          const casa::CoordinateSystem& cSys,
                          const casa::IPosition& shape,
                          casa::LogIO& os, casa::Bool log=casa::True,
                          casa::Bool overwrite=casa::False);
    
    // If file name empty make TempImage (allowTemp=T) or do nothing.
    // Otherwise, make a PagedImage from file name and copy mask and
    // misc from inimage.   Returns T if image made, F if not
    casa::Bool
      makeExternalImage (casa::PtrHolder<casa::ImageInterface<casa::Float> >& image,
                         const casa::String& fileName,
                         const casa::CoordinateSystem& cSys,
                         const casa::IPosition& shape,
                         const casa::ImageInterface<casa::Float>& inImage,
                         casa::LogIO& os, casa::Bool overwrite=casa::False,
                         casa::Bool allowTemp=casa::False,
                         casa::Bool copyMask=casa::True) const;
    
    // Make a mask and define it in the image.
    casa::Bool makeMask(casa::ImageInterface<casa::Float>& out,
                        casa::String& maskName,
                        casa::Bool init, casa::Bool makeDefault,
                        casa::LogIO& os, casa::Bool list) const;
    /*
    // Make a SubImage from a region and a WCLELMask string
    SubImage<Float> makeSubImage(
    	ImageRegion*& pRegionRegion, ImageRegion*& pMaskRegion,
        ImageInterface<Float>& inImage, const Record& theRegion,
        const String& mask, LogIO *os, Bool writableIfPossible,
        const AxesSpecifier& axesSpecifier=casa::AxesSpecifier()
    );
*/
// See if the combination of the 'region' and 'mask' ImageRegions have changed
    casa::Bool haveRegionsChanged (casa::ImageRegion* pNewRegionRegion,
                                   casa::ImageRegion* pNewMaskRegion,
                                   casa::ImageRegion* pOldRegionRegion,
                                   casa::ImageRegion* pOldMaskRegion) const;

// Convert a Record to a CoordinateSystem
    casa::CoordinateSystem*
      makeCoordinateSystem(const casa::Record& cSys,
                           const casa::IPosition& shape) const;
    
    // Make a block of regions from a Record
    void makeRegionBlock(casa::PtrBlock<const casa::ImageRegion*>& regions,
                         const casa::Record& Regions,
                         casa::LogIO& logger);
    
    // Set the cache
    void set_cache(const casa::IPosition& chunk_shape) const;
    
    // Make an estimate for single parameter fit models
    casa::Vector<casa::Double>
      singleParameterEstimate (casa::Fit2D& fitter,
                               casa::Fit2D::Types modelType,
                               const casa::MaskedArray<casa::Float>& pixels,
                               casa::Float minVal, casa::Float maxVal,
                               const casa::IPosition& minPos,
                               const casa::IPosition& maxPos,
                               casa::Stokes::StokesTypes stokes,
                               const casa::ImageInterface<casa::Float>& im,
                               casa::Bool xIsLong,
                               casa::LogIO& os) const;

    // Prints an error message if the image DO is detached and returns True.
    //bool detached() const;
    
    // Convert object-id's in the expression to LatticeExprNode objects.
    // It returns a string where the object-id's are placed by $n.
    // That string can be parsed by ImageExprParse.
    // Furthermore it fills the string exprName with the expression
    // where the object-id's are replaced by the image names.
    // Note that an image name can be an expression in itself, so
    // this string is not suitable for the ImageExprParse.
    //casa::String substituteOID (casa::Block<casa::LatticeExprNode>& nodes,
    //                            casa::String& exprName,
    //                            const casa::String& expr) const;


    // Some helper functions that needs to be in casa namespace coordsys
    
    Record toWorldRecord (const Vector<Double>& pixel, 
                       const String& format) const;

    Record worldVectorToRecord (const Vector<Double>& world, 
                                Int c, const String& format, 
                                Bool isAbsolute, Bool showAsAbsolute) const;

    Record worldVectorToMeasures(const Vector<Double>& world, 
                                 Int c, Bool abs) const;

    void trim (Vector<Double>& inout, 
               const Vector<Double>& replace) const;

    //return a vector of the spectral axis values in units requested
    //e.g "vel", "fre" or "pix"..specVal has to be sized already 
    Bool getSpectralAxisVal(const String& specaxis, Vector<Float>& specVal, 
                            const CoordinateSystem& cSys, const String& xunits, 
			    const String& freqFrame="");


    // Set the include and/or exclude pixel range for fitsky(). The algorithm is:
    // if both includepix and excludepix are set, throw and exception
    // if neither is set and stokes=="I", all pixels >= 0 are included and all < 0 are excluded
    // if neither is set and stokes != "I", all pixels <= 0 are included and all pixels > 0 are excluded
    // if include/exclude has one element then -abs(element(0)) to abs(element(0)) are included/excluded
    // if include/exclude has two elements, then the range betwenn those elements is included/excluded
  
    void _setFitSkyIncludeExclude(
        const Vector<Float>& includepix, const Vector<Float>& excludepix,
        const Stokes::StokesTypes& stokes, const Float minVal, const Float maxVal,
        Fit2D& fitter
    ) const;

    void _fitskyExtractBeam(
    	Vector<Double>& parameters, const ImageInfo& imageInfo,
    	const Bool xIsLong, const CoordinateSystem& cSys
    ) const;

    // Writes a residual image and gets the stats from that image, and then
    // deletes the image if the user did not request it be saved.
    Record _fitskyWriteResidualAndGetStats(
    	const SubImage<Float>& subImage, const Array<Float>& residPixels, String residImageName
    ) const;

};

} // casac namespace
#endif

