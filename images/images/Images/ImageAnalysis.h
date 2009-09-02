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
#include <lattices/LatticeMath/Fit2D.h>
#include <components/ComponentModels/ComponentType.h>
#include <casa/Arrays/AxesSpecifier.h>

namespace casa {

class DirectionCoordinate;
class LogIO;
class SkyComponent;
class String;
class Record;
class Fit2D;
class ImageRegion;
class ComponentList;
template<class T> class Array;
template<class T> class Block;
template<class T> class PtrBlock; 
template<class T> class Flux;
template<class T> class ImageInterface;
template<class T> class ImageStatistics;
template<class T> class ImageHistograms;
template<class T> class MaskedArray;
template<class T> class PagedImage;
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
    ImageAnalysis(const casa::ImageInterface<casa::Float>* inImage);
    
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

    Bool imagefromforeign(const String& outfile, const String& infile, 
                          const String& format, const Bool overwrite = False);

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

    Quantity convertflux(const Quantity& value, const Quantity& major, 
                         const Quantity& minor, 
                         const String& type = "Gaussian", 
                         const Bool topeak = True);

    ImageInterface<Float> * convolve2d(const String& outfile, 
                                       const Vector<Int>& axes, 
                                       const String& type, 
                                       const Quantity& major, 
                                       const Quantity& minor, 
                                       const Quantity& pa, 
                                       Double scale, Record& region,
                                       const String& mask, 
                                       const Bool overwrite = False, 
                                       const Bool async = False);

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

    Bool remove();

    Bool fft(const String& real, const String& imag, const String& amp, 
             const String& phase, const Vector<Int>& axes, Record& region, 
             const String& mask);

    Record findsources(const Int nmax, const Double cutoff, Record& region, 
                        const String& mask, const Bool point = True, 
                        const Int width = 5, const Bool negfind = False);

    Bool fitallprofiles(Record& region, const Int axis, const String& mask,
                        const Int ngauss, const Int poly, 
                        const String& sigma = "", const String& fit = "", 
                        const String& resid = "");

    Record fitprofile(Vector<Float>& values, Vector<Float>& resid, 
                       Record& region, const Int axis, const String& mask, 
                       Record& estimate, const Int ngauss = -1, 
                       const Int poly = -1, const Bool fit = True, 
                       const String sigma = "");

    ImageInterface<Float>* fitpolynomial(const String& residfile, 
                                         const String& fitfile, 
                                         const String& sigmafile, 
                                         const Int axis, const Int order, 
                                         Record& region, const String& mask, 
                                         const bool overwrite = false);

    ComponentList fitsky(Array<Float>& pixels, Array<Bool>& pixelmask,
            Bool& converged, Record& region,
            const uInt& chan, const String& stokesString,
            const String& mask, 
            const Vector<String>& models, Record& estimate, 
            const Vector<String>& fixedparams, 
            const Vector<Float>& includepix, 
            const Vector<Float>& excludepix, 
            const Bool fit = True, 
            const Bool deconvolve = False, const Bool list = True);

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

    Bool statistics(Record& statsout, const Vector<Int>& axes, Record& region, 
                    const String& mask, const Vector<String>& plotstats, 
                    const Vector<Float>& includepix, 
                    const Vector<Float>& excludepix, 
                    const String& plotter = "/NULL", const Int nx = 1, 
                    const Int ny = 1, const Bool list = True, 
                    const Bool force = False, const Bool disk = False, 
                    const Bool robust = False, const Bool verbose = True);

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
                const Bool dropstokes = False, const Bool stokeslast = False);

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
    Bool getFreqProfile(const Vector<Double>& xy,  
                        Vector<Float>& zxaxisval, Vector<Float>& zyaxisval,
                        const String& xytype="world", 
                        const String& specaxis="freq",
                        const Int& whichStokes=0,
                        const Int& whichTabular=0,
                        const Int& whichLinear=0,
                        const String& xunits="");

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
                        const String& xunits="");

    static  Record * tweakedRegionRecord(Record *Region);


    // Return a record of the associates ImageInterface 
    Bool toRecord(RecordInterface& rec);
    // Create a pagedimage if imagename is not "" else create a tempimage
    Bool fromRecord(const RecordInterface& rec, const String& imagename="");

    // Deconvolve from beam
    casa::Bool
      deconvolveFromBeam(Quantity& majorFit,
                         Quantity& minorFit,
                         Quantity& paFit,
                         const Vector<Quantity>& beam);

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
    
    // Deconvolve from beam
    casa::Bool
      deconvolveFromBeam(casa::Quantum<casa::Double>& majorFit,
                         casa::Quantum<casa::Double>& minorFit,
                         casa::Quantum<casa::Double>& paFit,
                         casa::LogIO& os,
                         const casa::Vector<casa::Quantum<casa::Double> >& beam) const;
    
    
    // Deconvolve SkyComponent from beam
    casa::SkyComponent
      deconvolveSkyComponent(casa::LogIO& os, const casa::SkyComponent& skyIn,
                             const casa::Vector<casa::Quantum<casa::Double> >& beam,
                             const casa::DirectionCoordinate& dirCoord) const;
    
    // Delete private ImageStatistics and ImageHistograms objects
    bool deleteHistAndStats();
    
    // Convert a parameters vector to a SkyComponent
    casa::SkyComponent
      encodeSkyComponent(casa::LogIO& os, casa::Double& fluxRatio,
                         const casa::ImageInterface<casa::Float>& im,
                         casa::ComponentType::Shape modelType,
                         const casa::Vector<casa::Double>& parameters,
                         casa::Stokes::StokesTypes stokes,
                         casa::Bool xIsLong, casa::Bool deconvolveIt) const;
    
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
                         casa::Bool copyMask=casa::True);
    
    // Make a mask and define it in the image.
    casa::Bool makeMask(casa::ImageInterface<casa::Float>& out,
                        casa::String& maskName,
                        casa::Bool init, casa::Bool makeDefault,
                        casa::LogIO& os, casa::Bool list) const;

// Convert region Record to an ImageRegion pointer
    casa::ImageRegion* makeRegionRegion(casa::ImageInterface<casa::Float>& inImage,
                                        const casa::Record& theRegion, 
                                        const casa::Bool listBoundingBox,
                                        casa::LogIO& logger);

// Make ImageRegion from 'mask' string
    casa::ImageRegion* makeMaskRegion (const casa::String& mask) const;
    
    // Make a SubImage from a region and a WCLELMask string
    casa::SubImage<casa::Float> 
      makeSubImage(casa::ImageRegion*& pRegionRegion,
                   casa::ImageRegion*& pMaskRegion,
                   casa::ImageInterface<casa::Float>& inImage,
                   const casa::Record& theRegion, const casa::String& mask,
                   casa::Bool listBoundingBox, casa::LogIO& os,
                   casa::Bool writableIfPossible,
                   const casa::AxesSpecifier& axesSpecifier=casa::AxesSpecifier());

// See if the combination of the 'region' and 'mask' ImageRegions have changed
    casa::Bool haveRegionsChanged (casa::ImageRegion* pNewRegionRegion,
                                   casa::ImageRegion* pNewMaskRegion,
                                   casa::ImageRegion* pOldRegionRegion,
                                   casa::ImageRegion* pOldMaskRegion) const;

// Convert a Record to a CoordinateSystem
    casa::CoordinateSystem*
      makeCoordinateSystem(const casa::Record& cSys,
                           const casa::IPosition& shape) const;
    
    // Make a block of regions from a GlishRecord (filled in by substitute.g).
    void makeRegionBlock(casa::PtrBlock<const casa::ImageRegion*>& regions,
                         const casa::Record& Regions,
                         casa::LogIO& logger);
    
    // Put beam into +x -> +y frame
    casa::Vector<casa::Quantum<casa::Double> >
      putBeamInXYFrame (const casa::Vector<casa::Quantum<casa::Double> >& beam,
                        const casa::DirectionCoordinate& dirCoord) const;
    
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
                       const String& format); 

    Record worldVectorToRecord (const Vector<Double>& world, 
                                Int c, const String& format, 
                                Bool isAbsolute, Bool showAsAbsolute);

    Record worldVectorToMeasures(const Vector<Double>& world, 
                                 Int c, Bool abs);

    void trim (Vector<Double>& inout, 
               const Vector<Double>& replace);

    //return a vector of the spectral axis values in units requested
    //e.g "vel", "fre" or "pix"..specVal has to be sized already 
    Bool getSpectralAxisVal(const String& specaxis, Vector<Float>& specVal, 
                            const CoordinateSystem& cSys, const String& xunits);

//


        

};

} // casac namespace
#endif

