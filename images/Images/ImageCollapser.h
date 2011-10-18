//# tSubImage.cc: Test program for class SubImage
//# Copyright (C) 1998,1999,2000,2001,2003
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
//#
//# $Id$

#ifndef IMAGES_IMAGECOLLAPSER_H
#define IMAGES_IMAGECOLLAPSER_H

#include <casa/Containers/HashMap.h>
#include <casa/Logging/LogIO.h>
#include <images/Images/ImageInputProcessor.h>
#include <images/Images/ImageInterface.h>

#include <casa/namespace.h>

namespace casa {

    class ImageCollapser {
        // <summary>
        // Top level interface which allows collapsing of images along a single axis. An aggregate method
    	// (average, sum, etc) is applied to the collapsed pixels.
        // </summary>

        // <reviewed reviewer="" date="" tests="" demos="">
        // </reviewed>

        // <prerequisite>
        // </prerequisite>

        // <etymology>
        // Collapses image.
        // </etymology>

        // <synopsis>
        // High level interface for collapsing an image along a single axis.
        // </synopsis>

        // <example>
        // <srcblock>
        // ImageCollapser collapser();
        // collapser.collapse();
        // </srcblock>
        // </example>

    public:

    	enum AggregateType {
    		AVDEV,
    		MAX,
    		MEAN,
    		MEDIAN,
    		MIN,
    		RMS,
    		STDDEV,
    		SUM,
    		VARIANCE,
    		// set all pixels in output image to 0
    		ZERO,
    		UNKNOWN
    	};

    	// if <src>outname</src> is empty, no image will be written
    	// if <src>overwrite</src> is True, if image already exists it will be removed
    	// if <src>overwrite</src> is False, if image already exists exception will be thrown
    	//
    	// <group>
    	ImageCollapser(
    	    String aggString,const String& imagename,
    	    const String& region, const String& box,
    	    const String& chanInp, const String& stokes,
    	    const String& maskInp, const uInt axis,
            const String& outname, const Bool overwrite
    	);

    	ImageCollapser(
    	    String aggString,const String& imagename,
    	    const String& region, const String& box,
    	    const String& chanInp, const String& stokes,
    	    const String& maskInp, const Vector<uInt>& axes,
            const String& outname, const Bool overwrite
    	);

    	ImageCollapser(
    	    String aggString, const ImageInterface<Float> * const image,
    	    const String& region, const String& box,
    	    const String& chanInp, const String& stokes,
    	    const String& maskInp, const uInt axis,
            const String& outname, const Bool overwrite
    	);

    	ImageCollapser(
    	    String aggString, const ImageInterface<Float> * const image,
    	    const String& region, const String& box,
    	    const String& chanInp, const String& stokes,
    	    const String& maskInp, const Vector<uInt> axes,
            const String& outname, const Bool overwrite
    	);

    	ImageCollapser(
            const ImageInterface<Float> * const image,
    	    const Vector<uInt>& axes, const Bool invertAxesSelection,
    	    const AggregateType aggregateType,
    	    const String& outname, const Bool overwrite
    	);
    	// </group>

        // destructor
        ~ImageCollapser();

        // perform the collapse. If <src>wantReturn</src> is True, return a pointer to the
        // collapsed image. The returned pointer is created via new(); it is the caller's
        // responsibility to delete the returned pointer. If <src>wantReturn</src> is False,
        // a NULL pointer is returned and pointer deletion is performed internally.
        ImageInterface<Float>* collapse(const Bool wantReturn) const;

        static const HashMap<uInt, Float (*)(const Array<Float>&)>* funcMap();
        static const HashMap<uInt, String>* funcNameMap();
        static const HashMap<uInt, String>* minMatchMap();

    private:
        LogIO *_log;
        ImageInterface<Float> * _image;
        Record _regionRecord;
        String _chan, _stokesString, _mask, _outname;
        Bool _overwrite, _destructImage, _invertAxesSelection;
        Vector<uInt> _axes;
        AggregateType _aggType;

        static HashMap<uInt, Float (*)(const Array<Float>&)> *_funcMap;
        static HashMap<uInt, String> *_funcNameMap;
        static HashMap<uInt, String> *_minMatchMap;

        // disallow default constructor
        ImageCollapser();

        // does the lion's share of constructing the object, ie checks validity of
        // inputs, etc.
        void _construct(
        	String& aggString, const String& imagename,
        	const String& box, const String& regionName
        );

        void _construct(
        	String& aggString, const ImageInterface<Float> *image,
        	const String& box, const String& regionName
        );

        void _setAggregateType(String& aggString);

        void _invert();

        Vector<ImageInputProcessor::OutputStruct> _getOutputStruct();

        void _finishConstruction();
    };
}

#endif
