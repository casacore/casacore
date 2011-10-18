//#ImageReorderer.h
//#
//# Copyright (C) 1998,1999,2000,2001,2003 Associated Universities, Inc. Washington DC, USA.
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
//#  Created on: May 7, 2010
//#     Author: dmehring

#ifndef IMAGEREORDERER_H_
#define IMAGEREORDERER_H_

#include <images/Images/PagedImage.h>
#include <casa/Logging/LogIO.h>

namespace casa {
class ImageReorderer {
    // <summary>
      // Top level interface for reordering image axes
      // </summary>

      // <reviewed reviewer="" date="" tests="" demos="">
      // </reviewed>

      // <prerequisite>
      // </prerequisite>

      // <etymology>
      // Reorders images axes.
      // </etymology>

      // <synopsis>
      // ImageReorderer is the top level interface for reordering image axes.
      // </synopsis>

      // <example>
      // <srcblock>
      // ImageReorderer reorderer(...)
      // reorderer.reorder();
      // </srcblock>
      // </example>
public:
	ImageReorderer(const String& imagename, uInt order, const String& outputImage);

	ImageReorderer(const String& imagename, const String& order, const String& outputImage);

	ImageReorderer(const String& imagename, const Vector<String> order, const String& outputImage);

	ImageReorderer(const ImageInterface<Float> * const image, uInt order, const String& outputImage);

	ImageReorderer(const ImageInterface<Float> * const image, const String& order, const String& outputImage);

	ImageReorderer(const ImageInterface<Float> * const image, const Vector<String> order, const String& outputImage);
	// destructor
	~ImageReorderer();

	// reorder the axes and write the output image. Returns the associated PagedImage object.
	ImageInterface<Float>* reorder() const;

private:
	LogIO *_log;
	ImageInterface<Float> *_image;
	Vector<Int> _order;
	String _outputImage;
	// Do not allow use of default constuctor
	ImageReorderer();

	void _construct(const String& imagename);

	Vector<Int> _getOrder(uInt order) const;

	Vector<Int> _getOrder(const String& order) const;
/*
	Vector<Int> _getOrder(Vector<String>& order) const;

	void _downcase(Vector<String>& vec) const;
	*/
};
}


#endif /* IMAGEREORDERER_H_ */
