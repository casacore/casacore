//# Slicer.cc: Class to specify which elements to extract from an n-dimensional array
//# Copyright (C) 1994,1995,1999
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

#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Arrays/ArrayError.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

Slicer::Slicer()
: asEnd_p (endIsLength),
  start_p (1,MimicSource),
  end_p   (1,MimicSource),
  stride_p(1,1),
  len_p   (1,MimicSource),
  fixed_p (False)
{}

Slicer::Slicer (const IPosition& bl, const IPosition& tr, const IPosition& in,
		LengthOrLast lol)
: asEnd_p (lol),
  start_p (bl),
  end_p   (tr),
  stride_p(in),
  len_p   (tr)
{
    fillEndLen();
}

Slicer::Slicer (const IPosition& bl, const IPosition& tr,
		LengthOrLast lol)
: asEnd_p (lol),
  start_p (bl),
  end_p   (tr),
  stride_p(bl.nelements(), 1),
  len_p   (tr)
{
    fillEndLen();
}

Slicer::Slicer (const IPosition& bl)
: asEnd_p (endIsLength),
  start_p (bl),
  end_p   (bl),
  stride_p(bl.nelements(), 1),
  len_p   (bl.nelements(), 1)
{
    fillFixed();
}

Slicer::Slicer (const Slice& x, const Slice& y, const Slice& z,
		LengthOrLast lol)
: asEnd_p (lol),
  start_p (3, MimicSource),
  end_p   (3, MimicSource),
  stride_p(3, 1),
  len_p   (3, MimicSource)
{
    fillSlice (x, start_p(0), len_p(0), stride_p(0));
    fillSlice (y, start_p(1), len_p(1), stride_p(1));
    fillSlice (z, start_p(2), len_p(2), stride_p(2));
    fillEndLen();
}

Slicer::Slicer (const Slice& x, const Slice& y,
		LengthOrLast lol)
: asEnd_p (lol),
  start_p (2, MimicSource),
  end_p   (2, MimicSource),
  stride_p(2, 1),
  len_p   (2, MimicSource)
{
    fillSlice (x, start_p(0), len_p(0), stride_p(0));
    fillSlice (y, start_p(1), len_p(1), stride_p(1));
    fillEndLen();
}

Slicer::Slicer (const Slice& x,
		LengthOrLast lol)
: asEnd_p (lol),
  start_p (1, MimicSource),
  end_p   (1, MimicSource),
  stride_p(1, 1),
  len_p   (1, MimicSource)
{
    fillSlice (x, start_p(0), len_p(0), stride_p(0));
    fillEndLen();
}


Slicer::Slicer (const Slicer& that)
: asEnd_p (that.asEnd_p),
  start_p (that.start_p),
  end_p   (that.end_p),
  stride_p(that.stride_p),
  len_p   (that.len_p),
  fixed_p (that.fixed_p)
{}

Slicer& Slicer::operator= (const Slicer& that)
{
    if (this != &that) {
	uInt nels = that.ndim();
	if (ndim() != nels) {
	    start_p.resize (nels);
	    end_p.resize (nels);
	    stride_p.resize (nels);
	    len_p.resize (nels);
	}
	asEnd_p  = that.asEnd_p;
	start_p  = that.start_p;
	end_p    = that.end_p;
	stride_p = that.stride_p;
	len_p    = that.len_p;
	fixed_p  = that.fixed_p;
    }
    return *this;
}


Bool Slicer::operator==(const Slicer& that) const
{
   return this->len_p.isEqual(that.len_p) &&
          this->start_p.isEqual(that.start_p) &&
          this->end_p.isEqual(that.end_p) &&
          this->stride_p.isEqual(that.stride_p) &&
          this->asEnd_p==that.asEnd_p &&
   	  this->fixed_p==that.fixed_p;
}


void Slicer::fillEndLen()
{
    //# First check if all positions have same length.
    if (start_p.nelements() != end_p.nelements()
    ||  start_p.nelements() != stride_p.nelements()) {
	throw (ArraySlicerError ("IPosition-lengths differ"));
    }
    //# Reversed strides are not allowed yet (thus stride>0).
    for (uInt i=0; i<start_p.nelements(); i++) {
        if (stride_p(i) <= 0) {
	    throw (ArraySlicerError ("stride<=0"));
	}else{
	    if (asEnd_p == endIsLast) {
		//# End is given; check if start<=end.
		//# Fill in length of result if start and end are given.
		len_p(i) = MimicSource;
		if (start_p(i) != MimicSource  &&  end_p(i) != MimicSource) {
		    len_p(i) = end_p(i) - start_p(i);         // input-length
		    if (len_p(i) < 0) {
			throw (ArraySlicerError ("end<start"));
		    }
		    len_p(i) = 1 + len_p(i) / stride_p(i);    // result-length
		}
	    }else{
		//# Length is given; check if >= 0.
		//# Fill in end if start and length are given.
		end_p(i) = MimicSource;
		if (len_p(i) != MimicSource) {
		    if (len_p(i) < 0) {
			throw (ArraySlicerError ("length<0"));
		    }
		    end_p(i) = start_p(i) + (len_p(i) - 1) * stride_p(i);
		}
	    }
	}
    }
    //# Set the fixed_p flag.
    fillFixed();
}

void Slicer::fillFixed()
{
    fixed_p = True;
    for (uInt i=0; i<start_p.nelements(); i++) {
	if (start_p(i) == MimicSource  ||  end_p(i) == MimicSource) {
	    fixed_p = False;
	    break;
	}
    }
}

void Slicer::fillSlice (const Slice& sl, ssize_t& start, ssize_t& len,
                        ssize_t& stride)
{
    //# Initialized values will do in case an "entire" slice is given.
    if (!sl.all()) {
	start  = sl.start();
	len    = sl.length();
	stride = sl.inc();
    }
}


IPosition Slicer::inferShapeFromSource (const IPosition& shp,
					IPosition& start,
					IPosition& end,
					IPosition& stride) const
{
    IPosition origin(shp.nelements(), 0);
    return inferShapeFromSource (shp, origin, start, end, stride);
}

IPosition Slicer::inferShapeFromSource (const IPosition& shp,
					const IPosition& ori,
					IPosition& start,
					IPosition& end,
					IPosition& stride) const
{
    //# Check if length of shape and origin conform the Slicer.
    if (shp.nelements() != start_p.nelements()
    ||  ori.nelements() != start_p.nelements()) {
	throw (ArraySlicerError
	               ("Shape/Origin IPosition-lengths differ from ndim()"));
    }
    //# Resize the output IPositions.
    //# Initialize them, so they will do for unspecified values.
    start.resize (start_p.nelements());
    end.resize (start_p.nelements());
    stride.resize (start_p.nelements());
    start  = 0;
    end    = shp - 1;
    stride = stride_p;
    IPosition res(start_p.nelements(), 0);
    for (uInt i=0; i<start_p.nelements(); i++) {
	//# Fill and check start value; unspecified means 0.
	if (start_p(i) != MimicSource) {
	    start(i) = start_p(i) - ori(i);
	}
	if (start(i) < 0) {
	    throw (ArraySlicerError ("infer: startResult<0"));
	}
	if (start(i) >= shp(i)) {
	    throw (ArraySlicerError ("infer: startResult>=shape"));
	}
	//# Fill end value.
	//# If given as end, unspecified is end of axis.
	//# If given as length, unspecified is also end of axis.
	if (asEnd_p == endIsLast) {
	    if (end_p(i) != MimicSource) {
		end(i) = end_p(i) - ori(i);
	    }
	}else{
	    if (len_p(i) != MimicSource) {
		end(i) = start(i) + len_p(i) * stride_p(i) - 1;
	    }
	}
	//# Get resulting shape and adjust and check end value.
	//# Length 0 is handled correctly.
	if (end(i) < start(i)) {
	    if (end(i) < start(i) - 1) {
		throw (ArraySlicerError ("infer: endResult<startResult-1"));
	    }
	}else{
	    res(i) = 1 + (end(i) - start(i)) / stride(i);
	    end(i) = start(i) + (res(i) - 1) * stride(i);
	}
	if (end(i) >= shp(i)) {
	    throw (ArraySlicerError ("infer: endResult>=shape"));
	}

    }
    return res;
}


ostream  &operator << (ostream &stream, const Slicer &slicer)
{
  
  stream << slicer.start () << " to " << slicer.end () 
         << " with stride " << slicer.stride () << ", length "
         << slicer.length ();

  return stream;
}

} //# NAMESPACE CASACORE - END

