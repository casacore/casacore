//# ProgressMeter.h: Visual indication of a tasks progress.
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
//#
//# $Id$

#include <trial/Tasking/ProgressMeter.h>
#include <aips/Utilities/String.h>

Int (*ProgressMeter::creation_function_p)(Double, Double, 
			      const String &, const String &,
                              const String &, const String &,
                              Bool) = 0;
void (*ProgressMeter::update_function_p)(Int, Double) = 0;

ProgressMeter::ProgressMeter()
    : id_p(-1), min_p(0.0), max_p(1.0), update_every_p(1), update_count_p(0)
{
}

ProgressMeter::ProgressMeter(Double min, Double max, 
			     const String &title, const String &subtitle,
			     const String &minlabel, const String &maxlabel,
			     Bool estimateTime, Int updateEvery)
    : id_p(-1), min_p(min), max_p(max), update_every_p(updateEvery), update_count_p(0)
{
    // Correct silently
    if (update_every_p <= 0) {
	update_every_p = 1;
    }
    if (creation_function_p) {
	id_p = creation_function_p(min, max, title, subtitle, minlabel, maxlabel,
				 estimateTime);
    }
}

ProgressMeter::~ProgressMeter()
{
    update_count_p++;
    update(max_p, True);
}

void ProgressMeter::update(Double value, Bool force)
{
    update_count_p++;
    // Always force the first one through
    if (update_count_p == 1) {
	force = True;
    }

    if (update_count_p == 1 || force || ((update_count_p%update_every_p) == 0)) {
	// Do the update if we have a "sink" and a valid id
	if (id_p > 0 && update_function_p) {
	    update_function_p(id_p, value);
	}	
    }
}

Double ProgressMeter::min() const
{
    return min_p;
}

Double ProgressMeter::max() const
{
    return max_p;
}

void ProgressMeter::cleanup()
{
    this->ProgressMeter::~ProgressMeter();
}
