//# ProgressMeter.h: Visual indication of a tasks progress.
//# Copyright (C) 1997,2000
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
#include <aips/Containers/Block.h>
#include <iostream.h>

// First implement a simple stderr based progress meter that just prints out
// 0%....10....20....30....40....50....60....70....80....90....100%
// cerr is better than cout because it isn't buffered usually, so the above
// will come out right away. Also, one often wants to direct "real" output
// to a file, but see informative messages on the screen.

// If we have lots and lots of progress meters we should figure out
// a way to reclaim the following storage.
static Block<Double> stderr_min, stderr_max, stderr_last;
static Int stderr_creation_function(Double min, Double max,
				    const String &, const String &,
				    const String &, const String &,
				    Bool)
{
    Int n = stderr_min.nelements() + 1;
    stderr_min.resize(n);
    stderr_max.resize(n);
    stderr_last.resize(n);
    stderr_min[n-1] = min;
    stderr_max[n-1] = max;
    stderr_last[n-1] = min;
    cerr << "\n0%";
    return n;
}

static void stderr_update_function(Int id, Double value)
{
    if (id < 0 || id > Int(stderr_min.nelements())) {
	cerr << __FILE__ << " illegal id " << id << endl;
	return;
    }
    id--; // 0-relative
    Int percent     = Int((value - stderr_min[id]) / 
			  (stderr_max[id] - stderr_min[id]) * 100.0);
    Int lastpercent = Int((stderr_last[id] - stderr_min[id]) / 
			  (stderr_max[id] - stderr_min[id]) * 100.0);
    if (percent > lastpercent) {
	stderr_last[id] = value;
	// Probably we could do this more efficiently. We need to get all the
	// "missing" ..'s etc if we have jumped a lot since our last updated.
	for (Int i=lastpercent+1; i<=percent; i++) {
	    if (i%2 == 0 && i%10 != 0) {
		cerr << ".";
	    } else if (i %10 == 0) {
		cerr << i;
		if (i >= 100) {
		    cerr << "%\n";
		}
	    }
	}	
    }
    
}

Int (*ProgressMeter::creation_function_p)(Double, Double, 
			      const String &, const String &,
                              const String &, const String &,
                              Bool) = stderr_creation_function;
void (*ProgressMeter::update_function_p)(Int, Double) = stderr_update_function;

ProgressMeter::ProgressMeter()
    : id_p(-1), min_p(0.0), max_p(1.0), update_every_p(1), update_count_p(0)
{
}

ProgressMeter::ProgressMeter(Double min, Double max, 
			     const String &title, const String &subtitle,
			     const String &minlabel, const String &maxlabel,
			     Bool estimateTime, Int updateEvery)
    : id_p(-1), min_p(min), max_p(max), update_every_p(updateEvery),
      update_count_p(0)
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

    if (update_count_p == 1 || force || ((update_count_p%update_every_p) == 0))
	{
	    // Do the update if we have a "sink" and a valid id
	    if (id_p > 0 && update_function_p) {
		update_function_p(id_p, value);
	    } else {
		// If we have more than one progress meter active at once
		// this might look pretty confusing. We can decide what to
		// do if that ever actually happens.
		
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
