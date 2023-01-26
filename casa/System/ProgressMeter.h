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

#ifndef CASA_PROGRESSMETER_H
#define CASA_PROGRESSMETER_H

//# Includes
#include <casacore/casa/aips.h>
#include <time.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class String;

// <summary>
// Visual indication of a tasks progress.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <synopsis>
// This class is used to provide a visual indication to the user of the progress
// of his task. If the process is not connected to the DO system, calls to the
// progress meter are NO-OP's, so you can safely use this class in general
// library code and it won't cause problems for processes which are not
// attached to the distributed object system. It also won't cause any extra
// stuff to be linked in to your executable in this case.
//
// The progress meter will usually be removed from the screen once the maximum
// value is set, so you should not reuse the ProgressMeter after that has
// happened. It is harmless, but it will not result in any visual feedback for
// the user.
//
// While the "min" is usually less than "max", if in fact it is greater than
// "max" the progress meter will count down correctly.
// </synopsis>
//
// <example>
// <srcblock>
// void calculate(uint32_t n) {
//   int32_t skip = n / 200;
//   ProgressMeter meter(0, n, "Title", "Subtitle", "", "", true, skip);
//   for (uint32_t i=0; i<n; i++) {
//       ... calculate ...
//       meter.update(i);
//   }
// }
// </srcblock>
// </example>
//
// <motivation>
// Give the user visual indication of a long-running tasks progress.
// </motivation>
//
// <todo asof="1997/03/03">
//   <li> When the upper bound isn't known, it might be useful to have a busy
//        bar that just moves back and forth to show that activity is happening.
//   <li> We should probably have some way to suppress progress bars for tasks
//        that are only going to take a few seconds.
// </todo>

class ProgressMeter
{
public:
    // Makes a null progress meter, i.e. no updates to the screen are
    // generated.
    ProgressMeter();

    // Create a progress meter with the given min and max values and labels.
    // if <src>estimateTime</src> is <src>true</src>, an estimate of the
    // time remaining will be made for the user. This estimate assumes that
    // the remaining portion will compute at the same rate as the portion
    // completed so far, so the time should not be estimated for processes
    // which are non-linear.
    //
    // Any labels which are set to the empty string will have sensible defaults
    // supplied. In particular, <src>minlabel</src> and <src>maxlabel</src>
    // will be set to the display the minimum and maximum values.
    //
    // Normally the progress bar will be updated with every call to
    // <src>update()</src>. If however you will be sending many events
    // then you might want to update the GUI every <src>updateEvery</src>'th
    // event for efficiency. Generally there's no point updating more than
    // a couple of hundred times since the eye can't distinguish differences
    // in the progress bar position at that level. If updateEvery is <=0, it
    // is set to 1 for you.
    ProgressMeter(double min, double max, 
		  const String &title, const String &subtitle,
		  const String &minlabel, const String &maxlabel,
		  bool estimateTime = true, int32_t updateEvery=1);

    ProgressMeter(double min, double max, const String &title);
    // The destruction of the meter will cause an update to be sent with the
    // maximum value. This will usually cause the GUI window to be removed
    // from the screen. Thus the progress meter should generally live as long
    // as the calculation it is tracking.
    ~ProgressMeter();

    void update(double value, bool force=false);
    void _update(double value, bool force=false);
    void busy();
    void done();

    // Display the min and max values of the progress meter.
    // <group>
    double min() const;
    double max() const;
    // </group>

    friend class ObjectController;
    static const char* PROGRESSFILE;
private:
    int32_t id_p;
    double min_p, max_p;
    int32_t update_every_p, update_count_p;
       // Time the progress meter began
    time_t startTime; 
    bool   showProgress;
    
    // These are set by ObjectController for executables that have the tasking
    // system in them, otherwise they are null and this class just does no-ops.
    static int32_t (*creation_function_p)(double, double, 
			      const String &, const String &,
                              const String &, const String &,
                              bool);
    static void (*update_function_p)(int32_t, double);
    static void (*show_function_p)(int32_t, double);
    static void (*busy_function_p)(int32_t);
    static void (*done_function_p)(int32_t);

    // Undefined and inaccessible
    ProgressMeter(const ProgressMeter &);
    ProgressMeter &operator=(const ProgressMeter &);
};


} //# NAMESPACE CASACORE - END

#endif


