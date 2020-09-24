//# AppState.h: casacore library configuration without environment variabes
//# Copyright (C) 2017
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

#ifndef CASA_APPSTATE_H
#define CASA_APPSTATE_H
#include <string>
#include <list>
#include <casacore/casa/aips.h>
#include <casacore/casa/OS/Mutex.h>

namespace casacore {

// <summary>
// Base class for application state
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <synopsis>
// This class is the base class for casacore state. Its purpose is to
// allow applications initialize casacore's state without resorting to
// environment variables. This is done by creating an object whose
// class is derived from this base class, and then initializing the
// AppStateSource with the newly created object. After initialization,
// the AppStateSource takes ownership of the object. Please see the
// documentation for AppStateSource for more information.
// </synopsis>

class AppState {
public:

    // use the data path to find the filename...
    virtual std::string resolve(const std::string &filename) const;

    // get the list of directories in the data path...
    virtual std::list<std::string> dataPath( ) const {
        static std::list<std::string> result;
        return result;
    }

    // get directory containing IERS measures data
    // an exception is thrown if it does not exist or
    // does not contain the IERS tables
    virtual std::string measuresDir( ) const {
        static std::string result;
        return result;
    }

    virtual bool initialized( ) const { return false; }

    virtual ~AppState( ) { }
};

// <summary>
// Allow configuration of casacore without environment variables
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <synopsis>
// This class allows packages which use casacore to configure casacore
// behavior without reverting to environment variables. It is composed
// primarly of static functions. An external application configures
// casacore by calling the initialize(...) member function passing in
// a pointer to an object which is derived from the AppState base class.
// AppStateSource takes ownership of the provided pointer.
//
// When casacore no longer depends on compilers whose standard is older
// than C++11, the raw pointers here should be changed to
// unique_ptrs. The std::unique_ptr constructor is a constexpr, and it
// does not throw exceptions.
// </synopsis>

// <example>
// class MyState: public casacore::AppState {
//     public:
//         MyState( ) { }
//  
//         const std::list<std::string> &dataPath( ) const {
//             static std::list<std::string> my_path;
//             return my_path;
//         }
//
//         bool initialized( ) const { return true; }
// };
//
// MyState &get_my_state( ) {
//     if ( AppStateSource::fetch( ).initialized( ) == false )
//         casacore::AppStateSource::initialize( new MyState );
//     return dynamic_cast<MyState&>(AppStateSource::fetch( ));
// }
//
// int main( int argc, char *argv[] ) {
//     MyState &state = get_my_state( );
//     ...
//     return 0;
// }
// </example>
class AppStateSource {
public:

    static void initialize(AppState *init) {
        static Mutex mutex_p;
        ScopedMutexLock lock(mutex_p);
        if ( user_state ) delete user_state;
        user_state = init;
    }
    static AppState &fetch( ) {
        static AppState default_result;
        return user_state ? *user_state : default_result;
    }

private:
    static AppState *user_state;
    AppStateSource( ) { }
    AppStateSource( AppStateSource const &) { }      // prevent copying
    void operator=(AppStateSource const &) { }       // prevent assignment
};

} //# NAMESPACE CASACORE - END

#endif
