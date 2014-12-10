//# HostInfo_hpux.h: HP/UX specific memory, swap, and CPU code.
//# $Id$

 /*
 **  This is a greatly MODIFIED version of a "top" machine dependent file.
 **  The only resemblance it bears to the original is with respect to the
 **  mechanics of finding various system details. The copyright details
 **  follow.
 **
 **  --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 **
 **  Top users/processes display for Unix
 **  Version 3
 **
 **  This program may be freely redistributed,
 **  but this entire comment MUST remain intact.
 **
 **  Copyright (c) 1984, 1989, William LeFebvre, Rice University
 **  Copyright (c) 1989 - 1994, William LeFebvre, Northwestern University
 **  Copyright (c) 1994, 1995, William LeFebvre, Argonne National Laboratory
 **  Copyright (c) 1996, William LeFebvre, Group sys Consulting
 **  Copyright (c) 2002, Associated Universities Inc.
 */

#ifndef CASA_HOSTINFOHPUX_H
#define CASA_HOSTINFOHPUX_H

# if defined(HOSTINFO_DO_IMPLEMENT)

/*
 *          AUTHOR:       Darrell Schiebel  <drs@nrao.edu>
 *
 * ORIGINAL AUTHOR:       John Haxby        <john_haxby@hp.com>
 * ORIGINAL CONTRIBUTORS: Rich Holland      <holland@synopsys.com>
 *                        Kevin Schmidt     <kevin@mcl.ucsb.edu> 
 */

#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/param.h>
#include <sys/pstat.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// HostInfo for HP-UX machines.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=HostInfo>HostInfo</linkto>
// </prerequisite>

// <synopsis> 
// This file provides the HP-UX specific functions for HostInfo.
// It is selectively included by HostInfo.cc.
// </synopsis>
//
// <group name="HostInfo">

/* these are for getting the memory statistics */

/* Log base 2 of 1024 is 10 (2^10 == 1024) */
#define LOG1024		10

/* define pagetok in terms of pageshift */
#define pagetok(size) ((size) << pageshift)

class HostMachineInfo {
friend class HostInfo;

    HostMachineInfo( );
    void update_info( );

    int valid;
    int cpus;

    ptrdiff_t swap_total;
    ptrdiff_t swap_used;
    ptrdiff_t swap_free;

    ptrdiff_t memory_total;
    ptrdiff_t memory_used;
    ptrdiff_t memory_free;

    int pageshift;		/* log base 2 of the pagesize */
};

// </group>


HostMachineInfo::HostMachineInfo( ) :valid(1) {

    struct pst_static info;
    int pagesize;

    if (pstat_getstatic (&info, sizeof (info), 1, 0) < 0) {
	perror ("pstat_getstatic");
	valid = 0;
    }

    /*
     * Calculate pageshift -- the value needed to convert pages to Kbytes.
     * This will usually be 2.
     */
    pageshift = 0;
    for (pagesize = info.page_size; pagesize > 1; pagesize >>= 1)
	pageshift += 1;
    pageshift -= LOG1024;

    static struct pst_dynamic dynamic;

    pstat_getdynamic (&dynamic, sizeof (dynamic), 1, 0);
    cpus = dynamic.psd_proc_cnt;
    memory_total = pagetok (dynamic.psd_rm);
}

void HostMachineInfo::update_info( ) {

    static struct pst_dynamic dynamic;

    pstat_getdynamic (&dynamic, sizeof (dynamic), 1, 0);
    memory_used = pagetok (dynamic.psd_arm);
    memory_free = memory_total - memory_used;
    swap_total = pagetok (dynamic.psd_vm);
    swap_used = pagetok (dynamic.psd_avm);
    swap_free = swap_total - swap_used;
}

# endif

} //# NAMESPACE CASACORE - END

#endif
