//# HostInfo_osf1.h: DEC OSF/1 specific memory, swap, and CPU code.
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

/*
 * LIBS: -lmach -lpset
 *
 *          AUTHOR:       Darrell Schiebel  <drs@nrao.edu>
 *
 * ORIGINAL AUTHOR:       Anthony Baxter    <anthony@aaii.oz.au>
 * ORIGINAL CONTRIBUTORS: David S. Comay    <dsc@seismo.css.gov>
 *                        Claus Kalle
 *                        Pat Welch         <tpw@physics.orst.edu>
 *                        William LeFebvre  <lefebvre@dis.anl.gov>
 *                        Rainer Orth       <ro@techfak.uni-bielefeld.de>
 */

#ifndef CASA_HOSTINFOOSF1_H
#define CASA_HOSTINFOOSF1_H

# if defined(HOSTINFO_DO_IMPLEMENT)

//
//--> /usr/include/mach/mach_interface.h:297:                                                        <-
//--> previous declaration of `int vm_statistics(long int, struct vm_statistics *)' with C++ linkage <-
//
// so, we hack this by defining _mach to kick <mach/mach_interface.h> out of inclusion,
// and then below we make this extern "C"... likely this is fixed in some version of
// DEC OSF/1 which is newer than what we have...
//
#define _mach

#include <stdio.h>

#include <unistd.h>
#include <mach.h>
#include <mach/mach_types.h>
#include <mach/vm_statistics.h>
#include <mach/host_info.h>
#include <sys/table.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// HostInfo for OSF1 machines.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=HostInfo>HostInfo</linkto>
// </prerequisite>

// <synopsis> 
// This file provides the OSF1 specific functions for HostInfo.
// It is selectively included by HostInfo.cc.
// </synopsis>
//
// <group name="HostInfo">

extern "C" kern_return_t host_info(int, int, host_info_t, unsigned int *);
extern "C" int host_self( );
extern "C" int vm_statistics(long, struct vm_statistics *);

/* Log base 2 of 1024 is 10 (2^10 == 1024) */
#define LOG1024		10

/* these are for getting the memory statistics */
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


HostMachineInfo::HostMachineInfo( ) : valid(1) {

    int i = 0;
    int pagesize;
    struct tbl_sysinfo sibuf;

    kern_return_t ret;
    struct host_basic_info basic_info;
    unsigned int count = HOST_BASIC_INFO_COUNT;

    /* get the page size with "getpagesize" and calculate pageshift from it */
    pagesize = getpagesize();
    pageshift = 0;
    while (pagesize > 1)
    {
	pageshift++;
	pagesize >>= 1;
    }

    /* we only need the amount of log(2)1024 for our conversion */
    pageshift -= LOG1024;

    ret = host_info( host_self(), HOST_BASIC_INFO, (host_info_t) &basic_info, &count );
    if ( ret != KERN_SUCCESS ) {
	valid = 0;
    } else {
	memory_total = basic_info.memory_size / 1024;
	cpus = basic_info.avail_cpus;
    }
}

void HostMachineInfo::update_info( ) {

    struct tbl_swapinfo swbuf;
    vm_statistics_data_t vmstats;
    int swappages=0,swapfree=0,i;

    /* memory information */
    /* this is possibly bogus - we work out total # pages by */
    /* adding up the free, active, inactive, wired down, and */
    /* zero filled. Anyone who knows a better way, TELL ME!  */
    /* Change: dont use zero filled. */
    (void) vm_statistics(task_self(),&vmstats);

    /* thanks DEC for the table() command. No thanks at all for   */
    /* omitting the man page for it from OSF/1 1.2, and failing   */
    /* to document SWAPINFO in the 1.3 man page. Lets hear it for */
    /* include files. */
    i=0;
    while(table(TBL_SWAPINFO,i,&swbuf,1,sizeof(struct tbl_swapinfo))>0) {
	swappages += swbuf.size;
	swapfree  += swbuf.free;
	i++;
    }

    swap_total = pagetok(swappages);
    swap_used = pagetok(swappages - swapfree);
    swap_free = pagetok(swapfree);

    memory_free = pagetok(vmstats.free_count);
    memory_used = memory_total - memory_free;
//  some memory is left unaccounted for, using the following...
//  memory_used = pagetok(vmstats.active_count + vmstats.inactive_count + vmstats.wire_count);
}

# endif

} //# NAMESPACE CASACORE - END

#endif
