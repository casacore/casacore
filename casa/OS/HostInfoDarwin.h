/*
**  This is a greatly MODIFIED version of a "top" machine dependent file.
**  The only resemblance it bears to the original is with respect to the
**  mechanics of finding various system details. The copyright details
**  follow.
**
**  This is a modified version of the osf1 version. Initially, I tried
**  to use the m_macosx.c version by Andrew S. Townley, but ran into
**  problems...
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
** LIBS: -lstdc++
**
**          AUTHOR:       Darrell Schiebel  <drs@nrao.edu>
**
** ORIGINAL AUTHOR:       Anthony Baxter    <anthony@aaii.oz.au>
** ORIGINAL CONTRIBUTORS: David S. Comay    <dsc@seismo.css.gov>
**                        Claus Kalle
**                        Pat Welch         <tpw@physics.orst.edu>
**                        William LeFebvre  <lefebvre@dis.anl.gov>
**                        Rainer Orth       <ro@techfak.uni-bielefeld.de>
**
*/
//# $Id$

#ifndef CASA_HOSTINFODARWIN_H
#define CASA_HOSTINFODARWIN_H

# if defined(HOSTINFO_DO_IMPLEMENT)


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <mach/mach.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// HostInfo for Darwin machines.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=HostInfo>HostInfo</linkto>
// </prerequisite>

// <synopsis> 
// This file provides the Linux specific functions for HostInfo.
// It is selectively included by HostInfo.cc.
// </synopsis>
//
// <group name="HostInfo">

/* Log base 2 of 1024 is 10 (2^10 == 1024) */
#define LOG1024		10

/* these are for getting the memory statistics */
static int pageshift;		/* log base 2 of the pagesize */
static int pagesize_;

/* define pagetok in terms of pageshift */
#define pagetok(size) ((size) << pageshift)

class HostMachineInfo {
friend class HostInfo;
  
    HostMachineInfo( );
    void update_info( );

    int valid;
    int cpus;

    ptrdiff_t memory_total;
    ptrdiff_t memory_used;
    ptrdiff_t memory_free;

    ptrdiff_t swap_total;
    ptrdiff_t swap_used;
    ptrdiff_t swap_free;
};

// </group>


HostMachineInfo::HostMachineInfo( ) : valid(1) {
    int pagesize;

    kern_return_t ret;
    struct host_basic_info basic_info;
    unsigned int count = HOST_BASIC_INFO_COUNT;

    /* get the page size with "getpagesize" and calculate pageshift from it */
    pagesize_ = pagesize = getpagesize();
    pageshift = 0;
    while (pagesize > 1)
    {
	pageshift++;
	pagesize >>= 1;
    }

    /* we only need the amount of log(2)1024 for our conversion */
    pageshift -= LOG1024;

#ifdef AIPS_64B
       ret = host_info( mach_host_self(), HOST_BASIC_INFO, (host_info64_t) &basic_info, &count );
#else
       ret = host_info( mach_host_self(), HOST_BASIC_INFO, (host_info_t) &basic_info, &count );
#endif
    if ( ret != KERN_SUCCESS ) {
	valid = 0;
    } else {
#ifdef AIPS_64B
	memory_total = basic_info.max_mem / 1024;
#else
	memory_total = basic_info.memory_size / 1024;
#endif
	cpus = basic_info.avail_cpus;
    }
}

void HostMachineInfo::update_info( ) {

#ifdef AIPS_64B
    struct vm_statistics64 vmstats;
#else
    struct vm_statistics vmstats;
#endif
    kern_return_t kr;
    unsigned int count;

    /* memory information */
    /* this is possibly bogus - we work out total # pages by */
    /* adding up the free, active, inactive, wired down, and */
    /* zero filled. Anyone who knows a better way, TELL ME!  */
    /* Change: dont use zero filled. */
    count = sizeof(vmstats)/sizeof(integer_t);

#ifdef AIPS_64B
       kr = host_statistics64( mach_host_self(), HOST_VM_INFO64, (host_info64_t) &vmstats, &count );
#else
       kr = host_statistics( mach_host_self(), HOST_VM_INFO, (host_info_t) &vmstats, &count );
#endif
    if ( kr != KERN_SUCCESS ) {
      valid = 0;
      return;
    }

//     /* thanks DEC for the table() command. No thanks at all for   */
//     /* omitting the man page for it from OSF/1 1.2, and failing   */
//     /* to document SWAPINFO in the 1.3 man page. Lets hear it for */
//     /* include files. */
//     i=0;
//     while(table(TBL_SWAPINFO,i,&swbuf,1,sizeof(struct tbl_swapinfo))>0) {
// 	swappages += swbuf.size;
// 	swapfree  += swbuf.free;
// 	i++;
//     }
// 
//     swap_used = pagetok(swappages - swapfree);
//     swap_free = pagetok(swapfree);
//     swap_total = pagetok(swappages);

    memory_used = pagetok(vmstats.active_count + vmstats.wire_count);
    memory_free = memory_total - memory_used;
    swap_used = pagetok( vmstats.active_count + vmstats.inactive_count + vmstats.wire_count );
    swap_free = pagetok( vmstats.free_count );
    swap_total = pagetok( vmstats.active_count + vmstats.inactive_count +
			  vmstats.wire_count + vmstats.free_count );
}


} //# NAMESPACE CASACORE - END

# endif
#endif
