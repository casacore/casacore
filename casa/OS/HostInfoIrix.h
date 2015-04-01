//# HostInfo_irix.h: SGI Irix specific memory, swap, and CPU code.
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

#ifndef CASA_HOSTINFOIRIX_H
#define CASA_HOSTINFOIRIX_H

# if defined(HOSTINFO_DO_IMPLEMENT)

/*
 *          AUTHOR:       Darrell Schiebel  <drs@nrao.edu>
 *
 * ORIGINAL AUTHORS:      Sandeep Cariapa   <cariapa@sgi.com>
 *                        Larry McVoy       <lm@sgi.com>
 *                        John Schimmel     <jes@sgi.com>
 *                        Ariel Faigon      <ariel@sgi.com>
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/swap.h>
#include <sys/sysmp.h>
#include <sys/sysinfo.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// HostInfo for IRIX machines.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=HostInfo>HostInfo</linkto>
// </prerequisite>

// <synopsis> 
// This file provides the IRIX specific functions for HostInfo.
// It is selectively included by HostInfo.cc.
// </synopsis>
//
// <group name="HostInfo">

#define pagetok(pages) ((((uint64_t) pages) * pagesize) >> 10)

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

    ptrdiff_t pagesize;

};

// </group>


HostMachineInfo::HostMachineInfo( )  : valid(1) {

	pagesize = getpagesize();

	if ((cpus = sysmp(MP_NPROCS)) == -1) {
		perror("sysmp(MP_NPROCS)");
		valid = 0;
		return;
	}

	struct rminfo	realmem;
	if (sysmp(MP_SAGET, MPSA_RMINFO, &realmem, sizeof(realmem)) == -1) {
		perror("sysmp(MP_SAGET,MPSA_RMINFO, ...)");
		valid = 0;
		return;
	}

	memory_total = pagetok(realmem.physmem);
}

void HostMachineInfo::update_info( ) {
	int		i;
	struct rminfo	realmem;
	struct sysinfo	sysinfo;
	off_t		fswap;		/* current free swap in blocks */
	off_t		tswap;		/* total swap in blocks */

	swapctl(SC_GETFREESWAP, &fswap);
	swapctl(SC_GETSWAPTOT, &tswap);

	if (sysmp(MP_SAGET, MPSA_RMINFO, &realmem, sizeof(realmem)) == -1) {
		perror("sysmp(MP_SAGET,MPSA_RMINFO, ...)");
		valid = 0;
		return;
	}

	memory_free = pagetok(realmem.freemem);
	memory_used =  memory_total - memory_free;
	swap_total = tswap / 2;
	swap_free = fswap / 2;
	swap_used = swap_total - swap_free;
}

# endif

} //# NAMESPACE CASACORE - END

#endif
