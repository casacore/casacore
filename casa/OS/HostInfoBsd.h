/*
** Author Tony Maher
**
** Based on HostInfoDarwin.h (just the scaffolding code).
**
** $Id$
**
**  Copyright (c) 2009, Associated Universities Inc.
*/

#ifndef CASA_HOSTINFOBSD_H
#define CASA_HOSTINFOBSD_H

# if defined(HOSTINFO_DO_IMPLEMENT)

#include <sys/types.h>
#include <sys/sysctl.h>
#include <sys/vmmeter.h>    // struct vmtotal

#include <fcntl.h>
#include <kvm.h>
#include <paths.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// HostInfo for BSD (FreeBSD) machines.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=HostInfo>HostInfo</linkto>
// </prerequisite>

// <synopsis> 
// This file provides the BSD (FreeBSD) specific functions for HostInfo.
// It is selectively included by HostInfo.cc.
// </synopsis>
//
// <group name="HostInfo">

// these are for getting the memory statistics
static int pagesize;
static int page_kb;

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
    size_t len;

    pagesize = getpagesize();   // size of page in bytes.
    page_kb = pagesize / 1024;  // in kilobytes.

    len = sizeof(cpus);
    if (sysctlbyname("hw.ncpu", &cpus, &len, NULL, 0) == -1)
        perror("sysctl");

    len = sizeof(memory_total);
    if (sysctlbyname("hw.physmem", &memory_total, &len, NULL, 0) == -1)
        perror("sysctl");
    else
        memory_total /= 1024; // in kilobytes
}


void HostMachineInfo::update_info( ) {
    size_t len;
    kvm_t *kd;
    struct vmtotal total;
    struct kvm_swap swapary[1];

    // Memory and swap values are in pages.

    len = sizeof(total);
    if (sysctlbyname("vm.vmtotal", &total, &len, NULL, 0) == -1)
        perror("sysctl");
    else
        memory_used = total.t_rm * page_kb;
        memory_free = total.t_free * page_kb;

    kd = kvm_open(NULL, _PATH_DEVNULL, NULL, O_RDONLY, "kvm_open");
    if (kd != NULL) {
        kvm_getswapinfo(kd, swapary, 1, 0);

        swap_total = swapary[0].ksw_total * page_kb;
        swap_used  = swapary[0].ksw_used * page_kb;
        swap_free  = swap_total - swap_used;
        }
}


} //# NAMESPACE CASACORE - END

# endif
#endif
