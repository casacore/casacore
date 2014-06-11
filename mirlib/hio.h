#if !defined(MIR_HIO_H)
#define MIR_HIO_H

#include "sysdep.h"

/* 
 * magic numbers at the start of an item, these are like BITPIX in fits, 
 * so don't change them or your MIRIAD files won't be exchangeable between
 * other MIRIAD implementations
 * MAXTYPES is pretty arbitrary, just make sure it's at least the last H_<type>+1
 *
 */

#define H_BYTE		1
#define H_INT		2
#define H_INT2		3
#define H_REAL		4
#define H_DBLE		5
#define H_TXT		6
#define H_CMPLX		7
#define H_INT8          8

#define MAXTYPES       10

#define H_BYTE_SIZE	1
#define H_INT_SIZE	4
#define H_INT2_SIZE	2
#define H_INT8_SIZE	8
#define H_REAL_SIZE	4
#define H_DBLE_SIZE	8
#define H_TXT_SIZE	1
#define H_CMPLX_SIZE	8

#define MAXPATH		256
#define MAXOPEN		26


/* prototypes are now in miriad.h (mostly) and sysdep.h (pack routines)  */

/* Other handy definitions. */

#define TRUE 			1
#define FALSE			0
#define max(a,b) 	((a)>(b)?(a):(b))
#define min(a,b) 	((a)<(b)?(a):(b))
#define mroundup(a,b)	((b)*(((a)+(b)-1)/(b)))

#endif /* MIR_HIO_H */
