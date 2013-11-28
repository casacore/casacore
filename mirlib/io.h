/************************************************************************/
/*									*/
/*	A general header file for the various file and i/o handling	*/
/*	routines.							*/
/*									*/
/*  History:								*/
/*   rjs  Dark-ages Original version.					*/
/*   rjs  20aug92   Correct "roundup" macro when rounding 0.		*/
/*   rjs  15may96   Moved roundup macro elsewhere.			*/
/************************************************************************/

/* Binary items start with a sequence to allow routines to blindly determine
   how to read them. The "binary_item" is a catch all with only indicates
   that the data is binary valued, but does not hint at the format. */

#include "hio.h"
#include <unistd.h>

#define ITEM_HDR_SIZE		4
extern char 	binary_item[ITEM_HDR_SIZE],
		real_item[ITEM_HDR_SIZE],
		int_item[ITEM_HDR_SIZE]	,
		int2_item[ITEM_HDR_SIZE],
		char_item[ITEM_HDR_SIZE],
		dble_item[ITEM_HDR_SIZE],
		cmplx_item[ITEM_HDR_SIZE];
