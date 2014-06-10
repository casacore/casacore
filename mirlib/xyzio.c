/*******************************************************************************

 Routines to read and write an image dataset

 History:

     bpw  19-apr-91  Created
     bpw  01-may-91  Algorithm ready
     bpw  03-may-91  Ready
     bpw  19-may-91  Add reverse
     bpw  20-may-91  Include dummy masking scheme
     bpw  21-jun-91  Installed
     bpw  25-jun-91  Created get_buflen function
     bpw  27-jun-91  Moved FORTRAN-C conversion to xyziowrap.h
     pjt/mjs 28jul91 Renamed internal variable "max" to "themax" to eliminate
                     conflict with max function.
     bpw  29-jul-91  Got rid of themax, and made it into sizeof
     bpw  09-aug-91  Added '-start' to bufend in zero(2)
     bpw  08-sep-92  Made ndata indeed output variable for xyzread
     rjs  22-dec-92  Delete inclusion of xyziowrap.h in xyzio.h
     rjs  23-feb-93  Include maxdimc.h, which includes definition of MAXNAX
                     and MAXBUF. Use MAXBUF. Get rid of xyzio.h
     bpw   2-mar-93  Add real masking
     bpw   9-jul-93  Added xyzflush_c and xyzmkbuf_c, and changed buffer
                     allocation scheme to avoid unnecessary allocations
     bpw  27-jul-93  Fixed allocation bug introduced in previous update
                     (problems for 1-plane datasets)
     rjs   4-sep-94  Change "word" to "words" to satisfy Cray compiler.
     rjs   6-nov-94  Change item handle to an integer.
     bpw   8-dec-94  Adapt two loop in bufferalloc for the fact that since
                     6 nov image handles are no longer in sequence.
     bpw  12-feb-96  follow rjs to eliminate nested comments (without using
                     tabs)
     bpw  12-jun-98  for zero(1,tno) set whole cube mask to FALSE
     pjt  16-nov-00  Fixed a pretty serious problem of bpw mixing up | w/ ||
     	             and & w/ &&.
                     Also forced initialized of mask due, there were some
                     side-effects here too if no mask was present
                     (note xyzio writes a mask, even if full mask ok)
     pjt  11-jun-01  added rjs' 10-jan-96 code changes that seemed lost
		     "Correct comparision bug in bufferallocation routine." 


*******************************************************************************/

/******************************************************************************/
/*                                                                            */
/* Declarations                                                               */
/*                                                                            */
/******************************************************************************/

#include "maxdimc.h"
#include "io.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
void    bug_c(char, char *);
#define check(x) if(x)bugno_c('f',x)


/* There is only one buffer array, of a length determined at run-time by
   get_buflen.
   buffersize is size of the virtual buffer for a particular image,
   which varies with the number of images handled */
int     get_buflen(), buffersize;
int     allocatebuffer, currentallocation=0, neverfree=FALSE;
float  *buffer = NULL;
int    *mbuffr = NULL;


/* Most of the code for reading and writing is exactly the same. Where a
   difference exists MODE (values GET and PUT) is used to discriminate.
   UP and DOWN are used for copying or reverse copying.
   ALL is used to see if all axes are reversed. */
int     MODE;
#define GET   0
#define PUT   1
#define UP    1
#define DOWN  2
#define ALL   2


/* MAXNAX: maximum number of axes that this can handle.
   ARRSIZ = MAXNAX+1, so that element 1<->x, 2<->y, etc */
#define ARRSIZ MAXNAX+1


/* imgs: dataset info; bufs: buffers info
   .itno: image handle for hio routines
   .number: counter of how many datasets were opened before
   .naxis, .axlen, .cubesize, .blc, .trc: you know
   .lower, .upper: lower and upper coordval of elements currently in buffer
   .filfir, .fillas: first and last element from cube currently in buffer
   .bufstart: abbreviation for -.filfir+tno*buffersize
   .lastwritten: to see if old data must be read in before writing buffer
   .nocopy: true if no transposition or region present
   axnum: relation between axes: i-th axis used to be axis axnum(i)
   reverse: tells if output data array must be reversed
   written: to see if buffer must be flushed on a close or new read to same
   newbuffer: to allow a check if xyzsetup is called more often for a dataset
   ntno: number of datasets currently opened
*/
static  struct Img { int itno; char *mask; int number;
                 int naxis, axlen[ARRSIZ], cubesize[ARRSIZ];
                 int blc[ARRSIZ], trc[ARRSIZ];
                 int lower[ARRSIZ], upper[ARRSIZ];
                 int filfir, fillas, bufstart;
                 int lastwritten, nocopy; }
        imgs[MAXOPEN], bufs[MAXOPEN];
int     axnum[MAXOPEN][ARRSIZ];
int     reverse[MAXOPEN][ARRSIZ];
int     written[MAXOPEN];
int     ntno = 0;

/* loop variables (dim and d) and dimension of subcube */
int     dim, d, dimsub[MAXOPEN];

/* arrays used to limit number of pointer calculations inside big loop
   in loop_inpbuffer (i.e. remove index [tno]) and to improve readability
   of the code */
int     naxes;
int     imgsblc[ARRSIZ],   imgstrc[ARRSIZ];
int     imgslower[ARRSIZ], imgsupper[ARRSIZ];
int     imgsaxlen[ARRSIZ], imgscubesize[ARRSIZ], imgscsz[ARRSIZ];
int     bufsblc[ARRSIZ],   bufstrc[ARRSIZ];
int     bufsaxlen[ARRSIZ], bufscubesize[ARRSIZ], bufscsz[ARRSIZ];
int     axnumr[ARRSIZ],    inv_axnumr[ARRSIZ],   reverses[ARRSIZ];


/* Some variables not used, but left in for the (hopefully never occuring)
   case that an error occurred and debugging is needed.
   Most if(.test) statements have been left active. Some, the ones in inner
   loops, are disabled. They can be found by searching for */
   /*$$ */

int     itest = 0; /* Information on buffers and datasets */
int     otest = 0; /* Information on subcubes */
int     rtest = 0; /* Information on each array element */
int     vtest = 0; /* Puts numbers in buffer without reading a dataset */
int     tcoo[ARRSIZ];
int     nfound, i;
char   *words[4] = { "get", "put", "filled", "emptied" };
int     nio=0;

void get_test(interactive)
int interactive;
{
    if(interactive)printf("iTest >"); scanf("%d",&itest);
    if(interactive)printf("rTest >"); scanf("%d",&rtest);
    if(interactive)printf("oTest >"); scanf("%d",&otest);
    if(interactive)printf("vTest >"); scanf("%d",&vtest);
}
int putnio(x) int x; {x++; return nio;}


/******************************************************************************/
/*                                                                            */
/* The FORTRAN-callable routines                                              */
/*                                                                            */
/******************************************************************************/

/** xyzopen -- Open an image file.                                            */
/*& bpw                                                                       */
/*: image-i/o                                                                 */
/*+                                          
      subroutine xyzopen( tno, name, status, naxis, axlen )
      integer       tno
      character*(*) name
      character*(*) status
      integer       naxis
      integer       axlen(naxis)

This opens an image file. For an old file, the number of axes is returned in
naxis and the size of each axis in axlen. For a new file, this information is
used to define the dataset.
    Input:
       name        The name of the file to be opened
       status      Either 'old' or 'new'
    Output:
       tno         The image-file handle
    Input or Output:
       naxis       For 'old' datasets: in input dimension of array axlen, on
                   output dimension of datacube; for 'new' datasets: dimension
                   of new dataset
       axlen       The length of the axes, output for 'old' datasets, 'input'
                   for 'new' datasets                                         */
/*-- */

int first=TRUE;

char * mkopen_c (int, char *, char *);
void bug_c (char, char *);
void bugno_c (char, int);

void xyzopen_c( handle, name, status, naxis, axlen )
int  *handle;
char *name;
char *status;
int  *naxis, axlen[];
{
/* This accesses the image data (hopen and haccess). Then it checks whether
   this was an OLD dataset. If so, the naxis. items are read from the header
   and a check is made on the size of the image file. For a new dataset the
   information is written out and the file initialized with the "binary item"
   sequence. Finally the information is stored in the imgs structure for later
   use.
*/
#define OLD 1
#define NEW 2
    void rdhdi_c(), wrhdi_c();
    int  iostat;
    int  tno;
    int  access; char *mode;
    int  cubesize; char axes[8];
    char *s[ITEM_HDR_SIZE];
    int  n_axis;

    if(first) { for(tno=0;tno<MAXOPEN;tno++) imgs[tno].itno=0;  first=FALSE; }

    if(itest)printf("Open %s; %s; naxis %d\n",name,status,*naxis);
    n_axis = *naxis;
    if(      !strcmp( "old",     status ) ) { access = OLD; mode = "read";  }
    else if( !strcmp( "new",     status ) ) { access = NEW; mode = "write"; }
    else { bug_c( 'f', "xyzopen: Unrecognised status" ); printf("bug\n"); }

    hopen_c(  &tno, name, status, &iostat );                   check(iostat);
    haccess_c( tno, &imgs[tno].itno, "image", mode, &iostat ); check(iostat);
    imgs[tno].mask = mkopen_c( tno, "mask", status );

    strcpy( axes, "naxis0" );
    if( access == OLD ) {
        rdhdi_c( tno, "naxis", naxis, 0 );
        if( *naxis > n_axis ) bug_c('f',"xyzopen: Too many axes for this task");
        if( *naxis<=0||*naxis>MAXNAX ) bug_c('f',"xyzopen: Bad number of axes");
        for( cubesize=1, d=0; d<*naxis; d++ ) {
            axes[5]++; rdhdi_c( tno, axes, &axlen[d], 0 );
            if( axlen[d] <= 0 ) bug_c( 'f', "xyzopen: Bad image dimension" );
            cubesize = cubesize * axlen[d];
        }
        if( hsize_c( imgs[tno].itno ) < H_REAL_SIZE*cubesize+ITEM_HDR_SIZE )
            bug_c( 'f', "xyzopen: Image file appears too small" );
        hreadb_c( imgs[tno].itno, s,0,ITEM_HDR_SIZE, &iostat ); check(iostat);
        if( memcmp( s, real_item, ITEM_HDR_SIZE ) )
            bug_c( 'f', "xyzopen: Bad image file" );
    } else {
        wrhdi_c( tno, "naxis", *naxis );
        for( d=0; d<*naxis; d++ ) {
            axes[5]++; wrhdi_c( tno, axes, axlen[d] );
        }
        hwriteb_c( imgs[tno].itno, real_item,0,ITEM_HDR_SIZE, &iostat );
        check(iostat);
    }

    imgs[tno].naxis       = *naxis;
    imgs[tno].cubesize[0] = 1;
    imgs[tno].axlen[0]    = 1;
    for( d=1; d<=*naxis; d++ ) {
        imgs[tno].axlen[d]    = axlen[d-1];
        imgs[tno].cubesize[d] = imgs[tno].cubesize[d-1] * imgs[tno].axlen[d];
    }
    if( access == OLD ) imgs[tno].lastwritten = imgs[tno].cubesize[*naxis];
    else                imgs[tno].lastwritten = -1;

    *handle = tno;
    ntno++;
    imgs[tno].number = ntno;
    dimsub[tno] = -1;
}

void mkclose_c (char *);

/** xyzclose - Close an image file                                            */
/*& bpw                                                                       */
/*: image-i/o                                                                 */
/*+
      subroutine xyzclose( tno )
      integer    tno

This closes an image file.

   Input:
      tno:     The image-file handle                                          */
/*--*/

void xyzclose_c( tno )
int tno;
{
    int  iostat;
    void xyzflush_c();
    xyzflush_c( tno );
    hdaccess_c( imgs[tno].itno, &iostat ); check(iostat);
    if( imgs[tno].mask ) mkclose_c( imgs[tno].mask );
    hclose_c( tno );
    ntno--;
    if( ntno == 0 && !neverfree ) {
        free( buffer ); buffer = NULL;
        free( mbuffr ); mbuffr = NULL;
    }
}

/** xyzflush - Force output buffer to be written to disk                      */
/*& bpw                                                                       */
/*: image-i/o                                                                 */
/*+
      subroutine xyzflush( tno )
      integer    tno

This flushes the output buffer to disk, just like when closing a dataset.
However, the dataset remains open. This is intended for usage where there
is a limit on the number of open datasets, so that one cannot have them
open all at the same time, and then do all setups once.

   Input:
      tno:     The image-file handle                                          */
/*--*/

void xyzflush_c( tno )
int tno;
{
    void manage_buffer(), zero();
    if( written[tno] ) { MODE=PUT; manage_buffer( tno, -1 ); }
    written[tno] = FALSE;
    if( imgs[tno].lastwritten<imgs[tno].cubesize[imgs[tno].naxis] ) zero(2,tno);
}


/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

/** xyzsetup - Set up arbitrary subcube                                       */
/*& bpw                                                                       */
/*: image-i/o                                                                 */
/*+
      subroutine xyzsetup( tno, subcube, blc, trc, viraxlen, vircubesize )
      integer       tno
      character*(*) subcube
      integer       blc(*), trc(*)
      integer       viraxlen(*), vircubesize(*)

This routine does the definitions necessary to allow reading or writing
an arbitrary subcube in a n-dimensional datacube. It is used to define
which part of an input datacube should be read or which part of an
output datacube should be written. 

The variable subcube is used to define the axes of the subcubes to be
read or written. The axes of a datacube are called 'x', 'y', 'z', 'a',
'b', ... 'subcube' consists of any non-redundant combination of these.
Zero-dimensional subcubes, i.e. single pixels, are specified by setting
'subcube' to ' '. 'subcube' can be e.g. 'z' to read/write lines in the
z-direction, 'xy' to read/write image planes or 'xyz' to read/write a
3-d subcube. Permutations (like 'zx') are also permitted. These will be
reflected in the ordering of the output array produced by subroutine
xyzread. Axis reversals are also possible, and are set up by preceding
the axis name with a '-'. Again, this will be reflected in the ordering
of elements in the output array of xyzread.
If xyzsetup is used to define an output cube, the subcube variable
should be interpreted as giving the names of the axes in the virtual
cube. E.g., for subcube='z', the first axis in the virtual cube is the
z-axis of the output cube. The second  axis then is the 'x'  axis, etc.

blc and trc give the bottom left and top right corner of the total
region of the input cube that needs to be worked on (in absolute pixels,
i.e. the bottom left of the original cube is 1,1,...), or the total
region of the output cube that should be written. If the output cube
did not yet exist and the region is smaller than the defined size of
the cube, the pixels outside the region are set to zero automatically.

viraxlen and vircubesize are provided for the convenience of the
programmer. They correspond to a virtual cube, whose axes are permuted
according to the specification of 'subcube', and whose axislengths are
given by the differences of blc and trc. This virtual (intermediate)
cube contains all the pixels on which the calling program should work,
sorted in the order in which they are needed.

With a call to xyzsetup all previous buffers are irrevocably lost; but
output buffers are flushed before that. However, all calls to xyzsetup
should be done before working on the data.

    Input:
      tno           image file handle
      subcube       a character variable defining the subcube type
      blc, trc      arrays giving the bottom left and top right corner
                    of the region in the input/output cube to work on/
                    write; the number of elements used equals the
                    dimension of the input/output cube
    Output:
      viraxlen:     length of axes of virtual cube
      vircubesize:  size of subcubes:
                    vircubesize(d) = Prod(i=1->d) viraxlen(i)                 */
/*--*/

int ferr (char *, char);

void xyzsetup_c( tno, subcube, blc, trc, viraxlen, vircubesize )
int   tno;
char *subcube;
int   blc[], trc[];
int   viraxlen[], vircubesize[];
{
/* This initializes some information needed later. It keeps separate values
   for each tno that was opened.
   dimsub:                dimension of subcube
   axnum:                 relation between axes
   imgs.blc, imgs.trc:    lower left and upper right used from input or
                          written to output
   bufs.axlen:            length of virtual axes
   imgs/bufs.cubesize:    cs(i) = (Prod)(d<i) axlen(d): # pix in line/plane etc
   viraxlen, vircubesize: info returned to caller
*/
   
    int  axisuse[ARRSIZ]; char *axisnames = { "xyzabcdefghij" };
    char *sub_cube; int reversal;
    int  naxes;

/* Because a new call to xyzsetup redefines all buffers, they are flushed
   before the redefinition is done */
    void manage_buffer();
    for( i=0; i<MAXOPEN; i++ ) {
       if( written[i] ) { MODE=PUT; manage_buffer( i, -1 ); }
       written[i] = FALSE;
    }

/* Intermediate variable for easier reading */
    naxes = imgs[tno].naxis;

/* Decode subcube argument into dimsub, and axnum and reverse arrays */
    dim=0;
    for( d=1;d<=MAXNAX;d++ ) axisuse[d]=FALSE; reversal=FALSE; sub_cube=subcube;
    while( *subcube ) {
        if(        *subcube == ' ' ) { ;
        } else if( *subcube == '-' ) {
            if( reversal ) bug_c( 'f', "xyzsetup: Bad syntax for subcube arg" );
            reversal=TRUE; if(itest)printf("reversal");
        } else {
            d=1;
            while( *subcube != *(axisnames+d-1) && d<=naxes && *axisnames ) d++;
            if( d>naxes || !*axisnames )
                ferr( "xyzsetup: Axis outside cube", *subcube );
            if(axisuse[d])ferr("xyzsetup: Axis given more than once",*subcube);
            dim++;
            axisuse[d]=TRUE; reverse[tno][dim]=reversal; axnum[tno][dim]=d;
            reversal=FALSE;
        }
        subcube++;
    }
    dimsub[tno] = dim;
    subcube = sub_cube;
/* Fill out the arrays axnum and reverse, so that all elements are defined */
    for( reverse[tno][0]=FALSE, d=0, dim=1; dim<=dimsub[tno]; dim++ ) {
        if( reverse[tno][dim] ) { reverse[tno][0]=TRUE; d++; } }
    if( d == dimsub[tno] ) reverse[tno][0]=ALL;
    for( d=1; d<=MAXNAX; d++ ) { if( !axisuse[d] ) {
        axnum[tno][dim]=d; reverse[tno][dim]=FALSE; dim++; } }

/* Save blc and trc */
    for( dim=1; dim<=naxes; dim++ ) {
        if( ( blc[dim-1] < 1 ) || ( trc[dim-1] > imgs[tno].axlen[dim] ) )
            bug_c( 'f', "xyzsetup: Subcube blc and/or trc outside range" );
        imgs[tno].blc[dim] = blc[dim-1]-1;
        imgs[tno].trc[dim] = trc[dim-1]-1;
    }

/* Save axislengths and cubesizes */
    bufs[tno].naxis       = naxes;
    bufs[tno].axlen[0]    = 1;
    bufs[tno].cubesize[0] = 1;
    for( dim=1; dim<=naxes; dim++ ) {
        bufs[tno].axlen[dim] = imgs[tno].trc[ axnum[tno][dim] ] - 
                               imgs[tno].blc[ axnum[tno][dim] ] + 1;
    }
    for( dim=1; dim<=naxes; dim++ ) {
        bufs[tno].cubesize[dim]=bufs[tno].cubesize[dim-1]*bufs[tno].axlen[dim];
    }

/* More initializations:
   pointers to window in file that is in buffer;
   variable indicating if write buffer was filled;
   variable indicating if any transposition must be done; */   
    for( d=0; d<MAXOPEN; d++ ) { bufs[d].filfir = -1; bufs[d].fillas = -1; }
    written[tno] = FALSE;
    imgs[tno].nocopy = TRUE;
    for( dim=1; dim<=naxes; dim++ )
        if( dim != axnum[tno][dim] ) imgs[tno].nocopy = FALSE;
    for( dim=1; dim<=naxes; dim++ ) {
        if( blc[dim-1] != 1 || trc[dim-1] != imgs[tno].axlen[dim] )
            imgs[tno].nocopy = FALSE;
    }

/* Some info for the caller */
    for( dim=1; dim<=naxes; dim++ ) {
        viraxlen[dim-1]    = bufs[tno].axlen[dim];
        vircubesize[dim-1] = bufs[tno].cubesize[dim];
    }

/* Set flag so that manage_buffer knows it has to (re)calculate the
   buffersize */
   allocatebuffer = TRUE;

/* Testoutput only */
    if(itest){
        printf("tno %d\n",tno);
        printf("d      incsz     vircsz    inaxlen   viraxlen        axnum\n");
        for( dim=1; dim<=naxes; dim++ )
            printf("%d %10d %10d %10d %10d %10d\n",
            dim, imgs[tno].cubesize[dim],bufs[tno].cubesize[dim],
            imgs[tno].axlen[dim], viraxlen[dim-1], axnum[tno][dim]);
    }

}

int ferr( char * string, char arg )
{
    char message[80]; char *msg;
    msg = &message[0];
    while( *string != '\0' ) *msg++ = *string++;
    *msg++ = ':'; *msg++ = ' '; *msg++ = arg; *msg = '\0';
    bug_c( 'f', message );

    return 0;
}


/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

/** xyzmkbuf - create the i/o buffer (only once)                              */
/*& bpw                                                                       */
/*: image-i/o                                                                 */
/*++
      subroutine xyzmkbuf

Usually, xyzio tries to be smart about allocating its own buffer and tries
to minimize its memory use. This works well if all calls to xyzsetup can
be done before any reading or writing is done. However, it may allocate
too much memory if calls to xyzsetup are followed by calls to xyzclose so
that all datasets are closed, and then more opens and setups are done. To
circumvent this, xyzmkbuf creates an i/o buffer of maximum size, and makes
sure it is never deallocated.                                                 */
/*--*/

void xyzmkbuf_c()
{
   int bufferallocation(), i;
   i = bufferallocation( MAXBUF );
   neverfree = TRUE;
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

/** xyzs2c - Get the fixed coordinates for a given subcube                    */
/*& bpw                                                                       */
/*: image-i/o                                                                 */
/*++
      subroutine xyzs2c( tno, subcubenr, coords )
      integer    tno
      integer    subcubenr
      integer    coords(*)

This routine, xyzsetup and xyzread/xyzwrite work together to allow
reading/writing an arbitrary subcube in a n-dimensional datacube.
xyzs2c calculates the fixed coordinates of a particular subcube.
xyzsetup defines a particular type of subcube in a datacube, e.g. line
profiles in the z-direction. For a given subcube there are varying (z
in the example) and fixed coordinates. The subcubes are ordered along
the fixed-coordinate axes: the first subcube has (x=1,y=1), the second
has (x=2,y=1), etc. xyzs2c returns the values of the fixed coordinates
for a given subcubenumber. These can then be used as input to xyzread
or xyzwrite.

   Input:
      tno           The handle of the dataset
      subcubenr     Identification of the subcube
   Output:
      coords        Coordinates of the blc of the subcube                     */
/*--*/

void xyzs2c_c( tno, subcubenr, coords )
int tno;
int subcubenr;
int coords[];
{
/* Calculates fixed coordinates of subcubenr:
   first calculate pixeloffset of lower left of subcube; convert to
   coordinates using virtual cube specifications; then add appropriate
   lower left offset of input and shift so that first element is first
   fixed axis.
*/
    int  dim_sub, naxes, offset;
    void p2c();
    int  coo[ARRSIZ];

    dim_sub = dimsub[tno];
    naxes   = bufs[tno].naxis;
    offset  = subcubenr * bufs[tno].cubesize[dim_sub];
    if( offset < 0 || offset >= bufs[tno].cubesize[naxes] )
        bug_c( 'f', "xyzs2c: Subcube lies outside cube" );
    p2c( offset, bufs[tno].axlen, bufs[tno].cubesize, naxes, coo );
    dim = dim_sub+1;
    while( dim<=naxes ) {
         coords[dim-dim_sub-1] = coo[dim] + imgs[tno].blc[axnum[tno][dim]] + 1;
         dim++; }

    if(otest) {
        printf( "\nsubcubenr %d starts at vircube coords:", subcubenr );
        for( dim=1; dim<=naxes; dim++ ) printf(" %d",coo[dim]);
        printf( ";  orig. cube coords:" );
        for( dim=0; dim<naxes-dim_sub; dim++ ) printf( " %d", coords[dim]-1 );
        printf( "\nvir filfir %d fillas %d virpix_off %d\n",
                  bufs[tno].filfir, bufs[tno].fillas, offset );
    }
}


/******************************************************************************/

/** xyzc2s - Get the subcubenr at a fixed coordinate                          */
/*& bpw                                                                       */
/*: image-i/o                                                                 */
/*++
      subroutine xyzc2s( tno, coords, subcubenr )
      integer    tno
      integer    coords(*)
      integer    subcubenr

This routine does the inverse of xyzs2c: it calculates the subcubenr
from a list of coordinates for a previously opened dataset after a
call to xyzsetup to define a particular type of subcube in a datacube.

   Input:
      tno           The handle of the dataset
      coords        Coordinates of the blc of the subcube
   Output:
      subcubenr     Identification of the subcube                             */
/*--*/

void xyzc2s_c( tno, coords, subcubenr )
int  tno;
int  coords[];
int *subcubenr;
{
/* Calculates subcubenr at fixed coordinates:
   Convert coordinates to virtual-cube coordinates, then calculate
   virtual-cube offset and divide by subcubelength.
*/
    int  dim_sub, naxes, offset;
    int  c2p();
    int  coo[ARRSIZ];

    dim_sub = dimsub[tno];
    naxes   = bufs[tno].naxis;
    for( dim=1; dim<=dim_sub; dim++ ) coo[dim] = 0;
    dim = 0;
    while( dim < naxes-dim_sub ) {
         coo[axnum[tno][dim+dim_sub+1]] = coords[dim] - imgs[tno].blc[dim] - 1;
         dim++; }
    offset = c2p( coo, bufs[tno].cubesize, naxes );
    if( offset < 0 || offset >= bufs[tno].cubesize[naxes] )
        bug_c( 'f', "xyzc2s: Coordinates lie outside cube" );
    *subcubenr = offset / bufs[tno].cubesize[dim_sub];

    if(itest) {
        printf( "\ncoords" );
        for( dim=1; dim<=naxes; dim++ ) printf(" %d",coo[dim]);
        printf( " are for subcubenr %d:", *subcubenr );
        printf( ";  orig. cube coords:" );
        for( dim=0; dim<naxes-dim_sub; dim++ ) printf( " %d", coords[dim]-1 );
        printf( "\nvir filfir %d fillas %d virpix_off %d\n",
                  bufs[tno].filfir, bufs[tno].fillas, offset );
    }
}


/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

/** xyzread - Read arbitrary subcube                                          */
/*& bpw                                                                       */
/*: image-i/o                                                                 */
/*+
      subroutine xyzread( tno, coords, data, mask, ndata )
      integer    tno
      integer    coords(*)
      real       data(*)
      logical    mask(*)
      integer    ndata

This routine, xyzsetup and xyzs2c work together to allow reading an
arbitrary subcube in a n-dimensional datacube. xyzread reads a subcube
from the datacube, as defined by xyzsetup at coordinates calculated by
xyzs2c.

The array coords gives the coordinates of the axes complementary to the
subcube axes. E.g., if 'subcube' in the call to xyzsetup was 'y' and
the datacube is 3-dimensional, coords(1) and coords(2) give the 'x' and
'z' coordinate of the requested line profile, respectively. Or, if
'subcube' was 'xz', coords(1) gives the 'y'-coordinate of the plane.
For a datacube of dimension 'd', only the first 'd - dimsub' elements
of the array coords will be used (where 'dimsub' is the dimension of
the subcube).

The array data (of dimension ndata) will hold the requested information.
If the subcube was 0-dimensional, the result is the pixel value. For a
1-d subcube the profile is returned in data. The number of returned
elements is vircubesize(1), as returned by xyzsetup. For a 2-d subcube
the array data gives the requested plane, as a 1-d array of length
vircubesize(2). Etc.

The mask array indicates if pixels in the data array were undefined
(a TRUE value means the pixel is OK). Element 1 corresponds to data(1),
etc.
(this is not yet implemented, so all elements are returned as TRUE).

N.B.: to scan a datacube pixel by pixel it is more efficient to use
subroutine xyzpixrd instead of xyzread, as the conversion from offset to
coordinates to offset that xyzs2c and xyzread do then is superfluous and
time-consuming.

    Input:
      tno           image file handle
      coords        array of which the first (dim cube)-(dim subcube)
                    elements are used, giving the coordinate values
                    along the complementary axes
    Output:
      data          array containing data read in
      mask          FALSE values indicate undefined pixels
      ndata         number of elements read                                   */
/*--*/

void xyzread_c( tno, coords, data, mask, ndata )
int    tno;
int    coords[];
float *data;
int   *mask;
int   *ndata;
{
/* The calculation first needs the pixeloffset of the input coordinate.
   For the varying axes the pixelnumber is 0, for the fixed axes of the
   subcube, the pixelnumber is the input minus the lower left offset.
   The input array had the first fixed coordinate as element 0, so a
   shift of -1 is necessary. After finding the pixelnumber in the virtual
   cube get_put_data is used.
*/
    int  virpix_off;
    int  dim_sub, naxes;
    void get_put_data();
    dim_sub    = dimsub[tno];
    naxes      = bufs[tno].naxis;
    virpix_off = 0;
    dim = dim_sub+1;
    while( dim <= naxes ) {
        virpix_off += bufs[tno].cubesize[dim-1] *
            ( coords[dim-dim_sub-1]-1 - imgs[tno].blc[ axnum[tno][dim] ] );
        dim++; }
    MODE=GET; get_put_data( tno, virpix_off, data, mask, ndata, dim_sub );
}



/** xyzpixrd - Get a pixel from a dataset                                     */
/*& bpw                                                                       */
/*: image-i/o                                                                 */
/*+
      subroutine xyzpixrd( tno, pixelnr, value, mask )
      integer    tno
      integer    pixelnr
      logical    mask
      real       value

This routine provides a faster version of calls to xyzs2c and xyzread
for the case that the calling program needs single pixels. It should
be used after a call to xyzsetup was used to set up zero-dimensional
subcubes. The calling program can then loop over all pixels (from 1 to
vircubesize(naxis)). xyzpixrd takes care of reading the datacube.
Using this routine instead of xyzs2c and xyzread reduces the overhead
by more than a factor 10.

    Input:
      tno           image file handle
      pixelnr       pixelnr to be read from virtual cube
      
    Output:
      value         pixel value
      mask          FALSE if pixel was undefined                              */
/*--*/

void xyzpixrd_c( tno, pixelnr, data, mask )
int    tno;
int    pixelnr;
float *data;
int   *mask;
{
    int  virpix_off;
    void manage_buffer();
    /*$$if(otest) xyzs2c_c( tno, pixelnr-1, tcoo );$$*/
    virpix_off = pixelnr - 1;
    if( virpix_off < bufs[tno].filfir || virpix_off > bufs[tno].fillas ) {
        MODE=GET; manage_buffer( tno, virpix_off );
    }
    *data = *( buffer + bufs[tno].bufstart + virpix_off );
    *mask = *( mbuffr + bufs[tno].bufstart + virpix_off );
    /*$$if(otest) testprint( tno, virpix_off, virpix_off );$$*/
}



/** xyzprfrd - Get a profile from a dataset                                   */
/*& bpw                                                                       */
/*: image-i/o                                                                 */
/*+
      subroutine xyzprfrd( tno, profilenr, profile, mask, ndata )
      integer    tno
      integer    profilenr
      real       profile(*)
      logical    mask(*)
      integer    ndata

This routine provides a (little) faster version for calls to xyzs2c and
xyzread for the case that the calling program needs profiles. It should
be used after a call to xyzsetup was used to set up one-dimensional
subcubes. The calling program can then loop over all profiles (from 1 to
vircubesize(naxis)/vircubesize(1)). xyzprfrd takes care of reading the
datacube. Using this routine instead of xyzs2c and xyzread reduces the
overhead by 10% (for 256-long profiles) to 30% (for 64-long profiles).

    Input:
      tno           image file handle
      profilenr     profile nr to be read from virtual cube
    Output:
      profile       will contain the profile
      mask          FALSE values indicate undefined pixels
      ndata         number of elements read                                   */
/*--*/

void xyzprfrd_c( tno, profilenr, data, mask, ndata )
int    tno;
int    profilenr;
float *data;
int   *mask;
int   *ndata;
{
    int  virpix_off;
    void get_put_data();
    /*$$if(otest) xyzs2c_c( tno, profilenr-1, tcoo );$$*/
    virpix_off = (profilenr-1) * bufs[tno].cubesize[1];
    MODE=GET; get_put_data( tno, virpix_off, data, mask, ndata, 1 );
}



/** xyzplnrd - Get a plane from a dataset                                     */
/*& bpw                                                                       */
/*: image-i/o                                                                 */
/*+
      subroutine xyzplnrd( tno, planenr, plane, mask, ndata )
      integer    tno
      integer    planenr
      real       plane(*)
      logical    mask(*)
      integer    ndata

This routine provides a more convenient version of calls to xyzs2c and
xyzread for the case that the calling program needs planes. It should
be used after a call to xyzsetup was used to set up two-dimensional
subcubes. The calling program can then loop over all planes (from 1 to
vircubesize(naxis)/vircubesize(2)). xyzplnrd takes care of reading the
datacube. The caveat is that the calling program should have an array
that is large enough to contain the complete plane.
Using this routine instead of xyzs2c and xyzread reduces the overhead
by 1% (for 64**2 cubes) or less.

    Input:
      tno           image file handle
      planenr       plane nr to be read from virtual-cube
    Output:
      plane         will contain the plane as a 1-d array
      mask          FALSE values indicate undefined pixels
      ndata         number of elements read                                   */
/*--*/

void xyzplnrd_c( tno, planenr, data, mask, ndata )
int    tno;
int    planenr;
float *data;
int   *mask;
int   *ndata;
{
    int  virpix_off;
    void get_put_data();
    /*$$if(otest) xyzs2c_c( tno, planenr-1, tcoo );$$*/
    virpix_off = (planenr-1) * bufs[tno].cubesize[2];
    MODE=GET; get_put_data( tno, virpix_off, data, mask, ndata, 2 );
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

/** xyzwrite - Write arbitrary subcube                                        */
/*& bpw                                                                       */
/*: image-i/o                                                                 */
/*+
      subroutine xyzwrite( tno, coords, data, mask, ndata )
      integer    tno
      integer    coords(*)
      real       data(*)
      logical    mask(*)
      integer    ndata

This routine, xyzsetup and xyzs2c work together to allow writing an
arbitrary subcube in a n-dimensional datacube. xyzwrite writes a subcube
to the datacube, as defined by xyzsetup at coordinates calculated by
xyzs2c.

The array coords gives the coordinates of the axes complementary to
the subcube axes. E.g., if 'subcube' in the call to xyzsetup was 'y'
and the datacube is 3-dimensional, coords(1) and coords(2) give the
'x' and 'z' coordinate of the requested line profile, respectively. Or,
if 'subcube' was 'xz', coords(1) gives the 'y'-coordinate of the plane.
For a datacube of dimension 'd', only the first 'd - dimsub' elements
of the array coords will be used (where 'dimsub' is the dimension of the
subcube).

The array data (of dimension ndata) holds the information to be written.
If the subcube was 0-dimensional, the first element of data is written.
For a 1-d subcube a profile is written. The first vircubesize(1) (as
returned by xyzsetup) elements of data are used. For a 2-d subcube the
array data gives the requested plane, as a 1-d array of length
vircubesize(2). Etc.

The mask array indicates if pixels in the data array must be set to
"undefined". A TRUE value means the data is OK, FALSE means it is
undefined. Element 1 corresponds to data 1, etc.
(this is not yet implemented, so ignored)

N.B.: to write a datacube pixel by pixel it is more efficient to use
subroutine xyzpixwr instead of xyzwrite, as the conversion from
offset to coordinates to offset that xyzs2c and xyzwrite do then is
superfluous and time-consuming.

    Input:
      tno           image file handle
      coords        array of which the first (dim cube)-(dim subcube)
                    elements are used, giving the coordinate values
                    along the complementary axes
      data          array containing data to be written
      mask          FALSE values indicate undefined pixel
      ndata         number of elements to write                               */
/*--*/

void xyzwrite_c( tno, coords, data, mask, ndata )
int    tno;
int    coords[];
float *data;
int   *mask;
int   *ndata;
{
/* The calculation first needs the pixeloffset of the input coordinate.
   For the varying axes the pixelnumber is 0, for the fixed axes of the
   subcube, the pixelnumber is the input minus the lower left offset.
   The input array had the first fixed coordinate as element 0, so a
   shift of -1 is necessary. After finding the pixelnumber in the
   virtual cube get_put_data is used.
*/
    int  virpix_off;
    int  dim_sub, naxes;
    void get_put_data();
    dim_sub    = dimsub[tno];
    naxes      = bufs[tno].naxis;
    virpix_off = 0;
    dim = dim_sub+1;
    while( dim <= naxes ) {
        virpix_off += bufs[tno].cubesize[dim-1] *
            ( coords[dim-dim_sub-1]-1 - imgs[tno].blc[ axnum[tno][dim] ] );
        dim++; }
    MODE=PUT; get_put_data( tno, virpix_off, data, mask, ndata, dim_sub );
}



/** xyzpixwr - Write a pixel to a dataset                                     */
/*& bpw                                                                       */
/*: image-i/o                                                                 */
/*+
      subroutine xyzpixwr( tno, pixelnr, value, mask )
      integer    tno
      integer    pixelnr
      real       value
      logical    mask

This routine provides a faster version of calls to xyzs2c and xyzwrite
for the case that the calling program provides single pixels. It should
be used after a call to xyzsetup was used to set up zero-dimensional
subcubes. The calling program can then loop over all pixels (from 1 to
vircubesize(naxis)). xyzpixwr takes care of writing the datacube.
Using this routine instead of xyzs2c and xyzwrite reduces the overhead
by more than a factor 10.

    Input:
      tno           image file handle
      pixelnr       pixelnr to be read from virtual cube
      value         pixel value
      mask          FALSE indicates pixel is undefined                        */
/*--*/

void xyzpixwr_c( tno, pixelnr, data, mask )
int    tno;
int    pixelnr;
float *data;
int   *mask;
{
    int  virpix_off;
    void manage_buffer();
    /*$$if(otest) xyzs2c_c( tno, pixelnr-1, tcoo );$$*/
    virpix_off = pixelnr - 1;
    if( virpix_off < bufs[tno].filfir || virpix_off > bufs[tno].fillas ) {
        MODE=PUT; manage_buffer( tno, virpix_off );
    }
    *( buffer + bufs[tno].bufstart + virpix_off ) = *data;
    *( mbuffr + bufs[tno].bufstart + virpix_off ) = *mask;
    written[tno] = TRUE;
    /*$$if(otest) testprint( tno, virpix_off, virpix_off );$$*/
}



/** xyzprfwr - Write a profile to a dataset                                   */
/*& bpw                                                                       */
/*: image-i/o                                                                 */
/*+
      subroutine xyzprfwr( tno, profilenr, profile, mask, ndata )
      integer    tno
      integer    profilenr
      real       profile(*)
      logical    mask(*)
      integer    ndata

This routine provides a (little) faster version for calls to xyzs2c and
xyzwrite for the case that the calling program provides profiles. It
should be used after a call to xyzsetup was used to set up 1-dimensional
subcubes. The calling program can then loop over all profiles (from 1 to
vircubesize(naxis)/vircubesize(1)). xyzprfwr takes care of writing the
datacube. Using this routine instead of xyzs2c and xyzwrite reduces the
overhead by 10% (for 256-long profiles) to 30% (for 64-long profiles).

    Input:
      tno           image file handle
      profilenr     profile nr to be read from virtual cube
      profile       contains the profile to be written
      mask          FALSE values indicate undefined pixels
      ndata         number of elements to write                               */
/*--*/

void xyzprfwr_c( tno, profilenr, data, mask, ndata )
int    tno;
int    profilenr;
float *data;
int   *mask;
int   *ndata;
{
    int  virpix_off;
    void get_put_data();
    /*$$if(otest) xyzs2c_c( tno, profilenr-1, tcoo );$$*/
    virpix_off = (profilenr-1) * bufs[tno].cubesize[1];
    MODE=PUT; get_put_data( tno, virpix_off, data, mask, ndata, 1 );
    written[tno] = TRUE;
}



/** xyzplnwr - Write a plane to a dataset                                     */
/*& bpw                                                                       */
/*: image-i/o                                                                 */
/*+
      subroutine xyzplnwr( tno, planenr, plane, mask, ndata )
      integer    tno
      integer    planenr
      real       plane(*)
      logical    mask(*)
      integer    ndata

This routine provides a more convenient version of calls to xyzs2c and
xyzwrite for the case that the calling program provides planes. It
should be used after a call to xyzsetup was used to set up 2-dimensional
subcubes. The calling program can then loop over all planes (from 1 to
vircubesize(naxis)/vircubesize(2)). xyzplnwr takes care of writing the
datacube. The caveat is that the calling program should have an array
that is large enough to contain the complete plane.
Using this routine instead of xyzs2c and xyzwrite reduces the overhead
by 1% (for 64**2 cubes) or less.

    Input:
      tno           image file handle
      planenr       plane nr to be read from virtual-cube
      plane         contains the plane to be written as a 1-d array
      mask          FALSE values indicate undefined pixels
      ndata         number of elements to write                               */
/*--*/

void xyzplnwr_c( tno, planenr, data, mask, ndata )
int    tno;
int    planenr;
float *data;
int   *mask;
int   *ndata;
{
    int  virpix_off;
    void get_put_data();
    /*$$if(otest) xyzs2c_c( tno, planenr-1, tcoo );$$*/
    virpix_off = (planenr-1) * bufs[tno].cubesize[2];
    MODE=PUT; get_put_data( tno, virpix_off, data, mask, ndata, 2 );
    written[tno] = TRUE;
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

/******************************************************************************/
/*                                                                            */
/* The routine that figures out if i-o must be done                           */
/*                                                                            */
/******************************************************************************/

void get_put_data( tno, virpix_off, data, mask, ndata, dim_sub )
int    tno;
int    virpix_off;
float *data;
int   *mask;
int   *ndata;
int    dim_sub;
{
/* This checks if the needed subcube is in the buffer. If so, a piece
   of the buffer is copied. If not, manage_buffer is called to fill or
   empty the buffer and then the copy is done.
*/
    int    virpix_lst;
    void   manage_buffer();
    void   do_copy();
    float *bufptr, *bufend, *bufsta;
    void   copy_to_one_d();
    int    i, coo[ARRSIZ], next;


    virpix_lst = virpix_off + bufs[tno].cubesize[dim_sub] - 1;
    if( MODE==GET ) *ndata = bufs[tno].cubesize[dim_sub];
    if( MODE==PUT && *ndata < bufs[tno].cubesize[dim_sub] )
        bug_c( 'f', "xyzio: Input array too small to hold subcube" );
    if( virpix_off < bufs[tno].filfir || virpix_lst > bufs[tno].fillas ) {
        if(itest)printf("\nNew buffer starts at %d MODE %d\n",virpix_off,MODE);
        if( virpix_off >= bufs[tno].cubesize[bufs[tno].naxis] )   bug_c( 'f',
           "xyzio: Caller tries to access pixel outside datacube");
        if( dimsub[tno] == -1 )                                   bug_c( 'f',
           "xyzio: xyzsetup was never called for dataset" );
        manage_buffer( tno, virpix_off );
    }

/* Plain copy */
    if( !reverse[tno][0] ) {
        bufptr = buffer + bufs[tno].bufstart + virpix_off;
        bufend = buffer + bufs[tno].bufstart + virpix_lst;
        do_copy( bufptr, bufend, UP,   data, mask );
/* Reverse copy */
    } else if( reverse[tno][0] == ALL ) {
        bufptr = buffer + bufs[tno].bufstart + virpix_lst;
        bufend = buffer + bufs[tno].bufstart + virpix_off;
        do_copy( bufptr, bufend, DOWN, data, mask );
/* Some axes reversed */
    } else {
        copy_to_one_d( tno );
/* Apply a trick to avoid a very strange error on the Cray */
/*      bufsta = buffer + bufs[tno].bufstart + virpix_off; */
        i = bufs[tno].bufstart + virpix_off;
        for( d=1; d<=dim_sub; d++ ) {
            if( !reverses[d] ) coo[d] = 0; else coo[d] = bufsaxlen[d] - 1;
/*          bufsta += coo[d] * bufscubesize[d-1]; } */
            i += coo[d] * bufscubesize[d-1]; } bufsta = buffer + i;
        for( i=1; i<=bufscubesize[dim_sub]/bufscubesize[1]; i++ ) {
            if( !reverses[1] ) {
                bufptr = bufsta;
                bufend = bufsta + bufsaxlen[1] - 1;
                do_copy( bufptr, bufend, UP,   data, mask );
            } else {
                bufptr = bufsta;
                bufend = bufsta - bufsaxlen[1] + 1;
                do_copy( bufptr, bufend, DOWN, data, mask );
            }
            data += bufsaxlen[1]; mask += bufsaxlen[1];
            next=TRUE; d=2; while( d<=dim_sub && next ) {
                if( !reverses[d] ) {
                    coo[d]++; bufsta += bufscubesize[d-1];
                    next = ( coo[d] == bufsaxlen[d] );
                    if(next) {coo[d]=0;              bufsta -= bufscubesize[d];}
                } else {
                    coo[d]--; bufsta -= bufscubesize[d-1];
                    next = ( coo[d] == -1 );
                    if(next) {coo[d]=bufsaxlen[d]-1; bufsta += bufscubesize[d];}
                }
            }
        }
    }
    /*$$if(otest) testprint( tno, virpix_off, virpix_lst );$$*/
}


void do_copy( bufptr, bufend, DIR, data, mask )
float *bufptr, *bufend;
int    DIR;
float *data;
int   *mask;
{
    int *mbufpt;

    mbufpt = mbuffr + (int)(bufptr-buffer);

    if(        DIR == UP ) {
      if( MODE==GET ) {
        while( bufptr<=bufend ) { *data++ = *bufptr++; *mask++ = *mbufpt++; }}
      if( MODE==PUT ) {
        while( bufptr<=bufend ) { *bufptr++ = *data++; *mbufpt++ = *mask++; }}
    } else if( DIR == DOWN ) {
      if( MODE==GET ) {
        while( bufptr>=bufend ) { *data++ = *bufptr--; *mask++ = *mbufpt--; }}
      if( MODE==PUT ) {
        while( bufptr>=bufend ) { *bufptr-- = *data++; *mbufpt-- = *mask++; }}
    }
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

/******************************************************************************/
/*                                                                            */
/* Buffer control, figures out how to call loop_buffer                        */
/*                                                                            */
/******************************************************************************/

void manage_buffer( tno, virpix_off )
int tno;
int virpix_off;
{
/* This controls the buffer. It tries to do the absolute minimum number
   of disk-i/o's, using the array buffer, whose total length is determined
   by get_buflen xyzsetup. The array divided into sections, each
   corresponding to a particular opened image dataset. The first section is
   used to collect data read in or to write. The size of the sections varies
   with the number of opened datasets and is xyziobuflen/(nopened+1). When
   elements are copied from or to the buffer, they come form or go into the
   appropriate section. If a request is made for a pixel outside the section,
   manage_buffer is called and the parameters of the section will change.
   manage_buffer takes care that all elements of the image section are
   read/written. Disk-i/o uses the first section. After reading into it,
   each pixel is checked and if it is in range it is copied to the
   appropriate element in the section corresponding to the image. The
   reading and checking is repeated until the whole section is filled.
   For writing, all pixels in the first section are checked and the ones
   that are the range of the section for the image, are copied to the first
   section. This is continued until all elements in the image-section
   have been written. A special case occurs when the subcube specification
   was such that no transposition or region was given. Then the read/write
   is done directly to the image section, and the loops are skipped.

   There is one extra stage for the case where reading and writing is done
   to the same dataset. If a new read is done, the old buffer is first
   flushed, if it was ever written into.

   copy_to_one_d makes 1-d arrays of some arrays, to reduce the number
   of pointer calculations and to improve code-readability.

   For reading data, some buffer parameters are obtained first, then
   the buffer is filled. For writing, the current buffer is first
   flushed and then the buffer parameters are set up for the next
   buffer.

   set_bufs_limits figures out the virtual-cube pixeloffsets of the
   first and last element in the buffer and the range in x, y, z etc
   in the virtual-cube buffer and the input/output cube. This allows
   shortcuts to be taken.
   Further it defines bufs[tno].bufstart, which gives the first element
   in the buffer corresponding to this image. Before leaving manage_buffer
   this is changed into a number that can be used to convert a virpix_off
   to a bufferelement. So, inside this routine bufs[tno].bufstart points
   to the buffer, outside it points to the buffer index of the first
   element of the virtual-cube.

   For output writing, on the very first pass the buffer was still
   empty, not full, so all that is done is to initialize it. Only at
   the second and all later passes is it written to disk.

   After all this the first and last pixel of the virtual cube are
   converted to the corresponding offsets in the input cube. All
   required pixels lie within that range.

   Then a loop is done over all pixels in the input/output buffer and
   elements of the virtual cube are copied. This is done in stages, as
   the full range of input/output pixels may be larger than the size
   of the buffer. So, in each stage a range from start to last is
   searched, until the finish is reached. Sometimes it is not necessary
   to really do the i/o, so then it is skipped.
*/
    void manage_the_buffer();
    if( MODE==GET && written[tno] ) {
        if(itest) printf("Flush previous output buffer\n"); 
        MODE=PUT; manage_the_buffer( tno, -1 ); MODE=GET;
        if(itest) printf("Set up new input buffer\n");
    }
    manage_the_buffer( tno, virpix_off );
}
void manage_the_buffer( tno, virpix_off )
int tno;
int virpix_off;
{
    void copy_to_one_d(), zero();
    void set_bufs_limits();
    int  transform_back(), c2p(), start, finis;
    int  get_last(), last;
    int  check_do_io();
    void loop_buffer();
    void fill_buffer(), empty_buffer();
    int  newstart;

    if( allocatebuffer ) get_buflen();

    copy_to_one_d( tno );

    if( imgs[tno].lastwritten == -1 ) zero( 1, tno );

    if( MODE==GET ) {
        set_bufs_limits( tno, virpix_off );
        written[tno] = FALSE;
    }
    if( MODE==PUT ) {
        if( bufs[tno].filfir == -1 ) {
            set_bufs_limits( tno, virpix_off );
            bufs[tno].bufstart = - bufs[tno].filfir + bufs[tno].bufstart;
            return;
        }
        bufs[tno].bufstart = bufs[tno].bufstart + bufs[tno].filfir;
        if(otest) printf("\n");
    }

    start = transform_back( bufs[tno].filfir );
    finis = transform_back( bufs[tno].fillas );
    if(itest) printf( "%s %d values: from %d to %d\n", 
              words[MODE], finis-start+1, start, finis );

    if(itest||rtest){nfound=0;if(imgs[tno].nocopy)nfound=finis-start+1;}
    while( start <= finis ) {
        last = get_last( start, finis );
        if( check_do_io( tno, start, last ) ) {
            if( MODE==GET ) {
                fill_buffer(  tno, start, last );
                loop_buffer(  tno, start, last, &newstart );
            }
            if( MODE==PUT ) {
                loop_buffer(  tno, start, last, &newstart );
                empty_buffer( tno, start, last );
            }
        } else {
            if(itest) printf( "Did not %s %d values: from %d to %d\n",
                      words[MODE], last-start+1, start, last );
        }
        start = newstart;
    }
    if(itest) printf( "virbuffer %s\n", words[MODE+2] );
    if( MODE==PUT ) set_bufs_limits( tno, virpix_off );
    bufs[tno].bufstart = - bufs[tno].filfir + bufs[tno].bufstart;
}

/******************************************************************************/
/*                                                                            */
/* Find the length of a buffer that fits in memory                            */
/*                                                                            */
/******************************************************************************/

int get_buflen()
{
    int  tno;
    int  try, maxsize, size;
    int *mbufpt, cnt;
    int  bufferallocation();
    if(itest)printf("# bytes per real %d\n",(int) sizeof(float));

    maxsize = 0;
    for( tno=1; tno<=MAXOPEN; tno++ ) {
      if( imgs[tno].itno != 0 ) {
        size    = bufs[tno].cubesize[bufs[tno].naxis];
        maxsize = ( (maxsize<size) ? size : maxsize );
      }
    }
    try = (ntno+1) * maxsize;
    if( (buffer==NULL) || (try>currentallocation) ) try = bufferallocation(try);
    allocatebuffer = FALSE;

    buffersize = try / (ntno+1);

    for( tno=0; tno<MAXOPEN; tno++ ) {
      if( imgs[tno].itno != 0 ) {
        if( bufs[tno].cubesize[dimsub[tno]] > buffersize )
        bug_c( 'f', "xyzsetup: Requested subcube too big for buffer" );
      }
    }

    /* set combined masking buffer to true, just in case no real mask
       is read in */
    mbufpt = mbuffr; cnt=0;
    while( cnt++ < try ) *mbufpt++ = FORT_TRUE;

    return 0;
}

int bufferallocation( n )
int n;
{
  /*
    char *malloc();
    int   free();
  */
    if( buffer != NULL ) { free( buffer ); buffer = NULL; }
    if( mbuffr != NULL ) { free( mbuffr ); mbuffr = NULL; }

    n  = ( (n < MAXBUF) ? n : MAXBUF );
    n *= 2;
    while( ( (buffer == NULL) || (mbuffr == NULL) ) && (n>1) ) {
	if( buffer != NULL ) { free( buffer ); buffer = NULL; }
        if( mbuffr != NULL ) { free( mbuffr ); mbuffr = NULL; }
        n /= 2;
        if(itest)printf("try %d\n",n);
        buffer = (float *)malloc((unsigned)(n*sizeof(float)));
        mbuffr = (int   *)malloc((unsigned)(n*sizeof(int)  ));
    }
    if( n == 1 ) bug_c( 'f', "xyzsetup: Failed to allocate any memory" );

    if(itest)printf("Allocated %d reals at %p\n",n,(void *) buffer);
    if(itest)printf("Allocated %d ints  at %p\n",n, (void *) mbuffr);

    currentallocation = n;
    return( n );
}

/******************************************************************************/

void copy_to_one_d( tno )
int tno;
{
/* All this does is make one-d arrays of some 2-d arrays, so that the
   number of pointer calculations is reduced. And also it makes the
   routines below manage_buffer more readable.
*/
    naxes = bufs[tno].naxis;
    for( d=0; d<=naxes; d++ ) {
    imgsaxlen[d]   =imgs[tno].axlen[d];   bufsaxlen[d]   =bufs[tno].axlen[d];
    imgscubesize[d]=imgs[tno].cubesize[d];bufscubesize[d]=bufs[tno].cubesize[d];
    imgsblc[d]     =imgs[tno].blc[d];     bufsblc[d]     =0;
    imgstrc[d]     =imgs[tno].trc[d];     bufstrc[d]     =bufs[tno].axlen[d]-1;
    imgscsz[d]     =imgscubesize[d-1];    bufscsz[d]     =bufscubesize[d-1];
    imgslower[d]   =imgs[tno].lower[d];
    imgsupper[d]   =imgs[tno].upper[d];
    axnumr[d]      =axnum[tno][d];
    reverses[d]    =reverse[tno][d];
    }
    for( d=1; d<=naxes; d++ ) inv_axnumr[ axnumr[d] ] = d;
}

int limprint( char * string, int lower [], int upper []);

void set_bufs_limits( tno, virpix_off )
int tno;
int virpix_off;
{
/* This gets some information about the virtual-cube buffer and the ranges
   of coordinates.
   First it figures out which range of pixeloffsets from the virtual cube
   fits in the buffer (from bufs[tno].filfir to bufs[tno].fillas).
   bufs[tno].fillas is found by figuring out what the pixeloffset of the
   last complete subcube was. It is limited by the size of the virtual cube.
   It also finds a pointer to the element in the buffer that will
   correspond to the first element of the virtual cube that is present
   (bufs[tno].bufstart).
   Next it finds the lower and upper limits that will ever be found for
   each coordinate: bufs.lower and bufs.upper. This is used later to limit
   the number of transformations by skipping over ranges where no buffer
   pixels will be found. Its main use is to be able to take shortcuts, to
   reduce the overhead.
*/
    void find_block();

    if( virpix_off == -1 ) return;

    bufs[tno].filfir   = virpix_off;
    bufs[tno].bufstart = imgs[tno].number*buffersize;
    bufs[tno].fillas   =
        (int)( (bufs[tno].filfir+buffersize) / bufscubesize[dimsub[tno]] )
                                             * bufscubesize[dimsub[tno]] - 1;
    if( bufs[tno].fillas > bufscubesize[ naxes ] - 1 )
        bufs[tno].fillas = bufscubesize[ naxes ] - 1;

    find_block( bufs[tno].filfir, bufs[tno].fillas,
                bufs[tno].lower, bufs[tno].upper,
                bufsaxlen, bufscubesize, bufsblc, bufstrc, naxes );
    for( dim=1; dim<=naxes; dim++ ) {
        imgs[tno].lower[axnumr[dim]]=bufs[tno].lower[dim]+imgsblc[axnumr[dim]];
        imgs[tno].upper[axnumr[dim]]=bufs[tno].upper[dim]+imgsblc[axnumr[dim]];
    }
    for( dim=1; dim<=naxes; dim++ ) {
        imgslower[dim] = imgs[tno].lower[dim];
        imgsupper[dim] = imgs[tno].upper[dim];
    }

    if(itest) { printf( "fill %s buffer; will be full after %d pixels\n",
                         words[MODE], bufs[tno].fillas - bufs[tno].filfir + 1 );
                limprint( "vircub", bufs[tno].lower, bufs[tno].upper ); }
}

int get_last( start, finis )
int  start, finis;
{
/* This routine figures out how many elements will fit into the buffer:
   the lower of the amount needed and the size of the buffer. It returns
   the fileoffset of the last element that fits.
*/
    int allocate;
    if( finis-start+1 > buffersize ) { allocate = buffersize;    }
    else                             { allocate = finis-start+1; }
    return( start + allocate - 1 );
}

int check_do_io( tno, start, last )
int  tno;
int  start, last;
{
/*
   This routine checks if it is really necessary to read or write data
   from/to disk.
   It calculates the lowest and highest coordinate value that will ever
   be encountered. A comparison is done with the lowest and highest that
   might go into the buffer. If at least part of "the subcube selected
   from the inputcube" and "the subcube from the virtual cube that will
   fit into the buffer" overlap, a disk-i/o is required, as there will be
   at least one element of the virtual-cube-buffer read or written. This
   mainly comes into play when the buffer is smaller than an image plane
   and z-profiles must be read/written.
*/
    void find_block();
    int  imgslow[ARRSIZ], imgsupp[ARRSIZ];
    int  do_io;

    find_block( start, last, imgslow, imgsupp,
                imgsaxlen, imgscubesize, imgsblc, imgstrc, naxes );
    do_io = FALSE;
    for( dim=1; dim<=naxes && !do_io; dim++ ) {
        do_io = (  bufs[tno].lower[ dim ] <= imgsupp[ axnumr[dim] ]  )  ||
                (  bufs[tno].upper[ dim ] >= imgslow[ axnumr[dim] ]  );
    }
    if(itest) limprint( "i-ocub", imgslow, imgsupp );
    return do_io;
}

void find_block( start, last, lower, upper, axlen, cubesize, blc, trc, naxis )
int start, last;
int lower[], upper[];
int axlen[], cubesize[], blc[], trc[], naxis;
{
/* Figures out from the first and last pixeloffset what the lowest and
   highest coordinate value are that could possibly be encountered. To do
   this it calculates the first coordinate value in the 'plane' (e.g. how
   many lines fit into z*(#lines/plane) and subtracts this from the
   non-modulo calculated coordinate value of the last pixeloffset. Then
   it checks if the next 'plane' was reached. If not, the coordinate
   limits are determined by the coordinate values themselves, else they
   are the lower/upper ends of the ranges.
*/
    void p2c();
    int  bot, top;
    int  strcoo[ARRSIZ], fincoo[ARRSIZ];
    p2c( start, axlen, cubesize, naxis, strcoo );
    p2c( last,  axlen, cubesize, naxis, fincoo );
    for( dim=1; dim<=naxis; dim++ ) {
        bot = (int)( start / cubesize[dim] ) * axlen[dim];
        top = (int)( last  / cubesize[dim-1] ) - bot;
        ( ( top >  trc[dim] ) ? (  lower[dim] = blc[dim]  )
                              : (  lower[dim] = strcoo[dim]   ) );
        ( ( top >= trc[dim] ) ? (  upper[dim] = trc[dim]  )
                              : (  upper[dim] = fincoo[dim]   ) );
    }
}

int transform_back( pix_off )
int pix_off;
{
/* Transforms an virtual-cube pixeloffset into an input pixeloffset.
*/
    int  inpcoo, vircoo;
    int  result, axnr;
    result = 0;
    for( dim=1; dim<=naxes; dim++ ) {
        axnr   = axnumr[dim];
        vircoo = ( pix_off / bufscubesize[ dim-1 ] ) % bufsaxlen[ dim ];
        inpcoo = vircoo + imgsblc[ axnr ];
        result += imgscubesize[axnr-1] * inpcoo;
    }
    return ( result );
}

int c2p( coords, cubesize, naxis )
int coords[];
int cubesize[];
int naxis;
{
/* Converts a pixeloffset into a list of coordinates
*/
    int pix_off; pix_off=0;
    for( d=1; d<=naxis; d++ ) pix_off += cubesize[d-1] * coords[d];
    return ( pix_off );
}
void p2c( pix_off, axlen, cubesize, naxis, coords )
int pix_off;
int axlen[], cubesize[];
int naxis;
int coords[];
{
/* Converts a list of coordinates into a pixeloffset
*/
    for( d=1; d<=naxis; d++ ) coords[d] = ( pix_off/cubesize[d-1] ) % axlen[d];
}

int mkread_c(char * handle, int mode, int *flags, int offset,int n,int nsize);

/******************************************************************************/
/*                                                                            */
/* The routines that do the i-o                                               */
/*                                                                            */
/******************************************************************************/

void fill_buffer( tno, start, last )
int tno;
int start, last;
{
    int length, begin;
    int bufstart, *buf;
    int i,iostat;

    nio++;
    if(itest) printf( "Read %d values: %d to %d\n", last-start+1, start, last );

    if( !imgs[tno].nocopy ) bufstart=0; else bufstart=bufs[tno].bufstart;
    length = H_REAL_SIZE * ( last - start + 1 );
    begin  = H_REAL_SIZE * start + ITEM_HDR_SIZE;
/*  hgrab_c(  imgs[tno].itno,(char *)(buffer+bufstart),begin,length,&iostat );*/
    hreadr_c( imgs[tno].itno,(char *)(buffer+bufstart),begin,length,&iostat );
    check(iostat);
    length = last - start + 1;
    begin  = start;
    if( imgs[tno].mask ) {
       mkread_c( imgs[tno].mask,1,mbuffr+bufstart,begin,length,length );
    } else {
       buf = mbuffr+bufstart;
       for (i=0; i<length; i++)
	 buf[i] = FORT_TRUE;
    }

    if(vtest){ for( i=0; i<last-start+1; i++ ) {
        p2c( i+start, imgsaxlen, imgscubesize, naxes, tcoo );
        *(buffer+i) = (float)( tcoo[1] + 1000*tcoo[2] + 1000000*tcoo[3] ); }}
}

void mkwrite_c(char * handle,int mode,int * flags,int offset,int n, int nsize);

void empty_buffer( tno, start, last )
int tno;
int start, last;
{
    int length, begin;
    int bufstart;
    int iostat;

    nio++;
    if(itest) printf( "Write %d values: %d to %d\n", last-start+1,start,last );

    if( !imgs[tno].nocopy ) bufstart=0; else bufstart=bufs[tno].bufstart;
    length = H_REAL_SIZE * ( last - start + 1 );
    begin  = H_REAL_SIZE * start + ITEM_HDR_SIZE;
/*  hdump_c(  imgs[tno].itno,(char *)(buffer+bufstart),begin,length,&iostat );*/
    hwriter_c(imgs[tno].itno,(char *)(buffer+bufstart),begin,length,&iostat );
    if( imgs[tno].lastwritten < last ) imgs[tno].lastwritten = last;
    check(iostat);
    if( imgs[tno].mask ) {
       length = last - start + 1;
       begin  = start;
       mkwrite_c( imgs[tno].mask,1,mbuffr+bufstart,begin,length,length);
    }
    
}

/******************************************************************************/
/*                                                                            */
/* Copy from the i-o buffer to the xyzio-buffer, the core of the routine      */
/*                                                                            */
/******************************************************************************/

void loop_buffer( tno, start, last, newstart )
int  tno;
int  start, last;
int *newstart;
{
/* This routine checks all pixels in the in/out buffer and puts them at
   the appropriate place in the virtual-cube buffer, or it takes them out
   of the virtual-cube buffer.
   In principle, for each pixel the input/output-pixeloffset is converted
   into a virtual-pixeloffset and this is checked against the range of
   elements in the virtual-cube buffer. For efficiency the conversion is
   not done explicitly. What is done is to loop over the x-coordinate of
   the in/out buffer and increment x, the pointer to the x-axis of the
   in/out buffer, and the virtual-cube offset until x flows over its
   maximum. This limits the number of operations in the innermost loop to
   only 11. Meanwhile the virtual-cube-offset is checked and data is
   copied as found. If x flowed over, the coordinates are recalculated
   and x is set to its lower limit. The other coordinates are either
   increased by one or, if they reached their upper limit, set to their
   lower limit. This allows for shortcuts to be taken if a region was
   specified and does not give a substantial overhead if no region was
   specified. From the new coordinates the pointer to the in/out buffer
   (bufptr) and virtual-cube-buffer (bufoff) are recalculated. All this
   continues until the in/out buffer is exhausted.
   Before starting to work on filling a buffer to write, a check is made
   whether is is necessary to set all elements to zero (if the output
   region is smaller than the output cube and the output cube did not
   yet exist) or to put the old values in the buffer (if the output cube
   existed and a transposition or region-selection was done).
   The speed of this algorithm is the limiting factor in the speed of
   filling/emptying buffers. Even more so than the size of the buffers.
   So anyone who can come up with a faster method of doing the same
   (transposing cubes) will be able to improve efficiency.
*/
    int    buffir, buflas, bufoff;
    float *bufptr, *bufend;
    int   *mbufpt;
    void   fill_buffer();
    int    to_in;
    int    filoff, coords[ARRSIZ];
    void   p2c();

    *newstart = last + 1;
    if( imgs[tno].nocopy ) return;

    buffir = bufs[tno].bufstart;
    buflas = bufs[tno].fillas - bufs[tno].filfir + buffir;

    bufptr = buffer;
    bufend = buffer + last - start;
    mbufpt = mbuffr;

    if( MODE==PUT ) {
        if( imgs[tno].lastwritten <= last ) {
            if( imgs[tno].lastwritten >= start ) {
                fill_buffer( tno, start, imgs[tno].lastwritten );
                bufptr = buffer + imgs[tno].lastwritten - start + 1;
                mbufpt = mbuffr + imgs[tno].lastwritten - start + 1;
            }
            if(itest) printf("zero buffer 0\n");
            while( bufptr <= bufend ) { *bufptr++ = 0; *mbufpt++ = FORT_TRUE; }
        } else {
            fill_buffer( tno, start, last );
        }
        bufptr = buffer;
        mbufpt = mbuffr;
    }

    p2c( start, imgsaxlen, imgscubesize, naxes, coords );
    bufoff = -bufs[tno].filfir + bufs[tno].bufstart;
    for( d=1; d<=naxes; d++ )
        bufoff += bufscsz[inv_axnumr[d]] * ( coords[d] - imgsblc[d] );

    to_in = ( MODE==GET );

    while( bufptr <= bufend ) {
        if( coords[1] <= imgsupper[1] ) {
    /*$$if(rtest)testsearch(1,coords,start+bufptr-buffer,bufoff-buffir);$$*/
            if( buffir <= bufoff && bufoff <= buflas ) {
                if( to_in ) { *(buffer+bufoff) = *bufptr;
                              *(mbuffr+bufoff) = *mbufpt; }
                else        { *bufptr = *(buffer+bufoff);
                              *mbufpt = *(mbuffr+bufoff); }
        /*$$if(itest||rtest)nfound++; if(rtest)$$*/
        /*$$printf(" found element %d; value %f %d",bufoff,*bufptr,*mbufpt);$$*/
            }
            /*$$if(rtest) printf("\n");$$*/
            coords[1]++;
            bufoff += bufscsz[inv_axnumr[1]];
            bufptr++; mbufpt++;
        } 
        if( coords[1] > imgsupper[1] ) {
            /*$$if(rtest) testsearch(0,coords,0,0);$$*/
            coords[1] = imgslower[1];
            d=2; while( d<=naxes ) {
                if( coords[d] == imgsupper[d] || coords[d] == imgstrc[d] )
                     { coords[d] = imgslower[d]; }
                else { coords[d]++; break;       }
                d++;
            }
            if( d > naxes ) break;
            /*$$if(rtest) testsearch(2,coords,0,0);$$*/
            filoff = -start;
            bufoff = -bufs[tno].filfir + bufs[tno].bufstart;
            for( d=1; d<=naxes; d++ ) {
                filoff += imgscsz[d]             *   coords[d];
                bufoff += bufscsz[inv_axnumr[d]] * ( coords[d] - imgsblc[d] );
            }
            bufptr = buffer + filoff;
            mbufpt = mbuffr + filoff;
        }
    }
    if(itest||rtest) printf( "found %d elements\n", nfound );
    *newstart = bufptr - buffer + start;
}

/******************************************************************************/

void zero( bl_tr, tno )
int bl_tr;
int tno;
{
/* This initializes parts of an output datacube that were not accessed
   because the new dataset has a blc and trc inside the full cube.
   It is called with bl_tr==1 just before the put buffer is first set up,
   and with bl_tr==2 just before the close.
*/
    int    start, last, finis;
    int    c2p(), get_last();
    float *bufptr, *bufend;
    int   *mbufpt;
    void   empty_buffer();

    if(        bl_tr == 1 ) {
        start = 0;
        finis = c2p( imgsblc, imgscubesize, naxes ) - 1;
        finis = imgscubesize[naxes] - 1;
    } else if( bl_tr == 2 ) {
        start = c2p( imgstrc, imgscubesize, naxes ) + 1;
        finis = imgscubesize[naxes] - 1;
    }
    while( start <= finis ) {
        last = get_last( start, finis );
        bufptr = buffer;
        bufend = buffer + last - start;
        mbufpt = mbuffr;
        if(itest) printf("zero part of buffer 0\n");
        while( bufptr <= bufend ) { *bufptr++ = 0.; *mbufpt++ = FORT_FALSE; }
        empty_buffer( tno, start, last );
        start = bufptr - buffer + start;
    }
}

/******************************************************************************/
/******************************************************************************/

int testprint( tno, virpix_off, virpix_lst )
int tno, virpix_off, virpix_lst;
{   int vircoo[ARRSIZ];
    int inpix_off, c2p(); void p2c();
    int naxes;
    naxes=imgs[tno].naxis;
    p2c( virpix_off, bufs[tno].axlen, bufs[tno].cubesize, naxes, vircoo );
    for( dim=1; dim<=naxes; dim++ )
         tcoo[dim] = vircoo[ inv_axnumr[dim] ] + imgs[tno].blc[dim];
    inpix_off = c2p( tcoo, imgs[tno].cubesize, naxes );
    printf( "coo:    " );
            for( dim=1; dim<=naxes; dim++) printf( "%4d ", tcoo[dim] );
    printf( "  offset: %10d\n", inpix_off );
    printf( "vircoo: " );
            for( dim=1; dim<=naxes; dim++) printf( "%4d ", vircoo[dim] );
    printf( "  offset: %20d\n", virpix_off );
    if( virpix_off == virpix_lst ) {
        printf( "%s copied element %d\n", words[MODE],
                 virpix_off+bufs[tno].bufstart );
    } else {
        printf( "%s copied %d elements starting at %d\n", words[MODE],
                 virpix_lst-virpix_off+1, virpix_off+bufs[tno].bufstart );
    }

    return 0;
}

int limprint( string, lower, upper )
char *string; int lower[], upper[];
{
    printf( "%s:", string );
    printf( "  lower" ); for( d=1; d<=naxes; d++ ) printf( " %d", lower[d] );
    printf( ": upper" ); for( d=1; d<=naxes; d++ ) printf( " %d", upper[d] );
    printf( "\n");

    return 0;
}

int testsearch( callnr, coords, filoff, viroff )
int callnr; 
int coords[];
int filoff, viroff;
{
    if( callnr == 2 ) printf( " -> " );
    for( d=1; d<=naxes; d++ ) printf("%d ", coords[d] );
    if( callnr == 1 ) printf( "  filoff %d viroff %d", filoff, viroff );
    if( callnr == 2 ) printf( "\n" );

    return 0;
}



/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/* Text for the userguide */
/*
To read or write a MIRIAD dataset the following set of routines can be used.

      xyzopen(  tno, name, status, naxis, axlen )
      xyzclose( tno )
      xyzsetup( tno, subcube, blc, trc, viraxlen, vircubesize )
      xyzs2c(   tno, subcubnr, coords )
      xyzc2s(   tno, coords, subcubenr )
      xyzread(  tno, coords,  data, mask, dimdata )
      xyzpixrd( tno, pixelnr, data, mask )
      xyzprfrd( tno, profinr, data, mask, dimdata )
      xyzplnrd( tno, planenr, data, mask, dimdata )
      xyzwrite( tno, coords,  data, mask, dimdata )
      xyzpixwr( tno, pixelnr, data, mask )
      xyzprfwr( tno, profinr, data, mask, dimdata )
      xyzplnwr( tno, planenr, data, mask, dimdata )

xyzopen opens the dataset and readies it for reading/writing. 'name' is the
name of the dataset. 'status' can be either "old" or "new", depending on
whether an existing dataset is opened or a new one must be created. For old
datasets naxis gives the dimension of array axlen on input and the dimension
of the dataset on output. On output axlen contains the length of the axes.
For new datasets naxis and axlen specify the number of axes and their length.

xyzclose closes the dataset.

The rest of the xyz routines can be used to read or write an arbitrary
subcube in the dataset in a manner that minimizes disk-i/o. To do this,
the datacube axes are named 'x', 'y', 'z', 'a', 'b', etc. 'x' may be RA
or DEC or velocity or anything else, but it is the first axis.

The xyzsetup subroutine is used to define a subcube in the dataset. There are
many subcubes in a dataset. They have axes with "varying coordinates" and axes
with "fixed coordinates". With n "varying coordinates" the subcube is
n-dimensional, and its position in the original cube is given by the "fixed
coordinates". The subcubes are also ordered, along the "fixed coordinates".
E.g., for profiles in the 'z' direction, the first subcube has (x=1,y=1), the
second has (x=2,y=1), on to (x=axlen(1),y=1) and then (x=1,y=2) etc, etc.

For datasets that must be read, the 'subcube' variable of xyzsetup specifies
which axes from the original cube have "varying coordinates"; e.g. 'z' for
profiles the z-direction, or 'xy' for image planes. It is also allowed to
transpose axes: e.g. 'zx' (which would usually correspond to making a vel-RA
plane). To understand the meaning of 'subcube' for datasets that must be written
a little explanation is in order: the xyz routines produce a "virtual cube",
one that never actually is written on disk or resides in memory, but which is
conceptually useful. In this virtual cube the axes are ordered such that the
ones with "varying coordinates" become the 'x', 'y' etc axes, and the ones with
"fixed coordinates" form the rest. So, if 'subcube' was 'z', a profile along
the 'x'-axis of the virtual cube contains the datavalues on a profile along the
'z'-axis of the input cube. The 'y' and 'z' axes of the virtual cube were the
'x' and 'y' axes of the original cube, respectively. For writing a dataset, the
'subcube' variable gives the correspondence between the axes of the virtual
cube and the output cube. E.g., if 'subcube' is 'z', this means that the first 
('x') axis of the virtual cube is the 'z'-axis of the output cube, and the 'y'
and 'z' axes of the virtual cube correspond to the 'x' and 'y' axes of the
output cube, respectively.

Preceding an axisname with a '-' results in mirror-imaging the input or output
data for that axis.

The blc and trc variables of xyzsetup give the bottom left and top right
corner of the part of the image cube to be worked on. The first naxis
elements of blc and trc are used. For reading, this is the region to be read,
for writing it is the region to be written. In the latter case, if the output
dataset did not yet exist and the region is smaller than the total cubesize
given in xyzopen, the outside-region is automatically set to zero.

The viraxlen and vircubesize variables of xyzsetup give some information
about the virtual cube: the axis lengths and the 'cubesizes'. 'cubesize(1)' is
the number of pixels in a profile, 'cubesize(2)' is the number of pixels in a
plane, 'cubesize(3)' is the number of pixels in a cube, etc. So, for a 3-d
input cube, 'cubesize(3)' gives the total number of pixels to work on.

The subroutine xyzs2c can be used to obtain the values of the "fixed
coordinates" for a given subcube number. The first element of the array coords
then corresponds to the first "fixed coordinate" value, etc. E.g., for profiles
in the 'z'-direction, coords(1) is the 'x'-position, coords(2) the 'y'-position.
Subroutine xyzc2s does the inverse operation.

xyzread, xyzpixrd, xyzprfrd and xyzplnrd do the actual reading of data. xyzread
takes as input the "fixed coordinate" values and returns the subcube in the
1-dimensional array data. The other 3 routines read a single pixel, a single
profile and a single plane, respectively. In each case the array data (whose
dimension is transferred to the subroutines in the variable dimdata) should be
large enough to hold the entire requested subcube. The logical array mask is
used to indicate if datapixels were undefined (this is not yet implemented).
mask=TRUE means the pixel is OK; FALSE means it is undefined.
The write routine works in the same manner.
If the program wants to loop over pixels or profiles, use of xyzs2c and xyzread
becomes less efficient than use of xyzpixrd or xyzprfrd. In fact, for looping
over pixels, the xyzs2c-xyzread combination is about 10 times less efficient
than xyzpixrd. This is because with xyzs2c and xyzread the pixelnumber is first
converted to a coordinate with xyzs2c and then converted back to a pixelnr in
xyzread, while xyzpixrd avoids this overhead.

A typical call sequence using the xyz routines to work on profiles in the
z-direction would be:

      call xyzopen(  tno1, name1, 'old', naxis, axlen )
      call xyzopen(  tno2, name2, 'new', naxis, axlen )
      call headcopy( tno1, tno2, axnum, naxis )          ! axnum(i)=i
      call boxinput( 'region', name, boxes, maxboxes )
      call boxinfo(  boxes, naxis, blc, trc )
      call xyzsetup( tno1, 'z', blc, trc, viraxlen, vircubesize )
      call xyzsetup( tno2, 'z', blc, trc, viraxlen, vircubesize )
      nprofiles = = vircubesize(naxis) / viraxlen(1)
      do profile = 1, nprofiles
          call xyzprfrd( tno1, profile, data, mask, dimdata )
          call work_on_profile( data, mask, dimdata )
          call xyzprfwr( tno2, profile, data, mask, dimdata )
      enddo

A warning is in order: each call to xyzsetup causes all internal buffers to be
lost completely, so xyzsetup should be called for all datasets before starting
to work on them. Output buffers are flushed before the buffers are lost,
however.

The overhead introduced by the calculations done by the xyz routines is shown
below. These were calculated using a testprogram that was complete but for
actually doing something with the data and reading them from disk. The first
number gives the times when using xyzpixrd, xyzprfrd and xyzplnrd, the second
when using xyzs2c and xyzread. The overhead does not change with changing
buffersize, but the number of disk-i/o's does. In a test using xyzprfrd and
xyzprfwr on a 128^3 cube, with a 4Mb buffer, it took 120s to copy the input
file using these routines, and 80s with a unix cp. With a 2Mb buffer the copy
took 120s too, even though the number of i-o's increased from 12 to 22.

buffer of 524288 (2Mb): 1/2th of 128^3 cube; 1/16th of 256^3 cube
           cubesize     32^3         64^3        128^3        256^3
pixels      time(s)   0.3(  2.6)   1.6( 20.6)  12.4(170.2)  98.2(1396.6)
             n_i/o       1            2           13           97
x profiles  time(s)   0.1(  0.2)   0.6(  0.9)   4.2(  5.2)  31.2( 35.5)
             n_i/o       1            1            2           16
y profiles  time(s)   0.4(  0.4)   2.2(  2.5)  16.8( 17.9) 129.8(134.0)
             n_i/o       1            1            2           16
z profiles  time(s)   0.4(  0.4)   2.4(  2.5)  16.7( 17.7) 129.5(133.7)
             n_i/o       1            1            4          256
xy planes   time(s)   0.2(  0.1)   0.6(  0.5)   3.9(  4.0)  30.1( 30.1)
             n_i/o       1            1            2           16
yx planes   time(s)   0.4(  0.4)   2.2(  2.2)  16.4( 16.5) 128.6(128.7)
             n_i/o       1            1            2           16
xz planes   time(s)   0.3(  0.3)   2.2(  2.1)  16.4( 16.4) 128.4(128.4)
             n_i/o       1            1            4          256
zx planes   time(s)   0.3(  0.4)   2.2(  2.2)  16.5( 16.4) 128.2(128.3)
             n_i/o       1            1            4          256
yz planes   time(s)   0.4(  0.3)   2.1(  2.2)  16.9( 16.8) 157.4(157.4)
             n_i/o       1            1            4          256
zy planes   time(s)   0.4(  0.4)   2.2(  2.1)  16.9( 16.8) 157.4(157.3)
             n_i/o       1            1            4          256

           cubesize   128*128*112  256*256*64
z profiles  time(s)     15.1         34.5
             n_i/o         8           32

*/


/*
Number of operations per call to xyz routines, 3-d cube:

xyzs2c:  82
xyzr/w:  82+3n
xyzpix:  15
xyzprf:  36+3n
xyzpln:  36+3n

          pix/prf/pln    xyzs2c & read     ratio
pixels    (15)N^3        (164)N^3          15/164
profiles  (36+3N)N^2     (164+3N)N^2       (36+3N)/(164+3N)
planes    (36+3N^2)N     (164+3N^2)N       (36+3N^2)/(164+3N^2)

             32^3    64^3   128^3   256^3   512^3
pixels      0.091
profiles    0.508   0.640   0.766   0.863   0.925
planes      0.960   0.990   0.997   0.999   1.000

*/


/******************************************************************************/
/******************************************************************************/
/******************************************************************************/






