/************************************************************************/
/* Bugs:								*/
/*    God only knows how many inconsistencies and bugs are left!	*/
/*									*/
/*  History:								*/
/*  rjs ??????? Original version.					*/
/*  rjs 16aug89 Fixed bug, in uvread, of initialisation.		*/
/*  rjs  2oct89 Fixed need for nschan,restfreq for wide-only file.	*/
/*  rjs 18oct89 UV data selection code. Better planet treatment. Tidied	*/
/*		uvread.							*/
/*  rjs  1nov89 Fixed bug with the trueval array.			*/
/*  rjs  2nov89 Fixed bug when calculating velocity channels, and the	*/
/*		"amplitude", "phase", "real" and "imaginary" linetypes.	*/
/*  rjs  7nov89 Fixed bug in uvread_velocity, when velocity increment	*/
/*		is negative. Better error checking in UVSET. Bug with	*/
/*		window selection.					*/
/*  rjs  8nov89 Allowed you to select just 1 visibility in uvselect.	*/
/*  rjs  9nov89 Allow negative step parameter for velocity linetype.	*/
/*		Check for variable names greater than MAXNAM chars.	*/
/*  rjs 13nov89 Made uvnext so that it handles OLD as well as NEW files,*/
/*		as advertised.						*/
/*  rjs  2feb90 Added uvoverride, and modified uvscan, to implement the */
/*		capacity to override values of variables.		*/
/*  rjs  7feb90 If not linetype is set, uvread_init checks for both corr*/
/*		and wcorr before deciding on the default linetype. Added*/
/*		lots of comments!					*/
/*  rjs 13feb90 Modified uvinfo to handle object='bandwidth'.		*/
/*  rjs 12mar90 Significant mods to the uv selection stuff. Modified	*/
/*		uvinfo to handle object='frequency'.			*/
/*  rjs 23mar90 Fixed minor bug in uvselect.				*/
/*  rjs 27mar90 Fixed a bug where the UVF_COPY and UVF_UPDATE flags	*/
/*		where not properly set, when uvselect was skipping some	*/
/*		data.							*/
/*  rjs/bpw 6apr90 Greater control over amplitude selectoin.		*/
/*  pjt  8apr90 Made the CHECK macro more user understandable. rjs      */
/*		re-installed the changes into this version.		*/
/*  rjs 10apr90 Wide flags, uvwread and uvwwrite. Added first_chan and	*/
/*		first_wind to the uv structure.				*/
/*  rjs 23apr90 Incorporated and enhanced pjt changes. Corrected a	*/
/*		comment.						*/
/*  rjs 24apr90 Fixed bug in uvread_select for RA selection.		*/
/*  rjs 25apr90 Enhancements to uvscan.					*/
/*  pjt 14aug90 Peter's TESTBED code.					*/
/*  rjs 16oct90 Improved uv_override somewhat. Changed uv selection	*/
/*		so that ant1 can equal ant2 (autocorrelation data).	*/
/*  rjs 17oct90 Handle selection by polarization and "on".		*/
/*  rjs  2nov90 Improved uv_override somewhat more.			*/
/*  rjs 14dec90 Fixed bug in uvread_select, when dra and ddec parameters*/
/*              are missing, but dra and ddec selection used.           */
/*  rjs  8feb91 Fixed integer overflow problems in uvread_amp, and 	*/
/*		amp flagging of H_CMPLX data.				*/
/*  rjs 11feb91 Added PJT's "testbed" (so he can sleep in comfort?),    */
/*		uvselect for time treats a "Julian date" of less than 1 */
/*		as specifying the current day. Better checking for	*/
/*		uninitialised variables. Added the uvmark routine. Some	*/
/*		changes in uvread to fix a potential bug when wcorr and	*/
/*		corr do not always appear in the same record.		*/
/*		Allow dra,ddec selection, when dra,ddec missing.	*/
/*		Yet more work on uv_override. Corrected select=inc(1).	*/
/*		Bug when selecting non-existent channels. Added ability */
/*		to get sfreq from uvinfo.				*/
/*  pjt 28feb91 Added record count to TESTBED - declared uvopen_c() etc */
/*  rjs  1mar91 Corrected bug in uvmark.				*/
/*  rjs  5mar91 Fixed error in calculation of uvinfo(...,'sfreq',...)	*/
/*		uvwrite writes out u,v,t,bl only when needed.		*/
/*		Changed definitions of TESTBED offsets.			*/
/*  rjs 11mar91 Write out variables, in uvputvr, only when they really  */
/*		change.	Fixed two more bugs, which lint discovered.	*/
/*  rjs 19mar91 Added some more comments, and discovered a bug in uvinfo*/
/*  rjs 21mar91 Improved error message in uvgetvr.			*/
/*  rjs 22mar91 Just comments.						*/
/*  rjs 27mar91 More comments. Routine uvwflgwr.			*/
/*  rjs 16apr91 Added shadowing to uvselect. Always writes wcorr in	*/
/*		in uvwwrite.						*/
/*  rjs 18apr91 Change to uvset(...'corr'...)				*/
/*  rjs 29may91 Corrected planet scaling, in uvread_updated()		*/
/*  rjs 12jun91 Changed calculation of "restfreq" of wide channels.	*/
/*		What sense does it make?				*/
/*  rjs 19jun91 Corrected shadowing calculation.			*/
/*  rjs  5aug91 Improved some error messages a bit.			*/
/*  mjs 05aug91 Replaced hardcoded MAXANTS by maxdimc.h MAXANT		*/
/*  rjs 09oct91 Uvwread returns nread=0 if there are no wides, to	*/
/*		appease pjt.						*/
/*  rjs 22nov91 select=auto selects autocorrelations only.		*/
/*  rjs 13dec91 Added uvopen(..,'append') status.			*/
/*  rjs  9dec91 Minor enhancement to uvwrite, to handle case of		*/
/*		preamble variables being written somewhere else.	*/
/*  rjs 10jan92 Slight mod to uvwrite, to account for lgm program.	*/
/*  rjs 25mar92 Selection based on the frequency of the first channel   */
/*		and the source name.					*/
/*  rjs  6apr92 Added specifying flags in runs form. The Convex found	*/
/*		found where I had forgotten a semi-colon.		*/
/*  rjs 12jun92 select=window makes sense for line=channel.		*/
/*  rjs 10jul92 one of the checks for linetype validity was incorrectly */
/*		too stringent.						*/
/*  mchw 29jul92 removed step<width check for channel linetype.         */
/*  rjs   6aug92 Added uvvarini,uvvarset,uvvarcpy,uvvarupd.		*/
/*  rjs  17aug92 Deleted uvmark. Corrected minor bug in uvputvr.	*/
/*  rjs  20aug92 Appending when there is no data works.			*/
/*  rjs  16sep92 Check validity of window selection.			*/
/*  rjs   2nov92 Doc only.						*/
/*  rjs  22nov92 Better defaults when required variables not present in */
/*		 selection.						*/
/*  rjs   9dec92 Fixed bug in shadowing, introduced 12 jun?		*/
/*  rjs  24dec92 Doc change only, at pjt's request.			*/
/*  rjs  10jan93 Add variance calculation to uvinfo. Get rid of int2.	*/
/*  rjs  12feb93 uvrdvr{i,r,d} now interconvert between int,float,double*/
/*  rjs  12feb93 uvselection of ra and dec cope with them being either  */
/*		 float or double.					*/
/*  rjs   3mar93 uvflush -- the way of the future.			*/
/*  rjs  16mar93 Always write vartable for new file if nvar==0.         */
/*  rjs  29mar93 Changed formula for calculation of variance.		*/
/*  rjs  11may93 Get rid of abs() function, which accidently stayed.    */
/*  rjs  13may93 Fix bug dealing with select=window in			*/
/*		 uvinfo(..,'variance'..)				*/
/*  rjs  21jul93 Divide variance by 2 for Stokes parameters.		*/
/*  mjs  26aug93 cast strlen (elim ansi warning on solaris).		*/
/*  rjs  16nov93 Handle planet scaling when there are a mix of planets  */
/*		 and other sources.					*/
/*  rjs  23dec93 Added uvinfo(..,'felocity'..), and removed a few for   */
/*		 wideband channels that did not make much sense.	*/
/*  rjs  05jan94 Trivial doc changes only.				*/
/*  rjs  21jul94 Slightly better planet handling.			*/
/*  rjs   1aug94 Internal u-v-w re-calculation. Changes to the shadowing*/
/*		 code.							*/
/*  rjs  30sep94 Fixed planet bug, which I must have introduced recently*/
/*  rjs  21oct94 Fix misleading error message.				*/
/*  rjs   6nov94 Change item and variable handle to an integer.		*/
/*  rjs  30nov94 Increase size of varnam by 1 char, in uvset_preamble.	*/
/*  rjs   9dec94 Less fussy when w coordinate is needed.		*/
/*  rjs   6jan95 Make buffer for "w" coordinate large enough!		*/
/*  rjs  13jan95 Added pulsar bin selection.				*/
/*  rjs  22feb95 Relax linetype step limitation in uvflgwr.		*/
/*  rjs  17apr96 uv_override can convert between numeric types.		*/
/*  rjs  15may96 Fiddles with roundup macro.				*/
/*----------------------------------------------------------------------*/
/*									*/
/*		Handle UV files.					*/
/*									*/
/*	A uv data set consists of the following data items:		*/
/*	visdata	 -- Varable stream. This data stream consists		*/
/*		of a stream of "records", each record giving either the	*/
/*		length or value of a "variable". Variables are anything	*/
/*		measured during an observation, and which can be	*/
/*		vary during the observation. These include uv		*/
/*		coordinate, correlation data, system temperature, time,	*/
/*		etc. Each record starts with 4 bytes which gives a	*/
/*		number identifying the variable, and indicates whether	*/
/*		this record give the variable's value or length (in	*/
/*		bytes).	The identidying numbers range from 0 to N-1	*/
/*		(for a file with info on N variables).			*/
/*	vartable -- Table of variable names and types. This is a text	*/
/*		item which which maps the number associated with a 	*/
/*		variable into some more	useable name. It also gives the	*/
/*		type (real, integer*2, double precision, etc) of	*/
/*		variables.						*/
/*	flags	 -- Flagging information. Each correlation has a flag	*/
/*		to indicate whether it is good or not. Flagging info is	*/
/*		written into an item consisting of a bit map.		*/
/*									*/
/*  The UV structure							*/
/*  ================							*/
/*  Each open UV data file is described by the UV structure, which in	*/
/*  turn contains a number of substructures.				*/
/*									*/
/*  item	This is the item-handle to access the variable		*/
/*		stream.							*/
/*  nvar	The number of different variables in the		*/
/*		variable stream.					*/
/*  offset	Current offset into uv data stream where i/o is		*/
/*		being performed.					*/
/*  tno		The file-handle of the overall uv data-set.		*/
/*  flags	Miscellaneous flags.					*/
/*  callno	This is initially zero, and incremented each call to	*/
/*		uv_scan.						*/
/*  mark	This gives the "callno" relative to which variables are */
/*		deemed to have been updated. i.e. a variable considered */
/*		as having changed if the variables "callno" is greater 	*/
/*		or equal to "mark".					*/
/*  variable	An array of structures defining the variables within the*/
/*		variable data stream.					*/
/*  vhash	A hash table of pointers to VARIABLE structures. This	*/
/*		allows fast access to the a particular variable by name.*/
/*  data_line	A structure (LINE_INFO) describing the data line type.	*/
/*  ref_line	A structure (LINE_INFO) describing the reference line	*/
/*		type.							*/
/*  sel		The uv data selection structure.			*/
/*  corr_flags.handle Handle used by the maskio routines.		*/
/*  corr_flags.offset Offset to the next flag to read in the mask file.	*/
/*  corr_flags.nflags Number of correlation channel flags.		*/
/*  corr_flags.flags  The flags for the last correlation record read.	*/
/*  corr_flags.init   Have they been read?				*/
/*  corr_flags.exists A flag whether the corr flags are believed to exist.*/
/*									*/
/*  Because we know that certain variables are accessed every call to	*/
/*  uvread, we keep pointers to them.					*/
/*									*/
/*	coord		corr		tscale		time		*/
/*	bl		nschan		sfreq		sdf		*/
/*	restfreq	axisrms		dra		ddec		*/
/*	nwide		wcorr		wfreq		veldop		*/
/*	vsource		plmaj		plmin		plangle		*/
/*	ra		dec		pol		on		*/
/*	obsra		obsdec		lst		antpos		*/
/*	antdiam		source		pol				*/
/*									*/
/* The VARIABLE structure						*/
/* ======================						*/
/* This structure describes a single variable from the variable data	*/
/* stream.								*/
/*  buf		Pointer to a buffer containing the current value of the	*/
/*		variable (in the format of the local machine).		*/
/*  name	The name of the variable.				*/
/*  length	The length of the variable (bytes).			*/
/*  flags	Miscellaneous flags.					*/
/*  type	The type of the variable, whether it is I*2,R*4, etc.	*/
/*  index	This gives the index of the variable in the "variable"	*/
/*		array of the UV structure.				*/
/*  callno	The call number to uv_scan when the variable was last	*/
/*		updated.						*/
/*  fwd		Pointer to the next variable. This allows a linked	*/
/*		list to be formed for hashing.				*/
/*									*/
/*----------------------------------------------------------------------*/
#define VERSION_ID "3-Mar-93 rjs"

#define private static

#define CKMS 299792.458
#define PI 3.141592653589793

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "io.h"

#define UVF_COPY	0x01	/* Set if this variable is to be copied by
				   the uvcopy routine. */
#define UVF_UPDATED	0x02	/* Set if noting updates to this variable. */
#define UVF_UPDATED_PLANET  0x04 /* Set if planet things have changed. */
#define UVF_UPDATED_SKYFREQ 0x08 /* Set if sky freq things have changed. */
#define UVF_NEW		0x10	/* Set if its a new uv data set being made. */
#define UVF_APPEND      0x20	/* Set if we are appending to a uv dataset. */
#define UVF_WAVELENGTH	0x40	/* Set if uvread is to convert uv to wavelengths. */
#define UVF_OVERRIDE	0x80	/* Set if the value of this variable is being
				   overriden. */
#define UVF_NOCHECK	0x200	/* Set if UVPUTVR is not to check this variable
				   as to whether it has really changed. */
#define UVF_AUTO	0x400
#define UVF_CROSS	0x800

#define UVF_RUNS	0x1000	/* Does uvwrite receive flags in runs
				   specification? */
#define UVF_INIT	0x2000	/* Set on first call to uvread or uvwrite. */
#define UVF_UPDATED_UVW 0x4000	/* Set if things needed for uvw have changed. */
#define UVF_REDO_UVW	0x8000	/* Set if u-v-w are to be recomputed. */
#define UVF_DOW		0x10000	/* Set if the caller wants w returned. */

#define UV_ALIGN	8
#define UV_HDR_SIZE	4

#define CHECK_THRESH	6
#define HASHSIZE      123
#define MAXVAR	      256
#define MAXNAM		8
#define MAXPRE		9
#define MAXLINE	      128
#define VAR_SIZE	0
#define VAR_DATA	1
#define VAR_EOR		2

#define MK_FLAGS	1
#define MK_RUNS		2

/*----------------------------------------------------------------------*/
/*									*/
/*	A few definitions to coax lint to like my code.			*/
/*									*/
/*----------------------------------------------------------------------*/

char *mkopen_c();
void mkclose_c(),mkflush_c(),mkread_c(),mkwrite_c();
void bug_c(),bugno_c(),rdhdi_c(),rdhda_c(),rdhdr_c(),rdhdd_c();
void wrhda_c(),wrhdi_c(),hdprobe_c();

#define Sscanf (void)sscanf
#define Sprintf (void)sprintf
#define Malloc(x) malloc((unsigned)(x))
#define Realloc(a,b) ((a)==NULL?malloc((unsigned)(b)):realloc((a),(unsigned)(b)))
#define Strcpy (void)strcpy

/*----------------------------------------------------------------------*/
/*									*/
/*	Macros to simplify life.					*/
/*									*/
/*----------------------------------------------------------------------*/

#define BUG(sev,a)   bug_c(sev,a)
#define ERROR(sev,a) bug_c(sev,((void)sprintf a,message))
#define CHECK(x,a) if(x) {  bug_c('w',((void)sprintf a,message)); \
			    bugno_c('f',x);			 \
			 }

#define uvputvra_c(tno,name,value)   \
	uvputvr_c(tno,H_BYTE,name,value,strlen(value))
#define uvputvrj_c(tno,name,value,n) \
	uvputvr_c(tno,H_INT2,name,(char *)(value),n)
#define uvputvri_c(tno,name,value,n) \
	uvputvr_c(tno,H_INT,name,(char *)(value),n)
#define uvputvrr_c(tno,name,value,n) \
	uvputvr_c(tno,H_REAL,name,(char *)(value),n)
#define uvputvrd_c(tno,name,value,n) \
	uvputvr_c(tno,H_DBLE,name,(char *)(value),n)
#define uvputvrc_c(tno,name,value,n) \
	uvputvr_c(tno,H_CMPLX,name,(char *)(value),n)

#define VARLEN(var)  ( (var)->length / external_size[(var)->type] )
#define VARTYPE(var) ( type_flag[(var)->type] )

#define NUMCHAN(var) ((var)->type == H_INT2 || (var)->type == H_REAL ?	\
	(var)->length / (2*external_size[(var)->type]) :		\
	(var)->length /    external_size[(var)->type]  )

#define MYABS(x) ( (x) > 0 ? (x) : -(x) )

/*----------------------------------------------------------------------*/
/*									*/
/*	Types and static variables.					*/
/*									*/
/*----------------------------------------------------------------------*/

static char message[MAXLINE];
static int internal_size[10];
static int external_size[10];
static char type_flag[10];

static char var_data_hdr[UV_HDR_SIZE]={0,0,VAR_DATA,0};
static char var_size_hdr[UV_HDR_SIZE]={0,0,VAR_SIZE,0};
static char var_eor_hdr[UV_HDR_SIZE]={0,0,VAR_EOR,0};


typedef struct variable{
	char *buf,name[MAXNAM+1];
	int length,flength,flags,type,index,callno;
	struct variable *fwd;
		} VARIABLE;

typedef struct varpnt{
	VARIABLE *v;
	struct varpnt *fwd;
		} VARPNT;

typedef struct varhand{
	int tno,callno,index;
	struct varhand *fwd;
	VARPNT *varhd;
		} VARHAND;

#define LINE_NONE	0
#define LINE_CHANNEL	1
#define LINE_WIDE	2
#define LINE_VELOCITY	3
#define LINE_FELOCITY	4

#include "maxdimc.h"
#define SEL_VIS   1
#define SEL_TIME  2
#define SEL_UVN   3
#define SEL_POINT 4
#define SEL_DRA	  5
#define SEL_DDEC  6
#define SEL_INC	  7
#define SEL_RA	  8
#define SEL_DEC	  9
#define SEL_POL  10
#define SEL_ON   11
#define SEL_SRC  12
#define SEL_UV   13
#define SEL_FREQ 14
#define SEL_SHADOW 15
#define SEL_BIN  16

typedef struct {
	int type,discard;
	double loval,hival;
	char *stval;
		} OPERS;

typedef struct {
	int discard,select;
	float loval,hival;
		} AMP;

typedef struct {
	int wins[MAXWIN];
	int first,last,n,select;
		} WINDOW;

typedef struct { double *table;
		 int vhan,nants,missing;
		 } SIGMA2;
		
typedef struct select {
		int ants[MAXANT*(MAXANT+1)/2];
		int selants;
		int maxoper,noper,and;
		WINDOW win;
		AMP amp;
		OPERS *opers;
		struct select *fwd;
		} SELECT;

typedef struct {
	int nants;
	double uu[MAXANT],vv[MAXANT],ww[MAXANT];} UVW;

typedef struct {
	int linetype;
	int start,width,step,n;
	float fstart,fwidth,fstep,*wts;
		} LINE_INFO;

typedef struct {
	char *handle;
	int offset,nflags,*flags,exists,init;
		} FLAGS;

typedef struct {
	int item;
	int nvar,offset,max_offset,saved_nvar,tno,flags,callno,maxvis,mark;
	int presize;
	FLAGS corr_flags,wcorr_flags;
	VARIABLE *coord,*corr,*time,*bl,*tscale,*nschan,*axisrms;
	VARIABLE *sfreq,*sdf,*restfreq,*wcorr,*wfreq,*veldop,*vsource;
	VARIABLE *plmaj,*plmin,*plangle,*dra,*ddec,*ra,*dec,*pol,*on;
	VARIABLE *obsra,*obsdec,*lst,*antpos,*antdiam,*source,*bin;
	VARIABLE *vhash[HASHSIZE],*prevar[MAXPRE];
	VARIABLE variable[MAXVAR];
        LINE_INFO data_line,ref_line,actual_line;
	int need_skyfreq,need_point,need_planet,need_dra,need_ddec,
	    need_ra,need_dec,need_pol,need_on,need_uvw,need_src,
	    need_win,need_bin;
	float ref_plmaj,ref_plmin,ref_plangle,plscale,pluu,pluv,plvu,plvv;
	double skyfreq;
        int skyfreq_start;
	VARHAND *vhans;
	SELECT *select;
	int apply_amp,apply_win;
	AMP *amp;
	SIGMA2 sigma2;
	UVW *uvw;
	WINDOW *win;
		} UV;

#define MAXVHANDS 20

static UV *uvs[MAXOPEN];
static VARHAND *varhands[MAXVHANDS];
static WINDOW truewin;
static AMP noamp;
static int first=TRUE;

void uvputvr_c();
private void uvinfo_chan(),uvinfo_variance();
private void uv_init(),uv_freeuv(),uv_free_select();
private void uvread_defline(),uvread_init(),uvread_velocity(),uvread_flags();
private void uvread_defvelline();
private void uvread_updated_planet(),uvread_reference();
private void uvread_updated_uvw(),uvread_preamble();
private void uv_vartable_out(),uv_vartable_in();
private void uvset_coord(),uvset_linetype(),uvset_planet();
private void uvset_selection(),uvset_preamble();
private void uv_addopers(),uv_override();
private UV *uv_getuv();
private VARIABLE *uv_mkvar(),*uv_locvar(),*uv_checkvar();
private int uv_scan(),uvread_line(),uvread_select(),uvread_maxvis();
private int uvread_shadowed(),uvread_match();
private double uv_getskyfreq();

/************************************************************************/
#ifdef TESTBED
static char *M[] = {
  "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
  "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"
};
/*  The following compiles a main program to give exercise to some of the
 *  uvio routines. It is essentially a debugging device (both for bad
 *  files and bad behaviour of uvio!).
 *
 *  Call several uvio.c poutines, some of which are the secret ones,
 *  to get a 'human' readable listing of a miriad visibility data set `
 *  Because it needs some of these 'static' routines, the source code
 *  of uvio.c needs to be included here directly, as opposed to linking
 *  it with the library ($MIRLIB/libmir.a in Unix)
 *
 *  Note:  This program does not have the normal miriad user interface
 *	   and should hence not live in $MIRBIN
 *
 *
 *
 */
main(ac,av)
int ac;
char *av[];
{
    int tno;
    char *fn;
    void uvopen_c(), uvclose_c();

    printf("%s Version %s\n",av[0],VERSION_ID);
    if (ac!=2) {
        printf("Usage: %s [vis=]vis-dataset\n",av[0]);
        printf("Expert listing of a miriad UV dataset\n");
        exit(0);
    }

    fn = av[1];
    if ((int)strlen(fn) > 4) {       /* see if vis= was used */
        if (strncmp(fn,"vis=",4)==0)
            fn += 4;                /* if so, increase pointer */
    }
    uvopen_c(&tno,fn,"old");
    my_uvlist(tno,fn);
    uvclose_c(tno);
}


my_uvlist(tno,fname)
int tno;
char *fname;
{
    double *dp;
    float *fp;
    short *sp;
    int offset, iostat, intsize, extsize, i, *ip, eor_count=0;
    VARIABLE *v;
    UV *uv;
    char s[UV_HDR_SIZE], *b, buffer[128];


    uv = uvs[tno];          /* get pointer to UV structure */

    offset = uv->offset;    /* should be 0 at start */
    printf("0x%8x FILE: %s\n",offset,fname);
    while(offset < uv->max_offset) {
	printf("0x%8x ",offset);
        hreadb_c(uv->item,s,offset,UV_HDR_SIZE,&iostat);   /* get header */ 
        if (iostat == -1) return(iostat);   /* End Of File */

        if(*(s+2) != VAR_EOR) {
            v = &uv->variable[*s];      /* get name of var */
            intsize = internal_size[v->type];
            extsize = external_size[v->type];
        }
        
        switch(*(s+2)) {
            case VAR_SIZE:
                hreadi_c(uv->item,&v->length,offset+UV_HDR_SIZE,H_INT_SIZE,
                                        &iostat);
                printf("SIZE: %-9s Count=%d,Type=%c\n",v->name,VARLEN(v),VARTYPE(v));
                v->buf = Realloc(v->buf, (v->length*intsize)/extsize);
                offset += UV_ALIGN;
                break;
            case VAR_DATA:
                offset += mroundup(UV_HDR_SIZE,extsize);
                hread_c(uv->item,v->type,v->buf,offset,v->length,
                                        &iostat);
                printf("DATA: %-9s",v->name);
		if (strcmp(v->name,"time") == 0) {
			int z,a,b,c,d,e,alpha,month,year,day,hr,minute,sec;
			int dsec,nchar;
			char string[100];
			double f;

                        dp = (double *) v->buf;
                        z = *dp + 0.5 + (1.0/1728000.0);
                        f = *dp + 0.5 + (1.0/1728000.0) - z;
                        if (z<2299161){a=z;}else{
			  alpha = ((z - 1867216.25) / 36524.25);
			  a = z + 1 + alpha - (int)(0.25 * alpha);
			}
			b = a + 1524;    c = (b - 122.1) / 365.25;
			d = 365.25 * c;  e = (b - d) / 30.6001;
			f += (b - d - (int)(30.6001 * e));
			day = f;         hr = 24 * (f - day);
			minute = 60 * (24 * (f - day) - hr);
			sec = 600 * (60 * (24 * (f - day) - hr) - minute);
			dsec = sec % 10; sec /= 10;
			month = (e<=13) ? e - 1 : e - 13;
			year = (month>2) ? c - 4716 : c - 4715;
			year %= 100;
                        printf(" %20.10lg ",*dp);
                        printf("  %2.2d%s%2.2d:%2.2d:%2.2d:%2.2d.%1d\n",
			  year,M[month-1],day,hr,minute,sec,dsec);
		}else
		switch (v->type) {
                  case H_BYTE:
			strncpy(buffer,v->buf,v->length);
			buffer[v->length] = 0;
                        printf(" %-8s\n",buffer);
                        break;
                  case H_INT2:
                        sp = (short *) v->buf;
                        printf(" %d\n",*sp);
                        break;
                  case H_INT:
                        ip = (int *) v->buf;
                        printf(" %d\n",*ip);
                        break;
                  case H_REAL:
                        fp = (float *) v->buf;
                        printf(" %20.10g\n",*fp);
                        break;
                  case H_DBLE:
                        dp = (double *) v->buf;
                        printf(" %20.10lg\n",*dp);
                        break;
                  case H_CMPLX:
                        fp = (float *) v->buf;
                        printf(" %20.10g %20.10g\n",fp[0], fp[1]);
                        break;
                  default:
                        printf("  (Invalid data type %d)\n",v->type);
                        break;       
                }
                offset = mroundup(offset+v->length,UV_ALIGN);
                break;
            case VAR_EOR:
                printf("========== EOR (%d) ========\n",++eor_count);
                offset += UV_ALIGN;
                                break;
            default:
                printf("No valid record code %d",*(s+2));
		exit(0);
        } /* switch */
        uv->offset = offset;
    }  /* for(;;) */
}
#endif
/************************************************************************/
void uvopen_c(tno,name,status)
int *tno;
char *name,*status;
/**UvOpen -- Open a uv data file.					*/
/*&rjs                                                                  */
/*:uv-i/o								*/
/*+ FORTRAN call sequence:

	subroutine uvopen(tno,name,status)
	integer tno
	character name*(*),status*(*)

  Create and/or ready a UV data base to be accessed.

  Input:
    name	Name of the directory tree containg the u-v data.
    status	Either "old", "new" or "append". Old files can be read,
		whereas new and append files can only be written. Append
		files must be pre-existing uv data-sets.
  Output:
    tno		Handle of the uv data set.				*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  UV *uv;
  int iostat;
  char line[MAXLINE];

  if(first)uv_init();

/*----------------------------------------------------------------------*/
/*									*/
/*	Handle an old file.						*/
/*									*/
/*----------------------------------------------------------------------*/

  if( !strcmp(status,"old") ) {
    hopen_c(tno,name,"old",&iostat);
    CHECK(iostat,(message,"Error opening %s, in UVOPEN(old)",name));
    uv = uv_getuv(*tno);
    haccess_c(*tno,&uv->item,"visdata","read",&iostat);
    CHECK(iostat,(message,"Error accessing visdata for %s, in UVOPEN(old)",name));
    rdhdi_c(*tno,"vislen",&(uv->max_offset),hsize_c(uv->item));
    uv_vartable_in(uv);
    uv_override(uv);

/*----------------------------------------------------------------------*/
/*									*/
/*	Handle a new file.						*/
/*									*/
/*----------------------------------------------------------------------*/

  } else if(!strcmp(status,"new")) {
    hopen_c(tno,name,"new",&iostat);
    CHECK(iostat,(message,"Error opening %s, in UVOPEN(new)",name));
    uv = uv_getuv(*tno);
    haccess_c(*tno,&uv->item,"visdata","write",&iostat);
    CHECK(iostat,(message,"Error accessing visdata for %s, in UVOPEN(new)",name));
    uv->flags = UVF_NEW;

/*----------------------------------------------------------------------*/
/*									*/
/*	Append to an old file.						*/
/*									*/
/*----------------------------------------------------------------------*/

  } else if(!strcmp(status,"append")) {
    hopen_c(tno,name,"old",&iostat);
    CHECK(iostat,(message,"Error opening %s, in UVOPEN(append)",name));
    uv = uv_getuv(*tno);
    haccess_c(*tno,&uv->item,"visdata","append",&iostat);
    CHECK(iostat,(message,"Error accessing visdata for %s, in UVOPEN(append)",name));
    uv->flags = UVF_APPEND;
    rdhdi_c(*tno,"vislen",&(uv->offset),hsize_c(uv->item));
    uv->offset = mroundup(uv->offset,UV_ALIGN);
    uv_vartable_in(uv);

/* Read items and fill in the appropriate value. */

    rdhda_c(*tno,"obstype",line,"",MAXLINE);
    if(!strcmp(line,"autocorrelation"))		uv->flags |= UVF_AUTO;
    else if(!strcmp(line,"crosscorrelation"))	uv->flags |= UVF_CROSS;
    rdhdi_c(*tno,"ncorr",&(uv->corr_flags.offset),-1);
    rdhdi_c(*tno,"nwcorr",&(uv->wcorr_flags.offset),-1);
    if(uv->corr_flags.offset < 0 || uv->wcorr_flags.offset < 0)
      BUG('f',"Cannot append to uv file without 'ncorr' and/or 'nwcorr' items");

/*----------------------------------------------------------------------*/
/*									*/
/*	Somethig else -- must be an error.				*/
/*									*/
/*----------------------------------------------------------------------*/

  } else ERROR('f',(message,"Status %s is not recognised by UVOPEN",status));
}
/************************************************************************/
void uvclose_c(tno)
int tno;
/**uvclose -- Close a uv file						*/
/*&rjs                                                                  */
/*:uv-i/o								*/
/*+ FORTRAN call sequence:
	subroutine uvclose(tno)
	integer tno

  This close a uv data file.
  Input:
    tno		Handle of the uv data set.				*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  UV *uv;
  int iostat;

  void uvflush_c();

  uv = uvs[tno];

/* Finished with the flagging information. */

  if(uv->corr_flags.handle != NULL) mkclose_c(uv->corr_flags.handle);
  if(uv->wcorr_flags.handle != NULL) mkclose_c(uv->wcorr_flags.handle);
  uv->corr_flags.handle = uv->wcorr_flags.handle = NULL;

/* Flush out all stuff appropriate for a new or append file. */

  if(uv->flags & (UVF_NEW|UVF_APPEND))uvflush_c(tno);

/* Close the visibility data stream, release structures, and close the
   whole thing. */

  hdaccess_c(uv->item,&iostat);
  CHECK(iostat,(message,"Error calling hdaccess for visdata, in UVCLOSE"));
  uv_freeuv(uv);
  uvs[tno] = NULL;
  hclose_c(tno);
}
/************************************************************************/
void uvflush_c(tno)
int tno;
/**uvflush -- Flush buffers of a uv dataset to disk.			*/
/*&rjs                                                                  */
/*:uv-i/o								*/
/*+ FORTRAN call sequence:
	subroutine uvflush(tno)
	integer tno

  This close a uv data file.
  Input:
    tno		Make sure anything buffered up is flushed to disk. The
		disk file should be readable (up to data written here)
		even if the caller or computer crashes.			*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  UV *uv;
  int iostat;

  uv = uvs[tno];

  if(!(uv->flags & (UVF_NEW|UVF_APPEND)))return;

/* Flush the masks out. */

  if(uv->corr_flags.handle != NULL) mkflush_c(uv->corr_flags.handle);
  if(uv->wcorr_flags.handle != NULL) mkflush_c(uv->wcorr_flags.handle);

/* Rewrite vartable, if needed. */

  if(uv->saved_nvar < uv->nvar || (uv->nvar == 0 && (uv->flags & UVF_NEW))) uv_vartable_out(uv);
  uv->saved_nvar = uv->nvar;

/* Rewrite the description indicating the type of the data. */

  if( ( uv->flags & (UVF_AUTO|UVF_CROSS) ) == (UVF_AUTO|UVF_CROSS))
    wrhda_c(tno,"obstype","mixed-auto-cross");
  else if(uv->flags & UVF_AUTO)
    wrhda_c(tno,"obstype","autocorrelation");
  else if(uv->flags & UVF_CROSS)
    wrhda_c(tno,"obstype","crosscorrelation");

/* Write out things to help recover the EOF. */

  wrhdi_c(tno,"nwcorr",uv->wcorr_flags.offset);
  wrhdi_c(tno,"ncorr",uv->corr_flags.offset);
  wrhdi_c(tno,"vislen",uv->offset);

/* Finally flush out everything to disk. */

  hflush_c(tno,&iostat);
  CHECK(iostat,(message,"Error calling hflush, in UVFLSH"));
}
/************************************************************************/
private void uv_init()
/*
  Initalise everything imaginable.
------------------------------------------------------------------------*/
{
  int i;

  first = FALSE;

  external_size[H_BYTE] = 1;	       internal_size[H_BYTE] = 1;
  type_flag[H_BYTE] = 'a';
  external_size[H_INT] = H_INT_SIZE;   internal_size[H_INT] = sizeof(int);
  type_flag[H_INT] = 'i';
  external_size[H_INT2] = H_INT2_SIZE; internal_size[H_INT2] = sizeof(int);
  type_flag[H_INT2] = 'j';
  external_size[H_REAL] = H_REAL_SIZE; internal_size[H_REAL] = sizeof(float);
  type_flag[H_REAL] = 'r';
  external_size[H_DBLE] = H_DBLE_SIZE; internal_size[H_DBLE] = sizeof(double);
  type_flag[H_DBLE] = 'd';
  external_size[H_CMPLX] = H_CMPLX_SIZE;
				       internal_size[H_CMPLX] = 2*sizeof(float);
  type_flag[H_CMPLX] = 'c';

/* Initialise the "true window" array. */

  noamp.select = FALSE;

  truewin.first = 0;
  truewin.last  = MAXWIN-1;
  truewin.n     = MAXWIN;
  truewin.select= FALSE;
  for(i=0; i < MAXWIN; i++) truewin.wins[i] = TRUE;

/* Initialise the table of variable handles. */

  for(i=0; i < MAXVHANDS; i++)varhands[i] = NULL;

}
/************************************************************************/
private void uv_freeuv(uv)
UV *uv;
/*
  Free a uv structure.
------------------------------------------------------------------------*/
{
  VARHAND *vh,*vht;
  VARPNT *vp,*vpt;

  vh = uv->vhans;
  while(vh != NULL){
    vp = vh->varhd;
    varhands[vh->index] = NULL;
    while(vp != NULL){
      vpt = vp;
      vp = vp->fwd;
      free((char *)vpt);
    }
    vht = vh;
    vh = vh->fwd;
    free((char *)vht);
  }
  if(uv->data_line.wts	!= NULL) free((char *)uv->data_line.wts);
  if(uv->ref_line.wts	!= NULL) free((char *)uv->ref_line.wts);
  if(uv->corr_flags.flags != NULL) free((char *)uv->corr_flags.flags);
  if(uv->wcorr_flags.flags != NULL ) free((char *)uv->wcorr_flags.flags);
  if(uv->sigma2.table != NULL)free((char *)uv->sigma2.table);
  uv_free_select(uv->select);
  if(uv->uvw != NULL) free((char *)(uv->uvw));
  free((char *)uv);
}
/************************************************************************/
private void uv_free_select(sel)
SELECT *sel;
{
  OPERS *op;
  SELECT *fwd;
  int i;

  while(sel != NULL){
    fwd = sel->fwd;
    if(sel->noper > 0){
      op = sel->opers;
      for(i=0; i < sel->noper; i++){
	if(op->stval != NULL) free(op->stval);
	op++;
      }
      free((char *)(sel->opers));
    }
    free((char *)sel);
    sel = fwd;
  }
}
/************************************************************************/
private UV *uv_getuv(tno)
/*
  Allocate a structure describing a uv file.
------------------------------------------------------------------------*/
{
  int i;
  UV *uv;
  VARIABLE *v;

  uv = (UV *)Malloc(sizeof(UV));
  uv->item	= 0;
  uv->tno	= tno;
  uv->vhans	= NULL;
  uv->nvar	= 0;
  uv->presize   = 0;
  uv->saved_nvar= 0;
  uv->offset    = 0;
  uv->max_offset= 0;
  uv->flags	= 0;
  uv->callno	= 0;
  uv->maxvis	= 0;
  uv->mark	= 0;
  uv->select    = NULL;
  uv->need_skyfreq = uv->need_point = uv->need_planet = FALSE;
  uv->need_pol	   = uv->need_on    = uv->need_uvw    = FALSE;
  uv->need_src	   = uv->need_win   = uv->need_bin    = FALSE;
  uv->need_dra	   = uv->need_ddec  = uv->need_ra     = uv->need_dec = FALSE;
  uv->uvw = NULL;
  uv->ref_plmaj = uv->ref_plmin = uv->ref_plangle = 0;
  uv->plscale = 1;
  uv->pluu = uv->plvv = 1;
  uv->plvu = uv->pluv = 0;
  uv->apply_amp = TRUE;
  uv->apply_win = TRUE;
  uv->skyfreq_start = 0;

  uv->corr_flags.exists = TRUE;
  uv->corr_flags.handle = NULL;
  uv->corr_flags.offset = 0;
  uv->corr_flags.flags = NULL;
  uv->corr_flags.nflags = 0;
  uv->wcorr_flags.exists = TRUE;
  uv->wcorr_flags.handle = NULL;
  uv->wcorr_flags.offset = 0;
  uv->wcorr_flags.flags = NULL;
  uv->wcorr_flags.nflags = 0;

  uv->data_line.wts = NULL;
  uv->data_line.linetype = LINE_NONE;
  uv->ref_line.wts  = NULL;
  uv->ref_line.linetype  = LINE_NONE;

  uv->sigma2.table = NULL;
  uv->sigma2.missing = FALSE;

  uv->corr = NULL;
  uv->wcorr = NULL;
  uv->coord = NULL;
  uv->time = NULL;
  uv->bl = NULL;

  for(i=0, v = uv->variable; i < MAXVAR; i++, v++){
    v->length = v->flength = 0;
    v->buf = NULL;
    v->flags = 0;
    v->type = 0;
    v->fwd = NULL;
    v->index = i;
    v->callno = 0;
  }
  for(i=0; i < HASHSIZE; i++) uv->vhash[i] = NULL;
  uvs[tno] = uv;
  return(uv);
}
/************************************************************************/
private void uv_vartable_out(uv)
UV *uv;
/*
  Write out a variable name table.
------------------------------------------------------------------------*/
{
  int item;
  char line[MAXLINE];
  int iostat,i;
  VARIABLE *v;

  haccess_c(uv->tno,&item,"vartable","write",&iostat);
  CHECK(iostat,(message,"Error opening vartable, in UVCLOSE(vartable_out)"));
  for(i=0, v = uv->variable; i < uv->nvar; i++,v++){
    Sprintf(line,"%c %s",VARTYPE(v),v->name);
    hwritea_c(item,line,strlen(line)+1,&iostat);
    CHECK(iostat,(message,"Error writing to vartable, in UVCLOSE(vartable_out)"));
  }
  hdaccess_c(item,&iostat);
  CHECK(iostat,(message,"Error closing vartable, in UVCLOSE(vartable_out)"));
}
/************************************************************************/
private void uv_override(uv)
UV *uv;
/*
  Determine if a variable has a item of the same name. If there is one, then
  the value of that item overrides the value of the variable. In this case,
  get the value of the item, and set a flag to indicate that the variable
  value is being overriden.
------------------------------------------------------------------------*/
{
  int item;
  char *b,varname[MAXLINE],vartype[MAXLINE],descr[MAXLINE];
  VARIABLE *v;
  int tno,iostat,n,ok,isnumeric,ischar;

  tno = uv->tno;
  haccess_c(uv->tno,&item,".","read",&iostat);
  CHECK(iostat,(message,"Error opening directory listing, in UVOPEN(override)"));
  while(hreada_c(item,varname,MAXLINE,&iostat),iostat==0){
    v = uv_locvar(tno,varname);
    if(v != NULL){
      hdprobe_c(tno,varname,descr,MAXLINE,vartype,&n);
      isnumeric = 
	(v->type == H_DBLE || v->type == H_REAL || v->type == H_INT) &&
        (!strcmp(vartype,"double") || !strcmp(vartype,"real") ||
	 !strcmp(vartype,"integer"));
      ischar =  (v->type == H_BYTE && !strcmp(vartype,"character"));
      ok = ( n == 1 && (isnumeric || ischar) );

      if(v->type == H_BYTE) {
	n = strlen(descr);
        b = Malloc(n+1);
      } else {
	b = Malloc(internal_size[v->type]);
      }
      if(ok)switch(v->type){
          case H_INT:   rdhdi_c(tno,varname,(int *)b,0);                break;
          case H_REAL:  rdhdr_c(tno,varname,(float *)b,0.0);            break;
	  case H_BYTE:  strcpy(b,descr);				break;
          case H_DBLE:  rdhdd_c(tno,varname,(double *)b,(double)0.0);   break;
          default:      ok = FALSE;
      }
      if(ok){
	v->flags |= UVF_OVERRIDE;
	v->buf = b;
	v->length = n*external_size[v->type];
	v->callno = 1;
      } else {
	free(b);
	ERROR('w',(message,"Cannot override variable %s, in UVOPEN",varname));
      }
    }
  }
  if(iostat != -1) ERROR('f',(message,
	"Error %d when performing override checks, in UVOPEN",iostat));
  hdaccess_c(item,&iostat);
}
/************************************************************************/
private void uv_vartable_in(uv)
UV *uv;
/*
  Scan the variable name table, to determine the names and types of the
  variables.
------------------------------------------------------------------------*/
{
  int item;
  char line[MAXLINE],name[MAXNAM+1],ctype;
  int iostat,type;

  haccess_c(uv->tno,&item,"vartable","read",&iostat);
  CHECK(iostat,(message,"Error opening vartable, in UVOPEN(vartable_in)"));

  while(hreada_c(item,line,(int)sizeof(line),&iostat),!iostat){
    Sscanf(line,"%c %s",&ctype,name);
    switch(ctype){
      case 'a': type = H_BYTE;	break;
      case 'j': type = H_INT2;	break;
      case 'i': type = H_INT;	break;
      case 'r':	type = H_REAL;	break;
      case 'd':	type = H_DBLE;	break;
      case 'c': type = H_CMPLX;	break;
      default: ERROR('f',(message,"Bad type (%c) for variable %s",ctype,name));
    }
    (void)uv_mkvar(uv->tno,name,type);
  }
  hdaccess_c(item,&iostat);
  uv->saved_nvar = uv->nvar;
}
/************************************************************************/
private VARIABLE *uv_mkvar(tno,name,type)
int tno,type;
char *name;
/*
  Add an entry for a new variable.
------------------------------------------------------------------------*/
{
  UV *uv;
  VARIABLE *v;
  int n,hashval;

/* Check if the variable already exists. */

  v = uv_locvar(tno,name);
  if(v != NULL) return(v);

/* Check that the variable has a good name. */
  if((int)strlen(name) > MAXNAM)
    ERROR('f',(message,"The variable name %s is too long, in UVPUTVR",name));

/* We are going to have to create it. */

  uv = uvs[tno];
  n = uv->nvar++;
  v = &uv->variable[n];
  Strcpy(v->name,name);
  v->type = type;

/* Add it to the hash table. */

  hashval = 0;
  while(*name)hashval += *name++;
  hashval %= HASHSIZE;
  v->fwd = uv->vhash[hashval];
  uv->vhash[hashval] = v;

  return(v);
}
/************************************************************************/
private VARIABLE *uv_locvar(tno,name)
int tno;
char *name;
/*
  Locate a variable from the hash table.
------------------------------------------------------------------------*/
{
  VARIABLE *v;
  int hashval;
  char *s;

  hashval = 0;
  for(s=name; *s; s++) hashval += *s;

  for(v = uvs[tno]->vhash[hashval%HASHSIZE]; v != NULL; v = v->fwd)
	if(!strcmp(v->name,name))break;
  return(v);
}
/************************************************************************/
void uvnext_c(tno)
int tno;
/**uvnext -- Skip to the next uv record.				*/
/*&rjs                                                                  */
/*:uv-i/o								*/
/*+ FORTRAN call sequence:
	subroutine uvnext(tno)
	integer tno

  Skip to the next uv data record. On write, this causes an end-of-record
  mark to be written. On read, this causes data to be read up until the
  next end-of-record mark.

  Input:
    tno		The uv data file handle.				*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  int iostat;
  UV *uv;

  uv = uvs[tno];
  if(uv->flags & (UVF_NEW|UVF_APPEND)){
    hwriteb_c(uv->item,var_eor_hdr,uv->offset,UV_HDR_SIZE,&iostat);
    CHECK(iostat,(message,"Error writing end-of-record, in UVNEXT"));
    uv->offset += UV_ALIGN;
  } else {
    uv->mark = uv->callno + 1;
    uv->flags &= ~(UVF_UPDATED | UVF_COPY);
    (void)uv_scan(uv,(VARIABLE *)NULL);
  }
}
/************************************************************************/
void uvrewind_c(tno)
int tno;
/**uvrewind -- Reset the uv data file to the start of the file.		*/
/*&rjs                                                                  */
/*:uv-i/o								*/
/*+  FORTRAN call sequence:

	subroutine uvrewind(tno)
	integer tno

  Rewind a uv file, readying it to be read from the begining again.

  Input:
    tno		The uv data file handle.				*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  UV *uv;
  VARIABLE *v;
  VARHAND *vh;
  int i;

  uv = uvs[tno];

  uv->callno = uv->mark = 0;
  for(i=0, v = uv->variable; i < uv->nvar; i++, v++) v->callno = 0;
  for(vh = uv->vhans; vh != NULL; vh = vh->fwd) vh->callno = 0;
  uv->offset = 0;
  uv->corr_flags.offset = 0;
  uv->wcorr_flags.offset = 0;
}
/************************************************************************/
void uvcopyvr_c(tin,tout)
int tin,tout;
/**uvcopyvr -- Copy variables from one uv file to another.		*/
/*&rjs                                                                  */
/*:uv-i/o								*/
/*+ FORTRAN call sequence:

	subroutine uvcopyvr(tin,tout)
	integer tin,tout


  This copies those variables, in the input uv data set, which have
  changed and which are marked as "copy" ('u' flag of a call to uvtrack).

  Inputs:
    tin		File handle of the input uv data set.
    tout	File handle of the output uv data set.			*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  UV *uv;
  VARIABLE *v;
  int i;

  uv = uvs[tin];
  if(uv->flags & UVF_COPY) for(i=0, v=uv->variable; i < uv->nvar; i++,v++){
    if(v->callno >= uv->mark && (v->flags & UVF_COPY))
      uvputvr_c(tout,v->type,v->name,v->buf,VARLEN(v));
  }
}
/************************************************************************/
int uvupdate_c(tno)
int tno;
/**uvupdate -- Check whether any "important" variables have changed.	*/
/*&rjs                                                                  */
/*:uv-i/o								*/
/*+ FORTRAN call sequence:

	logical function uvupdate(tno)
	integer tno

  This checks whether any ``important variables'' has been updated in the
  last call to uvread or uvscan. Important variables are those flagged
  with the 'u' flag in a call to uvtrack.

  Input:
    tno		File handle of the uv file to check.			*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  return(uvs[tno]->flags & UVF_UPDATED ? FORT_TRUE : FORT_FALSE);
}
/************************************************************************/
void uvvarini_c(tno,vhan)
int tno,*vhan;
/**uvvarini -- Retrieve a handle for the "uvVar" routines.		*/
/*&rjs									*/
/*:uv-i/o								*/
/*+ FORTRAN call sequence:

	subroutine uvvarini(tno,vhan)

  This routine allocates a handle for the uvVar routines. These routines
  are used to keep track of changes to variables, and to copy them when
  a change occurs.

  Input:
    tno		The handle of the uv data file.
  Output:
    vhan	Handle of the list of variables.			*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  int i;
  VARHAND *vh;
  UV *uv;

  uv = uvs[tno];

/* Locate a space handle slot. */

  for(i=0; i < MAXVHANDS; i++)if(varhands[i] == NULL)break;
  if(i == MAXVHANDS)BUG('f',"Ran out of variable handle slots, in UVVARINI");
  varhands[i] = vh = (VARHAND *)Malloc(sizeof(VARHAND));
  
  vh->index = i;
  vh->callno = 0;
  vh->tno = tno;
  vh->varhd = NULL;
  vh->fwd = uv->vhans;
  uv->vhans = vh;
  *vhan = i+1;
}
/************************************************************************/
void uvvarset_c(vhan,var)
int vhan;
char *var;
{
  VARHAND *vh;
  VARIABLE *v;
  VARPNT *vp;

  vh = varhands[vhan-1];
  v = uv_locvar(vh->tno,var);
  if(v != NULL){
    vp = (VARPNT *)Malloc(sizeof(VARPNT));
    vp->v = v;
    vp->fwd = vh->varhd;
    vh->varhd = vp;
  }
}
/************************************************************************/
void uvvarcpy_c(vhan,tout)
int vhan,tout;
{
  VARIABLE *v;
  VARHAND *vh;
  VARPNT *vp;
  int callno;

  vh = varhands[vhan-1];
  callno = vh->callno;
  vh->callno = uvs[vh->tno]->callno;

  for(vp = vh->varhd; vp != NULL; vp = vp->fwd){
    v = vp->v;
    if(v->callno > callno)
      uvputvr_c(tout,v->type,v->name,v->buf,VARLEN(v));
  }
}
/************************************************************************/
int uvvarupd_c(vhan)
int vhan;
{
  VARIABLE *v;
  VARHAND *vh;
  VARPNT *vp;
  int callno;

  vh = varhands[vhan-1];
  callno = vh->callno;
  vh->callno = uvs[vh->tno]->callno;

  for(vp = vh->varhd; vp != NULL; vp = vp->fwd){
    v = vp->v;
    if(v->callno > callno) return(FORT_TRUE);
  }
  return(FORT_FALSE);
}
/************************************************************************/
void uvrdvr_c(tno,type,var,data,def,n)
int tno,type,n;
char *var,*data,*def;
/**uvrdvr -- Return the value of a UV variable.				*/
/*&rjs                                                                  */
/*:uv-i/o								*/
/*+ FORTRAN call sequence:

	subroutine uvrdvra(tno,varname,adata,adefault)
	subroutine uvrdvri(tno,varname,idata,idefault)
	subroutine uvrdvrr(tno,varname,rdata,rdefault)
	subroutine uvrdvrd(tno,varname,ddata,ddefault)
	subroutine uvrdvrc(tno,varname,cdata,cdefault)
	integer tno
	character varname*(*)
	character adata*(*),adefault*(*)
	integer   idata,    idefault
	real	  rdata,    rdefault
	double precision ddata,ddefault
	complex cdata,cdefault

  These routines get the first value of a variable. If the variable is
  missing,the default value is returned.

  Input:
    tno		The handle of the uv data file.
    varname	The name of the variable to return.
    default	The default value.
  Output:
    data	The returned value of the variable.			*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  VARIABLE *v;
  int deflt,oktype;

  v = uv_locvar(tno,var);
  oktype = TRUE;
  deflt = (v == NULL);
  if(!deflt) deflt = (v->buf == NULL) || (v->length == 0);
  if(!deflt){
    switch(type){
      case H_BYTE:
	oktype = (v->type == H_BYTE);
	n = min(n-1,v->length);
	if(oktype)memcpy(data,v->buf,n);
	break;
      case H_INT:
	switch(v->type){
	  case H_INT:	*(int *)data = *(int *)(v->buf);	break;
	  case H_REAL:	*(int *)data = *(float *)(v->buf);	break;
	  case H_DBLE:	*(int *)data = *(double *)(v->buf);	break;
	  default:	oktype = FALSE;				break;
	}
	break;
      case H_REAL:
	switch(v->type){
	  case H_INT:	*(float *)data = *(int *)(v->buf);	break;
	  case H_REAL:	*(float *)data = *(float *)(v->buf);	break;
	  case H_DBLE:	*(float *)data = *(double *)(v->buf);	break;
	  default:	oktype = FALSE;				break;
	}
	break;
      case H_DBLE:
	switch(v->type){
	  case H_INT:	*(double *)data = *(int *)(v->buf);	break;
	  case H_REAL:	*(double *)data = *(float *)(v->buf);	break;
	  case H_DBLE:	*(double *)data = *(double *)(v->buf);	break;
	  default:	oktype = FALSE;				break;
	}
	break;
      case H_CMPLX:
	oktype = (v->type == H_CMPLX);
	if(oktype)memcpy(data,v->buf,internal_size[type]);
	break;
      default:
	oktype = FALSE;
    }
  }else{
    if( type == H_BYTE ) n = min(n-1,(int)strlen(def));
    else		 n = internal_size[type];
    memcpy(data,def,n);
  }

/* Give a fatal error message if there is a type mismatch. */

  if(!oktype)
    ERROR('f',(message,"Type incompatiblity for variable %s, in UVRDVR",var));

/* Null terminate the data, if its a character string. */

  if( type == H_BYTE ) *(data + n) = 0;
}
/************************************************************************/
void uvgetvr_c(tno,type,var,data,n)
int tno,type,n;
char *var;
char *data;
/**uvgetvr -- Get the values of a uv variable.				*/
/*&rjs                                                                  */
/*:uv-i/o								*/
/*+ FORTRAN call sequence:

	subroutine uvgetvra(tno,varname,adata)
	subroutine uvgetvri(tno,varname,idata,n)
	subroutine uvgetvrr(tno,varname,rdata,n)
	subroutine uvgetvrd(tno,varname,ddata,n)
	subroutine uvgetvrc(tno,varname,cdata,n)
	integer tno,n
	character varname*(*)
	character adata*(*)
	integer   idata(n)
	real	  rdata(n)
	double precision ddata(n)
	complex cdata(n)

  These routines return the current value of a uv variable. N gives the size
  of elements in the return array. This MUST match with the actual number
  of elements in the variable. An exception is for the character string
  routine, where the size of the "adata" string must be strictly greater
  than (not equal to!) the size of the string.

  Input:
    tno		The handle of the uv data file.
    varname	The name of the variable to return.
    n		The number of elements to return. This must agree with
		the size of the variable!
  Output:
    data	The returned values of the variable.			*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  VARIABLE *v;
  int size;

  v = uv_locvar(tno,var);
  if(v == NULL)
    ERROR('f',(message,"Variable %s not found, in UVGETVR",var));
  size = external_size[type];
  if( type != v->type )
    ERROR('f',(message,"Variable %s has wrong type, in UVGETVR",var));
  if(v->buf == NULL)
    ERROR('f',(message,"Variable %s currently has no value, in UVGETVR",var));
  if( (type == H_BYTE ? n < v->length + 1 : n*size != v->length) )
    ERROR('f',(message,"Buffer for variable %s has wrong size, in UVGETVR",var));

/* Copy the data. */

  memcpy(data,v->buf,internal_size[type]*v->length/size);

/* Null terminate the data, if its a character string. */

  if( type == H_BYTE ) *(data + v->length) = 0;
}
/************************************************************************/
void uvprobvr_c(tno,var,type,length,updated)
int tno;
char *var,*type;
int *length,*updated;
/**uvprobvr -- Return information about a variable.			*/
/*&rjs                                                                  */
/*:uv-i/o								*/
/*+ FORTRAN call sequence:

	subroutine uvprobvr(tno,varname,type,length,update)
	integer tno,length
	character varname*(*),type*1
	logical update

  This checks whether a particular variable exists. If it does, this
  passes back the type and (current) length of the variable, and whether
  it was updated on the last call to uvread or uvscan.

  Input:
    tno		The handle of the input uv data file.
    varname	The name of the variable to check.
  Output:
    type	The type of the variable. If the variable does not
		exist, this is blank. Otherwise it is one of 'a', 'r',
		'i', 'd' or 'c'.
    length	The number of elements in the uv variable. If this is not
		known (which is true if the variable has never been read)
		then this will be zero.
    update	This will be set .true. if this variable was updated
		on the last call to uvread or uvscan.			*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  UV *uv;
  VARIABLE *v;

  uv = uvs[tno];
  v = uv_locvar(tno,var);
  if(v == NULL) {
    *type = ' ';
    *length = 0;
    *updated = FORT_FALSE;
  } else {
    *type   = VARTYPE(v);
    *length = VARLEN(v);
    *updated = (v->callno >= uv->mark ? FORT_TRUE : FORT_FALSE);
  }
}
/************************************************************************/
void uvputvr_c(tno,type,var,data,n)
int tno,type,n;
char *var;
char *data;
/**uvputvr -- Write the value of a uv variable.				*/
/*&rjs                                                                  */
/*:uv-i/o								*/
/*+ FORTRAN call sequence:

	subroutine uvputvra(tno,varname,adata)
	subroutine uvputvri(tno,varname,idata,n)
	subroutine uvputvrr(tno,varname,rdata,n)
	subroutine uvputvrd(tno,varname,ddata,n)
	subroutine uvputvrc(tno,varname,cdata,n)
	integer tno,n
	character varname*(*)
	character adata*(*)
	integer   idata(n)
	real	  rdata(n)
	double precision ddata(n)
	complex cdata(n)

  These routines write new values for a uv variable. N gives the number
  of elements to write.

  Input:
    tno		The handle of the uv data file.
    varname	The name of the variable to write.
    n		The number of elements to write.
    data	The values of the variable.				*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  UV *uv;
  VARIABLE *v;
  int size,iostat,changed,length,i;
  char *in1,*in2;

  if(n <= 0){
    ERROR('w',(message,"Variable %s has zero or negative size, in UVPUTVR",var));
    return;
  }
  uv = uvs[tno];
  v = uv_mkvar(tno,var,type);
  if(v->type != type)
    ERROR('f',(message,"Variable %s has changed type, in UVPUTVR",var));
  size = external_size[type];

/* If the size of this variable has changed, write it out to the file. */

  changed = (v->flags & UVF_NOCHECK);
  if(v->length != size*n){
    changed = TRUE;
    v->length = size * n;
    var_size_hdr[0] = v->index;
    hwriteb_c(uv->item,var_size_hdr,uv->offset,UV_HDR_SIZE,&iostat);
    CHECK(iostat,(message,"Error writing variable-length header for %s, in UVPUTVR",var));
    hwritei_c(uv->item,&v->length,uv->offset+UV_HDR_SIZE,H_INT_SIZE,&iostat);
    CHECK(iostat,(message,"Error writing variable-length for %s, in UVPUTVR",var));
    uv->offset += UV_ALIGN;
    if( !(v->flags & UVF_NOCHECK) )
      v->buf = Realloc(v->buf,n*internal_size[type]);
  }

/* Check if this variable has really changed.  */

  if( !changed ) {
    length = internal_size[type] * n;
    in1 = v->buf;
    in2 = data;
    for( i = 0; i < length; i++ ) {
      if(*in1++ != *in2++){
	changed = TRUE;
	break;
      }
    }
  }

/* Write out the data itself. */

  if( changed ) {
    var_data_hdr[0] = v->index;
    hwriteb_c(uv->item,var_data_hdr,uv->offset,UV_HDR_SIZE,&iostat);
    CHECK(iostat,(message,"Error writing variable-value header for %s, in UVPUTVR",var));
    uv->offset += mroundup(UV_HDR_SIZE,size);
    hwrite_c(uv->item,type,data,uv->offset,v->length,&iostat);
    CHECK(iostat,(message,"Error writing variable-value for %s, in UVPUTVR",var));
    uv->offset = mroundup( uv->offset+v->length, UV_ALIGN);
    if(v->callno++ > CHECK_THRESH) {
      v->flags |= UVF_NOCHECK;
    } else if(!(v->flags & UVF_NOCHECK)){
      length = internal_size[type] * n;
      memcpy(v->buf,data,length);
    } 
  } else {
    v->callno = 0;
  }
}
/************************************************************************/
void uvtrack_c(tno,name,switches)
int tno;
char *name,*switches;
/**uvtrack -- Set flags and switches associated with a uv variable.	*/
/*&rjs                                                                  */
/*:uv-i/o								*/
/*+ FORTRAN call sequence:

	subroutine uvtrack(tno,varname,switches)
	integer tno
	character varname*(*),switches*(*)

  UVTRACK allows the programmer to set switches and flags associated with
  a particular uv variable, to allow extra processing steps of that
  variable.

  Input:
    tno		The handle of the input uv file.
    varname	The name of the variable of interest.
    switches	This is a character string, each character of which
		causes a particular flag or switch to be turned on for
		this particular variable. Valid values are:
		 'u'  Remember if this variable gets updated, and  when
		      it gets updated, uvupdate returns .true. the next
		      time it is called.
		 'c'  Remember if this variable gets updated, and when 
		      it gets updated, cause it to be copied during the
		      next call to uvcopyvr.				*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  UV *uv;
  VARIABLE *v;

  uv = uvs[tno];
  v = uv_locvar(tno,name);
  if(v == NULL) return;
  while(*switches)switch(*switches++){
    case 'u': v->flags |= UVF_UPDATED;
	      uv->flags |= UVF_UPDATED;				break;
    case 'c': v->flags |= UVF_COPY;
	      uv->flags |= UVF_COPY;				break;
    case ' ':							break;
    default:
      ERROR('w',(message,"Unrecognised switch %c, in UVTRACK",*(switches-1)));
								break;
  }
}
/************************************************************************/
int uvscan_c(tno,var)
int tno;
char *var;
/**uvscan -- Scan a uv file until a variable changes.			*/
/*&rjs                                                                  */
/*:uv-i/o								*/
/*+ FORTRAN call sequence:

	integer function uvscan(tno,varname)
	integer tno
	character varname*(*)

  Scan through a uv file until a particular variable changes. This always
  reads to the end of the record (i.e. until all variables that change
  simultaneously are read) after "varname" was encountered.

  Input:
    tno		The handle of the uv file to be scanned.
    varname	The variable to terminate the search.

  Output:
    uvscan_c	0 on success, -1 on end-of-file. Standard error
		number otherwise.					*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  UV *uv;
  VARIABLE *v;

/* Locate the variable to scan on. */

  uv = uvs[tno];
  if(*var){
    v = uv_locvar(tno,var);
    if(v == NULL) ERROR('f',(message,"Variable %s not found, in UVSCAN",var));
  } else v = NULL;
  uv->mark = uv->callno + 1;
  uv->flags &= ~(UVF_UPDATED | UVF_COPY);
  return( uv_scan(uv,v) );
}
/************************************************************************/
private int uv_scan(uv,vt)
UV *uv;
VARIABLE *vt;
/*
  Scan the UV data stream until we have all the data we desire.
  Inputs:
    uv		Structure describing uv file to scan through.
    vt		Structure describing variable to terminate scan when found.
------------------------------------------------------------------------*/
{
  int offset,iostat,intsize,extsize,terminate,found,changed,i;
  VARIABLE *v;
  char s[UV_HDR_SIZE],*b;

  uv->callno++;
  offset = uv->offset;
  found = (vt == NULL);
  terminate = FALSE;
  while(!terminate){
    if(offset >= uv->max_offset) return(-1);
    hreadb_c(uv->item,s,offset,UV_HDR_SIZE,&iostat);
    if(iostat == -1)return(-1);
    else CHECK(iostat,(message,"Error reading a record header, while UV scanning"));

/* Remember that this was updated, and set the "updated" flag if necessary.
   Save the internal and external size of an element of this type. */

    changed = FALSE;
    if(*(s+2) != VAR_EOR){
      v = &uv->variable[*s];
      intsize = internal_size[v->type];
      extsize = external_size[v->type];
    }

    switch(*(s+2)){

/* Process a specification of a variables length. Allocate buffers if needed. */
     case VAR_SIZE:
      hreadi_c(uv->item,&v->flength,offset+UV_HDR_SIZE,H_INT_SIZE,&iostat);
      CHECK(iostat,(message,"Error reading a variable-length for %s, while UV scanning",v->name));
      if(v->flength <= 0)
	ERROR('f',(message,"Variable %s has length of %d, when scanning",
			v->name,v->flength));
      if(v->flength % extsize)
        ERROR('f',(message,
	  "Non-integral no. elements in variable %s, when scanning",v->name));
      if(!(v->flags & UVF_OVERRIDE) || v->type != H_BYTE){
        v->length = v->flength;
        v->buf = Realloc( v->buf, (v->flength * intsize)/extsize );
        if(v->flags & UVF_OVERRIDE && v->flength > extsize)
          for(i=1, b = v->buf + intsize; i < v->flength/extsize; i++,b += intsize)
	    memcpy(b,v->buf,intsize);
        changed = TRUE;
      }
      offset += UV_ALIGN;
      break;

/* Process the data of a variable. If we want to keep track of the value
   of this variable, read it. */
     case VAR_DATA:
      offset += mroundup(UV_HDR_SIZE,extsize);
      if(!(v->flags & UVF_OVERRIDE)){
	hread_c(uv->item,v->type,v->buf,offset,v->flength,&iostat);
	CHECK(iostat,(message,"Error reading a variable value for %s, while UV scanning",v->name));
	changed = TRUE;
      }
      offset = mroundup(offset+v->flength,UV_ALIGN);
      found |= (v == vt);
      break;

/* End of a block of synchronised data. */

    case VAR_EOR:
     terminate = found;
     offset += UV_ALIGN;
     break;

/* Something is wrong. */

     default:
      ERROR('f',(message,"Unrecognised record code %d, when scanning",*(s+2)));
    }
    if(changed){
      v->callno = uv->callno;
      uv->flags |= v->flags & (UVF_UPDATED | UVF_UPDATED_PLANET |
			       UVF_UPDATED_SKYFREQ | UVF_UPDATED_UVW | UVF_COPY);
    }
  }
  uv->offset = offset;
  return(0);
}
/************************************************************************/
void uvwrite_c(tno,preamble,data,flags,n)
int tno,n,*flags;
double *preamble;
float *data;
/**uvwrite -- Write correlation data to a uv file.			*/
/*&rjs                                                                  */
/*:uv-i/o								*/
/*+ FORTRAN call sequence:

	subroutine uvwrite(tno,preamble,data,flags,n)
	integer tno,n
	double precision preamble(4)
	complex data(n)
	logical flags(n)

  Write a visibility record to the data file.
  Input:
    tno		Handle of the uv data set.
    n		Number of channels to be written.
    preamble	A double array of 4 elements giving u,v, time and
		baseline number (in that order).
    data	A complex array of n elements containing
		the correlation data.
    flags	Logical array of n elements. A true value for
		a channel indicates good data.				*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  UV *uv;
  int i,nchan,i1,i2,nuvw;
  float maxval,scale,*p,temp;
  double *d,dtemp;
  int *q;
  char *counter,*status;
  FLAGS *flags_info;
  VARIABLE *v;

  uv = uvs[tno];

/* Initialise things if this is the first call to uvwrite. */

  if(!(uv->flags & UVF_INIT)){
    uv->flags |= UVF_INIT;
    if( uv->data_line.linetype == LINE_NONE)
      uv->data_line.linetype = LINE_CHANNEL;
    if( uv->data_line.linetype == LINE_CHANNEL){
      if( uv->corr == NULL )
        uv->corr = uv_mkvar(tno,"corr", ( n > 4 ? H_INT2 : H_REAL) );
      uv->corr->flags |= UVF_NOCHECK;
      if(uv->corr_flags.handle == NULL){
        status = (uv->corr_flags.offset == 0 ? "new" : "old");
	uv->corr_flags.handle = mkopen_c(uv->tno,"flags",status);
      }
      if( uv->corr_flags.handle == NULL)
	BUG('f',"Failed to open the corr flags, in UVWRITE");
    } else if( uv->data_line.linetype == LINE_WIDE){
      if( uv->wcorr == NULL )
        uv->wcorr = uv_mkvar(tno,"wcorr", H_CMPLX);
      uv->wcorr->flags |= UVF_NOCHECK;
      if( uv->wcorr_flags.handle == NULL){
        status = (uv->wcorr_flags.offset == 0 ? "new" : "old");
	uv->wcorr_flags.handle = mkopen_c(uv->tno,"wflags",status);
      }
      if( uv->wcorr_flags.handle == NULL)
	BUG('f',"Failed to open the  wcorr flags, in UVWRITE");
    } else
      BUG('f',"Unrecognised or unsupported linetype, in UVWRITE");

/* Create the preamble variables, if needed. */

    if( uv->coord == NULL ){
      uv->coord = uv_mkvar(tno,"coord",H_DBLE);
      uv->coord->flags |= UVF_NOCHECK;
      if(uv->coord->buf == NULL){
        uv->coord->buf = Malloc(3*sizeof(double));
        d = (double *)(uv->coord->buf);
        *d = *(preamble) + 1000;
      }
    }

    if( uv->time == NULL ){
      uv->time = uv_mkvar(tno,"time",H_DBLE);
      uv->time->flags |= UVF_NOCHECK;
      if(uv->time->buf == NULL){
	uv->time->buf = Malloc(sizeof(double));
	*(double *)(uv->time->buf) = *(preamble+2) + 1000;
      }
    }

    if( uv->bl == NULL ){
      uv->bl = uv_mkvar(tno,"baseline",H_REAL);
      uv->bl->flags |= UVF_NOCHECK;
      if(uv->bl->buf == NULL){
	uv->bl->buf = Malloc(sizeof(float));
	*(float *)(uv->bl->buf) = *(preamble + 3) + 1000;
      }
    }
  }

/* Get info on whether we are dealing with corr or wcorr data. */

  if(uv->data_line.linetype == LINE_WIDE){
    counter = "nwide";
    v = uv->wcorr;
    flags_info = &(uv->wcorr_flags);
  } else {
    counter  = "nchan";
    v = uv->corr;
    flags_info = &(uv->corr_flags);
  }

/* Update the size of the variable, if necessary. */

  nchan = NUMCHAN(v);
  if(n != nchan) uvputvri_c(tno,counter,&n,1);

/* Write out the flagging info. */

  if(uv->flags & UVF_RUNS)
    mkwrite_c(flags_info->handle,MK_RUNS,flags+1,flags_info->offset,n,*flags);
  else
    mkwrite_c(flags_info->handle,MK_FLAGS,flags,flags_info->offset,n,n);
  flags_info->offset += n;

/* Write out the correlation data. */

  if(v->type == H_REAL){
    uvputvrr_c(tno,v->name,data,2*n);
  } else if(v->type == H_CMPLX) {
    uvputvrc_c(tno,v->name,data,n);
  } else {
    if(v->length != 2*n*H_INT2_SIZE)
      v->buf = Realloc(v->buf,2*n*sizeof(int));
    maxval = 0;
    p = data;
    for(i=0; i < 2*n; i++){
      temp = *p++;
      if(temp < 0)temp = -temp;
      maxval = max(maxval,temp);
    }
    if(maxval == 0) maxval = 1;
    scale = maxval / 32767;
    uvputvrr_c(tno,"tscale",&scale,1);
    scale = 32767 / maxval;
    p = data;
    q = (int *)v->buf;
    for(i=0; i < 2*n; i++) *q++ = scale * *p++;
    q = (int *)v->buf;
    uvputvrj_c(tno,v->name,(int *)v->buf,2*n);
  }

/* Write out the preamble. */

  d = (double *)(uv->coord->buf);
  nuvw = (uv->flags & UVF_DOW ? 3 : 2);
  if( *d != *preamble || *(d+1) != *(preamble+1) || 
      ( nuvw == 3 && *(d+2) != *(preamble+2) ) ){
    uvputvrd_c(tno,"coord",preamble,nuvw);
    *d = *preamble;
    *(d+1) = *(preamble+1);
    if(nuvw == 3) *(d+2) = *(preamble+2);
  }
  preamble += nuvw;

  dtemp = *preamble++;
  if( dtemp != *(double *)(uv->time->buf) ){
    uvputvrd_c(tno,"time",&dtemp,1);
    *(double *)(uv->time->buf) = dtemp;
  }

  temp = *preamble++;
  if( temp != *(float *)(uv->bl->buf) ){
    i1 = temp;
    i2 = i1 / 256;
    i1 %= 256;
    uv->flags |= ( i1 == i2 ? UVF_AUTO : UVF_CROSS);
    uvputvrr_c(tno,"baseline",&temp,1);
    *(float *)(uv->bl->buf) = temp;
  }

/* Write an end-of-record marker. */

  uvnext_c(tno);
}
/************************************************************************/
void uvwwrite_c(tno,data,flags,n)
int tno;
float *data;
int *flags,n;
/**uvwwrite -- Write wide-band correlation data to a uv file.		*/
/*&rjs                                                                  */
/*:uv-i/o								*/
/*+ FORTRAN call sequence:

	subroutine uvwwrite(tno,data,flags,n)
	integer tno,n
	complex data(n)
	logical flags(n)

  Write a wide-band visibility record to the data file.
  Input:
    tno		Handle of the uv data set.
    n		Number of channels to be written.
    data	A complex array of n elements containing
		the correlation data.
    flags	Logical array of n elements. A true value for
		a channel indicates good data.				*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  UV *uv;
  int nchan;
  VARIABLE *v;
  char *status;

  uv = uvs[tno];

/* Initialise things if needed. */

  if( uv->wcorr == NULL ){
    uv->wcorr = uv_mkvar(tno,"wcorr", H_CMPLX);
    uv->wcorr->flags |= UVF_NOCHECK;
  }
  if( uv->wcorr_flags.handle == NULL) {
    status = (uv->wcorr_flags.offset == 0 ? "new" : "old" );
    uv->wcorr_flags.handle = mkopen_c(uv->tno,"wflags",status);
    if(uv->wcorr_flags.handle == NULL)
      BUG('f',"Failed to open the wcorr flags, in UVWWRITE");
  }

/* Update the size of the variable, if necessary. */

  v = uv->wcorr;
  nchan = NUMCHAN(v);
  if(n != nchan) uvputvri_c(tno,"nwide",&n,1);

/* Write out the flagging info. */

  if(uv->flags & UVF_RUNS)
    mkwrite_c(uv->wcorr_flags.handle,MK_RUNS,flags+1,uv->wcorr_flags.offset,
						n,*flags);
  else 
    mkwrite_c(uv->wcorr_flags.handle,MK_FLAGS,flags,uv->wcorr_flags.offset,
						n,n);
  uv->wcorr_flags.offset += n;

/* Write out the correlation data. */

  uvputvrc_c(tno,v->name,data,n);
}
/************************************************************************/
void uvsela_c(tno,object,string,datasel)
int tno;
char *object,*string;
int datasel;
/** uvsela -- Select or reject uv data, based on a character string	*/
/*&rjs									*/
/*:uv-i/o								*/
/*+ FORTRAN call sequence:

	subroutine uvsela(tno,object,string,flag)
	integer tno
	character object*(*),string*(*)
	logical flag

  This specifies the portion of the data to be selected by calls to
  uvread. This sets the value of a character string to compare against.

  Input:
    tno		Handle of the uv data file.
    object	This can be one of "source".
    string	String value, used in the selection process.
    flag	If true, the data is selected. If false, the data is
		discarded.						*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  UV *uv;
  SELECT *sel;
  int discard;

  uv = uvs[tno];

  discard = !datasel;
  uv->flags &= ~UVF_INIT;
  sel = uv->select;

/* Either move to the last "SELECT" structure, or create the new structure. */

  if(sel != NULL)while(sel->fwd != NULL) sel = sel->fwd;
  else{
    sel = (SELECT *)Malloc(sizeof(SELECT));
    sel->amp.select = sel->selants = sel->win.select = FALSE;
    sel->fwd = NULL; sel->opers = NULL;
    sel->maxoper = sel->noper = 0;
    sel->and = TRUE;
    uv->select = sel;
  }

/* Selection by source. */

  if(!strcmp(object,"source")){
    uv_addopers(sel,SEL_SRC,discard,0.0,0.0,string);
    uv->need_src = TRUE;

/* Some unknown form of selection. */

  } else {
    ERROR('w',(message,
	"Unrecognised selection \"%s\" ignored, in UVSELA",object));
  }
}
/************************************************************************/
void uvselect_c(tno,object,p1,p2,datasel)
int tno;
char *object;
int datasel;
double p1,p2;
/**uvselect -- Select or reject uv data.				*/
/*&rjs                                                                  */
/*:uv-i/o								*/
/*+ FORTRAN call sequence:

	subroutine uvselect(tno,object,p1,p2,flag)
	integer tno
	character object*(*)
	double precision p1,p2
	logical flag

  This specifies the portion of the data to be selected by calls to
  uvread. Normally data that are not selected, are not returned.
  Exceptions are the "window" and "amplitude" objects, which cause the
  corresponding visibilities to be flagged as bad, but returned anyway.

  Input:
    tno		Handle of the uv data file.
    object	This can be one of "time","antennae","visibility",
		"uvrange","pointing","amplitude","window","or","dra",
		"ddec","uvnrange","increment","ra","dec","and", "clear",
		"on","polarization","shadow","auto".
    p1,p2	Generally this is the range of values to select. For
		"antennae", this is the two antennae pair to select.
		For "antennae", a zero indicates "all antennae".
		For "shadow", a zero indicates use "antdiam" variable.
		For "on","window","polarization","increment","shadow" only
		p1 is used.
		For "and","or","clear","auto" p1 and p2 are ignored.
    flag	If true, the data is selected. If false, the data is
		discarded. Ignored for "and","or","clear".		*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  UV *uv;
  SELECT *sel;
  int i,i1,i2,discard;

  uv = uvs[tno];

  discard = !datasel;
  uv->flags &= ~UVF_INIT;

  if(!strcmp(object,"clear")){
    uv_free_select(uv->select);
    uv->select = NULL;
    return;
  }

/* Ignore "and" and "or" objects if this is the first call. */

  sel = uv->select;
  if(sel == NULL && (!strcmp(object,"or") || !strcmp(object,"and"))) return;

/* Either move to the last "SELECT" structure, or create the new structure. */

  if(sel != NULL)while(sel->fwd != NULL) sel = sel->fwd;
  else{
    sel = (SELECT *)Malloc(sizeof(SELECT));
    sel->amp.select = sel->selants = sel->win.select = FALSE;
    sel->fwd = NULL; sel->opers = NULL;
    sel->maxoper = sel->noper = 0;
    sel->and = TRUE;
    uv->select = sel;
  }

/* AND or OR operation. */

  if(!strcmp(object,"or") || !strcmp(object,"and")){
    sel->fwd = (SELECT *)Malloc(sizeof(SELECT));
    sel = sel->fwd;
    sel->amp.select = sel->selants = sel->win.select = FALSE;
    sel->opers = NULL;
    sel->fwd = NULL;
    sel->maxoper = sel->noper = 0;
    sel->and = (*object == 'a');

/* Selection by time. */

  } else if(!strcmp(object,"time")){
    if(p1 >= p2) BUG('f',"Min time is greater than or equal to max time, in UVSELECT.");
    uv_addopers(sel,SEL_TIME,discard,p1,p2,(char *)NULL);

/* Selection by "on" parameter. */

  } else if(!strcmp(object,"on")){
    uv_addopers(sel,SEL_ON,discard,0.0,0.0,(char *)NULL);
    uv->need_on = TRUE;

/* Selection by polarisation. */

  } else if(!strcmp(object,"polarization")){
    uv_addopers(sel,SEL_POL,discard,p1,p1,(char *)NULL);
    uv->need_pol = TRUE;

/* Subfield parameters, dra and ddec. */

  } else if(!strcmp(object,"dra")){
    if(p1 >= p2) BUG('f',"Min dra is greater than or equal to max dra, in UVSELECT.");
    uv_addopers(sel,SEL_DRA,discard,p1,p2,(char *)NULL);
    uv->need_dra = TRUE;
  } else if(!strcmp(object,"ddec")){
    if(p1 >= p2) BUG('f',"Min ddec is greater than or equal to max ddec, in UVSELECT.");
    uv_addopers(sel,SEL_DDEC,discard,p1,p2,(char *)NULL);
    uv->need_ddec = TRUE;

/* Phase centre parameters, ra and dec. */

  } else if(!strcmp(object,"ra")){
    if(p1 >= p2) BUG('f',"Min ra is greater than or equal to max ra, in UVSELECT.");
    uv_addopers(sel,SEL_RA,discard,p1,p2,(char *)NULL);
    uv->need_ra = TRUE;
  } else if(!strcmp(object,"dec")){
    if(p1 >= p2) BUG('f',"Min dec is greater than or equal to max dec, in UVSELECT.");
    uv_addopers(sel,SEL_DEC,discard,p1,p2,(char *)NULL);
    uv->need_dec = TRUE;

/* Selection by uv baseline. */

  } else if(!strcmp(object,"uvrange")){
    if(p1 >= p2) BUG('f',"Min uv is greater than or equal to max uv in UVSELECT");
    if(p1 < 0)   BUG('f',"Min uv is negative, in UVSELECT");
    uv_addopers(sel,SEL_UV,discard,p1*p1,p2*p2,(char *)NULL);
    uv->need_skyfreq = TRUE;

/* Select by sky frequency of the first channel. */

  } else if(!strcmp(object,"frequency")){
     if(p1 >= p2 || p1 <= 0) BUG('f',
        "Illegal values for sky frequency selection, in UVSELECT");
     uv_addopers(sel,SEL_FREQ,discard,p1,p2,(char *)NULL);
     uv->need_skyfreq = TRUE;

/* Selection by uv baseline, given in nanosec. */

  } else if(!strcmp(object,"uvnrange")){
    if(p1 >= p2) BUG('f',"Min uv is greater than or equal to max uv in UVSELECT");
    if(p1 < 0)   BUG('f',"Min uv is negative, in UVSELECT");
    uv_addopers(sel,SEL_UVN,discard,p1*p1,p2*p2,(char *)NULL);

/* Selection by pointing parameter. */

  } else if(!strcmp(object,"pointing")){
    if(p1 >= p2) BUG('f',"Min pointing is greater than or equal to max pointing, in UVSELECT");
    if(p1 < 0)   BUG('f',"Min pointing is negative, in UVSELECT");
    uv_addopers(sel,SEL_POINT,discard,p1,p2,(char *)NULL);
    uv->need_point = TRUE;

/* Selection by visibility number. */

  } else if(!strcmp(object,"visibility")){
    if(p1 > p2) BUG('f',"Min visib is greater than max visib, in UVSELECT");
    if(p1 < 1)   BUG('f',"Min visibility is negative, in UVSELECT");
    uv_addopers(sel,SEL_VIS,discard,p1,p2,(char *)NULL);

/* Selection by visibility increment. */

  } else if(!strcmp(object,"increment")){
    if(p1 < 1) BUG('f',"Bad increment selected, in UVSELECT.");
    uv_addopers(sel,SEL_INC,discard,p1,0.0,(char *)NULL);

/* Selection by shadowing. */

  } else if(!strcmp(object,"shadow")){
    if(p1 != 0 || p2 < 0) BUG('f',"Bad antenna diameter, in UVSELECT.");
    uv_addopers(sel,SEL_SHADOW,discard,p1,p2,(char *)NULL);
    uv->need_uvw = TRUE;

/* Pulsar bin selection. */

  } else if(!strcmp(object,"bin")){
    if(p1 < 1 || p2 < p1) BUG('f',"Bad pulsar bin number, in UVSELECT.");
    uv_addopers(sel,SEL_BIN,discard,p1,p2,(char *)NULL);
    uv->need_bin = TRUE;
    
/* Amplitude selection. */

  } else if(!strcmp(object,"amplitude")){
    if(sel->amp.select)
      BUG('f',"Cannot handle multiple amplitude selections in a clause");
    if(p1 < 0 || p2 <= p1)
      BUG('f',"Bad amplitude range selected, in UVSELECT.");
    sel->amp.discard = discard;
    sel->amp.loval = p1;
    sel->amp.hival = p2;
    sel->amp.select = TRUE;

/* Window selection. */

  } else if(!strcmp(object,"window")){
    if(!sel->win.select)
      for(i=0; i < MAXWIN; i++)sel->win.wins[i] = discard;
    i = p1 + 0.5;
    if(i < 1 || i > MAXWIN) BUG('f',"Too many windows");
    sel->win.wins[i-1] = !discard;
    uv->need_win = TRUE;

    sel->win.select = TRUE;
    sel->win.n = 0;
    for(i=0; i < MAXWIN; i++) if(sel->win.wins[i]){
      if(sel->win.n == 0) sel->win.first = i;
      sel->win.last = i;
      sel->win.n++;
    }

/* Autocorrelation data. */

  } else if(!strcmp(object,"auto")){
    if(!sel->selants){
      for(i=0; i < MAXANT*(MAXANT+1)/2; i++)sel->ants[i] = !discard;
      sel->selants = TRUE;
    }
    for(i=0; i < MAXANT; i++)sel->ants[((i+1)*i)/2 + i] = discard;

/* Antennae and baseline selection. */

  } else if(!strcmp(object,"antennae")){
    if(!sel->selants){
      for(i=0; i < MAXANT*(MAXANT+1)/2; i++)sel->ants[i] = !discard;
      sel->selants = TRUE;
    }
    i1 = max(p1, p2) + 0.5;
    i2 = min(p1, p2) + 0.5;
    if(i1 < 0 || i1 > MAXANT) BUG('f',"Too many antennae");
    if(i2 < 0 || i2 > MAXANT) BUG('f',"Too many antennae");
    if(i1 == 0){
      for(i=0; i < MAXANT*(MAXANT+1)/2; i++)sel->ants[i] = discard;
    } else if(i2 == 0){
      for(i=1; i <= i1; i++)		sel->ants[(i1*(i1-1))/2+i-1] = discard;
      for(i=i1+1; i <= MAXANT; i++)	sel->ants[(i*(i-1))/2+i1 -1] = discard;
    } else {
      sel->ants[(i1*(i1-1))/2+i2-1] = discard;
    }

/* Some unknown form of selection. */

  } else {
    ERROR('w',(message,
	"Unrecognised selection \"%s\" ignored, in UVSELECT",object));
  }
}
/************************************************************************/
private void uv_addopers(sel,type,discard,p1,p2,ps)
SELECT *sel;
int type,discard;
double p1,p2;
char *ps;
{
  int n,i;
  OPERS *oper;

/* Allocate more space if needed. */

  if(sel->noper == sel->maxoper){
    sel->maxoper = max(2*sel->maxoper,16);
    sel->opers = (OPERS *)Realloc((char *)(sel->opers),sel->maxoper*sizeof(OPERS));
  }

/* Shift the list down, to make space for the newcomer. */

  n = sel->noper++;
  for(i = n-1; i >= 0; i--){
    oper = sel->opers + i;
    if( oper->type > type ) memcpy((char *)(oper+1),(char *)oper,sizeof(OPERS));
    else break;
  }

/* Squeeze the newcomer in. */

  oper = sel->opers + i + 1;
  oper->type = type;
  oper->discard = discard;
  oper->loval = p1;
  oper->hival = p2;
  oper->stval = NULL;
  if(ps != NULL){
    oper->stval = (char *)Malloc(strlen(ps)+1);
    strcpy(oper->stval,ps);
  }
}
/************************************************************************/
void uvset_c(tno,object,type,n,p1,p2,p3)
int tno,n;
double p1,p2,p3;
char *object,*type;
/**uvset -- Set up the uv linetype, and other massaging steps.		*/
/*&rjs                                                                  */
/*:uv-i/o								*/
/*+ FORTRAN call sequence:

	subroutine uvset(tno,object,type,n,p1,p2,p3)
	integer tno
	character object*(*),type*(*)
	integer n
	real p1,p2,p3

  Set up the way uvread behaves. This determines whether uvread returns
  correlation channels, wide-band channels or velocity channels. This also
  sets up whether u and v are returned in wavelengths or nanosec, and
  what planet processing is performed.

  Input:
    tno		Handle of the uv data set.
    object	Name of the object that we are setting the type of.
    type	The type of data that the user wants returned.
    n		Some integer parameter.
    p1,p2,p3	Some real parameters.					*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  UV *uv;

  uv = uvs[tno];
  uv->flags &= ~UVF_INIT;

  if(!strcmp(object,"data")){
    uvset_linetype(&uv->data_line,type,n,p1,p2,p3);
  } else if(!strcmp(object,"reference")) {
    uvset_linetype(&uv->ref_line,type,1,p1,p2,p2);
  } else if(!strcmp(object,"coord")) {
    uvset_coord(uv,type);
  } else if(!strcmp(object,"planet")) {
    uvset_planet(uv,p1,p2,p3);
  } else if(!strcmp(object,"preamble")) {
    uvset_preamble(uv,type);
  } else if(!strcmp(object,"selection")) {
    uvset_selection(uv,type,n);
  } else if(!strcmp(object,"flags")) {
    if(!strcmp(type,"logical"))
      uv->flags &= ~UVF_RUNS;
    else if(!strcmp(type,"runs"))
      uv->flags |= UVF_RUNS;
    else {
      ERROR('f',(message,"Unrecognised flags mode \'%s\', in UVSET",type));
    }
  } else if(!strcmp(object,"corr")) {
    if(uv->corr != NULL)return;
    if(!strcmp(type,"r"))
      uv->corr = uv_mkvar(tno,"corr", H_REAL );
    else if(!strcmp(type,"c" ))
      uv->corr = uv_mkvar(tno,"corr", H_CMPLX );
    else if(!strcmp(type,"j"))
      uv->corr = uv_mkvar(tno,"corr", H_INT2 );
    else
      ERROR('f',(message,"Unsupported correlation type %s, in UVSET",type));
  } else {
    ERROR('w',(message,"Unrecognised object \"%s\" ignored, in UVSET.",object));
  }
}
/************************************************************************/
private void uvset_preamble(uv,type)
UV *uv;
char *type;
/*
  Set the preamble that the user wants to use.
------------------------------------------------------------------------*/
{
  char varnam[MAXNAM+1],*s;
  int n,ok;
  VARIABLE *v;

  uv->flags &= ~UVF_DOW;
  if(uv->flags & UVF_NEW){
    uv->presize = 3;
    if(!strcmp(type,"uvw/time/baseline"))uv->flags |= UVF_DOW;
    else if(strcmp(type,"uv/time/baseline")){
      ERROR('f',(message,"Unsupported preamble \"%s\",in UVSET.",type));}
  } else {
    n = 0;
    while(*type){
      if(n >= MAXPRE){
	ERROR('f',(message,"Too many parameters in preamble \"%s\".",type));}

/* Get the variable name. */

      s = varnam;
      while(*type != 0 && *type != '/')*s++ = *type++;
      if(*type == '/')type++;
      *s = 0;

/* Locate the appropriate variable. */

      if(!strcmp(varnam,"uv")){
	v = uv->prevar[n] = uv_locvar(uv->tno,"coord");
	ok = (v != NULL && v->type == H_DBLE);
      } else if(!strcmp(varnam,"uvw")){
	v = uv->prevar[n] = uv_locvar(uv->tno,"coord");
	uv->flags |= UVF_DOW;
	ok = (v != NULL && v->type == H_DBLE);
      } else {
	v = uv->prevar[n] = uv_locvar(uv->tno,varnam);
	ok = (v == NULL || v->type == H_INT  || 
			   v->type == H_REAL || v->type == H_DBLE);
      }
      if(!ok){ERROR('f',(message,"Invalid preamble \"%s\".",type));}
      n++;
    }
    uv->presize = n;
  }
}
/************************************************************************/
private void uvset_selection(uv,type,n)
UV *uv;
char *type;
int n;
/*
  Set the way the uvselect routine works.
------------------------------------------------------------------------*/
{
  if(!strcmp(type,"amplitude")){
    uv->apply_amp = n > 0;
  } else if(!strcmp(type,"window")){
    uv->apply_win = n > 0;
  } else {
    ERROR('w',(message,"Unrecognised type %s ignored, in UVSET(amplitude)"));
  }
}
/************************************************************************/
private void uvset_planet(uv,p1,p2,p3)
UV *uv;
double p1,p2,p3;
/*
  Set the reference parameters for a planet, for scaling and rotation.
------------------------------------------------------------------------*/
{
  uv->ref_plmaj = p1;
  uv->ref_plmin = p2;
  uv->ref_plangle = p3;
  uv->need_planet = TRUE;
}
/************************************************************************/
private void uvset_coord(uv,type)
UV *uv;
char *type;
/*
  Set the flags to do with the processing of uv coordinates.

  Input:
    uv		The UV data structure.
    type	A char string containing a type consisting of the following
		string separated by dashes.
		 "wavelength"	Return u,v in units of wavelengths.
		 "nanosec"	Return u,v in units of nanosecs.
------------------------------------------------------------------------*/
{
  if(!strcmp(type,"wavelength")){	uv->need_skyfreq = TRUE;
					uv->flags |=  UVF_WAVELENGTH; }
  else if(!strcmp(type,"nanosec")){	uv->flags &= ~UVF_WAVELENGTH; }
  else{
     ERROR('w',(message,
	"Unrecognised coordinate type \"%s\" ignored, in UVSET",type));
  }
}
/************************************************************************/
private void uvset_linetype(line,type,n,start,width,step)
LINE_INFO *line;
char *type;
int n;
double start,width,step;
/*
  Decode the line type.
  Input:
    line	The LINE_INFO structure describing the line type.
    type	A char string being one of
		"velocity"
		"channel"
		"wide"
    n		Number of channels.
    start	First channel to select.
    width	Width of channel.
    step	Increment between channels.
------------------------------------------------------------------------*/
{
  if(!strcmp(type,"velocity") || !strcmp(type,"felocity")){
    if(width < 0) BUG('f',"Bad width in UVSET(line)");
    if(n < 0) BUG('f',"Bad number of channels, in UVSET(line).");
    if((width == 0 || n == 0) && (step != 0 || start != 0 || width != 0))
      BUG('f',"Invalid line parameters in UVSET(line)");
    line->linetype = (*type == 'v' ? LINE_VELOCITY : LINE_FELOCITY);
    line->n = n;
    line->fstart = start; line->fwidth = width; line->fstep  = step;
  } else if(!strcmp(type,"wide")) {
    if(width < 1 || step < 1 || step < width)
      BUG('f',"Bad width or step in UVSET(line)");
    if(start < 1 ) BUG('f',"Bad start value in UVSET(line)");
    if(n < 0) BUG('f',"Bad number of channels, in UVSET(line).");
    line->linetype = LINE_WIDE;
    line->n = n;
    line->start = start-1; line->width = width; line->step  = step;
  } else if(!strcmp(type,"channel")) {
    if(width < 1 || step < 1)
      BUG('f',"Bad width or step in UVSET(line)");
    if(start < 1 ) BUG('f',"Bad start value in UVSET(line)");
    if(n < 0) BUG('f',"Bad number of channels, in UVSET(line).");
    line->linetype = LINE_CHANNEL;
    line->n = n;
    line->start = start-1; line->width = width; line->step  = step;
  } else {
    ERROR('w',(message,
      "Unrecognised line type \"%s\" ignored, in UVSET",type));
  }
}
/************************************************************************/
void uvread_c(tno,preamble,data,flags,n,nread)
int tno,n,*flags,*nread;
double preamble[4];
float *data;
/**uvread -- Read in some uv correlation data.				*/
/*&rjs                                                                  */
/*:uv-i/o								*/
/*+ FORTRAN call sequence:

	subroutine uvread(tno,preamble,data,flags,n,nread)
	integer tno,n,nread
	double precision preamble(4)
	complex data(n)
	logical flags(n)

  This reads a single visibility from the data file. This starts by scanning
  the uv data stream until a correlation record is found. The correlations
  are then converted to complex numbers if necessary, and returned to the
  caller. Uvread also performs some massaging (see uvset) and selection
  (see uvselect) steps.

  Input:
    tno		Handle of the uv data set.
    n		Max number of channels that can be read.
  Output:
    preamble	A double array of 4 elements giving u,v, time and
		baseline number (in that order).
    data	A real array of at least n complex elements (or 2n real
		elements). This returns the correlation data.
    flags	Logical array of at least n elements. A true value for
		a channel indicates good data.
    nread	Number of correlations returned. On end-of-file, zero
		is returned.						*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  UV *uv;
  int more,nchan;
  VARIABLE *v;
  uv = uvs[tno];

/* Initialise everything if this is the first call to uvread. */

  if(!(uv->flags & UVF_INIT)) uvread_defline(tno);
  v = (uv->data_line.linetype == LINE_WIDE ? uv->wcorr : uv->corr);
  uv->corr_flags.init = FALSE;
  uv->wcorr_flags.init = FALSE;

/* Scan the input data stream until we hit some correlation data. */

  *nread = 0;
  more = TRUE;
  uv->mark = uv->callno + 1;
  uv->flags &= ~(UVF_UPDATED | UVF_COPY);

/* Scan the input data stream, and do any selection necessary. */

  while(more){
    if(uv->maxvis > 0 && uv->callno > uv->maxvis) return;
    do {
      if(uv_scan(uv,(VARIABLE *)NULL) != 0)return;
      if(!(uv->flags & UVF_INIT)) uvread_init(tno);
      if(uv->corr != NULL)if(uv->corr->callno == uv->callno){
        nchan = NUMCHAN(uv->corr);
        uv->corr_flags.offset += nchan;
      }
      if(uv->wcorr != NULL)if(uv->wcorr->callno == uv->callno){
        nchan = NUMCHAN(uv->wcorr);
        uv->wcorr_flags.offset += nchan;
      }
    } while(v->callno < uv->callno);

/* Perform uv selection. */

    uv->amp = &noamp;
    uv->win = &truewin;
    if(uv->select != NULL) more = uvread_select(uv);
    else		   more = FALSE;
  }

/* Update the planet parameters, if needed. */

  if(uv->flags & UVF_UPDATED_PLANET) uvread_updated_planet(uv);
  if(uv->flags & UVF_UPDATED_UVW)    uvread_updated_uvw(uv);

/* Apply linetype processing and planet scaling. */

  *nread = uvread_line(uv,&(uv->data_line),data,n,flags,&(uv->actual_line));
  if(*nread == 0)return;

/* Get preamble variables. */

  uvread_preamble(uv,preamble);

/* Divide by the reference line if there is one. */

  if(uv->ref_line.linetype != LINE_NONE) uvread_reference(uv,data,flags,*nread);
}
/************************************************************************/
private void uvread_preamble(uv,preamble)
UV *uv;
double *preamble;
/*
  Get the preamble associated with this record.
------------------------------------------------------------------------*/
{
  VARIABLE *v;
  double scale,uu,vv,ww,*coord;
  int bl,i1,i2,i;


  for(i=0; i < uv->presize; i++){
    v = uv->prevar[i];
    if(v == NULL){
      *preamble++ = 0;
    } else if(v == uv->coord){
      coord = (double *)(uv->coord->buf);
      uu = coord[0];
      vv = coord[1];
      if(uv->flags & UVF_REDO_UVW){
	bl = *((float *)(uv->bl->buf)) + 0.5;
	i1 = bl / 256 - 1;
	i2 = bl % 256 - 1;
	ww = uv->uvw->ww[i2] - uv->uvw->ww[i1];
      } else if(uv->flags & UVF_DOW) {
	ww = (VARLEN(uv->coord) >= 3 ? coord[2] : 0.0);
      }
      scale = (uv->flags & UVF_WAVELENGTH ? uv_getskyfreq(uv,uv->win) : 1.0);
      *preamble++ = scale * ( uv->pluu * uu + uv->pluv * vv );
      *preamble++ = scale * ( uv->plvu * uu + uv->plvv * vv );
      if(uv->flags & UVF_DOW ) *preamble++ = scale * ww;
    } else if(v->type == H_DBLE){
      *preamble++ = *(double *)(v->buf);
    } else if(v->type == H_REAL){
      *preamble++ = *(float *)(v->buf);
    } else if(v->type == H_INT){
      *preamble++ = *(int *)(v->buf);
    }
  }
}
/************************************************************************/
void uvwread_c(tno,data,flags,n,nread)
int tno,n,*flags,*nread;
float *data;
/**uvwread -- Read in the wideband uv correlation data.			*/
/*&rjs                                                                  */
/*:uv-i/o								*/
/*+ FORTRAN call sequence:

	subroutine uvwread(tno,data,flags,n,nread)
	integer tno,n,nread
	complex data(n)
	logical flags(n)

  This reads a single wideband visibility record from the data file.
  This should generally be called after uvread. It performs no scanning
  before returning the data. Thus it always returns any wideband data
  (even if uvread has detected end-of-file). Although uvwread is independent
  of the linetype set with uvset, it otherwise generally performs the
  same massaging steps as uvread (e.g. data selection, amplitude flagging
  and planet scaling).

  Input:
    tno		Handle of the uv data set.
    n		Max number of channels that can be read.
  Output:
    data	A array of at least n complex elements (or 2n real
		elements). This returns the correlation data.
    flags	Logical array of at least n elements. A true value for
		a channel indicates good data.
    nread	Number of correlations returned. 			*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  UV *uv;
  VARIABLE *v;
  LINE_INFO line,dummy;

  uv = uvs[tno];

/* Determine the number of channels in the output, if none where given. */

  if(uv->wcorr == NULL){
    if(uv_locvar(tno,"wcorr") != NULL){
      uv->wcorr = uv_checkvar(tno,"wcorr",H_CMPLX);
    }else{
      *nread = 0;
      return;
    }
  }
  if(uv->wcorr_flags.handle == NULL && uv->wcorr_flags.exists){
    uv->wcorr_flags.handle = mkopen_c(uv->tno,"wflags","old");
    uv->wcorr_flags.exists =  uv->wcorr_flags.handle != NULL;
    if(!uv->wcorr_flags.exists)
      BUG('w',"No flags found for wcorr -- assuming data are good");
   }
  v = uv->wcorr;

  line.n = NUMCHAN(v);
  line.linetype = LINE_WIDE;
  line.width = line.step = 1;
  line.start = 0;

  if(line.n > n)
    BUG('f',"Callers buffer too small for wide data, in UVWREAD");

/* Apply linetype processing and planet scaling. */

  *nread = uvread_line(uv,&line,data,n,flags,&dummy);
  if(*nread == 0)return;

/* Apply reference linetype, if there is one. */

  if(uv->ref_line.linetype != LINE_NONE) uvread_reference(uv,data,flags,*nread);
}
/************************************************************************/
private void uvread_reference(uv,data,flags,n)
UV *uv;
float *data;
int *flags,n;
/*
  Divide the data by the reference line. If the reference is bad, then mark
  all the data as bad.
------------------------------------------------------------------------*/
{
  float refline[2],t,rp,im;
  int refflag,nread;
  int i;
  LINE_INFO dummy;

  nread = uvread_line(uv,&(uv->ref_line),refline,1,&refflag,&dummy);
  if(nread <= 0 || refflag == FORT_FALSE){
    for(i = 0; i < n; i++)*flags++ = FORT_FALSE;
  } else {
    t = 1.0/(refline[0]*refline[0] + refline[1]*refline[1]);
    rp =  t * refline[0];
    im = -t * refline[1];
    for(i = 0; i < n; i++){
      t = *data * im + *(data+1) * rp;
      *data = *data * rp - *(data+1) * im;
      data++;
      *data++ = t;
    }
  }
}
/************************************************************************/
private double uv_getskyfreq(uv,win)
UV *uv;
WINDOW *win;
/*
  This computes the sky frequency for a particular something.
------------------------------------------------------------------------*/
{
  int i,i0,*nschan,start;
  float vobs;
  double *sdf,*sfreq,restfreq;
  double temp;

/* Check the validity of any window specification. */

  if(win->first != 0){
    if(win->first >= VARLEN(uv->nschan))
      BUG('f',"Invalid window selection, in UVREAD(skyfreq)");
  }

  if(uv->data_line.linetype == LINE_VELOCITY){
    start = win->first;
    if(uv->data_line.n == 0 || uv->data_line.fwidth == 0)
      uvread_defvelline(uv,&(uv->data_line),win);
  } else if(uv->data_line.linetype == LINE_FELOCITY){
    start = win->first;
    uvread_defvelline(uv,&(uv->data_line),win);
  } else {
    start = uv->data_line.start;
    if( win->first != 0 && uv->data_line.linetype == LINE_CHANNEL){
      nschan = (int *)uv->nschan->buf;
      for(i=0; i < win->first; i++)start += *nschan++;
    }
  }
  if(! (uv->flags & UVF_UPDATED_SKYFREQ) && start == uv->skyfreq_start)
    return(uv->skyfreq);

/* We have to recompute. First indicate that we have doe that already */

  uv->skyfreq_start = start;
  uv->flags &= ~UVF_UPDATED_SKYFREQ;

/* CHANNEL linetype. */

  if(uv->data_line.linetype == LINE_CHANNEL){
    nschan = (int *)uv->nschan->buf;
    sfreq  = (double *)uv->sfreq->buf;
    sdf =    (double *)uv->sdf->buf;
    temp = 0;
    while(start >= *nschan){
      start -= *nschan++;
      sfreq++; sdf++;
    }
    for(i=0; i<uv->data_line.width; i++){
      if(start == *nschan){
        start = 0;
        sfreq++; sdf++; nschan++;
      }
      temp += *sfreq + start * *sdf;
      start++;
    }
    uv->skyfreq = temp / uv->data_line.width;

/* VELOCITY linetype. */

  } else if(uv->data_line.linetype == LINE_VELOCITY){
    restfreq = *((double *)uv->restfreq->buf + start);
    vobs   = *(float  *)uv->veldop->buf - *(float *)uv->vsource->buf;
    uv->skyfreq = restfreq * (1 - (uv->data_line.fstart + vobs) / CKMS);

/* WIDE channels. */

  } else if(uv->data_line.linetype == LINE_WIDE){
    temp = 0;
    for(i=0, i0 = start; i<uv->data_line.width; i++, i0++){
      temp += *((float *)uv->wfreq->buf + i0);
    }
    uv->skyfreq = temp / uv->data_line.width;
  }
  return(uv->skyfreq);
}
/************************************************************************/
private void uvread_updated_planet(uv)
UV *uv;
/*
  This determines the planet rotation and scaling factors.

------------------------------------------------------------------------*/
{
  float plmaj,plmin,plangle;
  double theta;
  
/* Determine planet rotation and scaling factor. */

  if(uv->ref_plmaj * uv->ref_plmin <= 0){
    uv->ref_plmaj = *(float *)uv->plmaj->buf;
    uv->ref_plmin = *(float *)uv->plmin->buf;
    uv->ref_plangle = *(float *)uv->plangle->buf;
  } else {
    plmaj = *(float *)uv->plmaj->buf;
    plmin = *(float *)uv->plmin->buf;
    plangle = *(float *)uv->plangle->buf;
    if(plmaj > 0.0 && plmin > 0.0){
      uv->plscale = (uv->ref_plmaj * uv->ref_plmin) / (plmaj * plmin ) ;
      theta = PI/180 * (plangle - uv->ref_plangle);
      uv->pluu =  cos(theta) * (plmaj / uv->ref_plmaj);
      uv->pluv = -sin(theta) * (plmaj / uv->ref_plmaj);
      uv->plvu = -uv->pluv;
      uv->plvv =  uv->pluu;
    } else {
      uv->plscale = 1;
      uv->pluu = uv->plvv = 1;
      uv->plvu = uv->pluv = 0;
    }
  }
  uv->flags &= ~UVF_UPDATED_PLANET;
}
/************************************************************************/
private int uvread_select(uv)
UV *uv;
{
  int i1,i2,bl,pol,n,nants,inc,selectit,selprev,discard,binlo,binhi;
  float *point,pointerr,dra,ddec;
  double time,t0,uu,vv,uv2,uv2f,ra,dec,skyfreq,diameter;
  SELECT *sel;
  OPERS *op;
  WINDOW *win;

  selprev = TRUE;

  for(sel = uv->select; sel != NULL; sel = sel->fwd){
    if((!selprev && sel->and) || (selprev && !sel->and)) continue;

    discard = FALSE;
    n = 0;
    op = sel->opers;

/* Apply antennae/baseline selection. */

    if(sel->selants){
      bl = *((float *)(uv->bl->buf)) + 0.5;
      i1 = max( bl / 256, bl % 256);
      i2 = min( bl / 256, bl % 256);
      if(i2 < 1 || i1 > MAXANT){
	BUG('f',"Bad antenna numbers when doing selection, in UVREAD(select)"); }
      discard = sel->ants[(i1*(i1-1))/2+i2-1];
      if(discard) goto endloop;
    }
    if( n >= sel->noper ) goto endloop;

/* NOTE: The following tests must be in increasing size of the SEL_??
   parameters, because the list of selections has been sorted. Note that
   the SEL_?? parameters are numbered in roughly increasing order of
   the difficulty in computing them. */

/* Apply visibility number selection. */

    if(op->type == SEL_VIS){
      discard = !op->discard;
      while(n < sel->noper && op->type == SEL_VIS){
        if(op->loval <= uv->callno && uv->callno <= op->hival)
	  discard = op->discard;
        op++; n++;
      }
      if(discard || n >= sel->noper) goto endloop;
    }

/* Apply time selection. */

    if(op->type == SEL_TIME){
      time = *((double *)(uv->time->buf));
      i1 = time - 0.5;
      t0 = time - i1 - 0.5;
      discard = !op->discard;
      while(n < sel->noper && op->type == SEL_TIME){
        if( (op->loval <= time && time <= op->hival) ||
	    (op->loval <= t0   && t0   <= op->hival))
	  discard = op->discard;
        op++; n++;
      }
      if(discard || n >= sel->noper) goto endloop;
    }

/* Apply UV range selection, coordinates given in wavelengths. */

    if(op->type == SEL_UVN){
      uu = *((double *)(uv->coord->buf));
      vv = *((double *)(uv->coord->buf) + 1);
      uv2 = uu*uu + vv*vv;
      discard = !op->discard;
      while(n < sel->noper && op->type == SEL_UVN){
        if(op->loval <= uv2 && uv2 <= op->hival)
	  discard = op->discard;
        op++; n++;
      }
      if(discard || n >= sel->noper) goto endloop;
    }

/* Apply pointing selection. */

    if(op->type == SEL_POINT){
      bl = *((float *)(uv->bl->buf)) + 0.5;
      i1 = max( bl / 256, bl % 256);
      i2 = min( bl / 256, bl % 256);
      discard = !op->discard;
      point = (float *)(uv->axisrms->buf);
      nants = VARLEN(uv->axisrms)/2;
      if(i2 < 1 || i1 > nants){
	BUG('f',"Bad antenna numbers when checking pointing, in UVREAD(select)"); }
      pointerr = max( *(point+2*i1),*(point+2*i1-1));
      pointerr = max( *(point+2*i2), pointerr);
      pointerr = max( *(point+2*i2-1), pointerr);
    
      while(n < sel->noper && op->type == SEL_POINT){
        if(op->loval <= pointerr && pointerr <= op->hival)
	  discard = op->discard;
        op++; n++;
      }
      if(discard || n >= sel->noper) goto endloop;
    }

/* Apply delta RA selection. */

    if(op->type == SEL_DRA){
      discard = !op->discard;
      if(!uv->need_dra) dra = 0;
      else dra = *(float *)uv->dra->buf;
      while(n < sel->noper && op->type == SEL_DRA){
        if(op->loval <= dra && dra <= op->hival)
	  discard = op->discard;
        op++; n++;
      }
      if(discard || n >= sel->noper) goto endloop;
    }

/* Apply delta dec selection. */

    if(op->type == SEL_DDEC){
      discard = !op->discard;
      if(!uv->need_ddec) ddec = 0;
      else ddec = *(float *)uv->ddec->buf;
      while(n < sel->noper && op->type == SEL_DDEC){
        if(op->loval <= ddec && ddec <= op->hival)
	  discard = op->discard;
        op++; n++;
      }
      if(discard || n >= sel->noper) goto endloop;
    }

/* Apply visibility number increment selection. */

    if(op->type == SEL_INC){
      discard = !op->discard;
      while(n < sel->noper && op->type == SEL_INC){
	inc = op->loval + 0.5;
        if( (uv->callno - 1) % inc == 0) discard = op->discard;
        op++; n++;
      }
      if(discard || n >= sel->noper) goto endloop;
    }

/* Apply RA selection. */

    if(op->type == SEL_RA){
      discard = !op->discard;
      if(uv->ra->type == H_REAL)ra = *(float  *)(uv->ra->buf);
      else			ra = *(double *)(uv->ra->buf);
      if(uv->need_dra){
        if(uv->dec->type == H_REAL)dec = *(float  *)(uv->dec->buf);
        else			   dec = *(double *)(uv->dec->buf);
	if(uv->need_ddec)dec += *(float *)(uv->ddec->buf);
	ra += *(float *)(uv->dra->buf) / cos(dec);
      }
      while(n < sel->noper && op->type == SEL_RA){
        if(op->loval <= ra && ra <= op->hival)
	  discard = op->discard;
        op++; n++;
      }
      if(discard || n >= sel->noper) goto endloop;
    }

/* Apply DEC selection. */

    if(op->type == SEL_DEC){
      if(uv->dec->type == H_REAL)dec = *(float  *)(uv->dec->buf);
      else			 dec = *(double *)(uv->dec->buf);
      if(uv->need_ddec)dec += *(float *)(uv->ddec->buf);
      discard = !op->discard;
      while(n < sel->noper && op->type == SEL_DEC){
        if(op->loval <= dec && dec <= op->hival)
	  discard = op->discard;
        op++; n++;
      }
      if(discard || n >= sel->noper) goto endloop;
    }

/* Apply polarization selection. */

    if(op->type == SEL_POL){
      discard = !op->discard;
      if(uv->need_pol) pol = *(int *)(uv->pol->buf);
      else	       pol = 1;
      while(n < sel->noper && op->type == SEL_POL){
        if(op->loval == pol) discard = op->discard;
        op++; n++;
      }
      if(discard || n >= sel->noper) goto endloop;
    }

/* Apply selection based on the "on" parameter. */

    if(op->type == SEL_ON){
      discard = !op->discard;
      while(n < sel->noper && op->type == SEL_ON){
        if(*(int *)(uv->on->buf) == 1)
	  discard = op->discard;
        op++; n++;
      }
      if(discard || n >= sel->noper) goto endloop;
    }

/* Apply source name selection. */

    if(op->type == SEL_SRC){
      discard = !op->discard;
      while(n < sel->noper && op->type == SEL_SRC){
	if(uvread_match(op->stval,uv->source->buf,uv->source->length))
	  discard = op->discard;
	op++; n++;
      }
      if(discard || n >= sel->noper) goto endloop;
    }

/* Apply UV range selection, where u-v is in wavelengths. */

    if(op->type == SEL_UV){
      discard = !op->discard;
      win = (sel->win.select ? &(sel->win) : uv->win);
      skyfreq = uv_getskyfreq(uv,win);
      uu = *((double *)(uv->coord->buf));
      vv = *((double *)(uv->coord->buf) + 1);
      uv2f = (uu*uu + vv*vv) * skyfreq * skyfreq;
      while(n < sel->noper && op->type == SEL_UV){
        if(op->loval <= uv2f && uv2f <= op->hival)
	  discard = op->discard;
        op++; n++;
      }
      if(discard || n >= sel->noper) goto endloop;
    }

/* Apply sky frequency-based selection. */

    if(op->type == SEL_FREQ){
      discard = !op->discard;
      win = (sel->win.select ? &(sel->win) : uv->win);
      skyfreq = uv_getskyfreq(uv,win);
      while(n < sel->noper && op->type == SEL_FREQ){
        if(op->loval <= skyfreq && skyfreq <= op->hival)
	  discard = op->discard;
        op++; n++;
      }
      if(discard || n >= sel->noper) goto endloop;
    }

/* Apply shadowing selection. */

    if(op->type == SEL_SHADOW){
      discard = !op->discard;
      while(n < sel->noper && op->type == SEL_SHADOW){
	diameter = op->hival;
	if(diameter <= 0 && uv->antdiam != NULL)
	  diameter = *(float *)(uv->antdiam->buf);
	if(diameter <= 0)
	  BUG('f',"No antenna diameter info available, in UVREAD(shadow_select)");
        if(uvread_shadowed(uv,diameter))
	  discard = op->discard;
        op++; n++;
      }
      if(discard || n >= sel->noper) goto endloop;
    }

/* Apply apply pulsar bin selection. */

    if(op->type == SEL_BIN){
      discard = !op->discard;
      while(n < sel->noper && op->type == SEL_BIN){
	binlo = op->loval + 0.5;
	binhi = op->hival + 0.5;
        if(binlo <= *(int *)(uv->bin->buf) &&
	   *(int *)(uv->bin->buf) <= binhi ) discard = op->discard;
        op++; n++;
      }
      if(discard || n >= sel->noper) goto endloop;
    }


/* We have processed this selection clause. Now determine whether the
   overall selection criteria is to select or discard. Note that we cannot
   get here if sel->and == TRUE and selprev == FALSE. 		*/

endloop:
    selectit = !discard;
    if(selectit){
       if(uv->amp->select && sel->amp.select)
	BUG('f',"Multiple amplitude selection clauses are active");
      if( sel->amp.select ) uv->amp = &(sel->amp);
      if( uv->win->select && sel->win.select)
	BUG('f',"Multiple window selection clauses are active");
      if( sel->win.select ) uv->win = &(sel->win);
    }
    selprev = selectit;
  }

/* Check the validity of the window selection. */

  if(selprev && uv->win->first != 0){
    if(uv->win->first >= VARLEN(uv->nschan))
      BUG('f',"Invalid window selection, in UVREAD(select)");
  } 

  return(!selprev);
}
/************************************************************************/
private int uvread_match(s1,s2,length)
char *s1,*s2;
int length;
/*
    This matches two source names. The first name may contain wildcards
    (just asterisks, not the full blown UNIX thingos). The second string
    is not zero terminated.

  Input:
    s1		The first string. Can contain wildcards. Zero terminated.
    s2		The second string. No wildcards. Not zero terminated.
    length	Length of the second string.
  Output:
    uvread_match True if the two strings match.
------------------------------------------------------------------------*/
{
  while(*s1 && length > 0){
    if(*s1 == '*'){
      s1++;
      if(*s1 == 0)return 1;
      while(length > 0){
        if(uvread_match(s1,s2,length)) return 1;
        s2++;
	length--;
      }
      return 0;
    } else {
      if(*s1++ != *s2++) return 0;
      length--;
    }
  }
  return *s1 == 0 && length == 0;
}
/************************************************************************/
private int uvread_shadowed(uv,diameter)
UV *uv;
double diameter;
/*
    This determines if a particular baseline is shadowed.

    Inputs:
      uv	The uv data structure.
      diameter	The dish diameter, in meters.
------------------------------------------------------------------------*/
{
  int nants,i,j,i1,i2,i0,bl;
  double u,v,w,limit;
  UVW *uvw;

/* Get the table of U,V,W coordinates. */

  if(uv->flags & UVF_UPDATED_UVW) uvread_updated_uvw(uv);
  uvw = uv->uvw;

/* Convert the diameter to nanosec, and square it. */

  limit = diameter / CKMS * 1e6;
  limit *= limit;

/* Set the number of antennae as the number of positions that we have. */

  nants = uv->uvw->nants;
  bl = *((float *)(uv->bl->buf)) + 0.5;
  i1 = bl / 256 - 1;
  i2 = bl % 256 - 1;
  if(i1 < 0 || i2 >= nants){
    BUG('f',"Bad antenna numbers when checking shadowing, in UVREAD(select)"); }

  for(j=0; j < 2; j++){
    i0 = ( j == 0 ? i1 : i2);
    if(i1 == i2 && j == 1)return(0);
    for(i=0; i < nants; i++){
      if(i != i0){
        u = uvw->uu[i] - uvw->uu[i0];
        v = uvw->vv[i] - uvw->vv[i0];
        w = uvw->ww[i] - uvw->ww[i0];
        if(u*u + v*v <= limit && w >= 0) return(1);
      }
    }
  }
  return(0);
}
/************************************************************************/
private void uvread_updated_uvw(uv)
UV *uv;
/*
  Update the table of vectors used to computer u,v,w.
------------------------------------------------------------------------*/
{
  UVW *uvw;
  double ha,dec,sinha,cosha,sind,cosd;
  double *posx,*posy,*posz,bx,by,bz,bxy,byx;
  int i;

  if(uv->uvw == NULL)uv->uvw = (UVW *)Malloc(sizeof(UVW));
  uvw = uv->uvw;
  uvw->nants = VARLEN(uv->antpos)/3;

/* Get trig functions of the hour angle and the declination. */

  ha = *(double *)(uv->lst->buf) - *(double *)(uv->obsra->buf);
  dec = *(double *)(uv->obsdec->buf);
  sinha = sin(ha);  cosha = cos(ha);
  sind = sin(dec); cosd = cos(dec);

  posx = (double *)(uv->antpos->buf);
  posy = posx + uvw->nants;
  posz = posy + uvw->nants;
  for(i=0; i < uvw->nants; i++){
    bx = *posx++;
    by = *posy++;
    bz = *posz++;
    bxy =  bx*sinha + by*cosha;
    byx = -bx*cosha + by*sinha;
    uvw->uu[i] = bxy;
    uvw->vv[i] = byx*sind + bz*cosd;
    uvw->ww[i] = -byx*cosd + bz*sind;
  }

/* Remember that the table has been updated. */

  uv->flags &= ~UVF_UPDATED_UVW;
}
/************************************************************************/
private void uvread_defline(tno)
int tno;
/*
  Initialise everything, ready to start reading. In particular, this
  determines what variables are needed, makes sure they are there, and
  makes sure they are being tracked.

  Inputs:
    tno		The handle of the file of interest.
------------------------------------------------------------------------*/
{
  UV *uv;

  uv = uvs[tno];

/* If no line has been specified, return the default type -- i.e.
   all the channels. */

  uv->corr = uv_locvar(tno,"corr");
  uv->wcorr = uv_locvar(tno,"wcorr");

  if(uv->data_line.linetype == LINE_NONE){
    if(uv->corr != NULL)
      uv->data_line.linetype = LINE_CHANNEL;
    else if(uv->wcorr != NULL)
      uv->data_line.linetype = LINE_WIDE;
    else
      BUG('f',"UV file contains neither corr nor wcorr, in UVREAD(defline)");
    uv->data_line.start = 0;
    uv->data_line.width = 1;
    uv->data_line.step  = 1;
    uv->data_line.n = 0;
  }
}
/************************************************************************/
private void uvread_init(tno)
int tno;
/*
  Initialise everything, ready to start reading. In particular, this
  determines what variables are needed, makes sure they are there, and
  makes sure they are being tracked.

  Inputs:
    tno		The handle of the file of interest.
------------------------------------------------------------------------*/
{
  UV *uv;
  SELECT *sel;
  int n_win,n_or;

  uv = uvs[tno];
  uv->flags |= UVF_INIT;

/* Open the flagging file, if it is needed. */

  if(uv->data_line.linetype == LINE_CHANNEL  ||
     uv->data_line.linetype == LINE_VELOCITY ||
     uv->data_line.linetype == LINE_FELOCITY ||
     uv->ref_line.linetype  == LINE_CHANNEL  ||
     uv->ref_line.linetype  == LINE_VELOCITY ||
     uv->ref_line.linetype  == LINE_FELOCITY ){
    if(uv->corr_flags.handle == NULL && uv->corr_flags.exists){
      uv->corr_flags.handle = mkopen_c(uv->tno,"flags","old");
      uv->corr_flags.exists = uv->corr_flags.handle != NULL;
      if(!uv->corr_flags.exists)
        BUG('w',"No flags found for corr -- assuming data are good");
    }
  }
  if(uv->data_line.linetype == LINE_WIDE ||
     uv->ref_line.linetype  == LINE_WIDE ){
    if(uv->wcorr_flags.handle == NULL && uv->wcorr_flags.exists){
      uv->wcorr_flags.handle = mkopen_c(uv->tno,"wflags","old");
      uv->wcorr_flags.exists =  uv->wcorr_flags.handle != NULL;
      if(!uv->wcorr_flags.exists)
        BUG('w',"No flags found for wcorr -- assuming data are good");
    }
  }

/* Make sure we have the info to get the preamble. */

  uv->coord = uv_checkvar(tno,"coord",H_DBLE);
  if( VARLEN(uv->coord) < ( uv->flags & UVF_DOW ? 3 : 2 ) ){
    if(uv_locvar(tno,"obsra") != NULL && uv_locvar(tno,"obsdec") != NULL &&
       uv_locvar(tno,"lst")   != NULL && uv_locvar(tno,"antpos") != NULL){
      uv->flags |= UVF_REDO_UVW;
      uv->need_uvw = TRUE;
    } else {
      BUG('w',"Unable to compute w coordinate -- setting this to zero");
    }
  }
  uv->time  = uv_checkvar(tno,"time",H_DBLE);
  uv->bl    = uv_checkvar(tno,"baseline",H_REAL);

/* Set up the default preamble if one has not already been set. */

  if(uv->presize == 0){
    uv->presize = 3;
    uv->prevar[0] = uv->coord;
    uv->prevar[1] = uv->time;
    uv->prevar[2] = uv->bl;
  }

/* Get info to decode correlation data. */

  if( uv->data_line.linetype == LINE_CHANNEL  ||
      uv->data_line.linetype == LINE_VELOCITY ||
      uv->data_line.linetype == LINE_FELOCITY ||
      uv->ref_line.linetype  == LINE_CHANNEL  ||
      uv->ref_line.linetype  == LINE_VELOCITY ||
      uv->ref_line.linetype  == LINE_FELOCITY ){
    if(uv->corr == NULL)
      BUG('f',"Corr data missing, when channel linetype requested");
    if(uv->corr->type == H_INT2){
      uv->tscale = uv_checkvar(tno,"tscale",H_REAL);
    } else if(uv->corr->type != H_REAL){
      BUG('f',"Bad data type for variable corr, in UVREAD.");
    }
  }

/* Get variables needed for selection. */

  if(uv->need_point) uv->axisrms = uv_checkvar(tno,"axisrms",H_REAL);
  if(uv->need_pol)   uv->need_pol= uv_locvar(tno,"pol") != NULL;
  if(uv->need_pol)   uv->pol     = uv_checkvar(tno,"pol",H_INT);
  if(uv->need_on)    uv->on      = uv_checkvar(tno,"on",H_INT);
  if(uv->need_src)   uv->source  = uv_checkvar(tno,"source",H_BYTE);
  if(uv->need_bin)   uv->bin	 = uv_checkvar(tno,"bin",H_INT);
  if(uv->need_uvw){
    uv->obsra = uv_checkvar(tno,"obsra",H_DBLE);
    uv->obsdec = uv_checkvar(tno,"obsdec",H_DBLE);
    uv->lst = uv_checkvar(tno,"lst",H_DBLE);
    uv->antpos = uv_checkvar(tno,"antpos",H_DBLE);

    uv->obsra->flags |= UVF_UPDATED_UVW;
    uv->obsdec->flags |= UVF_UPDATED_UVW;
    uv->lst->flags |= UVF_UPDATED_UVW;
    uv->antpos->flags |= UVF_UPDATED_UVW;

    uv->flags |= UVF_UPDATED_UVW;

    if( ( uv->antdiam = uv_locvar(tno,"antdiam") ) != NULL)
      uv->antdiam = uv_checkvar(tno,"antdiam",H_REAL);
  }

/* Get extra info needed for decoding frequencies and velocities of "corr"
   data. */

  if( uv->data_line.linetype == LINE_VELOCITY ||
      uv->ref_line.linetype  == LINE_VELOCITY ||
      uv->data_line.linetype == LINE_FELOCITY ||
      uv->ref_line.linetype  == LINE_FELOCITY){
    uv->nschan = uv_checkvar(tno,"nschan",H_INT);
    uv->sfreq  = uv_checkvar(tno,"sfreq",H_DBLE);
    uv->sdf    = uv_checkvar(tno,"sdf",H_DBLE);
    uv->restfreq = uv_checkvar(tno,"restfreq",H_DBLE);
    uv->veldop = uv_checkvar(tno,"veldop",H_REAL);
    uv->vsource = uv_checkvar(tno,"vsource",H_REAL);
  }

/* Get info for decoding wide band stuff. */

  if( uv->data_line.linetype == LINE_WIDE ||
      uv->ref_line.linetype  == LINE_WIDE ){
    if(uv->wcorr == NULL)
      BUG('f',"Wcorr missing, when wide linetype was requested");
  }

/* Variables to determine the mapping from windows to channels. */

  if(uv->need_win)
    uv->nschan = uv_checkvar(tno,"nschan",H_INT);

/* Variables needed to determine the sky frequency. */

  if(uv->need_skyfreq){
    if(uv->data_line.linetype == LINE_WIDE){
      uv->wfreq = uv_checkvar(tno,"wfreq",H_REAL);
      uv->wfreq->flags |= UVF_UPDATED_SKYFREQ;

    } else if(uv->data_line.linetype == LINE_CHANNEL){
      uv->nschan = uv_checkvar(tno,"nschan",H_INT);
      uv->sfreq  = uv_checkvar(tno,"sfreq",H_DBLE);
      uv->sdf    = uv_checkvar(tno,"sdf",H_DBLE);
      uv->nschan->flags |= UVF_UPDATED_SKYFREQ;
      uv->sfreq->flags  |= UVF_UPDATED_SKYFREQ;
      uv->sdf->flags    |= UVF_UPDATED_SKYFREQ;

    } else if(uv->data_line.linetype == LINE_VELOCITY ||
	      uv->data_line.linetype == LINE_FELOCITY ){
      uv->veldop->flags    |= UVF_UPDATED_SKYFREQ;
      uv->restfreq->flags  |= UVF_UPDATED_SKYFREQ;
      uv->vsource->flags   |= UVF_UPDATED_SKYFREQ;
    }
    uv->flags |= UVF_UPDATED_SKYFREQ;
  }

/* RA,Dec, and delta ra and dec, possibly needed by the selection routines.
   We can do without dra and ddec (we assume they are zero if they are missing),
   but we cannot do without ra and dec if they are needed. */

  if(uv->need_ra) {
    uv->ra = uv_checkvar(tno,"ra",0);
    if(uv->ra->type != H_REAL && uv->ra->type != H_DBLE)
      BUG('f',"Variable ra has the wrong type, in UVREAD(ini)");
    uv->need_dra = uv_locvar(tno,"dra") != NULL;
    if(uv->need_dra) uv->need_dec = TRUE;
  }

  if(uv->need_dec) {
    uv->dec = uv_checkvar(tno,"dec",0);
    if(uv->dec->type != H_REAL && uv->dec->type != H_DBLE)
      BUG('f',"Variable dec has the wrong type, in UVREAD(ini)");
    uv->need_ddec = TRUE;
  }

  if(uv->need_dra) uv->need_dra = uv_locvar(tno,"dra") != NULL;
  if(uv->need_dra) uv->dra = uv_checkvar(tno,"dra",H_REAL);

  if(uv->need_ddec) uv->need_ddec = uv_locvar(tno,"ddec") != NULL;
  if(uv->need_ddec) uv->ddec = uv_checkvar(tno,"ddec",H_REAL);

/* Get info for performing planet corrections. If the data are missing, do
   not perform planet corrections. */

  if(uv->need_planet && uv_locvar(tno,"plmaj") != NULL){
    uv->plmaj = uv_checkvar(tno,"plmaj",H_REAL);
    uv->plmaj->flags |= UVF_UPDATED_PLANET;
    uv->plmin = uv_checkvar(tno,"plmin",H_REAL);
    uv->plmin->flags |= UVF_UPDATED_PLANET;
    uv->plangle = uv_checkvar(tno,"plangle",H_REAL);
    uv->plangle->flags |= UVF_UPDATED_PLANET;
    uv->flags |= UVF_UPDATED_PLANET;
  } else uv->need_planet = FALSE;

/* If line=channel and select=window, make sure our restrictions
   are met. */

  if(uv->data_line.linetype == LINE_CHANNEL && uv->apply_win){
    n_win = n_or = 0;
    for(sel = uv->select; sel != NULL; sel = sel->fwd){
      if(!sel->and) n_or++;
      if(sel->win.select){
	n_win++;
        if(sel->win.last - sel->win.first >=  sel->win.n)
	  BUG('f',"Unsupported window selection clause, in UVREAD(init)");
      }
    }
    if((n_or > 0 && n_win > 0) || (n_win > 1) )
      BUG('f',"Unsupported window selection clause, in UVREAD(init)");
  }

/* Reset the variance calibration, if needed. NOTE: THIS DOES NOT
   RELEASE THE variable handle -- which will not be released before
   the file it descroyed. This result in a mild memory leak of sorts. */

  if(uv->sigma2.table != NULL){
    free((char *)(uv->sigma2.table));
    uv->sigma2.table = NULL;
  }
    
/* Determine the max visibility that the user is interested in. */

  if(uv->select != NULL) uv->maxvis = uvread_maxvis(uv->select);

}
/************************************************************************/
private int uvread_maxvis(sel)
SELECT *sel;
/*
  Determine the maximum visibility number that the caller wants. If this
  cannot be determined, return 0.
------------------------------------------------------------------------*/
{
  OPERS *op;
  int temp,maxvis,ilo,ihi,n;

  maxvis = 0;
  while(sel != NULL){
    temp = 0;
    for(op = sel->opers,n = 0; n < sel->noper; n++, op++){
      if(op->type == SEL_VIS){
	ihi = op->hival + 0.5; ilo = op->loval + 0.5;
        if(op->discard && temp == 0) return(0);
        else if(op->discard && ihi >= temp) temp = min(temp, ilo);
        else if(!op->discard)		    temp = max(temp, ihi);
      }
    }
    if(temp <= 0) return(0);
    maxvis = max(maxvis, temp);
    sel = sel->fwd;
  }
  return(maxvis);
}
/************************************************************************/
private VARIABLE *uv_checkvar(tno,varname,type)
int tno,type;
char *varname;
/*
  Make sure a particular variable is present, and make sure
  we track it. Return the pointer to this variable.

  Input:
    tno		Handle of the uv data file.
    varname	The name of the variable we are interested in.
    type	The data type that the variable must be.
------------------------------------------------------------------------*/
{
  VARIABLE *v;

  v = uv_locvar(tno,varname);
  if(v == NULL) ERROR('f',(message,
	"Variable %s is missing, in UVREAD",varname));
  else if(type != 0 && type != v->type)ERROR('f',(message,
	"Variable %s has the wrong data type, in UVREAD",varname));
  else if(v->buf == NULL || v->length <= 0)ERROR('f',(message,
	"Variable %s was not initialised before it was required, in UVREAD",varname));
  return(v);
}
/************************************************************************/
private int uvread_line(uv,line,data,nsize,flags,actual)
UV *uv;
LINE_INFO *line,*actual;
float *data;
int *flags,nsize;
/*
  Determine the desired line.

  Input:
    uv		The uv structure.
    line	Info about the line we are interested in.
    nsize	Size of the "data" array (in complex elements).
  Output:
    data	The calculated line.
    flags	Flag info.
    actual	The actual line used.
    uvread_line	The number of values returned.
------------------------------------------------------------------------*/
{
  int i,j,n,nspect;
  VARIABLE *v;
  WINDOW *win;
  int *di;
  int rei,imi,nc,start,width,step,*flagin,nchan,*nschan;
  float scale,ref,imf,*df,*d;
  FLAGS *flag_info;

/* Determine the relevant variable and flagging info, and get the flags. */

  if(line->linetype == LINE_WIDE){
    v = uv->wcorr;
    flag_info = &(uv->wcorr_flags);
  } else {
    v = uv->corr;
    flag_info = &(uv->corr_flags);
  }
  nchan = NUMCHAN(v);
  if(! flag_info->init ) uvread_flags(uv,v,flag_info,nchan);

/* Handle velocity linetype. */

  if(line->linetype == LINE_VELOCITY || line->linetype == LINE_FELOCITY){
    uvread_velocity(uv,line,data,flags,nsize,actual);
    return(line->n);
  }

/* Determine the parameters which describe the line. */

  start = line->start;
  if(line->linetype == LINE_CHANNEL && uv->win->select && uv->apply_win){
    win = uv->win;
    nspect = VARLEN(uv->nschan);
    if(win->last >= nspect)
      BUG('f',"Invalid window selection, in UVREAD(channel)");
    nschan = (int *)uv->nschan->buf;
    nchan = 0;
    for(i=0; i < win->first; i++) nchan += *nschan++;
    start += nchan;
    for(i=0; i < win->n; i++)     nchan += *nschan++;
  }
  width = line->width;
  step  = line->step;
  n = line->n;
  if(n <= 0) n = (nchan - start) / step;
  if(n <= 0 || start < 0 || start + step * (n-1) + width > nchan)
    BUG('f',"Illegal channel range specified, in UVREAD");
  if(n > nsize)
    BUG('f',"Callers buffer too small for channel data, in UVREAD");

/* Return the actual line used. */

  actual->linetype = line->linetype;
  actual->start    = start;
  actual->width    = width;
  actual->step     = step;
  actual->n        = n;

/* Miscellaneous initialisation. */  

  step -= width;
  scale = uv->plscale;
  flagin = flag_info->flags + start;
  d = data;

/* Handle the common case of just a straight copy of the correlation data. */

  if(width == 1 && ( step == 0 || n == 1)){
    if(v->type == H_INT2){
      scale *= *(float *)uv->tscale->buf;
      di   = (int *)v->buf + 2*start;
      for(i=0; i < 2*n; i++) *d++ = scale * *di++;
    } else {
      df   = (float *)v->buf + 2*start;
      if(scale != 1)for(i=0; i < 2*n; i++) *d++ = scale * *df++;
      else	    memcpy((char *)d,(char *)df,2*sizeof(float)*n);
    }
    memcpy((char *)flags,(char *)flagin,sizeof(int)*n);

/* Handle the case of averaged, scaled integers. */

  } else if(v->type == H_INT2){
    di = (int *)(v->buf) + 2*start;
    scale *= *(float *)uv->tscale->buf;
    for(i=0; i<n; i++){
      rei = 0; imi = 0; nc  = 0;
      for(j=0; j<width; j++){
        if(*flagin++ == FORT_TRUE){ rei += *di++; imi += *di++; nc++; }
        else di += 2;
      }
      if(nc > 0){
        *d++ = rei*scale/nc; *d++ = imi*scale/nc; *flags++ = FORT_TRUE;
      } else {
        *d++ = 0; *d++ = 0; *flags++ = FORT_FALSE;
      }
      di += 2*step; flagin += step;
    }

/* Handle the case of averaged, reals. */

  } else {
    df = (float *)(v->buf) + 2*start;
    for(i=0; i<n; i++){
      ref = 0; imf = 0; nc  = 0;
      for(j=0; j<width; j++){
        if(*flagin++ == FORT_TRUE){ ref += *df++; imf += *df++; nc++; }
        else df += 2;
      }
      if(nc > 0){
        *d++ = scale*ref/nc; *d++ = scale*imf/nc; *flags++ = FORT_TRUE;
      } else {
        *d++ = 0; *d++ = 0; *flags++ = FORT_FALSE;
      }
      df += 2*step; flagin += step;
    }
  }
  return(n);
}
/************************************************************************/
private void uvread_velocity(uv,line,data,flags,nsize,actual)
UV *uv;
LINE_INFO *line,*actual;
float *data;
int *flags,nsize;
/*
  Calculate the velocity line type.

  Inputs:
    uv		Pointer to the uv data structure.
    nsize	Number of channels to return.
    line	Pointer to the structure defining the line type of interest.
  Outputs:
    data	The velocity line type.
    flags	Flags indicating whether the data is good or not.
------------------------------------------------------------------------*/
{
  float idv,idv2,odv2,dv2,scale,wt,v,vobs,temp;
  double *sfreq,*sdf,*restfreq;
  int nspect,first,last,fout,lout,i,j,n;
  int *nschan,*flagin,*flagin1,*flagout,*wins,doint2;
  float *wts,*dataout;
  int *di,*di1;
  float *df,*df1;

/* Set the default line if needed. */

  if(line->n == 0 || line->fstep == 0 || line->linetype == LINE_FELOCITY)
      uvread_defvelline(uv,line,uv->win);

/* A few simple checks. */

  if(line->n <= 0)
    BUG('f',"Bad number of channels, in UVREAD(velocity)");
  if(nsize < line->n)
    BUG('f',"Callers buffer too small for velocity data, in UVREAD(velocity)");
  if(uv->corr->type != H_INT2 && uv->corr->type != H_REAL)
    BUG('f',"Bad data type of corr data, in UVREAD(velocity).");
  doint2 = uv->corr->type == H_INT2;
  if(line->wts == NULL) line->wts = (float *)Malloc(sizeof(float)*nsize);

/* Return the actual line used. */

  actual->linetype = line->linetype;
  actual->n = line->n;
  actual->fstart = line->fstart;
  actual->fwidth = line->fwidth;
  actual->fstep  = line->fstep;

/* Set the weights and data arrays to zero. */

  nsize = line->n;
  wins = uv->win->wins;
  wts = line->wts;
  dataout  = data;
  for(i=0; i<nsize; i++){
   *wts++ = 0.0;
   *dataout++ = 0.0; *dataout++ = 0.0;
  }

/* Initialise lots of rubbish. */

  nspect = VARLEN(uv->nschan);
  if(nspect > MAXWIN) BUG('f',"Too many windows, in UVREAD(velocity)");

  temp = line->fstep;
  if(temp < 0) temp = -temp;
  odv2 = 0.5 * line->fwidth/temp;
  sfreq =    (double *)uv->sfreq->buf;
  sdf =      (double *)uv->sdf->buf;
  restfreq = (double *)uv->restfreq->buf;
  vobs =  *(float *)uv->veldop->buf - *(float *)uv->vsource->buf;
  nschan = (int *)uv->nschan->buf;
  scale = uv->plscale;
  if(doint2)scale *= *(float *)uv->tscale->buf;

  wts = line->wts;
  if(doint2)di1 = (int *)uv->corr->buf;
  else	    df1 = (float *)uv->corr->buf;
  dataout = data;
  flagin1 = uv->corr_flags.flags;

/* Now compute the velocity channels. The first loop moves over the windows,
   determining which channels, in this window, contribute. The second loop
   moves over these channels. The third loop moves over the output velocity
   channels, accumulating the contribution of a particular input channel. */

  for(n=0; n < nspect; n++){
    if(*wins++){
      v = (CKMS * (*restfreq - *sfreq) / *restfreq - vobs - line->fstart )
		/ line->fstep;
      idv = -CKMS * *sdf / (*restfreq * line->fstep);
      idv2 = 0.5 * idv;
      if(idv2 < 0) idv2 = - idv2;
      dv2 = idv2 + odv2;
      if(idv > 0){
        fout = ceil((-dv2-v)/idv);
        lout = floor((nsize - 1 + dv2 - v)/idv);
      } else if(idv < 0){
        fout = ceil((nsize - 1 + dv2 - v)/idv);
        lout = floor((-dv2-v)/idv);
      } else BUG('f',"File velocity increment is zero, in UVREAD(velocity).");
      if(fout < 0)	   fout = 0;
      if(lout > *nschan-1) lout = *nschan - 1;
      v += fout * idv;
      if(doint2)di = di1 + fout + fout;
      else	df = df1 + fout + fout;
      flagin = flagin1 + fout;
      for(i=fout; i <= lout; i++){
        if(*flagin == FORT_TRUE){
          first = max(0,      ceil (v-dv2));
          last  = min(nsize-1,floor(v+dv2));
	  if(doint2){
            for(j=first; j<=last; j++){
	      wt = ( min(v + idv2, j + odv2) - max(v - idv2, j - odv2) ) / idv2;
	      *(dataout + j+j)	   += wt * *(di);
	      *(dataout + j+j + 1) += wt * *(di+1);
	      *(wts + j)	   += wt;
	    }
	  } else {
            for(j=first; j<=last; j++){
	      wt = ( min(v + idv2, j + odv2) - max(v - idv2, j - odv2) ) / idv2;
	      *(dataout + j+j)	   += wt * *(df);
	      *(dataout + j+j + 1) += wt * *(df+1);
	      *(wts + j)	   += wt;
	    }
          }
        }
        v += idv;
        flagin++;
	if(doint2) di += 2;
	else	   df += 2;
      }
    }
    if(doint2) di1 += 2 * *nschan;
    else       df1 += 2 * *nschan;
    flagin1 += *nschan;
    nschan++; sfreq++; sdf++; restfreq++;
  }

/* Normalise and return. */

  flagout = flags;
  for(i=0; i<nsize; i++){
    if(*wts > 0.0){
      *dataout++ *= scale / *wts;
      *dataout++ *= scale / *wts++;
      *flagout++ = FORT_TRUE;
    } else {
      dataout += 2;
      wts ++;
      *flagout++ = FORT_FALSE;
    }
  }
}
/************************************************************************/
private void uvread_defvelline(uv,line,win)
UV *uv;
WINDOW *win;
LINE_INFO *line;
/*
  Determine a good, default, velocity line.

  Input:
    win		The window to use.
  Input/Output:
    line	n,fstart,fwidth,fstep are set if needed.
------------------------------------------------------------------------*/
{
  double f0,df,rfreq,fac;
  int n;
  float vobs;

/* Get the frequency, etc, description of the first window. */

  if(win->first != 0){
    if(win->first >= VARLEN(uv->nschan))
      BUG('f',"Invalid window selection, in UVREAD(skyfreq)");
  }

  vobs =  *(float *)uv->veldop->buf - *(float *)uv->vsource->buf;
  f0 = *((double *)uv->sfreq->buf + win->first);
  df = *((double *)uv->sdf->buf + win->first);
  n  = *((int *)uv->nschan->buf + win->first);
  rfreq = *((double *)uv->restfreq->buf + win->first);
  if(rfreq <= 0)BUG('f',"Invalid rest frequency when setting default linetype");

/* Set the defaults. */

  if(line->n == 0 || line->fwidth == 0){
    line->linetype = LINE_VELOCITY;
    line->fwidth = -CKMS * df / rfreq;
    line->fstep = MYABS(line->fwidth);
    if(line->n == 0) line->n = n;
    n = (n - line->n) / 2;
    line->fstart = CKMS * ( 1 - (f0+n*df)/rfreq ) - vobs;
  }

/* Translate a felocity linetype into a velocity one, if needed. */

  if(line->linetype == LINE_FELOCITY){
    line->linetype = LINE_VELOCITY;
    fac = CKMS / (CKMS + line->fstart + vobs );
    line->fstep  *= fac * fac;
    line->fwidth *= fac * fac;
    line->fstart = fac * (line->fstart + vobs) - vobs;
  }
}
/************************************************************************/
private void uvread_flags(uv,v,flag_info,nchan)
UV *uv;
VARIABLE *v;
FLAGS *flag_info;
int nchan;
/*
   Read in flagging information, and apply the amplitude flagging if needed.

------------------------------------------------------------------------*/
{
  int *di;
  float *df,amp2,amplo2,amphi2,tscale,ii,rr;
  int discard,i;
  int *flags;

  flag_info->init = TRUE;
  nchan = NUMCHAN(v);

/* Allocate space and read the flags. */

  if(flag_info->nflags < nchan){
    flag_info->nflags = nchan;
    flag_info->flags = (int *)Realloc((char *)flag_info->flags,
		sizeof(int) * flag_info->nflags);
  }
  flags = flag_info->flags;
  if(flag_info->exists)
      mkread_c(flag_info->handle,MK_FLAGS,flags,
	flag_info->offset-nchan,nchan,nchan);
  else for(i=0; i < nchan; i++) *flags++ = FORT_TRUE;

/* Return if there is no amplitude flagging to do. */

  if( !uv->amp->select || !uv->apply_amp ) return;

/* Flag the appropriate channels. */

  flags = flag_info->flags;
  amplo2 = uv->amp->loval / uv->plscale;
  amplo2 *= amplo2;
  amphi2 = uv->amp->hival / uv->plscale;
  amphi2 *= amphi2;
  discard = uv->amp->discard;

/* Case of real data. */

  if(v->type == H_REAL || v->type == H_CMPLX){
    df = ((float *)(v->buf));
    for(i=0; i < nchan; i++){
      amp2 = *df * *df + *(df+1) * *(df+1);
      if(amplo2 <= amp2 && amp2 <= amphi2)
	*flags = ((*flags == FORT_TRUE && !discard) ? FORT_TRUE : FORT_FALSE);
      else
	*flags = ((*flags == FORT_TRUE &&  discard) ? FORT_TRUE : FORT_FALSE);
      df += 2; flags++;
    }

/* Case of integer*2 data. */

  } else if(v->type == H_INT2){
    di = ((int *)(v->buf));
    tscale = *((float *)(uv->tscale->buf));
    for(i=0; i < nchan; i++){
      rr = tscale * *di;
      ii = tscale * *(di+1);
      amp2 = rr * rr + ii * ii;
      if(amplo2 <= amp2 && amp2 <= amphi2)
	*flags = ((*flags == FORT_TRUE && !discard) ? FORT_TRUE : FORT_FALSE);
      else
	*flags = ((*flags == FORT_TRUE &&  discard) ? FORT_TRUE : FORT_FALSE);
      di += 2; flags++;
    }
  }
}
/************************************************************************/
void uvflgwr_c(tno,flags)
int tno,*flags;
/**uvflgwr -- Write uv flags after a read.				*/
/*&rjs                                                                  */
/*:uv-i/o								*/
/*+ FORTRAN call sequence:

	subroutine uvflgwr(tno,flags)
	integer tno
	logical flags(*)

  This causes the flags associated with correlation data to be rewritten.
  It is typically used by a flagging program to overwrite old flagging
  information. It will typically be called soon after uvread (which is
  used to get the old flags, and position the file), thus overwriting
  the old flags.

  Input:
    tno		The handle of the input uv file.
    flags	Logical array of "nread" elements ("nread" as returned
		by the last call to uvread).				*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  int nchan,width,step,offset,n,i;
  UV *uv;
  VARIABLE *v;
  FLAGS *flags_info;

  uv = uvs[tno];

  if(uv->actual_line.linetype == LINE_CHANNEL){
    v = uv->corr;
    flags_info = &(uv->corr_flags);
  } else {
    v = uv->wcorr;
    flags_info = &(uv->wcorr_flags);
  }

  width = uv->actual_line.width;
  step  = uv->actual_line.step;
  if(uv->actual_line.linetype == LINE_VELOCITY ||
     flags_info->handle == NULL || width != 1)
    BUG('f',"Illegal request when trying to write to flagging file, in UVFLGWR");

  nchan = NUMCHAN(v);
  offset = flags_info->offset - nchan + uv->actual_line.start;
  n = min(uv->actual_line.n,nchan);
  if(step == 1){
    mkwrite_c(flags_info->handle,MK_FLAGS,flags,offset,n,n);
  } else {
    for(i = 0; i < n; i++){
      mkwrite_c(flags_info->handle,MK_FLAGS,flags,offset,1,1);
      offset += step;
      flags++;
    }
  }
}
/************************************************************************/
void uvwflgwr_c(tno,flags)
int tno,*flags;
/**uvwflgwr -- Write uv flags after a read.				*/
/*&rjs                                                                  */
/*:uv-i/o								*/
/*+ FORTRAN call sequence:

	subroutine uvwflgwr(tno,flags)
	integer tno
	logical flags(*)

  This rewrites the flags associated with the last call to uvwread.
  It will typically be called soon after uvwread, thus overwriting
  the old flags.

  Input:
    tno		The handle of the input uv file.
    flags	Logical array of "nread" elements ("nread" as returned
		by the last call to uvwread).				*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  int nchan,offset;
  UV *uv;
  VARIABLE *v;
  FLAGS *flags_info;

  uv = uvs[tno];

  v = uv->wcorr;
  if(v == NULL)
    BUG('f',"The wcorr variable has not been initialised, in UVWFLGWR\n");
  flags_info = &(uv->wcorr_flags);
  if(flags_info->handle == NULL)
    BUG('f',"No flagging file exists, in UVWFLGWR\n");

  nchan = NUMCHAN(v);
  offset = flags_info->offset - nchan;
  mkwrite_c(flags_info->handle,MK_FLAGS,flags,offset,nchan,nchan);
}
/************************************************************************/
void uvinfo_c(tno,object,data)
int tno;
char *object;
double *data;
/**uvinfo -- Get information about the last data read with uvread.	*/
/*&rjs                                                                  */
/*:uv-i/o								*/
/*+ FORTRAN call sequence:

	subroutine uvinfo(tno,object,data)
	integer tno
	character object*(*)
	double precision data(*)

  This returns extra information about the data read in the last call
  to uvread.

  Input:
    tno		The handle of the uv file.
    object	Indicates what information is required. Currently
		this can be
		'velocity' returns "nread" numbers, giving the velocity
			   (km/s) of each channel.
		'restfreq' returns "nread" numbers, giving the rest
			   frequency (GHz) of each channel.
		'bandwidth' returns "nread" numbers, giving the bandwidth
			   (GHz) of each channel.
		'visno'	   returns 1 number, which is the number of
			   visibilities read from this file.
		'frequency' returns "nread" numbers, giving the rest-frame
			   frequency (GHz) of each channel.
		'sfreq'	   returns "nread" numbers, giving the sky frequency
			   (GHz) of each channel.
		'amprange' returns 3 numbers. The first gives the amplitude
			   selection for this record, the next two give
			   the selection range. Possible values of data(1) are
			   -1 : Data outside the range [data(2),data(3)]
			        was rejected.
			    0 : No amplitude selection.
			   +1 : Data inside the range [data(2),data(3)]
				was rejected.
		'line'	   returns 6 numbers, giving the linetype. Possible
			   values of data(1) are 1, 2 or 3, corresponding to
			   'channel', 'wide' and 'velocity'.
			   data(2) thru data(5) are n,start,width,step.
			   data(6) is the first window used.
		'variance' returns the variance (based on system temp)
			   of the first channel. If this cannot be determined
			   it returns 0.
  Output:
    data	The actual information returned.			*/
/*--									*/
/*----------------------------------------------------------------------*/
{
#define VELO  1
#define FELO  2
#define RFREQ 3
#define BW    4
#define FREQ  5
#define SFREQ 6

  UV *uv;
  uv = uvs[tno];

/* Return the visibility number. */

  if(!strcmp(object,"visno")){
    *data = uv->callno;

/* Return the variance of the data in the first channel. */

  } else if(!strcmp(object,"variance")){
    uvinfo_variance(uv,data);
    
/* Return information about the amplitude selection. */

  } else if(!strcmp(object,"amprange")){
    if( ! uv->amp->select ) *data = 0;
    else{
      *data     = (uv->amp->discard ? -1 : 1);
      *(data+1) =  uv->amp->loval;
      *(data+2) =  uv->amp->hival;
    }

/* Return linetype information. */

  } else if(!strcmp(object,"line")){
    *data    = uv->actual_line.linetype;
    *(data+1) = uv->actual_line.n;
    if(uv->actual_line.linetype == LINE_VELOCITY){
      *(data+2) = uv->actual_line.fstart;
      *(data+3) = uv->actual_line.fwidth;
      *(data+4) = uv->actual_line.fstep;
      *(data+5) = uv->win->first + 1;
    } else {
      *(data+2) = uv->actual_line.start + 1;
      *(data+3) = uv->actual_line.width;
      *(data+4) = uv->actual_line.step;
      *(data+5) = 0; 
    }

/* Various bits and pieces of channel information. */

  } else if(!strcmp(object,"velocity"))	uvinfo_chan(uv,data,VELO);
  else if(!strcmp(object,"felocity"))   uvinfo_chan(uv,data,FELO);
  else if(!strcmp(object,"restfreq"))	uvinfo_chan(uv,data,RFREQ);
  else if(!strcmp(object,"bandwidth"))  uvinfo_chan(uv,data,BW);
  else if(!strcmp(object,"frequency"))  uvinfo_chan(uv,data,FREQ);
  else if(!strcmp(object,"sfreq"))	uvinfo_chan(uv,data,SFREQ);
  else
    ERROR('f',(message,"Unrecognised object %s, in UVINFO",object));
}
/************************************************************************/
private void uvinfo_variance(uv,data)
UV *uv;
double *data;
/*
  Determine the variance of the first channel of the last data read with
  uvread.

  For raw polarisation parameters,

  variance = JyperK**2 * T1*T2/(2*Bandwidth*IntTime)

  where

  JyperK = 2*k/eta*A

  where A is the antenna area, eta is an efficiency (both surface efficiency
  and correlator efficiency), and k is Boltzmans constant.

  For Stokes parameters, the variance returned is half the above variance,
  as its assumed that two things have been summed to get the Stokes
  parameter.
------------------------------------------------------------------------*/
{
  double *restfreq,*tab;
  float bw,inttime,jyperk,*syst,*t1,*t2,factor;
  int i,j,bl,i1,i2,nants,offset,nsyst,*nschan,start;
  LINE_INFO *line;
  VARIABLE *tsys;

/* Miscellaneous. */

  line = &(uv->actual_line);
  *data = 0;

/* Have we initialised the table? If not, intialise as much as possible. */

  if(!uv->sigma2.missing && uv->sigma2.table == NULL){
    if( (uv->pol = uv_locvar(uv->tno,"pol") ) != NULL)
      (void)uv_checkvar(uv->tno,"pol",H_INT);
    uvvarini_c(uv->tno,&(uv->sigma2.vhan));
    uvvarset_c(uv->sigma2.vhan,"nants");
    uvvarset_c(uv->sigma2.vhan,"inttime");
    uvvarset_c(uv->sigma2.vhan,"jyperk");
    uv->sigma2.missing = (uv_locvar(uv->tno,"inttime") == NULL) |
			 (uv_locvar(uv->tno,"jyperk")  == NULL) |
			 (uv_locvar(uv->tno,"nants")   == NULL);
    if(line->linetype == LINE_CHANNEL){
      uvvarset_c(uv->sigma2.vhan,"systemp");
      uvvarset_c(uv->sigma2.vhan,"sdf");
      uvvarset_c(uv->sigma2.vhan,"nschan");
      uv->sigma2.missing |= (uv_locvar(uv->tno,"systemp") == NULL) |
			    (uv_locvar(uv->tno,"sdf")     == NULL) |
			    (uv_locvar(uv->tno,"nschan")  == NULL);
    } else if(line->linetype == LINE_VELOCITY){
      uvvarset_c(uv->sigma2.vhan,"systemp");
      uvvarset_c(uv->sigma2.vhan,"restfreq");
      uv->sigma2.missing |= (uv_locvar(uv->tno,"systemp") == NULL) |
			    (uv_locvar(uv->tno,"restfreq")== NULL);
    } else {
      uvvarset_c(uv->sigma2.vhan,"wsystemp");
      uvvarset_c(uv->sigma2.vhan,"wwidth");
      uv->sigma2.missing |= (uv_locvar(uv->tno,"wsystemp") == NULL) |
			    (uv_locvar(uv->tno,"wwidth")   == NULL);
    }
    if(!uv->sigma2.missing)
      uv->sigma2.table = (double *)Malloc(sizeof(double)*(MAXANT*(MAXANT+1))/2);
  }

/* Return if we do not have enough info to determine the variance. */

  if(uv->sigma2.missing) return;

/* Is the table of variances out of date? If so recompute it. */

  if(uvvarupd_c(uv->sigma2.vhan) == FORT_TRUE){
    nants = *(int *)(uv_checkvar(uv->tno,"nants",H_INT)->buf);
    inttime = *(float *)(uv_checkvar(uv->tno,"inttime",H_REAL)->buf);
    jyperk  = *(float *)(uv_checkvar(uv->tno,"jyperk",H_REAL)->buf);
    if(line->linetype == LINE_CHANNEL){
      nschan = (int *)(uv_checkvar(uv->tno,"nschan",H_INT)->buf);
      start = line->start;
      offset = 0;
      while(start >= *nschan){
	start -= *nschan++;
	offset++;
      }
      bw = *((double *)(uv_checkvar(uv->tno,"sdf",H_DBLE)->buf) + offset)
	* line->width;
      tsys = uv_checkvar(uv->tno,"systemp",H_REAL);
    } else if(line->linetype == LINE_WIDE){
      offset = line->start;
      bw = *((float *)(uv_checkvar(uv->tno,"wwidth",H_REAL)->buf) + offset)
	* line->width;
      tsys = uv_checkvar(uv->tno,"wsystemp",H_REAL);
    } else if(line->linetype == LINE_VELOCITY){
      offset = uv->win->first;
      restfreq = (double *)(uv_checkvar(uv->tno,"restfreq",H_DBLE)->buf) +
		offset;
      bw = *restfreq * line->fwidth / CKMS;
      tsys = uv_checkvar(uv->tno,"systemp",H_REAL);
    }
    if(bw < 0) bw = - bw;

/* We have everything we ever wanted: jyperk,inttime,bw and Tsys. Compute
   variance. */

    uv->sigma2.nants = nants;
    nsyst = VARLEN(tsys);
    syst = (float *)(tsys->buf);
    factor = jyperk*jyperk/inttime/(2.0e9*bw) * uv->plscale;
    tab = uv->sigma2.table;
    if(nsyst < nants){
      factor *= *syst * *syst;
      for(i=0; i < (nants*(nants+1))/2; i++)*tab++ = factor;
    } else {
      if(nsyst >= nants*(offset+1)) syst +=  nants*offset;
      t2 = syst;
      for(j=0; j < nants; j++){
	t1 = syst;
        for(i=0; i <= j; i++){
	  *tab++ = factor * *t1++ * *t2;
	}
	t2++;
      }
    }
  }

/* All is up to date and OK. Return the result. */
    
  bl = *((float *)(uv->bl->buf)) + 0.5;
  i1 = max( bl / 256, bl % 256);
  i2 = min( bl / 256, bl % 256);
  if(i2 < 1 || i1 > uv->sigma2.nants)return;
  bl = (i1*(i1-1))/2+i2-1;
  *data = uv->sigma2.table[bl];

/* If its a Stokes parameter, multiply the variance by one half. */

  if(uv->pol != NULL && *((int*)(uv->pol->buf)) > 0) *data *= 0.5;
}
/************************************************************************/
private void uvinfo_chan(uv,data,mode)
UV *uv;
double *data;
int mode;
/*
------------------------------------------------------------------------*/
{
  LINE_INFO *line;
  int n,i,j,offset,step;
  double temp;
  float *wfreq,*wwide,vobs;
  int *nschan;
  double *sdf,*sfreq,*restfreq;

/* Get the velocity of the "channel" line type. */

  line = &(uv->actual_line);
  n = line->n;

  if(line->linetype == LINE_CHANNEL){
    vobs =   *(float *)(uv_checkvar(uv->tno,"veldop",H_REAL)->buf) -
	     *(float *)(uv_checkvar(uv->tno,"vsource",H_REAL)->buf);
    nschan = (int *)(uv_checkvar(uv->tno,"nschan",H_INT)->buf);
    sfreq = (double *)(uv_checkvar(uv->tno,"sfreq",H_DBLE)->buf);
    sdf = (double *)(uv_checkvar(uv->tno,"sdf",H_DBLE)->buf);
    restfreq = (double *)(uv_checkvar(uv->tno,"restfreq",H_DBLE)->buf);
    step = line->step - line->width;
    offset = line->start;
    for(j=0; j < n; j++){
      temp = 0;
      while(offset >= *nschan){
        offset -= *nschan++;
        sfreq++; sdf++; restfreq++;
      }
      for(i=0; i < line->width; i++){
	if(offset == *nschan){
	  offset = 0;
	  sfreq++; sdf++; nschan++;
	}
	if(mode == VELO)
	  temp += CKMS * ( 1 - ( *sfreq + offset * *sdf ) / *restfreq ) - vobs;
        else if(mode == FELO)
	  temp += CKMS * ( *restfreq / ( *sfreq + offset * *sdf ) - 1 ) - vobs;
	else if(mode == RFREQ) temp += *restfreq;
	else if(mode == BW)    temp += (*sdf > 0 ? *sdf : - *sdf);
	else if(mode == FREQ)
	  temp += *sfreq + offset * *sdf + vobs/CKMS * *restfreq;
	else if(mode == SFREQ)
	  temp += *sfreq + offset * *sdf;
	offset++;
      }
      if(mode != BW) *data++ = temp / line->width;
      else	     *data++ = temp;
      offset += step;
    }

/* Wide channel information. Getting the velocity of this does not make a
   great deal of sense. Assume the rest frequency is the same as the sky
   frequency of the first wide channel. */

  } else if(line->linetype == LINE_WIDE){
    if(mode == RFREQ || mode == VELO || mode == FELO){
      BUG('f',"Invalid object for wide linetype, in UVINFO\n");
    } else if(mode == FREQ || mode == SFREQ){
      step = line->step - line->width;
      wfreq = (float *)(uv_checkvar(uv->tno,"wfreq",H_REAL)->buf);
      wfreq += line->start;
      for(j=0; j < n; j++){
	temp = 0;
	for(i=0; i < line->width; i++) temp += *wfreq++;
	*data++ = temp/line->width;
	wfreq += step;
      }
    } else if(mode == BW){
      step = line->step - line->width;
      wwide = (float *)(uv_checkvar(uv->tno,"wwidth",H_REAL)->buf);
      wwide += line->start;
      for(j=0; j < n; j++){
	temp = 0;
	for(i=0; i < line->width; i++) temp += *wwide++;
	*data++ = temp;
	wwide += step;
      }
    }

/* Velocity channel information. This is pretty trivial. */

  } else if(line->linetype == LINE_VELOCITY){

    if(mode == VELO){
      for(i=0; i<n; i++) *data++ = line->fstart + i * line->fstep;
    } else if(mode == FELO){
      vobs =   *(float *)(uv_checkvar(uv->tno,"veldop",H_REAL)->buf) -
	       *(float *)(uv_checkvar(uv->tno,"vsource",H_REAL)->buf);
      for(i=0; i<n; i++){
	temp = line->fstart + i * line->fstep + vobs;
	*data++ = CKMS*temp / (CKMS-temp) - vobs;
      }
    } else if(mode == RFREQ){
      restfreq = (double *)(uv_checkvar(uv->tno,"restfreq",H_DBLE)->buf) +
		 uv->win->first;
      for(i=0; i<n; i++) *data++ = *restfreq;
    } else if(mode == FREQ || mode == SFREQ){
      restfreq = (double *)(uv_checkvar(uv->tno,"restfreq",H_DBLE)->buf) +
		 uv->win->first;
      for(i=0; i<n; i++)
        *data++ = *restfreq * (1 - (line->fstart + i *line->fstep)/CKMS);
    } else if(mode == BW){
      restfreq = (double *)(uv_checkvar(uv->tno,"restfreq",H_DBLE)->buf) +
		uv->win->first;
      temp = *restfreq * line->fwidth / CKMS;
      if(temp < 0) temp = - temp;
      for(i=0; i<n; i++) *data++ = temp;
    }
  }
}
