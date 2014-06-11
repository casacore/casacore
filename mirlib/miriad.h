/*
// Header that gives C++ code access to the MIRIAD IO routines
// Is now also used a global prototype header file for all C routines
//
// History:
//
// 21-oct-94 pjt Created as follows:
//               cproto uvio.c hio.c dio.c bug.c pack.c headio.c maskio.c > miriad.h
//                  and edited to please the C++ compiler and the eye:
//
// 22-oct-94 pjt added hio.h definitions - clumsy ....	pjt
//               (the hio routines are macros we don't want to allow that here)
//
//   fall-96 pjt minor tidying up for AIPS++ bimafiller release	- pjt
// 13-dec-96 rjs Regenerated, using
//		 cproto -I$MIRINC -p -fARGS hio.c headio.c uvio.c xyio.c xyzio.c bug.c
// 07-feb-97 rjs Added "Const" to definitions, and eliminate some rubbish, as
//		 suggested by Scott Gordon.
// 19-Mar-97 rjs Check for definition of various thingos before doing them.
// 15-jun-01 pjt Added key.c for BIMA version (ATNF uses keyc.c) as well as
//               the uvget* uvrd* macros from uvio.c
// 28-may-02 pjt LFS patches: make it for for > 2GB file using off_t/size_t (prep for MIR4)
// 17-jun-02 pjt added interface.c routines; now used as global prototype file
// 23-jun-02 pjt define MIR4 here if you want to enable the LSF and MIR4
// 30-aug-04 pjt removed deprecated ARGS() macro
//  1-dec-05 pjt added bugv_c
// 18-may-06 pjt/df  added mir.c prototypes for mir (the miriad->mir converter)
// 12-feb-09 dhem added bughandler_c prototype (new function in bug.c)
// 01-apr-09 rjs Add additional interface to scrRecSz
//  7-jan-10 pjt re-aligned ATNF and CARMA code
// 14-dec-11 pkgw Declare errmsg_c()
*/

#if !defined(MIR_MIRIAD_H)
#define MIR_MIRIAD_H


/*  comment this out if you only handle data < 2GB and need to be compatible with old MIRIAD */
/*  or simply define MIR3 through compile options                                            */
#if !defined(MIR3)
#define MIR4  
#endif

#include <sys/types.h>     /* provides off_t */
#include <unistd.h>
#include <stdarg.h>
#include "sysdep.h"        /* since it now contains the "pack.c" prototypes */

/* Define const and void if needed. */

#ifndef MIRIAD_TYPES_DEFINED

#define MIRIAD_TYPES_DEFINED 1
#ifdef __STDC__
#if (__STDC__ == 1)
typedef void Void;
#define Const const
#else
typedef char Void;
#define Const /* NULL */
#endif /* (__STDC__ == 1) */
#else
typedef char Void;
#define Const /* NULL */
#endif /* __STDC__ */

#if !defined(__cplusplus)
#  define private static
#endif /* __cplusplus */

/* Define the argument list if needed. */

#if !defined(ARGS)
#  if defined(__STDC__) || defined(__cplusplus)
#    define ARGS(s) s
#  else
#    define ARGS(s) ()
#  endif
#endif

#endif

#if defined(__cplusplus)
extern  "C" {
#endif

/* hio.h */

#if !defined(TRUE)
# define TRUE		1
#else
# if TRUE != 1
#  error "TRUE should be 1"
# endif
#endif

#if !defined(FALSE)
# define FALSE		0
#else
# if FALSE != 0
#  error "FALSE should be 0"
# endif
#endif

#define H_BYTE          1
#define H_INT           2
#define H_INT2          3
#define H_REAL          4
#define H_DBLE          5
#define H_TXT           6
#define H_CMPLX         7
#define H_INT8          8

/* hio.c */

void hopen_c(int *tno, Const char *name, Const char *status, int *iostat);
void hflush_c(int tno, int *iostat);
void habort_c(void);
void hrm_c(int tno);
void hclose_c(int tno);
void hdelete_c(int tno, Const char *keyword, int *iostat);
void haccess_c(int tno, int *ihandle, Const char *keyword, Const char *status, int *iostat);
void hmode_c(int tno, char *mode);
int  hexists_c(int tno, Const char *keyword);
void hdaccess_c(int ihandle, int *iostat);
off_t hsize_c(int ihandle);
void hio_c(int ihandle, int dowrite, int type, char *buf, off_t offset, size_t length, int *iostat);
void hseek_c(int ihandle, off_t offset);
off_t htell_c(int ihandle);
void hreada_c(int ihandle, char *line, size_t length, int *iostat);
void hwritea_c(int ihandle, Const char *line, size_t length, int *iostat);

/* Macros defined in hio.c */

#define hreadb_c(item,buf,offset,length,iostat) \
        hio_c(item,FALSE,H_BYTE,buf,offset,length,iostat)
#define hwriteb_c(item,buf,offset,length,iostat) \
        hio_c(item,TRUE,H_BYTE,buf,offset,length,iostat)
#define hreadi_c(item,buf,offset,length,iostat) \
        hio_c(item,FALSE,H_INT,(char *)(buf),offset,length,iostat)
#define hwritei_c(item,buf,offset,length,iostat) \
        hio_c(item,TRUE,H_INT,(char *)(buf),offset,length,iostat)
#define hreadj_c(item,buf,offset,length,iostat) \
        hio_c(item,FALSE,H_INT2,(char *)(buf),offset,length,iostat)
#define hwritej_c(item,buf,offset,length,iostat) \
        hio_c(item,TRUE,H_INT2,(char *)(buf),offset,length,iostat)
#define hreadl_c(item,buf,offset,length,iostat) \
        hio_c(item,FALSE,H_INT8,(char *)(buf),offset,length,iostat)
#define hwritel_c(item,buf,offset,length,iostat) \
        hio_c(item,TRUE,H_INT8,(char *)(buf),offset,length,iostat)
#define hreadr_c(item,buf,offset,length,iostat) \
        hio_c(item,FALSE,H_REAL,(char *)(buf),offset,length,iostat)
#define hwriter_c(item,buf,offset,length,iostat) \
        hio_c(item,TRUE,H_REAL,(char *)(buf),offset,length,iostat)
#define hreadd_c(item,buf,offset,length,iostat) \
        hio_c(item,FALSE,H_DBLE,(char *)(buf),offset,length,iostat)
#define hwrited_c(item,buf,offset,length,iostat) \
        hio_c(item,TRUE,H_DBLE,(char *)(buf),offset,length,iostat)
#define hreadc_c(item,buf,offset,length,iostat) \
        hio_c(item,FALSE,H_CMPLX,(char *)(buf),offset,length,iostat)
#define hwritec_c(item,buf,offset,length,iostat) \
        hio_c(item,TRUE,H_CMPLX,(char *)(buf),offset,length,iostat)
#define hwrite_c(item,type,buf,offset,length,iostat) \
        hio_c(item,TRUE,type,(char *)(buf),offset,length,iostat)
#define hread_c(item,type,buf,offset,length,iostat) \
        hio_c(item,FALSE,type,(char *)(buf),offset,length,iostat)
 
/* headio.c */

void hisopen_c  (int tno, Const char *status);
void hiswrite_c (int tno, Const char *text);
void hisread_c  (int tno, char *text, size_t length, int *eof);
void hisclose_c (int tno);
void wrhdr_c (int tno, Const char *keyword, double value);
void wrhdd_c (int tno, Const char *keyword, double value);
void wrhdi_c (int tno, Const char *keyword, int value);
void wrhdl_c (int tno, Const char *keyword, int8 value);
void wrhdc_c (int tno, Const char *keyword, Const float *value);
void wrhda_c (int tno, Const char *keyword, Const char *value);
void rdhdr_c (int tno, Const char *keyword, float *value, double defval);
void rdhdi_c (int tno, Const char *keyword, int *value, int defval);
void rdhdl_c (int tno, Const char *keyword, int8 *value, int8 defval);
void rdhdd_c (int tno, Const char *keyword, double *value, double defval);
void rdhdc_c (int tno, Const char *keyword, float *value, Const float *defval);
void rdhda_c (int tno, Const char *keyword, char *value, Const char *defval, int len);
void hdcopy_c (int tin, int tout, Const char *keyword);
int  hdprsnt_c (int tno, Const char *keyword);
void hdprobe_c (int tno, Const char *keyword, char *descr, size_t length, char *type, int *n);

/* dio.c */

void ddelete_c   (char *path, int *iostat);
void dtrans_c    (char *inpath, char *outpath, int *iostat);
void dmkdir_c    (char *path, int *iostat);
void drmdir_c    (char *path, int *iostat);
void dopen_c     (int *fd, char *name, char *status, off_t *size, int *iostat);
void dclose_c    (int fd, int *iostat);
void dread_c     (int fd, char *buffer, off_t offset, size_t length, int *iostat);
void dwrite_c    (int fd, char *buffer, off_t offset, size_t length, int *iostat);
void dwait_c     (int fd, int *iostat);
int dexpand_c    (char *tmplte, char *output, int length);
void dopendir_c  (char **contxt, char *path);
void dclosedir_c (char *contxt);
void dreaddir_c  (char *contxt, char *path, int length);

/* uvio.c */

void uvopen_c   (int *tno, Const char *name, Const char *status);
void uvclose_c  (int tno);
void uvflush_c  (int tno);
void uvnext_c   (int tno);
void uvrewind_c (int tno);
int  uvdim_c    (int tno);
void uvcopyvr_c (int tin, int tout);
int  uvupdate_c (int tno);
void uvvarini_c (int tno, int *vhan);
void uvvarset_c (int vhan, Const char *var);
void uvvarcpy_c (int vhan, int tout);
int  uvvarupd_c (int vhan);
void uvrdvr_c   (int tno, int type, Const char *var, char *data, char *def, int n);
void uvgetvr_c  (int tno, int type, Const char *var, char *data, int n);
void uvprobvr_c (int tno, Const char *var, char *type, int *length, int *updated);
void uvputvr_c  (int tno, int type, Const char *var, Const char *data, int n);
void uvtrack_c  (int tno, Const char *name, Const char *switches);
int  uvscan_c   (int tno, Const char *var);
void uvwrite_c  (int tno, Const double *preamble, Const float *data, Const int *flags, int n);
void uvwwrite_c (int tno, Const float *data, Const int *flags, int n);
void uvsela_c   (int tno, Const char *object, Const char *string, int datasel);
void uvselect_c (int tno, Const char *object, double p1, double p2, int datasel);
void uvset_c    (int tno, Const char *object, Const char *type, int n, double p1, double p2, double p3);
void uvread_c   (int tno, double *preamble, float *data, int *flags, int n, int *nread);
void uvwread_c  (int tno, float *data, int *flags, int n, int *nread);
void uvflgwr_c  (int tno, Const int *flags);
void uvwflgwr_c (int tno, Const int *flags);
void uvinfo_c   (int tno, Const char *object, double *data);
int  uvchkshadow_c (int tno, double diameter_meters);

/* Macros defined in uvio.c */

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

#define uvgetvra_c(tno,name,value,n)   \
        uvgetvr_c(tno,H_BYTE,name,value,n)
#define uvgetvrj_c(tno,name,value,n) \
        uvgetvr_c(tno,H_INT2,name,(char *)(value),n)
#define uvgetvri_c(tno,name,value,n) \
        uvgetvr_c(tno,H_INT,name,(char *)(value),n)
#define uvgetvrr_c(tno,name,value,n) \
        uvgetvr_c(tno,H_REAL,name,(char *)(value),n)
#define uvgetvrd_c(tno,name,value,n) \
        uvgetvr_c(tno,H_DBLE,name,(char *)(value),n)
#define uvgetvrc_c(tno,name,value,n) \
        uvgetvr_c(tno,H_CMPLX,name,(char *)(value),n)

#define uvrdvra_c(tno,name,data,def,len) \
	uvrdvr_c(tno,H_BYTE,name,data,def,len)
#define uvrdvri_c(tno,name,data,def) \
	uvrdvr_c(tno,H_INT,name,(char *)(data),(char *)(def),1)
#define uvrdvrr_c(tno,name,data,def) \
	uvrdvr_c(tno,H_REAL,name,(char *)(data),(char *)(def),1)
#define uvrdvrd_c(tno,name,data,def) \
	uvrdvr_c(tno,H_DBLE,name,(char *)(data),(char *)(def),1)
#define uvrdvrc_c(tno,name,data,def) \
	uvrdvr_c(tno,H_CMPLX,name,(char *)(data),(char *)(def),1)

/* xyio.c */

void xyopen_c  (int *tno, Const char *name, Const char *status, int naxis, int axes[]);
void xyflush_c (int tno);
void xyclose_c (int tno);
int  xydim_c   (int tno);
void xyread_c  (int tno, int index, float *array);
void xywrite_c (int tno, int index, Const float *array);
void xymkrd_c  (int tno, int index, int *runs, int n, int *nread);
void xymkwr_c  (int tno, int index, Const int *runs, int n);
void xyflgwr_c (int tno, int index, Const int *flags);
void xyflgrd_c (int tno, int index, int *flags);
void xysetpl_c (int tno, int naxis, Const int *axes);

/* maskio.c */
char *mkopen_c (int tno, char *name, char *status);
void mkclose_c (char *handle);
int  mkread_c  (char *handle, int mode, int *flags, off_t offset, int n, int nsize);
void mkwrite_c (char *handle, int mode, Const int *flags, off_t offset, int n, int nsize);
void mkflush_c (char *handle);
void setmaski_c(int *mask, Const int *masks);
void getmaski_c(Const int mask, int *masks);


/* xyzio.c */

void xyzopen_c  (int *tno, Const char *name, Const char *status, int *naxis, int axlen[]);
void xyzclose_c (int tno);
void xyzflush_c (int tno);
void xyzsetup_c (int tno, Const char *subcube, Const int blc[], Const int trc[], int viraxlen[], long vircubesize[]);
void xyzs2c_c   (int tno, long subcubenr, int coords[]);
void xyzc2s_c   (int tno, Const int coords[], long *subcubenr);
void xyzread_c  (int tno, Const int coords[], float *data, int *mask, int *ndata);
void xyzpixrd_c (int tno, long pixelnr, float *data, int *mask);
void xyzprfrd_c (int tno, int profilenr, float *data, int *mask, int *ndata);
void xyzplnrd_c (int tno, int planenr, float *data, int *mask, int *ndata);
void xyzwrite_c (int tno, Const int coords[], Const float *data, Const int *mask, Const int *ndata);
void xyzpixwr_c (int tno, long pixelnr, Const float *data, Const int *mask);
void xyzprfwr_c (int tno, int profilenr, Const float *data, Const int *mask, Const int *ndata);
void xyzplnwr_c (int tno, int planenr, Const float *data, Const int *mask, Const int *ndata);
void xyzmkbuf_c (void);
void xyzdim_c   (int tno, int *naxis, int *dimsub);
int xyzpix_c    (int tno, int dims);

/* bug.c */

char bugseverity_c(void);
char *bugmessage_c(void);
void bughandler_c(void (*handler)(char s, Const char *m));
void bugrecover_c(void (*cl)(void));
void buglabel_c  (Const char *name);
void bugno_c     (char s, int n);
char *errmsg_c   (int n);
void bug_c       (char s, Const char *m);
void bugv_c      (char s, Const char *m, ...);

/* scrio.c */

void scropen_c  (int *handle);
void scrclose_c (int handle);
void scrread_c  (int handle, float *buffer, off_t offset, size_t length);
void scrwrite_c (int handle, Const float *buffer, off_t offset, size_t length);
void scrrecsz_c (int handle, size_t recsize);

/* tabio.c */

void tabopen_c  (int *tno, Const char *name, Const char *status, int *ncol, int *nrow);
void tabclose_c (int tno);
void tabsetr_c  (int tno, int row);
void tabfmtc_c  (int tno, int col, char *fmt);
void tabcmt_c   (int tno, char *comment);
void tabwcr_c   (int tno, int col, float value);
void tabwcd_c   (int tno, int col, double value);
void tabwci_c   (int tno, int col, int value);
void tabwca_c   (int tno, int col, char *value);
void tabgetr_c  (int tno, int row, float *data);
void tabgetd_c  (int tno, int row, double *data);
void tabgeta_c  (int tno, int row, char *data);


/* key.c */

void keyinit_c (Const char *task);
void keyput_c  (Const char *task, char *string);
void keyini_c  (int argc, char *argv[]);
void keyfin_c  (void);
int keyprsnt_c (Const char *keyword);
void keya_c    (Const char *keyword, char *value, Const char *keydef);
void keyf_c    (Const char *keyword, char *value, Const char *keydef);
void keyd_c    (Const char *keyword, double *value, Const double keydef);
void keyr_c    (Const char *keyword, float *value, Const float keydef);
void keyi_c    (Const char *keyword, int *value, Const int keydef);
void keyl_c    (Const char *keyword, int *value, Const int keydef);
void mkeyd_c   (Const char *keyword, double value[], Const int nmax, int *n);
void mkeyr_c   (Const char *keyword, float value[], Const int nmax, int *n);
void mkeyi_c   (Const char *keyword, int value[], Const int nmax, int *n);

/* mir.c */
void mirInit_c(const char *f_name);
void mirClose_c(void);
void inWrite_c(const int conid, const int icocd, const int traid, const int inhid, const int ints, const int itq, const float az, const float el, const float ha, const int iut, const int iref_time, const double dhrs, const float vc, const int ivctype, const double sx, const double sy, const double sz, const float rinteg, const int proid, const int souid, const int isource, const int ipos, const float offx, const float offy, const int iofftype, const int ira, const int idec, const double rar, const double decr, const float epoch, const float sflux, const float size);
void blWrite_c(const int blhid, const int inhid, const int isb, const int ipol, const float pa, const int iaq, const int ibq, const int icq, const int ioq, const int irec, const int iffc, const float u, const float v, const float w, const float prbl, const float angres, const float vis, const float coh, const float sigcoh, const float csnr, const float vflux, const float cnoise, const double avedhrs, const float ampav, const float phaave, const float tpvar, const int blsid, const int itel1, const int itel2, const int iblcd, const float ble, const float bln, const float blu, const int soid);
void spWrite_c(const int sphid, const int blhid, const int inhid, const int igq, const int ipq, const int iband, const int ipstate, const float tau0, const double vel, const float vres, const int ivtype, const double fsky, const float fres, const float tssb, const float integ, const float wt, const int itaper, const float snoise, const int nch, const int nrec, const int dataoff, const int linid, const int itrans, const double rfreq, const int pasid, const int gaiidamp, const int gaiidpha, const int flcid, const int atmid);
void codeWrite_c(const char *v_name, const int icode, const char *code, const int ncode);
void visWrite_c(const float *re, const float *im, const int numvis, const int startvis, int *nbytes);

/* interface.c */
void pad(char *string, int length);
char *zterm(char *string, int length);

#if defined(__cplusplus)
}
#endif

#endif   /*  MIR_MIRIAD_H */
