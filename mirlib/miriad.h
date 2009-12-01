/*
// Header that gives C++ code access to the MIRIAD IO routines
// May need careful monitoring how MIRIAD code develops
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
*/

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

#define TRUE		1
#define FALSE		0
#define H_BYTE          1
#define H_INT           2
#define H_INT2          3
#define H_REAL          4
#define H_DBLE          5
#define H_TXT           6
#define H_CMPLX         7

/* hio.c */

void hopen_c ARGS((int *tno, Const char *name, Const char *status, int *iostat));
void hflush_c ARGS((int tno, int *iostat));
void habort_c();
void hrm_c ARGS((int tno));
void hclose_c ARGS((int tno));
void hdelete_c ARGS((int tno, Const char *keyword, int *iostat));
void haccess_c ARGS((int tno, int *ihandle, Const char *keyword, Const char *status, int *iostat));
void hmode_c ARGS((int tno, char *mode));
int  hexists_c ARGS((int tno, Const char *keyword));
void hdaccess_c ARGS((int ihandle, int *iostat));
int  hsize_c ARGS((int ihandle));
void hio_c ARGS((int ihandle, int dowrite, int type, char *buf, int offset, int length, int *iostat));
void hseek_c ARGS((int ihandle, int offset));
int  htell_c ARGS((int ihandle));
void hreada_c ARGS((int ihandle, char *line, int length, int *iostat));
void hwritea_c ARGS((int ihandle, Const char *line, int length, int *iostat));

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

void hisopen_c ARGS((int tno, Const char *status));
void hiswrite_c ARGS((int tno, Const char *text));
void hisread_c ARGS((int tno, char *text, int length, int *eof));
void hisclose_c ARGS((int tno));
void wrhdr_c ARGS((int tno, Const char *keyword, double value));
void wrhdd_c ARGS((int tno, Const char *keyword, double value));
void wrhdi_c ARGS((int tno, Const char *keyword, int value));
void wrhdc_c ARGS((int tno, Const char *keyword, Const float *value));
void wrhda_c ARGS((int tno, Const char *keyword, Const char *value));
void rdhdr_c ARGS((int tno, Const char *keyword, float *value, double defval));
void rdhdi_c ARGS((int tno, Const char *keyword, int *value, int defval));
void rdhdd_c ARGS((int tno, Const char *keyword, double *value, double defval));
void rdhdc_c ARGS((int tno, Const char *keyword, float *value, Const float *defval));
void rdhda_c ARGS((int tno, Const char *keyword, char *value, Const char *defval, int len));
void hdcopy_c ARGS((int tin, int tout, Const char *keyword));
int  hdprsnt_c ARGS((int tno, Const char *keyword));
void hdprobe_c ARGS((int tno, Const char *keyword, char *descr, int length, char *type, int *n));

/* uvio.c */

void uvopen_c ARGS((int *tno, Const char *name, Const char *status));
void uvclose_c ARGS((int tno));
void uvflush_c ARGS((int tno));
void uvnext_c ARGS((int tno));
void uvrewind_c ARGS((int tno));
void uvcopyvr_c ARGS((int tin, int tout));
int  uvupdate_c ARGS((int tno));
void uvvarini_c ARGS((int tno, int *vhan));
void uvvarset_c ARGS((int vhan, Const char *var));
void uvvarcpy_c ARGS((int vhan, int tout));
int  uvvarupd_c ARGS((int vhan));
void uvrdvr_c ARGS((int tno, int type, Const char *var, char *data, char *def, int n));
void uvgetvr_c ARGS((int tno, int type, Const char *var, char *data, int n));
void uvprobvr_c ARGS((int tno, Const char *var, char *type, int *length, int *updated));
void uvputvr_c ARGS((int tno, int type, Const char *var, Const char *data, int n));
void uvtrack_c ARGS((int tno, Const char *name, Const char *switches));
int  uvscan_c ARGS((int tno, Const char *var));
void uvwrite_c ARGS((int tno, Const double *preamble, Const float *data, Const int *flags, int n));
void uvwwrite_c ARGS((int tno, Const float *data, Const int *flags, int n));
void uvsela_c ARGS((int tno, Const char *object, Const char *string, int datasel));
void uvselect_c ARGS((int tno, Const char *object, double p1, double p2, int datasel));
void uvset_c ARGS((int tno, Const char *object, Const char *type, int n, double p1, double p2, double p3));
void uvread_c ARGS((int tno, double *preamble, float *data, int *flags, int n, int *nread));
void uvwread_c ARGS((int tno, float *data, int *flags, int n, int *nread));
void uvflgwr_c ARGS((int tno, Const int *flags));
void uvwflgwr_c ARGS((int tno, Const int *flags));
void uvinfo_c ARGS((int tno, Const char *object, double *data));

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

void xyopen_c ARGS((int *tno, Const char *name, Const char *status, int naxis, int axes[]));
void xyflush_c ARGS((int tno));
void xyclose_c ARGS((int tno));
void xyread_c ARGS((int tno, int index, float *array));
void xywrite_c ARGS((int tno, int index, Const float *array));
void xymkrd_c ARGS((int tno, int index, int *runs, int n, int *nread));
void xymkwr_c ARGS((int tno, int index, Const int *runs, int n));
void xyflgwr_c ARGS((int tno, int index, Const int *flags));
void xyflgrd_c ARGS((int tno, int index, int *flags));
void xysetpl_c ARGS((int tno, int naxis, Const int *axes));

/* xyzio.c */

void xyzopen_c ARGS((int *tno, Const char *name, Const char *status, int *naxis, int axlen[]));
void xyzclose_c ARGS((int tno));
void xyzflush_c ARGS((int tno));
void xyzsetup_c ARGS((int tno, Const char *subcube, Const int blc[], Const int trc[], int viraxlen[], int vircubesize[]));
void xyzs2c_c ARGS((int tno, int subcubenr, int coords[]));
void xyzc2s_c ARGS((int tno, Const int coords[], int *subcubenr));
void xyzread_c ARGS((int tno, Const int coords[], float *data, int *mask, int *ndata));
void xyzpixrd_c ARGS((int tno, int pixelnr, float *data, int *mask));
void xyzprfrd_c ARGS((int tno, int profilenr, float *data, int *mask, int *ndata));
void xyzplnrd_c ARGS((int tno, int planenr, float *data, int *mask, int *ndata));
void xyzwrite_c ARGS((int tno, Const int coords[], Const float *data, Const int *mask, Const int *ndata));
void xyzpixwr_c ARGS((int tno, int pixelnr, Const float *data, Const int *mask));
void xyzprfwr_c ARGS((int tno, int profilenr, Const float *data, Const int *mask, Const int *ndata));
void xyzplnwr_c ARGS((int tno, int planenr, Const float *data, Const int *mask, Const int *ndata));

/* bug.c */

void buglabel_c ARGS((Const char *name));
void bugno_c ARGS((char s, int n));
void bug_c ARGS((char s, Const char *m));

/* scrio.c */

void scropen_c ARGS((int *handle));
void scrclose_c ARGS((int handle));
void scrread_c ARGS((int handle, float *buffer, int offset, int length));
void scrwrite_c ARGS((int handle, Const float *buffer, int offset, int length));

/* key.c */

void keyinit_c(char *task);
void keyput_c(char *task, char *string);
void keyini_c(int argc, char *argv[]);
void keyfin_c(void);
int keyprsnt_c(char *keyword);
void keya_c(char *keyword, char *value, char *keydef);
void keyf_c(char *keyword, char *value, char *keydef);
void keyd_c(char *keyword, double *value, double keydef);
void keyr_c(char *keyword, float *value, float keydef);
void keyi_c(char *keyword, int *value, int keydef);
void keyl_c(char *keyword, int *value, int keydef);
void mkeyd_c(char *keyword, double value[], int nmax, int *n);
void mkeyr_c(char *keyword, float value[], int nmax, int *n);
void mkeyi_c(char *keyword, int value[], int nmax, int *n);


#if defined(__cplusplus)
}
#endif

