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
// 22-jan-97 pjt added key.c
//                (although I can't reproduce rjs's cproto example)
*/

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


#if !defined(ARGS)
#  if defined(__STDC__) || defined(__cplusplus)
#    define ARGS(s) s
#  else
#    define ARGS(s) ()
#  endif
#endif


/* hio.c */

void hopen_c ARGS((int *tno, char *name, char *status, int *iostat));
void hflush_c ARGS((int tno, int *iostat));
void habort_c ARGS((void));
void hrm_c ARGS((int tno));
void hclose_c ARGS((int tno));
void hdelete_c ARGS((int tno, char *keyword, int *iostat));
void haccess_c ARGS((int tno, int *ihandle, char *keyword, char *status, int *iostat));
void hmode_c ARGS((int tno, char *mode));
int  hexists_c ARGS((int tno, char *keyword));
void hdaccess_c ARGS((int ihandle, int *iostat));
int  hsize_c ARGS((int ihandle));
void hio_c ARGS((int ihandle, int dowrite, int type, char *buf, int offset, int length, int *iostat));
void hseek_c ARGS((int ihandle, int offset));
int  htell_c ARGS((int ihandle));
void hreada_c ARGS((int ihandle, char *line, int length, int *iostat));
void hwritea_c ARGS((int ihandle, char *line, int length, int *iostat));

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

void hisopen_c ARGS((int tno, char *status));
void hiswrite_c ARGS((int tno, char *text));
void hisread_c ARGS((int tno, char *text, int length, int *eof));
void hisclose_c ARGS((int tno));
void wrhdr_c ARGS((int thandle, char *keyword, double value));
void wrhdd_c ARGS((int thandle, char *keyword, double value));
void wrhdi_c ARGS((int thandle, char *keyword, int value));
void wrhdc_c ARGS((int thandle, char *keyword, float *value));
void wrhda_c ARGS((int thandle, char *keyword, char *value));
void rdhdr_c ARGS((int thandle, char *keyword, float *value, double defval));
void rdhdi_c ARGS((int thandle, char *keyword, int *value, int defval));
void rdhdd_c ARGS((int thandle, char *keyword, double *value, double defval));
void rdhdc_c ARGS((int thandle, char *keyword, float *value, float *defval));
void rdhda_c ARGS((int thandle, char *keyword, char *value, char *defval, int len));
void hdcopy_c ARGS((int tin, int tout, char *keyword));
int  hdprsnt_c ARGS((int tno, char *keyword));
void hdprobe_c ARGS((int tno, char *keyword, char *descr, int length, char *type, int *n));

/* uvio.c */

void uvopen_c ARGS((int *tno, const char *name, char *status));
void uvclose_c ARGS((int tno));
void uvflush_c ARGS((int tno));
void uvnext_c ARGS((int tno));
void uvrewind_c ARGS((int tno));
void uvcopyvr_c ARGS((int tin, int tout));
int  uvupdate_c ARGS((int tno));
void uvvarini_c ARGS((int tno, int *vhan));
void uvvarset_c ARGS((int vhan, char *var));
void uvvarcpy_c ARGS((int vhan, int tout));
int  uvvarupd_c ARGS((int vhan));
void uvrdvr_c ARGS((int tno, int type, char *var, char *data, char *def, int n));
void uvgetvr_c ARGS((int tno, int type, char *var, char *data, int n));
void uvprobvr_c ARGS((int tno, char *var, char *type, int *length, int *updated));
void uvputvr_c ARGS((int tno, int type, char *var, char *data, int n));
void uvtrack_c ARGS((int tno, char *name, char *switches));
int  uvscan_c ARGS((int tno, char *var));
void uvwrite_c ARGS((int tno, double *preamble, float *data, int *flags, int n));
void uvwwrite_c ARGS((int tno, float *data, int *flags, int n));
void uvsela_c ARGS((int tno, char *object, char *string, int datasel));
void uvselect_c ARGS((int tno, char *object, double p1, double p2, int datasel));
void uvset_c ARGS((int tno, char *object, char *type, int n, double p1, double p2, double p3));
void uvread_c ARGS((int tno, double preamble[4], float *data, int *flags, int n, int *nread));
void uvwread_c ARGS((int tno, float *data, int *flags, int n, int *nread));
void uvflgwr_c ARGS((int tno, int *flags));
void uvwflgwr_c ARGS((int tno, int *flags));
void uvinfo_c ARGS((int tno, char *object, double *data));

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

/* xyio.c */

void xyopen_c ARGS((int *thandle, char *name, char *status, int naxis, int axes[]));
void xyflush_c ARGS((int thandle));
void xyclose_c ARGS((int thandle));
void xyread_c ARGS((int thandle, int index, float array[]));
void xywrite_c ARGS((int thandle, int index, float array[]));
void xymkrd_c ARGS((int thandle, int index, int runs[], int n, int *nread));
void xymkwr_c ARGS((int thandle, int index, int runs[], int n));
void xyflgwr_c ARGS((int thandle, int index, int flags[]));
void xyflgrd_c ARGS((int thandle, int index, int flags[]));
void xysetpl_c ARGS((int thandle, int naxis, int axes[]));

/* xyzio.c */

void xyzopen_c ARGS((int *handle, char *name, char *status, int *naxis, int axlen[]));
void xyzclose_c ARGS((int tno));
void xyzflush_c ARGS((int tno));
void xyzsetup_c ARGS((int tno, char *subcube, int blc[], int trc[], int viraxlen[], int vircubesize[]));
void xyzs2c_c ARGS((int tno, int subcubenr, int coords[]));
void xyzc2s_c ARGS((int tno, int coords[], int *subcubenr));
void xyzread_c ARGS((int tno, int coords[], float *data, int *mask, int *ndata));
void xyzpixrd_c ARGS((int tno, int pixelnr, float *data, int *mask));
void xyzprfrd_c ARGS((int tno, int profilenr, float *data, int *mask, int *ndata));
void xyzplnrd_c ARGS((int tno, int planenr, float *data, int *mask, int *ndata));
void xyzwrite_c ARGS((int tno, int coords[], float *data, int *mask, int *ndata));
void xyzpixwr_c ARGS((int tno, int pixelnr, float *data, int *mask));
void xyzprfwr_c ARGS((int tno, int profilenr, float *data, int *mask, int *ndata));
void xyzplnwr_c ARGS((int tno, int planenr, float *data, int *mask, int *ndata));

/* bug.c */

void buglabel_c ARGS((char *name));
void bugno_c ARGS((char s, int n));
void bug_c ARGS((char s, char *m));

/* scrio.c */

void scropen_c ARGS((int *handle));
void scrclose_c ARGS((int handle));
void scrread_c ARGS((int handle, float *buffer, int offset, int length));
void scrwrite_c ARGS((int handle, float *buffer, int offset, int length));

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

