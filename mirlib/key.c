/***********************************************************************
 *
 *  Key routines provide keyword-oriented access to the command line.
 *
 *  History:
 *    rjs  24apr91 Original version.
 *     jm  28jun94 Wrote the ANSI-C version.
 *     jm  01nov94 Added expand and local traits and added keyl().
 *     jm  17nov94 Rewrote #if definitions for ANSI prototypes.  Sun
 *                 machines define __STDC__ even when it is 0!  This
 *                 involved creating and using PROTOTYPE in sysdep.h.
 *     jm  24oct96 Increased MAXSTRING from 256 to 512 and fixed a
 *                 few lint complaints.
 *     jm  01aug97 Changed getKeyValue to properly handle single and
 *                 double quotes around a value.
 *    pjt  03sep98 fixed malloc(size+1) bug with interesting side-effects
 *		   on linux and HP-UX
 *    pjt   5aug99 increased MAXSTRING to 1024 (also do keyf.f !!!)
 *    pjt   6mar01 increased MAXSTRING to 2048
 *    mchw 15mar02 increased MAXSTRING to 4096
 *    pjt  22jun02 MIR4 prototypes, also added a few more Const 
 *    jwr  22jul04 changed a few vars from size_t to ssize_t, since signed
 *                 arithmetic is required.  Also made failure of wildcard
 *		   expansion fatal (it would crash later if only a warning
 *		   is given)
 *    pjt  13jul07 make unique messages in different pieces of code
 *    pjt  12jun10 added keyputc_c for ATNF compatibility
 *   dm/pjt 2dec10 better protection for keyword values overrun
 *                 keywrap.f2c is now calling keya_len_c() instead
 *                 deprecate keya_c()
 *    pjt  21jul11 keyl_c() now calls keya_len_c()
 ***********************************************************************
 */

#if defined(HAVE_CONFIG_H) && HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "miriad.h"

#ifndef Null
#define Null '\0'
#endif

/* if you change MAXSTRING, also do keyf.for */
#define KEYTRUE        1
#define KEYFALSE       0
#define MAXSTRING   4096

typedef struct ckeys {
    char *key;   /* Pointer to a malloc'd string holding the key name. */
    char *Pvalue;   /* Pointer to a malloc'd string holding the value. */
    char *value;                 /* Pointer to current spot in Pvalue. */
    int isexpanded;      /* False if not yet expanded; true otherwise. */
    int islocal;        /* True if defined locally; false if globally. */
    struct ckeys *fwd;              /* Pointer to next ckey structure. */
} KEYS;

static KEYS *KeyHead = (KEYS *)NULL;

      /* This will be set to KEYTRUE only when keyini[_c]() is called. */
static int iniCalled = KEYFALSE;

/***********************************************************************/
static char *skipLeading(Const char *string)
{
    char *ptr;

    if (string == (Const char *)NULL)
      return((char *)NULL);

    for (ptr = (char *)string; ((*ptr != Null) && isspace(*ptr)); ptr++)
      /* NULL */ ;

    return(ptr);
}

/***********************************************************************/
static KEYS *getKey(Const char *key)
{
    char *ptr;
    KEYS *t;

    /* First, check that the key routines have been initialized. */
    if (iniCalled == KEYFALSE) {
      (void)bug_c('f', "The Key initialization routine must be called first.");
    }
    /*
     *  Search for a key by name.  If the key name is not found,
     *  return a NULL pointer.  Otherwise, return a pointer to the
     *  private structure of the key.
     */
    if ((ptr = skipLeading(key)) == (char *)NULL)
      return((KEYS *)NULL);

    for (t = KeyHead; t != (KEYS *)NULL; t = t->fwd)
      if (strcmp(ptr, t->key) == 0)
        break;

    return(t);
}

/***********************************************************************/
static char *getKeyValue(Const char *key, int doexpand)
{
    char *r, *s;
    char quoted;
    char string[MAXSTRING];
    int more;
    ssize_t size, depth;
    KEYS *t;
    FILE *fp;

    if ((t = getKey(key)) == (KEYS *)NULL)
      return((char *)NULL);
    if ((t->value == (char *)NULL) || (*(t->value) == Null))
      return((char *)NULL);
    /*
     *  At this point, there is a value to return.  Scan through to
     *  the end of the parameter value.
     */
    r = s = skipLeading(t->value);
    depth = 0;
    more = KEYTRUE;
    quoted = Null;               /* Initially, not in a quoted string. */
    while ((*s != Null) && (more == KEYTRUE)) {
      if (quoted == Null) {           /* Not currently within a quote. */
        if ((*s == '"') || (*s == '\'')) {
          quoted = *s;    /* Set this to the char that ends the quote. */
        } else {
          if (*s == '(') depth++;
          else if (*s == ')') depth--;
          else if (isspace(*s) || (*s == ','))
            more = (depth == 0) ? KEYFALSE : KEYTRUE;
        }
      } else if (*s == quoted) { /* Inside a quote; read till matched. */
        quoted = Null;           /* Reset this to mean not in a quote. */
      }
      if (more == KEYTRUE) s++;
    }
    t->value = (*s == Null) ? s : s + 1;
    *s-- = Null;          /* Subtract 1 from index for following test. */

    /* Remove leading and trailing quotes. */
    if ((*r != Null) && (s > r)) {
      if (((*r == '"') && (*s == '"')) || ((*r == '\'') && (*s == '\''))) {
        *s = Null;
        r++;
      }
    }
    /*
     *  If the value starts with a '@' character, then open the
     *  given file name and store each line as a comma separated list.
     *  Next, if there is anything left in the keyword value list,
     *  add it to the end of the newly generated list.  Finally, re-call
     *  this routine to read the first parameter off the new list.
     */
    if (*r == '@') {
      r++;
      if ((fp = fopen(r, "r")) == (FILE *)NULL) {
        (void)sprintf(string, "Error opening @ file [%s].", r);
        (void)bug_c('f', string);
      }

      more = KEYTRUE;
      while (fgets(string, MAXSTRING, fp) != (char *)NULL) {
        if (((size = strlen(string)) > (size_t)0) && (string[size-1] == '\n'))
          string[size-1] = Null;

        r = skipLeading(string);
        if ((r == (char *)NULL) || (*r == Null) || (*r == '#'))
          continue;

        if (more == KEYTRUE) {
          depth = strlen(r) + 1;
          if ((s = (char *)malloc(depth)) == (char *)NULL)
            (void)bug_c('f', "Could not allocate memory in the key routines.");
          (void)strcpy(s, r);
          more = KEYFALSE;
        } else {
          depth += strlen(r) + 2;
          if ((s = (char *)realloc(s, depth)) == (char *)NULL)
            (void)bug_c('f', "Could not allocate memory in the key routines.");
          (void)strcat(s, ",");
          (void)strcat(s, r);
        }
      }

      (void)fclose(fp);

      if (depth == 0)
        (void)bug_c('f', "Trouble processing the @ directive.");

      if (*(t->value) != Null) {
        depth += strlen(t->value) + 2;
        if ((s = (char *)realloc(s, depth)) == (char *)NULL)
          (void)bug_c('f', "Could not allocate memory in the key routines.");
        (void)strcat(s, ",");
        (void)strcat(s, t->value);
      }

      (void)free((Void *)t->Pvalue);
      t->value = t->Pvalue = s;
      t->isexpanded = KEYTRUE;
      r = getKeyValue(key, doexpand);
    } else if ((doexpand == KEYTRUE) && (t->isexpanded == KEYFALSE)) {
     /*
      *  Otherwise, if expansion is desired and the keyword has not
      *  yet been expanded, call the dio.c routine dexpand_c() to
      *  return the result of the system call to "echo r".
      */
      size = dexpand_c(r, string, MAXSTRING);
      if (size < 1) {
        (void)sprintf(string, "Error doing wildcard expansion of [%s].", r);
        (void)bug_c('f', string);
      } else {
        if (*(t->value) != Null)
          size += strlen(t->value) + 2;
        if ((s = (char *)malloc(size+1)) == (char *)NULL)
          (void)bug_c('f', "Could not allocate memory in the key routines.");
        (void)strcpy(s, string);
        if (*(t->value) != Null) {
          (void)strcat(s, ",");
          (void)strcat(s, t->value);
        }
        (void)free((Void *)t->Pvalue);
        t->value = t->Pvalue = s;
        t->isexpanded = KEYTRUE;
      }
      r = getKeyValue(key, doexpand);
    }

    return((*r == Null) ? (char *)NULL : r);
}

/***********************************************************************/
void keyinit_c(Const char *task)
{
    buglabel_c(task);      /* Let the bug routines know the task name. */
    iniCalled = KEYTRUE;  /* Is True only when keyini[_c]() is called. */
}

/* hack to be able to use the ATNF fortran subroutine keyputc */

void keyputc_c(char *string)
{
  keyput_c("unknown", string);
}
/***********************************************************************/
void keyput_c(Const char *task, char *string)
/** KeyPut -- Store a keyword for later retrieval. */
/*& pjt */
/*: user-input,command-line */
/*+ FORTRAN call sequence:

        subroutine keyput(task,value)
        character task*(*),value*(*)

   This task stores the keyword=value pair for later retrieval by the
   other key routines.  If the keyword has previously been saved prior
   to calling this routine, then this version will only be saved if
   (1) the previous was not defined locally (ie. task/keyword) or
   (2) this reference is also a local reference.

   If the keyword is locally defined (ie. task/keyword) but the task
   name does not match the value of the input string task, then the
   keyword is not (ever) saved.

   NOTE:  This is an internal routine that is only called by KeyIni.

   Input:
     task       The name of the current task.
     value      The keyword=value pair.

   Output:
     (none)
 */
/*--*/
/*---------------------------------------------------------------------*/
{
    char *s, *key;
    char *pequal, *pslash;
    char errmsg[MAXSTRING];
    int localkey;
    KEYS *t;

    if (iniCalled == KEYFALSE) {
      (void)bug_c('f',
        "The Key initialization routine must be called before calling KEYPUT.");
    }

    if (((s = skipLeading(string)) == (char *)NULL) || (*s == Null)) {
      (void)sprintf(errmsg, "Badly formed parameter-1: [%s].", string);
      (void)bug_c('w', errmsg);
      return;
    } else if (*s == '#') {        /* Quietly return on comment lines. */
      return;
    }

    key = s;                                      /* Get the key name. */
    while ((*s != Null) && (isalnum(*s) || (*s == '$')))
      s++;
    if (*s == Null) {
      (void)sprintf(errmsg, "Badly formed parameter-2: [%s].", string);
      (void)bug_c('w', errmsg);
      return;
    }
    /*
     *  Search for the local keyword character (/).  If it exists,
     *  it must appear before the equal sign with text that precedes
     *  and follows it.  If the key is local, the key and value will
     *  be saved only if the task name matches the text before the
     *  local flag character.
     */
    localkey = KEYFALSE;
    if (((pslash = strchr(s, '/')) != (char *)NULL) &&
        ((pequal = strchr(s, '=')) != (char *)NULL) &&
         (pslash < pequal)) {
      *s = Null;                           /* Terminate the task name. */
      if (strcmp(task, key) != 0)  /* This keyword doesn't match task. */
        return;
      s = skipLeading(pslash+1);  /* Skip blanks after the local char. */

      localkey = KEYTRUE;
      key = s;                  /* Now, get the real [local] key name. */
      while ((*s != Null) && (isalnum(*s) || (*s == '$'))) s++;
      if (*s == Null) {
        (void)sprintf(errmsg, "Badly formed parameter-3: [%s].", string);
        (void)bug_c('w', errmsg);
        return;
      }
    }
    *s++ = Null;                    /* Properly terminate the keyword. */

    /* Now move to the value part of this keyword. */
    while ((*s != Null) && (isspace(*s) || (*s == '=')))
      s++;
    if ((*s == Null) || (strlen(s) < (size_t)1)) {
      (void)sprintf(errmsg, "Badly formed parameter-4: [%s=%s].", key, string);
      (void)bug_c('w', errmsg);
      return;
    }
    /*
     *  See if this keyword already exists.  If not, then create it.
     *  If it exists and was not previously declared local, then
     *  this version will override the previous reference of the key.
     *  If this reference was previously defined locally then only
     *  another local reference will override it.
     */
    for (t = KeyHead; t != (KEYS *)NULL; t = t->fwd) {
      if (strcmp(key, t->key) == 0)
        break;
    }

    if (t == (KEYS *)NULL) {
      if ((t = (KEYS *)malloc(sizeof(KEYS))) == (KEYS *)NULL)
        (void)bug_c('f', "Could not allocate memory in the key routines.");
      if ((t->key = (char *)malloc(strlen(key) + 1)) == (char *)NULL)
        (void)bug_c('f', "Could not allocate memory in the key routines.");
      (void)strcpy(t->key, key);
      t->fwd = KeyHead;
      KeyHead = t;
    } else if ((localkey == KEYTRUE) || (t->islocal != KEYTRUE)) {
      if (t->Pvalue != (char *)NULL)
        (void)free((Void *)t->Pvalue);
    } else {
      return;
    }

    if ((t->Pvalue = (char *)malloc(strlen(s) + 1)) == (char *)NULL)
      (void)bug_c('f', "Could not allocate memory in the key routines.");
    (void)strcpy(t->Pvalue, s);
    t->value = t->Pvalue;
    t->isexpanded = KEYFALSE;
    t->islocal = localkey;

    return;
}

/***********************************************************************/
void keyini_c(int argc, char *argv[])
/** KeyIni_c -- Initialise the `key' routines (C version). */
/*& pjt */
/*: user-input, command-line */
/*+

        void keyini_c(int argc, char *argv[])

    Keyini_c performs some initial parsing of the command line breaking
    it up into its keyword=value pairs.  It also stores the name of
    the program (which is currently only used by the bug routines).

    NOTE:  This has a different calling sequence than the Fortran
    version.
 */
/*--*/
/*---------------------------------------------------------------------*/
{
    char *task;
    char string[MAXSTRING];
    register int i;
    size_t size;
    FILE *fp;
    /*
     *  Get the program name, and tell the bug routines what it
     *  really is.  Then strip off any leading path characters
     *  so only the task name remains.
     */
    keyinit_c(argv[0]);
    task = argv[0] + strlen(argv[0]) - 1;
    while ((task > argv[0]) && (strchr("]/", task[-1]) == (char *)NULL))
      task--;

    for (i = 1; i < argc; i++) {
      if (strcmp("-f", argv[i]) == 0) {      /* Read args from a file. */
        if (++i >= argc)
          (void)bug_c('f', "KeyIni: No parameter file given for -f option.");

        if ((fp = fopen(argv[i], "r")) == (FILE *)NULL) {
          (void)sprintf(string,
            "KeyIni: Failed to open the parameter file [%s].", argv[i]);
          (void)bug_c('f', string);
        }

        while (fgets(string, MAXSTRING, fp) != (char *)NULL) {
          if (((size = strlen(string)) > (size_t)0) && (string[size-1] == '\n'))
            string[size-1] = Null;
          keyput_c(task, string);
        }

        (void)fclose(fp);
      } else if (strcmp("-?", argv[i]) == 0) {           /* Give help. */
        (void)sprintf(string, "mirhelp %s", task);
        (void)system(string);
        (void)exit(0);
      } else if (strcmp("-k", argv[i]) == 0) {   /* List the keywords. */
        (void)sprintf(string, "doc %s", task);
        (void)system(string);
        (void)exit(0);
      } else if (argv[i][0] == '-') {    /* Others not understood yet. */
        (void)sprintf(string, "KeyIni: Flag [%s] not understood.", argv[i]);
        (void)bug_c('w', string);
      } else {              /* Otherwise, the argument is a parameter. */
        keyput_c(task, argv[i]);
      }
    }
    return;
}

/***********************************************************************/
void keyfin_c(void)
/** KeyFin -- Finish access to the 'key' routines. */
/*& pjt */
/*: user-input,command-line */
/*+ FORTRAN call sequence:

        subroutine keyfin

   A call to KeyFin indicates that all of the parameters that the
   program wants have been retrieved from the command line.  KeyFin
   makes sure all command line parameters have been read.
 */
/*--*/
/*---------------------------------------------------------------------*/
{
    char errmsg[MAXSTRING];
    KEYS *t, *next;

    if (iniCalled == KEYFALSE) {
      (void)bug_c('f',
        "The Key initialization routine must be called before calling KEYFIN.");
    }

    next = (KEYS *)NULL;
    for (t = KeyHead; t != (KEYS *)NULL; t = next) {
      next = t->fwd;
      if ((t->value != (char *)NULL) && (*(t->value) != Null)) {
        (void)sprintf(errmsg, "Keyword [%s] not used or not exhausted.",
          t->key);
        (void)bug_c('w', errmsg);
      }

      if (t->Pvalue != (char *)NULL)
        (void)free((Void *)t->Pvalue);
      if (t->key != (char *)NULL)
        (void)free((Void *)t->key);
      (void)free((Void *)t);
    }

    KeyHead = (KEYS *)NULL;
    iniCalled = KEYFALSE;
    return;
}

/***********************************************************************/
/*  Returns FORT_TRUE if keyword is present; FORT_FALSE otherwise. */
int keyprsnt_c(Const char *keyword)
/** KeyPrsnt -- Determine if a keyword is present on the command line. */
/*& pjt */
/*: user-input,command-line */
/*+ FORTRAN call sequence:

        logical function keyprsnt(key)
        character key*(*)

   Determine if a parameter is still present.

   Input:
     key        The keyword to check.

   Output:
     keyprsnt   Is .TRUE. if the keyword is present; .FALSE. otherwise.
 */
/*--*/
/*---------------------------------------------------------------------*/
{
    int isPresent;
    KEYS *t;

    t = getKey(keyword);
    isPresent = ((t != (KEYS *)NULL) &&
                 (t->value != (char *)NULL) &&
                 (*(t->value) != Null)) ? FORT_TRUE : FORT_FALSE;

    return(isPresent);
}

/***********************************************************************/
void keya_c(Const char *keyword, char *value, Const char *keydef)
/** Keya -- Retrieve a character string from the command line. */
/*& pjt */
/*: user-input,command-line */
/*+ FORTRAN call sequence:

        subroutine keya(key,value,default)
        character key*(*)
        character value*(*),default*(*)

   Retrieve a character string from the command line. If the keyword
   is not found, the default is returned.

   Input:
     key        The name of the keyword to return.
     default    The default value to return if the keyword is not
                present on the command line.
   Output:
     value      The returned value.
 */
/*--*/
/*---------------------------------------------------------------------*/
{
    char *s;

    bugv_c ('w', "KeyA: keyword \"%s\" length not checked", keyword);

    s = getKeyValue(keyword, KEYFALSE);
    (void)strcpy(value, ((s == (char *)NULL) ? keydef : s));
    return;
}

void keya_len_c(Const char *keyword, char *value, size_t vlen, Const char *keydef)
{
    char *s;

    s = getKeyValue(keyword, KEYFALSE);

    if (s && strlen (s) > vlen)
	bugv_c ('f', "KeyA: value \"%s\" of keyword \"%s\" is doesn\'t fit in its "
		"Fortran buffer, which is only %zd bytes.", s, keyword, vlen);
    if ((s==(char *)NULL) && strlen(keydef) > vlen)
	bugv_c ('f', "KeyA: default value \"%s\" of keyword \"%s\" is would not fit in its "
		"Fortran buffer, which is only %zd bytes.", keydef, keyword, vlen);

    (void)strncpy(value, ((s == (char *)NULL) ? keydef : s), vlen);
    return;
}

/***********************************************************************/
void keyf_c(Const char *keyword, char *value, Const char *keydef)
/** Keyf -- Retrieve a file name (with wildcards) from the command line. */
/*& pjt */
/*: user-input,command-line */
/*+ FORTRAN call sequence:

        subroutine keyf(key,value,default)
        character key*(*)
        character value*(*),default*(*)

   Retrieve a character string from the command line. If the keyword
   is not found, the default is returned.

   Input:
     key        The name of the keyword to return.
     default    The default value to return if the keyword is not
                present on the command line.
   Output:
     value      The returned value.
 */
/*--*/
/*---------------------------------------------------------------------*/
{
    char *s;

    /* Expand any wildcards and match them with files. */
    s = getKeyValue(keyword, KEYTRUE);
    (void)strcpy(value, ((s == (char *)NULL) ? keydef : s));
    return;
}

/***********************************************************************/
void keyd_c(Const char *keyword, double *value, Const double keydef)
/** Keyd -- Retrieve a double precision from the command line. */
/*& pjt */
/*: user-input,command-line */
/*+ FORTRAN call sequence:

        subroutine keyd(key,value,default)
        character key*(*)
        double precision value,default

   Retrieve a double precision value from the command line.  If the
   keyword is not found, the default is returned.

   Input:
     key        The name of the keyword to return.
     default    The default value to return, if the keyword is not
                present on the command line.
   Output:
     value      The returned value.
 */
/*--*/
/*---------------------------------------------------------------------*/
{
    char *s, *ptr;
    char errmsg[MAXSTRING];

    *value = keydef;
    if ((s = getKeyValue(keyword, KEYFALSE)) == (char *)NULL)
      return;

    ptr = (char *)NULL;
    *value = strtod(s, &ptr);
    if (s == ptr) {
      (void)sprintf(errmsg,
        "KeyD: Conversion error decoding parameter [%s=%s].", keyword, s);
      (void)bug_c('f', errmsg);
    }

    return;
}

/***********************************************************************/
void keyr_c(Const char *keyword, float *value, Const float keydef)
/** Keyr -- Retrieve a real value from the command line. */
/*& pjt */
/*: user-input,command-line */
/*+ FORTRAN call sequence:

        subroutine keyr(key,value,default)
        character key*(*)
        real value,default

   Retrieve a real value from the command line.  If the keyword is
   not found, the default is returned.

   Input:
     key        The name of the keyword to return.
     default    The default value to return, if the keyword is not
                present on the command line.
   Output:
     value      The returned value.
 */
/*--*/
/*---------------------------------------------------------------------*/
{
    double retval, defval;

    defval = keydef;
    keyd_c(keyword, &retval, defval);
    *value = retval;
    return;
}

/***********************************************************************/
void keyi_c(Const char *keyword, int *value, Const int keydef)
/** Keyi -- Retrieve an integer from the command line. */
/*& pjt */
/*: user-input,command-line */
/*+ FORTRAN call sequence:

        subroutine keyi(key,value,default)
        character key*(*)
        integer value,default

   Retrieve an integer from the command line.  If the keyword is
   not found, the default is returned.  The integer can be input
   as a hexadecimal, octal or decimal number using a prefix 0x, %x
   or h for hex; o or %o for octal; and +, - or nothing for decimal.

   Input:
     key        The name of the keyword to return.
     default    The default value to return, if the keyword is not
                present on the command line.
   Output:
     value      The returned value.
 */
/*--*/
/*---------------------------------------------------------------------*/
{
    char *s, *ptr;
    char temp[MAXSTRING];
    int iarg, dummy;
    double dval;

    *value = keydef;
    if ((s = getKeyValue(keyword, KEYFALSE)) == (char *)NULL)
      return;
    /*
     *  This business is done instead of a call to strtol because
     *  hexadecimal and octal are also acceptable integer inputs.
     */
    (void)sprintf(temp, "%s~~1", s);
    if ((sscanf(temp, "%i~~%d", &iarg, &dummy) == 2) && (dummy == 1)) {
      *value = iarg;               /* Token was just a simple integer. */
      return;
    }

    /*  Number is floating point; find it and then take nint(). */
    ptr = (char *)NULL;
    dval = strtod(s, &ptr);
    if (s == ptr) {
      (void)sprintf(temp,
        "KeyI: Conversion error decoding parameter [%s=%s].", keyword, s);
      (void)bug_c('f', temp);
    }
    *value = (dval >= 0) ? floor(dval + 0.5) : ceil(dval - 0.5);

    return;
}

/***********************************************************************/
void keyl_c(Const char *keyword, int *value, Const int keydef)
/** keyl -- Retrieve a logical value from the command line. */
/*& pjt */
/*: user-input,command-line */
/*+ FORTRAN call sequence:

        subroutine keyl(key,value,default)
        character key*(*)
        logical value,default

   Retrieve a logical value from the command line. If the keyword is
   not found, the default is returned.  It associates (case
   insensitive) words starting with 'y', 't' and '1' as .TRUE. and
   words starting with 'n', 'f' and '0' as .FALSE.  Values of .TRUE.
   and .FALSE. are also detected... both with minimum match.

   Input:
     key      The name of the keyword to return.
     default  The default value to return, if the keyword is not
              present on the command line.
   Output:
     value    The returned value.
 */
/*--*/
/*---------------------------------------------------------------------*/
{
    char string[MAXSTRING];
    char errmsg[MAXSTRING];
    int state;

    if (keydef == FORT_FALSE) {
      keya_len_c(keyword, string, MAXSTRING, "f");
      state = KEYFALSE;
    } else {
      keya_len_c(keyword, string, MAXSTRING, "t");
      state = KEYTRUE;
    }

    (void)sprintf(errmsg, "KeyL: invalid value for a logical: [%s].", string);
    switch ((int)string[0]) {
      case 'f': case 'F': case 'n': case 'N': case '0':
        state = KEYFALSE;
        break;
      case 't': case 'T': case 'y': case 'Y': case '1':
        state = KEYTRUE;
        break;
      case '.':
        switch ((int)string[1]) {
          case 'f': case 'F':
            state = KEYFALSE;
            break;
          case 't': case 'T':
            state = KEYTRUE;
            break;
          default:
            (void)bug_c('w', errmsg);
            break;
        }
        break;
      default:
        (void)bug_c('w', errmsg);
        break;
    }

    *value = (state == KEYTRUE) ? FORT_TRUE : FORT_FALSE;
    return;
}

/***********************************************************************/
/*
 *  The following macro is used by all of the subsequent multi-get
 *  routines.  It assumes that:
 *
 *    Const char *keyword is the name of the key to retrieve;
 *    T value[] is the array to receive each returned value;
 *    int nmax  is the maximum number of items to get; and
 *    int *n    is the number of items returned;
 *
 *  where T is the type of the variable (char *, double, float, or int).
 *
 *  In the macro below,
 *    task      is the name of the individual item retrieval task;
 *    defval    is the default value to be used if the keyword is missing; and
 *    name      is a string representing the task name (to be used in
 *              an error message.
 *
 *  The do {} while (1==0) construct is done so that the macro may
 *  be called like a regular subroutine.
 */
#define MULTIGET(task,defval,name)                  \
    do {                                            \
      char errmsg[MAXSTRING];                       \
      register int count = 0;                       \
                                                    \
      while ((count < nmax) && (keyprsnt_c(keyword) == FORT_TRUE)) \
        task(keyword, &value[count++], defval);     \
                                                    \
      if (keyprsnt_c(keyword) == FORT_TRUE) {       \
        (void)sprintf(errmsg, "%s: Buffer overflow for keyword [%s].", \
          name, keyword);                           \
        (void)bug_c('f', errmsg);                   \
      }                                             \
                                                    \
      *n = count;                                   \
    } while (1==0)

/***********************************************************************/
void mkeyd_c(Const char *keyword, double value[], Const int nmax, int *n)
/** MKeyd -- Retrieve multiple double values from the command line. */
/*& pjt */
/*: user-input,command-line */
/*+ FORTRAN call sequence:

        subroutine mkeyd(key,value,nmax,n)
        integer nmax, n
        character key*(*)
        double precision value(nmax)

   Retrieve multiple double precision values from the command line.
   If the keyword is not found, then zero values are returned.

   Input:
     key        The name of the keyword to return.
     nmax       The maximum number of values to return

   Output:
     n          The number of values returned.
     value      The returned values
 */
/*--*/
/*---------------------------------------------------------------------*/
{
    MULTIGET(keyd_c, 0.0, "MKeyD");
    return;
}

/***********************************************************************/
void mkeyr_c(Const char *keyword, float value[], Const int nmax, int *n)
/** MKeyr -- Retrieve multiple real values from the command line. */
/*& pjt */
/*: user-input,command-line */
/*+ FORTRAN call sequence:

        subroutine mkeyr(key,value,nmax,n)
        integer nmax, n
        character key*(*)
        real value(nmax)

   Retrieve multiple real values from the command line.  If the keyword
   is not found, then zero values are returned.

   Input:
     key        The name of the keyword to return.
     nmax       The maximum number of values to return

   Output:
     n          The number of values returned.
     value      The returned values
 */
/*--*/
/*---------------------------------------------------------------------*/
{
    MULTIGET(keyr_c, 0.0, "MKeyR");
    return;
}

/***********************************************************************/
void mkeyi_c(Const char *keyword, int value[], Const int nmax, int *n)
/** MKeyi -- Retrieve multiple integer values from the command line. */
/*& pjt */
/*: user-input,command-line */
/*+ FORTRAN call sequence:

        subroutine mkeyi(key,value,nmax,n)
        integer nmax, n
        character key*(*)
        integer value(nmax)

   Retrieve multiple integer values from the command line.  If the
   keyword is not found, then zero values are returned.

   Input:
     key        The name of the keyword to return.
     nmax       The maximum number of values to return

   Output:
     n          The number of values returned.
     value      The returned values
 */
/*--*/
/*---------------------------------------------------------------------*/
{
    MULTIGET(keyi_c, 0, "MKeyI");
    return;
}

#ifdef TESTBED
/***********************************************************************/
int main(int argc, char *argv[])
{
    char aval[100];
    register int i;
    int ival;
    float fval;
    double cnt[5];

    keyini_c(argc, argv);
    mkeyd_c("cnt", cnt, 5, &ival);
    for (i = 0; i < ival; i++)
      (void)printf("cnt[%d] = %g\n", i+1, cnt[i]);
    for (i = 0; i < 5; i++) {
      keyi_c("alpha", &ival, 100);
      (void)printf("Alpha[100] = %d ", ival);
      keya_c("beta", aval, "def-val");
      (void)printf("Beta[def-val] = %s ", aval);
      keyr_c("gamma", &fval, 10.0);
      (void)printf("Gamma[10.0] = %g\n", fval);
    }
    keyfin_c();
    (void)exit(0);
}
#endif
