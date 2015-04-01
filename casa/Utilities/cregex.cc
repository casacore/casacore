/*
    cregex.c: Extended regular expression matching and search library
    Copyright (C) 1993,1994,1995,1996,1997,1999,2001,2003
    Associated Universities, Inc. Washington DC, USA.

    This library is free software; you can redistribute it and/or modify it
    under the terms of the GNU Library General Public License as published by
    the Free Software Foundation; either version 2 of the License, or (at your
    option) any later version.

    This library is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
    License for more details.

    You should have received a copy of the GNU Library General Public License
    along with this library; if not, write to the Free Software Foundation,
    Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.

    Correspondence concerning AIPS++ should be addressed as follows:
           Internet email: aips2-request@nrao.edu.
           Postal address: AIPS++ Project Office
                           National Radio Astronomy Observatory
                           520 Edgemont Road
                           Charlottesville, VA 22903-2475 USA
*/
//# $Id$


/* Get the interface, including the syntax bits.  */
#include <casacore/casa/Utilities/cregex.h>
#include <casacore/casa/string.h>
#include <casacore/casa/stdlib.h>
/* We write fatal error messages on standard error.  */
#include <casacore/casa/stdio.h>
/* isalpha(3) etc. are used for the character classes.  */
/* The CB_CTYPE_MACROS causes incompatible ctype definitions on */
/* HPUX 10.2 systems, so we undefine it  AxC 23-1-2003 */
#if defined (HPUX10_2)
#  undef _SB_CTYPE_MACROS
#endif
#include <ctype.h>
/* Sequents are missing isgraph.  */
/* Get the RE_DUP_MAX value */
#include <limits.h>

// The Intel compiler buggers up limits.h so RE_DUP_MAX doesn't get defined so 
// if it's not defined we will do it here via the regex.h include.

#ifndef RE_DUP_MAX
#include <regex.h>
#endif


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Define some global variables; before they were externs in the .h file.

/* Set by a2_re_set_syntax to the current regexp syntax to recognize.  */
int obscure_syntax = 0;

char re_syntax_table[256];

/* Roughly the maximum number of failure points on the stack.  Would be
   exactly that if always pushed MAX_NUM_FAILURE_ITEMS each time we failed.  */
int re_max_failures = 2000;



// Hack to avoid requiring alloca. Define an instance of this at the beginning
// of your function and call it alloca.
class cregex_allocator
{
public:
    cregex_allocator();
    ~cregex_allocator();
     char * operator()(int nbytes);
private:
    enum {max_allocations_p = 128};
    char *allocated_table_p[max_allocations_p];
    int pos_p;
    
    cregex_allocator(const cregex_allocator &);
    cregex_allocator& operator=(const cregex_allocator &);
};

cregex_allocator::cregex_allocator()
  : pos_p(-1) 
{
}

cregex_allocator::~cregex_allocator()
{
  for (int i=0; i <= pos_p; i++) {
    delete [] allocated_table_p[i];
  }
}

char *cregex_allocator::operator()(int nbytes)
{
  if (pos_p >= max_allocations_p-1) { 
    fprintf(stderr, "cregex.cc: larger allocation table needed\n");
    return 0;
  }
  pos_p++;
  return (allocated_table_p[pos_p] = new char[nbytes]);
}

/* Define the syntax stuff, so we can do the \<, \>, etc.  */

/* This must be nonzero for the wordchar and notwordchar pattern
   commands in a2_re_match_2.  */
#ifndef Sword 
#define Sword 1
#endif

#define SYNTAX(c) re_syntax_table[c]

static void
init_syntax_once ()
{
   int c;
   static int done = 0;

   if (done)
     return;

   memset (re_syntax_table, 0, sizeof re_syntax_table);

   for (c = 'a'; c <= 'z'; c++)
     re_syntax_table[c] = Sword;

   for (c = 'A'; c <= 'Z'; c++)
     re_syntax_table[c] = Sword;

   for (c = '0'; c <= '9'; c++)
     re_syntax_table[c] = Sword;

   done = 1;
}

#ifndef isgraph
#define isgraph(c) (isprint((c)) && !isspace((c)))
#endif


/* These are the command codes that appear in compiled regular
   expressions, one per byte.  Some command codes are followed by
   argument bytes.  A command code can specify any interpretation
   whatsoever for its arguments.  Zero-bytes may appear in the compiled
   regular expression.
   
   The value of `exactn' is needed in search.c (search_buffer) in emacs.
   So regex.h defines a symbol `RE_EXACTN_VALUE' to be 1; the value of
   `exactn' we use here must also be 1.  */

enum regexpcode
  {
    unused=0,
    exactn=1, /* Followed by one byte giving n, then by n literal bytes.  */
    begline,  /* Fail unless at beginning of line.  */
    endline,  /* Fail unless at end of line.  */
    jump,     /* Followed by two bytes giving relative address to jump to.  */
    on_failure_jump,	 /* Followed by two bytes giving relative address of 
			    place to resume at in case of failure.  */
    finalize_jump,	 /* Throw away latest failure point and then jump to 
			    address.  */
    maybe_finalize_jump, /* Like jump but finalize if safe to do so.
			    This is used to jump back to the beginning
			    of a repeat.  If the command that follows
			    this jump is clearly incompatible with the
			    one at the beginning of the repeat, such that
			    we can be sure that there is no use backtracking
			    out of repetitions already completed,
			    then we finalize.  */
    dummy_failure_jump,  /* Jump, and push a dummy failure point. This 
			    failure point will be thrown away if an attempt 
                            is made to use it for a failure. A + construct 
                            makes this before the first repeat.  Also
                            use it as an intermediary kind of jump when
                            compiling an or construct.  */
    succeed_n,	 /* Used like on_failure_jump except has to succeed n times;
		    then gets turned into an on_failure_jump. The relative
                    address following it is useless until then.  The
                    address is followed by two bytes containing n.  */
    jump_n,	 /* Similar to jump, but jump n times only; also the relative
		    address following is in turn followed by yet two more bytes
                    containing n.  */
    set_number_at,	/* Set the following relative location to the
			   subsequent number.  */
    anychar,	 /* Matches any (more or less) one character.  */
    charset,     /* Matches any one char belonging to specified set.
		    First following byte is number of bitmap bytes.
		    Then come bytes for a bitmap saying which chars are in.
		    Bits in each byte are ordered low-bit-first.
		    A character is in the set if its bit is 1.
		    A character too large to have a bit in the map
		    is automatically not in the set.  */
    charset_not, /* Same parameters as charset, but match any character
                    that is not one of those specified.  */
    start_memory, /* Start remembering the text that is matched, for
		    storing in a memory register.  Followed by one
                    byte containing the register number.  Register numbers
                    must be in the range 0 through RE_NREGS.  */
    stop_memory, /* Stop remembering the text that is matched
		    and store it in a memory register.  Followed by
                    one byte containing the register number. Register
                    numbers must be in the range 0 through RE_NREGS.  */
    duplicate,   /* Match a duplicate of something remembered.
		    Followed by one byte containing the index of the memory 
                    register.  */
    before_dot,	 /* Succeeds if before point.  */
    at_dot,	 /* Succeeds if at point.  */
    after_dot,	 /* Succeeds if after point.  */
    begbuf,      /* Succeeds if at beginning of buffer.  */
    endbuf,      /* Succeeds if at end of buffer.  */
    wordchar,    /* Matches any word-constituent character.  */
    notwordchar, /* Matches any char that is not a word-constituent.  */
    wordbeg,	 /* Succeeds if at word beginning.  */
    wordend,	 /* Succeeds if at word end.  */
    wordbound,   /* Succeeds if at a word boundary.  */
    notwordbound,/* Succeeds if not at a word boundary.  */
    syntaxspec,  /* Matches any character whose syntax is specified.
		    followed by a byte which contains a syntax code,
                    e.g., Sword.  */
    notsyntaxspec /* Matches any character whose syntax differs from
                     that specified.  */
  };

 
/* Number of failure points to allocate space for initially,
   when matching.  If this number is exceeded, more space is allocated,
   so it is not a hard limit.  */

#ifndef NFAILURES
#define NFAILURES 80
#endif

/* Store NUMBER in two contiguous bytes starting at DESTINATION.  */
#define STORE_NUMBER(destination, number)				\
  { (destination)[0] = (signed char)((number) & 0377);                  \
    (destination)[1] = (signed char)((number) >> 8); }
  
/* Same as STORE_NUMBER, except increment the destination pointer to
   the byte after where the number is stored.  Watch out that values for
   DESTINATION such as p + 1 won't work, whereas p will.  */
#define STORE_NUMBER_AND_INCR(destination, number)			\
  { STORE_NUMBER(destination, number);					\
    (destination) += 2; }


/* Put into DESTINATION a number stored in two contingous bytes starting
   at SOURCE.  */
#define EXTRACT_NUMBER(destination, source)				\
  { (destination) = *(source) & 0377;					\
    (destination) += (*(signed char *)((source) + 1)) << 8; }

/* Same as EXTRACT_NUMBER, except increment the pointer for source to
   point to second byte of SOURCE.  Note that SOURCE has to be a value
   such as p, not, e.g., p + 1. */
#define EXTRACT_NUMBER_AND_INCR(destination, source)			\
  { EXTRACT_NUMBER (destination, source);				\
    (source) += 2; }


/* Specify the precise syntax of regexps for compilation.  This provides
   for compatibility for various utilities which historically have
   different, incompatible syntaxes.
   
   The argument SYNTAX is a bit-mask comprised of the various bits
   defined in regex.h.  */

int
a2_re_set_syntax (int syntax)
{
  int ret;

  ret = obscure_syntax;
  obscure_syntax = syntax;
  return ret;
}



/* Macros for a2_re_compile_pattern, which is found below these definitions.  */

#define CHAR_CLASS_MAX_LENGTH  6

/* Fetch the next character in the uncompiled pattern, translating it if
   necessary.  */
#define PATFETCH(c)							\
  {if (p == pend) goto end_of_pattern;					\
  c = * (unsigned char *) p++;						\
  if (translate) c = translate[c]; }

/* Fetch the next character in the uncompiled pattern, with no
   translation.  */
#define PATFETCH_RAW(c)							\
 {if (p == pend) goto end_of_pattern;					\
  c = * (unsigned char *) p++; }

#define PATUNFETCH p--


/* If the buffer isn't allocated when it comes in, use this.  */
#define INIT_BUF_SIZE  28

/* Make sure we have at least N more bytes of space in buffer.  */
#define GET_BUFFER_SPACE(n)						\
  {								        \
    while (long(b - bufp->buffer + (n)) >= bufp->allocated)		\
      EXTEND_BUFFER;							\
  }

/* Make sure we have one more byte of buffer space and then add CH to it.  */
#define BUFPUSH(ch)							\
  {									\
    GET_BUFFER_SPACE (1);						\
    *b++ = (char) (ch);							\
  }
  
/* Extend the buffer by twice its current size via reallociation and
   reset the pointers that pointed into the old allocation to point to
   the correct places in the new allocation.  If extending the buffer
   results in it being larger than 1 << 16, then flag memory exhausted.  */
#define EXTEND_BUFFER							\
  { char *old_buffer = bufp->buffer;					\
    if (bufp->allocated == (1L<<16)) goto too_big;			\
    bufp->allocated *= 2;						\
    if (bufp->allocated > (1L<<16)) bufp->allocated = (1L<<16);		\
    bufp->buffer = (char *) realloc (bufp->buffer, 			\
                                     (unsigned int) bufp->allocated);	\
    if (bufp->buffer == 0)						\
      goto memory_exhausted;						\
    b = (b - old_buffer) + bufp->buffer;				\
    if (fixup_jump)							\
      fixup_jump = (fixup_jump - old_buffer) + bufp->buffer;		\
    if (laststart)							\
      laststart = (laststart - old_buffer) + bufp->buffer;		\
    begalt = (begalt - old_buffer) + bufp->buffer;			\
    if (pending_exact)							\
      pending_exact = (pending_exact - old_buffer) + bufp->buffer;	\
  }

/* Set the bit for character C in a character set list.  */
#define SET_LIST_BIT(c)  (b[(c) / BYTEWIDTH] |= 1 << ((c) % BYTEWIDTH))

/* Get the next unsigned number in the uncompiled pattern.  */
#define GET_UNSIGNED_NUMBER(num) 					\
  { if (p != pend) 							\
      { 								\
        PATFETCH (c); 							\
	while (isdigit (c)) 						\
	  { 								\
	    if (num < 0) 						\
	       num = 0; 						\
            num = num * 10 + c - '0'; 					\
	    if (p == pend) 						\
	       break; 							\
	    PATFETCH (c); 						\
	  } 								\
        } 								\
  }

/* Subroutines for a2_re_compile_pattern.  */
static void store_jump (char *from, char opcode, char *to);
static void insert_jump (char op, char *from, char *to, char *current_end);
static void store_jump_n  (char *from, char opcode, char *to, unsigned n);
static void insert_jump_n (char, char *, char *, char *, unsigned);
static void insert_op_2 (char, char *, char *_end, int, int);


/* a2_re_compile_pattern takes a regular-expression string
   and converts it into a buffer full of byte commands for matching.

   PATTERN   is the address of the pattern string
   SIZE      is the length of it.
   BUFP	    is a  struct re_pattern_buffer *  which points to the info
	     on where to store the byte commands.
	     This structure contains a  char *  which points to the
	     actual space, which should have been obtained with malloc.
	     a2_re_compile_pattern may use realloc to grow the buffer space.

   The number of bytes of commands can be found out by looking in
   the `struct re_pattern_buffer' that bufp pointed to, after
   a2_re_compile_pattern returns. */

const char*
a2_re_compile_pattern (char *pattern, int size,
                       struct re_pattern_buffer *bufp)
{
  char *b = bufp->buffer;
  char *p = pattern;
  char *pend = pattern + size;
  unsigned c, c1;
  char *p1;
  unsigned char *translate = (unsigned char *) bufp->translate;

  /* Address of the count-byte of the most recently inserted `exactn'
     command.  This makes it possible to tell whether a new exact-match
     character can be added to that command or requires a new `exactn'
     command.  */
     
  char *pending_exact = 0;

  /* Address of the place where a forward-jump should go to the end of
     the containing expression.  Each alternative of an `or', except the
     last, ends with a forward-jump of this sort.  */

  char *fixup_jump = 0;

  /* Address of start of the most recently finished expression.
     This tells postfix * where to find the start of its operand.  */

  char *laststart = 0;

  /* In processing a repeat, 1 means zero matches is allowed.  */

  char zero_times_ok;

  /* In processing a repeat, 1 means many matches is allowed.  */

  char many_times_ok;

  /* Address of beginning of regexp, or inside of last \(.  */

  char *begalt = b;

  /* In processing an interval, at least this many matches must be made.  */
  int lower_bound;

  /* In processing an interval, at most this many matches can be made.  */
  int upper_bound;

  /* Place in pattern (i.e., the {) to which to go back if the interval
     is invalid.  */
  char *beg_interval = 0;
  
  /* Stack of information saved by \( and restored by \).
     Four stack elements are pushed by each \(:
       First, the value of b.
       Second, the value of fixup_jump.
       Third, the value of regnum.
       Fourth, the value of begalt.  */

  int stackb[40];
  int *stackp = stackb;
  int *stacke = stackb + 40;
  int *stackt;

  /* Counts \('s as they are encountered.  Remembered for the matching \),
     where it becomes the register number to put in the stop_memory
     command.  */

  int regnum = 1;

  bufp->fastmap_accurate = 0;

  /* Initialize the syntax table.  */
   init_syntax_once();

  if (bufp->allocated == 0)
    {
      bufp->allocated = INIT_BUF_SIZE;
      if (bufp->buffer)
	/* EXTEND_BUFFER loses when bufp->allocated is 0.  */
	bufp->buffer = (char *) realloc (bufp->buffer, INIT_BUF_SIZE);
      else
	/* Caller did not allocate a buffer.  Do it for them.  */
	bufp->buffer = (char *) malloc (INIT_BUF_SIZE);
      if (!bufp->buffer) goto memory_exhausted;
      begalt = b = bufp->buffer;
    }

  while (p != pend)
    {
      PATFETCH (c);

      switch (c)
	{
	case '$':
	  {
	    char *p1 = p;
	    /* When testing what follows the $,
	       look past the \-constructs that don't consume anything.  */
	    if (! (obscure_syntax & RE_CONTEXT_INDEP_OPS))
	      while (p1 != pend)
		{
		  if (*p1 == '\\' && p1 + 1 != pend
		      && (p1[1] == '<' || p1[1] == '>'
			  || p1[1] == '`' || p1[1] == '\''
			  || p1[1] == 'b' || p1[1] == 'B'))
		    p1 += 2;
		  else
		    break;
		}
            if (obscure_syntax & RE_TIGHT_VBAR)
	      {
		if (! (obscure_syntax & RE_CONTEXT_INDEP_OPS) && p1 != pend)
		  goto normal_char;
		/* Make operand of last vbar end before this `$'.  */
		if (fixup_jump)
		  store_jump (fixup_jump, jump, b);
		fixup_jump = 0;
		BUFPUSH (endline);
		break;
	      }
	    /* $ means succeed if at end of line, but only in special contexts.
	      If validly in the middle of a pattern, it is a normal character. */

            if ((obscure_syntax & RE_CONTEXTUAL_INVALID_OPS) && p1 != pend)
	      goto invalid_pattern;
	    if (p1 == pend || *p1 == '\n'
		|| (obscure_syntax & RE_CONTEXT_INDEP_OPS)
		|| (obscure_syntax & RE_NO_BK_PARENS
		    ? *p1 == ')'
		    : *p1 == '\\' && p1[1] == ')')
		|| (obscure_syntax & RE_NO_BK_VBAR
		    ? *p1 == '|'
		    : *p1 == '\\' && p1[1] == '|'))
	      {
		BUFPUSH (endline);
		break;
	      }
	    goto normal_char;
          }
	case '^':
	  /* ^ means succeed if at beg of line, but only if no preceding 
             pattern.  */
             
          if ((obscure_syntax & RE_CONTEXTUAL_INVALID_OPS) && laststart)
            goto invalid_pattern;
          if (laststart && p - 2 >= pattern && p[-2] != '\n'
	       && !(obscure_syntax & RE_CONTEXT_INDEP_OPS))
	    goto normal_char;
	  if (obscure_syntax & RE_TIGHT_VBAR)
	    {
	      if (p != pattern + 1
		  && ! (obscure_syntax & RE_CONTEXT_INDEP_OPS))
		goto normal_char;
	      BUFPUSH (begline);
	      begalt = b;
	    }
	  else
	    BUFPUSH (begline);
	  break;

	case '+':
	case '?':
	  if ((obscure_syntax & RE_BK_PLUS_QM)
	      || (obscure_syntax & RE_LIMITED_OPS))
	    goto normal_char;
	handle_plus:
	case '*':
	  /* If there is no previous pattern, char not special. */
	  if (!laststart)
            {
              if (obscure_syntax & RE_CONTEXTUAL_INVALID_OPS)
                goto invalid_pattern;
              else if (! (obscure_syntax & RE_CONTEXT_INDEP_OPS))
		goto normal_char;
            }
	  /* If there is a sequence of repetition chars,
	     collapse it down to just one.  */
	  zero_times_ok = 0;
	  many_times_ok = 0;
	  while (1)
	    {
	      zero_times_ok |= c != '+';
	      many_times_ok |= c != '?';
	      if (p == pend)
		break;
	      PATFETCH (c);
	      if (c == '*')
		;
	      else if (!(obscure_syntax & RE_BK_PLUS_QM)
		       && (c == '+' || c == '?'))
		;
	      else if ((obscure_syntax & RE_BK_PLUS_QM)
		       && c == '\\')
		{
		  int c1;
		  PATFETCH (c1);
		  if (!(c1 == '+' || c1 == '?'))
		    {
		      PATUNFETCH;
		      PATUNFETCH;
		      break;
		    }
		  c = c1;
		}
	      else
		{
		  PATUNFETCH;
		  break;
		}
	    }

	  /* Star, etc. applied to an empty pattern is equivalent
	     to an empty pattern.  */
	  if (!laststart)  
	    break;

	  /* Now we know whether or not zero matches is allowed
	     and also whether or not two or more matches is allowed.  */
	  if (many_times_ok)
	    {
	      /* If more than one repetition is allowed, put in at the
                 end a backward relative jump from b to before the next
                 jump we're going to put in below (which jumps from
                 laststart to after this jump).  */
              GET_BUFFER_SPACE (3);
	      store_jump (b,(char) maybe_finalize_jump, laststart - 3);
	      b += 3;  	/* Because store_jump put stuff here.  */
	    }
          /* On failure, jump from laststart to b + 3, which will be the
             end of the buffer after this jump is inserted.  */
          GET_BUFFER_SPACE (3);
	  insert_jump (on_failure_jump, laststart, b + 3, b);
	  pending_exact = 0;
	  b += 3;
	  if (!zero_times_ok)
	    {
	      /* At least one repetition is required, so insert a
                 dummy-failure before the initial on-failure-jump
                 instruction of the loop. This effects a skip over that
                 instruction the first time we hit that loop.  */
              GET_BUFFER_SPACE (6);
              insert_jump (dummy_failure_jump, laststart, laststart + 6, b);
	      b += 3;
	    }
	  break;

	case '.':
	  laststart = b;
	  BUFPUSH (anychar);
	  break;

        case '[':
          if (p == pend)
            goto invalid_pattern;
	  while (b - bufp->buffer
		 > bufp->allocated - 3 - (1 << BYTEWIDTH) / BYTEWIDTH)
	    EXTEND_BUFFER;

	  laststart = b;
	  if (*p == '^')
	    {
              BUFPUSH (charset_not); 
              p++;
            }
	  else
	    BUFPUSH (charset);
	  p1 = p;

	  BUFPUSH ((1 << BYTEWIDTH) / BYTEWIDTH);
	  /* Clear the whole map */
	  memset (b, 0, (1 << BYTEWIDTH) / BYTEWIDTH);
          
	  if ((obscure_syntax & RE_HAT_NOT_NEWLINE) && b[-2] == charset_not)
            SET_LIST_BIT ('\n');


	  /* Read in characters and ranges, setting map bits.  */
	  while (1)
	    {
	      /* Don't translate while fetching, in case it's a range bound.
		 When we set the bit for the character, we translate it.  */
	      PATFETCH_RAW (c);

	      /* If set, \ escapes characters when inside [...].  */
	      if ((obscure_syntax & RE_AWK_CLASS_HACK) && c == '\\')
	        {
	          PATFETCH(c1);
                  SET_LIST_BIT (c1);
	          continue;
	        }
              if (c == ']')
                {
                  if (p == p1 + 1)
                    {
		      /* If this is an empty bracket expression.  */
                      if ((obscure_syntax & RE_NO_EMPTY_BRACKETS) 
                          && p == pend)
                        goto invalid_pattern;
                    }
                  else 
		    /* Stop if this isn't merely a ] inside a bracket
                       expression, but rather the end of a bracket
                       expression.  */
                    break;
                }
              /* Get a range.  */
              if (p[0] == '-' && p[1] != ']')
		{
                  PATFETCH (c1);
		  /* Don't translate the range bounds while fetching them.  */
		  PATFETCH_RAW (c1);
                  
		  if ((obscure_syntax & RE_NO_EMPTY_RANGES) && c > c1)
                    goto invalid_pattern;
                    
		  if ((obscure_syntax & RE_NO_HYPHEN_RANGE_END) 
                      && c1 == '-' && *p != ']')
                    goto invalid_pattern;
                    
                  while (c <= c1)
		    {
		      /* Translate each char that's in the range.  */
		      if (translate)
			SET_LIST_BIT (translate[c]);
		      else
			SET_LIST_BIT (c);
                      c++;
		    }
                }
	      else if ((obscure_syntax & RE_CHAR_CLASSES)
			&&  c == '[' && p[0] == ':')
                {
		  /* Longest valid character class word has six characters.  */
                  char str[CHAR_CLASS_MAX_LENGTH+1];
		  PATFETCH (c);
		  c1 = 0;
		  /* If no ] at end.  */
                  if (p == pend)
                    goto invalid_pattern;
		  while (1)
		    {
		      /* Don't translate the ``character class'' characters.  */
                      PATFETCH_RAW (c);
		      if (c == ':' || c == ']' || p == pend
                          || c1 == CHAR_CLASS_MAX_LENGTH)
		        break;
		      str[c1++] = c;
		    }
		  str[c1] = '\0';
		  if (p == pend 	
		      || c == ']'	/* End of the bracket expression.  */
                      || p[0] != ']'
		      || p + 1 == pend
                      || (strcmp (str, "alpha") != 0 
                          && strcmp (str, "upper") != 0
			  && strcmp (str, "lower") != 0 
                          && strcmp (str, "digit") != 0
			  && strcmp (str, "alnum") != 0 
                          && strcmp (str, "xdigit") != 0
			  && strcmp (str, "space") != 0 
                          && strcmp (str, "print") != 0
			  && strcmp (str, "punct") != 0 
                          && strcmp (str, "graph") != 0
			  && strcmp (str, "cntrl") != 0))
		    {
		       /* Undo the ending character, the letters, and leave 
                          the leading : and [ (but set bits for them).  */
                      c1++;
		      while (c1--)    
			PATUNFETCH;
		      SET_LIST_BIT ('[');
		      SET_LIST_BIT (':');
	            }
                  else
                    {
                      /* The ] at the end of the character class.  */
                      PATFETCH (c);					
                      if (c != ']')
                        goto invalid_pattern;
		      for (c = 0; c < (1 << BYTEWIDTH); c++)
			{
			  if ((strcmp (str, "alpha") == 0  && isalpha (c))
			       || (strcmp (str, "upper") == 0  && isupper (c))
			       || (strcmp (str, "lower") == 0  && islower (c))
			       || (strcmp (str, "digit") == 0  && isdigit (c))
			       || (strcmp (str, "alnum") == 0  && isalnum (c))
			       || (strcmp (str, "xdigit") == 0  && isxdigit (c))
			       || (strcmp (str, "space") == 0  && isspace (c))
			       || (strcmp (str, "print") == 0  && isprint (c))
			       || (strcmp (str, "punct") == 0  && ispunct (c))
			       || (strcmp (str, "graph") == 0  && isgraph (c))
			       || (strcmp (str, "cntrl") == 0  && iscntrl (c)))
			    SET_LIST_BIT (c);
			}
		    }
                }
              else if (translate)
		SET_LIST_BIT (translate[c]);
	      else
                SET_LIST_BIT (c);
	    }

          /* Discard any character set/class bitmap bytes that are all
             0 at the end of the map. Decrement the map-length byte too.  */
          while ((int) b[-1] > 0 && b[b[-1] - 1] == 0) 
            b[-1]--; 
          b += b[-1];
          break;

	case '(':
	  if (! (obscure_syntax & RE_NO_BK_PARENS))
	    goto normal_char;
	  else
	    goto handle_open;

	case ')':
	  if (! (obscure_syntax & RE_NO_BK_PARENS))
	    goto normal_char;
	  else
	    goto handle_close;

        case '\n':
	  if (! (obscure_syntax & RE_NEWLINE_OR))
	    goto normal_char;
	  else
	    goto handle_bar;

	case '|':
	  if ((obscure_syntax & RE_CONTEXTUAL_INVALID_OPS)
              && (! laststart  ||  p == pend))
	    goto invalid_pattern;
          else if (! (obscure_syntax & RE_NO_BK_VBAR))
	    goto normal_char;
	  else
	    goto handle_bar;

	case '{':
           if (! ((obscure_syntax & RE_NO_BK_CURLY_BRACES)
                  && (obscure_syntax & RE_INTERVALS)))
             goto normal_char;
           else
             goto handle_interval;
             
        case '\\':
	  if (p == pend) goto invalid_pattern;
	  PATFETCH_RAW (c);
	  switch (c)
	    {
	    case '(':
	      if (obscure_syntax & RE_NO_BK_PARENS)
		goto normal_backsl;
	    handle_open:
	      if (stackp == stacke) goto nesting_too_deep;

              /* Laststart should point to the start_memory that we are about
                 to push (unless the pattern has RE_NREGS or more ('s).  */
              *stackp++ = b - bufp->buffer;    
	      if (regnum >= RE_NREGS) goto too_many_paren;
              BUFPUSH (start_memory);
              BUFPUSH (regnum);
	      *stackp++ = fixup_jump ? fixup_jump - bufp->buffer + 1 : 0;
	      *stackp++ = regnum++;
	      *stackp++ = begalt - bufp->buffer;
	      fixup_jump = 0;
	      laststart = 0;
	      begalt = b;
	      break;

	    case ')':
	      if (obscure_syntax & RE_NO_BK_PARENS)
		goto normal_backsl;
	    handle_close:
	      if (stackp == stackb) goto unmatched_close;
	      begalt = *--stackp + bufp->buffer;
	      if (fixup_jump)
		store_jump (fixup_jump, jump, b);
	      if (stackp[-1] < RE_NREGS)
		{
		  BUFPUSH (stop_memory);
		  BUFPUSH (stackp[-1]);
		}
	      stackp -= 2;
              fixup_jump = *stackp ? *stackp + bufp->buffer - 1 : 0;
              laststart = *--stackp + bufp->buffer;
	      break;

	    case '|':
              if ((obscure_syntax & RE_LIMITED_OPS)
	          || (obscure_syntax & RE_NO_BK_VBAR))
		goto normal_backsl;
	    handle_bar:
              if (obscure_syntax & RE_LIMITED_OPS)
                goto normal_char;
	      /* Insert before the previous alternative a jump which
                 jumps to this alternative if the former fails.  */
              GET_BUFFER_SPACE (6);
              insert_jump (on_failure_jump, begalt, b + 6, b);
	      pending_exact = 0;
	      b += 3;
	      /* The alternative before the previous alternative has a
                 jump after it which gets executed if it gets matched.
                 Adjust that jump so it will jump to the previous
                 alternative's analogous jump (put in below, which in
                 turn will jump to the next (if any) alternative's such
                 jump, etc.).  The last such jump jumps to the correct
                 final destination.  */
              if (fixup_jump)
		store_jump (fixup_jump, jump, b);
                
	      /* Leave space for a jump after previous alternative---to be 
                 filled in later.  */
              fixup_jump = b;
              b += 3;

              laststart = 0;
	      begalt = b;
	      break;

            case '{': 
              if (! (obscure_syntax & RE_INTERVALS)
		  /* Let \{ be a literal.  */
                  || ((obscure_syntax & RE_INTERVALS)
                      && (obscure_syntax & RE_NO_BK_CURLY_BRACES))
		  /* If it's the string "\{".  */
		  || (p - 2 == pattern  &&  p == pend))
                goto normal_backsl;
            handle_interval:
	      beg_interval = p - 1;		/* The {.  */
              /* If there is no previous pattern, this isn't an interval.  */
	      if (!laststart)
	        {
                  if (obscure_syntax & RE_CONTEXTUAL_INVALID_OPS)
		    goto invalid_pattern;
                  else
                    goto normal_backsl;
                }
              /* It also isn't an interval if not preceded by an re
                 matching a single character or subexpression, or if
                 the current type of intervals can't handle back
                 references and the previous thing is a back reference.  */
              if (! (*laststart == anychar
		     || *laststart == charset
		     || *laststart == charset_not
		     || *laststart == start_memory
		     || (*laststart == exactn  &&  laststart[1] == 1)
		     || (! (obscure_syntax & RE_NO_BK_REFS)
                         && *laststart == duplicate)))
                {
                  if (obscure_syntax & RE_NO_BK_CURLY_BRACES)
                    goto normal_char;
                    
		  /* Posix extended syntax is handled in previous
                     statement; this is for Posix basic syntax.  */
                  if (obscure_syntax & RE_INTERVALS)
                    goto invalid_pattern;
                    
                  goto normal_backsl;
		}
              lower_bound = -1;			/* So can see if are set.  */
	      upper_bound = -1;
              GET_UNSIGNED_NUMBER (lower_bound);
	      if (c == ',')
		{
		  GET_UNSIGNED_NUMBER (upper_bound);
		  if (upper_bound < 0)
		    upper_bound = RE_DUP_MAX;
		}
	      if (upper_bound < 0)
		upper_bound = lower_bound;
              if (! (obscure_syntax & RE_NO_BK_CURLY_BRACES)) 
                {
                  if (c != '\\')
                    goto invalid_pattern;
                  PATFETCH (c);
                }
	      if (c != '}' || lower_bound < 0 || upper_bound > RE_DUP_MAX
		  || lower_bound > upper_bound 
                  || ((obscure_syntax & RE_NO_BK_CURLY_BRACES) 
		      && p != pend  && *p == '{')) 
	        {
		  if (obscure_syntax & RE_NO_BK_CURLY_BRACES)
                    goto unfetch_interval;
                  else
                    goto invalid_pattern;
		}

	      /* If upper_bound is zero, don't want to succeed at all; 
 		 jump from laststart to b + 3, which will be the end of
                 the buffer after this jump is inserted.  */
                 
               if (upper_bound == 0)
                 {
                   GET_BUFFER_SPACE (3);
                   insert_jump (jump, laststart, b + 3, b);
                   b += 3;
                 }

               /* Otherwise, after lower_bound number of succeeds, jump
                  to after the jump_n which will be inserted at the end
                  of the buffer, and insert that jump_n.  */
               else 
		 { /* Set to 5 if only one repetition is allowed and
	              hence no jump_n is inserted at the current end of
                      the buffer; then only space for the succeed_n is
                      needed.  Otherwise, need space for both the
                      succeed_n and the jump_n.  */
                      
                   unsigned slots_needed = upper_bound == 1 ? 5 : 10;
                     
                   GET_BUFFER_SPACE (slots_needed);
                   /* Initialize the succeed_n to n, even though it will
                      be set by its attendant set_number_at, because
                      a2_re_compile_fastmap will need to know it.  Jump to
                      what the end of buffer will be after inserting
                      this succeed_n and possibly appending a jump_n.  */
                   insert_jump_n (succeed_n, laststart, b + slots_needed, 
		                  b, lower_bound);
                   b += 5; 	/* Just increment for the succeed_n here.  */

		  /* More than one repetition is allowed, so put in at
		     the end of the buffer a backward jump from b to the
                     succeed_n we put in above.  By the time we've gotten
                     to this jump when matching, we'll have matched once
                     already, so jump back only upper_bound - 1 times.  */

                   if (upper_bound > 1)
                     {
                       store_jump_n (b, jump_n, laststart, upper_bound - 1);
                       b += 5;
                       /* When hit this when matching, reset the
                          preceding jump_n's n to upper_bound - 1.  */
                       BUFPUSH (set_number_at);
		       GET_BUFFER_SPACE (2);
                       STORE_NUMBER_AND_INCR (b, -5);
                       STORE_NUMBER_AND_INCR (b, upper_bound - 1);
                     }
		   /* When hit this when matching, set the succeed_n's n.  */
                   GET_BUFFER_SPACE (5);
		   insert_op_2 (set_number_at, laststart, b, 5, lower_bound);
                   b += 5;
                 }
              pending_exact = 0;
	      beg_interval = 0;
              break;


            unfetch_interval:
	      /* If an invalid interval, match the characters as literals.  */
	       if (beg_interval)
                 p = beg_interval;
  	       else
                 {
                   fprintf (stderr, 
		      "regex: no interval beginning to which to backtrack.\n");
		   exit (1);
                 }
                 
               beg_interval = 0;
               PATFETCH (c);		/* normal_char expects char in `c'.  */
	       goto normal_char;
/**	       break;
 ** previous line commented out because:
 ** regex.cc, line 1046: warning:  statement after goto not reached
 **/

	    case 'w':
	      laststart = b;
	      BUFPUSH (wordchar);
	      break;

	    case 'W':
	      laststart = b;
	      BUFPUSH (notwordchar);
	      break;

	    case '<':
	      BUFPUSH (wordbeg);
	      break;

	    case '>':
	      BUFPUSH (wordend);
	      break;

	    case 'b':
	      BUFPUSH (wordbound);
	      break;

	    case 'B':
	      BUFPUSH (notwordbound);
	      break;

	    case '`':
	      BUFPUSH (begbuf);
	      break;

	    case '\'':
	      BUFPUSH (endbuf);
	      break;

	    case '1':
	    case '2':
	    case '3':
	    case '4':
	    case '5':
	    case '6':
	    case '7':
	    case '8':
	    case '9':
	      if (obscure_syntax & RE_NO_BK_REFS)
                goto normal_char;
              c1 = c - '0';
	      if (int(c1) >= regnum)
		{
  		  if (obscure_syntax & RE_NO_EMPTY_BK_REF)
                    goto invalid_pattern;
                  else
                    goto normal_char;
                }
              /* Can't back reference to a subexpression if inside of it.  */
              for (stackt = stackp - 2;  stackt > stackb;  stackt -= 4)
 		if (*stackt == int(c1))
		  goto normal_char;
	      laststart = b;
	      BUFPUSH (duplicate);
	      BUFPUSH (c1);
	      break;

	    case '+':
	    case '?':
	      if (obscure_syntax & RE_BK_PLUS_QM)
		goto handle_plus;
	      else
                goto normal_backsl;

            default:
	    normal_backsl:
	      /* You might think it would be useful for \ to mean
		 not to translate; but if we don't translate it
		 it will never match anything.  */
	      if (translate) c = translate[c];
	      goto normal_char;
	    }
	  break;

	default:
	normal_char:		/* Expects the character in `c'.  */
	  if (!pending_exact || pending_exact + *pending_exact + 1 != b
	      || *pending_exact == 0177 || *p == '*' || *p == '^'
	      || ((obscure_syntax & RE_BK_PLUS_QM)
		  ? *p == '\\' && (p[1] == '+' || p[1] == '?')
		  : (*p == '+' || *p == '?'))
	      || ((obscure_syntax & RE_INTERVALS) 
                  && ((obscure_syntax & RE_NO_BK_CURLY_BRACES)
		      ? *p == '{'
                      : (p[0] == '\\' && p[1] == '{'))))
	    {
	      laststart = b;
	      BUFPUSH (exactn);
	      pending_exact = b;
	      BUFPUSH (0);
	    }
	  BUFPUSH (c);
	  (*pending_exact)++;
	}
    }

  if (fixup_jump)
    store_jump (fixup_jump, jump, b);

  if (stackp != stackb) goto unmatched_open;

  bufp->used = b - bufp->buffer;
  return 0;

 invalid_pattern:
  static const char* invpatt = "Invalid regular expression";
  return invpatt;

 unmatched_open:
  static const char* unmopen = "Unmatched \\(";
  return unmopen;

 unmatched_close:
  static const char* unmclose = "Unmatched \\)";
  return unmclose;

 end_of_pattern:
  static const char* endpatt = "Premature end of regular expression";
  return endpatt;

 nesting_too_deep:
  static const char* toodeep = "Nesting too deep";
  return toodeep;

 too_many_paren:
  static const char* toomanyparen = "Too many parentheses";
  return toomanyparen;

 too_big:
  static const char* toobig = "Regular expression too big";
  return toobig;

 memory_exhausted:
  static const char* memexh = "Memory exhausted";
  return memexh;
}


/* Store a jump of the form <OPCODE> <relative address>.
   Store in the location FROM a jump operation to jump to relative
   address FROM - TO.  OPCODE is the opcode to store.  */

static void
store_jump (char *from, char opcode, char *to)
{
  from[0] = opcode;
  STORE_NUMBER(from + 1, to - (from + 3));
}


/* Open up space before char FROM, and insert there a jump to TO.
   CURRENT_END gives the end of the storage not in use, so we know 
   how much data to copy up. OP is the opcode of the jump to insert.

   If you call this function, you must zero out pending_exact.  */

static void
insert_jump (char op, char *from, char *to, char *current_end)
{
  char *pfrom = current_end;		/* Copy from here...  */
  char *pto = current_end + 3;		/* ...to here.  */

  while (pfrom != from)			       
    *--pto = *--pfrom;
  store_jump (from, op, to);
}


/* Store a jump of the form <opcode> <relative address> <n> .

   Store in the location FROM a jump operation to jump to relative
   address FROM - TO.  OPCODE is the opcode to store, N is a number the
   jump uses, say, to decide how many times to jump.
   
   If you call this function, you must zero out pending_exact.  */

static void
store_jump_n (char *from, char opcode, char *to, unsigned n)
{
  from[0] = opcode;
  STORE_NUMBER (from + 1, to - (from + 3));
  STORE_NUMBER (from + 3, n);
}


/* Similar to insert_jump, but handles a jump which needs an extra
   number to handle minimum and maximum cases.  Open up space at
   location FROM, and insert there a jump to TO.  CURRENT_END gives the
   end of the storage in use, so we know how much data to copy up. OP is
   the opcode of the jump to insert.

   If you call this function, you must zero out pending_exact.  */

static void
insert_jump_n (char op, char *from, char *to, char *current_end, unsigned n)
{
  char *pfrom = current_end;		/* Copy from here...  */
  char *pto = current_end + 5;		/* ...to here.  */

  while (pfrom != from)			       
    *--pto = *--pfrom;
  store_jump_n (from, op, to, n);
}


/* Open up space at location THERE, and insert operation OP followed by
   NUM_1 and NUM_2.  CURRENT_END gives the end of the storage in use, so
   we know how much data to copy up.

   If you call this function, you must zero out pending_exact.  */

static void
insert_op_2 (char op, char *there, char *current_end, int num_1, int num_2)
{
  char *pfrom = current_end;		/* Copy from here...  */
  char *pto = current_end + 5;		/* ...to here.  */

  while (pfrom != there)			       
    *--pto = *--pfrom;
  
  there[0] = op;
  STORE_NUMBER (there + 1, num_1);
  STORE_NUMBER (there + 3, num_2);
}



/* Given a pattern, compute a fastmap from it.  The fastmap records
   which of the (1 << BYTEWIDTH) possible characters can start a string
   that matches the pattern.  This fastmap is used by a2_re_search to skip
   quickly over totally implausible text.

   The caller must supply the address of a (1 << BYTEWIDTH)-byte data 
   area as bufp->fastmap.
   The other components of bufp describe the pattern to be used.  */

void
a2_re_compile_fastmap (struct re_pattern_buffer *bufp)
{
  unsigned char *pattern = (unsigned char *) bufp->buffer;
  long size = bufp->used;
  char *fastmap = bufp->fastmap;
  unsigned char *p = pattern;
  unsigned char *pend = pattern + size;
  int j, k;
  unsigned char *translate = (unsigned char *) bufp->translate;

  unsigned char *stackb[NFAILURES];
  unsigned char **stackp = stackb;

  unsigned is_a_succeed_n;

  memset (fastmap, 0, (1 << BYTEWIDTH));
  bufp->fastmap_accurate = 1;
  bufp->can_be_null = 0;
      
  while (p)
    {
      is_a_succeed_n = 0;
      if (p == pend)
	{
	  bufp->can_be_null = 1;
	  break;
	}
#ifdef SWITCH_ENUM_BUG
      switch ((int) ((enum regexpcode) *p++))
#else
      switch ((enum regexpcode) *p++)
#endif
	{
	case exactn:
	  if (translate)
	    fastmap[translate[p[1]]] = 1;
	  else
	    fastmap[p[1]] = 1;
	  break;

        case begline:
        case before_dot:
	case at_dot:
	case after_dot:
	case begbuf:
	case endbuf:
	case wordbound:
	case notwordbound:
	case wordbeg:
	case wordend:
          continue;

	case endline:
	  if (translate)
	    fastmap[translate[int('\n')]] = 1;
	  else
	    fastmap[int('\n')] = 1;
            
	  if (bufp->can_be_null != 1)
	    bufp->can_be_null = 2;
	  break;

	case jump_n:
        case finalize_jump:
	case maybe_finalize_jump:
	case jump:
	case dummy_failure_jump:
          EXTRACT_NUMBER_AND_INCR (j, p);
	  p += j;	
	  if (j > 0)
	    continue;
          /* Jump backward reached implies we just went through
	     the body of a loop and matched nothing.
	     Opcode jumped to should be an on_failure_jump.
	     Just treat it like an ordinary jump.
	     For a * loop, it has pushed its failure point already;
	     If so, discard that as redundant.  */

          if ((enum regexpcode) *p != on_failure_jump
	      && (enum regexpcode) *p != succeed_n)
	    continue;
          p++;
          EXTRACT_NUMBER_AND_INCR (j, p);
          p += j;		
          if (stackp != stackb && *stackp == p)
            stackp--;
          continue;
	  
        case on_failure_jump:
	handle_on_failure_jump:
          EXTRACT_NUMBER_AND_INCR (j, p);
          *++stackp = p + j;
	  if (is_a_succeed_n)
            EXTRACT_NUMBER_AND_INCR (k, p);	/* Skip the n.  */
	  continue;

	case succeed_n:
	  is_a_succeed_n = 1;
          /* Get to the number of times to succeed.  */
          p += 2;		
	  /* Increment p past the n for when k != 0.  */
          EXTRACT_NUMBER_AND_INCR (k, p);
          if (k == 0)
	    {
              p -= 4;
              goto handle_on_failure_jump;
            }
          continue;
          
	case set_number_at:
          p += 4;
          continue;

        case start_memory:
	case stop_memory:
	  p++;
	  continue;

	case duplicate:
	  bufp->can_be_null = 1;
	  fastmap[int('\n')] = 1;
	case anychar:
	  for (j = 0; j < (1 << BYTEWIDTH); j++)
	    if (j != '\n')
	      fastmap[j] = 1;
	  if (bufp->can_be_null)
	    return;
	  /* Don't return; check the alternative paths
	     so we can set can_be_null if appropriate.  */
	  break;

	case wordchar:
	  for (j = 0; j < (1 << BYTEWIDTH); j++)
	    if (SYNTAX (j) == Sword)
	      fastmap[j] = 1;
	  break;

	case notwordchar:
	  for (j = 0; j < (1 << BYTEWIDTH); j++)
	    if (SYNTAX (j) != Sword)
	      fastmap[j] = 1;
	  break;

	case charset:
	  for (j = *p++ * BYTEWIDTH - 1; j >= 0; j--)
	    if (p[j / BYTEWIDTH] & (1 << (j % BYTEWIDTH)))
	      {
		if (translate)
		  fastmap[translate[j]] = 1;
		else
		  fastmap[j] = 1;
	      }
	  break;

	case charset_not:
	  /* Chars beyond end of map must be allowed */
	  for (j = *p * BYTEWIDTH; j < (1 << BYTEWIDTH); j++)
	    if (translate)
	      fastmap[translate[j]] = 1;
	    else
	      fastmap[j] = 1;

	  for (j = *p++ * BYTEWIDTH - 1; j >= 0; j--)
	    if (!(p[j / BYTEWIDTH] & (1 << (j % BYTEWIDTH))))
	      {
		if (translate)
		  fastmap[translate[j]] = 1;
		else
		  fastmap[j] = 1;
	      }
	  break;
/** the following three cases were added to silence the compiler
 ** regex.cc, line 1330: warning: switch ( enum regexpcode ) with  28 cases ( 31 enumerators)
 **/
	case syntaxspec:
	case notsyntaxspec:
	case unused:
	  break;
	  
	}

      /* Get here means we have successfully found the possible starting
         characters of one path of the pattern.  We need not follow this
         path any farther.  Instead, look at the next alternative
         remembered in the stack.  */
   if (stackp != stackb)
	p = *stackp--;
      else
	break;
    }
}



/* Like a2_re_search_2, below, but only one string is specified, and
   doesn't let you say where to stop matching. */

int
a2_re_search (struct re_pattern_buffer *pbufp,
	   char *string,
	   int size,
	   int startpos,
	   int range,
	   struct re_registers *regs)
{
  return a2_re_search_2 (pbufp, static_cast<char *>(0), 0, string, size, startpos, range, 
		      regs, size);
}


/* Using the compiled pattern in PBUFP->buffer, first tries to match the
   virtual concatenation of STRING1 and STRING2, starting first at index
   STARTPOS, then at STARTPOS + 1, and so on.  RANGE is the number of
   places to try before giving up.  If RANGE is negative, it searches
   backwards, i.e., the starting positions tried are STARTPOS, STARTPOS
   - 1, etc.  STRING1 and STRING2 are of SIZE1 and SIZE2, respectively.
   In REGS, return the indices of the virtual concatenation of STRING1
   and STRING2 that matched the entire PBUFP->buffer and its contained
   subexpressions.  Do not consider matching one past the index MSTOP in
   the virtual concatenation of STRING1 and STRING2.

   The value returned is the position in the strings at which the match
   was found, or -1 if no match was found, or -2 if error (such as
   failure stack overflow).  */

int
a2_re_search_2 (struct re_pattern_buffer *pbufp,
	     char *string1, int size1,
	     char *string2, int size2,
	     int startpos,
	     int range,
	     struct re_registers *regs,
	     int mstop)
{
  char *fastmap = pbufp->fastmap;
  unsigned char *translate = (unsigned char *) pbufp->translate;
  int total_size = size1 + size2;
  int endpos = startpos + range;
  int val;

  /* Check for out-of-range starting position.  */
  if (startpos < 0  ||  startpos > total_size)
    return -1;
    
  /* Fix up range if it would eventually take startpos outside of the
     virtual concatenation of string1 and string2.  */
  if (endpos < -1)
    range = -1 - startpos;
  else if (endpos > total_size)
    range = total_size - startpos;

  /* Update the fastmap now if not correct already.  */
  if (fastmap && !pbufp->fastmap_accurate)
    a2_re_compile_fastmap (pbufp);
  
  /* If the search isn't to be a backwards one, don't waste time in a
     long search for a pattern that says it is anchored.  */
  if (pbufp->used > 0 && (enum regexpcode) pbufp->buffer[0] == begbuf
      && range > 0)
    {
      if (startpos > 0)
	return -1;
      else
	range = 1;
    }

  while (1)
    { 
      /* If a fastmap is supplied, skip quickly over characters that
         cannot possibly be the start of a match.  Note, however, that
         if the pattern can possibly match the null string, we must
         test it at each starting point so that we take the first null
         string we get.  */

      if (fastmap && startpos < total_size && pbufp->can_be_null != 1)
	{
	  if (range > 0)	/* Searching forwards.  */
	    {
	      int lim = 0;
	      unsigned char *p;
	      int irange = range;
	      if (startpos < size1 && startpos + range >= size1)
		lim = range - (size1 - startpos);

	      p = ((unsigned char *)
		   &(startpos >= size1 ? string2 - size1 : string1)[startpos]);

              while (range > lim && !fastmap[translate 
                                             ? translate[*p++]
                                             : *p++])
		    range--;
	      startpos += irange - range;
	    }
	  else				/* Searching backwards.  */
	    {
	      unsigned char c;

              if (string1 == 0 || startpos >= size1)
		c = string2[startpos - size1];
	      else 
		c = string1[startpos];

              c &= 0xff;
	      if (translate ? !fastmap[translate[c]] : !fastmap[c])
		goto advance;
	    }
	}

      if (range >= 0 && startpos == total_size
	  && fastmap && pbufp->can_be_null == 0)
	return -1;

      val = a2_re_match_2 (pbufp, string1, size1, string2, size2, startpos,
			regs, mstop);
      if (val >= 0)
	return startpos;
      if (val == -2)
	return -2;

    advance:
      if (!range) 
        break;
      else if (range > 0) 
        {
          range--; 
          startpos++;
        }
      else
        {
          range++; 
          startpos--;
        }
    }
  return -1;
}



int
a2_re_match (struct re_pattern_buffer *pbufp,
	  char *string,
	  int size,
	  int pos,
	  struct re_registers *regs)
{
  return a2_re_match_2 (pbufp, static_cast<char *>(0), 0, string, size, pos, regs, size); 
}


/* The following are used for a2_re_match_2, defined below:  */

/* Routine used by a2_re_match_2.  */
static int bcmp_translate (char *, char *, int, unsigned char *);


/* Structure and accessing macros used in a2_re_match_2:  */

struct register_info
{
  unsigned is_active : 1;
  unsigned matched_something : 1;
};

#define IS_ACTIVE(R)  ((R).is_active)
#define MATCHED_SOMETHING(R)  ((R).matched_something)


/* Macros used by a2_re_match_2:  */


/* I.e., regstart, regend, and reg_info.  */

#define NUM_REG_ITEMS  3

/* We push at most this many things on the stack whenever we
   fail.  The `+ 2' refers to PATTERN_PLACE and STRING_PLACE, which are
   arguments to the PUSH_FAILURE_POINT macro.  */

#define MAX_NUM_FAILURE_ITEMS   (RE_NREGS * NUM_REG_ITEMS + 2)


/* We push this many things on the stack whenever we fail.  */

#define NUM_FAILURE_ITEMS  (last_used_reg * NUM_REG_ITEMS + 2)


/* This pushes most of the information about the current state we will want
   if we ever fail back to it.  */

#define PUSH_FAILURE_POINT(pattern_place, string_place)			\
  {									\
    long last_used_reg, this_reg;					\
									\
    /* Find out how many registers are active or have been matched.	\
       (Aside from register zero, which is only set at the end.)  */	\
    for (last_used_reg = RE_NREGS - 1; last_used_reg > 0; last_used_reg--)\
      if (regstart[last_used_reg] != (unsigned char *) -1)		\
        break;								\
									\
    if (stacke - stackp < NUM_FAILURE_ITEMS)				\
      {									\
	unsigned char **stackx;						\
	unsigned int len = stacke - stackb;				\
	if (int(len) > re_max_failures * MAX_NUM_FAILURE_ITEMS)		\
	  return -2;							\
									\
        /* Roughly double the size of the stack.  */			\
stackx = (unsigned char **) alloca (2 * len * sizeof (unsigned char *));\
	/* Only copy what is in use.  */				\
        char* cpb=(char *)stackb;char* cpx=(char *)stackx; /* IBM! */	\
        memcpy (cpx, cpb, len * sizeof (char *));			\
	stackp = stackx + (stackp - stackb);				\
	stackb = stackx;						\
	stacke = stackb + 2 * len;					\
      }									\
									\
    /* Now push the info for each of those registers.  */		\
    for (this_reg = 1; this_reg <= last_used_reg; this_reg++)		\
      {									\
        *stackp++ = regstart[this_reg];					\
        *stackp++ = regend[this_reg];					\
        *stackp++ = (unsigned char *) &reg_info[this_reg];		\
      }									\
									\
    /* Push how many registers we saved.  */				\
    *stackp++ = (unsigned char *) last_used_reg;			\
									\
    *stackp++ = pattern_place;                                          \
    *stackp++ = string_place;                                           \
  }
  

/* This pops what PUSH_FAILURE_POINT pushes.  */

#define POP_FAILURE_POINT()						\
  {									\
    long temp;								\
    stackp -= 2;		/* Remove failure points.  */		\
    temp = (long) *--stackp;	/* How many regs pushed.  */	        \
    temp *= NUM_REG_ITEMS;	/* How much to take off the stack.  */	\
    stackp -= temp; 		/* Remove the register info.  */	\
  }


#define MATCHING_IN_FIRST_STRING  (dend == end_match_1)

/* Is true if there is a first string and if PTR is pointing anywhere
   inside it or just past the end.  */
   
#define IS_IN_FIRST_STRING(ptr) 					\
	(size1 && string1 <= (ptr) && (ptr) <= string1 + size1)

/* Call before fetching a character with *d.  This switches over to
   string2 if necessary.  */

#define PREFETCH							\
 while (d == dend)						    	\
  {									\
    /* end of string2 => fail.  */					\
    if (dend == end_match_2) 						\
      goto fail;							\
    /* end of string1 => advance to string2.  */ 			\
    d = string2;						        \
    dend = end_match_2;							\
  }


/* Call this when have matched something; it sets `matched' flags for the
   registers corresponding to the subexpressions of which we currently
   are inside.  */
#define SET_REGS_MATCHED 						\
  { unsigned this_reg; 							\
    for (this_reg = 0; this_reg < RE_NREGS; this_reg++) 		\
      { 								\
        if (IS_ACTIVE(reg_info[this_reg]))				\
          MATCHED_SOMETHING(reg_info[this_reg]) = 1;			\
        else								\
          MATCHED_SOMETHING(reg_info[this_reg]) = 0;			\
      } 								\
  }

/* Test if at very beginning or at very end of the virtual concatenation
   of string1 and string2.  If there is only one string, we've put it in
   string2.  */

#define AT_STRINGS_BEG  (d == (size1 ? string1 : string2)  ||  !size2)
#define AT_STRINGS_END  (d == end2)	

#define AT_WORD_BOUNDARY						\
  (AT_STRINGS_BEG || AT_STRINGS_END || IS_A_LETTER (d - 1) != IS_A_LETTER (d))

/* We have two special cases to check for: 
     1) if we're past the end of string1, we have to look at the first
        character in string2;
     2) if we're before the beginning of string2, we have to look at the
        last character in string1; we assume there is a string1, so use
        this in conjunction with AT_STRINGS_BEG.  */
#define IS_A_LETTER(d)							\
  (SYNTAX ((d) == end1 ? *string2 : (d) == string2 - 1 ? *(end1 - 1) : *(d))\
   == Sword)


/* Match the pattern described by PBUFP against the virtual
   concatenation of STRING1 and STRING2, which are of SIZE1 and SIZE2,
   respectively.  Start the match at index POS in the virtual
   concatenation of STRING1 and STRING2.  In REGS, return the indices of
   the virtual concatenation of STRING1 and STRING2 that matched the
   entire PBUFP->buffer and its contained subexpressions.  Do not
   consider matching one past the index MSTOP in the virtual
   concatenation of STRING1 and STRING2.

   If pbufp->fastmap is nonzero, then it had better be up to date.

   The reason that the data to match are specified as two components
   which are to be regarded as concatenated is so this function can be
   used directly on the contents of an Emacs buffer.

   -1 is returned if there is no match.  -2 is returned if there is an
   error (such as match stack overflow).  Otherwise the value is the
   length of the substring which was matched.  */

int
real_a2_re_match_2 (struct re_pattern_buffer *pbufp,
	    char *string1_arg, int size1,
	    char *string2_arg, int size2,
	    int pos,
	    struct re_registers *regs,
	    int mstop, cregex_allocator &)
{
  unsigned char *p = (unsigned char *) pbufp->buffer;

  /* Pointer to beyond end of buffer.  */
  unsigned char *pend = p + pbufp->used;

  unsigned char *string1 = (unsigned char *) string1_arg;
  unsigned char *string2 = (unsigned char *) string2_arg;
  unsigned char *end1;		/* Just past end of first string.  */
  unsigned char *end2;		/* Just past end of second string.  */

  /* Pointers into string1 and string2, just past the last characters in
     each to consider matching.  */
  unsigned char *end_match_1, *end_match_2;

  unsigned char *d, *dend;
  int mcnt;			/* Multipurpose.  */
  unsigned char *translate = (unsigned char *) pbufp->translate;
  unsigned is_a_jump_n = 0;

 /* Failure point stack.  Each place that can handle a failure further
    down the line pushes a failure point on this stack.  It consists of
    restart, regend, and reg_info for all registers corresponding to the
    subexpressions we're currently inside, plus the number of such
    registers, and, finally, two char *'s.  The first char * is where to
    resume scanning the pattern; the second one is where to resume
    scanning the strings.  If the latter is zero, the failure point is a
    ``dummy''; if a failure happens and the failure point is a dummy, it
    gets discarded and the next next one is tried.  */

  unsigned char *initial_stack[MAX_NUM_FAILURE_ITEMS * NFAILURES];
  unsigned char **stackb = initial_stack;
  unsigned char **stackp = stackb;
  unsigned char **stacke = &stackb[MAX_NUM_FAILURE_ITEMS * NFAILURES];


  /* Information on the contents of registers. These are pointers into
     the input strings; they record just what was matched (on this
     attempt) by a subexpression part of the pattern, that is, the
     regnum-th regstart pointer points to where in the pattern we began
     matching and the regnum-th regend points to right after where we
     stopped matching the regnum-th subexpression.  (The zeroth register
     keeps track of what the whole pattern matches.)  */
     
  unsigned char *regstart[RE_NREGS];
  unsigned char *regend[RE_NREGS];

  /* The is_active field of reg_info helps us keep track of which (possibly
     nested) subexpressions we are currently in. The matched_something
     field of reg_info[reg_num] helps us tell whether or not we have
     matched any of the pattern so far this time through the reg_num-th
     subexpression.  These two fields get reset each time through any
     loop their register is in.  */

  struct register_info reg_info[RE_NREGS];


  /* The following record the register info as found in the above
     variables when we find a match better than any we've seen before. 
     This happens as we backtrack through the failure points, which in
     turn happens only if we have not yet matched the entire string.  */

  unsigned best_regs_set = 0;
  unsigned char *best_regstart[RE_NREGS];
  unsigned char *best_regend[RE_NREGS];

  /* Initialize subexpression text positions to -1 to mark ones that no
     \( or ( and \) or ) has been seen for. Also set all registers to
     inactive and mark them as not having matched anything or ever
     failed.  */
  for (mcnt = 0; mcnt < RE_NREGS; mcnt++)
    {
      regstart[mcnt] = regend[mcnt] = (unsigned char *) -1;
      IS_ACTIVE (reg_info[mcnt]) = 0;
      MATCHED_SOMETHING (reg_info[mcnt]) = 0;
    }
  
  if (regs)
    for (mcnt = 0; mcnt < RE_NREGS; mcnt++)
      regs->start[mcnt] = regs->end[mcnt] = -1;

  /* Set up pointers to ends of strings.
     Don't allow the second string to be empty unless both are empty.  */
  if (size2 == 0)
    {
      string2 = string1;
      size2 = size1;
      string1 = 0;
      size1 = 0;
    }
  end1 = string1 + size1;
  end2 = string2 + size2;

  /* Compute where to stop matching, within the two strings.  */
  if (mstop <= size1)
    {
      end_match_1 = string1 + mstop;
      end_match_2 = string2;
    }
  else
    {
      end_match_1 = end1;
      end_match_2 = string2 + mstop - size1;
    }

  /* `p' scans through the pattern as `d' scans through the data. `dend'
     is the end of the input string that `d' points within. `d' is
     advanced into the following input string whenever necessary, but
     this happens before fetching; therefore, at the beginning of the
     loop, `d' can be pointing at the end of a string, but it cannot
     equal string2.  */

  if (size1 != 0 && pos <= size1)
    d = string1 + pos, dend = end_match_1;
  else
    d = string2 + pos - size1, dend = end_match_2;


  /* This loops over pattern commands.  It exits by returning from the
     function if match is complete, or it drops through if match fails
     at this starting point in the input data.  */

  while (1)
    {
      is_a_jump_n = 0;
      /* End of pattern means we might have succeeded.  */
      if (p == pend)
	{
	  /* If not end of string, try backtracking.  Otherwise done.  */
          if (d != end_match_2)
	    {
              if (stackp != stackb)
                {
                  /* More failure points to try.  */

                  unsigned in_same_string = 
        	          	IS_IN_FIRST_STRING (best_regend[0]) 
	        	        == MATCHING_IN_FIRST_STRING;

                  /* If exceeds best match so far, save it.  */
                  if (! best_regs_set
                      || (in_same_string && d > best_regend[0])
                      || (! in_same_string && ! MATCHING_IN_FIRST_STRING))
                    {
                      best_regs_set = 1;
		      best_regstart[0] = (unsigned char *) -1;
                      best_regend[0] = d;	/* Never use regstart[0].  */
                      
                      for (mcnt = 1; mcnt < RE_NREGS; mcnt++)
                        {
                          best_regstart[mcnt] = regstart[mcnt];
                          best_regend[mcnt] = regend[mcnt];
                        }
                    }
                  goto fail;	       
                }
              /* If no failure points, don't restore garbage.  */
              else if (best_regs_set)   
                {
	      restore_best_regs:
                  /* Restore best match.  */
                  d = best_regend[0];
                  
		  for (mcnt = 0; mcnt < RE_NREGS; mcnt++)
		    {
		      regstart[mcnt] = best_regstart[mcnt];
		      regend[mcnt] = best_regend[mcnt];
		    }
                }
            }

	  /* If caller wants register contents data back, convert it 
	     to indices.  */
	  if (regs)
	    {
	      regs->start[0] = pos;
	      if (MATCHING_IN_FIRST_STRING)
		regs->end[0] = d - string1;
	      else
		regs->end[0] = d - string2 + size1;
	      for (mcnt = 1; mcnt < RE_NREGS; mcnt++)
		{
		  if (regend[mcnt] == (unsigned char *) -1)
		    {
		      regs->start[mcnt] = -1;
		      regs->end[mcnt] = -1;
		      continue;
		    }
		  if (IS_IN_FIRST_STRING (regstart[mcnt]))
		    regs->start[mcnt] = regstart[mcnt] - string1;
		  else
		    regs->start[mcnt] = regstart[mcnt] - string2 + size1;
                    
		  if (IS_IN_FIRST_STRING (regend[mcnt]))
		    regs->end[mcnt] = regend[mcnt] - string1;
		  else
		    regs->end[mcnt] = regend[mcnt] - string2 + size1;
		}
	    }
	  return d - pos - (MATCHING_IN_FIRST_STRING 
			    ? string1 
			    : string2 - size1);
        }

      /* Otherwise match next pattern command.  */
#ifdef SWITCH_ENUM_BUG
      switch ((int) ((enum regexpcode) *p++))
#else
      switch ((enum regexpcode) *p++)
#endif
	{

	/* \( [or `(', as appropriate] is represented by start_memory,
           \) by stop_memory.  Both of those commands are followed by
           a register number in the next byte.  The text matched
           within the \( and \) is recorded under that number.  */
	case start_memory:
          regstart[*p] = d;
          IS_ACTIVE (reg_info[*p]) = 1;
          MATCHED_SOMETHING (reg_info[*p]) = 0;
          p++;
          break;

	case stop_memory:
          regend[*p] = d;
          IS_ACTIVE (reg_info[*p]) = 0;

          /* If just failed to match something this time around with a sub-
	     expression that's in a loop, try to force exit from the loop.  */
          if ((! MATCHED_SOMETHING (reg_info[*p])
	       || (enum regexpcode) p[-3] == start_memory)
	      && (p + 1) != pend)              
            {
	      unsigned char *p2 = p + 1;
              mcnt = 0;
              switch (*p2++)
                {
                  case jump_n:
		    is_a_jump_n = 1;
                  case finalize_jump:
		  case maybe_finalize_jump:
		  case jump:
		  case dummy_failure_jump:
                    EXTRACT_NUMBER_AND_INCR (mcnt, p2);
		    if (is_a_jump_n)
		      p2 += 2;
                    break;
                }
	      p2 += mcnt;
        
              /* If the next operation is a jump backwards in the pattern
	         to an on_failure_jump, exit from the loop by forcing a
                 failure after pushing on the stack the on_failure_jump's 
                 jump in the pattern, and d.  */
	      if (mcnt < 0 && (enum regexpcode) *p2++ == on_failure_jump)
		{
                  EXTRACT_NUMBER_AND_INCR (mcnt, p2);
                  PUSH_FAILURE_POINT (p2 + mcnt, d);
                  goto fail;
                }
            }
          p++;
          break;

	/* \<digit> has been turned into a `duplicate' command which is
           followed by the numeric value of <digit> as the register number.  */
        case duplicate:
	  {
	    int regno = *p++;   /* Get which register to match against */
	    unsigned char *d2, *dend2;

	    /* Where in input to try to start matching.  */
            d2 = regstart[regno];
            
            /* Where to stop matching; if both the place to start and
               the place to stop matching are in the same string, then
               set to the place to stop, otherwise, for now have to use
               the end of the first string.  */

            dend2 = ((IS_IN_FIRST_STRING (regstart[regno]) 
		      == IS_IN_FIRST_STRING (regend[regno]))
		     ? regend[regno] : end_match_1);
	    while (1)
	      {
		/* If necessary, advance to next segment in register
                   contents.  */
		while (d2 == dend2)
		  {
		    if (dend2 == end_match_2) break;
		    if (dend2 == regend[regno]) break;
		    d2 = string2, dend2 = regend[regno];  /* end of string1 => advance to string2. */
		  }
		/* At end of register contents => success */
		if (d2 == dend2) break;

		/* If necessary, advance to next segment in data.  */
		PREFETCH;

		/* How many characters left in this segment to match.  */
		mcnt = dend - d;
                
		/* Want how many consecutive characters we can match in
                   one shot, so, if necessary, adjust the count.  */
                if (mcnt > dend2 - d2)
		  mcnt = dend2 - d2;
                  
		/* Compare that many; failure if mismatch, else move
                   past them.  */ 
		char *cpp1=(char *)d;
		char *cpp2=(char *)d2; 
		if (translate   /* IBM! COMPATIBLILITY */ 
                    ? bcmp_translate (cpp1, (char *)d2, mcnt, translate) 
                    : memcmp (cpp1, cpp2, mcnt))
		  goto fail;
		d += mcnt, d2 += mcnt;
	      }
	  }
	  break;

	case anychar:
	  PREFETCH;	  /* Fetch a data character. */
	  /* Match anything but a newline, maybe even a null.  */
	  if ((translate ? translate[*d] : *d) == '\n'
              || ((obscure_syntax & RE_DOT_NOT_NULL) 
                  && (translate ? translate[*d] : *d) == '\000'))
	    goto fail;
	  SET_REGS_MATCHED;
          d++;
	  break;

	case charset:
	case charset_not:
	  {
	    int NOT = 0;	    /* Nonzero for charset_not.  */
	    int c;
	    if (*(p - 1) == (unsigned char) charset_not)
	      NOT = 1;

	    PREFETCH;	    /* Fetch a data character. */

	    if (translate)
	      c = translate[*d];
	    else
	      c = *d;

	    if (c < (int)(*p * BYTEWIDTH)
		&& p[1 + c / BYTEWIDTH] & (1 << (c % BYTEWIDTH)))
	      NOT = !NOT;

	    p += 1 + *p;

	    if (!NOT) goto fail;
	    SET_REGS_MATCHED;
            d++;
	    break;
	  }

	case begline:
          if ((size1 != 0 && d == string1)
              || (size1 == 0 && size2 != 0 && d == string2)
              || (d && d[-1] == '\n')
              || (size1 == 0 && size2 == 0))
            break;
          else
            goto fail;
            
	case endline:
	  if (d == end2
	      || (d == end1 ? (size2 == 0 || *string2 == '\n') : *d == '\n'))
	    break;
	  goto fail;

	/* `or' constructs are handled by starting each alternative with
           an on_failure_jump that points to the start of the next
           alternative.  Each alternative except the last ends with a
           jump to the joining point.  (Actually, each jump except for
           the last one really jumps to the following jump, because
           tensioning the jumps is a hassle.)  */

	/* The start of a stupid repeat has an on_failure_jump that points
	   past the end of the repeat text. This makes a failure point so 
           that on failure to match a repetition, matching restarts past
           as many repetitions have been found with no way to fail and
           look for another one.  */

	/* A smart repeat is similar but loops back to the on_failure_jump
	   so that each repetition makes another failure point.  */

	case on_failure_jump:
        on_failure:
          EXTRACT_NUMBER_AND_INCR (mcnt, p);
          PUSH_FAILURE_POINT (p + mcnt, d);
          break;

	/* The end of a smart repeat has a maybe_finalize_jump back.
	   Change it either to a finalize_jump or an ordinary jump.  */
	case maybe_finalize_jump:
          EXTRACT_NUMBER_AND_INCR (mcnt, p);
	  {
	    unsigned char *p2 = p;
	    /* Compare what follows with the beginning of the repeat.
	       If we can establish that there is nothing that they would
	       both match, we can change to finalize_jump.  */
	    while (p2 + 1 < pend
		   && (*p2 == (unsigned char) stop_memory
		       || *p2 == (unsigned char) start_memory))
	      p2 += 2;				/* Skip over reg number.  */
	    if (p2 == pend)
	      p[-3] = (unsigned char) finalize_jump;
	    else if (*p2 == (unsigned char) exactn
		     || *p2 == (unsigned char) endline)
	      {
		int c = *p2 == (unsigned char) endline ? '\n' : p2[2];
		unsigned char *p1 = p + mcnt;
		/* p1[0] ... p1[2] are an on_failure_jump.
		   Examine what follows that.  */
		if (p1[3] == (unsigned char) exactn && p1[5] != c)
		  p[-3] = (unsigned char) finalize_jump;
		else if (p1[3] == (unsigned char) charset
			 || p1[3] == (unsigned char) charset_not)
		  {
		    int NOT = p1[3] == (unsigned char) charset_not;
		    if (c < (int)(p1[4] * BYTEWIDTH)
			&& p1[5 + c / BYTEWIDTH] & (1 << (c % BYTEWIDTH)))
		      NOT = !NOT;
		    /* `not' is 1 if c would match.  */
		    /* That means it is not safe to finalize.  */
		    if (!NOT)
		      p[-3] = (unsigned char) finalize_jump;
		  }
	      }
	  }
	  p -= 2;		/* Point at relative address again.  */
	  if (p[-1] != (unsigned char) finalize_jump)
	    {
	      p[-1] = (unsigned char) jump;	
	      goto nofinalize;
	    }
        /* Note fall through.  */

	/* The end of a stupid repeat has a finalize_jump back to the
           start, where another failure point will be made which will
           point to after all the repetitions found so far.  */

        /* Take off failure points put on by matching on_failure_jump 
           because didn't fail.  Also remove the register information
           put on by the on_failure_jump.  */
        case finalize_jump:
          POP_FAILURE_POINT ();
        /* Note fall through.  */
        
	/* Jump without taking off any failure points.  */
        case jump:
	nofinalize:
	  EXTRACT_NUMBER_AND_INCR (mcnt, p);
	  p += mcnt;
	  break;

        case dummy_failure_jump:
          /* Normally, the on_failure_jump pushes a failure point, which
             then gets popped at finalize_jump.  We will end up at
             finalize_jump, also, and with a pattern of, say, `a+', we
             are skipping over the on_failure_jump, so we have to push
             something meaningless for finalize_jump to pop.  */
          PUSH_FAILURE_POINT (0, 0);
          goto nofinalize;


        /* Have to succeed matching what follows at least n times.  Then
          just handle like an on_failure_jump.  */
        case succeed_n: 
          EXTRACT_NUMBER (mcnt, p + 2);
          /* Originally, this is how many times we HAVE to succeed.  */
          if (mcnt)
            {
               mcnt--;
	       p += 2;
               STORE_NUMBER_AND_INCR (p, mcnt);
            }
	  else if (mcnt == 0)
            {
	      p[2] = unused;
              p[3] = unused;
              goto on_failure;
            }
          else
	    { 
              fprintf (stderr, "regex: the succeed_n's n is not set.\n");
              exit (1);
	    }
          break;
        
        case jump_n: 
          EXTRACT_NUMBER (mcnt, p + 2);
          /* Originally, this is how many times we CAN jump.  */
          if (mcnt)
            {
               mcnt--;
               STORE_NUMBER(p + 2, mcnt);
	       goto nofinalize;	     /* Do the jump without taking off
			                any failure points.  */
            }
          /* If don't have to jump any more, skip over the rest of command.  */
	  else      
	    p += 4;		     
          break;
        
	case set_number_at:
	  {
  	    unsigned char *p1;

            EXTRACT_NUMBER_AND_INCR (mcnt, p);
            p1 = p + mcnt;
            EXTRACT_NUMBER_AND_INCR (mcnt, p);
	    STORE_NUMBER (p1, mcnt);
            break;
          }

        /* Ignore these.  Used to ignore the n of succeed_n's which
           currently have n == 0.  */
        case unused:
          break;

        case wordbound:
	  if (AT_WORD_BOUNDARY)
	    break;
	  goto fail;

	case notwordbound:
	  if (AT_WORD_BOUNDARY)
	    goto fail;
	  break;

	case wordbeg:
          /* Have to check if AT_STRINGS_BEG before looking at d - 1.  */
	  if (IS_A_LETTER (d) && (AT_STRINGS_BEG || !IS_A_LETTER (d - 1)))
	    break;
	  goto fail;

	case wordend:
          /* Have to check if AT_STRINGS_BEG before looking at d - 1.  */
	  if (!AT_STRINGS_BEG && IS_A_LETTER (d - 1) 
              && (!IS_A_LETTER (d) || AT_STRINGS_END))
	    break;
	  goto fail;

	case wordchar:
	  PREFETCH;
          if (!IS_A_LETTER (d))
            goto fail;
	  SET_REGS_MATCHED;
	  break;
	  
	case notwordchar:
	  PREFETCH;
	  if (IS_A_LETTER (d))
            goto fail;
          SET_REGS_MATCHED;
	  break;

	case begbuf:
          if (AT_STRINGS_BEG)
            break;
          goto fail;

        case endbuf:
	  if (AT_STRINGS_END)
	    break;
	  goto fail;

	case exactn:
	  /* Match the next few pattern characters exactly.
	     mcnt is how many characters to match.  */
	  mcnt = *p++;
	  /* This is written out as an if-else so we don't waste time
             testing `translate' inside the loop.  */
          if (translate)
	    {
	      do
		{
		  PREFETCH;
		  if (translate[*d++] != *p++) goto fail;
		}
	      while (--mcnt);
	    }
	  else
	    {
	      do
		{
		  PREFETCH;
		  if (*d++ != *p++) goto fail;
		}
	      while (--mcnt);
	    }
	  SET_REGS_MATCHED;
          break;

	default:
	  { 
	    fprintf (stderr, "regex: internal error; unknown case label\n");
	    exit (1);
	  }
	}
      continue;  /* Successfully executed one pattern command; keep going.  */

    /* Jump here if any matching operation fails. */
    fail:
      if (stackp != stackb)
	/* A restart point is known.  Restart there and pop it. */
	{
          long last_used_reg, this_reg;
          
          /* If this failure point is from a dummy_failure_point, just
             skip it.  */
	  if (!stackp[-2])
            {
              POP_FAILURE_POINT ();
              goto fail;
            }

          d = *--stackp;
	  p = *--stackp;
          if (d >= string1 && d <= end1)
	    dend = end_match_1;
          /* Restore register info.  */
//          last_used_reg = (short) *--stackp;
          last_used_reg = (long) *--stackp;
          
          /* Make the ones that weren't saved -1 or 0 again.  */
          for (this_reg = RE_NREGS - 1; this_reg > last_used_reg; this_reg--)
            {
              regend[this_reg] = (unsigned char *) -1;
              regstart[this_reg] = (unsigned char *) -1;
              IS_ACTIVE (reg_info[this_reg]) = 0;
              MATCHED_SOMETHING (reg_info[this_reg]) = 0;
            }
          
          /* And restore the rest from the stack.  */
          for ( ; this_reg > 0; this_reg--)
            {
              reg_info[this_reg] = *(struct register_info *) *--stackp;
              regend[this_reg] = *--stackp;
              regstart[this_reg] = *--stackp;
            }
	}
      else
        break;   /* Matching at this starting point really fails.  */
    }

  if (best_regs_set)
    goto restore_best_regs;
  return -1;         			/* Failure to match.  */
}

int
a2_re_match_2 (struct re_pattern_buffer *pbufp,
	    char *string1_arg, int size1,
	    char *string2_arg, int size2,
	    int pos,
	    struct re_registers *regs,
	    int mstop)
{
  // Wrap the real implementation in a function since CFront doesn't like
  // to have "goto" and objects with destructors in the same block.
  cregex_allocator alloca;
  return real_a2_re_match_2 (pbufp, string1_arg, size1, string2_arg, size2,
			  pos, regs, mstop, alloca);
}




static int
bcmp_translate (char *s1, char *s2, int len, unsigned char *translate)
{
  // Use signed char instead of char to avoid compiler warnings
  // about subscripting with a char.
  signed char *p1 = (signed char*)s1, *p2 = (signed char*)s2;
  while (len)
    {
      if (translate [*p1++] != translate [*p2++]) return 1;
      len--;
    }
  return 0;
}



/* Entry points compatible with 4.2 BSD regex library.  */

static struct re_pattern_buffer re_comp_buf;

const char*
re_comp (char *s)
{
  static const char* noprev = "No previous regular expression";
  static const char* memexh = "Memory exhausted";
  if (!s)
    {
      if (!re_comp_buf.buffer) {
        return noprev;
      }
      return 0;
    }

  if (!re_comp_buf.buffer)
    {
      if (!(re_comp_buf.buffer = (char *) malloc (200))) {
	return memexh;
      }
      re_comp_buf.allocated = 200;
      if (!(re_comp_buf.fastmap = new char[1 << BYTEWIDTH])) {
	return memexh;
      }
    }
  return a2_re_compile_pattern (s, strlen (s), &re_comp_buf);
}

int
re_exec (char *s)
{
  int len = strlen (s);
  return 0 <= a2_re_search (&re_comp_buf, s, len, 0, len,
			 static_cast<struct re_registers *>(0));
}

} //# NAMESPACE CASACORE - END

