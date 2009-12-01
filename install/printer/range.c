
#include <ctype.h>
#include <stdio.h>

/*
 * Return 1 if the given number is in the specified range,
 * -1 if there is an error in the range specification,
 * 0 otherwise
 *
 * Ranges have a format *similar* to that used by [nt]roff; i.e.,
 * a comma separated list of specifiers of the form:
 *	1) n	means     x such that x = n
 *	2) :n	means all x such that x <= n
 *	3) n:	means all x such that x >= n
 *	4) n:m	means all x such that n <= x <= m
 *	5) :	means all x
 * n is an integer
 *
 * Problems:
 * The routine prints an error message if the range is strange - this might
 * not always be desirable.
 *
 * I've left the error checking in inrange; it is redundant (I hope!)
 * Nov/85 BJB
 * ========================================================================
 *
 * Copyright (c) Barry Brachman
 *
 * Permission is given to freely copy or distribute this program
 * with the following restrictions:
 *
 *	1) The author is not responsible for the consequences of use of
 *		this software and is not responsible for correcting any defects
 *	2) The origin of this software must not be misrepresented, either by
 *		explicit claim or by omission
 *	3) You may not sell this program
 *	4) Altered versions must be plainly marked as such, and must not
 *		be misrepresented as being the original software
 *
 *
 * Barry Brachman
 * Dept. of Computer Science
 * Univ. of British Columbia
 * Vancouver, B.C. V6T 1W5
 *
 * .. {ihnp4!alberta, uw-beaver}!ubc-vision!ubc-cs!brachman
 * brachman@cs.ubc.cdn
 * brachman%ubc.csnet@csnet-relay.arpa
 * brachman@ubc.csnet
 * ========================================================================
 */
/* $Id$ */

#define SEP_CHAR	':'
#define BAD_CHAR	'?'

static int getnum ();


inrange(num, range)
int num;
char *range;
{
	char *p;
	int type1, type2, val1, val2;

	if (checkrange(range))
		return(-1);
	type1 = type2 = 0;
	p = range;
	while (1) {
		val1 = getnum(&p, &type1);
		if (type1 == BAD_CHAR) {
			fprintf(stderr, "Bad first number in range\n");
			return(-1);
		}

		switch (*p) {
		case ',':
			if (type1 == SEP_CHAR)	/* just a colon */
				return(1);
			if (num == val1)	/* plain number */
				return(1);
			p++;
			continue;
		case ':':
			p++;
			if (*p == ',' || *p == '\0') {		/* no rhs */
				if (type1 == SEP_CHAR)
					return(1);
				if (num >= val1)
					return(1);
				if (*p == '\0') 
					return(0);
				p++;
				break;
			}

			val2 = getnum(&p, &type2);
			if (type2 == BAD_CHAR) {
				fprintf(stderr, "Bad second number in range\n");
				return(-1);
			}

			if (val1 > val2) {
				fprintf(stderr, "Range values reversed\n");
				return(-1);
			}
			if (type1 == SEP_CHAR && num <= val2)
				return(1);
			if (num >= val1 && num <= val2)
				return(1);
			if (*p == '\0')
				return(0);
			if (*p == ',')
				p++;
			break;
		case '\0':
			if (val1 == num)
				return(1);
			return(0);
		default:
			fprintf(stderr, "Bad character in range\n");
			return(-1);
		}
	}
}

checkrange(range)
char *range;
{
	char *p;
	int type1, type2, val1, val2;

	type1 = type2 = 0;
	p = range;
	while (1) {
		val1 = getnum(&p, &type1);
		if (type1 == BAD_CHAR) {
			fprintf(stderr, "Bad first number in range\n");
			return(-1);
		}

		switch (*p) {
		case ',':
			p++;
			continue;
		case ':':
			p++;
			if (*p == ',' || *p == '\0') {
				if (*p == '\0') 
					return(0);
				p++;
				break;
			}

			val2 = getnum(&p, &type2);
			if (type2 == BAD_CHAR) {
				fprintf(stderr, "Bad second number in range\n");
				return(-1);
			}

			if (val1 > val2) {
				fprintf(stderr, "Range values reversed\n");
				return(-1);
			}
			if (*p == '\0')
				return(0);
			if (*p == ',')
				p++;
			break;
		case '\0':
			return(0);
		default:
			fprintf(stderr, "Bad character in range\n");
			return(-1);
		}
	}
}

static
getnum(pp, type)
char **pp;
int *type;
{
	register int sign, val;
	register char *p;

	p = *pp;
	if (!isdigit(*p) && *p != '-') {
		if (*p == SEP_CHAR)
			*type = SEP_CHAR;
		else
			*type = BAD_CHAR;
		return(0);
	}
	sign = 1;
	if (*p == '-') {
		sign = -1;
		p++;
	}
	if (!isdigit(*p)) {
		*type = BAD_CHAR;
		return(0);
	}
	for (val = 0; isdigit(*p) && *p != '\0'; p++)
		val = val * 10 + *p - '0';
	if (*p != '\0' && *p != ',' && *p != SEP_CHAR) {
		*type = BAD_CHAR;
		return(0);
	}
	*pp = p;
	return(sign * val);
}
